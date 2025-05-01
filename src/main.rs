#![warn(clippy::pedantic)]
#![warn(clippy::nursery)]
use anyhow::{Result, anyhow};
use backon::{BlockingRetryable as _, ConstantBuilder};
use base64::{Engine, prelude::BASE64_STANDARD};
use chrono::{Datelike, Months, NaiveDate, Utc, Weekday};
use clap::{Arg, ArgAction, ArgMatches, command, error::ErrorKind, value_parser};
use comfy_table::{
    Attribute, Cell, CellAlignment, Color, ColumnConstraint, ContentArrangement, Table,
    modifiers::UTF8_ROUND_CORNERS, presets::UTF8_FULL,
};
use core::time::Duration;
use indoc::printdoc;
use reqwest::{
    blocking::Client,
    header::{REFERER, USER_AGENT},
};
use rust_decimal::{Decimal, MathematicalOps as _, RoundingStrategy};
use serde::Deserialize;
use std::thread;
use ua_generator::ua::spoof_ua;

mod dmy_hyphenated {
    use chrono::NaiveDate;
    use serde::{Deserialize as _, Deserializer, de};

    pub fn deserialize<'de, D>(deserializer: D) -> Result<NaiveDate, D::Error>
    where
        D: Deserializer<'de>,
    {
        let deserialised_string = String::deserialize(deserializer)?;
        let deserialised_date = NaiveDate::parse_from_str(&deserialised_string, "%d-%m-%Y")
            .map_err(de::Error::custom)?;
        Ok(deserialised_date)
    }
}

#[derive(Deserialize)]
struct Quote {
    // Use Formatted so that there's no weird JSON floating point taking place - direct from String to Decimal.
    #[serde(rename(deserialize = "priceFormatted"))]
    clean_price: Decimal, // Renaming specifically to be clear this is not the all-in price...
    #[serde(rename(deserialize = "currencyCode"))]
    currency_code: String,
}

struct TableRow {
    annualised_gross: Decimal,
    annualised_net: Decimal,
    clean_price: Decimal,
    coupon_rate: Decimal,
    dirty_price: Decimal,
    isin: String,
    maturity: NaiveDate,
    symbol: String,
    prefer_symbol: Option<String>,
    total_net: Decimal,
}

#[derive(Deserialize)]
struct Bond {
    #[serde(rename(deserialize = "coupon"))]
    coupon_percent: Decimal, // e.g. 1.5 means 1.5% coupon.
    isin: String,
    #[serde(rename(deserialize = "lotSize"))]
    lot_size: Decimal,
    #[serde(with = "dmy_hyphenated")]
    #[serde(rename(deserialize = "maturityDate"))]
    maturity_date: NaiveDate,
    name: String,
    quote: Quote,
    symbol: String,
}

#[derive(Deserialize)]
struct Bonds {
    data: Option<Vec<Bond>>,
    #[serde(rename(deserialize = "errorReference"))]
    error_reference: Option<String>,
    message: Option<String>,
    success: bool,
}

fn set_up_flags() -> ArgMatches {
    let mut cmd = command!()
    .arg_required_else_help(true)
    .arg(
        Arg::new("income-tax-percent")
            .short('t')
            .long("income-tax-percent")
            .action(ArgAction::Set)
            .value_name("PERCENTAGE_RATE")
            .value_parser(value_parser!(Decimal))
            .required(true)
            .help("The marginal rate of tax paid by the individual on arising income.  For example, '20' for 20% tax."),
    )
    .arg(
        Arg::new("show-hidden-rows")
        .long("show-hidden-rows")
        .short('s')
        .action(ArgAction::SetTrue)
        .help("Shows all rows in the table for completeness, even gilts which would return less money overall.")
    )
    ;
    let matches = cmd.get_matches_mut();

    let income_tax_percent: Decimal = *matches.get_one("income-tax-percent").unwrap();
    if income_tax_percent < Decimal::ZERO {
        cmd.error(
            ErrorKind::ValueValidation,
            "Your marginal tax rate must be 0% or higher.  Set --income-tax-percent to a number greater than 0."
        ).exit()
    }
    if income_tax_percent >= Decimal::ONE_HUNDRED {
        cmd.error(
            ErrorKind::ValueValidation,
            "Your marginal tax rate must be lower than 100%.  Set --income-tax-percent to a number less than 100."
        ).exit()
    }
    if income_tax_percent > Decimal::ZERO && income_tax_percent < Decimal::ONE {
        println!(
            "WARNING: You may have entered the wrong income tax percentage.  You indicated a tax rate of {income_tax_percent}%."
        );
        println!(
            "WARNING: If you meant to enter {}%, then set --income-tax-percent={} instead.",
            (income_tax_percent * Decimal::ONE_HUNDRED).normalize(),
            (income_tax_percent * Decimal::ONE_HUNDRED).normalize()
        );
    }
    matches
}

fn get_bond_list_retriable() -> Result<Bonds> {
    // I would just really prefer some strings did not show up in standard github-wide code searching.
    let resp = Client::new()
        .get(String::from_utf8(BASE64_STANDARD.decode("aHR0cHM6Ly93d3cuYWpiZWxsLmNvLnVrL2FwaS9zZWN1cml0aWVzL3RyYWRhYmxlLWJvbmRzLWdpbHRz")?)?)
        .header(
            REFERER,
            BASE64_STANDARD.decode("aHR0cHM6Ly93d3cuYWpiZWxsLmNvLnVrL291ci1zZXJ2aWNlcy9pbnZlc3RtZW50LW9wdGlvbnMvZ2lsdHMvcHJpY2Vz")?,
        )
        .header(USER_AGENT, spoof_ua())
        .header(
            BASE64_STANDARD.decode(b"WC1BdXRoLUFKQg==")?,
            BASE64_STANDARD.decode("WW91SW52ZXN0RGV2aWNlVG9rZW4gdG9rZW49Mjc4YmM4MzgtYjI0Ni00ZmE3LWIwNjYtNWUyMzFlYmMwODZl")?,
        )
        .send()?;
    let parsed = resp.json::<Bonds>()?;
    if !parsed.success || parsed.data.as_ref().is_none_or(Vec::is_empty) {
        Err(anyhow!(
            "fetch failed: API returned success={} data_exists={} data_elems={} message={:?} error_reference={:?}",
            parsed.success,
            parsed.data.is_some(),
            parsed.data.unwrap_or(Vec::new()).len(),
            parsed.message.unwrap_or(String::new()),
            parsed.error_reference.unwrap_or(String::new())
        ))
    } else {
        Ok(parsed)
    }
}

fn get_bond_list() -> Vec<Bond> {
    let delay = Duration::from_secs(2);
    get_bond_list_retriable
        .retry(
            ConstantBuilder::new()
                .with_delay(delay)
                .with_jitter()
                .with_max_times(10),
        )
        .sleep(thread::sleep)
        .notify(|err: &anyhow::Error, _: Duration| {
            // When jitter is turned on, the second param is super ugly (e.g. 2.78765s).
            println!("Got {err:?}, retrying after around {delay:?}.");
        })
        .call()
        .expect("Even after retries, the bond list could not be fetched.")
        .data
        .unwrap() // Unwrap is safe - get_bond_list_retriable retries if data is none.
}

fn filter_strictly_worse_rows(rows: &mut Vec<TableRow>) {
    // This ONLY removes bonds that illogically take more time to return strictly less.
    // Get the one that returns more, then hold it at 0% in a bank for the remaining time.
    rows.sort_by(|x, y| x.maturity.cmp(&y.maturity));
    let mut best_so_far = None;
    for row in rows {
        if best_so_far
            .as_ref()
            .is_some_and(|(x, _): &(Decimal, String)| x.gt(&row.total_net))
        {
            row.prefer_symbol = Some(best_so_far.as_ref().unwrap().1.clone());
        } else {
            best_so_far = Some((row.total_net, row.symbol.clone()));
        }
    }
}

fn date_n_weekdays_before(date: NaiveDate, days: i8) -> NaiveDate {
    let mut current_date = date;
    let mut days_before = 0;
    while days_before < days {
        current_date = current_date.pred_opt().unwrap();
        if matches!(
            current_date.weekday(),
            Weekday::Mon | Weekday::Tue | Weekday::Wed | Weekday::Thu | Weekday::Fri
        ) {
            days_before += 1;
        }
    }
    current_date
}

fn calculate_gilt_returns(bonds: Vec<Bond>, income_tax_rate: Decimal) -> Vec<TableRow> {
    // For simplicity, this calculation operates daily for midnight UTC each day, which isn't 'strictly' correct during DST.
    // In those circumstances, the data view between 0000 and 0100 AM BST may show the prior day's data - but markets won't
    // be open anyway, so the AJB quote information will already be many many hours stale.  Therefore it's not a concern.
    let today = Utc::now().date_naive();

    let mut table_rows = Vec::new();
    for bond in bonds {
        if bond.maturity_date <= today  // Already matured this morning (or earlier)
            || !bond.isin.to_lowercase().starts_with("gb")  // Not a GB bond.
            || !bond.name.to_lowercase().contains("treasury")  // Not a gilt.
            || (bond.name.to_lowercase().contains("index")  // Index linked gilt returns can't be calculated.
                && bond.name.to_lowercase().contains("linked"))
        {
            continue;
        }

        if !bond.quote.currency_code.is_empty() && bond.quote.currency_code.to_lowercase() != "gbp"
        {
            println!(
                "Skipping bond {:?} ({}/{}), because its currency was {:?}.",
                bond.name, bond.isin, bond.symbol, bond.quote.currency_code
            );
            continue;
        }

        if bond.lot_size != Decimal::ONE_HUNDRED {
            println!(
                "Bond {:?} ({}/{}) has an unexpected lot size of {} (usually 100).  Will calculate anyway.",
                bond.name,
                bond.isin,
                bond.symbol,
                bond.lot_size.normalize()
            );
        }

        let (first_day_in_period, last_day_in_period, num_periods) = {
            // Work backwards from the maturity date, six months at a time.
            // If we land on the current date, stop, because that means we're at the very very start of a period (i.e. the payout was pre-open today).
            let mut num_periods = 0;
            while bond
                .maturity_date
                .checked_sub_months(Months::new(num_periods * 6))
                .unwrap()
                > today
            {
                num_periods += 1;
            }
            assert!(num_periods > 0); // This would have been filtered otherwise.

            let first_day_in_period = bond
                .maturity_date
                .checked_sub_months(Months::new(num_periods * 6))
                .unwrap();
            let last_day_in_period = bond
                .maturity_date
                .checked_sub_months(Months::new((num_periods - 1) * 6)) // Requires num_periods > 0.
                .unwrap()
                .pred_opt()
                .unwrap();

            (
                first_day_in_period,
                last_day_in_period,
                Decimal::from(num_periods),
            )
        };

        // Interest payments go to the owner 7 business days before the payment.  This code is an approximation (7 weekdays).
        // No ex-dividend period exists for the final dividend which is paid on the principal repayment date.
        let ex_dividend_period_count_adjustment = if num_periods == Decimal::ONE
            || today < date_n_weekdays_before(last_day_in_period, 7)
        {
            Decimal::ZERO // No adjustment to make, we will receive the repayment.
        } else {
            Decimal::NEGATIVE_ONE // We will not receive the upcoming payment.
        };

        // Accrued interest is bought off the prior owner but is NOT subject to income tax.
        let days_through_period: Decimal = (today - first_day_in_period).num_days().into();
        let days_in_period: Decimal = (last_day_in_period - first_day_in_period).num_days().into();
        let coupon_rate = bond.coupon_percent * Decimal::new(1, 2); // e.g. 2 -> 0.02
        let period_coupon = coupon_rate * Decimal::new(5, 1); // Each 6-monthly period, only half the coupon is paid.
        // Note: This may end up negative, this is INTENTIONAL!
        let accrued_interest = ((days_through_period / days_in_period) + ex_dividend_period_count_adjustment)
            * period_coupon
            * bond.lot_size;
        let total_interest = period_coupon * num_periods * bond.lot_size;
        let taxable_interest = total_interest - accrued_interest;
        let tax_on_interest = taxable_interest * income_tax_rate;
        let dirty_price = bond.quote.clean_price + accrued_interest;
        let days_to_maturity: Decimal = (bond.maturity_date - today).num_days().into();
        let gross_receipt = bond.lot_size + total_interest;
        let net_receipt = gross_receipt - tax_on_interest;
        let net_return = net_receipt / dirty_price;

        // Sadly, to annualise the return, we have to choose how many days are in a 'year'.  I formally choose it to be 365.25.
        // The daily rate is the nth-root of the return (for n = days to maturity). By raising that to 365.25, we get an 'annual' rate.
        let annualised_net_return =
            net_return.powd(Decimal::new(36525, 2) / days_to_maturity) - Decimal::ONE;
        // Flag validation ensures this to be true, but let's explicitly state it here again to avoid a division by zero.
        assert!(income_tax_rate != Decimal::ONE);
        let grossed_up_return = annualised_net_return / (Decimal::ONE - income_tax_rate);

        table_rows.push(TableRow {
            isin: bond.isin,
            symbol: bond.symbol,
            coupon_rate,
            maturity: bond.maturity_date,
            // Round away from zero to make the gilt look more expensive than it actually is.
            clean_price: bond
                .quote
                .clean_price
                .round_dp_with_strategy(3, RoundingStrategy::AwayFromZero),
            dirty_price: dirty_price.round_dp_with_strategy(3, RoundingStrategy::AwayFromZero),
            annualised_net: annualised_net_return,
            annualised_gross: grossed_up_return,
            total_net: net_return,
            prefer_symbol: None,
        });
    }
    filter_strictly_worse_rows(&mut table_rows);
    table_rows.sort_by(|x, y| y.annualised_net.cmp(&x.annualised_net));
    table_rows
}

fn generate_table_headers(show_notes: bool) -> Table {
    let mut data_table = Table::new();
    data_table
        .load_preset(UTF8_FULL)
        .apply_modifier(UTF8_ROUND_CORNERS)
        .set_content_arrangement(ContentArrangement::Dynamic)
        .set_header(vec![
            Cell::new("\nISIN"),
            Cell::new("\nSymbol"),
            Cell::new("\nCoupon"),
            Cell::new("\nMaturity"),
            Cell::new("Clean\nPrice"),
            Cell::new("Dirty\nPrice"),
            Cell::new("Net\n(/yr)"),
            Cell::new("Equiv.\nGross"),
            Cell::new("\nNotes"),
        ]);

    // Numeric columns get aligned to the right so decimal places match up.
    data_table
        .column_mut(2)
        .unwrap()
        .set_cell_alignment(CellAlignment::Right);
    data_table
        .column_mut(4)
        .unwrap()
        .set_cell_alignment(CellAlignment::Right);
    data_table
        .column_mut(5)
        .unwrap()
        .set_cell_alignment(CellAlignment::Right);
    data_table
        .column_mut(6)
        .unwrap()
        .set_cell_alignment(CellAlignment::Right);
    data_table
        .column_mut(7)
        .unwrap()
        .set_cell_alignment(CellAlignment::Right);
    if !show_notes {
        data_table
            .column_mut(8)
            .unwrap()
            .set_constraint(ColumnConstraint::Hidden);
    }
    data_table
}

fn insert_table_entries(data_table: &mut Table, rows: Vec<TableRow>, show_hidden_rows: bool) {
    let hidden_row_count = rows.iter().filter(|x| x.prefer_symbol.is_some()).count();
    if hidden_row_count > 0 {
        println!(
            "Note: {hidden_row_count} of the discovered gilts are strictly worse (they take more time to return less money).",
        );

        if show_hidden_rows {
            println!("Those rows will still be shown because of --show-hidden-rows.");
        } else {
            println!("Those rows have been hidden - show them with --show-hidden-rows.");
        }
    }

    for row in rows {
        let should_hide = row.prefer_symbol.is_some();
        let (colour, style) = if should_hide {
            (Color::Red, vec![Attribute::CrossedOut, Attribute::Dim])
        } else {
            (Color::Reset, vec![Attribute::Reset])
        };
        data_table.add_row_if(
            |_, _| show_hidden_rows || !should_hide,
            vec![
                Cell::new(row.isin).add_attributes(style.clone()).fg(colour),
                Cell::new(row.symbol)
                    .add_attributes(style.clone())
                    .fg(colour),
                Cell::new(format!("{:6.3} %", row.coupon_rate * Decimal::ONE_HUNDRED))
                    .add_attributes(style.clone())
                    .fg(colour),
                Cell::new(row.maturity.format("%Y-%m-%d").to_string())
                    .add_attributes(style.clone())
                    .fg(colour),
                Cell::new(format!("\u{a3}{:7.3}", row.clean_price))
                    .add_attributes(style.clone())
                    .fg(colour),
                Cell::new(format!("\u{a3}{:7.3}", row.dirty_price))
                    .add_attributes(style.clone())
                    .fg(colour),
                Cell::new(format!(
                    "{:6.3}%",
                    row.annualised_net * Decimal::ONE_HUNDRED
                ))
                .add_attributes(style.clone())
                .fg(colour),
                Cell::new(format!(
                    "{:6.3}%",
                    row.annualised_gross * Decimal::ONE_HUNDRED
                ))
                .add_attributes(style.clone())
                .fg(colour),
                // Don't colour or cross out this cell, it needs to be readable.
                Cell::new(
                    row.prefer_symbol
                        .map_or_else(String::new, |x| format!("Use {x}")),
                ),
            ],
        );
    }
}

fn main() {
    let flags = set_up_flags(); // Important to do first, because we want to fail early if the flags are not set properly.
    let bonds = get_bond_list();

    let income_tax_percent: Decimal = *flags.get_one("income-tax-percent").unwrap(); // Safe because of value_parser.
    let income_tax_rate = income_tax_percent / Decimal::ONE_HUNDRED;
    let table_rows = calculate_gilt_returns(bonds, income_tax_rate);

    let mut data_table = generate_table_headers(flags.get_flag("show-hidden-rows"));
    insert_table_entries(
        &mut data_table,
        table_rows,
        flags.get_flag("show-hidden-rows"),
    );

    printdoc! {"
        
        =============================[IMPORTANT DISCLAIMER]=============================
        This program is neither financial nor tax advice.  It probably contains errors.
        It may no longer match current UK taxation legislation.  You MUST do your own
        checks before making any trading decision.
        ================================================================================
    
    "}
    println!("{data_table}");
}
