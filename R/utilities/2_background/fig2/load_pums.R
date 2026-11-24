# =====================================================================
# load_pums.R
#
# Loads ACS PUMS housing records and derives, by state and family
# type, the share of households with income below the cost of a
# basic standard of living (EPI's total, via load_epi.R). This is
# the US counterpart to JRF's "households below a Minimum Income
# Standard" series, and gives Section 2.3 the same statistic in both
# countries.
#
# Source: census.gov/programs-surveys/acs/microdata/access, FTP
# site, per-state CSVs.
# =====================================================================

source("R/utilities/2_background/fig2/schema_budget.R")
library(data.table)

# Load one or more PUMS state housing files.
load_pums <- function(paths,
                      income_var = c("HINCP", "FINCP"),
                      max_adults = 2,
                      max_children = 4) {
    income_var <- match.arg(income_var)

    keep_cols <- c(
        "SERIALNO", "STATE", "PUMA", "TYPEHUGQ", "WGTP",
        "NP", "NRC", "NOC", "NPF", "ADJINC", income_var
    )

    # STATE and PUMA are zero-padded FIPS codes ("02", "00400").
    # Left to fread's type inference they become integers and the
    # leading zeros are lost, (needed to join to EPI.
    dt <- rbindlist(lapply(paths, function(p) {
        fread(p,
            select = keep_cols, showProgress = FALSE,
            colClasses = list(character = c(
                "STATE", "PUMA",
                "SERIALNO"
            ))
        )
    }), use.names = TRUE)

    n_raw <- nrow(dt)

    # Housing units only; group quarters carry no household income.
    dt <- dt[TYPEHUGQ == 1 & WGTP > 0]
    n_hu <- nrow(dt)

    # VACANT units. TYPEHUGQ == 1 covers all housing units, occupied
    # or not, and vacant ones carry a positive weight but no
    # household: NP is 0 and every income variable is blank.
    dt <- dt[NP > 0]
    n_vacant <- n_hu - nrow(dt)

    setnames(dt, income_var, "income_nominal")

    n_before_income <- nrow(dt)
    dt <- dt[!is.na(income_nominal)]
    n_no_income <- n_before_income - nrow(dt)

    # Children: related children under 18 in the household. NRC is
    # used rather than NOC because EPI's budgets cover dependants in
    # the household, not only the householder's own children.
    #
    # NRC is blank for non-family households; treated as zero.
    dt[, children := fifelse(is.na(NRC), 0L, as.integer(NRC))]
    dt[, adults := NP - children]

    n_before_grid <- nrow(dt)
    dt <- dt[adults >= 1 & adults <= max_adults &
        children >= 0 & children <= max_children]
    n_dropped <- n_before_grid - nrow(dt)

    dt[, family := paste0(adults, "p", children, "c")]
    dt[, household_type := epi_family_label(family)]

    # ADJINC is stored as an integer with six implied decimals.
    dt[, income := income_nominal * (ADJINC / 1e6)]

    message(
        "PUMS: ", n_raw, " records read, ", n_hu,
        " housing units with positive weight, ", n_vacant,
        " vacant, ", n_no_income, " occupied but missing ",
        income_var, ", ", n_dropped, " outside EPI's ",
        max_adults, "-adult / ", max_children,
        "-child grid, ", nrow(dt), " retained."
    )

    # Among OCCUPIED units, HINCP should be near-universal, so a
    # large drop here is a real problem rather than housing stock.
    occupied <- n_hu - n_vacant
    if (occupied > 0 && n_no_income > 0.05 * occupied) {
        if (income_var == "FINCP") {
            warning(
                round(100 * n_no_income / occupied), " per cent ",
                "of occupied units dropped for missing FINCP. ",
                "This is expected and wrong for this purpose: ",
                "FINCP is blank for non-family households, so ",
                "single-adult households are being lost. Use ",
                "HINCP."
            )
        } else {
            warning(
                round(100 * n_no_income / occupied), " per cent ",
                "of occupied units dropped for missing HINCP, ",
                "which is higher than expected. Check the file ",
                "and the HINCP column before relying on this."
            )
        }
    }

    # PUMS identifies states by two-digit FIPS code; EPI uses the USPS
    # two-letter abbreviation. The two are joined on the abbreviation,
    # so the crosswalk lives here rather than being a further download.
    STATE_FIPS_TO_ABB <- c(
        "01" = "AL", "02" = "AK", "04" = "AZ", "05" = "AR", "06" = "CA",
        "08" = "CO", "09" = "CT", "10" = "DE", "11" = "DC", "12" = "FL",
        "13" = "GA", "15" = "HI", "16" = "ID", "17" = "IL", "18" = "IN",
        "19" = "IA", "20" = "KS", "21" = "KY", "22" = "LA", "23" = "ME",
        "24" = "MD", "25" = "MA", "26" = "MI", "27" = "MN", "28" = "MS",
        "29" = "MO", "30" = "MT", "31" = "NE", "32" = "NV", "33" = "NH",
        "34" = "NJ", "35" = "NM", "36" = "NY", "37" = "NC", "38" = "ND",
        "39" = "OH", "40" = "OK", "41" = "OR", "42" = "PA", "44" = "RI",
        "45" = "SC", "46" = "SD", "47" = "TN", "48" = "TX", "49" = "UT",
        "50" = "VT", "51" = "VA", "53" = "WA", "54" = "WV", "55" = "WI",
        "56" = "WY", "72" = "PR"
    )

    dt[, state_abb := STATE_FIPS_TO_ABB[STATE]]
    if (dt[is.na(state_abb), .N] > 0) {
        stop(
            "Unrecognised STATE FIPS code(s): ",
            paste(unique(dt[is.na(state_abb), STATE]),
                collapse = ", "
            )
        )
    }

    dt[, .(
        serialno = SERIALNO, state_fips = STATE,
        state = state_abb, puma = PUMA,
        family, household_type, adults, children,
        income, weight = WGTP
    )]
}

# Rebase PUMS incomes from the file's dollar year to another year.
deflate_to_year <- function(pums_dt, factor) {
    if (missing(factor) || !is.numeric(factor) || length(factor) != 1) {
        stop(
            "Supply an explicit numeric rebasing factor: ADJINC ",
            "leaves income in the dollars of the file's final year, ",
            "which is not necessarily EPI's basis."
        )
    }
    dt <- copy(pums_dt)
    dt[, income := income * factor]
    dt[]
}

# Share of households below the cost of the standard, by state and
# family type.
share_below_standard <- function(pums_dt, epi_dt) {
    thresholds <- epi_dt[
        measure == "required_income",
        .(state, household_type,
            threshold = value_real
        )
    ]
    thresholds <- unique(thresholds)

    dt <- merge(pums_dt, thresholds,
        by = c("state", "household_type"), all.x = FALSE
    )
    if (nrow(dt) == 0) {
        stop(
            "No rows matched between PUMS and EPI. load_pums() ",
            "returns `state` as a two-letter abbreviation; check ",
            "that epi_dt has been aggregated to state level and ",
            "carries the same column."
        )
    }

    dt[, .(
        threshold  = first(threshold),
        share_below = sum(weight * (income < threshold)) / sum(weight),
        n_records   = .N
    ), by = .(state, household_type)]
}

rm(BUDGET_SCHEMA)
