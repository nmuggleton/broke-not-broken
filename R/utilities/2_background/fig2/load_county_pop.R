# =====================================================================
# load_county_pop.R
#
# Loads Census county population estimates, used to weight EPI's
# county-level budgets when aggregating them to states.
#
# Source: census.gov/data/datasets/time-series/demo/popest/
# 2020s-counties-total.html, dataset CO-EST20XX-ALLDATA
#
#   SUMLEV  geographic summary level: 040 = state, 050 = county
#   STATE   state FIPS, COUNTY county FIPS (separate fields; the
#           five-digit code is the concatenation)
#   STNAME, CTYNAME  names
#   POPESTIMATE2020 ... POPESTIMATE2024  annual estimates
#
# The file contains both state (SUMLEV 040) and county (SUMLEV 050)
# rows. Summing without filtering double-counts every person.
# =====================================================================

library(data.table)

# Load county population estimates.
load_county_pop <- function(pop_csv_path,
                            pop_col = "POPESTIMATE2024") {
    dt <- fread(pop_csv_path,
        encoding = "Latin-1",
        showProgress = FALSE
    )

    if (!pop_col %in% names(dt)) {
        stop(
            pop_col, " not found. Columns available include: ",
            paste(grep("^POPESTIMATE", names(dt), value = TRUE),
                collapse = ", "
            )
        )
    }

    # County rows only. SUMLEV 040 rows are state totals and would
    # double-count if left in.
    dt <- dt[SUMLEV == 50]
    if (nrow(dt) == 0) {
        stop(
            "No SUMLEV == 50 rows. Check this is the county file ",
            "(CO-EST...) rather than the state file (NST-EST...)."
        )
    }

    dt[, county_fips := sprintf(
        "%02d%03d", as.integer(STATE),
        as.integer(COUNTY)
    )]
    dt[, population := as.numeric(get(pop_col))]

    dt[, .(county_fips,
        state_fips = sprintf("%02d", as.integer(STATE)),
        county = CTYNAME, population
    )]
}

# Aggregate EPI county budgets to state level, weighting counties
# by population.
aggregate_epi_to_state <- function(epi_dt, pop_dt) {
    dt <- merge(epi_dt, pop_dt[, .(county_fips, population)],
        by = "county_fips", all.x = TRUE
    )

    n_unmatched <- dt[is.na(population), uniqueN(county_fips)]
    if (n_unmatched > 0) {
        warning(
            n_unmatched, " EPI counties had no population match ",
            "and are dropped. Connecticut reorganised into ",
            "planning regions in 2022, so its codes differ ",
            "between vintages; check that first."
        )
        dt <- dt[!is.na(population)]
    }

    # area_median_income is EPI's own area-level figure and is not
    # meaningful once aggregated, so it is dropped rather than
    # averaged into something that looks like a household income.
    dt <- dt[measure != "area_median_income"]

    out <- dt[, .(
        value_real = sum(value_real * population) / sum(population),
        n_counties = uniqueN(county_fips),
        population = sum(population)
    ), by = .(
        country, state, household_type, measure, category,
        currency, source
    )]

    setnames(out, "value_real", "value_real")
    out[]
}

# Roll EPI state-level figures up to one national figure per
# household type and category, weighted by state population.
aggregate_epi_to_national <- function(epi_state_dt) {
    out <- epi_state_dt[
        ,
        .(
            value_real = sum(value_real * population) / sum(population),
            n_states   = uniqueN(state)
        ),
        by = .(country, household_type, measure, category, currency, source)
    ]
    out[]
}
