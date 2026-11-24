# ==============================================================================
# schema_budget.R
#
# Schema for the Section 2.3 budget-standards data: what a basic
# standard of living costs, against what households actually have.
#
#   country         "US" | "GB"
#   household_type  e.g., "Single adult", "Lone parent, 1 child"
#   measure         "basket" | "required_income" | "actual_income"
#                   | "area_median_income"
#   category        spending category for measure == "basket"
#                   (housing, food, childcare, ...); NA otherwise
#   value_real      annual amount, real, in base_year prices
#   currency        "USD" | "GBP"
#   source          citation string
#
# The measure column carries three different quantities in one table
# deliberately. "basket" rows decompose the cost of the standard;
# "required_income" is the pre-tax income needed to afford it (which
# is not the basket total, since it includes tax); "actual_income"
# is what a household in that position actually receives. The
# argument in 3.1 is the gap between the last two, so both need to
# sit in the same object as the basket that generates them.
#
# TIME SERIES: do not build one from EPI. EPI state that
# methodology and source data have changed across releases and that
# using the Family Budget Calculator as a price index over time is
# not an appropriate use, since it measures spending rather than
# prices alone. The US side of 3.1 is therefore a snapshot. GB can
# support a trend via JRF's annual count of households below MIS,
# which is a separate series from the MIS budgets themselves.
# ==============================================================================

library(data.table)

BUDGET_SCHEMA <- c(
    "country", "household_type", "measure", "category",
    "value_real", "currency", "source"
)

# Check a data.table matches the budget schema.
validate_budget <- function(dt) {
    BUDGET_MEASURES <- c(
        "basket", "required_income", "actual_income", "area_median_income"
    )

    missing_cols <- setdiff(BUDGET_SCHEMA, names(dt))
    if (length(missing_cols) > 0) {
        stop(
            "Missing column(s): ",
            paste(missing_cols, collapse = ", "),
            "\nExpected: ", paste(BUDGET_SCHEMA, collapse = ", ")
        )
    }

    bad_country <- setdiff(unique(dt$country), c("US", "GB"))
    if (length(bad_country) > 0) {
        stop(
            "Unexpected country code(s): ",
            paste(bad_country, collapse = ", ")
        )
    }

    bad_measure <- setdiff(unique(dt$measure), BUDGET_MEASURES)
    if (length(bad_measure) > 0) {
        stop(
            "Unexpected measure(s): ",
            paste(bad_measure, collapse = ", "),
            "\nExpected one of: ",
            paste(BUDGET_MEASURES, collapse = ", ")
        )
    }

    # A basket row without a category is meaningless; an income row
    # with one is a sign the loader has mislabelled something.
    if (dt[measure == "basket" & is.na(category), .N] > 0) {
        stop("Some measure == \"basket\" rows have no category.")
    }
    if (dt[measure != "basket" & !is.na(category), .N] > 0) {
        stop(
            "Some income rows carry a category; income is a total, ",
            "not a category-level quantity."
        )
    }

    invisible(dt)
}

# Shortfall of actual income against the income a household needs
# to reach the standard, by country and household type.
budget_shortfall <- function(dt) {
    validate_budget(dt)

    if (dt[measure == "actual_income", .N] == 0) {
        stop(
            "No actual_income rows: the shortfall comparison needs ",
            "income for the specific household type. Note that ",
            "area_median_income is NOT a substitute - it is the ",
            "same figure for every household type in an area."
        )
    }

    wide <- dcast(
        dt[measure %in% c("required_income", "actual_income")],
        country + household_type ~ measure,
        value.var = "value_real"
    )

    wide[, shortfall := required_income - actual_income]
    wide[, pct_of_required := actual_income / required_income * 100]
    wide[]
}


# =====================================================================
# Household-level bootstrap for the cross-area median CI shown in
# plot_below_standard.R.
# =====================================================================

# Poisson-bootstrap replicate multipliers.
.poisson_bootstrap_mult <- function(n) {
    stats::rpois(n, 1)
}

# =====================================================================
# National (pooled) estimate and its CI.
# =====================================================================

# Population-weighted national share below the threshold, per
# household type.
national_share <- function(dt) {
    dt <- as.data.table(dt)
    dt[, .(
        share_below = sum(weight * (income < threshold)) / sum(weight),
        n_records   = .N
    ), by = household_type]
}

# Bootstrap CI for the pooled national share, per household type.
bootstrap_national_ci <- function(dt, R = 500L, conf = 0.95) {
    dt <- as.data.table(dt)
    below <- dt$income < dt$threshold
    types <- unique(dt$household_type)

    out <- vector("list", length(types))
    for (ti in seq_along(types)) {
        rows <- which(dt$household_type == types[ti])
        w0 <- dt$weight[rows]
        b0 <- below[rows]
        n <- length(rows)

        shares <- vapply(seq_len(R), function(r) {
            w <- w0 * .poisson_bootstrap_mult(n)
            if (sum(w) == 0) return(NA_real_)
            sum(w * b0) / sum(w)
        }, numeric(1))

        shares <- shares[!is.na(shares)]
        q <- stats::quantile(shares, c(
            (1 - conf) / 2,
            1 - (1 - conf) / 2
        ),
        names = FALSE, na.rm = TRUE
        )
        out[[ti]] <- data.table(
            household_type = types[ti],
            ci_lo = q[1], ci_hi = q[2]
        )
    }
    rbindlist(out)
}

# =====================================================================
# Converting required_income from gross to net.
# =====================================================================
net_of_tax_required_income <- function(budget_dt) {
    budget_dt <- as.data.table(copy(budget_dt))

    taxes <- budget_dt[
        measure == "basket" & category == "Taxes",
        .(country, household_type,
            tax_value = value_real
        )
    ]

    if (nrow(taxes) == 0) {
        message(
            "net_of_tax_required_income(): no Taxes basket rows ",
            "found; required_income left unchanged."
        )
        return(budget_dt[])
    }

    budget_dt <- merge(budget_dt, taxes,
        by = c("country", "household_type"),
        all.x = TRUE
    )
    budget_dt[
        measure == "required_income" & !is.na(tax_value),
        value_real := value_real - tax_value
    ]
    budget_dt[, tax_value := NULL]
    budget_dt[]
}
