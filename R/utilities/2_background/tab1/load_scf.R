# =====================================================================
# load_scf.R
#
# Loads Federal Reserve Survey of Consumer Finances Bulletin-chart
# data, reconstructs per-household means, and produces a debt-type
# composition table for the lowest net-worth bracket.
#
# Source: federalreserve.gov/econres/scf/dataviz/scf/table/
# classification group "nwcat" (net worth percentile). Download both
# the "_mean" and "_have" CSVs for nwcat - we need both.
#
# critical: the "_mean" file's values are the mean AMONG HOUSEHOLDS
# THAT HOLD the item, not the mean across the whole group (Net_Worth
# and Before_Tax_ Income are the only two exceptions). To get a
# per-household mean we multiply by the matching household
# from the "_have" file (percent of the group holding that item):
#     per_household = mean_among_holders * (have_pct / 100)
# Verified: for Bottom25% in 1989, per-household Assets
# minus per-household Debt reconstructed this way equals the
# reported Net_Worth column to the dollar.
# This is checked against that identity every time this loader runs
# (see the internal check below), and will error out if a future
# vintage of the Fed's files breaks the identity.
#
# Units: confirmed against the Fed's own page note - "All dollar
# estimates are given in 2022 dollars," and the SCF Bulletin
# convention of reporting in THOUSANDS. Both applied below.
#
# Mortgage exclusion: only Home_Secured_Debt is excluded).
# Other_Res_Real_Estate_Debt (debt on non-primary
# residential property) is kept in: it is
# investment-property debt.
# =====================================================================

source("R/utilities/2_background/schema_net_assets.R")
library(data.table)

SCF_GROUP_LEVELS <- c(
    "Bottom 25%", "25-49.9%", "50-74.9%", "75-89.9%", "Top 10%"
)

.SCF_GROUP_LOOKUP <- c(
    "Less than 25" = "Bottom 25%",
    "25-49.9"      = "25-49.9%",
    "50-74.9"      = "50-74.9%",
    "75-89.9"      = "75-89.9%",
    "90-100"       = "Top 10%",
    "Top 10"       = "Top 10%"
)

# Debt subcategories that sum to total Debt (per the Fed's own
# variable definitions), excluding Home_Secured_Debt - see header.
# Vehicle/Education/Other_Installment_Loans are the sub-breakdown of
# Installment_Loans itself, pulled alongside it (not instead of it)
# so both the aggregate and the breakdown are available - always
# better to break down too far than not enough.
.SCF_DEBT_CATEGORIES <- c(
    "Other_Res_Real_Estate_Debt", "Installment_Loans",
    "Vehicle_Installment_Loans", "Education_Installment_Loans",
    "Other_Installment_Loans",
    "Other_Lines_of_Credit", "Credit_Card_Balances", "Other_Debt"
)

# Category code -> display label, sort order, and whether a row
# contributes to the printed total. The join below is an
# INNER join on `category`, which is also what excludes Assets, Debt,
# and Home_Secured_Debt from the final output - those three raw
# categories are pulled earlier (for the reconstruction identity
# check) but have no entry here, so they are dropped at this step
# rather than needing an explicit filter.
.DEBT_CATEGORY_LABELS <- data.table(
    category = c(
        "Installment_Loans",
        "Vehicle_Installment_Loans", "Education_Installment_Loans",
        "Other_Installment_Loans",
        "Credit_Card_Balances", "Other_Lines_of_Credit",
        "Other_Res_Real_Estate_Debt", "Other_Debt"
    ),
    label = c(
        "Instalment loans",
        "\\quad Vehicle", "\\quad Education", "\\quad Other instalment",
        "Credit card balances", "Other lines of credit",
        "Other residential real estate debt", "Other debt"
    ),
    sort_order  = 1:8,
    is_subtotal = c(TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE)
)

# Load SCF debt composition for a wealth group, by category, from
# the raw Bulletin-chart CSVs.

load_scf <- function(scf_mean_path,
                     scf_have_path,
                     deflator,
                     base_year) {
    mean_dt <- fread(scf_mean_path)
    have_dt <- fread(scf_have_path)

    id_cols <- c("year", "Category")
    debt_cols <- c(
        "Assets", "Debt", "Home_Secured_Debt",
        .SCF_DEBT_CATEGORIES
    )

    mean_long <- melt(mean_dt[, c(id_cols, debt_cols), with = FALSE],
        id.vars = id_cols, variable.name = "category",
        value.name = "mean_among_holders"
    )
    have_long <- melt(have_dt[, c(id_cols, debt_cols), with = FALSE],
        id.vars = id_cols, variable.name = "category",
        value.name = "have_pct"
    )

    dt <- merge(mean_long, have_long, by = c(id_cols, "category"))

    # Thousands of 2022 USD -> USD.
    dt[, value_2022usd := mean_among_holders * (have_pct / 100) * 1000]

    # Sanity check: Assets - Debt (per household, reconstructed) must
    # equal the reported Net_Worth-derived identity. Computed here
    # from the wide data directly, not the long/melted version, and
    # errors.
    wide_check <- merge(
        mean_dt[, c(id_cols, "Assets", "Debt"), with = FALSE],
        have_dt[, c(id_cols, "Assets", "Debt"), with = FALSE],
        by = id_cols, suffixes = c("_mean", "_have")
    )
    wide_check[, assets_hh := Assets_mean * (Assets_have / 100) * 1000]
    wide_check[, debt_hh   := Debt_mean * (Debt_have / 100) * 1000]
    wide_check[, implied_net_worth := assets_hh - debt_hh]
    # Reported Net_Worth is one of the two exceptions noted in the
    # header - it's already a per-household mean, no have%
    # adjustment needed, so pull it directly for comparison.
    net_worth_reported <- mean_dt[, .(year, Category,
        Net_Worth = Net_Worth * 1000
    )]
    wide_check <- merge(wide_check, net_worth_reported, by = id_cols)
    max_discrepancy <- max(abs(wide_check$implied_net_worth -
        wide_check$Net_Worth))
    if (max_discrepancy > 50) {
        stop(
            "SCF reconstruction identity check failed: implied net ",
            "worth (Assets - Debt, reconstructed) diverges from the ",
            "reported Net_Worth column by up to $", round(
                max_discrepancy
            ), ". Do not trust load_scf() output ",
            "until this is resolved - the mean-among-holders x ",
            "have% reconstruction may no longer hold for this file."
        )
    }

    dt[, group := .SCF_GROUP_LOOKUP[Category]]
    dt[is.na(group), group := Category]
    dt[, group := factor(group, levels = unique(c(SCF_GROUP_LEVELS, group)))]

    dt[, country  := "US"]
    dt[, currency := "USD"]
    dt[, source   :=
        "Federal Reserve Survey of Consumer Finances (Bulletin charts)"]

    setnames(dt, "value_2022usd", "value_real")
    dt <- deflate_from_2022(dt, "value_real", deflator, base_year)

    # Instalment_Loans should equal the sum of its Vehicle/Education/
    # Other sub-components, for every year and group - not just the
    # single (year, group) a caller happens to be looking at.
    inst_check <- dcast(
        dt[category %in% c(
            "Installment_Loans",
            "Vehicle_Installment_Loans",
            "Education_Installment_Loans",
            "Other_Installment_Loans"
        )],
        year + group ~ category,
        value.var = "value_real"
    )
    inst_check[, children_sum := Vehicle_Installment_Loans +
        Education_Installment_Loans +
        Other_Installment_Loans]
    inst_check[, discrepancy := abs(Installment_Loans - children_sum)]
    bad <- inst_check[discrepancy > 5]
    if (nrow(bad) > 0) {
        stop(
            "load_scf(): Installment_Loans ($",
            round(bad$Installment_Loans[1]), ") does not match the ",
            "sum of its Vehicle/Education/Other sub-components ($",
            round(bad$children_sum[1]), ") for ", nrow(bad),
            " year/group combination(s), e.g. year=", bad$year[1],
            ", group=", bad$group[1], ". Do not trust this output ",
            "until resolved."
        )
    }

    # Inner join: rows whose raw category has no entry in
    # .DEBT_CATEGORY_LABELS (Assets, Debt, Home_Secured_Debt) are
    # dropped here, not filtered - see the note above
    # .DEBT_CATEGORY_LABELS.
    dt <- merge(dt, .DEBT_CATEGORY_LABELS, by = "category")
    dt[, category := NULL]
    setnames(dt, "label", "category")

    dt[, .(
        country, year, group, category, sort_order, is_subtotal,
        value_real, currency, source
    )]
}

# Rebase a value already in constant 2022 USD to a different base
# year, using the US CPI series (load_cpi_us.R).
deflate_from_2022 <- function(dt, value_col, deflator, base_year) {
    dt <- as.data.table(dt)
    deflator <- as.data.table(deflator)

    index_2022 <- deflator[country == "US" & year == 2022, index]
    index_base <- deflator[country == "US" & year == base_year, index]
    if (length(index_2022) != 1 || length(index_base) != 1) {
        stop(
            "Need a US CPI index for both 2022 and base_year ",
            base_year, " to rebase SCF data."
        )
    }

    dt[, (value_col) := get(value_col) / index_2022 * index_base]
    dt[]
}
