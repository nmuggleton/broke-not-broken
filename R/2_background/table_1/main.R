# ==============================================================================
# 2_background/table_1/main.R
#
# Table 1. Composition of non-mortgage household debt, bottom 25% of the
# net worth distribution, one table with two panels (United States and Great
# Britain). Per household, each country in its own currency. Mortgage debt
# excluded throughout; secured debt on non-primary residential property is
# retained, since it is investment debt rather than owner-occupied housing
# debt. Sources: Federal Reserve Survey of Consumer Finances (US); Wealth and
# Assets Survey, Round 8 (GB).
# ==============================================================================

rm(list = ls())

source('R/utilities/_global/load_cpi_us.R')
source("R/utilities/2_background/tab1/load_scf.R")
source("R/utilities/2_background/tab1/load_was_debt.R")
source("R/utilities/2_background/tab1/write_debt_table.R")

# ---------------------------------------------------------------------
# 1. Deflator. US only: real BLS CPI-U (annual average). See
# load_cpi_us.R. GB's WAS-derived debt variables are not deflated.
# ---------------------------------------------------------------------

us_deflator <- load_cpi_us("data/raw/fred/CPIAUCSL.csv")

# ---------------------------------------------------------------------
# 5. Table 1: debt composition, lowest SCF wealth bracket (Bottom 25%)
#    and lowest 25% percentile for WAS in GB.
# ---------------------------------------------------------------------

us_dt <- load_scf(
    scf_mean_path = "data/raw/interactive_bulletin_charts_nwcat_mean.csv",
    scf_have_path = "data/raw/interactive_bulletin_charts_nwcat_have.csv",
    deflator      = us_deflator,
    base_year     = 2022
)
us_dt <- us_dt[group == "Bottom 25%" & year == max(year)]

gb_dt <- load_was_debt(
    was_hhold_path = "data/raw/was_round_8_hhold_eul_may_2025_230525.tab"
)

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
fwrite(
    rbindlist(list(us_dt, gb_dt), fill = TRUE),
    "data/processed/debt_composition.csv"
)

dir.create("tables", showWarnings = FALSE)
write_debt_table(
    us_dt, gb_dt,
    out_path = "tables/debt_composition.tex",
    caption  = paste0(
        "\\textbf{Composition of non-mortgage household debt, bottom ",
        "25\\% of the net worth distribution}. Per household. Mortgage ",
        "debt excluded throughout; secured debt on non-primary ",
        "residential property is retained, since it is investment debt ",
        "rather than owner-occupied housing debt. United States: ",
        us_dt$year[1], ", Federal Reserve Survey of Consumer Finances. ",
        "Great Britain: 2020-2022, Wealth and Assets Survey, Round 8."
    )
)

