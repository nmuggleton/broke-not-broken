# ==============================================================================
# 2_background/figure_1/main.R
#
# Figure 1. Average net wealth by wealth quintile, United States and
# Great Britain. Net personal wealth per adult on an equal-split basis,
# comprising all assets minus all liabilities, expressed in constant 2025
# prices. The vertical axis uses a pseudo-logarithmic transformation to render
# both quintiles legible on a common scale. United States estimates span 1980 to
# 2024. Great Britain estimates begin in 1995. Net wealth includes
# housing, pension, and business assets alongside financial holdings, a broader
# concept than savings net of debt. Source: World Inequality Database.
# ==============================================================================

rm(list = ls())

source('R/utilities/_global/load_cpi_us.R')
source('R/utilities/2_background/load_cpih_gb.R')
source("R/utilities/2_background/fig1/load_wid.R")
source("R/utilities/2_background/fig1/plot_net_assets.R")

# ------------------------------------------------------------------------------
# 1. Data source: WID. One consistent source for both countries, with
# matched quintiles and long coverage (US 1980-2024, GB 1995-2024). Its net
# wealth is broader than savings minus debt: it includes housing, pension and
# business equity, which matters most at the top of the distribution.
# ------------------------------------------------------------------------------

base_year <- 2025

# ------------------------------------------------------------------------------
# 2. Deflators. US: real BLS CPI-U (annual average). GB: real ONS CPIH (annual
# average). See load_cpi_us.R / load_cpih_gb.R.
# ------------------------------------------------------------------------------

us_deflator <- load_cpi_us("data/raw/fred/CPIAUCSL.csv")
gb_deflator <- load_cpih_gb("data/raw/ons/CPIH_L522.csv")

# ---------------------------------------------------------------------
# 3. Load, according to the choice above. Paper currently uses 'wid'
# ---------------------------------------------------------------------
net_assets_data <- load_wid(
    deflator  = rbindlist(list(us_deflator, gb_deflator)),
    base_year = base_year,
    years = 1980:2024,
    perc  = c("p0p20", "p80p100"),
    median = TRUE,
)

# ------------------------------------------------------------------------------
# 4. Plot and save into figures/.
# ------------------------------------------------------------------------------
p <- plot_net_assets(net_assets_data, base_year = base_year)
p
dir.create("figures", showWarnings = FALSE)
ggsave(
    "figures/figure_1.pdf",
    p,
    width  = 89,
    height = 50,
    units  = 'mm',
    device = cairo_pdf     # embeds the fonts
)
p
