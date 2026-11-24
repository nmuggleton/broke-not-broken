# ==============================================================================
# 2_background/figure_2/main.R
#
# Figure 2 (Section 2.3): the cost of a basic standard of living against
# the incomes households actually receive, in the United States and
# Great Britain.
#
# Confidence intervals are cached to data/processed/. The bootstrap is the slow
# step (hundreds of replicates over PUMS's full retained row count per household
# type); once computed for a given boot_R, it is read back with fread() rather
# than rerun. Delete the CSV to force a recompute (e.g., after changing boot_R,
# or the underlying data).
# ==============================================================================

rm(list = ls())

source("R/utilities/2_background/fig2/load_epi.R")
source("R/utilities/2_background/fig2/load_pums.R")
source("R/utilities/2_background/fig2/load_county_pop.R")
source("R/utilities/_global/load_cpi_us.R")
source("R/utilities/2_background/fig2/load_hbai.R")
source("R/utilities/2_background/fig2/load_mis.R")
source("R/utilities/2_background/fig2/plot_below_standard.R")

library(patchwork)

base_year <- 2025
boot_R    <- 500L   # bootstrap replicates for the national CI

# ---------------------------------------------------------------------
# 0. Paths.
# ---------------------------------------------------------------------
pums_dir       <- "data/raw/pums"
pums_cache     <- "data/processed/pums_households.rds"
nat_ci_us_path <- "data/processed/national_ci_us.csv"
nat_ci_gb_path <- "data/processed/national_ci_gb.csv"

find1 <- function(pattern, what) {
    f <- list.files("data/raw",
        pattern = pattern, recursive = TRUE,
        full.names = TRUE
    )
    if (length(f) == 0) {
        stop(
            "No file matching ", what, " (", pattern,
            ") under data/raw/."
        )
    }
    if (length(f) > 1) {
        message("Several files match ", what, "; using ", f[1])
    }
    f[1]
}

epi_path  <- find1("fbc_data_.*\\.xlsx$",           "EPI budgets")
cpi_path  <- find1("CPIAUCSL\\.csv$",               "US CPI")
pop_path  <- find1("^co-est\\d{4}-alldata\\.csv$",  "county population")
hbai_path <- find1("^i2425e.*\\.tab$",              "HBAI")
mis_path  <- find1("Annual.?Budgets.*\\.xlsx$",     "MIS budgets")

pop_year <- sub(".*co-est(\\d{4}).*", "\\1", basename(pop_path))
pop_col  <- paste0("POPESTIMATE", pop_year)

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
dir.create("figures", showWarnings = FALSE)

# =====================================================================
# UNITED STATES
# =====================================================================

if (file.exists(pums_cache)) {
    pums <- readRDS(pums_cache)
} else {
    pums_files <- list.files(pums_dir,
        pattern = "^psam_h.*\\.csv$",
        full.names = TRUE
    )
    if (length(pums_files) == 0) {
        stop(
            "No psam_h*.csv in ", pums_dir,
            ". Run scripts/download_pums.sh --download first."
        )
    }
    pums <- load_pums(pums_files)
    saveRDS(pums, pums_cache)
}

cpi <- load_cpi_us(cpi_path)
factor_2024_2025 <- cpi[year == base_year, index] /
    cpi[year == 2024, index]
pums <- deflate_to_year(pums, factor_2024_2025)

epi_county <- load_epi(epi_path,
    base_year = base_year,
    level = "county"
)
county_pop <- load_county_pop(pop_path, pop_col = pop_col)
epi_state  <- aggregate_epi_to_state(epi_county, county_pop)


below_us <- share_below_standard(pums, epi_state)
setorder(below_us, state, household_type)
fwrite(below_us, "data/processed/share_below_standard_us.csv")

# Household-level merge, used for the pooled national figure --
# every household nationally, not summarised by state first.
epi_thresholds_us <- unique(epi_state[
    measure == "required_income",
    .(state, household_type, threshold = value_real)
])
dt_us <- merge(pums, epi_thresholds_us,
    by = c("state", "household_type")
)

nat_us <- national_share(dt_us)
fwrite(nat_us, "data/processed/national_share_us.csv")

if (file.exists(nat_ci_us_path)) {
    nat_ci_us <- fread(nat_ci_us_path)
} else {
    nat_ci_us <- bootstrap_national_ci(dt_us, R = boot_R)
    fwrite(nat_ci_us, nat_ci_us_path)
}

# =====================================================================
# GREAT BRITAIN
# =====================================================================

hbai <- load_hbai(hbai_path, housing_basis = "bhc")
mis  <- load_mis(mis_path, basis = "hbai_bhc")[year == base_year]

below_gb <- share_below_mis(hbai, mis, by = "region")
setorder(below_gb, region, household_type)
fwrite(below_gb, "data/processed/share_below_standard_gb.csv")

# MIS has no regional variation - one threshold per household type,
# applied to every GB household regardless of region.
mis_thresholds <- unique(
    mis[
        measure == "required_income",
        .(household_type, threshold = value_real)
    ]
)
dt_gb <- merge(hbai, mis_thresholds, by = "household_type")

nat_gb <- national_share(dt_gb)
fwrite(nat_gb, "data/processed/national_share_gb.csv")

if (file.exists(nat_ci_gb_path)) {
    nat_ci_gb <- fread(nat_ci_gb_path)
} else {
    nat_ci_gb <- bootstrap_national_ci(dt_gb, R = boot_R)
    fwrite(nat_ci_gb, nat_ci_gb_path)
}

# =====================================================================
# FIGURE
# =====================================================================

fig <- plot_below_standard_pair(
    us = list(
        below = below_us, national = nat_us,
        national_ci = nat_ci_us
    ),
    gb = list(
        below = below_gb, national = nat_gb,
        national_ci = nat_ci_gb
    )
)

ggsave(
    "figures/figure_2.pdf",
    fig,
    width  = 183,
    height = 75,
    units  = 'mm',
    device = cairo_pdf     # embeds the fonts
)
