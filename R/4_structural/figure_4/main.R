# ==============================================================================
# 4_structural/figure_4/main.R
#
# Figure 4 (Section 4.1). Patience and income as predictors of saving and
# financial hardship within countries. Panel A: having saved money in
# the past year, 14 countries and pooled (the saving outcome of Falk
# et al. 2018, Table 7, under the same specification as panel B).
# Panel B: three hardship
# outcomes pooled over 70 to 72 countries. Each coefficient is the
# difference in probability, in percentage points, for one SD more
# patience or for the richest against the poorest income fifth, from
# a model that holds the other fixed. Panel C: mean patience against
# mean income by country, with each country's own slope.
#
# The models are estimated in R/4_structural/text/main.R
# (Steps 3, 4, and 6), which writes data/processed/within_country_results.csv
# and data/processed/within_country_step3_countries.csv. This script
# only draws. Source: Global Preference Survey and Gallup
# World Poll.
# ==============================================================================

rm(list = ls())

source("R/utilities/4_structural/fig4/plot_explanatory_power.R")

# ------------------------------------------------------------------------------
# 1. Load the estimates (step, model, term, estimate, lo, hi, outcome,
# country) and the country means and slopes (country, patience,
# log_inc, slope, pop2012, w_pop).
# ------------------------------------------------------------------------------
results   <- fread("data/processed/within_country_results.csv")
countries <- fread("data/processed/within_country_step3_countries.csv")

# ------------------------------------------------------------------------------
# 2. Plot and save into figures/.
# ------------------------------------------------------------------------------
p <- plot_explanatory_power(results, countries)
dir.create("figures", showWarnings = FALSE)
suppressWarnings(ggsave(
    "figures/figure_4.pdf",
    p,
    width  = 183,
    height = 120,
    units  = 'mm',
    device = cairo_pdf     # embeds the fonts
))
