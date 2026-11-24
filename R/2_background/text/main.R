# =====================================================================
# 2_background/text/main.R
#
# Numbers cited in the prose of Section 2.3 that don't correspond to a
# figure or table of their own:
#
#   1. Composition gaps - how the pooled national share below
#      standard moves between one and two adults, per additional
#      child, and (GB only) between working-age and pensioner
#      households. Reads the national_share_* output of
#      2_background/figure_2/main.R rather than recomputing it. Run
#      that script first.
#   2. MIS adequacy - the claim that a household working full time at
#      the legal minimum wage still falls short of MIS, e.g., "a single
#      working-age adult reaches 76 per cent of the requirement, a
#      lone parent with two children 69 per cent". See
#      utilities/2_background/text/load_mis_adequacy.R for why this needs no
#      tax-benefit microsimulation of its own: CRSP have already done
#      it, net of tax and benefits.
#
# Both write a CSV to data/processed/ (for the numbers to be checked
# against the manuscript) and print a summary to the console. Neither
# is sourced by any figure or table script - this is a citation aid,
# not part of the build for any figure or table.
# =====================================================================

rm(list = ls())

source("R/utilities/2_background/text/composition_gaps.R")
source("R/utilities/2_background/text/load_mis_adequacy.R")

library(data.table)

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)

# =====================================================================
# 1. Composition gaps.
# =====================================================================

nat_us_path <- "data/processed/national_share_us.csv"
nat_gb_path <- "data/processed/national_share_gb.csv"

if (!file.exists(nat_us_path) || !file.exists(nat_gb_path)) {
    stop(
        "Missing ", nat_us_path, " and/or ", nat_gb_path,
        ". Run R/2_background/figure_2/main.R first - this script ",
        "reads its cached national-share output."
    )
}

nat_us <- fread(nat_us_path)
nat_gb <- fread(nat_gb_path)

gaps_us <- compute_composition_gaps(nat_us, has_pensioners = FALSE)
gaps_gb <- compute_composition_gaps(nat_gb, has_pensioners = TRUE)

fwrite(gaps_us$adult_gap, "data/processed/in_text_adult_gap_us.csv")
fwrite(gaps_us$child_slope, "data/processed/in_text_child_slope_us.csv")
fwrite(gaps_gb$adult_gap, "data/processed/in_text_adult_gap_gb.csv")
fwrite(gaps_gb$child_slope, "data/processed/in_text_child_slope_gb.csv")
fwrite(gaps_gb$pensioner_gap, "data/processed/in_text_pensioner_gap_gb.csv")
fwrite(
    gaps_gb$pensioner_slope,
    "data/processed/in_text_pensioner_slope_gb.csv"
)

message("US, one vs two adults (share below standard):")
print(gaps_us$adult_gap)
message("GB, one vs two adults, working age (share below standard):")
print(gaps_gb$adult_gap)
message("GB, working-age vs pensioner, childless (share below standard):")
print(gaps_gb$pensioner_gap)

# =====================================================================
# 2. MIS adequacy: full-time minimum-wage income against the MIS
# requirement, by household type and work status.
# =====================================================================

mis_fig_path <- list.files(
    "data/raw",
    pattern = "MIS.*tables.*figures.*\\.xlsx$",
    recursive = TRUE,
    full.names = TRUE
)[1]

adequacy <- load_mis_adequacy(mis_fig_path)
fwrite(adequacy, "data/processed/in_text_mis_adequacy.csv")

message("MIS adequacy (proportion of requirement met by income):")
print(adequacy[, .(household_type, work_status, proportion_of_mis)])
