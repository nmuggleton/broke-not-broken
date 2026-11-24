# =====================================================================
# load_epi.R
#
# Loads EPI's Family Budget Calculator workbook and standardises it
# to the schema in schema_budget.R. Supplies the US side of Section
# 3.1: what a basic standard of living costs, by county or metro
# area and by family type.
#
# Source: epi.org/resources/budget, "fbc_data_2026.xlsx"
#
# median_family_income is the ACS 2019-2023 five-year median for the
# whole AREA, not for the family type in the row: it is identical
# across all ten family types in a county. It is therefore emitted
# as measure == "area_median_income".
# =====================================================================

library(data.table)
library(readxl)

# Column positions in the annual block (1-indexed), County sheet.
.EPI_COUNTY_ANNUAL <- 14:20
.EPI_COUNTY_ANNUAL_TOTAL <- 21
.EPI_METRO_ANNUAL <- 13:19
.EPI_METRO_ANNUAL_TOTAL <- 20

.EPI_CATEGORIES <- c(
    "Housing", "Food", "Transportation", "Healthcare",
    "Other necessities", "Childcare", "Taxes"
)

# Expand an EPI family code ("1p2c") into a readable label.
epi_family_label <- function(code) {
    parents  <- as.integer(sub("^(\\d+)p.*$", "\\1", code))
    children <- as.integer(sub("^.*p(\\d+)c$", "\\1", code))

    adult_part <- fifelse(parents == 1, "1 adult", "2 adults")
    child_part <- fifelse(
        children == 0, "",
        fifelse(
            children == 1, ", 1 child",
            paste0(", ", children, " children")
        )
    )
    paste0(adult_part, child_part)
}

# Load EPI Family Budget Calculator data.
load_epi <- function(epi_xlsx_path,
                     base_year,
                     level = c("county", "metro"),
                     areas = NULL,
                     family_codes = NULL) {
    source("R/utilities/2_background/fig2/schema_budget.R")

    level <- match.arg(level)

    if (base_year != 2025) {
        stop(
            "EPI figures are in 2025 dollars and this loader does ",
            "not rebase them. base_year is ", base_year,
            "; either set it to 2025 or add a rebasing step."
        )
    }

    sheet <- if (level == "county") "County" else "Metro"

    # skip = 1 drops the block-label row so the real names in row 2
    # become the header - see the note in the file header.
    raw <- suppressMessages(read_excel(
        epi_xlsx_path,
        sheet = sheet, skip = 1
    ))
    setDT(raw)

    area_col <- if (level == "county") "County" else "Areaname"
    ann_cols <- if (level == "county") .EPI_COUNTY_ANNUAL else
        .EPI_METRO_ANNUAL
    tot_col  <- if (level == "county") .EPI_COUNTY_ANNUAL_TOTAL else
        .EPI_METRO_ANNUAL_TOTAL
    inc_col  <- tot_col + 1

    keep <- data.table(
        area   = raw[[area_col]],
        state  = raw[["State abv."]],
        family = raw[["Family"]]
    )

    # county_fips is carried through so aggregation to state level
    # can join on FIPS rather than on county name: names repeat
    # across states (there are 34 Washington Counties), and spelling
    # differs between sources ("St." vs "Saint"). EPI stores it as a
    # number, so 1001 must be padded back to "01001".
    if (level == "county") {
        keep[, county_fips := sprintf(
            "%05d",
            as.integer(raw[["county_fips"]])
        )]
    } else {
        keep[, county_fips := NA_character_]
    }
    for (i in seq_along(ann_cols)) {
        keep[, (.EPI_CATEGORIES[i]) := raw[[ann_cols[i]]]]
    }
    keep[, total_required := raw[[tot_col]]]
    keep[, median_income  := raw[[inc_col]]]

    if (!is.null(areas)) keep <- keep[area %in% areas]
    if (!is.null(family_codes)) keep <- keep[family %in% family_codes]
    if (nrow(keep) == 0) {
        stop(
            "No rows left after filtering. Check `areas` and ",
            "`family_codes` against the values actually in the file."
        )
    }

    keep[, household_type := epi_family_label(family)]

    basket <- melt(
        keep,
        id.vars = c(
            "area", "state", "county_fips", "family",
            "household_type"
        ),
        measure.vars = .EPI_CATEGORIES,
        variable.name = "category", value.name = "value_real"
    )
    basket[, measure := "basket"]
    basket[, category := as.character(category)]

    incomes <- rbindlist(list(
        keep[, .(area, state, county_fips, family, household_type,
            measure = "required_income",
            category = NA_character_,
            value_real = total_required
        )],
        keep[, .(area, state, county_fips, family, household_type,
            measure = "area_median_income",
            category = NA_character_,
            value_real = median_income
        )]
    ))

    dt <- rbindlist(list(basket, incomes), use.names = TRUE)
    dt[, country  := "US"]
    dt[, currency := "USD"]
    dt[, source   := "EPI Family Budget Calculator (2026 edition)"]

    # area and state are kept alongside the schema columns: the
    # schema is the minimum, and 3.1's geography paragraph needs
    # them. validate_budget() checks for presence, not exclusivity.
    dt <- dt[, c(BUDGET_SCHEMA, "area", "state", "county_fips"),
        with = FALSE
    ]
    validate_budget(dt)
}
