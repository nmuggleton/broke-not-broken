# =====================================================================
# load_mis.R
#
# Loads the Minimum Income Standard annual budgets workbook: what a
# household needs for a minimum socially acceptable standard of
# living in Great Britain, by household type and category, 2008 to
# 2025. Supplies the GB side of Section 2.3.
#
# Source: Loughborough CRSP / JRF, "Annual_Budgets_2025.xlsx".
# =====================================================================

source("R/utilities/2_background/fig2/schema_budget.R")
library(data.table)
library(readxl)

.MIS_CATEGORY_ROWS <- 4:20
.MIS_YEAR_ROW      <- 3

# Total rows, confirmed against the real workbook. Row 22 is the
# full budget; rows 26 and 27 are the ones CRSP provide explicitly
# for comparison with HBAI, and are what should be used when joining
# to HBAI income. Row 27 (excluding council tax and childcare) is
# the before-housing-costs comparator; row 26 additionally excludes
# rent and water rates and is the after-housing-costs comparator.
# Using row 22 against HBAI income is close but not the comparison
# CRSP intend, since HBAI income is not net of council tax.
.MIS_TOTAL_ROWS <- c(
    "total"  = 22,   # Total (including rent & childcare)
    "hbai_bhc" = 27, # comparable to before housing costs in HBAI
    "hbai_ahc" = 26  # comparable to after housing costs in HBAI
)

# Load MIS budgets from the annual budgets workbook.
load_mis <- function(mis_xlsx_path,
                     annualise = TRUE,
                     basis = c("hbai_bhc", "hbai_ahc", "total")) {
    sheets <- c(
        "s"      = "Single, working age",
        "c"      = "Couple, working age",
        "pens s" = "Single pensioner",
        "pens c" = "Couple pensioner",
        "lp+1"   = "Lone parent, 1 child",
        "lp+2"   = "Lone parent, 2 children",
        "lp+3"   = "Lone parent, 3 children",
        "c+1"    = "Couple, 1 child",
        "c+2"    = "Couple, 2 children",
        "c+3"    = "Couple, 3 children",
        "c+4"    = "Couple, 4 children"
    )


    basis <- match.arg(basis)
    total_row <- unname(.MIS_TOTAL_ROWS[basis])
    mult <- if (annualise) 52 else 1

    out <- rbindlist(lapply(names(sheets), function(sh) {
        raw <- suppressMessages(read_excel(
            mis_xlsx_path,
            sheet = sh, col_names = FALSE
        ))
        setDT(raw)

        years <- suppressWarnings(
            as.integer(unlist(raw[.MIS_YEAR_ROW, -1, with = FALSE]))
        )
        keep <- which(!is.na(years))
        years <- years[keep]

        grab <- function(rows) {
            block <- raw[rows, c(1, keep + 1), with = FALSE]
            setnames(block, c("category", as.character(years)))
            block[, category := as.character(category)]

            ycols <- as.character(years)
            block[, (ycols) := lapply(.SD, as.character),
                .SDcols = ycols
            ]

            long <- melt(block,
                id.vars = "category",
                variable.name = "year",
                value.name = "value_real"
            )
            long[, year := as.integer(as.character(year))]
            # suppressWarnings: the workbook's label rows coerce to
            # NA here, which is expected and filtered on the next
            # line. Left unsuppressed they produce 22 warnings per
            # load and would mask a real coercion problem.
            long[, value_real := suppressWarnings(
                as.numeric(value_real)
            ) * mult]
            long[!is.na(value_real)]
        }

        basket <- grab(.MIS_CATEGORY_ROWS)
        basket[, measure := "basket"]

        total <- grab(total_row)
        total[, measure := "required_income"]
        total[, category := NA_character_]

        res <- rbindlist(list(basket, total), use.names = TRUE)
        res[, household_type := unname(sheets[sh])]
        res[]
    }), use.names = TRUE)

    out[, country  := "GB"]
    out[, currency := "GBP"]
    out[, source   := "Minimum Income Standard (Loughborough/JRF)"]
    attr(out, "mis_basis") <- basis

    # Drop categories that are zero in every year for a household
    # type (tobacco, and motoring in the early years) so they do not
    # clutter a stacked figure. They remain zero rows in the source.
    out <- out[!(measure == "basket" &
        ave(value_real, household_type, category,
            FUN = function(x) all(x == 0)
        ) == 1)]

    res <- out[, c(BUDGET_SCHEMA, "year"), with = FALSE]
    validate_budget(res)
}


# =====================================================================
# Regrouping MIS's fine-grained basket categories into EPI-style
# buckets. MIS reports roughly
# 14 categories, EPI 7; without regrouping, the two countries' rows
# don't line up and small GB line items sit unhelpfully beside large
# bundled US ones.
#
# The mapping is built from two primary sources:
#   - EPI's own stated definition of "Other necessities": apparel,
#     personal care, household supplies (furnishings, operations,
#     telephone), reading materials, school supplies. EPI's
#     technical documentation states that entertainment
#     and other miscellaneous spending were dropped
#     from this bucket after the 2018 update, specifically to keep
#     the calculator as narrow as possible (EPI FBC Technical
#     Documentation, 2026).
#   - MIS's own category structure, confirmed against a published
#     itemised budget (Loughborough CRSP, single working-age adult,
#     Aug 2026): Council tax, Water rates, Fuel and Household
#     insurances sit under MIS's own "D: Housing Costs"; items that
#     would be healthcare in the US sense (opticians, NHS dental
#     charges, over-the-counter medicines) are folded into "F:
#     Personal goods and services, including health", with no
#     separate health-only line at all; "H: Social and cultural
#     participation" includes broadband internet, TV licence,
#     leisure activities, holidays, and gifts.
#
# =====================================================================

.GB_CATEGORY_REGROUP <- c(
    "Rent"                              = "Housing",
    "Council tax"                       = "Housing",
    "Water rates"                       = "Housing",
    "Fuel"                              = "Housing",
    "Household insurances"              = "Housing",
    "Other housing costs"               = "Housing",
    "Food"                              = "Food",
    "Childcare"                         = "Childcare",
    "Motoring"                          = "Transportation",
    "Other travel costs"                = "Transportation",
    "Clothing"                          = "Other necessities",
    "Personal goods and services"       = "Other necessities",
    "Household goods"                   = "Other necessities",
    "Household services"                = "Other necessities",
    "Alcohol"                           = "Food",
    "Social and cultural participation" = "Social and cultural participation"
)

# Regroup MIS's fine-grained basket categories into EPI-style
# buckets, so the two countries' baskets line up.
regroup_gb_categories <- function(mis_dt) {
    mis_dt <- as.data.table(copy(mis_dt))

    basket <- mis_dt[measure == "basket"]
    other  <- mis_dt[measure != "basket"]

    unmapped <- setdiff(
        unique(basket$category),
        names(.GB_CATEGORY_REGROUP)
    )
    if (length(unmapped) > 0) {
        stop(
            "regroup_gb_categories(): no mapping for categor",
            if (length(unmapped) > 1) "ies " else "y ",
            paste(unmapped, collapse = ", "),
            ". Add to .GB_CATEGORY_REGROUP before regrouping ",
            "-- do not guess a bucket silently."
        )
    }

    basket[, category := .GB_CATEGORY_REGROUP[category]]

    basket_grouped <- basket[, .(
        value_real = sum(value_real)
    ), by = .(
        country, household_type, measure, category, currency,
        source
    )]

    rbindlist(list(basket_grouped, other), use.names = TRUE)
}
