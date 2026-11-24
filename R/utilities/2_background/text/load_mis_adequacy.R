# =====================================================================
# load_mis_adequacy.R
#
# Loads the MIS adequacy comparisons: disposable income as a
# proportion of the Minimum Income Standard requirement, by household
# type and work status, for Great Britain in 2025.
#
# Source: Loughborough CRSP / JRF, "MIS_tables_and_figures_2025.xlsx",
# sheets "Figure 5" to "Figure 10".
#
# critical: these figures are NET OF TAX and BENEFITS, computed by
# CRSP. Do not attempt to derive them by comparing gross National
# Living Wage earnings against the MIS budget in the annual budgets
# workbook: that ignores income tax, National Insurance, and
# Universal Credit, and for a lone parent the benefit component is
# most of the income. CRSP having already done this is the reason no
# tax-benefit microsimulation is needed here.
#
# =====================================================================

library(data.table)
library(readxl)

MIS_ADEQUACY_SHEETS <- c(
    "Figure 5"  = "1 adult, working age",
    "Figure 6"  = "2 adults, working age",
    "Figure 7"  = "1 adult, 2 children",
    "Figure 8"  = "2 adults, 2 children",
    "Figure 9"  = "1 adult, pensioner",
    "Figure 10" = "2 adults, pensioners"
)

.MIS_ROW_TOTAL      <- "Total weekly income"
.MIS_ROW_PROPORTION <- "Proportion of MIS"
.MIS_ROW_REQUIREMENT <- "^MIS requirement"

# Load MIS adequacy comparisons.
load_mis_adequacy <- function(mis_fig_xlsx_path,
                              sheets = MIS_ADEQUACY_SHEETS,
                              annualise = FALSE) {
    mult <- if (annualise) 52 else 1

    out <- rbindlist(lapply(names(sheets), function(sh) {
        raw <- suppressMessages(read_excel(
            mis_fig_xlsx_path,
            sheet = sh, col_names = FALSE
        ))
        setDT(raw)
        setnames(raw, paste0("V", seq_len(ncol(raw))))

        labels <- as.character(raw[[1]])

        row_of <- function(pattern, fixed = TRUE) {
            i <- grep(pattern, labels, fixed = fixed)
            if (length(i) != 1) {
                stop(
                    "Expected exactly one row matching '", pattern,
                    "' in sheet ", sh, "; found ", length(i),
                    ". The workbook layout may have changed."
                )
            }
            i
        }

        i_total <- row_of(.MIS_ROW_TOTAL)
        i_prop  <- row_of(.MIS_ROW_PROPORTION)
        i_req   <- row_of(.MIS_ROW_REQUIREMENT, fixed = FALSE)

        # The work-status header is the last blank-labelled row
        # above the total row.
        i_head <- max(which(is.na(labels) &
            seq_along(labels) < i_total))

        vals <- function(i) {
            v <- suppressWarnings(as.numeric(unlist(raw[i, -1])))
            v
        }

        statuses <- trimws(as.character(unlist(raw[i_head, -1])))
        total <- vals(i_total)
        prop  <- vals(i_prop)
        req   <- vals(i_req)

        keep <- !is.na(total) & !is.na(prop) & nzchar(statuses) &
            statuses != "NA"

        data.table(
            household_type  = unname(sheets[sh]),
            work_status     = statuses[keep],
            total_income    = total[keep] * mult,
            mis_requirement = req[keep] * mult,
            proportion_of_mis = prop[keep]
        )
    }), use.names = TRUE)

    # The proportion column should equal total / requirement. Check
    # rather than trust: if CRSP change the basis of one but not the
    # other, this catches it.
    out[, implied := total_income / mis_requirement]
    bad <- out[abs(implied - proportion_of_mis) > 0.01]
    if (nrow(bad) > 0) {
        warning(
            nrow(bad), " row(s) where the reported proportion ",
            "does not match total income over requirement. ",
            "Check the sheets before using these figures."
        )
    }
    out[, implied := NULL]

    out[]
}
