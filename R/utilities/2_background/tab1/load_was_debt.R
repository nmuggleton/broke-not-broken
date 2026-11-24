# =====================================================================
# load_was_debt.R
#
# Loads non-mortgage household debt composition from the Wealth and
# Assets Survey (WAS), Round 8 (April 2020 - March 2022), End User
# Licence. GB counterpart to load_scf.R, for the debt-composition
# table (Table 1). Built from the household file only
# (was_round_8_hhold_eul_*); no person-level join is needed, since
# every debt component used here has a pre-aggregated household-level
# ONS derived variable.
#
#   CASER8            household ID ("Anonymised case for R8")
#   R8xshhwgt         cross-sectional household weight for Round 8
#   TotalWlthR8       Total Household Wealth
#   T_SLNSR8_aggr     Hhold value of loans from the Student Loans
#                     Company
#   T_bslnR8_aggr     Total student loans from a bank/building
#                     society, in payment or not yet started (HH
#                     level)
#   totcsc_persR8_aggr  Hhold sum of PERSISTENT credit/store/charge
#                     card debt (excludes transient, i.e.,
#                     paid-in-full, balances; not the same
#                     construct as SCF's undifferentiated credit
#                     card balance figure; see the note in
#                     write_debt_table.R / the paper's Table 1
#                     footnote)
#   TOTHPR8_aggr      Hhold value of hire purchase agreements.
#                     UNDIFFERENTIATED: WAS's own questionnaire
#                     allows a respondent to select multiple
#                     instalment-good types (car, home improvements,
#                     holiday, etc.) via a single multi-select
#                     question, but only records up to two
#                     agreement-level balances with no variable
#                     linking a specific balance back to a specific
#                     good-type. A vehicle-specific figure is
#                     therefore not constructible from this data
#                     without restricting to single-good, single-
#                     agreement households only, which would
#                     introduce a selection bias against the
#                     multi-debt households this table is about.
#   TOTMOR8_aggr      Hhold value of outstanding mail order accounts
#   DVHseDebtR8_sum   Total debt, houses not main residence
#   DVBLtDebtR8_sum   Total debt, buy-to-let houses
#   DVBldDebtR8_sum   Total debt, buildings
#   DVLUKDebtR8_sum   Total debt, UK land
#   DVLOSDebtR8_sum   Total debt, overseas land
#   DVOPrDebtR8_sum   Total debt, other property
#   T_LNSR8_aggr      Hhold value of all FORMAL loans, excluding SLC
#   T_FLNSR8_aggr     Hhold value of all INFORMAL loans (friends/
#                     family etc.)
#
# WEIGHT: R8xshhwgt is used as the cross-sectional household weight
# ("R8 XS round-based HH weight"). Two other weight variables exist
# in the same file - SurvWgtR2R8 ("Calibration Weight") and
# R7R8LongWgt (a longitudinal weight spanning two rounds) - and
# R8xshhwgt was chosen as the one whose own label states it is for a
# single-round cross-section, matching how this table is used
# (Round 8 only, not a panel).
#
# MISSING VALUES: the dictionary states "SPSS user missing values =
# -9.0 thru -1.0" for every variable checked. These are treated as
# missing (NA) on read, then set to 0 for the debt-component sums
# specifically: a negative code on e.g. DVBLtDebtR8_sum most often
# means "does not hold this asset type" rather than "refused to
# answer", so 0 is the economically correct value to sum.
#
# WEALTH BASIS: households are ranked by TotalWlthR8 and the bottom
# 25% (weighted) is retained, matching SCF's "Bottom 25%" nwcat
# bracket used for the US table - the closest available cut to a
# wealth quintile in either source.
# =====================================================================

library(data.table)

.WAS_MISSING_MIN <- -9
.WAS_MISSING_MAX <- -1

.WAS_DEBT_VARS <- c(
    "T_SLNSR8_aggr", "T_bslnR8_aggr", "totcsc_persR8_aggr",
    "TOTHPR8_aggr", "TOTMOR8_aggr",
    "DVHseDebtR8_sum", "DVBLtDebtR8_sum", "DVBldDebtR8_sum",
    "DVLUKDebtR8_sum", "DVLOSDebtR8_sum", "DVOPrDebtR8_sum",
    "T_LNSR8_aggr", "T_FLNSR8_aggr"
)

.WAS_REQUIRED_COLS <- c(
    "CASER8", "R8xshhwgt", "TotalWlthR8",
    .WAS_DEBT_VARS
)

# Recode WAS's -9..-1 missing-value range to NA.
.was_na <- function(x) {
    x <- as.numeric(x)
    x[x >= .WAS_MISSING_MIN & x <= .WAS_MISSING_MAX] <- NA_real_
    x
}

# Load WAS Round 8 non-mortgage debt composition for the bottom 25%
# of the wealth distribution.
load_was_debt <- function(was_hhold_path, bottom_pct = 0.25) {
    dt <- fread(was_hhold_path,
        select = .WAS_REQUIRED_COLS,
        showProgress = FALSE
    )

    missing_cols <- setdiff(.WAS_REQUIRED_COLS, names(dt))
    if (length(missing_cols) > 0) {
        stop(
            "load_was_debt(): expected columns not found: ",
            paste(missing_cols, collapse = ", "),
            ". Check the file matches the Round 8 household ",
            "dictionary this loader was built against."
        )
    }

    dt[, TotalWlthR8 := .was_na(TotalWlthR8)]
    dt[, R8xshhwgt := .was_na(R8xshhwgt)]
    for (v in .WAS_DEBT_VARS) {
        dt[, (v) := .was_na(get(v))]
        # Missing -> 0 for debt components specifically - see the
        # header note on why this is a deliberate, visible choice,
        # not a blanket "replace all NA with 0".
        dt[is.na(get(v)), (v) := 0]
    }

    n_before <- nrow(dt)
    dt <- dt[!is.na(TotalWlthR8) & !is.na(R8xshhwgt) & R8xshhwgt > 0]
    n_after <- nrow(dt)
    if (n_after < n_before) {
        message(
            "load_was_debt(): dropped ", n_before - n_after,
            " of ", n_before, " households with missing wealth ",
            "or weight."
        )
    }
    if (n_after == 0) {
        stop(
            "load_was_debt(): no households remain after dropping ",
            "missing wealth/weight - check TotalWlthR8 and ",
            "R8xshhwgt are being read correctly."
        )
    }

    # Weighted bottom_pct cut on TotalWlthR8.
    setorder(dt, TotalWlthR8)
    dt[, cum_weight := cumsum(R8xshhwgt) / sum(R8xshhwgt)]
    cutoff <- dt[cum_weight >= bottom_pct, TotalWlthR8][1]
    bottom <- dt[TotalWlthR8 <= cutoff]

    if (nrow(bottom) < 30) {
        warning(
            "load_was_debt(): only ", nrow(bottom), " households ",
            "in the bottom ", bottom_pct * 100, "%; estimates ",
            "will be noisy. Check the wealth variable and cutoff ",
            "before trusting this table."
        )
    }

    w <- bottom$R8xshhwgt
    wmean <- function(x) sum(x * w) / sum(w)

    property_debt <- wmean(
        bottom$DVHseDebtR8_sum + bottom$DVBLtDebtR8_sum +
            bottom$DVBldDebtR8_sum + bottom$DVLUKDebtR8_sum +
            bottom$DVLOSDebtR8_sum + bottom$DVOPrDebtR8_sum
    )

    out <- data.table(
        category = c(
            "Student Loans Company", "Bank/building society loan",
            "Credit/store/charge card (persistent)",
            "Hire purchase and instalment credit",
            "Mail order",
            "Other residential real estate debt",
            "Other loans (formal)", "Other loans (informal)"
        ),
        sort_order = 1:8,
        # Every WAS category here is a leaf row: unlike SCF's
        # Instalment Loans (a parent whose Vehicle/Education/Other
        # children must not also be summed, to avoid double
        # counting), WAS has no such nesting, so every row
        # contributes to the total.
        is_subtotal = TRUE,
        value_real = c(
            wmean(bottom$T_SLNSR8_aggr), wmean(bottom$T_bslnR8_aggr),
            wmean(bottom$totcsc_persR8_aggr),
            wmean(bottom$TOTHPR8_aggr), wmean(bottom$TOTMOR8_aggr),
            property_debt,
            wmean(bottom$T_LNSR8_aggr), wmean(bottom$T_FLNSR8_aggr)
        ),
        n_households = nrow(bottom),
        country  = "GB",
        currency = "GBP",
        source   = "Wealth and Assets Survey, Round 8 (EUL)"
    )

    out[]
}
