# =====================================================================
# load_scf_credit_price.R
#
# Loads Survey of Consumer Finances 2022 MICRODATA (not the Bulletin
# chart extracts used by load_scf.R) and constructs household-level
# measures of the price of unsecured credit, by position in the
# income distribution.
#
# Four files are needed, all from
# federalreserve.gov/econres/scfindex.htm:
#   p22i6.dta           full public dataset, 5 implicates
#   rscfp2022.dta       summary extract (Fed-defined aggregates)
#   p22_rw1.dta         999 bootstrap replicate weights
#   bulletin.macro.txt  the Fed's own SAS macro defining the
#                       aggregates. Not read by this code, but the
#                       definitions replicated below are taken from
#                       it verbatim, so keep it alongside the data.
#
# ---------------------------------------------------------------------
# UNIT and IMPLICATE STRUCTURE
# ---------------------------------------------------------------------
# The SCF unit is the "primary economic unit" (PEU), called a family
# here. Each PEU appears FIVE times, once per multiple-imputation
# implicate. Identifiers:
#   YY1   household (PEU) id, constant across implicates
#   Y1    implicate id, equal to YY1 * 10 + implicate number
# so implicate = Y1 %% 10 and YY1 = Y1 %/% 10. verified against the
# replicate-weight file, which is keyed on YY1 with Y1 = YY1*10 + 1.
#
# ---------------------------------------------------------------------
# WEIGHTS - READ THIS before CHANGING ANYTHING
# ---------------------------------------------------------------------
#   X42001   the analysis weight, on the full-population scale. Sums
#            to 131,306,391 US families within each implicate.
#   wgt      the same weight in rscfp2022, already divided by 5 so
#            that stacking all five implicates reproduces the
#            population total. not used here: we use X42001 and keep
#            the implicates separate, because Rubin's rules need the
#            five implicate estimates individually.
#   wt1b1 .. wt1b999   replicate base weights, in p22_rw1.dta
#   mm1   .. mm999     replicate multiplicities (the number of times
#                      the household was drawn in that replicate)
#
# critical: the replicate analysis weight is wt1b{r} * mm{r}, not
# wt1b{r} on its own. wt1b{r} alone sums to only 92-99 million across
# replicates - roughly 70% of the population - and the shortfall
# varies by replicate, so using it distorts the sampling variance.
# The product wt1b{r} * mm{r} reproduces 131,306,391 in every
# replicate. Both facts are asserted below and error out.
# Households not drawn in a given replicate have wt1b{r} and mm{r}
# stored as MISSING, not zero; both are set to 0 on read.
#
# ---------------------------------------------------------------------
# INCOME (for the decile ranking)
# ---------------------------------------------------------------------
#   income   rscfp2022's total family income before TAX for the
#            calendar year preceding the interview (2021 for the 2022
#            wave), including transfers. GROSS. Note for the GB
#            panel: WAS's equivalent derived variable is NET, so the
#            two panels' deciles are not the same income concept.
#            Reported, not harmonised away.
#   houses   value of the primary residence, needed only to replicate
#            the Fed's OTHLOC fallback branch (below).
#
# =====================================================================
# WHAT IS and IS not CONSTRUCTIBLE FROM THE PUBLIC FILE
# =====================================================================
# The Fed's macro classifies the "other loan" grid using the RAW loan
# purpose codes (e.g. 83 = education, 67 = vacation property, 78 =
# investment real estate). THOSE CODES DO not EXIST IN THE PUBLIC
# DATA. The public file collapses the master loan purpose list into
# ten buckets, so X2710 and its five siblings take values 0-10 only
# (verified: no value of 83, 67 or 78 appears in any of the six).
#
# Two consequences, both load-bearing for this table:
#
#   1. OTH_INST as distributed in rscfp2022 cannot be reproduced from
#      the public file. Its definition is
#        OTH_INST = INSTALL - VEH_INST - EDN_INST
#      and both INSTALL and EDN_INST contain purpose-coded terms that
#      evaluate to zero on public data. This loader therefore does
#      not attempt to match OTH_INST, and no reconciliation check is
#      made against it. Anyone trying to rebuild the Fed's figure
#      from the public file will get a different number; that is a
#      property of the data release, not an error here.
#
#   2. EDN_INST reconciles anyway, because its purpose-coded
#      term (X2723*(X2710=83) and siblings) is identically zero on
#      public data. The loader asserts that no purpose code equals 83
#      so that this coincidence is checked rather than assumed.
#
# The collapsed buckets, from the codebook's public-data recode:
#    1  own home purchase/construction; cottage/vacation property
#    2  home improvements or additions; home repairs/maintenance
#    3  car; truck/jeep/utility vehicle
#    4  appliances and furniture
#    5  electronics, hobby, recreation items, power tools
#    6  boat, airplane; motorcycle, bicycle, moped; camper, RV
#    7  invest in own business; business investment; other asset
#       investment; investment real estate; to have cash reserve
#    8  divorce/separation; travel/vacation; weddings/funerals;
#       moving; other special expenses
#    9  medical/dental/veterinary/attorney's fees; EDUCATION/school
#   10  tax and insurance; legal judgment; "personal loan"; bill or
#       debt consolidation; personal items; vehicle repair; gifts;
#       living/general expenses; loans to others; contributions
#
# Bucket 9 fuses MEDICAL and EDUCATION expenses. They cannot be
# separated on public data. That matters for this paper, because
# medical borrowing is market-priced and belongs in a price-of-credit
# measure while federal education debt is priced by statute and does
# not. The measure that excludes bucket 9 therefore excludes medical
# borrowing too - a real gap, stated rather than hidden.
#
# =====================================================================
# three NESTED RATE MEASURES
# =====================================================================
# Each is a balance-weighted mean of component APRs,
#     r = sum_j (balance_j * rate_j) / sum_j balance_j
# over components with a POSITIVE balance and an OBSERVED rate. A
# component with a balance but no rate is dropped from numerator and
# denominator alike, so r is the price of the balance whose price we
# can see. The share of balance that is priced is reported by decile
# (`priced_share_*`) so the coverage is visible.
#
#   r_cardloc   PRIMARY. Credit cards plus non-HELOC lines of credit.
#               Both components are unambiguously unsecured,
#               unambiguously market-priced, carry a directly
#               observed APR, and reconcile to the Fed's own
#               CCBAL and OTHLOC aggregates. This is the revolving
#               consumer credit the paper's argument concerns.
#   r_consumer  r_cardloc plus other-loan-grid loans in buckets 4, 5,
#               8 and 10 - the buckets that are clearly unsecured
#               consumer borrowing. Excludes 1, 2 (property and home
#               improvement, commonly secured), 3, 6 (vehicle-
#               secured), 7 (business and investment), and 9
#               (medical fused with education).
#   r_all       r_consumer plus bucket 9 plus the education loan
#               grid: every unsecured balance we can price,
#               including statute-priced student debt. Reported to
#               show how much the federal rate pulls the average
#               down at the bottom of the distribution.
#
# ---------------------------------------------------------------------
# BALANCES
# ---------------------------------------------------------------------
# Credit cards. CCBAL, per the Fed's macro for 2016 onward:
#   CCBAL = MAX(0,X427)+MAX(0,X413)+MAX(0,X421)+MAX(0,X7575)
#   X413   credit cards: total balance still owed after last payment
#   X421   store-branded cards: same question
#   X427   charge cards: same question
#   X7575  STORE CHARGE ACCOUNTS - a separate product, not a
#          mop-up of the three card groups. Gated on X7577: "Other
#          than the store accounts where you have credit cards, do
#          you ... have any charge or revolving charge accounts at
#          stores where you owed money after your last payment?"
#          (X7576 = number of such accounts). X7575 is "the balance
#          still owed on all these accounts". No rate is asked.
#          Scale: 3.8% of card balance nationally, 11-17 sample
#          households per income decile, median holder balance
#          $1,000. Two households (yy1 234, $49,000; yy1 1341,
#          $33-34k) drive the wide priced_share intervals in income
#          deciles 4 and 2. Returned separately as bal_store_acct.
#   X7132  "What interest rate do you pay on the card where you have
#          the largest balance?" A SINGLE rate for the whole card
#          portfolio - the SCF does not collect a rate per card, so
#          this one rate is applied to X413+X421+X427. This is the
#          largest approximation in the construction and is stated in
#          the table note. X7132 is asked only where there is a
#          bank-type credit card (group #1; Inap = "no cards of
#          type"), so a family whose only cards are store-branded or
#          charge cards has a card balance but NO rate. That is
#          0.64% of card balance, but it sits at the bottom: 10
#          sample households in income decile 1, none in decile 10.
#
# Lines of credit. OTHLOC, replicating the Fed's macro,
# including the pro-rating of the mop-up total and the fallback
# branch. HELOCs are excluded - they are secured, and the Fed puts
# them in MRTHEL:
#   IF (X1108+X1119+X1130) >= 1 THEN
#     OTHLOC = X1108*(X1103^=1) + X1119*(X1114^=1) + X1130*(X1125^=1)
#              + MAX(0,X1136) * (non-HELOC balances)/(all balances)
#   ELSE
#     OTHLOC = ((HOUSES<=0) + 0.5*(HOUSES>0)) * MAX(0,X1136)
#   X1103 X1114 X1125   HELOC flag for line 1/2/3 (1 = secured on the
#                       primary residence). Grid stride is 11.
#   X1108 X1119 X1130   amount currently owed, line 1/2/3
#   X1111 X1122 X1133   interest rate, line 1/2/3
#   X1136               "the total amount ... on all other remaining
#                       lines of credit" (#4). no rate.
# Only non-HELOC lines contribute to the rate.
#
# Other loan grid (six loans, classified by collapsed purpose):
#   X2723 X2740 X2823 X2840 X2923 X2940   amount still owed
#   X2724 X2741 X2824 X2841 X2924 X2941   interest rate
#   X2710 X2727 X2810 X2827 X2910 X2927   collapsed purpose bucket
#   X7183   "How much in total is owed on all the remaining loans?"
#           (#7). no rate and NO PURPOSE, so it cannot be classified.
#           Pro-rated across the consumer / non-consumer split by the
#           balance shares of the classified loans, following the
#           same logic the Fed applies to X1136, and always unpriced.
#
# Education loan grid. EDN_INST, per the Fed's macro:
#   X7824 X7847 X7870 X7924 X7947 X7970   amount still owed, loans 1-6
#   X7822 X7845 X7868 X7922 X7945 X7968   interest rate, loans 1-6
#   X7179   remaining education loans total (#7). no rate.
#
# EXCLUDED throughout:
#   vehicle loans (VEH_INST: X2218 X2318 X2418 X7169 X2424 X2519
#     X2619 X2625) - secured on the vehicle
#   mortgages and HELOCs (MRTHEL) - secured on property
#   other residential real-estate debt (RESDBT) - secured
#   home improvement loans (X1044 X1215 X1219), which the Fed folds
#     into INSTALL - commonly secured on the home
#   ODEBT (margin loans, loans against pensions and life insurance)
#     - collateralised against the asset borrowed against
#   BNPL (X443) - unsecured, but priced through fees rather than an
#     APR, and the SCF collects no rate for it. Holding is recorded
#     as a supplementary indicator only.
#
# ---------------------------------------------------------------------
# RATES: STORAGE and SPECIAL CODES
# ---------------------------------------------------------------------
# Every SCF rate variable is stored as PERCENT * 100, so 1800 =
# 18.00% APR. Verified empirically: median X7132 = 1800 (cards),
# median X7822 = 500 (education, the federal rate), median X2724 =
# 775. Special codes on every rate variable:
#    -1  "No interest" / "Nothing"  -> rate of 0
#     0  Inap. (no loan of this type) -> NA, no rate observed
# Ten records (0.04%) report a rate above 100%, to a maximum of 128%,
# despite the interviewer instruction to cap entries at 100%. They
# are left uncapped; the count is reported in `meta`.
#
# ---------------------------------------------------------------------
# FEES / CHARGES and PENALTY EXPOSURE
# ---------------------------------------------------------------------
# The SCF asks NO question about fees or charges paid. The nearest
# observable is missed-payment behaviour, which triggers late fees
# and (post-CARD Act) penalty APRs, so it is a PROXY for fee
# incidence and is labelled as such - it is not a measure of fees
# paid.
#   X3004  "were all the payments made the way they were scheduled,
#          or were payments on any of the loans sometimes made later
#          or missed?"  1 = all as scheduled or ahead, 5 = sometimes
#          behind or missed, 0 = Inap. (no loans of any kind)
#   X3005  "Were you ever behind in your payments by two months or
#          more?"  1 = yes, 5 = no, 0 = Inap.
#   X7063  "During the past year, have you ... taken out a payday
#          loan ...?"  1 = yes, 5 = no
#   X432   "... do you almost always, sometimes, or hardly ever pay
#          off the total balance owed on the account each month?"
#          1 = always/almost always, 3 = sometimes, 5 = hardly ever,
#          0 = Inap.
#   X442   number of buy-now-pay-later purchases in the past year
#   X443   balance outstanding on revolving BNPL accounts
#
# Constructed indicators (household level, per implicate):
#   has_loan     X3004 != 0. The universe for late_any.
#   late_any     X3004 == 5.
#   deling_60    X3005 == 1.
#   payday       X7063 == 1.
#   pen_exposed  deling_60 | payday. Two directly observed triggers
#                of penalty pricing: 60+ day delinquency, the
#                contractual trigger for a penalty APR and late
#                fees, and payday borrowing, the penalty-priced
#                segment itself.
#   revolver     X432 in {3, 5}.
#   apr_ge25     revolver, positive card balance, and a reported card
#                APR of 25% or more.
#   has_bnpl     X443 > 0.
#
# ---------------------------------------------------------------------
# VALIDATION
# ---------------------------------------------------------------------
# Every check errors rather than warns, on the same principle as
# load_scf.R: a silent change in a future SCF vintage should stop the
# pipeline, not quietly alter the table.
#   1. 5 equal-sized implicates, YY1 == Y1 %/% 10
#   2. X42001 sums to the same population total in every implicate
#   3. wt1b{r} * mm{r} reproduces that total in every replicate, and
#      wt1b{r} alone does not
#   4. bal_card reconciles to CCBAL, bal_loc to OTHLOC, bal_ed to
#      EDN_INST, each to within $1 per household
#   5. no loan purpose code equals 83 (so the Fed's education term is
#      inoperative on public data)
#   6. purpose buckets partition the other-loan grid total
#   7. card APR median lands in a plausible range, catching a change
#      in the percent*100 storage convention
#   8. the returned output dictionary ($definitions) lists
#      the columns of hh, in order
# =====================================================================

library(data.table)
library(haven)

.SCF_POP_TOL <- 0.001   # 0.1% tolerance on population totals
.SCF_BAL_TOL <- 1       # $1 per household on balance reconciliation
.SCF_N_REP   <- 999

# - credit cards ----------------------------------------------------
# X413  After the last payment(s) (was/were) made, what was the total
# X421  balance still owed on (this account/all these accounts)?
# x427  WE WANT THE TOTAL AMOUNT OWED, not THE MINIMUM PAYMENT.
#       DO not INCLUDE CHARGES MADE SINCE THE CLOSE OF THE LAST
#       STATEMENT.
#       CODE zero IF ALL ACCOUNTS WERE PAID IN FULL.
.SCF_CARD_BAL   <- c("x413", "x421", "x427")
# X7132 What interest rate do you pay on the card where you have
#       the largest balance? Asked only if there is a bank-type
#       credit card (#1); 0 = Inap ("no cards of type").
.SCF_CARD_RATE  <- "x7132"
# X7575 STORE CHARGE ACCOUNTS, not extra cards. Gate X7577: "Other
#       than the store accounts where you have credit cards, do you
#       ... have any charge or revolving charge accounts at stores
#       where you owed money after your last payment?" Then: "what
#       was the balance still owed on all these accounts?"
#       No rate asked. In the Fed's CCBAL, so kept in bal_card.
.SCF_CARD_EXTRA <- "x7575"

# - lines of credit (grid stride 11) --------------------------------
# Whole grid is gated on X1101 (has any line of credit) and X1102
# (how many). The balance is asked only where X1105/X1116/X1127 = 1
# ("Do you currently owe any money on this line?"), and X1108 has no
# -1 code, so a 0 is Inap - never a reported zero balance.
# X1108(#1)       How much is currently owed?
# X1119(#2)
# X1130(#3)       $ AMOUNT: 0 = Inap.
.SCF_LOC_BAL   <- c("x1108", "x1119", "x1130")
# X1111(#1)       What is the current annual rate of interest being
# X1122(#2)       charged on this loan?
# X1133(#3)       PERCENT * 100: -1 = Nothing, 0 = Inap.
.SCF_LOC_RATE  <- c("x1111", "x1122", "x1133")
# X1103(#1)       Is (this/the largest/the next) line of credit
# X1114(#2)       secured by the equity in your home?
# X1125(#3)       1 = YES, 5 = NO, 0 = Inap.
.SCF_LOC_HELOC <- c("x1103", "x1114", "x1125")
# X1136(#4)       What is the total amount that you (and your family
#                 living here) currently owe on all other remaining
#                 lines of credit?
#                 -1 = Nothing, 0 = Inap (fewer than four lines,
#                 X1102<4). no rate COMPANION - balance only, so it
#                 is pro-rated by the non-HELOC share and excluded
#                 from the rate. See OTHLOC in the header.
.SCF_LOC_EXTRA <- "x1136"

# - other loan grid -------------------------------------------------
# How much is still owed on this loan?
.SCF_OI_BAL   <- c(
    "x2723", "x2740", "x2823", "x2840", "x2923",
    "x2940"
)
# What is the current annual rate of interest being charged on this loan?
.SCF_OI_RATE  <- c(
    "x2724", "x2741", "x2824", "x2841", "x2924",
    "x2941"
)
# X2710(#1)       What was the loan for?
# X2727(#2)
# X2810(#3)       What was the largest loan for?
# X2827(#4)
# X2910(#5)       What was the next of these other loans for?
# X2927(#6)
.SCF_OI_PURP  <- c(
    "x2710", "x2727", "x2810", "x2827", "x2910",
    "x2927"
)
.SCF_OI_EXTRA <- "x7183"

# Collapsed purpose buckets counted as unsecured CONSUMER borrowing.
# See the header for the full bucket list and the reasons for the
# exclusions (1, 2 property; 3, 6 vehicle; 7 business/investment;
# 9 medical fused with education).
.SCF_PURP_CONSUMER <- c(4L, 5L, 8L, 10L)
# Bucket 9 is medical and education, inseparable on public data. It
# enters r_all only.
.SCF_PURP_MEDEDU   <- 9L

# - education loan grid ---------------------------------------------
# How much is still owed on this loan?
.SCF_ED_BAL   <- c(
    "x7824", "x7847", "x7870", "x7924", "x7947",
    "x7970"
)
# What is the current annual rate of interest being charged on this loan?
.SCF_ED_RATE  <- c(
    "x7822", "x7845", "x7868", "x7922", "x7945",
    "x7968"
)
# How much in total is owed on all the remaining loans?
.SCF_ED_EXTRA <- "x7179"
# X3004         Now thinking of all the various loan or mortgage payments
#               you made during the last year, were all the payments made
#               the way they were scheduled, or were payments on any of
#               the loans sometimes made later or missed?
# X3005         Were you ever behind in your payments by two months or
#               more?
# X7063         During the past year, have you (or anyone in your family
#               living here) taken out a "payday loan," that is, borrowed
#               money that was supposed to be repaid in full out of your
#               next paycheck?
# X432          Thinking only about Visa, MasterCard, Discover, American
#               Express cards you can pay off over time, and store cards,
#               do you almost always, sometimes, or hardly ever pay off the
#               total balance owed on the account each month?
.SCF_BEHAVIOUR <- c(
    "x3004", "x3005", "x7063", "x432", "x442",
    "x443"
)

# Raw numeric accessor: NA to 0, negatives PRESERVED.
.scf_raw <- function(x) {
    x <- as.numeric(x)
    x[is.na(x)] <- 0
    x
}

# Dollar amount with the Fed's MAX(0, .) floor applied: -1 ("None")
# and any other negative become 0.
.scf_amt <- function(x) {
    x <- as.numeric(x)
    x[is.na(x) | x < 0] <- 0
    x
}

# Recode an SCF rate variable from percent*100 to percent.
.scf_rate <- function(x) {
    x <- as.numeric(x)
    out <- x / 100
    out[x == -1] <- 0
    out[x == 0]  <- NA_real_
    out
}

# Accumulate a balance-weighted rate over paired balance/rate
# columns.
.scf_accumulate <- function(bal_list, rate_list) {
    if (length(bal_list) != length(rate_list)) {
        stop(
            ".scf_accumulate(): bal_list and rate_list must be the ",
            "same length."
        )
    }
    n <- length(bal_list[[1]])
    num <- rep(0, n); den <- rep(0, n)
    for (i in seq_along(bal_list)) {
        b <- bal_list[[i]]
        r <- rate_list[[i]]
        priced <- b > 0 & !is.na(r)
        num <- num + ifelse(priced, b * r, 0)
        den <- den + ifelse(priced, b, 0)
    }
    list(num = num, den = den)
}

# Safe ratio returning NA on a non-positive denominator.
.scf_div <- function(num, den) {
    ifelse(is.finite(den) & den > 0, num / den, NA_real_)
}

# Output dictionary for load_scf_credit_price()$hh.
.scf_definitions <- function() {
    d <- function(column, definition, source, units, na_zero) {
        data.table(
            column = column, definition = definition,
            source = source, units = units, na_zero = na_zero
        )
    }
    usd <- "USD, 2022 (as reported at interview)"
    rbindlist(list(
        # - identifiers, weight, income ----------------------------
        d(
            "yy1",
            "Family (primary economic unit) id; same in all implicates.",
            "YY1 (= Y1 %/% 10)", "id", "Never NA."
        ),
        d(
            "impl",
            paste0(
                "Implicate, 1-5. Each is a complete copy of the data ",
                "with one draw for every missing answer."
            ),
            "Y1 %% 10", "integer 1-5", "Never NA."
        ),
        d(
            "wgt",
            paste0(
                "Analysis weight: US families this row represents. ",
                "Sums to 131,306,391 within ONE implicate; never sum ",
                "across implicates. Not rscfp2022's wgt, which is ",
                "X42001 / 5."
            ),
            "X42001", "families", "Never NA."
        ),
        d(
            "income",
            paste0(
                "Total family income before tax for 2021, including ",
                "transfers and IRA/pension withdrawals, in the ",
                "summary extract's base-period dollars (2025 in the ",
                "extract downloaded 4 Nov 2026; meta$dollar_factor). ",
                "GROSS. The decile ranking variable."
            ),
            paste0(
                "rscfp2022 income: MAX(0, X5729) + PENACCTWD, ",
                "inflated by the Fed"
            ),
            "USD, extract base period",
            "0 = zero OR negative (the Fed floors at 0)."
        ),

        # - balances -----------------------------------------------
        d(
            "bal_card",
            paste0(
                "Credit, store-branded and charge card balance still ",
                "owed after the last payment, PLUS store charge ",
                "accounts (bal_store_acct). Equals Fed CCBAL exactly."
            ),
            "MAX(0, .) of X413 X421 X427 X7575", usd,
            "0 = no balance (none, paid in full, or no cards)."
        ),
        d(
            "bal_store_acct",
            paste0(
                "Of bal_card: store charge or revolving charge ",
                "accounts, a separate product from store-branded ",
                "cards. No rate is asked, so it is never priced."
            ),
            "X7575 (gate X7577, count X7576)", usd,
            "0 = no store charge account balance."
        ),
        d(
            "bal_loc",
            paste0(
                "Balance owed on lines of credit NOT secured on the ",
                "home, incl. a pro-rated share of the 'all remaining ",
                "lines' total. Equals Fed OTHLOC exactly."
            ),
            paste0(
                "X1108 X1119 X1130; HELOC flags X1103 X1114 X1125; ",
                "X1136; houses"
            ),
            usd, "0 = no non-HELOC line balance."
        ),
        d(
            "bal_cardloc",
            "bal_card + bal_loc. Balance base for r_cardloc.",
            "derived", usd, "0 = no card or line balance."
        ),
        d(
            "bal_grid_cons",
            paste0(
                "Other-loan grid: loans in collapsed purpose buckets ",
                "4, 5, 8, 10 (unsecured consumer), plus their ",
                "pro-rated share of the 'all remaining loans' total."
            ),
            paste0(
                "X2723 X2740 X2823 X2840 X2923 X2940; purpose X2710 ",
                "X2727 X2810 X2827 X2910 X2927; X7183"
            ),
            usd, "0 = none."
        ),
        d(
            "bal_grid_medu",
            paste0(
                "Other-loan grid: loans in collapsed bucket 9 ",
                "(medical AND education, inseparable on public ",
                "data), plus their pro-rated share of X7183."
            ),
            "as bal_grid_cons", usd, "0 = none."
        ),
        d(
            "bal_ed",
            paste0(
                "Education loan balance, incl. the 'all remaining ",
                "education loans' total. Equals Fed EDN_INST ",
                "exactly."
            ),
            "X7824 X7847 X7870 X7924 X7947 X7970 X7179", usd,
            "0 = none."
        ),
        d(
            "bal_consumer",
            "bal_cardloc + bal_grid_cons. Balance base for r_consumer.",
            "derived", usd, "0 = none."
        ),
        d(
            "bal_unsec_all",
            paste0(
                "bal_consumer + bal_grid_medu + bal_ed. Base for ",
                "r_all. Excludes vehicle, mortgage, HELOC, home ",
                "improvement, other residential and ODEBT (secured)."
            ),
            "derived", usd, "0 = no unsecured debt."
        ),

        # - rates and their denominators ---------------------------
        d(
            "r_cardloc",
            paste0(
                "PRIMARY RATE. Balance-weighted mean ",
                "APR over cards and non-HELOC lines that carry a ",
                "rate. The one card rate covers X413 + X421 + X427."
            ),
            "card X7132; lines X1111 X1122 X1133", "percent APR",
            paste0(
                "NA = no balance with an observed rate (NOT zero). ",
                "0 = respondent reported no interest."
            )
        ),
        d(
            "r_consumer",
            "As r_cardloc, adding the bal_grid_cons loans.",
            "+ X2724 X2741 X2824 X2841 X2924 X2941", "percent APR",
            "NA as for r_cardloc."
        ),
        d(
            "r_all",
            paste0(
                "As r_consumer, adding bucket-9 and education loans. ",
                "Pulled down by federal student-loan pricing; do not ",
                "quote as the price of credit."
            ),
            "+ X7822 X7845 X7868 X7922 X7945 X7968", "percent APR",
            "NA as for r_cardloc."
        ),
        d(
            "priced_cardloc",
            paste0(
                "Denominator of r_cardloc: card and line balance that ",
                "carries an observed rate. Excludes store charge ",
                "accounts (X7575), the remaining-lines total (X1136), ",
                "and card balances of families never asked X7132."
            ),
            "derived", usd, "0 = nothing priced (r_cardloc is NA)."
        ),
        d(
            "priced_consumer", "Denominator of r_consumer.", "derived",
            usd, "0 = nothing priced."
        ),
        d(
            "priced_all", "Denominator of r_all.", "derived", usd,
            "0 = nothing priced."
        ),

        # - behaviour ----------------------------------------------
        d(
            "has_loan",
            paste0(
                "1 = holds debt of a type listed for X3004 (cards, ",
                "mortgages, vehicle, education, other loans), so was ",
                "asked X3004. Universe for late_any."
            ),
            "X3004 != 0", "0/1", "0 = no debt of any kind."
        ),
        d(
            "late_any",
            paste0(
                "1 = a loan payment made late or missed in the last ",
                "12 months. A proxy for late-fee ",
                "incidence, not fees paid."
            ),
            "X3004 == 5", "0/1",
            "0 = all on schedule OR no loans; see has_loan."
        ),
        d(
            "deling_60",
            "1 = ever 60+ days behind on a loan payment in the last year.",
            "X3005 == 1", "0/1",
            "0 = no, OR not asked (asked only if X3004 == 5)."
        ),
        d(
            "payday",
            "1 = took out a payday loan in the past year. Asked of all.",
            "X7063 == 1", "0/1", "0 = no."
        ),
        d(
            "pen_exposed",
            "deling_60 OR payday.",
            "derived", "0/1", "0 = neither."
        ),
        d(
            "revolver",
            "1 = sometimes or hardly ever pays card balance in full.",
            "X432 in {3, 5}", "0/1",
            "0 = always pays in full, OR no such cards (Inap)."
        ),
        d(
            "apr_ge25",
            "1 = revolver with a card balance and card APR of 25%+.",
            "revolver, bal_card, X7132", "0/1", "0 = otherwise."
        ),
        d(
            "has_bnpl",
            "1 = owes a balance on buy-now-pay-later.",
            "X443 > 0", "0/1", "0 = no BNPL balance."
        ),
        d(
            "has_cardloc", "1 = bal_cardloc > 0.", "derived", "0/1",
            "0 = no card or line balance."
        ),
        d(
            "has_unsec", "1 = bal_unsec_all > 0.", "derived", "0/1",
            "0 = no unsecured debt."
        )
    ))
}

# Load SCF 2022 microdata and construct household-level price-of-
# credit measures, keeping the five implicates separate.
load_scf_credit_price <- function(p_path, rscfp_path, rw_path) {
    x_vars <- c(
        "y1", "yy1", "x42001",
        .SCF_CARD_BAL, .SCF_CARD_RATE, .SCF_CARD_EXTRA,
        .SCF_LOC_BAL, .SCF_LOC_RATE, .SCF_LOC_HELOC,
        .SCF_LOC_EXTRA,
        .SCF_OI_BAL, .SCF_OI_RATE, .SCF_OI_PURP,
        .SCF_OI_EXTRA,
        .SCF_ED_BAL, .SCF_ED_RATE, .SCF_ED_EXTRA,
        .SCF_BEHAVIOUR
    )

    d <- as.data.table(read_dta(p_path, col_select = all_of(x_vars)))
    missing_x <- setdiff(x_vars, names(d))
    if (length(missing_x) > 0) {
        stop(
            "load_scf_credit_price(): X-variables not found in ",
            basename(p_path), ": ",
            paste(missing_x, collapse = ", "),
            ". Check this is the 2022 full public dataset."
        )
    }

    s <- as.data.table(read_dta(rscfp_path, col_select = all_of(
        c(
            "y1", "wgt", "income", "houses", "ccbal", "othloc",
            "oth_inst", "edn_inst", "veh_inst", "install", "debt"
        )
    )))

    dt <- merge(d, s, by = "y1", all.x = TRUE)
    if (anyNA(dt$income)) {
        stop(
            "load_scf_credit_price(): ", sum(is.na(dt$income)),
            " records failed to match rscfp2022 on y1 - the ",
            "summary extract and the full dataset are not the same ",
            "vintage."
        )
    }

    # - implicate structure (check 1) ---------------------------
    dt[, y1 := as.integer(y1)]
    dt[, yy1 := as.integer(yy1)]
    dt[, impl := y1 %% 10L]
    if (!identical(sort(unique(dt$impl)), 1:5)) {
        stop(
            "load_scf_credit_price(): expected implicates 1-5, ",
            "got ", paste(sort(unique(dt$impl)), collapse = ","),
            "."
        )
    }
    n_by_impl <- dt[, .N, by = impl]$N
    if (length(unique(n_by_impl)) != 1L) {
        stop(
            "load_scf_credit_price(): implicates are not equal in ",
            "size (", paste(n_by_impl, collapse = ", "), ")."
        )
    }
    if (!all(dt$yy1 == dt$y1 %/% 10L)) {
        stop(
            "load_scf_credit_price(): YY1 != Y1 %/% 10 for ",
            sum(dt$yy1 != dt$y1 %/% 10L), " records - the ",
            "implicate id convention has changed."
        )
    }

    # - population totals (check 2) -----------------------------
    setnames(dt, "x42001", "wgt_x")
    pop <- dt[, .(tot = sum(wgt_x)), by = impl]
    pop_target <- pop$tot[1]
    if (max(abs(pop$tot - pop_target)) / pop_target > .SCF_POP_TOL) {
        stop(
            "load_scf_credit_price(): X42001 does not sum to the ",
            "same population total in every implicate (range ",
            round(min(pop$tot)), " to ", round(max(pop$tot)), ")."
        )
    }

    # - replicate weights (check 3) -----------------------------
    rep_cols <- paste0("wt1b", seq_len(.SCF_N_REP))
    mm_cols  <- paste0("mm", seq_len(.SCF_N_REP))
    rw <- as.data.table(read_dta(rw_path, col_select = all_of(
        c("yy1", rep_cols, mm_cols)
    )))
    rw[, yy1 := as.integer(yy1)]

    hh_ids <- dt[impl == 1L][order(yy1), yy1]
    setkey(rw, yy1)
    rw <- rw[J(hh_ids)]
    if (nrow(rw) != length(hh_ids) || anyNA(rw$yy1)) {
        stop(
            "load_scf_credit_price(): the replicate-weight file ",
            "does not cover every household in the main file."
        )
    }

    repw <- matrix(0,
        nrow = length(hh_ids), ncol = .SCF_N_REP,
        dimnames = list(as.character(hh_ids), NULL)
    )
    base_only <- numeric(.SCF_N_REP)
    for (r in seq_len(.SCF_N_REP)) {
        b <- .scf_raw(rw[[rep_cols[r]]])
        m <- .scf_raw(rw[[mm_cols[r]]])
        repw[, r]    <- b * m
        base_only[r] <- sum(b)
    }

    rep_tot <- colSums(repw)
    if (max(abs(rep_tot - pop_target)) / pop_target > .SCF_POP_TOL) {
        stop(
            "load_scf_credit_price(): wt1b * mm does not ",
            "reproduce the X42001 population total (",
            round(pop_target), "); replicate totals range ",
            round(min(rep_tot)), " to ", round(max(rep_tot)),
            ". Do not trust any standard error from this object."
        )
    }
    # Converse check: wt1b alone must not reproduce the total. If a
    # future vintage changes convention so that it does, the * mm
    # multiplication above would double-count and must be removed.
    if (max(abs(base_only - pop_target)) / pop_target <=
        .SCF_POP_TOL) {
        stop(
            "load_scf_credit_price(): wt1b alone now reproduces ",
            "the population total, so the documented wt1b * mm ",
            "convention has changed. Re-derive the replicate ",
            "analysis weight before using this loader."
        )
    }

    # - purpose codes are collapsed (check 5) -------------------
    purp <- lapply(.SCF_OI_PURP, function(v) as.integer(dt[[v]]))
    purp_vals <- sort(unique(unlist(purp)))
    if (any(purp_vals == 83L)) {
        stop(
            "load_scf_credit_price(): a loan purpose code of 83 ",
            "(education) is present, so the public file is no ",
            "longer using the collapsed 0-10 buckets. The Fed's ",
            "EDN_INST purpose term is now live and the ",
            "reconciliation logic in this loader must be redone."
        )
    }
    if (!all(purp_vals %in% 0:10)) {
        stop(
            "load_scf_credit_price(): loan purpose codes outside ",
            "the expected collapsed range 0-10: ",
            paste(setdiff(purp_vals, 0:10), collapse = ", "), "."
        )
    }

    # - rate storage convention (check 7) -----------------------
    card_raw <- as.numeric(dt[[.SCF_CARD_RATE]])
    card_med <- median(card_raw[card_raw > 0], na.rm = TRUE)
    if (card_med < 500 || card_med > 4000) {
        stop(
            "load_scf_credit_price(): median positive X7132 is ",
            card_med, ", outside the 500-4000 range expected for ",
            "a percent*100 credit-card APR. The storage ",
            "convention may have changed."
        )
    }
    n_rate_gt100 <- sum(vapply(
        c(.SCF_CARD_RATE, .SCF_LOC_RATE, .SCF_OI_RATE, .SCF_ED_RATE),
        function(v) sum(as.numeric(dt[[v]]) > 10000), integer(1)
    ))

    # =============================================================
    # CREDIT CARDS: CCBAL
    # =============================================================
    card_parts <- lapply(.SCF_CARD_BAL, function(v)
        .scf_amt(dt[[v]]))
    card_detail <- Reduce(`+`, card_parts)
    card_mop    <- .scf_amt(dt[[.SCF_CARD_EXTRA]])
    dt[, bal_card := card_detail + card_mop]
    dt[, bal_store_acct := card_mop]

    card_rate <- .scf_rate(dt[[.SCF_CARD_RATE]])
    # One rate for the whole detailed card balance.
    card_acc <- .scf_accumulate(list(card_detail), list(card_rate))

    # =============================================================
    # LINES OF CREDIT: OTHLOC, replicating the Fed's macro
    # =============================================================
    loc_b   <- lapply(.SCF_LOC_BAL, function(v) .scf_raw(dt[[v]]))
    loc_hel <- lapply(.SCF_LOC_HELOC, function(v)
        as.numeric(.scf_raw(dt[[v]]) == 1))
    loc_mop <- .scf_amt(dt[[.SCF_LOC_EXTRA]])

    loc_tot3     <- Reduce(`+`, loc_b)
    loc_nonheloc <- Reduce(`+`, Map(
        function(b, h) b * (1 - h),
        loc_b, loc_hel
    ))
    houses <- .scf_raw(dt$houses)
    othloc_main <- loc_nonheloc +
        loc_mop * .scf_div(loc_nonheloc, loc_tot3)
    othloc_fallback <- ((houses <= 0) + 0.5 * (houses > 0)) * loc_mop
    dt[, bal_loc := ifelse(loc_tot3 >= 1,
        ifelse(is.na(othloc_main), loc_nonheloc,
            othloc_main
        ),
        othloc_fallback
    )]

    # Rate: non-HELOC lines only.
    loc_acc <- .scf_accumulate(
        Map(function(b, h) pmax(0, b) * (1 - h), loc_b, loc_hel),
        lapply(.SCF_LOC_RATE, function(v) .scf_rate(dt[[v]]))
    )

    # =============================================================
    # OTHER LOAN GRID, classified by collapsed purpose bucket
    # =============================================================
    oi_b <- lapply(.SCF_OI_BAL, function(v) .scf_amt(dt[[v]]))
    oi_r <- lapply(.SCF_OI_RATE, function(v) .scf_rate(dt[[v]]))

    is_cons <- lapply(purp, function(p)
        as.numeric(p %in% .SCF_PURP_CONSUMER))
    is_medu <- lapply(purp, function(p)
        as.numeric(p %in% .SCF_PURP_MEDEDU))

    oi_grid_tot <- Reduce(`+`, oi_b)
    oi_cons_bal <- Reduce(`+`, Map(`*`, oi_b, is_cons))
    oi_medu_bal <- Reduce(`+`, Map(`*`, oi_b, is_medu))

    # Pro-rate the unclassifiable mop-up total across the same
    # split, following the Fed's treatment of X1136.
    oi_mop <- .scf_amt(dt[[.SCF_OI_EXTRA]])
    cons_share <- .scf_div(oi_cons_bal, oi_grid_tot)
    medu_share <- .scf_div(oi_medu_bal, oi_grid_tot)
    cons_share[is.na(cons_share)] <- 0
    medu_share[is.na(medu_share)] <- 0

    dt[, bal_grid_cons := oi_cons_bal + oi_mop * cons_share]
    dt[, bal_grid_medu := oi_medu_bal + oi_mop * medu_share]

    # - purpose buckets partition the grid (check 6) ------------
    bucket_sum <- Reduce(`+`, lapply(0:10, function(k) {
        Reduce(`+`, Map(function(b, p)
            b * as.numeric(p == k), oi_b, purp))
    }))
    if (max(abs(bucket_sum - oi_grid_tot)) > .SCF_BAL_TOL) {
        stop(
            "load_scf_credit_price(): the purpose buckets do not ",
            "partition the other-loan grid total (max gap $",
            round(max(abs(bucket_sum - oi_grid_tot))),
            "). Some loans carry a purpose code outside 0-10."
        )
    }

    oi_cons_acc <- .scf_accumulate(Map(`*`, oi_b, is_cons), oi_r)
    oi_medu_acc <- .scf_accumulate(Map(`*`, oi_b, is_medu), oi_r)

    # =============================================================
    # EDUCATION LOAN GRID: EDN_INST
    # =============================================================
    ed_b   <- lapply(.SCF_ED_BAL, function(v) .scf_amt(dt[[v]]))
    ed_mop <- .scf_amt(dt[[.SCF_ED_EXTRA]])
    dt[, bal_ed := Reduce(`+`, ed_b) + ed_mop]
    ed_acc <- .scf_accumulate(
        ed_b, lapply(.SCF_ED_RATE, function(v) .scf_rate(dt[[v]]))
    )

    # =============================================================
    # RECONCILIATION (check 4)
    # =============================================================
    # rscfp2022 states every dollar amount in the real dollars of the
    # Fed's chosen base period (the macro's REAL=YES and CPIBASE), and
    # the Fed re-issues the extract when it moves the base. The
    # extract used when this table was built (Sept 2026) was in 2022
    # dollars; the one downloaded on 4 Nov 2026 is in 2025 dollars
    # (CPI-U-RS 4789 / 4376 = 1.0943784 on balances). The X-variables
    # are as reported at interview. So the factor is estimated from
    # CCBAL, required to be the same for every household, and applied
    # before reconciling. It does not touch any estimate: balances
    # come from the X-variables, and income only ranks households.
    pos <- dt$bal_card > 0 & .scf_raw(dt$ccbal) > 0
    fx_all <- .scf_raw(dt$ccbal)[pos] / dt$bal_card[pos]
    dollar_factor <- median(fx_all)
    if (diff(range(fx_all)) / dollar_factor > 1e-8) {
        stop(
            "load_scf_credit_price(): rscfp2022 ccbal is not a ",
            "constant multiple of the X-variable card balance (",
            "ratio ", signif(min(fx_all), 6), " to ",
            signif(max(fx_all), 6), "), so the extract is not a ",
            "simple re-basing of this full dataset."
        )
    }
    recon_gap <- c(
        ccbal    = max(abs(dt$bal_card * dollar_factor -
            .scf_raw(dt$ccbal))),
        othloc   = max(abs(dt$bal_loc * dollar_factor -
            .scf_raw(dt$othloc))),
        edn_inst = max(abs(dt$bal_ed * dollar_factor -
            .scf_raw(dt$edn_inst)))
    )
    bad <- names(recon_gap)[recon_gap > .SCF_BAL_TOL]
    if (length(bad) > 0) {
        stop(
            "load_scf_credit_price(): balances built from the ",
            "X-variables do not reconcile to the Fed's own ",
            "aggregates in rscfp2022 for: ",
            paste(sprintf(
                "%s (max gap $%.0f)", bad,
                recon_gap[bad]
            ), collapse = ", "),
            ". The component lists or the replicated macro logic ",
            "in this file are wrong for this vintage."
        )
    }

    # =============================================================
    # THE three NESTED RATE MEASURES
    # =============================================================
    num_cl <- card_acc$num + loc_acc$num
    den_cl <- card_acc$den + loc_acc$den
    dt[, `:=`(
        bal_cardloc    = bal_card + bal_loc,
        r_cardloc      = .scf_div(num_cl, den_cl),
        priced_cardloc = den_cl
    )]

    num_co <- num_cl + oi_cons_acc$num
    den_co <- den_cl + oi_cons_acc$den
    dt[, `:=`(
        bal_consumer    = bal_cardloc + bal_grid_cons,
        r_consumer      = .scf_div(num_co, den_co),
        priced_consumer = den_co
    )]

    num_all <- num_co + oi_medu_acc$num + ed_acc$num
    den_all <- den_co + oi_medu_acc$den + ed_acc$den
    dt[, `:=`(
        bal_unsec_all = bal_consumer + bal_grid_medu + bal_ed,
        r_all         = .scf_div(num_all, den_all),
        priced_all    = den_all
    )]

    # =============================================================
    # BEHAVIOURAL INDICATORS
    # =============================================================
    dt[, `:=`(
        has_loan  = as.integer(x3004 != 0),
        late_any  = as.integer(x3004 == 5),
        deling_60 = as.integer(x3005 == 1),
        payday    = as.integer(x7063 == 1),
        revolver  = as.integer(x432 %in% c(3, 5)),
        has_bnpl  = as.integer(.scf_amt(x443) > 0)
    )]
    dt[, pen_exposed := as.integer(deling_60 == 1 | payday == 1)]
    dt[, apr_ge25 := as.integer(revolver == 1 & bal_card > 0 &
        !is.na(card_rate) &
        card_rate >= 25)]
    dt[, has_cardloc := as.integer(bal_cardloc > 0)]
    dt[, has_unsec   := as.integer(bal_unsec_all > 0)]

    hh <- dt[order(impl, yy1), .(
        yy1, impl,
        wgt = wgt_x, income,
        bal_card, bal_store_acct, bal_loc, bal_cardloc, bal_grid_cons,
        bal_grid_medu, bal_ed, bal_consumer, bal_unsec_all,
        r_cardloc, r_consumer, r_all,
        priced_cardloc, priced_consumer, priced_all,
        has_loan, late_any, deling_60, payday, pen_exposed,
        revolver, apr_ge25, has_bnpl, has_cardloc, has_unsec
    )]

    # - output dictionary matches hh (check 8) ------------------
    defs <- .scf_definitions()
    if (!identical(defs$column, names(hh))) {
        stop(
            "load_scf_credit_price(): the output dictionary does ",
            "not match the columns of hh. In hh but not the ",
            "dictionary: ",
            paste(setdiff(names(hh), defs$column), collapse = ", "),
            "; in the dictionary but not hh: ",
            paste(setdiff(defs$column, names(hh)), collapse = ", "),
            " (if both are empty, the ORDER differs). Update ",
            ".scf_definitions() alongside any change to hh."
        )
    }

    list(
        hh          = hh,
        repw        = repw,
        definitions = defs,
        meta = list(
            n_households  = length(hh_ids),
            n_implicates  = 5L,
            n_replicates  = .SCF_N_REP,
            pop_total     = pop_target,
            recon_max_gap = recon_gap,
            card_rate_med = card_med / 100,
            dollar_factor = dollar_factor,
            n_rate_gt_100 = n_rate_gt100,
            purpose_vals  = purp_vals,
            oth_inst_note = paste0(
                "OTH_INST is NOT reconciled: its Fed definition ",
                "uses raw loan purpose codes that the public file ",
                "collapses, so it cannot be reproduced from public ",
                "data."
            ),
            source = paste0(
                "Federal Reserve Survey of Consumer ",
                "Finances, 2022 (full public dataset)"
            )
        )
    )
}
