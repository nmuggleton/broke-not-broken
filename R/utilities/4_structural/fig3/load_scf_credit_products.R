# =====================================================================
# load_scf_credit_products.R
#
# Household-level use and price of six credit products, from the
# Survey of Consumer Finances 2022, for the credit-products figure
# (Section 4): one point per product, the median income of the
# product's users (x) against the median APR they pay (y).
#
# Builds on load_scf_credit_price.R . That loader is called
# first, so the weights, implicates and 999 replicate weights pass
# through the same validation checks (see its header); this file
# then reads the extra X-variables and constructs the six products.
#
# ---------------------------------------------------------------------
# THE SIX PRODUCTS
# ---------------------------------------------------------------------
# A household is a USER of a product if it holds a positive balance
# on it at interview, except payday loans, which the SCF records only
# as use in the past year. The household's APR on a product is the
# balance-weighted mean of the rates on that product's loans with a
# positive balance and an observed rate:
#     r = sum_j (balance_j * rate_j) / sum_j balance_j
# Rates are stored as percent * 100; -1 = no interest (rate 0), 0 =
# Inap. (no rate). See .scf_rate() in load_scf_credit_price.R.
#
#   mortgage  First, second and third mortgages and land contracts on
#             the primary residence. Balance X805 X905 X1005; rate
#             X816 X916 X1016. HELOCs are excluded. The full balance
#             is used: for farms, the Fed's macro moves a share X507
#             (capped at 90%) of these loans into the farm business,
#             but the family still holds the whole mortgage, and the
#             share does not change its APR. The Fed's share is
#             applied only to reconcile to its NH_MORT (check 2).
#   vehicle   Vehicle loans: cars 1-4 (balance X2218 X2318 X2418
#             X7169; rate X2219 X2319 X2419 X7170), other vehicles
#             1-2 (balance X2519 X2619; rate X2520 X2620), plus the
#             two remainder totals X2424 and X2625, which have no
#             rate. Balance equals the Fed's VEH_INST.
#   student   Education loan grid, (EDN_INST). Mostly
#             federal loans, whose rates are set by statute.
#   personal  Other-loan-grid loans in the consumer purpose buckets
#             4, 5, 8 and 10, as in r_consumer, with the
#             unclassifiable remainder X7183 pro-rated.
#   personal_<lender>
#             The same loans split by who made them, from the Fed's
#             "Recode: type of institution" for each loan in the grid
#             (X9107-X9112; master institution list at X308, with the
#             public-data collapsing). Groups, by code:
#               bank         11 commercial bank (with 29), 12 savings
#                            and loan, 13 credit union, 38 bank-type
#                            card company
#               finance      14 finance or loan company
#               mortgage_co  18 mortgage company or broker
#               store        15 store, other business, dealer, utility
#               informal     26 friend or relative, 85 ex-spouse, 96
#                            individual n.e.c., -7 a person
#               other        every other code (mostly 35 government,
#                            22 doctor or hospital, 95 non-financial)
#             A household uses a lender group if it owes a positive
#             balance to that group; it can use several. The X7183
#             remainder has no lender and is left out of the split.
#   card      Credit, store-branded and charge cards plus store charge
#             accounts still owed after the last payment (CCBAL): a
#             balance carried past the statement. Rate X7132, ONE
#             rate per family, asked only where there is a bank-type
#             card, applied to the card balance (not to X7575).
#   payday    Took out a payday loan in the past year (X7063 == 1).
#             The SCF records no payday APR or balance; the APR for
#             the figure is supplied in main.R from published
#             sources.
#
# Checks (all error rather than warn):
#   1. vehicle balance reconciles to rscfp2022 veh_inst, card balance
#      to ccbal, student balance to edn_inst, each within $1 after
#      applying the extract's dollar re-basing factor (estimated and
#      checked in load_scf_credit_price(); see its check 4)
#   2. mortgage balance, after the Fed's farm-business share (X507),
#      reconciles to nh_mort within $1 (same dollar factor) for every
#      household where the Fed's macro adds no X1136 share
#   3. median positive mortgage rate is in a plausible range (catches
#      a change in the percent * 100 storage convention)
#   4. every consumer-purpose loan with a balance has a lender type
#      (no Inap. code), so the lender split is exhaustive
# =====================================================================

source("R/utilities/4_structural/fig3/load_scf_credit_price.R")
library(data.table)
library(haven)

.SCF_MORT_BAL  <- c("x805", "x905", "x1005")
.SCF_MORT_RATE <- c("x816", "x916", "x1016")
.SCF_VEH_BAL   <- c(
    "x2218", "x2318", "x2418", "x7169", "x2519",
    "x2619"
)
.SCF_VEH_RATE  <- c(
    "x2219", "x2319", "x2419", "x7170", "x2520",
    "x2620"
)
.SCF_VEH_EXTRA <- c("x2424", "x2625")

.SCF_PRODUCTS <- c(
    "mortgage", "vehicle", "student", "personal",
    "card", "payday"
)

# Lender type of other-loan-grid loans #1-#6 (see header).
.SCF_OI_INST <- paste0("x", 9107:9112)
.SCF_LENDERS <- list(
    bank        = c(11L, 12L, 13L, 38L),
    finance     = 14L,
    mortgage_co = 18L,
    store       = 15L,
    informal    = c(26L, 85L, 96L, -7L)
)
.SCF_PERSONAL_BY_LENDER <- paste0(
    "personal_",
    c(names(.SCF_LENDERS), "other")
)

# Load SCF 2022 household-level product use and APRs.
load_scf_credit_products <- function(p_path, rscfp_path, rw_path) {
    base <- load_scf_credit_price(p_path, rscfp_path, rw_path)

    x_vars <- c(
        "y1",
        .SCF_MORT_BAL, .SCF_MORT_RATE,
        .SCF_VEH_BAL, .SCF_VEH_RATE, .SCF_VEH_EXTRA,
        .SCF_CARD_BAL, .SCF_CARD_RATE, .SCF_CARD_EXTRA,
        .SCF_OI_BAL, .SCF_OI_RATE, .SCF_OI_PURP,
        .SCF_OI_EXTRA, .SCF_OI_INST,
        .SCF_ED_BAL, .SCF_ED_RATE, .SCF_ED_EXTRA,
        .SCF_LOC_BAL, .SCF_LOC_EXTRA,
        "x507", "x7063"
    )
    d <- as.data.table(read_dta(p_path, col_select = all_of(x_vars)))
    s <- as.data.table(read_dta(rscfp_path, col_select = all_of(
        c(
            "y1", "income", "nh_mort", "veh_inst", "ccbal",
            "edn_inst"
        )
    )))
    dt <- merge(d, s, by = "y1")
    dt[, y1 := as.integer(y1)]
    dt[, `:=`(yy1 = y1 %/% 10L, impl = y1 %% 10L)]

    acc <- function(bal_vars, rate_vars) {
        .scf_accumulate(
            lapply(bal_vars,  function(v) .scf_amt(dt[[v]])),
            lapply(rate_vars, function(v) .scf_rate(dt[[v]]))
        )
    }

    # - mortgage ------------------------------------------------
    mort_bal <- Reduce(`+`, lapply(
        .SCF_MORT_BAL,
        function(v) .scf_amt(dt[[v]])
    ))
    mort_acc <- acc(.SCF_MORT_BAL, .SCF_MORT_RATE)

    # - vehicle -------------------------------------------------
    veh_bal <- Reduce(`+`, lapply(
        c(.SCF_VEH_BAL, .SCF_VEH_EXTRA),
        function(v) .scf_amt(dt[[v]])
    ))
    veh_acc <- acc(.SCF_VEH_BAL, .SCF_VEH_RATE)

    # - student -------------------------------------------------
    ed_bal <- Reduce(`+`, lapply(
        c(.SCF_ED_BAL, .SCF_ED_EXTRA),
        function(v) .scf_amt(dt[[v]])
    ))
    ed_acc <- acc(.SCF_ED_BAL, .SCF_ED_RATE)

    # - personal (consumer purpose buckets) --------------------
    purp    <- lapply(.SCF_OI_PURP, function(v) as.integer(dt[[v]]))
    is_cons <- lapply(purp, function(p)
        as.numeric(p %in% .SCF_PURP_CONSUMER))
    oi_b    <- lapply(.SCF_OI_BAL,  function(v) .scf_amt(dt[[v]]))
    oi_r    <- lapply(.SCF_OI_RATE, function(v) .scf_rate(dt[[v]]))
    cons_b  <- Map(`*`, oi_b, is_cons)
    oi_tot  <- Reduce(`+`, oi_b)
    cons_tot <- Reduce(`+`, cons_b)
    cons_share <- .scf_div(cons_tot, oi_tot)
    cons_share[is.na(cons_share)] <- 0
    pers_bal <- cons_tot + .scf_amt(dt[[.SCF_OI_EXTRA]]) * cons_share
    pers_acc <- .scf_accumulate(cons_b, oi_r)

    # - personal, by lender type (check 4) ---------------------
    lend <- lapply(.SCF_OI_INST, function(v) {
        code <- as.integer(dt[[v]])
        out  <- rep("other", length(code))
        out[is.na(code) | code == 0L] <- NA_character_
        for (g in names(.SCF_LENDERS)) {
            out[code %in% .SCF_LENDERS[[g]]] <- g
        }
        out
    })
    n_unlent <- sum(mapply(
        function(b, l) sum(b > 0 & is.na(l)),
        cons_b, lend
    ))
    if (n_unlent > 0) {
        stop(
            "load_scf_credit_products(): ", n_unlent, " consumer-",
            "purpose loans with a balance have no lender type."
        )
    }
    lender_cols <- list()
    for (g in c(names(.SCF_LENDERS), "other")) {
        b_g <- Map(function(b, l) b * (l %in% g), cons_b, lend)
        acc <- .scf_accumulate(b_g, oi_r)
        lender_cols[[paste0("use_personal_", g)]] <-
            as.integer(Reduce(`+`, b_g) > 0)
        lender_cols[[paste0("apr_personal_", g)]] <-
            .scf_div(acc$num, acc$den)
    }

    # - card ----------------------------------------------------
    card_detail <- Reduce(`+`, lapply(
        .SCF_CARD_BAL,
        function(v) .scf_amt(dt[[v]])
    ))
    card_bal  <- card_detail + .scf_amt(dt[[.SCF_CARD_EXTRA]])
    card_acc  <- .scf_accumulate(
        list(card_detail),
        list(.scf_rate(dt[[.SCF_CARD_RATE]]))
    )

    # - reconciliation (checks 1 and 2) -------------------------
    fx <- base$meta$dollar_factor
    gap <- c(
        veh_inst = max(abs(veh_bal  * fx - .scf_raw(dt$veh_inst))),
        ccbal    = max(abs(card_bal * fx - .scf_raw(dt$ccbal))),
        edn_inst = max(abs(ed_bal   * fx - .scf_raw(dt$edn_inst)))
    )
    loc3 <- Reduce(`+`, lapply(
        .SCF_LOC_BAL,
        function(v) .scf_raw(dt[[v]])
    ))
    no_mop <- loc3 >= 1 | .scf_amt(dt[[.SCF_LOC_EXTRA]]) <= 0
    farm <- pmin(9000, pmax(0, .scf_raw(dt$x507)))
    mort_fed <- mort_bal * (10000 - farm) / 10000
    gap["nh_mort"] <- max(abs(mort_fed[no_mop] * fx -
        .scf_raw(dt$nh_mort)[no_mop]))
    bad <- names(gap)[gap > .SCF_BAL_TOL]
    if (length(bad) > 0) {
        stop(
            "load_scf_credit_products(): product balances do not ",
            "reconcile to rscfp2022 for: ",
            paste(sprintf("%s (max gap $%.0f)", bad, gap[bad]),
                collapse = ", "
            ), "."
        )
    }

    # - rate storage convention (check 3) -----------------------
    mort_raw <- as.numeric(dt$x816)
    mort_med <- median(mort_raw[mort_raw > 0], na.rm = TRUE)
    if (mort_med < 100 || mort_med > 1500) {
        stop(
            "load_scf_credit_products(): median positive X816 is ",
            mort_med, ", outside the 100-1500 range expected for ",
            "a percent*100 mortgage rate."
        )
    }

    dt[, `:=`(
        use_mortgage = as.integer(mort_bal > 0),
        apr_mortgage = .scf_div(mort_acc$num, mort_acc$den),
        use_vehicle  = as.integer(veh_bal > 0),
        apr_vehicle  = .scf_div(veh_acc$num, veh_acc$den),
        use_student  = as.integer(ed_bal > 0),
        apr_student  = .scf_div(ed_acc$num, ed_acc$den),
        use_personal = as.integer(pers_bal > 0),
        apr_personal = .scf_div(pers_acc$num, pers_acc$den),
        use_card     = as.integer(card_bal > 0),
        apr_card     = .scf_div(card_acc$num, card_acc$den),
        use_payday   = as.integer(x7063 == 1),
        apr_payday   = NA_real_
    )]
    dt[, names(lender_cols) := lender_cols]

    all_p <- c(.SCF_PRODUCTS, .SCF_PERSONAL_BY_LENDER)
    keep <- c(
        "yy1", "impl",
        as.vector(rbind(
            paste0("use_", all_p),
            paste0("apr_", all_p)
        ))
    )
    hh <- merge(base$hh[, .(yy1, impl, wgt, income)],
        dt[, ..keep],
        by = c("yy1", "impl")
    )
    if (nrow(hh) != nrow(base$hh)) {
        stop(
            "load_scf_credit_products(): ", nrow(base$hh) - nrow(hh),
            " family-implicate rows were lost in the merge."
        )
    }
    setorder(hh, impl, yy1)

    list(
        hh = hh, repw = base$repw,
        meta = c(
            base$meta[c(
                "n_households", "n_implicates",
                "n_replicates", "pop_total",
                "dollar_factor"
            )],
            list(
                recon_max_gap = gap,
                mort_rate_med = mort_med / 100,
                n_mort_not_checked = sum(!no_mop) / 5
            )
        )
    )
}
