# =====================================================================
# 4_structural/figure_3/car_loan_rates.R
#
# Inputs for Figure 3B: composition test for car loans. Run from the
# repository root, before main.R.
# Question: is the gap between what low-income borrowers report (SCF)
# and what lenders charge subprime borrowers (Experian) explained by
# the mix of credit tiers within each income group?
#
# Step 1 (SCE, New York Fed): among respondents who hold a car loan
#   (N1_5 == 1), the mix of credit score bands (N22) within each
#   household income group (Q47), weighted (weight).
# Step 2: predicted rate for each income group = that mix applied to
#   Experian's rate on loans still owed in each tier (2022).
# Step 3 (SCF 2022): reported rate per car loan (cars 1-4) by the same
#   income groups, all five implicates.
suppressMessages({
    library(readxl); library(data.table); library(haven)
})
SCE <- "data/raw/sce"
OUT <- "data/raw/external_apr"
wq <- function(x, w, p = 0.5) {
    ok <- !is.na(x) & !is.na(w); x <- x[ok]; w <- w[ok]
    o <- order(x); x <- x[o]; w <- w[o]
    x[which(cumsum(w) / sum(w) >= p)[1]]
}
GRP <- c("Under $30k", "$30k-$60k", "$60k-$100k", "Over $100k")

# ---- Step 1: tier mix among car loan holders -------------------------
core <- as.data.table(suppressMessages(cbind(
    read_excel(file.path(SCE, "core2024.xlsx"),
        sheet = "Data",
        range = cell_limits(c(2, 1), c(NA, 2)),
        col_types = "numeric"
    ),
    read_excel(file.path(SCE, "core2024.xlsx"),
        sheet = "Data",
        range = cell_limits(c(2, 192), c(NA, 192)),
        col_types = "numeric"
    )
)))
setnames(core, c("date", "userid", "Q47"))
core <- core[!is.na(Q47), .(Q47 = as.integer(Q47)[1]), by = userid]
ca <- as.data.table(suppressMessages(
    read_excel(file.path(SCE, "ca.xlsx"),
        sheet = "Data", skip = 1,
        guess_max = 1e6
    )
))
ca <- ca[, .(userid,
    date = as.integer(date),
    weight = as.numeric(weight), N22 = as.integer(N22),
    car = as.integer(N1_5)
)]
d <- merge(ca, core, by = "userid")[Q47 %in% 1:11 & N22 %in% 1:6 &
    !is.na(car) & !is.na(weight)]
d[, grp := cut(Q47, c(0, 3, 6, 8, 11), labels = GRP)]
# Survey band -> Experian tier (approximate).
d[, tier := c(
    "subprime", "near prime", "prime", "prime",
    "super prime", "unknown"
)[N22]]
TIER_RATE <- c(
    `super prime` = 3.86, prime = 5.34, `near prime` = 9.42,
    subprime = 16.32
)   # Experian, loans still owed, 2022

mix_for <- function(dd, label) {
    cat("\n==== Survey waves:", label, "====\n")
    cat("Share holding a car loan, by income group and tier (%):\n")
    hold <- dd[, .(
        pct = round(100 * weighted.mean(car == 1, weight)),
        n = .N
    ), by = .(grp, tier)]
    print(dcast(hold, grp ~ factor(tier, c(names(TIER_RATE), "unknown")),
        value.var = "pct"
    ))
    h <- dd[car == 1]
    cat(
        "Car loan holders:", nrow(h), "; score unknown:",
        round(100 * h[, weighted.mean(tier == "unknown", weight)], 1),
        "% (dropped)\n"
    )
    h <- h[tier != "unknown"]
    m <- h[, .(w = sum(weight), n = .N), by = .(grp, tier)][
        , share := w / sum(w),
        by = grp
    ]
    m[, rate := TIER_RATE[tier]]
    pred <- m[order(rate), .(
        n_holders = sum(n),
        pct_subprime = round(100 * sum(share[tier == "subprime"]), 1),
        pct_near = round(100 * sum(share[tier == "near prime"]), 1),
        pred_mean = round(sum(share * rate), 2),
        pred_median = rate[which(cumsum(share) >= 0.5)[1]]
    ),
    keyby = grp
    ]
    print(pred); pred[, waves := label][]
}
p22 <- mix_for(d[date %in% c(202202L, 202210L)], "2022")
p3y <- mix_for(d[date >= 202101L & date <= 202312L], "2021 to 2023")

# ---- Step 3: SCF reported rates by the same income groups ------------
cars <- list(
    bal = c("x2218", "x2318", "x2418", "x7169"),
    rate = c("x2219", "x2319", "x2419", "x7170")
)
p <- as.data.table(read_dta("data/raw/scf/p22i6.dta"))
setnames(p, tolower(names(p)))
s <- as.data.table(read_dta("data/raw/scf/rscfp2022.dta"))
setnames(s, tolower(names(s)))
p <- merge(p[, c("y1", unlist(cars)), with = FALSE],
    s[, .(y1, wgt, income)],
    by = "y1"
)
p[, impl := as.integer(y1) %% 10L]
stopifnot(identical(sort(unique(p$impl)), 1:5))
p[, grp := cut(income / 1000, c(-Inf, 30, 60, 100, Inf),
    labels = GRP,
    right = FALSE
)]
loans <- rbindlist(lapply(1:4, function(i)
    p[, .(impl, wgt, grp,
        bal = as.numeric(get(cars$bal[i])),
        r = as.numeric(get(cars$rate[i]))
    )]))[bal > 0]
loans[, rate := fifelse(r == -1, 0, r / 100)]
loans[r == 0, rate := NA_real_]
# Cut-offs halfway between Experian's tier rates: above 7.4% looks
# like near prime or worse; above 12.9% looks like subprime.
scf <- loans[, .(
    n = .N, mean = weighted.mean(rate, wgt, na.rm = TRUE),
    median = wq(rate, wgt),
    pct_over_7.4 = 100 * weighted.mean(rate > 7.38, wgt,
        na.rm = TRUE
    ),
    pct_over_12.9 = 100 * weighted.mean(rate > 12.87, wgt,
        na.rm = TRUE
    )
),
by = .(impl, grp)
][
    , lapply(.SD, mean),
    keyby = grp, .SDcols = !"impl"
]
cat("\n==== SCF 2022: reported car loan rates ====\n")
print(scf[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 2)
else v)])

cmp <- merge(
    p3y[, .(grp, pred_mean, pred_median,
        pred_pct_near_or_worse = pct_subprime + pct_near,
        pred_pct_subprime = pct_subprime
    )],
    scf[, .(grp,
        scf_mean = round(mean, 2),
        scf_median = round(median, 2),
        scf_pct_over_7.4 = round(pct_over_7.4, 1),
        scf_pct_over_12.9 = round(pct_over_12.9, 1)
    )],
    by = "grp"
)
cat("\n==== Predicted from tier mix (2021-23 waves) against SCF ====\n")
print(cmp)
fwrite(cmp, file.path(OUT, "car_composition_test.csv"))
fwrite(rbind(p22, p3y), file.path(OUT, "car_tier_mix_by_income.csv"))

# ---- Input for the figure: tier mix of car loan holders by income ----
# group (2021-23 waves), with each group's median household income
# among holders, interpolated within the survey's income bands ($000).
lo <- c(0, 10, 20, 30, 40, 50, 60, 75, 100, 150, 200)
hi <- c(10, 20, 30, 40, 50, 60, 75, 100, 150, 200, 300)
wq_band <- function(band, w, p = 0.5) {
    sh <- tapply(w, factor(band, 1:11), sum); sh[is.na(sh)] <- 0
    cum <- cumsum(sh) / sum(sh); k <- which(cum >= p)[1]
    below <- if (k == 1) 0 else cum[k - 1]
    lo[k] + (p - below) / (cum[k] - below) * (hi[k] - lo[k])
}
h <- d[date >= 202101L & date <= 202312L & car == 1 & tier != "unknown"]
mixout <- h[, .(w = sum(weight), n = .N), by = .(grp, tier)][
    , share := w / sum(w),
    by = grp
][
    , .(income_group = grp, tier, share, n)
]
mixout <- merge(mixout, h[, .(
    income_p50_000 = wq_band(Q47, weight),
    n_group = .N
), by = .(income_group = grp)],
by = "income_group"
)
setorder(mixout, income_group, tier)
print(mixout[, .(income_group, tier,
    share = round(share, 3), n,
    income_p50_000 = round(income_p50_000, 1), n_group
)])
fwrite(mixout, file.path(OUT, "car_tier_mix_by_income_2021_23.csv"))

# ---- 95% intervals for the lender-based rate by income group --------
# Only the tier mix is sampled (Experian's tier rates come from its
# full loan file and carry no published sampling error). Respondents
# can appear in several survey waves, so whole respondents (userid)
# are resampled with replacement within income group: 2,000 draws,
# percentile interval.
set.seed(20261112)
B <- 2000
pred_of <- function(x) x[, sum(weight * TIER_RATE[tier]) / sum(weight)]
ci <- rbindlist(lapply(GRP, function(g) {
    hg <- h[grp == g]; setkey(hg, userid)
    ids <- unique(hg$userid)
    draws <- vapply(seq_len(B), function(i) {
        pred_of(hg[.(sample(ids, length(ids), replace = TRUE)),
            allow.cartesian = TRUE
        ])
    }, numeric(1))
    data.table(
        income_group = g, pred_mean = pred_of(hg),
        lo = quantile(draws, 0.025), hi = quantile(draws, 0.975),
        n_holders = nrow(hg), n_respondents = length(ids),
        draws = B
    )
}))
print(ci[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 2)
else v)])
fwrite(ci, file.path(OUT, "car_rate_by_income_ci_2021_23.csv"))

# ---- Finer income groups: the survey's own income bands --------------
# Same calculation for each of the 11 income bands (FINE = band index
# -> group). The two lowest bands are pooled (under $20k) because few
# low-income respondents hold a car loan.
fine_lab <- c(
    "Under $20k", "Under $20k", "$20k-$30k", "$30k-$40k",
    "$40k-$50k", "$50k-$60k", "$60k-$75k", "$75k-$100k",
    "$100k-$150k", "$150k-$200k", "Over $200k"
)
h[, fine := factor(fine_lab[Q47], unique(fine_lab))]
cat("\nCar loan holders by raw income band (2021-23):\n")
print(h[, .N, keyby = Q47])
set.seed(20261112)
fine <- rbindlist(lapply(levels(h$fine), function(g) {
    hg <- h[fine == g]; setkey(hg, userid)
    ids <- unique(hg$userid)
    draws <- vapply(seq_len(B), function(i) {
        pred_of(hg[.(sample(ids, length(ids), replace = TRUE)),
            allow.cartesian = TRUE
        ])
    }, numeric(1))
    data.table(
        income_group = g, pred_mean = pred_of(hg),
        lo = quantile(draws, 0.025), hi = quantile(draws, 0.975),
        pct_subprime = 100 * hg[, sum(weight[tier == "subprime"]) /
            sum(weight)],
        income_p50_000 = wq_band(hg$Q47, hg$weight),
        n_holders = nrow(hg), n_respondents = length(ids),
        draws = B
    )
}))
print(fine[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 1)
else v)])
fwrite(fine, file.path(OUT, "car_rate_by_income_fine_2021_23.csv"))
