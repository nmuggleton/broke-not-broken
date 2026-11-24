# =====================================================================
# 4_structural/figure_3/main.R
#
# Figure 3. Price of credit by income in the United States, two
# panels. Run from the repository root.
#
#   A  One point per product.
#      x: median income of the families who use the product (SCF
#         2022), in dollars on the same log scale as panel B. The
#         dashed line is the median income of all US families. (The
#         ratio of the two is kept in the output CSV as rel_inc.)
#      y: typical APR on debt still owed in 2022, from external
#         sources (data/raw/external_apr/apr_sources.csv). The axis is
#         linear and broken between 20% and the payday point.
#      Personal loans: friend, relative and ex-spouse loans dropped;
#         store or dealer credit dropped; the remaining lenders pooled
#         into one point. No external rate exists for personal loans
#         still owed, so the point uses the Fed's rate on new 24-month
#         bank loans and is drawn open (provisional).
#   B  Car loans, one line: the rate lenders charge, by household
#      income group. For each income group, Experian's rate on loans
#      still owed in each credit tier (2022) is weighted by the mix of
#      credit tiers among that group's car loan holders (New York Fed
#      SCE, 2021 to 2023 pooled), for ten income groups. Each point
#      sits at the group's median household income among car loan
#      holders, with a 95% interval from resampling respondents.
#   No APR in either panel comes from the SCF. The SCF supplies only
#   the income of each product's users in panel A. (SCF self-reported
#   car loan rates agree with panel B to within a point; that check is
#   in car_loan_rates.R.)
#
# Variables: yy1 (family id), impl (implicate 1-5), wgt (family
# weight), income (family income), use_<product> (0/1 owes on the
# product), rel_inc (relative income of users), apr (typical APR, %),
# status (sourced or provisional), income_group, tier (credit tier),
# share (tier's share of the income group's car loan holders), rate
# (Experian tier rate, %), income_p50_000 (median income, $000).
# =====================================================================

rm(list = ls())
suppressMessages({
    library(data.table); library(ggplot2); library(ggrepel)
    library(patchwork); library(haven)
})
source("R/utilities/4_structural/fig3/load_scf_credit_products.R")

SCF_DIR <- "data/raw/scf"
EXT_DIR <- "data/raw/external_apr"

wtd_q <- function(x, w, p = 0.5) {
    ok <- !is.na(x) & !is.na(w); x <- x[ok]; w <- w[ok]
    o <- order(x); x <- x[o]; w <- w[o]
    x[which(cumsum(w) / sum(w) >= p)[1]]
}
PCT   <- scales::label_percent(accuracy = 1, scale = 1)        # 5%
USD_K <- scales::label_dollar(suffix = "k", accuracy = 1)      # $50k
# Figure style: Arial, all text between 5 and 7 pt, fonts
# embedded, 183 mm wide. base_size 7 gives 7 pt axis titles and 5.6 pt
# tick and legend text; tags are held at 7 pt.
BASE  <- theme_classic(base_size = 7, base_family = "Arial")
TAG   <- theme(
    plot.tag = element_text(face = "bold", size = 7),
    legend.key.size = unit(8, "pt"),
    legend.margin = margin(0, 0, 0, 0),
    legend.box.spacing = unit(4, "pt")
)

# ---------------------------------------------------------------------
# 1. SCF: who uses each product (x-axis of panel A).
# ---------------------------------------------------------------------
scf <- load_scf_credit_products(
    p_path     = file.path(SCF_DIR, "p22i6.dta"),
    rscfp_path = file.path(SCF_DIR, "rscfp2022.dta"),
    rw_path    = file.path(SCF_DIR, "p22_rw1.dta")
)
hh <- copy(scf$hh)
stopifnot(identical(sort(unique(hh$impl)), 1:5))   # all five implicates

# Personal loans: every lender except informal (friend, relative,
# ex-spouse) and store or dealer.
pool <- paste0("use_personal_", c(
    "bank", "finance", "mortgage_co",
    "other"
))
hh[, use_personal_pool := as.integer(rowSums(.SD) > 0), .SDcols = pool]

PRODUCTS <- c(
    "mortgage", "vehicle", "student", "card", "payday",
    "personal_pool"
)
# Relative income of users: within implicate, then mean of the five.
rel <- rbindlist(lapply(PRODUCTS, function(p) {
    u <- paste0("use_", p)
    hh[, .(
        rel = wtd_q(income[get(u) == 1], wgt[get(u) == 1]) /
            wtd_q(income, wgt),
        inc = wtd_q(income[get(u) == 1], wgt[get(u) == 1]) / 1000,
        n = sum(get(u) == 1)
    ), by = impl][
        , .(
            product = p, rel_inc = mean(rel), inc_users = mean(inc),
            n_users = floor(mean(n))
        )
    ]
}))
med_all <- hh[, .(m = wtd_q(income, wgt) / 1000), by = impl][, mean(m)]
X_INC <- scale_x_log10(
    breaks = c(20, 100, 200), limits = c(20, 200),
    labels = USD_K
)

# ---------------------------------------------------------------------
# 2. External typical APR (y-axis of panel A). `variant` picks the row
#    of apr_sources.csv for each product.
# ---------------------------------------------------------------------
src <- fread(file.path(EXT_DIR, "apr_sources.csv"))
pick <- data.table(
    product = PRODUCTS,
    src_product = c(
        "mortgage", "vehicle", "student", "card", "payday",
        "personal"
    ),
    variant = c(
        "loans still owed (median)",
        "loans still owed (prime tier; holds the median dollar)",
        "federal undergraduate",
        "accounts assessed interest",
        "typical storefront loan",
        "24-month bank loan"
    ),
    status = c(rep("sourced", 5), "provisional")
)
apr <- merge(pick, src[, .(
    src_product = product, variant,
    apr = suppressWarnings(as.numeric(apr)),
    source
)],
by = c("src_product", "variant")
)
stopifnot(nrow(apr) == length(PRODUCTS), !anyNA(apr$apr))
fig <- merge(rel, apr[, .(product, apr, status, source)], by = "product")
fig[, label := c(
    mortgage = "Home mortgage", vehicle = "Car loan",
    student = "Student loan",
    card = "Credit card balance", payday = "Payday loan",
    personal_pool = "Personal loan"
)[product]]
print(fig[order(rel_inc), .(label, n_users,
    rel_inc = round(rel_inc, 2),
    inc_users = round(inc_users), apr, status
)])
cat("Median income, all families ($000):", round(med_all, 1), "\n")

# Descriptive correlations only (no regression).
cc <- function(d) round(c(
    pearson = cor(d$rel_inc, d$apr),
    spearman = cor(d$rel_inc, d$apr,
        method = "spearman"
    )
), 2)
cat("All six points:        "); print(cc(fig))
cat("Excluding payday:      "); print(cc(fig[product != "payday"]))
cat("Five sourced points:   "); print(cc(fig[status == "sourced"]))

# ---------------------------------------------------------------------
# 3. Panel A.
# ---------------------------------------------------------------------
layer_a <- function(d) {
    ggplot(d, aes(inc_users, apr)) +
        geom_vline(
            xintercept = med_all, linewidth = 0.2,
            colour = "grey50", linetype = "dashed"
        ) +
        geom_point(aes(shape = status), size = 1.8, fill = "white") +
        geom_text_repel(aes(label = label),
            size = 2, seed = 3,   # 5.7 pt
            family = "Arial",
            box.padding = 0.5, point.padding = 0.3,
            min.segment.length = 0.2, segment.size = 0.2,
            segment.colour = "grey40", max.overlaps = Inf
        ) +
        scale_shape_manual(
            values = c(sourced = 16, provisional = 21), name = NULL,
            limits = c("sourced", "provisional"), drop = FALSE,
            labels = c(
                sourced = "Rate on debt still owed",
                provisional = "Rate on new bank loans only"
            )
        ) +
        X_INC +
        BASE +
        TAG +
        theme(legend.position = "bottom")
}
a_top <- layer_a(fig[apr > 100]) +
    scale_y_continuous(
        limits = c(370, 410), breaks = c(380, 400),
        labels = PCT
    ) +
    labs(x = NULL, y = NULL, tag = "A") +
    theme(
        axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.line.x = element_blank(), legend.position = "none"
    )
a_bot <- layer_a(fig[apr <= 100]) +
    scale_y_continuous(
        limits = c(0, 20), breaks = seq(0, 20, 5),
        labels = PCT
    ) +
    labs(
        x = "Household income",
        y = "Typical interest rate (APR)"
    )

# ---------------------------------------------------------------------
# 4. Panel B: car loan rate by household income, from lender data.
# ---------------------------------------------------------------------
# 4a. Experian: rate on loans still owed in 2022 by tier. Tier rate =
#     mean of Q4 2018-2022 rates, new and used blended by each tier's
#     share of new and used lending (equal dollars assumed).
er <- fread(file.path(EXT_DIR, "experian_tier_rates.csv"))
tiers <- c(
    "super prime", "prime", "near prime", "subprime",
    "deep subprime"
)
new_sh  <- c(29.86, 51.46, 13.16, 5.26, 0.25)   # 2022 Q4 report p17
used_sh <- c(12.92, 43.18, 21.34, 19.59, 2.97)  # p35
bal_sh  <- c(28.09, 41.79, 15.63, 11.54, 2.95)  # p45, balances owed
p_new <- setNames(new_sh / (new_sh + used_sh), tiers)
car_t <- er[, .(apr = mean(apr)), by = .(tier, vehicle)][
    , .(rate = sum(apr * ifelse(vehicle == "new", p_new[tier],
        1 - p_new[tier]
    ))),
    by = tier
]
car_t[, bal := setNames(bal_sh, tiers)[tier]]
# The income survey has one score band below 620, so subprime and deep
# subprime are pooled, weighted by balances owed.
car_t[tier == "deep subprime", tier := "subprime"]
car_t <- car_t[, .(rate = weighted.mean(rate, bal)), by = tier]

# 4b. Mix of credit tiers among car loan holders in each income group
#     (survey score bands matched to Experian's tiers: below 620
#     subprime, 620-679 near prime, 680-760 prime, above 760 super
#     prime; 2.4% of holders do not know their score and are left out).
mix <- fread(file.path(EXT_DIR, "car_tier_mix_by_income_2021_23.csv"))
GRP <- c("Under $30k", "$30k-$60k", "$60k-$100k", "Over $100k")
mix[, income_group := factor(income_group, GRP)]
stopifnot(all(abs(mix[, sum(share), by = income_group]$V1 - 1) < 1e-9))
mix <- merge(mix, car_t, by = "tier")
stopifnot(!anyNA(mix$rate))
car_b <- mix[, .(
    y = sum(share * rate),
    pct_subprime = 100 * sum(share[tier == "subprime"]),
    inc = income_p50_000[1], n_holders = n_group[1]
),
keyby = income_group
]
print(car_t[, .(tier, rate = round(rate, 2))])
print(car_b[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 1)
else v)])

# 4c. 95% intervals from resampling survey respondents within income
#     group (2,000 draws; car_loan_rates.R). They cover the
#     tier mix only, not Experian's tier rates or the tier matching.
SHOW_CI_B <- TRUE
ci <- fread(file.path(EXT_DIR, "car_rate_by_income_ci_2021_23.csv"))
car_b <- merge(car_b, ci[, .(
    income_group = factor(income_group, GRP),
    pred_mean, lo, hi
)], by = "income_group")
stopifnot(all(abs(car_b$y - car_b$pred_mean) < 0.01))

# 4d. Drawn: the same calculation for ten finer income groups (the
#     survey's own income bands, the two lowest pooled), from
#     car_loan_rates.R. The four-group version above is kept
#     as a check that this script's tier rates match that file's.
#     The top group (over $200k) is placed at $250k.
car_f <- fread(file.path(EXT_DIR, "car_rate_by_income_fine_2021_23.csv"))
setnames(car_f, c("pred_mean", "income_p50_000"), c("y", "inc"))
print(car_f[, .(income_group,
    y = round(y, 1), lo = round(lo, 1),
    hi = round(hi, 1), inc, n_holders
)])
# The APR axis covers the range of the estimates (it does not start at
# zero and does not match panel A's 0 to 20%).
Y_B <- c(4, 12)

# Income groups are categories on the vertical axis, lowest income at
# the top; the rate runs along the horizontal axis. Points only, no
# connecting line.
car_f[, income_group := factor(income_group, rev(income_group))]
panel_b <- ggplot(car_f, aes(y, income_group)) +
    {
        if (SHOW_CI_B) geom_linerange(aes(xmin = lo, xmax = hi),
            linewidth = 0.25
        )
    } +
    geom_point(size = 1.8, shape = 16) +
    scale_x_continuous(
        limits = Y_B, breaks = seq(Y_B[1], Y_B[2], 2),
        labels = PCT
    ) +
    labs(x = "Car loan rate (APR)", y = "Household income") +
    BASE +
    TAG

# ---------------------------------------------------------------------
# 5. Assemble. Panel A is two stacked segments (the broken APR axis);
#    panel B spans the full height of both.
# ---------------------------------------------------------------------
figure <- a_top + a_bot + (panel_b + labs(tag = "B")) +
    plot_layout(design = "AC\nBC", heights = c(1, 5))
# No outer margin, so the file is the size given here. The gap
# between the two segments of panel A (the axis break) is kept.
figure[[1]] <- figure[[1]] + theme(plot.margin = margin(0, 4, 5, 0))
figure[[2]] <- figure[[2]] + theme(plot.margin = margin(5, 4, 0, 0))
figure[[3]] <- figure[[3]] + theme(plot.margin = margin(0, 0, 0, 4))
dir.create("figures", showWarnings = FALSE)
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
ggsave(
    "figures/figure_3.pdf",
    figure,
    width  = 183,
    height = 95,
    units  = "mm",
    device = cairo_pdf     # embeds the fonts
)
fwrite(fig, "data/processed/credit_products_a.csv")
fwrite(car_f, "data/processed/credit_products_b.csv")
