# =====================================================================
# 4_structural/text/main.R
#
# Within-country patience and income (Section 4.1): within
# countries, are poorer people less patient, and is that gap larger
# than the gap between rich and poor countries (an
# Easterlin-style comparison)? Step 4 adds whether patience predicts
# financial hardship once income is held fixed. Step 5 replicates
# Falk et al.'s (2018) estimates of patience and saving and sets
# income against them.
#
# Run from the repository root, top to bottom; each step prints its
# results.
#
#   Step 1  Merge the GPS individual file to the Gallup World Poll
#           (GWP) 2012 wave on the Gallup respondent id.
#   Step 2  Patience by within-country income quintile, with country
#           fixed effects.
#   Step 3  Within- against between-country income slopes of
#           patience (Mundlak decomposition).
#   Step 4  Patience and income as predictors of hardship, within
#           countries.
#   Step 5  Patience and saving: Falk et al. (2018, Table 7)
#           replicated in the 14 countries asked about saving, with
#           income quintiles as a contrast, overall and by country.
#
# Data (see data/raw/SOURCES.txt):
#   data/raw/GPS_dataset_individual_level/individual_new.dta
#   data/raw/gwp/gwp_2024_with_label.csv   (Gallup World Poll,
#       cumulative file, 11.8 GB; institutional licence)
#   data/raw/wdi/API_SP.POP.TOTL_DS2_en_csv_v2_*.csv   (World Bank,
#       total population)
#
# Writes:
#   data/raw/gwp/gwp2012_gps_extract.csv   (respondent-level GWP
#       extract; kept in data/raw because data/raw is git-ignored and
#       Gallup microdata must not be committed)
#   data/processed/within_country_results.csv   (estimates only)
#   data/processed/within_country_step3_countries.csv
#     (country means and slopes, Step 3)
#   figures/diagnostics/within_country_step{2,3,4}.pdf
#   figures/diagnostics/within_country_step5_by_country.pdf
# =====================================================================

rm(list = ls())

library(data.table)
library(haven)
library(ggplot2)
library(sandwich)

GPS_PATH      <- "data/raw/GPS_dataset_individual_level/individual_new.dta"
GWP_FULL_PATH <- "data/raw/gwp/gwp_2024_with_label.csv"
GWP_EXTRACT   <- "data/raw/gwp/gwp2012_gps_extract.csv"
WDI_POP_PATH  <- Sys.glob(
    "data/raw/wdi/API_SP.POP.TOTL_DS2_en_csv_v2_*.csv"
)
stopifnot(length(WDI_POP_PATH) == 1)

# GWP columns kept. Labels as in the Gallup codebook.
GWP_KEEP <- c(
    "wave", "wpid", "wgt", "FIELD_DATE", "COUNTRY_ISO3",
    "INCOME_1",   # annual household income, local currency
    "INCOME_2",   # annual household income, international dollars
    "INCOME_3",   # per capita income, local currency
    "INCOME_4",   # per capita income, international dollars
    "INCOME_5",   # per capita income quintile, within country
    "INCOME_7",   # how income was obtained (reported or imputed)
    "WP1219",     # sex
    "WP1220",     # age
    "WP3117",     # education, three levels
    "EMP_2010",   # employment status
    "WP40",       # not enough money for food, past 12 months
    "WP43",       # not enough money for shelter, past 12 months
    "WP2319",     # feelings about household income
    "WP7297",     # able to save any money this past year (Step 5)
    "WP1233"      # religion (Step 5 control)
)

# ---------------------------------------------------------------------
# Helpers: weighted OLS with country-clustered standard errors.
# ---------------------------------------------------------------------

# Fit weighted OLS on complete cases, with clustered SEs.
fit_cl <- function(formula, data, weighted = TRUE) {
    vars <- unique(c(all.vars(formula), "wgt", "iso"))
    dd <- data[stats::complete.cases(data[, ..vars])]
    fit <- if (weighted) lm(formula, data = dd, weights = wgt) else
        lm(formula, data = dd)
    vc  <- vcovCL(fit, cluster = dd$iso, type = "HC1")
    list(
        fit = fit, vcov = vc, n = nrow(dd),
        n_clusters = uniqueN(dd$iso), data = dd
    )
}

# Coefficients, clustered SEs, and 95% intervals for chosen terms.
tidy_cl <- function(m, terms, model) {
    est <- coef(m$fit)[terms]
    se  <- sqrt(diag(m$vcov))[terms]
    data.table(
        model = model, term = terms, estimate = est, se = se,
        lo = est - 1.96 * se, hi = est + 1.96 * se,
        p = 2 * pnorm(-abs(est / se)), n = m$n,
        clusters = m$n_clusters
    )
}

# Weighted mean by group, as a named vector.
wmean_by <- function(x, w, g) {
    d <- data.table(x, w, g)[!is.na(x)]
    out <- d[, .(m = sum(w * x) / sum(w)), by = g]
    setNames(out$m, out$g)
}

results <- list()

# =====================================================================
# STEP 1. Merge GPS to GWP 2012.
# =====================================================================

gps <- as.data.table(read_dta(GPS_PATH, encoding = "latin1"))
# id_gallup is stored as a double; 13 digits, so exact as a string.
gps[, wpid := sprintf("%.0f", as.numeric(id_gallup))]
stopifnot(!anyDuplicated(gps$wpid), all(nchar(gps$wpid) == 13))

# Read the cached extract if present; otherwise pull
# the kept columns from the 11.8 GB cumulative file (one fread()
# pass, several minutes) and keep the GPS respondents. The cache
# holds 80,337 rows. Delete the file to rebuild it here.
if (file.exists(GWP_EXTRACT)) {
    gwp <- fread(GWP_EXTRACT,
        colClasses = "character",
        encoding = "UTF-8"
    )
} else {
    message(
        "Building ", GWP_EXTRACT, " from ", GWP_FULL_PATH,
        ". This reads the whole file and takes several minutes."
    )
    gwp <- fread(GWP_FULL_PATH,
        select = GWP_KEEP,
        colClasses = "character", encoding = "UTF-8"
    )
    gwp <- gwp[wpid %in% gps$wpid]
    fwrite(gwp, GWP_EXTRACT)
}
stopifnot(setequal(names(gwp), GWP_KEEP), !anyDuplicated(gwp$wpid))

d <- merge(gps, gwp, by = "wpid", all.x = TRUE)
match_rate <- mean(!is.na(d$COUNTRY_ISO3))
message(sprintf(
    "Step 1: %d of %d GPS respondents matched (%.2f%%).",
    sum(!is.na(d$COUNTRY_ISO3)), nrow(d),
    100 * match_rate
))
if (match_rate < 0.99) {
    stop("Fewer than 99% of GPS respondents matched to the GWP.")
}

# Checks that the match is the same person, not just the same id.
chk <- d[!is.na(COUNTRY_ISO3), .(
    weight = max(abs(wgt.x - as.numeric(wgt.y))),
    country = mean(isocode == COUNTRY_ISO3),
    sex = mean((gender == 1) == (WP1219 == "Female"), na.rm = TRUE),
    age = mean(abs(age - suppressWarnings(as.numeric(WP1220))) <= 1,
        na.rm = TRUE
    )
)]
print(chk)
if (chk$weight > 1e-8 || chk$country < 1 || chk$sex < 0.99 ||
    chk$age < 0.99) {
    stop(
        "GPS and GWP records disagree on weight, country, sex, or ",
        "age for matched respondents."
    )
}
d[, wgt.y := NULL]
setnames(d, "wgt.x", "wgt")      # GPS weight (= GWP)

# - recodes ----------------------------------------------------------
d[, iso := isocode]
d[, female := as.integer(gender == 1)]
d[, inc_q := factor(INCOME_5, levels = c(
    "Poorest 20%", "Second 20%", "Middle 20%", "Fourth 20%",
    "Richest 20%"
))]
d[, inc_pc := suppressWarnings(as.numeric(INCOME_4))]
d[, inc_hh := suppressWarnings(as.numeric(INCOME_2))]
# Log per capita income, international dollars. Zero
# incomes (733 respondents) have no log and are dropped from the log
# specifications only; they stay in the quintile models.
d[, log_inc := fifelse(inc_pc > 0, log(inc_pc), NA_real_)]
d[, inc_imputed := INCOME_7 == "Imputed without categorical response"]
d[, educ := factor(
    fcase(
        grepl("^Completed elementary", WP3117), "Primary or less",
        grepl("^Secondary", WP3117), "Secondary",
        grepl("^Completed four years", WP3117), "Tertiary",
        default = NA_character_
    ),
    levels = c("Primary or less", "Secondary", "Tertiary")
)]
yes_no <- function(x) fcase(x == "Yes", 1L, x == "No", 0L,
    default = NA_integer_
)
d[, food := yes_no(WP40)]
d[, shelter := yes_no(WP43)]
d[, difficult := fcase(
    WP2319 %in% c(
        "Finding it difficult on present income",
        "Finding it very difficult on present income"
    ), 1L,
    WP2319 %in% c(
        "Getting by on present income",
        "Living comfortably on present income"
    ), 0L,
    default = NA_integer_
)]
d <- d[!is.na(patience)]

# Coverage by variable: countries where a variable is entirely
# missing drop out of the models that use it.
cov <- d[, .(
    quintile = mean(!is.na(inc_q)), food = mean(!is.na(food)),
    shelter = mean(!is.na(shelter)),
    difficult = mean(!is.na(difficult))
), by = iso]
message("Countries with no data on each variable:")
print(sapply(cov[, -"iso"], function(x) paste(cov$iso[x == 0],
    collapse = " "
)))

# =====================================================================
# STEP 2. Patience by within-country income quintile.
# =====================================================================

# Share of the variance in patience that lies between countries.
r2_country <- summary(lm(patience ~ factor(iso),
    data = d,
    weights = wgt
))$r.squared
message(sprintf(
    paste0(
        "Step 2: countries account for %.1f%% of the ",
        "variance in individual patience."
    ),
    100 * r2_country
))

q_terms <- paste0("inc_q", levels(d$inc_q)[-1])
# Age, age squared, and sex; education in a second
# model. Robustness: drop respondents whose income is wholly imputed.
m2a <- fit_cl(patience ~ inc_q + age + I(age^2) + female +
    factor(iso), d)
m2b <- fit_cl(patience ~ inc_q + educ + age + I(age^2) + female +
    factor(iso), d)
m2c <- fit_cl(patience ~ inc_q + age + I(age^2) + female +
    factor(iso), d[inc_imputed == FALSE])
step2 <- rbind(
    tidy_cl(m2a, q_terms, "2a: age, sex"),
    tidy_cl(m2b, q_terms, "2b: + education"),
    tidy_cl(m2c, q_terms, "2c: reported income only")
)
step2[, term := sub("^inc_q", "", term)]
print(step2[, .(model, term,
    estimate = round(estimate, 3),
    lo = round(lo, 3), hi = round(hi, 3), n, clusters
)])
results$step2 <- step2

p2 <- ggplot(
    step2[model != "2c: reported income only"],
    aes(
        x = factor(term, levels = levels(d$inc_q)[-1]),
        y = estimate, colour = model
    )
) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3) +
    geom_errorbar(aes(ymin = lo, ymax = hi),
        width = 0.15,
        linewidth = 0.3,
        position = position_dodge(width = 0.4)
    ) +
    geom_point(position = position_dodge(width = 0.4), size = 1.8) +
    labs(
        x = "Income quintile (vs poorest 20%)",
        y = "Difference in patience from the poorest 20% (SD)",
        colour = NULL
    ) +
    coord_flip() +
    theme_classic(base_size = 9) +
    theme(legend.position = "bottom")
print(p2)

# =====================================================================
# STEP 3. Within- against between-country income slopes.
# =====================================================================

# Mundlak decomposition: log income split into the country mean and
# the deviation from it. The deviation's slope is the within-country
# income gradient; the mean's slope is the between-country gradient.
# The country mean of the same log per capita income,
# weighted. GDP per capita (as in Falk et al.) would need a new
# source (World Bank WDI).
cm <- wmean_by(d$log_inc, d$wgt, d$iso)
d[, log_inc_cm := cm[iso]]
d[, log_inc_dm := log_inc - log_inc_cm]
m3 <- fit_cl(patience ~ log_inc_dm + log_inc_cm + age + I(age^2) +
    female, d)
step3 <- tidy_cl(m3, c("log_inc_dm", "log_inc_cm"), "3: Mundlak")
step3[, term := c(
    "within (deviation from country mean)",
    "between (country mean)"
)]
# The within slope again, from a country fixed-effects model, as a
# check (it should equal the Mundlak within slope).
m3fe <- fit_cl(patience ~ log_inc + age + I(age^2) + female +
    factor(iso), d)
step3 <- rbind(step3, tidy_cl(m3fe, "log_inc", "3: fixed effects")[
    , term := "within (country fixed effects)"
])
# Difference between the slopes (between minus within), with its SE.
dif_vec <- c(log_inc_dm = -1, log_inc_cm = 1)
dif_est <- sum(coef(m3$fit)[names(dif_vec)] * dif_vec)
dif_se  <- sqrt(as.numeric(t(dif_vec) %*%
    m3$vcov[names(dif_vec), names(dif_vec)] %*%
    dif_vec))
step3 <- rbind(step3, data.table(
    model = "3: Mundlak", term = "between minus within",
    estimate = dif_est, se = dif_se, lo = dif_est - 1.96 * dif_se,
    hi = dif_est + 1.96 * dif_se, p = 2 * pnorm(-abs(dif_est / dif_se)),
    n = m3$n, clusters = m3$n_clusters
))
print(step3[, .(term,
    estimate = round(estimate, 3), lo = round(lo, 3),
    hi = round(hi, 3), p = signif(p, 2)
)])
results$step3 <- step3

# Figure, after Stevenson and Wolfers' life-satisfaction chart: one
# dot per country at its mean income and mean patience, with a short
# line through the dot whose angle is that country's own slope of
# patience on log income (no controls).
# Every line has the same, short length on the page
# (SEG_HALF_LENGTH as a share of the panel width), so only position
# (the means) and angle (the gradient) carry information. The angle
# is computed in page units, which is why the axis limits and the
# panel aspect ratio are fixed below. The panel is taller than it is
# wide (ASPECT), as agreed at the 10 Nov meeting.
SEG_HALF_LENGTH <- 0.02
ASPECT <- 1.1
ctry <- d[!is.na(log_inc),
    {
        w <- wgt / sum(wgt)
        fit <- lm(patience ~ log_inc, weights = wgt)
        .(
            country = country[1],
            patience = sum(w * patience), log_inc = sum(w * log_inc),
            slope = unname(coef(fit)[2]),
            slope_se = sqrt(vcovHC(fit, type = "HC1")[2, 2])
        )
    },
    by = iso
]

# The average within-country line is the mean of the
# country slopes weighted by each country's 2012 population (World
# Bank), so that large countries count for more than small ones
# (sample sizes are about 1,000 almost everywhere, so weighting by
# sample size would be close to equal weighting). It is drawn through
# the population-weighted mean of the country means. Its SE treats
# the country slopes as independent.
pop <- fread(WDI_POP_PATH,
    skip = 4, header = TRUE,
    select = c("Country Code", "2012")
)
setnames(pop, c("iso", "pop2012"))
ctry <- merge(ctry, pop, by = "iso", all.x = TRUE)
if (anyNA(ctry$pop2012)) {
    stop(
        "No 2012 World Bank population for: ",
        paste(ctry[is.na(pop2012), iso], collapse = ", ")
    )
}
ctry[, w_pop := pop2012 / sum(pop2012)]
within_pop <- ctry[, sum(w_pop * slope)]
within_pop_se <- ctry[, sqrt(sum(w_pop^2 * slope_se^2))]
step3 <- rbind(step3, data.table(
    model = "3: country slopes",
    term = "within (population-weighted mean of country slopes)",
    estimate = within_pop, se = within_pop_se,
    lo = within_pop - 1.96 * within_pop_se,
    hi = within_pop + 1.96 * within_pop_se,
    p = 2 * pnorm(-abs(within_pop / within_pop_se)),
    n = sum(!is.na(d$log_inc)), clusters = nrow(ctry)
))
results$step3 <- step3
message(sprintf(
    paste0(
        "Step 3: population-weighted mean of country ",
        "slopes %.3f (95%% CI %.3f to %.3f)."
    ),
    within_pop, within_pop - 1.96 * within_pop_se,
    within_pop + 1.96 * within_pop_se
))

# The two lines drawn across the figure.
between_fit <- coef(lm(patience ~ log_inc, data = ctry))
lines3 <- data.table(
    line = c(
        "Between countries",
        "Within countries (population-weighted average)"
    ),
    slope = c(between_fit[["log_inc"]], within_pop),
    intercept = c(
        between_fit[["(Intercept)"]],
        ctry[, sum(w_pop * patience)] -
            within_pop * ctry[, sum(w_pop * log_inc)]
    )
)

inc_breaks <- c(250, 500, 1000, 2000, 4000, 8000, 16000, 32000)
x_lim <- range(ctry$log_inc) + c(-0.3, 0.3)
y_lim <- range(ctry$patience) + c(-0.1, 0.1)
# Direction of each line in page units (panel width 1, height
# ASPECT), scaled to SEG_HALF_LENGTH, then converted back to data
# units.
ctry[, `:=`(ux = 1 / diff(x_lim), uy = slope * ASPECT / diff(y_lim))]
ctry[, len := sqrt(ux^2 + uy^2)]
ctry[, `:=`(
    dx = SEG_HALF_LENGTH * ux / len * diff(x_lim),
    dy = SEG_HALF_LENGTH * uy / len * diff(y_lim) / ASPECT
)]
p3 <- ggplot(ctry, aes(x = log_inc, y = patience)) +
    geom_abline(
        data = lines3,
        aes(
            intercept = intercept, slope = slope,
            linetype = line
        ), linewidth = 0.5
    ) +
    geom_segment(
        aes(
            x = log_inc - dx, xend = log_inc + dx,
            y = patience - dy, yend = patience + dy
        ),
        colour = "grey50", linewidth = 0.8
    ) +
    geom_point(size = 1) +
    ggrepel::geom_text_repel(aes(label = country),
        size = 2,
        colour = "grey25", seed = 1,
        max.overlaps = 12, segment.size = 0.15,
        min.segment.length = 0.3
    ) +
    scale_linetype_manual(
        values = c(
            "Between countries" = "dashed",
            "Within countries (population-weighted average)" =
                "solid"
        ),
        name = NULL
    ) +
    scale_x_continuous(
        breaks = log(inc_breaks),
        labels = scales::dollar(inc_breaks)
    ) +
    coord_cartesian(xlim = x_lim, ylim = y_lim, expand = FALSE) +
    labs(
        x = paste0(
            "Per capita household income, country mean ",
            "(international $, log scale)"
        ),
        y = "Mean patience (SD)"
    ) +
    theme_classic(base_size = 9) +
    theme(legend.position = "bottom", aspect.ratio = ASPECT) +
    guides(linetype = guide_legend(nrow = 2))
print(p3)

# =====================================================================
# STEP 4. Patience, income, and hardship within countries.
# =====================================================================

# Three binary outcomes, linear probability models (so
# coefficients are in percentage points): not enough money for food
# (WP40) or shelter (WP43) in the past 12 months, and finding it
# difficult or very difficult to live on present income (WP2319).
# For each outcome, the three models use the same
# respondents, so their coefficients and R-squared are comparable.
step4 <- list()
for (y in c("food", "shelter", "difficult")) {
    vars <- c(y, "patience", "inc_q", "age", "female", "iso", "wgt")
    dd <- d[stats::complete.cases(d[, ..vars])]
    base <- paste(y, "~ age + I(age^2) + female + factor(iso)")
    mA <- fit_cl(as.formula(paste(base, "+ patience")), dd)
    mB <- fit_cl(as.formula(paste(base, "+ inc_q")), dd)
    mC <- fit_cl(as.formula(paste(base, "+ patience + inc_q")), dd)
    mD <- fit_cl(as.formula(base), dd)
    r2 <- sapply(
        list(D = mD, A = mA, B = mB, C = mC),
        function(m) summary(m$fit)$r.squared
    )
    step4[[y]] <- rbind(
        tidy_cl(mA, "patience", "A: patience"),
        tidy_cl(mB, "inc_qRichest 20%", "B: income"),
        tidy_cl(
            mC, c("patience", "inc_qRichest 20%"),
            "C: patience + income"
        )
    )[, outcome := y][
        , `:=`(
            r2_patience_given_income = r2["C"] - r2["B"],
            r2_income_given_patience = r2["C"] - r2["A"]
        )
    ]
}
step4 <- rbindlist(step4)
step4[, term := fifelse(
    term == "patience", "patience (+1 SD)",
    "richest vs poorest quintile"
)]
print(step4[, .(outcome, model, term,
    pp = round(100 * estimate, 1),
    lo = round(100 * lo, 1), hi = round(100 * hi, 1), n,
    dR2_pat = round(100 * r2_patience_given_income, 2),
    dR2_inc = round(100 * r2_income_given_patience, 2)
)])
results$step4 <- step4

p4 <- ggplot(
    step4,
    aes(x = outcome, y = 100 * estimate, colour = model, shape = term)
)+
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3) +
    geom_errorbar(aes(ymin = 100 * lo, ymax = 100 * hi),
        width = 0.15,
        linewidth = 0.3,
        position = position_dodge(width = 0.4)
    ) +
    geom_point(position = position_dodge(width = 0.4), size = 1.8) +
    scale_x_discrete(
        limits = c("shelter", "food", "difficult"),
        labels = c(
            difficult = "Finding it difficult\non present income",
            food = "Not enough money\nfor food",
            shelter = "Not enough money\nfor shelter"
        )
    ) +
    scale_colour_discrete(
        name = "Model",
        labels = c(
            "A: patience" = "A: patience only",
            "B: income" = "B: income only",
            "C: patience + income" = "C: patience and income"
        )
    ) +
    scale_shape_discrete(
        name = "Coefficient",
        labels = c(
            "patience (+1 SD)" = "Patience (+1 SD)",
            "richest vs poorest quintile" =
                "Richest vs poorest fifth"
        )
    ) +
    labs(
        x = NULL,
        y = "Difference in probability (percentage points)"
    ) +
    coord_flip() +
    theme_classic(base_size = 9) +
    theme(
        legend.position = 'bottom', legend.box = "vertical",
        legend.title = element_text(face = "bold")
    ) +
    guides(
        colour = guide_legend(
            order = 1, nrow = 2,
            title.position = "top"
        ),
        shape = guide_legend(
            order = 2, nrow = 2,
            title.position = "top"
        )
    )
print(p4)

# =====================================================================
# STEP 5. Patience, income, and saving (Falk et al. 2018, Table 7).
# =====================================================================

# Falk et al. (2018, NBER WP 23943, Table 7) regress having saved in
# the past year on patience: 0.038 with country fixed effects (column
# 1; N = 15,260; R-squared 0.07) and 0.027 with subnational region
# fixed effects and controls (column 2; N = 14,459; R-squared 0.18).
# The item is Gallup's WP7297, "Were you able to save any money this
# past year?", which GPS respondents were asked in 14 countries only.
d[, saved := yes_no(WP7297)]
k_saved <- d[!is.na(saved), sort(unique(iso))]
ds <- d[iso %in% k_saved & !is.na(saved)]
message(sprintf(
    "Step 5: saving asked in %d countries (%s); N = %d.",
    length(k_saved), paste(k_saved, collapse = " "),
    nrow(ds)
))
ds[, reg := paste(iso, region)]       # subnational region (GPS)
ds[, religion := fifelse(
    WP1233 %in% c(
        "", "(DK)", "(Refused)",
        "(No response)(2011 and earlier)"
    ),
    NA_character_, WP1233
)]

# Falk et al.'s specification, not ours, so that the
# estimates can be compared: unweighted OLS (weighting gives 0.036 in
# column 1) and SEs clustered by country (14 clusters only). Their
# income control is log household income per capita (their Appendix
# J.1): log_inc here, so the 79 respondents with zero income
# drop out of column 2. They do not report how religion is coded;
# Gallup's religion item (WP1233) with non-response missing comes
# closest to their N. Column 2 then gives 0.027 (theirs 0.027), N =
# 14,419 (14,459), and R-squared 0.19 (0.18). The remaining gap may
# come from coding they do not report or from differences between
# their release of the Gallup data and ours (2024).
f5 <- list(
    col1 = saved ~ patience + factor(iso),
    col2 = saved ~ patience + age + I(age^2) + female +
        subj_math_skills + log_inc + factor(religion) + factor(reg)
)
# Income as a contrast. Gallup's within-country income
# quintiles are added to column 1 and replace log income in
# column 2, and the coefficient for the richest against the poorest
# fifth is set against that for one SD of patience. Same estimator as
# above. Each model uses the same respondents as the column it extends:
# income quintiles are never missing here, and the column 2 version
# is fitted on column 2's respondents (zero incomes excluded).
f5$col1q <- update(f5$col1, . ~ . + inc_q)
f5$col2q <- update(f5$col2, . ~ . - log_inc + inc_q)
m5 <- list(
    col1  = fit_cl(f5$col1, ds, weighted = FALSE),
    col1q = fit_cl(f5$col1q, ds, weighted = FALSE),
    col2  = fit_cl(f5$col2, ds, weighted = FALSE),
    col2q = fit_cl(f5$col2q, ds[!is.na(log_inc)], weighted = FALSE)
)
stopifnot(m5$col1$n == m5$col1q$n, m5$col2$n == m5$col2q$n)

q5 <- "inc_qRichest 20%"
lab5 <- c(
    col1 = "Falk (1): country FE",
    col1q = "Falk (1) + income quintiles",
    col2 = "Falk (2): region FE and controls",
    col2q = "Falk (2), income quintiles for log income"
)
step5 <- rbindlist(lapply(names(lab5), function(k) {
    terms <- if (grepl("q$", k)) c("patience", q5) else "patience"
    tidy_cl(m5[[k]], terms, lab5[[k]])[
        , r2 := summary(m5[[k]]$fit)$r.squared
    ]
}))
step5[, term := fifelse(
    term == "patience", "patience (+1 SD)",
    "richest vs poorest quintile"
)]
print(step5[, .(model, term,
    estimate = round(estimate, 4),
    se = round(se, 4), n, clusters, r2 = round(r2, 3)
)])
ratio5 <- step5[grepl("income quintiles", model), .(
    ratio = estimate[term == "richest vs poorest quintile"] /
        estimate[term == "patience (+1 SD)"]
), by = model]
print(ratio5)
results$step5 <- step5

# Per-country estimates, as a diagnostic (not in the
# report): column 1, and column 1 plus income quintiles, within each
# country; unweighted OLS with HC1 SEs. Falk et al. report that the
# patience coefficient is positive in more than 90% of countries.
tidy_hc <- function(fit, terms, model) {
    est <- coef(fit)[terms]
    se  <- sqrt(diag(vcovHC(fit, type = "HC1")))[terms]
    data.table(
        model = model, term = terms, estimate = est, se = se,
        lo = est - 1.96 * se, hi = est + 1.96 * se,
        p = 2 * pnorm(-abs(est / se)), n = nobs(fit)
    )
}
step5_ctry <- ds[, rbind(
    tidy_hc(lm(saved ~ patience), "patience", lab5[["col1"]]),
    tidy_hc(
        lm(saved ~ patience + inc_q), c("patience", q5),
        lab5[["col1q"]]
    )
), by = .(iso, country)]
step5_ctry[, term := fifelse(
    term == "patience", "patience (+1 SD)",
    "richest vs poorest quintile"
)]
message(sprintf(
    paste0(
        "Step 5: patience coefficient positive in %d ",
        "of %d countries (column 1)."
    ),
    step5_ctry[model == lab5[["col1"]], sum(estimate > 0)],
    length(k_saved)
))
print(step5_ctry[, .(country, model, term,
    pp = round(100 * estimate, 1),
    lo = round(100 * lo, 1), hi = round(100 * hi, 1),
    n
)])
results$step5_country <- step5_ctry

all5 <- sprintf("All %d countries", length(k_saved))
p5_dat <- rbind(
    step5[
        model %in% lab5[c("col1", "col1q")],
        .(country = all5, model, term, estimate, lo, hi)
    ],
    step5_ctry[, .(country, model, term, estimate, lo, hi)]
)
p5_dat[, country := factor(country, levels = c(
    all5, sort(unique(step5_ctry$country))
))]
p5_dat[, model := factor(model, levels = lab5[c("col1", "col1q")])]
p5 <- ggplot(p5_dat, aes(
    x = term, y = 100 * estimate,
    colour = model
)) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3) +
    geom_errorbar(aes(ymin = 100 * lo, ymax = 100 * hi),
        width = 0.2,
        linewidth = 0.3,
        position = position_dodge(width = 0.5)
    ) +
    geom_point(position = position_dodge(width = 0.5), size = 1.5) +
    facet_wrap(~ country, ncol = 5) +
    scale_x_discrete(
        limits = c("richest vs poorest quintile", "patience (+1 SD)"),
        labels = c(
            "richest vs poorest quintile" =
                "Richest vs\npoorest fifth",
            "patience (+1 SD)" = "Patience\n(+1 SD)"
        )
    ) +
    scale_colour_discrete(
        name = "Model",
        labels = setNames(
            c(
                "Patience only (Falk et al., column 1)",
                "Patience and income quintiles"
            ),
            lab5[c("col1", "col1q")]
        )
    ) +
    labs(
        x = NULL,
        y = paste(
            "Difference in the probability of having saved in",
            "the past year (percentage points)"
        )
    ) +
    coord_flip() +
    theme_classic(base_size = 9) +
    theme(
        legend.position = "bottom",
        legend.title = element_text(face = "bold"),
        strip.background = element_blank()
    )
print(p5)

# =====================================================================
# STEP 6. Saving and hardship on one specification, and a like-for-
# like contrast (Section 4.1 figure, R/4_structural/figure_4/main.R).
# =====================================================================

# Saving under the Step 4 specification, so that both
# panels of the Section 4.1 figure control for the same things:
# patience and income quintiles together, age, age squared, sex, and
# country fixed effects, weighted, SEs clustered by country.
# Step 5 keeps Falk et al.'s specification (unweighted, no controls)
# for the replication. Per-country models have the same controls,
# weighted, with HC1 SEs.
lab6a <- "6a: saving, Step 4 specification"
m6a <- fit_cl(saved ~ patience + inc_q + age + I(age^2) + female +
    factor(iso), ds)
step6 <- tidy_cl(m6a, c("patience", q5), lab6a)[, outcome := "saved"]
step6_ctry <- ds[,
    {
        fit <- lm(saved ~ patience + inc_q + age + I(age^2) + female,
            weights = wgt
        )
        tidy_hc(fit, c("patience", q5), lab6a)
    },
    by = .(iso, country)
]

# A like-for-like contrast. One SD of patience is a
# smaller step than the gap between the richest and poorest fifths of
# income, so patience is also cut into fifths within each country and
# the most patient fifth is set against the least patient. Fifths are
# by weighted rank within country; respondents with the same patience
# score share a fifth (the one holding the midpoint of their tie), so
# the fifths are not equal in size. Same specification as
# Step 4, for saving and the three hardship outcomes.
pat_fifth <- function(x, w) {
    o <- data.table(x, w)[, .(w = sum(w)), keyby = x]
    o[, mid := (cumsum(w) - w / 2) / sum(w)]
    pmin(5L, floor(5 * o$mid) + 1L)[match(x, o$x)]
}
pat_levels <- c(
    "Least patient 20%", "Second 20%", "Middle 20%",
    "Fourth 20%", "Most patient 20%"
)
d[, pat_q := factor(pat_levels[pat_fifth(patience, wgt)],
    levels = pat_levels
), by = iso]
ds[, pat_q := factor(pat_levels[pat_fifth(patience, wgt)],
    levels = pat_levels
), by = iso]
message("Step 6: weighted share of respondents in each patience fifth:")
print(d[, .(share = round(sum(wgt) / sum(d$wgt), 3)), keyby = pat_q])
lab6b <- "6b: patience fifths and income fifths"
p5q <- "pat_qMost patient 20%"
step6b <- rbindlist(lapply(
    c("saved", "food", "shelter", "difficult"), function(y) {
        dat <- if (y == "saved") ds else d
        m <- fit_cl(as.formula(paste(
            y, "~ pat_q + inc_q + age + I(age^2) + female +",
            "factor(iso)"
        )), dat)
        tidy_cl(m, c(p5q, q5), lab6b)[, outcome := y]
    }
))
step6 <- rbind(step6, step6b)
step6[, term := fcase(term == "patience", "patience (+1 SD)",
    term == p5q, "most vs least patient quintile",
    default = "richest vs poorest quintile"
)]
step6_ctry[, term := fifelse(
    term == "patience", "patience (+1 SD)",
    "richest vs poorest quintile"
)]
print(step6[, .(model, outcome, term,
    pp = round(100 * estimate, 1),
    lo = round(100 * lo, 1), hi = round(100 * hi, 1), n,
    clusters
)])
print(step6_ctry[, .(country, term,
    pp = round(100 * estimate, 1),
    lo = round(100 * lo, 1), hi = round(100 * hi, 1),
    n
)])
results$step6 <- step6
results$step6_country <- step6_ctry

# =====================================================================
# Save.
# =====================================================================
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
dir.create("figures/diagnostics",
    showWarnings = FALSE,
    recursive = TRUE
)
fwrite(
    rbindlist(results, fill = TRUE, idcol = "step"),
    "data/processed/within_country_results.csv"
)
# Country means and slopes behind the Step 3 figure, for panel C of
# the Section 4.1 figure (R/4_structural/figure_4/main.R). One row
# per country: iso, country, patience (weighted mean, SD units),
# log_inc (weighted mean of log per capita income), slope and
# slope_se (patience on log income within the country, no controls),
# pop2012 and w_pop (2012 population and its share).
fwrite(
    ctry[, .(
        iso, country, patience, log_inc, slope, slope_se,
        pop2012, w_pop
    )],
    "data/processed/within_country_step3_countries.csv"
)
ggsave("figures/diagnostics/within_country_step2.pdf", p2,
    width = 120, height = 90, units = "mm"
)
ggsave("figures/diagnostics/within_country_step3.pdf", p3,
    width = 140, height = 185, units = "mm"
)
ggsave("figures/diagnostics/within_country_step4.pdf", p4,
    width = 120, height = 90, units = "mm"
)
ggsave("figures/diagnostics/within_country_step5_by_country.pdf", p5,
    width = 180, height = 140, units = "mm"
)
