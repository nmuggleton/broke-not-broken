# ==============================================================================
# plot_explanatory_power.R
#
# Forest plot of patience and income coefficients (Section 4.1). Styled
# after forest_plot_utils.R in nmuggleton/two_paradoxes_womens_wellbeing:
# square points, capped error bars, estimates faded when the 95% CI
# includes zero, dashed grey line at zero.
#
# Panel C is redrawn from Step 3 of the within-country analysis (after
# Stevenson and Wolfers): one dot per country, with a short line whose
# angle is that country's own slope of patience on log income.
#
# Inputs, both written by R/4_structural/text/main.R:
#   results   - long table of estimates. Variables used: step, model,
#               term, estimate, lo, hi (95% CI bounds), outcome, country.
#   countries - one row per country. Variables used: iso, patience
#               (mean, SD units), log_inc (mean log per capita income),
#               slope (patience on log income within the country),
#               pop2012, w_pop (2012 population and its share).
# ==============================================================================

library(data.table)
library(ggplot2)
library(scales)
library(patchwork)

# Colour-blind-safe colours (Okabe-Ito), shared with Figures 1 and 4:
# patience is vermillion, income is blue.
.EXPLANATORY_COLOURS <- c(Patience = "#d55e00", Income = "#0072b2")
.EXPLANATORY_LABELS <- c(
    Patience = "Patience (one SD higher)",
    Income   = "Income (richest fifth against poorest fifth)"
)
.POOLED_LABEL <- "All 14 countries"
.HARDSHIP_LABELS <- c(
    shelter   = "Not enough money\nfor shelter",
    food      = "Not enough money\nfor food",
    difficult = "Finds it difficult to live\non present income"
)

# Coefficients in percentage points, with a key for the predictor.
.prepare_coefficients <- function(dt) {
    dt <- copy(dt)
    dt[, `:=`(
        est = 100 * estimate, lower = 100 * lo, upper = 100 * hi,
        predictor = fifelse(
            grepl("patience", term), "Patience",
            "Income"
        )
    )]
    dt
}

.plot_forest <- function(dt, y_var, title, tag) {
    axis_grey <- "grey50"
    # Estimates whose 95% CI includes zero are
    # drawn at alpha 0.5.
    dt[, fade := fifelse(lower <= 0 & upper >= 0, 0.5, 1)]

    ggplot(dt, aes(
        x = est, y = .data[[y_var]], colour = predictor,
        alpha = fade
    )) +
        geom_vline(
            xintercept = 0, linetype = "dashed",
            colour = axis_grey, linewidth = 0.3
        ) +
        geom_errorbar(aes(xmin = lower, xmax = upper),
            width = 0.25,
            linewidth = 0.3, orientation = "y"
        ) +
        geom_point(shape = 15, size = 0.8) +
        scale_alpha_identity() +
        # Coefficients are in percentage points; the tick labels carry
        # the percent sign and the caption states the unit.
        scale_x_continuous(labels = label_percent(scale = 1)) +
        scale_colour_manual(
            values = .EXPLANATORY_COLOURS,
            labels = .EXPLANATORY_LABELS, name = NULL
        ) +
        labs(
            x = "Coefficient (difference in probability)", y = NULL,
            title = title, tag = tag
        ) +
        # Figure style: Arial, all text between 5 and 7 pt.
        theme_classic(base_size = 7, base_family = "Arial") +
        theme(
            axis.text = element_text(size = 6, colour = "black"),
            axis.title = element_text(size = 7),
            axis.ticks.y = element_blank(),
            axis.line.y = element_blank(),
            plot.title = element_text(
                size = 7,
                margin = margin(b = 1)
            ),
            plot.tag = element_text(face = "bold", size = 7),
            plot.margin = margin(1, 2, 1, 1, "mm")
        )
}

# Panel C: mean patience against mean income, with each country's own
# slope, the between-country line, and the average within-country line.
.plot_patience_income <- function(countries, tag = "C") {
    ctry <- copy(as.data.table(countries))
    # The
    # within-country line is the population-weighted mean of the
    # country slopes, drawn through the population-weighted mean of
    # the country means. The between-country line is the unweighted
    # fit through the country means.
    within_slope <- ctry[, sum(w_pop * slope)]
    between <- coef(lm(patience ~ log_inc, data = ctry))
    lines <- data.table(
        line = c("Between countries", "Within countries (average)"),
        slope = c(between[["log_inc"]], within_slope),
        intercept = c(
            between[["(Intercept)"]],
            ctry[, sum(w_pop * patience)] -
                within_slope * ctry[, sum(w_pop * log_inc)]
        )
    )

    # Every country line has the same short
    # length on the page, so only its angle carries information. The
    # length is computed in page units from the axis limits and the
    # panel's height-to-width ratio. The panel is not held to a fixed
    # ratio (its x-axis has to line up with panel B's), so `aspect` is
    # the ratio it takes at 183 mm by 120 mm; if the figure size
    # changes, the lines stay at the right angle and differ only
    # slightly in length.
    seg_half_length <- 0.025
    aspect <- 1.4
    x_lim <- range(ctry$log_inc) + c(-0.3, 0.3)
    y_lim <- range(ctry$patience) + c(-0.1, 0.1)
    ctry[, `:=`(ux = 1 / diff(x_lim), uy = slope * aspect / diff(y_lim))]
    ctry[, len := sqrt(ux^2 + uy^2)]
    ctry[, `:=`(
        dx = seg_half_length * ux / len * diff(x_lim),
        dy = seg_half_length * uy / len * diff(y_lim) / aspect
    )]

    # Every country is labelled with its
    # two-letter ISO 3166-1 code (country names do not fit a single
    # column), with no leader lines.
    ctry[, label := countrycode::countrycode(iso, "iso3c", "iso2c")]
    stopifnot(!anyNA(ctry$label))

    inc_breaks <- c(250, 1000, 4000, 16000)
    ggplot(ctry, aes(x = log_inc, y = patience)) +
        geom_abline(
            data = lines,
            aes(
                intercept = intercept, slope = slope,
                linetype = line
            ), linewidth = 0.3,
            key_glyph = "path"
        ) +
        geom_segment(
            aes(
                x = log_inc - dx, xend = log_inc + dx,
                y = patience - dy, yend = patience + dy
            ),
            colour = "grey50", linewidth = 0.3
        ) +
        geom_point(shape = 15, size = 0.8) +
        # Labels are nudged clear of the points but have no leader
        # lines (min.segment.length = Inf).
        ggrepel::geom_text_repel(
            aes(label = label),
            size = 5 / .pt, family = "Arial",
            seed = 1, max.overlaps = Inf, min.segment.length = Inf,
            box.padding = 0.05, point.padding = 0.1, force = 1,
            max.iter = 50000, max.time = 5
        ) +
        scale_linetype_manual(
            values = c(
                "Between countries" = "dashed",
                "Within countries (average)" = "solid"
            ),
            name = NULL
        ) +
        scale_x_continuous(
            breaks = log(inc_breaks),
            labels = label_dollar()(inc_breaks)
        ) +
        coord_cartesian(xlim = x_lim, ylim = y_lim, expand = FALSE) +
        labs(
            x = "Mean household income per person (log scale)",
            y = "Mean patience (SD)",
            title = "Patience and income", tag = tag
        ) +
        theme_classic(base_size = 7, base_family = "Arial") +
        theme(
            axis.text = element_text(size = 6, colour = "black"),
            axis.title = element_text(size = 7),
            plot.title = element_text(
                size = 7,
                margin = margin(b = 1)
            ),
            plot.tag = element_text(face = "bold", size = 7),
            plot.margin = margin(1, 2, 1, 3, "mm"),
            # The key for the two lines sits below the panel.
            legend.position = "bottom",
            legend.text = element_text(size = 6),
            legend.key.height = unit(3, "mm"),
            legend.key.width = unit(7, "mm"),
            legend.margin = margin(0, 0, 0, 0)
        )
}

plot_explanatory_power <- function(results, countries) {
    results <- as.data.table(results)

    # Both coefficients come from the model that
    # holds the other fixed (patience and income quintiles together),
    # and both panels use one specification: age, age squared, sex,
    # and country fixed effects, weighted (Step 6 in the within-country
    # script). Falk et al.'s own specification is Step 5 there.
    saving_model   <- "6a: saving, Step 4 specification"
    hardship_model <- "C: patience + income"

    # Panel A: saved money in the past year, 14 countries and pooled.
    saving <- .prepare_coefficients(rbind(
        results[step == "step6_country" & model == saving_model],
        results[step == "step6" & model == saving_model][
            , country := .POOLED_LABEL
        ]
    ))
    # Countries ordered by the income
    # coefficient; the pooled estimate sits on the top row, in bold,
    # set apart by a grey rule. Both coefficients share a row and are
    # told apart by colour alone.
    country_order <- saving[predictor == "Income" &
        country != .POOLED_LABEL][
        order(est), country
    ]
    saving[, country := factor(country,
        levels = c(country_order, .POOLED_LABEL)
    )]
    n_countries <- length(country_order)
    # A vector of faces is not officially supported by element_text()
    # (ggplot2 warns), but it is the simplest way to bold one row.
    panel_a <- suppressWarnings(
        .plot_forest(saving, "country",
            title = "Saved money in the past year", tag = "A"
        ) +
            geom_hline(
                yintercept = n_countries + 0.5, colour = "grey50",
                linewidth = 0.2
            ) +
            theme(axis.text.y = element_text(
                face = c(rep("plain", n_countries), "bold")
            ))
    )

    # Panel B: three hardship outcomes, pooled over 70 to 72 countries.
    hardship <- .prepare_coefficients(
        results[step == "step4" & model == hardship_model]
    )
    hardship[, outcome_txt := factor(.HARDSHIP_LABELS[outcome],
        levels = .HARDSHIP_LABELS
    )]
    panel_b <- .plot_forest(
        hardship, "outcome_txt",
        title = "Financial hardship", tag = "B"
    )

    panel_c <- .plot_patience_income(countries)

    # Left column: A above B, heights following the number of rows (15
    # against 3). Right column: C.
    # The colour key sits below the left column
    # (A and B), not below the whole figure, because panel C does not
    # use it. Panel B carries the key; panel A's is dropped.
    panel_a <- panel_a + theme(legend.position = "none")
    panel_b <- panel_b +
        guides(colour = guide_legend(nrow = 1, reverse = TRUE)) +
        theme(
            legend.position = "bottom",
            legend.text = element_text(size = 6),
            legend.key.size = unit(3, "mm"),
            legend.margin = margin(0, 0, 0, 0)
        )
    # One layout for all three panels, so panel
    # C runs from the top of A to the bottom of B and the x-axes of B
    # and C sit on the same line.
    wrap_plots(
        A = panel_a, B = panel_b, C = panel_c,
        design = "AC\nBC", heights = c(15, 3.6),
        widths = c(1, 1)
    )
}
