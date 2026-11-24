# =====================================================================
# plot_below_standard.R
#
# Figure for Section 2.3: the share of households falling below the
# cost of a basic standard of living, by household type, with one
# point per state (US) or region (GB).
#
# =====================================================================

library(data.table)
library(ggplot2)
library(scales)
library(patchwork)

# canonical household_type (either country's vocabulary) -> display
# label, size group, and rank (1 = most resourced, drawn at the top).
.HOUSEHOLD_TYPE_MAP <- data.table::data.table(
    household_type = c(
        # GB: pensioner rows have no US equivalent.
        "Couple pensioner",
        "Couple, working age", "2 adults",
        "Couple, 1 child",          "2 adults, 1 child",
        "Couple, 2 children",       "2 adults, 2 children",
        "Couple, 3 children",       "2 adults, 3 children",
        "Couple, 4 children",       "2 adults, 4 children",
        "Single pensioner",
        "Single, working age", "1 adult",
        "Lone parent, 1 child",     "1 adult, 1 child",
        "Lone parent, 2 children",  "1 adult, 2 children",
        "Lone parent, 3 children",  "1 adult, 3 children",
        "1 adult, 4 children"       # EPI goes to 4 children; MIS lone
        # parent only goes to 3
    ),
    display_label = c(
        "2 adults (pensioner)",
        "2 adults (working age)", "2 adults (working age)",
        "2 adults, 1 child",       "2 adults, 1 child",
        "2 adults, 2 children",    "2 adults, 2 children",
        "2 adults, 3 children",    "2 adults, 3 children",
        "2 adults, 4 children",    "2 adults, 4 children",
        "1 adult (pensioner)",
        "1 adult (working age)", "1 adult (working age)",
        "1 adult, 1 child",       "1 adult, 1 child",
        "1 adult, 2 children",    "1 adult, 2 children",
        "1 adult, 3 children",    "1 adult, 3 children",
        "1 adult, 4 children"
    ),
    size_group = c(
        "2 adults",
        "2 adults", "2 adults",
        "2 adults", "2 adults",
        "2 adults", "2 adults",
        "2 adults", "2 adults",
        "2 adults", "2 adults",
        "1 adult",
        "1 adult", "1 adult",
        "1 adult", "1 adult",
        "1 adult", "1 adult",
        "1 adult", "1 adult",
        "1 adult"
    ),
    rank = c(
        1,
        2, 2,
        3, 3,
        4, 4,
        5, 5,
        6, 6,
        7,
        8, 8,
        9, 9,
        10, 10,
        11, 11,
        12
    )
)

# Colour-blind-safe colours (Okabe-Ito), shared with the other
# figures: the less resourced group is vermillion, the more resourced
# blue.
.SIZE_GROUP_COLOURS <- c("1 adult" = "#d55e00", "2 adults" = "#0072b2")

# Plot share below standard by household type, coloured by
# household size and ordered by composition.
plot_below_standard <- function(below,
                                size_range = c(0.7, 4.5),
                                label = "United States",
                                national = NULL,
                                national_ci = NULL,
                                tag = NULL,
                                show_y = TRUE,
                                all_rows = FALSE) {
    dt <- as.data.table(copy(below))

    dt <- merge(dt, .HOUSEHOLD_TYPE_MAP,
        by = "household_type",
        all.x = TRUE
    )
    unmapped <- unique(dt[is.na(rank), household_type])
    if (length(unmapped) > 0) {
        stop(
            "Household type(s) not in .HOUSEHOLD_TYPE_MAP: ",
            paste(unmapped, collapse = ", "),
            ". Add them there - do not rename the loaders' output."
        )
    }

    # Order by rank (most resourced first), reversed for ggplot's
    # bottom-to-top y-axis convention so rank 1 ends up at the top.
    ord <- if (all_rows) {
        unique(.HOUSEHOLD_TYPE_MAP[order(rank), display_label])
    } else {
        unique(dt[order(rank), display_label])
    }
    dt[, display_label := factor(display_label, levels = rev(ord))]

    type_lookup <- unique(dt[, .(
        household_type, display_label,
        size_group
    )])

    if (!is.null(national)) {
        med <- merge(type_lookup, national[, .(
            household_type,
            share_below
        )],
        by = "household_type"
        )
        unmapped <- setdiff(
            unique(dt$household_type),
            national$household_type
        )
        if (length(unmapped) > 0) {
            warning(
                "No national_share() row for: ",
                paste(unmapped, collapse = ", "),
                " - falling back to median-of-areas for those, ",
                "which is a different and generally less correct ",
                "statistic. See the `national` argument's ",
                "documentation."
            )
            fallback <- dt[household_type %in% unmapped,
                .(share_below = stats::median(share_below)),
                by = .(household_type)
            ]
            fallback <- merge(type_lookup, fallback,
                by = "household_type"
            )
            med <- rbindlist(list(med, fallback), use.names = TRUE)
        }
    } else {
        message(
            "plot_below_standard: no `national` estimate ",
            "supplied - drawing the median of area-level points ",
            "instead, which is a different (and generally wrong) ",
            "statistic for a national figure. Pass national_share",
            "() output via `national` for anything going in the ",
            "paper."
        )
        med <- dt[, .(share_below = stats::median(share_below)),
            by = .(display_label, size_group)
        ]
    }

    if (!is.null(national_ci)) {
        med <- merge(med, national_ci,
            by = "household_type",
            all.x = TRUE
        )
    } else {
        med[, `:=`(ci_lo = NA_real_, ci_hi = NA_real_)]
    }

    # Style shared with the other figures (Arial, all
    # text between 5 and 7 pt): one faded point per state or region,
    # sized by the number of survey households behind it, and the
    # population-weighted national share as a black square with a
    # capped 95% interval.
    # No size legend is drawn (the caption states what
    # size encodes), so the two panels share one colour key.
    p <- ggplot(dt, aes(x = share_below, y = display_label)) +
        geom_point(
            aes(size = n_records, colour = size_group),
            alpha = 0.35, shape = 16, stroke = 0,
            position = position_jitter(height = 0.2, width = 0, seed = 1)
        ) +
        geom_errorbar(
            aes(xmin = ci_lo, xmax = ci_hi),
            data = med,
            width = 0.25, linewidth = 0.3, orientation = "y",
            na.rm = TRUE
        ) +
        geom_point(data = med, shape = 15, size = 0.8) +
        scale_colour_manual(values = .SIZE_GROUP_COLOURS, name = NULL) +
        scale_size(range = size_range, guide = "none") +
        scale_x_continuous(
            labels = label_percent(accuracy = 1),
            limits = c(0, 1), breaks = seq(0, 1, 0.25),
            expand = expansion(mult = c(0, 0.02))
        ) +
        scale_y_discrete(drop = FALSE) +
        labs(
            x = "Households below the standard", y = NULL,
            title = label, tag = tag
        ) +
        theme_classic(base_size = 7, base_family = "Arial") +
        theme(
            axis.text = element_text(size = 6, colour = "black"),
            axis.title = element_text(size = 7),
            axis.ticks.y = element_blank(),
            axis.line.y = element_blank(),
            panel.grid.major.x = element_line(
                colour = "grey50", linetype = "dashed",
                linewidth = 0.15
            ),
            plot.title = element_text(size = 7, margin = margin(b = 1)),
            plot.tag = element_text(face = "bold", size = 7),
            plot.margin = margin(1, 2, 1, 1, "mm"),
            legend.position = "bottom",
            legend.text = element_text(size = 6),
            legend.key.size = unit(3, "mm"),
            legend.margin = margin(0, 0, 0, 0)
        ) +
        guides(colour = guide_legend(
            nrow = 1, override.aes = list(size = 1.5, alpha = 1)
        ))
    if (!show_y) p <- p + theme(axis.text.y = element_blank())
    p
}

# Both countries side by side on shared rows (Figure 2).
plot_below_standard_pair <- function(us, gb) {
    panel_a <- plot_below_standard(
        us$below,
        label = "United States", national = us$national,
        national_ci = us$national_ci, size_range = c(0.5, 3),
        tag = "A", all_rows = TRUE
    )
    panel_b <- plot_below_standard(
        gb$below,
        label = "Great Britain", national = gb$national,
        national_ci = gb$national_ci, size_range = c(0.5, 2),
        tag = "B", show_y = FALSE, all_rows = TRUE
    )
    panel_a + panel_b +
        plot_layout(nrow = 1, guides = "collect") &
        theme(legend.position = "bottom")
}
