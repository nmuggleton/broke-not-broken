# ==============================================================================
# plot_net_assets.R
# ==============================================================================

library(data.table)
library(ggplot2)
library(scales)
library(patchwork)

# Colour-blind-safe colours (Okabe-Ito), shared with the credit
# figure: low is vermillion, middle bluish green, high blue.
.NET_ASSET_COLOURS <- c(
    "Bottom 20%" = "#d55e00", "Bottom 50%" = "#e69f00",
    "Median"     = "#009e73", "Top 20%"    = "#0072b2",
    "Top 1%"     = "#56b4e9", "Top 0.1%"   = "#cc79a7"
)

.plot_one_country <- function(dt_country, range, y_title = NULL,
                              tag = NULL) {
    source("R/utilities/2_background/schema_net_assets.R")

    CURRENCY_PREFIX <- c(USD = "$", GBP = "\u00a3")

    .PSEUDO_LOG_BREAKS <- c(
        -1e+05, -1e+04, 0,
        1e+04, 1e+05, 1e+06, 1e+07, 1e+08, 1e+09
    )

    currency_symbol <- CURRENCY_PREFIX[unique(dt_country$currency)]
    axis_grey <- rgb(73, 73, 73, maxColorValue = 255)

    p <- ggplot(
        dt_country,
        aes(x = year, y = value_real, colour = group)
    ) +
        geom_hline(
            yintercept  = 0,
            colour      = axis_grey,
            linewidth   = 0.4,
            linetype    = "dashed"
        ) +
        geom_line(linewidth = 0.7) +
        scale_y_continuous(
            trans  = pseudo_log_trans(sigma = 1000, base = 10),
            labels = label_dollar(prefix = currency_symbol),
            breaks = .PSEUDO_LOG_BREAKS,
        ) +
        coord_cartesian(
            ylim = range
        ) +
        scale_colour_manual(
            values = .NET_ASSET_COLOURS, name = NULL,
            drop = FALSE
        ) +
        labs(
            x = NULL, y = y_title,
            title = unique(dt_country$country_txt), tag = tag
        ) +
        # Figure style: Arial, all text between 5 and 7 pt.
        # base_size 7 gives 7 pt titles and 5.6 pt tick and legend
        # text; the panel title and tag are held at 7 pt.
        theme_classic(base_size = 7, base_family = "Arial") +
        theme(
            legend.position = "bottom",
            plot.title = element_text(size = 7),
            plot.tag = element_text(face = "bold", size = 7)
        )

    if ("concept_txt" %in% names(dt_country) &&
        length(unique(dt_country$concept_txt)) > 1) {
        p <- p + facet_grid(concept_txt ~ ., scales = "free_y")
    }

    p
}

plot_net_assets <- function(dt,
                            base_year,
                            highlight_groups = c(
                                "Bottom 20%", "Bottom 50%", "Top 20%",
                                "Top 1%", "Top 0.1%", "Median"
                            )) {
    dt <- as.data.table(dt)
    validate_net_assets(dt)

    range <- range(dt$value_real)
    # range <- sign(x) * 10^ceiling(log10(abs(x)))

    if (!is.null(highlight_groups)) {
        dt <- dt[group %in% highlight_groups]
    }
    # Same groups, so the same colours, in both panels.
    dt[, group := droplevels(group)]

    concept_label <- c(
        net_wealth           = "Net wealth (assets minus all debts)",
        net_financial_assets = "Net financial assets (savings minus debt)"
    )
    country_label <- c(US = "United States", GB = "Great Britain")

    dt[, concept_txt := concept_label[concept]]
    dt[, country_txt := country_label[country]]

    countries <- sort(unique(dt$country))
    y_title <- paste0("Net wealth per adult (", base_year, " prices)")
    panels <- lapply(seq_along(countries), function(i) {
        .plot_one_country(dt[country == countries[i]],
            range = range,
            y_title = if (i == 1) y_title else NULL,
            tag = LETTERS[i]
        )
    })
    # No outer margin, so the saved file is the size given to
    # ggsave(): the left panel loses its left margin and the right
    # panel its right margin; the gap between panels stays.
    n <- length(panels)
    panels[[1]] <- panels[[1]] +
        theme(plot.margin = margin(0, 5.5, 0, 0))
    panels[[n]] <- panels[[n]] +
        theme(plot.margin = margin(0, 0, 0, if (n > 1) 5.5 else 0))

    wrap_plots(panels, nrow = 1) +
        plot_layout(guides = "collect") &
        theme(
            legend.position = "bottom",
            legend.margin = margin(0, 0, 0, 0),
            legend.box.margin = margin(0, 0, 0, 0),
            legend.box.spacing = unit(4, "pt"),
            legend.key.size = unit(8, "pt")
        )
}
