# =====================================================================
# write_debt_table.R
#
# Turns debt-composition data (from load_scf.R for the US, or
# load_was_debt.R for GB) a debt-composition output into a booktabs +
# siunitx LaTeX table (Table 1: current debt breakdown, lowest wealth
# group, mortgage excluded).
#
# =====================================================================

library(data.table)

.write_debt_panel_rows <- function(dt, panel_label) {
    if (length(unique(dt$country)) != 1) {
        stop(
            ".write_debt_panel_rows(): expected a single country, ",
            "got ", length(unique(dt$country)), "."
        )
    }
    if (length(unique(dt$currency)) != 1) {
        stop(
            ".write_debt_panel_rows(): more than one currency ",
            "present for a single country's panel - check the ",
            "input."
        )
    }

    dt <- copy(dt)
    setorder(dt, sort_order)

    total <- sum(dt[is_subtotal == TRUE, value_real])
    currency_symbol <- if (unique(dt$currency) == "USD") "\\$" else
        "\\textsterling{}"

    body_rows <- paste0(
        "        ", dt$category, " & ", sprintf("%.0f", dt$value_real),
        " \\\\",
        collapse = "\n"
    )

    sprintf(
        "        \\multicolumn{2}{l}{\\textit{%s (%s)}} \\\\
%s
        \\textbf{Total (non-mortgage)} & \\bfseries %s \\\\",
        panel_label, currency_symbol, body_rows, sprintf("%.0f", total)
    )
}

# Write one continuous debt-composition table, econ-journal style:
# single header, both countries in one tabular, italic panel labels
# as rows, no internal rules.
write_debt_table <- function(us_dt, gb_dt,
                             out_path,
                             caption,
                             label = "tab:debt-composition",
                             us_panel_label = "United States",
                             gb_panel_label = "Great Britain",
                             panel_gap = "8pt") {
    us_rows <- .write_debt_panel_rows(us_dt, us_panel_label)
    gb_rows <- .write_debt_panel_rows(gb_dt, gb_panel_label)

    tex <- sprintf(
        "\\begin{table}[h!]
    \\centering
    \\caption{%s}
    \\label{%s}
    \\begin{tabular}{l S[table-format=5.0]}
        \\toprule
        \\multicolumn{1}{l}{Debt type} &
        \\multicolumn{1}{c}{Per household} \\\\
        \\midrule
%s
        \\addlinespace[%s]
%s
        \\bottomrule
    \\end{tabular}
\\end{table}
",
        caption, label, us_rows, panel_gap, gb_rows
    )

    writeLines(tex, out_path)
    invisible(out_path)
}
