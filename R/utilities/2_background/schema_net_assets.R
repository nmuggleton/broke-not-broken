# =====================================================================
# schema_net_assets.R
#
# One tidy schema for the net-assets figure.
#
#   country      "US" | "GB"
#   concept      "net_wealth" | "net_financial_assets"
#   group        ordered factor, e.g. "Bottom 20%", ..., "Top 20%"
#   year         integer
#   value_real   real local-currency units, deflated to base_year
#   currency     "USD" | "GBP"
#   source       citation string, printed in the figure caption
#
# Deflation: each loader takes a base_year and a deflator series
# (a data.table with columns year, index) and divides value by
# index / index[year == base_year].
# =====================================================================

library(data.table)

NET_ASSETS_SCHEMA <- c(
    "country", "concept", "group", "year",
    "value_real", "currency", "source"
)

# Check a data.table matches the tidy net-assets schema.
validate_net_assets <- function(dt) {
    missing_cols <- setdiff(NET_ASSETS_SCHEMA, names(dt))
    if (length(missing_cols) > 0) {
        stop(
            "Missing column(s): ", paste(missing_cols, collapse = ", "),
            "\nExpected: ", paste(NET_ASSETS_SCHEMA, collapse = ", ")
        )
    }
    bad_country <- setdiff(unique(dt$country), c("US", "GB"))
    if (length(bad_country) > 0) {
        stop(
            "Unexpected country code(s): ",
            paste(bad_country, collapse = ", ")
        )
    }
    invisible(dt)
}

# Deflate a value column to constant prices in `base_year`.
deflate_to_base_year <- function(dt, value_col, deflator, base_year) {
    dt <- as.data.table(dt)
    deflator <- as.data.table(deflator)

    base_index <- deflator[year == base_year, .(country, index)]
    setnames(base_index, "index", "base_index")
    if (nrow(base_index) == 0) {
        stop(
            "base_year ", base_year,
            " not found in the deflator series"
        )
    }
    if (anyDuplicated(base_index$country) != 0) {
        stop(
            "base_year ", base_year, " has more than one index ",
            "value for at least one country in the deflator series"
        )
    }

    dt <- merge(dt, deflator,
        by = c("country", "year"),
        all.x = TRUE, sort = FALSE
    )
    dt <- merge(dt, base_index,
        by = "country",
        all.x = TRUE, sort = FALSE
    )
    dt[, (value_col) := get(value_col) / index * base_index]
    dt[, c("index", "base_index") := NULL]
    dt[]
}
