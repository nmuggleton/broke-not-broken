# =====================================================================
# load_cpi_us.R
#
# Loads BLS CPI-U (CPIAUCSL, seasonally adjusted, from FRED) and
# collapses monthly observations to an annual average index, for use
# as the US deflator in deflate_to_base_year() (schema_net_assets.R).
#
# Source: fred.stlouisfed.org/series/CPIAUCSL
#
# Uses the annual average (mean of the 12 monthly values in a year),
# not the December/Q4 value - the standard convention for deflating
# annual wealth/income series.
# =====================================================================

library(data.table)

# Load US CPI-U as an annual deflator, in schema_net_assets.R's deflator shape.
load_cpi_us <- function(cpi_csv_path,
                        col_map = c(
                            date  = "observation_date",
                            index = "CPIAUCSL"
                        )) {
    source("R/utilities/2_background/schema_net_assets.R")

    dt <- fread(cpi_csv_path)

    setnames(
        dt,
        old = unname(col_map[c("date", "index")]),
        new = c("date", "index")
    )

    dt[, year := as.integer(substr(date, 1, 4))]

    annual <- dt[, .(index = mean(index, na.rm = T)), by = year]
    annual[, country := "US"]

    setcolorder(annual, c("country", "year", "index"))
    annual[]
}
