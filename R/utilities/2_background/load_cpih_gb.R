# =====================================================================
# load_cpih_gb.R
#
# Loads ONS's CPIH index (series L522, "CPIH INDEX 00: ALL ITEMS
# 2015=100") as an annual deflator, in schema_net_assets.R's deflator shape.
# Same shape as load_cpi_us.R.
#
# Source: ons.gov.uk/economy/inflationandpriceindices/timeseries/
# l522/mm23
#
# CPIH, not CPI: CPIH also includes owner-occupiers' housing costs
# and Council Tax, so it will not match BoE-published CPI figures.
# =====================================================================

library(data.table)

# Load the ONS CPIH annual index as a deflator.
load_cpih_gb <- function(cpih_csv_path) {
    source("R/utilities/2_background/schema_net_assets.R")

    raw <- fread(cpih_csv_path, header = FALSE, fill = TRUE)

    annual <- raw[grepl("^[0-9]{4}$", V1)]

    data.table(
        country = "GB",
        year    = as.integer(annual$V1),
        index   = as.numeric(annual$V2)
    )
}
