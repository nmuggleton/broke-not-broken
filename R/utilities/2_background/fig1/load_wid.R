# =====================================================================
# load_wid.R
#
# Loads WID average net household wealth (ahweal, equal-split
# adults, 20+) by percentile group, for US and GB, via the `wid` R
# package's download_wid(). Standardises to the schema in
# schema_net_assets.R with concept = "net_wealth".
#
# Indicator is "ahweal", not "shweal" - "a" = average, a currency
# amount; "s" = share, a 0-1 fraction of the total.
#
# pop = "j" (equal-split adults) + ages = 992 (20+) is WID's
# benchmark combination.
#
# Median: the threshold variable "thweal" at p50p51, i.e. the net
# wealth of the adult at the 50th percentile (WID threshold variables
# give the value at the lower bound of the percentile group).
#
# Components (hwfin, hwnfa, hwdeb) do not have percentile-level
# coverage under pop = "j" - confirmed by testing; only the
# aggregate (p0p100) is populated. They exist under pop = "i"
# (individuals), but that's a different ranking population to
# ahweal(j), so the two can't be combined group-for-group. This
# loader can therefore only ever produce net_wealth (assets minus
# all debts), not net_financial_assets (savings minus debt).
# In practice this matters less at the bottom of the distribution
# (little housing/business equity there) than at the top (where non-
# financial assets dominate).
#
# GB has a real series break at 1994/1995: WID splices Alvaredo,
# Atkinson & Morelli (pre-1995, estate-multiplier method, excludes
# pension wealth, not well suited to bottom-of-distribution debt)
# onto Blanchet & Martinez-Toledano (1995 on, includes funded
# pension wealth).
# =====================================================================

source("R/utilities/2_background/schema_net_assets.R")
library(data.table)
library(wid)

# Load WID average net wealth (ahweal, pop = "j", ages = 992).
load_wid <- function(deflator,
                     base_year,
                     areas = c("US", "GB"),
                     years = 1980:2024,
                     perc  = c("p0p20", "p80p100"),
                     median = TRUE,
                     pop   = "j",
                     ages  = 992) {
    # Base quintile levels. Extra percentiles passed via `perc` that
    # aren't quintiles (e.g. exploratory p0p10/p0p50 pulls) fall back to
    # their raw WID label rather than collapsing to NA - see the
    # factor() call below.
    WID_GROUP_LEVELS <- c(
        "Bottom 20%", "20-40%", "Median", "60-80%", "Top 20%", 'Top 0.1%'
    )

    pull <- function(indicator, pc) {
        lapply(areas, function(area) {
            wid::download_wid(
                indicators = indicator,
                areas      = area,
                ages       = ages,
                years      = years,
                perc       = pc,
                pop        = pop,
                metadata   = FALSE,
                verbose    = FALSE
            )
        }) |> rbindlist()
    }
    dt <- pull("ahweal", perc)
    if (median) {
        med <- pull("thweal", "p50p51")
        if (nrow(med) == 0) {
            stop(
                "load_wid(): WID returned no thweal p50p51 (median) ",
                "values."
            )
        }
        med[, percentile := "median"]
        dt <- rbindlist(list(dt, med), use.names = TRUE)
    }

    setnames(dt, "value", "value_real")

    group_lookup <- c(
        p0p20   = "Bottom 20%",
        p20p40  = "20-40%",
        median  = "Median",
        p60p80  = "60-80%",
        p80p100 = "Top 20%",
        p99.9p100 = "Top 0.1%"
    )
    dt[, group := group_lookup[percentile]]
    dt[is.na(group), group := percentile]
    dt[, group := factor(group, levels = unique(c(WID_GROUP_LEVELS, group)))]

    dt[, concept  := "net_wealth"]
    dt[, currency := fifelse(country == "US", "USD", "GBP")]
    dt[, source   := "World Inequality Database (wid.world)"]

    # GB pre-1995 splices onto a different underlying methodology
    # (Alvaredo/Atkinson/Morelli, estate-multiplier, no pension
    # wealth) than 1995 onwards (Blanchet/Martinez-Toledano) - see
    # the header note. Confirmed visually as a real, large
    # discontinuity, not noise: dropping the pre-1995 GB portion
    # rather than plotting one line across an undisclosed break.
    # US is unaffected. Decision: truncate, not mark or splice-adjust
    # - revisit if the longer GB run is wanted instead.
    dt <- dt[!(country == "GB" & year < 1995)]

    dt <- dt[, ..NET_ASSETS_SCHEMA]
    dt <- deflate_to_base_year(dt, "value_real", deflator, base_year)

    validate_net_assets(dt)
}
