# =====================================================================
# load_hbai.R
#
# Loads Households Below Average Income (HBAI) household records and
# derives, by household type, the share of GB households with net
# income below the Minimum Income Standard. This is the GB
# counterpart to share_below_standard() on the US side.
#
# Source: UK Data Service SN 5828, tab version, file
# tab/24-25prices/i2425e_2425prices_admin_linked.tab.
#
#   1. PERSON-LEVEL FILE. Each row is a person, with household
#      variables repeated across everyone in the household. Counting
#      rows would weight large households by their size. Deduplicated
#      to one row per household below.
#   3. UNEQUIVALISED income is required. ESNINCHH and ESAHCHH are
#      actual household amounts; everything named S_OE_* or OE_* is
#      equivalised to a couple-with-no-children basis and cannot be
#      compared with a MIS threshold for a specific household type.
#   3. COUNTRY is 1 England, 2 Wales, 3 Scotland, 4 Northern Ireland.
#      MIS is a Great Britain standard, as are the WID and WAS series
#      used elsewhere in the paper, so Northern Ireland is dropped.
#
# SPI'd variables (ES*) rather than non-SPI'd (E*) are used: these
# correct the survey's known under-capture of very high incomes using
# tax data.
#
# HOUSING COSTS BASIS. HBAI reports income before (ESNINCHH) and
# after (ESAHCHH) housing costs, and the MIS threshold must be on the
# same basis. load_mis(basis = "hbai_bhc") pairs with
# housing_basis = "bhc" here, and load_mis(basis = "hbai_ahc") with
# "ahc".
# =====================================================================

source("R/utilities/2_background/fig2/schema_budget.R")
library(data.table)

.HBAI_COLS <- c(
    "sernum", "ADULTH", "DEPCHLDH", "COUNTRY",
    "ESNINCHH", "ESAHCHH", "GS_INDHH",
    "TYPE", "GVTREGN"
)

.HBAI_COUNTRY_GB <- 1:3   # England, Wales, Scotland

.HBAI_CHECKS <- list(
    sernum   = function(x) all(x > 0, na.rm = TRUE) &&
        length(unique(x)) > 0.2 * length(x),
    ADULTH   = function(x) all(x >= 1 & x <= 12, na.rm = TRUE),
    DEPCHLDH = function(x) all(x >= 0 & x <= 15, na.rm = TRUE),
    COUNTRY  = function(x) all(x %in% 1:4),
    TYPE      = function(x) all(x %in% 1:3),
    GVTREGN   = function(x) all(x %in% 1:13),
    GS_INDHH = function(x) all(x >= 0, na.rm = TRUE) &&
        length(unique(x)) > 0.2 * length(x),
    ESNINCHH = function(x) is.numeric(x) && any(x != 0, na.rm = TRUE),
    ESAHCHH  = function(x) is.numeric(x) && any(x != 0, na.rm = TRUE)
)

# Read selected HBAI columns, applying the offset explicitly.
.hbai_fread <- function(path, cols, shifted = "sernum",
                        probe = 2000L) {
    head1 <- readLines(path, n = 1L)
    names_hdr <- trimws(strsplit(head1, "\t", fixed = TRUE)[[1]])

    line2 <- readLines(path, n = 2L)[2]
    n_sep <- lengths(regmatches(line2, gregexpr("\t", line2,
        fixed = TRUE
    )))
    n_data <- n_sep + 1L
    n_names <- length(names_hdr)
    slack <- n_data - n_names

    if (slack < 0L || slack > 1L) {
        stop(
            "HBAI: ", n_data, " data columns against ", n_names,
            " header names. Only a difference of 0 or 1 is handled."
        )
    }

    missing <- setdiff(cols, names_hdr)
    if (length(missing) > 0) {
        stop(
            "Not in the header: ", paste(missing, collapse = ", "),
            ". Note sernum is lower case and the header line ends ",
            "with a carriage return."
        )
    }

    pos <- match(cols, names_hdr) + slack * as.integer(cols %in% shifted)

    if (slack > 0L) {
        message(
            "HBAI: header has ", n_names, " names against ",
            n_data, " data columns; treating ",
            paste(intersect(cols, shifted), collapse = ", "),
            " as sitting one position later."
        )
    }

    # Check the resolved positions rather than trust them: a wrong
    # `shifted` argument would otherwise produce plausible nonsense.
    sample <- fread(path,
        select = pos, sep = "\t", header = FALSE,
        skip = 1L, nrows = probe, showProgress = FALSE
    )
    ret_s <- as.integer(sub("^V", "", names(sample)))
    setnames(sample, cols[match(ret_s, pos)])

    for (nm in intersect(cols, names(.HBAI_CHECKS))) {
        ok <- isTRUE(tryCatch(.HBAI_CHECKS[[nm]](sample[[nm]]),
            error = function(e) FALSE
        ))
        if (!ok) {
            stop(
                "Column ", nm, " does not look like ", nm,
                " at the position used. Values begin: ",
                paste(utils::head(sample[[nm]], 5), collapse = ", "),
                ". Re-check which columns belong in `shifted`."
            )
        }
    }

    dt <- fread(path,
        select = pos, sep = "\t", header = FALSE,
        skip = 1L, showProgress = FALSE
    )
    returned <- as.integer(sub("^V", "", names(dt)))
    setnames(dt, cols[match(returned, pos)])
    dt
}

# Load HBAI household records.
load_hbai <- function(hbai_tab_path,
                      housing_basis = c("bhc", "ahc"),
                      annualise = TRUE,
                      gb_only = TRUE,
                      shifted_cols = "sernum") {
    housing_basis <- match.arg(housing_basis)
    income_var <- if (housing_basis == "bhc") "ESNINCHH" else "ESAHCHH"
    mult <- if (annualise) 52 else 1

    dt <- .hbai_fread(hbai_tab_path, .HBAI_COLS,
        shifted = shifted_cols
    )

    n_persons <- nrow(dt)

    if (gb_only) {
        dt <- dt[COUNTRY %in% .HBAI_COUNTRY_GB]
    }
    n_gb <- nrow(dt)

    # One row per household. Household variables repeat across every
    # person in the household, so any row will do; the first is taken.
    if (!all(dt$COUNTRY %in% 1:4)) {
        stop(
            "COUNTRY holds values outside 1-4; columns are ",
            "misaligned. Do not use this output."
        )
    }
    if (uniqueN(dt$sernum) < 0.2 * nrow(dt)) {
        stop(
            "sernum has only ", uniqueN(dt$sernum), " distinct ",
            "values across ", nrow(dt), " person records, which is ",
            "far too few for a household identifier. Columns are ",
            "misaligned. Do not use this output."
        )
    }

    # Pensioner counts come from TYPE (1 child, 2 working age,
    # 3 pensioner), which is a PER-PERSON variable, so it must be
    # aggregated across the household's rows before deduplicating.
    #
    dt[, pensioners := sum(TYPE == 3L), by = sernum]
    dt[, adults_from_type := sum(TYPE %in% c(2L, 3L)), by = sernum]

    dt <- unique(dt, by = "sernum")
    n_hh <- nrow(dt)

    # ADULTH and the TYPE-derived adult count measure the same thing
    # by different routes, so a large disagreement means one of them
    # is misaligned.
    disagree <- dt[adults_from_type != ADULTH, .N] / nrow(dt)
    if (disagree > 0.05) {
        warning(
            round(100 * disagree), " per cent of households have ",
            "ADULTH disagreeing with the adult count derived from ",
            "TYPE. Check column alignment before using this."
        )
    }

    setnames(dt, income_var, "income_weekly")
    dt <- dt[!is.na(income_weekly) & !is.na(GS_INDHH) & GS_INDHH > 0]

    dt[, adults := as.integer(ADULTH)]
    dt[, children := as.integer(DEPCHLDH)]
    dt[, household_type := hbai_household_label(
        adults, children,
        pensioners
    )]
    dt[, region := as.integer(GVTREGN)]
    dt[, income := income_weekly * mult]

    message(
        "HBAI: ", n_persons, " person records, ", n_gb,
        " in Great Britain, ", n_hh, " households, ",
        nrow(dt), " retained with income and weight (",
        toupper(housing_basis), " basis)."
    )

    # A pensioner share far from roughly a third would indicate the
    # flags are being read wrongly, as ERICPHH once was.
    pen_share <- dt[
        grepl("pensioner", household_type),
        sum(GS_INDHH)
    ] / dt[, sum(GS_INDHH)]
    if (pen_share < 0.05 || pen_share > 0.6) {
        warning(
            "Pensioner households are ", round(100 * pen_share),
            " per cent of the weighted total, which is well ",
            "outside the expected range. Check PENFLAGHD and ",
            "PENFLAGSP are being read correctly."
        )
    }

    dt[, .(sernum, adults, children, pensioners, household_type,
        region, income,
        weight = GS_INDHH
    )]
}

# Label a household to match the MIS household types.
hbai_household_label <- function(adults, children, pensioners) {
    all_pensioners <- pensioners >= adults & children == 0

    lab <- fifelse(
        all_pensioners & adults == 1, "Single pensioner",
        fifelse(
            all_pensioners & adults == 2, "Couple pensioner",
            fifelse(
                adults == 1 & children == 0, "Single, working age",
                fifelse(
                    adults == 2 & children == 0, "Couple, working age",
                    fifelse(
                        adults == 1 & children == 1, "Lone parent, 1 child",
                        fifelse(
                            adults == 1 & children >= 2,
                            paste0("Lone parent, ", children, " children"),
                            fifelse(
                                adults == 2 & children == 1, "Couple, 1 child",
                                fifelse(
                                    adults == 2 & children >= 2,
                                    paste0("Couple, ", children, " children"),
                                    paste0(
                                        adults, " adults, ", children,
                                        " children"
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
    )
    lab
}

# Share of GB households below the MIS requirement, by household
# type.
share_below_mis <- function(hbai_dt, mis_dt, by = NULL) {
    thresholds <- unique(
        mis_dt[
            measure == "required_income",
            .(household_type, threshold = value_real)
        ]
    )

    unmatched <- setdiff(
        unique(hbai_dt$household_type),
        thresholds$household_type
    )
    dt <- merge(hbai_dt, thresholds, by = "household_type")
    if (nrow(dt) == 0) {
        stop("No household types matched between HBAI and MIS.")
    }
    if (length(unmatched) > 0) {
        dropped <- hbai_dt[
            household_type %in% unmatched,
            sum(weight)
        ] / hbai_dt[, sum(weight)]
        message(
            "share_below_mis: ", length(unmatched),
            " household type(s) have no MIS budget and are ",
            "excluded (", round(100 * dropped, 1),
            " per cent of weighted households): ",
            paste(head(sort(unmatched), 5), collapse = "; "),
            if (length(unmatched) > 5) " ..." else ""
        )
    }

    grp <- c("household_type", by)
    out <- dt[, .(
        threshold   = first(threshold),
        share_below = sum(weight * (income < threshold)) / sum(weight),
        n_records   = .N
    ), by = grp]

    if (!is.null(by) && "region" %in% by) {
        # GVTREGN codes. Note 3 IS SKIPPED, and that GVTREGN orders the
        # nations Wales then Scotland while NEWGVTREGN does the reverse, so
        # the two must not be mixed.
        HBAI_REGIONS <- c(
            "1" = "North East", "2" = "North West",
            "4" = "Yorkshire and The Humber", "5" = "East Midlands",
            "6" = "West Midlands", "7" = "East", "8" = "London",
            "9" = "South East", "10" = "South West", "11" = "Wales",
            "12" = "Scotland", "13" = "Northern Ireland"
        )

        out[, region_name := HBAI_REGIONS[as.character(region)]]
    }
    out[]
}
