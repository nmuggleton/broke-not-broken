# =====================================================================
# composition_gaps.R
#
# In-text statistics for Section 2.3: how the share of households
# below the standard moves with household composition.
# =====================================================================

library(data.table)
library(stringr)

# Parse household composition out of the US/GB household_type labels.
parse_household_composition <- function(household_type) {
    adults <- fifelse(
        grepl("^1 adult|^Single|^Lone", household_type), 1L, 2L
    )
    children <- as.integer(str_extract(household_type, "\\d+(?=\\schild)"))
    children[is.na(children)] <- 0L
    pensioner <- as.integer(grepl("pensioner", household_type,
        ignore.case = TRUE
    ))

    data.table(household_type, adults, children, pensioner)
}

# Composition gaps in the national share below standard: one vs two
# adults (matched on children), the per-child slope within each
# adult count, and (GB) the working-age/pensioner gap and slope among
# childless households.
compute_composition_gaps <- function(national, has_pensioners = FALSE) {
    dt <- merge(as.data.table(national),
        parse_household_composition(national$household_type),
        by = "household_type"
    )

    if (has_pensioners) dt <- dt[pensioner == 0]

    # One vs two adults, matched on children.
    adult_gap <- dcast(dt, children ~ adults, value.var = "share_below")
    setnames(adult_gap, c("1", "2"), c("one_adult", "two_adults"))
    adult_gap[, `:=`(
        ratio   = one_adult / two_adults,
        pp_diff = one_adult - two_adults
    )]

    # Per additional child, within adult count.
    child_slope <- dt[order(children),
        .(slope = coef(lm(share_below ~ children))[2]),
        by = adults
    ]

    out <- list(adult_gap = adult_gap[], child_slope = child_slope[])

    if (has_pensioners) {
        pen <- merge(as.data.table(national),
            parse_household_composition(national$household_type),
            by = "household_type"
        )
        pen <- pen[children == 0, .(
            household_type, share_below, adults,
            pensioner
        )]

        pensioner_gap <- dcast(pen, pensioner ~ adults,
            value.var = "share_below"
        )
        setnames(pensioner_gap, c("1", "2"), c("one_adult", "two_adults"))
        pensioner_gap[, `:=`(
            ratio   = one_adult / two_adults,
            pp_diff = one_adult - two_adults
        )]

        pensioner_slope <- pen[order(pensioner),
            .(slope = coef(lm(share_below ~ pensioner))[2]),
            by = adults
        ]

        out$pensioner_gap   <- pensioner_gap[]
        out$pensioner_slope <- pensioner_slope[]
    }

    out
}
