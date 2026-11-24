# Broke, not broken

Replication code for the figures, tables, and in-text statistics in
"Broke, not broken".

## Layout

One script per figure, table, or block of in-text numbers, grouped by
the section of the paper it belongs to.

- `R/2_background/figure_1/main.R`: net wealth by quintile, US and GB
- `R/2_background/table_1/main.R`: debt composition, bottom 25%
- `R/2_background/figure_2/main.R`: share below a minimum living
  standard
- `R/2_background/text/main.R`: numbers quoted in Section 2.3
- `R/4_structural/figure_3/main.R`: price of credit by income
- `R/4_structural/figure_3/car_loan_rates.R`: inputs for panel B
- `R/4_structural/figure_4/main.R`: patience, income, financial
  outcomes
- `R/4_structural/text/main.R`: models behind Figure 4 and Section 4.1
- `R/utilities/`: loaders and plotting code, by section
- `data/raw/SOURCES.txt`: where every raw file comes from
- `data/raw/external_apr/`: small inputs for Figure 3 (tracked)
- `data/processed/`: written by the scripts (not tracked)
- `scripts/`: download helpers for PUMS and the SCF

## Running

Open `R/R.Rproj` or work from the repository root. Put the raw files
in `data/raw/` as listed in `data/raw/SOURCES.txt`, then:

```bash
Rscript R/2_background/figure_1/main.R
Rscript R/2_background/table_1/main.R
Rscript R/2_background/figure_2/main.R   # reads PUMS; slow first time
Rscript R/2_background/text/main.R       # after figure_2
Rscript R/4_structural/figure_3/car_loan_rates.R
Rscript R/4_structural/figure_3/main.R
Rscript R/4_structural/text/main.R       # GPS + Gallup; slow first time
Rscript R/4_structural/figure_4/main.R   # after text
```

Figures are written to `figures/` and tables to `tables/`.

Packages: data.table, ggplot2, patchwork, ggrepel, scales, haven,
readxl, stringr, sandwich, countrycode, wid.

## Data

Most sources are public (WID, FRED, ONS, SCF, ACS PUMS, EPI, MIS,
World Bank, Global Preferences Survey, New York Fed SCE). Three need
an application or a licence: the Wealth and Assets Survey and HBAI
(UK Data Service) and the Gallup World Poll. None of the microdata is
redistributed here.
