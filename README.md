# Harmonized Historical ACS Measures

This repository builds population, socioeconomic, and housing measures from
five-year American Community Survey (ACS) estimates. It covers **2021–2024** for
census tracts in the 50 states and DC, and for ZIP Code Tabulation Areas (ZCTAs).

[Measure catalog: definitions and year-specific notes](measures.yml) ·
[GitHub releases](https://github.com/geomarker-io/hh_acs_measures/releases)

## Using the data

Download a Parquet data file and its accompanying YAML catalog from a GitHub
release. The catalog describes the measures, source variables, units,
calculation rules, geographic coverage, and year-specific notes.

Read the downloaded data in R:

```r
data <- arrow::read_parquet("path/to/downloaded-file.parquet")
```

You can use `stow` to download and cache release assets. Use the catalog attached
to the same release as the data; [measures.yml](measures.yml) describes the
current definitions in this repository.

Each geography's release build creates three versioned files:

- `.parquet`: data, with one row per geography identifier, vintage, and ACS year.
- `.measures.yml`: the measure catalog, plus release version, data filename,
  row count, column names, keys, and source-file URLs.
- `.CPI.csv`: the inflation-adjustment input.

## Maintaining the catalog

All 30 measures are defined in **one `measures.yml` file**. Each entry contains its
title, definition, domain, units, calculation method, MOE method, estimate
rounding, source rules, and notes. Year ranges are inclusive `[first, last]`
ranges of ACS ending years. The R calculations use these rules directly.

To update a measure, edit its catalog entry and review the changes. Rebuild data
when source variables, calculation rules, or estimate rounding change. Edits to
definitions and notes are made directly in the catalog.

To add a later year, extend the geography coverage and measure rules and add its
CPI value. The importer supports table-based Summary Files from 2021 onward.
Overlapping rules and gaps between rules are rejected.

`calculation` selects an implementation in `R/calculations.R`:

- `proportion`: sum numerator components and divide by the specified denominator.
- `count`: sum source counts.
- `estimate`: use a published ACS estimate, including medians.
- `inflation_adjusted`: scale a source measure and its MOE by
  CPI(base year) / CPI(ACS year).

YAML holds the definitions and configuration. A measure requiring a new algorithm
also needs a corresponding R calculator and catalog validator update.

## Building a release

Run commands from the repository root. Install the R dependencies:

```r
install.packages(c("yaml", "dplyr", "data.table", "stow", "foreign", "arrow"))
```

Choose a release version and build both geographies:

```sh
Rscript make_hh_acs_data.R 2.0.0
Rscript make_hh_acs_data_zcta.R 2.0.0
```

These commands stage files under `data/v2.0.0/`. An optional second argument
selects another output directory. Existing assets are protected from overwriting.
Upload the completed assets to GitHub Releases separately.

## Source downloads

The pipeline downloads Census Summary File tables through `stow::stow()`. The
catalog currently requires 19 tables per year, or 76 files. Each file supplies
both tract and ZCTA data, so builds share the downloaded files. The importer
reads the required estimate and MOE columns and selects the geographic records.

The pipeline also downloads 2020 cartographic boundary ZIP files through `stow`
and reads their DBF attributes to define output identifiers. All supported years
use 2020 boundaries. For 2022 onward, Connecticut planning-region tract IDs are
mapped to 2020 tract IDs using the crosswalk in `data-raw/`. Water-only tracts
outside the cartographic boundaries are excluded. The crosswalk and CPI input
are tracked in this repository.

`stow` stores source files under the `hh.acs.measures` package namespace, in
`summary-files/<year>` and `boundaries` subdirectories. Downloads use
`etag = FALSE`, so an existing year-specific copy is reused without contacting
the server. Parsed tables are reused within a build; measures are recalculated.

Use `stow::stow_path(package = "hh.acs.measures")` to locate the cache,
`stow::stow_info()` to inspect its files, and `stow::stow_remove()` to remove a
copy before downloading a revised source file. `R_USER_DATA_DIR` can relocate
the cache.

Once all source files are cached, builds can run offline:

```sh
HH_ACS_OFFLINE=1 Rscript make_hh_acs_data.R 2.0.0
HH_ACS_OFFLINE=1 Rscript make_hh_acs_data_zcta.R 2.0.0
```

## Data conventions

`year` is the ending year of a five-year ACS estimate. Identifiers are strings
with leading zeros preserved. Each measure has an accompanying `_moe` column
in the same units, at the **published 90% confidence level**. Source MOEs are
retained; derived measures use those inputs to calculate their MOEs.

Estimate rounding is specified in the catalog: counts are rounded to integers,
fractions to three decimal places, and medians to three significant digits.
MOEs retain their precision. Missing source components remain missing, and a
zero or missing denominator produces a missing fraction and MOE.

`_2010adj` fields express dollar values in 2010 dollars using R-CPI-U-RS from
`CPI_2010-2024.csv`. Their MOEs are scaled by the same CPI ratio to keep the units
consistent. Revised CPI inputs can change adjusted values in later releases.

The 2024 annual R-CPI-U-RS value is **462.5**, from the Census Bureau's
[2024 ACS comparison guidance](https://www.census.gov/programs-surveys/acs/guidance/comparing-acs-data/2024.html).
Earlier CPI values are retained from the previous input. The required 2024
source-variable labels match 2023, apart from dollar-year references, and none
of the 19 source tables appear in the Census Bureau's
[2024 table changes](https://www.census.gov/programs-surveys/acs/technical-documentation/table-and-geography-changes/2024/5-year.html).

The 2024 Summary Files include tract `36111954401` (Ulster County, NY),
which was absent in 2023. It already exists in the 2020 scaffold, so its 2024
measures are populated without changing identifiers. Scaffold rows absent from
the source files retain missing estimates and MOEs.
