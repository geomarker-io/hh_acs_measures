# Harmonized Historical ACS Measures

This repository builds population, socioeconomic, and housing measures from
five-year American Community Survey (ACS) estimates. The builder supports
**2010–2024** for census tracts in the 50 states and DC, and **2011–2024** for
ZIP Code Tabulation Areas (ZCTAs). Each release catalog records its actual coverage.

Harmonization applies to measure definitions and year-specific ACS variable
mappings. Geography identifiers follow each ACS release; boundaries are not
harmonized across years.

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

For the planned v3.0.0 release, download and read the tract data
as follows (these URLs will be available after the release is published):

```r
# install.packages(c("stow", "nanoparquet"))
d <-
  "https://github.com/geomarker-io/hh_acs_measures/releases/download" |>
  paste0("/v3.0.0/hh_acs_measures-v3.0.0.parquet") |>
  stow::stow() |>
  nanoparquet::read_parquet()

# Select a measure and its margin of error for 2024:
d[d$year == 2024, c("census_tract_id", "year", "fraction_poverty", "fraction_poverty_moe")]
```

`stow::stow()` returns the path to a managed local copy; `nanoparquet` reads that
file into an R data frame using the column names and types stored in Parquet.
View the accompanying `.measures.yml` catalog for measure definitions, units,
source rules, and notes; it does not need to be read into R.
Repeat the `stow` call to reuse the cached file, or
add `offline = TRUE` to require an existing local copy. For ZCTAs, use the
`hh_acs_measures_zcta-v3.0.0.parquet` and matching `.measures.yml` filenames,
and select the `zcta` identifier column instead of `census_tract_id`.

Each geography's release build creates three versioned files:

- `.parquet`: data, with one row per geography identifier and ACS year.
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
CPI value. The importer uses sequence-based Summary Files for 2010–2020 and
table-based Summary Files from 2021 onward. Overlapping rules and gaps between
rules are rejected. A measure can begin later than geography coverage; its
earlier estimates and MOEs remain missing.

Historical availability follows the previous pipeline: employment begins in
2011, health insurance and education in 2012, and limited-English households
(`C16002`) in 2016. Pre-1970 housing uses `B25034_007`–`010` through 2014 and
`B25034_008`–`011` from 2015. These differences are specified in the catalog.
Catalog schema version 3 keys each row by geography identifier and ACS year.
It records the coverage and preserves each year's published identifiers.

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
install.packages(c("yaml", "dplyr", "data.table", "stow", "arrow"))
```

Choose a release version and build both geographies:

```sh
Rscript make_hh_acs_data.R 3.0.0
Rscript make_hh_acs_data_zcta.R 3.0.0
```

These commands stage files under `data/v3.0.0/`. An optional second argument
selects another output directory. Existing assets are protected from overwriting.
Upload the completed assets to GitHub Releases separately.

## Source downloads

The pipeline downloads Census Summary File tables through `stow::stow()`. The
catalog requires 15–19 tables per year, depending on historical availability.
For 2021 onward, each table file supplies both tract and ZCTA data, so builds
share downloads. For 2010–2020, the annual lookup maps table variables to
sequence-file columns. The importer downloads only required sequence ZIPs and
geography CSVs: state files for tracts and national files for ZCTAs. It joins
estimates and MOEs to geographic records by `LOGRECNO`, preserves leading zeros,
and reads the selected columns. Neither importer requires a Census API key.

Each year's output rows are the union of identifiers present in the required
ACS source tables. Census tract coverage remains the 50 states and DC; ZCTAs
include all published national records. The importer preserves source
identifiers, including Connecticut's planning-region tract IDs from 2022 onward
and water-only tracts when published. No cartographic boundary files or tract
crosswalks are used. An identifier absent from every required table in a year
does not receive an output row. A record present in only some tables has missing
values for measures whose source records are absent.

`stow` stores source files under the `hh.acs.measures` package namespace, in
`summary-files/<year>` subdirectories. Historical files are
grouped further by geography and state; their lookup files are shared. Downloads use
`etag = FALSE`, so an existing year-specific copy is reused without contacting
the server. Parsed tables are reused within a build; measures are recalculated.

Use `stow::stow_path(package = "hh.acs.measures")` to locate the cache,
`stow::stow_info()` to inspect its files, and `stow::stow_remove()` to remove a
copy before downloading a revised source file. `R_USER_DATA_DIR` can relocate
the cache.

Once all source files are cached, builds can run offline:

```sh
HH_ACS_OFFLINE=1 Rscript make_hh_acs_data.R 3.0.0
HH_ACS_OFFLINE=1 Rscript make_hh_acs_data_zcta.R 3.0.0
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
which was absent in 2023. It therefore receives a 2024 row and no 2023 row unless
another required source table publishes it for that year.

Geographic definitions can change between ACS years, even when an identifier is
unchanged. `year` identifies the ACS release geography as well as the ending
year of the five-year estimates. There are no fixed decennial vintage columns
and no interpolation to a common set of boundaries.
