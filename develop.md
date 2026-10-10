# Developing Harmonized Historical ACS Measures

This guide covers maintaining the measure catalog, building the dataset, and
preparing releases. For downloading and using the data, see [README.md](README.md).

## Repository layout

| File | Role |
| --- | --- |
| `measures.yml` | Measure definitions, source-variable rules, geography coverage, and calculation settings |
| `CPI_2010-2024.csv` | R-CPI-U-RS inputs for inflation adjustment |
| `make_hh_acs_data.R` | Command-line entry point for a complete tract release |
| `R/catalog.R` | Catalog validation, year-rule selection, and source-file inventories |
| `R/summary_files.R` | Source downloads, missing-value parsing, and table-based Summary File reading |
| `R/sequence_files.R` | Sequence-based Summary File reading |
| `R/calculations.R` and `R/moe.R` | Estimate calculations, inflation adjustment, rounding, and derived MOEs |
| `make_hh_acs_data.R` and `R/release.R` | Annual builds and versioned release-asset preparation |
| `tests/test-corrections.R` | Regression checks for MOEs, source parsing, catalog validation, and annual calculations |
| `tests/test-release.R` | Fixture-based checks for release output and annual column types |

## Maintain the catalog

All 30 measures are defined in [measures.yml](measures.yml). Each entry contains
its title, definition, domain, units, calculation method, MOE method, estimate
rounding, source rules, and notes. Year ranges are inclusive `[first, last]`
ranges of ACS ending years. The R calculations use these rules directly.

To update a measure, edit its catalog entry and review the changes. Rebuild data
when source variables, calculation rules, or estimate rounding change. Edit
definitions and notes directly in the catalog, and include the updated catalog
in the release that uses those definitions.

`calculation` selects an implementation in `R/calculations.R`:

- `proportion`: sum numerator components and divide by the specified denominator.
- `count`: sum source counts.
- `estimate`: use a published ACS estimate, including medians.
- `inflation_adjusted`: scale a source measure and its MOE by
  CPI(base year) / CPI(ACS year).

YAML holds the definitions and configuration. A measure requiring a new algorithm
also needs a corresponding R calculator and catalog validator update.

Catalog schema version 3 keys each row by geography identifier and ACS year.
It records coverage and preserves each year's published identifiers. Overlapping
rules and gaps between rules are rejected. A measure can begin later than
geography coverage; its earlier estimates and MOEs remain missing.
Every measure's rules must cover the final release year, so a forgotten year
extension fails validation instead of silently producing missing values.

Proportion MOEs use the ACS subset formula, with the ratio approximation only
when the radicand is negative. A proportion equal to one uses the numerator MOE
divided by the denominator. Controlled-estimate MOE markers (`*****` and
`-555555555`) are parsed as zero; suppressed and unavailable MOEs remain missing.

The catalog specifies employment from 2011, health insurance and education from
2012, and limited-English households (`C16002`) from 2016. Pre-1970 housing uses
`B25034_007`–`010` through 2014 and `B25034_008`–`011` from 2015.

## Add an ACS year

Extend the tract geography coverage and applicable measure rules, and add the
annual CPI value to the inflation input. If you rename the CPI file, update
`dataset.inflation.file` in the catalog. Check source-variable labels, table
changes, and geography changes for the added year before building.

The 2024 annual R-CPI-U-RS input is **462.5**, documented in the catalog from the
Census Bureau's
[2024 ACS comparison guidance](https://www.census.gov/programs-surveys/acs/guidance/comparing-acs-data/2024.html).
The required 2024 source-variable labels match 2023 apart from dollar-year
references, and none of the 19 required source tables appear in the Census
Bureau's
[2024 table changes](https://www.census.gov/programs-surveys/acs/technical-documentation/table-and-geography-changes/2024/5-year.html).

Geographic coverage comes from source records, so check changes in yearly tract
sets as well as column definitions. For example, the 2024 Summary Files include
tract `36111954401` (Ulster County, NY), which was absent in 2023. It receives a
2024 row; a 2023 row would require a record in a required source table for 2023.

## Build a release

Run commands from the repository root. Install the R dependencies:

```r
install.packages(c("yaml", "dplyr", "data.table", "stow", "arrow"))
```

Choose a semantic release version and build the complete tract history:

```sh
./make_hh_acs_data.R 3.0.0
```

This example stages three assets under `data/v3.0.0/`:

- `hh_acs_measures-v3.0.0.parquet`
- `hh_acs_measures-v3.0.0.measures.yml`
- `hh_acs_measures-v3.0.0.CPI.csv`

An optional second argument selects another output directory. The builder
calculates one year at a time and writes one Parquet row group per year, keeping
build memory bounded by annual data. Each row group is sorted by tract identifier,
with statistics enabled for filtering. All years in the catalog's tract coverage
must be present; a partial build is not finalized as a release. Existing assets
are protected from overwriting.

The release catalog snapshots the measure definitions and adds coverage, row
counts, primary keys, columns, yearly row groups, and source-file URLs. The CPI
input is copied into the release, and the snapshot references its versioned
filename.

## Test and publish

Run the release tests from the repository root:

```sh
Rscript tests/test-release.R
Rscript tests/test-corrections.R
```

Tests also require `nanoparquet` in addition to the build dependencies. They use
generated fixtures and do not download Census source files.

Upload the completed assets to the matching GitHub release separately; the
builder stages local files and does not publish them. For the example above,
use the `v3.0.0` release tag. Adding a year or changing definitions, calculations,
or CPI inputs produces a new complete snapshot under a new version. Keep
published releases available so analyses can pin an exact dataset.

## Source downloads and caching

The pipeline downloads Census Summary Files through `stow::stow()`. The catalog
requires 15–19 tables per year, depending on measure availability. Neither
importer requires a Census API key.

- **2010–2020:** The annual lookup maps table variables to sequence-file columns.
  The importer downloads only required state sequence ZIPs and geography CSVs,
  joins estimates and MOEs to geographic records by `LOGRECNO`, preserves
  leading zeros, and reads the selected columns.
- **2021 onward:** Table-based files supply tract estimates and MOEs.

Each year's output rows are the union of identifiers present in the required
source tables, restricted to tracts in the 50 states and DC. The importer
preserves published identifiers, including Connecticut's planning-region tract
IDs from 2022 onward and water-only tracts when published. No cartographic
boundary files or tract crosswalks are used. Records absent from some tables
have missing values for measures that depend on those tables; an identifier
absent from every required table has no output row.

`stow` stores source files under the `hh.acs.measures` package namespace, in
`summary-files/<year>` subdirectories. Sequence-based files are grouped further
by geography and state; their lookup files are shared. Downloads use
`etag = FALSE`, so an existing year-specific copy is reused without contacting
the server. Parsed tables are reused within an annual build; measures are
recalculated.

Locate and inspect the build's cache:

```r
stow::stow_path(package = "hh.acs.measures")
stow::stow_info(package = "hh.acs.measures")
```

To download a revised source file, remove its cached copy using the source URL
and subdirectory used by the importer. For a table-based 2024 source file:

```r
# Replace source_url with the URL of the file to remove:
# stow::stow_remove(source_url, package = "hh.acs.measures", subdir = "summary-files/2024")
```

`R_USER_DATA_DIR` can relocate the cache. Once all source files are cached, builds
can run offline:

```sh
HH_ACS_OFFLINE=1 ./make_hh_acs_data.R 3.0.0
```
