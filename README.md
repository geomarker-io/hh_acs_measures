# Harmonized Historical ACS Measures

This repository builds harmonized measures from the five-year American Community
Survey (ACS), covering population, socioeconomic conditions, and housing.
The implemented coverage is **2021–2023** for census tracts in the 50 states and
DC and for ZCTAs. The importer supports the table-based Summary File format
used from 2021 onward. Earlier years are outside the supported scope.

[Measure definitions and year-specific notes](metadata.md) ·
[Authoritative measure catalog](measures.yml) ·
[GitHub releases](https://github.com/geomarker-io/hh_acs_measures/releases)

## Data and metadata

The next release built with this workflow will contain ordinary Parquet data
and accompanying JSON metadata. Data access does not require `dpkg` or a package
from this repository. Download the assets from GitHub Releases, optionally cache
them using `stow`, and read the local Parquet file with `arrow::read_parquet()`.
Historical releases retain their existing asset formats and names.

Each geography's local release build stages five versioned assets:

- A `.parquet` data file.
- A `.metadata.json` file describing every column, including MOEs, keys, units,
  calculation rules, coverage, and geography notes.
- A `.measures.yml` snapshot of the catalog used for the build.
- A `.metadata.md` snapshot of the human-readable documentation.
- A `.CPI.csv` snapshot of the inflation-adjustment input.

Release metadata is a small project-specific JSON format, identified by
`metadata_schema_version`; it does not claim Frictionless schema compliance.
Use the metadata attached to the same release as the data. The documentation on
`main` describes the current catalog and may differ from an older release.
Build scripts stage files locally and refuse to overwrite existing assets.
Upload and publication are separate, manual GitHub Release steps.

## Maintaining definitions

All 30 measures live in **one `measures.yml` file**. Each entry contains its title,
definition, domain, units, calculation method, MOE method, rounding, year-specific
source rules, and explanatory notes. Year ranges are inclusive `[first, last]`
ranges of ACS ending years.

Each rule specifies the ACS source variables and denominator for an inclusive
year range. The same rules select the calculation inputs and appear in the
generated documentation. All currently supported years use 2020 boundaries.
Connecticut tract identifiers are harmonized before measures are calculated
for 2022 onward, as documented in the geography notes.

To update a measure:

1. Edit its definition, notes, or source rules in `measures.yml`.
2. Run `Rscript build_metadata.R` to regenerate Markdown and both JSON dictionaries.
3. Review the generated changes and run `Rscript tests/run.R`.
4. Rebuild data when inputs, calculation rules, or rounding change. A prose-only
   edit can regenerate documentation without downloading or calculating ACS data.

Do not edit the generated Markdown or JSON files directly. Missing years outside
a measure's rules remain missing in the output. Overlapping rules and gaps
between rules are rejected. Availability starts preserve the previous pipeline;
they are not claims that an ACS question was absent in earlier years. To add a
later year, extend the geography coverage and measure rules and add its CPI
value. Years before 2021 are rejected.

`calculation` selects a shared R implementation in `R/calculations.R`:

- `proportion`: sum numerator components, divide by the specified denominator,
  and calculate the MOE using the independent ACS formulas in `R/moe.R`.
- `count`: sum source counts with the corresponding MOE.
- `estimate`: use a single published ACS estimate and MOE, including medians.
- `inflation_adjusted`: scale an unrounded source measure and its MOE by
  CPI(base year) / CPI(ACS year).

YAML contains configuration, not executable R. A future measure requiring a new
algorithm would need an R calculator and a matching catalog validator update.
Source-variable codes can be specified directly as lists; do not encode R
expressions such as `paste0()` in YAML.

## Local builds

Run commands from the repository root. Install the R dependencies as needed:

```r
install.packages(c("yaml", "jsonlite", "dplyr", "data.table", "stow",
                  "foreign", "arrow"))
```

Documentation alone requires only `yaml` and `jsonlite`:

```sh
Rscript build_metadata.R
# Optional: write documentation into a separate directory.
Rscript build_metadata.R /tmp/hh-acs-docs
```

Data builds download public Census files through `stow`; no API key is needed.
There is no dependency on `tidycensus` or `tigris`. Provide an explicit version;
these examples illustrate a prospective version, not a published release:

```sh
Rscript make_hh_acs_data.R 2.0.0
Rscript make_hh_acs_data_zcta.R 2.0.0
```

Both commands stage their assets under `data/v2.0.0/`. An optional second argument
selects another output directory. The two entry points use the same catalog,
calculators, metadata generator, and release writer.

## Downloaded source files

`R/summary_files.R` derives the required table IDs from `measures.yml` and downloads
only those year-specific `.dat` files through `stow::stow()`. The current catalog
uses 19 tables per year (57 files in total). Each file supplies both tract and
ZCTA data, so the second geography build reuses the same downloads. The importer
reads only the required estimate and MOE columns and retains the requested
geographic summary level. Census sentinel values become missing values.

The fixed 2020 cartographic boundary ZIP files are also downloaded through `stow`.
Only their DBF attribute tables are read; no geometry library is needed. They
preserve the previous output scaffold and its exclusion of water-only tracts.
The Connecticut crosswalk and CPI input are already tracked in this repository.
JSON metadata records the source table URLs and boundary asset URL.

`stow` owns durable raw-file storage under the `hh.acs.measures` package namespace,
with `summary-files/<year>` and `boundaries` subdirectories. The importer reads
the path returned by `stow` directly. Parsed tables are reused within a build;
calculated measures are always recomputed. The old API-response and
calculated-measure caches are not used.

Downloads use `etag = FALSE`: an existing year-specific file is reused without
checking the server. Use `stow::stow_remove()` to deliberately remove a source
copy before downloading a revised Census file. `stow::stow_info()` and
`stow::stow_path(package = "hh.acs.measures")` can inspect the managed files.
The standard `R_USER_DATA_DIR` environment variable can relocate the cache.
After all needed assets have been downloaded, builds can run entirely offline:

```sh
HH_ACS_OFFLINE=1 Rscript make_hh_acs_data.R 2.0.0
HH_ACS_OFFLINE=1 Rscript make_hh_acs_data_zcta.R 2.0.0
```

An offline build fails if any required file is absent; it never falls back to
an API or makes network requests.

## Conventions and verification

Census files publish 90% MOEs. The importer converts these to 95% using
`1.960 / 1.645`. Component sums use root-sum-of-squares, retaining only the
largest MOE among zero-estimate components. Subset proportions use the ACS
subtraction formula, with the previous ratio fallback for nonpositive
radicands. Denominator MOEs retain the previous input adapter's whole-unit
rounding; numerator MOEs remain unrounded until final formatting. The formulas
are documented in [Census accuracy guidance](https://www2.census.gov/programs-surveys/acs/tech_docs/accuracy/MultiyearACSAccuracyofData2023.pdf).

Counts and their MOEs are rounded to integers; fractions and their MOEs to three
decimal places; medians and their MOEs to three significant digits. Fraction
estimates represent proportions, while fraction MOEs are uncertainty widths and
are not constrained to `[0, 1]`. Zero or missing denominators produce missing
fractions and MOEs. Missing source components are not silently dropped.

Nominal dollar measures retain the original values. `_2010adj` fields and their
MOEs use R-CPI-U-RS from `CPI_2010-2023.csv` to express values in 2010 dollars.
Revised CPI inputs can change earlier adjusted estimates in later releases.

```sh
Rscript tests/run.R
```

Tests use artificial ACS responses without network access or credentials. They
compare all measures for 2021–2023 against frozen results from the
pre-migration scripts, exercise the independent MOE formulas and bulk-file
parser, check geography corrections and missing-value handling, confirm
that catalog edits affect both calculation inputs and documentation, and stage
and read back a small Parquet/JSON release. They do not substitute for a live
full source-file rebuild and review before publishing a new data release.
