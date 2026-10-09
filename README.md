# Harmonized Historical ACS Measures

This repository builds harmonized measures from the five-year American Community
Survey (ACS), covering population, socioeconomic conditions, and housing.
The implemented coverage is 2010–2023 for census tracts in the 50 states and DC
and 2013–2023 for ZCTAs. Coverage differs by measure.

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

For example, `fraction_builtbf1970` has one rule for 2010–2014 and another for
2015–2023 because the ACS construction-period categories changed. The same
rules select the R calculation inputs and appear in the generated documentation.
The geography section records a separate vintage transition for ZCTAs, and
Connecticut tract identifiers are harmonized before measures are calculated.

To update a measure:

1. Edit its definition, notes, or source rules in `measures.yml`.
2. Run `Rscript build_metadata.R` to regenerate Markdown and both JSON dictionaries.
3. Review the generated changes and run `Rscript tests/run.R`.
4. Rebuild data when inputs, calculation rules, or rounding change. A prose-only
   edit can be rendered without downloading or calculating ACS data.

Do not edit the generated Markdown or JSON files directly. Missing years outside
a measure's rules remain missing in the output. Overlapping rules and gaps
between rules are rejected. Availability starts preserve the previous pipeline;
they are not claims that an ACS question was absent in earlier years.

`calculation` selects a shared R implementation in `R/calculations.R`:

- `proportion`: sum numerator components, divide by the specified denominator,
  and calculate the MOE using `tidycensus::moe_prop()` and, when needed, `moe_sum()`.
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
install.packages(c("yaml", "jsonlite", "dplyr", "tidycensus", "tigris",
                  "sf", "digest", "arrow"))
```

Documentation alone requires only `yaml` and `jsonlite`:

```sh
Rscript build_metadata.R
# Optional: write documentation into a separate directory.
Rscript build_metadata.R /tmp/hh-acs-docs
```

An optional Quarto website renders the same generated Markdown. With Quarto
installed, `quarto preview` or `quarto render` runs the documentation build first
and produces a searchable catalog. Rendering does not download ACS data.

To build data, set `CENSUS_API_KEY` in your private R environment. Provide an
explicit version; these examples illustrate a prospective version, not a
published release:

```sh
Rscript make_hh_acs_data.R 2.0.0
Rscript make_hh_acs_data_zcta.R 2.0.0
```

Both commands stage their assets under `data/v2.0.0/`. An optional second argument
selects another output directory. The two entry points use the same catalog,
calculators, metadata generator, and release writer.

Raw ACS responses are cached under `acs_data_cache/raw-v1/`, keyed by query
arguments including variables, geography, year, survey, and MOE confidence.
This cache is separate from the old calculated-measure cache. Calculations and
Connecticut identifier harmonization are rerun from raw responses, so definition
and crosswalk edits do not reuse stale derived measures. To refresh upstream ACS
responses, remove the raw cache before rebuilding.

## Conventions and verification

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
compare all measures and implemented years against frozen results from the
pre-migration scripts, exercise the existing historical corrections, confirm
that catalog edits affect both calculation inputs and documentation, and stage
and read back a small Parquet/JSON release. They do not substitute for a live
ACS rebuild and review before publishing a new data release.
