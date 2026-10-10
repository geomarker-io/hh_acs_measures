# Release changelog

## 3.0.0 — In preparation

### Data and compatibility

- Build a complete 2010–2024 tract history for the 50 states and DC, with
  30 estimates and 30 accompanying 90% margins of error.
- Preserve each ACS year's published tract identifiers and boundaries, including
  Connecticut planning-region identifiers from 2022 onward. Remove geographic
  crosswalks and boundary harmonization. Join on `census_tract_id` and `year`;
  the previous `census_tract_vintage` column is removed.
- Limit the current builder to census tracts; historical ZCTA assets remain
  separate releases.
- Replace embedded field metadata and tabular-data-resource packaging with three
  matching assets: Parquet data, a `.measures.yml` catalog, and a `.CPI.csv` input.
  Consumers using the previous `dpkg` interface should read Parquet directly.
- Use the 2024 R-CPI-U-RS input and express adjusted dollars in 2010 purchasing
  power. Revised CPI inputs can change earlier adjusted values.

### Corrections

- Correct proportion MOEs when the estimate equals one; use the ratio
  approximation only when the subset formula's radicand is negative.
- Interpret controlled-estimate MOE markers (`*****` and `-555555555`) as zero
  sampling error while retaining missing values for suppressed estimates.
- Correct the pre-1970 housing variable mapping at the 2015 table change and
  describe poverty as a person-based measure.
- Include inflation-adjusted MOEs for income and home value. Apply estimate
  rounding after calculations and retain MOE precision.

### Build and documentation

- Drive definitions, availability, calculations, and source-variable mappings
  from a validated shared YAML catalog. Reject missing final-year rules and
  invalid geography settings.
- Read sequence-based Summary Files through 2020 and table-based files from
  2021 onward, with persistent `stow` caching and optional offline builds.
- Build one year at a time and write one Parquet row group per year. Protect
  existing release assets and record coverage, row counts, columns, and sources.
- Declare build dependencies in `DESCRIPTION` and record the R version,
  platform, and build-package versions in each newly built release catalog.
- Add fixture-based regression checks for parsing, calculations, catalog
  validation, and release writing.
- Add an MIT license for original code and documentation, source-data reuse
  guidance, a suggested citation, and this changelog.

## Historical releases

These entries summarize changes verified in the repository's tagged history.
Tag names retain their historical spelling.

### [hh_acs_measures_zcta-v1.3.0](https://github.com/geomarker-io/hh_acs_measures/releases/tag/hh_acs_measures_zcta-v1.3.0)

- Add a separate ZCTA-level dataset.

### [hh_acs_measures-v1.3.0](https://github.com/geomarker-io/hh_acs_measures/releases/tag/hh_acs_measures-v1.3.0)

- Add 2023 ACS data and update the build and metadata workflow.

### [v1.2.0](https://github.com/geomarker-io/hh_acs_measures/releases/tag/v1.2.0)

- Add 2022 ACS data and update CPI inputs and release metadata.

### [v1.1.0](https://github.com/geomarker-io/hh_acs_measures/releases/tag/v1.1.0)

- Change `fraction_snap` to use `B19058`, measuring households receiving public
  assistance income or food stamps/SNAP, and update its definition accordingly.

### [v1.0.0](https://github.com/geomarker-io/hh_acs_measures/releases/tag/v1.0.0)

- Add 2021 ACS data.

### [v0.2.0](https://github.com/geomarker-io/hh_acs_measures/releases/tag/v0.2.0)

- Add housing-unit measures. The same commit is also tagged `0.2.0`.

### [0.1.0](https://github.com/geomarker-io/hh_acs_measures/releases/tag/0.1.0)

- Add 2020 ACS data and introduce the historical `census_tract_vintage` field.
