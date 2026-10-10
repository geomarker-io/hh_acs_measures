# Harmonized Historical ACS Measures

Harmonized Historical ACS Measures provides **30 measures of neighborhood
population, socioeconomic conditions, and housing** for census tracts in the
**50 states and DC, 2010–2024**. Use these five-year American Community Survey
(ACS) estimates to link neighborhood characteristics to research data and examine
patterns over time.

## Download

Download the `.parquet` file from
[GitHub Releases](https://github.com/geomarker-io/hh_acs_measures/releases).
Each file contains the complete supported history and can be read by R, Python,
and database tools with Parquet support.

Keep the matching `.measures.yml` catalog as your data dictionary: it documents
measure definitions, units, source variables, available years, and release
coverage. The accompanying `.CPI.csv` records the inflation-adjustment inputs;
adjusted values are already included in the data.

## Data structure

The dataset is one wide table with **one row per census tract and ACS ending
year**, and **62 columns**: two identifiers, 30 estimates, and 30 margins of error.

| Column | Type | Meaning |
| --- | --- | --- |
| `census_tract_id` | String | 11-character tract identifier; preserve leading zeros |
| `year` | Integer | Ending year of the five-year estimate; `2024` covers 2020–2024 |
| `<measure>` | Numeric | Estimate, such as `n_pop`, `fraction_poverty`, or `median_income` |
| `<measure>_moe` | Numeric | Margin of error at the 90% confidence level, in the estimate's units |

Use **both `census_tract_id` and `year`** as the key when joining or importing
records. Fractions use proportions (`0.15` means 15%); fields ending in
`_2010adj` express dollars in 2010 purchasing power. Unavailable or suppressed
values are missing, and **the set of tracts can differ by year**.

Measure definitions account for year-specific ACS variables. **Tract boundaries
follow each ACS release and are not harmonized across years**, so match geographic
assignments to the selected ACS year.

[Browse measure definitions](measures.yml) ·
[Development guide](develop.md) · [Release changelog](CHANGELOG.md)

## Read into R

```r
# install.packages(c("stow", "nanoparquet"))

d <- 
  "https://github.com/geomarker-io/hh_acs_measures/releases/download/" |>
  paste0("v3.0.0/hh_acs_measures-v3.0.0.parquet") |>
  stow::stow() |>
  nanoparquet::read_parquet()
```

## Read subset into R

Use Arrow to filter before loading rows into R. This example reads only Ohio
tracts (`39` is Ohio's state FIPS code) for 2024. The complete file is downloaded
and cached by `stow`, but only the matching rows are collected into memory.

```r
# install.packages(c("stow", "arrow", "dplyr"))

d <-
  "https://github.com/geomarker-io/hh_acs_measures/releases/download/" |>
  paste0("v3.0.0/hh_acs_measures-v3.0.0.parquet") |>
  stow::stow() |>
  arrow::open_dataset() |>
  dplyr::filter(year == 2024, substr(census_tract_id, 1, 2) == "39") |>
  dplyr::collect()
```

## License and source data

Original project code, documentation, and any copyrightable contributions to
released datasets are available under the [MIT license](LICENSE). Retain its
copyright and permission notice when redistributing that material.

The public ACS Summary File data are U.S. government works and are not subject
to copyright protection in the United States. They may be used and redistributed
without copyright permission; the MIT license does not impose restrictions on
these public-domain source data. See
[17 U.S.C. § 105](https://www.copyright.gov/title17/92chap1.html#105) and the
[Census Bureau's public-access policy](https://www2.census.gov/foia/ds_policies/ds027.pdf).
The BLS inflation inputs are also public domain under its
[copyright policy](https://www.bls.gov/bls/linksite.htm).

Credit the U.S. Census Bureau as the source of the original ACS data and BLS as
the source of the inflation inputs. Identify this project as the source of the
derived measures and calculations. These derived measures are produced by the
project contributors; they are not official Census Bureau estimates. Census's
[citation guidance](https://www.census.gov/about/policies/citation.html) explains
how to acknowledge original data separately from subsequent analyses.

## Suggested citation

> Harmonized Historical ACS Measures contributors. *Harmonized Historical ACS
> Measures* (version VERSION) [Data set]. geomarker.io.
> https://github.com/geomarker-io/hh_acs_measures/releases/tag/TAG

Replace `VERSION` and `TAG` with the version and exact release tag used in your
analysis. Use the release's publication year when your citation style requires
one. [CITATION.cff](CITATION.cff) supplies citation metadata for GitHub and other
tools; the template above identifies the specific dataset snapshot.

Also acknowledge the original inputs:

> Original data: U.S. Census Bureau, American Community Survey 5-Year Summary
> Files, 2010–2024. Inflation inputs: U.S. Bureau of Labor Statistics, Consumer
> Price Index retroactive series using current methods (R-CPI-U-RS).

Adjust the ACS years to those used in your analysis. The matching release
catalog records the source URLs and CPI documentation.
