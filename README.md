# Harmonized Historical ACS Measures

<!-- badges: start -->
[![latest github release for hh_acs_measures dpkg](https://img.shields.io/github/v/release/geomarker-io/hh_acs_measures?sort=date&filter=hh_acs_measures-*&display_name=tag&label=%5B%E2%98%B0%5D&labelColor=%238CB4C3&color=%23396175)](https://github.com/geomarker-io/hh_acs_measures/releases?q=hh_acs_measures&expanded=false)
<!-- badges: end -->

## About

This R code generates the **Harmonized Historical ACS Measures** (`hh_acs_measures`) data resource. Census tract-level measures derived from the American Community Survey (ACS) are generally available annually from 2013 through 2023 and were selected to cover three domains:

- **Population**: number of kids, number of households with kids, single-parent households, racial and ethnic composition, limited English speaking households
- **Socioeconomic**: education, income, poverty, employment, health insurance, SNAP receipt
- **Housing**: vacancy, age, substandard conditions, monthly rent and housing costs

## Accessing Data

Download the `hh_acs_measures` datapackage from the [release](https://github.com/geomarker-io/hh_acs_measures/releases), or read into R directly using the [dpkg](https://cole-brokamp.github.io/dpkg/) package:

```r
dpkg::stow("https://github.com/geomarker-io/hh_acs_measures/releases/download/hh_acs_measures-v1.3.0/hh_acs_measures-v1.3.0.parquet") |>
	dpkg::read_dpkg()
```

[View field metadata](https://github.com/geomarker-io/hh_acs_measures/blob/main/metadata.md)

## Data Details

#### Types of ACS measures

Each of the derived ACS measures are expressed in one of three ways:

1. measures starting with **`n_`** represent a **number** of something, such as `n_household`
    - always rounded to the nearest integer

2. measures starting with **`fraction_`** represent a **fraction** of some total, such as `fraction_poverty`
    - bounded between 0 and 1
	- always rounded to three decimal places (e.g. 0.064)

3. measures starting with **`median_`** represent a **median** of a number in a population, such as `median_income`
    - always expressed using three significant digits (e.g., 5,410 and 145,000)

#### Currency adjustments for purchasing power over time

Measures that are reported in US Dollars (e.g., Median Household Income) are adjusted for changes in purchasing power over time by using the annual [Consumer Price Index](https://www.bls.gov/cpi/research-series/r-cpi-u-rs-home.htm). The 'Consumer Price Index retroactive series using current methods' (R-CPI-U-RS) produced by the Bureau of Labor Statistics harmonizes historical annual indices by incorporating methodological improvements into earlier calculations with new releases. This means that `hh_acs_measures` fields that are adjusted for inflation in prior years will likely be scaled to a different number in later releases.

#### Missing Data

Some data are unavailable in certain earlier years because the question was not included in the ACS that year.  Additionally, data summaries may be [suppressed ](https://www.census.gov/programs-surveys/acs/technical-documentation/data-suppression.html) by the census bureau.

#### Margin of error

Each measure includes a accompanying [margin of error](https://walker-data.com/tidycensus/articles/margins-of-error.html) (or `_moe`) variable, such as `median_income_moe`, that is calculated using the ACS's suggested methodology implemented in [tidycensus](https://walker-data.com/tidycensus/index.html).

