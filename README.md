# Harmonized Historical ACS Measures

## About

This R code generates the **Harmonized Historical ACS Measures** (`hh_acs_measures`) data resource. Census tract-level measures derived from the American Community Survey (ACS) are generally available annually from 2013 onward and were selected to cover three domains:

- **Population**: number of kids, number of households with kids, single-parent households, racial and ethnic composition, limited English speaking households
- **Socioeconomic**: education, income, poverty, employment, health insurance, SNAP receipt
- **Housing**: vacancy, age, substandard conditions, monthly rent and housing costs

## Getting Data

#### CSV

Data as a CSV file is stored on AWS S3 at: [`s3://codec-data/hh_acs_measures/hh_ACS_measures.csv`](https://codec-data.s3.amazonaws.com/hh_acs_measures/hh_acs_measures.csv)

To read this CSV file into R directly from its online location, use:

```r
readr::read_csv("https://codec-data.s3.amazonaws.com/hh_acs_measures/hh_acs_measures.csv")
```

#### CODEC tabular-data-resource

Data as a CODEC tabular-data-resource are stored on AWS S3 at: `s3://codec-data/hh_acs_measures/`. 

After downloading this folder to your working directory, read this data and its [metadata](https://geomarker.io/CODECtools/articles/codec-metadata.html) into R with:

```r
CODECtools::read_tdr_csv("hh_acs_measures")
```

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

Measures that are reported in US Dollars (e.g., Median Household Income) are adjusted for changes in purchasing power over time by using the annual [Consumer Price Index](https://www.bls.gov/cpi/research-series/r-cpi-u-rs-home.htm)

#### Missing Data

Some data are unavailable in certain earlier years because the question was not included in the ACS that year.  Additionally, data summaries may be [suppressed ](https://www.census.gov/programs-surveys/acs/technical-documentation/data-suppression.html) by the census bureau.

#### Margin of error

Each measure includes a accompanying [margin of error](https://walker-data.com/tidycensus/articles/margins-of-error.html) (or `_moe`) variable, such as `median_income_moe`, that is calculated using the ACS's suggested methodology implemented in [tidycensus](https://walker-data.com/tidycensus/index.html).

## Metadata

### Descriptors

|name        |value                                                           |
|:-----------|:---------------------------------------------------------------|
|name        |hh_acs_measures                                                 |
|path        |hh_acs_measures.csv                                             |
|title       |Harmonized Historical American Community Survey Data            |
|description |ACS variables from 2010 - 2019, census tracts for contiguous US |
|url         |https://github.com/geomarker-io/hh_acs_measures                 |

### Schema

Columns ending with `_moe` represent the margin of error accompanying another column and are not included in the schema table here.

|name                        |title                                                            |description                                                                                                                      |type    |
|:---------------------------|:----------------------------------------------------------------|:--------------------------------------------------------------------------------------------------------------------------------|:-------|
|census_tract_id_2010        |Census Tract Identifier (2010)                                   |FIPS identifier for census tracts 2010 - 2019                                                                                    |string  |
|year                        |Year                                                             |Vintage of 5-year ACS estimates                                                                                                  |integer |
|fraction_poverty            |Fraction of Households in Poverty                                |Fraction of households with income below poverty level within the past 12 months                                                 |number  |
|n_children_lt18             |NA                                                               |NA                                                                                                                               |integer |
|n_pop                       |NA                                                               |NA                                                                                                                               |integer |
|n_household_lt18            |NA                                                               |NA                                                                                                                               |integer |
|n_household                 |NA                                                               |NA                                                                                                                               |integer |
|fraction_insured            |Fraction of People Insured                                       |Fraction of population with health insurance (available from 2012 onwards only)                                                  |number  |
|fraction_snap               |Fraction of Households Receiving SNAP                            |Fraction of households receiving food stamps/SNAP in the past 12 months                                                          |number  |
|fraction_fam_nospouse       |Fraction of family households with a single householder          |Single householder is male or female household, with no spouse present                                                           |number  |
|fraction_employment         |Fraction of People Employed                                      |Fraction of people employed in civilian labor force                                                                              |number  |
|median_housing_cost         |Median Monthly Housing Costs                                     |Median monthly housing costs in 2010-adjusted dollars                                                                            |number  |
|median_housing_cost_2010adj |NA                                                               |NA                                                                                                                               |number  |
|median_rent                 |Median Monthly Rent                                              |Median monthly rent in 2010-adjusted dollars                                                                                     |number  |
|median_rent_2010adj         |NA                                                               |NA                                                                                                                               |number  |
|fraction_conditions         |Fraction of households with substandard housing conditions       |substandard housing: incomplete plumbing or kitchens, overcrowding, 30% or more of household income spent on rent or owner costs |number  |
|fraction_builtbf1970        |NA                                                               |NA                                                                                                                               |number  |
|fraction_vacant             |Fraction of housing units that are vacant                        |NA                                                                                                                               |number  |
|fraction_nhl                |Fraction of People Not Hispanic/Latino                           |NA                                                                                                                               |number  |
|fraction_nhl_w              |Fraction of People White and Not Hispanic/Latino                 |NA                                                                                                                               |number  |
|fraction_nhl_b              |Fraction of People Black and Not Hispanic/Latino                 |NA                                                                                                                               |number  |
|fraction_nhl_o              |Fraction of People Not Black, Not White, and Not Hispanic/Latino |NA                                                                                                                               |number  |
|fraction_hl                 |Fraction of People Hispanic/Latino                               |NA                                                                                                                               |number  |
|fraction_hl_w               |Fraction of People White and Hispanic/Latino                     |NA                                                                                                                               |number  |
|fraction_hl_b               |Fraction of People Black and Hispanic/Latino                     |NA                                                                                                                               |number  |
|fraction_hl_o               |Fraction of People Not Black, Not White, and Hispanic/Latino     |NA                                                                                                                               |number  |
|fraction_lesh               |Fraction of Households Speaking Limited English                  |Available from 2016 onwards                                                                                                      |number  |
|median_income               |Median Household Income                                          |NA                                                                                                                               |number  |
|median_income_2010adj       |NA                                                               |NA                                                                                                                               |number  |
|fraction_hs                 |Fraction of Adults with At Least High School Education           |Available from 2012 onwards                                                                                                      |number  |
