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
[Development guide](develop.md)

## Short R example

```r
# install.packages(c("stow", "nanoparquet"))

d <- 
  "https://github.com/geomarker-io/hh_acs_measures/releases/download/" |>
  paste0("v3.0.0/hh_acs_measures-v3.0.0.parquet") |>
  stow::stow() |>
  nanoparquet::read_parquet()
```
