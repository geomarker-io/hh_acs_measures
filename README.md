# Harmonized Historical ACS Measures

Harmonized Historical ACS Measures provides **30 ready-to-use measures of
neighborhood population, socioeconomic conditions, and housing** for census
tracts in the 50 states and DC from **2010–2024**. Use these five-year American
Community Survey (ACS) estimates to add neighborhood characteristics to your
research data and examine patterns over time.

Measures use year-specific ACS variable mappings to keep their definitions
consistent across years. **Tract boundaries follow each ACS release and are not
harmonized across years.**

[Download the data](https://github.com/geomarker-io/hh_acs_measures/releases) ·
[Browse measure definitions](measures.yml)

## What is in the dataset?

The data contain one row per **census tract and ACS ending year**, with an
estimate and a margin of error for each measure.

| Domain | Examples |
| --- | --- |
| Population | Population and household counts, children, race and ethnicity, limited-English households |
| Socioeconomic | Poverty, household income, employment, health insurance, education, assisted income |
| Housing | Housing units, home values, renters, rent burden, housing conditions, age of housing, vacancy |

The [measure catalog](measures.yml) gives the definition, units, denominator,
source variables, available years, and calculation notes for every measure.

## Get started in R

1. Open [GitHub Releases](https://github.com/geomarker-io/hh_acs_measures/releases)
   and choose a release.
2. Download its `.parquet` data file and matching `.measures.yml` catalog from
   the release's **Assets** list. Keep them together so you have the definitions
   that accompany your data.
3. Read the Parquet file and select the measures and years you need:

```r
# Install once:
install.packages("nanoparquet")

# Choose the downloaded .parquet file:
path <- file.choose()
acs <- nanoparquet::read_parquet(path)

# Poverty estimates and their margins of error for 2024:
poverty_2024 <- acs[
  acs$year == 2024,
  c("census_tract_id", "year", "fraction_poverty", "fraction_poverty_moe")
]
head(poverty_2024)
```

`nanoparquet` loads the complete file, including all years, into an R data frame.
You can use the downloaded data directly without building it or obtaining a
Census API key.

Each release includes three files, named with the release version:

| File ending | Contents |
| --- | --- |
| `.parquet` | Tract estimates and margins of error for the complete supported history |
| `.measures.yml` | Measure definitions and release metadata, including coverage, row counts, columns, and source-file URLs |
| `.CPI.csv` | Inflation-adjustment inputs, for documenting or reproducing adjusted dollar values |

The inflation adjustments are already applied in the data. You do not need to
load the CPI file or the YAML catalog to read the Parquet file. Use the catalog
from your chosen release as its data dictionary; [measures.yml](measures.yml)
describes the definitions in this repository. Record the release version in your
analysis so you can identify the exact data you used.

For a cached download in R, copy the Parquet asset's download URL from the
release's Assets list:

```r
# Install once:
install.packages("stow")

parquet_url <- readline("Paste the Parquet download URL: ")
path <- stow::stow(parquet_url)
acs <- nanoparquet::read_parquet(path)
```

`stow::stow()` returns the path to a managed local copy. Repeat the call to reuse
it, or add `offline = TRUE` to require an existing cached copy.

## Join the data to your analysis

Use **both `census_tract_id` and `year`** as the join keys. `census_tract_id` is an
11-character tract identifier stored as text to preserve leading zeros. Keep
your tract identifiers as text too, and choose the ACS ending year that fits
your analysis.

Match your tract assignments or geographic files to the geography of the chosen
ACS year. Boundaries and identifiers can change between years, and an unchanged
identifier does not guarantee unchanged boundaries. The dataset preserves
published identifiers, including Connecticut's planning-region tract identifiers
from 2022 onward. Comparing areas on fixed boundaries requires a separate
geographic harmonization step.

## Interpret the estimates

- **Years:** `year` is the ending year of a five-year estimate. For example,
  `2024` covers the five-year period 2020–2024. Successive ACS ending years cover
  overlapping periods.
- **Units:** `fraction_` measures are fractions, so `0.15` means 15%. Check the
  catalog for each field's units; `median_rent_to_income_percentage` is expressed
  as a percentage.
- **Margins of error:** Every measure has a matching `_moe` column in the same
  units, at the published **90% confidence level**. Published measures retain
  their source MOEs; derived measures have calculated MOEs.
- **Dollar values:** Fields ending in `_2010adj` express values in **2010 US
  dollars**, adjusted using R-CPI-U-RS. Their MOEs use the same adjustment.
  Unadjusted income and home-value fields are in nominal dollars.
- **Missing values:** Missing or suppressed source values remain missing. A
  missing or zero denominator produces a missing fraction and MOE. Employment
  measures begin in 2011, health insurance and education in 2012, and
  limited-English households in 2016; their estimates and MOEs are missing
  before those years.
- **Rounding:** Counts are rounded to integers, fractions to three decimal
  places, and medians to three significant digits. MOEs retain their precision.

The set of tracts can differ by year. Each year's rows include the tract
identifiers present in at least one required ACS source table, including
water-only tracts when published. A tract absent from all required tables has
no row for that year; a tract present in only some tables has missing values
for measures whose source records are absent.

## Read selected years and columns with Arrow

If you only need part of the data, Arrow can query the downloaded Parquet file
without loading its full history into an R data frame. Use the same `path` as
above:

```r
# Install once:
install.packages(c("arrow", "dplyr"))
library(dplyr)

result <- arrow::open_dataset(path) |>
  filter(year %in% c(2023L, 2024L), census_tract_id == "39061000200") |>
  select(census_tract_id, year, fraction_poverty, fraction_poverty_moe) |>
  collect()
```

Apply filters and column selections before `collect()`, which returns the result
as an R data frame. Arrow can read only the selected columns and skip years
outside the query. The initial download still transfers the complete file.

For catalog maintenance, building data, testing, and publishing releases, see
[develop.md](develop.md).
