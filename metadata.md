# Harmonized Historical ACS Measures

<!-- Generated from measures.yml. Edit the catalog, then run Rscript build_metadata.R. -->

## Dataset conventions

- Year is the ending year of the five-year ACS estimate, not a single-year estimate.
- Availability ranges describe the implemented pipeline; they do not establish that a question or table was absent in earlier ACS years.
- Suppressed or missing ACS values remain missing. A zero denominator yields a missing fraction and MOE.
- Inflation-adjusted estimates and MOEs use the same CPI ratio. Revised CPI inputs can change earlier estimates in later releases.
- Source MOEs are converted from 90% to 95% confidence using 1.960 / 1.645. Denominator MOEs are rounded to whole units before derived proportions, preserving the previous input adapter.
- This build supports 2021 onward table-based Summary Files; the current release coverage is 2021–2023.

All `_moe` fields are 95% confidence margins of error in the units of their accompanying estimates.

## Geography: tract

Coverage: 2021–2023. Key: `census_tract_id`, `census_tract_vintage`, `year`.

### Boundary vintages

- 2021–2023: 2020 boundaries.

- 2022–2023: Connecticut planning-region tract IDs are mapped one-to-one back to 2020 tract IDs using data-raw/ct_tract_crosswalk_2022_2020.csv. Estimates are not interpolated; water-only tracts outside the cartographic scaffold are excluded. [Source](data-raw/README.md).

## Geography: zcta

Coverage: 2021–2023. Key: `zcta`, `zcta_vintage`, `year`.

### Boundary vintages

- 2021–2023: 2020 boundaries.

- 2021–2023: All supported ACS years use 2020 ZCTA boundaries.

## `fraction_poverty` — Fraction of People in Poverty

Fraction of people with income below the poverty level in the past 12 months, among people for whom poverty status is determined

Domain: Socioeconomic. Units: fraction.

Calculation: `proportion`. MOE: ACS subset-proportion formula; ratio fallback for nonpositive radicands; component MOEs combined by root-sum-of-squares with the largest MOE retained among zero-estimate components.

Rounding (estimate and MOE): decimal, 3 digits.

- tract: 2021–2023.
- zcta: 2021–2023.

### Year-specific calculation rules

- 2021–2023: `B17001_002` divided by `B17001_001`.
  2021 source files: [B17001](https://www2.census.gov/programs-surveys/acs/summary_file/2021/table-based-SF/data/5YRData/acsdt5y2021-b17001.dat).
  2022 source files: [B17001](https://www2.census.gov/programs-surveys/acs/summary_file/2022/table-based-SF/data/5YRData/acsdt5y2022-b17001.dat).
  2023 source files: [B17001](https://www2.census.gov/programs-surveys/acs/summary_file/2023/table-based-SF/data/5YRData/acsdt5y2023-b17001.dat).

## `n_children_lt18` — Number of Children

Number of children and adolescents < 18 years of age

Domain: Population. Units: count.

Calculation: `count`. MOE: Root-sum-of-squares for multiple components, retaining the largest MOE among zero-estimate components; source MOE for one component.

Rounding (estimate and MOE): integer.

- tract: 2021–2023.
- zcta: 2021–2023.

### Year-specific calculation rules

- 2021–2023: `B01001_003` + `B01001_004` + `B01001_005` + `B01001_006` + `B01001_027` + `B01001_028` + `B01001_029` + `B01001_030`.
  2021 source files: [B01001](https://www2.census.gov/programs-surveys/acs/summary_file/2021/table-based-SF/data/5YRData/acsdt5y2021-b01001.dat).
  2022 source files: [B01001](https://www2.census.gov/programs-surveys/acs/summary_file/2022/table-based-SF/data/5YRData/acsdt5y2022-b01001.dat).
  2023 source files: [B01001](https://www2.census.gov/programs-surveys/acs/summary_file/2023/table-based-SF/data/5YRData/acsdt5y2023-b01001.dat).

## `n_pop` — Number of Total People

Total population.

Domain: Population. Units: count.

Calculation: `count`. MOE: Root-sum-of-squares for multiple components, retaining the largest MOE among zero-estimate components; source MOE for one component.

Rounding (estimate and MOE): integer.

- tract: 2021–2023.
- zcta: 2021–2023.

### Year-specific calculation rules

- 2021–2023: `B01001_001`.
  2021 source files: [B01001](https://www2.census.gov/programs-surveys/acs/summary_file/2021/table-based-SF/data/5YRData/acsdt5y2021-b01001.dat).
  2022 source files: [B01001](https://www2.census.gov/programs-surveys/acs/summary_file/2022/table-based-SF/data/5YRData/acsdt5y2022-b01001.dat).
  2023 source files: [B01001](https://www2.census.gov/programs-surveys/acs/summary_file/2023/table-based-SF/data/5YRData/acsdt5y2023-b01001.dat).

## `n_household_lt18` — Number of Households With Children

Number of households with children or adolescents < 18 years of age

Domain: Population. Units: count.

Calculation: `count`. MOE: Root-sum-of-squares for multiple components, retaining the largest MOE among zero-estimate components; source MOE for one component.

Rounding (estimate and MOE): integer.

- tract: 2021–2023.
- zcta: 2021–2023.

### Year-specific calculation rules

- 2021–2023: `B11005_002`.
  2021 source files: [B11005](https://www2.census.gov/programs-surveys/acs/summary_file/2021/table-based-SF/data/5YRData/acsdt5y2021-b11005.dat).
  2022 source files: [B11005](https://www2.census.gov/programs-surveys/acs/summary_file/2022/table-based-SF/data/5YRData/acsdt5y2022-b11005.dat).
  2023 source files: [B11005](https://www2.census.gov/programs-surveys/acs/summary_file/2023/table-based-SF/data/5YRData/acsdt5y2023-b11005.dat).

## `n_household` — Number of Households

Total households.

Domain: Population. Units: count.

Calculation: `count`. MOE: Root-sum-of-squares for multiple components, retaining the largest MOE among zero-estimate components; source MOE for one component.

Rounding (estimate and MOE): integer.

- tract: 2021–2023.
- zcta: 2021–2023.

### Year-specific calculation rules

- 2021–2023: `B11005_001`.
  2021 source files: [B11005](https://www2.census.gov/programs-surveys/acs/summary_file/2021/table-based-SF/data/5YRData/acsdt5y2021-b11005.dat).
  2022 source files: [B11005](https://www2.census.gov/programs-surveys/acs/summary_file/2022/table-based-SF/data/5YRData/acsdt5y2022-b11005.dat).
  2023 source files: [B11005](https://www2.census.gov/programs-surveys/acs/summary_file/2023/table-based-SF/data/5YRData/acsdt5y2023-b11005.dat).

## `fraction_insured` — Fraction of People Insured

People with health insurance divided by the civilian noninstitutionalized population.

Domain: Socioeconomic. Units: fraction.

Calculation: `proportion`. MOE: ACS subset-proportion formula; ratio fallback for nonpositive radicands; component MOEs combined by root-sum-of-squares with the largest MOE retained among zero-estimate components.

Rounding (estimate and MOE): decimal, 3 digits.

- tract: 2021–2023.
- zcta: 2021–2023.

### Year-specific calculation rules

- 2021–2023: `B27001_004` + `B27001_007` + `B27001_010` + `B27001_013` + `B27001_016` + `B27001_019` + `B27001_022` + `B27001_025` + `B27001_028` + `B27001_032` + `B27001_035` + `B27001_038` + `B27001_041` + `B27001_044` + `B27001_047` + `B27001_050` + `B27001_053` + `B27001_056` divided by `B27001_001`.
  2021 source files: [B27001](https://www2.census.gov/programs-surveys/acs/summary_file/2021/table-based-SF/data/5YRData/acsdt5y2021-b27001.dat).
  2022 source files: [B27001](https://www2.census.gov/programs-surveys/acs/summary_file/2022/table-based-SF/data/5YRData/acsdt5y2022-b27001.dat).
  2023 source files: [B27001](https://www2.census.gov/programs-surveys/acs/summary_file/2023/table-based-SF/data/5YRData/acsdt5y2023-b27001.dat).

## `fraction_snap` — Fraction of Households Receiving Assisted Income

Fraction of households receiving public assistance income or food stamps/SNAP in the past 12 months

Domain: Socioeconomic. Units: fraction.

Calculation: `proportion`. MOE: ACS subset-proportion formula; ratio fallback for nonpositive radicands; component MOEs combined by root-sum-of-squares with the largest MOE retained among zero-estimate components.

Rounding (estimate and MOE): decimal, 3 digits.

- tract: 2021–2023.
- zcta: 2021–2023.

### Year-specific calculation rules

- 2021–2023: `B19058_002` divided by `B19058_001`.
  2021 source files: [B19058](https://www2.census.gov/programs-surveys/acs/summary_file/2021/table-based-SF/data/5YRData/acsdt5y2021-b19058.dat).
  2022 source files: [B19058](https://www2.census.gov/programs-surveys/acs/summary_file/2022/table-based-SF/data/5YRData/acsdt5y2022-b19058.dat).
  2023 source files: [B19058](https://www2.census.gov/programs-surveys/acs/summary_file/2023/table-based-SF/data/5YRData/acsdt5y2023-b19058.dat).

## `fraction_fam_nospouse` — Fraction of family households with a single householder

Family households with a male or female householder and no spouse present divided by all family households.

Domain: Population. Units: fraction.

Calculation: `proportion`. MOE: ACS subset-proportion formula; ratio fallback for nonpositive radicands; component MOEs combined by root-sum-of-squares with the largest MOE retained among zero-estimate components.

Rounding (estimate and MOE): decimal, 3 digits.

- tract: 2021–2023.
- zcta: 2021–2023.

### Year-specific calculation rules

- 2021–2023: `B11001_004` divided by `B11001_002`.
  2021 source files: [B11001](https://www2.census.gov/programs-surveys/acs/summary_file/2021/table-based-SF/data/5YRData/acsdt5y2021-b11001.dat).
  2022 source files: [B11001](https://www2.census.gov/programs-surveys/acs/summary_file/2022/table-based-SF/data/5YRData/acsdt5y2022-b11001.dat).
  2023 source files: [B11001](https://www2.census.gov/programs-surveys/acs/summary_file/2023/table-based-SF/data/5YRData/acsdt5y2023-b11001.dat).

## `fraction_employment` — Fraction of People Employed

Fraction of people employed in civilian labor force

Domain: Socioeconomic. Units: fraction.

Calculation: `proportion`. MOE: ACS subset-proportion formula; ratio fallback for nonpositive radicands; component MOEs combined by root-sum-of-squares with the largest MOE retained among zero-estimate components.

Rounding (estimate and MOE): decimal, 3 digits.

- tract: 2021–2023.
- zcta: 2021–2023.

### Year-specific calculation rules

- 2021–2023: `B23025_004` divided by `B23025_003`.
  2021 source files: [B23025](https://www2.census.gov/programs-surveys/acs/summary_file/2021/table-based-SF/data/5YRData/acsdt5y2021-b23025.dat).
  2022 source files: [B23025](https://www2.census.gov/programs-surveys/acs/summary_file/2022/table-based-SF/data/5YRData/acsdt5y2022-b23025.dat).
  2023 source files: [B23025](https://www2.census.gov/programs-surveys/acs/summary_file/2023/table-based-SF/data/5YRData/acsdt5y2023-b23025.dat).

## `n_housing_units` — Number of Housing Units

Housing units are any separate living quarters (e.g., house, apartment, mobile home) either occupied or vacant

Domain: Housing. Units: count.

Calculation: `count`. MOE: Root-sum-of-squares for multiple components, retaining the largest MOE among zero-estimate components; source MOE for one component.

Rounding (estimate and MOE): integer.

- tract: 2021–2023.
- zcta: 2021–2023.

### Year-specific calculation rules

- 2021–2023: `B25001_001`.
  2021 source files: [B25001](https://www2.census.gov/programs-surveys/acs/summary_file/2021/table-based-SF/data/5YRData/acsdt5y2021-b25001.dat).
  2022 source files: [B25001](https://www2.census.gov/programs-surveys/acs/summary_file/2022/table-based-SF/data/5YRData/acsdt5y2022-b25001.dat).
  2023 source files: [B25001](https://www2.census.gov/programs-surveys/acs/summary_file/2023/table-based-SF/data/5YRData/acsdt5y2023-b25001.dat).

## `median_home_value` — Median Value of Owner-Occupied Housing Units

Median value of owner-occupied housing units, in nominal US dollars.

Domain: Housing. Units: USD.

Calculation: `estimate`. MOE: Source MOE.

Rounding (estimate and MOE): significant, 3 digits.

- tract: 2021–2023.
- zcta: 2021–2023.

### Year-specific calculation rules

- 2021–2023: `B25077_001`.
  2021 source files: [B25077](https://www2.census.gov/programs-surveys/acs/summary_file/2021/table-based-SF/data/5YRData/acsdt5y2021-b25077.dat).
  2022 source files: [B25077](https://www2.census.gov/programs-surveys/acs/summary_file/2022/table-based-SF/data/5YRData/acsdt5y2022-b25077.dat).
  2023 source files: [B25077](https://www2.census.gov/programs-surveys/acs/summary_file/2023/table-based-SF/data/5YRData/acsdt5y2023-b25077.dat).

## `median_home_value_2010adj` — Median Value of Owner-Occupied Housing Units (in 2010 USD)

Median value of owner-occupied housing units adjusted to 2010 US dollars using R-CPI-U-RS.

Domain: Housing. Units: USD_2010.

Calculation: `inflation_adjusted`. MOE: Source MOE scaled by CPI ratio.

Rounding (estimate and MOE): significant, 3 digits.

- tract: 2021–2023.
- zcta: 2021–2023.

### Year-specific calculation rules

- 2021–2023: `median_home_value` × CPI(2010) / CPI(year).
  [Source](median_home_value).

## `fraction_housing_renters` — Fraction of Housing Units Occupied by Renters

fraction denominator is number of *occupied* housing units

Domain: Housing. Units: fraction.

Calculation: `proportion`. MOE: ACS subset-proportion formula; ratio fallback for nonpositive radicands; component MOEs combined by root-sum-of-squares with the largest MOE retained among zero-estimate components.

Rounding (estimate and MOE): decimal, 3 digits.

- tract: 2021–2023.
- zcta: 2021–2023.

### Year-specific calculation rules

- 2021–2023: `B25003_003` divided by `B25003_001`.
  2021 source files: [B25003](https://www2.census.gov/programs-surveys/acs/summary_file/2021/table-based-SF/data/5YRData/acsdt5y2021-b25003.dat).
  2022 source files: [B25003](https://www2.census.gov/programs-surveys/acs/summary_file/2022/table-based-SF/data/5YRData/acsdt5y2022-b25003.dat).
  2023 source files: [B25003](https://www2.census.gov/programs-surveys/acs/summary_file/2023/table-based-SF/data/5YRData/acsdt5y2023-b25003.dat).

## `median_rent_to_income_percentage` — Median Rent to Income Percentage

The median of the percentage of rent to income among all renter-occupied housing units

Domain: Housing. Units: percent.

Calculation: `estimate`. MOE: Source MOE.

Rounding (estimate and MOE): significant, 3 digits.

- tract: 2021–2023.
- zcta: 2021–2023.

### Year-specific calculation rules

- 2021–2023: `B25071_001`.
  2021 source files: [B25071](https://www2.census.gov/programs-surveys/acs/summary_file/2021/table-based-SF/data/5YRData/acsdt5y2021-b25071.dat).
  2022 source files: [B25071](https://www2.census.gov/programs-surveys/acs/summary_file/2022/table-based-SF/data/5YRData/acsdt5y2022-b25071.dat).
  2023 source files: [B25071](https://www2.census.gov/programs-surveys/acs/summary_file/2023/table-based-SF/data/5YRData/acsdt5y2023-b25071.dat).

## `fraction_high_rent` — Fraction of Housing Units Paying at least 30% of Income on Rent

Renter-occupied housing units paying at least 30% of household income in gross rent divided by all renter-occupied housing units in B25070.

Domain: Housing. Units: fraction.

Calculation: `proportion`. MOE: ACS subset-proportion formula; ratio fallback for nonpositive radicands; component MOEs combined by root-sum-of-squares with the largest MOE retained among zero-estimate components.

Rounding (estimate and MOE): decimal, 3 digits.

- tract: 2021–2023.
- zcta: 2021–2023.

### Year-specific calculation rules

- 2021–2023: `B25070_007` + `B25070_008` + `B25070_009` + `B25070_010` divided by `B25070_001`.
  2021 source files: [B25070](https://www2.census.gov/programs-surveys/acs/summary_file/2021/table-based-SF/data/5YRData/acsdt5y2021-b25070.dat).
  2022 source files: [B25070](https://www2.census.gov/programs-surveys/acs/summary_file/2022/table-based-SF/data/5YRData/acsdt5y2022-b25070.dat).
  2023 source files: [B25070](https://www2.census.gov/programs-surveys/acs/summary_file/2023/table-based-SF/data/5YRData/acsdt5y2023-b25070.dat).

### Notes

- The denominator retains the not-computed rent-to-income category, matching the existing calculation.

## `fraction_conditions` — Fraction of Housing Units with Substandard Housing Conditions

substandard housing: incomplete plumbing or kitchens, overcrowding, 30% or more of household income spent on rent or owner costs

Domain: Housing. Units: fraction.

Calculation: `proportion`. MOE: ACS subset-proportion formula; ratio fallback for nonpositive radicands; component MOEs combined by root-sum-of-squares with the largest MOE retained among zero-estimate components.

Rounding (estimate and MOE): decimal, 3 digits.

- tract: 2021–2023.
- zcta: 2021–2023.

### Year-specific calculation rules

- 2021–2023: `B25123_003` + `B25123_004` + `B25123_005` + `B25123_006` + `B25123_009` + `B25123_010` + `B25123_011` + `B25123_012` divided by `B25123_001`.
  2021 source files: [B25123](https://www2.census.gov/programs-surveys/acs/summary_file/2021/table-based-SF/data/5YRData/acsdt5y2021-b25123.dat).
  2022 source files: [B25123](https://www2.census.gov/programs-surveys/acs/summary_file/2022/table-based-SF/data/5YRData/acsdt5y2022-b25123.dat).
  2023 source files: [B25123](https://www2.census.gov/programs-surveys/acs/summary_file/2023/table-based-SF/data/5YRData/acsdt5y2023-b25123.dat).

## `fraction_builtbf1970` — Fraction of Housing Units Built Before 1970

Housing units built before 1970 divided by all housing units.

Domain: Housing. Units: fraction.

Calculation: `proportion`. MOE: ACS subset-proportion formula; ratio fallback for nonpositive radicands; component MOEs combined by root-sum-of-squares with the largest MOE retained among zero-estimate components.

Rounding (estimate and MOE): decimal, 3 digits.

- tract: 2021–2023.
- zcta: 2021–2023.

### Year-specific calculation rules

- 2021–2023: `B25034_008` + `B25034_009` + `B25034_010` + `B25034_011` divided by `B25034_001`.
  A new construction-period category shifts the pre-1970 bins by one position.
  [Source](https://api.census.gov/data/2015/acs/acs5/groups/B25034.html).
  2021 source files: [B25034](https://www2.census.gov/programs-surveys/acs/summary_file/2021/table-based-SF/data/5YRData/acsdt5y2021-b25034.dat).
  2022 source files: [B25034](https://www2.census.gov/programs-surveys/acs/summary_file/2022/table-based-SF/data/5YRData/acsdt5y2022-b25034.dat).
  2023 source files: [B25034](https://www2.census.gov/programs-surveys/acs/summary_file/2023/table-based-SF/data/5YRData/acsdt5y2023-b25034.dat).

## `fraction_vacant` — Fraction of Housing Units that are Vacant

Vacant housing units divided by all housing units.

Domain: Housing. Units: fraction.

Calculation: `proportion`. MOE: ACS subset-proportion formula; ratio fallback for nonpositive radicands; component MOEs combined by root-sum-of-squares with the largest MOE retained among zero-estimate components.

Rounding (estimate and MOE): decimal, 3 digits.

- tract: 2021–2023.
- zcta: 2021–2023.

### Year-specific calculation rules

- 2021–2023: `B25002_003` divided by `B25002_001`.
  2021 source files: [B25002](https://www2.census.gov/programs-surveys/acs/summary_file/2021/table-based-SF/data/5YRData/acsdt5y2021-b25002.dat).
  2022 source files: [B25002](https://www2.census.gov/programs-surveys/acs/summary_file/2022/table-based-SF/data/5YRData/acsdt5y2022-b25002.dat).
  2023 source files: [B25002](https://www2.census.gov/programs-surveys/acs/summary_file/2023/table-based-SF/data/5YRData/acsdt5y2023-b25002.dat).

## `fraction_nhl` — Fraction of People Not Hispanic/Latino

Fraction of People Not Hispanic/Latino divided by the total population.

Domain: Population. Units: fraction.

Calculation: `proportion`. MOE: ACS subset-proportion formula; ratio fallback for nonpositive radicands; component MOEs combined by root-sum-of-squares with the largest MOE retained among zero-estimate components.

Rounding (estimate and MOE): decimal, 3 digits.

- tract: 2021–2023.
- zcta: 2021–2023.

### Year-specific calculation rules

- 2021–2023: `B03002_002` divided by `B03002_001`.
  2021 source files: [B03002](https://www2.census.gov/programs-surveys/acs/summary_file/2021/table-based-SF/data/5YRData/acsdt5y2021-b03002.dat).
  2022 source files: [B03002](https://www2.census.gov/programs-surveys/acs/summary_file/2022/table-based-SF/data/5YRData/acsdt5y2022-b03002.dat).
  2023 source files: [B03002](https://www2.census.gov/programs-surveys/acs/summary_file/2023/table-based-SF/data/5YRData/acsdt5y2023-b03002.dat).

## `fraction_nhl_w` — Fraction of People White and Not Hispanic/Latino

Fraction of People White and Not Hispanic/Latino divided by the total population.

Domain: Population. Units: fraction.

Calculation: `proportion`. MOE: ACS subset-proportion formula; ratio fallback for nonpositive radicands; component MOEs combined by root-sum-of-squares with the largest MOE retained among zero-estimate components.

Rounding (estimate and MOE): decimal, 3 digits.

- tract: 2021–2023.
- zcta: 2021–2023.

### Year-specific calculation rules

- 2021–2023: `B03002_003` divided by `B03002_001`.
  2021 source files: [B03002](https://www2.census.gov/programs-surveys/acs/summary_file/2021/table-based-SF/data/5YRData/acsdt5y2021-b03002.dat).
  2022 source files: [B03002](https://www2.census.gov/programs-surveys/acs/summary_file/2022/table-based-SF/data/5YRData/acsdt5y2022-b03002.dat).
  2023 source files: [B03002](https://www2.census.gov/programs-surveys/acs/summary_file/2023/table-based-SF/data/5YRData/acsdt5y2023-b03002.dat).

## `fraction_nhl_b` — Fraction of People Black and Not Hispanic/Latino

Fraction of People Black and Not Hispanic/Latino divided by the total population.

Domain: Population. Units: fraction.

Calculation: `proportion`. MOE: ACS subset-proportion formula; ratio fallback for nonpositive radicands; component MOEs combined by root-sum-of-squares with the largest MOE retained among zero-estimate components.

Rounding (estimate and MOE): decimal, 3 digits.

- tract: 2021–2023.
- zcta: 2021–2023.

### Year-specific calculation rules

- 2021–2023: `B03002_004` divided by `B03002_001`.
  2021 source files: [B03002](https://www2.census.gov/programs-surveys/acs/summary_file/2021/table-based-SF/data/5YRData/acsdt5y2021-b03002.dat).
  2022 source files: [B03002](https://www2.census.gov/programs-surveys/acs/summary_file/2022/table-based-SF/data/5YRData/acsdt5y2022-b03002.dat).
  2023 source files: [B03002](https://www2.census.gov/programs-surveys/acs/summary_file/2023/table-based-SF/data/5YRData/acsdt5y2023-b03002.dat).

## `fraction_nhl_o` — Fraction of People Not Black, Not White, and Not Hispanic/Latino

Fraction of People Not Black, Not White, and Not Hispanic/Latino divided by the total population.

Domain: Population. Units: fraction.

Calculation: `proportion`. MOE: ACS subset-proportion formula; ratio fallback for nonpositive radicands; component MOEs combined by root-sum-of-squares with the largest MOE retained among zero-estimate components.

Rounding (estimate and MOE): decimal, 3 digits.

- tract: 2021–2023.
- zcta: 2021–2023.

### Year-specific calculation rules

- 2021–2023: `B03002_005` + `B03002_006` + `B03002_007` + `B03002_008` + `B03002_009` divided by `B03002_001`.
  2021 source files: [B03002](https://www2.census.gov/programs-surveys/acs/summary_file/2021/table-based-SF/data/5YRData/acsdt5y2021-b03002.dat).
  2022 source files: [B03002](https://www2.census.gov/programs-surveys/acs/summary_file/2022/table-based-SF/data/5YRData/acsdt5y2022-b03002.dat).
  2023 source files: [B03002](https://www2.census.gov/programs-surveys/acs/summary_file/2023/table-based-SF/data/5YRData/acsdt5y2023-b03002.dat).

### Notes

- The numerator includes the other race categories and people reporting two or more races, matching the existing calculation.

## `fraction_hl` — Fraction of People Hispanic/Latino

Fraction of People Hispanic/Latino divided by the total population.

Domain: Population. Units: fraction.

Calculation: `proportion`. MOE: ACS subset-proportion formula; ratio fallback for nonpositive radicands; component MOEs combined by root-sum-of-squares with the largest MOE retained among zero-estimate components.

Rounding (estimate and MOE): decimal, 3 digits.

- tract: 2021–2023.
- zcta: 2021–2023.

### Year-specific calculation rules

- 2021–2023: `B03002_012` divided by `B03002_001`.
  2021 source files: [B03002](https://www2.census.gov/programs-surveys/acs/summary_file/2021/table-based-SF/data/5YRData/acsdt5y2021-b03002.dat).
  2022 source files: [B03002](https://www2.census.gov/programs-surveys/acs/summary_file/2022/table-based-SF/data/5YRData/acsdt5y2022-b03002.dat).
  2023 source files: [B03002](https://www2.census.gov/programs-surveys/acs/summary_file/2023/table-based-SF/data/5YRData/acsdt5y2023-b03002.dat).

## `fraction_hl_w` — Fraction of People White and Hispanic/Latino

Fraction of People White and Hispanic/Latino divided by the total population.

Domain: Population. Units: fraction.

Calculation: `proportion`. MOE: ACS subset-proportion formula; ratio fallback for nonpositive radicands; component MOEs combined by root-sum-of-squares with the largest MOE retained among zero-estimate components.

Rounding (estimate and MOE): decimal, 3 digits.

- tract: 2021–2023.
- zcta: 2021–2023.

### Year-specific calculation rules

- 2021–2023: `B03002_013` divided by `B03002_001`.
  2021 source files: [B03002](https://www2.census.gov/programs-surveys/acs/summary_file/2021/table-based-SF/data/5YRData/acsdt5y2021-b03002.dat).
  2022 source files: [B03002](https://www2.census.gov/programs-surveys/acs/summary_file/2022/table-based-SF/data/5YRData/acsdt5y2022-b03002.dat).
  2023 source files: [B03002](https://www2.census.gov/programs-surveys/acs/summary_file/2023/table-based-SF/data/5YRData/acsdt5y2023-b03002.dat).

## `fraction_hl_b` — Fraction of People Black and Hispanic/Latino

Fraction of People Black and Hispanic/Latino divided by the total population.

Domain: Population. Units: fraction.

Calculation: `proportion`. MOE: ACS subset-proportion formula; ratio fallback for nonpositive radicands; component MOEs combined by root-sum-of-squares with the largest MOE retained among zero-estimate components.

Rounding (estimate and MOE): decimal, 3 digits.

- tract: 2021–2023.
- zcta: 2021–2023.

### Year-specific calculation rules

- 2021–2023: `B03002_014` divided by `B03002_001`.
  2021 source files: [B03002](https://www2.census.gov/programs-surveys/acs/summary_file/2021/table-based-SF/data/5YRData/acsdt5y2021-b03002.dat).
  2022 source files: [B03002](https://www2.census.gov/programs-surveys/acs/summary_file/2022/table-based-SF/data/5YRData/acsdt5y2022-b03002.dat).
  2023 source files: [B03002](https://www2.census.gov/programs-surveys/acs/summary_file/2023/table-based-SF/data/5YRData/acsdt5y2023-b03002.dat).

## `fraction_hl_o` — Fraction of People Not Black, Not White, and Hispanic/Latino

Fraction of People Not Black, Not White, and Hispanic/Latino divided by the total population.

Domain: Population. Units: fraction.

Calculation: `proportion`. MOE: ACS subset-proportion formula; ratio fallback for nonpositive radicands; component MOEs combined by root-sum-of-squares with the largest MOE retained among zero-estimate components.

Rounding (estimate and MOE): decimal, 3 digits.

- tract: 2021–2023.
- zcta: 2021–2023.

### Year-specific calculation rules

- 2021–2023: `B03002_015` + `B03002_016` + `B03002_017` + `B03002_018` + `B03002_019` divided by `B03002_001`.
  2021 source files: [B03002](https://www2.census.gov/programs-surveys/acs/summary_file/2021/table-based-SF/data/5YRData/acsdt5y2021-b03002.dat).
  2022 source files: [B03002](https://www2.census.gov/programs-surveys/acs/summary_file/2022/table-based-SF/data/5YRData/acsdt5y2022-b03002.dat).
  2023 source files: [B03002](https://www2.census.gov/programs-surveys/acs/summary_file/2023/table-based-SF/data/5YRData/acsdt5y2023-b03002.dat).

### Notes

- The numerator includes the other race categories and people reporting two or more races, matching the existing calculation.

## `fraction_lesh` — Fraction of Households Speaking Limited English

Limited English speaking households divided by all households in C16002.

Domain: Population. Units: fraction.

Calculation: `proportion`. MOE: ACS subset-proportion formula; ratio fallback for nonpositive radicands; component MOEs combined by root-sum-of-squares with the largest MOE retained among zero-estimate components.

Rounding (estimate and MOE): decimal, 3 digits.

- tract: 2021–2023.
- zcta: 2021–2023.

### Year-specific calculation rules

- 2021–2023: `C16002_004` + `C16002_007` + `C16002_010` + `C16002_013` divided by `C16002_001`.
  2021 source files: [C16002](https://www2.census.gov/programs-surveys/acs/summary_file/2021/table-based-SF/data/5YRData/acsdt5y2021-c16002.dat).
  2022 source files: [C16002](https://www2.census.gov/programs-surveys/acs/summary_file/2022/table-based-SF/data/5YRData/acsdt5y2022-c16002.dat).
  2023 source files: [C16002](https://www2.census.gov/programs-surveys/acs/summary_file/2023/table-based-SF/data/5YRData/acsdt5y2023-c16002.dat).

## `median_income` — Median Household Income

Median household income in nominal US dollars.

Domain: Socioeconomic. Units: USD.

Calculation: `estimate`. MOE: Source MOE.

Rounding (estimate and MOE): significant, 3 digits.

- tract: 2021–2023.
- zcta: 2021–2023.

### Year-specific calculation rules

- 2021–2023: `B19013_001`.
  2021 source files: [B19013](https://www2.census.gov/programs-surveys/acs/summary_file/2021/table-based-SF/data/5YRData/acsdt5y2021-b19013.dat).
  2022 source files: [B19013](https://www2.census.gov/programs-surveys/acs/summary_file/2022/table-based-SF/data/5YRData/acsdt5y2022-b19013.dat).
  2023 source files: [B19013](https://www2.census.gov/programs-surveys/acs/summary_file/2023/table-based-SF/data/5YRData/acsdt5y2023-b19013.dat).

## `median_income_2010adj` — Median Household Income (in 2010 USD)

Median household income adjusted to 2010 US dollars using R-CPI-U-RS.

Domain: Socioeconomic. Units: USD_2010.

Calculation: `inflation_adjusted`. MOE: Source MOE scaled by CPI ratio.

Rounding (estimate and MOE): significant, 3 digits.

- tract: 2021–2023.
- zcta: 2021–2023.

### Year-specific calculation rules

- 2021–2023: `median_income` × CPI(2010) / CPI(year).
  [Source](median_income).

## `fraction_hs` — Fraction of Adults with At Least High School Education

People age 25 years and older with at least a high school diploma or equivalent divided by all people age 25 years and older.

Domain: Socioeconomic. Units: fraction.

Calculation: `proportion`. MOE: ACS subset-proportion formula; ratio fallback for nonpositive radicands; component MOEs combined by root-sum-of-squares with the largest MOE retained among zero-estimate components.

Rounding (estimate and MOE): decimal, 3 digits.

- tract: 2021–2023.
- zcta: 2021–2023.

### Year-specific calculation rules

- 2021–2023: `B15003_017` + `B15003_018` + `B15003_019` + `B15003_020` + `B15003_021` + `B15003_022` + `B15003_023` + `B15003_024` + `B15003_025` divided by `B15003_001`.
  2021 source files: [B15003](https://www2.census.gov/programs-surveys/acs/summary_file/2021/table-based-SF/data/5YRData/acsdt5y2021-b15003.dat).
  2022 source files: [B15003](https://www2.census.gov/programs-surveys/acs/summary_file/2022/table-based-SF/data/5YRData/acsdt5y2022-b15003.dat).
  2023 source files: [B15003](https://www2.census.gov/programs-surveys/acs/summary_file/2023/table-based-SF/data/5YRData/acsdt5y2023-b15003.dat).
