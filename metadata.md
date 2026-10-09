# Harmonized Historical ACS Measures

<!-- Generated from measures.yml. Edit the catalog, then run Rscript build_metadata.R. -->

## Dataset conventions

- Year is the ending year of the five-year ACS estimate, not a single-year estimate.
- Availability ranges describe the implemented pipeline; they do not establish that a question or table was absent in earlier ACS years.
- Suppressed or missing ACS values remain missing. A zero denominator yields a missing fraction and MOE.
- Inflation-adjusted estimates and MOEs use the same CPI ratio. Revised CPI inputs can change earlier estimates in later releases.

All `_moe` fields are 95% confidence margins of error in the units of their accompanying estimates.

## Geography: tract

Coverage: 2010–2023. Key: `census_tract_id`, `census_tract_vintage`, `year`.

### Boundary vintages

- 2010–2019: 2010 boundaries.
- 2020–2023: 2020 boundaries.

- 2022–2023: Connecticut planning-region tract IDs are mapped one-to-one back to 2020 tract IDs using data-raw/ct_tract_crosswalk_2022_2020.csv. Estimates are not interpolated; water-only tracts outside the cartographic scaffold are excluded. [Source](data-raw/README.md).

## Geography: zcta

Coverage: 2013–2023. Key: `zcta`, `zcta_vintage`, `year`.

### Boundary vintages

- 2013–2020: 2010 boundaries.
- 2021–2023: 2020 boundaries.

- 2013–2023: The existing ZCTA pipeline starts in 2013; earlier years are not included.
- 2020–2020: The 2020 ACS still uses 2010 ZCTAs; 2020 ZCTAs begin with the 2021 ACS. [Source](https://www.census.gov/programs-surveys/acs/geography-acs/geography-boundaries-by-year/2020.html).

## `fraction_poverty` — Fraction of People in Poverty

Fraction of people with income below the poverty level in the past 12 months, among people for whom poverty status is determined

Domain: Socioeconomic. Units: fraction.

Calculation: `proportion`. MOE: moe_prop with moe_sum for multiple numerator components.

Rounding (estimate and MOE): decimal, 3 digits.

- tract: 2010–2023.
- zcta: 2013–2023.

### Year-specific calculation rules

- 2010–2023: `B17001_002` divided by `B17001_001`.
  ACS source: [B17001](https://api.census.gov/data/2023/acs/acs5/groups/B17001.html).

## `n_children_lt18` — Number of Children

Number of children and adolescents < 18 years of age

Domain: Population. Units: count.

Calculation: `count`. MOE: moe_sum for multiple components; source MOE for one component.

Rounding (estimate and MOE): integer.

- tract: 2010–2023.
- zcta: 2013–2023.

### Year-specific calculation rules

- 2010–2023: `B01001_003` + `B01001_004` + `B01001_005` + `B01001_006` + `B01001_027` + `B01001_028` + `B01001_029` + `B01001_030`.
  ACS source: [B01001](https://api.census.gov/data/2023/acs/acs5/groups/B01001.html).

## `n_pop` — Number of Total People

Total population.

Domain: Population. Units: count.

Calculation: `count`. MOE: moe_sum for multiple components; source MOE for one component.

Rounding (estimate and MOE): integer.

- tract: 2010–2023.
- zcta: 2013–2023.

### Year-specific calculation rules

- 2010–2023: `B01001_001`.
  ACS source: [B01001](https://api.census.gov/data/2023/acs/acs5/groups/B01001.html).

## `n_household_lt18` — Number of Households With Children

Number of households with children or adolescents < 18 years of age

Domain: Population. Units: count.

Calculation: `count`. MOE: moe_sum for multiple components; source MOE for one component.

Rounding (estimate and MOE): integer.

- tract: 2010–2023.
- zcta: 2013–2023.

### Year-specific calculation rules

- 2010–2023: `B11005_002`.
  ACS source: [B11005](https://api.census.gov/data/2023/acs/acs5/groups/B11005.html).

## `n_household` — Number of Households

Total households.

Domain: Population. Units: count.

Calculation: `count`. MOE: moe_sum for multiple components; source MOE for one component.

Rounding (estimate and MOE): integer.

- tract: 2010–2023.
- zcta: 2013–2023.

### Year-specific calculation rules

- 2010–2023: `B11005_001`.
  ACS source: [B11005](https://api.census.gov/data/2023/acs/acs5/groups/B11005.html).

## `fraction_insured` — Fraction of People Insured

People with health insurance divided by the civilian noninstitutionalized population.

Domain: Socioeconomic. Units: fraction.

Calculation: `proportion`. MOE: moe_prop with moe_sum for multiple numerator components.

Rounding (estimate and MOE): decimal, 3 digits.

- tract: 2012–2023.
- zcta: 2013–2023.

### Year-specific calculation rules

- 2012–2023: `B27001_004` + `B27001_007` + `B27001_010` + `B27001_013` + `B27001_016` + `B27001_019` + `B27001_022` + `B27001_025` + `B27001_028` + `B27001_032` + `B27001_035` + `B27001_038` + `B27001_041` + `B27001_044` + `B27001_047` + `B27001_050` + `B27001_053` + `B27001_056` divided by `B27001_001`.
  ACS source: [B27001](https://api.census.gov/data/2023/acs/acs5/groups/B27001.html).

### Notes

- The existing tract pipeline begins this measure in 2012; earlier years remain missing. This is an implementation coverage boundary.

## `fraction_snap` — Fraction of Households Receiving Assisted Income

Fraction of households receiving public assistance income or food stamps/SNAP in the past 12 months

Domain: Socioeconomic. Units: fraction.

Calculation: `proportion`. MOE: moe_prop with moe_sum for multiple numerator components.

Rounding (estimate and MOE): decimal, 3 digits.

- tract: 2010–2023.
- zcta: 2013–2023.

### Year-specific calculation rules

- 2010–2023: `B19058_002` divided by `B19058_001`.
  ACS source: [B19058](https://api.census.gov/data/2023/acs/acs5/groups/B19058.html).

## `fraction_fam_nospouse` — Fraction of family households with a single householder

Family households with a male or female householder and no spouse present divided by all family households.

Domain: Population. Units: fraction.

Calculation: `proportion`. MOE: moe_prop with moe_sum for multiple numerator components.

Rounding (estimate and MOE): decimal, 3 digits.

- tract: 2010–2023.
- zcta: 2013–2023.

### Year-specific calculation rules

- 2010–2023: `B11001_004` divided by `B11001_002`.
  ACS source: [B11001](https://api.census.gov/data/2023/acs/acs5/groups/B11001.html).

## `fraction_employment` — Fraction of People Employed

Fraction of people employed in civilian labor force

Domain: Socioeconomic. Units: fraction.

Calculation: `proportion`. MOE: moe_prop with moe_sum for multiple numerator components.

Rounding (estimate and MOE): decimal, 3 digits.

- tract: 2011–2023.
- zcta: 2013–2023.

### Year-specific calculation rules

- 2011–2023: `B23025_004` divided by `B23025_003`.
  ACS source: [B23025](https://api.census.gov/data/2023/acs/acs5/groups/B23025.html).

### Notes

- The existing tract pipeline begins this measure in 2011; earlier years remain missing. This is an implementation coverage boundary.

## `n_housing_units` — Number of Housing Units

Housing units are any separate living quarters (e.g., house, apartment, mobile home) either occupied or vacant

Domain: Housing. Units: count.

Calculation: `count`. MOE: moe_sum for multiple components; source MOE for one component.

Rounding (estimate and MOE): integer.

- tract: 2010–2023.
- zcta: 2013–2023.

### Year-specific calculation rules

- 2010–2023: `B25001_001`.
  ACS source: [B25001](https://api.census.gov/data/2023/acs/acs5/groups/B25001.html).

## `median_home_value` — Median Value of Owner-Occupied Housing Units

Median value of owner-occupied housing units, in nominal US dollars.

Domain: Housing. Units: USD.

Calculation: `estimate`. MOE: moe_sum for multiple components; source MOE for one component.

Rounding (estimate and MOE): significant, 3 digits.

- tract: 2010–2023.
- zcta: 2013–2023.

### Year-specific calculation rules

- 2010–2023: `B25077_001`.
  ACS source: [B25077](https://api.census.gov/data/2023/acs/acs5/groups/B25077.html).

## `median_home_value_2010adj` — Median Value of Owner-Occupied Housing Units (in 2010 USD)

Median value of owner-occupied housing units adjusted to 2010 US dollars using R-CPI-U-RS.

Domain: Housing. Units: USD_2010.

Calculation: `inflation_adjusted`. MOE: source MOE scaled by CPI ratio.

Rounding (estimate and MOE): significant, 3 digits.

- tract: 2010–2023.
- zcta: 2013–2023.

### Year-specific calculation rules

- 2010–2023: `median_home_value` × CPI(2010) / CPI(year).
  [Source](median_home_value).

## `fraction_housing_renters` — Fraction of Housing Units Occupied by Renters

fraction denominator is number of *occupied* housing units

Domain: Housing. Units: fraction.

Calculation: `proportion`. MOE: moe_prop with moe_sum for multiple numerator components.

Rounding (estimate and MOE): decimal, 3 digits.

- tract: 2011–2023.
- zcta: 2013–2023.

### Year-specific calculation rules

- 2011–2023: `B25003_003` divided by `B25003_001`.
  ACS source: [B25003](https://api.census.gov/data/2023/acs/acs5/groups/B25003.html).

### Notes

- The existing tract pipeline begins this measure in 2011; earlier years remain missing. This is an implementation coverage boundary.

## `median_rent_to_income_percentage` — Median Rent to Income Percentage

The median of the percentage of rent to income among all renter-occupied housing units

Domain: Housing. Units: percent.

Calculation: `estimate`. MOE: moe_sum for multiple components; source MOE for one component.

Rounding (estimate and MOE): significant, 3 digits.

- tract: 2010–2023.
- zcta: 2013–2023.

### Year-specific calculation rules

- 2010–2023: `B25071_001`.
  ACS source: [B25071](https://api.census.gov/data/2023/acs/acs5/groups/B25071.html).

## `fraction_high_rent` — Fraction of Housing Units Paying at least 30% of Income on Rent

Renter-occupied housing units paying at least 30% of household income in gross rent divided by all renter-occupied housing units in B25070.

Domain: Housing. Units: fraction.

Calculation: `proportion`. MOE: moe_prop with moe_sum for multiple numerator components.

Rounding (estimate and MOE): decimal, 3 digits.

- tract: 2010–2023.
- zcta: 2013–2023.

### Year-specific calculation rules

- 2010–2023: `B25070_007` + `B25070_008` + `B25070_009` + `B25070_010` divided by `B25070_001`.
  ACS source: [B25070](https://api.census.gov/data/2023/acs/acs5/groups/B25070.html).

### Notes

- The denominator retains the not-computed rent-to-income category, matching the existing calculation.

## `fraction_conditions` — Fraction of Housing Units with Substandard Housing Conditions

substandard housing: incomplete plumbing or kitchens, overcrowding, 30% or more of household income spent on rent or owner costs

Domain: Housing. Units: fraction.

Calculation: `proportion`. MOE: moe_prop with moe_sum for multiple numerator components.

Rounding (estimate and MOE): decimal, 3 digits.

- tract: 2010–2023.
- zcta: 2013–2023.

### Year-specific calculation rules

- 2010–2023: `B25123_003` + `B25123_004` + `B25123_005` + `B25123_006` + `B25123_009` + `B25123_010` + `B25123_011` + `B25123_012` divided by `B25123_001`.
  ACS source: [B25123](https://api.census.gov/data/2023/acs/acs5/groups/B25123.html).

## `fraction_builtbf1970` — Fraction of Housing Units Built Before 1970

Housing units built before 1970 divided by all housing units.

Domain: Housing. Units: fraction.

Calculation: `proportion`. MOE: moe_prop with moe_sum for multiple numerator components.

Rounding (estimate and MOE): decimal, 3 digits.

- tract: 2010–2023.
- zcta: 2013–2023.

### Year-specific calculation rules

- 2010–2014: `B25034_007` + `B25034_008` + `B25034_009` + `B25034_010` divided by `B25034_001`.
  Before the 2015 construction-period category change.
  ACS source: [B25034](https://api.census.gov/data/2014/acs/acs5/groups/B25034.html).
- 2015–2023: `B25034_008` + `B25034_009` + `B25034_010` + `B25034_011` divided by `B25034_001`.
  A new construction-period category shifts the pre-1970 bins by one position.
  [Source](https://api.census.gov/data/2015/acs/acs5/groups/B25034.html).
  ACS source: [B25034](https://api.census.gov/data/2023/acs/acs5/groups/B25034.html).

## `fraction_vacant` — Fraction of Housing Units that are Vacant

Vacant housing units divided by all housing units.

Domain: Housing. Units: fraction.

Calculation: `proportion`. MOE: moe_prop with moe_sum for multiple numerator components.

Rounding (estimate and MOE): decimal, 3 digits.

- tract: 2010–2023.
- zcta: 2013–2023.

### Year-specific calculation rules

- 2010–2023: `B25002_003` divided by `B25002_001`.
  ACS source: [B25002](https://api.census.gov/data/2023/acs/acs5/groups/B25002.html).

## `fraction_nhl` — Fraction of People Not Hispanic/Latino

Fraction of People Not Hispanic/Latino divided by the total population.

Domain: Population. Units: fraction.

Calculation: `proportion`. MOE: moe_prop with moe_sum for multiple numerator components.

Rounding (estimate and MOE): decimal, 3 digits.

- tract: 2010–2023.
- zcta: 2013–2023.

### Year-specific calculation rules

- 2010–2023: `B03002_002` divided by `B03002_001`.
  ACS source: [B03002](https://api.census.gov/data/2023/acs/acs5/groups/B03002.html).

## `fraction_nhl_w` — Fraction of People White and Not Hispanic/Latino

Fraction of People White and Not Hispanic/Latino divided by the total population.

Domain: Population. Units: fraction.

Calculation: `proportion`. MOE: moe_prop with moe_sum for multiple numerator components.

Rounding (estimate and MOE): decimal, 3 digits.

- tract: 2010–2023.
- zcta: 2013–2023.

### Year-specific calculation rules

- 2010–2023: `B03002_003` divided by `B03002_001`.
  ACS source: [B03002](https://api.census.gov/data/2023/acs/acs5/groups/B03002.html).

## `fraction_nhl_b` — Fraction of People Black and Not Hispanic/Latino

Fraction of People Black and Not Hispanic/Latino divided by the total population.

Domain: Population. Units: fraction.

Calculation: `proportion`. MOE: moe_prop with moe_sum for multiple numerator components.

Rounding (estimate and MOE): decimal, 3 digits.

- tract: 2010–2023.
- zcta: 2013–2023.

### Year-specific calculation rules

- 2010–2023: `B03002_004` divided by `B03002_001`.
  ACS source: [B03002](https://api.census.gov/data/2023/acs/acs5/groups/B03002.html).

## `fraction_nhl_o` — Fraction of People Not Black, Not White, and Not Hispanic/Latino

Fraction of People Not Black, Not White, and Not Hispanic/Latino divided by the total population.

Domain: Population. Units: fraction.

Calculation: `proportion`. MOE: moe_prop with moe_sum for multiple numerator components.

Rounding (estimate and MOE): decimal, 3 digits.

- tract: 2010–2023.
- zcta: 2013–2023.

### Year-specific calculation rules

- 2010–2023: `B03002_005` + `B03002_006` + `B03002_007` + `B03002_008` + `B03002_009` divided by `B03002_001`.
  ACS source: [B03002](https://api.census.gov/data/2023/acs/acs5/groups/B03002.html).

### Notes

- The numerator includes the other race categories and people reporting two or more races, matching the existing calculation.

## `fraction_hl` — Fraction of People Hispanic/Latino

Fraction of People Hispanic/Latino divided by the total population.

Domain: Population. Units: fraction.

Calculation: `proportion`. MOE: moe_prop with moe_sum for multiple numerator components.

Rounding (estimate and MOE): decimal, 3 digits.

- tract: 2010–2023.
- zcta: 2013–2023.

### Year-specific calculation rules

- 2010–2023: `B03002_012` divided by `B03002_001`.
  ACS source: [B03002](https://api.census.gov/data/2023/acs/acs5/groups/B03002.html).

## `fraction_hl_w` — Fraction of People White and Hispanic/Latino

Fraction of People White and Hispanic/Latino divided by the total population.

Domain: Population. Units: fraction.

Calculation: `proportion`. MOE: moe_prop with moe_sum for multiple numerator components.

Rounding (estimate and MOE): decimal, 3 digits.

- tract: 2010–2023.
- zcta: 2013–2023.

### Year-specific calculation rules

- 2010–2023: `B03002_013` divided by `B03002_001`.
  ACS source: [B03002](https://api.census.gov/data/2023/acs/acs5/groups/B03002.html).

## `fraction_hl_b` — Fraction of People Black and Hispanic/Latino

Fraction of People Black and Hispanic/Latino divided by the total population.

Domain: Population. Units: fraction.

Calculation: `proportion`. MOE: moe_prop with moe_sum for multiple numerator components.

Rounding (estimate and MOE): decimal, 3 digits.

- tract: 2010–2023.
- zcta: 2013–2023.

### Year-specific calculation rules

- 2010–2023: `B03002_014` divided by `B03002_001`.
  ACS source: [B03002](https://api.census.gov/data/2023/acs/acs5/groups/B03002.html).

## `fraction_hl_o` — Fraction of People Not Black, Not White, and Hispanic/Latino

Fraction of People Not Black, Not White, and Hispanic/Latino divided by the total population.

Domain: Population. Units: fraction.

Calculation: `proportion`. MOE: moe_prop with moe_sum for multiple numerator components.

Rounding (estimate and MOE): decimal, 3 digits.

- tract: 2010–2023.
- zcta: 2013–2023.

### Year-specific calculation rules

- 2010–2023: `B03002_015` + `B03002_016` + `B03002_017` + `B03002_018` + `B03002_019` divided by `B03002_001`.
  ACS source: [B03002](https://api.census.gov/data/2023/acs/acs5/groups/B03002.html).

### Notes

- The numerator includes the other race categories and people reporting two or more races, matching the existing calculation.

## `fraction_lesh` — Fraction of Households Speaking Limited English

Limited English speaking households divided by all households in C16002.

Domain: Population. Units: fraction.

Calculation: `proportion`. MOE: moe_prop with moe_sum for multiple numerator components.

Rounding (estimate and MOE): decimal, 3 digits.

- tract: 2016–2023.
- zcta: 2016–2023.

### Year-specific calculation rules

- 2016–2023: `C16002_004` + `C16002_007` + `C16002_010` + `C16002_013` divided by `C16002_001`.
  ACS source: [C16002](https://api.census.gov/data/2023/acs/acs5/groups/C16002.html).

### Notes

- The existing tract pipeline begins this measure in 2016; earlier years remain missing. This is an implementation coverage boundary.

## `median_income` — Median Household Income

Median household income in nominal US dollars.

Domain: Socioeconomic. Units: USD.

Calculation: `estimate`. MOE: moe_sum for multiple components; source MOE for one component.

Rounding (estimate and MOE): significant, 3 digits.

- tract: 2010–2023.
- zcta: 2013–2023.

### Year-specific calculation rules

- 2010–2023: `B19013_001`.
  ACS source: [B19013](https://api.census.gov/data/2023/acs/acs5/groups/B19013.html).

## `median_income_2010adj` — Median Household Income (in 2010 USD)

Median household income adjusted to 2010 US dollars using R-CPI-U-RS.

Domain: Socioeconomic. Units: USD_2010.

Calculation: `inflation_adjusted`. MOE: source MOE scaled by CPI ratio.

Rounding (estimate and MOE): significant, 3 digits.

- tract: 2010–2023.
- zcta: 2013–2023.

### Year-specific calculation rules

- 2010–2023: `median_income` × CPI(2010) / CPI(year).
  [Source](median_income).

## `fraction_hs` — Fraction of Adults with At Least High School Education

People age 25 years and older with at least a high school diploma or equivalent divided by all people age 25 years and older.

Domain: Socioeconomic. Units: fraction.

Calculation: `proportion`. MOE: moe_prop with moe_sum for multiple numerator components.

Rounding (estimate and MOE): decimal, 3 digits.

- tract: 2012–2023.
- zcta: 2013–2023.

### Year-specific calculation rules

- 2012–2023: `B15003_017` + `B15003_018` + `B15003_019` + `B15003_020` + `B15003_021` + `B15003_022` + `B15003_023` + `B15003_024` + `B15003_025` divided by `B15003_001`.
  ACS source: [B15003](https://api.census.gov/data/2023/acs/acs5/groups/B15003.html).

### Notes

- The existing tract pipeline begins this measure in 2012; earlier years remain missing. This is an implementation coverage boundary.
