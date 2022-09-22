#### Metadata

|name        |value                                                                                  |
|:-----------|:--------------------------------------------------------------------------------------|
|name        |hh_acs_measures                                                                        |
|title       |Harmonized Historical American Community Survey Measures                               |
|description |2010 - 2020 measures derived from ACS variables for census tracts in the contiguous US |
|url         |https://github.com/geomarker-io/hh_acs_measures                                        |

#### Schema

Columns ending with `_moe` represent the margin of error accompanying another column and are not included in the schema table here.

|name                        |title                                                            |description                                                                                                                      |type    |
|:---------------------------|:----------------------------------------------------------------|:--------------------------------------------------------------------------------------------------------------------------------|:-------|
|census_tract_id             |Census Tract Identifier                                          |can refer to 2010 or 2020 vintage census tracts; unique only in combination with `census_tract_vintage`                          |string  |
|census_tract_vintage        |Census Tract Vintage                                             |The year of the decennial census that defines the tract (2010 for years 2010-2019 and 2020 for years 2020-2029)                  |string  |
|year                        |Year                                                             |The year of the 5-year ACS estimates (e.g., the 2019 ACS covers 2015 - 2019)                                                     |integer |
|fraction_poverty            |Fraction of Households in Poverty                                |Fraction of households with income below poverty level within the past 12 months                                                 |number  |
|n_children_lt18             |                                                                 |                                                                                                                                 |integer |
|n_pop                       |                                                                 |                                                                                                                                 |integer |
|n_household_lt18            |                                                                 |                                                                                                                                 |integer |
|n_household                 |                                                                 |                                                                                                                                 |integer |
|fraction_insured            |Fraction of People Insured                                       |Fraction of population with health insurance (available from 2012 onwards only)                                                  |number  |
|fraction_snap               |Fraction of Households Receiving SNAP                            |Fraction of households receiving food stamps/SNAP in the past 12 months                                                          |number  |
|fraction_fam_nospouse       |Fraction of family households with a single householder          |Single householder is male or female household, with no spouse present                                                           |number  |
|fraction_employment         |Fraction of People Employed                                      |Fraction of people employed in civilian labor force                                                                              |number  |
|median_housing_cost         |Median Monthly Housing Costs                                     |                                                                                                                                 |number  |
|median_housing_cost_2010adj |Median Monthly Housing Cost (in 2010 USD)                        |                                                                                                                                 |number  |
|median_rent                 |Median Monthly Rent                                              |                                                                                                                                 |number  |
|median_rent_2010adj         |Median Monthly Rent (in 2010 USD)                                |                                                                                                                                 |number  |
|fraction_conditions         |Fraction of households with substandard housing conditions       |substandard housing: incomplete plumbing or kitchens, overcrowding, 30% or more of household income spent on rent or owner costs |number  |
|fraction_builtbf1970        |Fraction of Homes Built Before 1970                              |                                                                                                                                 |number  |
|fraction_vacant             |Fraction of housing units that are vacant                        |                                                                                                                                 |number  |
|fraction_nhl                |Fraction of People Not Hispanic/Latino                           |                                                                                                                                 |number  |
|fraction_nhl_w              |Fraction of People White and Not Hispanic/Latino                 |                                                                                                                                 |number  |
|fraction_nhl_b              |Fraction of People Black and Not Hispanic/Latino                 |                                                                                                                                 |number  |
|fraction_nhl_o              |Fraction of People Not Black, Not White, and Not Hispanic/Latino |                                                                                                                                 |number  |
|fraction_hl                 |Fraction of People Hispanic/Latino                               |                                                                                                                                 |number  |
|fraction_hl_w               |Fraction of People White and Hispanic/Latino                     |                                                                                                                                 |number  |
|fraction_hl_b               |Fraction of People Black and Hispanic/Latino                     |                                                                                                                                 |number  |
|fraction_hl_o               |Fraction of People Not Black, Not White, and Hispanic/Latino     |                                                                                                                                 |number  |
|fraction_lesh               |Fraction of Households Speaking Limited English                  |Available from 2016 onwards                                                                                                      |number  |
|median_income               |Median Household Income                                          |                                                                                                                                 |number  |
|median_income_2010adj       |Median Household Income (in 2010 USD)                            |                                                                                                                                 |number  |
|fraction_hs                 |Fraction of Adults with At Least High School Education           |Available from 2012 onwards                                                                                                      |number  |
