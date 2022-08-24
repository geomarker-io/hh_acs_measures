library(dplyr)
library(tigris)
options(tigris_use_cache = TRUE)
options(tigris_class = "sf")
library(tidycensus)
library(purrr)
library(CODECtools)

# TODO run check for census api key
Sys.getenv("CENSUS_API_KEY")

#### get 2010-2020 5-year ACS tract-level variables -----------------------------------

states_needed <-
  tigris::fips_codes |>
  select(state_code, state_name) |>
  filter(!state_name %in% c(
    "American Samoa", "Guam", "Northern Mariana Islands",
    "Puerto Rico", "U.S. Minor Outlying Islands",
    "U.S. Virgin Islands"
  )) |>
  unique() |>
  pull(state_code)

tracts_needed <-
  tigris::tracts(year = 2019, cb = TRUE) |>
  filter(STATEFP %in% states_needed) |>
  sf::st_drop_geometry() |>
  transmute(census_tract_id_2010 = GEOID) |>
  tibble::as_tibble()

my_get_acs <-
  purrr::partial(
    get_acs,
    geography = "tract",
    state = states_needed,
    moe_level = 95,
    survey = "acs5"
  )

mappp_dfr <- function(.x, .f) {
  mappp::mappp(.x, .f, parallel = TRUE, cache = TRUE, cache_name = "acs_data_cache") |>
    dplyr::bind_rows()
}

# cpi for inflation adjustment
cpi <- read.csv(file = "CPI_2010-2020.csv")
cpi <- cpi |>
  mutate(annual_cpi_2010 = subset(cpi, year == 2010)$annual_cpi) |> # in 2010 inflation-adjusted dollars
  mutate(ratio = annual_cpi_2010 / annual_cpi)

d_acs <- list()

get_acs_poverty <- function(year = 2019) {
  d <-
    my_get_acs(
      variables = "B17001_002",
      summary_var = "B17001_001",
      year = year
    ) |>
    suppressMessages() |>
    transmute(
      census_tract_id_2010 = GEOID,
      fraction_poverty = estimate / summary_est,
      fraction_poverty_moe = moe_prop(estimate, summary_est, moe, summary_moe)
    )
  left_join(tracts_needed, d, by = "census_tract_id_2010") |>
    mutate(year = year)
}

d_acs$acs_poverty <-
  mappp_dfr(2010:2019, get_acs_poverty) |>
  add_col_attrs(fraction_poverty,
    title = "Fraction of Households in Poverty",
    description = "Fraction of households with income below poverty level within the past 12 months"
  )

get_acs_children <- function(year = 2019) {
  d <-
    my_get_acs(
      variables = c(
        paste0("B01001_00", 3:6),
        paste0("B01001_0", 27:30)
      ),
      summary_var = "B01001_001",
      year = year
    ) |>
    suppressMessages() |>
    group_by(GEOID) |>
    summarize(
      n_children_lt18 = sum(estimate),
      n_children_lt18_moe = moe_sum(moe, estimate),
      n_pop = unique(summary_est),
      n_pop_moe = unique(summary_moe)
    ) |>
    rename(census_tract_id_2010 = GEOID)
  left_join(tracts_needed, d, by = "census_tract_id_2010") |>
    mutate(year = year)
}

d_acs$acs_children <-
  mappp_dfr(2010:2019, get_acs_children) |>
  add_col_attrs(n_children_lt18,
    title = "Number of Children",
    description = "Number of children and adolescents < 18 years of age"
  ) |>
  add_col_attrs(n_pop,
    title = "Number of Total People"
  )

get_acs_households <- function(year = 2019) {
  d <-
    my_get_acs(
      variables = "B11005_002",
      summary_var = "B11005_001",
      year = year
    ) |>
    suppressMessages() |>
    select(GEOID,
      n_household_lt18 = estimate,
      n_household_lt18_moe = moe,
      n_household = summary_est,
      n_household_moe = summary_moe
    ) |>
    rename(census_tract_id_2010 = GEOID)
  left_join(tracts_needed, d, by = "census_tract_id_2010") |>
    mutate(year = year)
}

d_acs$acs_households <-
  mappp_dfr(2010:2019, get_acs_households) |>
  add_col_attrs(n_household_lt18,
    title = "Number of Households With Children",
    description = "Number of households with children or adolescents < 18 years of age"
  ) |>
  add_col_attrs(n_household,
    title = "Number of Households"
  )

get_acs_insurance <- function(year = 2019) {
  d <-
    my_get_acs(
      variables = c(
        paste0("B27001_00", c(4, 7)),
        paste0("B27001_0", seq(10, 28, by = 3)),
        paste0("B27001_0", seq(32, 56, by = 3))
      ),
      summary_var = "B27001_001",
      year = year
    ) |>
    suppressMessages() |>
    group_by(GEOID) |>
    summarize(
      n_insured = sum(estimate),
      n_insured_moe = moe_sum(moe, estimate),
      n_total = unique(summary_est),
      n_total_moe = unique(summary_moe)
    ) |>
    transmute(
      census_tract_id_2010 = GEOID,
      fraction_insured = n_insured / n_total,
      fraction_insured_moe = moe_prop(n_insured, n_total, n_insured_moe, n_total_moe)
    )
  left_join(tracts_needed, d, by = "census_tract_id_2010") |>
    mutate(year = year)
}

d_acs$acs_insurance <-
  mappp_dfr(2012:2019, get_acs_insurance) |>
  add_col_attrs(fraction_insured,
    title = "Fraction of People Insured",
    description = "Fraction of population with health insurance (available from 2012 onwards only)"
  )

get_acs_snap <- function(year = 2019) {
  d <-
    my_get_acs(
      variables = "B22003_002",
      summary_var = "B22003_001",
      year = year
    ) |>
    suppressMessages() |>
    transmute(
      census_tract_id_2010 = GEOID,
      fraction_snap = estimate / summary_est,
      fraction_snap_moe = moe_prop(estimate, summary_est, moe, summary_moe)
    )
  left_join(tracts_needed, d, by = "census_tract_id_2010") |>
    mutate(year = year)
}

d_acs$acs_snap <-
  mappp_dfr(2010:2019, get_acs_snap) |>
  add_col_attrs(fraction_snap,
    title = "Fraction of Households Receiving SNAP",
    description = "Fraction of households receiving food stamps/SNAP in the past 12 months"
  )

get_acs_hh_type <- function(year = 2019) {
  d <-
    my_get_acs(
      variables = "B11001_004",
      summary_var = "B11001_002",
      year = year
    ) |>
    suppressMessages() |>
    transmute(
      census_tract_id_2010 = GEOID,
      fraction_fam_nospouse = estimate / summary_est,
      fraction_fam_nospouse_moe = moe_prop(estimate, summary_est, moe, summary_moe)
    )
  left_join(tracts_needed, d, by = "census_tract_id_2010") |>
    mutate(year = year)
}

d_acs$acs_hh_type <-
  mappp_dfr(2010:2019, get_acs_hh_type) |>
  add_col_attrs(fraction_fam_nospouse,
    title = "Fraction of family households with a single householder",
    description = "Single householder is male or female household, with no spouse present"
  )

get_acs_employment <- function(year = 2019) {
  d <-
    my_get_acs(
      variables = "B23025_004",
      summary_var = "B23025_003",
      year = year
    ) |>
    suppressMessages() |>
    transmute(
      census_tract_id_2010 = GEOID,
      fraction_employment = estimate / summary_est,
      fraction_employment_moe = moe_prop(estimate, summary_est, moe, summary_moe)
    )
  left_join(tracts_needed, d, by = "census_tract_id_2010") |>
    mutate(year = year)
}

d_acs$acs_employment <-
  mappp_dfr(2011:2019, get_acs_employment) |>
  add_col_attrs(fraction_employment,
    title = "Fraction of People Employed",
    description = "Fraction of people employed in civilian labor force"
  )

get_acs_housing_cost <- function(year = 2019) {
  d <-
    my_get_acs(
      variables = "B25105_001",
      year = year
    ) |>
    suppressMessages() |>
    transmute(
      census_tract_id_2010 = GEOID,
      median_housing_cost = estimate,
      median_housing_cost_moe = moe
    )
  left_join(tracts_needed, d, by = "census_tract_id_2010") |>
    mutate(year = year)
}

d_acs$acs_housing_cost <-
  mappp_dfr(2010:2019, get_acs_housing_cost) |>
  left_join(cpi, by = "year") |>
  mutate(median_housing_cost_2010adj = median_housing_cost * ratio) |>
  select(-annual_cpi, -annual_cpi_2010, -ratio) |>
  add_col_attrs(median_housing_cost,
    title = "Median Monthly Housing Costs",
    description = "Median monthly housing costs in 2010-adjusted dollars"
  )

# TODO B25106: Tenure by Housing Costs as a Percentage of Household Income
# fraction of renter households making less than $35k/year and spending at least 30% of income on housing costs??

get_acs_rent <- function(year = 2019) {
  d <-
    my_get_acs(
      variables = "B25113_001",
      year = year
    ) |>
    suppressMessages() |>
    transmute(
      census_tract_id_2010 = GEOID,
      median_rent = estimate,
      median_rent_moe = moe
    )
  left_join(tracts_needed, d, by = "census_tract_id_2010") |>
    mutate(year = year)
}

d_acs$acs_rent <-
  mappp_dfr(2010:2019, get_acs_rent) |>
  left_join(cpi, by = "year") |>
  mutate(median_rent_2010adj = median_rent * ratio) |>
  select(-annual_cpi, -annual_cpi_2010, -ratio) |>
  add_col_attrs(median_rent,
    title = "Median Monthly Rent",
    description = "Median monthly rent in 2010-adjusted dollars"
  )


get_acs_conditions <- function(year = 2019) {
  d <-
    my_get_acs(
      variables = c(
        paste0("B25123_00", 3:6),
        "B25123_009",
        paste0("B25123_0", 10:12)
      ),
      summary_var = "B25123_001",
      year = year
    ) |>
    suppressMessages() |>
    group_by(GEOID) |>
    summarize(
      n_conditions = sum(estimate),
      n_conditions_moe = moe_sum(moe, estimate),
      n_total = unique(summary_est),
      n_total_moe = unique(summary_moe)
    ) |>
    transmute(
      census_tract_id_2010 = GEOID,
      fraction_conditions = n_conditions / n_total,
      fraction_conditions_moe = moe_prop(n_conditions, n_total, n_conditions_moe, n_total_moe)
    )
  left_join(tracts_needed, d, by = "census_tract_id_2010") |>
    mutate(year = year)
}

d_acs$acs_conditions <-
  mappp_dfr(2010:2019, get_acs_conditions) |>
  add_col_attrs(fraction_conditions,
    title = "Fraction of households with substandard housing conditions",
    description = "substandard housing: incomplete plumbing or kitchens, overcrowding, 30% or more of household income spent on rent or owner costs"
  )


get_acs_yrbuilt <- function(year = 2019) {
  d <-
    my_get_acs(
      variables = c(
        paste0("B25034_00", 7:9),
        "B25034_010"
      ),
      summary_var = "B25034_001",
      year = year
    ) |>
    suppressMessages() |>
    group_by(GEOID) |>
    summarize(
      n_bf1970 = sum(estimate),
      n_bf1970_moe = moe_sum(moe, estimate),
      n_total = unique(summary_est),
      n_total_moe = unique(summary_moe)
    ) |>
    transmute(
      census_tract_id_2010 = GEOID,
      fraction_builtbf1970 = n_bf1970 / n_total,
      fraction_builtbf1970_moe = moe_prop(n_bf1970, n_total, n_bf1970_moe, n_total_moe)
    )
  left_join(tracts_needed, d, by = "census_tract_id_2010") |>
    mutate(year = year)
}

d_acs$acs_yrbuilt <-
  mappp_dfr(2010:2019, get_acs_yrbuilt) |>
  add_col_attrs(fraction_builtbf1970,
    name = "Fraction of Homes Built Before 1970"
  )


get_acs_vacant <- function(year = 2019) {
  d <-
    my_get_acs(
      variables = "B25002_003",
      summary_var = "B25002_001",
      year = year
    ) |>
    suppressMessages() |>
    transmute(
      census_tract_id_2010 = GEOID,
      fraction_vacant = estimate / summary_est,
      fraction_vacant_moe = moe_prop(estimate, summary_est, moe, summary_moe)
    )
  left_join(tracts_needed, d, by = "census_tract_id_2010") |>
    mutate(year = year)
}

d_acs$acs_vacant <-
  mappp_dfr(2010:2019, get_acs_vacant) |>
  add_col_attrs(fraction_vacant,
    title = "Fraction of housing units that are vacant"
  )


get_acs_fraction_nhl <- function(year = 2019) {
  d <-
    my_get_acs(
      variables = "B03002_002",
      summary_var = "B03002_001",
      year = year
    ) |>
    suppressMessages() |>
    transmute(
      census_tract_id_2010 = GEOID,
      fraction_nhl = estimate / summary_est,
      fraction_nhl_moe = moe_prop(estimate, summary_est, moe, summary_moe)
    )
  left_join(tracts_needed, d, by = "census_tract_id_2010") |>
    mutate(year = year)
}

d_acs$acs_fraction_nhl <-
  mappp_dfr(2010:2019, get_acs_fraction_nhl) |>
  add_col_attrs(fraction_nhl,
    title = "Fraction of People Not Hispanic/Latino"
  )


get_acs_fraction_nhl_w <- function(year = 2019) {
  d <-
    my_get_acs(
      variables = "B03002_003",
      summary_var = "B03002_001",
      year = year
    ) |>
    suppressMessages() |>
    transmute(
      census_tract_id_2010 = GEOID,
      fraction_nhl_w = estimate / summary_est,
      fraction_nhl_w_moe = moe_prop(estimate, summary_est, moe, summary_moe)
    )
  left_join(tracts_needed, d, by = "census_tract_id_2010") |>
    mutate(year = year)
}

d_acs$acs_fraction_nhl_w <-
  mappp_dfr(2010:2019, get_acs_fraction_nhl_w) |>
  add_col_attrs(fraction_nhl_w,
    title = "Fraction of People White and Not Hispanic/Latino"
  )

get_acs_fraction_nhl_b <- function(year = 2019) {
  d <-
    my_get_acs(
      variables = "B03002_004",
      summary_var = "B03002_001",
      year = year
    ) |>
    suppressMessages() |>
    transmute(
      census_tract_id_2010 = GEOID,
      fraction_nhl_b = estimate / summary_est,
      fraction_nhl_b_moe = moe_prop(estimate, summary_est, moe, summary_moe)
    )
  left_join(tracts_needed, d, by = "census_tract_id_2010") |>
    mutate(year = year)
}

d_acs$acs_fraction_nhl_b <-
  mappp_dfr(2010:2019, get_acs_fraction_nhl_b) |>
  add_col_attrs(fraction_nhl_b,
    title = "Fraction of People Black and Not Hispanic/Latino"
  )

get_acs_fraction_nhl_o <- function(year = 2019) {
  d <-
    my_get_acs(
      variables = paste0("B03002_00", 5:9),
      summary_var = "B03002_001",
      year = year
    ) |>
    suppressMessages() |>
    group_by(GEOID) |>
    summarize(
      n_nhl_o = sum(estimate),
      n_nhl_o_moe = moe_sum(moe, estimate),
      n_total = unique(summary_est),
      n_total_moe = unique(summary_moe)
    ) |>
    transmute(
      census_tract_id_2010 = GEOID,
      fraction_nhl_o = n_nhl_o / n_total,
      fraction_nhl_o_moe = moe_prop(n_nhl_o, n_total, n_nhl_o_moe, n_total_moe)
    )
  left_join(tracts_needed, d, by = "census_tract_id_2010") |>
    mutate(year = year)
}

d_acs$acs_fraction_nhl_o <-
  mappp_dfr(2010:2019, get_acs_fraction_nhl_o) |>
  add_col_attrs(fraction_nhl_o,
    title = "Fraction of People Not Black, Not White, and Not Hispanic/Latino"
  )

get_acs_fraction_hl <- function(year = 2019) {
  d <-
    my_get_acs(
      variables = "B03002_012",
      summary_var = "B03002_001",
      year = year
    ) |>
    suppressMessages() |>
    transmute(
      census_tract_id_2010 = GEOID,
      fraction_hl = estimate / summary_est,
      fraction_hl_moe = moe_prop(estimate, summary_est, moe, summary_moe)
    )
  left_join(tracts_needed, d, by = "census_tract_id_2010") |>
    mutate(year = year)
}

d_acs$acs_fraction_hl <-
  mappp_dfr(2010:2019, get_acs_fraction_hl) |>
  add_col_attrs(fraction_hl,
    title = "Fraction of People Hispanic/Latino"
  )

get_acs_fraction_hl_w <- function(year = 2019) {
  d <-
    my_get_acs(
      variables = "B03002_013",
      summary_var = "B03002_001",
      year = year
    ) |>
    suppressMessages() |>
    transmute(
      census_tract_id_2010 = GEOID,
      fraction_hl_w = estimate / summary_est,
      fraction_hl_w_moe = moe_prop(estimate, summary_est, moe, summary_moe)
    )
  left_join(tracts_needed, d, by = "census_tract_id_2010") |>
    mutate(year = year)
}

d_acs$acs_fraction_hl_w <-
  mappp_dfr(2010:2019, get_acs_fraction_hl_w) |>
  add_col_attrs(fraction_hl_w,
    title = "Fraction of People White and Hispanic/Latino"
  )

get_acs_fraction_hl_b <- function(year = 2019) {
  d <-
    my_get_acs(
      variables = "B03002_014",
      summary_var = "B03002_001",
      year = year
    ) |>
    suppressMessages() |>
    transmute(
      census_tract_id_2010 = GEOID,
      fraction_hl_b = estimate / summary_est,
      fraction_hl_b_moe = moe_prop(estimate, summary_est, moe, summary_moe)
    )
  left_join(tracts_needed, d, by = "census_tract_id_2010") |>
    mutate(year = year)
}

d_acs$acs_fraction_hl_b <-
  mappp_dfr(2010:2019, get_acs_fraction_hl_b) |>
  add_col_attrs(fraction_hl_b,
    title = "Fraction of People Black and Hispanic/Latino"
  )

get_acs_fraction_hl_o <- function(year = 2019) {
  d <-
    my_get_acs(
      variables = paste0("B03002_01", 5:9),
      summary_var = "B03002_001",
      year = year
    ) |>
    suppressMessages() |>
    group_by(GEOID) |>
    summarize(
      n_hl_o = sum(estimate),
      n_hl_o_moe = moe_sum(moe, estimate),
      n_total = unique(summary_est),
      n_total_moe = unique(summary_moe)
    ) |>
    transmute(
      census_tract_id_2010 = GEOID,
      fraction_hl_o = n_hl_o / n_total,
      fraction_hl_o_moe = moe_prop(n_hl_o, n_total, n_hl_o_moe, n_total_moe)
    )
  left_join(tracts_needed, d, by = "census_tract_id_2010") |>
    mutate(year = year)
}

d_acs$acs_fraction_hl_o <-
  mappp_dfr(2010:2019, get_acs_fraction_hl_o) |>
  add_col_attrs(fraction_hl_o,
    title = "Fraction of People Not Black, Not White, and Hispanic/Latino"
  )

get_acs_lesh <- function(year = 2019) {
  d <-
    my_get_acs(
      variables = c(
        paste0("C16002_00", c(4, 7)),
        paste0("C16002_01", c(0, 3))
      ),
      summary_var = "C16002_001",
      year = year
    ) |>
    suppressMessages() |>
    group_by(GEOID) |>
    summarize(
      n_lesh = sum(estimate),
      n_lesh_moe = moe_sum(moe, estimate),
      n_total = unique(summary_est),
      n_total_moe = unique(summary_moe)
    ) |>
    transmute(
      census_tract_id_2010 = GEOID,
      fraction_lesh = n_lesh / n_total,
      fraction_lesh_moe = moe_prop(n_lesh, n_total, n_lesh_moe, n_total_moe)
    )
  left_join(tracts_needed, d, by = "census_tract_id_2010") |>
    mutate(year = year)
}

d_acs$acs_leshh <-
  mappp_dfr(2016:2019, get_acs_lesh) |>
  add_col_attrs(fraction_lesh,
    title = "Fraction of Households Speaking Limited English",
    description = "Available from 2016 onwards"
  )

get_acs_income <- function(year = 2019) {
  d <-
    my_get_acs(
      variables = "B19013_001",
      year = year
    ) |>
    suppressMessages() |>
    transmute(
      census_tract_id_2010 = GEOID,
      median_income = estimate,
      median_income_moe = moe
    )
  left_join(tracts_needed, d, by = "census_tract_id_2010") |>
    mutate(year = year)
}

d_acs$acs_income <-
  mappp_dfr(2010:2019, get_acs_income) |>
  left_join(cpi, by = "year") |>
  mutate(median_income_2010adj = median_income * ratio) |>
  select(-annual_cpi, -annual_cpi_2010, -ratio) |>
  add_col_attrs(median_income,
    title = "Median Household Income"
  )

get_acs_hs <- function(year = 2019) {
  d <-
    my_get_acs(
      variables = paste0("B15003_0", 17:25),
      summary_var = "B15003_001",
      year = year
    ) |>
    suppressMessages() |>
    group_by(GEOID) |>
    summarize(
      n_hs = sum(estimate),
      n_hs_moe = moe_sum(moe, estimate),
      n_total = unique(summary_est),
      n_total_moe = unique(summary_moe)
    ) |>
    transmute(
      census_tract_id_2010 = GEOID,
      fraction_hs = n_hs / n_total,
      fraction_hs_moe = moe_prop(n_hs, n_total, n_hs_moe, n_total_moe)
    )
  left_join(tracts_needed, d, by = "census_tract_id_2010") |>
    mutate(year = year)
}

d_acs$acs_hs <-
  mappp_dfr(2012:2019, get_acs_hs) |>
  add_col_attrs(fraction_hs,
    title = "Fraction of Adults with At Least High School Education",
    description = "Available from 2012 onwards"
  )

d <- purrr::reduce(d_acs, left_join, by = c("census_tract_id_2010", "year"))

d <- d |>
  add_col_attrs(census_tract_id_2010,
    title = "Census Tract Identifier (2010)",
    description = "FIPS identifier for census tracts 2010 - 2019"
  ) |>
  add_col_attrs(year,
    title = "Year",
    description = "Vintage of 5-year ACS estimates"
  )

d <- d |>
  add_attrs(
    name = "harmonized_historical_acs_data",
    path = "harmonized_historical_ACS_data.csv",
    title = "Harmonized Historical American Community Survey Data",
    description = "ACS variables from 2010 - 2019, census tracts for contiguous US",
    url = "https://github.com/geomarker-io/harmonized_historical_ACS_data",
  ) |>
  add_type_attrs()

# d <- select(d, -contains("_moe"))

## get_descriptors(d)
## get_schema(d)

write_tdr_csv(d)
system("aws s3 cp --recursive ./harmonized_historical_acs_data s3://codec-data/harmonized_historical_acs_data")

arrow::write_parquet(d, "harmonized_historical_acs_data.parquet")
system("aws cp harmonized_historical_acs_data.parquet s3://codec-data/harmonized_historical_acs_data/")
