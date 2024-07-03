library(dplyr)
library(tigris)
options(tigris_use_cache = TRUE)
options(tigris_class = "sf")
library(tidycensus)
library(purrr)
library(mappp)
library(digest)
library(codec)
library(fr)

if (Sys.getenv("CENSUS_API_KEY") == "") stop("set CENSUS_API_KEY enviroment variable")

#### get 2010-2022 5-year ACS tract-level variables -----------------------------------

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

tracts_2010 <-
  tigris::tracts(year = 2019, cb = TRUE) |>
  filter(STATEFP %in% states_needed) |>
  sf::st_drop_geometry() |>
  transmute(census_tract_id = GEOID,
            census_tract_vintage = "2010") |>
  tibble::as_tibble()

tracts_2020 <-
  tigris::tracts(year = 2020, cb = TRUE) |>
  filter(STATEFP %in% states_needed) |>
  sf::st_drop_geometry() |>
  transmute(census_tract_id = GEOID,
            census_tract_vintage = "2020") |>
  tibble::as_tibble()

tracts_needed <- bind_rows(tracts_2010, tracts_2020)

my_get_acs <-
  purrr::partial(
    get_acs,
    geography = "tract",
    state = states_needed,
    moe_level = 95,
    survey = "acs5"
  )

mappp_dfr <- function(.x, .f) {
  mappp(.x, .f, parallel = FALSE, cache = TRUE, cache_name = "acs_data_cache") |>
    dplyr::bind_rows()
}

# cpi for inflation adjustment
cpi <- read.csv(file = "CPI_2010-2023.csv")
cpi <- cpi |>
  mutate(annual_cpi_2010 = subset(cpi, year == 2010)$annual_cpi) |> # in 2010 inflation-adjusted dollars
  mutate(ratio = annual_cpi_2010 / annual_cpi)

d_acs <- list()

get_acs_poverty <- function(year) {
  tract_vintage <- as.character(10 * floor(year / 10))
  d <-
    my_get_acs(
      variables = "B17001_002",
      summary_var = "B17001_001",
      year = year
    ) |>
    suppressMessages() |>
    transmute(
      census_tract_id = GEOID,
      census_tract_vintage = tract_vintage,
      fraction_poverty = estimate / summary_est,
      fraction_poverty_moe = moe_prop(estimate, summary_est, moe, summary_moe)
    )
  left_join(tracts_needed, d, by = c("census_tract_id", "census_tract_vintage")) |>
    filter(.data$census_tract_vintage == .env$tract_vintage) |>
    mutate(year = year)
}

d_acs$acs_poverty <- mappp_dfr(2010:2022, get_acs_poverty) |>
  as_fr_tdr(
    name = "hh_acs_measures",
    version = "1.2.0",
    title = "Harmonized Historical American Community Survey Measures",
    description = "2010 - 2022 measures derived from ACS variables for census tracts in the contiguous US",
    homepage = "https://geomarker.io/hh_acs_measures"
  ) |>
  update_field("fraction_poverty",
               title = "Fraction of Households in Poverty",
               description = "Fraction of households with income below poverty level within the past 12 months"
  )

get_acs_children <- function(year) {
  tract_vintage <- as.character(10 * floor(year / 10))
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
    rename(census_tract_id = GEOID) |>
    mutate(census_tract_vintage = tract_vintage)
  left_join(tracts_needed, d, by = c("census_tract_id", "census_tract_vintage")) |>
    filter(.data$census_tract_vintage == .env$tract_vintage) |>
    mutate(year = year)
}

d_acs$acs_children <- mappp_dfr(2010:2022, get_acs_children) |>
  as_fr_tdr(name = "name") |>
  update_field("n_children_lt18",
               title = "Number of Children",
               description = "Number of children and adolescents < 18 years of age"
  ) |>
  update_field("n_pop",
               title = "Number of Total People"
  )

get_acs_households <- function(year) {
  tract_vintage <- as.character(10 * floor(year / 10))
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
    rename(census_tract_id = GEOID) |>
    mutate(census_tract_vintage = tract_vintage)
  left_join(tracts_needed, d, by = c("census_tract_id", "census_tract_vintage")) |>
    filter(.data$census_tract_vintage == .env$tract_vintage) |>
    mutate(year = year)
}

d_acs$acs_households <- mappp_dfr(2010:2022, get_acs_households) |>
  as_fr_tdr() |>
  update_field("n_household_lt18",
               title = "Number of Households With Children",
               description = "Number of households with children or adolescents < 18 years of age"
  ) |>
  update_field("n_household",
               title = "Number of Households"
  )

get_acs_insurance <- function(year) {
  tract_vintage <- as.character(10 * floor(year / 10))
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
      census_tract_id = GEOID,
      census_tract_vintage = tract_vintage,
      fraction_insured = n_insured / n_total,
      fraction_insured_moe = moe_prop(n_insured, n_total, n_insured_moe, n_total_moe)
    )
  left_join(tracts_needed, d, by = c("census_tract_id", "census_tract_vintage")) |>
    filter(.data$census_tract_vintage == .env$tract_vintage) |>
    mutate(year = year)
}

d_acs$acs_insurance <- mappp_dfr(2012:2022, get_acs_insurance) |>
  as_fr_tdr() |>
  update_field("fraction_insured",
               title = "Fraction of People Insured",
               description = "Fraction of population with health insurance (available from 2012 onwards only)"
  )

get_acs_snap <- function(year) {
  tract_vintage <- as.character(10 * floor(year / 10))
  d <-
    my_get_acs(
      variables = "B19058_002",
      summary_var = "B19058_001",
      year = year
    ) |>
    suppressMessages() |>
    transmute(
      census_tract_id = GEOID,
      census_tract_vintage = tract_vintage,
      fraction_snap = estimate / summary_est,
      fraction_snap_moe = moe_prop(estimate, summary_est, moe, summary_moe)
    )
  left_join(tracts_needed, d, by = c("census_tract_id", "census_tract_vintage")) |>
    filter(.data$census_tract_vintage == .env$tract_vintage) |>
    mutate(year = year)
}

d_acs$acs_snap <-mappp_dfr(2010:2022, get_acs_snap) |>
  as_fr_tdr() |>
  update_field("fraction_snap",
               title = "Fraction of Households Receiving Assisted Income",
               description = "Fraction of households receiving public assistance income or food stamps/SNAP in the past 12 months"
  )

get_acs_hh_type <- function(year) {
  tract_vintage <- as.character(10 * floor(year / 10))
  d <-
    my_get_acs(
      variables = "B11001_004",
      summary_var = "B11001_002",
      year = year
    ) |>
    suppressMessages() |>
    transmute(
      census_tract_id = GEOID,
      census_tract_vintage = tract_vintage,
      fraction_fam_nospouse = estimate / summary_est,
      fraction_fam_nospouse_moe = moe_prop(estimate, summary_est, moe, summary_moe)
    )
  left_join(tracts_needed, d, by = c("census_tract_id", "census_tract_vintage")) |>
    filter(.data$census_tract_vintage == .env$tract_vintage) |>
    mutate(year = year)
}

d_acs$acs_hh_type <- mappp_dfr(2010:2022, get_acs_hh_type) |>
  as_fr_tdr() |>
  update_field("fraction_fam_nospouse",
               title = "Fraction of family households with a single householder",
               description = "Single householder is male or female household, with no spouse present"
  )

get_acs_employment <- function(year) {
  tract_vintage <- as.character(10 * floor(year / 10))
  d <-
    my_get_acs(
      variables = "B23025_004",
      summary_var = "B23025_003",
      year = year
    ) |>
    suppressMessages() |>
    transmute(
      census_tract_id = GEOID,
      census_tract_vintage = tract_vintage,
      fraction_employment = estimate / summary_est,
      fraction_employment_moe = moe_prop(estimate, summary_est, moe, summary_moe)
    )
  left_join(tracts_needed, d, by = c("census_tract_id", "census_tract_vintage")) |>
    filter(.data$census_tract_vintage == .env$tract_vintage) |>
    mutate(year = year)
}

d_acs$acs_employment <- mappp_dfr(2011:2022, get_acs_employment) |>
  as_fr_tdr() |>
  update_field("fraction_employment",
               title = "Fraction of People Employed",
               description = "Fraction of people employed in civilian labor force"
  ) 

get_acs_housing_units <- function(year) {
  tract_vintage <- as.character(10 * floor(year / 10))
  d <-
    my_get_acs(
      variables = "B25001_001",
      year = year
    ) |>
    suppressMessages() |>
    transmute(
      census_tract_id = GEOID,
      census_tract_vintage = tract_vintage,
      n_housing_units = estimate,
      n_housing_units_moe = moe
    )
  left_join(tracts_needed, d, by = c("census_tract_id", "census_tract_vintage")) |>
    filter(.data$census_tract_vintage == .env$tract_vintage) |>
    mutate(year = year)
}

d_acs$acs_housing_units <- mappp_dfr(2010:2022, get_acs_housing_units) |>
  as_fr_tdr() |>
  update_field("n_housing_units",
               title = "Number of Housing Units",
               description = "Housing units are any separate living quarters (e.g., house, apartment, mobile home) either occupied or vacant"
  ) 

get_acs_home_value <- function(year) {
  tract_vintage <- as.character(10 * floor(year / 10))
  d <-
    my_get_acs(
      variables = "B25077_001",
      year = year
    ) |>
    suppressMessages() |>
    transmute(
      census_tract_id = GEOID,
      census_tract_vintage = tract_vintage,
      median_home_value = estimate,
      median_home_value_moe = moe
    )
  left_join(tracts_needed, d, by = c("census_tract_id", "census_tract_vintage")) |>
    filter(.data$census_tract_vintage == .env$tract_vintage) |>
    mutate(year = year)
}

d_acs$acs_home_value <-
  mappp_dfr(2010:2022, get_acs_home_value) |>
  left_join(cpi, by = "year") |>
  mutate(median_home_value_2010adj = median_home_value * ratio) |>
  select(-annual_cpi, -annual_cpi_2010, -ratio) |>
  as_fr_tdr(name = "home value") |>
  update_field("median_home_value",
               title = "Median Value of Owner-Occupied Housing Units"
  ) |>
  update_field("median_home_value_2010adj",
               title = "Median Value of Owner-Occupied Housing Units (in 2010 USD)"
  ) 

get_acs_renters <- function(year) {
  tract_vintage <- as.character(10 * floor(year / 10))
  d <-
    my_get_acs(
      variables = "B25003_003",
      summary_var = "B25003_001",
      year = year
    ) |>
    suppressMessages() |>
    transmute(
      census_tract_id = GEOID,
      census_tract_vintage = tract_vintage,
      fraction_housing_renters = estimate / summary_est,
      fraction_housing_renters_moe = moe_prop(estimate, summary_est, moe, summary_moe)
    )
  left_join(tracts_needed, d, by = c("census_tract_id", "census_tract_vintage")) |>
    filter(.data$census_tract_vintage == .env$tract_vintage) |>
    mutate(year = year)
}

d_acs$acs_renters <- mappp_dfr(2011:2022, get_acs_renters) |>
  as_fr_tdr() |>
  update_field("fraction_housing_renters",
               title = "Fraction of Housing Units Occupied by Renters",
               description = "fraction denominator is number of *occupied* housing units"
  ) 

get_acs_rent <- function(year) {
  tract_vintage <- as.character(10 * floor(year / 10))
  d <-
    my_get_acs(
      variables = "B25071_001",
      year = year
    ) |>
    suppressMessages() |>
    transmute(
      census_tract_id = GEOID,
      census_tract_vintage = tract_vintage,
      median_rent_to_income_percentage = estimate,
      median_rent_to_income_percentage_moe = moe
    )
  left_join(tracts_needed, d, by = c("census_tract_id", "census_tract_vintage")) |>
    filter(.data$census_tract_vintage == .env$tract_vintage) |>
    mutate(year = year)
}

d_acs$acs_rent <- mappp_dfr(2010:2022, get_acs_rent) |>
  as_fr_tdr() |>
  update_field("median_rent_to_income_percentage",
               title = "Median Rent to Income Percentage",
               description = "The median of the percentage of rent to income among all renter-occupied housing units"
  ) 
  
get_acs_high_rent <- function(year) {
  tract_vintage <- as.character(10 * floor(year / 10))
  d <-
    my_get_acs(
      variables = paste0("B25070_", c("007", "008", "009", "010")),
      summary_var = "B25070_001",
      year = year
    ) |>
    suppressMessages() |>
    group_by(GEOID) |>
    summarize(
      n_high_rent = sum(estimate),
      n_high_rent_moe = moe_sum(moe, estimate),
      n_total = unique(summary_est),
      n_total_moe = unique(summary_moe)
    ) |>
    transmute(
      census_tract_id = GEOID,
      census_tract_vintage = tract_vintage,
      fraction_high_rent = n_high_rent / n_total,
      fraction_high_rent_moe = moe_prop(n_high_rent, n_total, n_high_rent_moe, n_total_moe)
    )
  left_join(tracts_needed, d, by = c("census_tract_id", "census_tract_vintage")) |>
    filter(.data$census_tract_vintage == .env$tract_vintage) |>
    mutate(year = year)
}

d_acs$acs_high_rent <- mappp_dfr(2010:2022, get_acs_high_rent) |>
  as_fr_tdr() |>
  update_field("fraction_high_rent",
               title = "Fraction of Housing Units Paying at least 30% of Income on Rent"
  ) 

get_acs_conditions <- function(year) {
  tract_vintage <- as.character(10 * floor(year / 10))
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
      census_tract_id = GEOID,
      census_tract_vintage = tract_vintage,
      fraction_conditions = n_conditions / n_total,
      fraction_conditions_moe = moe_prop(n_conditions, n_total, n_conditions_moe, n_total_moe)
    )
  left_join(tracts_needed, d, by = c("census_tract_id", "census_tract_vintage")) |>
    filter(.data$census_tract_vintage == .env$tract_vintage) |>
    mutate(year = year)
}

d_acs$acs_conditions <- mappp_dfr(2010:2022, get_acs_conditions) |>
  as_fr_tdr() |>
  update_field("fraction_conditions",
               title = "Fraction of Housing Units with Substandard Housing Conditions",
               description = "substandard housing: incomplete plumbing or kitchens, overcrowding, 30% or more of household income spent on rent or owner costs"
  )

get_acs_yrbuilt <- function(year) {
  tract_vintage <- as.character(10 * floor(year / 10))
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
      census_tract_id = GEOID,
      census_tract_vintage = tract_vintage,
      fraction_builtbf1970 = n_bf1970 / n_total,
      fraction_builtbf1970_moe = moe_prop(n_bf1970, n_total, n_bf1970_moe, n_total_moe)
    )
  left_join(tracts_needed, d, by = c("census_tract_id", "census_tract_vintage")) |>
    filter(.data$census_tract_vintage == .env$tract_vintage) |>
    mutate(year = year)
}

d_acs$acs_yrbuilt <- mappp_dfr(2010:2022, get_acs_yrbuilt) |>
  as_fr_tdr() |>
  update_field("fraction_builtbf1970",
               title = "Fraction of Housing Units Built Before 1970"
  )

get_acs_vacant <- function(year) {
  tract_vintage <- as.character(10 * floor(year / 10))
  d <-
    my_get_acs(
      variables = "B25002_003",
      summary_var = "B25002_001",
      year = year
    ) |>
    suppressMessages() |>
    transmute(
      census_tract_id = GEOID,
      census_tract_vintage = tract_vintage,
      fraction_vacant = estimate / summary_est,
      fraction_vacant_moe = moe_prop(estimate, summary_est, moe, summary_moe)
    )
  left_join(tracts_needed, d, by = c("census_tract_id", "census_tract_vintage")) |>
    filter(.data$census_tract_vintage == .env$tract_vintage) |>
    mutate(year = year)
}

d_acs$acs_vacant <- mappp_dfr(2010:2022, get_acs_vacant) |>
  as_fr_tdr() |>
  update_field("fraction_vacant",
               title = "Fraction of Housing Units that are Vacant"
  ) 


get_acs_fraction_nhl <- function(year) {
  tract_vintage <- as.character(10 * floor(year / 10))
  d <-
    my_get_acs(
      variables = "B03002_002",
      summary_var = "B03002_001",
      year = year
    ) |>
    suppressMessages() |>
    transmute(
      census_tract_id = GEOID,
      census_tract_vintage = tract_vintage,
      fraction_nhl = estimate / summary_est,
      fraction_nhl_moe = moe_prop(estimate, summary_est, moe, summary_moe)
    )
  left_join(tracts_needed, d, by = c("census_tract_id", "census_tract_vintage")) |>
    filter(.data$census_tract_vintage == .env$tract_vintage) |>
    mutate(year = year)
}

d_acs$acs_fraction_nhl <- mappp_dfr(2010:2022, get_acs_fraction_nhl) |>
  as_fr_tdr() |>
  update_field("fraction_nhl",
               title = "Fraction of People Not Hispanic/Latino"
  )

get_acs_fraction_nhl_w <- function(year) {
  tract_vintage <- as.character(10 * floor(year / 10))
  d <-
    my_get_acs(
      variables = "B03002_003",
      summary_var = "B03002_001",
      year = year
    ) |>
    suppressMessages() |>
    transmute(
      census_tract_id = GEOID,
      census_tract_vintage = tract_vintage,
      fraction_nhl_w = estimate / summary_est,
      fraction_nhl_w_moe = moe_prop(estimate, summary_est, moe, summary_moe)
    )
  left_join(tracts_needed, d, by = c("census_tract_id", "census_tract_vintage")) |>
    filter(.data$census_tract_vintage == .env$tract_vintage) |>
    mutate(year = year)
}

d_acs$acs_fraction_nhl_w <- mappp_dfr(2010:2022, get_acs_fraction_nhl_w) |>
  as_fr_tdr() |>
  update_field("fraction_nhl_w",
               title = "Fraction of People White and Not Hispanic/Latino"
  )

get_acs_fraction_nhl_b <- function(year) {
  tract_vintage <- as.character(10 * floor(year / 10))
  d <-
    my_get_acs(
      variables = "B03002_004",
      summary_var = "B03002_001",
      year = year
    ) |>
    suppressMessages() |>
    transmute(
      census_tract_id = GEOID,
      census_tract_vintage = tract_vintage,
      fraction_nhl_b = estimate / summary_est,
      fraction_nhl_b_moe = moe_prop(estimate, summary_est, moe, summary_moe)
    )
  left_join(tracts_needed, d, by = c("census_tract_id", "census_tract_vintage")) |>
    filter(.data$census_tract_vintage == .env$tract_vintage) |>
    mutate(year = year)
}

d_acs$acs_fraction_nhl_b <- mappp_dfr(2010:2022, get_acs_fraction_nhl_b) |>
  as_fr_tdr() |>
  update_field("fraction_nhl_b",
               title = "Fraction of People Black and Not Hispanic/Latino"
  )

get_acs_fraction_nhl_o <- function(year) {
  tract_vintage <- as.character(10 * floor(year / 10))
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
      census_tract_id = GEOID,
      census_tract_vintage = tract_vintage,
      fraction_nhl_o = n_nhl_o / n_total,
      fraction_nhl_o_moe = moe_prop(n_nhl_o, n_total, n_nhl_o_moe, n_total_moe)
    )
  left_join(tracts_needed, d, by = c("census_tract_id", "census_tract_vintage")) |>
    filter(.data$census_tract_vintage == .env$tract_vintage) |>
    mutate(year = year)
}

d_acs$acs_fraction_nhl_o <- mappp_dfr(2010:2022, get_acs_fraction_nhl_o) |>
  as_fr_tdr() |>
  update_field("fraction_nhl_o",
               title = "Fraction of People Not Black, Not White, and Not Hispanic/Latino"
  )

get_acs_fraction_hl <- function(year) {
  tract_vintage <- as.character(10 * floor(year / 10))
  d <-
    my_get_acs(
      variables = "B03002_012",
      summary_var = "B03002_001",
      year = year
    ) |>
    suppressMessages() |>
    transmute(
      census_tract_id = GEOID,
      census_tract_vintage = tract_vintage,
      fraction_hl = estimate / summary_est,
      fraction_hl_moe = moe_prop(estimate, summary_est, moe, summary_moe)
    )
  left_join(tracts_needed, d, by = c("census_tract_id", "census_tract_vintage")) |>
    filter(.data$census_tract_vintage == .env$tract_vintage) |>
    mutate(year = year)
}

d_acs$acs_fraction_hl <- mappp_dfr(2010:2022, get_acs_fraction_hl) |>  
  as_fr_tdr() |>
  update_field("fraction_hl",
               title = "Fraction of People Hispanic/Latino"
  )

get_acs_fraction_hl_w <- function(year) {
  tract_vintage <- as.character(10 * floor(year / 10))
  d <-
    my_get_acs(
      variables = "B03002_013",
      summary_var = "B03002_001",
      year = year
    ) |>
    suppressMessages() |>
    transmute(
      census_tract_id = GEOID,
      census_tract_vintage = tract_vintage,
      fraction_hl_w = estimate / summary_est,
      fraction_hl_w_moe = moe_prop(estimate, summary_est, moe, summary_moe)
    )
  left_join(tracts_needed, d, by = c("census_tract_id", "census_tract_vintage")) |>
    filter(.data$census_tract_vintage == .env$tract_vintage) |>
    mutate(year = year)
}

d_acs$acs_fraction_hl_w <- mappp_dfr(2010:2022, get_acs_fraction_hl_w) |>
  as_fr_tdr() |>
  update_field("fraction_hl_w",
               title = "Fraction of People White and Hispanic/Latino"
  )

get_acs_fraction_hl_b <- function(year) {
  tract_vintage <- as.character(10 * floor(year / 10))
  d <-
    my_get_acs(
      variables = "B03002_014",
      summary_var = "B03002_001",
      year = year
    ) |>
    suppressMessages() |>
    transmute(
      census_tract_id = GEOID,
      census_tract_vintage = tract_vintage,
      fraction_hl_b = estimate / summary_est,
      fraction_hl_b_moe = moe_prop(estimate, summary_est, moe, summary_moe)
    )
  left_join(tracts_needed, d, by = c("census_tract_id", "census_tract_vintage")) |>
    filter(.data$census_tract_vintage == .env$tract_vintage) |>
    mutate(year = year)
}

d_acs$acs_fraction_hl_b <- mappp_dfr(2010:2022, get_acs_fraction_hl_b) |>
  as_fr_tdr() |>
  update_field("fraction_hl_b",
               title = "Fraction of People Black and Hispanic/Latino"
  )

get_acs_fraction_hl_o <- function(year) {
  tract_vintage <- as.character(10 * floor(year / 10))
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
      census_tract_id = GEOID,
      census_tract_vintage = tract_vintage,
      fraction_hl_o = n_hl_o / n_total,
      fraction_hl_o_moe = moe_prop(n_hl_o, n_total, n_hl_o_moe, n_total_moe)
    )
  left_join(tracts_needed, d, by = c("census_tract_id", "census_tract_vintage")) |>
    filter(.data$census_tract_vintage == .env$tract_vintage) |>
    mutate(year = year)
}

d_acs$acs_fraction_hl_o <- mappp_dfr(2010:2022, get_acs_fraction_hl_o) |>
  as_fr_tdr() |>
  update_field("fraction_hl_o",
               title = "Fraction of People Not Black, Not White, and Hispanic/Latino"
  ) 

get_acs_lesh <- function(year) {
  tract_vintage <- as.character(10 * floor(year / 10))
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
      census_tract_id = GEOID,
      census_tract_vintage = tract_vintage,
      fraction_lesh = n_lesh / n_total,
      fraction_lesh_moe = moe_prop(n_lesh, n_total, n_lesh_moe, n_total_moe)
    )
  left_join(tracts_needed, d, by = c("census_tract_id", "census_tract_vintage")) |>
    filter(.data$census_tract_vintage == .env$tract_vintage) |>
    mutate(year = year)
}

d_acs$acs_leshh <- mappp_dfr(2016:2022, get_acs_lesh) |>
  as_fr_tdr() |>
  update_field("fraction_lesh",
               title = "Fraction of Households Speaking Limited English",
               description = "Available from 2016 onwards"
  ) 
  
get_acs_income <- function(year) {
  tract_vintage <- as.character(10 * floor(year / 10))
  d <-
    my_get_acs(
      variables = "B19013_001",
      year = year
    ) |>
    suppressMessages() |>
    transmute(
      census_tract_id = GEOID,
      census_tract_vintage = tract_vintage,
      median_income = estimate,
      median_income_moe = moe
    )
  left_join(tracts_needed, d, by = c("census_tract_id", "census_tract_vintage")) |>
    filter(.data$census_tract_vintage == .env$tract_vintage) |>
    mutate(year = year)
}

d_acs$acs_income <-
  mappp_dfr(2010:2022, get_acs_income) |>
  left_join(cpi, by = "year") |>
  mutate(median_income_2010adj = median_income * ratio) |>
  select(-annual_cpi, -annual_cpi_2010, -ratio) |>
  as_fr_tdr(name = "income") |>
  update_field("median_income",
               title = "Median Household Income"
  ) |>
  update_field("median_income_2010adj",
               title = "Median Household Income (in 2010 USD)"
  ) 

get_acs_hs <- function(year) {
  tract_vintage <- as.character(10 * floor(year / 10))
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
      census_tract_id = GEOID,
      census_tract_vintage = tract_vintage,
      fraction_hs = n_hs / n_total,
      fraction_hs_moe = moe_prop(n_hs, n_total, n_hs_moe, n_total_moe)
    )
  left_join(tracts_needed, d, by = c("census_tract_id", "census_tract_vintage")) |>
    filter(.data$census_tract_vintage == .env$tract_vintage) |>
    mutate(year = year)
}

d_acs$acs_hs <- mappp_dfr(2012:2022, get_acs_hs) |>
  as_fr_tdr() |>
  update_field("fraction_hs",
               title = "Fraction of Adults with At Least High School Education",
               description = "Available from 2012 onwards"
  )

fr_left_join <- function(x, y, ...) {
  out <- as_fr_tdr(dplyr::left_join(x, y, ...), .template = x)
  
  x_schema_fields <- x@schema@fields
  y_schema_fields <- y@schema@fields
  
  common_fields <- x_schema_fields[names(x_schema_fields) %in% names(y_schema_fields)]
  unique_x_fields <- x_schema_fields[!names(x_schema_fields) %in% names(y_schema_fields)]
  unique_y_fields <- y_schema_fields[!names(y_schema_fields) %in% names(x_schema_fields)]
  out@schema@fields <- c(common_fields, unique_x_fields, unique_y_fields)
  
  return(out)
}

d <- purrr::reduce(d_acs, fr_left_join, by = c("census_tract_id", "census_tract_vintage", "year")) |>
  fr_select(census_tract_id, census_tract_vintage, year,  # no fr_relocate, so use fr_select
            fraction_poverty:fraction_hs_moe) |>          # to order columns
  fr_mutate(across(starts_with("fraction_"), \(.) round(., 3))) |>
  fr_mutate(across(starts_with("n_"), as.integer)) |>
  fr_mutate(across(starts_with("median_"), \(.) signif(., 3))) |>
  update_field("census_tract_id",
               title = "Census Tract Identifier",
               description = "can refer to 2010 or 2020 vintage census tracts; unique only in combination with `census_tract_vintage`"
               ) |>
  update_field("census_tract_vintage",
               title = "Census Tract Vintage",
               description = "The year of the decennial census that defines the tract (2010 for years 2010-2019 and 2020 for years 2020-2029)"
               ) |>
  update_field("year",
               title = "Year",
               description = "The year of the 5-year ACS estimates (e.g., the 2019 ACS covers 2015 - 2019)"
               )

# save to disk
fr::write_fr_tdr(d, dir = ".")
