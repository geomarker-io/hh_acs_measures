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
cpi <- read.csv(file='CPI_2010-2020.csv')   
cpi <- cpi |> 
  mutate(annual_cpi_2010 = subset(cpi, year == 2010)$annual_cpi) |>  # in 2010 inflation-adjusted dollars
  mutate(ratio = annual_cpi_2010 / annual_cpi)


d_acs <- list()


# fraction_poverty
# income in past 12 months below poverty level
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

d_acs$acs_poverty <- mappp_dfr(2010:2019, get_acs_poverty)


# number of children under age 18
# total population
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

d_acs$acs_children <- mappp_dfr(2010:2019, get_acs_children)


# number of household with children under age 18
# total household
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

d_acs$acs_households <- mappp_dfr(2010:2019, get_acs_households)


# fraction insured (2012 onwards only)
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

d_acs$acs_insurance <- mappp_dfr(2012:2019, get_acs_insurance) 


# fraction received food stamps or snap
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

d_acs$acs_snap <- mappp_dfr(2010:2019, get_acs_snap)


# fraction family household with no spouse
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
    
d_acs$acs_hh_type <- mappp_dfr(2010:2019, get_acs_hh_type)


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


# median monthly housing cost
get_acs_housingcost <- function(year = 2019) {
  d <-
    my_get_acs(
      variables = "B25105_001",
      year = year
    ) |>
    suppressMessages() |>
    transmute(
      census_tract_id_2010 = GEOID,
      median_housingcost = estimate,
      median_housingcost_moe = moe
    )
  left_join(tracts_needed, d, by = "census_tract_id_2010") |> 
    mutate(year = year)
}

d_acs$acs_housingcost <- mappp_dfr(2010:2019, get_acs_housingcost) |> 
  left_join(cpi, by = "year") |> 
  mutate(median_housingcost_2010adj = median_housingcost * ratio) |> 
  select(-annual_cpi, -annual_cpi_2010, -ratio)


# median gross rent
get_acs_grossrent <- function(year = 2019) {
  d <-
    my_get_acs(
      variables = "B25113_001",
      year = year
    ) |>
    suppressMessages() |>
    transmute(
      census_tract_id_2010 = GEOID,
      median_grossrent = estimate,
      median_grossrent_moe = moe
    )
  left_join(tracts_needed, d, by = "census_tract_id_2010") |> 
    mutate(year = year)
}

d_acs$acs_grossrent <- mappp_dfr(2010:2019, get_acs_grossrent) |> 
  left_join(cpi, by = "year") |> 
  mutate(median_grossrent_2010adj = median_grossrent * ratio) |> 
  select(-annual_cpi, -annual_cpi_2010, -ratio)


# fraction of tenure with selected physical and financial condition
get_acs_tenure <- function(year = 2019) {
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
      n_tenure_cond = sum(estimate),
      n_tenure_cond_moe = moe_sum(moe, estimate),
      n_total = unique(summary_est),
      n_total_moe = unique(summary_moe)
    ) |>
    transmute(
      census_tract_id_2010 = GEOID,
      fraction_tenure_cond = n_tenure_cond / n_total,
      fraction_tenure_cond_moe = moe_prop(n_tenure_cond, n_total, n_tenure_cond_moe, n_total_moe)
    )
  left_join(tracts_needed, d, by = "census_tract_id_2010") |> 
    mutate(year = year)
}

d_acs$acs_tenure <- mappp_dfr(2010:2019, get_acs_tenure)


# fraction of structure built before 1970
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

d_acs$acs_yrbuilt <- mappp_dfr(2010:2019, get_acs_yrbuilt)


# fraction of vacant housing
get_acs_occup <- function(year = 2019) {
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

d_acs$acs_occup <- mappp_dfr(2010:2019, get_acs_occup)


# racial/ethnic composition - non-Hispanic or Latino, total
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

d_acs$acs_fraction_nhl <- mappp_dfr(2010:2019, get_acs_fraction_nhl)


# racial/ethnic composition - non-Hispanic or Latino, White
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

d_acs$acs_fraction_nhl_w <- mappp_dfr(2010:2019, get_acs_fraction_nhl_w)


# racial/ethnic composition - non-Hispanic or Latino, Black
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

d_acs$acs_fraction_nhl_b <- mappp_dfr(2010:2019, get_acs_fraction_nhl_b)


# racial/ethnic composition - non-Hispanic or Latino, Other
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

d_acs$acs_fraction_nhl_o <- mappp_dfr(2010:2019, get_acs_fraction_nhl_o)


# racial/ethnic composition - Hispanic or Latino, total
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

d_acs$acs_fraction_hl <- mappp_dfr(2010:2019, get_acs_fraction_hl)


# racial/ethnic composition - Hispanic or Latino, White
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

d_acs$acs_fraction_hl_w <- mappp_dfr(2010:2019, get_acs_fraction_hl_w)


# racial/ethnic composition - Hispanic or Latino, Black
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

d_acs$acs_fraction_hl_b <- mappp_dfr(2010:2019, get_acs_fraction_hl_b)


# racial/ethnic composition - Hispanic or Latino, Other
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

d_acs$acs_fraction_hl_o <- mappp_dfr(2010:2019, get_acs_fraction_hl_o)


# fraction of limited english speaking household (2016 onwards only)
get_acs_leshh <- function(year = 2019) {
  d <-
    my_get_acs(
      variables = c(paste0("C16002_00", c(4,7)),
                    paste0("C16002_01", c(0,3))),
      summary_var = "C16002_001",
      year = year
    ) |>
    suppressMessages() |>
    group_by(GEOID) |>
    summarize(
      n_leshh = sum(estimate),
      n_leshh_moe = moe_sum(moe, estimate),
      n_total = unique(summary_est),
      n_total_moe = unique(summary_moe)
    ) |>
    transmute(
      census_tract_id_2010 = GEOID,
      fraction_leshh = n_leshh / n_total,
      fraction_leshh_moe = moe_prop(n_leshh, n_total, n_leshh_moe, n_total_moe)
    )
  left_join(tracts_needed, d, by = "census_tract_id_2010") |> 
    mutate(year = year)
}

d_acs$acs_leshh <- mappp_dfr(2016:2019, get_acs_leshh) 


# median household income
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

d_acs$acs_income <- mappp_dfr(2010:2019, get_acs_income) |> 
  left_join(cpi, by = "year") |> 
  mutate(median_income_2010adj = median_income * ratio) |> 
  select(-annual_cpi, -annual_cpi_2010, -ratio)


# fraction of high school education (2012 onwards only)
get_acs_hs <- function(year = 2019) {
  d <-
    my_get_acs(
      variables = paste0('B15003_0',17:25),
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

d_acs$acs_hs <- mappp_dfr(2012:2019, get_acs_hs) 
  

# join all
d <- purrr::reduce(d_acs, left_join, by = c("census_tract_id_2010", "year"))
d <- select(d, -contains("_moe")) 

# saveRDS(d, "data/harmonized_historical_acs_data.rds")

d |>
  group_by(year) |>
  summarize(n = n())

source("00_helpers.R")

d <- d |>
  set_attrs(
    name = "harmonized_historical_acs_data",
    path = "s3://geomarker-io/data/harmonized_historical_ACS_data.csv",
    # TODO how to utilize object versioning on AWS S3
    ## version = "0.1",
    title = "Harmonized Historical American Community Survey Data",
    description = "ACS variables from 2010 - 2019, census tracts for contiguous US",
    format = "csv",
    url = "https://github.com/geomarker-io/harmonized_historical_ACS_data",
    # TODO change these to use foreign keys specification
    codec.spatial = "census_tract_id_2010",
    codec.temporal = "year"
  )

d
attributes(d)
attr(d, "name")
attr(d, "path")

d <- d |>
  set_col_attrs(census_tract_id_2010,
    title = "Census Tract Identifier (2010)",
    description = "FIPS identifier for census tracts 2010 - 2019"
  ) |>
  set_col_attrs(n_children_lt18,
    title = "Number of Children",
    description = "Number of children and adolescents < 18 years of age"
  ) |>
  set_col_attrs(year,
    title = "Year",
    description = "Vintage of 5-year ACS estimates"
  ) |>
  set_col_attrs(n_household_lt18,
    title = "Number of Households with Children"
  ) |>
  set_col_attrs(n_household,
    title = "Number of Households"
  ) |>
  set_col_attrs(fraction_insured,
    title = "Fraction Population with Health Insurance"
  ) |>
  set_col_attrs(fraction_poverty,
    title = "Fraction of Households in Poverty"
    ) |>
  set_col_attrs(n_pop,
    title = "Number of Total People"
    ) |>
  set_col_attrs(fraction_snap,
    title = "Fraction of Households Receiving Food Stamps or SNAP Dollars"
  ) |>
  set_col_attrs(fraction_fam_nospouse,
                title = "Fraction of Households with No Spouse Present"
  ) |>
  set_col_attrs(fraction_laborforce,
                title = "Fraction of Population 16 Years and Over in Labor Force"
  )  |>
  set_col_attrs(median_housingcost,
                title = "Median Monthly Housing Costs"
  ) |>
  set_col_attrs(median_housingcost_2010adj,
                title = "Median Monthly Housing Costs (Inflation Adjusted)"
  ) |>
  set_col_attrs(median_grossrent,
                title = "Median Gross Rent by Year Householder Moved into Unit"
  ) |>
  set_col_attrs(median_grossrent_2010adj,
                title = "Median Gross Rent by Year Householder Moved into Unit (Inflation Adjusted)"
  ) |>
  set_col_attrs(fraction_tenure_cond,
                title = "Occupied Housing Units with One or More Selected Physical and Financial Conditions"
  ) |>
  set_col_attrs(fraction_builtbf1970,
                title = "Fraction of Housing Units Built Before 1970",
                description = "The variable represents housing stock correlated with high risk of elevated blood level concentrations in children"
  ) |>
  set_col_attrs(fraction_vacant,
                title = "Fraction of Vacant Housing Units"
  ) |>
  set_col_attrs(fraction_nhl,
                title = "Fraction of Non Hispanic or Latino (Total All Races)"
  ) |>
  set_col_attrs(fraction_nhl_w,
                title = "Fraction of Non Hispanic or Latino, White"
  ) |>
  set_col_attrs(fraction_nhl_b,
                title = "Fraction of Non Hispanic or Latino, Black"
  ) |>
  set_col_attrs(fraction_nhl_o,
                title = "Fraction of Non Hispanic or Latino, Any Other Race (Including Two or More Races)"
  ) |>
  set_col_attrs(fraction_hl,
                title = "Fraction of Fraction of Hispanic or Latino (Total All Races)"
  ) |>
  set_col_attrs(fraction_hl_w,
                title = "Fraction of Hispanic or Latino, White"
  ) |>
  set_col_attrs(fraction_hl_b,
                title = "Fraction of Hispanic or Latino, Black"
  ) |>
  set_col_attrs(fraction_hl_o,
                title = "Fraction of Hispanic or Latino, Any Other Race (Including Two or More Races)"
  ) |>
  set_col_attrs(fraction_leshh,
                title = "Fraction of Limited English Speaking Households"
  ) |>
  set_col_attrs(median_income,
                title = "Median Household Income in the Past 12 Months"
  ) |>
  set_col_attrs(median_income_2010adj,
                title = "Median Household Income in the Past 12 Months (2010 Inflation-Adjusted Dollars)"
  ) |>
  set_col_attrs(fraction_hs,
                title = "Fraction of Population 25 and Older with Education Attainment of at Least High School Graduate"
  )

## add type (from the Frictionless Table Schema) to each column based on its class

d <- add_type_attrs(d)

## a quick visual check of metadata in a browser
CB::ltable(make_metadata_from_attr(d))

# write metadata to file
write_metadata(d, "datapackage.yaml")

# write data to CSV file
readr::write_csv(d, "harmonized_historical_acs_data.csv")

# TODO how to upload to s3 location based on metadata$path?

# TODO special case of 2020 where census tracts ids are different

