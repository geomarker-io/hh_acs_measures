library(dplyr)
library(tigris)
options(tigris_use_cache = TRUE)
options(tigris_class = "sf")
library(tidycensus)
library(purrr)
## library(rlang)

# TODO run check for census api key
Sys.getenv("CENSUS_API_KEY")

#### get 2010-2020 5-year ACS tract-level variables -----------------------------------

states_needed <-
  tigris::fips_codes %>%
  select(state_code, state_name) %>%
  filter(!state_name %in% c(
    "American Samoa", "Guam", "Northern Mariana Islands",
    "Puerto Rico", "U.S. Minor Outlying Islands",
    "U.S. Virgin Islands"
  )) %>%
  unique() %>%
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
      year = year,
      fraction_poverty = estimate / summary_est,
      fraction_poverty_moe = moe_prop(estimate, summary_est, moe, summary_moe),
      n_pop = summary_est,
      n_pop_moe = summary_moe
    )
  left_join(tracts_needed, d, by = "census_tract_id_2010")
}

d_acs$acs_poverty <- mappp_dfr(2010:2019, get_acs_poverty)

get_acs_children <- function(year = 2019) {
  d <-
    my_get_acs(
    variables = c(
      paste0("B01001_00", 3:6),
      paste0("B01001_0", 27:30)
    ),
    year = year
    ) |>
    suppressMessages() |>
    group_by(GEOID) |>
    summarize(
      n_children_lt18 = sum(estimate),
      n_children_lt18_moe = moe_sum(moe, estimate)
    ) |>
    rename(census_tract_id_2010 = GEOID) |>
    mutate(year = year)
  left_join(tracts_needed, d, by = "census_tract_id_2010")
}

d_acs$acs_children <- mappp_dfr(2010:2019, get_acs_children)

# number of household with children under age 18
# total number of household
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
    rename(census_tract_id_2010 = GEOID) |>
    mutate(year = year)
  left_join(tracts_needed, d, by = "census_tract_id_2010")
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
    ) %>%
    transmute(
      census_tract_id_2010 = GEOID,
      fraction_insured = n_insured / n_total,
      fraction_insured_moe = moe_prop(n_insured, n_total, n_insured_moe, n_total_moe),
      year = year
    )
  left_join(tracts_needed, d, by = "census_tract_id_2010")
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
      fraction_snap_moe = moe_prop(estimate, summary_est, moe, summary_moe),
      year = year
    )
  left_join(tracts_needed, d, by = "census_tract_id_2010")
}

d_acs$acs_snap <- mappp_dfr(2010:2019, get_acs_snap)

# join all
d <- purrr::reduce(d_acs, left_join, by = c("census_tract_id_2010", "year"))
d <- select(d, -contains("_moe"))

# TODO missing years??
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

## # fraction family household with no spouse
## acs_perc_fam_nospouse <- my_get_acs(
##   variables = "B11001_004",
##   summary_var = "B11001_001",
##   year = censusYr
## ) %>%
##   transmute(
##     GEOID = GEOID,
##     fraction_fam_nospouse = estimate / summary_est,
##     fraction_fam_nospouse_moe = moe_prop(estimate, summary_est, moe, summary_moe)
##   )

## # fraction of population 16 years and Over in labor force (2011-2020)
## if (censusYr >= 2011) {
##   acs_perc_laborforce <- my_get_acs(
##     variables = "B23025_002",
##     summary_var = "B23025_001",
##     year = censusYr
##   ) %>%
##     transmute(
##       GEOID = GEOID,
##       fraction_laborforce = estimate / summary_est,
##       fraction_laborforce_moe = moe_prop(estimate, summary_est, moe, summary_moe)
##     )
## } else {
##   acs_perc_laborforce <-
##     tibble::tibble(
##       GEOID = tracts_needed,
##       fraction_laborforce = NA,
##       fraction_laborforce_moe = NA
##     )
## }

## # median monthly housing cost
## acs_median_housingcost <- my_get_acs(
##   variables = "B25105_001",
##   year = censusYr
## ) %>%
##   transmute(
##     GEOID = GEOID,
##     median_housingcost = estimate,
##     median_housingcost_moe = moe
##   )

## # median gross rent
## acs_median_grossrent <- my_get_acs(
##   variables = "B25113_001",
##   year = censusYr
## ) %>%
##   transmute(
##     GEOID = GEOID,
##     median_rent = estimate,
##     median_rent_moe = moe
##   )

## # fraction selected condition
## acs_perc_condition <- my_get_acs(
##   variables = c(
##     paste0("B25123_00", 3:6),
##     "B25123_009",
##     paste0("B25123_0", 10:12)
##   ),
##   summary_var = "B25123_001",
##   year = censusYr
## ) %>%
##   group_by(GEOID) %>%
##   summarize(
##     n_condition = sum(estimate),
##     n_condition_moe = moe_sum(moe, estimate),
##     n_total = unique(summary_est),
##     n_total_moe = unique(summary_moe)
##   ) %>%
##   transmute(
##     GEOID = GEOID,
##     fraction_condition = n_condition / n_total,
##     fraction_condition_moe = moe_prop(n_condition, n_total, n_condition_moe, n_total_moe)
##   )

## # fraction built before 1970
## acs_perc_bf1970 <- my_get_acs(
##   variables = c(
##     paste0("B25034_00", 7:9),
##     "B25034_010"
##   ),
##   summary_var = "B25034_001",
##   year = censusYr
## ) %>%
##   group_by(GEOID) %>%
##   summarize(
##     n_bf1970 = sum(estimate),
##     n_bf1970_moe = moe_sum(moe, estimate),
##     n_total = unique(summary_est),
##     n_total_moe = unique(summary_moe)
##   ) %>%
##   transmute(
##     GEOID = GEOID,
##     fraction_bf1970 = n_bf1970 / n_total,
##     fraction_bf1970_moe = moe_prop(n_bf1970, n_total, n_bf1970_moe, n_total_moe)
##   )

## # fraction vacant housing
## acs_perc_vacant <- my_get_acs(
##   variables = "B25002_003",
##   summary_var = "B25002_001",
##   year = censusYr
## ) %>%
##   transmute(
##     GEOID = GEOID,
##     fraction_vacant = estimate / summary_est,
##     fraction_vacant_moe = moe_prop(estimate, summary_est, moe, summary_moe)
##   )


# TODO special case of 2020 where census tracts ids are different

