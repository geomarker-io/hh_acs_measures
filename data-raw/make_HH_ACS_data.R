library(dplyr)
library(tigris)
options(tigris_use_cache = TRUE)
options(tigris_class = "sf")
library(tidycensus)

library(purrr)
library(rlang)

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

mappp_dfr(2010:2019, get_acs_poverty) |>
  saveRDS("data-raw/acs_poverty.rds")

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

mappp_dfr(2010:2019, get_acs_children) |>
  saveRDS("data-raw/acs_children.rds")

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

mappp_dfr(2010:2019, get_acs_households) |>
  saveRDS("data-raw/acs_households.rds")


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

mappp_dfr(2012:2019, get_acs_insurance) |>
  saveRDS("data-raw/acs_insurance.rds")

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

mappp_dfr(2010:2019, get_acs_snap) |>
  saveRDS("data-raw/acs_snap.rds")

# join all
d <-
  fs::dir_ls("data-raw", glob = "*acs_*.rds") |>
  purrr::map(readRDS) |>
  purrr::reduce(left_join, by = c("census_tract_id_2010", "year"))


# based on the deprecated rlang::set_attrs()
set_attrs <- function(.x, ...) {
  attrs <- rlang::dots_list(...)
  attributes(.x) <- c(attributes(.x), attrs)
  .x
}

# domain-style names?  or is attribute a list?
## d <- d |>
##   set_attrs(
##     codec.name = "harmonized_historical_acs_data",
##     codec.title = "Harmonized Historical American Community Survey Data",
##     codec.url = "https://github.com/geomarker-io/harmonized_historical_ACS_data",
##     codec.description = "ACS variables from 2010 - 2019, census tracts for contiguous US",
##     codec.version = "0.1"
##     )

d <- d |>
  set_attrs(
    codec = list(
      name = "harmonized_historical_acs_data",
      version = "0.1",
      title = "Harmonized Historical American Community Survey Data",
      url = "https://github.com/geomarker-io/harmonized_historical_ACS_data",
      description = "ACS variables from 2010 - 2019, census tracts for contiguous US",
      spatial = "census_tract_id_2010",
      temporal = "year"
    )
  )

attributes(d)
attr(d, "codec")
attr(d, "codec")$name
attr(d, "codec")$version
## attr(d, "name")
## attr(d, "version")

# could we make a cool print method for a data.frame with codec attributes?
# or with codec variable names?

set_var_attrs <- function(.x, var, ...) {
  .x[[rlang::enexpr(var)]] <-
    set_attrs(pull(.x, {{ var }}), ...)
  .x
}

d <- select(d, -contains("_moe"))

d <- d |>
  set_var_attrs(census_tract_id_2010,
    title = "Census Tract Identifier (2010)",
    description = "FIPS identifier for census tracts 2010 - 2019"
  ) |>
  set_var_attrs(n_children_lt18,
    title = "Number of Children",
    description = "Number of children and adolescents < 18 years of age"
  ) |>
  set_var_attrs(year,
    title = "Year",
    description = "Vintage of 5-year ACS estimates"
  ) |>
  set_var_attrs(n_household_lt18,
    title = "Number of Households with Children"
  ) |>
  set_var_attrs(n_household,
    title = "Number of Households"
  ) |>
  set_var_attrs(fraction_insured,
    title = "Fraction Population with Health Insurance"
  ) |>
  set_var_attrs(fraction_poverty,
    title = "Fraction of Households in Poverty"
    ) |>
  set_var_attrs(n_pop,
    title = "Number of Total People"
    ) |>
  set_var_attrs(fraction_snap,
    title = "Fraction of Households Receiving Food Stamps or SNAP Dollars"
  )

# after metadata is set, save to parquet file system
## fs::dir_delete(fs::path("data", "harmonized_historical_acs_data"))
## d |>
##   group_by(year) |>
##   arrow::write_dataset(fs::path("data", "harmonized_historical_acs_data"))

# missing years??
d |>
  group_by(year) |>
  summarize(n = n())


arrow::write_parquet(d, fs::path("data", "harmonized_historical_acs_data.parquet"))

d <- arrow::read_parquet(fs::path("data", "harmonized_historical_acs_data.parquet"))

# only read some of the columns from a file:
arrow::read_parquet("data/harmonized_historical_acs_data.parquet",
  col_select = c(census_tract_id_2010, year)
)

# dataset metadata
attr(d, "codec")

# e.g.
attr(d, "codec") |>
  tibble::enframe() |>
  tidyr::unnest(cols = c(value)) |>
  knitr::kable()

# column metadata:
purrr::map_dfr(d, attributes) |>
  mutate(name = attr(d, "names")) |>
  relocate(name) |>
  knitr::kable()
  




## arrow::open_dataset("data/hh_acs-parquet") |>
##   filter(
##     year >= 2016,
##     census_tract_id_2010 %in% sf::st_drop_geometry(cincy::tract_tigris_2010)$tract_fips
##   ) |>
##   collect()




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

