library(dplyr)
library(tigris)
options(tigris_use_cache = TRUE)
options(tigris_class = "sf")
library(tidycensus)

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
  pull(GEOID)

my_get_acs <-
  purrr::partial(
    get_acs,
    geography = "tract",
    state = states_needed,
    moe_level = 95,
    survey = "acs5"
  )
                             
# get acs data by year
get_acs_data <- function(censusYr) {

  # fraction_poverty
  # income in past 12 months below poverty level
  acs_poverty <- my_get_acs(
    variables = "B17001_002",
    summary_var = "B17001_001",
    year = censusYr
  ) %>%
    transmute(
      GEOID = GEOID,
      fraction_poverty = estimate / summary_est,
      fraction_poverty_moe = moe_prop(estimate, summary_est, moe, summary_moe),
      n_pop = summary_est,
      n_pop_moe = summary_moe
    )

  # Number of children under age 18
  acs_n_child_lt18 <- my_get_acs(
    variables = c(
      paste0("B01001_00", 3:6),
      paste0("B01001_0", 27:30)
    ),
    year = censusYr
  ) %>%
    group_by(GEOID) %>%
    summarize(
      n_children_lt18 = sum(estimate),
      n_children_lt18_moe = moe_sum(moe, estimate)
    )

  # number of household with children under age 18
  # total number of household
  acs_n_hh_lt18 <- my_get_acs(
    variables = "B11005_002",
    summary_var = "B11005_001",
    year = censusYr
  ) %>%
    select(GEOID,
      n_household_lt18 = estimate,
      n_household_lt18_moe = moe,
      n_household = summary_est,
      n_household_moe = summary_moe
    )

  # Fraction insured (2012-2020)
  if (censusYr >= 2012) {
    acs_perc_insured <- my_get_acs(
      variables = c(
        paste0("B27001_00", c(4, 7)),
        paste0("B27001_0", seq(10, 28, by = 3)),
        paste0("B27001_0", seq(32, 56, by = 3))
      ),
      summary_var = "B27001_001",
      year = censusYr
    ) %>%
      group_by(GEOID) %>%
      summarize(
        n_insured = sum(estimate),
        n_insured_moe = moe_sum(moe, estimate),
        n_total = unique(summary_est),
        n_total_moe = unique(summary_moe)
      ) %>%
      transmute(
        GEOID = GEOID,
        fraction_insured = n_insured / n_total,
        fraction_insured_moe = moe_prop(n_insured, n_total, n_insured_moe, n_total_moe)
      )
  } else {
    acs_perc_insured <-
      tibble::tibble(
        GEOID = tracts_needed,
        fraction_insured = NA,
        fraction_insured_moe = NA
      )
  }

  # fraction family household with no spouse
  acs_perc_fam_nospouse <- my_get_acs(
    variables = "B11001_004",
    summary_var = "B11001_001",
    year = censusYr
  ) %>%
    transmute(
      GEOID = GEOID,
      fraction_fam_nospouse = estimate / summary_est,
      fraction_fam_nospouse_moe = moe_prop(estimate, summary_est, moe, summary_moe)
    )

  # fraction of population 16 years and Over in labor force (2011-2020)
  if (censusYr >= 2011) {
    acs_perc_laborforce <- my_get_acs(
      variables = "B23025_002",
      summary_var = "B23025_001",
      year = censusYr
    ) %>%
      transmute(
        GEOID = GEOID,
        fraction_laborforce = estimate / summary_est,
        fraction_laborforce_moe = moe_prop(estimate, summary_est, moe, summary_moe)
      )
  } else {
    acs_perc_laborforce <-
      tibble::tibble(
        GEOID = tracts_needed,
        fraction_laborforce = NA,
        fraction_laborforce_moe = NA
      )
  }

  # fraction received food stamps
  acs_perc_foodstamp <- my_get_acs(
    variables = "B22003_002",
    summary_var = "B22003_001",
    year = censusYr
  ) %>%
    transmute(
      GEOID = GEOID,
      fraction_foodstamp = estimate / summary_est,
      fraction_foodstamp_moe = moe_prop(estimate, summary_est, moe, summary_moe)
    )

  # median monthly housing cost
  acs_median_housingcost <- my_get_acs(
    variables = "B25105_001",
    year = censusYr
  ) %>%
    transmute(
      GEOID = GEOID,
      median_housingcost = estimate,
      median_housingcost_moe = moe
    )

  # median gross rent
  acs_median_grossrent <- my_get_acs(
    variables = "B25113_001",
    year = censusYr
  ) %>%
    transmute(
      GEOID = GEOID,
      median_rent = estimate,
      median_rent_moe = moe
    )

  # fraction selected condition
  acs_perc_condition <- my_get_acs(
    variables = c(
      paste0("B25123_00", 3:6),
      "B25123_009",
      paste0("B25123_0", 10:12)
    ),
    summary_var = "B25123_001",
    year = censusYr
  ) %>%
    group_by(GEOID) %>%
    summarize(
      n_condition = sum(estimate),
      n_condition_moe = moe_sum(moe, estimate),
      n_total = unique(summary_est),
      n_total_moe = unique(summary_moe)
    ) %>%
    transmute(
      GEOID = GEOID,
      fraction_condition = n_condition / n_total,
      fraction_condition_moe = moe_prop(n_condition, n_total, n_condition_moe, n_total_moe)
    )

  # fraction built before 1970
  acs_perc_bf1970 <- my_get_acs(
    variables = c(
      paste0("B25034_00", 7:9),
      "B25034_010"
    ),
    summary_var = "B25034_001",
    year = censusYr
  ) %>%
    group_by(GEOID) %>%
    summarize(
      n_bf1970 = sum(estimate),
      n_bf1970_moe = moe_sum(moe, estimate),
      n_total = unique(summary_est),
      n_total_moe = unique(summary_moe)
    ) %>%
    transmute(
      GEOID = GEOID,
      fraction_bf1970 = n_bf1970 / n_total,
      fraction_bf1970_moe = moe_prop(n_bf1970, n_total, n_bf1970_moe, n_total_moe)
    )

  # fraction vacant housing
  acs_perc_vacant <- my_get_acs(
    variables = "B25002_003",
    summary_var = "B25002_001",
    year = censusYr
  ) %>%
    transmute(
      GEOID = GEOID,
      fraction_vacant = estimate / summary_est,
      fraction_vacant_moe = moe_prop(estimate, summary_est, moe, summary_moe)
    )

  out <-
    list(
      tibble::tibble(GEOID = tracts_needed),
      acs_poverty,
      acs_n_child_lt18,
      acs_n_hh_lt18,
      acs_perc_insured,
      acs_perc_fam_nospouse,
      acs_perc_laborforce,
      acs_perc_foodstamp,
      acs_median_housingcost,
      acs_median_grossrent,
      acs_perc_condition,
      acs_perc_bf1970,
      acs_perc_vacant
    ) |>
    purrr::reduce(left_join, by = "GEOID")

  saveRDS(out, paste0("HH_ACS_", censusYr, ".rds"))

  return(out)
}

# saves all years of data as HH_ACS_{censusYr}.rds files
purrr::walk(2010:2019, get_acs_data)

# TODO special case of 2020 where census tracts ids are different


