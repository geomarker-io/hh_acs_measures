library(dplyr)
library(tigris)
options(tigris_use_cache = TRUE)
options(tigris_class = "sf")
library(tidycensus)
library(purrr)
library(mappp)
library(digest)
library(dpkg)

if (Sys.getenv("CENSUS_API_KEY") == "") {
  stop("set CENSUS_API_KEY enviroment variable")
}

#### get 2013-2023 5-year ACS zcta-level variables -----------------------------------
# 2010 causes get_acs to fail
# 2011 and 2012 all values are NA

zctas_2010 <-
  tigris::zctas(year = 2019, cb = TRUE) |>
  sf::st_drop_geometry() |>
  transmute(zcta = GEOID10, zcta_vintage = "2010") |>
  tibble::as_tibble()

zctas_2020 <-
  tigris::zctas(year = 2020, cb = TRUE) |>
  sf::st_drop_geometry() |>
  transmute(zcta = GEOID20, zcta_vintage = "2020") |>
  tibble::as_tibble()

zctas_needed <- bind_rows(zctas_2010, zctas_2020)

my_get_acs <-
  purrr::partial(
    get_acs,
    geography = "zcta",
    moe_level = 95,
    survey = "acs5"
  )

mappp_dfr <- function(.x, .f) {
  mappp(
    .x,
    .f,
    parallel = FALSE,
    cache = TRUE,
    cache_name = "acs_data_cache"
  ) |>
    dplyr::bind_rows()
}

# cpi for inflation adjustment
cpi <- read.csv(file = "CPI_2010-2023.csv")
cpi <- cpi |>
  mutate(annual_cpi_2010 = subset(cpi, year == 2010)$annual_cpi) |> # in 2010 inflation-adjusted dollars
  mutate(ratio = annual_cpi_2010 / annual_cpi)

d_acs <- list()

get_acs(
  geography = "zcta",
  variables = "B17001_002",
  summary_var = "B17001_001",
  year = 2011,
  moe_level = 95,
  survey = "acs5"
)

get_acs_poverty <- function(year) {
  zcta_vintage <- as.character(10 * floor(year / 10))
  d <-
    my_get_acs(
      variables = "B17001_002",
      summary_var = "B17001_001",
      year = year
    ) |>
    suppressMessages() |>
    transmute(
      zcta = GEOID,
      zcta_vintage = zcta_vintage,
      fraction_poverty = estimate / summary_est,
      fraction_poverty_moe = moe_prop(estimate, summary_est, moe, summary_moe)
    )
  left_join(
    zctas_needed,
    d,
    by = c("zcta", "zcta_vintage")
  ) |>
    filter(.data$zcta_vintage == .env$zcta_vintage) |>
    mutate(year = year)
}

d_acs$acs_poverty <- mappp_dfr(2013:2023, get_acs_poverty)

get_acs_children <- function(year) {
  zcta_vintage <- as.character(10 * floor(year / 10))
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
    rename(zcta = GEOID) |>
    mutate(zcta_vintage = zcta_vintage)
  left_join(
    zctas_needed,
    d,
    by = c("zcta", "zcta_vintage")
  ) |>
    filter(.data$zcta_vintage == .env$zcta_vintage) |>
    mutate(year = year)
}

d_acs$acs_children <- mappp_dfr(2013:2023, get_acs_children)

get_acs_households <- function(year) {
  zcta_vintage <- as.character(10 * floor(year / 10))
  d <-
    my_get_acs(
      variables = "B11005_002",
      summary_var = "B11005_001",
      year = year
    ) |>
    suppressMessages() |>
    select(
      GEOID,
      n_household_lt18 = estimate,
      n_household_lt18_moe = moe,
      n_household = summary_est,
      n_household_moe = summary_moe
    ) |>
    rename(zcta = GEOID) |>
    mutate(zcta_vintage = zcta_vintage)
  left_join(
    zctas_needed,
    d,
    by = c("zcta", "zcta_vintage")
  ) |>
    filter(.data$zcta_vintage == .env$zcta_vintage) |>
    mutate(year = year)
}

d_acs$acs_households <- mappp_dfr(2013:2023, get_acs_households)

get_acs_insurance <- function(year) {
  zcta_vintage <- as.character(10 * floor(year / 10))
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
      zcta = GEOID,
      zcta_vintage = zcta_vintage,
      fraction_insured = n_insured / n_total,
      fraction_insured_moe = moe_prop(
        n_insured,
        n_total,
        n_insured_moe,
        n_total_moe
      )
    )
  left_join(
    zctas_needed,
    d,
    by = c("zcta", "zcta_vintage")
  ) |>
    filter(.data$zcta_vintage == .env$zcta_vintage) |>
    mutate(year = year)
}

d_acs$acs_insurance <- mappp_dfr(2013:2023, get_acs_insurance)

get_acs_snap <- function(year) {
  zcta_vintage <- as.character(10 * floor(year / 10))
  d <-
    my_get_acs(
      variables = "B19058_002",
      summary_var = "B19058_001",
      year = year
    ) |>
    suppressMessages() |>
    transmute(
      zcta = GEOID,
      zcta_vintage = zcta_vintage,
      fraction_snap = estimate / summary_est,
      fraction_snap_moe = moe_prop(estimate, summary_est, moe, summary_moe)
    )
  left_join(
    zctas_needed,
    d,
    by = c("zcta", "zcta_vintage")
  ) |>
    filter(.data$zcta_vintage == .env$zcta_vintage) |>
    mutate(year = year)
}

d_acs$acs_snap <- mappp_dfr(2013:2023, get_acs_snap)

get_acs_hh_type <- function(year) {
  zcta_vintage <- as.character(10 * floor(year / 10))
  d <-
    my_get_acs(
      variables = "B11001_004",
      summary_var = "B11001_002",
      year = year
    ) |>
    suppressMessages() |>
    transmute(
      zcta = GEOID,
      zcta_vintage = zcta_vintage,
      fraction_fam_nospouse = estimate / summary_est,
      fraction_fam_nospouse_moe = moe_prop(
        estimate,
        summary_est,
        moe,
        summary_moe
      )
    )
  left_join(
    zctas_needed,
    d,
    by = c("zcta", "zcta_vintage")
  ) |>
    filter(.data$zcta_vintage == .env$zcta_vintage) |>
    mutate(year = year)
}

d_acs$acs_hh_type <- mappp_dfr(2013:2023, get_acs_hh_type)

get_acs_employment <- function(year) {
  zcta_vintage <- as.character(10 * floor(year / 10))
  d <-
    my_get_acs(
      variables = "B23025_004",
      summary_var = "B23025_003",
      year = year
    ) |>
    suppressMessages() |>
    transmute(
      zcta = GEOID,
      zcta_vintage = zcta_vintage,
      fraction_employment = estimate / summary_est,
      fraction_employment_moe = moe_prop(
        estimate,
        summary_est,
        moe,
        summary_moe
      )
    )
  left_join(
    zctas_needed,
    d,
    by = c("zcta", "zcta_vintage")
  ) |>
    filter(.data$zcta_vintage == .env$zcta_vintage) |>
    mutate(year = year)
}

d_acs$acs_employment <- mappp_dfr(2013:2023, get_acs_employment)

get_acs_housing_units <- function(year) {
  zcta_vintage <- as.character(10 * floor(year / 10))
  d <-
    my_get_acs(
      variables = "B25001_001",
      year = year
    ) |>
    suppressMessages() |>
    transmute(
      zcta = GEOID,
      zcta_vintage = zcta_vintage,
      n_housing_units = estimate,
      n_housing_units_moe = moe
    )
  left_join(
    zctas_needed,
    d,
    by = c("zcta", "zcta_vintage")
  ) |>
    filter(.data$zcta_vintage == .env$zcta_vintage) |>
    mutate(year = year)
}

d_acs$acs_housing_units <- mappp_dfr(2013:2023, get_acs_housing_units)

get_acs_home_value <- function(year) {
  zcta_vintage <- as.character(10 * floor(year / 10))
  d <-
    my_get_acs(
      variables = "B25077_001",
      year = year
    ) |>
    suppressMessages() |>
    transmute(
      zcta = GEOID,
      zcta_vintage = zcta_vintage,
      median_home_value = estimate,
      median_home_value_moe = moe
    )
  left_join(
    zctas_needed,
    d,
    by = c("zcta", "zcta_vintage")
  ) |>
    filter(.data$zcta_vintage == .env$zcta_vintage) |>
    mutate(year = year)
}

d_acs$acs_home_value <-
  mappp_dfr(2013:2023, get_acs_home_value) |>
  left_join(cpi, by = "year") |>
  mutate(median_home_value_2010adj = median_home_value * ratio) |>
  select(-annual_cpi, -annual_cpi_2010, -ratio)

get_acs_renters <- function(year) {
  zcta_vintage <- as.character(10 * floor(year / 10))
  d <-
    my_get_acs(
      variables = "B25003_003",
      summary_var = "B25003_001",
      year = year
    ) |>
    suppressMessages() |>
    transmute(
      zcta = GEOID,
      zcta_vintage = zcta_vintage,
      fraction_housing_renters = estimate / summary_est,
      fraction_housing_renters_moe = moe_prop(
        estimate,
        summary_est,
        moe,
        summary_moe
      )
    )
  left_join(
    zctas_needed,
    d,
    by = c("zcta", "zcta_vintage")
  ) |>
    filter(.data$zcta_vintage == .env$zcta_vintage) |>
    mutate(year = year)
}

d_acs$acs_renters <- mappp_dfr(2013:2023, get_acs_renters)

get_acs_rent <- function(year) {
  zcta_vintage <- as.character(10 * floor(year / 10))
  d <-
    my_get_acs(
      variables = "B25071_001",
      year = year
    ) |>
    suppressMessages() |>
    transmute(
      zcta = GEOID,
      zcta_vintage = zcta_vintage,
      median_rent_to_income_percentage = estimate,
      median_rent_to_income_percentage_moe = moe
    )
  left_join(
    zctas_needed,
    d,
    by = c("zcta", "zcta_vintage")
  ) |>
    filter(.data$zcta_vintage == .env$zcta_vintage) |>
    mutate(year = year)
}

d_acs$acs_rent <- mappp_dfr(2013:2023, get_acs_rent)

get_acs_high_rent <- function(year) {
  zcta_vintage <- as.character(10 * floor(year / 10))
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
      zcta = GEOID,
      zcta_vintage = zcta_vintage,
      fraction_high_rent = n_high_rent / n_total,
      fraction_high_rent_moe = moe_prop(
        n_high_rent,
        n_total,
        n_high_rent_moe,
        n_total_moe
      )
    )
  left_join(
    zctas_needed,
    d,
    by = c("zcta", "zcta_vintage")
  ) |>
    filter(.data$zcta_vintage == .env$zcta_vintage) |>
    mutate(year = year)
}

d_acs$acs_high_rent <- mappp_dfr(2013:2023, get_acs_high_rent)

get_acs_conditions <- function(year) {
  zcta_vintage <- as.character(10 * floor(year / 10))
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
      zcta = GEOID,
      zcta_vintage = zcta_vintage,
      fraction_conditions = n_conditions / n_total,
      fraction_conditions_moe = moe_prop(
        n_conditions,
        n_total,
        n_conditions_moe,
        n_total_moe
      )
    )
  left_join(
    zctas_needed,
    d,
    by = c("zcta", "zcta_vintage")
  ) |>
    filter(.data$zcta_vintage == .env$zcta_vintage) |>
    mutate(year = year)
}

d_acs$acs_conditions <- mappp_dfr(2013:2023, get_acs_conditions)

get_acs_yrbuilt <- function(year) {
  zcta_vintage <- as.character(10 * floor(year / 10))
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
      zcta = GEOID,
      zcta_vintage = zcta_vintage,
      fraction_builtbf1970 = n_bf1970 / n_total,
      fraction_builtbf1970_moe = moe_prop(
        n_bf1970,
        n_total,
        n_bf1970_moe,
        n_total_moe
      )
    )
  left_join(
    zctas_needed,
    d,
    by = c("zcta", "zcta_vintage")
  ) |>
    filter(.data$zcta_vintage == .env$zcta_vintage) |>
    mutate(year = year)
}

d_acs$acs_yrbuilt <- mappp_dfr(2013:2023, get_acs_yrbuilt)

get_acs_vacant <- function(year) {
  zcta_vintage <- as.character(10 * floor(year / 10))
  d <-
    my_get_acs(
      variables = "B25002_003",
      summary_var = "B25002_001",
      year = year
    ) |>
    suppressMessages() |>
    transmute(
      zcta = GEOID,
      zcta_vintage = zcta_vintage,
      fraction_vacant = estimate / summary_est,
      fraction_vacant_moe = moe_prop(estimate, summary_est, moe, summary_moe)
    )
  left_join(
    zctas_needed,
    d,
    by = c("zcta", "zcta_vintage")
  ) |>
    filter(.data$zcta_vintage == .env$zcta_vintage) |>
    mutate(year = year)
}

d_acs$acs_vacant <- mappp_dfr(2013:2023, get_acs_vacant)

get_acs_fraction_nhl <- function(year) {
  zcta_vintage <- as.character(10 * floor(year / 10))
  d <-
    my_get_acs(
      variables = "B03002_002",
      summary_var = "B03002_001",
      year = year
    ) |>
    suppressMessages() |>
    transmute(
      zcta = GEOID,
      zcta_vintage = zcta_vintage,
      fraction_nhl = estimate / summary_est,
      fraction_nhl_moe = moe_prop(estimate, summary_est, moe, summary_moe)
    )
  left_join(
    zctas_needed,
    d,
    by = c("zcta", "zcta_vintage")
  ) |>
    filter(.data$zcta_vintage == .env$zcta_vintage) |>
    mutate(year = year)
}

d_acs$acs_fraction_nhl <- mappp_dfr(2013:2023, get_acs_fraction_nhl)

get_acs_fraction_nhl_w <- function(year) {
  zcta_vintage <- as.character(10 * floor(year / 10))
  d <-
    my_get_acs(
      variables = "B03002_003",
      summary_var = "B03002_001",
      year = year
    ) |>
    suppressMessages() |>
    transmute(
      zcta = GEOID,
      zcta_vintage = zcta_vintage,
      fraction_nhl_w = estimate / summary_est,
      fraction_nhl_w_moe = moe_prop(estimate, summary_est, moe, summary_moe)
    )
  left_join(
    zctas_needed,
    d,
    by = c("zcta", "zcta_vintage")
  ) |>
    filter(.data$zcta_vintage == .env$zcta_vintage) |>
    mutate(year = year)
}

d_acs$acs_fraction_nhl_w <- mappp_dfr(2013:2023, get_acs_fraction_nhl_w)

get_acs_fraction_nhl_b <- function(year) {
  zcta_vintage <- as.character(10 * floor(year / 10))
  d <-
    my_get_acs(
      variables = "B03002_004",
      summary_var = "B03002_001",
      year = year
    ) |>
    suppressMessages() |>
    transmute(
      zcta = GEOID,
      zcta_vintage = zcta_vintage,
      fraction_nhl_b = estimate / summary_est,
      fraction_nhl_b_moe = moe_prop(estimate, summary_est, moe, summary_moe)
    )
  left_join(
    zctas_needed,
    d,
    by = c("zcta", "zcta_vintage")
  ) |>
    filter(.data$zcta_vintage == .env$zcta_vintage) |>
    mutate(year = year)
}

d_acs$acs_fraction_nhl_b <- mappp_dfr(2013:2023, get_acs_fraction_nhl_b)

get_acs_fraction_nhl_o <- function(year) {
  zcta_vintage <- as.character(10 * floor(year / 10))
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
      zcta = GEOID,
      zcta_vintage = zcta_vintage,
      fraction_nhl_o = n_nhl_o / n_total,
      fraction_nhl_o_moe = moe_prop(n_nhl_o, n_total, n_nhl_o_moe, n_total_moe)
    )
  left_join(
    zctas_needed,
    d,
    by = c("zcta", "zcta_vintage")
  ) |>
    filter(.data$zcta_vintage == .env$zcta_vintage) |>
    mutate(year = year)
}

d_acs$acs_fraction_nhl_o <- mappp_dfr(2013:2023, get_acs_fraction_nhl_o)

get_acs_fraction_hl <- function(year) {
  zcta_vintage <- as.character(10 * floor(year / 10))
  d <-
    my_get_acs(
      variables = "B03002_012",
      summary_var = "B03002_001",
      year = year
    ) |>
    suppressMessages() |>
    transmute(
      zcta = GEOID,
      zcta_vintage = zcta_vintage,
      fraction_hl = estimate / summary_est,
      fraction_hl_moe = moe_prop(estimate, summary_est, moe, summary_moe)
    )
  left_join(
    zctas_needed,
    d,
    by = c("zcta", "zcta_vintage")
  ) |>
    filter(.data$zcta_vintage == .env$zcta_vintage) |>
    mutate(year = year)
}

d_acs$acs_fraction_hl <- mappp_dfr(2013:2023, get_acs_fraction_hl)

get_acs_fraction_hl_w <- function(year) {
  zcta_vintage <- as.character(10 * floor(year / 10))
  d <-
    my_get_acs(
      variables = "B03002_013",
      summary_var = "B03002_001",
      year = year
    ) |>
    suppressMessages() |>
    transmute(
      zcta = GEOID,
      zcta_vintage = zcta_vintage,
      fraction_hl_w = estimate / summary_est,
      fraction_hl_w_moe = moe_prop(estimate, summary_est, moe, summary_moe)
    )
  left_join(
    zctas_needed,
    d,
    by = c("zcta", "zcta_vintage")
  ) |>
    filter(.data$zcta_vintage == .env$zcta_vintage) |>
    mutate(year = year)
}

d_acs$acs_fraction_hl_w <- mappp_dfr(2013:2023, get_acs_fraction_hl_w)

get_acs_fraction_hl_b <- function(year) {
  zcta_vintage <- as.character(10 * floor(year / 10))
  d <-
    my_get_acs(
      variables = "B03002_014",
      summary_var = "B03002_001",
      year = year
    ) |>
    suppressMessages() |>
    transmute(
      zcta = GEOID,
      zcta_vintage = zcta_vintage,
      fraction_hl_b = estimate / summary_est,
      fraction_hl_b_moe = moe_prop(estimate, summary_est, moe, summary_moe)
    )
  left_join(
    zctas_needed,
    d,
    by = c("zcta", "zcta_vintage")
  ) |>
    filter(.data$zcta_vintage == .env$zcta_vintage) |>
    mutate(year = year)
}

d_acs$acs_fraction_hl_b <- mappp_dfr(2013:2023, get_acs_fraction_hl_b)

get_acs_fraction_hl_o <- function(year) {
  zcta_vintage <- as.character(10 * floor(year / 10))
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
      zcta = GEOID,
      zcta_vintage = zcta_vintage,
      fraction_hl_o = n_hl_o / n_total,
      fraction_hl_o_moe = moe_prop(n_hl_o, n_total, n_hl_o_moe, n_total_moe)
    )
  left_join(
    zctas_needed,
    d,
    by = c("zcta", "zcta_vintage")
  ) |>
    filter(.data$zcta_vintage == .env$zcta_vintage) |>
    mutate(year = year)
}

d_acs$acs_fraction_hl_o <- mappp_dfr(2013:2023, get_acs_fraction_hl_o)

get_acs_lesh <- function(year) {
  zcta_vintage <- as.character(10 * floor(year / 10))
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
      zcta = GEOID,
      zcta_vintage = zcta_vintage,
      fraction_lesh = n_lesh / n_total,
      fraction_lesh_moe = moe_prop(n_lesh, n_total, n_lesh_moe, n_total_moe)
    )
  left_join(
    zctas_needed,
    d,
    by = c("zcta", "zcta_vintage")
  ) |>
    filter(.data$zcta_vintage == .env$zcta_vintage) |>
    mutate(year = year)
}

d_acs$acs_leshh <- mappp_dfr(2016:2023, get_acs_lesh)

get_acs_income <- function(year) {
  zcta_vintage <- as.character(10 * floor(year / 10))
  d <-
    my_get_acs(
      variables = "B19013_001",
      year = year
    ) |>
    suppressMessages() |>
    transmute(
      zcta = GEOID,
      zcta_vintage = zcta_vintage,
      median_income = estimate,
      median_income_moe = moe
    )
  left_join(
    zctas_needed,
    d,
    by = c("zcta", "zcta_vintage")
  ) |>
    filter(.data$zcta_vintage == .env$zcta_vintage) |>
    mutate(year = year)
}

d_acs$acs_income <-
  mappp_dfr(2013:2023, get_acs_income) |>
  left_join(cpi, by = "year") |>
  mutate(median_income_2010adj = median_income * ratio) |>
  select(-annual_cpi, -annual_cpi_2010, -ratio)

get_acs_hs <- function(year) {
  zcta_vintage <- as.character(10 * floor(year / 10))
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
      zcta = GEOID,
      zcta_vintage = zcta_vintage,
      fraction_hs = n_hs / n_total,
      fraction_hs_moe = moe_prop(n_hs, n_total, n_hs_moe, n_total_moe)
    )
  left_join(
    zctas_needed,
    d,
    by = c("zcta", "zcta_vintage")
  ) |>
    filter(.data$zcta_vintage == .env$zcta_vintage) |>
    mutate(year = year)
}

d_acs$acs_hs <- mappp_dfr(2013:2023, get_acs_hs)

d <- purrr::reduce(
  d_acs,
  left_join,
  by = c("zcta", "zcta_vintage", "year")
) |>
  relocate(year, .after = zcta_vintage) |>
  mutate(across(starts_with("fraction_"), \(.) round(., 3))) |>
  mutate(across(starts_with("n_"), as.integer)) |>
  mutate(across(starts_with("median_"), \(.) signif(., 3)))

d_dpkg <-
  d |>
  dpkg::as_dpkg(
    name = "hh_acs_measures_zcta",
    title = "Harmonized Historical American Community Survey Measures (ZCTA)",
    version = "1.3.0",
    homepage = "https://geomarker.io/hh_acs_measures",
    description = paste(
      "# Harmonized Historical ACS Measures \n",
      "ZCTA-level measures derived from the American Community Survey (ACS) generally available annually from 2013 through 2023."
    )
  )

# dpkg::write_dpkg(d_dpkg, "data")

dpkg::dpkg_gh_release(d_dpkg, draft = TRUE)
