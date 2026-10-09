# Run from the repository root: Rscript tests/test-inflation-moes.R
# Evaluate the production inflation-adjustment expressions using offline data.
suppressPackageStartupMessages(library(dplyr))

cpi <- read.csv("CPI_2010-2023.csv") |>
  mutate(annual_cpi_2010 = annual_cpi[year == 2010],
         ratio = annual_cpi_2010 / annual_cpi)

for (file in c("make_hh_acs_data.R", "make_hh_acs_data_zcta.R")) {
  env <- new.env()
  env$cpi <- cpi
  env$d_acs <- list()
  # Supply source estimates instead of invoking a download/cache/release.
  env$mappp_dfr <- function(.x, .f) .f(.x)
  env$get_acs_home_value <- function(year) {
    tibble(year = year, median_home_value = ifelse(year == 2015, NA_real_, 200000),
           median_home_value_moe = ifelse(year == 2016, NA_real_, 12345))
  }
  env$get_acs_income <- function(year) {
    tibble(year = year, median_income = ifelse(year == 2015, NA_real_, 75000),
           median_income_moe = ifelse(year == 2016, NA_real_, 6789))
  }
  definitions <- Filter(function(e) {
    is.call(e) && identical(e[[1]], as.name("<-")) &&
      is.call(e[[2]]) && identical(e[[2]][[1]], as.name("$")) &&
      identical(e[[2]][[2]], as.name("d_acs")) &&
      as.character(e[[2]][[3]]) %in% c("acs_home_value", "acs_income")
  }, parse(file))
  stopifnot(length(definitions) == 2L)
  for (definition in definitions) eval(definition, env)

  for (measure in c("home_value", "income")) {
    result <- env$d_acs[[paste0("acs_", measure)]]
    field <- paste0("median_", measure)
    moe <- paste0(field, "_moe")
    adjusted <- paste0(field, "_2010adj")
    adjusted_moe <- paste0(adjusted, "_moe")
    original <- env[[paste0("get_acs_", measure)]](result$year)
    ratio <- cpi$ratio[match(result$year, cpi$year)]
    stopifnot(identical(result[[field]], original[[field]]),
              identical(result[[moe]], original[[moe]]),
              identical(is.na(result[[adjusted]]), is.na(original[[field]])),
              identical(is.na(result[[adjusted_moe]]), is.na(original[[moe]])),
              isTRUE(all.equal(result[[adjusted]], original[[field]] * ratio)),
              isTRUE(all.equal(result[[adjusted_moe]], original[[moe]] * ratio)),
              !any(c("ratio", "annual_cpi", "annual_cpi_2010") %in% names(result)))
    # Positive scaling should preserve the relative uncertainty and scale both
    # endpoints of the original estimate +/- MOE interval consistently.
    stopifnot(isTRUE(all.equal(result[[adjusted_moe]] / result[[adjusted]],
                              original[[moe]] / original[[field]])),
              isTRUE(all.equal(result[[adjusted]] - result[[adjusted_moe]],
                              (original[[field]] - original[[moe]]) * ratio)),
              isTRUE(all.equal(result[[adjusted]] + result[[adjusted_moe]],
                              (original[[field]] + original[[moe]]) * ratio)))
  }
}
cat("Income/home-value MOEs, interval scaling, and missingness pass for both scripts.\n")
