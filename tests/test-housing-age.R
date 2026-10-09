# Run from the repository root: Rscript tests/test-housing-age.R
# Evaluate only the calculation function; never download data or create a release.
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidycensus))

for (file in c("make_hh_acs_data.R", "make_hh_acs_data_zcta.R")) {
  env <- new.env()
  expressions <- parse(file)
  definition <- Filter(function(e) {
    is.call(e) && identical(e[[1]], as.name("<-")) &&
      identical(e[[2]], as.name("get_acs_yrbuilt"))
  }, expressions)
  stopifnot(length(definition) == 1L)
  eval(definition[[1]], env)

  for (year in 2010:2023) {
    # The physical housing stock is identical on both sides of the table change.
    bins <- if (year < 2015) 7:10 else 8:11
    estimates <- setNames(c(20, 30, 40, 50), sprintf("B25034_%03d", bins))
    margins <- setNames(c(2, 3, 4, 5), names(estimates))
    env$my_get_acs <- function(variables, summary_var, year) {
      stopifnot(identical(variables, names(estimates)),
                summary_var == "B25034_001")
      tibble(GEOID = "12345", estimate = unname(estimates[variables]),
             moe = unname(margins[variables]), summary_est = 200, summary_moe = 10)
    }
    vintage <- as.character(10 * floor(year / 10))
    env$tracts_needed <- tibble(census_tract_id = "12345", census_tract_vintage = vintage)
    env$zctas_needed <- tibble(zcta = "12345", zcta_vintage = vintage)
    result <- env$get_acs_yrbuilt(year)
    stopifnot(nrow(result) == 1L, result$year == year,
              result$fraction_builtbf1970 == 0.7,
              isTRUE(all.equal(result$fraction_builtbf1970_moe,
                moe_prop(140, 200, moe_sum(c(2, 3, 4, 5), c(20, 30, 40, 50)), 10))))
  }
}
cat("Housing-age estimates and MOEs pass for both scripts, 2010-2023.\n")
