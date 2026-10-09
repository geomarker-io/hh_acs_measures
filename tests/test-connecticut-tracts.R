# Run from the repository root: Rscript tests/test-connecticut-tracts.R
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidycensus))
source("R/harmonize_ct_tracts.R")

crosswalk <- ct_tract_crosswalk
stopifnot(nrow(crosswalk) == 879L,
          !anyDuplicated(crosswalk$geoid_2022),
          !anyDuplicated(crosswalk$geoid_2020),
          all(grepl("^09[0-9]{9}$", crosswalk$geoid_2022)),
          all(grepl("^09[0-9]{9}$", crosswalk$geoid_2020)))

# Exercise every mapping with repeated variable rows, a different state, and
# the water-only tracts that are intentionally outside the output scaffold.
water <- c("09120990000", "09190990000", "09170990000",
           "09130990100", "09180990100")
raw <- tibble(GEOID = c(rep(crosswalk$geoid_2022, each = 2), "01001020100", water),
              variable = rep(c("estimate1", "estimate2"), length.out = 1764),
              estimate = seq_len(1764))
for (year in c(2022, 2023)) {
  result <- harmonize_ct_tracts(raw, year)
  stopifnot(identical(result$GEOID,
                     c(rep(crosswalk$geoid_2020, each = 2), "01001020100", water)),
            identical(result$estimate, raw$estimate),
            identical(result$variable, raw$variable))
}
stopifnot(identical(harmonize_ct_tracts(raw, 2021), raw))
stopifnot(inherits(tryCatch(
  harmonize_ct_tracts(tibble(GEOID = "09110999999"), 2022),
  error = identity), "error"))

# Exercise the actual ACS wrapper and downstream poverty join for every pair.
env <- new.env()
definitions <- Filter(function(e) {
  is.call(e) && identical(e[[1]], as.name("<-")) &&
    is.symbol(e[[2]]) && as.character(e[[2]]) %in% c("my_get_acs", "get_acs_poverty")
}, parse("make_hh_acs_data.R"))
stopifnot(length(definitions) == 2L)
for (definition in definitions) eval(definition, env)
env$get_acs_tracts <- function(..., year) {
  tibble(GEOID = c(crosswalk$geoid_2022, "01001020100"),
         estimate = 25, summary_est = 100, moe = 5, summary_moe = 10)
}
env$tracts_needed <- tibble(
  census_tract_id = c(crosswalk$geoid_2020, "01001020100"),
  census_tract_vintage = "2020"
)
for (year in c(2022, 2023)) {
  result <- env$get_acs_poverty(year)
  stopifnot(nrow(result) == 880L, !anyDuplicated(result$census_tract_id),
            all(result$fraction_poverty == 0.25),
            all(result$fraction_poverty_moe == moe_prop(25, 100, 5, 10)),
            all(result$year == year))
}
cat("All 879 Connecticut mappings and downstream estimates/MOEs pass.\n")
