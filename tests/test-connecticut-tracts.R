# Run from the repository root: Rscript tests/test-connecticut-tracts.R
suppressPackageStartupMessages(library(dplyr))
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

# Exercise the independent shared calculator after identifier harmonization.
source("R/build.R")
catalog <- read_catalog()
spec <- catalog$measures$fraction_poverty
rule <- select_rule(spec$rules, 2023, "fraction_poverty")
getter <- function(variables, year, summary_var) {
  raw <- tibble(GEOID = crosswalk$geoid_2022, variable = variables,
                estimate = 25, summary_est = 100, moe = 5, summary_moe = 10)
  harmonize_ct_tracts(raw, year)
}
result <- calculate_acs_measure(spec, rule, 2023, getter)
stopifnot(nrow(result) == 879L, !anyDuplicated(result$GEOID),
          setequal(result$GEOID, crosswalk$geoid_2020),
          all(result$estimate == 0.25),
          all(result$moe == sqrt(25 - 0.25^2 * 100) / 100))
cat("All 879 Connecticut mappings and downstream estimates/MOEs pass.\n")
