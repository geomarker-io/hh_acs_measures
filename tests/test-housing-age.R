source('R/catalog.R')
source('R/moe.R')
source('R/calculations.R')
catalog <- read_catalog()
spec <- catalog$measures$fraction_builtbf1970
for (year in 2021:2023) {
  bins <- 8:11
  estimates <- setNames(c(20, 30, 40, 50), sprintf('B25034_%03d', bins))
  margins <- setNames(c(2, 3, 4, 5), names(estimates))
  get_acs <- function(variables, summary_var, year) {
    stopifnot(identical(variables, names(estimates)), summary_var == 'B25034_001')
    data.frame(GEOID = '12345', variable = variables,
               estimate = unname(estimates[variables]), moe = unname(margins[variables]),
               summary_est = 200, summary_moe = 10)
  }
  result <- calculate_acs_measure(spec, select_rule(spec$rules, year, 'housing age'), year, get_acs)
  stopifnot(result$estimate == 0.7,
            isTRUE(all.equal(result$moe, acs_moe_proportion(140, 200,
              acs_moe_sum(c(2, 3, 4, 5), c(20, 30, 40, 50)), 10))))
}
cat('Housing-age rules, estimates, and MOEs pass for 2021–2023.\n')
