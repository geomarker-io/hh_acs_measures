source('R/moe.R')
# Independent hand-calculated results, including the zero-count exception.
stopifnot(acs_moe_factor(90) == 1, acs_moe_factor(95) == 1.96 / 1.645,
          acs_moe_sum(c(3, 4), c(20, 30)) == 5,
          acs_moe_sum(c(3, 4, 12), c(0, 0, 20)) == sqrt(160),
          acs_moe_sum(c(3, 4), c(0, 0)) == 4,
          is.na(acs_moe_sum(c(NA_real_, 4), c(0, 20))),
          acs_moe_proportion(25, 100, 5, 10) == sqrt(25 - 0.25^2 * 100) / 100,
          acs_moe_proportion(50, 100, 2, 10) == sqrt(4 + 0.5^2 * 100) / 100,
          acs_moe_proportion(50, 100, 5, 10) == sqrt(25 + 0.5^2 * 100) / 100,
          is.na(acs_moe_proportion(0, 0, 0, 0)),
          is.na(acs_moe_proportion(NA_real_, 100, 5, 10)))
cat('Independent ACS MOE sum/proportion formulas and confidence conversion pass.\n')
