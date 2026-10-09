source('R/catalog.R')
source('R/moe.R')
source('R/calculations.R')
catalog <- read_catalog()
stopifnot(identical(round_measure(c(100, 0, 12.4, 12.6, 12.5, NA_real_),
                                 catalog$measures$n_pop),
                    c(100L, 0L, 12L, 13L, 12L, NA_integer_)),
          identical(round_measure(c(2.9, 0, 2.4, 2.6, 3.5, NA_real_),
                                 catalog$measures$n_pop),
                    c(3L, 0L, 2L, 3L, 4L, NA_integer_)),
          identical(round_measure(c(54123, 0, 123456, 12.345, 9.8765, NA_real_),
                                 catalog$measures$median_income_2010adj),
                    c(54100, 0, 123000, 12.3, 9.88, NA_real_)))
cat('Count/MOE rounding, integer types, and adjusted-median formatting pass.\n')
