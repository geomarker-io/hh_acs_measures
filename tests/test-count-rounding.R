# Run from the repository root: Rscript tests/test-count-rounding.R
# Evaluate the production assembly/formatting expression without running ACS.
suppressPackageStartupMessages(library(dplyr))

for (file in c("make_hh_acs_data.R", "make_hh_acs_data_zcta.R")) {
  env <- new.env()
  zcta <- grepl("zcta", file)
  id <- if (zcta) "zcta" else "census_tract_id"
  vintage <- if (zcta) "zcta_vintage" else "census_tract_vintage"
  fixture <- tibble(year = rep(2023L, 6),
                    n_pop = c(100, 0, 12.4, 12.6, 12.5, NA_real_),
                    n_pop_moe = c(2.9, 0, 2.4, 2.6, 3.5, NA_real_))
  fixture[[id]] <- sprintf("%011d", seq_len(6))
  fixture[[vintage]] <- "2020"
  # Include a second measure to exercise the join and median/MOE formatting.
  medians <- fixture[c(id, vintage, "year")]
  medians$median_income_2010adj <- c(54123, 0, 123456, 12.345, 9.8765, NA_real_)
  medians$median_income_2010adj_moe <- c(1234.56, 0, 2345.67, 1.2345, 0.98765, NA_real_)
  env$d_acs <- list(counts = fixture, medians = medians)
  definition <- Filter(function(e) {
    is.call(e) && identical(e[[1]], as.name("<-")) &&
      identical(e[[2]], as.name("d"))
  }, parse(file))
  stopifnot(length(definition) == 1L)
  eval(definition[[1]], env)
  result <- env$d
  stopifnot(identical(result$n_pop, c(100L, 0L, 12L, 13L, 12L, NA_integer_)),
            identical(result$n_pop_moe, c(3L, 0L, 2L, 3L, 4L, NA_integer_)),
            identical(result[[id]], fixture[[id]]),
            identical(result[[vintage]], fixture[[vintage]]),
            identical(result$year, fixture$year),
            identical(result$median_income_2010adj,
                      c(54100, 0, 123000, 12.3, 9.88, NA_real_)),
            identical(result$median_income_2010adj_moe,
                      c(1230, 0, 2350, 1.23, 0.988, NA_real_)))
}
cat("Count/MOE rounding, integer types, missingness, and adjusted-median formatting pass.\n")
