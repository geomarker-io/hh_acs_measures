# Run from the repository root: Rscript tests/test-zcta-vintages.R
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidycensus))

env <- new.env()
definitions <- Filter(function(e) {
  is.call(e) && identical(e[[1]], as.name("<-")) &&
    is.symbol(e[[2]]) && startsWith(as.character(e[[2]]), "get_acs_")
}, parse("make_hh_acs_data_zcta.R"))
stopifnot(length(definitions) == 26L)
for (definition in definitions) eval(definition, env)

# A shared code, a retired code, and a newly introduced code make selecting the
# wrong scaffold observable even when the shared code happens to match.
env$zctas_needed <- tibble(zcta = c("12345", "23456", "12345", "34567"),
                          zcta_vintage = c("2010", "2010", "2020", "2020"))
env$my_get_acs <- function(variables, year, summary_var = NULL) {
  ids <- if (year <= 2020) c("12345", "23456") else c("12345", "34567")
  expand.grid(GEOID = ids, variable = variables, stringsAsFactors = FALSE) |>
    as_tibble() |>
    mutate(estimate = 5, moe = 1, summary_est = 200, summary_moe = 10)
}

for (year in c(2019, 2020, 2021, 2023)) {
  expected_ids <- if (year <= 2020) c("12345", "23456") else c("12345", "34567")
  expected_vintage <- if (year <= 2020) "2010" else "2020"
  for (definition in definitions) {
    name <- as.character(definition[[2]])
    result <- env[[name]](year)
    stopifnot(nrow(result) == 2L, identical(result$zcta, expected_ids),
              all(result$zcta_vintage == expected_vintage),
              all(result$year == year), !anyDuplicated(result[c("zcta", "zcta_vintage", "year")]),
              all(is.finite(as.matrix(select(result, where(is.numeric))))))
  }
}
cat("All 26 ZCTA measures pass vintage/scaffold checks for 2019, 2020, 2021, and 2023.\n")
