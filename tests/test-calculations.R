# Run from the repository root: Rscript tests/test-calculations.R
source('R/catalog.R')
source('R/moe.R')
source('R/calculations.R')
source('tests/helpers.R')

run_calculation_tests <- function() {
  catalog <- read_catalog()
  variables <- c('B01001_001', 'B01001_002', 'B01001_003')
  raw <- data.frame(
    GEOID = rep(c('01001020100', '01001020200', '01001020300'), each = 3L),
    variable = rep(variables, 3L),
    estimate = c(10, 0, 0, 0, 0, 0, 10, NA, 2),
    moe = c(3, 4, 5, 2, 4, 3, 1, 2, 3)
  )
  get_acs <- function(variables, year, summary_var = NULL) raw
  rule <- list(variables = variables, denominator = 'B01001_004')
  # Zero-count terms contribute only their largest MOE, not a sum of squares.
  d <- calculate_acs_measure(list(calculation = 'count'), rule, 2024L, get_acs)
  stopifnot(
    identical(d$estimate, c(10, 0, NA_real_)),
    isTRUE(all.equal(d$moe, c(sqrt(34), 4, sqrt(14))))
  )
  raw$moe[3] <- NA_real_
  stopifnot(is.na(calculate_acs_measure(
    list(calculation = 'count'),
    rule,
    2024L,
    get_acs
  )$moe[1]))
  raw$moe[3] <- 5

  # A source can be present but suppressed; a missing component row is an error.
  original <- raw
  raw <- original[-1, ]
  expect_error(
    calculate_acs_measure(list(calculation = 'count'), rule, 2024L, get_acs),
    'Missing or unexpected ACS components'
  )
  raw <- rbind(original, original[1, ])
  expect_error(
    calculate_acs_measure(list(calculation = 'count'), rule, 2024L, get_acs),
    'Duplicate ACS variable rows'
  )
  raw <- original
  raw$variable[1] <- 'B01001_099'
  expect_error(
    calculate_acs_measure(list(calculation = 'count'), rule, 2024L, get_acs),
    'Missing or unexpected ACS components'
  )

  raw <- original
  raw$summary_est <- rep(c(100, 0, -1), each = 3L)
  raw$summary_moe <- 2
  d <- calculate_acs_measure(
    list(calculation = 'proportion'),
    rule,
    2024L,
    get_acs
  )
  stopifnot(
    d$estimate[1] == 0.1,
    isTRUE(all.equal(d$moe[1], sqrt(34 - 0.04) / 100)),
    all(is.na(d$estimate[-1])),
    all(is.na(d$moe[-1]))
  )
  raw$summary_est[2] <- 101
  expect_error(
    calculate_acs_measure(
      list(calculation = 'proportion'),
      rule,
      2024L,
      get_acs
    ),
    'Inconsistent ACS denominator'
  )
  raw$summary_est[1:3] <- NA_real_
  d <- calculate_acs_measure(
    list(calculation = 'proportion'),
    rule,
    2024L,
    get_acs
  )
  stopifnot(is.na(d$estimate[1]), is.na(d$moe[1]))

  raw <- original[c(1, 4, 7), ]
  raw$estimate <- c(12345, NA, 0)
  raw$moe <- c(12.25, NA, 3)
  d <- calculate_acs_measure(
    list(calculation = 'estimate'),
    list(variables = variables[1]),
    2024L,
    get_acs
  )
  stopifnot(identical(d, raw[c('GEOID', 'estimate', 'moe')]))
  cpi <- data.frame(year = c(2010L, 2024L), annual_cpi = c(100, 250))
  adjusted <- adjust_for_inflation(d, cpi, 2024L, 2010L)
  stopifnot(
    isTRUE(all.equal(adjusted$estimate, c(4938, NA, 0))),
    isTRUE(all.equal(adjusted$moe, c(4.9, NA, 1.2))),
    identical(adjust_for_inflation(d, cpi, 2010L, 2010L), d)
  )
  for (bad_cpi in list(cpi[1, ], rbind(cpi, cpi[2, ]))) {
    expect_error(
      adjust_for_inflation(d, bad_cpi, 2024L, 2010L),
      'Missing or invalid CPI'
    )
  }
  for (column in 1:2) {
    for (value in c(0, -1, NA, Inf)) {
      bad_cpi <- cpi
      bad_cpi$annual_cpi[column] <- value
      expect_error(
        adjust_for_inflation(d, bad_cpi, 2024L, 2010L),
        'Missing or invalid CPI'
      )
    }
  }
  stopifnot(
    identical(
      round_measure(c(1.4, 1.6, NA), catalog$measures$n_pop),
      c(1L, 2L, NA_integer_)
    ),
    identical(
      round_measure(c(0.12345, NA), catalog$measures$fraction_poverty),
      c(0.123, NA_real_)
    ),
    identical(
      round_measure(c(12345, NA), catalog$measures$median_income),
      c(12300, NA_real_)
    )
  )

  # Different tables supply different tract sets. The annual result retains
  # their union and leaves unavailable or absent measures missing in every row.
  small <- catalog
  small$measures <- catalog$measures[c('fraction_hs', 'n_pop', 'n_household')]
  get_acs <- function(variables, year, summary_var = NULL) {
    ids <- if (variables == 'B01001_001') {
      c('01001020100', '01001020200')
    } else {
      c('01001020200', '01001020300')
    }
    data.frame(
      GEOID = ids,
      variable = variables,
      estimate = c(10.4, 20.6),
      moe = c(1.25, 2.75)
    )
  }
  d <- build_year_data(small, 'tract', 2010L, get_acs, cpi)
  d <- d[order(d$census_tract_id), ]
  stopifnot(
    identical(
      d$census_tract_id,
      c('01001020100', '01001020200', '01001020300')
    ),
    identical(d$n_pop, c(10L, 21L, NA_integer_)),
    identical(d$n_household, c(NA_integer_, 10L, 21L)),
    identical(d$n_pop_moe, c(1.25, 2.75, NA_real_)),
    all(is.na(d$fraction_hs)),
    all(is.na(d$fraction_hs_moe)),
    identical(names(d), measure_columns(small, 'tract'))
  )
  # Pin the actual housing rule transition, not only arbitrary year selection.
  housing <- catalog$measures$fraction_builtbf1970
  stopifnot(
    identical(
      unlist(
        select_rule(housing$rules, 2014L, 'housing')$variables,
        use.names = FALSE
      ),
      sprintf('B25034_%03d', 7:10)
    ),
    identical(
      unlist(
        select_rule(housing$rules, 2015L, 'housing')$variables,
        use.names = FALSE
      ),
      sprintf('B25034_%03d', 8:11)
    )
  )
  cat(
    'Calculation tests passed: multi-component counts/MOEs, missing inputs, denominators, inflation, rounding, yearly tract unions and housing-rule transition.\n'
  )
}

run_calculation_tests()
