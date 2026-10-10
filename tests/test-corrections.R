# Run from the repository root: Rscript tests/test-corrections.R
source('R/catalog.R')
source('R/moe.R')
source('R/calculations.R')
source('R/summary_files.R')
source('R/sequence_files.R')

run_correction_tests <- function() {
  directory <- tempfile('hh-acs-correction-tests-')
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  rejected <- function(expr, message) {
    error <- tryCatch(force(expr), error = identity)
    stopifnot(
      inherits(error, 'error'),
      grepl(message, conditionMessage(error), fixed = TRUE)
    )
  }

  # The first case has zero subset variance; the next two have P = 1.
  # P = 1 uses the numerator MOE even when the denominator MOE is missing.
  actual <- acs_moe_proportion(
    c(50, 100, 100, 50, 50, 0, 5, NA, 100),
    c(100, 100, 100, 100, 100, 0, -10, 100, 100),
    c(5, 10, 12, 3, 8, 2, 2, 2, 10),
    c(10, 10, 10, 10, 10, 2, 2, 2, NA)
  )
  expected <- c(0, 0.1, 0.12, sqrt(34) / 100, sqrt(39) / 100, NA, NA, NA, 0.1)
  stopifnot(isTRUE(all.equal(actual, expected)))
  rejected(acs_moe_proportion(1:2, 10, 1, 1), 'equal lengths')

  tokens <- c(
    ' ***** ',
    '-555555555',
    '**',
    '***',
    '-222222222',
    '-333333333',
    '(X)',
    '-888888888',
    NA,
    '',
    '0',
    '12'
  )
  stopifnot(
    identical(acs_numeric(tokens, 'moe'), c(0, 0, rep(NA_real_, 8), 0, 12)),
    identical(acs_numeric(tokens, 'estimate'), c(rep(NA_real_, 10), 0, 12))
  )
  rejected(acs_numeric('unexpected', 'moe'), 'Unrecognized ACS value')

  # Exercise the table reader's MOE-column parsing, including leading-zero IDs.
  table_path <- file.path(directory, 'table.dat')
  writeLines(
    c(
      'GEO_ID|B01001_E001|B01001_M001',
      '1400000US01001020100|100|-555555555',
      '1400000US01001020200|50|-222222222',
      '1400000US02001020100|20|3',
      '0500000US01001|170|4'
    ),
    table_path
  )
  table <- read_summary_table(table_path, 'B01001_001', 'tract', '01')
  stopifnot(
    identical(table$GEOID, c('01001020100', '01001020200')),
    identical(table$B01001_E001, c(100, 50)),
    identical(table$B01001_M001, c(0, NA_real_))
  )

  # Exercise the historical ZIP reader with the same controlled/missing MOEs.
  estimate_path <- file.path(directory, 'e20205al0001000.txt')
  moe_path <- file.path(directory, 'm20205al0001000.txt')
  writeLines(
    c(
      'ACSSF,2020,al,000,0001,0000001,100',
      'ACSSF,2020,al,000,0001,0000002,50'
    ),
    estimate_path
  )
  writeLines(
    c(
      'ACSSF,2020,al,000,0001,0000001,-555555555',
      'ACSSF,2020,al,000,0001,0000002,-222222222'
    ),
    moe_path
  )
  archive <- file.path(directory, 'sequence.zip')
  utils::zip(archive, c(estimate_path, moe_path), flags = '-jq')
  mapping <- data.frame(variable = 'B01001_001', sequence = 1L, position = 7L)
  geo <- data.frame(
    LOGRECNO = c('0000002', '0000001'),
    GEOID = c('01001020200', '01001020100')
  )
  sequence <- read_sequence_archive(
    archive,
    mapping,
    geo,
    2020L,
    data.frame(abbreviation = 'al')
  )
  stopifnot(
    identical(sequence$GEOID, geo$GEOID),
    identical(sequence$B01001_E001, c(50, 100)),
    identical(sequence$B01001_M001, c(NA_real_, 0))
  )

  catalog <- read_catalog()
  for (name in c('fraction_poverty', 'median_income')) {
    changed <- catalog
    changed$measures[[name]]$rules[[1]]$years[2] <- 2023L
    rejected(validate_catalog(changed), 'final release year 2024')
  }
  changed <- catalog
  changed$geographies$tract$years[2] <- 2025L
  rejected(validate_catalog(changed), 'final release year 2025')
  for (field in c('id', 'resource', 'state_fips')) {
    changed <- catalog
    changed$geographies$tract[[field]] <- NULL
    rejected(validate_catalog(changed), field)
  }
  for (states in list(
    character(),
    c('01', '01'),
    '1',
    '99',
    1L,
    NA_character_
  )) {
    changed <- catalog
    changed$geographies$tract$state_fips <- states
    rejected(validate_catalog(changed), 'state_fips')
  }
  for (digits in list(NULL, NA_real_, Inf, -1, 0.5)) {
    changed <- catalog
    changed$measures$n_pop$rounding$digits <- digits
    rejected(validate_catalog(changed), 'invalid rounding')
  }
  changed <- catalog
  changed$measures$n_pop$rounding$method <- NULL
  rejected(validate_catalog(changed), 'invalid rounding')
  changed <- catalog
  changed$measures$median_income$rounding$digits <- 0L
  rejected(validate_catalog(changed), 'invalid rounding')
  changed <- catalog
  changed$measures$fraction_poverty$rules <- list(
    list(
      years = c(2010L, 2014L),
      variables = 'B17001_002',
      denominator = 'B17001_001'
    ),
    list(
      years = c(2014L, 2024L),
      variables = 'B17001_002',
      denominator = 'B17001_001'
    )
  )
  rejected(validate_catalog(changed), 'overlapping year rules')
  changed$measures$fraction_poverty$rules[[2]]$years[1] <- 2016L
  rejected(validate_catalog(changed), 'gaps between year rules')
  stopifnot(length(sequence_units(catalog, 'tract')$state) == 51L)

  # An annual build preserves catalog coverage and derives from unrounded inputs,
  # even when the adjusted measure appears before its source in the catalog.
  annual_catalog <- catalog
  annual_catalog$measures <- catalog$measures[c(
    'median_income_2010adj',
    'median_income',
    'fraction_hs'
  )]
  validate_catalog(annual_catalog)
  getter <- function(variables, year, summary_var = NULL) {
    d <- data.frame(
      GEOID = '01001020100',
      variable = variables,
      estimate = 12345,
      moe = 15
    )
    if (!is.null(summary_var)) {
      d$summary_est <- 200000
      d$summary_moe <- 20
    }
    d
  }
  cpi <- read.csv(catalog$dataset$inflation$file)
  original <- annual_catalog
  early <- build_year_data(annual_catalog, 'tract', 2010L, getter, cpi)
  later <- build_year_data(annual_catalog, 'tract', 2024L, getter, cpi)
  ratio <- cpi$annual_cpi[cpi$year == 2010L] / cpi$annual_cpi[cpi$year == 2024L]
  stopifnot(
    identical(annual_catalog, original),
    identical(names(later), measure_columns(annual_catalog, 'tract')),
    identical(
      vapply(early, typeof, character(1)),
      vapply(later, typeof, character(1))
    ),
    is.na(early$fraction_hs),
    is.na(early$fraction_hs_moe),
    later$median_income == 12300,
    later$median_income_2010adj == signif(12345 * ratio, 3),
    isTRUE(all.equal(later$median_income_2010adj_moe, 15 * ratio))
  )
  rejected(
    build_year_data(annual_catalog, 'tract', 2025L, getter, cpi),
    'within catalog coverage'
  )
  rejected(
    build_year_data(annual_catalog, 'tract', 2024.5, getter, cpi),
    'within catalog coverage'
  )
  cat(
    'Correction tests passed: MOE edge cases, controlled values in both readers, catalog validation and annual calculations.\n'
  )
}

run_correction_tests()
