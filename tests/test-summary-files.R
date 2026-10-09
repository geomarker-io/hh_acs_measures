source('R/build.R')
catalog <- read_catalog()
stopifnot(inherits(tryCatch(summary_file_url(2020, 'B19013'), error = identity), 'error'))
# Realistic input includes both supported summary levels, leading-zero IDs,
# other geographic components, excluded Puerto Rico tracts, and sentinel values.
file <- tempfile(fileext = '.dat')
writeLines(c('GEO_ID|B19013_E001|B19013_M001',
             '0100000US|70000|100', '1400000US01001020100|50000|1000',
             '1400000US72001020100|25000|500', '1400089US01001020100|12345|99',
             '860Z200US00601|15000|1200', '860Z200US01234|-666666666|-222222222'), file)
d <- read_summary_table(file, 'B19013_001', 'tract', catalog$geographies$tract$state_fips)
stopifnot(identical(d$GEOID, '01001020100'), d$B19013_E001 == 50000,
          d$B19013_M001 == 1000 * (1.96 / 1.645))
z <- read_summary_table(file, 'B19013_001', 'zcta')
stopifnot(identical(z$GEOID, c('00601', '01234')),
          is.na(z$B19013_E001[2]), is.na(z$B19013_M001[2]))
# The adapter reads the downloader's returned path and parses a table once.
calls <- 0L
download <- function(url, subdir, offline) {
  calls <<- calls + 1L
  stopifnot(endsWith(url, 'acsdt5y2021-b19013.dat'),
            subdir == 'summary-files/2021', offline)
  file
}
getter <- summary_file_getter(catalog, 'tract', offline = TRUE, download = download)
result <- getter('B19013_001', 2021)
again <- getter('B19013_001', 2021)
stopifnot(calls == 1L, identical(result, again), result$GEOID == '01001020100')
# Multiple components and denominator fields must retain missingness, scaling,
# and the previous denominator-MOE rounding.
writeLines(c('GEO_ID|B17001_E001|B17001_M001|B17001_E002|B17001_M002',
             '1400000US01001020100|100|10|25|5',
             '1400000US01001020200|0|2|0|1',
             '1400000US01001020300|-666666666|-222222222|10|3'), file)
getter <- summary_file_getter(catalog, 'tract', download = function(...) file)
raw <- getter('B17001_002', 2021, summary_var = 'B17001_001')
stopifnot(identical(raw$summary_est, c(100, 0, NA_real_)),
          raw$moe[1] == 5 * (1.96 / 1.645),
          raw$summary_moe[1] == round(10 * 1.96 / 1.645))
spec <- catalog$measures$fraction_poverty
result <- calculate_acs_measure(spec, spec$rules[[1]], 2021, getter)
stopifnot(result$estimate[1] == 0.25, all(is.na(result$estimate[2:3])))
# Missing columns and unrecognized values must not silently alter definitions.
stopifnot(inherits(tryCatch(read_summary_table(file, 'B17001_003', 'tract'), error = identity), 'error'),
          inherits(tryCatch(acs_numeric('unexpected'), error = identity), 'error'),
          all(is.na(acs_numeric(as.character(-111111111 * seq_len(9))))))
bad <- catalog
bad$geographies$tract$years[1] <- 2020L
stopifnot(inherits(tryCatch(validate_catalog(bad), error = identity), 'error'))
sources <- summary_source_files(catalog, 'tract')
stopifnot(!anyDuplicated(sources$url), all(sources$year >= 2021),
          identical(sources, summary_source_files(catalog, 'zcta')))
unlink(file)
cat('Summary File parsing, geography filters, stow-path adapter, caching, and missingness pass.\n')
