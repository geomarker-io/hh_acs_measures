# Run from the repository root: Rscript tests/test-sources.R
source('R/catalog.R')
source('R/summary_files.R')
source('R/sequence_files.R')
source('R/sources.R')
source('tests/helpers.R')

run_source_tests <- function() {
  directory <- tempfile('hh-acs-source-tests-')
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  catalog <- read_catalog()
  catalog$measures <- catalog$measures[c('n_pop', 'n_household')]
  catalog$geographies$tract$years <- c(2020L, 2022L)
  catalog$geographies$tract$state_fips <- c('01', '11')
  lookup_path <- write_lookup_fixture(file.path(directory, 'lookup.csv'))
  calls <- list()
  download <- function(url, subdir, offline = FALSE) {
    calls[[length(calls) + 1L]] <<- list(
      url = url,
      subdir = subdir,
      offline = offline
    )
    lookup_path
  }
  sources <- summary_source_files(
    catalog,
    'tract',
    offline = TRUE,
    download = download
  )
  historical <- sources[sources$year == 2020L, ]
  modern <- sources[sources$year >= 2021L, ]
  stopifnot(
    length(calls) == 1L,
    identical(
      calls[[1]],
      list(
        url = sequence_lookup_url(2020L),
        subdir = 'summary-files/2020/lookup',
        offline = TRUE
      )
    ),
    nrow(historical) == 5L,
    nrow(modern) == 4L,
    !anyDuplicated(sources$url),
    sum(historical$type == 'lookup') == 1L,
    sum(historical$type == 'geography') == 2L,
    sum(historical$type == 'sequence') == 2L,
    all(historical$sequence[historical$type == 'sequence'] == 1L),
    all(historical$table[historical$type == 'sequence'] == 'B01001,B11005'),
    any(grepl(
      '/DistrictOfColumbia/Tracts_Block_Groups_Only/20205dc0001000.zip',
      historical$url,
      fixed = TRUE
    )),
    setequal(
      modern$url,
      c(
        summary_file_url(2021L, 'B01001'),
        summary_file_url(2021L, 'B11005'),
        summary_file_url(2022L, 'B01001'),
        summary_file_url(2022L, 'B11005')
      )
    )
  )
  # Pure historical enumeration takes a parsed lookup and never fetches files.
  lookup <- read_sequence_lookup(lookup_path)
  stopifnot(isTRUE(all.equal(
    sequence_source_files(catalog, 'tract', 2020L, lookup),
    historical,
    check.attributes = FALSE
  )))
  expect_error(
    sequence_source_files(catalog, 'tract', 2020L, rbind(lookup, lookup[1, ])),
    'Ambiguous variables'
  )
  expect_error(
    sequence_source_files(catalog, 'tract', 2020L, lookup[-1, ]),
    'Missing variables'
  )
  expect_error(
    summary_source_files(catalog, 'tract', download = function(...) {
      stop('Lookup unavailable')
    }),
    'Lookup unavailable'
  )
  stopifnot(
    grepl(
      '/5_year/user_tools/Sequence_Number_and_Table_Number_Lookup.txt',
      sequence_lookup_url(2011L),
      fixed = TRUE
    ),
    grepl(
      '/user_tools/ACS_5yr_Seq_Table_Number_Lookup.txt',
      sequence_lookup_url(2013L),
      fixed = TRUE
    )
  )
  expect_error(sequence_lookup_url(2021L), '2010 through 2020')
  expect_error(summary_file_url(2020L, 'B01001'), '2021 onward')
  expect_error(summary_file_url(2021L, 'invalid'), 'Invalid ACS table ID')

  # The historical reader shares one archive across tables and joins each file
  # using LOGRECNO rather than assuming estimate/MOE/geography row order.
  catalog$geographies$tract$state_fips <- '01'
  geo_path <- write_geography_fixture(
    file.path(directory, 'geography.csv'),
    c('01001020200', '01001020100'),
    c('0000002', '0000001')
  )
  archive <- write_archive_fixture(
    file.path(directory, 'archive'),
    c(
      'ACSSF,2020,al,000,0001,0000001,100,40',
      'ACSSF,2020,al,000,0001,0000002,200,80'
    ),
    c(
      'ACSSF,2020,al,000,0001,0000002,5,6',
      'ACSSF,2020,al,000,0001,0000001,3,4'
    )
  )
  calls <- list()
  download <- function(url, subdir, offline = FALSE) {
    calls[[length(calls) + 1L]] <<- list(
      url = url,
      subdir = subdir,
      offline = offline
    )
    if (url == sequence_lookup_url(2020L)) {
      lookup_path
    } else if (grepl('\\.zip$', url)) {
      archive
    } else {
      geo_path
    }
  }
  getter <- summary_file_getter(
    catalog,
    'tract',
    offline = TRUE,
    download = download
  )
  population <- getter('B01001_001', 2020L)
  households <- getter('B11005_001', 2020L)
  again <- getter('B01001_001', 2020L, summary_var = 'B01001_001')
  stopifnot(
    length(calls) == 3L,
    all(vapply(calls, function(x) x$offline, logical(1))),
    identical(population$GEOID, c('01001020200', '01001020100')),
    identical(population$estimate, c(200, 100)),
    identical(population$moe, c(5, 3)),
    identical(households$estimate, c(80, 40)),
    identical(households$moe, c(6, 4)),
    identical(again$summary_est, population$estimate),
    identical(again$summary_moe, population$moe)
  )

  # Modern tables are parsed/downloaded once per table/year; the returned long
  # rows and denominator columns retain the declared getter contract.
  pop_paths <- file.path(directory, c('pop2021.dat', 'pop2022.dat'))
  for (i in seq_along(pop_paths)) {
    writeLines(
      c(
        'GEO_ID|B01001_E001|B01001_M001',
        paste0('1400000US01001020100|', i * 100, '|3')
      ),
      pop_paths[i]
    )
  }
  hh_path <- file.path(directory, 'households.dat')
  writeLines(
    c('GEO_ID|B11005_E001|B11005_M001', '1400000US01001020100|40|4'),
    hh_path
  )
  calls <- list()
  download <- function(url, subdir, offline = FALSE) {
    calls[[length(calls) + 1L]] <<- list(
      url = url,
      subdir = subdir,
      offline = offline
    )
    if (grepl('b11005.dat$', url)) {
      hh_path
    } else if (grepl('/2021/', url, fixed = TRUE)) {
      pop_paths[1]
    } else {
      pop_paths[2]
    }
  }
  getter <- summary_file_getter(
    catalog,
    'tract',
    offline = TRUE,
    download = download
  )
  population <- getter('B01001_001', 2021L)
  combined <- getter(c('B01001_001', 'B11005_001'), 2021L)
  denominator <- getter('B01001_001', 2021L, summary_var = 'B01001_001')
  later <- getter('B01001_001', 2022L)
  stopifnot(
    length(calls) == 3L,
    all(vapply(calls, function(x) x$offline, logical(1))),
    identical(population$estimate, 100),
    identical(later$estimate, 200),
    identical(combined$variable, c('B01001_001', 'B11005_001')),
    identical(combined$estimate, c(100, 40)),
    identical(denominator$summary_est, 100),
    identical(denominator$summary_moe, 3)
  )
  expect_error(getter('B01001_001', 2023L), 'Year outside catalog coverage')
  cat(
    'Source tests passed: mixed-format inventories, shared-sequence enumeration, offline propagation and per-table/year reader caching.\n'
  )
}

run_source_tests()
