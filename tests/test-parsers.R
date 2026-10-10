# Run from the repository root: Rscript tests/test-parsers.R
source('R/catalog.R')
source('R/summary_files.R')
source('R/sequence_files.R')
source('tests/helpers.R')

run_parser_tests <- function() {
  directory <- tempfile('hh-acs-parser-tests-')
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)

  # Every documented sentinel is missing; ordinary negative values remain data.
  missing <- c('.', 'NA', 'N', 'null', 'NULL', '-', '(X)', '**', '***', '')
  sentinels <- as.character(-111111111 * seq_len(9L))
  stopifnot(
    all(is.na(acs_numeric(missing))),
    all(is.na(acs_numeric(sentinels))),
    identical(acs_numeric(c(' 1.25 ', '-42', '1e3')), c(1.25, -42, 1000)),
    identical(acs_numeric(character()), numeric())
  )
  expect_error(
    acs_numeric(c('12', 'oops', 'oops')),
    'Unrecognized ACS value: oops'
  )

  # Requested column order is independent of file order. Ignore other levels,
  # malformed IDs and unselected states without losing leading zeroes.
  table_path <- file.path(directory, 'table.dat')
  writeLines(
    c(
      'GEO_ID|B01001_M002|B01001_E001|B01001_E002|B01001_M001|UNUSED',
      '1400000US01001020100|2|100|50|3|ignored',
      '1400000US02001020100|4|200|80|5|ignored',
      '0500000US01001|4|200|80|5|ignored',
      '1400000US0100102010|4|200|80|5|ignored',
      '1400000USabcdefghijk|4|200|80|5|ignored'
    ),
    table_path
  )
  d <- read_summary_table(
    table_path,
    c('B01001_002', 'B01001_001'),
    'tract',
    '01'
  )
  stopifnot(
    identical(
      names(d),
      c('GEOID', 'B01001_E002', 'B01001_E001', 'B01001_M002', 'B01001_M001')
    ),
    identical(d$GEOID, '01001020100'),
    d$B01001_E002 == 50,
    d$B01001_E001 == 100,
    d$B01001_M002 == 2,
    d$B01001_M001 == 3
  )
  expect_error(
    read_summary_table(table_path, 'B01001_003', 'tract'),
    'Missing Summary File columns'
  )
  expect_error(
    read_summary_table(table_path, 'B01001_001', 'tract', '04'),
    'No tract records'
  )
  expect_error(
    read_summary_table(table_path, 'B01001_001', 'county'),
    'Unknown geography'
  )
  writeLines(
    c(
      'GEO_ID|B01001_E001|B01001_M001',
      '1400000US01001020100|100|3',
      '1400000US01001020100|200|4'
    ),
    table_path
  )
  expect_error(
    read_summary_table(table_path, 'B01001_001', 'tract'),
    'Duplicate geography'
  )

  # The 2011 headers have the same positional meanings as later lookup files.
  lookup_path <- file.path(directory, 'lookup.csv')
  write_lookup_fixture(lookup_path)
  expected <- read_sequence_lookup(lookup_path)
  write_lookup_fixture(lookup_path, alternate_headers = TRUE)
  stopifnot(identical(read_sequence_lookup(lookup_path), expected))
  stopifnot(
    identical(expected$variable, c('B01001_001', 'B11005_001')),
    identical(expected$position, c(7, 8))
  )
  # A continuation starts at line 3, while noninteger subheadings have no data.
  writeLines(
    c(
      'File,Table,Sequence,Line,Start,Cells,Total,Label,Subject',
      'ACSSF,B01001,2,,10,2,2,Continuation,Population',
      'ACSSF,B01001,2,2.5,,,,Subheading,Population',
      'ACSSF,B01001,2,3,,,,First cell,Population',
      'ACSSF,B01001,2,4,,,,Second cell,Population'
    ),
    lookup_path
  )
  continued <- read_sequence_lookup(lookup_path)
  stopifnot(
    identical(continued$variable, c('B01001_003', 'B01001_004')),
    identical(continued$position, c(10, 11)),
    all(continued$sequence == 2L)
  )
  writeLines(c('A,B', '1,2'), lookup_path)
  expect_error(
    read_sequence_lookup(lookup_path),
    'Unexpected sequence lookup layout'
  )
  for (offset in c('', '6')) {
    writeLines(
      c(
        'File,Table,Sequence,Line,Start,Cells,Total,Label,Subject',
        paste0('ACSSF,B01001,1,,', offset, ',1,1,Table,Population'),
        'ACSSF,B01001,1,1,,,,Total,Population'
      ),
      lookup_path
    )
    expect_error(
      read_sequence_lookup(lookup_path),
      'Invalid sequence table offset'
    )
  }

  geo_path <- file.path(directory, 'geography.csv')
  write_geography_fixture(
    geo_path,
    c('01001020100', '01001020200', '01001020300'),
    c('0000001', '0000002', '0000003'),
    levels = c('140', '150', '140'),
    components = c('00', '00', '01')
  )
  geo <- read_sequence_geography(geo_path, 'tract')
  stopifnot(
    identical(geo$LOGRECNO, '0000001'),
    identical(geo$GEOID, '01001020100')
  )
  for (ids in list(c('01001020100', '01001020100'), '0100102010')) {
    write_geography_fixture(geo_path, ids, sprintf('%07d', seq_along(ids)))
    expect_error(
      read_sequence_geography(geo_path, 'tract'),
      'Invalid historical geography'
    )
  }
  write_geography_fixture(
    geo_path,
    c('01001020100', '01001020200'),
    c('0000001', '0000001')
  )
  expect_error(
    read_sequence_geography(geo_path, 'tract'),
    'Invalid historical geography'
  )

  mapping <- data.frame(
    variable = c('B01001_001', 'B11005_001'),
    sequence = 1L,
    position = c(7L, 8L)
  )
  geo <- data.frame(
    LOGRECNO = c('0000002', '0000001'),
    GEOID = c('01001020200', '01001020100')
  )
  estimates <- c(
    'ACSSF,2020,al,000,0001,0000001,100,40',
    'ACSSF,2020,al,000,0001,0000002,200,80'
  )
  moes <- c(
    'ACSSF,2020,al,000,0001,0000002,5,6',
    'ACSSF,2020,al,000,0001,0000001,3,4'
  )
  archive <- write_archive_fixture(
    file.path(directory, 'valid'),
    estimates,
    moes
  )
  d <- read_sequence_archive(
    archive,
    mapping,
    geo,
    2020L,
    data.frame(abbreviation = 'al')
  )
  stopifnot(
    identical(d$B11005_E001, c(80, 40)),
    identical(d$B01001_M001, c(5, 3))
  )
  cases <- list(
    missing_moe = list(estimates, NULL, 'Missing estimate/MOE sequence file'),
    duplicate_key = list(rep(estimates[1], 2), moes, 'LOGRECNO keys'),
    wrong_sequence = list(
      sub(',0001,', ',0002,', estimates),
      moes,
      'Unexpected sequence number'
    ),
    missing_geography = list(estimates[1], moes, 'Geography records missing')
  )
  for (name in names(cases)) {
    case <- cases[[name]]
    archive <- write_archive_fixture(
      file.path(directory, name),
      case[[1]],
      case[[2]]
    )
    expect_error(
      read_sequence_archive(
        archive,
        mapping,
        geo,
        2020L,
        data.frame(abbreviation = 'al')
      ),
      case[[3]]
    )
  }
  cat(
    'Parser tests passed: annotations, table columns/filtering, lookup layouts/offsets, geography keys and sequence archive failures.\n'
  )
}

run_parser_tests()
