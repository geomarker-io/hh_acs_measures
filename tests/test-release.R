# Run from the repository root: Rscript tests/test-release.R
source('R/catalog.R')
source('R/moe.R')
source('R/calculations.R')
source('R/summary_files.R')
source('R/sequence_files.R')
source('R/sources.R')
source('R/release.R')

run_release_tests <- function() {
  directory <- tempfile('hh-acs-release-tests-')
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  catalog <- read_catalog()
  stopifnot(identical(names(catalog$geographies), 'tract'))
  catalog$geographies$tract$years <- c(2021L, 2023L)
  sources <- summary_source_files(catalog, 'tract', download = function(...) {
    stop('Table-based inventories must not download files')
  })
  # The writer must use the supplied inventory, even when source discovery is
  # unavailable. This catches accidental file access while saving metadata.
  writer <- new.env(parent = globalenv())
  writer$summary_source_files <- writer$stow_source_file <- function(...) {
    stop('Release serialization must not discover or download sources')
  }
  sys.source('R/release.R', envir = writer)
  write_release_assets <- writer$write_release_assets
  columns <- measure_columns(catalog, 'tract')
  get_year <- function(year) {
    d <- data.frame(
      census_tract_id = c('39061000200', '01001020100'),
      year = rep(year, 2L)
    )
    for (name in names(catalog$measures)) {
      d[[name]] <- round_measure(
        c(year / 10000, NA_real_),
        catalog$measures[[name]]
      )
      d[[paste0(name, '_moe')]] <- c(0.01, NA_real_)
    }
    d[columns]
  }
  paths <- write_release_assets(
    get_year,
    catalog,
    'tract',
    '3.0.0',
    directory,
    sources
  )
  saved <- as.data.frame(arrow::read_parquet(paths[1]))
  ordinary <- nanoparquet::read_parquet(paths[1])
  expected <- dplyr::bind_rows(lapply(2021:2023, function(year) {
    d <- get_year(year)
    d[order(d$census_tract_id), ]
  }))
  stopifnot(
    isTRUE(all.equal(saved, expected, check.attributes = FALSE)),
    isTRUE(all.equal(ordinary, expected, check.attributes = FALSE)),
    identical(
      vapply(saved, typeof, character(1)),
      vapply(expected, typeof, character(1))
    ),
    !anyDuplicated(saved[c('census_tract_id', 'year')])
  )
  reader <- arrow::ParquetFileReader$create(paths[1])
  stopifnot(reader$num_row_groups == 3L)
  metadata <- nanoparquet::read_parquet_metadata(paths[1])
  chunks <- metadata$column_chunks
  year_chunks <- chunks[
    vapply(chunks$path_in_schema, identical, logical(1), 'year'),
  ]
  decode_year <- function(x) readBin(x, integer(), n = 1L, endian = 'little')
  stopifnot(
    all(chunks$codec == 'SNAPPY'),
    identical(
      vapply(year_chunks$min_value, decode_year, integer(1)),
      2021:2023
    ),
    identical(vapply(year_chunks$max_value, decode_year, integer(1)), 2021:2023)
  )
  for (i in 0:2) {
    group <- as.data.frame(reader$ReadRowGroup(i))
    stopifnot(identical(unique(group$year), 2021L + i), nrow(group) == 2L)
  }
  snapshot <- yaml::read_yaml(paths[2])
  software <- snapshot$release$software
  build_packages <- c('arrow', 'data.table', 'dplyr', 'stow', 'yaml')
  stopifnot(
    software$r_version == as.character(getRversion()),
    software$platform == R.version$platform,
    identical(names(software$packages), build_packages),
    identical(
      unlist(software$packages),
      vapply(
        build_packages,
        function(package) {
          as.character(utils::packageVersion(package))
        },
        character(1)
      )
    )
  )
  stopifnot(
    snapshot$release$tag == 'v3.0.0',
    snapshot$release$rows == 6L,
    identical(unlist(snapshot$release$years), 2021:2023),
    identical(unlist(snapshot$release$columns), columns),
    identical(
      unlist(snapshot$release$primary_key),
      c('census_tract_id', 'year')
    ),
    length(snapshot$release$row_groups) == 3L,
    identical(
      snapshot$release$source_files,
      lapply(seq_len(nrow(sources)), function(i) {
        as.list(sources[i, ])
      })
    ),
    setequal(
      vapply(snapshot$release$source_files, function(x) x$year, integer(1)),
      2021:2023
    ),
    identical(read.csv(paths[3]), read.csv(catalog$dataset$inflation$file)),
    snapshot$dataset$inflation$file == basename(paths[3])
  )
  result <- arrow::open_dataset(paths[1]) |>
    dplyr::filter(
      year %in% c(2021L, 2023L),
      census_tract_id == '39061000200'
    ) |>
    dplyr::select(
      census_tract_id,
      year,
      fraction_poverty,
      fraction_poverty_moe
    ) |>
    dplyr::collect()
  expected_query <- expected[
    expected$year %in%
      c(2021L, 2023L) &
      expected$census_tract_id == '39061000200',
    names(result)
  ]
  # Parallel Arrow scans need not return row groups in file order.
  result <- result[order(result$year, result$census_tract_id), ]
  expected_query <- expected_query[
    order(expected_query$year, expected_query$census_tract_id),
  ]
  stopifnot(isTRUE(all.equal(
    as.data.frame(result),
    expected_query,
    check.attributes = FALSE
  )))

  rejected <- function(expr, message) {
    error <- tryCatch(force(expr), error = identity)
    stopifnot(
      inherits(error, 'error'),
      grepl(message, conditionMessage(error), fixed = TRUE)
    )
  }
  rejected(
    write_release_assets(
      get_year,
      catalog,
      'tract',
      '3.0.0',
      directory,
      sources
    ),
    'already exist'
  )
  rejected(
    release_asset_paths(catalog, 'other', '3.0.0', directory),
    'Unknown geography'
  )
  rejected(
    release_asset_paths(catalog, 'tract', 'bad', directory),
    'semantic release version'
  )
  # A failed inventory must surface before invoking the year builder or
  # creating output files, including when the caller passes a lazy expression.
  target <- file.path(directory, 'inventory_failure')
  rejected(
    write_release_assets(
      function(year) stop('Year builder must not run'),
      catalog,
      'tract',
      '3.0.0',
      target,
      stop('Source preparation failed')
    ),
    'Source preparation failed'
  )
  stopifnot(!dir.exists(target))
  failures <- list(
    missing_year = function(year) {
      d <- get_year(year)
      if (year == 2022L) {
        d <- d[FALSE, ]
      }
      d
    },
    wrong_year = function(year) {
      d <- get_year(year)
      if (year == 2022L) {
        d$year[] <- 2021L
      }
      d
    },
    duplicate_keys = function(year) {
      d <- get_year(year)
      if (year == 2022L) {
        d <- d[c(1L, 1L), ]
      }
      d
    },
    changed_types = function(year) {
      d <- get_year(year)
      if (year == 2022L) {
        d$n_pop <- as.double(d$n_pop)
      }
      d
    },
    missing_column = function(year) {
      d <- get_year(year)
      if (year == 2022L) {
        d$n_pop <- NULL
      }
      d
    }
  )
  for (name in names(failures)) {
    target <- file.path(directory, name)
    error <- tryCatch(
      write_release_assets(
        failures[[name]],
        catalog,
        'tract',
        '3.0.0',
        target,
        sources
      ),
      error = identity
    )
    stopifnot(
      inherits(error, 'error'),
      length(list.files(target, all.files = TRUE, no.. = TRUE)) == 0L
    )
  }

  # Earlier unavailable measures must retain the same column types as later years.
  full_catalog <- read_catalog()
  schemas <- lapply(c(2010L, 2024L), function(year) {
    getter <- function(variables, year, summary_var = NULL) {
      d <- data.frame(
        GEOID = '01001020100',
        variable = variables,
        estimate = 100,
        moe = 5
      )
      if (!is.null(summary_var)) {
        d$summary_est <- 1000
        d$summary_moe <- 10
      }
      d
    }
    d <- build_year_data(
      full_catalog,
      'tract',
      year,
      getter,
      read.csv(full_catalog$dataset$inflation$file)
    )
    stopifnot(identical(unique(d$year), year))
    if (year == 2010L) {
      stopifnot(is.na(d$fraction_insured), is.na(d$fraction_hs))
    }
    vapply(d, typeof, character(1))
  })
  stopifnot(identical(schemas[[1]], schemas[[2]]))
  cat(
    'Release tests passed: complete coverage, yearly row groups, both readers, catalogs, filtered queries, stable schemas and failure cleanup.\n'
  )
}

run_release_tests()
