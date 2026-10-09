# Each reader uses the path returned by stow directly. etag = FALSE pins the
# local copy to its year-specific URL and avoids repeated network probes.
stow_source_file <- function(url, subdir, offline = FALSE) {
  stow::stow(url, package = 'hh.acs.measures', subdir = subdir,
             etag = FALSE, offline = offline)
}

acs_numeric <- function(x) {
  x <- trimws(as.character(x))
  missing <- is.na(x) | x %in% c('', 'NA', 'N', 'null', 'NULL', '-', '(X)',
                                '**', '***', '*****')
  value <- suppressWarnings(as.numeric(x))
  if (any(is.na(value) & !missing)) {
    stop('Unrecognized ACS value: ', paste(unique(x[is.na(value) & !missing]), collapse = ', '))
  }
  # Published sentinel values are annotations, not negative estimates or MOEs.
  value[missing | value %in% (-111111111 * seq_len(9))] <- NA_real_
  value
}

read_summary_table <- function(path, variables, geography, states = NULL) {
  columns <- unique(c('GEO_ID', sub('_', '_E', variables, fixed = TRUE),
                      sub('_', '_M', variables, fixed = TRUE)))
  header <- strsplit(readLines(path, n = 1L, warn = FALSE), '|', fixed = TRUE)[[1]]
  absent <- setdiff(columns, header)
  if (length(absent)) stop('Missing Summary File columns: ', paste(absent, collapse = ', '))
  d <- data.table::fread(path, sep = '|', select = columns, colClasses = 'character',
                         na.strings = NULL, showProgress = FALSE, nThread = 2L,
                         data.table = FALSE)
  pattern <- switch(geography, tract = '^1400000US[0-9]{11}$',
                    zcta = '^860Z200US[0-9]{5}$', stop('Unknown geography'))
  d <- d[grepl(pattern, d$GEO_ID), , drop = FALSE]
  d$GEOID <- sub('^.{9}', '', d$GEO_ID)
  if (geography == 'tract' && !is.null(states)) {
    d <- d[substr(d$GEOID, 1L, 2L) %in% states, , drop = FALSE]
  }
  if (!nrow(d)) stop('No ', geography, ' records in Summary File: ', path)
  if (anyDuplicated(d$GEOID)) stop('Duplicate geography records in Summary File')
  for (variable in variables) {
    estimate <- sub('_', '_E', variable, fixed = TRUE)
    moe <- sub('_', '_M', variable, fixed = TRUE)
    d[[estimate]] <- acs_numeric(d[[estimate]])
    d[[moe]] <- acs_numeric(d[[moe]])
  }
  d[c('GEOID', setdiff(columns, 'GEO_ID'))]
}

summary_file_getter <- function(catalog, geography, offline = FALSE,
                                download = stow_source_file) {
  tables <- new.env(parent = emptyenv())
  function(variables, year, summary_var = NULL) {
    if (!year %in% rule_years(catalog$geographies[[geography]])) stop('Year outside catalog coverage')
    requested <- unique(c(variables, summary_var))
    output <- lapply(unique(sub('_.*', '', variables)), function(table) {
      key <- paste(year, table, sep = '-')
      if (!exists(key, tables, inherits = FALSE)) {
        needed <- unique(c(required_acs_variables(catalog, year), requested))
        needed <- needed[sub('_.*', '', needed) == table]
        path <- download(summary_file_url(year, table),
                         paste0('summary-files/', year), offline = offline)
        d <- read_summary_table(path, needed, geography,
                                catalog$geographies[[geography]]$state_fips)
        if (geography == 'tract') d <- harmonize_ct_tracts(d, year)
        assign(key, d, tables)
      }
      d <- get(key, tables, inherits = FALSE)
      dplyr::bind_rows(lapply(variables[sub('_.*', '', variables) == table], function(variable) {
        data.frame(GEOID = d$GEOID, variable = variable,
                   estimate = d[[sub('_', '_E', variable, fixed = TRUE)]],
                   moe = d[[sub('_', '_M', variable, fixed = TRUE)]])
      }))
    })
    output <- dplyr::bind_rows(output)
    if (!is.null(summary_var)) {
      # Current measures draw their numerator and denominator from one table.
      table <- sub('_.*', '', summary_var)
      if (!exists(paste(year, table, sep = '-'), tables, inherits = FALSE)) {
        stop('Denominator must be in the same table as a numerator component')
      }
      d <- get(paste(year, table, sep = '-'), tables)
      index <- match(output$GEOID, d$GEOID)
      output$summary_est <- d[[sub('_', '_E', summary_var, fixed = TRUE)]][index]
      output$summary_moe <- d[[sub('_', '_M', summary_var, fixed = TRUE)]][index]
    }
    output
  }
}

read_boundary_scaffold <- function(path, geo) {
  files <- utils::unzip(path, list = TRUE)$Name
  dbf <- files[grepl('[.]dbf$', files, ignore.case = TRUE)]
  if (length(dbf) != 1L) stop('Expected one boundary attribute table')
  directory <- tempfile('hh-acs-boundary-')
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  utils::unzip(path, files = dbf, exdir = directory)
  d <- foreign::read.dbf(file.path(directory, dbf), as.is = TRUE)
  if (!geo$scaffold$id_column %in% names(d)) stop('Boundary identifier column is absent')
  if (!is.null(geo$state_fips)) d <- d[d$STATEFP %in% geo$state_fips, , drop = FALSE]
  result <- data.frame(id = d[[geo$scaffold$id_column]], vintage = '2020')
  names(result) <- c(geo$id, geo$vintage)
  result
}

make_scaffold <- function(catalog, geography, offline = FALSE, download = stow_source_file) {
  geo <- catalog$geographies[[geography]]
  path <- download(geo$scaffold$url, 'boundaries', offline = offline)
  read_boundary_scaffold(path, geo)
}
