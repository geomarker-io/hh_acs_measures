# A release is a complete history for one geography and one definition version.
release_asset_paths <- function(catalog, geography, version, directory) {
  if (!geography %in% names(catalog$geographies)) {
    stop('Unknown geography')
  }
  if (
    length(version) != 1L ||
      is.na(version) ||
      !grepl('^[0-9]+\\.[0-9]+\\.[0-9]+([.-][A-Za-z0-9.-]+)?$', version)
  ) {
    stop('Supply a semantic release version, e.g. 3.0.0')
  }
  stem <- paste0(catalog$geographies[[geography]]$resource, '-v', version)
  file.path(directory, paste0(stem, c('.parquet', '.measures.yml', '.CPI.csv')))
}

# Record the loaded versions of the build dependencies declared in DESCRIPTION.
# Optional reader/test dependencies do not need to be installed for a build.
release_software_versions <- function(description_path = 'DESCRIPTION') {
  imports <- read.dcf(description_path, fields = 'Imports')[1, 'Imports']
  packages <- trimws(strsplit(imports, ',', fixed = TRUE)[[1]])
  packages <- sub('[[:space:]]*\\(.*\\)$', '', packages)
  versions <- vapply(
    packages,
    function(package) {
      as.character(getNamespaceVersion(loadNamespace(package)))
    },
    character(1)
  )
  list(
    r_version = as.character(getRversion()),
    platform = R.version$platform,
    packages = as.list(versions)
  )
}

# get_year returns one calculated year. Only that year's rows need to be in memory.
write_release_assets <- function(
  get_year,
  catalog,
  geography,
  version,
  directory
) {
  paths <- release_asset_paths(catalog, geography, version, directory)
  if (any(file.exists(paths))) {
    stop('Release assets already exist; use a new version or empty directory')
  }
  software <- release_software_versions()
  geo <- catalog$geographies[[geography]]
  keys <- c(geo$id, 'year')
  columns <- measure_columns(catalog, geography)
  years <- rule_years(geo)
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  staging <- tempfile('.release-', tmpdir = directory)
  dir.create(staging)
  staged <- file.path(staging, basename(paths))
  writer <- sink <- NULL
  on.exit(
    {
      if (!is.null(writer)) {
        try(writer$Close(), silent = TRUE)
      }
      if (!is.null(sink)) {
        try(sink$close(), silent = TRUE)
      }
      unlink(staging, recursive = TRUE)
    },
    add = TRUE
  )
  groups <- vector('list', length(years))
  schema <- NULL
  for (i in seq_along(years)) {
    year <- years[i]
    d <- get_year(year)
    if (!identical(names(d), columns)) {
      stop('Data columns do not match the catalog for ', year)
    }
    if (!nrow(d) || anyNA(d$year) || any(d$year != year)) {
      stop('Missing or incorrect release coverage for ', year)
    }
    if (
      !is.character(d[[geo$id]]) || anyNA(d[[geo$id]]) || !is.integer(d$year)
    ) {
      stop('Release identifiers must be strings and year must be an integer')
    }
    if (anyDuplicated(d[keys])) {
      stop('Duplicate release keys for ', year)
    }
    d <- d[order(d[[geo$id]]), , drop = FALSE]
    table <- arrow::Table$create(d)
    if (is.null(writer)) {
      schema <- table$schema
      sink <- arrow::FileOutputStream$create(staged[1])
      properties <- arrow::ParquetWriterProperties$create(
        columns,
        version = '2.4',
        compression = 'snappy',
        write_statistics = TRUE
      )
      writer <- arrow::ParquetFileWriter$create(
        schema,
        sink,
        properties = properties
      )
    } else if (!schema$Equals(table$schema)) {
      stop('Inconsistent release column types for ', year)
    }
    writer$WriteTable(table, chunk_size = nrow(d))
    groups[[i]] <- list(year = year, rows = nrow(d))
    message('Wrote ', geography, ' ', year, ': ', nrow(d), ' rows')
    rm(d, table)
  }
  writer$Close()
  writer <- NULL
  sink$close()
  sink <- NULL
  snapshot <- catalog
  snapshot$dataset$inflation$file <- basename(paths[3])
  sources <- summary_source_files(catalog, geography)
  snapshot$release <- list(
    name = geo$resource,
    tag = paste0('v', version),
    version = version,
    geography = geography,
    data_file = basename(paths[1]),
    format = 'parquet',
    software = software,
    rows = sum(vapply(groups, function(x) x$rows, integer(1))),
    years = years,
    primary_key = keys,
    columns = columns,
    row_groups = groups,
    source_files = lapply(seq_len(nrow(sources)), function(i) {
      as.list(sources[i, ])
    })
  )
  yaml::write_yaml(snapshot, staged[2])
  if (!file.copy(catalog$dataset$inflation$file, staged[3])) {
    stop('Could not copy CPI input')
  }
  if (any(file.exists(paths))) {
    stop('Release assets already exist; use a new version or empty directory')
  }
  moved <- character()
  for (i in seq_along(paths)) {
    if (!file.rename(staged[i], paths[i])) {
      unlink(moved)
      stop('Could not finalize release assets')
    }
    moved <- c(moved, paths[i])
  }
  message(
    'Staged release assets in ',
    directory,
    '. Upload separately to GitHub Releases.'
  )
  invisible(paths)
}
