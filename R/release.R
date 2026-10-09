# Release data and its catalog together as ordinary files.
release_asset_paths <- function(catalog, geography, version, directory) {
  if (
    length(version) != 1L ||
      is.na(version) ||
      !grepl('^[0-9]+\\.[0-9]+\\.[0-9]+([.-][A-Za-z0-9.-]+)?$', version)
  ) {
    stop('Supply a semantic release version, e.g. 2.0.0')
  }
  stem <- paste0(catalog$geographies[[geography]]$resource, '-v', version)
  file.path(directory, paste0(stem, c('.parquet', '.measures.yml', '.CPI.csv')))
}

write_release_assets <- function(d, catalog, geography, version, directory) {
  geo <- catalog$geographies[[geography]]
  keys <- c(geo$id, geo$vintage, 'year')
  columns <- c(
    keys,
    unlist(
      lapply(names(catalog$measures), function(name) {
        c(name, paste0(name, '_moe'))
      }),
      use.names = FALSE
    )
  )
  if (!identical(names(d), columns)) {
    stop('Data columns do not match the catalog')
  }
  if (anyDuplicated(d[keys])) {
    stop('Duplicate release keys')
  }
  paths <- release_asset_paths(catalog, geography, version, directory)
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  if (any(file.exists(paths))) {
    stop('Release assets already exist; use a new version or empty directory')
  }
  arrow::write_parquet(d, paths[1])
  snapshot <- catalog
  snapshot$dataset$inflation$file <- basename(paths[3])
  sources <- summary_source_files(catalog, geography)
  snapshot$release <- list(
    name = geo$resource,
    version = version,
    geography = geography,
    data_file = basename(paths[1]),
    format = 'parquet',
    rows = nrow(d),
    years = sort(unique(d$year)),
    primary_key = keys,
    columns = columns,
    source_files = lapply(seq_len(nrow(sources)), function(i) {
      as.list(sources[i, ])
    })
  )
  yaml::write_yaml(snapshot, paths[2])
  if (!file.copy(catalog$dataset$inflation$file, paths[3])) {
    stop('Could not copy CPI input')
  }
  message(
    'Staged release assets in ',
    directory,
    '. Upload separately to GitHub Releases.'
  )
  invisible(paths)
}
