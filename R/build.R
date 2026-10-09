source('R/catalog.R')
source('R/moe.R')
source('R/calculations.R')
source('R/summary_files.R')
source('R/release.R')
source('R/harmonize_ct_tracts.R')

build_release <- function(geography, version, directory = file.path('data', paste0('v', version))) {
  catalog <- read_catalog()
  if (!geography %in% names(catalog$geographies)) stop('Unknown geography')
  paths <- release_asset_paths(catalog, geography, version, directory)
  if (any(file.exists(paths))) stop('Release assets already exist; use a new version or empty directory')
  offline <- nzchar(Sys.getenv('HH_ACS_OFFLINE'))
  scaffold <- make_scaffold(catalog, geography, offline = offline)
  cpi <- read.csv(catalog$dataset$inflation$file)
  d <- build_measure_data(catalog, geography, scaffold,
                         summary_file_getter(catalog, geography, offline = offline), cpi)
  write_release_assets(d, catalog, geography, version, directory)
  invisible(d)
}

release_cli <- function(geography) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) < 1L || length(args) > 2L) {
    stop('Usage: Rscript make_hh_acs_data', if (geography == 'zcta') '_zcta',
         '.R VERSION [OUTPUT_DIRECTORY]')
  }
  directory <- if (length(args) == 2L) args[2] else file.path('data', paste0('v', args[1]))
  build_release(geography, args[1], directory)
}
