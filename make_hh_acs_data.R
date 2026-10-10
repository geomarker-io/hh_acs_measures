#!/usr/bin/env Rscript

source('R/catalog.R')
source('R/moe.R')
source('R/calculations.R')
source('R/summary_files.R')
source('R/sequence_files.R')
source('R/release.R')

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L || length(args) > 2L) {
  stop('Usage: ./make_hh_acs_data.R VERSION [OUTPUT_DIRECTORY]')
}

version <- args[1]
directory <- if (length(args) == 2L) {
  args[2]
} else {
  file.path('data', paste0('v', version))
}

catalog <- read_catalog()
paths <- release_asset_paths(catalog, 'tract', version, directory)
if (any(file.exists(paths))) {
  stop('Release assets already exist; use a new version or empty directory')
}
offline <- nzchar(Sys.getenv('HH_ACS_OFFLINE'))
cpi <- read.csv(catalog$dataset$inflation$file)

get_year <- function(year) {
  message('Building tract ', year)
  build_year_data(
    catalog,
    'tract',
    year,
    summary_file_getter(catalog, 'tract', offline = offline),
    cpi
  )
}

write_release_assets(get_year, catalog, 'tract', version, directory)
