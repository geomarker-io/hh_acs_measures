source('R/catalog.R')
source('R/calculations.R')
source('R/metadata.R')
source('R/harmonize_ct_tracts.R')

make_scaffold <- function(catalog, geography, states) {
  geo <- catalog$geographies[[geography]]
  dplyr::bind_rows(lapply(c('2010', '2020'), function(vintage) {
    boundaries <- if (geography == 'tract') {
      tigris::tracts(year = if (vintage == '2010') 2019 else 2020, cb = TRUE)
    } else {
      tigris::zctas(year = if (vintage == '2010') 2019 else 2020, cb = TRUE)
    }
    boundaries <- sf::st_drop_geometry(boundaries)
    if (geography == 'tract') boundaries <- boundaries[boundaries$STATEFP %in% states, ]
    id <- if (geography == 'tract') boundaries$GEOID else
      boundaries[[if (vintage == '2010') 'GEOID10' else 'GEOID20']]
    d <- data.frame(id = id, vintage = vintage, stringsAsFactors = FALSE)
    names(d) <- c(geo$id, geo$vintage)
    d
  }))
}

cached_acs_getter <- function(catalog, geography, states,
                              cache_directory = 'acs_data_cache/raw-v1') {
  dir.create(cache_directory, recursive = TRUE, showWarnings = FALSE)
  function(variables, year, summary_var = NULL) {
    args <- list(geography = geography, variables = variables,
                 year = year, survey = catalog$dataset$survey,
                 moe_level = catalog$dataset$moe_level)
    if (geography == 'tract') args$state <- states
    if (!is.null(summary_var)) args$summary_var <- summary_var
    path <- file.path(cache_directory, paste0(digest::digest(args), '.rds'))
    if (file.exists(path)) {
      d <- readRDS(path)
    } else {
      d <- do.call(tidycensus::get_acs, args)
      saveRDS(d, path)
    }
    # Harmonize after reading raw data so catalog/crosswalk edits cannot reuse
    # previously calculated measures or stale transformed tract identifiers.
    if (geography == 'tract') d <- harmonize_ct_tracts(d, year)
    d
  }
}

build_release <- function(geography, version, directory = file.path('data', paste0('v', version))) {
  catalog <- read_catalog()
  if (!geography %in% names(catalog$geographies)) stop('Unknown geography')
  paths <- release_asset_paths(catalog, geography, version, directory)
  if (any(file.exists(paths))) stop('Release assets already exist; use a new version or empty directory')
  if (!nzchar(Sys.getenv('CENSUS_API_KEY'))) stop('Set CENSUS_API_KEY')
  options(tigris_use_cache = TRUE, tigris_class = 'sf')
  states <- unique(tigris::fips_codes$state_code[
    !tigris::fips_codes$state_name %in% c('American Samoa', 'Guam',
       'Northern Mariana Islands', 'Puerto Rico', 'U.S. Minor Outlying Islands',
       'U.S. Virgin Islands')])
  scaffold <- make_scaffold(catalog, geography, states)
  cpi <- read.csv(catalog$dataset$inflation$file)
  d <- build_measure_data(catalog, geography, scaffold,
                         cached_acs_getter(catalog, geography, states), cpi)
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
