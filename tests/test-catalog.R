# Run from the repository root: Rscript tests/test-catalog.R
source('R/catalog.R')
source('R/calculations.R')
source('R/metadata.R')
source('R/harmonize_ct_tracts.R')
source('tests/fixtures/acs.R')
catalog <- read_catalog()
stopifnot(length(catalog$measures) == 30L)
cpi <- read.csv(catalog$dataset$inflation$file)
for (geography in c('tract', 'zcta')) {
  getter <- fixture_acs(geography)
  get_acs <- function(..., year) {
    d <- getter(..., year = year)
    if (geography == 'tract') d <- harmonize_ct_tracts(d, year)
    d
  }
  actual <- build_measure_data(catalog, geography, fixture_scaffold(geography), get_acs, cpi)
  expected <- readRDS(paste0('tests/fixtures/legacy-', geography, '.rds'))
  keys <- c(catalog$geographies[[geography]]$id, catalog$geographies[[geography]]$vintage, 'year')
  sort_rows <- function(d) as.data.frame(d[do.call(order, d[keys]), names(actual)])
  actual <- sort_rows(actual)
  expected <- sort_rows(expected)
  rownames(actual) <- rownames(expected) <- NULL
  stopifnot(isTRUE(all.equal(actual, expected, check.attributes = TRUE)),
            identical(names(actual), vapply(field_metadata(catalog, geography), `[[`, character(1), 'name')))
}
# A catalog edit changes both calculation inputs and generated documentation.
changed <- catalog
changed$measures$fraction_poverty$rules[[1]]$variables <- 'B17001_003'
queried <- NULL
getter <- function(variables, year, summary_var) {
  queried <<- variables
  data.frame(GEOID = '12345', variable = variables, estimate = 40, moe = 2,
             summary_est = 100, summary_moe = 5)
}
spec <- changed$measures$fraction_poverty
result <- calculate_acs_measure(spec, select_rule(spec$rules, 2023, 'poverty'), 2023, getter)
stopifnot(identical(queried, 'B17001_003'), result$estimate == 0.4,
          any(grepl('B17001_003', catalog_markdown(changed))))
# Ambiguous rules and invalid derivations fail before data are downloaded.
bad <- catalog
bad$measures$fraction_poverty$rules[[2]] <- bad$measures$fraction_poverty$rules[[1]]
stopifnot(inherits(tryCatch(validate_catalog(bad), error = identity), 'error'))
bad <- catalog
bad$measures$median_income_2010adj$rules[[1]]$source_measure <- 'nonexistent'
stopifnot(inherits(tryCatch(validate_catalog(bad), error = identity), 'error'))
# Undefined proportions must not enter release data as NaN or Inf.
getter <- function(variables, year, summary_var) {
  data.frame(GEOID = '12345', variable = variables, estimate = 0, moe = 2,
             summary_est = 0, summary_moe = 5)
}
result <- calculate_acs_measure(spec, spec$rules[[1]], 2023, getter)
stopifnot(is.na(result$estimate), is.na(result$moe))
# Stage a complete small release and read the Parquet and JSON back.
get_acs <- fixture_acs('zcta')
d <- build_measure_data(catalog, 'zcta', fixture_scaffold('zcta'), get_acs, cpi)
directory <- tempfile('hh-acs-release-')
paths <- write_release_assets(d, catalog, 'zcta', '2.0.0', directory)
metadata <- jsonlite::read_json(paths[2], simplifyVector = TRUE)
stopifnot(isTRUE(all.equal(as.data.frame(arrow::read_parquet(paths[1])), as.data.frame(d))),
          identical(metadata$fields$name, names(d)), metadata$rows == nrow(d),
          metadata$version == '2.0.0', metadata$moe_level == 95L,
          identical(metadata$primary_key, c('zcta', 'zcta_vintage', 'year')),
          file.exists(file.path(directory, metadata$inflation$file)),
          inherits(tryCatch(write_release_assets(d, catalog, 'zcta', '2.0.0', directory),
                            error = identity), 'error'))
unlink(directory, recursive = TRUE)
cat('Catalog rules, all legacy calculations, documentation linkage, and release round-trip pass.\n')
