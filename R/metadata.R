# JSON metadata and Markdown are generated from the same catalog as calculations.
field_metadata <- function(catalog, geography) {
  geo <- catalog$geographies[[geography]]
  fields <- list(
    list(name = geo$id, title = if (geography == 'tract') 'Census tract identifier' else 'ZCTA identifier',
         type = 'string', description = 'Zero-padded identifier; unique in combination with vintage and year.'),
    list(name = geo$vintage, title = 'Geography vintage', type = 'string',
         description = 'Decennial boundary vintage selected using the documented geography rules.'),
    list(name = 'year', title = 'ACS ending year', type = 'integer',
         description = 'Ending year of the five-year ACS estimates.'))
  for (name in names(catalog$measures)) {
    spec <- catalog$measures[[name]]
    type <- if (spec$rounding$method == 'integer') 'integer' else 'number'
    fields <- c(fields, list(
      list(name = name, title = spec$title, type = type, description = spec$definition,
           units = spec$units, measure = name),
      list(name = paste0(name, '_moe'), title = paste(spec$title, 'margin of error'),
           type = type, description = paste0(catalog$dataset$moe_level,
             '% confidence margin of error; ', spec$moe_method, '.'),
           units = spec$units, measure = name)))
  }
  fields
}

resource_metadata <- function(catalog, geography, version = NULL, filename = NULL) {
  geo <- catalog$geographies[[geography]]
  list(metadata_schema_version = catalog$schema_version,
       name = geo$resource, title = catalog$dataset$title,
       version = version, path = filename, format = 'parquet',
       geography = geography, years = geo$years,
       survey = catalog$dataset$survey, moe_level = catalog$dataset$moe_level,
       primary_key = c(geo$id, geo$vintage, 'year'),
       geography_rules = geo$vintage_rules, geography_notes = geo$notes,
       inflation = catalog$dataset$inflation, notes = catalog$dataset$notes,
       fields = field_metadata(catalog, geography), measures = catalog$measures)
}

catalog_markdown <- function(catalog) {
  lines <- c('# Harmonized Historical ACS Measures', '',
             '<!-- Generated from measures.yml. Edit the catalog, then run Rscript build_metadata.R. -->', '',
             '## Dataset conventions', '', paste0('- ', unlist(catalog$dataset$notes)), '',
             paste0('All `_moe` fields are ', catalog$dataset$moe_level,
                    '% confidence margins of error in the units of their accompanying estimates.'), '')
  for (geography in names(catalog$geographies)) {
    geo <- catalog$geographies[[geography]]
    lines <- c(lines, paste0('## Geography: ', geography), '',
               paste0('Coverage: ', paste(unlist(geo$years), collapse = '–'),
                      '. Key: `', paste(c(geo$id, geo$vintage, 'year'), collapse = '`, `'), '`.'), '',
               '### Boundary vintages', '')
    for (rule in geo$vintage_rules) lines <- c(lines,
      paste0('- ', paste(unlist(rule$years), collapse = '–'), ': ', rule$value, ' boundaries.'))
    lines <- c(lines, '')
    for (note in geo$notes) lines <- c(lines,
      paste0('- ', paste(unlist(note$years), collapse = '–'), ': ', note$text,
             if (!is.null(note$source)) paste0(' [Source](', note$source, ').') else '' ))
    lines <- c(lines, '')
  }
  for (name in names(catalog$measures)) {
    spec <- catalog$measures[[name]]
    lines <- c(lines, paste0('## `', name, '` — ', spec$title), '', spec$definition, '',
               paste0('Domain: ', spec$domain, '. Units: ', spec$units, '.'), '',
               paste0('Calculation: `', spec$calculation, '`. MOE: ', spec$moe_method, '.'), '',
               paste0('Rounding (estimate and MOE): ', spec$rounding$method,
                      if (spec$rounding$method != 'integer') paste0(', ', spec$rounding$digits, ' digits') else '', '.'), '')
    for (geography in names(catalog$geographies)) {
      years <- measure_years(catalog, name, geography)
      lines <- c(lines, paste0('- ', geography, ': ', min(years), '–', max(years), '.'))
    }
    lines <- c(lines, '', '### Year-specific calculation rules', '')
    for (rule in spec$rules) {
      formula <- if (spec$calculation == 'inflation_adjusted') {
        paste0('`', rule$source_measure, '` × CPI(', catalog$dataset$inflation$base_year, ') / CPI(year)')
      } else {
        paste0('`', paste(unlist(rule$variables), collapse = '` + `'), '`',
          if (!is.null(rule$denominator)) paste0(' divided by `', rule$denominator, '`') else '')
      }
      lines <- c(lines, paste0('- ', paste(unlist(rule$years), collapse = '–'), ': ', formula, '.'),
                 if (!is.null(rule$note)) paste0('  ', rule$note),
                 if (!is.null(rule$source)) paste0('  [Source](', rule$source, ').'))
      if (!is.null(rule$variables)) {
        tables <- unique(sub('_.*', '', unlist(rule$variables)))
        lines <- c(lines, paste0('  ACS source: ', paste(vapply(tables, function(table)
          paste0('[', table, '](https://api.census.gov/data/', rule$years[[2]],
                 '/acs/acs5/groups/', table, '.html)'), character(1)), collapse = ', '), '.'))
      }
    }
    if (length(spec$notes)) lines <- c(lines, '', '### Notes', '', paste0('- ', unlist(spec$notes)))
    lines <- c(lines, '')
  }
  while (length(lines) && identical(tail(lines, 1), '')) lines <- head(lines, -1)
  lines
}

write_documentation <- function(catalog, directory = '.') {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  writeLines(catalog_markdown(catalog), file.path(directory, 'metadata.md'))
  for (geography in names(catalog$geographies)) {
    resource <- catalog$geographies[[geography]]$resource
    jsonlite::write_json(resource_metadata(catalog, geography),
      file.path(directory, paste0(resource, '.metadata.json')),
      auto_unbox = TRUE, pretty = TRUE, null = 'null')
  }
}

release_asset_paths <- function(catalog, geography, version, directory) {
  if (length(version) != 1L || is.na(version) ||
      !grepl('^[0-9]+\\.[0-9]+\\.[0-9]+([.-][A-Za-z0-9.-]+)?$', version)) {
    stop('Supply a semantic release version, e.g. 2.0.0')
  }
  stem <- paste0(catalog$geographies[[geography]]$resource, '-v', version)
  file.path(directory, paste0(stem, c('.parquet', '.metadata.json', '.measures.yml', '.metadata.md', '.CPI.csv')))
}

write_release_assets <- function(d, catalog, geography, version, directory) {
  geo <- catalog$geographies[[geography]]
  fields <- field_metadata(catalog, geography)
  if (!identical(names(d), vapply(fields, `[[`, character(1), 'name'))) {
    stop('Data columns do not match release metadata')
  }
  if (anyDuplicated(d[c(geo$id, geo$vintage, 'year')])) stop('Duplicate release keys')
  paths <- release_asset_paths(catalog, geography, version, directory)
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  if (any(file.exists(paths))) stop('Release assets already exist; use a new version or empty directory')
  arrow::write_parquet(d, paths[1])
  metadata <- resource_metadata(catalog, geography, version, basename(paths[1]))
  metadata$rows <- nrow(d)
  metadata$inflation$file <- basename(paths[5])
  jsonlite::write_json(metadata, paths[2], auto_unbox = TRUE, pretty = TRUE, null = 'null')
  snapshot <- catalog
  snapshot$dataset$inflation$file <- basename(paths[5])
  yaml::write_yaml(snapshot, paths[3])
  writeLines(catalog_markdown(catalog), paths[4])
  if (!file.copy(catalog$dataset$inflation$file, paths[5])) stop('Could not copy CPI input')
  message('Staged release assets in ', directory, '. Upload separately to GitHub Releases.')
  invisible(paths)
}
