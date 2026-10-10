# Catalog parsing and rule selection are independent of downloads and release preparation.
rule_years <- function(rule) seq.int(rule$years[[1]], rule$years[[2]])

select_rule <- function(rules, year, label, allow_missing = FALSE) {
  matches <- Filter(function(rule) year %in% rule_years(rule), rules)
  if (length(matches) == 0L && allow_missing) {
    return(NULL)
  }
  if (length(matches) != 1L) {
    stop(label, ': expected one rule for ', year, '; found ', length(matches))
  }
  matches[[1]]
}

catalog_error <- function(...) stop(..., call. = FALSE)

is_catalog_string <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
}

validate_year_range <- function(spec) {
  years <- unlist(spec$years)
  if (
    length(years) != 2L ||
      !is.numeric(years) ||
      anyNA(years) ||
      any(!is.finite(years)) ||
      any(years != as.integer(years)) ||
      years[1] > years[2]
  ) {
    catalog_error('Each year range must contain two ordered integer years')
  }
}

validate_measure_fields <- function(spec, name) {
  for (field in c(
    'title',
    'domain',
    'definition',
    'units',
    'calculation',
    'moe_method'
  )) {
    if (!is_catalog_string(spec[[field]])) {
      catalog_error(name, ': missing ', field)
    }
  }
  if (
    !spec$calculation %in%
      c('proportion', 'count', 'estimate', 'inflation_adjusted')
  ) {
    catalog_error(name, ': unknown calculation')
  }
  if (!length(spec$rules)) {
    catalog_error(name, ': missing rules')
  }
  method <- spec$rounding$method
  digits <- spec$rounding$digits
  if (
    !is_catalog_string(method) ||
      !method %in% c('integer', 'decimal', 'significant') ||
      !is.numeric(digits) ||
      length(digits) != 1L ||
      is.na(digits) ||
      !is.finite(digits) ||
      digits < 0 ||
      digits != as.integer(digits) ||
      (method == 'significant' && digits == 0)
  ) {
    catalog_error(name, ': invalid rounding')
  }
}

validate_measure_rules <- function(spec, name, measures) {
  for (rule in spec$rules) {
    validate_year_range(rule)
    if (rule$years[[1]] < 2010L) {
      catalog_error(name, ': source rules must begin in 2010 or later')
    }
    if (spec$calculation == 'inflation_adjusted') {
      source <- rule$source_measure
      if (
        !is_catalog_string(source) ||
          !source %in% names(measures) ||
          measures[[source]]$calculation == 'inflation_adjusted'
      ) {
        catalog_error(name, ': inflation source must be an unadjusted measure')
      }
    } else {
      variables <- unlist(rule$variables)
      if (
        !length(variables) ||
          !is.character(variables) ||
          anyNA(variables) ||
          anyDuplicated(variables) ||
          any(!grepl('^[BC][0-9]{5}_[0-9]{3}$', variables))
      ) {
        catalog_error(name, ': invalid ACS variables')
      }
      if (spec$calculation == 'estimate' && length(variables) != 1L) {
        catalog_error(name, ': estimate requires one variable')
      }
      if (spec$calculation == 'proportion') {
        if (
          !is_catalog_string(rule$denominator) ||
            !grepl('^[BC][0-9]{5}_[0-9]{3}$', rule$denominator)
        ) {
          catalog_error(name, ': invalid denominator')
        }
        if (!sub('_.*', '', rule$denominator) %in% sub('_.*', '', variables)) {
          catalog_error(
            name,
            ': denominator must share a table with a numerator component'
          )
        }
      }
    }
  }
  years <- unlist(lapply(spec$rules, rule_years))
  if (anyDuplicated(years)) {
    catalog_error(name, ': overlapping year rules')
  }
  if (!identical(sort(years), seq.int(min(years), max(years)))) {
    catalog_error(name, ': gaps between year rules')
  }
}

# The supported 50 states and DC, in FIPS order.
acs_state_fips <- c(
  '01',
  '02',
  '04',
  '05',
  '06',
  '08',
  '09',
  '10',
  '11',
  '12',
  '13',
  '15',
  '16',
  '17',
  '18',
  '19',
  '20',
  '21',
  '22',
  '23',
  '24',
  '25',
  '26',
  '27',
  '28',
  '29',
  '30',
  '31',
  '32',
  '33',
  '34',
  '35',
  '36',
  '37',
  '38',
  '39',
  '40',
  '41',
  '42',
  '44',
  '45',
  '46',
  '47',
  '48',
  '49',
  '50',
  '51',
  '53',
  '54',
  '55',
  '56'
)

validate_geography <- function(spec, geography, measures) {
  validate_year_range(spec)
  if (spec$years[[1]] < 2010L) {
    catalog_error(geography, ': supported ACS coverage begins in 2010')
  }
  for (field in c('id', 'resource')) {
    if (!is_catalog_string(spec[[field]])) {
      catalog_error(geography, ': missing ', field)
    }
  }
  states <- spec$state_fips
  if (
    !is.character(states) ||
      !length(states) ||
      anyNA(states) ||
      anyDuplicated(states) ||
      any(!states %in% acs_state_fips)
  ) {
    catalog_error(
      geography,
      ': state_fips must contain unique supported two-digit codes'
    )
  }
  final_year <- spec$years[[2]]
  for (name in names(measures)) {
    measure <- measures[[name]]
    years <- unlist(lapply(measure$rules, rule_years))
    if (!final_year %in% years) {
      catalog_error(
        name,
        ': source rules must cover the final release year ',
        final_year
      )
    }
    for (year in intersect(rule_years(spec), years)) {
      if (measure$calculation == 'inflation_adjusted') {
        rule <- select_rule(measure$rules, year, name)
        select_rule(
          measures[[rule$source_measure]]$rules,
          year,
          rule$source_measure
        )
      }
    }
  }
}

validate_catalog <- function(catalog) {
  if (!identical(catalog$schema_version, 3L)) {
    catalog_error('Unsupported catalog schema_version')
  }
  if (!identical(catalog$dataset$moe_level, 90L)) {
    catalog_error('Use the published 90% ACS MOEs')
  }
  measure_names <- names(catalog$measures)
  if (
    !length(catalog$measures) ||
      length(measure_names) != length(catalog$measures) ||
      anyNA(measure_names) ||
      any(!nzchar(measure_names)) ||
      anyDuplicated(measure_names)
  ) {
    catalog_error('Measures must have unique names')
  }
  # Check every measure's fields before resolving references between measures.
  for (name in measure_names) {
    validate_measure_fields(catalog$measures[[name]], name)
  }
  for (name in measure_names) {
    validate_measure_rules(catalog$measures[[name]], name, catalog$measures)
  }
  if (!identical(names(catalog$geographies), 'tract')) {
    catalog_error('Only census tract releases are supported')
  }
  validate_geography(catalog$geographies$tract, 'tract', catalog$measures)
  invisible(catalog)
}

# Calculations and serialization use the same catalog-defined column order.
measure_columns <- function(catalog, geography) {
  c(
    catalog$geographies[[geography]]$id,
    'year',
    unlist(
      lapply(names(catalog$measures), function(name) {
        c(name, paste0(name, '_moe'))
      }),
      use.names = FALSE
    )
  )
}

read_catalog <- function(path = 'measures.yml') {
  catalog <- yaml::read_yaml(path, eval.expr = FALSE)
  validate_catalog(catalog)
  catalog
}

measure_years <- function(catalog, name, geography) {
  intersect(
    rule_years(catalog$geographies[[geography]]),
    unlist(lapply(catalog$measures[[name]]$rules, rule_years))
  )
}

# URL helper for table-based five-year Summary Files (2021 onward).
summary_file_url <- function(year, table) {
  if (
    length(year) != 1L || is.na(year) || year < 2021 || year != as.integer(year)
  ) {
    stop('Table-based downloads support ACS years 2021 onward')
  }
  if (length(table) != 1L || !grepl('^[BC][0-9]{5}$', table)) {
    stop('Invalid ACS table ID')
  }
  sprintf(
    paste0(
      'https://www2.census.gov/programs-surveys/acs/summary_file/',
      '%d/table-based-SF/data/5YRData/acsdt5y%d-%s.dat'
    ),
    year,
    year,
    tolower(table)
  )
}

required_acs_variables <- function(catalog, year) {
  unique(unlist(
    lapply(catalog$measures, function(spec) {
      if (spec$calculation == 'inflation_adjusted') {
        return(NULL)
      }
      rule <- select_rule(
        spec$rules,
        year,
        'source variables',
        allow_missing = TRUE
      )
      if (is.null(rule)) {
        return(NULL)
      }
      c(unlist(rule$variables, use.names = FALSE), rule$denominator)
    }),
    use.names = FALSE
  ))
}
