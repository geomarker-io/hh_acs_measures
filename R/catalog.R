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

validate_catalog <- function(catalog) {
  fail <- function(...) stop(..., call. = FALSE)
  check_range <- function(rule) {
    years <- unlist(rule$years)
    if (
      length(years) != 2L ||
        anyNA(years) ||
        !is.numeric(years) ||
        any(years != as.integer(years)) ||
        years[1] > years[2]
    ) {
      fail('Each year range must contain two ordered integer years')
    }
  }
  if (!identical(catalog$schema_version, 3L)) {
    fail('Unsupported catalog schema_version')
  }
  if (!identical(catalog$dataset$moe_level, 90L)) {
    fail('Use the published 90% ACS MOEs')
  }
  if (!length(catalog$measures) || anyDuplicated(names(catalog$measures))) {
    fail('Measures must have unique names')
  }
  calculators <- c('proportion', 'count', 'estimate', 'inflation_adjusted')
  for (name in names(catalog$measures)) {
    spec <- catalog$measures[[name]]
    for (field in c(
      'title',
      'domain',
      'definition',
      'units',
      'calculation',
      'moe_method'
    )) {
      if (
        !is.character(spec[[field]]) ||
          length(spec[[field]]) != 1L ||
          !nzchar(spec[[field]])
      ) {
        fail(name, ': missing ', field)
      }
    }
    if (!spec$calculation %in% calculators) {
      fail(name, ': unknown calculation')
    }
    if (!length(spec$rules)) {
      fail(name, ': missing rules')
    }
    if (
      !spec$rounding$method %in% c('integer', 'decimal', 'significant') ||
        length(spec$rounding$digits) != 1L ||
        !is.numeric(spec$rounding$digits) ||
        spec$rounding$digits < 0
    ) {
      fail(name, ': invalid rounding')
    }
    for (rule in spec$rules) {
      check_range(rule)
      if (rule$years[[1]] < 2010L) {
        fail(name, ': source rules must begin in 2010 or later')
      }
      if (spec$calculation == 'inflation_adjusted') {
        source <- rule$source_measure
        if (
          is.null(source) ||
            !source %in% names(catalog$measures) ||
            catalog$measures[[source]]$calculation == 'inflation_adjusted'
        ) {
          fail(name, ': inflation source must be an unadjusted measure')
        }
      } else {
        variables <- unlist(rule$variables)
        if (
          !length(variables) ||
            anyDuplicated(variables) ||
            any(!grepl('^[BC][0-9]{5}_[0-9]{3}$', variables))
        ) {
          fail(name, ': invalid ACS variables')
        }
        if (spec$calculation == 'estimate' && length(variables) != 1L) {
          fail(name, ': estimate requires one variable')
        }
        if (
          spec$calculation == 'proportion' &&
            (length(rule$denominator) != 1L ||
              !grepl('^[BC][0-9]{5}_[0-9]{3}$', rule$denominator))
        ) {
          fail(name, ': invalid denominator')
        }
        if (
          spec$calculation == 'proportion' &&
            !sub('_.*', '', rule$denominator) %in% sub('_.*', '', variables)
        ) {
          fail(
            name,
            ': denominator must share a table with a numerator component'
          )
        }
      }
    }
    years <- unlist(lapply(spec$rules, rule_years))
    if (anyDuplicated(years)) {
      fail(name, ': overlapping year rules')
    }
    if (!identical(sort(years), seq.int(min(years), max(years)))) {
      fail(name, ': gaps between year rules')
    }
  }
  for (geography in names(catalog$geographies)) {
    spec <- catalog$geographies[[geography]]
    check_range(spec)
    first_year <- if (geography == 'zcta') 2011L else 2010L
    if (spec$years[[1]] < first_year) {
      fail(geography, ': supported ACS coverage begins in ', first_year)
    }
    for (year in rule_years(spec)) {
      for (name in names(catalog$measures)) {
        measure <- catalog$measures[[name]]
        rule <- select_rule(measure$rules, year, name, allow_missing = TRUE)
        if (!is.null(rule) && measure$calculation == 'inflation_adjusted') {
          select_rule(
            catalog$measures[[rule$source_measure]]$rules,
            year,
            rule$source_measure
          )
        }
      }
    }
  }
  invisible(catalog)
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

# Only table-based five-year Summary Files (2021 onward) are supported.
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

summary_source_files <- function(catalog, geography) {
  dplyr::bind_rows(lapply(
    rule_years(catalog$geographies[[geography]]),
    function(year) {
      if (year < 2021L) {
        return(sequence_source_files(catalog, geography, year))
      }
      tables <- unique(sub('_.*', '', required_acs_variables(catalog, year)))
      data.frame(
        year = year,
        table = tables,
        url = vapply(
          tables,
          function(table) summary_file_url(year, table),
          character(1)
        )
      )
    }
  ))
}
