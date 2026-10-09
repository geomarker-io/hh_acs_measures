# Catalog parsing and rule selection are independent of downloads and rendering.
rule_years <- function(rule) seq.int(rule$years[[1]], rule$years[[2]])

select_rule <- function(rules, year, label, allow_missing = FALSE) {
  matches <- Filter(function(rule) year %in% rule_years(rule), rules)
  if (length(matches) == 0L && allow_missing) return(NULL)
  if (length(matches) != 1L) {
    stop(label, ': expected one rule for ', year, '; found ', length(matches))
  }
  matches[[1]]
}

validate_catalog <- function(catalog) {
  fail <- function(...) stop(..., call. = FALSE)
  check_range <- function(rule) {
    years <- unlist(rule$years)
    if (length(years) != 2L || anyNA(years) || !is.numeric(years) ||
        any(years != as.integer(years)) || years[1] > years[2]) {
      fail('Each year range must contain two ordered integer years')
    }
  }
  if (!identical(catalog$schema_version, 1L)) fail('Unsupported catalog schema_version')
  if (!length(catalog$measures) || anyDuplicated(names(catalog$measures))) {
    fail('Measures must have unique names')
  }
  calculators <- c('proportion', 'count', 'estimate', 'inflation_adjusted')
  for (name in names(catalog$measures)) {
    spec <- catalog$measures[[name]]
    for (field in c('title', 'domain', 'definition', 'units', 'calculation', 'moe_method')) {
      if (!is.character(spec[[field]]) || length(spec[[field]]) != 1L ||
          !nzchar(spec[[field]])) fail(name, ': missing ', field)
    }
    if (!spec$calculation %in% calculators) fail(name, ': unknown calculation')
    if (!length(spec$rules)) fail(name, ': missing rules')
    if (!spec$rounding$method %in% c('integer', 'decimal', 'significant') ||
        length(spec$rounding$digits) != 1L || !is.numeric(spec$rounding$digits) ||
        spec$rounding$digits < 0) fail(name, ': invalid rounding')
    for (rule in spec$rules) {
      check_range(rule)
      if (spec$calculation == 'inflation_adjusted') {
        source <- rule$source_measure
        if (is.null(source) || !source %in% names(catalog$measures) ||
            catalog$measures[[source]]$calculation == 'inflation_adjusted') {
          fail(name, ': inflation source must be an unadjusted measure')
        }
      } else {
        variables <- unlist(rule$variables)
        if (!length(variables) || anyDuplicated(variables) ||
            any(!grepl('^[BC][0-9]{5}_[0-9]{3}$', variables))) {
          fail(name, ': invalid ACS variables')
        }
        if (spec$calculation == 'estimate' && length(variables) != 1L) {
          fail(name, ': estimate requires one variable')
        }
        if (spec$calculation == 'proportion' &&
            (length(rule$denominator) != 1L ||
             !grepl('^[BC][0-9]{5}_[0-9]{3}$', rule$denominator))) {
          fail(name, ': invalid denominator')
        }
      }
    }
    years <- unlist(lapply(spec$rules, rule_years))
    if (anyDuplicated(years)) fail(name, ': overlapping year rules')
    if (!identical(sort(years), seq.int(min(years), max(years)))) {
      fail(name, ': gaps between year rules')
    }
  }
  for (geography in names(catalog$geographies)) {
    spec <- catalog$geographies[[geography]]
    check_range(spec)
    for (rule in spec$vintage_rules) check_range(rule)
    for (year in rule_years(spec)) {
      select_rule(spec$vintage_rules, year, geography)
      for (name in names(catalog$measures)) {
        measure <- catalog$measures[[name]]
        rule <- select_rule(measure$rules, year, name, allow_missing = TRUE)
        if (!is.null(rule) && measure$calculation == 'inflation_adjusted') {
          select_rule(catalog$measures[[rule$source_measure]]$rules, year,
                      rule$source_measure)
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
  intersect(rule_years(catalog$geographies[[geography]]),
            unlist(lapply(catalog$measures[[name]]$rules, rule_years)))
}

geography_vintage <- function(catalog, geography, year) {
  select_rule(catalog$geographies[[geography]]$vintage_rules, year, geography)$value
}
