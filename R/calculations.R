# All calculators consume the same ACS long-table input. get_acs is injected so
# calculation tests can use fixtures without a Census key or network access.
calculate_acs_measure <- function(spec, rule, year, get_acs) {
  variables <- unlist(rule$variables, use.names = FALSE)
  args <- list(variables = variables, year = year)
  if (spec$calculation == 'proportion') args$summary_var <- rule$denominator
  raw <- do.call(get_acs, args)
  if (anyDuplicated(raw[c('GEOID', 'variable')])) stop('Duplicate ACS variable rows')
  groups <- split(raw, raw$GEOID)
  rows <- lapply(groups, function(d) {
    if (!setequal(d$variable, variables)) stop('Missing or unexpected ACS components')
    estimate <- sum(d$estimate)
    moe <- if (length(variables) == 1L) d$moe[[1]] else
      tidycensus::moe_sum(d$moe, d$estimate)
    if (spec$calculation == 'proportion') {
      denominator <- unique(d$summary_est)
      denominator_moe <- unique(d$summary_moe)
      if (length(denominator) != 1L || length(denominator_moe) != 1L) {
        stop('Inconsistent ACS denominator within GEOID')
      }
      if (is.na(denominator) || denominator <= 0) {
        estimate <- moe <- NA_real_
      } else {
        moe <- tidycensus::moe_prop(estimate, denominator, moe, denominator_moe)
        estimate <- estimate / denominator
      }
    }
    data.frame(GEOID = d$GEOID[[1]], estimate = estimate, moe = moe)
  })
  if (!length(rows)) return(data.frame(GEOID = character(), estimate = numeric(), moe = numeric()))
  do.call(rbind, unname(rows))
}

adjust_for_inflation <- function(d, cpi, year, base_year) {
  current <- cpi$annual_cpi[cpi$year == year]
  base <- cpi$annual_cpi[cpi$year == base_year]
  if (length(current) != 1L || length(base) != 1L ||
      !is.finite(current) || !is.finite(base) || current <= 0 || base <= 0) {
    stop('Missing or invalid CPI for ', year, ' or base year ', base_year)
  }
  d$estimate <- d$estimate * base / current
  d$moe <- d$moe * base / current
  d
}

round_measure <- function(x, spec) {
  switch(spec$rounding$method,
         integer = as.integer(round(x)),
         decimal = round(x, spec$rounding$digits),
         significant = signif(x, spec$rounding$digits))
}

build_measure_data <- function(catalog, geography, scaffold, get_acs, cpi) {
  geo <- catalog$geographies[[geography]]
  if (is.null(geo)) stop('Unknown geography: ', geography)
  keys <- c(geo$id, geo$vintage, 'year')
  if (anyDuplicated(scaffold[c(geo$id, geo$vintage)])) stop('Duplicate scaffold keys')
  # The scaffold determines the rows, including years where a measure is absent.
  output <- dplyr::bind_rows(lapply(rule_years(geo), function(year) {
    vintage <- geography_vintage(catalog, geography, year)
    d <- scaffold[scaffold[[geo$vintage]] == vintage, , drop = FALSE]
    d$year <- year
    d
  }))
  names_order <- names(catalog$measures)
  # Derivations use unrounded source estimates and MOEs, as in the old pipeline.
  names_order <- c(names_order[vapply(catalog$measures, function(x)
    x$calculation != 'inflation_adjusted', logical(1))],
    names_order[vapply(catalog$measures, function(x)
      x$calculation == 'inflation_adjusted', logical(1))])
  computed <- list()
  for (name in names_order) {
    spec <- catalog$measures[[name]]
    parts <- lapply(measure_years(catalog, name, geography), function(year) {
      rule <- select_rule(spec$rules, year, name)
      if (spec$calculation == 'inflation_adjusted') {
        d <- computed[[rule$source_measure]]
        d <- d[d$year == year, c('GEOID', 'estimate', 'moe'), drop = FALSE]
        d <- adjust_for_inflation(d, cpi, year, catalog$dataset$inflation$base_year)
      } else {
        d <- calculate_acs_measure(spec, rule, year, get_acs)
      }
      d$year <- rep(year, nrow(d))
      d
    })
    d <- dplyr::bind_rows(parts)
    computed[[name]] <- d
    names(d)[names(d) == 'GEOID'] <- geo$id
    d[[geo$vintage]] <- vapply(d$year, function(year)
      geography_vintage(catalog, geography, year), character(1))
    d[[name]] <- round_measure(d$estimate, spec)
    d[[paste0(name, '_moe')]] <- round_measure(d$moe, spec)
    d <- d[c(keys, name, paste0(name, '_moe'))]
    output <- dplyr::left_join(output, d, by = keys)
  }
  output[c(keys, unlist(lapply(names(catalog$measures), function(name)
    c(name, paste0(name, '_moe')))))]
}
