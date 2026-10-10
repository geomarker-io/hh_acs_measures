# All calculators consume the same ACS long-table input. The input reader is
# supplied separately to keep file access independent of the calculations.
calculate_acs_measure <- function(spec, rule, year, get_acs) {
  variables <- unlist(rule$variables, use.names = FALSE)
  args <- list(variables = variables, year = year)
  if (spec$calculation == 'proportion') {
    args$summary_var <- rule$denominator
  }
  raw <- do.call(get_acs, args)
  if (anyDuplicated(raw[c('GEOID', 'variable')])) {
    stop('Duplicate ACS variable rows')
  }
  if (
    !setequal(raw$variable, variables) ||
      any(table(raw$GEOID) != length(variables))
  ) {
    stop('Missing or unexpected ACS components')
  }
  if (length(variables) == 1L) {
    result <- raw[c('GEOID', 'estimate', 'moe')]
  } else {
    # Aggregate in vectorized groups rather than constructing one R data frame
    # per tract. Zero-count terms retain only their largest MOE.
    zero <- !is.na(raw$estimate) & raw$estimate == 0
    terms <- data.table::data.table(
      GEOID = raw$GEOID,
      estimate = raw$estimate,
      nonzero_square = ifelse(zero, 0, raw$moe^2),
      zero_moe = ifelse(zero, raw$moe, 0)
    )
    sums <- terms[,
      list(
        estimate = sum(estimate),
        squares = sum(nonzero_square),
        zero = max(zero_moe)
      ),
      by = GEOID
    ]
    result <- data.frame(
      GEOID = sums$GEOID,
      estimate = sums$estimate,
      moe = sqrt(sums$squares + sums$zero^2)
    )
  }
  if (spec$calculation == 'proportion') {
    denominators <- unique(raw[c('GEOID', 'summary_est', 'summary_moe')])
    if (anyDuplicated(denominators$GEOID)) {
      stop('Inconsistent ACS denominator within GEOID')
    }
    index <- match(result$GEOID, denominators$GEOID)
    denominator <- denominators$summary_est[index]
    denominator_moe <- denominators$summary_moe[index]
    result$moe <- acs_moe_proportion(
      result$estimate,
      denominator,
      result$moe,
      denominator_moe
    )
    result$estimate <- result$estimate / denominator
    result$estimate[is.na(denominator) | denominator <= 0] <- NA_real_
  }
  result
}

adjust_for_inflation <- function(d, cpi, year, base_year) {
  current <- cpi$annual_cpi[cpi$year == year]
  base <- cpi$annual_cpi[cpi$year == base_year]
  if (
    length(current) != 1L ||
      length(base) != 1L ||
      !is.finite(current) ||
      !is.finite(base) ||
      current <= 0 ||
      base <= 0
  ) {
    stop('Missing or invalid CPI for ', year, ' or base year ', base_year)
  }
  d$estimate <- d$estimate * base / current
  d$moe <- d$moe * base / current
  d
}

round_measure <- function(x, spec) {
  switch(
    spec$rounding$method,
    integer = as.integer(round(x)),
    decimal = round(x, spec$rounding$digits),
    significant = signif(x, spec$rounding$digits)
  )
}

build_measure_data <- function(catalog, geography, get_acs, cpi) {
  geo <- catalog$geographies[[geography]]
  if (is.null(geo)) {
    stop('Unknown geography: ', geography)
  }
  keys <- c(geo$id, 'year')
  # Each table contributes its published records; retain their union by ACS year.
  output <- data.frame(id = character(), year = integer())
  names(output)[1] <- geo$id
  names_order <- names(catalog$measures)
  # Derivations use unrounded source estimates and MOEs.
  names_order <- c(
    names_order[vapply(
      catalog$measures,
      function(x) {
        x$calculation != 'inflation_adjusted'
      },
      logical(1)
    )],
    names_order[vapply(
      catalog$measures,
      function(x) {
        x$calculation == 'inflation_adjusted'
      },
      logical(1)
    )]
  )
  computed <- list()
  for (name in names_order) {
    spec <- catalog$measures[[name]]
    years <- measure_years(catalog, name, geography)
    if (!length(years)) {
      output[[name]] <- round_measure(rep(NA_real_, nrow(output)), spec)
      output[[paste0(name, '_moe')]] <- rep(NA_real_, nrow(output))
      next
    }
    parts <- lapply(years, function(year) {
      rule <- select_rule(spec$rules, year, name)
      if (spec$calculation == 'inflation_adjusted') {
        d <- computed[[rule$source_measure]]
        d <- d[d$year == year, c('GEOID', 'estimate', 'moe'), drop = FALSE]
        d <- adjust_for_inflation(
          d,
          cpi,
          year,
          catalog$dataset$inflation$base_year
        )
      } else {
        d <- calculate_acs_measure(spec, rule, year, get_acs)
      }
      d$year <- rep(year, nrow(d))
      d
    })
    d <- dplyr::bind_rows(parts)
    computed[[name]] <- d
    names(d)[names(d) == 'GEOID'] <- geo$id
    d[[name]] <- round_measure(d$estimate, spec)
    d[[paste0(name, '_moe')]] <- d$moe
    d <- d[c(keys, name, paste0(name, '_moe'))]
    output <- dplyr::full_join(output, d, by = keys)
  }
  output[c(
    keys,
    unlist(lapply(names(catalog$measures), function(name) {
      c(name, paste0(name, '_moe'))
    }))
  )]
}
