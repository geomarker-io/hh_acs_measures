# Deterministic, artificial ACS responses; no Census records are stored here.
fixture_scaffold <- function(geography) {
  if (geography == 'tract') {
    data.frame(census_tract_id = rep(c('01001020100', ct_tract_crosswalk$geoid_2020[1], '01001999999'), 2),
               census_tract_vintage = rep(c('2010', '2020'), each = 3))
  } else {
    data.frame(zcta = c('12345', '23456', '99999', '12345', '34567', '99999'),
               zcta_vintage = rep(c('2010', '2020'), each = 3))
  }
}

fixture_acs <- function(geography) {
  function(variables, year, summary_var = NULL) {
    ids <- if (geography == 'tract') {
      c('01001020100', if (year >= 2022) ct_tract_crosswalk$geoid_2022[1] else
        ct_tract_crosswalk$geoid_2020[1])
    } else if (year <= 2020) c('12345', '23456') else c('12345', '34567')
    d <- expand.grid(GEOID = ids, variable = variables, stringsAsFactors = FALSE)
    bin <- as.numeric(sub('.*_', '', d$variable))
    d$estimate <- bin + seq_len(nrow(d)) / 10
    d$estimate[d$variable == 'B19013_001'] <- 75432.1
    d$estimate[d$variable == 'B25077_001'] <- 203456.7
    d$estimate[d$variable == 'B25071_001'] <- 24.567
    d$moe <- 1.2 + bin / 10
    d$summary_est <- 2000.25
    d$summary_moe <- 10.75
    if (year == 2015) d$estimate[d$GEOID == ids[1]] <- NA_real_
    if (year == 2016) d$moe[d$GEOID == ids[1]] <- NA_real_
    # Direct total requests match the summary fields in component requests.
    totals <- d$variable %in% c('B01001_001', 'B11005_001')
    d$estimate[totals] <- d$summary_est[totals]
    d$moe[totals] <- d$summary_moe[totals]
    dplyr::as_tibble(d)
  }
}
