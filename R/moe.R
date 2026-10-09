# ACS approximation formulas, independent of a data-access package.
# Census guidance: https://www2.census.gov/programs-surveys/acs/tech_docs/accuracy/MultiyearACSAccuracyofData2023.pdf
# MOEs are scaled to the requested confidence level before any derivations.
acs_moe_factor <- function(level) {
  critical <- c(`90` = 1.645, `95` = 1.960, `99` = 2.576)
  if (length(level) != 1L || is.na(level) || !as.character(level) %in% names(critical)) {
    stop('MOE confidence level must be 90, 95, or 99')
  }
  unname(critical[as.character(level)] / critical['90'])
}

acs_moe_sum <- function(moe, estimate) {
  if (length(moe) != length(estimate)) stop('Estimate and MOE lengths differ')
  # Multiple zero-count categories contribute only their largest MOE.
  zero <- !is.na(estimate) & estimate == 0
  terms <- if (any(zero)) c(max(moe[zero]), moe[!zero]) else moe
  sqrt(sum(terms^2))
}

acs_moe_proportion <- function(num, denom, moe_num, moe_denom) {
  if (length(unique(lengths(list(num, denom, moe_num, moe_denom)))) != 1L) {
    stop('Proportion inputs must have equal lengths')
  }
  proportion <- num / denom
  subtract <- moe_num^2 - proportion^2 * moe_denom^2
  # Retain the previous calculator's ratio fallback for nonpositive radicands.
  variance <- ifelse(!is.na(subtract) & subtract > 0, subtract,
                     moe_num^2 + proportion^2 * moe_denom^2)
  result <- sqrt(variance) / denom
  result[is.na(denom) | denom <= 0] <- NA_real_
  result
}
