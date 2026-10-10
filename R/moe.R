# ACS approximation formulas, independent of a data-access package.
# Census guidance: https://www2.census.gov/programs-surveys/acs/tech_docs/accuracy/MultiyearACSAccuracyofData2023.pdf
# Inputs retain the published 90% confidence level.
acs_moe_proportion <- function(num, denom, moe_num, moe_denom) {
  if (length(unique(lengths(list(num, denom, moe_num, moe_denom)))) != 1L) {
    stop('Proportion inputs must have equal lengths')
  }
  proportion <- num / denom
  subtract <- moe_num^2 - proportion^2 * moe_denom^2
  # Use the ratio approximation when the subset radicand is nonpositive.
  variance <- ifelse(
    !is.na(subtract) & subtract > 0,
    subtract,
    moe_num^2 + proportion^2 * moe_denom^2
  )
  result <- sqrt(variance) / denom
  result[is.na(denom) | denom <= 0] <- NA_real_
  result
}
