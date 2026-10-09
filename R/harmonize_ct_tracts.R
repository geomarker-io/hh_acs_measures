# Restore the 2020 tract identifiers after Connecticut adopted planning regions.
# Read as character to preserve the leading zero in state FIPS 09.
ct_tract_crosswalk <- read.csv(
  "data-raw/ct_tract_crosswalk_2022_2020.csv",
  colClasses = "character"
)

harmonize_ct_tracts <- function(d, year, crosswalk = ct_tract_crosswalk) {
  if (year < 2022) return(d)

  ct <- startsWith(d$GEOID, "09")
  match_index <- match(d$GEOID, crosswalk$geoid_2022)
  # These five water-only tracts are absent from the cartographic scaffold.
  # Leave their IDs unchanged; they will not enter the output through the join.
  water_tracts <- c("09120990000", "09190990000", "09170990000",
                    "09130990100", "09180990100")
  unmapped <- ct & is.na(match_index) & !d$GEOID %in% water_tracts
  if (any(unmapped)) {
    stop("Connecticut ACS tract IDs missing from the crosswalk: ",
         paste(unique(d$GEOID[unmapped]), collapse = ", "))
  }

  matched <- ct & !is.na(match_index)
  d$GEOID[matched] <- crosswalk$geoid_2020[match_index[matched]]
  d
}
