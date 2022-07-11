library(dplyr)
library(tidyr)

d <- arrow::read_parquet(fs::path("data", "harmonized_historical_acs_data.parquet"))
d <- arrow::read_feather(fs::path("data", "harmonized_historical_acs_data.feather"))

## # only read some of the columns from a file:
## arrow::read_parquet("data/harmonized_historical_acs_data.parquet",
##   col_select = c(census_tract_id_2010, year)
## )

# dataset metadata
attr(d, "codec")

# e.g.
attr(d, "codec") |>
  tibble::enframe() |>
  unnest(cols = c(value)) |>
  knitr::kable()

# column metadata:
purrr::map_dfr(d, attributes) |>
  mutate(name = attr(d, "names")) |>
  relocate(name) |>
  knitr::kable()

d |>
  group_by(year) |>
  summarise(across(c(fraction_insured, fraction_poverty, fraction_snap),
    mean,
    na.rm = TRUE
  ))


toi <- c("39061006600", "39061006800", "39061006900")

d_toi <-
  d |>
  filter(census_tract_id_2010 %in% toi) |>
  tidyr::pivot_longer(c(
    fraction_insured,
    fraction_poverty,
    fraction_snap
  ))
  
library(ggplot2)            

ggplot(d_toi, aes(year, value)) +
  geom_line(aes(color = census_tract_id_2010)) +
  facet_wrap(~name)
