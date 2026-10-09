source('R/build.R')
source('tests/fixtures/acs.R')
catalog <- read_catalog()
d <- build_measure_data(catalog, 'zcta', fixture_scaffold('zcta'),
                        fixture_acs('zcta'), read.csv(catalog$dataset$inflation$file))
for (year in c(2019, 2020, 2021, 2023)) {
  result <- d[d$year == year, ]
  expected_ids <- if (year <= 2020) c('12345', '23456', '99999') else c('12345', '34567', '99999')
  stopifnot(identical(result$zcta, expected_ids),
            all(result$zcta_vintage == if (year <= 2020) '2010' else '2020'),
            !anyDuplicated(result[c('zcta', 'zcta_vintage', 'year')]))
  measures <- setdiff(names(result), c('zcta', 'zcta_vintage', 'year'))
  stopifnot(all(is.finite(as.matrix(result[result$zcta != '99999', measures]))),
            all(is.na(as.matrix(result[result$zcta == '99999', measures]))))
}
cat('All 30 ZCTA measures pass vintage/scaffold checks for 2019, 2020, 2021, and 2023.\n')
