source('R/catalog.R')
source('R/moe.R')
source('R/calculations.R')
catalog <- read_catalog()
cpi <- read.csv(catalog$dataset$inflation$file)
for (year in 2010:2023) {
  source <- data.frame(estimate = c(200000, 75000, NA_real_, 12345),
                       moe = c(12345, 6789, 42, NA_real_))
  result <- adjust_for_inflation(source, cpi, year, 2010)
  ratio <- cpi$annual_cpi[cpi$year == 2010] / cpi$annual_cpi[cpi$year == year]
  stopifnot(identical(is.na(result$estimate), is.na(source$estimate)),
            identical(is.na(result$moe), is.na(source$moe)),
            isTRUE(all.equal(result$estimate, source$estimate * ratio)),
            isTRUE(all.equal(result$moe, source$moe * ratio)),
            isTRUE(all.equal(result$estimate - result$moe, (source$estimate - source$moe) * ratio)),
            isTRUE(all.equal(result$estimate + result$moe, (source$estimate + source$moe) * ratio)))
}
stopifnot(inherits(tryCatch(adjust_for_inflation(source, cpi, 2024, 2010), error = identity), 'error'))
cat('Inflation-adjusted estimates, MOEs, intervals, missingness, and CPI checks pass.\n')
