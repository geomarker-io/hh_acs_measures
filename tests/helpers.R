# Small synthetic files shared by the parser and source-inventory tests.
expect_error <- function(expr, message) {
  error <- tryCatch(force(expr), error = identity)
  stopifnot(
    inherits(error, 'error'),
    grepl(message, conditionMessage(error), fixed = TRUE)
  )
}

write_lookup_fixture <- function(path, alternate_headers = FALSE) {
  header <- if (alternate_headers) {
    'File Name,Table ID,Sequence Number,Line Number,Start Position,Total Cells,Total,Label,Subject Area'
  } else {
    'File,Table,Sequence,Line,Start,Cells,Total,Label,Subject'
  }
  writeLines(
    c(
      header,
      'ACSSF,B01001,1,,7,1,1,Sex by age,Population',
      'ACSSF,B01001,1,1,,,,Total population,Population',
      'ACSSF,B11005,1,,8,1,1,Household type,Households',
      'ACSSF,B11005,1,1,,,,Total households,Households'
    ),
    path
  )
  path
}

write_geography_fixture <- function(
  path,
  ids,
  keys,
  levels = '140',
  components = '00'
) {
  rows <- lapply(seq_along(ids), function(i) {
    values <- rep('', 49L)
    values[c(1L, 3L, 4L, 5L, 49L)] <- c(
      'ACSSF',
      rep_len(levels, length(ids))[i],
      rep_len(components, length(ids))[i],
      keys[i],
      paste0('1400000US', ids[i])
    )
    paste(values, collapse = ',')
  })
  writeLines(unlist(rows), path)
  path
}

write_archive_fixture <- function(directory, estimates, moes, year = 2020L) {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  paths <- file.path(
    directory,
    sprintf('%s%d5al0001000.txt', c('e', 'm'), year)
  )
  writeLines(estimates, paths[1])
  if (!is.null(moes)) {
    writeLines(moes, paths[2])
  } else {
    paths <- paths[1]
  }
  archive <- file.path(directory, 'sequence.zip')
  utils::zip(archive, paths, flags = '-jq')
  archive
}
