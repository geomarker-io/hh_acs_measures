# Prepare source metadata before writing a release. Historical inventories need
# lookup files; download policy is explicit and injectable for offline fixtures.
summary_source_files <- function(
  catalog,
  geography,
  offline = FALSE,
  download = stow_source_file
) {
  dplyr::bind_rows(lapply(
    rule_years(catalog$geographies[[geography]]),
    function(year) {
      if (year < 2021L) {
        path <- download(
          sequence_lookup_url(year),
          paste0('summary-files/', year, '/lookup'),
          offline = offline
        )
        return(sequence_source_files(
          catalog,
          geography,
          year,
          read_sequence_lookup(path)
        ))
      }
      tables <- unique(sub('_.*', '', required_acs_variables(catalog, year)))
      data.frame(
        year = year,
        table = tables,
        url = vapply(
          tables,
          function(table) summary_file_url(year, table),
          character(1)
        )
      )
    }
  ))
}

# A parsed lookup makes historical URL enumeration independent of file access.
sequence_source_files <- function(catalog, geography, year, lookup) {
  variables <- required_acs_variables(catalog, year)
  if (anyDuplicated(lookup$variable[lookup$variable %in% variables])) {
    stop('Ambiguous variables in historical source inventory')
  }
  mapping <- lookup[match(variables, lookup$variable), , drop = FALSE]
  if (anyNA(mapping$variable)) {
    stop('Missing variables in historical source inventory')
  }
  units <- sequence_units(catalog, geography)
  files <- dplyr::bind_rows(lapply(seq_len(nrow(units)), function(i) {
    unit <- units[i, , drop = FALSE]
    dplyr::bind_rows(
      data.frame(
        year = year,
        type = 'geography',
        state = unit$state,
        url = sequence_data_url(year, geography, unit)
      ),
      lapply(unique(mapping$sequence), function(sequence) {
        data.frame(
          year = year,
          type = 'sequence',
          state = unit$state,
          sequence = sequence,
          table = paste(
            unique(mapping$table[mapping$sequence == sequence]),
            collapse = ','
          ),
          url = sequence_data_url(year, geography, unit, sequence)
        )
      })
    )
  }))
  dplyr::bind_rows(
    data.frame(year = year, type = 'lookup', url = sequence_lookup_url(year)),
    files
  )
}
