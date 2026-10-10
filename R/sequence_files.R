# Historical Summary Files put multiple tables in each sequence. The annual
# lookup supplies the table offsets; estimate and MOE files share LOGRECNO keys.
sequence_lookup_url <- function(year) {
  if (length(year) != 1L || is.na(year) || !year %in% 2010:2020) {
    stop('Sequence-based downloads support ACS years 2010 through 2020')
  }
  suffix <- if (year <= 2012L) {
    '5_year/user_tools/Sequence_Number_and_Table_Number_Lookup.txt'
  } else {
    'user_tools/ACS_5yr_Seq_Table_Number_Lookup.txt'
  }
  paste0(
    'https://www2.census.gov/programs-surveys/acs/summary_file/',
    year,
    '/documentation/',
    suffix
  )
}

read_sequence_lookup <- function(path) {
  d <- data.table::fread(
    path,
    colClasses = 'character',
    na.strings = NULL,
    encoding = 'Latin-1',
    data.table = FALSE,
    showProgress = FALSE
  )
  # The 2011 headers differ; the nine column positions have the same meanings.
  if (ncol(d) != 9L) {
    stop('Unexpected sequence lookup layout')
  }
  names(d) <- c(
    'file',
    'table',
    'sequence',
    'line',
    'start',
    'cells',
    'total',
    'label',
    'subject'
  )
  d$sequence <- as.integer(d$sequence)
  d$line <- suppressWarnings(as.numeric(d$line))
  d$start <- suppressWarnings(as.integer(d$start))
  groups <- split(d, paste(d$table, d$sequence, sep = '-'))
  output <- lapply(groups, function(x) {
    start <- unique(x$start[!is.na(x$start)])
    lines <- x[!is.na(x$line) & x$line == as.integer(x$line), , drop = FALSE]
    if (!nrow(lines)) {
      return(NULL)
    }
    if (length(start) != 1L || start < 7L) {
      stop('Invalid sequence table offset')
    }
    data.frame(
      variable = sprintf('%s_%03d', lines$table, as.integer(lines$line)),
      table = lines$table,
      sequence = lines$sequence,
      position = start + as.integer(lines$line) - min(lines$line),
      label = lines$label
    )
  })
  output <- dplyr::bind_rows(output)
  output
}

sequence_units <- function(catalog, geography) {
  if (geography == 'zcta') {
    return(data.frame(
      state = 'US',
      abbreviation = 'us',
      directory = 'UnitedStates'
    ))
  }
  state_fips <- c(
    1,
    2,
    4,
    5,
    6,
    8,
    9,
    10,
    12,
    13,
    15,
    16,
    17,
    18,
    19,
    20,
    21,
    22,
    23,
    24,
    25,
    26,
    27,
    28,
    29,
    30,
    31,
    32,
    33,
    34,
    35,
    36,
    37,
    38,
    39,
    40,
    41,
    42,
    44,
    45,
    46,
    47,
    48,
    49,
    50,
    51,
    53,
    54,
    55,
    56
  )
  units <- data.frame(
    state = sprintf('%02d', state_fips),
    abbreviation = tolower(state.abb),
    directory = gsub(' ', '', state.name)
  )
  units <- rbind(
    units,
    data.frame(
      state = '11',
      abbreviation = 'dc',
      directory = 'DistrictOfColumbia'
    )
  )
  wanted <- catalog$geographies[[geography]]$state_fips
  index <- match(wanted, units$state)
  if (anyNA(index)) {
    stop('Unknown state in historical tract coverage')
  }
  units[index, , drop = FALSE]
}

sequence_data_url <- function(year, geography, unit, sequence = NULL) {
  sequence_lookup_url(year) # Check the supported range.
  part <- if (geography == 'tract') {
    'Tracts_Block_Groups_Only'
  } else {
    'All_Geographies_Not_Tracts_Block_Groups'
  }
  file <- if (is.null(sequence)) {
    paste0('g', year, '5', unit$abbreviation, '.csv')
  } else {
    sprintf('%d5%s%04d000.zip', year, unit$abbreviation, sequence)
  }
  paste0(
    'https://www2.census.gov/programs-surveys/acs/summary_file/',
    year,
    '/data/5_year_seq_by_state/',
    unit$directory,
    '/',
    part,
    '/',
    file
  )
}

read_sequence_geography <- function(path, geography) {
  d <- data.table::fread(
    path,
    header = FALSE,
    sep = ',',
    select = c(3L, 4L, 5L, 49L),
    colClasses = 'character',
    na.strings = NULL,
    encoding = 'Latin-1',
    data.table = FALSE,
    showProgress = FALSE
  )
  names(d) <- c('level', 'component', 'LOGRECNO', 'GEO_ID')
  level <- if (geography == 'tract') '140' else '860'
  d <- d[d$level == level & d$component == '00', , drop = FALSE]
  d$GEOID <- sub('^.*US', '', d$GEO_ID)
  width <- if (geography == 'tract') 11L else 5L
  if (
    !nrow(d) ||
      any(nchar(d$GEOID) != width) ||
      anyDuplicated(d$LOGRECNO) ||
      anyDuplicated(d$GEOID)
  ) {
    stop('Invalid historical geography records')
  }
  d[c('LOGRECNO', 'GEOID')]
}

read_sequence_archive <- function(path, mapping, geo, year, unit) {
  files <- utils::unzip(path, list = TRUE)$Name
  directory <- tempfile('hh-acs-sequence-')
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  output <- data.frame(GEOID = geo$GEOID)
  for (prefix in c('e', 'm')) {
    target <- sprintf(
      '%s%d5%s%04d000.txt',
      prefix,
      year,
      unit$abbreviation,
      unique(mapping$sequence)
    )
    file <- files[basename(files) == target]
    if (length(file) != 1L) {
      stop('Missing estimate/MOE sequence file: ', target)
    }
    utils::unzip(path, files = file, exdir = directory)
    columns <- c(5L, 6L, mapping$position)
    d <- data.table::fread(
      file.path(directory, file),
      header = FALSE,
      sep = ',',
      select = columns,
      colClasses = 'character',
      na.strings = NULL,
      data.table = FALSE,
      showProgress = FALSE
    )
    if (ncol(d) != length(columns) || anyDuplicated(d[[2]])) {
      stop('Invalid sequence data columns or LOGRECNO keys')
    }
    if (any(as.integer(d[[1]]) != unique(mapping$sequence))) {
      stop('Unexpected sequence number')
    }
    index <- match(geo$LOGRECNO, d[[2]])
    if (anyNA(index)) {
      stop('Geography records missing from sequence data')
    }
    for (i in seq_len(nrow(mapping))) {
      name <- sub(
        '_',
        if (prefix == 'e') '_E' else '_M',
        mapping$variable[i],
        fixed = TRUE
      )
      output[[name]] <- acs_numeric(d[[i + 2L]][index])
    }
  }
  output
}

sequence_file_reader <- function(catalog, geography, offline, download) {
  lookups <- new.env(parent = emptyenv())
  geographies <- new.env(parent = emptyenv())
  sequences <- new.env(parent = emptyenv())
  units <- sequence_units(catalog, geography)
  function(variables, year) {
    key <- as.character(year)
    if (!exists(key, lookups, inherits = FALSE)) {
      path <- download(
        sequence_lookup_url(year),
        paste0('summary-files/', year, '/lookup'),
        offline = offline
      )
      assign(key, read_sequence_lookup(path), lookups)
    }
    lookup <- get(key, lookups, inherits = FALSE)
    needed <- unique(c(required_acs_variables(catalog, year), variables))
    if (anyDuplicated(lookup$variable[lookup$variable %in% needed])) {
      stop('Ambiguous required variables in sequence lookup')
    }
    mapping <- lookup[match(needed, lookup$variable), , drop = FALSE]
    if (anyNA(mapping$variable)) {
      stop(
        'Missing historical source variables: ',
        paste(setdiff(needed, lookup$variable), collapse = ', ')
      )
    }
    requested <- mapping[mapping$variable %in% variables, , drop = FALSE]
    pieces <- lapply(seq_len(nrow(units)), function(i) {
      unit <- units[i, , drop = FALSE]
      geo_key <- paste(year, unit$abbreviation, sep = '-')
      if (!exists(geo_key, geographies, inherits = FALSE)) {
        path <- download(
          sequence_data_url(year, geography, unit),
          paste0(
            'summary-files/',
            year,
            '/',
            geography,
            '/',
            unit$abbreviation
          ),
          offline = offline
        )
        assign(geo_key, read_sequence_geography(path, geography), geographies)
      }
      geo <- get(geo_key, geographies, inherits = FALSE)
      output <- data.frame(GEOID = geo$GEOID)
      for (sequence in unique(requested$sequence)) {
        sequence_key <- paste(geo_key, sequence, sep = '-')
        if (!exists(sequence_key, sequences, inherits = FALSE)) {
          # Read every catalog variable in this sequence once, even when several
          # measures use different tables stored in the same archive.
          all_mapping <- mapping[mapping$sequence == sequence, , drop = FALSE]
          path <- download(
            sequence_data_url(year, geography, unit, sequence),
            paste0(
              'summary-files/',
              year,
              '/',
              geography,
              '/',
              unit$abbreviation
            ),
            offline = offline
          )
          assign(
            sequence_key,
            read_sequence_archive(path, all_mapping, geo, year, unit),
            sequences
          )
        }
        d <- get(sequence_key, sequences, inherits = FALSE)
        vars <- requested$variable[requested$sequence == sequence]
        columns <- c(
          sub('_', '_E', vars, fixed = TRUE),
          sub('_', '_M', vars, fixed = TRUE)
        )
        output[columns] <- d[columns]
      }
      output
    })
    dplyr::bind_rows(pieces)
  }
}

sequence_source_files <- function(catalog, geography, year) {
  path <- stow_source_file(
    sequence_lookup_url(year),
    paste0('summary-files/', year, '/lookup'),
    offline = nzchar(Sys.getenv('HH_ACS_OFFLINE'))
  )
  lookup <- read_sequence_lookup(path)
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
