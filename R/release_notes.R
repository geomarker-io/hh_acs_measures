# Render the release's own catalog, so browsing documentation stays tied to the
# data snapshot. Generated notes live beside the assets under ignored data/.
markdown_cell <- function(x) {
  x <- gsub('&', '&amp;', x, fixed = TRUE)
  x <- gsub('<', '&lt;', x, fixed = TRUE)
  x <- gsub('>', '&gt;', x, fixed = TRUE)
  x <- gsub('|', '\\|', x, fixed = TRUE)
  gsub('[[:space:]]+', ' ', x)
}

catalog_year_label <- function(years) {
  if (length(years) == 1L) {
    as.character(years)
  } else {
    paste(range(years), collapse = '–')
  }
}

release_notes_markdown <- function(catalog) {
  release <- catalog$release
  if (is.null(release)) {
    stop('Release notes require a release catalog, including release metadata')
  }
  geography <- release$geography
  geo <- catalog$geographies[[geography]]
  years <- rule_years(geo)
  repo <- 'https://github.com/geomarker-io/hh_acs_measures'
  download <- paste0(repo, '/releases/download/', release$tag, '/')
  stem <- sub('\\.parquet$', '', release$data_file)
  lines <- c(
    paste0('# ', markdown_cell(catalog$dataset$title), ' — ', release$version),
    '',
    paste0(
      length(catalog$measures),
      ' measures with ',
      catalog$dataset$moe_level,
      '% margins of error; ACS five-year estimates ending in ',
      catalog_year_label(years),
      '.'
    ),
    '',
    paste0(
      'One row per census tract and ACS ending year; ',
      format(release$rows, big.mark = ',', scientific = FALSE, trim = TRUE),
      ' rows.'
    ),
    '',
    paste0(
      '[Parquet data](',
      download,
      release$data_file,
      ') · ',
      '[YAML catalog](',
      download,
      stem,
      '.measures.yml) · ',
      '[CPI inputs](',
      download,
      catalog$dataset$inflation$file,
      ')'
    ),
    '',
    '## Measure catalog',
    '',
    paste0(
      'Join on `',
      geo$id,
      '` and `year`. Years below are the ending years ',
      'of five-year ACS estimates. Every measure also has a `<field>_moe` column.'
    ),
    ''
  )
  domains <- vapply(catalog$measures, function(spec) spec$domain, character(1))
  for (domain in unique(domains)) {
    lines <- c(
      lines,
      paste0('### ', markdown_cell(domain)),
      '',
      '| Field | Definition | Units | Years |',
      '| --- | --- | --- | --- |'
    )
    for (name in names(catalog$measures)[domains == domain]) {
      spec <- catalog$measures[[name]]
      lines <- c(
        lines,
        paste0(
          '| `',
          name,
          '` | ',
          markdown_cell(spec$definition),
          ' | ',
          markdown_cell(spec$units),
          ' | ',
          catalog_year_label(measure_years(catalog, name, geography)),
          ' |'
        )
      )
    }
    lines <- c(lines, '')
  }
  lines <- c(
    lines,
    '<details>',
    '<summary>Source variables and calculation details</summary>',
    ''
  )
  for (name in names(catalog$measures)) {
    spec <- catalog$measures[[name]]
    rounding <- switch(
      spec$rounding$method,
      integer = 'nearest integer',
      decimal = paste(spec$rounding$digits, 'decimal places'),
      significant = paste(spec$rounding$digits, 'significant digits')
    )
    lines <- c(
      lines,
      paste0('### `', name, '` — ', markdown_cell(spec$title)),
      '',
      paste0(
        'Calculation: `',
        spec$calculation,
        '`. Estimate rounding: ',
        rounding,
        '.'
      ),
      '',
      paste0('MOE: ', markdown_cell(spec$moe_method)),
      '',
      '| Years | Source variables or source measure | Denominator |',
      '| --- | --- | --- |'
    )
    for (rule in spec$rules) {
      applicable <- intersect(years, rule_years(rule))
      if (!length(applicable)) {
        next
      }
      variables <- if (spec$calculation == 'inflation_adjusted') {
        rule$source_measure
      } else {
        unlist(rule$variables)
      }
      source <- paste(paste0('`', variables, '`'), collapse = ' + ')
      denominator <- if (is.null(rule$denominator)) {
        '—'
      } else {
        paste0('`', rule$denominator, '`')
      }
      lines <- c(
        lines,
        paste0(
          '| ',
          catalog_year_label(applicable),
          ' | ',
          source,
          ' | ',
          denominator,
          ' |'
        )
      )
    }
    lines <- c(lines, '')
    notes <- c(
      unlist(spec$notes),
      unlist(lapply(spec$rules, function(rule) {
        if (length(intersect(years, rule_years(rule)))) rule$note else NULL
      }))
    )
    if (length(notes)) lines <- c(lines, paste0('- ', markdown_cell(notes)), '')
  }
  lines <- c(
    lines,
    '</details>',
    '',
    '## Interpretation and sources',
    '',
    paste0('- ', markdown_cell(unlist(catalog$dataset$notes))),
    paste0('- ', markdown_cell(unlist(geo$notes))),
    '',
    paste0(
      'Original data: [U.S. Census Bureau, ACS five-year Summary Files](',
      catalog$dataset$source$documentation,
      '). ',
      'Inflation inputs: [U.S. Bureau of Labor Statistics, R-CPI-U-RS](',
      catalog$dataset$inflation$source,
      '); adjusted dollars use ',
      catalog$dataset$inflation$base_year,
      ' as the base year.'
    ),
    '',
    'The original federal source data are public domain in the United States. Original project code, documentation, and copyrightable dataset contributions use the MIT license.',
    '',
    '## Suggested citation',
    '',
    paste0(
      'Harmonized Historical ACS Measures contributors. *',
      markdown_cell(catalog$dataset$title),
      '* (version ',
      release$version,
      ') [Data set]. geomarker.io. ',
      repo,
      '/releases/tag/',
      release$tag
    ),
    ''
  )
  lines
}
