# Run from the repository root: Rscript tests/test-release-notes.R
source('R/catalog.R')
source('R/release_notes.R')
source('tests/helpers.R')

catalog <- read_catalog()
expect_error(release_notes_markdown(catalog), 'require a release catalog')
catalog$dataset$inflation$file <- 'hh_acs_measures-v3.0.0.CPI.csv'
catalog$release <- list(
  name = 'hh_acs_measures',
  version = '3.0.0',
  tag = 'v3.0.0',
  geography = 'tract',
  data_file = 'hh_acs_measures-v3.0.0.parquet',
  rows = 1152605L
)
original <- catalog
notes <- release_notes_markdown(catalog)
stopifnot(
  identical(catalog, original),
  identical(notes, release_notes_markdown(catalog)),
  sum(grepl('^\\| `', notes)) == 30L,
  all(vapply(
    names(catalog$measures),
    function(name) {
      sum(startsWith(notes, paste0('| `', name, '` | '))) == 1L
    },
    logical(1)
  )),
  any(grepl('1,152,605 rows', notes, fixed = TRUE)),
  any(
    grepl('fraction_lesh` | Limited English', notes, fixed = TRUE) &
      grepl('2016–2024', notes, fixed = TRUE)
  ),
  any(grepl('2010–2014 | `B25034_007`', notes, fixed = TRUE)),
  any(grepl('2015–2024 | `B25034_008`', notes, fixed = TRUE)),
  any(grepl('`median_income` | — |', notes, fixed = TRUE)),
  any(grepl('/v3.0.0/hh_acs_measures-v3.0.0.CPI.csv', notes, fixed = TRUE)),
  any(grepl('boundaries are not harmonized', notes, fixed = TRUE)),
  sum(notes == '<details>') == 1L,
  sum(notes == '</details>') == 1L
)
# A table cell must remain one cell even when catalog prose has pipes/newlines
# or text that could be mistaken for HTML.
catalog$measures$n_pop$definition <- 'People | households\n<18 & >0'
notes <- release_notes_markdown(catalog)
stopifnot(any(grepl(
  'People \\| households &lt;18 &amp; &gt;0',
  notes,
  fixed = TRUE
)))
# A release subset displays only applicable years and omits historical rules
# outside its coverage, rather than inheriting the catalog's full rule ranges.
catalog$geographies$tract$years <- c(2021L, 2023L)
notes <- release_notes_markdown(catalog)
stopifnot(
  !any(grepl('2010–2014', notes, fixed = TRUE)),
  any(grepl('2021–2023 | `B25034_008`', notes, fixed = TRUE)),
  all(grepl('2021–2023', notes[grepl('^\\| `', notes)], fixed = TRUE))
)
cat(
  'Release-note tests passed: measure coverage, source-rule changes, pinned links, escaping, deterministic output and release-year limits.\n'
)
