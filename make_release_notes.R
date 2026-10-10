#!/usr/bin/env Rscript

source('R/catalog.R')
source('R/release_notes.R')

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) {
  stop('Usage: Rscript make_release_notes.R PATH_TO_RELEASE.measures.yml')
}
catalog <- read_catalog(args[1])
notes <- release_notes_markdown(catalog)
path <- file.path(
  dirname(args[1]),
  paste0(catalog$release$name, '-v', catalog$release$version, '.notes.md')
)
writeLines(notes, path)
message('Generated release notes: ', path)
