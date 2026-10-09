# Documentation-only build: no Census key, downloads, or data calculations.
source('R/catalog.R')
source('R/metadata.R')
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 1L) stop('Usage: Rscript build_metadata.R [OUTPUT_DIRECTORY]')
write_documentation(read_catalog(), if (length(args)) args[1] else '.')
