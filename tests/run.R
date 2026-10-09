# Run from the repository root: Rscript tests/run.R
files <- sort(list.files('tests', '^test-.*[.]R$', full.names = TRUE))
for (file in files) {
  cat('\n', file, '\n', sep = '')
  source(file, local = new.env(parent = globalenv()))
}
