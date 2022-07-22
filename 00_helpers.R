#### functions that will eventually live in another package

## all based around the Frictionless Tabular Data Resource Specification

# based on the deprecated rlang::set_attrs()
## move to CODECtools ??
set_attrs <- function(.x, ...) {
  attrs <- rlang::dots_list(...)
  attributes(.x) <- c(attributes(.x), attrs)
  .x
}

set_col_attrs <- function(.x, var, ...) {
  .x[[rlang::enexpr(var)]] <-
    set_attrs(dplyr::pull(.x, {{ var }}), ...)
  .x
}

class_type_cw <- c(
  "character" = "string",
  "Date" = "date",
  "numeric" = "number",
  "difftime" = "number",
  "factor" = "string",
  "hms,difftime" = "time",
  "integer" = "integer",
  "logical" = "boolean",
  "POSIXct,POSIXt" = "datetime"
  # TODO support for spatial columns?
)

#' automatically add "type" attributes for all columns in a data.frame based on their class
add_type_attrs <- function(.x) {
  col_classes <- purrr::map_chr(.x, ~ paste(class(.), collapse = ","))
  col_frictionless_classes <- class_type_cw[col_classes]

  # TODO add into above function the special case of factor levels as constraints
  ## enum <- levels(x)
  ## list(name = name, type = type, constraints = list(enum = enum))

  out <- purrr::map2_dfc(.x, col_frictionless_classes, ~ set_attrs(..1, type = ..2))
  attributes(out) <- attributes(.x)
  return(out)
}


#' make metadata list based on attributes of data.frame (including schema based on attributes of columns)
make_metadata_from_attr <- function(.x, schema = TRUE) {
  metadata_list <- as.list(attributes(.x))
  # TODO only keep frictionless-relevant attributes
  metadata_list$names <- NULL
  metadata_list$row.names <- NULL
  metadata_list$class <- NULL
  metadata_list$tigris <- NULL
  if (schema) {
    metadata_list$schema <- list(fields = purrr::map(.x, attributes))
  }
  return(metadata_list)
}

#' save (table- and column-specific) metadata to a file
write_metadata <- function(.x, file = "tabular-data-resource.yaml") {
  # TODO automatically make schema metadata from col classes if it doesn't already exist
  .x |>
    set_attrs(profile = "tabular-data-resource") |>
    make_metadata_from_attr() |>
    yaml::as.yaml() |>
    cat(file = file)
}

# TODO wrapper save function that extracts metadata from attributes and then saves the data as a CSV file and the metadata as a datapackage.json

# could we make a cool print method for a data.frame with codec attributes?
# or with codec variable names?

# TODO add function to read metadata (and schema) and create a 'pretty' data dictionary / readme / about file
