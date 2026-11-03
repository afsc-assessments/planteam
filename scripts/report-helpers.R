# Checkout-only adapter: no package installation, authentication or network I/O.
prepare_catch_report <- function(params) {
  fail <- function(message) stop(message, call. = FALSE)
  text_fields <- c("doc_title", "authors", "stock", "area", "catch_data_date",
                   "input_csv", "input_source", "snapshot_date")
  for (field in text_fields) {
    value <- params[[field]]
    if (!is.character(value) || length(value) != 1L || is.na(value) ||
        !nzchar(trimws(value))) {
      fail(paste("Supply a nonempty", field, "in the parameter YAML."))
    }
  }
  if (!is.logical(params$example) || length(params$example) != 1L ||
      is.na(params$example)) fail("example must be true or false.")
  for (field in c("year", "last_full_year", "next_full_year")) {
    value <- params[[field]]
    if (!is.numeric(value) || length(value) != 1L || !is.finite(value) ||
        value != floor(value) || value < 1900 || value > 2200) {
      fail(paste(field, "must be an integer year between 1900 and 2200."))
    }
  }
  if (params$last_full_year >= params$year || params$next_full_year <= params$year) {
    fail("A catch report requires last_full_year < year < next_full_year.")
  }
  if (!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", params$snapshot_date) ||
      is.na(as.Date(params$snapshot_date, format = "%Y-%m-%d"))) {
    fail("snapshot_date must be a quoted ISO date (YYYY-MM-DD).")
  }
  if (!file.exists(params$input_csv)) {
    fail(paste("Missing input CSV:", params$input_csv,
               "— supply an approved snapshot; see README.md."))
  }
  input_path <- normalizePath(params$input_csv, mustWork = TRUE)
  example_path <- normalizePath("examples", mustWork = TRUE)
  if (!params$example && (startsWith(input_path, paste0(example_path, "/")) ||
                         identical(input_path, example_path))) {
    fail("Production mode refuses files in examples/. Use --example explicitly.")
  }
  df <- utils::read.csv(input_path, colClasses = "character", check.names = FALSE,
                        na.strings = c("", "NA"), strip.white = TRUE)
  if (anyDuplicated(names(df))) fail("CSV column names must be unique.")
  # Both names occur in repository history. Accept the shorthand alias only.
  if (!"stock" %in% names(df) && "species" %in% names(df)) df$stock <- df$species
  if (all(c("stock", "species") %in% names(df)) &&
      !identical(df$stock, df$species)) {
    fail("Conflicting stock/species columns; supply one consistent shorthand field.")
  }
  required <- c("stock", "year", "area", "ofl", "abc", "tac", "catch")
  missing <- setdiff(required, names(df))
  if (length(missing)) fail(paste("CSV is missing columns:", paste(missing, collapse = ", ")))
  for (field in c("stock", "area", "year")) {
    if (anyNA(df[[field]]) || any(!nzchar(trimws(df[[field]])))) {
      fail(paste("CSV", field, "must be present on every row."))
    }
  }
  for (field in c("year", "ofl", "abc", "tac", "catch")) {
    value <- suppressWarnings(as.numeric(df[[field]]))
    invalid <- !is.na(df[[field]]) & (is.na(value) | !is.finite(value) | value < 0)
    if (any(invalid)) fail(paste("CSV", field, "must contain nonnegative numbers or NA; no commas or units."))
    df[[field]] <- value
  }
  if (any(df$year != floor(df$year))) fail("CSV year must contain integer years.")
  if (anyDuplicated(df[c("stock", "year", "area")])) {
    fail("Duplicate stock/year/area keys in CSV; resolve them before rendering.")
  }
  stock <- tolower(params$stock)
  region <- params$area
  if (toupper(region) %in% c("GOA", "BSAI")) region <- toupper(region)
  keep <- df$stock == stock & df$year %in% (params$year + (-1:2))
  if (region == "GOA") {
    keep <- keep & df$area %in% c("Total", "GOA-wide")
  } else if (region != "BSAI") {
    keep <- keep & df$area == region
  }
  selected <- df[keep, required, drop = FALSE]
  if (!nrow(selected)) fail("No rows match stock, area and the four-year reporting window.")
  if (!all((params$year + (-1:2)) %in% selected$year)) {
    fail("Selected rows must cover year-1 through year+2; use explicit NA for unavailable values.")
  }
  table <- selected[c(if (region != "GOA") "area", "year", "ofl", "abc", "tac", "catch")]
  names(table) <- c(if (region != "GOA") "Area", "Year", "OFL", "ABC", "TAC", "Catch")
  list(table = table, selected = selected, input_md5 = unname(tools::md5sum(input_path)))
}
