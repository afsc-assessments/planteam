# Offline regression and failure-path tests. Run from the checkout root.
source("scripts/report-helpers.R")
p <- yaml::read_yaml("examples/catch-report.yml", eval.expr = FALSE)
x <- prepare_catch_report(p)
stopifnot(identical(x$table$Catch, c(720, 0, NA_real_, NA_real_)),
          identical(x$table$Year, as.numeric(2022:2025)),
          identical(x$table$ABC, c(1100, 1050, 1000, 950)))
expect_failure <- function(expr, pattern) {
  error <- tryCatch({ force(expr); NULL }, error = identity)
  stopifnot(inherits(error, "error"), grepl(pattern, conditionMessage(error), fixed = TRUE))
}
expect_failure(prepare_catch_report(modifyList(p, list(example = FALSE))), "Production mode refuses")
expect_failure(prepare_catch_report(modifyList(p, list(input_csv = "absent.csv"))), "Missing input CSV")
expect_failure(prepare_catch_report(modifyList(p, list(stock = "absent"))), "No rows match")
expect_failure(prepare_catch_report(modifyList(p, list(year = 2023.5))), "integer year")
expect_failure(prepare_catch_report(modifyList(p, list(next_full_year = 2023))), "last_full_year < year")
expect_failure(prepare_catch_report(modifyList(p, list(snapshot_date = "invalid"))), "ISO date")
fixture <- tempfile(fileext = ".csv")
tryCatch({
  df <- read.csv(p$input_csv)
  p$input_csv <- fixture
  write_fixture <- function(data) write.csv(data, fixture, row.names = FALSE, na = "NA")
  write_fixture(df)
  stopifnot(identical(prepare_catch_report(p)$table, x$table))
  alias <- df
  names(alias)[1] <- "species"
  write_fixture(alias)
  stopifnot(identical(prepare_catch_report(p)$table, x$table))
  alias$stock <- "pcod"
  write_fixture(alias)
  expect_failure(prepare_catch_report(p), "Conflicting stock/species")
  write_fixture(rbind(df, df[1, ]))
  expect_failure(prepare_catch_report(p), "Duplicate stock/year/area")
  write_fixture(df[-1, ])
  expect_failure(prepare_catch_report(p), "year-1 through year+2")
}, finally = unlink(fixture))
fixture <- tempfile(fileext = ".csv")
tryCatch({
  p$input_csv <- fixture
  bad <- df
  bad$abc[1] <- "1,100"
  write.csv(bad, fixture, row.names = FALSE)
  expect_failure(prepare_catch_report(p), "no commas or units")
  write.csv(df[-5], fixture, row.names = FALSE)
  expect_failure(prepare_catch_report(p), "missing columns")
  areas <- do.call(rbind, lapply(c("Total", "GOA-wide", "W"), function(area) {
    out <- df
    out$area <- area
    out
  }))
  write.csv(areas, fixture, row.names = FALSE)
  p$area <- "goa"
  goa <- prepare_catch_report(p)
  stopifnot(nrow(goa$table) == 8L, !"Area" %in% names(goa$table),
            identical(goa$selected$area, rep(c("Total", "GOA-wide"), each = 4)))
  p$area <- "bsai"
  stopifnot(nrow(prepare_catch_report(p)$table) == 12L)
  p$area <- "W"
  stopifnot(nrow(prepare_catch_report(p)$table) == 4L)
}, finally = unlink(fixture))
cat("PASS: selection, unchanged values/NA/zero, aliases, GOA/BSAI branches and invalid-input guards.\n")
