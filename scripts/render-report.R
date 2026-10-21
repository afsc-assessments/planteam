# Run from the checkout root. Usage: --example OR --params path.yml
args <- commandArgs(trailingOnly = TRUE)
if (identical(args, "--example")) {
  params_file <- "examples/catch-report.yml"
} else if (length(args) == 2L && args[1] == "--params") {
  params_file <- args[2]
} else {
  stop("Usage: Rscript --vanilla scripts/render-report.R --example\n",
       "   or: Rscript --vanilla scripts/render-report.R --params inputs/catch-report.yml",
       call. = FALSE)
}
if (!file.exists("_quarto.yml")) stop("Run from the planteam checkout root.", call. = FALSE)
for (package in c("knitr", "rmarkdown", "yaml")) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop("Install the report dependency: ", package, call. = FALSE)
  }
}
quarto <- Sys.which("quarto")
if (!nzchar(quarto)) stop("Install Quarto CLI and add it to PATH.", call. = FALSE)
params <- yaml::read_yaml(params_file, eval.expr = FALSE)
source("scripts/report-helpers.R")
report <- prepare_catch_report(params)
parameters_md5 <- unname(tools::md5sum(params_file))
if (!identical(args, "--example") && params$example) {
  stop("Use --example for the bundled fixture; --params requires example: false.", call. = FALSE)
}
output_dir <- file.path("_output", if (params$example) "example" else "production")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
# Avoid leaving a previous success record after a failed rerender.
manifest_file <- file.path(output_dir, "provenance.yml")
if (file.exists(manifest_file)) unlink(manifest_file)
status <- system2(quarto, c("render", "catch-report.qmd", "--to", "all",
                          "--execute-params", shQuote(normalizePath(params_file)),
                          "--output-dir", shQuote(output_dir)))
if (status != 0L) stop("Quarto render failed; inspect the error above. No success manifest written.", call. = FALSE)
if (!identical(unname(tools::md5sum(params$input_csv)), report$input_md5) ||
    !identical(unname(tools::md5sum(params_file)), parameters_md5)) {
  stop("Input or parameters changed during rendering; rerun with a stable snapshot.", call. = FALSE)
}
utils::write.csv(report$selected, file.path(output_dir, "catch-table.csv"),
                 row.names = FALSE, na = "NA")
git_read <- function(args) {
  if (!nzchar(Sys.which("git"))) return("git unavailable")
  value <- suppressWarnings(system2("git", args, stdout = TRUE, stderr = FALSE))
  if (!is.null(attr(value, "status"))) return("unavailable")
  value
}
artifacts <- file.path(output_dir, c("catch-report.html", "catch-report.docx", "catch-table.csv"))
if (!all(file.exists(artifacts))) stop("Expected output missing; no success manifest written.", call. = FALSE)
manifest <- list(
  workflow = "planteam-quarto-catch-report",
  example = params$example,
  rendered_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  source_revision = git_read(c("rev-parse", "HEAD")),
  source_changes = git_read(c("status", "--porcelain", "--untracked-files=all")),
  parameters = params,
  parameters_md5 = parameters_md5,
  source_md5 = as.list(tools::md5sum(c("_quarto.yml", "catch-report.qmd",
    "scripts/report-helpers.R", "scripts/render-report.R", "inst/template.docx"))),
  input_md5 = report$input_md5,
  software = list(R = R.version.string,
                  quarto = system2(quarto, "--version", stdout = TRUE),
                  packages = setNames(lapply(c("knitr", "rmarkdown", "yaml"),
                    function(x) as.character(utils::packageVersion(x))),
                    c("knitr", "rmarkdown", "yaml"))),
  output_md5 = as.list(tools::md5sum(artifacts))
)
yaml::write_yaml(manifest, manifest_file)
writeLines(capture.output(sessionInfo()), file.path(output_dir, "session-info.txt"))
message("Rendered report, selected data, provenance and session information in ", output_dir)
