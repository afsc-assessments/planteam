run_builder_tests <- function(root) {
  source(file.path(root, "app/R/document.R"))
  directory <- tempfile("builder-tests-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  expect_error_text <- function(expression, pattern) {
    error <- tryCatch({ force(expression); NULL }, error = identity)
    stopifnot(inherits(error, "error"), grepl(pattern, conditionMessage(error), fixed = TRUE))
  }
  doc <- builder_new()
  builder_validate(doc)
  stopifnot(length(builder_spec()$stocks$BSAI) == 20L, length(builder_spec()$stocks$GOA) == 20L)
  stopifnot("BSAI|pollock|bogoslof|changes" %in% names(builder_index("BSAI")),
    !"GOA|pollock|bogoslof|changes" %in% names(builder_index("GOA")))
  a <- "BSAI|pollock|ebs|changes"
  b <- "GOA|pcod|_whole|trends"
  section <- builder_blank_section()
  section$text <- "First paragraph\nSecond line with <script>alert('x')</script>\n\n```{r}\nstop('NEVER EXECUTE')\n```\n{{< include /etc/passwd >}}\n$math$ \\input{secret}"
  doc <- builder_set(doc, a, section)
  later <- builder_blank_section(); later$text <- "Separate GOA edit\nwith a second line"
  doc <- builder_set(doc, b, later)
  file <- file.path(directory, "working.json")
  hash <- builder_save(doc, file, "absent")
  stopifnot(identical(builder_read(file), doc))
  legacy <- doc; legacy$schema_version <- 1L
  for (fmp in c("BSAI", "GOA")) legacy$fmps[[fmp]]$sections[[paste(fmp, "_general", "_whole", "sculpins", sep = "|")]] <- NULL
  legacy_path <- file.path(directory, "legacy.json")
  jsonlite::write_json(legacy, legacy_path, auto_unbox = TRUE)
  migrated <- builder_read(legacy_path)
  stopifnot(identical(migrated$schema_version, 2L), identical(migrated$fmps$BSAI$sections[[a]]$text, section$text),
    identical(migrated$fmps$GOA$sections[["GOA|_general|_whole|sculpins"]]$include, FALSE))
  changed <- doc
  changed$fmps$GOA$sections[[b]]$text <- "Changed GOA edit"
  next_hash <- builder_save(changed, file, hash)
  stopifnot(length(list.files(file.path(directory, "backups"))) == 1L,
    identical(builder_read(file)$fmps$BSAI$sections[[a]]$text, section$text))
  expect_error_text(builder_save(doc, file, hash), "changed in another session")
  stopifnot(identical(builder_hash(file), next_hash))
  dir.create(paste0(file, ".lock"))
  expect_error_text(builder_save(doc, file, next_hash), "Save locked")
  unlink(paste0(file, ".lock"), recursive = TRUE)
  bad <- doc; bad$schema_version <- 9L
  expect_error_text(builder_save(bad, file, next_hash), "schema version")
  stopifnot(identical(builder_hash(file), next_hash))
  corrupt <- file.path(directory, "bad.json"); writeLines("{broken", corrupt)
  expect_error_text(builder_read(corrupt), "lexical error")
  table <- builder_table("Area\tYear\tABC\nEBS\t2026\t100\nAI\t2026\t")
  stopifnot(identical(table[3,3], ""), ncol(table) == 3L)
  expect_error_text(builder_table("A\tB\n1"), "equal-width")
  expect_error_text(builder_table("A\tA\n1\t2"), "unique")
  table <- matrix(as.character(seq_len(30)), nrow = 3)
  panels <- builder_markdown_table(table, "Units t")
  stopifnot(any(grepl("panel 1/2", panels, fixed = TRUE)), any(grepl("panel 2/2", panels, fixed = TRUE)))
  safe <- builder_escape(section$text)
  stopifnot(!grepl("<script>", safe, fixed = TRUE), !grepl("```{r}", safe, fixed = TRUE),
    !grepl("{{<", safe, fixed = TRUE), !grepl("$math$", safe, fixed = TRUE))
  expect_error_text(builder_qmd(doc, "BSAI", "review"), "items require review")
  stale <- section; stale$text <- "Current interpretation"; stale$source <- "source"; stale$source_year <- "2024"; stale$reviewed <- TRUE
  doc <- builder_set(doc, a, stale)
  stopifnot(any(grepl("stale", builder_issues(doc, "BSAI"), fixed = TRUE)))
  stale$table_tsv <- "X\tY\n1"; doc <- builder_set(doc, a, stale)
  expect_error_text(builder_render(doc, "BSAI", "draft", root), "equal-width")
  expect_error_text(builder_render(builder_new(), "BSAI", "draft", root, quarto = ""), "Quarto CLI")
  failing <- file.path(directory, "quarto-fail")
  writeLines(c("#!/bin/sh", "echo intentional-render-failure >&2", "exit 7"), failing)
  Sys.chmod(failing, "0755")
  mock_root <- file.path(directory, "mock-root")
  dir.create(mock_root)
  failed <- builder_render(builder_new(), "BSAI", "draft", mock_root, formats = "html", quarto = failing)
  stopifnot(identical(failed$results$html$ok, FALSE), file.exists(file.path(failed$directory, "working-document.json")),
    any(grepl("intentional-render-failure", readLines(failed$results$html$log), fixed = TRUE)))
  cat("PASS model: FMP maps, keyed multiline edits, save/reload/backups, conflicts/locks, malformed JSON, tables, stale flags and markup escaping.\n")

  previous <- getwd()
  on.exit(setwd(previous), add = TRUE)
  setwd(file.path(root, "app"))
  Sys.setenv(PLANTEAM_BUILDER_DATA = file.path(directory, "shiny-data"))
  on.exit(Sys.unsetenv("PLANTEAM_BUILDER_DATA"), add = TRUE)
  environment <- new.env(parent = globalenv())
  sys.source("app.R", envir = environment)
  shiny::testServer(environment$server, {
    session$setInputs(fmp = "BSAI", topic = "pollock", component = "ebs", section = "changes")
    payload <- builder_blank_section(); payload$text <- "Multiline BSAI\npasted text"
    session$setInputs(section_edit = list(key = a, section = payload, nonce = 1))
    session$setInputs(fmp = "GOA", topic = "pcod", component = "_whole", section = "trends")
    second <- builder_blank_section(); second$text <- "GOA stock edit"
    session$setInputs(section_edit = list(key = b, section = second, nonce = 2))
    # A late event retains its original FMP/stock key after navigation.
    payload$text <- "Late BSAI edit\nsecond paragraph"
    session$setInputs(section_edit = list(key = a, section = payload, nonce = 3))
    stopifnot(identical(doc()$fmps$BSAI$sections[[a]]$text, payload$text),
      identical(doc()$fmps$GOA$sections[[b]]$text, second$text))
    stopifnot(save_now())
    session$setInputs(copy_section = 1)
    session$setInputs(fmp = "BSAI", topic = "pollock", component = "ebs", section = "changes")
    session$setInputs(paste_section = 1)
    stopifnot(identical(doc()$fmps$BSAI$sections[[a]]$text, second$text),
      identical(doc()$fmps$BSAI$sections[[a]]$reviewed, FALSE))
    session$setInputs(undo_edit = 1)
    stopifnot(identical(doc()$fmps$BSAI$sections[[a]]$text, payload$text))
    session$setInputs(cut_section = 1)
    stopifnot(identical(doc()$fmps$BSAI$sections[[a]]$text, ""))
    session$setInputs(undo_edit = 2)
    stopifnot(identical(doc()$fmps$BSAI$sections[[a]]$text, payload$text))
    stopifnot(save_now())
    session$setInputs(reopen = 1)
    stopifnot(identical(doc()$fmps$BSAI$sections[[a]]$text, payload$text),
      identical(doc()$fmps$GOA$sections[[b]]$text, second$text))
    session$setInputs(open_json = list(datapath = legacy_path, name = "legacy.json", size = file.info(legacy_path)$size, type = "application/json"))
    stopifnot(identical(doc()$schema_version, 2L), identical(doc()$fmps$BSAI$sections[[a]]$text, section$text))
    before_bad_import <- doc()
    session$setInputs(open_json = list(datapath = corrupt, name = "bad.json", size = file.info(corrupt)$size, type = "application/json"))
    stopifnot(identical(doc(), before_bad_import))
    session$setInputs(render_mode = "review", render_formats = c("html", "docx", "pdf"), render = 1)
    stopifnot(grepl("RENDER FAILED:", output$render_status, fixed = TRUE))
  })
  cat("PASS Shiny server: navigation, late edits, copy/cut/paste/undo, explicit save and reopen preserve both FMPs.\n")
}

run_builder_tests(normalizePath("."))
