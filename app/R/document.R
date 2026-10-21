# The editor stores plain text and explicit tables, never executable report code.
builder_spec <- function() {
  general <- c(summary = "Summary", introduction = "Introduction",
    background = "Background information", historical_catch = "Historical catch statistics",
    recent_tac = "Recent total allowable catches", definitions = "Biological reference points, ABC and OFL",
    scenarios = "Harvest and recruitment scenarios and projection methodology",
    overview = "Overview of stock assessments", risk = "Risk tables", terms = "Summary and use of terms",
    projections = "Two-year OFL and ABC projections", frequency = "Stock assessment frequency",
    economic = "Economic summary", ecosystem = "Ecosystem considerations",
    multispecies = "Multi-species model", references = "References",
    forage = "Appendix 1. Forage", grenadier = "Appendix 2. Grenadier", sculpins = "Appendix. Sculpins (historical / optional)",
    status_table = "Tables: stock status and harvest specifications",
    discard_table = "Tables: halibut discard mortality rates",
    reductions_table = "Tables: recommendations below maximum permissible ABC",
    historical_table = "Tables: historical catches")
  stock_sections <- c(overview = "Status and catch specifications",
    changes = "Changes from the previous assessment", trends = "Spawning biomass and stock trends",
    tier = "Tier determination / Plan Team discussion and resulting ABCs and OFLs",
    status = "Status determination", apportionment = "Area apportionment", notes = "Additional material")
  bsai <- c(pollock = "1. Walleye pollock", pcod = "2. Pacific cod", sablefish = "3. Sablefish",
    yellowfin = "4. Yellowfin sole", greenland = "5. Greenland turbot", arrowtooth = "6. Arrowtooth flounder",
    kamchatka = "7. Kamchatka flounder", northern_rs = "8. Northern rock sole", flathead = "9. Flathead sole",
    ak_plaice = "10. Alaska plaice", flatfish = "11. Other flatfish complex", pop = "12. Pacific ocean perch",
    nork = "13. Northern rockfish", bsre = "14. Blackspotted and rougheye rockfish",
    shortraker = "15. Shortraker rockfish", orox = "16. Other rockfish complex", atka = "17. Atka mackerel",
    skates = "18. Skates", sharks = "19. Sharks", octopus = "20. Octopus")
  goa <- c(pollock = "1. Walleye pollock", pcod = "2. Pacific cod", sablefish = "3. Sablefish",
    shallow_flat = "4. Shallow-water flatfish", deep_flat = "5. Deep-water flatfish", rex = "6. Rex sole",
    arrowtooth = "7. Arrowtooth flounder", flathead = "8. Flathead sole", pop = "9. Pacific ocean perch",
    nork = "10. Northern rockfish", shortraker = "11. Shortraker rockfish", dusky = "12. Dusky rockfish",
    rebs = "13. Rougheye and blackspotted rockfish", dsr = "14. Demersal shelf rockfish",
    thornyhead = "15. Thornyhead rockfish", orox = "16. Other rockfish", atka = "17. Atka mackerel",
    skates = "18. Skates", sharks = "19. Sharks", octopus = "20. Octopus")
  list(general = general, sections = stock_sections, stocks = list(BSAI = bsai, GOA = goa),
    regions = c(BSAI = "Bering Sea and Aleutian Islands", GOA = "Gulf of Alaska"))
}

builder_index <- function(fmp) {
  spec <- builder_spec()
  stopifnot(fmp %in% names(spec$stocks))
  records <- list()
  add <- function(stock, component, section, topic_label, component_label, section_label) {
    key <- paste(fmp, stock, component, section, sep = "|")
    records[[key]] <<- list(fmp = fmp, stock = stock, component = component, section = section,
      topic_label = topic_label, component_label = component_label, label = section_label)
  }
  for (id in names(spec$general)) {
    if (fmp == "GOA" && id == "multispecies") next
    add("_general", "_whole", id, "Front matter, appendices and tables", "Whole document", spec$general[[id]])
  }
  for (stock in names(spec$stocks[[fmp]])) {
    components <- c(`_whole` = "Whole stock / shared table")
    if (fmp == "BSAI" && stock == "pollock") {
      components <- c(components, ebs = "Eastern Bering Sea pollock", ai = "Aleutian Islands pollock",
                       bogoslof = "Bogoslof pollock")
    }
    if (fmp == "BSAI" && stock == "pcod") {
      components <- c(components, ebs = "Eastern Bering Sea Pacific cod", ai = "Aleutian Islands Pacific cod")
    }
    for (component in names(components)) {
      sections <- spec$sections
      if (length(components) > 1L) sections <- if (component == "_whole") sections["overview"] else sections[names(sections) != "overview"]
      for (id in names(sections)) add(stock, component, id, spec$stocks[[fmp]][[stock]], components[[component]], sections[[id]])
    }
  }
  records
}

builder_blank_section <- function(include = TRUE) {
  list(text = "", table_tsv = "", caption = "", source = "", source_year = "", reviewed = FALSE, include = include)
}

builder_boilerplate <- function(fmp, key, metadata) {
  item <- builder_index(fmp)[[key]]
  subject <- if (item$stock == "_general") builder_spec()$regions[[fmp]] else item$topic_label
  prompts <- c(
    overview = "Identify the accepted assessment/run, biomass definition, units and four-year catch/specification table. Explain any qualifications; retain missing values.",
    changes = "Describe changes in data, model, methods and assessment type since the previous accepted assessment. Identify the baseline year and source.",
    trends = "Describe spawning biomass, recruitment and stock trends with years, uncertainty and supporting evidence.",
    tier = "Record the tier/subtier, author recommendation, Plan Team discussion, ABC/OFL values and reasons for any reduction. Cite the accepted decision.",
    status = "Record the current overfishing, overfished and approaching-overfished determinations from the authoritative source.",
    apportionment = "Describe the approved apportionment method and paste the current area table with units and qualifications. Check overlapping totals at source.",
    notes = "Add any additional stock-specific material with its source.")
  prompt <- if (item$stock != "_general" && item$section %in% names(prompts)) prompts[[item$section]] else
    paste("Supply the current", item$label, "content, with source, units and approval status where applicable.")
  paste0("TODO: ", subject, " - ", item$label, ".\n\nTODO: ", prompt,
    "\n\nTODO: Review for report year ", metadata$year, ". Historical examples are structural references only.")
}

builder_new <- function() {
  fmps <- lapply(c("BSAI", "GOA"), function(fmp) {
    index <- builder_index(fmp)
    sections <- lapply(index, function(item) builder_blank_section(!item$section %in%
      c("notes", "forage", "grenadier", "sculpins", "multispecies", "historical_table", "reductions_table")))
    list(metadata = list(year = format(Sys.Date(), "%Y"), title = paste(fmp, "SAFE introduction"),
      contributors = "", document_date = "", catch_data_date = "", source = ""), sections = sections)
  })
  names(fmps) <- c("BSAI", "GOA")
  for (fmp in names(fmps)) for (key in names(fmps[[fmp]]$sections)) {
    fmps[[fmp]]$sections[[key]]$text <- builder_boilerplate(fmp, key, fmps[[fmp]]$metadata)
  }
  list(schema_version = 2L, document_id = paste0("safe-", format(Sys.time(), "%Y%m%d-%H%M%S"), "-",
    paste(sample(c(letters, 0:9), 6L, replace = TRUE), collapse = "")), fmps = fmps)
}

builder_validate <- function(doc) {
  fail <- function(message) stop(message, call. = FALSE)
  keys <- function(x, expected) is.list(x) && !is.null(names(x)) &&
    !anyDuplicated(names(x)) && setequal(names(x), expected)
  scalar_text <- function(x, limit = 200000L) is.character(x) && length(x) == 1L &&
    !is.na(x) && nchar(x, type = "bytes") <= limit
  if (!keys(doc, c("schema_version", "document_id", "fmps")) ||
      !identical(as.numeric(doc$schema_version), 2) ||
      !scalar_text(doc$document_id, 100L) || !grepl("^[A-Za-z0-9_-]+$", doc$document_id)) {
    fail("Unsupported or malformed document. Expected builder schema version 2.")
  }
  if (!keys(doc$fmps, c("BSAI", "GOA"))) fail("The working file must preserve both FMPs.")
  for (fmp in c("BSAI", "GOA")) {
    part <- doc$fmps[[fmp]]
    if (!keys(part, c("metadata", "sections")) || !keys(part$metadata,
        c("year", "title", "contributors", "document_date", "catch_data_date", "source")) ||
        !all(vapply(part$metadata, scalar_text, logical(1), limit = 5000L))) fail("Malformed document metadata.")
    if (!keys(part$sections, names(builder_index(fmp)))) fail("Section keys do not match this template version.")
    for (section in part$sections) {
      if (!keys(section, c("text", "table_tsv", "caption", "source", "source_year", "reviewed", "include")) ||
          !all(vapply(section[c("text", "table_tsv", "caption", "source", "source_year")], scalar_text, logical(1))) ||
          !all(vapply(section[c("reviewed", "include")], function(x) is.logical(x) && length(x) == 1L && !is.na(x), logical(1)))) {
        fail("Malformed section. All content must be plain text, with logical review/include fields.")
      }
    }
  }
  invisible(doc)
}

builder_read <- function(path) {
  if (!file.exists(path) || file.info(path)$size > 20 * 1024^2) stop("Working file missing or larger than 20 MB.", call. = FALSE)
  doc <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  if (is.list(doc) && identical(as.numeric(doc$schema_version), 1)) {
    for (fmp in c("BSAI", "GOA")) {
      key <- paste(fmp, "_general", "_whole", "sculpins", sep = "|")
      expected <- setdiff(names(builder_index(fmp)), key)
      if (!setequal(names(doc$fmps[[fmp]]$sections), expected)) stop("Malformed schema 1 section map; original file preserved.", call. = FALSE)
      doc$fmps[[fmp]]$sections[[key]] <- builder_blank_section(FALSE)
      doc$fmps[[fmp]]$sections[[key]]$text <- builder_boilerplate(fmp, key, doc$fmps[[fmp]]$metadata)
    }
    doc$schema_version <- 2L
  }
  builder_validate(doc)
  doc
}

builder_hash <- function(path) if (file.exists(path)) unname(tools::md5sum(path)) else "absent"

builder_save <- function(doc, path, expected_hash = builder_hash(path)) {
  builder_validate(doc)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  lock <- paste0(path, ".lock")
  if (!dir.create(lock, showWarnings = FALSE)) stop("Save locked by another process. Your edits remain in memory; download a copy.", call. = FALSE)
  on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  if (!identical(builder_hash(path), expected_hash)) stop("The working file changed in another session. Download your edits before reopening it.", call. = FALSE)
  temporary <- tempfile(".pending-", tmpdir = dirname(path), fileext = ".json")
  on.exit(unlink(temporary), add = TRUE)
  jsonlite::write_json(doc, temporary, auto_unbox = TRUE, pretty = TRUE, null = "null")
  builder_read(temporary)
  if (file.exists(path)) {
    backups <- file.path(dirname(path), "backups")
    dir.create(backups, showWarnings = FALSE)
    backup <- tempfile(paste0(format(Sys.time(), "%Y%m%d-%H%M%S"), "-"), tmpdir = backups, fileext = ".json")
    if (!file.copy(path, backup)) stop("Could not create a backup; original file preserved.", call. = FALSE)
    old <- list.files(backups, full.names = TRUE, pattern = "\\.json$")
    old <- old[order(file.info(old)$mtime, decreasing = TRUE)]
    if (length(old) > 20L) unlink(old[-seq_len(20L)])
  }
  if (!file.rename(temporary, path)) stop("Atomic save failed; original file and backup preserved.", call. = FALSE)
  builder_hash(path)
}

builder_set <- function(doc, key, section) {
  fmp <- strsplit(key, "|", fixed = TRUE)[[1]][1]
  if (!fmp %in% c("BSAI", "GOA") || !key %in% names(builder_index(fmp))) stop("Unknown section key.", call. = FALSE)
  doc$fmps[[fmp]]$sections[[key]] <- section
  builder_validate(doc)
  doc
}

builder_table <- function(text) {
  if (!nzchar(trimws(text))) return(NULL)
  lines <- strsplit(gsub("\r\n?", "\n", text), "\n", fixed = TRUE)[[1]]
  while (length(lines) && !nzchar(tail(lines, 1))) lines <- head(lines, -1)
  cells <- lapply(lines, function(line) {
    # A sentinel preserves trailing blank cells when splitting spreadsheet TSV.
    out <- strsplit(paste0(line, "\t__end__"), "\t", fixed = TRUE)[[1]]
    head(out, -1)
  })
  widths <- lengths(cells)
  if (length(cells) < 2L || widths[1] < 2L || widths[1] > 30L || length(cells) > 1001L || any(widths != widths[1])) {
    stop("Table needs a header and data rows, 2–30 equal-width tab-separated columns, and at most 1000 data rows.", call. = FALSE)
  }
  if (any(!nzchar(trimws(cells[[1]]))) || anyDuplicated(cells[[1]])) stop("Table column headings must be nonempty and unique.", call. = FALSE)
  do.call(rbind, cells)
}

builder_issues <- function(doc, fmp) {
  builder_validate(doc)
  part <- doc$fmps[[fmp]]
  issues <- character()
  for (field in names(part$metadata)) {
    if (!nzchar(trimws(part$metadata[[field]]))) issues <- c(issues, paste("Metadata:", field, "is required"))
  }
  if (!grepl("^(19|20|21)[0-9]{2}$", part$metadata$year)) issues <- c(issues, "Metadata: year must be a four-digit report year")
  index <- builder_index(fmp)
  if (!any(vapply(part$sections, function(x) x$include, logical(1)))) issues <- c(issues, "Include at least one subsection before rendering for review.")
  for (key in names(index)) {
    section <- part$sections[[key]]
    if (!section$include) next
    label <- paste(index[[key]]$topic_label, index[[key]]$component_label, index[[key]]$label, sep = " / ")
    problems <- character()
    if (!nzchar(trimws(section$text)) && !nzchar(trimws(section$table_tsv))) problems <- c(problems, "missing content")
    if (grepl("TODO|EXAMPLE|SYNTHETIC|PLACEHOLDER", paste(section$text, section$caption, section$table_tsv, section$source), ignore.case = TRUE)) problems <- c(problems, "placeholder/example content")
    if (!nzchar(trimws(section$source))) problems <- c(problems, "missing source")
    if (!identical(section$source_year, part$metadata$year)) problems <- c(problems, "source year is missing or stale")
    if (!section$reviewed) problems <- c(problems, "requires author review")
    table <- tryCatch(builder_table(section$table_tsv), error = identity)
    if (inherits(table, "error")) problems <- c(problems, conditionMessage(table))
    if (!is.null(table) && !nzchar(trimws(section$caption))) problems <- c(problems, "table caption/units required")
    if (length(problems)) issues <- c(issues, paste(label, paste(problems, collapse = "; "), sep = ": "))
  }
  issues
}

builder_escape <- function(text) {
  # Escape every ASCII punctuation character: pasted Markdown, HTML, TeX,
  # executable chunks, citations and Quarto shortcodes remain literal text.
  vapply(strsplit(enc2utf8(text), "", fixed = TRUE), function(chars) {
    punct <- grepl("^[[:punct:]]$", chars) & utf8ToInt(paste(chars, collapse = "")) < 128L
    chars[punct] <- paste0("\\", chars[punct])
    paste(chars, collapse = "")
  }, character(1), USE.NAMES = FALSE)
}

builder_paragraphs <- function(text) {
  if (!nzchar(trimws(text))) return("TODO: supply reviewed content.")
  lines <- strsplit(gsub("\r\n?", "\n", text), "\n", fixed = TRUE)[[1]]
  paste(builder_escape(lines), collapse = "  \n")
}

builder_markdown_table <- function(table, caption) {
  # Wide print tables are split into panels with their first two identifiers
  # repeated; no values are aggregated, dropped or recomputed.
  columns <- ncol(table)
  panels <- if (columns <= 7L) list(seq_len(columns)) else
    lapply(split(seq.int(3L, columns), ceiling(seq_along(seq.int(3L, columns)) / 5L)), function(x) c(1L, 2L, x))
  unlist(lapply(seq_along(panels), function(i) {
    selected <- table[, panels[[i]], drop = FALSE]
    rows <- apply(selected, 1L, function(row) paste0("| ", paste(builder_escape(row), collapse = " | "), " |"))
    c(rows[1], paste0("|", paste(rep("---", ncol(selected)), collapse = "|"), "|"), rows[-1], "",
      paste0(": ", builder_escape(caption), if (length(panels) > 1L) paste0(" (panel ", i, "/", length(panels), ")") else ""), "")
  }), use.names = FALSE)
}

builder_qmd <- function(doc, fmp, mode = "draft") {
  builder_validate(doc)
  if (!fmp %in% c("BSAI", "GOA") || !mode %in% c("draft", "review")) stop("Unknown FMP or render mode.", call. = FALSE)
  issues <- builder_issues(doc, fmp)
  if (mode == "review" && length(issues)) stop(paste(length(issues), "items require review. Render a draft or complete the checklist."), call. = FALSE)
  part <- doc$fmps[[fmp]]
  meta <- part$metadata
  header <- list(title = builder_escape(if (nzchar(meta$title)) meta$title else paste(fmp, "SAFE introduction")),
    subtitle = builder_escape(paste("Groundfish resources of the", builder_spec()$regions[[fmp]])),
    author = builder_escape(if (nzchar(meta$contributors)) meta$contributors else "TODO contributors"),
    date = builder_escape(if (nzchar(meta$document_date)) meta$document_date else "TODO document date"), lang = "en-US")
  output <- c("---", yaml::as.yaml(header), "---", "", if (mode == "draft") "**WORKING DRAFT - requires scientific and editorial review.**" else "**Prepared for review - author checks recorded in the working file.**", "",
    paste("Report year:", builder_escape(meta$year), " | Catch data through:", builder_escape(meta$catch_data_date)), "",
    paste("Document source:", builder_escape(meta$source)), "")
  if (length(issues)) output <- c(output, paste("**Open review items:", length(issues), "(see the app checklist). Missing content and stale sources are labelled below.**"), "")
  index <- builder_index(fmp)
  last_stock <- last_component <- ""
  tail_ids <- c("references", "forage", "grenadier", "sculpins", "status_table", "discard_table", "reductions_table", "historical_table")
  post <- names(index)[vapply(index, function(x) x$stock == "_general" && x$section %in% tail_ids, logical(1))]
  order <- c(setdiff(names(index), post), post)
  for (key in order) {
    item <- index[[key]]
    section <- part$sections[[key]]
    if (!section$include) next
    if (item$stock != "_general" && !identical(item$stock, last_stock)) {
      if (!nzchar(last_stock)) output <- c(output, "# Stock status summaries", "")
      output <- c(output, paste("##", item$topic_label), "")
      last_stock <- item$stock
      last_component <- ""
    }
    if (item$stock != "_general" && item$component != "_whole" && !identical(item$component, last_component)) {
      output <- c(output, paste("###", item$component_label), "")
      last_component <- item$component
    }
    level <- if (item$stock == "_general") "#" else if (item$component == "_whole") "###" else "####"
    output <- c(output, paste(level, item$label), "")
    if (!section$reviewed || section$source_year != meta$year || !nzchar(section$source)) {
      output <- c(output, paste("**REVIEW REQUIRED - source year:", builder_escape(if (nzchar(section$source_year)) section$source_year else "missing"), "**"), "")
    }
    table <- builder_table(section$table_tsv)
    if (nzchar(trimws(section$text)) || is.null(table)) output <- c(output, builder_paragraphs(section$text), "")
    if (!is.null(table)) output <- c(output, builder_markdown_table(table,
      if (nzchar(section$caption)) section$caption else "TODO table caption and units"))
    if (nzchar(section$source)) output <- c(output, paste("Source:", builder_escape(section$source)), "")
  }
  paste(output, collapse = "\n")
}

builder_render <- function(doc, fmp, mode, root, formats = c("html", "docx", "pdf"), quarto = Sys.which("quarto")) {
  if (!nzchar(quarto)) stop("Quarto CLI is missing. The working file is still saved.", call. = FALSE)
  if (!length(formats) || any(!formats %in% c("html", "docx", "pdf"))) stop("Choose HTML, Word or PDF.", call. = FALSE)
  content <- builder_qmd(doc, fmp, mode)
  run <- tempfile(paste0(doc$document_id, "-", fmp, "-"), tmpdir = file.path(root, "_output", "builder"))
  dir.create(run, recursive = TRUE, showWarnings = FALSE)
  config <- list(project = list(type = "default", `output-dir` = "rendered", render = list("document.qmd")),
    execute = list(enabled = FALSE), format = list(
      html = list(`embed-resources` = TRUE, toc = TRUE, `toc-depth` = 3),
      docx = list(toc = TRUE, `toc-depth` = 3, `reference-doc` = file.path(root, "inst", "template.docx")),
      pdf = list(`pdf-engine` = "xelatex", `latex-auto-install` = FALSE, toc = TRUE, `toc-depth` = 3, papersize = "letter", fontsize = "11pt",
        geometry = "margin=1in", mainfont = "Times New Roman", `keep-tex` = TRUE)))
  yaml::write_yaml(config, file.path(run, "_quarto.yml"),
    handlers = list(logical = function(x) structure(ifelse(x, "true", "false"), class = "verbatim")))
  writeLines(content, file.path(run, "document.qmd"), useBytes = TRUE)
  jsonlite::write_json(doc, file.path(run, "working-document.json"), auto_unbox = TRUE, pretty = TRUE)
  previous_directory <- getwd()
  on.exit(setwd(previous_directory), add = TRUE)
  setwd(run)
  results <- list()
  for (format in formats) {
    log <- file.path(run, paste0("render-", format, ".log"))
    messages <- suppressWarnings(system2(quarto, c("render", shQuote(file.path(run, "document.qmd")), "--to", format,
      "--no-execute"), stdout = TRUE, stderr = TRUE))
    writeLines(messages, log)
    status <- attr(messages, "status")
    if (is.null(status)) status <- 0L
    expected <- file.path(run, "rendered", paste0("document.", format))
    results[[format]] <- list(ok = identical(status, 0L) && file.exists(expected), file = expected, log = log)
  }
  artifacts <- c(file.path(run, c("document.qmd", "working-document.json", "_quarto.yml")),
    vapply(results[vapply(results, function(x) x$ok, logical(1))], function(x) x$file, character(1)))
  manifest <- list(document_id = doc$document_id, fmp = fmp, mode = mode,
    rendered_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    open_review_items = length(builder_issues(doc, fmp)), R = R.version.string,
    quarto = suppressWarnings(system2(quarto, "--version", stdout = TRUE, stderr = TRUE)),
    source_revision = if (nzchar(Sys.which("git"))) suppressWarnings(system2("git", c("-C", shQuote(root), "rev-parse", "HEAD"), stdout = TRUE, stderr = FALSE)) else "unavailable",
    builder_source_md5 = as.list(tools::md5sum(file.path(root, c("app/R/document.R", "app/app.R", "app/www/editor.js", "app/www/editor.css")))),
    package_versions = setNames(lapply(c("shiny", "jsonlite", "yaml"), function(p) as.character(utils::packageVersion(p))), c("shiny", "jsonlite", "yaml")),
    checksums = as.list(tools::md5sum(artifacts)), results = results)
  yaml::write_yaml(manifest, file.path(run, "manifest.yml"))
  list(directory = run, results = results)
}
