library(shiny)
source(file.path("R", "document.R"), local = TRUE)
root <- normalizePath("..")
storage <- Sys.getenv("PLANTEAM_BUILDER_DATA", file.path(root, "appdata"))
dir.create(storage, recursive = TRUE, showWarnings = FALSE)
working_file <- file.path(storage, "working.json")
options(shiny.maxRequestSize = 20 * 1024^2)

ui <- fluidPage(
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "editor.css"),
    tags$script(src = "editor.js")),
  div(class = "app-heading", div(span(class = "eyebrow", "COUNCIL / PLANTEAM"),
    h1("SAFE introduction builder"), p("A local workspace for BSAI and GOA Plan Team documents")),
    div(class = "save-status", textOutput("save_status"))),
  fluidRow(
    column(3, div(class = "sidebar-card",
      selectInput("fmp", "Fishery management plan", c("BSAI", "GOA"), selectize = FALSE),
      selectInput("topic", "Document part or stock", choices = NULL, selectize = FALSE),
      selectInput("component", "Regional component", choices = NULL, selectize = FALSE),
      selectInput("section", "Subsection", choices = NULL, selectize = FALSE),
      div(class = "nav-actions", actionButton("include_stock", "Include whole stock"),
        actionButton("exclude_stock", "Exclude whole stock")),
      tags$hr(), h3("Document details"),
      textInput("meta_year", "Report year"), textInput("meta_title", "Title"),
      textAreaInput("meta_contributors", "Contributors / authors", rows = 3),
      textInput("meta_document_date", "Document date or meeting label"),
      textInput("meta_catch_data_date", "Catch data through"),
      textAreaInput("meta_source", "Document source / approval reference", rows = 2),
      p(class = "help-text", "Both FMPs remain in the working file. Switching views preserves edits."))),
    column(9,
      div(class = "toolbar", actionButton("save_now", "Save now", class = "btn-primary"),
        actionButton("reopen", "Reopen saved file"), downloadButton("download_json", "Download working copy"),
        actionButton("new_document", "New document")),
      tabsetPanel(id = "workspace_tab",
        tabPanel("Edit", value = "edit",
          div(class = "editor-card", h2(textOutput("current_heading", inline = TRUE)),
            p(class = "help-text", "Paste plain text with Cmd+V. Formatting, code, HTML and Quarto markup are kept as literal text."),
            checkboxInput("section_include", "Include this subsection in the document", TRUE),
            textAreaInput("section_text", "Narrative", rows = 16, width = "100%"),
            div(class = "clipboard-toolbar", actionButton("copy_section", "Copy whole subsection"),
              actionButton("cut_section", "Cut subsection"), actionButton("paste_section", "Paste copied subsection (replace)"),
              actionButton("undo_edit", "Undo cut / paste")),
            p(class = "help-text", textOutput("clipboard_status", inline = TRUE)),
            tags$details(tags$summary("Table, caption and units"),
              p("Paste tab-separated rows from a spreadsheet, including one header row. Values remain text; no totals or projections are calculated. One table per subsection."),
              textInput("section_caption", "Table caption and units"),
              textAreaInput("section_table_tsv", "Table (tab-separated columns)", rows = 8, width = "100%"),
              p(class = "help-text", "Tables over seven columns render in panels, repeating their first two identifying columns. Empty cells are retained.")),
            fluidRow(column(8, textAreaInput("section_source", "Source, accepted run or approval reference", rows = 2)),
              column(4, textInput("section_source_year", "Source reviewed for report year"))),
            checkboxInput("section_reviewed", "I reviewed this subsection for the selected report year", FALSE),
            textOutput("section_feedback"))),
        tabPanel("Review and render", value = "review",
          div(class = "editor-card", h2("Review checklist"),
            p("Drafts show missing, stale and unreviewed material. The for-review render requires every included subsection and document detail to pass the checklist."),
            radioButtons("render_mode", "Render status", c("Working draft" = "draft", "For review" = "review"), inline = TRUE),
            checkboxGroupInput("render_formats", "Formats", c("HTML QA/QC" = "html", "Word" = "docx", "PDF" = "pdf"), selected = c("html", "docx", "pdf"), inline = TRUE),
            actionButton("render", "Save and render selected FMP", class = "btn-primary"),
            p(class = "help-text", "Rendering uses the current saved document. It creates a new local bundle on every run and preserves previous renders."),
            verbatimTextOutput("render_status"), uiOutput("render_links"),
            uiOutput("review_list"))),
        tabPanel("Working files", value = "files",
          div(class = "editor-card", h2("Save and reopen"),
            p("The app autosaves after a short pause. Save now before closing. Download a working copy for a portable backup; reopening that JSON restores both FMPs, all stocks and review fields."),
            verbatimTextOutput("file_location"),
            fileInput("open_json", "Open a builder working copy (.json)", accept = ".json"),
            p("Opening a working copy saves the current document as a backup first. Malformed files are rejected. Reopen saved file reloads the disk version; use Download working copy first if another session has changed it."),
            p("The latest 20 disk versions are retained in appdata/backups. Concurrent-session conflicts never overwrite another session's document; recovery copies are saved separately."),
            p("Use this app on this computer only. Inputs, working files and generated outputs may contain restricted data."),
            tags$a(href = "https://github.com/afsc-assessments/planteam", "Upstream package repository", target = "_blank", rel = "noopener")))))))

server <- function(input, output, session) {
  initial <- tryCatch(if (file.exists(working_file)) builder_read(working_file) else builder_new(), error = identity)
  if (inherits(initial, "error")) stop(paste("Working file could not be opened; it was preserved:", conditionMessage(initial)))
  doc <- reactiveVal(initial)
  dirty <- reactiveVal(!file.exists(working_file))
  disk_hash <- builder_hash(working_file)
  save_status <- reactiveVal(if (file.exists(working_file)) "Opened saved working file" else "Creating local working file…")
  render_status <- reactiveVal("No render in this session.")
  render_result <- reactiveVal(NULL)
  clip <- reactiveVal(NULL)
  undo <- reactiveVal(NULL)
  fmp <- reactive(if (isTruthy(input$fmp) && input$fmp %in% c("BSAI", "GOA")) input$fmp else "BSAI")
  index <- reactive(builder_index(fmp()))
  current_key <- reactive({
    ix <- index()
    stock <- if (isTruthy(input$topic)) input$topic else "_general"
    candidates <- names(ix)[vapply(ix, function(x) x$stock == stock, logical(1))]
    if (!length(candidates)) candidates <- names(ix)[1]
    component <- if (isTruthy(input$component)) input$component else "_whole"
    match <- candidates[vapply(ix[candidates], function(x) x$component == component, logical(1))]
    if (length(match)) candidates <- match
    section <- if (isTruthy(input$section)) input$section else "summary"
    match <- candidates[vapply(ix[candidates], function(x) x$section == section, logical(1))]
    if (length(match)) match[1] else candidates[1]
  })
  load_section <- function() {
    key <- isolate(current_key())
    part <- strsplit(key, "|", fixed = TRUE)[[1]][1]
    session$sendCustomMessage("builder-section", c(list(key = key), isolate(doc())$fmps[[part]]$sections[[key]]))
  }
  load_metadata <- function() session$sendCustomMessage("builder-metadata", c(list(fmp = isolate(fmp())), isolate(doc())$fmps[[isolate(fmp())]]$metadata))
  save_now <- function() {
    tryCatch({
      disk_hash <<- builder_save(isolate(doc()), working_file, disk_hash)
      dirty(FALSE)
      save_status(paste("Saved locally", format(Sys.time(), "%H:%M:%S")))
      session$sendCustomMessage("builder-saved", TRUE)
      TRUE
    }, error = function(error) {
      save_status(paste("SAVE FAILED:", conditionMessage(error)))
      session$sendCustomMessage("builder-saved", FALSE)
      FALSE
    })
  }
  observeEvent(fmp(), {
    labels <- c(`Front matter, appendices and tables` = "_general", builder_spec()$stocks[[fmp()]])
    # selectInput choices use names as labels and values as stable identifiers.
    choices <- setNames(c("_general", names(builder_spec()$stocks[[fmp()]])),
      c("Front matter, appendices and tables", unname(builder_spec()$stocks[[fmp()]])))
    updateSelectInput(session, "topic", choices = choices, selected = "_general")
    load_metadata()
  })
  observeEvent(list(fmp(), input$topic), {
    ix <- index()
    selected <- ix[vapply(ix, function(x) identical(x$stock, input$topic), logical(1))]
    if (!length(selected)) selected <- ix[1]
    components <- unique(vapply(selected, function(x) x$component, character(1)))
    labels <- vapply(components, function(id) selected[[which(vapply(selected, function(x) x$component == id, logical(1)))[1]]]$component_label, character(1))
    updateSelectInput(session, "component", choices = setNames(components, labels), selected = components[1])
  })
  observeEvent(list(fmp(), input$topic, input$component), {
    ix <- index()
    selected <- ix[vapply(ix, function(x) identical(x$stock, input$topic) && identical(x$component, input$component), logical(1))]
    if (!length(selected)) selected <- ix[1]
    updateSelectInput(session, "section", choices = setNames(vapply(selected, function(x) x$section, character(1)),
      vapply(selected, function(x) x$label, character(1))), selected = selected[[1]]$section)
  })
  observeEvent(current_key(), load_section())
  observeEvent(input$section_edit, {
    edit <- input$section_edit
    tryCatch({
      old <- isolate(doc())
      section <- edit$section
      section$reviewed <- isTRUE(section$reviewed)
      section$include <- isTRUE(section$include)
      updated <- builder_set(old, edit$key, section)
      doc(updated); dirty(TRUE)
    }, error = function(error) showNotification(conditionMessage(error), type = "error", duration = NULL))
  }, ignoreInit = TRUE)
  observeEvent(input$metadata_edit, {
    edit <- input$metadata_edit
    if (!edit$fmp %in% c("BSAI", "GOA")) return()
    next_doc <- isolate(doc())
    next_doc$fmps[[edit$fmp]]$metadata <- edit$metadata
    tryCatch({ builder_validate(next_doc); doc(next_doc); dirty(TRUE) },
      error = function(error) showNotification(conditionMessage(error), type = "error"))
  }, ignoreInit = TRUE)
  paused_doc <- debounce(reactive(doc()), 800)
  observeEvent(paused_doc(), if (isolate(dirty())) save_now(), ignoreInit = FALSE)
  observeEvent(input$save_now, save_now())
  observeEvent(input$reopen, {
    tryCatch({
      next_doc <- builder_read(working_file)
      doc(next_doc); disk_hash <<- builder_hash(working_file); dirty(FALSE)
      load_section(); load_metadata(); save_status("Reopened saved working file")
      session$sendCustomMessage("builder-saved", TRUE)
    }, error = function(error) showNotification(conditionMessage(error), type = "error", duration = NULL))
  })
  observeEvent(input$open_json, {
    tryCatch({
      imported <- builder_read(input$open_json$datapath)
      if (!save_now()) stop("Save the current document or download it before importing.")
      doc(imported); dirty(TRUE)
      if (save_now()) { load_section(); load_metadata(); save_status("Opened working copy; previous document backed up") }
    }, error = function(error) showNotification(conditionMessage(error), type = "error", duration = NULL))
  })
  observeEvent(input$new_document, {
    if (save_now()) { doc(builder_new()); dirty(TRUE); save_now(); load_section(); load_metadata() }
  })
  output$download_json <- downloadHandler(filename = function() paste0(doc()$document_id, ".json"),
    content = function(file) jsonlite::write_json(doc(), file, auto_unbox = TRUE, pretty = TRUE))
  observeEvent(input$copy_section, {
    key <- current_key(); part <- strsplit(key, "|", fixed = TRUE)[[1]][1]
    clip(list(key = key, section = doc()$fmps[[part]]$sections[[key]]))
  })
  observeEvent(input$cut_section, {
    key <- current_key(); part <- strsplit(key, "|", fixed = TRUE)[[1]][1]
    old <- doc()$fmps[[part]]$sections[[key]]
    clip(list(key = key, section = old)); undo(list(key = key, section = old))
    doc(builder_set(doc(), key, builder_blank_section(old$include))); dirty(TRUE); load_section()
  })
  observeEvent(input$paste_section, {
    req(clip())
    key <- current_key(); part <- strsplit(key, "|", fixed = TRUE)[[1]][1]
    undo(list(key = key, section = doc()$fmps[[part]]$sections[[key]]))
    pasted <- clip()$section; pasted$reviewed <- FALSE
    doc(builder_set(doc(), key, pasted)); dirty(TRUE); load_section()
  })
  observeEvent(input$undo_edit, {
    req(undo()); old <- undo(); doc(builder_set(doc(), old$key, old$section)); dirty(TRUE); undo(NULL); load_section()
  })
  for (action in c("include_stock", "exclude_stock")) local({
    action_id <- action
    observeEvent(input[[action_id]], {
      if (identical(input$topic, "_general")) { showNotification("Choose a stock first."); return() }
      next_doc <- doc()
      for (key in names(index())) if (index()[[key]]$stock == input$topic) {
        next_doc$fmps[[fmp()]]$sections[[key]]$include <- action_id == "include_stock"
      }
      doc(next_doc); dirty(TRUE); load_section()
    })
  })
  observeEvent(input$render, {
    if (!save_now()) return()
    render_result(NULL)
    result <- tryCatch(withProgress(message = "Rendering local documents", value = 0.1, {
      builder_render(isolate(doc()), isolate(fmp()), input$render_mode, root, input$render_formats)
    }), error = identity)
    if (inherits(result, "error")) {
      render_status(paste("RENDER FAILED:", conditionMessage(result), "Your working file is saved."))
    } else {
      render_result(result)
      lines <- vapply(names(result$results), function(format) paste(toupper(format), if (result$results[[format]]$ok) "created" else "FAILED; inspect log"), character(1))
      render_status(paste(c(lines, result$directory), collapse = "\n"))
      addResourcePath(paste0("bundle-", session$token), result$directory)
    }
  })
  output$render_links <- renderUI({
    result <- render_result(); req(result)
    base <- paste0("bundle-", session$token, "/")
    tagList(lapply(names(result$results), function(format) {
      item <- result$results[[format]]
      if (item$ok) tags$a(class = "btn btn-default", href = paste0(base, "rendered/document.", format),
        target = "_blank", rel = "noopener", paste("Open", toupper(format))) else
        tags$a(class = "btn btn-default", href = paste0(base, basename(item$log)), target = "_blank", paste("Read", format, "error log"))
    }))
  })
  output$render_status <- renderText(render_status())
  output$save_status <- renderText(save_status())
  output$file_location <- renderText(paste("Working file:", working_file, "\nBackups:", file.path(storage, "backups")))
  output$current_heading <- renderText({ item <- index()[[current_key()]]; paste(item$topic_label, item$component_label, item$label, sep = " / ") })
  output$clipboard_status <- renderText(if (is.null(clip())) "Internal subsection clipboard is empty. Native Cmd+C / Cmd+V also works in each field." else paste("Copied:", clip()$key, "— paste resets author review."))
  output$section_feedback <- renderText({
    key <- current_key(); section <- doc()$fmps[[fmp()]]$sections[[key]]
    tryCatch({ table <- builder_table(section$table_tsv)
      paste(if (section$reviewed) "Author review marked." else "Author review pending.",
        if (section$source_year != doc()$fmps[[fmp()]]$metadata$year) "Source year is missing or stale." else "",
        if (!is.null(table)) paste(nrow(table) - 1L, "table rows;", ncol(table), "columns.") else "")
    }, error = function(error) conditionMessage(error))
  })
  output$review_list <- renderUI({
    issues <- builder_issues(doc(), fmp())
    if (!length(issues)) return(tags$p(class = "review-pass", "All included fields passed the checklist. Scientific approval remains the authors' responsibility."))
    tagList(h3(paste(length(issues), "items need attention")), tags$ul(class = "review-list", lapply(issues, tags$li)))
  })
  session$onSessionEnded(function() {
    if (isolate(dirty()) && !save_now()) {
      recovery <- tempfile("recovery-", tmpdir = storage, fileext = ".json")
      jsonlite::write_json(isolate(doc()), recovery, auto_unbox = TRUE, pretty = TRUE)
    }
    if (!is.null(isolate(render_result()))) try(removeResourcePath(paste0("bundle-", session$token)), silent = TRUE)
  })
}

shinyApp(ui, server)
