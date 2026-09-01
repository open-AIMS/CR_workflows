# Concentration-response analysis interface.
#
# A front end to the Quarto workflow documents, not a second implementation of
# the analysis. Every run renders the same document the command line renders, so
# there is one analysis path and the report stays the record.
#
# Launch with crworkflows::run_cr_app().

library(shiny)
library(bslib)
library(crworkflows)

root <- getShinyOption("cr_root", default = cr_output_root())
registry <- cr_test_types()

# Grouped so the dropdown reads as the laboratory's own list of tests rather
# than fourteen unordered identifiers.
test_choices <- lapply(split(registry, registry$group), function(g) {
  stats::setNames(g$id, g$label)
})
names(test_choices) <- tools::toTitleCase(names(test_choices))

engine_available <- c(
  drc = requireNamespace("drc", quietly = TRUE),
  bayesnec = requireNamespace("bayesnec", quietly = TRUE)
)

ui <- page_sidebar(
  title = "Concentration-response analysis",
  theme = bs_theme(version = 5),

  sidebar = sidebar(
    width = 360,
    textInput("sample_id", "Sample identifier", value = "example"),
    selectInput("test_type", "Test type", choices = test_choices),
    radioButtons("source", "Data",
      choices = c("Shipped example data" = "example", "Upload a csv" = "upload"),
      selected = "example"
    ),
    conditionalPanel(
      "input.source == 'upload'",
      fileInput("file", NULL, accept = ".csv", buttonLabel = "Choose csv"),
      helpText(textOutput("expected_cols", inline = TRUE))
    ),
    hr(),
    radioButtons("engine", "Engine",
      choices = c("drc (seconds)" = "drc", "bayesnec (8-15 minutes)" = "bayesnec"),
      selected = "drc"
    ),
    conditionalPanel(
      "input.engine == 'drc'",
      checkboxInput("average", "Average over candidate mean functions", TRUE),
      helpText("Averaging includes the uncertainty in the choice of curve form.")
    ),
    conditionalPanel(
      "input.engine == 'bayesnec'",
      numericInput("chains", "Chains", 4, min = 2, max = 8, step = 1),
      numericInput("iter", "Iterations", 4000, min = 1000, max = 20000, step = 500),
      checkboxInput("save_fit", "Keep the fitted model object (large)", FALSE),
      helpText("Fitted with the cmdstanr backend. Submitted as a background job.")
    ),
    hr(),
    actionButton("run", "Run analysis", class = "btn-primary w-100"),
    uiOutput("run_note")
  ),

  navset_card_tab(
    id = "tabs",
    nav_panel(
      "Data",
      card(card_header("Observed data"), plotOutput("raw_plot", height = "380px")),
      layout_columns(
        card(card_header("Design"), tableOutput("design")),
        card(card_header("First rows"), DT::DTOutput("preview"))
      )
    ),
    nav_panel(
      "Checks",
      card(
        card_header("Data checks"),
        p(class = "text-muted",
          "These decide whether the analysis is valid. Anything listed must be resolved, or recorded in the report as accepted, before the estimates are used."),
        uiOutput("checks")
      )
    ),
    nav_panel(
      "Results",
      uiOutput("results_ui")
    ),
    nav_panel(
      "Jobs",
      card(
        card_header("Analyses this session"),
        DT::DTOutput("jobs"),
        div(
          class = "mt-3",
          downloadButton("download_zip", "Download all outputs", class = "btn-sm"),
          actionButton("clear_done", "Clear finished", class = "btn-sm btn-outline-secondary")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  jobs <- reactiveVal(list())
  # Bumped whenever a job finishes, so that anything showing results recomputes
  # without polling the filesystem on every reactive flush.
  completed <- reactiveVal(0)

  tt <- reactive(cr_test_type(input$test_type))

  output$expected_cols <- renderText({
    r <- tt()
    need <- c(r$x_var, r$y_var, if (!is.na(r$trials_var)) r$trials_var)
    paste0("Required columns: ", paste(need, collapse = ", "),
           ". Concentration in ", r$conc_units, ".")
  })

  # Every session gets its own directory for uploads. Shiny sessions share one
  # tempdir(), so a path built from the uploaded file's name alone would be the
  # same for two analysts who both uploaded "results.csv": the second upload
  # would overwrite the first, and a background job started before it and still
  # reading that path would analyse the wrong sample under the first analyst's
  # sample identifier. The name is also supplied by the client, so it is reduced
  # to its base name before being used to build a path.
  session_upload_dir <- local({
    d <- tempfile("cr_upload_")
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
    onSessionEnded(function() unlink(d, recursive = TRUE))
    d
  })

  # The uploaded file is copied out of Shiny's own upload path, which has no
  # extension and is cleaned up when the session ends, whereas a background job
  # may still need to read it.
  upload_path <- reactive({
    req(input$source == "upload", input$file)
    dest <- file.path(session_upload_dir, paste0("upload_", basename(input$file$name)))
    file.copy(input$file$datapath, dest, overwrite = TRUE)
    dest
  })

  cr_data <- reactive({
    if (input$source == "example") {
      get(input$test_type, envir = asNamespace("crworkflows"))
    } else {
      as.data.frame(utils::read.csv(upload_path(), stringsAsFactors = FALSE))
    }
  })

  # Choosing "Upload a csv" before picking a file is an ordinary intermediate
  # state, not a fault, and must not be reported as one.
  awaiting_file <- reactive(identical(input$source, "upload") && is.null(input$file))

  # The check is the gate. A structural problem is an error, not a warning, and
  # is surfaced here rather than being discovered inside a background job.
  checks <- reactive({
    req(!awaiting_file())
    tryCatch(check_cr_data(cr_data(), input$test_type),
      error = function(e) e
    )
  })

  # req() throws a silent error of its own while no file has been chosen, and
  # the tryCatch above catches it along with everything else, so the two states
  # are separated here: nothing to check yet is not the same as data that cannot
  # be analysed.
  blocked <- reactive(!awaiting_file() && inherits(checks(), "error"))

  output$checks <- renderUI({
    if (awaiting_file()) {
      return(div(class = "alert alert-secondary",
        "Choose a csv to see the data checks."))
    }
    ch <- checks()
    if (inherits(ch, "error")) {
      return(div(
        class = "alert alert-danger",
        strong("The data cannot be analysed as this test type."), br(),
        conditionMessage(ch)
      ))
    }
    if (!length(ch$issues)) {
      return(div(class = "alert alert-success", "No issues identified."))
    }
    div(
      div(class = "alert alert-warning",
        strong(paste(length(ch$issues), "item(s) to review."))),
      tags$ul(lapply(ch$issues, tags$li))
    )
  })

  output$run_note <- renderUI({
    if (awaiting_file()) {
      div(class = "text-muted small mt-2", "Choose a csv before running.")
    } else if (blocked()) {
      div(class = "text-danger small mt-2", "Fix the data before running.")
    } else if (input$engine == "bayesnec") {
      div(class = "text-muted small mt-2",
          "This will take several minutes. It runs in the background.")
    }
  })

  observe({
    updateActionButton(session, "run",
      label = if (awaiting_file()) {
        "Choose a csv first"
      } else if (blocked()) {
        "Fix the data first"
      } else {
        "Run analysis"
      }
    )
  })

  output$preview <- DT::renderDT(
    {
      DT::datatable(utils::head(cr_data(), 25),
        options = list(dom = "tp", pageLength = 8, scrollX = TRUE),
        rownames = FALSE
      )
    },
    server = FALSE
  )

  output$design <- renderTable(
    {
      req(!blocked())
      summarise_design(cr_data(), input$test_type)
    },
    digits = 4
  )

  output$raw_plot <- renderPlot({
    req(!blocked())
    plot_cr_data(cr_data(), input$test_type)
  })

  observeEvent(input$run, {
    if (awaiting_file()) {
      showNotification("Choose a csv before running.", type = "warning")
      return()
    }
    if (blocked()) {
      showNotification("The data cannot be analysed as this test type.",
        type = "error"
      )
      return()
    }
    if (!isTRUE(engine_available[[input$engine]])) {
      showNotification(paste0("Package '", input$engine, "' is not installed."),
        type = "error"
      )
      return()
    }

    extra <- if (input$engine == "drc") {
      list(average = isTRUE(input$average))
    } else {
      list(
        chains = input$chains, iter = input$iter,
        warmup = max(500, floor(input$iter / 2)),
        save_fit = isTRUE(input$save_fit)
      )
    }

    job <- tryCatch(
      do.call(cr_start_job, c(
        list(
          engine = input$engine, test_type = input$test_type,
          sample_id = input$sample_id, root = root,
          data_file = if (input$source == "upload") upload_path() else NULL
        ),
        extra
      )),
      error = function(e) e
    )
    if (inherits(job, "error")) {
      showNotification(conditionMessage(job), type = "error", duration = NULL)
      return()
    }

    jobs(c(jobs(), list(job)))
    showNotification(
      paste0("Started: ", job$id,
        if (input$engine == "bayesnec") ". This will take several minutes." else ""),
      type = "message"
    )
    nav_select("tabs", if (input$engine == "drc") "Results" else "Jobs")
  })

  # Polling stops as soon as nothing is running, so an idle app does no work.
  observe({
    js <- jobs()
    if (!length(js)) {
      return()
    }
    running <- vapply(js, function(j) j$process$is_alive(), logical(1))
    if (any(running)) {
      invalidateLater(1500, session)
    } else {
      isolate(completed(completed() + 1))
    }
  })

  job_table <- reactive({
    js <- jobs()
    if (!length(js)) {
      return(NULL)
    }
    do.call(rbind, lapply(js, cr_job_status))
  })

  output$jobs <- DT::renderDT(
    {
      tab <- job_table()
      validate(need(!is.null(tab), "No analyses started yet."))
      tab$minutes <- round(tab$minutes, 2)
      DT::datatable(
        tab[, c("sample_id", "test_type", "engine", "status", "minutes", "message")],
        rownames = FALSE, selection = "single",
        options = list(dom = "tp", pageLength = 10, scrollX = TRUE)
      )
    },
    server = FALSE
  )

  observeEvent(input$clear_done, {
    js <- jobs()
    keep <- vapply(js, function(j) j$process$is_alive(), logical(1))
    jobs(js[keep])
  })

  # The job whose results are shown: the selected row, or the most recent
  # finished one.
  shown_job <- reactive({
    completed()
    js <- jobs()
    if (!length(js)) {
      return(NULL)
    }
    sel <- input$jobs_rows_selected
    if (length(sel) == 1) {
      return(js[[sel]])
    }
    done <- Filter(function(j) !j$process$is_alive(), js)
    if (!length(done)) NULL else done[[length(done)]]
  })

  output$results_ui <- renderUI({
    job <- shown_job()
    if (is.null(job)) {
      return(div(class = "alert alert-secondary",
        "No finished analysis yet. Run one, or select a row on the Jobs tab."))
    }
    st <- cr_job_status(job)
    if (st$status == "running") {
      return(div(class = "alert alert-info",
        sprintf("%s is still running (%.1f minutes).", st$id, st$minutes)))
    }
    if (st$status == "failed") {
      return(div(class = "alert alert-danger",
        strong(paste0(st$id, " failed.")), br(), st$message))
    }
    tagList(
      div(class = "mb-2",
        strong(st$id), " ",
        span(class = "text-muted", sprintf("(%.2f minutes)", st$minutes))),
      layout_columns(
        col_widths = c(7, 5),
        card(card_header("Fitted curve"), imageOutput("fit_plot", height = "420px")),
        card(card_header("Model weights"), tableOutput("weights"))
      ),
      card(card_header("Estimates"), DT::DTOutput("estimates")),
      card(
        card_header("Outputs"),
        downloadButton("download_report", "Report (html)", class = "btn-sm"),
        downloadButton("download_table", "Estimates (csv)", class = "btn-sm")
      )
    )
  })

  job_files <- reactive({
    job <- shown_job()
    req(job)
    cr_output_files(job$engine, job$test_type, job$sample_id, job$root)
  })

  output$fit_plot <- renderImage(
    {
      f <- job_files()[["figure"]]
      req(file.exists(f))
      list(src = f, contentType = "image/png", width = "100%")
    },
    deleteFile = FALSE
  )

  estimates <- reactive({
    f <- job_files()[["table"]]
    req(file.exists(f))
    utils::read.csv(f, stringsAsFactors = FALSE)
  })

  output$estimates <- DT::renderDT(
    {
      e <- estimates()
      show <- e[, intersect(
        c("estimate_type", "level", "estimate", "lower", "upper", "interval"),
        names(e)
      )]
      DT::datatable(show,
        rownames = FALSE,
        options = list(dom = "t", scrollX = TRUE)
      ) |>
        DT::formatSignif(intersect(c("estimate", "lower", "upper"), names(show)), 4)
    },
    server = FALSE
  )

  # Read from the file the run wrote rather than refitting here. Refitting in
  # the app would be a second analysis path that could disagree with the report.
  output$weights <- renderTable(
    {
      f <- job_files()[["weights"]]
      if (!file.exists(f)) {
        e <- estimates()
        return(data.frame(
          Note = paste0(
            "A single model was fitted, so there are no weights. ",
            e$interval[1]
          ),
          stringsAsFactors = FALSE
        ))
      }
      utils::read.csv(f, stringsAsFactors = FALSE)
    },
    digits = 4
  )

  output$download_report <- downloadHandler(
    filename = function() basename(job_files()[["report"]]),
    content = function(file) file.copy(job_files()[["report"]], file, overwrite = TRUE)
  )

  output$download_table <- downloadHandler(
    filename = function() basename(job_files()[["table"]]),
    content = function(file) file.copy(job_files()[["table"]], file, overwrite = TRUE)
  )

  output$download_zip <- downloadHandler(
    filename = function() {
      paste0("cr_outputs_", format(Sys.Date()), ".zip")
    },
    content = function(file) {
      js <- Filter(function(j) !j$process$is_alive(), jobs())
      if (!length(js)) {
        stop("No finished analyses to download.", call. = FALSE)
      }
      cr_bundle_outputs(js, zipfile = file)
    }
  )
}

shinyApp(ui, server)
