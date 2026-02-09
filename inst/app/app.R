# EcoReview: Ecological Data Review App
# Generic review interface for human validation of extracted ecological interactions

# Get configuration from options (set by run_app())
app_title <- getOption("ecoreview.title", "EcoReview: Data Validation")
app_name <- getOption("ecoreview.app_name", "EcoReview")
github_url <- getOption("ecoreview.github_url", NULL)
export_prefix <- getOption("ecoreview.export_prefix", "ecoextract")

# Database path: user must always choose on load
db_path <- ""
db_exists <- FALSE

options(shiny.maxRequestSize = 50*1024^2)

# Helper function to detect project root directory
detect_project_root <- function() {
  # Try to use here package if available (best option for finding project root)
  if (requireNamespace("here", quietly = TRUE)) {
    tryCatch({
      return(here::here())
    }, error = function(e) {
      # If here() fails, continue to fallback methods
    })
  }

  # Fallback: manual detection
  current_dir <- getwd()

  # Check if we're in an inst/app subdirectory of a package
  if (grepl("/inst/app$", current_dir)) {
    pkg_root <- dirname(dirname(current_dir))
    if (dir.exists(file.path(pkg_root, ".git")) ||
        length(list.files(pkg_root, pattern = "\\.Rproj$")) > 0) {
      return(pkg_root)
    }
  }

  # Look for project markers by walking up the directory tree
  search_dir <- current_dir
  for (i in 1:10) {
    if (dir.exists(file.path(search_dir, ".git")) ||
        length(list.files(search_dir, pattern = "\\.Rproj$")) > 0) {
      return(search_dir)
    }
    parent_dir <- dirname(search_dir)
    if (parent_dir == search_dir) break
    search_dir <- parent_dir
  }

  # Final fallback to Documents folder
  return(file.path(fs::path_home(), "Documents"))
}

# UI Definition
ui <- shiny::fluidPage(

  title = app_title,
  shinyjs::useShinyjs(),

 # Title with optional GitHub link
  shiny::titlePanel(
    shiny::div(style = "display: flex; justify-content: space-between; align-items: center; margin: 0;",
      shiny::span(app_title),
      if (!is.null(github_url)) {
        shiny::tags$a(href = github_url, target = "_blank",
               style = "font-size: 14px; color: #6c757d; text-decoration: none; margin-left: 20px;",
               title = "View source code on GitHub",
               shiny::HTML('<i class="fab fa-github"></i> Source'))
      }
    )
  ),

  # Head section with styles and scripts
  shiny::tags$head(
    shiny::tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css"),
    shiny::tags$script(src = "https://unpkg.com/split.js/dist/split.min.js"),
    shiny::tags$style(shiny::HTML("
      table{border-collapse:collapse;width:100%;margin:1em 0}
      th,td{border:1px solid #ddd;padding:8px;text-align:left}
      th{background-color:#f2f2f2;font-weight:bold}
      tr:nth-child(even){background-color:#f9f9f9}

      .human-edited {
        background-color: #d4edda !important;
        border-left: 3px solid #28a745 !important;
        color: #155724 !important;
        font-weight: 500 !important;
      }

      .gutter { background-color: #dee2e6; cursor: col-resize; }
      .split-container { display: flex; flex-direction: row; height: calc(100vh - 280px); }
      .left-panel { overflow-y: auto; padding-right: 10px; }
      .right-panel { overflow-y: auto; padding-left: 10px; }
      .table-container { height: 100%; overflow: hidden; }
      .tab-content { height: 100%; overflow-y: auto; }

      .modal-overlay { position: fixed; top: 0; left: 0; width: 100%; height: 100%; background-color: rgba(0, 0, 0, 0.8); z-index: 9999; display: none; }
      .modal-content { position: relative; width: 95%; height: 95%; margin: 2.5% auto; background-color: white; border-radius: 8px; padding: 20px; box-shadow: 0 4px 20px rgba(0, 0, 0, 0.3); }
      .modal-header { display: flex; justify-content: space-between; align-items: center; margin-bottom: 20px; padding-bottom: 10px; border-bottom: 2px solid #dee2e6; }
      .modal-close { background: #dc3545; color: white; border: none; border-radius: 4px; padding: 8px 12px; cursor: pointer; font-size: 16px; }
      .modal-close:hover { background: #c82333; }
      .modal-table-container { height: calc(100% - 80px); overflow: hidden; }

      mark.text-highlight {
        background-color: #fff3cd !important;
        border: 2px solid #ffc107 !important;
        border-radius: 4px;
        padding: 2px 4px;
        box-shadow: 0 0 8px rgba(255, 193, 7, 0.3);
        animation: highlightFlash 2s ease-in-out;
      }

      @keyframes highlightFlash {
        0% { background-color: #fff3cd; }
        50% { background-color: #ffe066; }
        100% { background-color: #fff3cd; }
      }

      .ocr-page { margin-bottom: 40px; padding-bottom: 20px; border-bottom: 1px solid #dee2e6; }
      .ocr-page:last-child { border-bottom: none; }
      .page-number { font-weight: bold; color: #495057; margin-bottom: 10px; padding: 5px 10px; background-color: #f8f9fa; border-radius: 4px; }
      .tensorlake-table { border-collapse: collapse; width: 100%; margin: 15px 0; }
      .tensorlake-table th, .tensorlake-table td { border: 1px solid #ccc; padding: 6px 10px; }
      .tensorlake-table th { background: #f2f2f2; }
    ")),
    shiny::tags$script(shiny::HTML("window.MathJax={tex:{inlineMath:[['$','$'],['\\\\(','\\\\)']],displayMath:[['$$','$$'],['\\\\[','\\\\]']],processEscapes:true,processEnvironments:true},options:{skipHtmlTags:['script','noscript','style','textarea','pre']}};")),
    shiny::tags$script(src = "https://polyfill.io/v3/polyfill.min.js?features=es6"),
    shiny::tags$script(id = "MathJax-script", async = TRUE, src = "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js")
  ),


  # Document selector panel (shown when database is connected)
  shiny::conditionalPanel(
    condition = "output.dbConnected",
    shiny::wellPanel(
      shiny::fluidRow(
      shiny::column(6,
        shiny::selectInput("document_select", "Select Document",
                    choices = NULL,
                    width = "100%")
      ),
      shiny::column(3,
        shiny::div(style = "padding-top: 25px;",
          shiny::checkboxInput("show_unreviewed_only", "Show only unreviewed", value = TRUE)
        )
      ),
      shiny::column(3,
        shiny::div(style = "padding-top: 30px;",
          shiny::uiOutput("documentCount")
        )
      )
    ),
    shiny::div(style = "margin-top: 5px;",
      shiny::fluidRow(
        shiny::column(12,
          shiny::div(style = "display: flex; justify-content: space-between; align-items: center;",
            shiny::div(style = "display: flex; gap: 12px; align-items: center;",
              shiny::uiOutput("dbNameBtn"),
              shiny::downloadButton("exportDbBtn", "Export CSV",
                           class = "btn-sm btn-outline-secondary",
                           style = "font-size: 12px; padding: 2px 8px;"),
              shiny::downloadButton("downloadDbBtn", "Save Database",
                           class = "btn-sm btn-outline-primary",
                           style = "font-size: 12px; padding: 2px 8px;"),
              shiny::actionButton("showAccuracyBtn", shiny::HTML('<i class="fa fa-chart-bar"></i> Accuracy'),
                           class = "btn-sm btn-outline-info",
                           style = "font-size: 12px; padding: 2px 8px;"),
              shiny::uiOutput("dbStats")
            ),
            shiny::actionButton("acceptBtn", shiny::HTML('<i class="fa fa-check"></i> Verify Records'),
                        class = "btn-success btn-lg",
                        style = "font-size: 16px; padding: 10px 30px;")
          )
        )
      )
    )
    )  # closes wellPanel
  ),  # closes conditionalPanel

  # Main content area - flexbox container for Split.js
  shiny::div(id = "splitContainer", style = "height: calc(100vh - 220px); display: flex; flex-direction: row; margin-bottom: 20px;",

    # Left panel with PDF viewer and content tabs
    shiny::div(id = "leftPanel", style = "overflow-y: auto; padding-right: 5px;",
      shiny::tabsetPanel(
        id = "mainTabs",
        shiny::tabPanel("OCR Viewer",
          shiny::conditionalPanel("output.documentSelected",
            shiny::div(style = "height: 100%; overflow-y: auto; padding: 15px;",
              shiny::htmlOutput("ocrViewer")
            )
          ),
          shiny::conditionalPanel("!output.documentSelected",
            shiny::div(style = "height: 100%; display: flex; align-items: center; justify-content: center; background-color: #f8f9fa; border: 2px dashed #dee2e6; border-radius: 8px;",
                shiny::div(style = "text-align: center; color: #6c757d;",
                    shiny::h4("No Document Selected"),
                    shiny::p("Select a document from the dropdown to begin review")
                )
            )
          )
        ),
        shiny::tabPanel("PDF Viewer",
          shiny::conditionalPanel("output.documentSelected", shiny::uiOutput("pdfViewer")),
          shiny::conditionalPanel("!output.documentSelected",
            shiny::div("Select a document to view the original PDF",
                style = "color: #999; padding: 20px; text-align: center;")
          )
        ),
        shiny::tabPanel("Metadata",
          shiny::div(style = "padding: 15px;",
            shiny::conditionalPanel("output.documentSelected",
              shiny::h4("Document Metadata"),
              shiny::p(style = "color: #6c757d; font-size: 0.9em; margin-bottom: 15px;",
                "Edit the paper metadata below. Changes are saved when you click 'Verify Records'."),
              shiny::div(style = "background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 4px; padding: 15px;",
                shiny::textInput("docTitle", "Title", value = "", width = "100%"),
                shiny::textInput("docAuthors", "Authors", value = "", width = "100%", placeholder = "Last, First; Last, First"),
                shiny::fluidRow(
                  shiny::column(4, shiny::numericInput("docYear", "Year", value = NA, min = 1800, max = 2100, width = "100%")),
                  shiny::column(8, shiny::textInput("docDoi", "DOI", value = "", width = "100%"))
                ),
                shiny::fluidRow(
                  shiny::column(8, shiny::textInput("docJournal", "Journal", value = "", width = "100%")),
                  shiny::column(4, shiny::textInput("docVolume", "Vol/Issue", value = "", width = "100%"))
                )
              )
            ),
            shiny::conditionalPanel("!output.documentSelected",
              shiny::div(style = "text-align: center; color: #6c757d; padding: 40px;",
                shiny::tags$i(class = "fas fa-file-alt", style = "font-size: 48px; opacity: 0.5; margin-bottom: 15px;"),
                shiny::h4("No Document Selected"),
                shiny::p("Select a document to view and edit its metadata.")
              )
            )
          )
        )
      )
    ),

    # Right panel with extracted records
    shiny::div(id = "rightPanel", style = "overflow-y: auto; padding-left: 5px;",
      shiny::conditionalPanel("output.documentSelected",
        # Row action buttons
        shiny::div(style = "display: flex; justify-content: flex-end; gap: 8px; margin-bottom: 10px;",
          shiny::actionButton("addRowBtn",
                       label = shiny::HTML('<i class="fa fa-plus"></i> Add Row'),
                       class = "btn-outline-primary btn-sm"),
          shiny::actionButton("deleteRowBtn",
                       label = shiny::HTML('<i class="fa fa-trash"></i> Delete Row'),
                       class = "btn-outline-danger btn-sm"),
          shiny::actionButton("showDeletedBtn",
                       label = "Show Deleted",
                       class = "btn-outline-info btn-sm")
        ),
        shiny::conditionalPanel("output.flaggedCount > 0",
          shiny::div(style = "background-color: #fff3cd; border: 1px solid #ffc107; padding: 8px; margin-bottom: 10px; border-radius: 4px;",
            shiny::tags$i(class = "fas fa-flag", style = "color: #856404; margin-right: 5px;"),
            shiny::strong(shiny::textOutput("flaggedCount", inline = TRUE), " interactions flagged for review"),
            shiny::span(style = "margin-left: 10px; font-size: 0.9em; color: #856404;",
                 "Missing organism identification.")
          )
        ),
        DT::dataTableOutput("interactiveTable", height = "100%")
      ),
      shiny::conditionalPanel("!output.documentSelected",
        shiny::div("Select a document to see extracted records",
            style = "color: #999; padding: 20px; text-align: center; background-color: #f8f9fa; border: 2px dashed #dee2e6; border-radius: 8px; margin-top: 20px;")
      )
    )
  ),

  # Split.js initialization for resizable panes
  shiny::tags$script(shiny::HTML("Split(['#leftPanel', '#rightPanel'], {sizes: [50, 50], minSize: 200, gutterSize: 6, cursor: 'col-resize'});"))
)

# Server Definition
server <- function(input, output, session) {

  # Initialize reactive values
  values <- shiny::reactiveValues(
    db_conn = if (db_exists) db_path else NULL,
    db_name = NULL,
    db_reload_trigger = 0,
    markdown_text = NULL,
    ocr_response = NULL,
    extracted_df = NULL,
    original_df = NULL,
    selected_evidence = NULL,
    document_id = NULL,
    edit_trigger = 0,
    unsaved_changes = list(),
    show_deleted = FALSE,
    doc_metadata = NULL,
    doc_metadata_original = NULL
  )

  # Define volumes for shinyFiles
  # Use the working directory captured by run_app(), or fall back to detection
  user_wd <- getOption("ecoreview.user_working_dir", NULL)

  if (is.null(user_wd)) {
    # Fallback to detection if option not set (e.g., running app directly)
    project_root <- detect_project_root()
  } else {
    project_root <- user_wd
  }

  volumes <- c(
    "Project" = project_root,
    Documents = file.path(fs::path_home(), "Documents")
  )

  # Setup shinyFiles for database file browsing
  shinyFiles::shinyFileChoose(input, "dbFileBrowse", roots = volumes, session = session,
                  filetypes = c("db", "sqlite", "sqlite3"))

  # Handle shinyFiles selection
  shiny::observeEvent(input$dbFileBrowse, {
    shiny::req(input$dbFileBrowse)
    if (!is.integer(input$dbFileBrowse)) {
      file_selected <- shinyFiles::parseFilePaths(volumes, input$dbFileBrowse)
      if (nrow(file_selected) > 0) {
        db_path <- as.character(file_selected$datapath)
        shiny::updateSelectInput(session, "document_select", choices = c("Loading..." = ""), selected = "")
        values$db_conn <- db_path
        values$db_name <- basename(db_path)
        values$db_reload_trigger <- values$db_reload_trigger + 1
        values$document_id <- NULL
        values$markdown_text <- NULL
        values$extracted_df <- NULL
        values$original_df <- NULL
        values$unsaved_changes <- list()
        shiny::removeModal()
        shiny::showNotification(paste("Connected to database:", basename(db_path)), type = "message")
      }
    }
  }, ignoreInit = TRUE)

  # Display selected file path in modal
  output$selectedDbPath <- shiny::renderText({
    if (is.integer(input$dbFileBrowse)) {
      "No file selected"
    } else {
      file_selected <- shinyFiles::parseFilePaths(volumes, input$dbFileBrowse)
      if (nrow(file_selected) > 0) {
        as.character(file_selected$datapath)
      } else {
        "No file selected"
      }
    }
  })

  # Setup shinyFiles for database file browsing in change modal
  shinyFiles::shinyFileChoose(input, "dbFileBrowseChange", roots = volumes, session = session,
                  filetypes = c("db", "sqlite", "sqlite3"))

  # Handle shinyFiles selection in change modal
  shiny::observeEvent(input$dbFileBrowseChange, {
    shiny::req(input$dbFileBrowseChange)
    if (!is.integer(input$dbFileBrowseChange)) {
      file_selected <- shinyFiles::parseFilePaths(volumes, input$dbFileBrowseChange)
      if (nrow(file_selected) > 0) {
        db_path <- as.character(file_selected$datapath)
        shiny::updateSelectInput(session, "document_select", choices = c("Loading..." = ""), selected = "")
        values$db_conn <- db_path
        values$db_name <- basename(db_path)
        values$db_reload_trigger <- values$db_reload_trigger + 1
        values$document_id <- NULL
        values$markdown_text <- NULL
        values$extracted_df <- NULL
        values$original_df <- NULL
        values$unsaved_changes <- list()
        shiny::removeModal()
        shiny::showNotification(paste("Connected to database:", basename(db_path)), type = "message")
      }
    }
  }, ignoreInit = TRUE)

  # Display selected file path in change modal
  output$selectedDbPathChange <- shiny::renderText({
    if (is.integer(input$dbFileBrowseChange)) {
      "No file selected"
    } else {
      file_selected <- shinyFiles::parseFilePaths(volumes, input$dbFileBrowseChange)
      if (nrow(file_selected) > 0) {
        as.character(file_selected$datapath)
      } else {
        "No file selected"
      }
    }
  })

  # Show database connection modal on startup
  shiny::observe({
    if (is.null(values$db_conn)) {
      shiny::showModal(shiny::modalDialog(
        title = "Connect to Database",
        size = "m",
        easyClose = FALSE,
        footer = NULL,

        shiny::div(style = "padding: 10px;",
          shiny::h5("Option 1: Browse for Database File"),
          shiny::div(style = "display: flex; gap: 10px; align-items: center;",
            shinyFiles::shinyFilesButton("dbFileBrowse", "Browse...",
                            title = "Select SQLite Database",
                            multiple = FALSE,
                            class = "btn-primary"),
            shiny::textOutput("selectedDbPath", inline = TRUE)
          ),

          shiny::hr(style = "margin: 20px 0;"),

          shiny::h5("Option 2: Upload Database File"),
          shiny::div(style = "background-color: #fff3cd; border: 1px solid #ffc107; border-radius: 4px; padding: 10px; margin-bottom: 10px;",
            shiny::tags$i(class = "fas fa-exclamation-triangle", style = "color: #856404; margin-right: 5px;"),
            shiny::span(style = "color: #856404; font-size: 0.9em;",
              shiny::strong("Warning:"), " Upload creates a temporary copy. Download the database when finished to save your changes."
            )
          ),
          shiny::fileInput("dbFileModal", NULL,
                    accept = c(".db", ".sqlite", ".sqlite3"),
                    width = "100%",
                    placeholder = "Choose .db, .sqlite, or .sqlite3 file"),

          shiny::hr(style = "margin: 20px 0;"),

          shiny::h5("Option 3: Enter Database URL or Path"),
          shiny::p(style = "font-size: 0.9em; color: #6c757d; margin-bottom: 10px;",
            "For remote databases (https://...) or if you prefer to enter a path manually."
          ),
          shiny::textInput("dbPathModal", NULL,
                    placeholder = "https://example.com/database.db",
                    width = "100%"),
          shiny::div(style = "margin-top: 10px;",
            shiny::actionButton("connectPathBtn", "Connect", class = "btn-primary", style = "width: 100%;")
          )
        )
      ))
    }
  }) |> shiny::bindEvent(values$db_conn, ignoreNULL = FALSE, once = TRUE)

  # Database connected output for conditional panels
  output$dbConnected <- shiny::reactive(!is.null(values$db_conn) && file.exists(values$db_conn))
  shiny::outputOptions(output, "dbConnected", suspendWhenHidden = FALSE)

  # Create reactive output for UI conditionals
  output$documentSelected <- shiny::reactive(!is.null(values$document_id) && values$document_id != "")
  shiny::outputOptions(output, "documentSelected", suspendWhenHidden = FALSE)

  # Handle database file upload from modal
  shiny::observeEvent(input$dbFileModal, {
    shiny::req(input$dbFileModal)
    values$db_conn <- input$dbFileModal$datapath
    values$db_name <- input$dbFileModal$name
    shiny::removeModal()
    shiny::showNotification(paste("Connected to database:", input$dbFileModal$name), type = "message")
  })

  # Handle database path/URL connection from modal
  shiny::observeEvent(input$connectPathBtn, {
    shiny::req(input$dbPathModal, nchar(trimws(input$dbPathModal)) > 0)
    db_path_input <- trimws(input$dbPathModal)

    if (grepl("^https?://", db_path_input)) {
      tryCatch({
        temp_db <- tempfile(fileext = ".db")
        utils::download.file(db_path_input, temp_db, mode = "wb", quiet = TRUE)
        values$db_conn <- temp_db
        values$db_name <- basename(db_path_input)
        shiny::removeModal()
        shiny::showNotification(paste("Connected to remote database:", basename(db_path_input)), type = "message")
      }, error = function(e) {
        shiny::showNotification(paste("Error downloading database:", e$message), type = "error")
      })
    } else {
      if (file.exists(db_path_input)) {
        values$db_conn <- db_path_input
        values$db_name <- basename(db_path_input)
        shiny::removeModal()
        shiny::showNotification(paste("Connected to database:", basename(db_path_input)), type = "message")
      } else {
        shiny::showNotification("Database file not found at specified path.", type = "error")
      }
    }
  })

  # Handle change database button - show modal
  shiny::observeEvent(input$changeDbBtn, {
    shiny::showModal(shiny::modalDialog(
      title = "Change Database",
      size = "m",
      easyClose = TRUE,
      footer = shiny::modalButton("Cancel"),

      shiny::div(style = "padding: 10px;",
        if (!is.null(values$db_name)) {
          shiny::div(style = "margin-bottom: 15px; padding: 10px; background-color: #e9ecef; border-radius: 4px;",
            shiny::strong("Current: "), values$db_name
          )
        },

        shiny::h5("Option 1: Browse for Database File"),
        shiny::div(style = "display: flex; gap: 10px; align-items: center;",
          shinyFiles::shinyFilesButton("dbFileBrowseChange", "Browse...",
                          title = "Select SQLite Database",
                          multiple = FALSE,
                          class = "btn-primary"),
          shiny::textOutput("selectedDbPathChange", inline = TRUE)
        ),

        shiny::hr(style = "margin: 20px 0;"),

        shiny::h5("Option 2: Upload Database File"),
        shiny::div(style = "background-color: #fff3cd; border: 1px solid #ffc107; border-radius: 4px; padding: 10px; margin-bottom: 10px;",
          shiny::tags$i(class = "fas fa-exclamation-triangle", style = "color: #856404; margin-right: 5px;"),
          shiny::span(style = "color: #856404; font-size: 0.9em;",
            shiny::strong("Warning:"), " Upload creates a temporary copy. Download the database when finished to save your changes."
          )
        ),
        shiny::fileInput("dbFileChange", NULL,
                  accept = c(".db", ".sqlite", ".sqlite3"),
                  width = "100%",
                  placeholder = "Choose .db, .sqlite, or .sqlite3 file"),

        shiny::hr(style = "margin: 20px 0;"),

        shiny::h5("Option 3: Enter Database URL or Path"),
        shiny::p(style = "font-size: 0.9em; color: #6c757d; margin-bottom: 10px;",
          "For remote databases (https://...) or if you prefer to enter a path manually."
        ),
        shiny::textInput("dbPathChange", NULL,
                  placeholder = "https://example.com/database.db",
                  width = "100%"),
        shiny::div(style = "margin-top: 10px;",
          shiny::actionButton("connectPathChangeBtn", "Connect", class = "btn-primary", style = "width: 100%;")
        )
      )
    ))
  })

  # Handle file upload from change modal
  shiny::observeEvent(input$dbFileChange, {
    shiny::req(input$dbFileChange)
    shiny::updateSelectInput(session, "document_select", choices = c("Loading..." = ""), selected = "")
    values$db_conn <- input$dbFileChange$datapath
    values$db_name <- input$dbFileChange$name
    values$document_id <- NULL
    values$markdown_text <- NULL
    values$extracted_df <- NULL
    values$original_df <- NULL
    values$unsaved_changes <- list()
    shiny::removeModal()
    shiny::showNotification(paste("Connected to database:", input$dbFileChange$name), type = "message")
  })

  # Handle path connection from change modal
  shiny::observeEvent(input$connectPathChangeBtn, {
    shiny::req(input$dbPathChange, nchar(trimws(input$dbPathChange)) > 0)
    db_path_input <- trimws(input$dbPathChange)

    if (grepl("^https?://", db_path_input)) {
      tryCatch({
        temp_db <- tempfile(fileext = ".db")
        utils::download.file(db_path_input, temp_db, mode = "wb", quiet = TRUE)
        shiny::updateSelectInput(session, "document_select", choices = c("Loading..." = ""), selected = "")
        values$db_conn <- temp_db
        values$db_name <- basename(db_path_input)
        values$document_id <- NULL
        values$markdown_text <- NULL
        values$extracted_df <- NULL
        values$original_df <- NULL
        values$unsaved_changes <- list()
        shiny::removeModal()
        shiny::showNotification(paste("Connected to remote database:", basename(db_path_input)), type = "message")
      }, error = function(e) {
        shiny::showNotification(paste("Error downloading database:", e$message), type = "error")
      })
    } else {
      if (file.exists(db_path_input)) {
        shiny::updateSelectInput(session, "document_select", choices = c("Loading..." = ""), selected = "")
        values$db_conn <- db_path_input
        values$db_name <- basename(db_path_input)
        values$document_id <- NULL
        values$markdown_text <- NULL
        values$extracted_df <- NULL
        values$original_df <- NULL
        values$unsaved_changes <- list()
        shiny::removeModal()
        shiny::showNotification(paste("Connected to database:", basename(db_path_input)), type = "message")
      } else {
        shiny::showNotification("Database file not found at specified path.", type = "error")
      }
    }
  })

  # Load all documents from database
  all_documents <- shiny::reactive({
    shiny::req(values$db_conn)
    values$edit_trigger
    values$db_reload_trigger

    tryCatch({
      ecoextract::get_documents(db_conn = values$db_conn) |>
        dplyr::select(-dplyr::any_of(c("document_content", "ocr_images", "bibliography",
                                "extraction_reasoning", "refinement_reasoning")))
    }, error = function(e) NULL)
  })

  # Filter documents based on checkbox
  filtered_documents <- shiny::reactive({
    docs <- all_documents()
    if (is.null(docs) || nrow(docs) == 0) return(NULL)

    has_reviewed_at <- "reviewed_at" %in% colnames(docs)

    if (input$show_unreviewed_only && has_reviewed_at) {
      docs <- docs |> dplyr::filter(is.na(reviewed_at))
    }
    if (has_reviewed_at) {
      docs |> dplyr::arrange(dplyr::desc(is.na(reviewed_at)), file_name)
    } else {
      docs |> dplyr::arrange(file_name)
    }
  })

  # Update document selector choices
  shiny::observe({
    docs <- filtered_documents()
    all_docs <- all_documents()

    if (is.null(docs) || nrow(docs) == 0) {
      if (!is.null(all_docs) && nrow(all_docs) > 0 && input$show_unreviewed_only) {
        choices <- c("All documents reviewed!" = "")
      } else {
        choices <- c("No documents available" = "")
      }
      shiny::updateSelectInput(session, "document_select", choices = choices)

      values$document_id <- NULL
      values$markdown_text <- NULL
      values$extracted_df <- NULL
      values$original_df <- NULL
      values$doc_metadata <- NULL
      values$doc_metadata_original <- NULL
    } else {
      has_reviewed_at <- "reviewed_at" %in% colnames(docs)
      display_names <- sapply(seq_len(nrow(docs)), function(i) {
        status <- if (has_reviewed_at && is.na(docs$reviewed_at[i])) "" else ""
        paste0(status, " ", docs$file_name[i])
      })
      choices <- stats::setNames(docs$document_id, display_names)

      doc_ids_char <- as.character(docs$document_id)
      current_id_char <- as.character(values$document_id)
      selected <- if (is.null(values$document_id) || !(current_id_char %in% doc_ids_char)) {
        as.character(docs$document_id[1])
      } else {
        current_id_char
      }

      need_force_load <- is.null(values$document_id) &&
                         !is.null(input$document_select) &&
                         input$document_select == selected &&
                         selected != ""

      shiny::updateSelectInput(session, "document_select", choices = choices, selected = selected)

      if (need_force_load) {
        values$document_id <- selected

        doc_id_int <- as.integer(selected)
        doc_info <- docs |> dplyr::filter(document_id == doc_id_int)
        if (nrow(doc_info) > 0) {
          values$doc_metadata <- doc_info
          values$doc_metadata_original <- doc_info
          shiny::updateTextInput(session, "docTitle", value = doc_info$title[1] %||% "")
          authors_val <- doc_info$authors[1] %||% ""
          if (nchar(authors_val) > 0 && grepl("^\\[", authors_val)) {
            authors_val <- tryCatch(paste(jsonlite::fromJSON(authors_val), collapse = "; "), error = function(e) authors_val)
          }
          shiny::updateTextInput(session, "docAuthors", value = authors_val)
          shiny::updateNumericInput(session, "docYear", value = if (!is.na(doc_info$publication_year[1])) doc_info$publication_year[1] else NA)
          shiny::updateTextInput(session, "docDoi", value = doc_info$doi[1] %||% "")
          shiny::updateTextInput(session, "docJournal", value = doc_info$journal[1] %||% "")
          vol_issue <- paste0(doc_info$volume[1] %||% "", if (!is.null(doc_info$issue[1]) && !is.na(doc_info$issue[1])) paste0("(", doc_info$issue[1], ")") else "")
          shiny::updateTextInput(session, "docVolume", value = vol_issue)
        }

        tryCatch({
          values$markdown_text <- ecoextract::get_ocr_markdown(doc_id_int, db_conn = values$db_conn)
        }, error = function(e) NULL)

        tryCatch({
          records <- ecoextract::get_records(doc_id_int, db_conn = values$db_conn)
          values$extracted_df <- records
          values$original_df <- records
        }, error = function(e) NULL)

        values$edit_trigger <- values$edit_trigger + 1
      }
    }
  })

  # Document count display
  output$documentCount <- shiny::renderUI({
    docs <- all_documents()
    if (is.null(docs) || nrow(docs) == 0) {
      return(shiny::div(style = "color: #6c757d;", "No documents"))
    }
    total <- nrow(docs)
    has_reviewed_at <- "reviewed_at" %in% colnames(docs)
    unreviewed <- if (has_reviewed_at) sum(is.na(docs$reviewed_at)) else total
    shiny::div(style = "color: #6c757d; font-size: 0.9em;",
      shiny::strong(unreviewed), " of ", shiny::strong(total), " unreviewed"
    )
  })

  # Handle document selection
  shiny::observeEvent(input$document_select, {
    shiny::req(input$document_select, input$document_select != "")

    doc_id <- input$document_select
    values$document_id <- doc_id

    values$markdown_text <- NULL
    values$ocr_response <- NULL
    values$extracted_df <- NULL
    values$original_df <- NULL
    values$selected_evidence <- NULL
    values$unsaved_changes <- list()
    values$doc_metadata <- NULL
    values$doc_metadata_original <- NULL

    tryCatch({
      docs <- all_documents()
      doc_id_int <- as.integer(doc_id)
      doc_info <- docs |> dplyr::filter(document_id == doc_id_int)
      if (nrow(doc_info) > 0) {
        values$doc_metadata <- doc_info
        values$doc_metadata_original <- doc_info

        shiny::updateTextInput(session, "docTitle", value = doc_info$title[1] %||% "")
        authors_val <- doc_info$authors[1] %||% ""
        if (nchar(authors_val) > 0 && grepl("^\\[", authors_val)) {
          authors_val <- tryCatch(paste(jsonlite::fromJSON(authors_val), collapse = "; "), error = function(e) authors_val)
        }
        shiny::updateTextInput(session, "docAuthors", value = authors_val)
        shiny::updateNumericInput(session, "docYear", value = if (!is.na(doc_info$publication_year[1])) doc_info$publication_year[1] else NA)
        shiny::updateTextInput(session, "docDoi", value = doc_info$doi[1] %||% "")
        shiny::updateTextInput(session, "docJournal", value = doc_info$journal[1] %||% "")
        vol_issue <- paste0(doc_info$volume[1] %||% "", if (!is.null(doc_info$issue[1]) && !is.na(doc_info$issue[1])) paste0("(", doc_info$issue[1], ")") else "")
        shiny::updateTextInput(session, "docVolume", value = vol_issue)
      }
    }, error = function(e) {
      shiny::showNotification(paste("Error loading document metadata:", e$message), type = "error")
    })

    tryCatch({
      doc_id_int <- as.integer(doc_id)
      values$markdown_text <- ecoextract::get_ocr_markdown(doc_id_int, db_conn = values$db_conn)
    }, error = function(e) {
      shiny::showNotification(paste("Error loading OCR:", e$message), type = "error")
    })

    tryCatch({
      doc_id_int <- as.integer(doc_id)
      records <- ecoextract::get_records(doc_id_int, db_conn = values$db_conn)
      values$extracted_df <- records
      values$original_df <- records
    }, error = function(e) {
      shiny::showNotification(paste("Error loading records:", e$message), type = "error")
    })

    values$edit_trigger <- values$edit_trigger + 1
  })


  # Database name button - shows db name, click to change
  output$dbNameBtn <- shiny::renderUI({
    db_display_name <- if (!is.null(values$db_name)) values$db_name else if (!is.null(values$db_conn)) basename(values$db_conn) else "No Database"
    shiny::actionButton("changeDbBtn", shiny::HTML(paste0('<i class="fa fa-database"></i> ', db_display_name)),
                 class = "btn-sm btn-outline-secondary",
                 style = "font-size: 12px; padding: 2px 8px;")
  })

  # Database stats output
  output$dbStats <- shiny::renderUI({
    values$edit_trigger
    docs <- all_documents()
    if (is.null(docs)) {
      return(shiny::div(style = "font-size: 0.9em; color: #6c757d;", "No database connection"))
    }
    total_docs <- nrow(docs)
    has_reviewed_at <- "reviewed_at" %in% colnames(docs)
    reviewed_docs <- if (has_reviewed_at) sum(!is.na(docs$reviewed_at)) else 0

    records_info <- tryCatch({
      records <- ecoextract::get_records(db_conn = values$db_conn)
      list(
        total = nrow(records),
        verified = if ("human_edited" %in% names(records)) sum(records$human_edited == TRUE, na.rm = TRUE) else 0
      )
    }, error = function(e) list(total = 0, verified = 0))

    shiny::div(style = "font-size: 0.85em; color: #6c757d;",
      shiny::span(style = "margin-right: 15px;", shiny::tags$strong("Docs: "), paste0(reviewed_docs, "/", total_docs, " reviewed")),
      shiny::span(shiny::tags$strong("Records: "), paste0(records_info$verified, "/", records_info$total, " verified"))
    )
  })

  # PDF viewer output
  output$pdfViewer <- shiny::renderUI({
    shiny::req(values$document_id)

    docs <- all_documents()
    doc_info <- docs |> dplyr::filter(document_id == values$document_id)

    if (nrow(doc_info) == 0 || is.null(doc_info$file_path) || !file.exists(doc_info$file_path[1])) {
      return(shiny::div(style = "padding: 40px; text-align: center;",
        shiny::div(style = "color: #6c757d; margin-bottom: 15px;",
          shiny::tags$i(class = "fas fa-file-pdf", style = "font-size: 48px; opacity: 0.5;")
        ),
        shiny::h5("PDF Not Available", style = "color: #6c757d; margin-bottom: 10px;"),
        shiny::p(style = "color: #999; font-size: 0.9em;",
          "The original PDF file could not be found. This may occur when:"
        ),
        shiny::tags$ul(style = "color: #999; font-size: 0.85em; text-align: left; display: inline-block;",
          shiny::tags$li("Using a database downloaded from a remote URL"),
          shiny::tags$li("PDFs are stored in a different location"),
          shiny::tags$li("Running on a machine without access to the original files")
        ),
        shiny::p(style = "color: #999; font-size: 0.85em; margin-top: 10px;",
          "The OCR text is still available in the OCR Viewer tab."
        )
      ))
    }

    pdf_path <- doc_info$file_path[1]
    pdf_dir <- dirname(pdf_path)
    pdf_name <- basename(pdf_path)

    shiny::addResourcePath("pdfs", pdf_dir)

    shiny::tags$iframe(
      src = paste0("pdfs/", pdf_name),
      style = "height: calc(100vh - 280px); width: 100%; border: 1px solid #ddd;",
      type = "application/pdf"
    )
  })

  # Accept button handler - marks document as reviewed and saves edits
  shiny::observeEvent(input$acceptBtn, {
    shiny::req(values$document_id)

    shinyjs::html("acceptBtn", '<i class="fa fa-spinner fa-spin"></i> Saving...')

    tryCatch({
      doc_id_int <- as.integer(values$document_id)

      conn <- DBI::dbConnect(RSQLite::SQLite(), values$db_conn)
      on.exit(DBI::dbDisconnect(conn), add = TRUE)

      vol_issue <- input$docVolume %||% ""
      volume <- sub("\\(.*", "", vol_issue)
      issue <- if (grepl("\\(", vol_issue)) sub(".*\\((.*)\\).*", "\\1", vol_issue) else NA

      authors_list <- if (nchar(input$docAuthors %||% "") > 0) {
        trimws(strsplit(input$docAuthors, ";")[[1]])
      } else character(0)
      authors_json <- if (length(authors_list) > 0) jsonlite::toJSON(authors_list, auto_unbox = FALSE) else NA

      new_first_author_lastname <- if (length(authors_list) > 0) {
        first_author <- authors_list[1]
        if (grepl(",", first_author)) {
          trimws(sub(",.*", "", first_author))
        } else {
          words <- strsplit(trimws(first_author), "\\s+")[[1]]
          if (length(words) > 0) words[length(words)] else first_author
        }
      } else NA

      new_year <- if (!is.na(input$docYear) && input$docYear > 0) as.integer(input$docYear) else NA

      orig_year <- values$doc_metadata_original$publication_year[1]
      orig_author <- values$doc_metadata_original$first_author_lastname[1]

      year_changed <- !identical(as.integer(new_year), as.integer(orig_year))
      author_changed <- !identical(new_first_author_lastname, orig_author)

      if ((year_changed || author_changed) && !is.null(values$extracted_df) && nrow(values$extracted_df) > 0) {
        old_records <- values$extracted_df

        for (i in seq_len(nrow(old_records))) {
          old_id <- old_records$record_id[i]
          parts <- strsplit(old_id, "_")[[1]]
          if (length(parts) >= 4) {
            combo_num <- parts[length(parts) - 1]
            record_part <- parts[length(parts)]
            record_num <- sub("^r", "", record_part)
          } else {
            combo_num <- "1"
            record_num <- as.character(i)
          }

          author_part <- if (!is.na(new_first_author_lastname)) new_first_author_lastname else "Unknown"
          year_part <- if (!is.na(new_year)) new_year else "NA"
          new_record_id <- paste0(author_part, "_", year_part, "_", combo_num, "_r", record_num)

          if (old_id != new_record_id) {
            DBI::dbExecute(conn, "UPDATE records SET record_id = ? WHERE document_id = ? AND record_id = ?",
                          params = list(new_record_id, doc_id_int, old_id))

            values$extracted_df$record_id[i] <- new_record_id
            values$original_df$record_id[i] <- new_record_id
          }
        }

        DBI::dbExecute(conn, "UPDATE records SET publication_year = ? WHERE document_id = ?",
                      params = list(new_year, doc_id_int))
      }

      doc_metadata <- list(
        title = if (nchar(input$docTitle %||% "") > 0) input$docTitle else NA,
        authors = authors_json,
        publication_year = new_year,
        first_author_lastname = new_first_author_lastname,
        doi = if (nchar(input$docDoi %||% "") > 0) input$docDoi else NA,
        journal = if (nchar(input$docJournal %||% "") > 0) input$docJournal else NA,
        volume = if (nchar(volume) > 0) volume else NA,
        issue = if (!is.na(issue) && nchar(issue) > 0) issue else NA
      )

      do.call(ecoextract::save_document, c(
        list(
          document_id = doc_id_int,
          records_df = values$extracted_df %||% data.frame(),
          original_df = values$original_df,
          db_conn = conn
        ),
        doc_metadata
      ))

      values$unsaved_changes <- list()
      values$original_df <- values$extracted_df

      values$doc_metadata_original <- values$doc_metadata_original |>
        dplyr::mutate(
          publication_year = new_year,
          first_author_lastname = new_first_author_lastname,
          reviewed_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        )

      values$edit_trigger <- values$edit_trigger + 1

      shiny::showNotification("Document verified and saved to database!", type = "message", duration = 3)

      docs <- filtered_documents()
      if (!is.null(docs) && nrow(docs) > 0 && "reviewed_at" %in% colnames(docs)) {
        unreviewed <- docs |> dplyr::filter(is.na(reviewed_at))
        if (nrow(unreviewed) > 0 && unreviewed$document_id[1] != values$document_id) {
          shiny::updateSelectInput(session, "document_select", selected = unreviewed$document_id[1])
          shiny::showNotification("Advanced to next unreviewed document", type = "message", duration = 2)
        }
      }

    }, error = function(e) {
      shiny::showNotification(paste("Error saving:", e$message), type = "error", duration = 5)
    })

    shinyjs::html("acceptBtn", '<i class="fa fa-check"></i> Verify Records')
  })

  # OCR viewer output - renders tensorlake JSON to HTML with highlighting
  output$ocrViewer <- shiny::renderUI({
    shiny::req(values$markdown_text)

    rendered_html <- ecoreview::render_tensorlake_html(values$markdown_text)

    if (!is.null(values$selected_evidence) && length(values$selected_evidence) > 0) {
      rendered_html <- tryCatch({
        ecoreview::highlight_similar_text(rendered_html, values$selected_evidence)
      }, error = function(e) {
        rendered_html
      })
    }

    shiny::tagList(
      shiny::HTML(rendered_html),
      shiny::tags$script(shiny::HTML("setTimeout(function(){if(window.MathJax&&window.MathJax.typesetPromise){window.MathJax.typesetPromise();}},100);"))
    )
  })

  # Database export handler - exports all records joined with document metadata to CSV
  output$exportDbBtn <- shiny::downloadHandler(
    filename = function() {
      paste0(export_prefix, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      shiny::req(values$db_conn)

      records <- ecoextract::get_records(db_conn = values$db_conn)
      documents <- ecoextract::get_documents(db_conn = values$db_conn)

      export_data <- records |>
        dplyr::left_join(
          documents |> dplyr::select(dplyr::any_of(c("document_id", "file_name", "file_path", "reviewed_at"))),
          by = "document_id"
        )

      readr::write_csv(export_data, file)
    },
    contentType = "text/csv"
  )

  # Database download handler - saves the current database file (with all changes)
  output$downloadDbBtn <- shiny::downloadHandler(
    filename = function() {
      if (!is.null(values$db_name)) {
        values$db_name
      } else {
        paste0(export_prefix, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".db")
      }
    },
    content = function(file) {
      shiny::req(values$db_conn)
      file.copy(values$db_conn, file)
    },
    contentType = "application/x-sqlite3"
  )

  # Interactive data table
  output$interactiveTable <- DT::renderDataTable({
    shiny::req(values$extracted_df)
    values$edit_trigger

    display_data <- values$extracted_df

    if (is.null(display_data) || nrow(display_data) == 0) {
      return(DT::datatable(
        data.frame(Message = "No records found for this document"),
        options = list(dom = 't'),
        rownames = FALSE
      ))
    }

    dt <- ecoreview::create_styled_datatable(display_data, height = "600px", page_length = 15)

    if ("id" %in% names(display_data)) {
      tryCatch({
        dt <- ecoreview::apply_deleted_row_styling(dt, display_data)
        all_edited_cells <- ecoreview::get_all_edited_cells(values$document_id, values$unsaved_changes)
        dt <- ecoreview::apply_edited_styling(dt, values$extracted_df, all_edited_cells)
        restored_ids <- ecoreview::get_restored_interaction_ids(values$document_id)
        dt <- ecoreview::apply_restored_row_styling(dt, display_data, restored_ids)
      }, error = function(e) {
        # Styling functions may not exist, continue without styling
      })
    }

    return(dt)
  })

  # Handle cell edits
  shiny::observeEvent(input$interactiveTable_cell_edit, {
    shiny::req(values$extracted_df)
    edit_info <- input$interactiveTable_cell_edit

    row_num <- edit_info$row
    col_num <- edit_info$col + 1

    if (row_num >= nrow(values$extracted_df) || col_num >= ncol(values$extracted_df)) return()

    col_name <- names(values$extracted_df)[col_num]
    old_value <- values$extracted_df[[row_num, col_name]]

    current_type <- class(values$extracted_df[[col_name]])
    converted_value <- ecoreview::convert_edit_value(edit_info$value, current_type)

    values$extracted_df[[row_num, col_name]] <- converted_value

    if ("id" %in% names(values$extracted_df)) {
      interaction_id <- values$extracted_df[[row_num, "id"]]
      if (!is.na(interaction_id)) {
        current_record_id <- values$extracted_df[[row_num, "record_id"]]

        edit_key <- paste(interaction_id, col_name, sep = "_")
        values$unsaved_changes[[edit_key]] <- tibble::tibble(
          interaction_id = interaction_id,
          column_name = col_name,
          old_value = as.character(old_value),
          new_value = as.character(converted_value),
          timestamp = Sys.time(),
          record_id_snapshot = as.character(current_record_id)
        )

        values$edit_trigger <- values$edit_trigger + 1
      }
    }
  })

  # Handle row selection for text highlighting
  shiny::observe({
    if (!is.null(input$interactiveTable_rows_selected) && length(input$interactiveTable_rows_selected) > 0) {
      shiny::req(values$extracted_df)
      selected_row <- input$interactiveTable_rows_selected[1]
      if (selected_row <= nrow(values$extracted_df)) {
        row_data <- values$extracted_df[selected_row, ]
        if ("all_supporting_source_sentences" %in% names(row_data) && !is.na(row_data$all_supporting_source_sentences)) {
          tryCatch({
            sentences_array <- jsonlite::fromJSON(row_data$all_supporting_source_sentences)
            values$selected_evidence <- sentences_array
          }, error = function(e) {
            values$selected_evidence <- row_data$all_supporting_source_sentences
          })

          shinyjs::delay(500, {
            shinyjs::runjs("
              setTimeout(function() {
                var firstHighlight = document.querySelector('mark.text-highlight');
                if (firstHighlight) {
                  firstHighlight.scrollIntoView({behavior: 'smooth', block: 'center', inline: 'nearest'});
                }
              }, 100);
            ")
          })
        } else {
          values$selected_evidence <- NULL
        }
      }
    } else {
      values$selected_evidence <- NULL
    }
  })

  # Add row functionality
  shiny::observeEvent(input$addRowBtn, {
    shiny::req(values$extracted_df)
    new_row <- values$extracted_df[1, ]
    new_row[1, ] <- NA
    new_row$id <- max(values$extracted_df$id, na.rm = TRUE) + 1
    new_row$document_id <- values$document_id
    new_row$record_id <- paste0("NewRow-", new_row$id)
    values$extracted_df <- rbind(values$extracted_df, new_row)
    values$edit_trigger <- values$edit_trigger + 1
    shiny::showNotification("New row added. Click Accept to save changes.", type = "message", duration = 3)
  })

  # Delete row functionality
  shiny::observeEvent(input$deleteRowBtn, {
    shiny::req(values$extracted_df, input$interactiveTable_rows_selected)
    selected_row <- input$interactiveTable_rows_selected
    if (length(selected_row) > 0) {
      row_to_delete <- selected_row[1]
      interaction_id <- if ("id" %in% names(values$extracted_df)) values$extracted_df[[row_to_delete, "id"]] else NA

      if ("deleted_by_human" %in% names(values$extracted_df)) {
        values$extracted_df[[row_to_delete, "deleted_by_human"]] <- TRUE
      }

      if (!is.na(interaction_id)) {
        delete_key <- paste("DELETE", interaction_id, sep = "_")
        values$unsaved_changes[[delete_key]] <- tibble::tibble(
          interaction_id = interaction_id,
          column_name = "deleted_by_human",
          old_value = "FALSE",
          new_value = "TRUE",
          timestamp = Sys.time()
        )
      }

      values$edit_trigger <- values$edit_trigger + 1
      shiny::showNotification("Row marked for deletion. Click Accept to save changes.", type = "message", duration = 3)
    } else {
      shiny::showNotification("Please select a row to delete.", type = "warning", duration = 3)
    }
  })

  # Show/hide deleted toggle
  shiny::observeEvent(input$showDeletedBtn, {
    values$show_deleted <- !values$show_deleted
    if (values$show_deleted) {
      shiny::updateActionButton(session, "showDeletedBtn", label = "Hide Deleted", icon = shiny::icon("eye-slash"))
      shiny::showNotification("Showing deleted interactions", type = "message", duration = 2)
    } else {
      shiny::updateActionButton(session, "showDeletedBtn", label = "Show Deleted", icon = shiny::icon("eye"))
      shiny::showNotification("Hiding deleted interactions", type = "message", duration = 2)
    }
    values$edit_trigger <- values$edit_trigger + 1
  })

  # Flagged count (placeholder)
  output$flaggedCount <- shiny::renderText("0")
  shiny::outputOptions(output, "flaggedCount", suspendWhenHidden = FALSE)

  # Show accuracy modal
  shiny::observeEvent(input$showAccuracyBtn, {
    acc <- accuracy_data()
    has_verified <- !is.null(acc) && acc$verified_documents > 0

    shiny::showModal(shiny::modalDialog(
      title = "Extraction Accuracy Metrics",
      size = "l",
      easyClose = TRUE,
      footer = shiny::tagList(
        if (has_verified) {
          shiny::downloadButton("exportAccuracyBtn", "Export Metrics CSV",
                         class = "btn-primary")
        },
        shiny::modalButton("Close")
      ),

      if (has_verified) {
        shiny::div(
          shiny::fluidRow(
            shiny::column(6,
              shiny::h4("Overall Metrics"),
              shiny::tableOutput("accuracyMetricsTable")
            ),
            shiny::column(6,
              shiny::h4("Summary"),
              shiny::tableOutput("accuracySummaryTable")
            )
          ),
          shiny::hr(),
          shiny::h4("Column-Level Accuracy"),
          shiny::p(style = "font-size: 0.9em; color: #6c757d; margin-bottom: 10px;",
            shiny::tags$i(class = "fas fa-info-circle", style = "margin-right: 5px;"),
            "Hover over the chart to reveal the toolbar. Click the camera icon to download as PNG."
          ),
          plotly::plotlyOutput("columnAccuracyPlot", height = "300px"),
          shiny::hr(),
          shiny::h4("Edit Counts by Column"),
          shiny::tableOutput("columnEditsTable")
        )
      } else {
        shiny::div(style = "text-align: center; color: #6c757d; padding: 40px;",
          shiny::tags$i(class = "fas fa-chart-bar", style = "font-size: 48px; opacity: 0.5; margin-bottom: 15px;"),
          shiny::h4("No Verified Documents"),
          shiny::p("Accuracy metrics are calculated from verified documents."),
          shiny::p("Use the 'Verify Records' button to mark documents as reviewed.")
        )
      }
    ))
  })

  # Accuracy metrics outputs
  accuracy_data <- shiny::reactive({
    shiny::req(values$db_conn)
    values$edit_trigger
    tryCatch({
      ecoextract:::calculate_accuracy(values$db_conn)
    }, error = function(e) {
      list(verified_documents = 0, message = e$message)
    })
  })

  output$hasVerifiedDocs <- shiny::reactive({
    acc <- accuracy_data()
    !is.null(acc) && acc$verified_documents > 0
  })
  shiny::outputOptions(output, "hasVerifiedDocs", suspendWhenHidden = FALSE)

  output$accuracyMetricsTable <- shiny::renderTable({
    acc <- accuracy_data()
    shiny::req(acc$verified_documents > 0)

    data.frame(
      Metric = c("Precision", "Recall", "F1 Score"),
      Value = c(
        sprintf("%.1f%%", acc$precision * 100),
        sprintf("%.1f%%", acc$recall * 100),
        sprintf("%.1f%%", acc$f1_score * 100)
      )
    )
  }, striped = TRUE, hover = TRUE, width = "100%")

  output$accuracySummaryTable <- shiny::renderTable({
    acc <- accuracy_data()
    shiny::req(acc$verified_documents > 0)

    data.frame(
      Category = c("Verified Documents", "Total Records", "Model Extracted",
                   "Correct (unchanged)", "Edited", "Deleted (false positives)",
                   "Human Added (missed)"),
      Count = c(acc$verified_documents, acc$verified_records, acc$model_extracted,
                acc$correct, acc$edited, acc$deleted, acc$human_added)
    )
  }, striped = TRUE, hover = TRUE, width = "100%")

  output$columnAccuracyPlot <- plotly::renderPlotly({
    acc <- accuracy_data()
    shiny::req(acc$verified_documents > 0)
    shiny::req(length(acc$column_accuracy) > 0)

    col_acc_df <- data.frame(
      Column = names(acc$column_accuracy),
      Accuracy = acc$column_accuracy * 100
    )

    col_acc_df <- col_acc_df[order(col_acc_df$Accuracy), ]
    col_acc_df$Column <- factor(col_acc_df$Column, levels = col_acc_df$Column)

    col_acc_df$color_val <- sqrt(100 - col_acc_df$Accuracy)

    plotly::plot_ly(col_acc_df,
            y = ~Column,
            x = ~Accuracy,
            type = "bar",
            orientation = "h",
            marker = list(
              color = ~color_val,
              colorscale = "Viridis",
              reversescale = TRUE,
              showscale = FALSE
            ),
            hovertemplate = "%{y}: %{x:.1f}%<extra></extra>",
            showlegend = FALSE) %>%
      plotly::layout(
        xaxis = list(title = "Accuracy (%)", range = c(0, 100)),
        yaxis = list(title = ""),
        shapes = list(
          list(type = "line", x0 = 90, x1 = 90, y0 = -0.5, y1 = nrow(col_acc_df) - 0.5,
               line = list(color = "gray", dash = "dash"))
        ),
        annotations = list(
          list(x = 90, y = nrow(col_acc_df) - 0.5, text = "90%",
               showarrow = FALSE, xanchor = "left", yanchor = "bottom",
               font = list(size = 10, color = "gray"))
        ),
        margin = list(l = 150)
      )
  })

  output$columnEditsTable <- shiny::renderTable({
    acc <- accuracy_data()
    shiny::req(acc$verified_documents > 0)

    if (length(acc$column_edits) == 0) {
      return(data.frame(Column = "No edits recorded", `Edit Count` = 0, check.names = FALSE))
    }

    col_edits_df <- data.frame(
      Column = names(acc$column_edits),
      `Edit Count` = acc$column_edits,
      `Accuracy` = sprintf("%.1f%%", acc$column_accuracy[names(acc$column_edits)] * 100),
      check.names = FALSE
    )
    col_edits_df <- col_edits_df[order(-col_edits_df$`Edit Count`), ]
    col_edits_df
  }, striped = TRUE, hover = TRUE, width = "100%")

  # Accuracy metrics export handler
  output$exportAccuracyBtn <- shiny::downloadHandler(
    filename = function() {
      paste0(export_prefix, "_accuracy_metrics_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      acc <- accuracy_data()
      shiny::req(acc$verified_documents > 0)

      # Combine all metrics into a single data frame
      metrics_df <- data.frame(
        Section = c(
          "Overall Metrics", "Overall Metrics", "Overall Metrics",
          rep("Summary", 7),
          rep("Column Accuracy", length(acc$column_accuracy)),
          rep("Column Edits", length(acc$column_edits))
        ),
        Metric = c(
          "Precision", "Recall", "F1 Score",
          "Verified Documents", "Total Records", "Model Extracted",
          "Correct (unchanged)", "Edited", "Deleted (false positives)",
          "Human Added (missed)",
          names(acc$column_accuracy),
          names(acc$column_edits)
        ),
        Value = c(
          sprintf("%.1f%%", acc$precision * 100),
          sprintf("%.1f%%", acc$recall * 100),
          sprintf("%.1f%%", acc$f1_score * 100),
          as.character(acc$verified_documents),
          as.character(acc$verified_records),
          as.character(acc$model_extracted),
          as.character(acc$correct),
          as.character(acc$edited),
          as.character(acc$deleted),
          as.character(acc$human_added),
          sprintf("%.1f%%", acc$column_accuracy * 100),
          as.character(acc$column_edits)
        )
      )

      readr::write_csv(metrics_df, file)
    },
    contentType = "text/csv"
  )

  # Session cleanup
  session$allowReconnect(TRUE)
  session$onSessionEnded(function() {
    if (exists("cleanup_temp_files", where = asNamespace("ecoreview"))) {
      tryCatch(ecoreview::cleanup_temp_files(), error = function(e) NULL)
    }
  })
}

# Launch the application
shiny::shinyApp(ui = ui, server = server)
