#' User-interface for transforming data
#'
#' An interactive shiny app for transforming data. Also available online at
#' <http://animalnexus.ca> or by launching the local animalnexus app through
#' \code{animalnexus()}.
#'
#' See individual data transformations for more details: \code{visits()},
#' \code{presence()}, \code{move()}, \code{disp()}, \code{activity()}, and
#' \code{daily()}.
#'
#' @param r Data frame. Raw RFID data to transform
#' @param verbose Logical. Print log events to console.
#'
#' @return Returns nothing. Data can be saved to disk from the user-interface,
#'   but transformation are not returned to the current R session.
#'
#' @examples
#' \dontrun{
#'   ui_trans(r = my_data)
#' }
#'
#' @export
ui_trans <- function(r, verbose = FALSE) {
  if(missing(r)) stop("ui_trans() requires raw data to transform")

  addResourcePath("assets", system.file("extdata", "shiny-examples",
                                        "app_files", package = "feedrUI"))

  shiny::shinyApp(ui = shiny::fluidPage(
    includeCSS(system.file("shiny-examples", "app_files", "style.css",
                           package = "feedr")),
    shinyjs::useShinyjs(),
    mod_UI_nav("standalone",
               tabPanel("Transformations", icon = icon("exchange-alt"),
                        mod_UI_trans("standalone")),
               tabPanel("Settings", icon = icon("cog"),
                        mod_UI_settings("standalone_trans"))#,
               #mod_UI_stop("stp")
    )),
    server = function(input, output, session) {
      settings <- shiny::callModule(mod_settings, id = "standalone_trans", verbose = verbose)
      shiny::callModule(mod_trans, id = "standalone",
                        r = shiny::reactive({r}),
                        settings = settings,
                        verbose = verbose)
      #shiny::callModule(mod_stop, id = "stp")  # Add Exit Buttons
      session$onSessionEnded(stopApp)
    }
  )
}

trans_preamble <- function(args = TRUE) {
  trans_functions <- c("raw", "visits", "move", "presence", "disp", "activity", "daily")
  manual <- feedr:::man %>%
    dplyr::right_join(
      tibble::tibble(
        f = trans_functions,
        req = c(NA, "raw", "visits", "visits", "visits", "presence", "activity"),
        p = round(seq(0, 1, length.out = 7), 2),
        details = c("<h3>Raw RFID data</h3> <p>Each row corresponds to an RFID 'read' event.</p>",
                    "<h3>Visits</h3> <p>Each row corresponds to a single 'visit' to the reader. Visits are defined as a series of consecutive RFID reads, with each read occurring within 3s of the next. See the visits() function in the feedr package for R to fine tune these settings.</p><p>Animal N and Logger N refer to the total number of individuals and readers in the data, respectively.</p>",
                    "<h3>Presence</h3> <p>Each row corresponds to a single 'presence event' at the reader if the reader is a logger, or a period of time spent near the reader otherwise. These are defined as a series of visits at a single logger separated by no more than 15min. See the presence() function in the feedr package for R to fine tune these settings.</p><p>Start and End reflect the start and end of the time present and length refers to the length in minutes.</p><p>Animal N and Logger N refer to the total number of individuals and readers in the data, respectively.</p>",
                    "<h3>Movements</h3> <p>Each two rows correspond to a single 'movement' from one reader to another. A movement is defined as the last and first consecutive visits made by an individual to two different readers.</p><p>Move Id refers to the unique identifier of each movement made by an individual. Move Path reflects the unique path between readers (without accounting for direction) whereas Move Dir reflect s the unique path between readers, including direction. Strength is a measure of how connected two readers are and is calculated as the inverse of time taken to move between the readers.</p>",
                    "<h3>Displacements</h3> <p>Each row corresponds to a single displacement event recorded at a particular RFID logger. Displacements are events when one animal leaves the logger within 5s of the arrival of another. In some species this can be used to infer dominance.</p>",
                    "<h3>Activity</h3> <p>Each row corresponds to a 15-min time period and is scored as active or inactive (activity_c) or 1 or 0 (activity). Activity is definied by whether or not the individual had a 'presence' bout (at any logger) which overlapped the 15-min time slot. Rise and set reflect the time of sunrise and sun set based on the lat/lon of the logger.</p>",
                    "<h3>Daily Activity</h3> <p>Each row corresponds to an average activity score for that 15-min period calculated across all days included in the activity dataset. Rise and set reflect the time of sunrise and sun set based on the lat/lon of the logger.</p>
                    <p> Note that as this represents an average 24-hr activity cycle, output dates are irrelevant, as the data is tied to times, not dates. Therefore the dates are all assigned to 1970-01-01.</p>")),
      by = "f") %>%
    dplyr::mutate(f = factor(f, levels = trans_functions),
                  title = replace(title, f == "raw", "Raw"),
                  file_name = tolower(stringr::str_extract(title, "^[^ ]*"))) %>%
    dplyr::arrange(f) %>%
    dplyr::mutate(f = as.character(f))

  if(args) return(dplyr::filter(manual, !is.na(value), !(arg %in% c("missing", "pass"))))
  if(!args) return(dplyr::distinct(dplyr::select(manual, -arg, -desc, -class, -value, -id, -lab)))
}

#' @import shiny
#' @import magrittr
mod_UI_trans <- function(id) {
  ns <- NS(id)

  tagList(
    column(3,
           htmlOutput(ns("data_desc")),
           hr(),
           h3("Downloads"),
           shinyjs::disabled(downloadButton(ns("data_dl"), "All")),
           hr(),
           uiOutput(ns("dl_buttons"))
    ),
    column(9, uiOutput(ns("data_tables")))
  )
}

# Module server function
#' @import shiny
#' @import magrittr
mod_trans <- function(input, output, session, r, settings, verbose = FALSE) {

  ns <- session$ns

  # Values ---------------------------------------------

  types_args <- trans_preamble()
  types <- trans_preamble(args = FALSE)

  # Help ---------------------------------------------------
  # Data Descriptions
  output$data_desc <- renderText({
    req(input$data_tabs)
    types$details[types$title == input$data_tabs]
  })

  # Data Transformations --------------------------------
  raw <- reactive({
    req(r())

    raw <- r()
    if(!("dataaccess" %in% names(raw))) raw$dataaccess <- 0

    raw
  })

  feedrTrans <- function(fun, required, name) {
    req(settings(), required)
    withProgress(message = paste("Transforming", name), {

      if(name == "Visits") {
        shinyjs::html(
          id = "messages",
          html = paste0(
            "Date: ", Sys.Date(), "\n",
            "feedr version: ", utils::packageVersion("feedr"), "\n",
            paste0(paste0(names(settings()), ": ", settings()), collapse = "\n"),
            "\n"))
      }

      shinyjs::html(
        id = "messages",
        html = paste0("\n", name, ": \n"),
        add = TRUE)

      x <- try(withCallingHandlers(
        fun,
        error = function(e) {
          shinyjs::html(id = "messages", html = e$message, add = TRUE)
        },
        message = function(m) {
          shinyjs::html(id = "messages", html = m$message, add = TRUE)
        }
      ), silent = TRUE)

      x
    })
  }

  v <- reactive({
    feedrTrans(fun = visits(
      dplyr::filter(raw(), dataaccess == 0) %>% dplyr::select(-"dataaccess"),
      bw = as.numeric(settings()$set_visits_bw),
      allow_imp = as.logical(settings()$set_visits_allow_imp),
      na_rm = as.logical(settings()$set_visits_na_rm),
      bw_imp = as.numeric(settings()$set_visits_bw_imp)),
      required = raw(),
      name = "Visits")
  })

  m <- reactive({
    feedrTrans(fun = move(v(), all = as.logical(settings()$set_move_all)),
               required = v(),
               name = "Movements")
  })

  p <- reactive({
    feedrTrans(fun = presence(v(), bw = as.numeric(settings()$set_presence_bw)),
               required = v(),
               name = "Presence")
  })

  d <- reactive({
    feedrTrans(fun = disp(v(), bw = as.numeric(settings()$set_disp_bw))$displacements,
               required = v(),
               name = "Displacements")
  })

  a <- reactive({
    feedrTrans(fun = activity(
      p(),
      res = as.numeric(settings()$set_activity_res),
      by_logger = as.logical(settings()$set_activity_by_logger),
      keep_all = as.logical(settings()$set_activity_keep_all),
      sun = as.logical(settings()$set_activity_sun)),
      required = p(),
      name = "Activity")
  })

  da <- reactive({
    feedrTrans(fun = daily(a()),
               required = a(),
               name = "Daily Activity")
  })

  # Buttons ------------------------------------------------
  ## Create download buttons
  output$dl_buttons <- renderUI({
    lapply(1:nrow(types), function(x) {
      x <- types[x, ]
      htmltools::p(downloadButton(ns(paste0('data_dl_', x$f)), x$title))
    })
  })

  ## Activate/deactivate buttons
  # observe({
  #   req("data_tabs" %in% names(input))
  #   shinyjs::toggleState("data_dl_visits", condition = !is.null(v()) && nrow(v()) > 0)
  #   shinyjs::toggleState("data_dl_move", condition = !is.null(m()) && nrow(m()) > 0)
  #   shinyjs::toggleState("data_dl_presence", condition = !is.null(p()) && nrow(p()) > 0)
  #   shinyjs::toggleState("data_dl_disp", condition = !is.null(d()) && nrow(d()) > 0)
  #   shinyjs::toggleState("data_dl_activity", condition = !is.null(a()) && nrow(a()) > 0)
  #   shinyjs::toggleState("data_dl_daily", condition = !is.null(da()) && nrow(da()) > 0)
  #
  #   shinyjs::toggleState("data_dl",
  #                        condition = ("data_tabs" %in% names(input)) && nrow(raw()) > 0)
  # })

  # Messages -------------------------------------------------
  msg_select <- "Please select data through the Database or by Importing"
  msg_error <- "No data (see log for more details)"
  msg_private <- "None of the currently selected data is available for download.\n
  Some of data in our Database is restricted to visualizations only to protect the hard work of scientists until they've had a chance to publish their findings."


  # Table Output ---------------------------------------------

  prepTables <- function(x) {
    req(x)
    validate(need(!is.null(r()), msg_select))
    validate(need(!is.null(raw()) && nrow(raw()) > 0, msg_private))
    validate(need(nrow(x) > 0, msg_error))

    x <- dplyr::mutate(x,
                       dplyr::across(tidyselect:::where(lubridate::is.POSIXct),
                                     as.character))
    DT::datatable(x,
                  filter = "top",
                  options = list(pageLength = 100),
                  rownames = FALSE,
                  colnames = gsub("_", " ", names(x)) %>% gsub("\\b(\\w)", "\\U\\1", ., perl=TRUE))
  }

  output$dt_raw <- DT::renderDataTable(prepTables(dplyr::select(raw(), -"dataaccess")))
  output$dt_visits <- DT::renderDataTable(prepTables(v()))
  output$dt_move <- DT::renderDataTable(prepTables(m()))
  output$dt_presence <- DT::renderDataTable(prepTables(p()))
  output$dt_disp <- DT::renderDataTable(prepTables(d()))
  output$dt_activity <- DT::renderDataTable(prepTables(a()))
  output$dt_daily <- DT::renderDataTable(prepTables(da()))

  # Downloads ---------------------------------------------------
  observe({
    req(raw())
    lapply(paste0("data_dl_", types$f), shinyjs::enable)
    shinyjs::enable("data_dl")
  })

  feedrDownload <- function(x, name) {
    downloadHandler(
      filename = function() paste0(name, "_", Sys.Date(), '.csv'),
      content = function(file) {
        req(x)
        utils::write.csv(x, file, row.names = FALSE)
      }
    )
  }

  output$data_dl_raw <- feedrDownload(raw(), "raw")
  output$data_dl_visits <- feedrDownload(v(), "visits")
  output$data_dl_move <- feedrDownload(m(), "movements")
  output$data_dl_presence <- feedrDownload(p(), "presence")
  output$data_dl_disp <- feedrDownload(d(), "displacements")
  output$data_dl_activity <- feedrDownload(a(), "activity")
  output$data_dl_daily <- feedrDownload(da(), "daily")

  # Download All
  output$data_dl <- downloadHandler(
    filename = function() paste0("feedr_all_", Sys.Date(), ".zip"),
    content = function(file) {
      tmpdir <- tempdir()
      setwd(tempdir())
      cat(tempdir())
      req(v(), m(), p(), d(), a(), da())

      ## Regular files
      fs <- paste0(types$file_name, "_", Sys.Date(), ".csv")
      utils::write.csv(raw(), file = fs[1], row.names = FALSE)
      utils::write.csv(v(), file = fs[2], row.names = FALSE)
      utils::write.csv(m(), file = fs[3], row.names = FALSE)
      utils::write.csv(p(), file = fs[4], row.names = FALSE)
      utils::write.csv(d(), file = fs[5], row.names = FALSE)
      utils::write.csv(a(), file = fs[6], row.names = FALSE)
      utils::write.csv(da(), file = fs[7], row.names = FALSE)

      write(fs[length(fs)])

      utils::zip(zipfile = file, files = fs)
    },
    contentType = "application/zip"
  )

  # Tabs -------------------------------------------------------------
  output$data_tables <- renderUI({
    if(verbose) cat("Rendering data tabs\n")
    tabs <- lapply(1:nrow(types), function(x) {
      x <- types[x, ]
      tabPanel(x$title, DT::dataTableOutput(ns(paste0("dt_", x$f))))
    })
    tabs[[length(tabs) + 1]] <- tabPanel("Log", value = "log",
                                         verbatimTextOutput(ns("messages"),
                                                            placeholder = TRUE))
    tabs$id = ns("data_tabs")
    do.call(tabsetPanel, tabs)
  })


  snapshotPreprocessInput("dt_raw_state", function(value) {})
  snapshotPreprocessInput("dt_visits_state", function(value) {})
  snapshotPreprocessInput("dt_move_state", function(value) {})
  snapshotPreprocessInput("dt_presence_state", function(value) {})
  snapshotPreprocessInput("dt_disp_state", function(value) {})
  snapshotPreprocessInput("dt_activity_state", function(value) {})
  snapshotPreprocessInput("dt_daily_state", function(value) {})

# Return ------------------------------------------------------------------
  return(c(raw = reactive({r()}),
           visits = reactive({v()})))
}
