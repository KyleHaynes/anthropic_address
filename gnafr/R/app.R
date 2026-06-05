#' Launch an interactive geocoding app for GNAF matching
#'
#' Opens a Shiny app that lets you connect to a DuckDB database, submit one
#' address per line, review `gnaf_match()` results, and inspect full-string
#' Jaro-Winkler and Jaccard similarity diagnostics between the input string and
#' the matched address label.
#'
#' @param con Optional DBI connection created by [gnaf_connect()]. If omitted,
#'   the app can open a connection from `db_path`.
#' @param db_path Optional DuckDB path to pre-fill in the app and connect to on
#'   launch when `con` is `NULL`.
#' @param launch.browser Passed to [shiny::runApp()] when `run = TRUE`.
#' @param run If `TRUE` (default), launches the app immediately. If `FALSE`,
#'   returns the [shiny::shiny.appobj()] for embedding or testing.
#' @return Invisibly returns the app object when `run = TRUE`, otherwise returns
#'   the app object.
#' @export
gnaf_app <- function(con = NULL, db_path = NULL,
                     launch.browser = interactive(), run = TRUE) {
  .gnaf_require_app_packages()

  if (!is.null(con) && !inherits(con, "DBIConnection")) {
    stop("'con' must be a DBI connection returned by gnaf_connect()")
  }
  if (!is.null(db_path) && (!is.character(db_path) || length(db_path) != 1L)) {
    stop("'db_path' must be a single character string")
  }

  app_state <- new.env(parent = emptyenv())
  app_state$con <- con
  app_state$owns_connection <- FALSE

  ui <- shiny::fluidPage(
    shiny::tags$head(
      shiny::tags$style(shiny::HTML(
        ".app-shell {max-width: 1380px; margin: 0 auto; padding-bottom: 24px;}\n",
        ".app-title {margin: 20px 0 6px; font-size: 30px; font-weight: 700; color: #102a43;}\n",
        ".app-subtitle {margin-bottom: 22px; color: #486581;}\n",
        ".panel {background: #f8fbff; border: 1px solid #d9e2ec; border-radius: 14px; padding: 18px;}\n",
        ".metric-grid {display: grid; grid-template-columns: repeat(3, minmax(0, 1fr)); gap: 12px; margin: 16px 0 20px;}\n",
        ".metric-card {background: linear-gradient(135deg, #102a43, #1f5f8b); color: #fff; border-radius: 14px; padding: 16px 18px;}\n",
        ".metric-label {font-size: 12px; text-transform: uppercase; letter-spacing: 0.08em; opacity: 0.8;}\n",
        ".metric-value {font-size: 28px; font-weight: 700; line-height: 1.2; margin-top: 6px;}\n",
        ".status-pill {display: inline-block; padding: 6px 10px; border-radius: 999px; background: #d9f99d; color: #365314; font-weight: 600;}\n",
        ".status-pill--warn {background: #fee2e2; color: #991b1b;}\n",
        ".help-text {color: #52606d; font-size: 13px;}\n",
        ".section-gap {margin-top: 16px;}\n",
        "@media (max-width: 900px) {.metric-grid {grid-template-columns: 1fr;}}"
      ))
    ),
    shiny::div(
      class = "app-shell",
      shiny::div(class = "app-title", "gnafr geocoder"),
      shiny::div(
        class = "app-subtitle",
        "Run address matching against your DuckDB-backed GNAF database and inspect text similarity diagnostics for each match."
      ),
      shiny::fluidRow(
        shiny::column(
          width = 4,
          shiny::div(
            class = "panel",
            shiny::textInput("db_path", "DuckDB path", value = if (is.null(db_path)) "" else db_path),
            shiny::actionButton("connect", "Connect", class = "btn-primary"),
            shiny::div(class = "section-gap"),
            shiny::uiOutput("connection_status"),
            shiny::hr(),
            shiny::textAreaInput(
              "addresses",
              "Addresses",
              rows = 12,
              placeholder = paste(
                "10 St James Ct, Tamborine Mountain QLD 4272",
                "77 broadwater rd mount gravatt east 4122",
                sep = "\n"
              )
            ),
            shiny::numericInput("max_results", "Matches per input", value = 3, min = 1, max = 20, step = 1),
            shiny::numericInput("min_score", "Minimum total score", value = 40, min = 0, max = 100, step = 1),
            shiny::checkboxInput("include_custom", "Include custom addresses", value = TRUE),
            shiny::checkboxInput("locality_fallback", "Enable locality fallback", value = TRUE),
            shiny::numericInput("fallback_threshold", "Fallback threshold", value = 80, min = 0, max = 100, step = 1),
            shiny::actionButton("run_match", "Geocode", class = "btn-success"),
            shiny::downloadButton("download_results", "Download CSV"),
            shiny::div(
              class = "help-text section-gap",
              "The results table adds full-string Jaro-Winkler and Jaccard similarity scores between each input string and its matched address label."
            )
          )
        ),
        shiny::column(
          width = 8,
          shiny::uiOutput("metrics"),
          shiny::tabsetPanel(
            shiny::tabPanel("Matches", reactable::reactableOutput("results_table")),
            shiny::tabPanel("Parsed Inputs", reactable::reactableOutput("parsed_table"))
          )
        )
      )
    )
  )

  server <- function(input, output, session) {
    connection_info <- shiny::reactiveVal(list(connected = !is.null(con), path = db_path, status = NULL))
    results_rv <- shiny::reactiveVal(.gnaf_empty_app_results())
    parsed_rv <- shiny::reactiveVal(.gnaf_empty_parsed())

    release_connection <- function() {
      if (isTRUE(app_state$owns_connection) && !is.null(app_state$con)) {
        try(gnaf_disconnect(app_state$con), silent = TRUE)
      }
      app_state$con <- NULL
      app_state$owns_connection <- FALSE
    }

    current_con <- shiny::reactive({
      app_state$con
    })

    if (!is.null(con)) {
      connection_info(list(
        connected = TRUE,
        path = if (is.null(db_path)) "Existing connection" else db_path,
        status = tryCatch(gnaf_status(con), error = function(e) NULL)
      ))
    } else if (!is.null(db_path) && nzchar(trimws(db_path))) {
      tryCatch({
        app_state$con <- gnaf_connect(trimws(db_path))
        app_state$owns_connection <- TRUE
        connection_info(list(
          connected = TRUE,
          path = trimws(db_path),
          status = gnaf_status(app_state$con)
        ))
      }, error = function(e) {
        connection_info(list(connected = FALSE, path = trimws(db_path), status = conditionMessage(e)))
      })
    }

    shiny::observeEvent(input$connect, {
      shiny::req(nzchar(trimws(input$db_path)))

      tryCatch({
        release_connection()
        app_state$con <- gnaf_connect(trimws(input$db_path))
        app_state$owns_connection <- TRUE
        connection_info(list(
          connected = TRUE,
          path = trimws(input$db_path),
          status = gnaf_status(app_state$con)
        ))
        shiny::showNotification("Connected to DuckDB database.", type = "message")
      }, error = function(e) {
        connection_info(list(connected = FALSE, path = trimws(input$db_path), status = conditionMessage(e)))
        shiny::showNotification(conditionMessage(e), type = "error", duration = NULL)
      })
    })

    shiny::observeEvent(input$run_match, {
      shiny::req(current_con())

      addresses <- trimws(unlist(strsplit(input$addresses %||% "", "\\r?\\n", perl = TRUE)))
      addresses <- addresses[nzchar(addresses)]

      if (length(addresses) == 0L) {
        shiny::showNotification("Enter at least one address.", type = "warning")
        return(invisible(NULL))
      }

      tryCatch({
        shiny::withProgress(message = "Geocoding", value = 0.1, {
          parsed <- address_parse(addresses)
          shiny::incProgress(0.35, detail = "Parsed input addresses")

          matches <- gnaf_match(
            current_con(),
            addresses,
            max_results = as.integer(input$max_results),
            min_score = as.integer(input$min_score),
            include_custom = isTRUE(input$include_custom),
            locality_fallback = isTRUE(input$locality_fallback),
            fallback_threshold = as.integer(input$fallback_threshold)
          )
          shiny::incProgress(0.45, detail = "Matched against database")

          results_rv(.gnaf_prepare_app_results(matches))
          parsed_rv(parsed)
          shiny::incProgress(0.1, detail = "Rendered tables")
        })
      }, error = function(e) {
        results_rv(.gnaf_empty_app_results())
        parsed_rv(.gnaf_empty_parsed())
        shiny::showNotification(conditionMessage(e), type = "error", duration = NULL)
      })
    })

    session$onSessionEnded(function() {
      release_connection()
    })

    output$connection_status <- shiny::renderUI({
      info <- connection_info()
      if (isTRUE(info$connected)) {
        status_rows <- info$status
        rows_text <- if (is.data.table(status_rows) && nrow(status_rows) > 0L) {
          paste(sprintf("%s: %s", status_rows$table, format(status_rows$rows, big.mark = ",")), collapse = " | ")
        } else {
          "Connection ready"
        }

        shiny::tagList(
          shiny::div(class = "status-pill", "Connected"),
          shiny::p(shiny::tags$strong("Database:"), info$path %||% "Existing connection"),
          shiny::p(rows_text, class = "help-text")
        )
      } else {
        message_text <- info$status %||% "Not connected"
        shiny::tagList(
          shiny::div(class = "status-pill status-pill--warn", "Disconnected"),
          shiny::p(message_text, class = "help-text")
        )
      }
    })

    output$metrics <- shiny::renderUI({
      results <- results_rv()
      best <- results[match_rank == 1L]
      matched_inputs <- uniqueN(best$input_id)
      avg_score <- if (nrow(best) > 0L) round(mean(best$total_score, na.rm = TRUE), 1) else NA_real_
      avg_text <- if (nrow(best) > 0L) round(mean(best$text_similarity, na.rm = TRUE), 1) else NA_real_

      shiny::div(
        class = "metric-grid",
        .gnaf_metric_card("Matched inputs", format(matched_inputs, big.mark = ",")),
        .gnaf_metric_card("Avg total score", if (is.na(avg_score)) "-" else sprintf("%.1f", avg_score)),
        .gnaf_metric_card("Avg text similarity", if (is.na(avg_text)) "-" else sprintf("%.1f", avg_text))
      )
    })

    output$results_table <- reactable::renderReactable({
      results <- results_rv()
      score_fill <- function(score) .gnaf_score_fill(score)
      text_cell <- function(value, index) {
        fill <- score_fill(results$text_similarity[index])
        shiny::div(
          style = sprintf(
            "background:%s; border-radius:10px; padding:8px 10px; font-weight:600;",
            fill
          ),
          value
        )
      }

      reactable::reactable(
        results,
        defaultPageSize = 12,
        searchable = TRUE,
        filterable = TRUE,
        highlight = TRUE,
        striped = TRUE,
        defaultSorted = list(total_score = "desc"),
        columns = list(
          input_id = reactable::colDef(name = "Input", maxWidth = 80),
          match_rank = reactable::colDef(name = "Rank", maxWidth = 80),
          input_raw = reactable::colDef(name = "Input string", minWidth = 250, cell = text_cell),
          address_label = reactable::colDef(name = "Matched string", minWidth = 280, cell = text_cell),
          total_score = .gnaf_score_col("Total", digits = 0),
          text_similarity = .gnaf_score_col("Text score", digits = 1),
          jarowinkler_score = .gnaf_score_col("Jaro-Winkler", digits = 1),
          jaccard_score = .gnaf_score_col("Jaccard", digits = 1),
          longitude = reactable::colDef(format = reactable::colFormat(digits = 6)),
          latitude = reactable::colDef(format = reactable::colFormat(digits = 6))
        ),
        details = function(index) {
          row <- results[index, ]
          parsed <- parsed_rv()[input_id == row$input_id, .(
            in_locality,
            in_street_name,
            in_street_type,
            in_number_first,
            in_number_last,
            in_flat_type,
            in_flat_number,
            in_building_name
          )]

          shiny::tagList(
            shiny::tags$div(
              style = "padding: 10px 14px; background: #f8fbff; border-radius: 10px;",
              shiny::tags$strong("Parsed input"),
              reactable::reactable(parsed, compact = TRUE, bordered = TRUE, pagination = FALSE)
            )
          )
        }
      )
    })

    output$parsed_table <- reactable::renderReactable({
      reactable::reactable(
        parsed_rv(),
        defaultPageSize = 10,
        searchable = TRUE,
        filterable = TRUE,
        striped = TRUE,
        columns = list(input_raw = reactable::colDef(minWidth = 280))
      )
    })

    output$download_results <- shiny::downloadHandler(
      filename = function() {
        sprintf("gnafr-geocode-%s.csv", format(Sys.time(), "%Y%m%d-%H%M%S"))
      },
      content = function(file) {
        data.table::fwrite(results_rv(), file)
      }
    )
  }

  app <- shiny::shinyApp(ui = ui, server = server)
  if (!isTRUE(run)) {
    return(app)
  }

  shiny::runApp(app, launch.browser = launch.browser)
  invisible(app)
}

.gnaf_require_app_packages <- function() {
  needed <- c("shiny", "reactable")
  missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0L) {
    stop(
      "Install app dependencies first: install.packages(c(",
      paste(sprintf('"%s"', missing), collapse = ", "),
      "))",
      call. = FALSE
    )
  }
}

.gnaf_prepare_app_results <- function(results) {
  if (!is.data.table(results) || nrow(results) == 0L) {
    return(.gnaf_empty_app_results())
  }

  out <- copy(results)
  input_norm <- .normalize_addr(out$input_raw)
  match_norm <- .normalize_addr(out$address_label)

  jw <- 1 - stringdist::stringdist(input_norm, match_norm, method = "jw", p = 0.1)
  jaccard <- 1 - stringdist::stringdist(input_norm, match_norm, method = "jaccard", q = 2)

  out[, jarowinkler_score := round(pmax(jw, 0) * 100, 1)]
  out[, jaccard_score := round(pmax(jaccard, 0) * 100, 1)]
  out[, text_similarity := round((jarowinkler_score + jaccard_score) / 2, 1)]
  out[]
}

.gnaf_empty_app_results <- function() {
  data.table(
    input_id = integer(),
    input_raw = character(),
    match_rank = integer(),
    total_score = integer(),
    score_postcode = integer(),
    score_suburb = integer(),
    score_street_name = integer(),
    score_street_type = integer(),
    score_number = integer(),
    score_flat = integer(),
    address_detail_pid = character(),
    address_label = character(),
    building_name = character(),
    flat_type = character(),
    flat_number = character(),
    number_first = integer(),
    number_last = integer(),
    street_name = character(),
    street_type = character(),
    street_suffix = character(),
    locality_name = character(),
    state = character(),
    postcode = integer(),
    longitude = numeric(),
    latitude = numeric(),
    source = character(),
    in_postcode = integer(),
    in_state = character(),
    in_locality = character(),
    in_street_name = character(),
    in_street_type = character(),
    in_street_suffix = character(),
    in_number_last = integer(),
    in_flat_type = character(),
    in_flat_number = character(),
    in_building_name = character(),
    in_number_first = integer(),
    jarowinkler_score = numeric(),
    jaccard_score = numeric(),
    text_similarity = numeric()
  )
}

.gnaf_empty_parsed <- function() {
  data.table(
    input_id = integer(),
    input_raw = character(),
    in_postcode = integer(),
    in_state = character(),
    in_locality = character(),
    in_street_name = character(),
    in_street_type = character(),
    in_street_suffix = character(),
    in_number_first = integer(),
    in_number_last = integer(),
    in_flat_type = character(),
    in_flat_number = character(),
    in_building_name = character()
  )
}

.gnaf_metric_card <- function(label, value) {
  shiny::div(
    class = "metric-card",
    shiny::div(class = "metric-label", label),
    shiny::div(class = "metric-value", value)
  )
}

.gnaf_score_fill <- function(score) {
  palette <- grDevices::colorRampPalette(c("#7f1d1d", "#b45309", "#f59e0b", "#84cc16", "#166534"))(101)
  idx <- max(1L, min(101L, as.integer(round(score)) + 1L))
  palette[idx]
}

.gnaf_score_col <- function(name, digits = 1) {
  reactable::colDef(
    name = name,
    align = "center",
    format = reactable::colFormat(digits = digits),
    style = function(value) {
      list(
        background = .gnaf_score_fill(value),
        color = if (isTRUE(value >= 70)) "#f8fafc" else "#102a43",
        fontWeight = 700
      )
    }
  )
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || (is.character(x) && !nzchar(x))) y else x
}