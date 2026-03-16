library(shiny)
library(quantmod)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(readr)
library(DT)
library(scales)
library(zoo)

source("R/utils_data.R")
source("R/utils_finance.R")

ensure_data_dir("data")

ui <- fluidPage(
  titlePanel("STAN - STock ANalyser"),
  tags$head(
    tags$style(HTML("
      .section-box {
        background: #f8f9fa;
        border: 1px solid #d9dee3;
        border-radius: 8px;
        padding: 16px;
        margin-bottom: 16px;
      }
      .section-title {
        font-weight: 600;
        margin-bottom: 10px;
      }
      .metric-value {
        font-size: 1.3em;
        font-weight: 600;
        color: #0b3954;
      }
      .small-note {
        color: #5c677d;
        font-size: 0.92em;
      }
    "))
  ),
  sidebarLayout(
    sidebarPanel(
      h4("Panel 2 - Réglages"),
      selectInput("ticker", "Ticker disponible", choices = NULL),
      numericInput("start_year", "Année de début d'analyse", value = year(Sys.Date()) - 10, min = 1990, max = year(Sys.Date())),
      hr(),
      h4("Panel 5 - Gestion de données"),
      actionButton("update_ticker", "Mettre à jour le ticker sélectionné", class = "btn-primary"),
      tags$hr(),
      textInput("new_yahoo_ticker", "Nouveau ticker Yahoo", placeholder = "Ex. AAPL ou BNP.PA"),
      actionButton("add_yahoo_ticker", "Ajouter depuis Yahoo Finance"),
      tags$hr(),
      textInput("csv_ticker", "Nom du ticker pour le CSV", placeholder = "Optionnel"),
      fileInput("csv_file", "Importer un fichier CSV", accept = c(".csv")),
      actionButton("add_csv_ticker", "Ajouter depuis un CSV")
    ),
    mainPanel(
      div(
        class = "section-box",
        div(class = "section-title", "Panel 1 - Indicateurs de base"),
        uiOutput("basic_indicators")
      ),
      div(
        class = "section-box",
        div(class = "section-title", "Panel 3 - Performance / Régression"),
        DTOutput("performance_table"),
        br(),
        DTOutput("regression_table")
      ),
      div(
        class = "section-box",
        div(class = "section-title", "Panel 4 - Graphique"),
        plotOutput("price_plot", height = "520px")
      ),
      div(
        class = "section-box",
        div(class = "section-title", "Panel 5 - Gestion des données"),
        verbatimTextOutput("data_message"),
        br(),
        DTOutput("available_tickers_table")
      )
    )
  )
)

server <- function(input, output, session) {
  data_dir <- "data"
  data_message <- reactiveVal("Application prête. Sélectionnez un ticker ou ajoutez-en un nouveau.")
  refresh_counter <- reactiveVal(0)

  refresh_tickers <- function(selected = NULL) {
    tickers <- list_local_tickers(data_dir)
    if (length(tickers) == 0) {
      updateSelectInput(session, "ticker", choices = character(0), selected = character(0))
      return(invisible(NULL))
    }

    if (is.null(selected) || !selected %in% tickers) {
      selected <- tickers[1]
    }

    updateSelectInput(session, "ticker", choices = tickers, selected = selected)
  }

  observe({
    refresh_counter()
    refresh_tickers(isolate(input$ticker))
  })

  observeEvent(input$ticker, {
    req(input$ticker)
    file_path <- file.path(data_dir, paste0(input$ticker, ".csv"))
    if (!file.exists(file_path)) {
      return()
    }

    data <- tryCatch(
      read_stock_csv(file_path),
      error = function(e) NULL
    )

    if (is.null(data)) {
      return()
    }

    min_year <- year(min(data$Date))
    max_year <- year(max(data$Date))
    default_year <- max(max_year - 10, min_year)
    updateNumericInput(
      session,
      "start_year",
      min = min_year,
      max = max_year,
      value = max(min(input$start_year, max_year), default_year)
    )
  }, ignoreNULL = FALSE)

  stock_data <- reactive({
    req(input$ticker)
    file_path <- file.path(data_dir, paste0(input$ticker, ".csv"))

    validate(
      need(file.exists(file_path), "Le fichier local du ticker sélectionné est introuvable.")
    )

    tryCatch(
      read_stock_csv(file_path),
      error = function(e) {
        validate(need(FALSE, e$message))
      }
    )
  })

  analysis_data <- reactive({
    data <- stock_data()
    filtered <- filter_analysis_period(data, input$start_year)

    validate(
      need(nrow(filtered) >= 3, "La période sélectionnée contient trop peu d'observations."),
      need(any(filtered$Close > 0), "Les prix doivent être strictement positifs pour l'analyse.")
    )

    filtered
  })

  basic_indicators <- reactive({
    compute_basic_indicators(analysis_data())
  })

  regression_result <- reactive({
    tryCatch(
      compute_log_regression(analysis_data()),
      error = function(e) {
        validate(need(FALSE, e$message))
      }
    )
  })

  output$basic_indicators <- renderUI({
    indicators <- basic_indicators()

    fluidRow(
      column(
        width = 3,
        div("Dernier prix"),
        div(class = "metric-value", if (is.na(indicators$last_price)) "NA" else format(indicators$last_price, nsmall = 2, digits = 6, decimal.mark = ",")),
        div(class = "small-note", "Clôture la plus récente")
      ),
      column(
        width = 3,
        div("Dernière mise à jour"),
        div(class = "metric-value", if (is.na(indicators$last_update)) "NA" else format(indicators$last_update, "%Y-%m-%d")),
        div(class = "small-note", "Date la plus récente disponible")
      ),
      column(
        width = 3,
        div("Volatilité"),
        div(class = "metric-value", if (is.na(indicators$volatility)) "NA" else format_percentage(indicators$volatility)),
        div(class = "small-note", "Écart-type des rendements simples")
      ),
      column(
        width = 3,
        div("CAGR"),
        div(class = "metric-value", if (is.na(indicators$cagr)) "NA" else format_percentage(indicators$cagr)),
        div(class = "small-note", "Taux de croissance annualisé")
      )
    )
  })

  output$performance_table <- renderDT({
    perf <- compute_performance_table(analysis_data()) |>
      dplyr::mutate(
        `Date de référence` = ifelse(is.na(ReferencePrice), NA_character_, format(ReferenceDate, "%Y-%m-%d")),
        `Prix de référence` = ifelse(is.na(ReferencePrice), NA_character_, format_number(ReferencePrice, 2)),
        Performance = ifelse(is.na(Performance), "Historique insuffisant", format_percentage(Performance))
      ) |>
      dplyr::select(Horizon, `Date de référence`, `Prix de référence`, Performance)

    datatable(
      perf,
      rownames = FALSE,
      options = list(dom = "t", pageLength = 5),
      caption = htmltools::tags$caption(style = "caption-side: top; text-align: left;", "Performances historiques demandées")
    )
  })

  output$regression_table <- renderDT({
    reg <- regression_result()

    reg_table <- tibble::tibble(
      Indicateur = c(
        "Valeur théorique actuelle",
        "Beta (pente)",
        "Sigma des résidus",
        "Position actuelle en sigma",
        "Valeur théorique dans 1 an",
        "Valeur théorique dans 5 ans"
      ),
      Valeur = c(
        format_number(reg$theoretical_current, 2),
        format_number(reg$beta, 6),
        format_number(reg$sigma, 6),
        format_number(reg$current_position_sigma, 4),
        format_number(reg$theoretical_1y, 2),
        format_number(reg$theoretical_5y, 2)
      )
    )

    datatable(
      reg_table,
      rownames = FALSE,
      options = list(dom = "t", pageLength = 6),
      caption = htmltools::tags$caption(style = "caption-side: top; text-align: left;", "Indicateurs issus de la régression log-linéaire")
    )
  })

  output$price_plot <- renderPlot({
    reg <- regression_result()
    build_price_plot(reg, input$ticker)
  })

  output$available_tickers_table <- renderDT({
    tickers <- list_local_tickers(data_dir)
    ticker_table <- tibble::tibble(
      Ticker = tickers,
      Fichier = paste0(tickers, ".csv")
    )

    datatable(
      ticker_table,
      rownames = FALSE,
      options = list(dom = "t", pageLength = 10)
    )
  })

  output$data_message <- renderText({
    data_message()
  })

  observeEvent(input$add_yahoo_ticker, {
    req(input$new_yahoo_ticker)
    ticker <- sanitize_ticker(input$new_yahoo_ticker)

    message_text <- tryCatch({
      result <- download_and_save_yahoo(ticker, data_dir = data_dir)
      refresh_counter(refresh_counter() + 1)
      paste0(
        "Succès : le ticker ", result$ticker, " a été téléchargé depuis Yahoo Finance et sauvegardé dans ",
        result$path, ". Période récupérée : ", result$min_date, " à ", result$max_date,
        " (", result$rows, " lignes)."
      )
    }, error = function(e) {
      paste0(
        "Erreur lors du téléchargement Yahoo Finance pour ", ticker, " : ",
        conditionMessage(e),
        ". Vérifiez le ticker ou réessayez plus tard."
      )
    })

    data_message(message_text)
  })

  observeEvent(input$update_ticker, {
    req(input$ticker)

    message_text <- tryCatch({
      result <- download_and_save_yahoo(input$ticker, data_dir = data_dir)
      refresh_counter(refresh_counter() + 1)
      paste0(
        "Mise à jour réussie pour ", result$ticker, ". Dernière date disponible : ",
        result$max_date, ". Nombre de lignes : ", result$rows, "."
      )
    }, error = function(e) {
      paste0(
        "Échec de la mise à jour pour ", input$ticker, " : ",
        conditionMessage(e),
        ". Les données locales existantes ont été conservées."
      )
    })

    data_message(message_text)
  })

  observeEvent(input$add_csv_ticker, {
    req(input$csv_file)

    message_text <- tryCatch({
      result <- import_csv_file(
        file_path = input$csv_file$datapath,
        ticker = input$csv_ticker,
        data_dir = data_dir
      )
      refresh_counter(refresh_counter() + 1)
      paste0(
        "Import CSV réussi pour ", result$ticker, ". Fichier enregistré : ",
        result$path, ". Nombre de lignes validées : ", result$rows, "."
      )
    }, error = function(e) {
      paste0(
        "Échec de l'import CSV : ",
        conditionMessage(e)
      )
    })

    data_message(message_text)
  })
}

shinyApp(ui = ui, server = server)
