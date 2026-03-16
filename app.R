## -------------------------------------------------------------------------
## app.R
## Point d'entrée principal de l'application STAN.
## Ce fichier :
## - construit l'interface Shiny ;
## - relie les entrées utilisateur aux données locales et Yahoo Finance ;
## - délègue la validation et les calculs aux utilitaires du dossier R/.
## -------------------------------------------------------------------------

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

# Carte de présentation d'un indicateur clé dans le panel 1.
metric_card <- function(title, value, note, accent = "default") {
  div(
    class = paste("metric-card", paste0("metric-card-", accent)),
    div(class = "metric-card-title", title),
    div(class = "metric-card-value", value),
    div(class = "metric-card-note", note)
  )
}

# En-tête harmonisé des sections principales de l'interface.
section_header <- function(panel_label, title, subtitle = NULL) {
  div(
    class = "section-header",
    div(class = "section-label", panel_label),
    div(class = "section-heading", title),
    if (!is.null(subtitle)) div(class = "section-subtitle", subtitle)
  )
}

# Légende standard pour les tableaux DT.
table_caption <- function(text) {
  htmltools::tags$caption(
    style = "caption-side: top; text-align: left;",
    text
  )
}

ui <- fluidPage(
  tags$head(
    tags$title("STAN - STock ANalyser"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  div(
    class = "app-shell",
    div(
      class = "app-hero",
      div(
        class = "app-hero-text",
        div(class = "app-kicker", "Master 2 MAS / STD • Projet Shiny en R"),
        h1("STAN", class = "app-title"),
        p(
          class = "app-subtitle",
          "Analyse de stocks, indicateurs financiers, régression log-linéaire et gestion de données locales."
        ),
        div(class = "app-signature", "by Ayoub ACHOUR")
      )
    ),
    sidebarLayout(
      sidebarPanel(
        width = 4,
        class = "app-sidebar",
        div(
          class = "sidebar-card",
          section_header(
            "Panel 2",
            "Réglages de l'analyse",
            "Sélection du ticker local et de la période d'étude."
          ),
          div(
            class = "control-group",
            selectInput("ticker", "Ticker disponible", choices = NULL, width = "100%"),
            numericInput(
              "start_year",
              "Année de début d'analyse",
              value = year(Sys.Date()) - 10,
              min = 1990,
              max = year(Sys.Date()),
              width = "100%"
            )
          )
        ),
        div(
          class = "sidebar-card",
          section_header(
            "Panel 5",
            "Gestion des données",
            "Mise à jour depuis Yahoo Finance et import de fichiers CSV."
          ),
          div(
            class = "control-subgroup",
            div(class = "control-subtitle", "Ticker déjà présent"),
            actionButton(
              "update_ticker",
              "Mettre à jour le ticker sélectionné",
              class = "btn-stan btn-stan-primary"
            )
          ),
          div(
            class = "control-subgroup",
            div(class = "control-subtitle", "Ajout depuis Yahoo Finance"),
            textInput(
              "new_yahoo_ticker",
              "Nouveau ticker Yahoo",
              placeholder = "Ex. AAPL, MSFT ou BNP.PA",
              width = "100%"
            ),
            actionButton(
              "add_yahoo_ticker",
              "Ajouter depuis Yahoo Finance",
              class = "btn-stan btn-stan-secondary"
            )
          ),
          div(
            class = "control-subgroup",
            div(class = "control-subtitle", "Ajout depuis un fichier CSV"),
            textInput(
              "csv_ticker",
              "Nom du ticker pour le CSV",
              placeholder = "Optionnel : déduit du nom de fichier sinon",
              width = "100%"
            ),
            fileInput("csv_file", "Importer un fichier CSV", accept = c(".csv"), width = "100%"),
            actionButton(
              "add_csv_ticker",
              "Ajouter depuis un CSV",
              class = "btn-stan btn-stan-neutral"
            )
          )
        )
      ),
      mainPanel(
        width = 8,
        class = "app-main",
        div(
          class = "content-card",
          section_header(
            "Panel 1",
            "Indicateurs de base",
            "Vue synthétique des mesures clés sur la période sélectionnée."
          ),
          uiOutput("analysis_context"),
          uiOutput("basic_indicators")
        ),
        div(
          class = "content-card",
          section_header(
            "Panel 3",
            "Performance et régression",
            "Performances multi-horizons et lecture de la tendance log-linéaire."
          ),
          div(class = "table-block", DTOutput("performance_table")),
          div(class = "table-block", DTOutput("regression_table"))
        ),
        div(
          class = "content-card",
          section_header(
            "Panel 4",
            "Graphique",
            "Prix observé, tendance théorique et bandes de dispersion autour de la régression."
          ),
          plotOutput("price_plot", height = "560px")
        ),
        div(
          class = "content-card",
          section_header(
            "Panel 5",
            "Base locale",
            "Retour des opérations et liste des tickers actuellement disponibles."
          ),
          uiOutput("data_message"),
          div(class = "table-block", DTOutput("available_tickers_table"))
        )
      )
    )
  )
)

server <- function(input, output, session) {
  data_dir <- "data"

  # Message d'état centralisé pour les opérations de gestion de données.
  data_message <- reactiveVal(list(
    type = "info",
    title = "Application prête",
    text = "Sélectionnez un ticker local ou ajoutez-en un nouveau depuis Yahoo Finance ou un fichier CSV."
  ))
  refresh_counter <- reactiveVal(0)

  set_data_message <- function(type = "info", title, text) {
    data_message(list(type = type, title = title, text = text))
  }

  # Recharge la liste des tickers disponibles dans le dossier data/.
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

  # Rafraîchit automatiquement la liste après ajout, import ou mise à jour.
  observe({
    refresh_counter()
    refresh_tickers(isolate(input$ticker))
  })

  # Ajuste dynamiquement la plage d'années selon les données disponibles.
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

  # Lecture du fichier local correspondant au ticker sélectionné.
  stock_data <- reactive({
    req(input$ticker)
    file_path <- file.path(data_dir, paste0(input$ticker, ".csv"))

    validate(
      need(file.exists(file_path), "Le fichier local associé au ticker sélectionné est introuvable.")
    )

    tryCatch(
      read_stock_csv(file_path),
      error = function(e) {
        validate(need(FALSE, e$message))
      }
    )
  })

  # Données restreintes à la période choisie dans le panel de réglages.
  analysis_data <- reactive({
    data <- stock_data()
    filtered <- filter_analysis_period(data, input$start_year)

    validate(
      need(nrow(filtered) >= 3, "La période retenue contient trop peu d'observations pour produire une analyse fiable."),
      need(any(filtered$Close > 0), "Les prix de clôture doivent être strictement positifs.")
    )

    filtered
  })

  # Calcul des indicateurs de synthèse du panel 1.
  basic_indicators <- reactive({
    compute_basic_indicators(analysis_data())
  })

  # Régression log-linéaire utilisée dans les panels 3 et 4.
  regression_result <- reactive({
    tryCatch(
      compute_log_regression(analysis_data()),
      error = function(e) {
        validate(need(FALSE, e$message))
      }
    )
  })

  output$analysis_context <- renderUI({
    data <- analysis_data()
    div(
      class = "context-banner",
      div(
        class = "context-main",
        div(class = "context-ticker", input$ticker),
        div(
          class = "context-period",
          paste0(
            "Période analysée : ",
            format(min(data$Date), "%d/%m/%Y"),
            " au ",
            format(max(data$Date), "%d/%m/%Y"),
            " • ",
            nrow(data),
            " observations"
          )
        )
      )
    )
  })

  output$basic_indicators <- renderUI({
    indicators <- basic_indicators()

    fluidRow(
      column(
        width = 6,
        metric_card(
          "Dernier prix",
          format_price(indicators$last_price),
          "Dernière clôture disponible sur la période étudiée.",
          accent = "navy"
        )
      ),
      column(
        width = 6,
        metric_card(
          "Dernière mise à jour",
          format_date_fr(indicators$last_update),
          "Date la plus récente présente dans les données locales.",
          accent = "slate"
        )
      ),
      column(
        width = 6,
        metric_card(
          "Volatilité",
          format_percentage(indicators$volatility),
          "Écart-type des rendements journaliers simples.",
          accent = "gold"
        )
      ),
      column(
        width = 6,
        metric_card(
          "CAGR",
          format_percentage(indicators$cagr),
          "Taux de croissance annualisé sur la période sélectionnée.",
          accent = "green"
        )
      )
    )
  })

  output$performance_table <- renderDT({
    perf_raw <- compute_performance_table(analysis_data())

    perf_display <- perf_raw |>
      dplyr::mutate(
        `Date de référence` = dplyr::if_else(
          is.na(ReferencePrice),
          NA_character_,
          format(ReferenceDate, "%d/%m/%Y")
        ),
        `Prix de référence` = format_price(ReferencePrice),
        `Performance affichée` = dplyr::if_else(
          is.na(Performance),
          "Historique insuffisant",
          format_percentage(Performance)
        )
      ) |>
      dplyr::select(Horizon, `Date de référence`, `Prix de référence`, `Performance affichée`, Performance)

    datatable(
      perf_display,
      rownames = FALSE,
      class = "compact stripe hover stan-table",
      options = list(
        dom = "t",
        paging = FALSE,
        searching = FALSE,
        info = FALSE,
        ordering = FALSE,
        autoWidth = TRUE,
        columnDefs = list(list(targets = 4, visible = FALSE))
      ),
      colnames = c("Horizon", "Date de référence", "Prix de référence", "Performance", "Performance brute"),
      caption = table_caption("Performances du titre sur les horizons demandés")
    ) |>
      formatStyle(
        "Performance affichée",
        valueColumns = "Performance",
        color = styleInterval(c(-1e-12, 1e-12), c("#B42318", "#6B7280", "#027A48")),
        fontWeight = styleInterval(c(-1e-12, 1e-12), c("600", "500", "600"))
      )
  })

  output$regression_table <- renderDT({
    reg <- regression_result()

    reg_table <- tibble::tibble(
      Indicateur = c(
        "Valeur théorique actuelle",
        "Beta (pente journalière)",
        "Sigma des résidus",
        "Position actuelle en sigma",
        "Valeur théorique dans 1 an",
        "Valeur théorique dans 5 ans"
      ),
      Valeur = c(
        format_price(reg$theoretical_current),
        format_number(reg$beta, 6),
        format_number(reg$sigma, 6),
        format_number(reg$current_position_sigma, 4),
        format_price(reg$theoretical_1y),
        format_price(reg$theoretical_5y)
      )
    )

    datatable(
      reg_table,
      rownames = FALSE,
      class = "compact stripe hover stan-table",
      options = list(
        dom = "t",
        paging = FALSE,
        searching = FALSE,
        info = FALSE,
        ordering = FALSE,
        autoWidth = TRUE
      ),
      caption = table_caption("Lecture des paramètres de la régression log-linéaire")
    ) |>
      formatStyle(
        "Valeur",
        fontWeight = "600",
        color = "#0F172A"
      )
  })

  output$price_plot <- renderPlot({
    reg <- regression_result()
    data <- analysis_data()
    build_price_plot(
      regression_result = reg,
      ticker = input$ticker,
      start_date = min(data$Date),
      end_date = max(data$Date)
    )
  }, res = 110)

  output$available_tickers_table <- renderDT({
    tickers <- list_local_tickers(data_dir)
    ticker_table <- tibble::tibble(
      Ticker = tickers,
      `Fichier local` = paste0(tickers, ".csv")
    )

    datatable(
      ticker_table,
      rownames = FALSE,
      class = "compact stripe hover stan-table",
      options = list(
        dom = "t",
        paging = FALSE,
        searching = FALSE,
        info = FALSE,
        ordering = FALSE,
        autoWidth = TRUE
      ),
      caption = table_caption("Tickers actuellement disponibles dans la base locale")
    )
  })

  output$data_message <- renderUI({
    message <- data_message()
    div(
      class = paste("status-message", paste0("status-", message$type)),
      div(class = "status-title", message$title),
      div(class = "status-text", message$text)
    )
  })

  observeEvent(input$add_yahoo_ticker, {
    req(input$new_yahoo_ticker)
    req(nzchar(trimws(input$new_yahoo_ticker)))
    ticker <- sanitize_ticker(input$new_yahoo_ticker)

    tryCatch({
      result <- withProgress(
        message = "Téléchargement Yahoo Finance",
        detail = paste("Récupération de", ticker),
        value = 0.2,
        {
          incProgress(0.5)
          result <- download_and_save_yahoo(ticker, data_dir = data_dir)
          incProgress(0.4)
          result
        }
      )
      refresh_counter(refresh_counter() + 1)
      set_data_message(
        type = "success",
        title = "Ajout Yahoo Finance réussi",
        text = paste0(
          "Le ticker ", result$ticker, " a été téléchargé puis enregistré dans ", result$path,
          ". Historique récupéré du ", format(result$min_date, "%d/%m/%Y"),
          " au ", format(result$max_date, "%d/%m/%Y"),
          " (", result$rows, " lignes)."
        )
      )
    }, error = function(e) {
      set_data_message(
        type = "error",
        title = "Échec du téléchargement Yahoo Finance",
        text = paste0(
          "Impossible d'ajouter ", ticker, " : ",
          conditionMessage(e),
          ". Vérifiez le ticker saisi ou réessayez ultérieurement."
        )
      )
    })
  })

  observeEvent(input$update_ticker, {
    req(input$ticker)

    tryCatch({
      result <- withProgress(
        message = "Mise à jour du ticker local",
        detail = paste("Actualisation de", input$ticker),
        value = 0.2,
        {
          incProgress(0.5)
          result <- download_and_save_yahoo(input$ticker, data_dir = data_dir)
          incProgress(0.4)
          result
        }
      )
      refresh_counter(refresh_counter() + 1)
      set_data_message(
        type = "success",
        title = "Mise à jour terminée",
        text = paste0(
          "Le ticker ", result$ticker, " a été actualisé avec succès. Dernière date disponible : ",
          format(result$max_date, "%d/%m/%Y"),
          ". Le fichier local contient désormais ", result$rows, " lignes."
        )
      )
    }, error = function(e) {
      set_data_message(
        type = "error",
        title = "Mise à jour impossible",
        text = paste0(
          "Les données locales de ", input$ticker, " ont été conservées. Motif : ",
          conditionMessage(e)
        )
      )
    })
  })

  observeEvent(input$add_csv_ticker, {
    req(input$csv_file)
    req(!is.null(input$csv_file$datapath))

    tryCatch({
      result <- withProgress(
        message = "Import du fichier CSV",
        detail = "Validation et enregistrement des données",
        value = 0.2,
        {
          incProgress(0.5)
          result <- import_csv_file(
            file_path = input$csv_file$datapath,
            ticker = input$csv_ticker,
            data_dir = data_dir
          )
          incProgress(0.4)
          result
        }
      )
      refresh_counter(refresh_counter() + 1)
      set_data_message(
        type = "success",
        title = "Import CSV réussi",
        text = paste0(
          "Le ticker ", result$ticker, " a été importé avec succès dans ", result$path,
          ". Nombre de lignes validées : ", result$rows, "."
        )
      )
    }, error = function(e) {
      set_data_message(
        type = "error",
        title = "Import CSV impossible",
        text = conditionMessage(e)
      )
    })
  })
}

shinyApp(ui = ui, server = server)
