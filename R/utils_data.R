## -------------------------------------------------------------------------
## utils_data.R
## Fonctions utilitaires pour :
## - gérer la base locale CSV
## - valider le format des données boursières
## - importer des données depuis Yahoo Finance ou un fichier utilisateur
## -------------------------------------------------------------------------

ensure_data_dir <- function(data_dir = "data") {
  if (!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  }
}

# Normalise le nom d'un ticker pour produire un nom de fichier propre.
sanitize_ticker <- function(ticker) {
  ticker <- trimws(toupper(ticker))
  ticker <- gsub("[^A-Z0-9._-]", "_", ticker)
  ticker
}

# Liste les tickers actuellement disponibles dans la base locale.
list_local_tickers <- function(data_dir = "data") {
  ensure_data_dir(data_dir)
  files <- list.files(data_dir, pattern = "\\.csv$", full.names = FALSE)
  tickers <- tools::file_path_sans_ext(files)
  sort(unique(tickers))
}

# Uniformise les noms de colonnes attendus, indépendamment de la casse.
standardize_stock_columns <- function(data) {
  names(data) <- trimws(names(data))

  lower_names <- tolower(names(data))
  names(data)[lower_names == "date"] <- "Date"
  names(data)[lower_names == "close"] <- "Close"
  names(data)[lower_names == "open"] <- "Open"
  names(data)[lower_names == "high"] <- "High"
  names(data)[lower_names == "low"] <- "Low"
  names(data)[lower_names == "volume"] <- "Volume"
  names(data)[lower_names == "adjusted"] <- "Adjusted"

  data
}

# Lit un CSV en capturant les erreurs de lecture avec un message explicite.
read_csv_safely <- function(file_path) {
  tryCatch(
    readr::read_csv(file_path, show_col_types = FALSE),
    error = function(e) {
      stop(
        paste0(
          "Impossible de lire le fichier CSV : ",
          basename(file_path),
          ". Détail : ",
          conditionMessage(e)
        ),
        call. = FALSE
      )
    }
  )
}

# Déduit le ticker à partir du nom du fichier si aucun ticker valide n'est fourni.
infer_ticker_from_file <- function(file_path, ticker = NULL) {
  if (is.null(ticker) || !nzchar(trimws(ticker)) || trimws(ticker) == "0") {
    return(tools::file_path_sans_ext(basename(file_path)))
  }

  ticker
}

# Convertit les colonnes numériques optionnelles lorsqu'elles sont présentes.
coerce_optional_numeric_columns <- function(data) {
  optional_numeric_cols <- c("Open", "High", "Low", "Volume", "Adjusted")

  for (col_name in optional_numeric_cols) {
    if (col_name %in% names(data)) {
      data[[col_name]] <- suppressWarnings(as.numeric(data[[col_name]]))
    }
  }

  data
}

# Vérifie qu'un jeu de données respecte le format minimal attendu par STAN.
validate_stock_data <- function(data) {
  data <- standardize_stock_columns(data)

  required_cols <- c("Date", "Close")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    return(list(
      ok = FALSE,
      message = paste(
        "Le fichier doit contenir au minimum les colonnes :",
        paste(required_cols, collapse = ", "),
        ". Colonnes manquantes :",
        paste(missing_cols, collapse = ", ")
      ),
      data = NULL
    ))
  }

  data <- dplyr::mutate(
    data,
    Date = as.Date(Date),
    Close = suppressWarnings(as.numeric(Close))
  )

  data <- coerce_optional_numeric_columns(data)

  if (any(is.na(data$Date))) {
    return(list(
      ok = FALSE,
      message = "La colonne Date contient des valeurs invalides. Le format attendu est YYYY-MM-DD.",
      data = NULL
    ))
  }

  if (any(is.na(data$Close))) {
    return(list(
      ok = FALSE,
      message = "La colonne Close contient des valeurs non numériques ou manquantes.",
      data = NULL
    ))
  }

  if (any(data$Close <= 0)) {
    return(list(
      ok = FALSE,
      message = "La colonne Close doit contenir uniquement des prix strictement positifs.",
      data = NULL
    ))
  }

  if (anyDuplicated(data$Date) > 0) {
    return(list(
      ok = FALSE,
      message = "Le fichier contient des dates dupliquées. Une seule ligne par date est attendue.",
      data = NULL
    ))
  }

  data <- data |>
    dplyr::arrange(Date) |>
    dplyr::distinct(Date, .keep_all = TRUE)

  if (nrow(data) < 3) {
    return(list(
      ok = FALSE,
      message = "Le fichier contient trop peu d'observations. Au moins 3 dates sont nécessaires.",
      data = NULL
    ))
  }

  list(
    ok = TRUE,
    message = "Validation réussie.",
    data = data
  )
}

# Lit puis valide un fichier de la base locale.
read_stock_csv <- function(file_path) {
  data <- read_csv_safely(file_path)
  validation <- validate_stock_data(data)
  if (!validation$ok) {
    stop(validation$message, call. = FALSE)
  }
  validation$data
}

# Sauvegarde un ticker validé dans le dossier data/.
save_stock_data <- function(data, ticker, data_dir = "data") {
  ensure_data_dir(data_dir)
  ticker <- sanitize_ticker(ticker)
  output_path <- file.path(data_dir, paste0(ticker, ".csv"))
  readr::write_csv(data, output_path)
  output_path
}

# Importe un CSV utilisateur, le valide et l'enregistre dans la base locale.
import_csv_file <- function(file_path, ticker = NULL, data_dir = "data") {
  raw_data <- read_csv_safely(file_path)
  validation <- validate_stock_data(raw_data)
  if (!validation$ok) {
    stop(validation$message, call. = FALSE)
  }

  ticker <- infer_ticker_from_file(file_path, ticker)

  output_path <- save_stock_data(validation$data, ticker, data_dir = data_dir)
  list(
    ticker = sanitize_ticker(ticker),
    path = output_path,
    rows = nrow(validation$data)
  )
}

# Télécharge l'historique d'un ticker depuis Yahoo Finance et le convertit en tibble.
fetch_yahoo_data <- function(ticker, from = as.Date("2000-01-01")) {
  clean_ticker <- sanitize_ticker(ticker)

  yahoo_xts <- quantmod::getSymbols(
    Symbols = clean_ticker,
    src = "yahoo",
    from = from,
    auto.assign = FALSE,
    warnings = FALSE
  )

  df <- tibble::tibble(
    Date = as.Date(zoo::index(yahoo_xts)),
    Open = as.numeric(quantmod::Op(yahoo_xts)),
    High = as.numeric(quantmod::Hi(yahoo_xts)),
    Low = as.numeric(quantmod::Lo(yahoo_xts)),
    Close = as.numeric(quantmod::Cl(yahoo_xts)),
    Volume = as.numeric(quantmod::Vo(yahoo_xts)),
    Adjusted = as.numeric(quantmod::Ad(yahoo_xts))
  ) |>
    dplyr::filter(!is.na(Date), !is.na(Close), Close > 0) |>
    dplyr::arrange(Date)

  validation <- validate_stock_data(df)
  if (!validation$ok) {
    stop(validation$message, call. = FALSE)
  }

  validation$data
}

# Télécharge puis enregistre un ticker Yahoo dans la base locale.
download_and_save_yahoo <- function(ticker, data_dir = "data") {
  data <- fetch_yahoo_data(ticker)
  path <- save_stock_data(data, ticker, data_dir = data_dir)
  list(
    ticker = sanitize_ticker(ticker),
    path = path,
    rows = nrow(data),
    min_date = min(data$Date),
    max_date = max(data$Date)
  )
}
