filter_analysis_period <- function(data, start_year) {
  if (is.null(data) || nrow(data) == 0) {
    return(data.frame())
  }

  filtered <- data |>
    dplyr::filter(lubridate::year(Date) >= start_year) |>
    dplyr::arrange(Date)

  filtered
}

compute_daily_returns <- function(data) {
  if (nrow(data) < 2) {
    return(numeric())
  }

  returns <- data$Close / dplyr::lag(data$Close) - 1
  returns <- returns[!is.na(returns) & is.finite(returns)]
  returns
}

compute_cagr <- function(data) {
  if (nrow(data) < 2) {
    return(NA_real_)
  }

  first_price <- data$Close[1]
  last_price <- data$Close[nrow(data)]
  years_span <- as.numeric(difftime(max(data$Date), min(data$Date), units = "days")) / 365.25

  if (!is.finite(years_span) || years_span <= 0 || first_price <= 0 || last_price <= 0) {
    return(NA_real_)
  }

  (last_price / first_price)^(1 / years_span) - 1
}

get_reference_price <- function(data, target_date) {
  subset <- data |>
    dplyr::filter(Date <= target_date) |>
    dplyr::arrange(Date)

  if (nrow(subset) == 0) {
    return(NA_real_)
  }

  subset$Close[nrow(subset)]
}

compute_performance_table <- function(data) {
  if (nrow(data) < 2) {
    return(tibble::tibble(Horizon = character(), Performance = numeric()))
  }

  last_date <- max(data$Date)
  last_price <- data$Close[nrow(data)]

  horizons <- list(
    "1M" = last_date %m-% months(1),
    "6M" = last_date %m-% months(6),
    "1A" = last_date %m-% years(1),
    "3A" = last_date %m-% years(3),
    "5A" = last_date %m-% years(5)
  )

  tibble::tibble(
    Horizon = names(horizons),
    ReferenceDate = as.Date(unlist(horizons)),
    ReferencePrice = purrr::map_dbl(horizons, ~ get_reference_price(data, .x)),
    Performance = dplyr::if_else(
      is.na(ReferencePrice) | ReferencePrice <= 0,
      NA_real_,
      last_price / ReferencePrice - 1
    )
  )
}

compute_log_regression <- function(data) {
  valid_data <- data |>
    dplyr::filter(!is.na(Close), Close > 0) |>
    dplyr::arrange(Date) |>
    dplyr::mutate(
      t = as.numeric(Date - min(Date)),
      log_close = log(Close)
    )

  if (nrow(valid_data) < 10) {
    stop("Données insuffisantes pour estimer une régression log-linéaire robuste.", call. = FALSE)
  }

  model <- stats::lm(log_close ~ t, data = valid_data)
  fitted_log <- stats::predict(model, newdata = valid_data)
  residuals_log <- valid_data$log_close - fitted_log
  sigma <- stats::sd(residuals_log)

  if (!is.finite(sigma) || sigma == 0) {
    sigma <- NA_real_
  }

  last_t <- max(valid_data$t)
  current_fit <- stats::predict(model, newdata = data.frame(t = last_t))
  future_fit_1y <- stats::predict(model, newdata = data.frame(t = last_t + 365))
  future_fit_5y <- stats::predict(model, newdata = data.frame(t = last_t + 5 * 365))
  last_residual <- residuals_log[length(residuals_log)]

  plot_data <- valid_data |>
    dplyr::mutate(
      fitted_log = fitted_log,
      fitted_price = exp(fitted_log),
      plus_1sigma = exp(fitted_log + sigma),
      minus_1sigma = exp(fitted_log - sigma),
      plus_2sigma = exp(fitted_log + 2 * sigma),
      minus_2sigma = exp(fitted_log - 2 * sigma)
    )

  list(
    model = model,
    data = plot_data,
    intercept = unname(stats::coef(model)[1]),
    beta = unname(stats::coef(model)[2]),
    sigma = sigma,
    theoretical_current = as.numeric(exp(current_fit)),
    current_position_sigma = if (is.na(sigma)) NA_real_ else as.numeric(last_residual / sigma),
    theoretical_1y = as.numeric(exp(future_fit_1y)),
    theoretical_5y = as.numeric(exp(future_fit_5y))
  )
}

compute_basic_indicators <- function(data) {
  if (nrow(data) == 0) {
    return(list(
      last_price = NA_real_,
      last_update = as.Date(NA),
      volatility = NA_real_,
      cagr = NA_real_
    ))
  }

  returns <- compute_daily_returns(data)

  list(
    last_price = data$Close[nrow(data)],
    last_update = data$Date[nrow(data)],
    volatility = if (length(returns) >= 2) stats::sd(returns) else NA_real_,
    cagr = compute_cagr(data)
  )
}

format_percentage <- function(x) {
  ifelse(is.na(x), "NA", scales::percent(x, accuracy = 0.01, decimal.mark = ","))
}

format_number <- function(x, digits = 2) {
  ifelse(
    is.na(x),
    "NA",
    format(round(x, digits), nsmall = digits, decimal.mark = ",", big.mark = " ")
  )
}

format_price <- function(x, digits = 2) {
  ifelse(
    is.na(x),
    "NA",
    format_number(x, digits = digits)
  )
}

format_date_fr <- function(x) {
  ifelse(is.na(x), "NA", format(x, "%d/%m/%Y"))
}

build_price_plot <- function(regression_result, ticker, start_date = NULL, end_date = NULL) {
  plot_data <- regression_result$data

  ggplot2::ggplot(plot_data, ggplot2::aes(x = Date)) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = minus_2sigma, ymax = plus_2sigma),
      fill = "#D7DEE7",
      alpha = 0.45
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = minus_1sigma, ymax = plus_1sigma),
      fill = "#AFC3D7",
      alpha = 0.40
    ) +
    ggplot2::geom_line(ggplot2::aes(y = Close, color = "Prix observé"), linewidth = 1.05) +
    ggplot2::geom_line(ggplot2::aes(y = fitted_price, color = "Régression"), linewidth = 0.95, linetype = "longdash") +
    ggplot2::geom_line(ggplot2::aes(y = plus_1sigma, color = "+1 sigma"), linewidth = 0.65, linetype = "dashed") +
    ggplot2::geom_line(ggplot2::aes(y = minus_1sigma, color = "-1 sigma"), linewidth = 0.65, linetype = "dashed") +
    ggplot2::geom_line(ggplot2::aes(y = plus_2sigma, color = "+2 sigma"), linewidth = 0.55, linetype = "dotted") +
    ggplot2::geom_line(ggplot2::aes(y = minus_2sigma, color = "-2 sigma"), linewidth = 0.55, linetype = "dotted") +
    ggplot2::scale_y_log10(labels = scales::label_number(big.mark = " ", decimal.mark = ",")) +
    ggplot2::scale_color_manual(
      values = c(
        "Prix observé" = "#0F172A",
        "Régression" = "#1D4E89",
        "+1 sigma" = "#6C8EAD",
        "-1 sigma" = "#6C8EAD",
        "+2 sigma" = "#94A3B8",
        "-2 sigma" = "#94A3B8"
      )
    ) +
    ggplot2::labs(
      title = paste("Ticker", ticker, "• prix observé et trajectoire théorique"),
      subtitle = paste0(
        "Période analysée : ",
        format(start_date, "%d/%m/%Y"),
        " au ",
        format(end_date, "%d/%m/%Y"),
        " • échelle logarithmique"
      ),
      x = "Date",
      y = "Prix de clôture (échelle logarithmique)",
      color = NULL
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.text = ggplot2::element_text(size = 10),
      plot.title = ggplot2::element_text(face = "bold", size = 15, color = "#0F172A"),
      plot.subtitle = ggplot2::element_text(size = 10.5, color = "#475467"),
      axis.title = ggplot2::element_text(face = "bold", color = "#344054"),
      axis.text = ggplot2::element_text(color = "#344054"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "#E5E7EB", linewidth = 0.45),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA)
    )
}
