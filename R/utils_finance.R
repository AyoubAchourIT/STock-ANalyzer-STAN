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
  scales::percent(x, accuracy = 0.01, decimal.mark = ",")
}

format_number <- function(x, digits = 2) {
  ifelse(is.na(x), NA_character_, format(round(x, digits), nsmall = digits, decimal.mark = ","))
}

build_price_plot <- function(regression_result, ticker) {
  plot_data <- regression_result$data

  ggplot2::ggplot(plot_data, ggplot2::aes(x = Date)) +
    ggplot2::geom_line(ggplot2::aes(y = Close, color = "Prix observé"), linewidth = 0.9) +
    ggplot2::geom_line(ggplot2::aes(y = fitted_price, color = "Régression"), linewidth = 0.9, linetype = "dashed") +
    ggplot2::geom_line(ggplot2::aes(y = plus_1sigma, color = "+1 sigma"), linewidth = 0.7) +
    ggplot2::geom_line(ggplot2::aes(y = minus_1sigma, color = "-1 sigma"), linewidth = 0.7) +
    ggplot2::geom_line(ggplot2::aes(y = plus_2sigma, color = "+2 sigma"), linewidth = 0.6, alpha = 0.85) +
    ggplot2::geom_line(ggplot2::aes(y = minus_2sigma, color = "-2 sigma"), linewidth = 0.6, alpha = 0.85) +
    ggplot2::scale_y_log10(labels = scales::label_number(big.mark = " ", decimal.mark = ",")) +
    ggplot2::scale_color_manual(
      values = c(
        "Prix observé" = "#0B3954",
        "Régression" = "#C81D25",
        "+1 sigma" = "#FF7F11",
        "-1 sigma" = "#FF7F11",
        "+2 sigma" = "#5C677D",
        "-2 sigma" = "#5C677D"
      )
    ) +
    ggplot2::labs(
      title = paste("Analyse du ticker", ticker),
      subtitle = "Prix observé, tendance log-linéaire et bandes de dispersion",
      x = "Date",
      y = "Prix de clôture (échelle logarithmique)",
      color = NULL
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank()
    )
}
