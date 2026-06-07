#' Prognozowanie sprzedazy modelami ARIMA i Prophet
#'
#' Buduje prognoze sprzedazy na podstawie dotychczasowego szeregu czasowego z
#' wykorzystaniem dwoch metod: modelu \strong{ARIMA} (pakiet \pkg{forecast},
#' funkcja \code{auto.arima}) oraz modelu \strong{Prophet} (pakiet
#' \pkg{prophet}). Podejscie odwzorowuje workflow z materialow o predykcji:
#' dane sa agregowane do jednego szeregu, dzielone na czesc treningowa i
#' prognozowana, a wyniki obu modeli sa zwracane lacznie do porownania.
#'
#' @param data \code{tibble} z danymi sprzedazowymi. Jezeli zawiera wiele
#'   sklepow/kategorii, sprzedaz jest sumowana do jednego szeregu per data
#'   (mozna wczesniej zawezic dane filtrem lub \code{\link{sales_ts_logic}}).
#' @param value_col Nazwa kolumny ze sprzedaza. Domyslnie \code{"sales"}.
#' @param date_col Nazwa kolumny z data. Domyslnie \code{"date"}.
#' @param h Horyzont prognozy (liczba okresow w przod). Domyslnie 30.
#' @param frequency Czestotliwosc sezonowa szeregu dla ARIMA (np. 7 dla danych
#'   dziennych z tygodniowa sezonowoscia, 12 dla miesiecznych). Domyslnie 7.
#' @param methods Wektor metod do uzycia: \code{"arima"}, \code{"prophet"} lub
#'   obie. Domyslnie obie.
#'
#' @return Lista klasy \code{sales_prognosis} z polami: \code{history}
#'   (zagregowany szereg historyczny), \code{forecast} (\code{tibble} z
#'   kolumnami \code{date}, \code{arima}, \code{prophet}), \code{models}
#'   (dopasowane obiekty modeli), \code{h} oraz \code{methods}.
#'
#' @examples
#' \dontrun{
#' fc <- create_prognosis(sales, h = 30, frequency = 7)
#' fc$forecast
#' plot_prognosis(fc)
#' }
#'
#' @importFrom dplyr group_by summarise arrange rename
#' @importFrom rlang .data
#' @importFrom stats ts predict
#' @export
create_prognosis <- function(data,
                             value_col = "sales",
                             date_col  = "date",
                             h         = 30,
                             frequency = 7,
                             methods   = c("arima", "prophet")) {

  stopifnot(is.data.frame(data))
  methods <- match.arg(methods, c("arima", "prophet"), several.ok = TRUE)

  # --- Agregacja do jednego szeregu czasowego ---
  hist <- data %>%
    dplyr::group_by(.data[[date_col]]) %>%
    dplyr::summarise(sales = sum(.data[[value_col]], na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::arrange(.data[[date_col]])
  names(hist)[names(hist) == date_col] <- "date"

  if (nrow(hist) < frequency * 2) {
    warning("Krotki szereg - prognoza moze byc niestabilna.", call. = FALSE)
  }

  # Daty przyszle - kontynuacja z modalnym krokiem czasowym
  step <- as.numeric(stats::median(diff(hist$date)))
  future_dates <- max(hist$date) + seq_len(h) * step

  forecast_tbl <- data.frame(date = future_dates)
  models <- list()

  # --- ARIMA (forecast::auto.arima) ---
  if ("arima" %in% methods) {
    if (!requireNamespace("forecast", quietly = TRUE)) {
      warning("Pakiet 'forecast' nie jest zainstalowany - pomijam ARIMA.",
              call. = FALSE)
    } else {
      ts_train <- stats::ts(hist$sales, frequency = frequency)
      model_arima <- forecast::auto.arima(ts_train)
      fc_arima <- forecast::forecast(model_arima, h = h)
      forecast_tbl$arima <- as.numeric(fc_arima$mean)
      models$arima <- model_arima
    }
  }

  # --- Prophet (z metoda awaryjna ETS) ---
  # Prophet ma ciezkie zaleznosci (rstan). Jezeli nie jest dostepny, korzystamy
  # z modelu ETS z pakietu forecast - lekka, druga metoda spelniajaca wymog
  # "ARIMA + jedna dodatkowa metoda".
  if ("prophet" %in% methods) {
    if (requireNamespace("prophet", quietly = TRUE)) {
      df_prophet <- data.frame(ds = hist$date, y = hist$sales)
      m <- prophet::prophet(df_prophet, verbose = FALSE,
                            daily.seasonality = FALSE,
                            weekly.seasonality = TRUE)
      freq_str <- if (step == 1) "day" else if (step == 7) "week" else "day"
      future <- prophet::make_future_dataframe(m, periods = h, freq = freq_str)
      fc_prophet <- stats::predict(m, future)
      forecast_tbl$prophet <- utils::tail(fc_prophet$yhat, h)
      models$prophet <- m
    } else if (requireNamespace("forecast", quietly = TRUE)) {
      warning("Pakiet 'prophet' niedostepny - uzywam metody awaryjnej ETS.",
              call. = FALSE)
      ts_train <- stats::ts(hist$sales, frequency = frequency)
      model_ets <- forecast::ets(ts_train)
      fc_ets <- forecast::forecast(model_ets, h = h)
      forecast_tbl$ets <- as.numeric(fc_ets$mean)
      models$ets <- model_ets
    } else {
      warning("Brak 'prophet' i 'forecast' - pomijam druga metode.",
              call. = FALSE)
    }
  }

  out <- list(
    history  = hist,
    forecast = tibble::as_tibble(forecast_tbl),
    models   = models,
    h        = h,
    methods  = methods
  )
  class(out) <- "sales_prognosis"
  out
}

#' Wizualizacja prognozy sprzedazy
#'
#' Rysuje szereg historyczny wraz z prognozami ARIMA i Prophet na jednym
#' wykresie (\pkg{ggplot2}), umozliwiajac wizualne porownanie modeli.
#'
#' @param prognosis Obiekt klasy \code{sales_prognosis} z
#'   \code{\link{create_prognosis}}.
#' @param title Tytul wykresu.
#'
#' @return Obiekt \code{ggplot}.
#'
#' @importFrom ggplot2 ggplot aes geom_line labs theme_minimal scale_color_manual
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#' @export
plot_prognosis <- function(prognosis,
                           title = "Prognoza sprzedazy: ARIMA vs Prophet") {

  stopifnot(inherits(prognosis, "sales_prognosis"))

  hist_df <- data.frame(
    date = prognosis$history$date,
    value = prognosis$history$sales,
    series = "Historia"
  )

  fc <- prognosis$forecast
  fc_long <- tidyr::pivot_longer(
    fc, cols = setdiff(names(fc), "date"),
    names_to = "series", values_to = "value"
  )
  label_map <- c(arima = "ARIMA", prophet = "Prophet", ets = "ETS")
  fc_long$series <- ifelse(fc_long$series %in% names(label_map),
                           label_map[fc_long$series], fc_long$series)

  plot_df <- rbind(hist_df, fc_long[, c("date", "value", "series")])

  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = .data$date, y = .data$value, color = .data$series)
  ) +
    ggplot2::geom_line(linewidth = 0.6) +
    ggplot2::scale_color_manual(
      values = c("Historia" = "black", "ARIMA" = "#C00000",
                 "Prophet" = "#2E75B6", "ETS" = "#2E9B57")
    ) +
    ggplot2::labs(title = title, x = "Data", y = "Sprzedaz", color = "Seria") +
    ggplot2::theme_minimal()
}
