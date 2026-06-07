#' Srednia kroczaca dla szeregu sprzedazy
#'
#' Dodaje kolumne ze srednia kroczaca (rolling mean) o zadanym oknie.
#' Implementacja bazowa (bez dodatkowych zaleznosci).
#'
#' @param data Tabela z pojedynczym szeregiem (jedna obserwacja na date).
#' @param value Kolumna z wartoscia (domyslnie `"sales"`).
#' @param window Szerokosc okna w liczbie okresow (domyslnie `7`).
#' @param date_col Kolumna z data (domyslnie `"date"`); uzyta do sortowania.
#' @return Tabela z dodatkowa kolumna `.ma`.
#' @examples
#' \dontrun{ add_moving_average(one_series, window = 30) }
#' @export
add_moving_average <- function(data, value = "sales", window = 7, date_col = "date") {
  if (date_col %in% names(data)) data <- dplyr::arrange(data, .data[[date_col]])
  data$.ma <- .rollmean(data[[value]], window)
  data
}

.rollmean <- function(x, k) {
  n <- length(x)
  if (k <= 1 || n == 0) return(as.numeric(x))
  out <- rep(NA_real_, n)
  cs <- cumsum(c(0, x))
  for (i in seq(k, n)) out[i] <- (cs[i + 1] - cs[i - k + 1]) / k
  out
}

# wykrywanie lokalnych szczytow (wartosc wieksza od obu sasiadow)
.detect_peaks <- function(x) {
  n <- length(x)
  if (n < 3) return(integer(0))
  which(x[2:(n - 1)] > x[1:(n - 2)] & x[2:(n - 1)] > x[3:n]) + 1L
}

#' Obliczanie metryk biznesowych sprzedazy
#'
#' Liczy zestaw kluczowych metryk dla szeregu czasowego sprzedazy:
#' sprzedaz calkowita i przecietna, mediane, zmiennosc (odchylenie
#' standardowe i wspolczynnik zmiennosci), ostatnia wartosc sredniej
#' kroczacej, udzial promocji, dynamike (wzrost \%), nachylenie trendu
#' oraz srednia odleglosc miedzy szczytami sprzedazy.
#'
#' Funkcja dziala na pojedynczym szeregu. Aby policzyc metryki dla wielu
#' sklepow/kategorii naraz, uzyj [sales_ts_logic()] lub argumentu `by`.
#'
#' @param data Tabela z danymi (jedna obserwacja na date, jesli `by = NULL`).
#' @param value Kolumna z wartoscia sprzedazy (domyslnie `"sales"`).
#' @param date_col Kolumna z data (domyslnie `"date"`).
#' @param promo_col Kolumna z liczba promocji (domyslnie `"onpromotion"`).
#' @param ma_window Okno sredniej kroczacej (domyslnie `7`).
#' @param recent_window Liczba ostatnich okresow do liczenia dynamiki
#'   (domyslnie `30`).
#' @param by Opcjonalny wektor kolumn grupujacych. Gdy podany, metryki sa
#'   liczone osobno dla kazdej grupy (kazdy szereg jest najpierw agregowany
#'   po dacie).
#'
#' @return Jednowierszowa tabela metryk (lub wielowierszowa przy `by`).
#'
#' @examples
#' \dontrun{
#' compute_sales_metrics(one_series)
#' compute_sales_metrics(sales, by = "store_nbr")
#' }
#' @export
compute_sales_metrics <- function(data,
                                  value = "sales",
                                  date_col = "date",
                                  promo_col = "onpromotion",
                                  ma_window = 7,
                                  recent_window = 30,
                                  by = NULL) {

  if (!is.null(by)) {
    by <- intersect(by, names(data))
    parts <- split(data, data[by], drop = TRUE)
    res <- lapply(parts, function(g) {
      g_agg <- .collapse_by_date(g, value, date_col, promo_col)
      m <- compute_sales_metrics(g_agg, value, date_col, promo_col,
                                 ma_window, recent_window, by = NULL)
      keys <- g[1, by, drop = FALSE]
      dplyr::bind_cols(keys, m)
    })
    return(dplyr::bind_rows(res))
  }

  d <- dplyr::arrange(data, .data[[date_col]])
  x <- d[[value]]
  n <- length(x)
  mu <- mean(x, na.rm = TRUE)
  sdev <- stats::sd(x, na.rm = TRUE)

  ma <- .rollmean(x, ma_window)

  # dynamika: ostatnie vs poprzednie okno
  if (n >= 2 * recent_window) {
    recent <- mean(utils::tail(x, recent_window), na.rm = TRUE)
    prior  <- mean(utils::tail(utils::head(x, n - recent_window), recent_window), na.rm = TRUE)
    growth <- if (prior != 0) (recent - prior) / prior * 100 else NA_real_
  } else {
    growth <- if (!is.na(x[1]) && x[1] != 0) (x[n] - x[1]) / x[1] * 100 else NA_real_
  }

  # nachylenie trendu (jednostka: wartosc na 1 okres)
  tt <- seq_len(n)
  slope <- if (n >= 2) unname(stats::lm(x ~ tt)$coefficients[2]) else NA_real_

  # odleglosci miedzy szczytami
  peaks <- .detect_peaks(x)
  avg_peak_gap <- if (length(peaks) >= 2) mean(diff(peaks)) else NA_real_

  # promocje
  promo_share <- if (promo_col %in% names(d)) {
    mean(d[[promo_col]] > 0, na.rm = TRUE)
  } else NA_real_
  promo_intensity <- if (promo_col %in% names(d)) {
    mean(d[[promo_col]], na.rm = TRUE)
  } else NA_real_

  dplyr::tibble(
    n_obs            = n,
    date_from        = min(d[[date_col]], na.rm = TRUE),
    date_to          = max(d[[date_col]], na.rm = TRUE),
    total_sales      = sum(x, na.rm = TRUE),
    mean_sales       = mu,
    median_sales     = stats::median(x, na.rm = TRUE),
    sd_sales         = sdev,
    cv_sales         = if (!is.na(mu) && mu != 0) sdev / mu else NA_real_,
    max_sales        = max(x, na.rm = TRUE),
    min_sales        = min(x, na.rm = TRUE),
    ma_last          = utils::tail(ma[!is.na(ma)], 1)[1],
    growth_pct       = growth,
    trend_slope      = slope,
    avg_peak_gap     = avg_peak_gap,
    promo_share      = promo_share,
    promo_intensity  = promo_intensity
  )
}

# zwija dane do jednej wartosci na date (suma sprzedazy i promocji)
.collapse_by_date <- function(g, value, date_col, promo_col) {
  has_promo <- promo_col %in% names(g)
  g %>%
    dplyr::group_by(.data[[date_col]]) %>%
    dplyr::summarise(
      !!value := sum(.data[[value]], na.rm = TRUE),
      !!promo_col := if (has_promo) sum(.data[[promo_col]], na.rm = TRUE) else NA_real_,
      .groups = "drop"
    )
}
