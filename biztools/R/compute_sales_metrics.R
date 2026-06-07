#' Obliczanie kluczowych metryk biznesowych sprzedazy
#'
#' Oblicza zestaw metryk uzytecznych dla menedzera: sprzedaz calkowita,
#' przecietna sprzedaz w okresie, srednia kroczaca, zmiennosc (odchylenie
#' standardowe i wspolczynnik zmiennosci), udzial promocji, sredni odstep
#' miedzy szczytami sprzedazy oraz tempo wzrostu okres do okresu.
#'
#' Metryki mozna liczyc globalnie dla calego zbioru albo w obrebie grup
#' (np. per sklep, per kategoria) za pomoca argumentu \code{by}.
#'
#' @param data Wyczyszczony \code{tibble} (zob. \code{\link{clean_sales_ts}}).
#' @param value_col Nazwa kolumny ze sprzedaza. Domyslnie \code{"sales"}.
#' @param date_col Nazwa kolumny z data. Domyslnie \code{"date"}.
#' @param by Opcjonalny wektor kolumn grupujacych (np. \code{"store_nbr"}).
#'   \code{NULL} oznacza obliczenia dla calego zbioru.
#' @param ma_window Okno sredniej kroczacej (w liczbie okresow). Domyslnie 7.
#' @param promo_col Nazwa kolumny z liczba produktow w promocji.
#'   Domyslnie \code{"onpromotion"} (jezeli nie istnieje, udzial promocji = NA).
#'
#' @return \code{tibble} z jednym wierszem na grupe (lub jednym wierszem
#'   ogolem) i kolumnami metryk: \code{total_sales}, \code{mean_sales},
#'   \code{median_sales}, \code{sd_sales}, \code{cv_sales},
#'   \code{last_ma}, \code{promo_share}, \code{avg_peak_distance},
#'   \code{growth_rate}, \code{n_periods}.
#'
#' @examples
#' df <- data.frame(
#'   date = as.Date("2023-01-01") + 0:9,
#'   store_nbr = rep(1:2, each = 5), family = "A",
#'   sales = c(10, 12, 30, 11, 9, 20, 22, 50, 21, 19),
#'   onpromotion = c(0, 1, 5, 0, 0, 2, 3, 8, 1, 0)
#' )
#' compute_sales_metrics(df, by = "store_nbr", ma_window = 3)
#'
#' @importFrom dplyr group_by summarise ungroup arrange across all_of n
#' @importFrom rlang .data
#' @importFrom stats sd median
#' @export
compute_sales_metrics <- function(data,
                                  value_col = "sales",
                                  date_col  = "date",
                                  by        = NULL,
                                  ma_window = 7,
                                  promo_col = "onpromotion") {

  stopifnot(is.data.frame(data))
  by <- intersect(by, names(data))
  has_promo <- !is.null(promo_col) && promo_col %in% names(data)

  # Pojedyncza funkcja liczaca metryki dla jednego szeregu (wektora) -
  # wywolywana zarowno globalnie, jak i per grupa.
  metric_block <- function(d) {
    d <- d[order(d[[date_col]]), , drop = FALSE]
    v <- d[[value_col]]
    n <- length(v)

    total  <- sum(v, na.rm = TRUE)
    mean_v <- mean(v, na.rm = TRUE)
    med_v  <- stats::median(v, na.rm = TRUE)
    sd_v   <- if (n > 1) stats::sd(v, na.rm = TRUE) else NA_real_
    cv_v   <- if (!is.na(sd_v) && mean_v != 0) sd_v / mean_v else NA_real_

    # Srednia kroczaca - zwracamy jej ostatnia wartosc jako biezacy poziom
    last_ma <- if (n >= ma_window) {
      mean(utils::tail(v, ma_window), na.rm = TRUE)
    } else {
      mean_v
    }

    # Udzial promocji: srednia liczba produktow w promocji / przecietna sprzedaz
    promo_share <- if (has_promo) {
      p <- d[[promo_col]]
      total_p <- sum(p, na.rm = TRUE)
      if (total > 0) total_p / total else NA_real_
    } else NA_real_

    # Odstep miedzy szczytami: lokalne maksima > srednia + 1 sd
    avg_peak_distance <- NA_real_
    if (!is.na(sd_v) && n >= 3) {
      thr <- mean_v + sd_v
      peak_idx <- which(v > thr)
      if (length(peak_idx) >= 2) {
        avg_peak_distance <- mean(diff(peak_idx))
      }
    }

    # Tempo wzrostu: porownanie sredniej drugiej i pierwszej polowy okresu
    growth_rate <- NA_real_
    if (n >= 4) {
      half <- floor(n / 2)
      first_half  <- mean(v[seq_len(half)], na.rm = TRUE)
      second_half <- mean(v[(n - half + 1):n], na.rm = TRUE)
      if (first_half != 0) {
        growth_rate <- (second_half - first_half) / first_half
      }
    }

    tibble::tibble(
      total_sales       = total,
      mean_sales        = mean_v,
      median_sales      = med_v,
      sd_sales          = sd_v,
      cv_sales          = cv_v,
      last_ma           = last_ma,
      promo_share       = promo_share,
      avg_peak_distance = avg_peak_distance,
      growth_rate       = growth_rate,
      n_periods         = n
    )
  }

  if (length(by) == 0) {
    return(metric_block(data))
  }

  # Per grupa: dzielimy zbior i sklejamy wyniki
  split_keys <- interaction(data[by], drop = TRUE)
  parts <- split(data, split_keys)
  out <- lapply(parts, function(d) {
    key_vals <- d[1, by, drop = FALSE]
    cbind(tibble::as_tibble(key_vals), metric_block(d))
  })
  res <- dplyr::bind_rows(out)
  dplyr::arrange(res, dplyr::across(dplyr::all_of(by)))
}
