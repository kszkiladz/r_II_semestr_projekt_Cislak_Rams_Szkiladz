#' Podsumowanie biznesowe dla menedzera
#'
#' Tworzy zwiezle, gotowe do raportu podsumowanie sprzedazy: najlepszy i
#' najgorszy sklep, najszybciej rosnaca kategoria, najwiekszy procentowy spadek
#' w ostatnim okresie, srednia sprzedaz w ostatnim okresie oraz dodatkowe
#' metryki biznesowe.
#'
#' @param data Wyczyszczony \code{tibble} z danymi sprzedazowymi (powinien
#'   zawierac kolumny \code{store_nbr}, \code{family}, \code{date},
#'   \code{sales}).
#' @param value_col Nazwa kolumny ze sprzedaza. Domyslnie \code{"sales"}.
#' @param date_col Nazwa kolumny z data. Domyslnie \code{"date"}.
#' @param store_col Nazwa kolumny ze sklepem. Domyslnie \code{"store_nbr"}.
#' @param category_col Nazwa kolumny z kategoria. Domyslnie \code{"family"}.
#' @param recent_n Liczba ostatnich okresow (dat) traktowanych jako "ostatni
#'   okres" przy liczeniu zmian. Domyslnie 30.
#'
#' @return Lista klasy \code{sales_summary} z polami: \code{best_store},
#'   \code{worst_store}, \code{fastest_growing_category},
#'   \code{biggest_decline_category}, \code{recent_avg_sales},
#'   \code{total_sales}, \code{date_range}, \code{store_ranking},
#'   \code{category_growth}.
#'
#' @examples
#' set.seed(1)
#' df <- data.frame(
#'   date = rep(as.Date("2023-01-01") + 0:59, 2),
#'   store_nbr = rep(1:2, each = 60),
#'   family = rep(c("A", "B"), 60),
#'   sales = c(runif(60, 10, 20), runif(60, 30, 60)),
#'   onpromotion = 0L
#' )
#' create_management_summary(df, recent_n = 15)
#'
#' @importFrom dplyr group_by summarise arrange desc ungroup across all_of slice
#' @importFrom rlang .data
#' @export
create_management_summary <- function(data,
                                      value_col    = "sales",
                                      date_col     = "date",
                                      store_col    = "store_nbr",
                                      category_col = "family",
                                      recent_n     = 30) {

  stopifnot(is.data.frame(data))
  has_store    <- store_col %in% names(data)
  has_category <- category_col %in% names(data)

  total_sales <- sum(data[[value_col]], na.rm = TRUE)
  all_dates   <- sort(unique(data[[date_col]]))
  date_range  <- range(all_dates)

  # --- Ranking sklepow wg sprzedazy calkowitej ---
  store_ranking <- NULL
  best_store <- worst_store <- NA
  if (has_store) {
    store_ranking <- data %>%
      dplyr::group_by(.data[[store_col]]) %>%
      dplyr::summarise(total = sum(.data[[value_col]], na.rm = TRUE),
                       .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(.data$total))
    best_store  <- store_ranking[[store_col]][1]
    worst_store <- store_ranking[[store_col]][nrow(store_ranking)]
  }

  # --- Wzrost kategorii: srednia ostatniego okresu vs poprzedniego ---
  category_growth <- NULL
  fastest_growing_category <- biggest_decline_category <- NA
  if (has_category && length(all_dates) > recent_n) {
    recent_dates   <- utils::tail(all_dates, recent_n)
    previous_dates <- utils::tail(
      utils::head(all_dates, length(all_dates) - recent_n), recent_n
    )

    recent_df   <- data[data[[date_col]] %in% recent_dates, , drop = FALSE]
    previous_df <- data[data[[date_col]] %in% previous_dates, , drop = FALSE]

    rec <- recent_df %>%
      dplyr::group_by(.data[[category_col]]) %>%
      dplyr::summarise(recent_mean = mean(.data[[value_col]], na.rm = TRUE),
                       .groups = "drop")
    prev <- previous_df %>%
      dplyr::group_by(.data[[category_col]]) %>%
      dplyr::summarise(prev_mean = mean(.data[[value_col]], na.rm = TRUE),
                       .groups = "drop")

    category_growth <- dplyr::left_join(rec, prev, by = category_col)
    category_growth$growth_pct <- with(
      category_growth,
      ifelse(prev_mean != 0, (recent_mean - prev_mean) / prev_mean * 100, NA_real_)
    )
    category_growth <- dplyr::arrange(category_growth, dplyr::desc(.data$growth_pct))

    valid <- category_growth[!is.na(category_growth$growth_pct), , drop = FALSE]
    if (nrow(valid) > 0) {
      fastest_growing_category <- valid[[category_col]][1]
      biggest_decline_category <- valid[[category_col]][nrow(valid)]
    }
  }

  # --- Srednia sprzedaz w ostatnim okresie ---
  recent_dates    <- utils::tail(all_dates, recent_n)
  recent_avg_sales <- mean(
    data[[value_col]][data[[date_col]] %in% recent_dates], na.rm = TRUE
  )

  out <- list(
    best_store               = best_store,
    worst_store              = worst_store,
    fastest_growing_category = fastest_growing_category,
    biggest_decline_category = biggest_decline_category,
    recent_avg_sales         = recent_avg_sales,
    total_sales              = total_sales,
    date_range               = date_range,
    store_ranking            = store_ranking,
    category_growth          = category_growth,
    recent_n                 = recent_n
  )
  class(out) <- "sales_summary"
  out
}

#' @export
#' @noRd
print.sales_summary <- function(x, ...) {
  cat("============================================\n")
  cat("   PODSUMOWANIE SPRZEDAZY DLA MENEDZERA\n")
  cat("============================================\n")
  cat(sprintf("Okres analizy:              %s - %s\n",
              x$date_range[1], x$date_range[2]))
  cat(sprintf("Sprzedaz calkowita:         %s\n",
              format(round(x$total_sales), big.mark = " ")))
  cat(sprintf("Sr. sprzedaz (ost. %d okr.): %s\n",
              x$recent_n, format(round(x$recent_avg_sales, 1), big.mark = " ")))
  cat("--------------------------------------------\n")
  cat(sprintf("Najlepszy sklep:            %s\n", x$best_store))
  cat(sprintf("Najgorszy sklep:            %s\n", x$worst_store))
  cat(sprintf("Najszybciej rosnaca kat.:   %s\n", x$fastest_growing_category))
  cat(sprintf("Najwiekszy spadek (kat.):   %s\n", x$biggest_decline_category))
  cat("============================================\n")
  invisible(x)
}
