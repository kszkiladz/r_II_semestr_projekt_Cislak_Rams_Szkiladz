#' Wizualizacja trendow sprzedazy
#'
#' Tworzy wykres liniowy sprzedazy w czasie z opcjonalna srednia kroczaca,
#' kolorowaniem/facetowaniem wedlug wybranej zmiennej oraz oznaczeniem
#' okresow promocyjnych. Zbudowana na \pkg{ggplot2}, zgodnie ze stylem
#' z zajec.
#'
#' @param data Tabela z danymi sprzedazowymi.
#' @param value Kolumna z wartoscia (domyslnie `"sales"`).
#' @param date_col Kolumna z data (domyslnie `"date"`).
#' @param group Opcjonalna kolumna roznicujaca serie kolorem (np. `"family"`).
#' @param facet Opcjonalna kolumna do podzialu na panele (`facet_wrap`).
#' @param ma_window Jesli podane, dorysowuje srednia kroczaca o tym oknie.
#' @param show_promo Czy zaznaczyc okresy z promocja (domyslnie `FALSE`).
#' @param promo_col Kolumna z promocjami (domyslnie `"onpromotion"`).
#' @param title Tytul wykresu.
#'
#' @return Obiekt `ggplot`.
#'
#' @examples
#' \dontrun{
#' plot_sales_trends(clean, group = "family", ma_window = 30,
#'                  title = "Sprzedaz wg kategorii")
#' }
#' @export
plot_sales_trends <- function(data,
                              value = "sales",
                              date_col = "date",
                              group = NULL,
                              facet = NULL,
                              ma_window = NULL,
                              show_promo = FALSE,
                              promo_col = "onpromotion",
                              title = "Trend sprzedazy w czasie") {

  stopifnot(date_col %in% names(data), value %in% names(data))

  aes_base <- if (!is.null(group) && group %in% names(data)) {
    ggplot2::aes(x = .data[[date_col]], y = .data[[value]],
                 colour = .data[[group]], group = .data[[group]])
  } else {
    ggplot2::aes(x = .data[[date_col]], y = .data[[value]])
  }

  p <- ggplot2::ggplot(data, aes_base) +
    ggplot2::geom_line(alpha = 0.85) +
    ggplot2::labs(title = title, x = "Data", y = "Sprzedaz",
                  colour = group) +
    ggplot2::theme_minimal(base_size = 12)

  if (isTRUE(show_promo) && promo_col %in% names(data)) {
    promo_dat <- data[data[[promo_col]] > 0, , drop = FALSE]
    if (nrow(promo_dat) > 0) {
      p <- p + ggplot2::geom_point(
        data = promo_dat,
        ggplot2::aes(x = .data[[date_col]], y = .data[[value]]),
        colour = "firebrick", size = 0.8, alpha = 0.5,
        inherit.aes = FALSE
      )
    }
  }

  if (!is.null(ma_window)) {
    if (!is.null(group) && group %in% names(data)) {
      ma_dat <- data %>%
        dplyr::group_by(.data[[group]]) %>%
        dplyr::group_modify(~ add_moving_average(.x, value, ma_window, date_col)) %>%
        dplyr::ungroup()
      ma_aes <- ggplot2::aes(x = .data[[date_col]], y = .ma,
                             group = .data[[group]])
    } else {
      ma_dat <- add_moving_average(data, value, ma_window, date_col)
      ma_aes <- ggplot2::aes(x = .data[[date_col]], y = .ma)
    }
    p <- p + ggplot2::geom_line(
      data = ma_dat, mapping = ma_aes,
      linewidth = 1, colour = "black", alpha = 0.7,
      inherit.aes = FALSE
    )
  }

  if (!is.null(facet) && facet %in% names(data)) {
    p <- p + ggplot2::facet_wrap(ggplot2::vars(.data[[facet]]), scales = "free_y")
  }

  p
}
