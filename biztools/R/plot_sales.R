#' Wizualizacja trendow sprzedazy
#'
#' Rysuje przebieg sprzedazy w czasie z wykorzystaniem pakietu \pkg{ggplot2}.
#' Dane sa agregowane do poziomu daty (suma po grupach), a opcjonalnie
#' rozbijane na kolory wedlug wskazanej zmiennej (np. sklep lub kategoria).
#' Mozna dodac wygladzona linie trendu.
#'
#' @param data \code{tibble} z danymi sprzedazowymi.
#' @param value_col Nazwa kolumny ze sprzedaza. Domyslnie \code{"sales"}.
#' @param date_col Nazwa kolumny z data. Domyslnie \code{"date"}.
#' @param color_by Opcjonalna kolumna roznicujaca serie kolorem
#'   (np. \code{"family"} lub \code{"store_nbr"}). \code{NULL} = jedna seria.
#' @param smooth Czy dodac wygladzona linie trendu (\code{geom_smooth}).
#'   Domyslnie \code{FALSE}.
#' @param title Tytul wykresu.
#'
#' @return Obiekt \code{ggplot}.
#'
#' @examples
#' df <- data.frame(
#'   date = rep(as.Date("2023-01-01") + 0:9, 2),
#'   store_nbr = rep(1:2, each = 10), family = "A",
#'   sales = runif(20, 5, 50), onpromotion = 0L
#' )
#' plot_sales_trends(df, color_by = "store_nbr")
#'
#' @importFrom dplyr group_by summarise ungroup across all_of
#' @importFrom ggplot2 ggplot aes geom_line geom_smooth labs theme_minimal
#' @importFrom rlang .data sym
#' @export
plot_sales_trends <- function(data,
                              value_col = "sales",
                              date_col  = "date",
                              color_by  = NULL,
                              smooth    = FALSE,
                              title     = "Trend sprzedazy w czasie") {

  stopifnot(is.data.frame(data))
  color_by <- if (!is.null(color_by) && color_by %in% names(data)) color_by else NULL

  grp <- c(date_col, color_by)
  agg <- data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grp))) %>%
    dplyr::summarise(.sales = sum(.data[[value_col]], na.rm = TRUE),
                     .groups = "drop")

  if (is.null(color_by)) {
    p <- ggplot2::ggplot(
      agg, ggplot2::aes(x = .data[[date_col]], y = .data$.sales)
    ) +
      ggplot2::geom_line(color = "#2E75B6", linewidth = 0.6)
  } else {
    agg[[color_by]] <- as.factor(agg[[color_by]])
    p <- ggplot2::ggplot(
      agg, ggplot2::aes(x = .data[[date_col]], y = .data$.sales,
                        color = .data[[color_by]])
    ) +
      ggplot2::geom_line(linewidth = 0.6)
  }

  if (smooth) {
    p <- p + ggplot2::geom_smooth(method = "loess", se = FALSE,
                                  linewidth = 0.8, formula = y ~ x)
  }

  p +
    ggplot2::labs(title = title, x = "Data", y = "Sprzedaz",
                  color = color_by) +
    ggplot2::theme_minimal()
}

#' Wizualizacja metryk biznesowych
#'
#' Rysuje wykres slupkowy wybranej metryki obliczonej przez
#' \code{\link{compute_sales_metrics}} w rozbiciu na grupy (np. sklepy lub
#' kategorie). Sluzy do szybkiego porownania jednostek biznesowych.
#'
#' @param metrics \code{tibble} zwrocony przez \code{\link{compute_sales_metrics}}
#'   z argumentem \code{by} (musi zawierac kolumne grupujaca).
#' @param metric Nazwa metryki do wyswietlenia (np. \code{"total_sales"},
#'   \code{"growth_rate"}, \code{"cv_sales"}).
#' @param group_col Kolumna grupujaca na osi X. Jezeli \code{NULL}, brana jest
#'   pierwsza kolumna nie bedaca metryka.
#' @param top_n Opcjonalnie - pokaz tylko \code{top_n} grup o najwyzszej
#'   wartosci metryki. \code{NULL} = wszystkie.
#' @param title Tytul wykresu. \code{NULL} = automatyczny.
#'
#' @return Obiekt \code{ggplot}.
#'
#' @examples
#' m <- data.frame(store_nbr = 1:3, total_sales = c(100, 250, 175))
#' plot_sales_metrics(m, metric = "total_sales", group_col = "store_nbr")
#'
#' @importFrom dplyr arrange desc slice_head mutate
#' @importFrom ggplot2 ggplot aes geom_col coord_flip labs theme_minimal
#' @importFrom rlang .data sym
#' @importFrom stats reorder
#' @export
plot_sales_metrics <- function(metrics,
                               metric    = "total_sales",
                               group_col = NULL,
                               top_n     = NULL,
                               title     = NULL) {

  stopifnot(is.data.frame(metrics))
  if (!metric %in% names(metrics)) {
    stop("Brak metryki '", metric, "' w danych.", call. = FALSE)
  }

  metric_names <- c("total_sales", "mean_sales", "median_sales", "sd_sales",
                    "cv_sales", "last_ma", "promo_share", "avg_peak_distance",
                    "growth_rate", "n_periods")
  if (is.null(group_col)) {
    candidates <- setdiff(names(metrics), metric_names)
    if (length(candidates) == 0) {
      stop("Nie znaleziono kolumny grupujacej. Podaj 'group_col'.", call. = FALSE)
    }
    group_col <- candidates[1]
  }

  metrics[[group_col]] <- as.factor(metrics[[group_col]])

  if (!is.null(top_n)) {
    metrics <- metrics %>%
      dplyr::arrange(dplyr::desc(.data[[metric]])) %>%
      dplyr::slice_head(n = top_n)
  }

  if (is.null(title)) title <- paste("Metryka:", metric, "wg", group_col)

  ggplot2::ggplot(
    metrics,
    ggplot2::aes(x = stats::reorder(.data[[group_col]], .data[[metric]]),
                 y = .data[[metric]])
  ) +
    ggplot2::geom_col(fill = "#2E75B6") +
    ggplot2::coord_flip() +
    ggplot2::labs(title = title, x = group_col, y = metric) +
    ggplot2::theme_minimal()
}
