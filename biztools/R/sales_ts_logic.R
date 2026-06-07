#' Funkcja wyzszego rzedu do analizy sprzedazy na wybranych podzbiorach
#'
#' \code{sales_ts_logic()} jest funkcja wyzszego rzedu (ang. \emph{higher-order
#' function}) w duchu materialow o funkcjach w R: przyjmuje \strong{inne
#' funkcje} jako argumenty (np. \code{\link{compute_sales_metrics}} oraz
#' \code{\link{plot_sales_metrics}}) i stosuje je do podzbioru danych
#' wyfiltrowanego po metadanych sklepu (miasto, stan, typ) i zakresie czasu.
#'
#' Oddziela \emph{co} robimy (przekazana logika analityczna) od \emph{jak}
#' wybieramy dane (filtrowanie i grupowanie wykonywane wewnatrz tej funkcji).
#'
#' @param data \code{tibble} z danymi sprzedazowymi wraz z metadanymi sklepow
#'   (kolumny \code{city}, \code{state}, \code{type}) - zob.
#'   \code{\link{load_sales_data}}.
#' @param metric_fun Funkcja obliczajaca metryki, wywolywana na wyfiltrowanych
#'   danych. Domyslnie \code{compute_sales_metrics}.
#' @param plot_fun Funkcja rysujaca wynik. Domyslnie \code{plot_sales_metrics}.
#'   Mozna podac \code{NULL}, aby pominac wykres.
#' @param city,state,type Opcjonalne filtry metadanych sklepu. Kazdy moze byc
#'   wektorem wartosci. \code{NULL} = brak filtra.
#' @param date_from,date_to Opcjonalne granice zakresu czasu (\code{Date} lub
#'   tekst \code{"RRRR-MM-DD"}). \code{NULL} = brak ograniczenia.
#' @param by Kolumny grupujace przekazywane do \code{metric_fun}.
#'   Domyslnie \code{"store_nbr"}.
#' @param metric Nazwa metryki przekazywana do \code{plot_fun}.
#'   Domyslnie \code{"total_sales"}.
#' @param ... Dodatkowe argumenty przekazywane do \code{metric_fun}.
#'
#' @return Lista (klasy \code{sales_ts_logic_result}) z elementami:
#'   \code{metrics} (\code{tibble} metryk), \code{plot} (obiekt \code{ggplot}
#'   lub \code{NULL}), \code{n_rows} (liczba wierszy po filtrowaniu) oraz
#'   \code{filters} (zastosowane filtry).
#'
#' @examples
#' \dontrun{
#' res <- sales_ts_logic(
#'   sales,
#'   city   = "Quito",
#'   type   = c("A", "B"),
#'   date_from = "2016-01-01",
#'   by     = "store_nbr",
#'   metric = "total_sales"
#' )
#' res$metrics
#' res$plot
#' }
#'
#' @importFrom dplyr filter across all_of
#' @importFrom rlang .data
#' @export
sales_ts_logic <- function(data,
                           metric_fun = compute_sales_metrics,
                           plot_fun   = plot_sales_metrics,
                           city       = NULL,
                           state      = NULL,
                           type       = NULL,
                           date_from  = NULL,
                           date_to    = NULL,
                           by         = "store_nbr",
                           metric     = "total_sales",
                           ...) {

  stopifnot(is.data.frame(data))
  stopifnot(is.function(metric_fun))

  d <- data

  # Filtr metadanych - tylko jezeli kolumna istnieje i filtr podano
  apply_meta_filter <- function(df, col, vals) {
    if (!is.null(vals) && col %in% names(df)) {
      df <- df[df[[col]] %in% vals, , drop = FALSE]
    }
    df
  }
  d <- apply_meta_filter(d, "city",  city)
  d <- apply_meta_filter(d, "state", state)
  d <- apply_meta_filter(d, "type",  type)

  # Filtr czasu
  if ("date" %in% names(d)) {
    if (!is.null(date_from)) d <- d[d$date >= as.Date(date_from), , drop = FALSE]
    if (!is.null(date_to))   d <- d[d$date <= as.Date(date_to),   , drop = FALSE]
  }

  if (nrow(d) == 0) {
    warning("Po filtrowaniu nie pozostaly zadne dane.", call. = FALSE)
  }

  # === Zastosowanie przekazanej logiki analitycznej ===
  metrics <- metric_fun(d, by = by, ...)

  plot_obj <- NULL
  if (!is.null(plot_fun) && nrow(d) > 0) {
    group_col <- if (length(by) >= 1) by[1] else NULL
    plot_obj <- tryCatch(
      plot_fun(metrics, metric = metric, group_col = group_col),
      error = function(e) {
        warning("plot_fun nie powiodla sie: ", conditionMessage(e), call. = FALSE)
        NULL
      }
    )
  }

  out <- list(
    metrics = metrics,
    plot    = plot_obj,
    n_rows  = nrow(d),
    filters = list(city = city, state = state, type = type,
                   date_from = date_from, date_to = date_to,
                   by = by, metric = metric)
  )
  class(out) <- "sales_ts_logic_result"
  out
}

#' @export
#' @noRd
print.sales_ts_logic_result <- function(x, ...) {
  cat("=== Wynik sales_ts_logic ===\n")
  cat(sprintf("Wierszy po filtrowaniu: %s\n", format(x$n_rows, big.mark = " ")))
  f <- x$filters
  cat("Filtry: ",
      paste0("city=", ifelse(is.null(f$city), "-", paste(f$city, collapse = "/")),
             "; state=", ifelse(is.null(f$state), "-", paste(f$state, collapse = "/")),
             "; type=", ifelse(is.null(f$type), "-", paste(f$type, collapse = "/")),
             "; od=", ifelse(is.null(f$date_from), "-", f$date_from),
             "; do=", ifelse(is.null(f$date_to), "-", f$date_to)),
      "\n", sep = "")
  cat("Metryki:\n")
  print(x$metrics)
  invisible(x)
}
