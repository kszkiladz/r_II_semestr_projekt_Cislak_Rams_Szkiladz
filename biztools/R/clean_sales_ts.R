#' Czyszczenie i przygotowanie danych szeregu czasowego sprzedazy
#'
#' Porzadkuje surowe dane sprzedazowe: usuwa lub uzupelnia braki, scala
#' duplikaty dat, sortuje obserwacje oraz opcjonalnie agreguje je do nizszej
#' czestotliwosci (np. z dziennej na tygodniowa lub miesieczna).
#'
#' @param data \code{data.frame}/\code{tibble} z danymi sprzedazowymi.
#' @param value_col Nazwa kolumny z wartoscia sprzedazy. Domyslnie \code{"sales"}.
#' @param date_col Nazwa kolumny z data. Domyslnie \code{"date"}.
#' @param group_cols Kolumny identyfikujace pojedynczy szereg.
#'   Domyslnie \code{c("store_nbr", "family")}.
#' @param na_strategy Sposob obslugi brakow: \code{"drop"} (usun wiersze),
#'   \code{"zero"} (zastap zerem), \code{"locf"} (przenies ostatnia obserwacje
#'   w przod) lub \code{"interpolate"} (interpolacja liniowa).
#' @param dedup_fun Funkcja agregujaca przy scalaniu duplikatow dat
#'   (np. \code{sum}, \code{mean}). Domyslnie \code{sum}.
#' @param sort Czy sortowac dane wg grup i daty. Domyslnie \code{TRUE}.
#' @param aggregate Poziom agregacji czasowej: \code{NULL} (bez zmian),
#'   \code{"week"} lub \code{"month"}.
#'
#' @return Wyczyszczony \code{tibble} o tej samej strukturze kolumn (kolumny
#'   metadanych i promocji sa zachowywane podczas agregacji).
#'
#' @examples
#' df <- data.frame(
#'   date = as.Date("2023-01-01") + c(0, 0, 1, 3),
#'   store_nbr = 1L, family = "A",
#'   sales = c(10, 5, NA, 20), onpromotion = c(1L, 0L, 0L, 2L)
#' )
#' clean_sales_ts(df, na_strategy = "zero")
#'
#' @importFrom dplyr group_by summarise ungroup arrange across all_of mutate filter first
#' @importFrom lubridate floor_date
#' @importFrom zoo na.locf na.approx
#' @importFrom rlang .data sym :=
#' @export
clean_sales_ts <- function(data,
                           value_col   = "sales",
                           date_col    = "date",
                           group_cols  = c("store_nbr", "family"),
                           na_strategy = c("drop", "zero", "locf", "interpolate"),
                           dedup_fun   = sum,
                           sort        = TRUE,
                           aggregate = NULL) {

  stopifnot(is.data.frame(data))
  na_strategy <- match.arg(na_strategy)
  group_cols  <- intersect(group_cols, names(data))

  # Daty do typu Date
  if (!inherits(data[[date_col]], "Date")) {
    data[[date_col]] <- as.Date(as.character(data[[date_col]]))
  }
  # Usuwamy wiersze z niemozliwymi do naprawienia datami
  data <- data[!is.na(data[[date_col]]), , drop = FALSE]

  # Kolumny metadanych (niezmienne w obrebie grupy) - zachowujemy przy agregacjach
  meta_cols <- intersect(c("city", "state", "type", "cluster"), names(data))
  has_promo <- "onpromotion" %in% names(data)
  has_holiday <- "is_holiday" %in% names(data)

  # --- 1. Scalanie duplikatow data+grupa ---
  grp <- c(group_cols, date_col)
  agg_exprs <- list()
  agg_exprs[[value_col]] <- rlang::expr(dedup_fun(.data[[value_col]], na.rm = TRUE))
  if (has_promo)   agg_exprs[["onpromotion"]] <- rlang::expr(sum(.data$onpromotion, na.rm = TRUE))
  if (has_holiday) agg_exprs[["is_holiday"]]  <- rlang::expr(any(.data$is_holiday))
  for (m in meta_cols) agg_exprs[[m]] <- rlang::expr(dplyr::first(.data[[!!m]]))

  data <- data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grp))) %>%
    dplyr::summarise(!!!agg_exprs, .groups = "drop")

  # --- 2. Obsluga brakow danych ---
  if (na_strategy == "drop") {
    data <- data[!is.na(data[[value_col]]), , drop = FALSE]
  } else if (na_strategy == "zero") {
    data[[value_col]][is.na(data[[value_col]])] <- 0
  } else {
    # locf / interpolate dzialaja w obrebie grupy, po posortowaniu wg daty
    data <- data %>%
      dplyr::arrange(dplyr::across(dplyr::all_of(c(group_cols, date_col)))) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
      dplyr::mutate(
        !!value_col := if (na_strategy == "locf") {
          zoo::na.locf(.data[[value_col]], na.rm = FALSE)
        } else {
          tryCatch(
            zoo::na.approx(.data[[value_col]], na.rm = FALSE),
            error = function(e) .data[[value_col]]
          )
        }
      ) %>%
      dplyr::ungroup()
    # ewentualne brzegowe NA po locf/approx -> zero
    data[[value_col]][is.na(data[[value_col]])] <- 0
  }

  # --- 3. Agregacja czasowa (opcjonalna) ---
  if (!is.null(aggregate)) {
    unit <- match.arg(aggregate, c("week", "month"))
    data[[date_col]] <- lubridate::floor_date(data[[date_col]], unit = unit)

    grp2 <- c(group_cols, date_col)
    agg2 <- list()
    agg2[[value_col]] <- rlang::expr(sum(.data[[value_col]], na.rm = TRUE))
    if (has_promo)   agg2[["onpromotion"]] <- rlang::expr(sum(.data$onpromotion, na.rm = TRUE))
    if (has_holiday) agg2[["is_holiday"]]  <- rlang::expr(any(.data$is_holiday))
    for (m in meta_cols) agg2[[m]] <- rlang::expr(dplyr::first(.data[[!!m]]))

    data <- data %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(grp2))) %>%
      dplyr::summarise(!!!agg2, .groups = "drop")
  }

  # --- 4. Sortowanie ---
  if (sort) {
    data <- dplyr::arrange(
      data, dplyr::across(dplyr::all_of(c(group_cols, date_col)))
    )
  }

  data
}
