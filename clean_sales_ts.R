#' Czyszczenie i przygotowanie danych szeregu czasowego
#'
#' Porzadkuje dane sprzedazowe: obsluguje duplikaty dat, braki danych,
#' opcjonalnie uzupelnia luki w kalendarzu, agreguje czasowo i sortuje.
#'
#' @param data Tabela z danymi (np. wynik [load_sales_data()]).
#' @param key Kolumny identyfikujace szereg poza data (domyslnie
#'   `c("store_nbr", "family")`). Uzywane do deduplikacji i agregacji.
#' @param value Kolumna z wartoscia sprzedazy (domyslnie `"sales"`).
#' @param date_col Kolumna z data (domyslnie `"date"`).
#' @param na_action Obsluga brakow w `value`: `"zero"` (zamien na 0),
#'   `"drop"` (usun wiersze), `"locf"` (przenies ostatnia wartosc w przod),
#'   `"interpolate"` (interpolacja liniowa), `"none"`.
#' @param dedup Obsluga duplikatow daty w obrebie szeregu: `"sum"`, `"mean"`,
#'   `"first"`, `"none"`.
#' @param fill_gaps Czy uzupelnic brakujace daty w sekwencji (domyslnie
#'   `FALSE`). Nowe daty otrzymuja `value = 0` (sklep zamkniety) oraz
#'   `onpromotion = 0`.
#' @param aggregate Agregacja czasowa: `"none"`, `"week"`, `"month"`,
#'   `"quarter"`, `"year"`.
#' @param sort Czy posortowac wynik po kluczu i dacie (domyslnie `TRUE`).
#'
#' @return Wyczyszczona tabela (`tibble`).
#'
#' @examples
#' \dontrun{
#' clean <- clean_sales_ts(sales, na_action = "zero",
#'                         dedup = "sum", fill_gaps = TRUE,
#'                         aggregate = "month")
#' }
#' @export
clean_sales_ts <- function(data,
                           key = c("store_nbr", "family"),
                           value = "sales",
                           date_col = "date",
                           na_action = c("zero", "drop", "locf", "interpolate", "none"),
                           dedup = c("sum", "mean", "first", "none"),
                           fill_gaps = FALSE,
                           aggregate = c("none", "week", "month", "quarter", "year"),
                           sort = TRUE) {

  na_action <- match.arg(na_action)
  dedup     <- match.arg(dedup)
  aggregate <- match.arg(aggregate)
  key       <- intersect(key, names(data))
  stopifnot(date_col %in% names(data), value %in% names(data))

  d <- dplyr::mutate(data, !!date_col := lubridate::as_date(.data[[date_col]]))

  # 1. Duplikaty daty w obrebie szeregu
  group_full <- c(key, date_col)
  if (dedup != "none" && length(key) > 0) {
    has_promo <- "onpromotion" %in% names(d)
    agg_fun <- switch(dedup, sum = sum, mean = mean, first = dplyr::first)
    if (dedup == "first") {
      d <- d %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(group_full))) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup()
    } else {
      d <- d %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(group_full))) %>%
        dplyr::summarise(
          !!value := agg_fun(.data[[value]], na.rm = TRUE),
          onpromotion = if (has_promo) sum(.data[["onpromotion"]], na.rm = TRUE) else NA_real_,
          dplyr::across(dplyr::any_of(c("city", "state", "type", "cluster",
                                        "is_holiday", "is_event")), dplyr::first),
          .groups = "drop"
        )
      if (!has_promo) d$onpromotion <- NULL
    }
  }

  # 2. Uzupelnienie luk w sekwencji dat
  if (isTRUE(fill_gaps)) {
    d <- d %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(key))) %>%
      tidyr::complete(
        !!date_col := seq(min(.data[[date_col]]), max(.data[[date_col]]), by = "day")
      ) %>%
      dplyr::ungroup()
    d[[value]] <- ifelse(is.na(d[[value]]), 0, d[[value]])
    if ("onpromotion" %in% names(d))
      d[["onpromotion"]] <- ifelse(is.na(d[["onpromotion"]]), 0, d[["onpromotion"]])
  }

  # 3. Obsluga brakow danych
  if (na_action == "zero") {
    d[[value]] <- ifelse(is.na(d[[value]]), 0, d[[value]])
  } else if (na_action == "drop") {
    d <- d[!is.na(d[[value]]), ]
  } else if (na_action %in% c("locf", "interpolate")) {
    d <- d %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(key))) %>%
      dplyr::arrange(.data[[date_col]], .by_group = TRUE)
    if (na_action == "locf") {
      d <- tidyr::fill(d, dplyr::all_of(value), .direction = "downup")
    } else {
      d <- dplyr::mutate(d, !!value := .interp_na(.data[[value]]))
    }
    d <- dplyr::ungroup(d)
  }

  # 4. Agregacja czasowa
  if (aggregate != "none") {
    has_promo <- "onpromotion" %in% names(d)
    d <- d %>%
      dplyr::mutate(!!date_col := lubridate::floor_date(.data[[date_col]], unit = aggregate)) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c(key, date_col)))) %>%
      dplyr::summarise(
        !!value := sum(.data[[value]], na.rm = TRUE),
        onpromotion = if (has_promo) sum(.data[["onpromotion"]], na.rm = TRUE) else NA_real_,
        dplyr::across(dplyr::any_of(c("city", "state", "type", "cluster")), dplyr::first),
        is_holiday = if ("is_holiday" %in% names(d)) any(.data[["is_holiday"]], na.rm = TRUE) else NA,
        is_event   = if ("is_event"   %in% names(d)) any(.data[["is_event"]],   na.rm = TRUE) else NA,
        .groups = "drop"
      )
    if (!has_promo) d$onpromotion <- NULL
  }

  if (isTRUE(sort) && length(key) > 0) {
    d <- dplyr::arrange(d, dplyr::across(dplyr::all_of(c(key, date_col))))
  } else if (isTRUE(sort)) {
    d <- dplyr::arrange(d, .data[[date_col]])
  }

  d
}

# interpolacja liniowa NA wewnatrz wektora (brzegi: najblizsza wartosc)
.interp_na <- function(x) {
  if (all(is.na(x))) return(x)
  idx <- seq_along(x)
  stats::approx(idx[!is.na(x)], x[!is.na(x)], xout = idx, rule = 2)$y
}
