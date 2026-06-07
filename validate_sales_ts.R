#' Kontrola jakosci danych szeregu czasowego sprzedazy
#'
#' Sprawdza najwazniejsze problemy danych przed analiza: braki danych,
#' duplikaty klucza, poprawnosc dat, zakres wartosci oraz spojnosc
#' czestotliwosci (luki w sekwencji dat poszczegolnych szeregow).
#'
#' @param data Tabela z danymi (wynik [load_sales_data()]).
#' @param key Wektor kolumn identyfikujacych pojedyncza obserwacje
#'   (domyslnie `c("date", "store_nbr", "family")`). Kolumny nieobecne
#'   w danych sa pomijane.
#' @param value Nazwa kolumny z wartoscia sprzedazy (domyslnie `"sales"`).
#' @param date_col Nazwa kolumny z data (domyslnie `"date"`).
#' @param min_value Dolny dopuszczalny prog wartosci (domyslnie `0` - sprzedaz
#'   nie powinna byc ujemna).
#' @param expected_step Oczekiwany krok czasowy w dniach (domyslnie `1`).
#'
#' @return Obiekt klasy `biztools_validation`: lista z elementami `$ok`
#'   (czy dane przechodza wszystkie krytyczne testy) oraz `$checks`
#'   (tabela z wynikami poszczegolnych kontroli). Posiada metode `print`.
#'
#' @examples
#' \dontrun{
#' rep <- validate_sales_ts(sales)
#' rep            # czytelne podsumowanie
#' rep$checks     # szczegoly w formie tabeli
#' }
#' @export
validate_sales_ts <- function(data,
                              key = c("date", "store_nbr", "family"),
                              value = "sales",
                              date_col = "date",
                              min_value = 0,
                              expected_step = 1) {

  stopifnot(is.data.frame(data))
  key <- intersect(key, names(data))
  add <- function(check, status, n, detail) {
    dplyr::tibble(check = check, status = status,
                  n_problems = n, detail = detail)
  }
  checks <- list()

  # 1. Wymagane kolumny
  required <- c(date_col, value)
  missing_cols <- setdiff(required, names(data))
  checks[[length(checks) + 1]] <- add(
    "wymagane_kolumny",
    if (length(missing_cols) == 0) "OK" else "ERROR",
    length(missing_cols),
    if (length(missing_cols) == 0) "wszystkie obecne"
    else paste("brak:", paste(missing_cols, collapse = ", "))
  )
  if (length(missing_cols) > 0) {
    out <- list(ok = FALSE, checks = dplyr::bind_rows(checks))
    class(out) <- "biztools_validation"
    return(out)
  }

  # 2. Braki danych
  na_val  <- sum(is.na(data[[value]]))
  na_date <- sum(is.na(data[[date_col]]))
  checks[[length(checks) + 1]] <- add(
    "braki_w_wartosci", if (na_val == 0) "OK" else "WARNING",
    na_val, sprintf("NA w '%s'", value))
  checks[[length(checks) + 1]] <- add(
    "braki_w_dacie", if (na_date == 0) "OK" else "ERROR",
    na_date, sprintf("NA w '%s'", date_col))

  # 3. Duplikaty klucza
  if (length(key) > 0) {
    dups <- sum(duplicated(data[key]))
  } else {
    dups <- NA_integer_
  }
  checks[[length(checks) + 1]] <- add(
    "duplikaty_klucza",
    if (is.na(dups)) "SKIP" else if (dups == 0) "OK" else "WARNING",
    dups, paste("klucz:", paste(key, collapse = " + ")))

  # 4. Poprawnosc dat (klasa + brak dat z przyszlosci)
  is_date <- inherits(data[[date_col]], "Date")
  future  <- if (is_date) sum(data[[date_col]] > Sys.Date(), na.rm = TRUE) else NA_integer_
  checks[[length(checks) + 1]] <- add(
    "typ_daty", if (is_date) "OK" else "ERROR",
    if (is_date) 0L else 1L,
    if (is_date) "klasa Date" else "kolumna nie jest typu Date")
  checks[[length(checks) + 1]] <- add(
    "daty_z_przyszlosci",
    if (!is_date) "SKIP" else if (future == 0) "OK" else "WARNING",
    future, "daty > dzisiaj")

  # 5. Zakres wartosci
  neg <- sum(data[[value]] < min_value, na.rm = TRUE)
  checks[[length(checks) + 1]] <- add(
    "zakres_wartosci", if (neg == 0) "OK" else "WARNING",
    neg, sprintf("wartosci < %s", min_value))
  if ("onpromotion" %in% names(data)) {
    neg_p <- sum(data[["onpromotion"]] < 0, na.rm = TRUE)
    checks[[length(checks) + 1]] <- add(
      "zakres_promocji", if (neg_p == 0) "OK" else "WARNING",
      neg_p, "onpromotion < 0")
  }

  # 6. Spojnosc czestotliwosci (luki w sekwencji dat per szereg)
  group_cols <- setdiff(key, date_col)
  if (is_date) {
    if (length(group_cols) > 0) {
      gap_tab <- data %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
        dplyr::summarise(
          n_dates = dplyr::n_distinct(.data[[date_col]]),
          span    = as.integer(max(.data[[date_col]]) -
                                 min(.data[[date_col]])) / expected_step + 1L,
          .groups = "drop"
        ) %>%
        dplyr::mutate(missing = pmax(0, round(span) - n_dates))
    } else {
      d <- sort(unique(data[[date_col]]))
      span <- as.integer(max(d) - min(d)) / expected_step + 1L
      gap_tab <- dplyr::tibble(missing = max(0, round(span) - length(d)))
    }
    total_missing <- sum(gap_tab$missing)
    n_series_gap  <- sum(gap_tab$missing > 0)
    checks[[length(checks) + 1]] <- add(
      "spojnosc_czestotliwosci",
      if (total_missing == 0) "OK" else "WARNING",
      total_missing,
      sprintf("luk w datach lacznie (szeregow z lukami: %d)", n_series_gap))
  }

  out <- list(ok = !any(dplyr::bind_rows(checks)$status == "ERROR"),
              checks = dplyr::bind_rows(checks))
  class(out) <- "biztools_validation"
  out
}

#' @export
print.biztools_validation <- function(x, ...) {
  cat("=== Raport jakosci danych (biztools) ===\n")
  cat("Status ogolny:", if (x$ok) "PRZESZEDL (brak bledow krytycznych)"
      else "BLAD KRYTYCZNY - napraw przed analiza", "\n\n")
  ch <- x$checks
  for (i in seq_len(nrow(ch))) {
    cat(sprintf("[%-8s] %-26s | %s\n",
                ch$status[i], ch$check[i], ch$detail[i]))
  }
  invisible(x)
}
