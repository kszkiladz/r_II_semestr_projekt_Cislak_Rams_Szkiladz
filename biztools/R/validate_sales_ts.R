#' Kontrola jakosci danych szeregu czasowego sprzedazy
#'
#' Sprawdza najczestsze problemy z danymi sprzedazowymi: braki danych,
#' duplikaty kombinacji data-sklep-kategoria, poprawnosc dat, zakres wartosci
#' sprzedazy oraz spojnosc czestotliwosci (luki w datach). Zwraca raport, ktory
#' mozna wydrukowac lub przekazac dalej do \code{\link{clean_sales_ts}}.
#'
#' @param data \code{data.frame}/\code{tibble} zwrocony przez
#'   \code{\link{load_sales_data}}.
#' @param value_col Nazwa kolumny z wartoscia sprzedazy. Domyslnie \code{"sales"}.
#' @param date_col Nazwa kolumny z data. Domyslnie \code{"date"}.
#' @param group_cols Kolumny identyfikujace pojedynczy szereg czasowy.
#'   Domyslnie \code{c("store_nbr", "family")}.
#' @param verbose Czy wypisac raport na konsole. Domyslnie \code{TRUE}.
#'
#' @return Niewidocznie (\code{invisible}) lista klasy \code{sales_validation}
#'   z polami diagnostycznymi: \code{n_rows}, \code{n_missing_value},
#'   \code{n_negative}, \code{n_duplicates}, \code{n_invalid_dates},
#'   \code{date_range}, \code{frequency}, \code{n_date_gaps}, \code{passed}.
#'
#' @examples
#' df <- data.frame(
#'   date = as.Date("2023-01-01") + 0:4,
#'   store_nbr = 1L, family = "A",
#'   sales = c(10, NA, -3, 20, 15), onpromotion = 0L
#' )
#' validate_sales_ts(df)
#'
#' @importFrom dplyr group_by summarise n filter pull arrange across all_of
#' @importFrom rlang .data sym syms
#' @export
validate_sales_ts <- function(data,
                              value_col  = "sales",
                              date_col   = "date",
                              group_cols = c("store_nbr", "family"),
                              verbose    = TRUE) {

  stopifnot(is.data.frame(data))
  group_cols <- intersect(group_cols, names(data))

  if (!value_col %in% names(data)) {
    stop("Brak kolumny z wartoscia: ", value_col, call. = FALSE)
  }
  if (!date_col %in% names(data)) {
    stop("Brak kolumny z data: ", date_col, call. = FALSE)
  }

  vals  <- data[[value_col]]
  dates <- data[[date_col]]

  # 1. Braki danych
  n_missing_value <- sum(is.na(vals))
  n_missing_date  <- sum(is.na(dates))

  # 2. Zakres wartosci - sprzedaz ujemna jest niemozliwa biznesowo
  n_negative <- sum(vals < 0, na.rm = TRUE)

  # 3. Poprawnosc dat
  if (!inherits(dates, "Date")) {
    parsed <- suppressWarnings(as.Date(as.character(dates)))
    n_invalid_dates <- sum(is.na(parsed) & !is.na(dates))
    dates <- parsed
  } else {
    n_invalid_dates <- 0L
  }

  # 4. Duplikaty: ten sam dzien dla tej samej kombinacji sklep-kategoria
  if (length(group_cols) > 0) {
    dup_tbl <- data %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c(group_cols, date_col)))) %>%
      dplyr::summarise(.n = dplyr::n(), .groups = "drop") %>%
      dplyr::filter(.data$.n > 1)
    n_duplicates <- nrow(dup_tbl)
  } else {
    n_duplicates <- sum(duplicated(dates))
  }

  # 5. Czestotliwosc i luki w datach (na pierwszej grupie reprezentatywnej)
  uniq_sorted <- sort(unique(dates[!is.na(dates)]))
  if (length(uniq_sorted) > 2) {
    diffs <- as.numeric(diff(uniq_sorted))
    modal_step <- as.numeric(names(sort(table(diffs), decreasing = TRUE))[1])
    frequency <- switch(as.character(modal_step),
                        "1" = "dzienna", "7" = "tygodniowa",
                        "30" = "miesieczna", "31" = "miesieczna",
                        paste0("co ", modal_step, " dni"))
    n_date_gaps <- sum(diffs > modal_step)
  } else {
    frequency <- "nieokreslona"
    n_date_gaps <- NA_integer_
  }

  date_range <- if (length(uniq_sorted) > 0) range(uniq_sorted) else c(NA, NA)

  passed <- n_missing_value == 0 && n_negative == 0 &&
            n_duplicates == 0 && n_invalid_dates == 0 &&
            (is.na(n_date_gaps) || n_date_gaps == 0)

  result <- list(
    n_rows          = nrow(data),
    n_missing_value = n_missing_value,
    n_missing_date  = n_missing_date,
    n_negative      = n_negative,
    n_duplicates    = n_duplicates,
    n_invalid_dates = n_invalid_dates,
    date_range      = date_range,
    frequency       = frequency,
    n_date_gaps     = n_date_gaps,
    passed          = passed
  )
  class(result) <- "sales_validation"

  if (verbose) print(result)
  invisible(result)
}

#' @export
#' @noRd
print.sales_validation <- function(x, ...) {
  cat("=== Raport jakosci danych sprzedazowych ===\n")
  cat(sprintf("Liczba wierszy:            %s\n", format(x$n_rows, big.mark = " ")))
  cat(sprintf("Braki w sprzedazy:         %d\n", x$n_missing_value))
  cat(sprintf("Braki w datach:            %d\n", x$n_missing_date))
  cat(sprintf("Wartosci ujemne:           %d\n", x$n_negative))
  cat(sprintf("Duplikaty (data+grupa):    %d\n", x$n_duplicates))
  cat(sprintf("Niepoprawne daty:          %d\n", x$n_invalid_dates))
  cat(sprintf("Zakres dat:                %s - %s\n",
              x$date_range[1], x$date_range[2]))
  cat(sprintf("Czestotliwosc:             %s\n", x$frequency))
  cat(sprintf("Luki w datach:             %s\n",
              ifelse(is.na(x$n_date_gaps), "n/d", x$n_date_gaps)))
  cat(sprintf("Wynik koncowy:             %s\n",
              ifelse(x$passed, "OK", "WYMAGA CZYSZCZENIA")))
  invisible(x)
}
