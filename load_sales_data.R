#' Wczytanie i polaczenie danych sprzedazowych
#'
#' Wczytuje trzy pliki zbioru "Store Sales" (transakcje, metadane sklepow,
#' kalendarz swiat) z wykorzystaniem \pkg{readr}, laczy je w jedna tabele
#' i opcjonalnie dolacza flagi swiat (`is_holiday`) oraz wydarzen (`is_event`).
#'
#' Regula flagi swiatecznej (zgodna ze struktura danych Kaggle):
#' dniem wolnym jest data o typie `Holiday`, `Additional`, `Bridge` lub
#' `Transfer`, ktora nie zostala przeniesiona (`transferred == FALSE`).
#' Typ `Work Day` traktowany jest jako dzien roboczy. Zasieg uwzglednia
#' poziom `National` (caly kraj), `Regional` (po stanie) oraz `Local`
#' (po miescie). Typ `Event` trafia do osobnej flagi `is_event`.
#'
#' @param train_path Sciezka do pliku z transakcjami (kolumny: `id`, `date`,
#'   `store_nbr`, `family`, `sales`, `onpromotion`).
#' @param stores_path Sciezka do pliku z metadanymi sklepow.
#' @param holidays_path Sciezka do pliku ze swietami.
#' @param families Opcjonalny wektor kategorii do wczytania (filtr).
#' @param stores Opcjonalny wektor numerow sklepow do wczytania (filtr).
#' @param add_holidays Czy dolaczyc flagi swiat/wydarzen (domyslnie `TRUE`).
#' @param n_max Maksymalna liczba wierszy transakcji do wczytania
#'   (przydatne przy testach na probce; domyslnie `Inf`).
#'
#' @return Tabela (`tibble`) z transakcjami wzbogacona o metadane sklepu
#'   (`city`, `state`, `type`, `cluster`) i ewentualnie o `is_holiday`,
#'   `is_event`.
#'
#' @examples
#' \dontrun{
#' sales <- load_sales_data("train.csv", "stores.csv", "holidays_events.csv")
#' # Szybki test na probce jednej kategorii:
#' s <- load_sales_data(families = "BEVERAGES", n_max = 1e5)
#' }
#' @export
load_sales_data <- function(train_path = "train.csv",
                            stores_path = "stores.csv",
                            holidays_path = "holidays_events.csv",
                            families = NULL,
                            stores = NULL,
                            add_holidays = TRUE,
                            n_max = Inf) {

  for (p in c(train_path, stores_path)) {
    if (!file.exists(p)) {
      stop("Nie znaleziono pliku: ", p,
           ". Sprawdz sciezke lub ustaw working directory (setwd()).",
           call. = FALSE)
    }
  }

  train <- readr::read_csv(
    train_path,
    col_types = readr::cols(
      id          = readr::col_double(),
      date        = readr::col_date(format = ""),
      store_nbr   = readr::col_integer(),
      family      = readr::col_character(),
      sales       = readr::col_double(),
      onpromotion = readr::col_double()
    ),
    n_max = n_max,
    progress = FALSE
  )

  if (!is.null(families)) train <- dplyr::filter(train, family %in% families)
  if (!is.null(stores))   train <- dplyr::filter(train, store_nbr %in% stores)

  stores_df <- readr::read_csv(
    stores_path,
    col_types = readr::cols(
      store_nbr = readr::col_integer(),
      city      = readr::col_character(),
      state     = readr::col_character(),
      type      = readr::col_character(),
      cluster   = readr::col_integer()
    ),
    progress = FALSE
  )

  dat <- dplyr::left_join(train, stores_df, by = "store_nbr")

  if (isTRUE(add_holidays)) {
    if (!file.exists(holidays_path)) {
      warning("Nie znaleziono pliku swiat: ", holidays_path,
              " - pomijam flagi swiateczne.", call. = FALSE)
      dat$is_holiday <- NA
      dat$is_event   <- NA
    } else {
      hol <- readr::read_csv(
        holidays_path,
        col_types = readr::cols(
          date        = readr::col_date(format = ""),
          type        = readr::col_character(),
          locale      = readr::col_character(),
          locale_name = readr::col_character(),
          description = readr::col_character(),
          transferred = readr::col_logical()
        ),
        progress = FALSE
      )
      flags <- .build_holiday_flags(hol)
      dat   <- .apply_holiday_flags(dat, flags)
    }
  }

  class(dat) <- c("biztools_sales", class(dat))
  dat
}

# --- helpery wewnetrzne (nieeksportowane) -------------------------------

.build_holiday_flags <- function(hol) {
  hol <- dplyr::mutate(
    hol,
    transferred = ifelse(is.na(transferred), FALSE, transferred)
  )
  dayoff <- dplyr::filter(
    hol,
    type %in% c("Holiday", "Additional", "Bridge", "Transfer"),
    !transferred
  )
  events <- dplyr::filter(hol, type == "Event")

  list(
    national    = unique(dplyr::pull(dplyr::filter(dayoff, locale == "National"), date)),
    regional    = dplyr::distinct(dplyr::filter(dayoff, locale == "Regional"),
                                  date, state = locale_name),
    local       = dplyr::distinct(dplyr::filter(dayoff, locale == "Local"),
                                  date, city = locale_name),
    event_dates = unique(events$date)
  )
}

.apply_holiday_flags <- function(dat, flags) {
  reg_key <- paste(flags$regional$date, flags$regional$state)
  loc_key <- paste(flags$local$date,    flags$local$city)

  dat$is_holiday <-
    dat$date %in% flags$national |
    paste(dat$date, dat$state) %in% reg_key |
    paste(dat$date, dat$city)  %in% loc_key
  dat$is_event <- dat$date %in% flags$event_dates
  dat
}
