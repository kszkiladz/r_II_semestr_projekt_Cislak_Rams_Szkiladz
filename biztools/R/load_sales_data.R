#' Wczytanie danych sprzedazowych do jednego ujednoliconego zbioru
#'
#' Wczytuje surowe pliki z konkursu Kaggle \emph{Store Sales - Time Series
#' Forecasting} (\code{train.csv}, \code{stores.csv}, \code{holidays_events.csv})
#' i laczy je w jeden uporzadkowany \code{tibble}. Wykorzystuje pakiet
#' \pkg{readr} z rodziny \pkg{tidyverse}, ktory poprawnie obsluguje pola w
#' cudzyslowach (np. kategoria \code{"LIQUOR,WINE,BEER"} zawierajaca przecinek).
#'
#' @param train_path Sciezka do pliku \code{train.csv}.
#' @param stores_path Sciezka do pliku \code{stores.csv}. Jezeli \code{NULL},
#'   metadane sklepow nie sa dolaczane.
#' @param holidays_path Sciezka do pliku \code{holidays_events.csv}. Jezeli
#'   \code{NULL}, informacje o swietach nie sa dolaczane.
#' @param stores Opcjonalny wektor numerow sklepow do wczytania. Pozwala
#'   ograniczyc ilosc danych (plik \code{train.csv} ma ok. 3 mln wierszy).
#'   \code{NULL} oznacza wszystkie sklepy.
#' @param families Opcjonalny wektor kategorii produktow (kolumna
#'   \code{family}) do wczytania. \code{NULL} oznacza wszystkie kategorie.
#' @param n_max Maksymalna liczba wierszy do wczytania z \code{train.csv}.
#'   Domyslnie \code{Inf} (caly plik).
#'
#' @return \code{tibble} z kolumnami: \code{date}, \code{store_nbr},
#'   \code{family}, \code{sales}, \code{onpromotion} oraz – jezeli dostepne –
#'   \code{city}, \code{state}, \code{type}, \code{cluster}, \code{is_holiday}.
#'
#' @examples
#' \dontrun{
#' sales <- load_sales_data(
#'   train_path    = "train.csv",
#'   stores_path   = "stores.csv",
#'   holidays_path = "holidays_events.csv",
#'   stores        = c(1, 2, 3)
#' )
#' }
#'
#' @importFrom readr read_csv cols col_double col_date col_character col_integer
#' @importFrom dplyr filter left_join mutate distinct select if_else
#' @importFrom rlang .data
#' @export
load_sales_data <- function(train_path,
                            stores_path   = NULL,
                            holidays_path = NULL,
                            stores        = NULL,
                            families      = NULL,
                            n_max         = Inf) {

  if (!file.exists(train_path)) {
    stop("Nie znaleziono pliku train: ", train_path, call. = FALSE)
  }

  # Jawnie definiujemy typy kolumn - szybsze i bezpieczniejsze niz zgadywanie.
  train <- readr::read_csv(
    train_path,
    n_max = n_max,
    col_types = readr::cols(
      id          = readr::col_double(),
      date        = readr::col_date(format = "%Y-%m-%d"),
      store_nbr   = readr::col_integer(),
      family      = readr::col_character(),
      sales       = readr::col_double(),
      onpromotion = readr::col_integer()
    )
  )

  # Filtrowanie na etapie wczytywania ogranicza zuzycie pamieci.
  if (!is.null(stores)) {
    train <- dplyr::filter(train, .data$store_nbr %in% stores)
  }
  if (!is.null(families)) {
    train <- dplyr::filter(train, .data$family %in% families)
  }

  train <- dplyr::select(
    train,
    "date", "store_nbr", "family", "sales", "onpromotion"
  )

  # Dolaczenie metadanych sklepow (miasto, stan, typ) - potrzebne w sales_ts_logic().
  if (!is.null(stores_path)) {
    if (!file.exists(stores_path)) {
      stop("Nie znaleziono pliku stores: ", stores_path, call. = FALSE)
    }
    stores_df <- readr::read_csv(
      stores_path,
      col_types = readr::cols(
        store_nbr = readr::col_integer(),
        city      = readr::col_character(),
        state     = readr::col_character(),
        type      = readr::col_character(),
        cluster   = readr::col_integer()
      )
    )
    train <- dplyr::left_join(train, stores_df, by = "store_nbr")
  }

  # Dolaczenie flagi swiatecznej - przydatne przy interpretacji szczytow.
  if (!is.null(holidays_path)) {
    if (!file.exists(holidays_path)) {
      stop("Nie znaleziono pliku holidays: ", holidays_path, call. = FALSE)
    }
    holidays_df <- readr::read_csv(
      holidays_path,
      col_types = readr::cols(
        date        = readr::col_date(format = "%Y-%m-%d"),
        type        = readr::col_character(),
        locale      = readr::col_character(),
        locale_name = readr::col_character(),
        description = readr::col_character(),
        transferred = readr::col_character()
      )
    )
    # Liczy sie tylko obecnosc swieta danego dnia; pomijamy swieta przeniesione.
    holiday_dates <- holidays_df %>%
      dplyr::filter(.data$transferred != "True") %>%
      dplyr::distinct(.data$date) %>%
      dplyr::mutate(is_holiday = TRUE)

    train <- train %>%
      dplyr::left_join(holiday_dates, by = "date") %>%
      dplyr::mutate(
        is_holiday = dplyr::if_else(is.na(.data$is_holiday), FALSE, .data$is_holiday)
      )
  }

  train
}
