#' biztools: Analiza szeregow czasowych sprzedazy detalicznej
#'
#' Pakiet dla wewnetrznego zespolu analitycznego firmy handlowej. Zapewnia
#' kompletny workflow: od wczytania surowych danych, przez kontrole jakosci,
#' czyszczenie, obliczanie metryk biznesowych, wizualizacje, az po
#' podsumowanie dla menedzera i prognozowanie (ARIMA + Prophet + ETS).
#'
#' Glowne funkcje:
#' \itemize{
#'   \item \code{\link{load_sales_data}} - wczytanie danych,
#'   \item \code{\link{validate_sales_ts}} - kontrola jakosci,
#'   \item \code{\link{clean_sales_ts}} - czyszczenie i agregacja,
#'   \item \code{\link{compute_sales_metrics}} - metryki biznesowe,
#'   \item \code{\link{plot_sales_trends}}, \code{\link{plot_sales_metrics}} - wizualizacje,
#'   \item \code{\link{sales_ts_logic}} - funkcja wyzszego rzedu,
#'   \item \code{\link{create_management_summary}} - podsumowanie menedzerskie,
#'   \item \code{\link{create_prognosis}} - prognozowanie.
#' }
#'
#' @keywords internal
"_PACKAGE"
