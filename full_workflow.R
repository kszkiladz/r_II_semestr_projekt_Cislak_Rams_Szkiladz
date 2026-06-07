# =============================================================================
# biztools - pelny workflow analizy szeregow czasowych sprzedazy (Poziom 3)
# =============================================================================
# Skrypt pokazuje kompletny przeplyw pracy z pakietem biztools:
#   1. Wczytanie danych          (load_sales_data)
#   2. Kontrola jakosci          (validate_sales_ts)
#   3. Czyszczenie               (clean_sales_ts)
#   4. Metryki biznesowe         (compute_sales_metrics)
#   5. Wizualizacje              (plot_sales_trends / plot_sales_metrics)
#   6. Funkcja wyzszego rzedu    (sales_ts_logic)
#   7. Podsumowanie menedzerskie (create_management_summary)
#   8. Prognozowanie ARIMA+Prophet (create_prognosis / plot_prognosis)
# -----------------------------------------------------------------------------
# UWAGA: plik train.csv ma ok. 3 mln wierszy (~120 MB). Dla wygody i szybkosci
# wczytujemy tylko wybrane sklepy. Aby przetworzyc caly zbior, ustaw stores=NULL.
# =============================================================================

# --- Instalacja / zaladowanie pakietu (wybierz wlasciwa opcje) ---------------
# Wersja lokalna (z folderu zrodlowego):
#   devtools::load_all("biztools")        # podczas rozwoju
#   devtools::install("biztools")         # instalacja jak uzytkownik
# Wersja z GitHub:
#   devtools::install_github("kszkiladz/r_II_semestr_projekt_Cieslak_Rams_Szkiladz")

library(biztools)
library(ggplot2)

# Sciezki do danych - dostosuj do swojego srodowiska
train_path    <- "train.csv"
stores_path   <- "stores.csv"
holidays_path <- "holidays_events.csv"

# --- 1. Wczytanie danych -----------------------------------------------------
# Wczytujemy 6 reprezentatywnych sklepow z roznych miast/typow.
sales_raw <- load_sales_data(
  train_path    = train_path,
  stores_path   = stores_path,
  holidays_path = holidays_path,
  stores        = c(1, 3, 8, 44, 45, 47)
)

cat("Wczytano wierszy:", nrow(sales_raw), "\n")
print(utils::head(sales_raw))

# --- 2. Kontrola jakosci -----------------------------------------------------
validate_sales_ts(sales_raw)

# --- 3. Czyszczenie ----------------------------------------------------------
# Braki -> zero (typowe dla danych sprzedazowych: brak rekordu = brak sprzedazy),
# duplikaty dat scalane suma, sortowanie wlaczone.
sales_clean <- clean_sales_ts(
  sales_raw,
  na_strategy = "zero",
  dedup_fun   = sum,
  sort        = TRUE
)

# Wersja tygodniowa - stabilniejsza do prognoz i wykresow trendu
sales_weekly <- clean_sales_ts(
  sales_raw,
  na_strategy  = "zero",
  aggregate = "week"
)

# --- 4. Metryki biznesowe ----------------------------------------------------
# Globalne
metrics_all <- compute_sales_metrics(sales_clean, ma_window = 7)
print(metrics_all)

# Per sklep
metrics_store <- compute_sales_metrics(sales_clean, by = "store_nbr", ma_window = 7)
print(metrics_store)

# Per kategoria
metrics_cat <- compute_sales_metrics(sales_clean, by = "family", ma_window = 7)
print(utils::head(metrics_cat))

# --- 5. Wizualizacje ---------------------------------------------------------
p_trend <- plot_sales_trends(
  sales_weekly, color_by = "store_nbr", smooth = TRUE,
  title = "Tygodniowy trend sprzedazy wg sklepu"
)
print(p_trend)

p_metric <- plot_sales_metrics(
  metrics_store, metric = "total_sales", group_col = "store_nbr"
)
print(p_metric)

# --- 6. Funkcja wyzszego rzedu ----------------------------------------------
# Analiza wybranego podzbioru: sklepy w Quito, od 2016 r., metryka growth_rate.
res_quito <- sales_ts_logic(
  sales_clean,
  metric_fun = compute_sales_metrics,
  plot_fun   = plot_sales_metrics,
  city       = "Quito",
  date_from  = "2016-01-01",
  by         = "store_nbr",
  metric     = "growth_rate"
)
print(res_quito)
if (!is.null(res_quito$plot)) print(res_quito$plot)

# Mozemy podmienic logike analityczna bez zmiany sales_ts_logic():
res_typeA <- sales_ts_logic(
  sales_clean, type = "A", by = "family", metric = "cv_sales"
)
print(res_typeA$metrics)

# --- 7. Podsumowanie menedzerskie -------------------------------------------
summary_mgmt <- create_management_summary(sales_clean, recent_n = 30)
print(summary_mgmt)

# --- 8. Prognozowanie (ARIMA + Prophet) -------------------------------------
# Prognoza calej sprzedazy na 12 tygodni w przod (dane tygodniowe).
prognosis <- create_prognosis(
  sales_weekly, h = 12, frequency = 52, methods = c("arima", "prophet")
)
print(prognosis$forecast)

p_fc <- plot_prognosis(prognosis)
print(p_fc)

# Porownanie modeli na ostatnim odcinku (jak w materialach o predykcji)
cat("\nModele prognostyczne dopasowane:",
    paste(names(prognosis$models), collapse = ", "), "\n")

cat("\n=== Workflow zakonczony ===\n")
