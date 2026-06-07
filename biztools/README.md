# biztools

Pakiet R do analizy szeregów czasowych sprzedaży detalicznej. Zaprojektowany dla
wewnętrznego zespołu analitycznego firmy handlowej. Obsługuje pełny przepływ
pracy: wczytanie danych, walidację, czyszczenie, obliczanie metryk, wizualizację,
raportowanie i prognozowanie.

Projekt zaliczeniowy z analizy szeregów czasowych w R — Paulina Cieślak,
Aleksandra Rams, Katarzyna Szkiłądź.

## Instalacja

```r
# Z GitHub
devtools::install_github("kszkiladz/r_II_semestr_projekt_Cieslak_Rams_Szkiladz")

# Lub lokalnie z folderu pakietu (podczas rozwoju)
devtools::load_all(".")   # szybkie testy bez instalacji
devtools::install(".")    # instalacja jak u użytkownika
```

Pakiety wymagane (`Imports`): `readr`, `dplyr`, `tidyr`, `lubridate`, `ggplot2`,
`rlang`, `tibble`, `zoo`. Prognozowanie korzysta z pakietów opcjonalnych
(`Suggests`): `forecast` (ARIMA, ETS) oraz `prophet`. Jeśli `prophet` nie jest
zainstalowany, `create_prognosis()` automatycznie używa metody awaryjnej **ETS**.

```r
install.packages(c("forecast", "prophet"))
```

## Dane

Zbiór z konkursu Kaggle
[Store Sales – Time Series Forecasting](https://www.kaggle.com/competitions/store-sales-time-series-forecasting).
Wymagane pliki: `train.csv`, `stores.csv`, `holidays_events.csv`. Umieść je w
katalogu roboczym. Plik `train.csv` ma ok. 3 mln wierszy — przy testach warto
ograniczyć liczbę sklepów argumentem `stores`.

## Quick Start

```r
library(biztools)

sales <- load_sales_data(
  "train.csv",
  "stores.csv",
  "holidays_events.csv",
  stores = c(1, 3, 8, 44, 45, 47)   # podzbiór dla szybkości; NULL = wszystkie
)

validate_sales_ts(sales)

clean <- clean_sales_ts(sales, na_strategy = "zero", aggregate = "month")

metrics <- compute_sales_metrics(clean, by = "store_nbr")

plot_sales_trends(clean, color_by = "store_nbr", smooth = TRUE)

summary <- create_management_summary(clean)

forecast <- create_prognosis(clean, h = 12, frequency = 12)
plot_prognosis(forecast)
```

## Główne funkcje

| Funkcja | Rola | Poziom |
|---|---|---|
| `load_sales_data()` | wczytanie i połączenie danych (tidyverse) | 1 |
| `validate_sales_ts()` | kontrola jakości (braki, duplikaty, daty, zakres, częstotliwość) | 1 |
| `clean_sales_ts()` | czyszczenie (braki, duplikaty, sortowanie, agregacja czasowa) | 1 |
| `compute_sales_metrics()` | metryki biznesowe (suma, średnia, MA, zmienność, promocje, odstęp szczytów) | 1 |
| `plot_sales_trends()` / `plot_sales_metrics()` | wizualizacje | 2 |
| `create_management_summary()` | podsumowanie menedżerskie | 2 |
| `sales_ts_logic()` | funkcja wyższego rzędu (metryki/wykresy na podzbiorach) | 3 |
| `create_prognosis()` / `plot_prognosis()` | prognoza ARIMA + Prophet (z ETS jako fallback) | 3 |

## Zaawansowane użycie

### Funkcja wyższego rzędu

`sales_ts_logic()` przyjmuje inne funkcje jako argumenty i stosuje je do
podzbioru danych wybranego po metadanych sklepu (miasto, stan, typ) i czasie:

```r
# Metryki + wykres dla sklepów w Quito od 2016 r.
res <- sales_ts_logic(
  clean,
  metric_fun = compute_sales_metrics,
  plot_fun   = plot_sales_metrics,
  city       = "Quito",
  date_from  = "2016-01-01",
  by         = "store_nbr",
  metric     = "growth_rate"
)
res$metrics
res$plot
```

### Metody prognozowania

Pakiet obsługuje:

* **ARIMA** (`forecast::auto.arima`)
* **Prophet** (opcjonalnie, pakiet `prophet`)
* **ETS** (metoda awaryjna, gdy `prophet` nie jest dostępny)

## Pełny workflow i raport

* `scripts/full_workflow.R` — jeden skrypt wykonujący cały przepływ pracy.
* `vignettes/raport_sprzedazy.Rmd` — raport (Poziom 3): trendy, skuteczność
  promocji, porównanie sklepów/kategorii oraz prognozowanie.

```r
rmarkdown::render("vignettes/raport_sprzedazy.Rmd")
```

## Kontrola jakości pakietu

```r
devtools::document()   # generuje man/ i NAMESPACE z komentarzy roxygen2
devtools::check()      # pełna kontrola (Errors / Warnings / Notes)
devtools::test()       # testy jednostkowe
```

## Autorzy

Paulina Cieślak, Aleksandra Rams, Katarzyna Szkiłądź — projekt uczelniany
z analizy szeregów czasowych w R.
