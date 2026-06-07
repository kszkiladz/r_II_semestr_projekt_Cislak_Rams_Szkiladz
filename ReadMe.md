# biztools

R package for analysis of retail sales time series data.
Designed for internal analytics teams in retail companies.

The package supports the full workflow:
data loading, validation, cleaning, feature engineering, visualization, reporting, and forecasting.

---

## Installation

```r
# install from GitHub
devtools::install_github("your_username/biztools")
```

---

## Data

The package uses the Kaggle dataset:

Store Sales - Time Series Forecasting

Download files from:
https://www.kaggle.com/competitions/store-sales-time-series-forecasting

Required files:

* train.csv
* stores.csv
* holidays_events.csv

Place them in your working directory.

---

## Quick Start

```r
library(biztools)

sales <- load_sales_data(
  "train.csv",
  "stores.csv",
  "holidays_events.csv"
)

validate_sales_ts(sales)

clean <- clean_sales_ts(sales, aggregate = "month")

metrics <- compute_sales_metrics(clean)

plot_sales_trends(clean)

summary <- create_management_summary(clean)

forecast <- create_prognosis(clean, h = 12)
```

---

## Main Functions

### Data loading

* load_sales_data()

### Validation

* validate_sales_ts()

### Cleaning

* clean_sales_ts()

### Metrics

* compute_sales_metrics()

### Visualization

* plot_sales_trends()

### Business summary

* create_management_summary()

### Forecasting

* create_prognosis()

---

## Advanced usage

### Group analysis

```r
sales_ts_logic(sales, compute_sales_metrics, by = "store_nbr")
```

---

## Forecasting methods

The package supports:

* ARIMA
* Prophet (optional)
* ETS fallback

---

## Author

Created as a university project for time series analysis in R by Paulina Ciślak, Aleksandra Rams, and Katarzyna Szkiłądź.
