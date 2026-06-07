test_that("load_sales_data wczytuje i laczy dane", {
  train <- system.file("extdata", "train_sample.csv", package = "biztools")
  stores <- system.file("extdata", "stores.csv", package = "biztools")
  skip_if(train == "", "Brak danych przykladowych")

  d <- load_sales_data(train, stores_path = stores, n_max = 5000)
  expect_s3_class(d, "tbl_df")
  expect_true(all(c("date", "store_nbr", "family", "sales", "onpromotion") %in% names(d)))
  expect_true("city" %in% names(d))           # metadane dolaczone
  expect_s3_class(d$date, "Date")
})

test_that("validate_sales_ts wykrywa braki, ujemne i duplikaty", {
  df <- data.frame(
    date = as.Date("2023-01-01") + c(0, 0, 1, 2),
    store_nbr = 1L, family = "A",
    sales = c(10, 5, NA, -3), onpromotion = 0L
  )
  v <- validate_sales_ts(df, verbose = FALSE)
  expect_equal(v$n_missing_value, 1)
  expect_equal(v$n_negative, 1)
  expect_equal(v$n_duplicates, 1)
  expect_false(v$passed)
})

test_that("clean_sales_ts scala duplikaty i obsluguje braki", {
  df <- data.frame(
    date = as.Date("2023-01-01") + c(0, 0, 1, 3),
    store_nbr = 1L, family = "A",
    sales = c(10, 5, NA, 20), onpromotion = c(1L, 0L, 0L, 2L)
  )
  out <- clean_sales_ts(df, na_strategy = "zero")
  # duplikat 2023-01-01 scalony do jednego wiersza (suma 15)
  expect_equal(sum(out$date == as.Date("2023-01-01")), 1)
  expect_equal(out$sales[out$date == as.Date("2023-01-01")], 15)
  expect_false(any(is.na(out$sales)))
})

test_that("compute_sales_metrics zwraca poprawne metryki", {
  df <- data.frame(
    date = as.Date("2023-01-01") + 0:9,
    store_nbr = 1L, family = "A",
    sales = c(10, 12, 30, 11, 9, 20, 22, 50, 21, 19),
    onpromotion = 0L
  )
  m <- compute_sales_metrics(df, ma_window = 3)
  expect_equal(m$total_sales, sum(df$sales))
  expect_equal(m$n_periods, 10)
  expect_true(is.finite(m$cv_sales))
})

test_that("compute_sales_metrics dziala per grupa", {
  df <- data.frame(
    date = rep(as.Date("2023-01-01") + 0:4, 2),
    store_nbr = rep(1:2, each = 5), family = "A",
    sales = c(10, 12, 30, 11, 9, 20, 22, 50, 21, 19), onpromotion = 0L
  )
  m <- compute_sales_metrics(df, by = "store_nbr")
  expect_equal(nrow(m), 2)
  expect_true("store_nbr" %in% names(m))
})

test_that("sales_ts_logic jest funkcja wyzszego rzedu", {
  df <- data.frame(
    date = rep(as.Date("2023-01-01") + 0:9, 2),
    store_nbr = rep(1:2, each = 10), family = "A",
    city = rep(c("Quito", "Guayaquil"), each = 10),
    type = "A",
    sales = runif(20, 5, 50), onpromotion = 0L
  )
  res <- sales_ts_logic(df, city = "Quito", by = "store_nbr",
                        plot_fun = NULL)
  expect_s3_class(res, "sales_ts_logic_result")
  expect_equal(res$n_rows, 10)              # tylko Quito
  expect_true(is.data.frame(res$metrics))
})

test_that("create_management_summary zwraca kluczowe pola", {
  set.seed(1)
  df <- data.frame(
    date = rep(as.Date("2023-01-01") + 0:59, 2),
    store_nbr = rep(1:2, each = 60),
    family = rep(c("A", "B"), 60),
    sales = c(runif(60, 10, 20), runif(60, 30, 60)), onpromotion = 0L
  )
  s <- create_management_summary(df, recent_n = 15)
  expect_s3_class(s, "sales_summary")
  expect_true(!is.na(s$best_store))
  expect_true(s$total_sales > 0)
})
