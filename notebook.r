
dir("datasets")

library(testthat) 
library(IRkernel.testthat)
`%$%` <- magrittr::`%$%`

soln_sorghum_median_price_by_date <- readr::read_csv("datasets/Sorghum.csv") %>% 
  dplyr::mutate(date = lubridate::ymd(paste(mp_year, mp_month, "01", sep = "-"))) %>% 
  dplyr::group_by(date) %>% 
  dplyr::summarize(median_price_rwf = median(mp_price))


soln_cassava_median_price_by_date <- readr::read_csv("datasets/Cassava.csv") %>% 
  dplyr::mutate(date = lubridate::ymd(paste(mp_year, mp_month, "01", sep = "-"))) %>% 
  dplyr::group_by(date) %>% 
  dplyr::summarize(median_price_rwf = median(mp_price))


soln_potatoes_median_price_by_date <- readr::read_csv("datasets/Potatoes (Irish).csv") %>% 
  dplyr::mutate(date = lubridate::ymd(paste(mp_year, mp_month, "01", sep = "-"))) %>% 
  dplyr::group_by(date) %>% 
  dplyr::summarize(median_price_rwf = median(mp_price))

soln_get_median_price_by_date <- function(filename) {
  filename %>%
    readr::read_csv(col_types = readr::cols()) %>% 
    dplyr::mutate(date = lubridate::ymd(paste(mp_year, mp_month, "01", sep = "-"))) %>% 
    dplyr::group_by(date) %>% 
    dplyr::summarize(median_price_rwf = median(mp_price))
}

soln_forecast_price <- function(median_price_by_date) {
  commodity_ts <- median_price_by_date %$% 
    ts(
      median_price_rwf, 
      start = c(lubridate::year(min(date)), lubridate::month(min(date))), 
      end = c(lubridate::year(max(date)), lubridate::month(max(date))),
      frequency = 12
    )

  forecast::forecast(commodity_ts)
}

run_tests({       
  test_that("TASK1: get_median_price_by_date exists", {
    expect_true(
      exists("get_median_price_by_date"), 
      label = "`get_median_price_by_date()` has not been defined."
    )  
  })
 
  test_that("TASK1: get_median_price_by_date is a function", {
    expect_type(
      get_median_price_by_date, 
      type = "closure"
    )  
  })
    
  test_that("TASK1: get_median_price_by_date returns a data frame", {
    expect_s3_class(
      get_median_price_by_date("datasets/Sorghum.csv"), 
      class = "data.frame"
    )  
  })
    
  test_that("TASK1: get_median_price_by_date works with sorghum", {
    expect_equal(
      get_median_price_by_date("datasets/Sorghum.csv"),
      soln_get_median_price_by_date("datasets/Sorghum.csv"),
      label = "`get_median_price_by_date()` does not return the correct answer for the Sorghum CSV file."
    )
  })
    
  test_that("TASK1: get_median_price_by_date works with cassava", {
    expect_equal(
      get_median_price_by_date("datasets/Cassava.csv"),
      soln_get_median_price_by_date("datasets/Cassava.csv"),
      label = "`get_median_price_by_date()` does not return the correct answer for the Cassava CSV file."
    )
  })
   
  test_that("TASK1: get_median_price_by_date works with potatoes", {
    expect_equal(
      get_median_price_by_date("datasets/Potatoes (Irish).csv"),
      soln_get_median_price_by_date("datasets/Potatoes (Irish).csv"),
      label = "`get_median_price_by_date()` does not return the correct answer for the Irish Potatoes CSV file."
    )
  })

# ----
   
  test_that("TASK2: forecast_price exists", {
    expect_true(
      exists("forecast_price"), 
      label = "`forecast_price()` has not been defined."
    )  
  })
 
  test_that("TASK2: forecast_price is a function", {
    expect_type(
      forecast_price, 
      type = "closure"
    )  
  })
    
  test_that("TASK2: forecast_price is a function", {
    expect_s3_class(
      forecast_price(soln_sorghum_median_price_by_date), 
      class = "forecast"
    )  
  })
    
  test_that("TASK2: forecast_price works with sorghum", {
    expect_equal(
      forecast_price(soln_sorghum_median_price_by_date),
      soln_forecast_price(soln_sorghum_median_price_by_date),
      label = "`forecast_price()` does not return the correct answer for the Sorghum CSV file."
    )
  })
    
  test_that("TASK2: forecast_price works with cassava", {
    expect_equal(
      forecast_price(soln_cassava_median_price_by_date),
      soln_forecast_price(soln_cassava_median_price_by_date),
      label = "`forecast_price()` does not return the correct answer for the Cassava CSV file."
    )
  })
   
  test_that("TASK2: forecast_price works with potatoes", {
    expect_equal(
      forecast_price(soln_potatoes_median_price_by_date),
      soln_forecast_price(soln_potatoes_median_price_by_date),
      label = "`forecast_price()` does not return the correct answer for the Irish Potatoes CSV file."
    )
  })    
})

library(readr)
library(data.table)
library(lubridate)
library(ggplot2)
library(dplyr)
library(forecast)

sorghum <- read_csv("datasets/Sorghum.csv")
glimpse(sorghum)

sorghum <- sorghum %>%
    mutate(date = ymd(paste(mp_year, mp_month, "01")))
glimpse(sorghum)

ggplot(sorghum, aes(date, mp_price, group = mkt_id)) + geom_line(alpha = 0.25) + theme_bw()

sorghum_median_price_by_date <- sorghum %>%
    group_by(date) %>%
    summarize(median_price_rwf = median(mp_price))

first_date <- min(sorghum_median_price_by_date$date)
last_date <- max(sorghum_median_price_by_date$date)

sorghum_ts <- ts(
    sorghum_median_price_by_date$median_price_rwf,
    start = c(year(first_date), month(first_date)),
    end =  c(year(last_date), month(last_date)),
    frequency = 12
)

forecast(sorghum_ts)

get_median_price_by_date <- function(filepath) {
    commodity <- read_csv(filepath, col_types = cols())
    commodity <- commodity %>%
        mutate(date = ymd(paste(mp_year, mp_month, "01")))
    commodity %>%
        group_by(date) %>%
        summarize(median_price_rwf = median(mp_price), .groups = "drop_last")
}

sorghum_median_price_by_date <- get_median_price_by_date("datasets/Sorghum.csv")

Cassava_median_price_by_date <- get_median_price_by_date("datasets/Cassava.csv")

potatoes_median_price_by_date <- get_median_price_by_date("datasets/Potatoes (Irish).csv")

forecast_price <- function(commodity_median_price_by_date) {
    first_date <- min(commodity_median_price_by_date$date)
    last_date <- max(commodity_median_price_by_date$date)

commodity_ts <- ts(
    commodity_median_price_by_date$median_price_rwf,
    start = c(year(first_date), month(first_date)),
    end =  c(year(last_date), month(last_date)),
    frequency = 12
)
    forecast(commodity_ts)
}

forecast_price(sorghum_median_price_by_date)

forecast_price(Cassava_median_price_by_date)

forecast_price(potatoes_median_price_by_date)

sorghum <- fread("datasets/Sorghum.csv")
glimpse(sorghum)

sorghum[j = date := ymd(paste(mp_year, mp_month, "01"))]
glimpse(sorghum)

sorghum[j = .(median_price_rwf = median(mp_price)), by = date]


