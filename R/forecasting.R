#install.packages("prophet")
#install.packages("forecast")
#install.packages("slider")
library(prophet)
library(lubridate)
library(forecast)
library(stringr)

source("R/filter_functions.R")

cov_data <- read.csv("R/RKI_COVID19.csv")
cov_data %>%
  arrange(desc(Meldedatum))
test_data <- get_infections_per_federal_states(cov_data, federal_state = c("Bayern", "Berlin"), date_start = "2020/01/01", date_end = "2021/06/01")



attr(test_data, "flag") <- "f_inf"
predict_future_progress <- function(data, future_days = 730) {

  selected_value <- c()
  if(str_detect(attr(data, "flag"), "^[:alpha:]_deaths")) {
    selected_value <- "Deaths"
  } else if (str_detect(attr(data, "flag"), "^[:alpha:]_inf")) {
    selected_value <- "Infections"
  } else if ((str_detect(attr(data, "flag"), "^[:alpha:]_rec"))) {
    selected_value <- "Recovered"
  } else stop("invalid flag for provided data")

  data %>%
    ungroup() %>%
    select(date = Meldedatum, value = selected_value) %>%
    mutate(date = gsub('.{12}$', '', date)) %>%
    mutate(date = ymd(date)) -> data_df

  # if there is not only one federal state or district sum up the values
  if(str_detect(attr(data, "flag"), "^f") & length(distinct(test_data, Bundesland)) > 1) {
    data %>%
      group_by(Meldedatum) %>%
      summarise(selected_value = sum(selected_value))
  }

  data_df %>%
    mutate(ds = date, y = value) %>%
    select(ds, y) -> data_df

  data_df %>%
    mutate(y = slider::slide_dbl(y, mean, .before = 3, .after = 3)) -> data_df

  prophet_model <- prophet(data_df, yearly.seasonality = TRUE)
  future <- make_future_dataframe(prophet_model, periods = 730)
  forecast <- predict(prophet_model, future)

  title_text <- paste("Forecast of", selected_value)
  y_text <- paste("Number of", selected_value)
  x_text <- "Date"

  plot(prophet_model, forecast, xlab=x_text, ylab=y_text)

  #prophet_plot_components(prophet_model, forecast)
}

predict_future_progress(test_data)


length(distinct(test_data, Bundesland))
test_data %>%
  group_by(Meldedatum) %>%
  summarise(Infections = sum(Infections))

selected_value = "Infections"
test_data %>%
  group_by(Meldedatum) %>%
  summarise(!!selected_value := sum(!!(selected_value)))
