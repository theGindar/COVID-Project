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
data_df <- get_infections_per_federal_states(cov_data, federal_state = "Bayern", date_start = "2020/01/01 00:00:00+00", date_end = "2021/06/01 00:00:00+00")

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

  data_df %>%
    mutate(ds = date, y = value) %>%
    select(ds, y) -> data_df

  data_df %>%
    mutate(y = slider::slide_dbl(y, mean, .before = 3, .after = 3)) -> data_df

  prophet_model <- prophet(data_df, yearly.seasonality = TRUE)
  future <- make_future_dataframe(prophet_model, periods = 730)
  forecast <- predict(prophet_model, future)
  plot(prophet_model, forecast)
  forecast
  future
  data_df

  prophet_plot_components(prophet_model, forecast)
  data_df
}
ggplot(data=data_df, aes(x=Meldedatum, y=Deaths, group=1)) +
  geom_line()

str_detect("f_dasdfkajösd", "^[:alpha:]_d")
