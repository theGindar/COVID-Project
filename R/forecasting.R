install.packages("prophet")
install.packages("forecast")
install.packages("slider")
library(prophet)
library(lubridate)
library(forecast)

source("R/filter_functions.R")

cov_data <- read.csv("R/RKI_COVID19.csv")
cov_data %>%
  arrange(desc(Meldedatum))
data_df <- get_infections_per_federal_states(cov_data, federal_state = "Bayern", date_start = "2019/01/01 00:00:00+00", date_end = "2021/06/01 00:00:00+00")
ggplot(data=data_df, aes(x=Meldedatum, y=Deaths, group=1)) +
  geom_line()

data_df %>%
  ungroup() %>%
  select(date = Meldedatum, value = Infections) %>%
  mutate(date = gsub('.{12}$', '', date)) %>%
  mutate(date = ymd(date)) -> data_df

data_df %>%
  mutate(ds = date, y = value) %>%
  select(ds, y) -> data_df

data_df %>%
  mutate(y = slider::slide_dbl(y, mean, .before = 3, .after = 3)) -> data_df
data_df

#data_df <- column_to_rownames(data_df, var = "date")
data_df

#lam <- BoxCox.lambda(data_df$value, method = "loglik")
#data_df$y <- BoxCox(data_df$value, lam)

prophet_model <- prophet(data_df, yearly.seasonality = TRUE)
future <- make_future_dataframe(prophet_model, periods = 730)
forecast <- predict(prophet_model, future)
plot(prophet_model, forecast)
forecast
future
data_df

prophet_plot_components(prophet_model, forecast)
data_df
