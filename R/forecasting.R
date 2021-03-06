#' Nutzt Prophet um Vorhersagen zu zukünftigen Entwicklungen (z. B. Infektionen) zu machen
#'
#' @param data Data frame mit Daten zu Todesfällen, Infektionsfällen, oder Genesenenfällen
#' @param future_days Anzahl Tage, die in die Zukunft vorhergesagt werden soll
#' @return NULL, bzw. Plot mit Vorhersage
#'
#' @examples
#' predict_future_progress(df)
#' @export
predict_future_progress <- function(data, future_days = 730) {

  selected_value <- c()
  if(str_detect(attr(data, "flag"), "^[:alpha:]_deaths")) {
    selected_value <- "Deaths"
  } else if (str_detect(attr(data, "flag"), "^[:alpha:]_inf")) {
    selected_value <- "Infections"
  } else if ((str_detect(attr(data, "flag"), "^[:alpha:]_rec"))) {
    selected_value <- "Recovered"
  } else stop("invalid flag for provided data")

  flag <- attr(data, "flag")
  data_df <- ungroup(data)

  # if there is not only one federal state or district sum up the values
  if(str_detect(flag, "^f") & "Bundesland" %in% colnames(data_df)) {
    if(length(distinct(data_df, Bundesland)$Bundesland) > 1) {
      data_df %>%
        group_by(Meldedatum) %>%
        summarise(!!selected_value := sum(!!parse_expr(selected_value))) -> data_df
    }
  }

  if(str_detect(flag, "^d") & "Landkreis" %in% colnames(data_df)) {
    if(length(distinct(data_df, Landkreis)$Landkreis) > 1) {

      data_df %>%
        group_by(Meldedatum) %>%
        summarise(!!selected_value := sum(!!parse_expr(selected_value))) -> data_df
    }
  }

  data_df %>%
    ungroup() %>%
    select(date = Meldedatum, y = selected_value) %>%
    mutate(date = gsub('.{9}$', '', date)) %>%
    mutate(date = ymd(date)) -> data_df

  data_df %>%
    mutate(ds = date) %>%
    select(ds, y) -> data_df

  data_df %>%
    mutate(y = slider::slide_dbl(y, mean, .before = 3, .after = 3)) -> data_df

  prophet_model <- prophet(data_df, yearly.seasonality = TRUE)
  future <- make_future_dataframe(prophet_model, periods = future_days)
  forecast <- predict(prophet_model, future)

  title_text <- paste("Forecast of", selected_value)
  y_text <- paste("Number of", selected_value)
  x_text <- "Date"

  plot(prophet_model, forecast, xlab=x_text, ylab=y_text)

  #prophet_plot_components(prophet_model, forecast)
}
