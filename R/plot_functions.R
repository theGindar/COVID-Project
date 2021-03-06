#' Erzeugt, bzw. Speichert Plot aus data frame. Kann als Barchart dargestellt werden, oder als Linien (für Zeitreihen)
#'
#' @param data Data frame mit Daten aus get_... Methoden. Sollte ein beschreibendes "flag" als Attribut enthalten.
#' @param add_weather Boolean. Gibt an, ob zusätzlich bei einer Zeitreihe die Durschnittstemperatur des entsprechenden Datums geplottet werden soll.
#' @param scaling_coeff Zahl, um die Skalierung der beiden Achsen bei Wetter-Plots anzupassen.
#' @param save_in Pfad zu Verzeichnis, in welchem der Plot gespeichert werden soll. Wenn save_in NA ist, wird der Plot direkt angezeigt
#' @return NULL, bzw. Plot
#'
#' @examples
#' plot_function(df, add_weather=TRUE, save_in="some/path")
#' @export
plot_function <- function(data, add_weather=FALSE, scaling_coeff = 700, save_in = NA){
  save_plot <- !is.na(save_in)

  if(add_weather){
    data <- add_weather_data(data)
  }

  add_weather_line <- function(cov_plot, axis_1) {
    return(cov_plot +
             stat_smooth(aes(x = as.Date(Meldedatum), y = data$Temperatur), geom="area", alpha=.4, color="brown1", fill="brown1") +
             scale_y_continuous(

               name = "Durchschnittl. Temperatur in [°C]",

               sec.axis = sec_axis(~.*scaling_coeff, name = axis_1),

               expand = c(0, 0))

             )
  }

  finish_plotting <- function(plot_obj){
    if(save_plot) ggsave(paste0(attr(data, "flag"), ".png"),
                         plot = plot_obj, width = 10, height = 10, path=save_in)
    else plot_obj
  }
  y_val <- c()

  if(str_detect(attr(data, "flag") , "^[df]_inf")) {
    if(add_weather) y_val <- data$Infections / scaling_coeff
    else y_val <- data$Infections
  } else if(str_detect(attr(data, "flag") , "^[df]_deaths")) {
    if(add_weather) y_val <- data$Deaths / scaling_coeff
    else y_val <- data$Deaths
  } else if(str_detect(attr(data, "flag") , "^[df]_rec")) {
    if(add_weather) y_val <- data$Recovered / scaling_coeff
    else y_val <- data$Recovered
  } else if(str_detect(attr(data, "flag") , "^DE_F")) {
    if(add_weather) y_val <- data$Fallsterblichkeit / scaling_coeff
    else y_val <- data$Fallsterblichkeit
  }


  switch(attr(data, "flag"),
         "DE_Fallsterblichkeit" = {
           cov_plot <- ggplot(data) + geom_line(aes(x = as.Date(Meldedatum), y = y_val)) +
             xlab("Meldedatum") +
             stat_smooth(aes(x = as.Date(Meldedatum), y = Fallsterblichkeit), method = "loess", se = FALSE)

           if(add_weather) {
             cov_plot <- add_weather_line(cov_plot, axis_1 = "Tote")
           } else cov_plot <- cov_plot + scale_y_continuous(name = "Fallsterblichkeit", expand = c(0, 0))

           finish_plotting(cov_plot)
         },
         "DE_Fallsterblichkeit_age" = {
           cov_plot <- ggplot(data) + geom_line(aes(x = as.Date(Meldedatum), y = y_val, group = Altersgruppe, color = Altersgruppe)) +
             xlab("Meldedatum") +
             stat_smooth(aes(x = as.Date(Meldedatum), y = Fallsterblichkeit, group = Altersgruppe, color = Altersgruppe), method = "loess", se = FALSE)

           if(add_weather) {
             cov_plot <- add_weather_line(cov_plot, axis_1 = "Tote")
           } else cov_plot <- cov_plot + scale_y_continuous(name = "Fallsterblichkeit", expand = c(0, 0))

           finish_plotting(cov_plot)
         },
         "f_deaths_Age-Datum" = {
           cov_plot <- ggplot(data) + geom_line(aes(x = as.Date(Meldedatum), y = y_val, group = Altersgruppe, color = Altersgruppe)) +
           xlab("Meldedatum") +
           stat_smooth(aes(x = as.Date(Meldedatum), y = Deaths, group = Altersgruppe, color = Altersgruppe), method = "loess", se = FALSE)

           if(add_weather) {
             cov_plot <- add_weather_line(cov_plot, axis_1 = "Deaths")
           } else cov_plot <- cov_plot + scale_y_continuous(name = "Tode", expand = c(0, 0))

           finish_plotting(cov_plot)
           },
         "f_deaths_Bundesland-Datum" = {
           cov_plot <- ggplot(data) + geom_line(aes(x = as.Date(Meldedatum), y = y_val, group = Bundesland, color = Bundesland)) +
           xlab("Meldedatum") +
           geom_line(aes(x = as.Date(Meldedatum), y = Deaths, group = Bundesland, color = Bundesland)) +
           stat_smooth(aes(x = as.Date(Meldedatum), y = Deaths, group = Bundesland, color = Bundesland), method = "loess", se = FALSE)

           if(add_weather) {
             cov_plot <- add_weather_line(cov_plot, axis_1 = "Tode")
           } else cov_plot <- cov_plot + scale_y_continuous(expand = c(0, 0))

           finish_plotting(cov_plot)
           },
         "f_deaths_Bundesland" = {
           cov_plot <- ggplot(data, aes(x = Bundesland, y = Deaths, color = Bundesland)) +
           geom_bar(stat= "identity", aes(fill = Bundesland), position = "dodge") +
           geom_text(aes(label = Deaths),vjust = -0.3, color = "black", size = 3.5)

           finish_plotting(cov_plot)
           },
         "f_deaths_Bundesland-Age" = {
           cov_plot <- ggplot(data, aes(x = Altersgruppe, y = Deaths, color = Bundesland)) +
           geom_bar(stat= "identity", aes(fill = Bundesland), position = "dodge") +
           geom_text(aes(label = Deaths),vjust = -0.3, color = "black", size = 3.5)

           finish_plotting(cov_plot)
           },
         "f_deaths_Datum" = {
           cov_plot <- ggplot(data) + geom_line(aes(x = as.Date(Meldedatum), y = y_val)) +
           xlab("Meldedatum") +
           stat_smooth(aes(x = as.Date(Meldedatum), y = Deaths), method = "loess", se = FALSE)

           if(add_weather) {
             cov_plot <- add_weather_line(cov_plot, axis_1 = "Tode")
           } else cov_plot <- cov_plot + scale_y_continuous(name = "Tode", expand = c(0, 0))

           finish_plotting(cov_plot)
           },
         "f_deaths_Age" = {
           cov_plot <- ggplot(data, aes(x = Altersgruppe, y = Deaths, color = Bundesland)) +
           geom_bar(stat= "identity", aes(fill = Bundesland), position = "dodge")

           finish_plotting(cov_plot)
           },
         "f_deaths_Bundesland-Age-Datum" = "Datum Alter Bundesland",

         "d_deaths_Age-Datum" = {
           cov_plot <- ggplot(data) + geom_line(aes(x = as.Date(Meldedatum), y = y_val, group = Altersgruppe, color = Altersgruppe)) +
           xlab("Meldedatum") +
           stat_smooth(aes(x = as.Date(Meldedatum), y = Deaths, group = Altersgruppe, color = Altersgruppe), method = "loess", se = FALSE)

           if(add_weather) {
             cov_plot <- add_weather_line(cov_plot, axis_1 = "Tode")
           } else cov_plot <- cov_plot + scale_y_continuous(name = "Tode", expand = c(0, 0))

           finish_plotting(cov_plot)
           },
         "d_deaths_Landkreis-Datum" = {
           cov_plot <- ggplot(data) + geom_line(aes(x = as.Date(Meldedatum), y = y_val, group = Landkreis, color = Landkreis)) +
           xlab("Meldedatum") +
           stat_smooth(aes(x = as.Date(Meldedatum), y = Deaths, group = Landkreis, color = Landkreis), method = "loess", se = FALSE)

           if(add_weather) {
             cov_plot <- add_weather_line(cov_plot, axis_1 = "Tode")
           } else cov_plot <- cov_plot + scale_y_continuous(name = "Tode", expand = c(0, 0))

           finish_plotting(cov_plot)
           },
         "d_deaths_Landkreis" = {
           cov_plot <- ggplot(data, aes(x = Landkreis, y = Deaths, color = Landkreis)) +
           geom_bar(stat= "identity", aes(fill = Landkreis), position = "dodge") +
           geom_text(aes(label = Deaths),vjust = -0.3, color = "black", size = 3.5)
           },
         "d_deaths_Landkreis-Age" = {
           cov_plot <- ggplot(data, aes(x = Altersgruppe, y = Deaths, color = Landkreis)) +
           geom_bar(stat= "identity", aes(fill = Landkreis), position = "dodge") +
           geom_text(aes(label = Deaths),vjust = -0.3, color = "black", size = 3.5)

           finish_plotting(cov_plot)
           },
         "d_deaths_Datum" = {
           cov_plot <- ggplot(data) + geom_line(aes(x = as.Date(Meldedatum), y = y_val)) +
           xlab("Meldedatum") +
           stat_smooth(aes(x = as.Date(Meldedatum), y = Deaths), method = "loess", se = FALSE)

           if(add_weather) {
             cov_plot <- add_weather_line(cov_plot, axis_1 = "Tode")
           } else cov_plot <- cov_plot + scale_y_continuous(name = "Tode", expand = c(0, 0))

           finish_plotting(cov_plot)
           },
         "d_deaths_Age" = {
           cov_plot <- ggplot(data, aes(x = Altersgruppe, y = Deaths, color = Landkreis)) +
           geom_bar(stat= "identity", aes(fill = Landkreis), position = "dodge")

           finish_plotting(cov_plot)
           },
         "d_deaths_Landkreis-Age-Datum" = "Datum Alter Landkreis",

         "f_inf_Age-Datum" = {
           cov_plot <- ggplot(data) + geom_line(aes(x = as.Date(Meldedatum), y = y_val, group = Altersgruppe, color = Altersgruppe)) +
           xlab("Meldedatum") +
           stat_smooth(aes(x = as.Date(Meldedatum), y = Infections, group = Altersgruppe, color = Altersgruppe), method = "loess", se = FALSE)

           if(add_weather) {
             cov_plot <- add_weather_line(cov_plot, axis_1 = "Infizierte")
           } else cov_plot <- cov_plot + scale_y_continuous(name = "Infizierte", expand = c(0, 0))

           finish_plotting(cov_plot)
           },
         "f_inf_Bundesland-Datum" = {
           cov_plot <- ggplot(data) + geom_line(aes(x = as.Date(Meldedatum), y = y_val, group = Bundesland, color = Bundesland)) +
           xlab("Meldedatum") +
           stat_smooth(aes(x = as.Date(Meldedatum), y = y_val, group = Bundesland, color = Bundesland), method = "loess", se = FALSE)

           if(add_weather) {
             cov_plot <- add_weather_line(cov_plot, axis_1 = "Infizierte")
           } else cov_plot <- cov_plot + scale_y_continuous(name = "Infizierte", expand = c(0, 0))

           finish_plotting(cov_plot)
           },
         "f_inf_Bundesland" = {
           cov_plot <- ggplot(data, aes(x = Bundesland, y = Infections, color = Bundesland)) +
           geom_bar(stat= "identity", aes(fill = Bundesland), position = "dodge") +
           geom_text(aes(label = Infections),vjust = -0.3, color = "black", size = 3.5)

           finish_plotting(cov_plot)
           },
         "f_inf_Bundesland-Age" = {
           cov_plot <- ggplot(data, aes(x = Altersgruppe, y = Infections, color = Bundesland)) +
           geom_bar(stat= "identity", aes(fill = Bundesland), position = "dodge") +
           geom_text(aes(label = Infections),vjust = -0.3, color = "black", size = 3.5)

           finish_plotting(cov_plot)
           },
         "f_inf_Datum" = {
           cov_plot <- ggplot(data) + geom_line(aes(x = as.Date(Meldedatum), y = y_val)) +
           xlab("Meldedatum") +
           stat_smooth(aes(x=as.Date(Meldedatum), y=y_val), method = "loess", se = FALSE)

           if(add_weather) {
             cov_plot <- add_weather_line(cov_plot, axis_1 = "Infizierte")
           } else cov_plot <- cov_plot + scale_y_continuous(name = "Infizierte", expand = c(0, 0))

           finish_plotting(cov_plot)
           },
         "f_inf_Age" = {
           cov_plot <- ggplot(data, aes(x = Altersgruppe, y = Infections, color = Bundesland)) +
           geom_bar(stat= "identity", aes(fill = Bundesland), position = "dodge")

           finish_plotting(cov_plot)
           },
         "f_inf_Bundesland-Age-Datum" = "Datum Alter Bundesland",

         "d_inf_Age-Datum" = {
           cov_plot <- ggplot(data) + geom_line(aes(x = as.Date(Meldedatum), y = y_val, group = Altersgruppe, color = Altersgruppe)) +
           xlab("Meldedatum") +
           stat_smooth(aes(x = as.Date(Meldedatum), y = Infections, group = Altersgruppe, color = Altersgruppe), method = "loess", se = FALSE)

           if(add_weather) {
             cov_plot <- add_weather_line(cov_plot, axis_1 = "Infizierte")
           } else cov_plot <- cov_plot + scale_y_continuous(name = "Infizierte", expand = c(0, 0))

           finish_plotting(cov_plot)
           },
         "d_inf_Landkreis-Datum" = {
           cov_plot <- ggplot(data) + geom_line(aes(x = as.Date(Meldedatum), y = y_val, group = Landkreis, color = Landkreis)) +
           xlab("Meldedatum") +
           stat_smooth(aes(x = as.Date(Meldedatum), y = Deaths, group = Landkreis, color = Landkreis), method = "loess", se = FALSE)

           if(add_weather) {
             cov_plot <- add_weather_line(cov_plot, axis_1 = "Infizierte")
           } else cov_plot <- cov_plot + scale_y_continuous(name = "Infizierte", expand = c(0, 0))

           finish_plotting(cov_plot)
           },
         "d_inf_Landkreis" = {
           cov_plot <- ggplot(data, aes(x = Landkreis, y = Infections, color = Landkreis)) +
           geom_bar(stat= "identity", aes(fill = Landkreis), position = "dodge") +
           geom_text(aes(label = Infections),vjust = -0.3, color = "black", size = 3.5)

           finish_plotting(cov_plot)
           },
         "d_inf_Landkreis-Age" = {
           cov_plot <- ggplot(data, aes(x = Altersgruppe, y = Infections, color = Landkreis)) +
           geom_bar(stat= "identity", aes(fill = Landkreis), position = "dodge") +
           geom_text(aes(label = Infections),vjust = -0.3, color = "black", size = 3.5)

           finish_plotting(cov_plot)
           },
         "d_inf_Datum" = {
           cov_plot <- ggplot(data) + geom_line(aes(x = as.Date(Meldedatum), y = y_val)) +
           xlab("Meldedatum") +
           stat_smooth(aes(x = as.Date(Meldedatum), y = Infections), method = "loess", se = FALSE)

           if(add_weather) {
             cov_plot <- add_weather_line(cov_plot, axis_1 = "Infizierte")
           } else cov_plot <- cov_plot + scale_y_continuous(name = "Infizierte", expand = c(0, 0))

           finish_plotting(cov_plot)
           },
         "d_inf_Age" = {
           cov_plot <- ggplot(data, aes(x = Altersgruppe, y = Infections, color = Landkreis)) +
           geom_bar(stat= "identity", aes(fill = Landkreis), position = "dodge")

           finish_plotting(cov_plot)
           },
         "d_inf_Landkreis-Age-Datum" = "Datum Alter Landkreis",

         "f_rec_Age-Datum" = {
           cov_plot <- ggplot(data) + geom_line(aes(x = as.Date(Meldedatum), y = y_val, group = Altersgruppe, color = Altersgruppe)) +
           xlab("Meldedatum") +
           stat_smooth(aes(x = as.Date(Meldedatum), y = Recovered, group = Altersgruppe, color = Altersgruppe), method = "loess", se = FALSE)

           if(add_weather) {
             cov_plot <- add_weather_line(cov_plot, axis_1 = "Infizierte")
           } else cov_plot <- cov_plot + scale_y_continuous(name = "Infizierte", expand = c(0, 0))

           finish_plotting(cov_plot)
           },
         "f_rec_Bundesland-Datum" = {
           cov_plot <- ggplot(data) + geom_line(aes(x = as.Date(Meldedatum), y = y_val, group = Bundesland, color = Bundesland)) +
           xlab("Meldedatum") +
           stat_smooth(aes(x = as.Date(Meldedatum), y = Deaths, group = Bundesland, color = Bundesland), method = "loess", se = FALSE)

           if(add_weather) {
             cov_plot <- add_weather_line(cov_plot, axis_1 = "Genesene")
           } else cov_plot <- cov_plot + scale_y_continuous(name = "Genesene", expand = c(0, 0))

           finish_plotting(cov_plot)
           },
         "f_rec_Bundesland" = {
           cov_plot <- ggplot(data, aes(x = Bundesland, y = Recovered, color = Bundesland)) +
           geom_bar(stat= "identity", aes(fill = Bundesland), position = "dodge") +
           geom_text(aes(label = Recovered),vjust = -0.3, color = "black", size = 3.5)

           finish_plotting(cov_plot)
           },
         "f_rec_Bundesland-Age" = {
           cov_plot <- ggplot(data, aes(x = Altersgruppe, y = Recovered, color = Bundesland)) +
           geom_bar(stat= "identity", aes(fill = Bundesland), position = "dodge") +
           geom_text(aes(label = Recovered),vjust = -0.3, color = "black", size = 3.5)

           finish_plotting(cov_plot)
           },
         "f_rec_Datum" = {
           cov_plot <- ggplot(data) + geom_line(aes(x = as.Date(Meldedatum), y = y_val)) +
           xlab("Meldedatum") +
           stat_smooth(aes(x = as.Date(Meldedatum), y = Recovered), method = "loess", se = FALSE)

           if(add_weather) {
             cov_plot <- add_weather_line(cov_plot, axis_1 = "Genesene")
           } else cov_plot <- cov_plot + scale_y_continuous(name = "Genesene", expand = c(0, 0))

           finish_plotting(cov_plot)
           },
         "f_rec_Age" = {
           cov_plot <- ggplot(data, aes(x = Altersgruppe, y = Recovered, color = Bundesland)) +
           geom_bar(stat= "identity", aes(fill = Bundesland), position = "dodge")

           finish_plotting(cov_plot)
           },
         "f_rec_Bundesland-Age-Datum" = "Datum Alter Bundesland",

         "d_rec_Age-Datum" = {
           cov_plot <- ggplot(data) + geom_line(aes(x = as.Date(Meldedatum), y = y_val, group = Altersgruppe, color = Altersgruppe)) +
           xlab("Meldedatum") +
           stat_smooth(aes(x = as.Date(Meldedatum), y = Recovered, group = Altersgruppe, color = Altersgruppe), method = "loess", se = FALSE)

           if(add_weather) {
             cov_plot <- add_weather_line(cov_plot, axis_1 = "Genesene")
           } else cov_plot <- cov_plot + scale_y_continuous(name = "Genesene", expand = c(0, 0))

           finish_plotting(cov_plot)
           },
         "d_rec_Landkreis-Datum" = {
           cov_plot <- ggplot(data) + geom_line(aes(x = as.Date(Meldedatum), y = y_val, group = Landkreis, color = Landkreis)) +
           xlab("Meldedatum") +
           stat_smooth(aes(x = as.Date(Meldedatum), y = Deaths, group = Landkreis, color = Landkreis), method = "loess", se = FALSE)

           if(add_weather) {
             cov_plot <- add_weather_line(cov_plot, axis_1 = "Genesene")
           } else cov_plot <- cov_plot + scale_y_continuous(name = "Genesene", expand = c(0, 0))

           finish_plotting(cov_plot)
           },
         "d_rec_Landkreis-Age" = {
           cov_plot <- ggplot(data, aes(x = Landkreis, y = Recovered, color = Landkreis)) +
           geom_bar(stat= "identity", aes(fill = Landkreis), position = "dodge") +
           geom_text(aes(label = Recovered),vjust = -0.3, color = "black", size = 3.5)

           finish_plotting(cov_plot)
           },
         "d_rec_Datum" = {
           cov_plot <- ggplot(data, aes(x = Altersgruppe, y = Recovered, color = Landkreis)) +
           geom_bar(stat= "identity", aes(fill = Landkreis), position = "dodge") +
           geom_text(aes(label = Recovered),vjust = -0.3, color = "black", size = 3.5)
           },
         "d_rec_Datum" = {
           cov_plot <- ggplot(data) + geom_line(aes(x = as.Date(Meldedatum), y = y_val)) +
           xlab("Meldedatum") +
           stat_smooth(aes(x = as.Date(Meldedatum), y = Recovered), method = "loess", se = FALSE)

           if(add_weather) {
             cov_plot <- add_weather_line(cov_plot, axis_1 = "Genesene")
           } else cov_plot <- cov_plot + scale_y_continuous(name = "Genesene", expand = c(0, 0))

           finish_plotting(cov_plot)
           },
         "d_rec_Age" = {
           cov_plot <- ggplot(data, aes(x = Altersgruppe, y = Recovered, color = Landkreis)) +
           geom_bar(stat= "identity", aes(fill = Landkreis), position = "dodge")

           finish_plotting(cov_plot)
           },
         "d_rec_Landkreis-Age-Datum" = "Datum Alter Landkreis")
}

#' Erzeugt, bzw. Speichert Correlationsmatrix-Plot aus data frame.
#'
#' @param correlations_data Data frame mit Daten zu Inzidenz-Correlations-Paaren von Landkreisen.
#' @param districts Vektor aus Strings mit Landkreisnamen, die in der Matrix dargestellt werden sollen (zur Übersichtlichkeit).
#' @param save_in Pfad zu Verzeichnis, in welchem der Plot gespeichert werden soll. Wenn save_in NA ist, wird der Plot direkt angezeigt
#' @param file_name Name der zu speichernden Bilddatei
#' @return NULL, bzw. Plot
#'
#' @examples
#' plot_incidence_correlations_matrix(df, districts = c("LK Kiel", "LK Karlsruhe", "SK Karlsruhe"), save_in = "some/path", file_name = "correlation_matrix_plot")
#' @export
plot_incidence_correlations_matrix <- function(correlations_data, districts = NA, save_in = NA, file_name = NA) {
  save_plot <- !is.na(save_in) & !is.na(file_name)

  suppressWarnings(if(!is.na(districts)) {
    correlations_data %>%
      filter(Landkreis_1 %in% districts) %>%
      filter(Landkreis_2 %in% districts) -> correlations_data
  })

  # add values for upper triangle
  swapped_df <- data.frame(IdLandkreis_1 = correlations_data$IdLandkreis_2,
                           IdLandkreis_2 = correlations_data$IdLandkreis_1,
                           Landkreis_1 = correlations_data$Landkreis_2,
                           Landkreis_2 = correlations_data$Landkreis_1,
                           Correlation = correlations_data$Correlation)
  correlations_data <- rbind(correlations_data, swapped_df)


  # add values for diagonal
  all_lk_names <- distinct(correlations_data, Landkreis_1)$Landkreis_1
  all_lk_names <- unique(c(all_lk_names,
                    distinct(correlations_data, Landkreis_2)$Landkreis_2))

  for(lk in all_lk_names) {
    new_row_df <- data.frame(IdLandkreis_1 = NA,
                     Landkreis_1 = lk,
                     IdLandkreis_2 = NA,
                     Landkreis_2 = lk,
                     Correlation = 1)
    correlations_data <- rbind(correlations_data, new_row_df)
  }

  cov_plot <- ggplot(data = correlations_data, aes(x = Landkreis_1, y = Landkreis_2,
                                       fill = Correlation)) +
    geom_tile()

  if(save_plot) ggsave(paste0(file_name, ".png"),
                                    plot = cov_plot, width = 10, height = 10, path=save_in)
  else cov_plot
}

#' Erzeugt, bzw. Speichert Barchart von höchsten Inzidenz-Correlations-Paaren aus data frame.
#'
#' @param correlations_data Data frame mit Daten zu Inzidenz-Correlations-Paaren von Landkreisen.
#' @param top Anzahl der Paare, die betrachtet werden sollen. "top = 10" bedeutet die 10 Landkreispaare mit der höchsten Correlation zwischen Inzidenzen
#' @param save_in Pfad zu Verzeichnis, in welchem der Plot gespeichert werden soll. Wenn save_in NA ist, wird der Plot direkt angezeigt
#' @param file_name Name der zu speichernden Bilddatei
#' @return NULL, bzw. Plot
#'
#' @examples
#' plot_incidence_correlationsbarchart(df, top = 10, save_in = "some/path", file_name = "correlation_matrix_plot")
#' @export
plot_incidence_correlations_barchart <- function(correlations_data, top = 10, save_in = NA, file_name = NA) {
  save_plot <- !is.na(save_in) & !is.na(file_name)

  correlations_data %>%
    arrange(desc(Correlation)) -> correlations_data

  correlations_data <- head(correlations_data, top)

  correlations_data %>%
    mutate(namings = paste(Landkreis_1, Landkreis_2, sep = " & ")) -> correlations_data

  ylim_min <- min(correlations_data$Correlation)-0.001
  if(ylim_min < 0) ylim_min <- 0
  ylim_max <- max(correlations_data$Correlation)
  cov_plot <- ggplot(correlations_data, aes(x=reorder(namings, -Correlation),
                                y=Correlation)) +
    #scale_fill_brewer(palette = "Set1") +
    theme(axis.text.x = element_text(angle = 70, vjust = 1, hjust=1)) +
    coord_cartesian(ylim=c(ylim_min,ylim_max)) +
    ggtitle(paste0("Top ", as.character(top), " Correlation Pairs of District's Incidences")) +
    xlab("District Pairs") + ylab("Incidence Correlation") +
    geom_bar(stat = "identity", fill="steelblue")

  if(save_plot) ggsave(paste0(file_name, ".png"),
                       plot = cov_plot, width = 10, height = 10, path=save_in)
  else cov_plot
}

#' Erzeugt, bzw. speichert Plot von Karte Deutschlands, welche Landkreise darstellt
#'
#' @param data Data frame mit Daten zu Todesfällen, Infektionen, oder Genesenenzahlen
#' @param save_in Pfad zu Verzeichnis, in welchem der Plot gespeichert werden soll. Wenn save_in NA ist, wird der Plot direkt angezeigt
#' @param file_name Name der zu speichernden Bilddatei
#' @return NULL, bzw. Plot
#'
#' @examples
#' plot_district_map(df, save_in = "some/path", file_name = "map_plot")
#' @export
plot_district_map <- function(data, save_in = NA, file_name = NA) {
  stopifnot("provided dataframe should have a flag attribute" = !is.null(attr(data, "flag")))

  save_plot <- !is.na(save_in) & !is.na(file_name)

  switch(attr(data, "flag"),
         "d_deaths_Landkreis" = {
           data <- ungroup(data)
           data %>%
             select(IdLandkreis, datapoints = Deaths) -> data
           cov_plot <- plot_map(data, plot_title = "Tote pro Landkreis", legend_title = "Anzahl Tote")
         },
         "d_inf_Landkreis" = {
           data <- ungroup(data)
           data %>%
             select(IdLandkreis, datapoints = Infections) -> data
           cov_plot <- plot_map(data, plot_title = "Infizierte pro Landkreis", legend_title = "Anzahl Infizierte")
         },
         "d_rec_Landkreis" = {
           data <- ungroup(data)
           data %>%
             select(IdLandkreis, datapoints = Recovered) -> data
           cov_plot <- plot_map(data, plot_title = "Genesene pro Landkreis", legend_title = "Anzahl Genesene")
         })

  if(save_plot) ggsave(paste0(file_name, ".png"),
                       plot = cov_plot, width = 10, height = 10, path=save_in)
  else cov_plot
}


