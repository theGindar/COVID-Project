library(ggplot2)

plot_function <- function(data){
  switch(attr(data, "flag"), 
         "f_deaths_Age-Datum" = data %>% 
           ggplot(aes(x = as.Date(Meldedatum), y = Deaths, group = Altersgruppe, color = Altersgruppe)) +
           xlab("Meldedatum") +
           geom_line() + 
           stat_smooth(method = "loess", se = FALSE),
         "f_deaths_Bundesland-Datum" = data %>% 
           ggplot(aes(x = as.Date(Meldedatum), y = Deaths, group = Bundesland, color = Bundesland)) +
           xlab("Meldedatum") +
           geom_line() + 
           stat_smooth(method = "loess", se = FALSE),
         "f_deaths_Bundesland" = data %>% 
           ggplot(aes(x = Bundesland, y = Deaths, color = Bundesland)) + 
           geom_bar(stat= "identity", aes(fill = Bundesland), position = "dodge") + 
           geom_text(aes(label = Deaths),vjust = -0.3, color = "black", size = 3.5),
         "f_deaths_Bundesland-Age" = data %>% 
           ggplot(aes(x = Altersgruppe, y = Deaths, color = Bundesland)) + 
           geom_bar(stat= "identity", aes(fill = Bundesland), position = "dodge") + 
           geom_text(aes(label = Deaths),vjust = -0.3, color = "black", size = 3.5),
         "f_deaths_Datum" = data %>% 
           ggplot(aes(x = as.Date(Meldedatum), y = Deaths)) +
           xlab("Meldedatum") +
           geom_line() + 
           stat_smooth(method = "loess", se = FALSE),
         "f_deaths_Age" = data %>% 
           ggplot(aes(x = Altersgruppe, y = Deaths, color = Bundesland)) + 
           geom_bar(stat= "identity", aes(fill = Bundesland), position = "dodge"),
         "f_deaths_Bundesland-Age-Datum" = "Datum Alter Bundesland",
         
         "d_deaths_Age-Datum" = data %>% 
           ggplot(aes(x = as.Date(Meldedatum), y = Deaths, group = Altersgruppe, color = Altersgruppe)) +
           xlab("Meldedatum") +
           geom_line() + 
           stat_smooth(method = "loess", se = FALSE),
         "d_deaths_Landkreis-Datum" = %>% 
           ggplot(aes(x = as.Date(Meldedatum), y = Deaths, group = Landkreis, color = Landkreis)) +
           xlab("Meldedatum") +
           geom_line() + 
           stat_smooth(method = "loess", se = FALSE),
         "d_deaths_Landkreis" = data %>% 
           ggplot(aes(x = Landkreis, y = Deaths, color = Landkreis)) + 
           geom_bar(stat= "identity", aes(fill = Landkreis), position = "dodge") + 
           geom_text(aes(label = Deaths),vjust = -0.3, color = "black", size = 3.5),
         "d_deaths_Landkreis-Age" = data %>% 
           ggplot(aes(x = Altersgruppe, y = Deaths, color = Landkreis)) + 
           geom_bar(stat= "identity", aes(fill = Landkreis), position = "dodge") + 
           geom_text(aes(label = Deaths),vjust = -0.3, color = "black", size = 3.5),
         "d_deaths_Datum" = data %>% 
           ggplot(aes(x = as.Date(Meldedatum), y = Deaths)) +
           xlab("Meldedatum") +
           geom_line() + 
           stat_smooth(method = "loess", se = FALSE),
         "d_deaths_Age" = data %>% 
           ggplot(aes(x = Altersgruppe, y = Deaths, color = Landkreis)) + 
           geom_bar(stat= "identity", aes(fill = Landkreis), position = "dodge"),
         "d_deaths_Landkreis-Age-Datum" = "Datum Alter Landkreis",
         
         "f_inf_Age-Datum" = data %>% 
           ggplot(aes(x = as.Date(Meldedatum), y = Infections, group = Altersgruppe, color = Altersgruppe)) +
           xlab("Meldedatum") +
           geom_line() + 
           stat_smooth(method = "loess", se = FALSE),
         "f_inf_Bundesland-Datum" = data %>% 
           ggplot(aes(x = as.Date(Meldedatum), y = Deaths, group = Bundesland, color = Bundesland)) +
           xlab("Meldedatum") +
           geom_line() + 
           stat_smooth(method = "loess", se = FALSE),
         "f_inf_Bundesland" = data %>% 
           ggplot(aes(x = Bundesland, y = Infections, color = Bundesland)) + 
           geom_bar(stat= "identity", aes(fill = Bundesland), position = "dodge") + 
           geom_text(aes(label = Infections),vjust = -0.3, color = "black", size = 3.5),
         "f_inf_Bundesland-Age" = data %>% 
           ggplot(aes(x = Altersgruppe, y = Infections, color = Bundesland)) + 
           geom_bar(stat= "identity", aes(fill = Bundesland), position = "dodge") + 
           geom_text(aes(label = Infections),vjust = -0.3, color = "black", size = 3.5),
         "f_inf_Datum" = data %>% 
           ggplot(aes(x = as.Date(Meldedatum), y = Infections)) +
           xlab("Meldedatum") +
           geom_line() + 
           stat_smooth(method = "loess", se = FALSE),
         "f_inf_Age" = data %>% 
           ggplot(aes(x = Altersgruppe, y = Infections, color = Bundesland)) + 
           geom_bar(stat= "identity", aes(fill = Bundesland), position = "dodge"),
         "f_inf_Bundesland-Age-Datum" = "Datum Alter Bundesland",
         
         "d_inf_Age-Datum" = data %>% 
           ggplot(aes(x = as.Date(Meldedatum), y = Infections, group = Altersgruppe, color = Altersgruppe)) +
           xlab("Meldedatum") +
           geom_line() + 
           stat_smooth(method = "loess", se = FALSE),
         "d_inf_Landkreis-Datum" = %>% 
           ggplot(aes(x = as.Date(Meldedatum), y = Deaths, group = Landkreis, color = Landkreis)) +
           xlab("Meldedatum") +
           geom_line() + 
           stat_smooth(method = "loess", se = FALSE),
         "d_inf_Landkreis" = data %>% 
           ggplot(aes(x = Landkreis, y = Infections, color = Landkreis)) + 
           geom_bar(stat= "identity", aes(fill = Landkreis), position = "dodge") + 
           geom_text(aes(label = Infections),vjust = -0.3, color = "black", size = 3.5),
         "d_inf_Landkreis-Age" = data %>% 
           ggplot(aes(x = Altersgruppe, y = Infections, color = Landkreis)) + 
           geom_bar(stat= "identity", aes(fill = Landkreis), position = "dodge") + 
           geom_text(aes(label = Infections),vjust = -0.3, color = "black", size = 3.5),
         "d_inf_Datum" = data %>% 
           ggplot(aes(x = as.Date(Meldedatum), y = Infections)) +
           xlab("Meldedatum") +
           geom_line() + 
           stat_smooth(method = "loess", se = FALSE),
         "d_inf_Age" = data %>% 
           ggplot(aes(x = Altersgruppe, y = Infections, color = Landkreis)) + 
           geom_bar(stat= "identity", aes(fill = Landkreis), position = "dodge"),
         "d_inf_Landkreis-Age-Datum" = "Datum Alter Landkreis",
         
         "f_rec_Age-Datum" = data %>% 
           ggplot(aes(x = as.Date(Meldedatum), y = Recovered, group = Altersgruppe, color = Altersgruppe)) +
           xlab("Meldedatum") +
           geom_line() + 
           stat_smooth(method = "loess", se = FALSE),
         "f_rec_Bundesland-Datum" = data %>% 
           ggplot(aes(x = as.Date(Meldedatum), y = Deaths, group = Bundesland, color = Bundesland)) +
           xlab("Meldedatum") +
           geom_line() + 
           stat_smooth(method = "loess", se = FALSE),
         "f_rec_Bundesland" = data %>% 
           ggplot(aes(x = Bundesland, y = Recovered, color = Bundesland)) + 
           geom_bar(stat= "identity", aes(fill = Bundesland), position = "dodge") + 
           geom_text(aes(label = Recovered),vjust = -0.3, color = "black", size = 3.5),
         "f_rec_Bundesland-Age" = data %>% 
           ggplot(aes(x = Altersgruppe, y = Recovered, color = Bundesland)) + 
           geom_bar(stat= "identity", aes(fill = Bundesland), position = "dodge") + 
           geom_text(aes(label = Recovered),vjust = -0.3, color = "black", size = 3.5),
         "f_rec_Datum" = data %>% 
           ggplot(aes(x = as.Date(Meldedatum), y = Recovered)) +
           xlab("Meldedatum") +
           geom_line() + 
           stat_smooth(method = "loess", se = FALSE),
         "f_rec_Age" = data %>% 
           ggplot(aes(x = Altersgruppe, y = Recovered, color = Bundesland)) + 
           geom_bar(stat= "identity", aes(fill = Bundesland), position = "dodge"),
         "f_rec_Bundesland-Age-Datum" = "Datum Alter Bundesland",
         
         "d_rec_Age-Datum" = data %>% 
           ggplot(aes(x = as.Date(Meldedatum), y = Recovered, group = Altersgruppe, color = Altersgruppe)) +
           xlab("Meldedatum") +
           geom_line() + 
           stat_smooth(method = "loess", se = FALSE),
         "d_rec_Landkreis-Datum" = %>% 
           ggplot(aes(x = as.Date(Meldedatum), y = Deaths, group = Landkreis, color = Landkreis)) +
           xlab("Meldedatum") +
           geom_line() + 
           stat_smooth(method = "loess", se = FALSE),
         "d_rec_Landkreis-Age" = data %>% 
           ggplot(aes(x = Landkreis, y = Recovered, color = Landkreis)) + 
           geom_bar(stat= "identity", aes(fill = Landkreis), position = "dodge") + 
           geom_text(aes(label = Recovered),vjust = -0.3, color = "black", size = 3.5),
         "d_rec_Datum" = data %>% 
           ggplot(aes(x = Altersgruppe, y = Recovered, color = Landkreis)) + 
           geom_bar(stat= "identity", aes(fill = Landkreis), position = "dodge") + 
           geom_text(aes(label = Recovered),vjust = -0.3, color = "black", size = 3.5),
         "d_rec_Datum" = data %>% 
           ggplot(aes(x = as.Date(Meldedatum), y = Recovered)) +
           xlab("Meldedatum") +
           geom_line() + 
           stat_smooth(method = "loess", se = FALSE),
         "d_rec_Age" = data %>% 
           ggplot(aes(x = Altersgruppe, y = Recovered, color = Landkreis)) + 
           geom_bar(stat= "identity", aes(fill = Landkreis), position = "dodge"),
         "d_rec_Landkreis-Age-Datum" = "Datum Alter Landkreis")
}

plot_incidence_correlations_matrix <- function(correlations_data, districts = NA) {
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

  ggplot(data = correlations_data, aes(x = Landkreis_1, y = Landkreis_2,
                                       fill = Correlation)) +
    geom_tile()
}


plot_incidence_correlations_barchart <- function(correlations_data, top = 10) {
  correlations_data %>%
    arrange(desc(Correlation)) -> correlations_data

  correlations_data <- head(correlations_data, top)

  correlations_data %>%
    mutate(namings = paste(Landkreis_1, Landkreis_2, sep = " & ")) -> correlations_data

  ylim_min <- min(correlations_data$Correlation)-0.001
  if(ylim_min < 0) ylim_min <- 0
  ylim_max <- max(correlations_data$Correlation)
  ggplot(correlations_data, aes(x=reorder(namings, -Correlation),
                                y=Correlation)) +
    #scale_fill_brewer(palette = "Set1") +
    theme(axis.text.x = element_text(angle = 70, vjust = 1, hjust=1)) +
    coord_cartesian(ylim=c(ylim_min,ylim_max)) +
    ggtitle(paste0("Top ", as.character(top), " Correlation Pairs of District's Incidences")) +
    xlab("District Pairs") + ylab("Incident Correlation") +
    geom_bar(stat = "identity", fill="steelblue")
}
plot_incidence_correlations_barchart(incidence_correlation_pairs)
