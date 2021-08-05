library(ggplot2)

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
