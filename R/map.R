# returns plot of map of german districts
#
# cov_data: a dataframe with columns "IdLandkreis" and "datapoints" (data that should be plotted for each district)
# plot_title: title of the diagram
# legend_title: title of the legend
#

#' Erzeugt aus data frame einen Plot von Deutschlandkarte mit Landkreisen und Bundesländern
#'
#' @param cov_data Ein data frame mit den Spalten "IdLandkreis" und "datapoints". "datapoints" wird dann farblich pro Landkreis dargestellt
#' @param plot_title Titel des Plots
#' @param legend_title Titel der Legende
#' @return Plot objekt mit Deutschlandkarten-Plot
#'
#' @examples
#' plot_map(incidence.df, plot_title = "Beispiel", legend_title = "noch ein Beispiel")
#' @export
plot_map <- function(cov_data, plot_title, legend_title){
  suppressWarnings(gpclibPermit())

  districts_shp <- raster::getData("GADM", country = "DEU", level = 2)


  districts_shp_f <- fortify(districts_shp, region = "CC_2")

  # "IdLandkreis should be char"
  cov_data <- transform(cov_data, IdLandkreis = as.character(IdLandkreis))

  # add leading zeros to districts with id of length 4
  add_leading_zeros <- function(data) {
    if(nchar(data) <= 4) {
      return(paste0("0", data))
    } else {
      return(data)
    }
  }
  cov_data$IdLandkreis <- sapply(cov_data$IdLandkreis, add_leading_zeros)

  # Fix for wrong districts
  districts_shp_f <- districts_shp_f %>%
    mutate(
      # göttingen
      id = if_else(id == "03152", "03159", id),
      id = if_else(id == "03156", "03159", id),
      # berlin
      id = if_else(id == "11000", "11001", id)
    )
  merged_shp <- full_join(districts_shp_f, cov_data, by=c("id" = "IdLandkreis"))
  federal_states_shp <- raster::getData("GADM", country = "DEU", level = 1)

  # for color scaling of the map
  max_datapoints_value <- max(merged_shp$datapoints)
  median_datapoints_value <- median(merged_shp$datapoints)

  single_map <-  function(merged_shp, districts_shp_f){
    ggplot() +
      geom_polygon(data = merged_shp, aes(x = long, y = lat, group = group,
                                              fill = datapoints), color = "black", size = 0.25) +
      geom_polygon(data = federal_states_shp, aes(x = long, y = lat, group = group),
                   alpha = 0, color = "black", size = 1.0) +
      coord_map() +
      scale_fill_gradient2(midpoint = median_datapoints_value*3, low = "green", mid = "yellow",
                           high = "red", space = "Lab", limits=c(0, max_datapoints_value), oob=squish,
                           name = legend_title) +
      theme_nothing(legend = TRUE) +
      labs(
        title = plot_title
      )
  }
  return(single_map(merged_shp, districts_shp_f))
}

