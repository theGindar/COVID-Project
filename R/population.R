#' lädt Daten zu Bevölkerungszahlen pro Landkreis herunter, welche zur Berechnung der Inzidenzen benötigt werden
#'
#' @return NULL
#' @export
download_population_data <- function() {
  data = fromJSON("https://services2.arcgis.com/jUpNdisbWqRpMo35/arcgis/rest/services/KRS_ew_2019/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json")

  population_df = data.frame(LandkreisId = data$features$attributes$AGS,
                             Population = data$features$attributes$EWZ)

  remove_zeros <- function(x) {
    if(str_detect(x, "^0")) x <- substring(x, 2)
    return(x)
  }
  # remove leading zeros to districts with id of length 4
  remove_leading_zeros <- function(x) {
    if(nchar(x) >= 4) {
      return(substring(x, 2))
    } else {
      return(x)
    }
  }
  population_df$LandkreisId <- sapply(population_df$LandkreisId, remove_leading_zeros)
  population_df$LandkreisId <- as.integer(population_df$LandkreisId)

  # save data
  fpath <- system.file("extdata/population_data", "population_data_df.csv", package="covidproject")
  write.csv(population_df, fpath, row.names = FALSE)
  message("Population data successfully downloaded")
}

