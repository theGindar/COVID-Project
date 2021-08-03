#install.packages("rdwd")
#install.packages('bit64')
#install.packages("sf")
library(rdwd)
library(dplyr)
library(tidyr)
library(stringr)


#install.packages("rgeos", repos="http://R-Forge.R-project.org", type="source")
#install.packages("rgdal", repos="http://R-Forge.R-project.org", type="source")
library(devtools)
#install_github("r-spatial/sf", configure.args = "--with-proj-lib=/usr/local/lib/")
library(sf)


download_weather_data <- function() {
  data("metaIndex")
  metaInd <- metaIndex
  metaInd <- metaInd[metaInd$res=="subdaily" & metaInd$var=="air_temperature" & metaInd$per=="recent" & metaInd$hasfile, ]
  msf <- sf::st_as_sf(metaInd, coords=c("geoLaenge", "geoBreite"), crs=4326)
  federal_states_shp <- raster::getData("GADM", country = "DEU", level = 1)
  lk <- sf::st_read("R/vg2500_geo84/vg2500_krs.shp", quiet=TRUE)
  int <- sf::st_intersects(lk, msf)
  # get station_ids for each district
  df_districts <- data.frame(lk_name=lk$GEN, lk_id=1:402)
  df_stations <- data.frame(lk_id=1:402, station_id=unlist(lapply(int, function(x) x[1])))
  df_all <- left_join(df_districts,df_stations, by="lk_id")
  df_all %>%
    mutate(station_id = metaInd[station_id, "Stations_id"]) -> df_all
  df_all$LandkreisId <- lk$RS

  # print progress bar
  pb = txtProgressBar(min = 0, max = length(df_all$LandkreisId), initial = 0)
  df_weather_data_all = data.frame()
  for(ind in seq_along(df_all$LandkreisId)) {

    setTxtProgressBar(pb,ind)
    if(is.na(df_all$station_id[[ind]])) next

    weather_link <- selectDWD(id=df_all$station_id[[ind]], var="air_temperature", per="recent", res="subdaily")
    weather_file <- suppressMessages(dataDWD(weather_link,
                                             read=FALSE,
                                             dir="R/weather_data",
                                             force=NA,
                                             overwrite=TRUE))
    skip_to_next <- FALSE
    weather_data <- tryCatch({
      suppressMessages(readDWD(weather_file, varnames=FALSE))
    },
    error=function(cond) {
      message("skipped weather station because of invalid url")
      skip_to_next <<- TRUE
    })

    if(skip_to_next) next

    # only get the temperature at 12:00
    df_weather_data <- data.frame(stations_id = weather_data[1],
                                  mess_datum = weather_data[2],
                                  QN_4 = weather_data[3],
                                  TT_TER = weather_data[4],
                                  RF_TER = weather_data[5])



    df_weather_data <- separate(df_weather_data, "MESS_DATUM", c("date", "time"), sep=" ")
    df_weather_data$date <- format(as.Date(df_weather_data$date), "%Y/%m/%d")

    df_weather_data %>%
      mutate(time = as.character(time)) %>%
      mutate(date = as.character(date)) %>%
      filter(time == '12:00:00') -> df_weather_data

    df_weather_data$date <- paste(df_weather_data$date, "00:00:00+00", sep = " ")

    lk_id <- df_all$LandkreisId[[ind]]
    # remove leading zeros from LandkreisId
    if(str_detect(lk_id, "^0")) lk_id <- substring(lk_id, 2)
    lk_id <- as.double(lk_id)

    df_weather_data$LandkreisId <- lk_id
    df_weather_data %>%
      select(Refdatum = date, LandkreisId, Temperatur = TT_TER) -> df_weather_data

    df_weather_data_all <- rbind(df_weather_data_all, df_weather_data)
  }

  # save data
  write.csv(df_weather_data_all, "R/weather_data/weather_data_df.csv", row.names = FALSE)

}

add_weather_data <- function(cov_data) {
  data("metaIndex")
  metaInd <- metaIndex
  metaInd <- metaInd[metaInd$res=="subdaily" & metaInd$var=="air_temperature" & metaInd$per=="recent" & metaInd$hasfile, ]
  msf <- sf::st_as_sf(metaInd, coords=c("geoLaenge", "geoBreite"), crs=4326)
  federal_states_shp <- raster::getData("GADM", country = "DEU", level = 1)
  lk <- sf::st_read("R/vg2500_geo84/vg2500_krs.shp", quiet=TRUE)
  int <- sf::st_intersects(lk, msf)
  # get station_ids for each district
  df_districts <- data.frame(lk_name=lk$GEN, lk_id=1:402)
  df_stations <- data.frame(lk_id=1:402, station_id=unlist(lapply(int, function(x) x[1])))
  df_all <- left_join(df_districts,df_stations, by="lk_id")
  df_all %>%
    mutate(station_id = metaInd[station_id, "Stations_id"]) -> df_all
  df_all$LandkreisId <- lk$RS
  df_weather_data <- read.csv("R/weather_data/weather_data_df.csv")
  cov_data <- left_join(cov_data, df_weather_data, by = c("Refdatum" = "Refdatum", "IdLandkreis" = "LandkreisId"))
  return(cov_data)
}

cov_data <- read.csv("R/RKI_COVID19.csv")
download_weather_data()
cov_data_with_weather <- add_weather_data(cov_data)
