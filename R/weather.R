#' Lädt historische Klimadaten von Deutschem Wetterdienst herunter
#'
#' @return NULL
#' @export
download_weather_data <- function() {
  metaIndex <- rdwd:::metaIndex
  metaInd <- metaIndex
  metaInd <- metaInd[metaInd$res=="subdaily" & metaInd$var=="air_temperature" & metaInd$per=="recent" & metaInd$hasfile, ]
  msf <- sf::st_as_sf(metaInd, coords=c("geoLaenge", "geoBreite"), crs=4326)
  federal_states_shp <- raster::getData("GADM", country = "DEU", level = 1)
  fpath <- system.file("extdata/vg2500_geo84", "vg2500_krs.shp", package="covidproject")
  lk <- sf::st_read(fpath, quiet=TRUE)
  int <- sf::st_intersects(lk, msf)
  # get station_ids for each district
  df_districts <- data.frame(lk_name=lk$GEN, lk_id=1:402)
  df_stations <- data.frame(lk_id=1:402, station_id=unlist(lapply(int, function(x) x[1])))
  df_all <- left_join(df_districts,df_stations, by="lk_id")
  df_all %>%
    mutate(station_id = metaInd[station_id, "Stations_id"]) -> df_all
  df_all$LandkreisId <- lk$RS

  # print progress bar
  print("Downloading weather data from DWD")
  pb = txtProgressBar(min = 0, max = length(df_all$LandkreisId), initial = 0)
  df_weather_data_all = data.frame()
  for(ind in seq_along(df_all$LandkreisId)) {

    setTxtProgressBar(pb,ind)
    if(is.na(df_all$station_id[[ind]])) next
    dirpath <- system.file("extdata/weather_data", package="covidproject")

    weather_link <- selectDWD(id=df_all$station_id[[ind]], var="air_temperature", per="recent", res="subdaily")
    weather_file <- suppressMessages(dataDWD(weather_link,
                                             read=FALSE,
                                             dir=dirpath,
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
      select(Meldedatum = date, LandkreisId, Temperatur = TT_TER) -> df_weather_data

    df_weather_data_all <- rbind(df_weather_data_all, df_weather_data)
  }

  # save data
  fpath <- system.file("extdata/weather_data", "weather_data_df.csv", package="covidproject")
  write.csv(df_weather_data_all, fpath, row.names = FALSE)

  message("successfully downloaded weather data!")
}

#' Fügt data frame mit COVID Daten die durchschnittliche Temperatur (für alle verfügbaren Wetterstationen in entsprechendem Landkreis) des Entsprechenden Tages hinzu.
#' @param cov_data Data frame mit COVID-Daten. Muss Spalte "IdLandkreis", sowie "Meldedatum" enthalten.
#' @return data frame mit neuer Spalte "Temperatur"
#' @export
add_weather_data <- function(cov_data) {
  stopifnot("No 'Meldedatum' column provided" = "Meldedatum" %in% colnames(cov_data))
  df_flag <- attr(cov_data, "flag")
  # download weather data, if it does not exist
  fpath <- system.file("extdata/weather_data", "weather_data_df.csv", package="covidproject")
  if(!file.exists(fpath)) download_weather_data()

  df_weather_data <- read.csv(fpath)

  # if cov_data has wrong date format remove ending zeros
  remove_zeros <- function(x) {
    if(str_detect(x, "\\+00$")) x <- gsub('.{3}$', '', x)
    return(x)
  }

  df_weather_data$Meldedatum <- sapply(df_weather_data$Meldedatum, remove_zeros)
  cov_data$Meldedatum <- sapply(cov_data$Meldedatum, remove_zeros)


  # if IdLandkreis exists in data add temperatures
  if("IdLandkreis" %in% colnames(cov_data)) {
    cov_data <- left_join(cov_data, df_weather_data, by = c("Meldedatum" = "Meldedatum",
                                                            "IdLandkreis" = "LandkreisId"))
  } else if("Bundesland" %in% colnames(cov_data)) {
    # if only Bundesland is provided
    federal_states_provided <- distinct(cov_data, Bundesland)$Bundesland

    df_weather_data %>%
      drop_na(Temperatur) -> df_weather_data

    df_weather_data$Bundesland <- sapply(df_weather_data$LandkreisId, get_federal_state_by_district_id)

    df_weather_data %>%
      group_by(Meldedatum, Bundesland) %>%
      summarise(Temperatur = mean(Temperatur)) -> df_weather_data

    cov_data <- left_join(cov_data, df_weather_data, by = c("Meldedatum" = "Meldedatum",
                                                            "Bundesland" = "Bundesland"))

  } else {
    # if nothing is provided use the mean of all measurements to get the mean temperature for germany
    df_weather_data %>%
      drop_na(Temperatur) %>%
      group_by(Meldedatum) %>%
      summarise(Temperatur = mean(Temperatur)) -> df_weather_data
    cov_data <- left_join(cov_data, df_weather_data, by = c("Meldedatum" = "Meldedatum"))
  }
  attr(cov_data, "flag") <- df_flag
  return(cov_data)
}


