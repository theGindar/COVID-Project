#install.packages("RcppRoll")
#library(tidyverse)
#library(dplyr)
#library(stringr)
#library(lubridate)
#library(RcppRoll)

#cov_data <- read.csv("R/data.csv")



#source("R/population.R")

# filter by date (Meldedatum)
#
# data: df with covid data
# date_start: "2020-11-05"
# date_end: "2020-11-08"
#
# outputs the rows of the input df that are between the specified dates

#' Filtert nach Einträgen im data frame, die zwischen dem spezifizierten Start- und Enddatum liegen.
#'
#' @param data Data frame mit COVID-Daten
#' @param date_start Startdatum als String ("YYYY/MM/DD")
#' @param date_end Enddatum als String ("YYYY/MM/DD")
#' @return gefiltertes data frame
filter_by_date <- function(data, date_start, date_end){
  stopifnot("wrong format for date_start" = str_detect(date_start, "[:digit:]{4}[////][:digit:]{2}[////][:digit:]{2}"))
  stopifnot("wrong format for date_end" = str_detect(date_end, "[:digit:]{4}[////][:digit:]{2}[////][:digit:]{2}"))
  result <- data[(as.Date(data$Meldedatum)> date_start & as.Date(data$Meldedatum) < date_end),]
  data %>%
    filter(Meldedatum %in% result$Meldedatum) -> result
  return(result)
}

# filters by age group (Altersgruppe)
#
# data: df with covid data
# age_group_start: lower bound of age groups, e. g. "A05"
# age_group_end: upper bound of age groups, e. g. "A59"
#
# outputs the rows of the input df that are between the specified age groups


#' Filtert nach Altersgruppen im data frame, die zwischen der spezifizierten Start- und Endaltersgruppe liegen.
#'
#' @param data Data frame mit COVID-Daten
#' @param age_group_start String mit Startaltersgruppe (z. B. "A00")
#' @param age_group_end String mit Startaltersgruppe (z. B. "A59")
#' @return gefiltertes data frame
filter_by_age_group <- function(data, age_group_start = NA, age_group_end = NA) {
  if(is.na(age_group_start) | is.na(age_group_end)) {
    if(is.na(age_group_start)) age_group_start <- "A00"
    if(is.na(age_group_end)) age_group_end <- "A80"

    start_ages <- c("A00-A04" = 0,
                    "A05-A14" = 5,
                    "A15-A34" = 15,
                    "A35-A59" = 35,
                    "A60-A79" = 60,
                    "A80+" = 80)
    end_ages <- c("A00-A04" = 4 ,
                  "A05-A14" = 14,
                  "A15-A34" = 34,
                  "A35-A59" = 59,
                  "A60-A79" = 79,
                  "A80+" = 80)
    # check if age_group is consistent
    stopifnot("wrong format for age_group_start" = str_detect(age_group_start, "^A[:digit:]{2}$"))
    stopifnot("wrong format for age_group_end" = str_detect(age_group_end, "^A[:digit:]{2}$"))
    stopifnot("age group for age_group_start does not exist" = as.double(substr(age_group_start, 2, 3)) %in% start_ages)
    stopifnot("age group for age_group_end does not exist" = as.double(substr(age_group_end, 2, 3)) %in% end_ages)
    stopifnot("age_group_start should be smaller than age_group_end" = as.double(substr(age_group_start, 2, 3)) < as.double(substr(age_group_end, 2, 3)))

    start_age <- as.double(substr(age_group_start, 2, 3))
    end_age <- as.double(substr(age_group_end, 2, 3))

    queried_age_groups <- attributes(start_ages[which(start_ages == start_age):which(end_ages == end_age)])$names

    data %>%
      filter(Altersgruppe %in% queried_age_groups) -> result
    return(result)
  } else {
    return(data)
  }
}


# get_x_per_y
#
# x: specification of search in either deaths, recovered or infections
# y: specification of place in districts or federal states
# data: df with covid data
# age_group_start: lower bound of age groups, e. g. "A05"
# age_group_end: upper bound of age groups, e. g. "A59"
# federal_state: optional bound of the c("Berlin","Hessen")
# district: optional bound of the c("SK Flensburg","LK Uckermark")
# date_start: optional lower bound of the dates, e. g. "2020/12/04"
# date_end: optional upper bound of the dates, e. g."2021/04/15"
#
# outputs a dataframe with the specified grouping that was put in

#' Erzeugt Daten zu Todesfällen pro Bundesland
#'
#' @param data Data frame mit COVID-Daten
#' @param age_group_start String mit Startaltersgruppe (z. B. "A00")
#' @param age_group_end String mit Startaltersgruppe (z. B. "A59")
#' @param federal_state Einzelner String oder Vektor aus Strings mit angefragten Bundesländernamen
#' @param date_start Startdatum als String ("YYYY/MM/DD")
#' @param date_end Enddatum als String ("YYYY/MM/DD")
#' @return Data frame mit gesuchte Daten und beschreibendem Flag
#'
#' @examples
#' get_deaths_per_federal_states(df, age_group_start = "A00", age_group_end = "A59", federal_state = c("Berlin", "Bayern"), date_start = "2021/05/01", date_end = "2021/06/01")
#' get_deaths_per_federal_states(df, federal_state = c("Berlin", "Bayern"), date_start = "2021/05/01", date_end = "2021/06/01")
#' get_deaths_per_federal_states(df, federal_state = c("Berlin", "Bayern"))
#' @export
get_deaths_per_federal_states <- function(data, age_group_start = NA, age_group_end = NA, federal_state = NA, date_start = NA, date_end = NA) {

  federal_state <- gsub(pattern = "[??]",replacement = "ö", federal_state)
  federal_state <- gsub(pattern = "[??]",replacement = "ä", federal_state)
  federal_state <- gsub(pattern = "[??]",replacement = "ü", federal_state)
  federal_state <- gsub(pattern = "[?]",replacement = "?Y", federal_state)



  # check if federal state is consistent
  if(!is.na(federal_state)){
    stopifnot("federal state does not exist" = is.element(federal_state, data$Bundesland))
  }
  if(is.na(federal_state) & !is.na(age_group_start) & !is.na(age_group_end) & !is.na(date_start) & !is.na(date_end)) {
    print("Age Datum")
    data %>%
      filter(NeuerTodesfall %in% c(0,1)) %>%
      group_by(Altersgruppe, Meldedatum) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      filter_by_date(date_start, date_end) %>%
      summarize(Deaths = sum(AnzahlTodesfall)) -> result
    attr(result, "flag") <- "f_deaths_Age-Datum"
  } else if(!is.na(federal_state) & is.na(age_group_start) & is.na(age_group_end) & !is.na(date_start) & !is.na(date_end)){
    print("Bundesland Datum")
    data %>%
      filter(NeuerTodesfall %in% c(0,1)) %>%
      group_by(Bundesland, Meldedatum) %>%
      filter(Bundesland %in% federal_state)  %>%
      filter_by_date(date_start, date_end)  %>%
      summarize(Deaths = sum(AnzahlTodesfall)) -> result
    attr(result, "flag") <- "f_deaths_Bundesland-Datum"
  } else if(!is.na(federal_state) & is.na(age_group_start) & is.na(age_group_end) & is.na(date_start) & is.na(date_end)){
    print("Bundesland")
    data %>%
      filter(NeuerTodesfall %in% c(0,1)) %>%
      group_by(Bundesland) %>%
      summarize(Deaths = sum(AnzahlTodesfall)) %>%
      filter(Bundesland %in% federal_state) -> result
    attr(result, "flag") <- "f_deaths_Bundesland"
  }else if(!is.na(federal_state) & !is.na(age_group_start) & !is.na(age_group_end) & is.na(date_start) & is.na(date_end)) {
    print("Bundesland Age")
    data %>%
      filter(NeuerTodesfall %in% c(0,1)) %>%
      group_by(Bundesland, Altersgruppe) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      filter(Bundesland %in% federal_state) %>%
      summarize(Deaths = sum(AnzahlTodesfall)) -> result
    attr(result, "flag") <- "f_deaths_Bundesland-Age"
  }else if(is.na(federal_state) & is.na(age_group_start) & is.na(age_group_end) & !is.na(date_start) & !is.na(date_end)){
    print("Datum")
    data %>%
      filter(NeuerTodesfall %in% c(0,1)) %>%
      group_by(Meldedatum) %>%
      filter_by_date(date_start, date_end)  %>%
      summarize(Deaths = sum(AnzahlTodesfall)) -> result
    attr(result, "flag") <- "f_deaths_Datum"
  }else if(is.na(federal_state) & !is.na(age_group_start) & !is.na(age_group_end) & is.na(date_start) & is.na(date_end)){
    print("Age")
    data %>%
      filter(NeuerTodesfall %in% c(0,1)) %>%
      group_by(Bundesland, Altersgruppe) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      summarize(Deaths = sum(AnzahlTodesfall)) -> result
    attr(result, "flag") <- "f_deaths_Age"
  }else if(!is.na(federal_state) & !is.na(age_group_start) & !is.na(age_group_end) & !is.na(date_start) & !is.na(date_end)){
    print("Bundesland Age Datum")
    data %>%
      filter(NeuerTodesfall %in% c(0,1)) %>%
      group_by(Bundesland, Altersgruppe, Meldedatum) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      filter(Bundesland %in% federal_state) %>%
      filter_by_date(date_start, date_end)  %>%
      summarize(Deaths = sum(AnzahlTodesfall)) -> result
    attr(result, "flag") <- "f_deaths_Bundesland-Age-Datum"
  }else{
    print("Keine Spezifikationen")
    data %>%
      filter(NeuerFall %in% c(0,1)) %>%
      group_by(Bundesland) %>%
      summarize(Deaths = sum(AnzahlTodesfall)) -> result
    attr(result, "flag") <- "f_deaths"
  }
  return(result)
}

#' Erzeugt Daten zu Todesfällen pro Landkreis
#'
#' @param data Data frame mit COVID-Daten
#' @param age_group_start String mit Startaltersgruppe (z. B. "A00")
#' @param age_group_end String mit Startaltersgruppe (z. B. "A59")
#' @param district Einzelner String oder Vektor aus Strings mit angefragten Landkreisnamen (beginnend mit z. B. "LK" für Landkreis und "SK" für Stadtkreis)
#' @param date_start Startdatum als String ("YYYY/MM/DD")
#' @param date_end Enddatum als String ("YYYY/MM/DD")
#' @return Data frame mit gesuchte Daten und beschreibendem Flag
#'
#' @examples
#' get_deaths_per_district(df, age_group_start = "A00", age_group_end = "A59", district = c("LK Karlsruhe", "SK Karlsruhe"), date_start = "2021/05/01", date_end = "2021/06/01")
#' get_deaths_per_district(df, district = c("LK Karlsruhe", "SK Karlsruhe"), date_start = "2021/05/01", date_end = "2021/06/01")
#' get_deaths_per_district(df, district = c("LK Karlsruhe", "SK Karlsruhe"))
#' @export
get_deaths_per_district <- function(data, age_group_start = NA, age_group_end = NA, district = NA, date_start = NA, date_end = NA){


  district <- gsub(pattern = "[??]",replacement = "ö", district)
  district <- gsub(pattern = "[??]",replacement = "ä", district)
  district <- gsub(pattern = "[??]",replacement = "ü", district)
  district <- gsub(pattern = "[?]",replacement = "?Y", district)


  # check if district is consistent
  if(!is.na(district)){
    stopifnot("district does not exist" = is.element(district, data$Landkreis))
  }
  if(is.na(district) & !is.na(age_group_start) & !is.na(age_group_end) & !is.na(date_start) & !is.na(date_end)) {
    print("Age Datum")
    data %>%
      filter(NeuerTodesfall %in% c(0,1)) %>%
      group_by(Altersgruppe, Meldedatum) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      filter_by_date(date_start, date_end) %>%
      summarize(Deaths = sum(AnzahlTodesfall)) -> result
    attr(result, "flag") <- "d_deaths_Age-Datum"
  } else if(!is.na(district) & is.na(age_group_start) & is.na(age_group_end) & !is.na(date_start) & !is.na(date_end)){
    print("Landkreis Datum")
    data %>%
      filter(NeuerTodesfall %in% c(0,1)) %>%
      group_by(IdLandkreis, Landkreis, Meldedatum) %>%
      filter(Landkreis %in% district) %>%
      filter_by_date(date_start, date_end)  %>%
      summarize(Deaths = sum(AnzahlTodesfall)) -> result
    attr(result, "flag") <- "d_deaths_Landkreis-Datum"
  } else if(!is.na(district) & is.na(age_group_start) & is.na(age_group_end) & is.na(date_start) & is.na(date_end)){
    print("Landkreis")
    data %>%
      filter(NeuerTodesfall %in% c(0,1)) %>%
      group_by(IdLandkreis, Landkreis) %>%
      summarize(Deaths = sum(AnzahlTodesfall)) %>%
      filter(Landkreis %in% district) -> result
    attr(result, "flag") <- "d_deaths_Landkreis"
    print(attr(result, "flag"))
  }else if(!is.na(district) & !is.na(age_group_start) & !is.na(age_group_end) & is.na(date_start) & is.na(date_end)) {
    print("Landkreis Age")
    data %>%
      filter(NeuerTodesfall %in% c(0,1)) %>%
      group_by(IdLandkreis, Landkreis, Altersgruppe) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      summarize(Deaths = sum(AnzahlTodesfall))  %>%
      filter(Landkreis %in% district) -> result
    attr(result, "flag") <- "d_deaths_Landkreis-Age"
  }else if(is.na(district) & is.na(age_group_start) & is.na(age_group_end) & !is.na(date_start) & !is.na(date_end)){
    print("Datum")
    data %>%
      filter(NeuerTodesfall %in% c(0,1)) %>%
      group_by(Meldedatum) %>%
      filter_by_date(date_start, date_end)  %>%
      summarize(Deaths = sum(AnzahlTodesfall)) -> result
    attr(result, "flag") <- "d_deaths_Datum"
  }else if(is.na(district) & !is.na(age_group_start) & !is.na(age_group_end) & is.na(date_start) & is.na(date_end)){
    print("Age")
    data %>%
      filter(NeuerTodesfall %in% c(0,1)) %>%
      group_by(Altersgruppe) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      summarize(Deaths = sum(AnzahlTodesfall)) -> result
    attr(result, "flag") <- "d_deaths_Age"
  }else if(!is.na(district) & !is.na(age_group_start) & !is.na(age_group_end) & !is.na(date_start) & !is.na(date_end)){
    print("Landkreis Age Datum")
    data %>%
      filter(NeuerTodesfall %in% c(0,1)) %>%
      group_by(IdLandkreis, Landkreis, Altersgruppe, Meldedatum) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      filter(Landkreis %in% district) %>%
      filter_by_date(date_start, date_end)  %>%
      summarize(Deaths = sum(AnzahlTodesfall)) -> result
    attr(result, "flag") <- "d_deaths_Landkreis-Age-Datum"
    }else{
    print("Keine Spezifikationen")
    data %>%
      filter(NeuerFall %in% c(0,1)) %>%
      group_by(Meldedatum) %>%
      summarize(Deaths = sum(AnzahlTodesfall)) -> result
    attr(result, "flag") <- "d_deaths"
    }
  return(result)
}


#' Erzeugt Daten zu Infektionsfällen pro Bundesland
#'
#' @param data Data frame mit COVID-Daten
#' @param age_group_start String mit Startaltersgruppe (z. B. "A00")
#' @param age_group_end String mit Startaltersgruppe (z. B. "A59")
#' @param federal_state Einzelner String oder Vektor aus Strings mit angefragten Bundesländernamen
#' @param date_start Startdatum als String ("YYYY/MM/DD")
#' @param date_end Enddatum als String ("YYYY/MM/DD")
#' @return Data frame mit gesuchte Daten und beschreibendem Flag
#'
#' @examples
#' get_infections_per_federal_states(df, age_group_start = "A00", age_group_end = "A59", federal_state = c("Berlin", "Bayern"), date_start = "2021/05/01", date_end = "2021/06/01")
#' get_infections_per_federal_states(df, federal_state = c("Berlin", "Bayern"), date_start = "2021/05/01", date_end = "2021/06/01")
#' get_infections_per_federal_states(df, federal_state = c("Berlin", "Bayern"))
#' @export
get_infections_per_federal_states <- function(data, age_group_start = NA, age_group_end = NA, federal_state = NA, date_start = NA, date_end = NA) {


  federal_state <- gsub(pattern = "[??]",replacement = "ö", federal_state)
  federal_state <- gsub(pattern = "[??]",replacement = "ä", federal_state)
  federal_state <- gsub(pattern = "[??]",replacement = "ü", federal_state)
  federal_state <- gsub(pattern = "[?]",replacement = "?Y", federal_state)

  # check if federal state is consistent
  if(!is.na(federal_state)){
    stopifnot("federal state does not exist" = is.element(federal_state, data$Bundesland))
  }
  if(is.na(federal_state) & !is.na(age_group_start) & !is.na(age_group_end) & !is.na(date_start) & !is.na(date_end)) {
    print("Age Datum")
    data %>%
      filter(NeuerFall %in% c(0,1)) %>%
      group_by(Altersgruppe, Meldedatum) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      filter_by_date(date_start, date_end) %>%
      summarize(Infections = sum(AnzahlFall)) -> result
    attr(result, "flag") <- "f_inf_Age-Datum"
  } else if(!is.na(federal_state) & is.na(age_group_start) & is.na(age_group_end) & !is.na(date_start) & !is.na(date_end)){
    print("Bundesland Datum")
    data %>%
      filter(NeuerFall %in% c(0,1)) %>%
      group_by(Bundesland, Meldedatum) %>%
      filter(Bundesland %in% federal_state)  %>%
      filter_by_date(date_start, date_end)  %>%
      summarize(Infections = sum(AnzahlFall)) -> result
    attr(result, "flag") <- "f_inf_Bundesland-Datum"
  } else if(!is.na(federal_state) & is.na(age_group_start) & is.na(age_group_end) & is.na(date_start) & is.na(date_end)){
    print("Bundesland")
    data %>%
      filter(NeuerFall %in% c(0,1)) %>%
      group_by(Bundesland) %>%
      summarize(Infections = sum(AnzahlFall)) %>%
      filter(Bundesland %in% federal_state) -> result
    attr(result, "flag") <- "f_inf_Bundesland"
  }else if(!is.na(federal_state) & !is.na(age_group_start) & !is.na(age_group_end) & is.na(date_start) & is.na(date_end)) {
    print("Bundesland Age")
    data %>%
      filter(NeuerFall %in% c(0,1)) %>%
      group_by(Bundesland, Altersgruppe) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      filter(Bundesland %in% federal_state) %>%
      summarize(Infections = sum(AnzahlFall)) -> result
    attr(result, "flag") <- "f_inf_Bundesland-Age"
  }else if(is.na(federal_state) & is.na(age_group_start) & is.na(age_group_end) & !is.na(date_start) & !is.na(date_end)){
    print("Datum")
    data %>%
      filter(NeuerFall %in% c(0,1)) %>%
      group_by(Meldedatum) %>%
      filter_by_date(date_start, date_end)  %>%
      summarize(Infections = sum(AnzahlFall)) -> result
    attr(result, "flag") <- "f_inf_Datum"
  }else if(is.na(federal_state) & !is.na(age_group_start) & !is.na(age_group_end) & is.na(date_start) & is.na(date_end)){
    print("Age")
    data %>%
      filter(NeuerFall %in% c(0,1)) %>%
      group_by(Bundesland, Altersgruppe) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      summarize(Infections = sum(AnzahlFall)) -> result
    attr(result, "flag") <- "f_inf_Age"
  }else if(is.na(federal_state) & is.na(date_start) & is.na(date_end)){
    print("Age 2")
    data %>%
      filter(NeuerFall %in% c(0,1)) %>%
      group_by(Bundesland) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      summarize(Infections = sum(AnzahlFall)) -> result
  }else if(!is.na(federal_state) & !is.na(age_group_start) & !is.na(age_group_end) & !is.na(date_start) & !is.na(date_end)){
    print("Bundesland Age Datum")
    data %>%
      filter(NeuerFall %in% c(0,1)) %>%
      group_by(Bundesland, Altersgruppe, Meldedatum) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      filter(Bundesland %in% federal_state) %>%
      filter_by_date(date_start, date_end)  %>%
      summarize(Infections = sum(AnzahlFall)) -> result
    attr(result, "flag") <- "f_inf_Bundesland-Age-Datum"
  }else{
    print("Keine Spezifikationen")
    data %>%
      filter(NeuerFall %in% c(0,1)) %>%
      group_by(Bundesland) %>%
      summarize(Infections = sum(AnzahlFall)) -> result
    attr(result, "flag") <- "f_inf"
  }
  return(result)
}

#' Erzeugt Daten zu Infectionsfällen pro Landkreis
#'
#' @param data Data frame mit COVID-Daten
#' @param age_group_start String mit Startaltersgruppe (z. B. "A00")
#' @param age_group_end String mit Startaltersgruppe (z. B. "A59")
#' @param district Einzelner String oder Vektor aus Strings mit angefragten Landkreisnamen (beginnend mit z. B. "LK" für Landkreis und "SK" für Stadtkreis)
#' @param date_start Startdatum als String ("YYYY/MM/DD")
#' @param date_end Enddatum als String ("YYYY/MM/DD")
#' @return Data frame mit gesuchte Daten und beschreibendem Flag
#'
#' @examples
#' get_infections_per_district(df, age_group_start = "A00", age_group_end = "A59", district = c("LK Karlsruhe", "SK Karlsruhe"), date_start = "2021/05/01", date_end = "2021/06/01")
#' get_infections_per_district(df, district = c("LK Karlsruhe", "SK Karlsruhe"), date_start = "2021/05/01", date_end = "2021/06/01")
#' get_infections_per_district(df, district = c("LK Karlsruhe", "SK Karlsruhe"))
#' @export
get_infections_per_district <- function(data, age_group_start = NA, age_group_end = NA, district = NA, date_start = NA, date_end = NA) {

  district <- gsub(pattern = "[??]",replacement = "ö", district)
  district <- gsub(pattern = "[??]",replacement = "ä", district)
  district <- gsub(pattern = "[??]",replacement = "ü", district)
  district <- gsub(pattern = "[?]",replacement = "?Y", district)


  # check if district is consistent
  if(!is.na(district)){
    stopifnot("district does not exist" = is.element(district, data$Landkreis))
  }
  if(is.na(district) & !is.na(age_group_start) & !is.na(age_group_end) & !is.na(date_start) & !is.na(date_end)) {
    print("Age Datum")
    data %>%
      filter(NeuerFall %in% c(0,1)) %>%
      group_by(Altersgruppe, Meldedatum) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      filter_by_date(date_start, date_end) %>%
      summarize(Infections = sum(AnzahlFall)) -> result
    attr(result, "flag") <- "d_inf_Age-Datum"
  } else if(!is.na(district) & is.na(age_group_start) & is.na(age_group_end) & !is.na(date_start) & !is.na(date_end)){
    print("Landkreis Datum")
    data %>%
      filter(NeuerFall %in% c(0,1)) %>%
      group_by(IdLandkreis, Landkreis, Meldedatum) %>%
      filter(Landkreis %in% district) %>%
      filter_by_date(date_start, date_end)  %>%
      summarize(Infections = sum(AnzahlFall)) -> result
    attr(result, "flag") <- "d_inf_Landkreis-Datum"
  } else if(!is.na(district) & is.na(age_group_start) & is.na(age_group_end) & is.na(date_start) & is.na(date_end)){
    print("Landkreis")
    data %>%
      filter(NeuerFall %in% c(0,1)) %>%
      group_by(IdLandkreis, Landkreis) %>%
      summarize(Infections = sum(AnzahlFall)) %>%
      filter(Landkreis %in% district) -> result
    attr(result, "flag") <- "d_inf_Landkreis"
  }else if(!is.na(district) & !is.na(age_group_start) & !is.na(age_group_end) & is.na(date_start) & is.na(date_end)) {
    print("Landkreis Age")
    data %>%
      filter(NeuerFall %in% c(0,1)) %>%
      group_by(IdLandkreis, Landkreis, Altersgruppe) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      summarize(Infections = sum(AnzahlFall)) %>%
      filter(Landkreis %in% district) -> result
    attr(result, "flag") <- "d_inf_Landkreis-Age"
  }else if(is.na(district) & is.na(age_group_start) & is.na(age_group_end) & !is.na(date_start) & !is.na(date_end)){
    print("Datum")
    data %>%
      filter(NeuerFall %in% c(0,1)) %>%
      group_by(Meldedatum) %>%
      filter_by_date(date_start, date_end)  %>%
      summarize(Infections = sum(AnzahlFall)) -> result
    attr(result, "flag") <- "d_inf_Datum"
  }else if(is.na(district) & !is.na(age_group_start) & !is.na(age_group_end) & is.na(date_start) & is.na(date_end)){
    print("Age")
    data %>%
      filter(NeuerFall %in% c(0,1)) %>%
      group_by(Altersgruppe) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      summarize(Infections = sum(AnzahlFall)) -> result
    attr(result, "flag") <- "d_inf_Age"
  }else if(is.na(district) & !is.na(age_group_start) & !is.na(age_group_end) & is.na(date_start) & is.na(date_end)){
    print("Age 2")
    print(age_group_start)
    data %>%
      filter(NeuerFall %in% c(0,1)) %>%
      group_by(IdLandkreis, Landkreis) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      summarize(Infections = sum(AnzahlFall)) -> result
  }else if(!is.na(district) & !is.na(age_group_start) & !is.na(age_group_end) & !is.na(date_start) & !is.na(date_end)){
    print("Landkreis Age Datum")
    data %>%
      filter(NeuerFall %in% c(0,1)) %>%
      group_by(IdLandkreis, Landkreis, Altersgruppe, Meldedatum) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      filter(Landkreis %in% district) %>%
      filter_by_date(date_start, date_end)  %>%
      summarize(Infections = sum(AnzahlFall)) -> result
    attr(result, "flag") <- "d_inf_Landkreis-Age-Datum"
  }else{
    print("Keine Spezifikationen")
    data %>%
      filter(NeuerFall %in% c(0,1)) %>%
      group_by(Meldedatum) %>%
      summarize(Infections = sum(AnzahlFall)) -> result
    attr(result, "flag") <- "d_inf"
  }
  return(result)
}

#' Erzeugt Daten zu Genesenenfällen pro Bundesland
#'
#' @param data Data frame mit COVID-Daten
#' @param age_group_start String mit Startaltersgruppe (z. B. "A00")
#' @param age_group_end String mit Startaltersgruppe (z. B. "A59")
#' @param federal_state Einzelner String oder Vektor aus Strings mit angefragten Bundesländernamen
#' @param date_start Startdatum als String ("YYYY/MM/DD")
#' @param date_end Enddatum als String ("YYYY/MM/DD")
#' @return Data frame mit gesuchte Daten und beschreibendem Flag
#'
#' @examples
#' get_recovered_per_federal_states(df, age_group_start = "A00", age_group_end = "A59", federal_state = c("Berlin", "Bayern"), date_start = "2021/05/01", date_end = "2021/06/01")
#' get_recovered_per_federal_states(df, federal_state = c("Berlin", "Bayern"), date_start = "2021/05/01", date_end = "2021/06/01")
#' get_recovered_per_federal_states(df, federal_state = c("Berlin", "Bayern"))
#' @export
get_recovered_per_federal_states <- function(data, age_group_start = NA, age_group_end = NA, federal_state = NA, date_start = NA, date_end = NA) {


  federal_state <- gsub(pattern = "[??]",replacement = "ö", federal_state)
  federal_state <- gsub(pattern = "[??]",replacement = "ä", federal_state)
  federal_state <- gsub(pattern = "[??]",replacement = "ü", federal_state)
  federal_state <- gsub(pattern = "[?]",replacement = "?Y", federal_state)

  # check if federal state is consistent
  if(!is.na(federal_state)){
    stopifnot("federal state does not exist" = is.element(federal_state, data$Bundesland))
  }
  if(is.na(federal_state) & !is.na(age_group_start) & !is.na(age_group_end) & !is.na(date_start) & !is.na(date_end)) {
    print("Age Datum")
    data %>%
      filter(NeuGenesen %in% c(0,1)) %>%
      group_by(Altersgruppe, Meldedatum) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      filter_by_date(date_start, date_end) %>%
      summarize(Recovered = sum(AnzahlGenesen)) -> result
    attr(result, "flag") <- "f_rec_Age-Datum"
  } else if(!is.na(federal_state) & is.na(age_group_start) & is.na(age_group_end) & !is.na(date_start) & !is.na(date_end)){
    print("Bundesland Datum")
    data %>%
      filter(NeuGenesen %in% c(0,1)) %>%
      group_by(Bundesland, Meldedatum) %>%
      filter(Bundesland %in% federal_state)  %>%
      filter_by_date(date_start, date_end)  %>%
      summarize(Recovered = sum(AnzahlGenesen)) -> result
    attr(result, "flag") <- "f_rec_Bundesland-Datum"
  } else if(!is.na(federal_state) & is.na(age_group_start) & is.na(age_group_end) & is.na(date_start) & is.na(date_end)){
    print("Bundesland")
    data %>%
      filter(NeuGenesen %in% c(0,1)) %>%
      group_by(Bundesland) %>%
      summarize(Recovered = sum(AnzahlGenesen)) %>%
      filter(Bundesland %in% federal_state) -> result
    attr(result, "flag") <- "f_rec_Bundesland"
  }else if(!is.na(federal_state) & !is.na(age_group_start) & !is.na(age_group_end) & is.na(date_start) & is.na(date_end)) {
    print("Bundesland Age")
    data %>%
      filter(NeuGenesen %in% c(0,1)) %>%
      group_by(Bundesland, Altersgruppe) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      filter(Bundesland %in% federal_state) %>%
      summarize(Recovered = sum(AnzahlGenesen)) -> result
    attr(result, "flag") <- "f_rec_Bundesland-Age"
  }else if(is.na(federal_state) & is.na(age_group_start) & is.na(age_group_end) & !is.na(date_start) & !is.na(date_end)){
    print("Datum")
    data %>%
      filter(NeuGenesen %in% c(0,1)) %>%
      group_by(Meldedatum) %>%
      filter_by_date(date_start, date_end)  %>%
      summarize(Recovered = sum(AnzahlGenesen)) -> result
    attr(result, "flag") <- "f_rec_Datum"
  }else if(is.na(federal_state) & !is.na(age_group_start) & !is.na(age_group_end) & is.na(date_start) & is.na(date_end)){
    print("Age")
    data %>%
      filter(NeuGenesen %in% c(0,1)) %>%
      group_by(Bundesland, Altersgruppe) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      summarize(Recovered = sum(AnzahlGenesen)) -> result
    attr(result, "flag") <- "f_rec_Age"
  }else if(is.na(federal_state) & is.na(date_start) & is.na(date_end)){
    print("Age 2")
    data %>%
      filter(NeuGenesen %in% c(0,1)) %>%
      group_by(Bundesland) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      summarize(Recovered = sum(AnzahlGenesen)) -> result
  }else if(!is.na(federal_state) & !is.na(age_group_start) & !is.na(age_group_end) & !is.na(date_start) & !is.na(date_end)){
    print("Bundesland Age Datum")
    data %>%
      filter(NeuGenesen %in% c(0,1)) %>%
      group_by(Bundesland, Altersgruppe, Meldedatum) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      filter(Bundesland %in% federal_state) %>%
      filter_by_date(date_start, date_end)  %>%
      summarize(Recovered = sum(AnzahlGenesen)) -> result
    attr(result, "flag") <- "f_rec_Bundesland-Age-Datum"
  }else{
    print("Keine Spezifikationen")
    data %>%
      filter(NeuerFall %in% c(0,1)) %>%
      group_by(Bundesland) %>%
      summarize(Recovered = sum(AnzahlGenesen)) -> result
    attr(result, "flag") <- "f_rec"
  }
  return(result)
}

#' Erzeugt Daten zu Genesenenfällen pro Landkreis
#'
#' @param data Data frame mit COVID-Daten
#' @param age_group_start String mit Startaltersgruppe (z. B. "A00")
#' @param age_group_end String mit Startaltersgruppe (z. B. "A59")
#' @param district Einzelner String oder Vektor aus Strings mit angefragten Landkreisnamen (beginnend mit z. B. "LK" für Landkreis und "SK" für Stadtkreis)
#' @param date_start Startdatum als String ("YYYY/MM/DD")
#' @param date_end Enddatum als String ("YYYY/MM/DD")
#' @return Data frame mit gesuchte Daten und beschreibendem Flag
#'
#' @examples
#' get_recovered_per_district(df, age_group_start = "A00", age_group_end = "A59", district = c("LK Karlsruhe", "SK Karlsruhe"), date_start = "2021/05/01", date_end = "2021/06/01")
#' get_recovered_per_district(df, district = c("LK Karlsruhe", "SK Karlsruhe"), date_start = "2021/05/01", date_end = "2021/06/01")
#' get_recovered_per_district(df, district = c("LK Karlsruhe", "SK Karlsruhe"))
#' @export
get_recovered_per_district <- function(data, age_group_start = NA, age_group_end = NA, district = NA, date_start = NA, date_end = NA) {


  district <- gsub(pattern = "[??]",replacement = "ö", district)
  district <- gsub(pattern = "[??]",replacement = "ä", district)
  district <- gsub(pattern = "[??]",replacement = "ü", district)
  district <- gsub(pattern = "[?]",replacement = "?Y", district)


  # check if district is consistent
  if(!is.na(district)){
    print(district)
    stopifnot("district does not exist" = is.element(district, data$Landkreis))
  }
  if(is.na(district) & !is.na(age_group_start) & !is.na(age_group_end) & !is.na(date_start) & !is.na(date_end)) {
    print("Age Datum")
    data %>%
      filter(NeuGenesen %in% c(0,1)) %>%
      group_by(Altersgruppe, Meldedatum) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      filter_by_date(date_start, date_end) %>%
      summarize(Recovered = sum(AnzahlGenesen)) -> result
    attr(result, "flag") <- "d_rec_Age-Datum"
  } else if(!is.na(district) & is.na(age_group_start) & is.na(age_group_end) & !is.na(date_start) & !is.na(date_end)){
    print("Landkreis Datum")
    data %>%
      filter(NeuGenesen %in% c(0,1)) %>%
      group_by(IdLandkreis, Landkreis, Meldedatum) %>%
      filter(Landkreis %in% district) %>%
      filter_by_date(date_start, date_end)  %>%
      summarize(Recovered = sum(AnzahlGenesen)) -> result
    attr(result, "flag") <- "d_rec_Landkreis-Datum"
  } else if(!is.na(district) & is.na(age_group_start) & is.na(age_group_end) & is.na(date_start) & is.na(date_end)){
    print("Landkreis")
    data %>%
      filter(NeuGenesen %in% c(0,1)) %>%
      group_by(IdLandkreis, Landkreis) %>%
      summarize(Recovered = sum(AnzahlGenesen)) %>%
      filter(Landkreis %in% district) -> result
    attr(result, "flag") <- "d_rec_Landkreis"
  }else if(!is.na(district) & !is.na(age_group_start) & !is.na(age_group_end) & is.na(date_start) & is.na(date_end)) {
    print("Landkreis Age")
    data %>%
      filter(NeuGenesen %in% c(0,1)) %>%
      group_by(IdLandkreis, Landkreis, Altersgruppe) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      summarize(Recovered = sum(AnzahlGenesen)) %>%
      filter(Landkreis %in% district) -> result
    attr(result, "flag") <- "d_rec_Landkreis-Age"
  }else if(is.na(district) & is.na(age_group_start) & is.na(age_group_end) & !is.na(date_start) & !is.na(date_end)){
    print("Datum")
    data %>%
      filter(NeuGenesen %in% c(0,1)) %>%
      group_by(Meldedatum) %>%
      filter_by_date(date_start, date_end)  %>%
      summarize(Recovered = sum(AnzahlGenesen)) -> result
    attr(result, "flag") <- "d_rec_Datum"
  }else if(is.na(district) & !is.na(age_group_start) & !is.na(age_group_end) & is.na(date_start) & is.na(date_end)){
    print("Age")
    data %>%
      filter(NeuGenesen %in% c(0,1)) %>%
      group_by(Altersgruppe) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      summarize(Recovered = sum(AnzahlGenesen)) -> result
    attr(result, "flag") <- "d_rec_Age"
  }else if(is.na(district) & is.na(date_start) & is.na(date_end)){
    print("Age 2")
    data %>%
      filter(NeuGenesen %in% c(0,1)) %>%
      group_by(IdLandkreis, Landkreis) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      summarize(Recovered = sum(AnzahlGenesen)) -> result
  }else if(!is.na(district) & !is.na(age_group_start) & !is.na(age_group_end) & !is.na(date_start) & !is.na(date_end)){
    print("Landkreis Age Datum")
    data %>%
      filter(NeuGenesen %in% c(0,1)) %>%
      group_by(IdLandkreis, Landkreis, Altersgruppe, Meldedatum) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      filter(Landkreis %in% district) %>%
      filter_by_date(date_start, date_end)  %>%
      summarize(Recovered = sum(AnzahlGenesen)) -> result
    attr(result, "flag") <- "d_rec_Landkreis-Age-Datum"
  }else{
    print("Keine Spezifikationen")
    data %>%
      filter(NeuerFall %in% c(0,1)) %>%
      group_by(Meldedatum) %>%
      summarize(Recovered = sum(AnzahlGenesen)) -> result
    attr(result, "flag") <- "d_rec"
  }
  return(result)
}

# get infections where the start of illness is known in all of germany
#
# data: df with covid data
# age_group_start: optional lower bound of age groups, e. g. "A05"
# age_group_end: optional upper bound of age groups, e. g. "A59"
# date_start: optional lower bound of the dates, e. g. "2020/12/04"
# date_end: optional upper bound of the dates, e. g."2021/04/15"
#
# outputs a dataframe with the specifications that were made

#' Erzeugt Daten zu Infektionen mit bekanntem Krankheitsbeginndatum in ganz Deutschland
#'
#' @param data Data frame mit COVID-Daten
#' @param age_group_start String mit Startaltersgruppe (z. B. "A00")
#' @param age_group_end String mit Startaltersgruppe (z. B. "A59")
#' @param date_start Startdatum als String ("YYYY/MM/DD")
#' @param date_end Enddatum als String ("YYYY/MM/DD")
#' @return Data frame mit gesuchte Daten und beschreibendem Flag
#'
#' @examples
#' get_infections_overall(df, age_group_start = "A00", age_group_end = "A59", date_start = "2021/05/01", date_end = "2021/06/01")
#' @export
get_infections_overall <- function(data, age_group_start = NA, age_group_end = NA, date_start = NA, date_end = NA){

  if(is.na(age_group_start) & is.na(age_group_end) & !is.na(date_start) & !is.na(date_end)){
    print("Datum")
    data %>%
      filter(IstErkrankungsbeginn == "1") %>%
      group_by(Meldedatum) %>%
      filter_by_date(date_start, date_end) %>%
      summarize(Infections = sum(AnzahlFall)) -> result
    attr(result, "flag") <- "f_inf_Datum"
  }else{
    data %>%
      filter(IstErkrankungsbeginn == "1") %>%
      group_by(Meldedatum) %>%
      summarize(Infections = sum(AnzahlFall)) -> result
    attr(result, "flag") <- "f_inf_Datum"
  }
  return(result)
}

# appends the number of days it took to report the infection to a healthdepartment
#
# data: df with covid data
#
# output: df with a column date_diff

#' Fügt Spalte "date_diff" an data frame an, welche die Anzahl Tage zwischen Infektion und Meldung beim Gesundheitsamt beschreibt.
#'
#' @param data Data frame mit COVID-Daten
#' @return Data frame mit gesuchte Daten und beschreibendem Flag
#'
#' @examples
#' append_report_duration(df)
#' @export
append_report_duration <- function(data){

  data %>%
    filter(IstErkrankungsbeginn == "1")
  data$date_diff <- as.Date(as.character(data$Meldedatum),format="%Y/%m/%d")-as.Date(as.character(data$Refdatum),format="%Y/%m/%d")
  row_to_keep <- as.Date(as.character(data$Meldedatum),format="%Y/%m/%d")>=as.Date(as.character(data$Refdatum))
  data <- data[row_to_keep,]
  attr(data, "flag") <- "date_diff"
  return(data)
}

# create a df with infections, deaths and mortalityrate
#
# data: df with covid data
# age_group_start: optional lower bound of age groups, e. g. "A05"
# age_group_end: optional upper bound of age groups, e. g. "A59"
#
# outputs: a summarized dataframe of the mortalityrate if a age_group is being filtered
# outputs: a plottable dataframe of mortalityrate for every case in data

#' Erzeugt Daten zu Fallsterblichkeit in ganz Deutschland abhängig von Altersgruppen
#'
#' @param data Data frame mit COVID-Daten
#' @param age_group_start String mit Startaltersgruppe (z. B. "A00")
#' @param age_group_end String mit Startaltersgruppe (z. B. "A59")
#'
#' @return Data frame mit gesuchte Daten und beschreibendem Flag
#'
#' @examples
#' get_fallsterblichkeit_overall(df, age_group_start = "A00", age_group_end = "A59")
#' @export
get_fallsterblichkeit_overall <- function(data, age_group_start = NA, age_group_end = NA){
  if(!is.na(age_group_start) & !is.na(age_group_start)){
    get_infections_per_district(data, age_group_start, age_group_end) -> x1
    get_deaths_per_district(data, age_group_start, age_group_end) -> result
    result$Infections <- x1$Infections
    result$Fallsterblichkeit <- result$Deaths/x1$Infections
    attr(result, "flag") <- "DE_Fallsterblichkeit_age"
    return(result)

  }else{
  get_infections_per_district(data) -> x1
  get_deaths_per_district(data) -> result
  result$Infections <- x1$Infections
  result$Fallsterblichkeit <- result$Deaths/x1$Infections
  attr(result, "flag") <- "DE_Fallsterblichkeit"
  return(result)
  }
}

# get every unclean data in your covid data
#
# data: df with covid data
#
# output: returns a dataframe with every unclean row in your data

#' Erzeugt data frame bestehend aus "unsauberen" Daten, also Unklarheiten und fehlerhaften Einträgen.
#'
#' @param data Data frame mit COVID-Daten
#' @return Data frame mit gesuchte Daten
#'
#' @examples
#' get_unclean_data(df)
#' @export
get_unclean_data <- function(data){
  unclean_NeuerFall <- data %>%
    filter(NeuerFall == -1)
  unclean_NeuerTodesfall <- data %>%
    filter(NeuerTodesfall == -1)
  unclean_NeuGenesen <- data %>%
    filter(NeuGenesen == -1)
  unclean_Altersgruppe <- data %>%
    filter(Altersgruppe == "unbekannt")
  unclean_data <- rbind(unclean_NeuerFall,unclean_NeuerTodesfall,unclean_NeuGenesen,unclean_Altersgruppe)
  return(unclean_data)
}

# remove unclean data
#
# data: df with covid data to clean from
#
# output: returns a datafram without unclean data

#' Reinigt data frame von "unsauberen" Daten, also Unklarheiten und fehlerhaften Einträgen.
#'
#' @param data Data frame mit COVID-Daten
#' @return Gesäubertes data frame
#'
#' @examples
#' remove_unclean_data(df)
#' @export
remove_unclean_data <- function(data){
  unclean_NeuerFall <- data$NeuerFall == -1
  unclean_NeuerTodesfall <- data$NeuerTodesfall == -1
  unclean_NeuGenesen <- data$NeuGenesen == -1
  unclean_Altersgruppe <- data$Altersgruppe == "unbekannt"
  #unclean_date <- data$as.Date(as.character(cov_data$Meldedatum),format="%Y/%m/%d")>=as.Date(as.character(cov_data$Refdatum),format="%Y/%m/%d"))
  unclean_to_remove <- unclean_NeuerFall + unclean_NeuerTodesfall + unclean_NeuGenesen + unclean_Altersgruppe
  row_to_keep = !unclean_to_remove
  data <- data[row_to_keep,]
  return(data)
}


#' Erzeugt Daten zu Inzidenzen pro District.
#'
#' @param data Data frame mit COVID-Daten
#' @param age_group_start String mit Startaltersgruppe (z. B. "A00")
#' @param age_group_end String mit Startaltersgruppe (z. B. "A59")
#' @param district Einzelner String oder Vektor aus Strings mit angefragten Landkreisnamen (beginnend mit z. B. "LK" für Landkreis und "SK" für Stadtkreis). Falls district NA ist werden Daten für alle Landkreise erzeugt.
#' @param date_start Startdatum als String ("YYYY/MM/DD")
#' @param date_end Enddatum als String ("YYYY/MM/DD")
#' @param incidence_days Zahl der Tage zur Berechnung der Inzidenz, z. B. incidence_days = 7 für 7-Tage-Inzidenz
#' @return Data frame mit gesuchte Daten
#'
#' @examples
#' get_incidence_per_district(df, age_group_start = "A00", age_group_end = "A59", district = "LK Karlsruhe", date_start = "2021/05/01", date_end = "2021/06/01")
#' @export
get_incidence_per_district <- function(data, age_group_start = "A00", age_group_end = "A80", district = NA, date_start = NA, date_end = NA, incidence_days = 7) {
  stopifnot("start date should be provided" = !is.na(date_start))
  stopifnot("end date should be provided" = !is.na(date_end))
  # from infections function
  if(is.na(district)) {
    data %>%
      filter(NeuerFall %in% c(0,1)) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      filter_by_date(date_start, date_end) %>%
      group_by(Meldedatum, IdLandkreis, Landkreis) %>%
      summarize(Infections = sum(AnzahlFall)) -> infect_data
  } else {
    data %>%
      filter(NeuerFall %in% c(0,1)) %>%
      filter(Landkreis %in% district) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      filter_by_date(date_start, date_end) %>%
      group_by(Meldedatum, IdLandkreis, Landkreis) %>%
      summarize(Infections = sum(AnzahlFall)) -> infect_data
  }

  stopifnot("No entries found matching the filter" = (0 < nrow(infect_data)))

  # add population data
  fpath <- system.file("extdata/population_data", "population_data_df.csv", package="covidproject")
  population_data <- read.csv(fpath)
  infect_data <- left_join(infect_data, population_data, by=c("IdLandkreis" = "LandkreisId"))

  infect_data <- ungroup(infect_data)
  lk_ids <- distinct(infect_data, IdLandkreis)$IdLandkreis

  result_df <- data.frame()
  message("Calculating Incidences")
  pb = txtProgressBar(min = 0, max = length(lk_ids), initial = 0, style = 3)
  ind <- 1
  for (lk_id in lk_ids) {
    infect_data %>%
      filter(IdLandkreis == lk_id) -> infect_data_district

    infect_data_district %>%
      arrange(Meldedatum) %>%
      ungroup %>%
      mutate(Combined_Infections = roll_sum(Infections,
                                            incidence_days,
                                            align = "right",
                                            fill = NA)) -> infect_data_district

    result_df <- rbind(infect_data_district, result_df)
    setTxtProgressBar(pb,ind)
    ind <- ind+1
  }

  result_df %>%
    mutate(Inzidenz = Combined_Infections/Population*100000) -> result_df

  result_df %>%
    select(-c("Population", "Infections", "Combined_Infections")) -> result_df

  return(result_df)
}

#' Erzeugt data frame, welches die Correlation zwischen Landkreispaaren bezüglich derer Inzidenz beschreibt
#'
#' @param incidence_data Data frame mit Inzidenz Daten. Erzeugbar durch get_incidence_per_district(...)
#' @return Data frame mit gesuchte Daten
#'
#' @examples
#' get_correlation_for_incidence_pairs(incidence.df)
#' @export
get_correlation_for_incidence_pairs <- function(incidence_data) {

  # remove days/ districts without incidences
  df_incidences <- na.omit(incidence_data)
  lk_ids <- distinct(df_incidences, IdLandkreis)$IdLandkreis

  all_correlations_df <- data.frame()

  message("Calculating Correlations for District Pairs")
  num_comparisons <- length(lk_ids) * (length(lk_ids) - 1) / 2
  pb = txtProgressBar(min = 0, max = num_comparisons, initial = 0, style = 3)

  ind <- 1
  lk_ind <- 1
  for(lk_id_1 in lk_ids) {
    for(lk_id_2 in lk_ids[lk_ind:length(lk_ids)]) {
      setTxtProgressBar(pb,ind)
      if(lk_id_1 != lk_id_2) {
        df_incidences %>%
          filter(IdLandkreis == lk_id_1) -> df_cor_1
        df_incidences %>%
          filter(IdLandkreis == lk_id_2) -> df_cor_2

        # make sure both districts got the same time span
        df_both <- inner_join(df_cor_1, df_cor_2, by="Meldedatum")

        df_both %>%
          select(IdLandkreis_1 = IdLandkreis.x,
                 Landkreis_1 = Landkreis.x,
                 IdLandkreis_2 = IdLandkreis.y,
                 Landkreis_2 = Landkreis.y) -> new_pair_df
        new_pair_df <- new_pair_df[1,]
        new_pair_df$Correlation <- cor(df_both$Inzidenz.x, df_both$Inzidenz.y)
        all_correlations_df <- rbind(all_correlations_df, new_pair_df)
        ind <- ind+1
      }
    }
    lk_ind <- lk_ind+1
  }
  return(all_correlations_df)
}

