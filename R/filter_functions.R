#install.packages("RcppRoll")
library(tidyverse)
library(dplyr)
library(stringr)
library(lubridate)
library(RcppRoll)

#cov_data <- read.csv("data.csv")
source("R/population.R")


# filter by date (Meldedatum)
#
# data: df with covid data
# date_start: "2020-11-05"
# date_end: "2020-11-08"
#
# outputs the rows of the input df that are between the specified dates

filter_by_date <- function(data, date_start, date_end){
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


#get_deaths_per_federal_states(cov_data,date_start = "dsa", date_end = "sadad", federal_state = "Berlin")
#age_group_start = "A05", age_group_end = "A80",




get_deaths_per_federal_states <- function(data, age_group_start = NA, age_group_end = NA, federal_state = NA, date_start = NA, date_end = NA) {
  federal_state_names = c("Schleswig-Holstein",
                          "Hamburg",
                          "Niedersachsen",
                          "Bremen",
                          "Nordrhein-Westfalen",
                          "Hessen",
                          "Rheinland-Pfalz",
                          "Baden-Württemberg",
                          "Bayern",
                          "Saarland",
                          "Berlin",
                          "Brandenburg",
                          "Mecklenburg-Vorpommern",
                          "Sachsen",
                          "Sachsen-Anhalt",
                          "Thüringen")
  federal_state <- gsub(pattern = "[??]",replacement = "ö", federal_state)
  federal_state <- gsub(pattern = "[??]",replacement = "ä", federal_state)
  federal_state <- gsub(pattern = "[??]",replacement = "ü", federal_state)

  # check if federal state is consistent
  if(!is.na(federal_state)){
    stopifnot("federal state does not exist" = federal_state %in% federal_state_names)
  }
  if(is.na(federal_state) & !is.na(age_group_start) & !is.na(age_group_end) & !is.na(date_start) & !is.na(date_end)) {
    print("Age Datum")
    data %>%
      filter(NeuerTodesfall %in% c(0,1)) %>%
      group_by(Altersgruppe, Meldedatum) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      filter_by_date(date_start, date_end) %>%
      summarize(Deaths = sum(AnzahlTodesfall)) -> result
  } else if(!is.na(federal_state) & is.na(age_group_start) & is.na(age_group_end) & !is.na(date_start) & !is.na(date_end)){
    print("Bundesland Datum")
    data %>%
      filter(NeuerTodesfall %in% c(0,1)) %>%
      group_by(Bundesland, Meldedatum) %>%
      filter(Bundesland %in% federal_state)  %>%
      filter_by_date(date_start, date_end)  %>%
      summarize(Deaths = sum(AnzahlTodesfall)) -> result
  } else if(!is.na(federal_state) & is.na(age_group_start) & is.na(age_group_end) & is.na(date_start) & is.na(date_end)){
    print("Bundesland")
    data %>%
      filter(NeuerTodesfall %in% c(0,1)) %>%
      group_by(Bundesland, Altersgruppe) %>%
      summarize(Deaths = sum(AnzahlTodesfall)) %>%
      filter(Bundesland %in% federal_state) -> result
  }else if(!is.na(federal_state) & !is.na(age_group_start) & !is.na(age_group_end) & is.na(date_start) & is.na(date_end)) {
    print("Bundesland Age")
    data %>%
      filter(NeuerTodesfall %in% c(0,1)) %>%
      group_by(Bundesland, Altersgruppe) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      filter(Bundesland %in% federal_state) %>%
      summarize(Deaths = sum(AnzahlTodesfall)) -> result
  }else if(is.na(federal_state) & is.na(age_group_start) & is.na(age_group_end) & !is.na(date_start) & !is.na(date_end)){
    print("Datum")
    data %>%
      filter(NeuerTodesfall %in% c(0,1)) %>%
      group_by(Bundesland, Meldedatum) %>%
      filter_by_date(date_start, date_end)  %>%
      summarize(Deaths = sum(AnzahlTodesfall)) -> result
  }else if(is.na(federal_state) & !is.na(age_group_start) & !is.na(age_group_end) & is.na(date_start) & is.na(date_end)){
    print("Age")
    data %>%
      filter(NeuerTodesfall %in% c(0,1)) %>%
      group_by(Bundesland, Altersgruppe) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      summarize(Deaths = sum(AnzahlTodesfall)) -> result
  }else if(is.na(federal_state) & is.na(date_start) & is.na(date_end)){
    print("Age 2")
    data %>%
      filter(NeuerTodesfall %in% c(0,1)) %>%
      group_by(Bundesland) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      summarize(Deaths = sum(AnzahlTodesfall)) -> result
  }else {
    print("Bundesland Age Datum")
    data %>%
      filter(NeuerTodesfall %in% c(0,1)) %>%
      group_by(Bundesland, Altersgruppe, Meldedatum) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      filter(Bundesland %in% federal_state) %>%
      filter_by_date(date_start, date_end)  %>%
      summarize(Deaths = sum(AnzahlTodesfall)) -> result

  }
  result <- ungroup(result)
  return(result)
}

get_deaths_per_district <- function(data, age_group_start = NA, age_group_end = NA, district = NA, date_start = NA, date_end = NA){

  district <- gsub(pattern = "[??]",replacement = "ö", district)
  district <- gsub(pattern = "[??]",replacement = "ä", district)
  district <- gsub(pattern = "[??]",replacement = "ü", district)
  # check if district is consistent
  if(!is.na(district)){
    stopifnot("district does not exist" = Landkreis %in% district)
  }
  if(is.na(district) & !is.na(age_group_start) & !is.na(age_group_end) & !is.na(date_start) & !is.na(date_end)) {
    print("Age Datum")
    data %>%
      filter(NeuerTodesfall %in% c(0,1)) %>%
      group_by(Altersgruppe, Meldedatum) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      filter_by_date(date_start, date_end) %>%
      summarize(Deaths = sum(AnzahlTodesfall)) -> result
  } else if(!is.na(district) & is.na(age_group_start) & is.na(age_group_end) & !is.na(date_start) & !is.na(date_end)){
    print("Landkreis Datum")
    data %>%
      filter(NeuerTodesfall %in% c(0,1)) %>%
      group_by(Landkreis, Meldedatum) %>%
      filter(Landkreis %in% district) %>%
      filter_by_date(date_start, date_end)  %>%
      summarize(Deaths = sum(AnzahlTodesfall)) -> result
  } else if(!is.na(district) & is.na(age_group_start) & is.na(age_group_end) & is.na(date_start) & is.na(date_end)){
    print("Landkreis")
    data %>%
      filter(NeuerTodesfall %in% c(0,1)) %>%
      group_by(Landkreis, Altersgruppe) %>%
      summarize(Deaths = sum(AnzahlTodesfall)) %>%
      filter(Landkreis %in% district) -> result
  }else if(!is.na(district) & !is.na(age_group_start) & !is.na(age_group_end) & is.na(date_start) & is.na(date_end)) {
    print("Landkreis Age")
    data %>%
      filter(NeuerTodesfall %in% c(0,1)) %>%
      group_by(Landkreis, Altersgruppe) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      filter(Landkreis %in% district) %>%
      summarize(Deaths = sum(AnzahlTodesfall)) -> result
  }else if(is.na(district) & is.na(age_group_start) & is.na(age_group_end) & !is.na(date_start) & !is.na(date_end)){
    print("Datum")
    data %>%
      filter(NeuerTodesfall %in% c(0,1)) %>%
      group_by(Landkreis, Meldedatum) %>%
      filter_by_date(date_start, date_end)  %>%
      summarize(Deaths = sum(AnzahlTodesfall)) -> result
  }else if(is.na(district) & !is.na(age_group_start) & !is.na(age_group_end) & is.na(date_start) & is.na(date_end)){
    print("Age")
    data %>%
      filter(NeuerTodesfall %in% c(0,1)) %>%
      group_by(Landkreis, Altersgruppe) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      summarize(Deaths = sum(AnzahlTodesfall)) -> result
  }else if(is.na(district) & is.na(date_start) & is.na(date_end)){
    print("Age 2")
    data %>%
      filter(NeuerTodesfall %in% c(0,1)) %>%
      group_by(Landkreis) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      summarize(Deaths = sum(AnzahlTodesfall)) -> result
  }else {
    print("Landkreis Age Datum")
    data %>%
      filter(NeuerTodesfall %in% c(0,1)) %>%
      group_by(Landkreis, Altersgruppe, Meldedatum) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      filter(Landkreis %in% district) %>%
      filter_by_date(date_start, date_end)  %>%
      summarize(Deaths = sum(AnzahlTodesfall)) -> result


  }
  result <- ungroup(result)
  return(result)
}

get_infections_per_federal_states <- function(data, age_group_start = NA, age_group_end = NA, federal_state = NA, date_start = NA, date_end = NA) {
  federal_state_names = c("Schleswig-Holstein",
                          "Hamburg",
                          "Niedersachsen",
                          "Bremen",
                          "Nordrhein-Westfalen",
                          "Hessen",
                          "Rheinland-Pfalz",
                          "Baden-Württemberg",
                          "Bayern",
                          "Saarland",
                          "Berlin",
                          "Brandenburg",
                          "Mecklenburg-Vorpommern",
                          "Sachsen",
                          "Sachsen-Anhalt",
                          "Thüringen")
  federal_state <- gsub(pattern = "[??]",replacement = "ö", federal_state)
  federal_state <- gsub(pattern = "[??]",replacement = "ä", federal_state)
  federal_state <- gsub(pattern = "[??]",replacement = "ü", federal_state)

  # check if federal state is consistent
  if(!is.na(federal_state)){
    stopifnot("federal state does not exist" = federal_state %in% federal_state_names)
  }
  if(is.na(federal_state) & !is.na(age_group_start) & !is.na(age_group_end) & !is.na(date_start) & !is.na(date_end)) {
    print("Age Datum")
    data %>%
      filter(NeuerFall %in% c(0,1)) %>%
      group_by(Altersgruppe, Meldedatum) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      filter_by_date(date_start, date_end) %>%
      summarize(Infections = sum(AnzahlFall)) -> result
  } else if(!is.na(federal_state) & is.na(age_group_start) & is.na(age_group_end) & !is.na(date_start) & !is.na(date_end)){
    print("Bundesland Datum")
    data %>%
      filter(NeuerFall %in% c(0,1)) %>%
      group_by(Bundesland, Meldedatum) %>%
      filter(Bundesland %in% federal_state)  %>%
      filter_by_date(date_start, date_end)  %>%
      summarize(Infections = sum(AnzahlFall)) -> result
  } else if(!is.na(federal_state) & is.na(age_group_start) & is.na(age_group_end) & is.na(date_start) & is.na(date_end)){
    print("Bundesland")
    data %>%
      filter(NeuerFall %in% c(0,1)) %>%
      group_by(Bundesland, Altersgruppe) %>%
      summarize(Infections = sum(AnzahlFall)) %>%
      filter(Bundesland %in% federal_state) -> result
  }else if(!is.na(federal_state) & !is.na(age_group_start) & !is.na(age_group_end) & is.na(date_start) & is.na(date_end)) {
    print("Bundesland Age")
    data %>%
      filter(NeuerFall %in% c(0,1)) %>%
      group_by(Bundesland, Altersgruppe) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      filter(Bundesland %in% federal_state) %>%
      summarize(Infections = sum(AnzahlFall)) -> result
  }else if(is.na(federal_state) & is.na(age_group_start) & is.na(age_group_end) & !is.na(date_start) & !is.na(date_end)){
    print("Datum")
    data %>%
      filter(NeuerFall %in% c(0,1)) %>%
      group_by(Bundesland, Meldedatum) %>%
      filter_by_date(date_start, date_end)  %>%
      summarize(Infections = sum(AnzahlFall)) -> result
  }else if(is.na(federal_state) & !is.na(age_group_start) & !is.na(age_group_end) & is.na(date_start) & is.na(date_end)){
    print("Age")
    data %>%
      filter(NeuerFall %in% c(0,1)) %>%
      group_by(Bundesland, Altersgruppe) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      summarize(Infections = sum(AnzahlFall)) -> result
  }else if(is.na(federal_state) & is.na(date_start) & is.na(date_end)){
    print("Age")
    data %>%
      filter(NeuerFall %in% c(0,1)) %>%
      group_by(Bundesland) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      summarize(Infections = sum(AnzahlFall)) -> result
  }else {
    print("Bundesland Age Datum")
    data %>%
      filter(NeuerFall %in% c(0,1)) %>%
      group_by(Bundesland, Altersgruppe, Meldedatum) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      filter(Bundesland %in% federal_state) %>%
      filter_by_date(date_start, date_end)  %>%
      summarize(Infections = sum(AnzahlFall)) -> result
  }
  result <- ungroup(result)
  return(result)
}

get_infections_per_district <- function(data, age_group_start = NA, age_group_end = NA, district = NA, date_start = NA, date_end = NA) {

  district <- gsub(pattern = "[??]",replacement = "ö", district)
  district <- gsub(pattern = "[??]",replacement = "ä", district)
  district <- gsub(pattern = "[??]",replacement = "ü", district)
  # check if district is consistent
  if(!is.na(district)){
    stopifnot("district does not exist" = Landkreis %in% district)
  }
  if(is.na(district) & !is.na(age_group_start) & !is.na(age_group_end) & !is.na(date_start) & !is.na(date_end)) {
    print("Age Datum")
    data %>%
      filter(NeuerFall %in% c(0,1)) %>%
      group_by(Altersgruppe, Meldedatum) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      filter_by_date(date_start, date_end) %>%
      summarize(Infections = sum(AnzahlFall)) -> result
  } else if(!is.na(district) & is.na(age_group_start) & is.na(age_group_end) & !is.na(date_start) & !is.na(date_end)){
    print("Landkreis Datum")
    data %>%
      filter(NeuerFall %in% c(0,1)) %>%
      group_by(Landkreis, Meldedatum) %>%
      filter(Landkreis %in% district) %>%
      filter_by_date(date_start, date_end)  %>%
      summarize(Infections = sum(AnzahlFall)) -> result
  } else if(!is.na(district) & is.na(age_group_start) & is.na(age_group_end) & is.na(date_start) & is.na(date_end)){
    print("Landkreis")
    data %>%
      filter(NeuerFall %in% c(0,1)) %>%
      group_by(Landkreis, Altersgruppe) %>%
      summarize(Infections = sum(AnzahlFall)) %>%
      filter(Landkreis %in% district) -> result
  }else if(!is.na(district) & !is.na(age_group_start) & !is.na(age_group_end) & is.na(date_start) & is.na(date_end)) {
    print("Landkreis Age")
    data %>%
      filter(NeuerFall %in% c(0,1)) %>%
      group_by(Landkreis, Altersgruppe) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      filter(Landkreis %in% district) %>%
      summarize(Infections = sum(AnzahlFall)) -> result
  }else if(is.na(district) & is.na(age_group_start) & is.na(age_group_end) & !is.na(date_start) & !is.na(date_end)){
    print("Datum")
    data %>%
      filter(NeuerFall %in% c(0,1)) %>%
      group_by(Landkreis, Meldedatum) %>%
      filter_by_date(date_start, date_end)  %>%
      summarize(Infections = sum(AnzahlFall)) -> result
  }else if(is.na(district) & !is.na(age_group_start) & !is.na(age_group_end) & is.na(date_start) & is.na(date_end)){
    print("Age")
    data %>%
      filter(NeuerFall %in% c(0,1)) %>%
      group_by(Landkreis, Altersgruppe) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      summarize(Infections = sum(AnzahlFall)) -> result
  }else if(is.na(district) & !is.na(age_group_start) & !is.na(age_group_end) & is.na(date_start) & is.na(date_end)){
    print("Age")
    data %>%
      filter(NeuerFall %in% c(0,1)) %>%
      group_by(Landkreis) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      summarize(Infections = sum(AnzahlFall)) -> result
  }else {
    print("Landkreis Age Datum")
    data %>%
      filter(NeuerFall %in% c(0,1)) %>%
      group_by(Landkreis, Altersgruppe, Meldedatum) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      filter(Landkreis %in% district) %>%
      filter_by_date(date_start, date_end)  %>%
      summarize(Infections = sum(AnzahlFall)) -> result
  }
  result <- ungroup(result)
  return(result)
}

get_recovered_per_federal_states <- function(data, age_group_start = NA, age_group_end = NA, federal_state = NA, date_start = NA, date_end = NA) {
  federal_state_names = c("Schleswig-Holstein",
                          "Hamburg",
                          "Niedersachsen",
                          "Bremen",
                          "Nordrhein-Westfalen",
                          "Hessen",
                          "Rheinland-Pfalz",
                          "Baden-Württemberg",
                          "Bayern",
                          "Saarland",
                          "Berlin",
                          "Brandenburg",
                          "Mecklenburg-Vorpommern",
                          "Sachsen",
                          "Sachsen-Anhalt",
                          "Thüringen")
  federal_state <- gsub(pattern = "[??]",replacement = "ö", federal_state)
  federal_state <- gsub(pattern = "[??]",replacement = "ä", federal_state)
  federal_state <- gsub(pattern = "[??]",replacement = "ü", federal_state)

  # check if federal state is consistent
  if(!is.na(federal_state)){
    stopifnot("federal state does not exist" = federal_state %in% federal_state_names)
  }
  if(is.na(federal_state) & !is.na(age_group_start) & !is.na(age_group_end) & !is.na(date_start) & !is.na(date_end)) {
    print("Age Datum")
    data %>%
      filter(NeuGenesen %in% c(0,1)) %>%
      group_by(Altersgruppe, Meldedatum) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      filter_by_date(date_start, date_end) %>%
      summarize(Recovered = sum(AnzahlGenesen)) -> result
  } else if(!is.na(federal_state) & is.na(age_group_start) & is.na(age_group_end) & !is.na(date_start) & !is.na(date_end)){
    print("Bundesland Datum")
    data %>%
      filter(NeuGenesen %in% c(0,1)) %>%
      group_by(Bundesland, Meldedatum) %>%
      filter(Bundesland %in% federal_state)  %>%
      filter_by_date(date_start, date_end)  %>%
      summarize(Recovered = sum(AnzahlGenesen)) -> result
  } else if(!is.na(federal_state) & is.na(age_group_start) & is.na(age_group_end) & is.na(date_start) & is.na(date_end)){
    print("Bundesland")
    data %>%
      filter(NeuGenesen %in% c(0,1)) %>%
      group_by(Bundesland, Altersgruppe) %>%
      summarize(Recovered = sum(AnzahlGenesen)) %>%
      filter(Bundesland %in% federal_state) -> result
  }else if(!is.na(federal_state) & !is.na(age_group_start) & !is.na(age_group_end) & is.na(date_start) & is.na(date_end)) {
    print("Bundesland Age")
    data %>%
      filter(NeuGenesen %in% c(0,1)) %>%
      group_by(Bundesland, Altersgruppe) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      filter(Bundesland %in% federal_state) %>%
      summarize(Recovered = sum(AnzahlGenesen)) -> result
  }else if(is.na(federal_state) & is.na(age_group_start) & is.na(age_group_end) & !is.na(date_start) & !is.na(date_end)){
    print("Datum")
    data %>%
      filter(NeuGenesen %in% c(0,1)) %>%
      group_by(Bundesland, Meldedatum) %>%
      filter_by_date(date_start, date_end)  %>%
      summarize(Recovered = sum(AnzahlGenesen)) -> result
  }else if(is.na(federal_state) & !is.na(age_group_start) & !is.na(age_group_end) & is.na(date_start) & is.na(date_end)){
    print("Age")
    data %>%
      filter(NeuGenesen %in% c(0,1)) %>%
      group_by(Bundesland, Altersgruppe) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      summarize(Recovered = sum(AnzahlGenesen)) -> result
  }else if(is.na(federal_state) & is.na(date_start) & is.na(date_end)){
    print("Age 2")
    data %>%
      filter(NeuGenesen %in% c(0,1)) %>%
      group_by(Bundesland) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      summarize(Recovered = sum(AnzahlGenesen)) -> result
  }else {
    print("Bundesland Age Datum")
    data %>%
      filter(NeuGenesen %in% c(0,1)) %>%
      group_by(Bundesland, Altersgruppe, Meldedatum) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      filter(Bundesland %in% federal_state) %>%
      filter_by_date(date_start, date_end)  %>%
      summarize(Recovered = sum(AnzahlGenesen)) -> result
  }
  result <- ungroup(result)
  return(result)
}

get_recovered_per_district <- function(data, age_group_start = NA, age_group_end = NA, district = NA, date_start = NA, date_end = NA) {

  district <- gsub(pattern = "[??]",replacement = "ö", district)
  district <- gsub(pattern = "[??]",replacement = "ä", district)
  district <- gsub(pattern = "[??]",replacement = "ü", district)
  # check if district is consistent
  if(!is.na(district)){
    stopifnot("district does not exist" = Landkreis %in% district)
  }
  if(is.na(district) & !is.na(age_group_start) & !is.na(age_group_end) & !is.na(date_start) & !is.na(date_end)) {
    print("Age Datum")
    data %>%
      filter(NeuGenesen %in% c(0,1)) %>%
      group_by(Altersgruppe, Meldedatum) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      filter_by_date(date_start, date_end) %>%
      summarize(Recovered = sum(AnzahlGenesen)) -> result
  } else if(!is.na(district) & is.na(age_group_start) & is.na(age_group_end) & !is.na(date_start) & !is.na(date_end)){
    print("Landkreis Datum")
    data %>%
      filter(NeuGenesen %in% c(0,1)) %>%
      group_by(Landkreis, Meldedatum) %>%
      filter(Landkreis %in% district) %>%
      filter_by_date(date_start, date_end)  %>%
      summarize(Recovered = sum(AnzahlGenesen)) -> result
  } else if(!is.na(district) & is.na(age_group_start) & is.na(age_group_end) & is.na(date_start) & is.na(date_end)){
    print("Landkreis")
    data %>%
      filter(NeuGenesen %in% c(0,1)) %>%
      group_by(Landkreis, Altersgruppe) %>%
      summarize(Recovered = sum(AnzahlGenesen)) %>%
      filter(Landkreis %in% district) -> result
  }else if(!is.na(district) & !is.na(age_group_start) & !is.na(age_group_end) & is.na(date_start) & is.na(date_end)) {
    print("Landkreis Age")
    data %>%
      filter(NeuGenesen %in% c(0,1)) %>%
      group_by(Landkreis, Altersgruppe) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      filter(Landkreis %in% district) %>%
      summarize(Recovered = sum(AnzahlGenesen)) -> result
  }else if(is.na(district) & is.na(age_group_start) & is.na(age_group_end) & !is.na(date_start) & !is.na(date_end)){
    print("Datum")
    data %>%
      filter(NeuGenesen %in% c(0,1)) %>%
      group_by(Landkreis, Meldedatum) %>%
      filter_by_date(date_start, date_end)  %>%
      summarize(Recovered = sum(AnzahlGenesen)) -> result
  }else if(is.na(district) & !is.na(age_group_start) & !is.na(age_group_end) & is.na(date_start) & is.na(date_end)){
    print("Age")
    data %>%
      filter(NeuGenesen %in% c(0,1)) %>%
      group_by(Landkreis, Altersgruppe) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      summarize(Recovered = sum(AnzahlGenesen)) -> result
  }else if(is.na(district) & is.na(date_start) & is.na(date_end)){
    print("Age 2")
    data %>%
      filter(NeuGenesen %in% c(0,1)) %>%
      group_by(Landkreis) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      summarize(Recovered = sum(AnzahlGenesen)) -> result
  }else {
    print("Landkreis Age Datum")
    data %>%
      filter(NeuGenesen %in% c(0,1)) %>%
      group_by(Landkreis, Altersgruppe, Meldedatum) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      filter(Landkreis %in% district) %>%
      filter_by_date(date_start, date_end)  %>%
      summarize(Recovered = sum(AnzahlGenesen)) -> result
  }
  result <- ungroup(result)
  return(result)
}




get_unclean_data <- function(cov_data){
  unclean_NeuerFall <- cov_data %>%
    filter(NeuerFall == -1)
  unclean_NeuerTodesfall <- cov_data %>%
    filter(NeuerTodesfall == -1)
  unclean_NeuGenesen <- cov_data %>%
    filter(NeuGenesen == -1)
  unclean_Altersgruppe <- cov_data %>%
    filter(Altersgruppe == unbekannt)
  unclean_data <- rbind(unclean_NeuerFall,unclean_NeuerTodesfall,unclean_NeuGenesen,unclean_Altersgruppe)
  return(unclean_data)
}

remove_unclean_data <- function(cov_data){
  xx <- cov_data$NeuerFall == -1
  yy <- cov_data$NeuerTodesfall == -1
  zz <- cov_data$NeuGenesen == -1
  tt <- xx + yy + zz
  row_to_keep = !tt
  cov_data <- cov_data[row_to_keep,]
}

get_incidence_per_district <- function(data, incidence_days = 7) {
  #
  #
  #
  # TODO: get_infections benutzen und parameter erweitern
  #
  #
  #


  # from infections function
  data %>%
    filter(NeuerFall %in% c(0,1)) %>%
    group_by(Meldedatum, IdLandkreis, Landkreis, Altersgruppe) %>%
    summarize(Infections = sum(AnzahlFall)) -> infect_data



  # don't care about age groups
  infect_data %>%
    group_by(Meldedatum, IdLandkreis, Landkreis) %>%
    summarize(Infections = sum(Infections)) -> infect_data

  # add population data
  population_data <- read.csv("R/population_data/population_data_df.csv")
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


get_correlation_for_incidence_pairs <- function(incidence_data) {

  # remove days/ districts without incidences
  df_incidences <- na.omit(df_incidences)
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
