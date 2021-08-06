#install.packages("RcppRoll")
library(tidyverse)
library(dplyr)
library(stringr)
library(lubridate)
library(RcppRoll)

cov_data <- read.csv("data.csv")
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
filter_by_age_group <- function(data, age_group_start, age_group_end) {
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
                          "Baden-WÃ¼rttemberg",
                          "Bayern",
                          "Saarland",
                          "Berlin",
                          "Brandenburg",
                          "Mecklenburg-Vorpommern",
                          "Sachsen",
                          "Sachsen-Anhalt",
                          "ThÃ¼ringen")
  federal_state <- gsub(pattern = "[öÖ]",replacement = "Ã¶", federal_state)
  federal_state <- gsub(pattern = "[äÄ]",replacement = "Ã¤", federal_state)
  federal_state <- gsub(pattern = "[üÜ]",replacement = "Ã¼", federal_state)
  federal_state <- gsub(pattern = "[ß]",replacement = "ÃY", federal_state)

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
  }else {
    print("Bundesland Age Datum")
    flag <- "f_deaths7"
    data %>%
      filter(NeuerTodesfall %in% c(0,1)) %>%
      group_by(Bundesland, Altersgruppe, Meldedatum) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      filter(Bundesland %in% federal_state) %>%
      filter_by_date(date_start, date_end)  %>%
      summarize(Deaths = sum(AnzahlTodesfall)) -> result
    attr(result, "flag") <- "f_deaths_Bundesland-Age-Datum"


  }
  return(result)
}

get_deaths_per_district <- function(data, age_group_start = NA, age_group_end = NA, district = NA, date_start = NA, date_end = NA){

  district <- gsub(pattern = "[öÖ]",replacement = "Ã¶", district)
  district <- gsub(pattern = "[äÄ]",replacement = "Ã¤", district)
  district <- gsub(pattern = "[üÜ]",replacement = "Ã¼", district)
  district <- gsub(pattern = "[ß]",replacement = "ÃY", district)
  # check if district is consistent
  #if(!is.na(district)){
  #  stopifnot("district does not exist" = Landkreis %in% district)
  #}
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
      group_by(Landkreis, Meldedatum) %>%
      filter(Landkreis %in% district) %>%
      filter_by_date(date_start, date_end)  %>%
      summarize(Deaths = sum(AnzahlTodesfall)) -> result
    attr(result, "flag") <- "d_deaths_Landkreis-Datum"
  } else if(!is.na(district) & is.na(age_group_start) & is.na(age_group_end) & is.na(date_start) & is.na(date_end)){
    print("Landkreis")
    data %>%
      filter(NeuerTodesfall %in% c(0,1)) %>%
      group_by(Landkreis) %>%
      summarize(Deaths = sum(AnzahlTodesfall)) %>%
      filter(Landkreis %in% district) -> result
    attr(result, "flag") <- "d_deaths_Landkreis"
  }else if(!is.na(district) & !is.na(age_group_start) & !is.na(age_group_end) & is.na(date_start) & is.na(date_end)) {
    print("Landkreis Age")
    data %>%
      filter(NeuerTodesfall %in% c(0,1)) %>%
      group_by(Landkreis, Altersgruppe) %>%
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
  }else {
    print("Landkreis Age Datum")
    data %>%
      filter(NeuerTodesfall %in% c(0,1)) %>%
      group_by(Landkreis, Altersgruppe, Meldedatum) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      filter(Landkreis %in% district) %>%
      filter_by_date(date_start, date_end)  %>%
      summarize(Deaths = sum(AnzahlTodesfall)) -> result
    attr(result, "flag") <- "d_deaths_Landkreis-Age-Datum"


  }
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
                          "Baden-WÃ¼rttemberg",
                          "Bayern",
                          "Saarland",
                          "Berlin",
                          "Brandenburg",
                          "Mecklenburg-Vorpommern",
                          "Sachsen",
                          "Sachsen-Anhalt",
                          "ThÃ¼ringen")
  federal_state <- gsub(pattern = "[öÖ]",replacement = "Ã¶", federal_state)
  federal_state <- gsub(pattern = "[äA]",replacement = "Ã¤", federal_state)
  federal_state <- gsub(pattern = "[üÜ]",replacement = "Ã¼", federal_state)
  federal_state <- gsub(pattern = "[ß]",replacement = "ÃY", federal_state)

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
  }else {
    print("Bundesland Age Datum")
    data %>%
      filter(NeuerFall %in% c(0,1)) %>%
      group_by(Bundesland, Altersgruppe, Meldedatum) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      filter(Bundesland %in% federal_state) %>%
      filter_by_date(date_start, date_end)  %>%
      summarize(Infections = sum(AnzahlFall)) -> result
    attr(result, "flag") <- "f_inf_Bundesland-Age-Datum"


  }
  return(result)
}

get_infections_per_district <- function(data, age_group_start = NA, age_group_end = NA, district = NA, date_start = NA, date_end = NA) {

  district <- gsub(pattern = "[öÖ]",replacement = "Ã¶", district)
  district <- gsub(pattern = "[äÄ]",replacement = "Ã¤", district)
  district <- gsub(pattern = "[üÜ]",replacement = "Ã¼", district)
  district <- gsub(pattern = "[ß]",replacement = "ÃY", district)
  # check if district is consistent
  #if(!is.na(district)){
  #  stopifnot("district does not exist" = Landkreis %in% district)
  #}
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
      group_by(Landkreis, Meldedatum) %>%
      filter(Landkreis %in% district) %>%
      filter_by_date(date_start, date_end)  %>%
      summarize(Infections = sum(AnzahlFall)) -> result
    attr(result, "flag") <- "d_inf_Landkreis-Datum"
  } else if(!is.na(district) & is.na(age_group_start) & is.na(age_group_end) & is.na(date_start) & is.na(date_end)){
    print("Landkreis")
    data %>%
      filter(NeuerFall %in% c(0,1)) %>%
      group_by(Landkreis) %>%
      summarize(Infections = sum(AnzahlFall)) %>%
      filter(Landkreis %in% district) -> result
    attr(result, "flag") <- "d_inf_Landkreis"
  }else if(!is.na(district) & !is.na(age_group_start) & !is.na(age_group_end) & is.na(date_start) & is.na(date_end)) {
    print("Landkreis Age")
    data %>%
      filter(NeuerFall %in% c(0,1)) %>%
      group_by(Landkreis, Altersgruppe) %>%
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
  }else {
    print("Landkreis Age Datum")
    data %>%
      filter(NeuerFall %in% c(0,1)) %>%
      group_by(Landkreis, Altersgruppe, Meldedatum) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      filter(Landkreis %in% district) %>%
      filter_by_date(date_start, date_end)  %>%
      summarize(Infections = sum(AnzahlFall)) -> result
    attr(result, "flag") <- "d_inf_Landkreis-Age-Datum"


  }
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
                          "Baden-WÃ¼rttemberg",
                          "Bayern",
                          "Saarland",
                          "Berlin",
                          "Brandenburg",
                          "Mecklenburg-Vorpommern",
                          "Sachsen",
                          "Sachsen-Anhalt",
                          "ThÃ¼ringen")

  federal_state <- gsub(pattern = "[öÖ]",replacement = "Ã¶", federal_state)
  federal_state <- gsub(pattern = "[äÄ]",replacement = "Ã¤", federal_state)
  federal_state <- gsub(pattern = "[üÜ]",replacement = "Ã¼", federal_state)
  federal_state <- gsub(pattern = "[ß]",replacement = "ÃY", federal_state)

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
  }else {
    print("Bundesland Age Datum")
    data %>%
      filter(NeuGenesen %in% c(0,1)) %>%
      group_by(Bundesland, Altersgruppe, Meldedatum) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      filter(Bundesland %in% federal_state) %>%
      filter_by_date(date_start, date_end)  %>%
      summarize(Recovered = sum(AnzahlGenesen)) -> result
    attr(result, "flag") <- "f_rec_Bundesland-Age-Datum"


  }
  return(result)
}

get_recovered_per_district <- function(data, age_group_start = NA, age_group_end = NA, district = NA, date_start = NA, date_end = NA) {
  
  district <- gsub(pattern = "[öÖ]",replacement = "Ã¶", district)
  district <- gsub(pattern = "[äÄ]",replacement = "Ã¤", district)
  district <- gsub(pattern = "[üÜ]",replacement = "Ã¼", district)
  district <- gsub(pattern = "[ß]",replacement = "ÃY", district)
  # check if district is consistent
  #if(!is.na(district)){
  #  print(district)
  #  stopifnot("district does not exist" = data$Landkreis %in% district)
  #}
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
      group_by(Landkreis, Meldedatum) %>%
      filter(Landkreis %in% district) %>%
      filter_by_date(date_start, date_end)  %>%
      summarize(Recovered = sum(AnzahlGenesen)) -> result
    attr(result, "flag") <- "d_rec_Landkreis-Datum"
  } else if(!is.na(district) & is.na(age_group_start) & is.na(age_group_end) & is.na(date_start) & is.na(date_end)){
    print("Landkreis")
    data %>%
      filter(NeuGenesen %in% c(0,1)) %>%
      group_by(Landkreis) %>%
      summarize(Recovered = sum(AnzahlGenesen)) %>%
      filter(Landkreis %in% district) -> result
    attr(result, "flag") <- "d_rec_Landkreis"
  }else if(!is.na(district) & !is.na(age_group_start) & !is.na(age_group_end) & is.na(date_start) & is.na(date_end)) {
    print("Landkreis Age")
    data %>%
      filter(NeuGenesen %in% c(0,1)) %>%
      group_by(Landkreis, Altersgruppe) %>%
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
  }else {
    print("Landkreis Age Datum")
    data %>%
      filter(NeuGenesen %in% c(0,1)) %>%
      group_by(Landkreis, Altersgruppe, Meldedatum) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      filter(Landkreis %in% district) %>%
      filter_by_date(date_start, date_end)  %>%
      summarize(Recovered = sum(AnzahlGenesen)) -> result
    attr(result, "flag") <- "d_rec_Landkreis-Age-Datum"


  }
  return(result)
}


plot_function <- function(data){
  switch(attr(data, "flag"), 
         "f_deaths_Age-Datum" = data %>% 
           ggplot(aes(x = as.Date(Meldedatum), y = Deaths, group = Altersgruppe, fill = Altersgruppe)) +
           xlab("Meldedatum") +
           geom_line() + 
           stat_smooth(method = "loess", se = FALSE),
         "f_deaths_Bundesland-Datum" = data %>% 
           ggplot(aes(x = as.Date(Meldedatum), y = Deaths, group = Bundesland, fill = Bundesland)) +
           xlab("Meldedatum") +
           geom_line() + 
           stat_smooth(method = "loess", se = FALSE),
         "f_deaths_Bundesland" = data %>% 
           ggplot(aes(x = Bundesland, y = Deaths, fill = Bundesland)) + 
           geom_bar(stat= "identity", aes(fill = Bundesland), position = "dodge") + 
           geom_text(aes(label = Deaths),position=position_dodge(width=0.9),vjust = -0.3, color = "black", size = 3.5),
         "f_deaths_Bundesland-Age" = data %>% 
           ggplot(aes(x = Altersgruppe, y = Deaths, fill = Bundesland)) + 
           geom_bar(stat= "identity", aes(fill = Bundesland), position = "dodge") + 
           geom_text(aes(label = Deaths),position=position_dodge(width=0.9),vjust = -0.3, color = "black", size = 3.5),
         "f_deaths_Datum" = data %>% 
           ggplot(aes(x = as.Date(Meldedatum), y = Deaths)) +
           xlab("Meldedatum") +
           geom_line() + 
           stat_smooth(method = "loess", se = FALSE),
         "f_deaths_Age" = data %>% 
           ggplot(aes(x = Altersgruppe, y = Deaths, fill = Bundesland)) + 
           geom_bar(stat= "identity", aes(fill = Bundesland), position = "dodge") +
           geom_text(aes(label = Deaths),position=position_dodge(width=0.9),vjust = -0.3, color = "black", size = 3.5),
         "f_deaths_Bundesland-Age-Datum" = "Datum Alter Bundesland",
         
         "d_deaths_Age-Datum" = data %>% 
           ggplot(aes(x = as.Date(Meldedatum), y = Deaths, group = Altersgruppe, fill = Altersgruppe)) +
           xlab("Meldedatum") +
           geom_line() + 
           stat_smooth(method = "loess", se = FALSE),
         "d_deaths_Landkreis-Datum" = data %>% 
           ggplot(aes(x = as.Date(Meldedatum), y = Deaths, group = Landkreis, fill = Landkreis)) +
           xlab("Meldedatum") +
           geom_line() + 
           stat_smooth(method = "loess", se = FALSE),
         "d_deaths_Landkreis" = data %>% 
           ggplot(aes(x = Landkreis, y = Deaths, fill = Landkreis)) + 
           geom_bar(stat= "identity", aes(fill = Landkreis), position = "dodge") + 
           geom_text(aes(label = Deaths),position=position_dodge(width=0.9),vjust = -0.3, color = "black", size = 3.5),
         "d_deaths_Landkreis-Age" = data %>% 
           ggplot(aes(x = Altersgruppe, y = Deaths, fill = Landkreis)) + 
           geom_bar(stat= "identity", aes(fill = Landkreis), position = "dodge") + 
           geom_text(aes(label = Deaths),position=position_dodge(width=0.9),vjust = -0.3, color = "black", size = 3.5),
         "d_deaths_Datum" = data %>% 
           ggplot(aes(x = as.Date(Meldedatum), y = Deaths)) +
           xlab("Meldedatum") +
           geom_line() + 
           stat_smooth(method = "loess", se = FALSE),
         "d_deaths_Age" = data %>% 
           ggplot(aes(x = Altersgruppe, y = Deaths, fill = Altersgruppe)) + 
           geom_bar(stat= "identity", aes(fill = Altersgruppe), position = "dodge")+
           geom_text(aes(label = Deaths),position=position_dodge(width=0.9),vjust = -0.3, color = "black", size = 3.5),
         "d_deaths_Landkreis-Age-Datum" = "Datum Alter Landkreis",
         
         "f_inf_Age-Datum" = data %>% 
           ggplot(aes(x = as.Date(Meldedatum), y = Infections, group = Altersgruppe, fill = Altersgruppe)) +
           xlab("Meldedatum") +
           geom_line() + 
           stat_smooth(method = "loess", se = FALSE),
         "f_inf_Bundesland-Datum" = data %>% 
           ggplot(aes(x = as.Date(Meldedatum), y = Deaths, group = Bundesland, fill = Bundesland)) +
           xlab("Meldedatum") +
           geom_line() + 
           stat_smooth(method = "loess", se = FALSE),
         "f_inf_Bundesland" = data %>% 
           ggplot(aes(x = Bundesland, y = Infections, fill = Bundesland)) + 
           geom_bar(stat= "identity", aes(fill = Bundesland), position = "dodge") + 
           geom_text(aes(label = Infections),position=position_dodge(width=0.9),vjust = -0.3, color = "black", size = 3.5),
         "f_inf_Bundesland-Age" = data %>% 
           ggplot(aes(x = Altersgruppe, y = Infections, fill = Bundesland)) + 
           geom_bar(stat= "identity", aes(fill = Bundesland), position = "dodge") + 
           geom_text(aes(label = Infections),position=position_dodge(width=0.9),vjust = -0.3, color = "black", size = 3.5),
         "f_inf_Datum" = data %>% 
           ggplot(aes(x = as.Date(Meldedatum), y = Infections)) +
           xlab("Meldedatum") +
           geom_line() + 
           stat_smooth(method = "loess", se = FALSE),
         "f_inf_Age" = data %>% 
           ggplot(aes(x = Altersgruppe, y = Infections, fill = Bundesland)) + 
           geom_bar(stat= "identity", aes(fill = Bundesland), position = "dodge")+
           geom_text(aes(label = Deaths),position=position_dodge(width=0.9),vjust = -0.3, color = "black", size = 3.5),
         "f_inf_Bundesland-Age-Datum" = "Datum Alter Bundesland",
         
         "d_inf_Age-Datum" = data %>% 
           ggplot(aes(x = as.Date(Meldedatum), y = Infections, group = Altersgruppe, fill = Altersgruppe)) +
           xlab("Meldedatum") +
           geom_line() + 
           stat_smooth(method = "loess", se = FALSE),
         "d_inf_Landkreis-Datum" = data %>% 
           ggplot(aes(x = as.Date(Meldedatum), y = Deaths, group = Landkreis, fill = Landkreis)) +
           xlab("Meldedatum") +
           geom_line() + 
           stat_smooth(method = "loess", se = FALSE),
         "d_inf_Landkreis" = data %>% 
           ggplot(aes(x = Landkreis, y = Infections, fill = Landkreis)) + 
           geom_bar(stat= "identity", aes(fill = Landkreis), position = "dodge") + 
           geom_text(aes(label = Infections),position=position_dodge(width=0.9),vjust = -0.3, color = "black", size = 3.5),
         "d_inf_Landkreis-Age" = data %>% 
           ggplot(aes(x = Altersgruppe, y = Infections, fill = Landkreis)) + 
           geom_bar(stat= "identity", aes(fill = Landkreis), position = "dodge") + 
           geom_text(aes(label = Infections),position=position_dodge(width=0.9),vjust = -0.3, color = "black", size = 3.5),
         "d_inf_Datum" = data %>% 
           ggplot(aes(x = as.Date(Meldedatum), y = Infections)) +
           xlab("Meldedatum") +
           geom_line() + 
           stat_smooth(method = "loess", se = FALSE),
         "d_inf_Age" = data %>% 
           ggplot(aes(x = Altersgruppe, y = Infections, fill = Altersgruppe)) + 
           geom_bar(stat= "identity", aes(fill = Altersgruppe), position = "dodge")+
           geom_text(aes(label = Deaths),position=position_dodge(width=0.9),vjust = -0.3, color = "black", size = 3.5),
         "d_inf_Landkreis-Age-Datum" = "Datum Alter Landkreis",
         
         "f_rec_Age-Datum" = data %>% 
           ggplot(aes(x = as.Date(Meldedatum), y = Recovered, group = Altersgruppe, fill = Altersgruppe)) +
           xlab("Meldedatum") +
           geom_line() + 
           stat_smooth(method = "loess", se = FALSE),
         "f_rec_Bundesland-Datum" = data %>% 
           ggplot(aes(x = as.Date(Meldedatum), y = Deaths, group = Bundesland, fill = Bundesland)) +
           xlab("Meldedatum") +
           geom_line() + 
           stat_smooth(method = "loess", se = FALSE),
         "f_rec_Bundesland" = data %>% 
           ggplot(aes(x = Bundesland, y = Recovered, fill = Bundesland)) + 
           geom_bar(stat= "identity", aes(fill = Bundesland), position = "dodge") + 
           geom_text(aes(label = Recovered),position=position_dodge(width=0.9),vjust = -0.3, color = "black", size = 3.5),
         "f_rec_Bundesland-Age" = data %>% 
           ggplot(aes(x = Altersgruppe, y = Recovered, fill = Bundesland)) + 
           geom_bar(stat= "identity", aes(fill = Bundesland), position = "dodge") + 
           geom_text(aes(label = Recovered),position=position_dodge(width=0.9),vjust = -0.3, color = "black", size = 3.5),
         "f_rec_Datum" = data %>% 
           ggplot(aes(x = as.Date(Meldedatum), y = Recovered)) +
           xlab("Meldedatum") +
           geom_line() + 
           stat_smooth(method = "loess", se = FALSE),
         "f_rec_Age" = data %>% 
           ggplot(aes(x = Altersgruppe, y = Recovered, fill = Bundesland)) + 
           geom_bar(stat= "identity", aes(fill = Bundesland), position = "dodge")+
           geom_text(aes(label = Deaths),position=position_dodge(width=0.9),vjust = -0.3, color = "black", size = 3.5),
         "f_rec_Bundesland-Age-Datum" = "Datum Alter Bundesland",
         
         "d_rec_Age-Datum" = data %>% 
           ggplot(aes(x = as.Date(Meldedatum), y = Recovered, group = Altersgruppe, fill = Altersgruppe)) +
           xlab("Meldedatum") +
           geom_line() + 
           stat_smooth(method = "loess", se = FALSE),
         "d_rec_Landkreis-Datum" = data %>% 
           ggplot(aes(x = as.Date(Meldedatum), y = Deaths, group = Landkreis, fill = Landkreis)) +
           xlab("Meldedatum") +
           geom_line() + 
           stat_smooth(method = "loess", se = FALSE),
         "d_rec_Landkreis-Age" = data %>% 
           ggplot(aes(x = Landkreis, y = Recovered, fill = Landkreis)) + 
           geom_bar(stat= "identity", aes(fill = Landkreis), position = "dodge") + 
           geom_text(aes(label = Recovered),position=position_dodge(width=0.9),vjust = -0.3, color = "black", size = 3.5),
         "d_rec_Datum" = data %>% 
           ggplot(aes(x = Altersgruppe, y = Recovered, fill = Landkreis)) + 
           geom_bar(stat= "identity", aes(fill = Landkreis), position = "dodge") + 
           geom_text(aes(label = Recovered),position=position_dodge(width=0.9),vjust = -0.3, color = "black", size = 3.5),
         "d_rec_Datum" = data %>% 
           ggplot(aes(x = as.Date(Meldedatum), y = Recovered)) +
           xlab("Meldedatum") +
           geom_line() + 
           stat_smooth(method = "loess", se = FALSE),
         "d_rec_Age" = data %>% 
           ggplot(aes(x = Altersgruppe, y = Recovered, fill = Altersgruppe)) + 
           geom_bar(stat= "identity", aes(fill = Altersgruppe), position = "dodge")+
           geom_text(aes(label = Deaths),position=position_dodge(width=0.9),vjust = -0.3, color = "black", size = 3.5),
         "d_rec_Landkreis-Age-Datum" = "Datum Alter Landkreis")
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
# example
incidences_df <- get_incidence_per_district(cov_data, 7)
incidence_correlation_pairs <- get_correlation_for_incidence_pairs(incidences_df)
incidence_correlation_pairs

lk_ids[30:length(lk_ids)]
incidence_correlation_pairs %>%
  arrange(desc(Correlation))
