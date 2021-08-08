#install.packages("RcppRoll")
library(tidyverse)
library(dplyr)
library(stringr)
library(lubridate)
library(RcppRoll)

cov_data <- read.csv("R/data.csv")



source("R/population.R")

# filter by date (Meldedatum)
#
# data: df with covid data
# date_start: "2020-11-05"
# date_end: "2020-11-08"
#
# outputs the rows of the input df that are between the specified dates

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

get_deaths_per_federal_states <- function(data, age_group_start = NA, age_group_end = NA, federal_state = NA, date_start = NA, date_end = NA) {

  federal_state <- gsub(pattern = "[öÖ]",replacement = "Ã¶", federal_state)
  federal_state <- gsub(pattern = "[äÄ]",replacement = "Ã¤", federal_state)
  federal_state <- gsub(pattern = "[üÜ]",replacement = "Ã¼", federal_state)
  federal_state <- gsub(pattern = "[ß]",replacement = "ÃY", federal_state)



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
  result <- ungroup(result)
  return(result)
}

get_deaths_per_district <- function(data, age_group_start = NA, age_group_end = NA, district = NA, date_start = NA, date_end = NA){


  district <- gsub(pattern = "[öÖ]",replacement = "Ã¶", district)
  district <- gsub(pattern = "[äÄ]",replacement = "Ã¤", district)
  district <- gsub(pattern = "[üÜ]",replacement = "Ã¼", district)
  district <- gsub(pattern = "[ß]",replacement = "ÃY", district)


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
  }else if(!is.na(district) & !is.na(age_group_start) & !is.na(age_group_end) & !is.na(date_start) & !is.na(date_end)){
    print("Landkreis Age Datum")
    data %>%
      filter(NeuerTodesfall %in% c(0,1)) %>%
      group_by(Landkreis, Altersgruppe, Meldedatum) %>%
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
  result <- ungroup(result)
  return(result)
}

get_infections_per_federal_states <- function(data, age_group_start = NA, age_group_end = NA, federal_state = NA, date_start = NA, date_end = NA) {


  federal_state <- gsub(pattern = "[öÖ]",replacement = "Ã¶", federal_state)
  federal_state <- gsub(pattern = "[äÄ]",replacement = "Ã¤", federal_state)
  federal_state <- gsub(pattern = "[üÜ]",replacement = "Ã¼", federal_state)
  federal_state <- gsub(pattern = "[ß]",replacement = "ÃY", federal_state)

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
  result <- ungroup(result)
  return(result)
}

get_infections_per_district <- function(data, age_group_start = NA, age_group_end = NA, district = NA, date_start = NA, date_end = NA) {

  district <- gsub(pattern = "[öÖ]",replacement = "Ã¶", district)
  district <- gsub(pattern = "[äÄ]",replacement = "Ã¤", district)
  district <- gsub(pattern = "[üÜ]",replacement = "Ã¼", district)
  district <- gsub(pattern = "[ß]",replacement = "ÃY", district)


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
  }else if(is.na(district) & !is.na(age_group_start) & !is.na(age_group_end) & is.na(date_start) & is.na(date_end)){
    print("Age 2")
    print(age_group_start)
    data %>%
      filter(NeuerFall %in% c(0,1)) %>%
      group_by(Landkreis) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      summarize(Infections = sum(AnzahlFall)) -> result
  }else if(!is.na(district) & !is.na(age_group_start) & !is.na(age_group_end) & !is.na(date_start) & !is.na(date_end)){
    print("Landkreis Age Datum")
    data %>%
      filter(NeuerFall %in% c(0,1)) %>%
      group_by(Landkreis, Altersgruppe, Meldedatum) %>%
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
  result <- ungroup(result)
  return(result)
}

get_recovered_per_federal_states <- function(data, age_group_start = NA, age_group_end = NA, federal_state = NA, date_start = NA, date_end = NA) {


  federal_state <- gsub(pattern = "[öÖ]",replacement = "Ã¶", federal_state)
  federal_state <- gsub(pattern = "[äÄ]",replacement = "Ã¤", federal_state)
  federal_state <- gsub(pattern = "[üÜ]",replacement = "Ã¼", federal_state)
  federal_state <- gsub(pattern = "[ß]",replacement = "ÃY", federal_state)

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
  result <- ungroup(result)
  return(result)
}

get_recovered_per_district <- function(data, age_group_start = NA, age_group_end = NA, district = NA, date_start = NA, date_end = NA) {


  district <- gsub(pattern = "[öÖ]",replacement = "Ã¶", district)
  district <- gsub(pattern = "[äÄ]",replacement = "Ã¤", district)
  district <- gsub(pattern = "[üÜ]",replacement = "Ã¼", district)
  district <- gsub(pattern = "[ß]",replacement = "ÃY", district)


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
  }else if(is.na(district) & is.na(date_start) & is.na(date_end)){
    print("Age 2")
    data %>%
      filter(NeuGenesen %in% c(0,1)) %>%
      group_by(Landkreis) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      summarize(Recovered = sum(AnzahlGenesen)) -> result
  }else if(!is.na(district) & !is.na(age_group_start) & !is.na(age_group_end) & !is.na(date_start) & !is.na(date_end)){
    print("Landkreis Age Datum")
    data %>%
      filter(NeuGenesen %in% c(0,1)) %>%
      group_by(Landkreis, Altersgruppe, Meldedatum) %>%
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

get_infections_overall <- function(data, age_group_start = NA, age_group_end = NA, district = NA, date_start = NA, date_end = NA){

  if(!is.na(district)){
    stopifnot("district does not exist" = is.element(district, data$Landkreis))
  }

  if(is.na(district) & is.na(age_group_start) & is.na(age_group_end) & !is.na(date_start) & !is.na(date_end)){
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
  result <- ungroup(result)
  return(result)
}

# appends the number of days it took to report the infection to a healthdepartment
#
# data: df with covid data
#
# output: df with a column date_diff

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
# outputs: a summarized dataframe of the mortalityrate if a age_group is beeing filtered
# outputs: a plottable dataframe of mortalityrate for every case in data

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

# a function to automatically plot your dataframe from any get_x_per_y output
#
# data: a df from get_x_per_y
#
# output: a already predefined custom plot for data

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
         "f_deaths_Bundesland-Age-Datum" = "Not Plottable",
         "f_deaths" = data %>%
           ggplot(aes(x = Bundesland, y = Deaths, fill = Bundesland)) +
           geom_bar(stat= "identity", aes(fill = Bundesland), position = "dodge") +
           geom_text(aes(label = Deaths),position=position_dodge(width=0.9),vjust = -0.3, color = "black", size = 3.5),

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
         "d_deaths_Landkreis-Age-Datum" = "Not Plottable",
         "d_deaths" = data %>%
           ggplot(aes(x = as.Date(Meldedatum), y = Deaths)) +
           geom_bar(stat= "identity", position = "dodge", fill = "steelblue"),

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
         "f_inf_Bundesland-Age-Datum" = "Not Plottable",
         "f_inf" = data %>%
           ggplot(aes(x = Bundesland, y = Infections, fill = Bundesland)) +
           geom_bar(stat= "identity", aes(fill = Bundesland), position = "dodge") +
           geom_text(aes(label = Infections),position=position_dodge(width=0.9),vjust = -0.3, color = "black", size = 3.5),

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
         "d_inf_Landkreis-Age-Datum" = "Not Plottable",
         "d_inf" = data %>%
           ggplot(aes(x = as.Date(Meldedatum), y = Infections)) +
           geom_bar(stat= "identity", position = "dodge", fill = "steelblue"),

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
         "f_rec_Bundesland-Age-Datum" = "Not Plottable",
         "f_rec" = data %>%
           ggplot(aes(x = Bundesland, y = Recovered, fill = Bundesland)) +
           geom_bar(stat= "identity", aes(fill = Bundesland), position = "dodge") +
           geom_text(aes(label = Recovered),position=position_dodge(width=0.9),vjust = -0.3, color = "black", size = 3.5),

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
         "d_rec_Landkreis-Age-Datum" = "Not Plottable",
         "d_rec" = data %>%
           ggplot(aes(x = as.Date(Meldedatum), y = Recovered)) +
           geom_bar(stat= "identity", position = "dodge", fill = "steelblue"),

         "DE_Fallsterblichkeit" = data %>%
           ggplot(aes(x = as.Date(Meldedatum), y = Fallsterblichkeit)) +
           xlab("Meldedatum") +
           geom_bar(stat= "identity", position = "dodge", fill = "steelblue"),
         "DE_Fallsterblichkeit_age" = "Not Plottable",
         "date_diff" = data %>%
           ggplot(aes(x = as.Date(Meldedatum), y = date_diff)) +
           xlab("Meldedatum") +
           ylab("Tage bis zum Meldedatum") +
           geom_line()
         )
}

# get every unclean data in your covid data
#
# data: df with covid data
#
# output: returns a dataframe with every unclean row in your data

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
