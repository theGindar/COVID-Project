library(tidyverse)
library(dplyr)
library(stringr)

cov_data <- read.csv("R/RKI_COVID19.csv")

<<<<<<< Updated upstream
# returns death cases
# data: df with covid data
# age_group_start: the lower bound age group, e. g. "A05"
# age_group_end: the upper bound age group, e. g. "A59"
# federal_state: the federal state to filter by
# district: district to filter by, can be the name or id of the district
get_deaths <- function(data, age_group_start = "A00", age_group_end = "A80", federal_state, district) {
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

  # check if federal state is consistent
  if(!is.na(federal_state)) {
    stopifnot("federal state does not exist" = federal_state %in% federal_state_names)
  }

  # check if district is consistent
  if(!is.na(district)) {
    stopifnot("invalid district" = grepl("^[A-Za-z\\-Ã¤Ã¶Ã¼[:space:]]+$", district, perl = T) | grepl("^[\\d]{5}$", district, perl = T))
  }

  district_is_id = FALSE
  if(grepl("^[\\d]{5}$", district, perl = T)) {
    district_is_id = TRUE
  } else {
    # add prefix to district name
    district = c("LK ", district)
  }
  if(is.na(federal_state) & is.na(district)) {
    data %>%
      group_by(Refdatum, Altersgruppe, Bundesland) %>%
      summarize(Todesfaelle = sum(AnzahlTodesfall)) %>%
      filter_by_age_group(age_group_start, age_group_end) -> result
  }
  if(is.na(district)) {
    data %>%
      group_by(Refdatum, Altersgruppe, Bundesland) %>%
      summarize(Todesfaelle = sum(AnzahlTodesfall)) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      filter(Bundesland == federal_state) -> result
  } else if(district_is_id) {
    data %>%
      group_by(Refdatum, Altersgruppe, Landkreis) %>%
      summarize(Todesfaelle = sum(AnzahlTodesfall)) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      filter(IdLandkreis == district)-> result
  } else {
    data %>%
      group_by(Refdatum, Altersgruppe, Landkreis) %>%
      summarize(Todesfaelle = sum(AnzahlTodesfall)) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      filter(Landkreis == district)-> result
  }
=======
# filter by date (Meldedatum)
#
# data: df with covid data
# date_start: "2020-11-05"
# date_end: "2020-11-08"
#
# outputs the rows of the input df that are between the specified dates
>>>>>>> Stashed changes

  return(result)
  # Wenn nach datum gesucht wird, dann auch nach Meldedatum gruppieren
}

<<<<<<< Updated upstream

=======
cov_data %>% 
  filter_by_date(date_start = "2020/11/08", date_end = "2020/11/10")
>>>>>>> Stashed changes
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

<<<<<<< Updated upstream
cov_data %>%
  distinct(Landkreis)
=======

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
  # check if federal state is consistent
  if(!is.na(federal_state)){
    stopifnot("federal state does not exist" = federal_state %in% federal_state_names)
  }
  if(is.na(federal_state) & !is.na(age_group_start) & !is.na(age_group_end) & !is.na(date_start) & !is.na(date_end)) {
    print("Age Datum")
    data %>%
      filter(NeuerTodesfall %in% c(0,1)) %>%
      group_by(Bundesland, Altersgruppe) %>%
      filter_by_age_group(age_group_start, age_group_end)  %>% 
      filter_by_date(date_start, date_end) %>% 
      summarize(Deaths = sum(AnzahlTodesfall)) -> result
  } else if(!is.na(federal_state) & is.na(age_group_start) & is.na(age_group_end) & !is.na(date_start) & !is.na(date_end)){
    print("Bundesland Datum")
    data %>%
      filter(NeuerTodesfall %in% c(0,1)) %>%
      group_by(Bundesland, Altersgruppe) %>%
      filter_by_date(date_start, date_end) %>% 
      filter(Bundesland %in% federal_state) %>% 
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
      filter(Bundesland %in% federal_state) %>% 
      filter_by_age_group(age_group_start, age_group_end) %>%
      summarize(Deaths = sum(AnzahlTodesfall)) -> result
  }else if(is.na(federal_state) & is.na(age_group_start) & is.na(age_group_end) & !is.na(date_start) & !is.na(date_end)){
    print("Datum")
    data %>%
      filter(NeuerTodesfall %in% c(0,1)) %>%
      group_by(Bundesland, Altersgruppe) %>%
      filter_by_date(date_start, date_end)  %>%
      summarize(Deaths = sum(AnzahlTodesfall)) -> result
  }else if(is.na(federal_state) & !is.na(age_group_start) & !is.na(age_group_end) & is.na(date_start) & is.na(date_end)){
    print("Age")
    data %>%
      filter(NeuerTodesfall %in% c(0,1)) %>%
      group_by(Bundesland, Altersgruppe) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      summarize(Deaths = sum(AnzahlTodesfall)) -> result
  }else {
    print("Bundesland Age Datum")
    data %>%
      filter(NeuerTodesfall %in% c(0,1)) %>%
      group_by(Bundesland, Altersgruppe) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      filter_by_date(date_start, date_end)  %>%
      filter(Bundesland %in% federal_state)  %>% 
      summarize(Deaths = sum(AnzahlTodesfall)) -> result
    
    
  }
  return(result)
}

get_deaths_per_district <- function(data, age_group_start = NA, age_group_end = NA, district = NA, date_start = NA, date_end = NA){
  district <- gsub(pattern = "[öÖ]",replacement = "Ã¶", district)
  district <- gsub(pattern = "[äÄ]",replacement = "Ã¤", district)
  district <- gsub(pattern = "[üÜ]",replacement = "Ã¼", district)
  district_names <- distinct(cov_data, Landkreis)
  
  if(!is.na(district)) {
    stopifnot("not a correct district" = any(district %in% cov_data$Landkreis))
  }
  
  if(is.na(district) & !is.na(age_group_start) & !is.na(age_group_end) & !is.na(date_start) & !is.na(date_end)) {
    print("Age Datum")
    data %>%
      filter(NeuerTodesfall %in% c(0,1)) %>%
      group_by(Landkreis, Altersgruppe) %>%
      filter_by_age_group(age_group_start, age_group_end) -> result
      filter_by_date(date_start, date_end)  %>% 
      summarize(Todesfälle = sum(AnzahlTodesfall)) -> result
  } else if(!is.na(district) & is.na(age_group_start) & is.na(age_group_end) & !is.na(date_start) & !is.na(date_end)){
    print("District Datum")
    data %>%
      filter(NeuerTodesfall %in% c(0,1)) %>%
      group_by(Landkreis, Altersgruppe) %>%
      filter_by_date(date_start, date_end) %>% 
      filter(Landkreis %in% district)  %>% 
      summarize(Todesfälle = sum(AnzahlTodesfall)) -> result
  } else if(!is.na(district) & is.na(age_group_start) & is.na(age_group_end) & is.na(date_start) & is.na(date_end)){
    print("District ")
    data %>%
      filter(NeuerTodesfall %in% c(0,1)) %>%
      group_by(Landkreis, Altersgruppe) %>%
      summarize(Todesfälle = sum(AnzahlTodesfall)) %>%
      filter(Landkreis %in% district) -> result
  } else if(!is.na(district) & !is.na(age_group_start) & !is.na(age_group_end) & is.na(date_start) & is.na(date_end)) {
    print("District Age")
    data %>%
      filter(NeuerTodesfall %in% c(0,1)) %>%
      group_by(Landkreis, Altersgruppe) %>%
      summarize(Todesfälle = sum(AnzahlTodesfall)) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      filter(Landkreis %in% district) -> result
  }else if(is.na(district) & is.na(age_group_start) & is.na(age_group_end) & !is.na(date_start) & !is.na(date_end)){
    print("Datum")
    data %>%
      filter(NeuerTodesfall %in% c(0,1)) %>%
      group_by(Landkreis, Altersgruppe) %>%
      filter_by_date(date_start, date_end) %>%
      summarize(Todesfälle = sum(AnzahlTodesfall)) -> result
  }else if(is.na(district) & is.na(age_group_start) & is.na(age_group_end) & is.na(date_start) & is.na(date_end)){
    print("Keins")
    data %>%
      filter(NeuerTodesfall %in% c(0,1)) %>%
      group_by(Landkreis, Altersgruppe) %>%
      summarize(Todesfälle = sum(AnzahlTodesfall)) -> result
  }else if(is.na(district) & !is.na(age_group_start) & !is.na(age_group_end) & is.na(date_start) & is.na(date_end)){
    print("Age ")
    data %>%
      filter(NeuerTodesfall %in% c(0,1)) %>%
      group_by(Landkreis, Altersgruppe) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      summarize(Todesfälle = sum(AnzahlTodesfall)) -> result
  }else {
    print("District Age Datum")
    data %>%
      filter(NeuerTodesfall %in% c(0,1)) %>%
      group_by(Landkreis, Altersgruppe) %>%
      filter_by_date(date_start, date_end)  %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      filter(Landkreis %in% district)  %>% 
      summarize(Todesfälle = sum(AnzahlTodesfall)) -> result
    
    
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
  federal_state <- gsub(pattern = "[äÄ]",replacement = "Ã¤", federal_state)
  federal_state <- gsub(pattern = "[üÜ]",replacement = "Ã¼", federal_state)

  # check if federal state is consistent
  if(!is.na(federal_state)){
    stopifnot("federal state does not exist" = federal_state %in% federal_state_names)
  }
  if(is.na(federal_state) & !is.na(age_group_start) & !is.na(age_group_end) & !is.na(date_start) & !is.na(date_end)) {
    print("Age Datum")
    data %>%
      filter(NeuerFall %in% c(0,1)) %>%
      group_by(Bundesland, Altersgruppe) %>%
      filter_by_age_group(age_group_start, age_group_end)  %>% 
      filter_by_date(date_start, date_end) %>% 
      summarize(Infections = sum(AnzahlFall)) -> result
  } else if(!is.na(federal_state) & is.na(age_group_start) & is.na(age_group_end) & !is.na(date_start) & !is.na(date_end)){
    print("Bundesland Datum")
    data %>%
      filter(NeuerFall %in% c(0,1)) %>%
      group_by(Bundesland, Altersgruppe) %>%
      filter_by_date(date_start, date_end) %>% 
      filter(Bundesland %in% federal_state)  %>% 
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
      filter(Bundesland %in% federal_state) %>% 
      filter_by_age_group(age_group_start, age_group_end)  %>% 
      summarize(Infections = sum(AnzahlFall)) -> result
  }else if(is.na(federal_state) & is.na(age_group_start) & is.na(age_group_end) & !is.na(date_start) & !is.na(date_end)){
    print("Datum")
    data %>%
      filter(NeuerFall %in% c(0,1)) %>%
      group_by(Bundesland, Altersgruppe) %>%
      filter_by_date(date_start, date_end)  %>%
      summarize(Infections = sum(AnzahlFall)) -> result
  }else if(is.na(federal_state) & !is.na(age_group_start) & !is.na(age_group_end) & is.na(date_start) & is.na(date_end)){
    print("Age")
    data %>%
      filter(NeuerFall %in% c(0,1)) %>%
      group_by(Bundesland, Altersgruppe) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      summarize(Infections = sum(AnzahlFall)) -> result
  }else {
    print("Bundesland Age Datum")
    data %>%
      filter(NeuerFall %in% c(0,1)) %>%
      group_by(Bundesland, Altersgruppe) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      filter_by_date(date_start, date_end)  %>%
      filter(Bundesland %in% federal_state)  %>% 
      summarize(Infections = sum(AnzahlFall)) -> result


  }
  return(result)
}

get_infections_per_district <- function(data, age_group_start = NA, age_group_end = NA, district = NA, date_start = NA, date_end = NA) {
  district <- gsub(pattern = "[öÖ]",replacement = "Ã¶", district)
  district <- gsub(pattern = "[äÄ]",replacement = "Ã¤", district)
  district <- gsub(pattern = "[üÜ]",replacement = "Ã¼", district)
  district_names <- distinct(cov_data, Landkreis)
  
  if(!is.na(district)) {
    stopifnot("not a correct district" = any(district %in% cov_data$Landkreis))
  }

  if(is.na(district) & !is.na(age_group_start) & !is.na(age_group_end) & !is.na(date_start) & !is.na(date_end)) {
    print("Age Datum")
    data %>%
      filter(NeuerFall %in% c(0,1)) %>%
      group_by(Landkreis, Altersgruppe) %>%
      filter_by_age_group(age_group_start, age_group_end) %>% 
      filter_by_date(date_start, date_end)  %>% 
      summarize(Infections = sum(AnzahlFall)) -> result
  } else if(!is.na(district) & is.na(age_group_start) & is.na(age_group_end) & !is.na(date_start) & !is.na(date_end)){
    print("District Datum")
    data %>%
      filter(NeuerFall %in% c(0,1)) %>%
      group_by(Landkreis, Altersgruppe) %>%
      filter_by_date(date_start, date_end) %>% 
      filter(Landkreis %in% district) %>% 
      summarize(Infections = sum(AnzahlFall)) -> result
  } else if(!is.na(district) & is.na(age_group_start) & is.na(age_group_end) & is.na(date_start) & is.na(date_end)){
    print("District ")
    data %>%
      filter(NeuerFall %in% c(0,1)) %>%
      group_by(Landkreis, Altersgruppe) %>%
      summarize(Infections = sum(AnzahlFall)) %>%
      filter(Landkreis %in% district) -> result
  } else if(!is.na(district) & !is.na(age_group_start) & !is.na(age_group_end) & is.na(date_start) & is.na(date_end)) {
    print("District Age")
    data %>%
      filter(NeuerFall %in% c(0,1)) %>%
      group_by(Landkreis, Altersgruppe) %>%
      summarize(Infections = sum(AnzahlFall)) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      filter(Landkreis %in% district) -> result
  }else if(is.na(district) & is.na(age_group_start) & is.na(age_group_end) & !is.na(date_start) & !is.na(date_end)){
    print("Datum")
    data %>%
      filter(NeuerFall %in% c(0,1)) %>%
      group_by(Landkreis, Altersgruppe) %>%
      filter_by_date(date_start, date_end)  %>%
      summarize(Infections = sum(AnzahlFall)) -> result
  }else if(is.na(district) & is.na(age_group_start) & is.na(age_group_end) & is.na(date_start) & is.na(date_end)){
    print("Keins")
    data %>%
      filter(NeuerFall %in% c(0,1)) %>%
      group_by(Landkreis, Altersgruppe) %>%
      summarize(Infections = sum(AnzahlFall)) -> result
  }else if(is.na(district) & !is.na(age_group_start) & !is.na(age_group_end) & is.na(date_start) & is.na(date_end)){
    print("Age ")
    data %>%
      filter(NeuerFall %in% c(0,1)) %>%
      group_by(Landkreis, Altersgruppe) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      summarize(Infections = sum(AnzahlFall)) -> result
  }else {
    print("District Age Datum")
    data %>%
      filter(NeuerFall %in% c(0,1)) %>%
      group_by(Landkreis, Altersgruppe) %>%
      filter_by_date(date_start, date_end)  %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      filter(Landkreis %in% district) %>% 
      summarize(Infections = sum(AnzahlFall)) -> result


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
  
  # check if federal state is consistent
  if(!is.na(federal_state)){
    stopifnot("federal state does not exist" = federal_state %in% federal_state_names)
  }
  if(is.na(federal_state) & !is.na(age_group_start) & !is.na(age_group_end) & !is.na(date_start) & !is.na(date_end)) {
    print("Age Datum")
    data %>%
      filter(NeuGenesen %in% c(0,1)) %>%
      group_by(Bundesland, Altersgruppe) %>%
      summarize(Recovered = sum(AnzahlGenesen)) %>%
      filter_by_age_group(age_group_start, age_group_end) %>% 
      filter_by_date(date_start, date_end) %>% 
      summarize(Recovered = sum(AnzahlGenesen)) -> result
  } else if(!is.na(federal_state) & is.na(age_group_start) & is.na(age_group_end) & !is.na(date_start) & !is.na(date_end)){
    print("Bundesland Datum")
    data %>%
      filter(NeuGenesen %in% c(0,1)) %>%
      group_by(Bundesland, Altersgruppe) %>%
      filter_by_date(date_start, date_end) %>% 
      filter(Bundesland %in% federal_state)  %>% 
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
      filter(Bundesland %in% federal_state) %>% 
      filter_by_age_group(age_group_start, age_group_end) %>%
      summarize(Recovered = sum(AnzahlGenesen)) -> result
  }else if(is.na(federal_state) & is.na(age_group_start) & is.na(age_group_end) & !is.na(date_start) & !is.na(date_end)){
    print("Datum")
    data %>%
      filter(NeuGenesen %in% c(0,1)) %>%
      group_by(Bundesland, Altersgruppe) %>%
      filter_by_date(date_start, date_end)  %>%
      summarize(Recovered = sum(AnzahlGenesen)) -> result
  }else if(is.na(federal_state) & !is.na(age_group_start) & !is.na(age_group_end) & is.na(date_start) & is.na(date_end)){
    print("Age")
    data %>%
      filter(NeuGenesen %in% c(0,1)) %>%
      group_by(Bundesland, Altersgruppe) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      summarize(Recovered = sum(AnzahlGenesen)) -> result
  }else {
    print("Bundesland Age Datum")
    data %>%
      filter(NeuGenesen %in% c(0,1)) %>%
      group_by(Bundesland, Altersgruppe) %>%
      filter_by_date(date_start, date_end)  %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      filter(Bundesland %in% federal_state) %>% 
      summarize(Recovered = sum(AnzahlGenesen)) -> result
    
    
  }
  return(result)
}

get_recovered_per_district <- function(data, age_group_start = NA, age_group_end = NA, district = NA, date_start = NA, date_end = NA) {
  district <- gsub(pattern = "[öÖ]",replacement = "Ã¶", district)
  district <- gsub(pattern = "[äÄ]",replacement = "Ã¤", district)
  district <- gsub(pattern = "[üÜ]",replacement = "Ã¼", district)
  district_names <- distinct(cov_data, Landkreis)
  
  if(!is.na(district)) {
    stopifnot("not a correct district" = any(district %in% cov_data$Landkreis))
  }
  
  if(is.na(district) & !is.na(age_group_start) & !is.na(age_group_end) & !is.na(date_start) & !is.na(date_end)) {
    print("Age Datum")
    data %>%
      filter(NeuGenesen %in% c(0,1)) %>%
      group_by(Meldedatum, Altersgruppe) %>%
      filter_by_date(date_start, date_end)  %>% 
      filter_by_age_group(age_group_start, age_group_end)  %>% 
      summarize(Recovered = sum(AnzahlGenesen)) -> result
  } else if(!is.na(district) & is.na(age_group_start) & is.na(age_group_end) & !is.na(date_start) & !is.na(date_end)){
    print("District Datum")
    data %>%
      filter(NeuGenesen %in% c(0,1)) %>%
      group_by(Landkreis, Meldedatum) %>%
      filter_by_date(date_start, date_end) %>% 
      filter(Landkreis %in% district) %>% 
      summarize(Recovered = sum(AnzahlGenesen)) -> result
  } else if(!is.na(district) & is.na(age_group_start) & is.na(age_group_end) & is.na(date_start) & is.na(date_end)){
    print("District")
    data %>%
      filter(NeuGenesen %in% c(0,1)) %>%
      group_by(Landkreis, Altersgruppe) %>%
      summarize(Recovered = sum(AnzahlGenesen)) %>%
      filter(Landkreis %in% district) -> result
  } else if(!is.na(district) & !is.na(age_group_start) & !is.na(age_group_end) & is.na(date_start) & is.na(date_end)) {
    print("District Age")
    data %>%
      filter(NeuGenesen %in% c(0,1)) %>%
      group_by(Landkreis, Altersgruppe) %>%
      summarize(Recovered = sum(AnzahlGenesen)) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      filter(Landkreis %in% district) -> result
  }else if(is.na(district) & is.na(age_group_start) & is.na(age_group_end) & !is.na(date_start) & !is.na(date_end)){
    print("Datum")
    data %>%
      filter(NeuGenesen %in% c(0,1)) %>%
      group_by(Landkreis, Meldedatum) %>%
      filter_by_date(date_start, date_end)  %>%
      summarize(Recovered = sum(AnzahlGenesen)) -> result
  }else if(is.na(district) & is.na(age_group_start) & is.na(age_group_end) & is.na(date_start) & is.na(date_end)){
    print("Keins")
    data %>%
      filter(NeuGenesen %in% c(0,1)) %>%
      group_by(Landkreis, Altersgruppe) %>%
      summarize(Recovered = sum(AnzahlGenesen)) -> result
  }else if(is.na(district) & !is.na(age_group_start) & !is.na(age_group_end) & is.na(date_start) & is.na(date_end)){
    print("Age ")
    data %>%
      filter(NeuGenesen %in% c(0,1)) %>%
      group_by(Landkreis, Altersgruppe) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      summarize(Recovered = sum(AnzahlGenesen)) -> result
  }else {
    print("District Age Datum")
    data %>%
      filter(NeuGenesen %in% c(0,1)) %>%
      group_by(Landkreis, Altersgruppe, Meldedatum) %>%
      filter_by_date(date_start, date_end) %>% 
      filter_by_age_group(age_group_start, age_group_end) %>%
      filter(Landkreis %in% district) %>% 
      summarize(Recovered = sum(AnzahlGenesen)) -> result
    
    
  }
  return(result)
}


get_recovered_per_district(cov_data, district = c("SK Flensburg", "SK Erfurt"), age_group_start = "A15", age_group_end = "A59")




get_unclean_data <- function(cov_data){
  unclean_NeuerFall <- cov_data %>%
    filter(NeuerFall == -1)
  unclean_NeuerTodesfall <- cov_data %>%
    filter(NeuerTodesfall == -1)
  unclean_NeuGenesen <- cov_data %>%
    filter(NeuGenesen == -1)
  unclean_age <- cov_data %>% 
    filter(Altersgruppe == "unbekannt")
  unclean_data <- rbind(unclean_NeuerFall,unclean_NeuerTodesfall,unclean_NeuGenesen, unclean_age)
  return(unclean_data)
}

remove_unclean_data <- function(cov_data){
  xx <- cov_data$NeuerFall == -1
  yy <- cov_data$NeuerTodesfall == -1
  zz <- cov_data$NeuGenesen == -1
  tt <- xx + yy + zz
  row_to_keep = !tt
  clean_data <- cov_data[row_to_keep,]
  return(clean_data)
}


get_incidence_per_district <- function(data, age_group_start = NA, age_group_end = NA, district = NA, date_start = NA, date_end = NA) {

  district <- gsub(pattern = "[??]",replacement = "Ã¶", district)
  district <- gsub(pattern = "[??]",replacement = "Ã¤", district)
  district <- gsub(pattern = "[??]",replacement = "Ã¼", district)
  district_names <- distinct(cov_data, Landkreis)
  if(!is.na(district)) {
    stopifnot("not a correct district" = any(district[[1]] == district_names))
  }
  # check if district state is consistent
  if(!is.na(district)){
    stopifnot("district does not exist" = district %in% district_names)
  }
  if(is.na(district)) {
    print("is.na(district")
    data %>%
      filter(NeuGenesen %in% c(0,1)) %>%
      group_by(Landkreis, Altersgruppe) %>%
      summarize(Recovered = sum(AnzahlGenesen)) %>%
      filter_by_age_group(age_group_start, age_group_end) -> result
    #filter_by_date(date_start, date_end) -> result
  } else if (is.na(age_group_start) & is.na(age_group_end)){
    print("is.na(age_group_start) & is.na(age_group_end)")
    data %>%
      filter(NeuGenesen %in% c(0,1)) %>%
      group_by(Landkreis, Altersgruppe) %>%
      summarize(Recovered = sum(AnzahlGenesen)) %>%
      filter(district %in% Landkreis) -> result
    #filter_by_date(date_start, date_end) -> result
  } else if (is.na(date_start) & is.na(date_end)) {
    print("is.na(date_start) & is.na(date_end))")
    data %>%
      filter(NeuGenesen %in% c(0,1)) %>%
      group_by(Landkreis, Altersgruppe) %>%
      summarize(Recovered = sum(AnzahlGenesen)) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      filter(district %in% Landkreis) -> result
  }else if (is.na(age_group_start) & is.na(age_group_end) & is.na(district)){
    print("((is.na(age_group_start) & is.na(age_group_end) & is.na(district))")
    data %>%
      filter(NeuGenesen %in% c(0,1)) %>%
      group_by(Landkreis, Altersgruppe) %>%
      #filter_by_date(date_start, date_end)  %>%
      summarize(Recovered = sum(AnzahlGenesen)) -> result
  }else if (is.na(age_group_start) & is.na(age_group_end) & is.na(district) & is.na(date_start) & is.na(date_end)){
    print("((is.na(age_group_start) & is.na(age_group_end) & is.na(district))is.na(date_start) & is.na(date_end)")
    data %>%
      filter(NeuGenesen %in% c(0,1)) %>%
      group_by(Landkreis, Altersgruppe) %>%
      summarize(Recovered = sum(AnzahlGenesen)) -> result
  }else if (is.na(district) & is.na(date_start) & is.na(date_end)){
    print("( is.na(district))is.na(date_start) & is.na(date_end)")
    data %>%
      filter(NeuGenesen %in% c(0,1)) %>%
      group_by(Landkreis, Altersgruppe) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      summarize(Recovered = sum(AnzahlGenesen)) -> result
  }else {
    print("else")
    data %>%
      filter(NeuGenesen %in% c(0,1)) %>%
      group_by(Landkreis, Altersgruppe) %>%
      summarize(Recovered = sum(AnzahlGenesen)) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      filter(district %in% Landkreis) -> result


  }
  return(result)
}


cov_data <- read.csv("R/RKI_COVID19.csv")
distinct(cov_data, Landkreis)
"LK Heidenheim" %in% district_names
get_infections_per_district(cov_data, district="LK Heidenheim")
any(district_names == "LK Heidenheim")

district_names
>>>>>>> Stashed changes


get_deaths(cov_data, age_group_start = "A05", age_group_end = "A59", )
