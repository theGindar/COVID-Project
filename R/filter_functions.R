library(tidyverse)
library(dplyr)
library(stringr)

cov_data <- read.csv("data.csv")

# returns death cases
# data: df with covid data
# age_group_start: the lower bound age group, e. g. "A05"
# age_group_end: the upper bound age group, e. g. "A59"
# federal_state: the federal state to filter by
# district: district to filter by, can be the name or id of the district
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

  # check if federal state is consistent
  if(!is.na(federal_state)){
    stopifnot("federal state does not exist" = federal_state %in% federal_state_names)
  }
  if(is.na(federal_state)) {
    print("is.na(federal_state")
    data %>%
      filter(NeuerTodesfall %in% c(0,1)) %>% 
      group_by(Bundesland, Altersgruppe) %>%
      summarize(Todesfaelle = sum(AnzahlTodesfall)) %>%
      filter_by_age_group(age_group_start, age_group_end) -> result
      #filter_by_date(date_start, date_end) -> result
  } else if (is.na(age_group_start) & is.na(age_group_end)){
    print("is.na(age_group_start) & is.na(age_group_end)")
    data %>%
      filter(NeuerTodesfall %in% c(0,1)) %>% 
      group_by(Bundesland, Altersgruppe) %>%
      summarize(Todesfaelle = sum(AnzahlTodesfall)) %>%
      filter(Bundesland %in% federal_state) -> result
      #filter_by_date(date_start, date_end) -> result
  } else if (is.na(date_start) & is.na(date_end)) {
    print("is.na(date_start) & is.na(date_end))")
    data %>%
      filter(NeuerTodesfall %in% c(0,1)) %>% 
      group_by(Bundesland, Altersgruppe) %>%
      summarize(Todesfaelle = sum(AnzahlTodesfall)) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      filter(Bundesland %in% federal_state) -> result
  }else if ((is.na(age_group_start) & is.na(age_group_end) & is.na(federal_state)){
    print("((is.na(age_group_start) & is.na(age_group_end) & is.na(federal_state))")
    data %>% 
      filter(NeuerTodesfall %in% c(0,1)) %>% 
      group_by(Bundesland, Altersgruppe) %>%
      #filter_by_date(date_start, date_end)  %>% 
      summarize(Todesfaelle = sum(AnzahlTodesfall)) -> result
  }else if ((is.na(age_group_start) & is.na(age_group_end) & is.na(federal_state) & is.na(date_start) & is.na(date_end)){
    print("((is.na(age_group_start) & is.na(age_group_end) & is.na(federal_state))is.na(date_start) & is.na(date_end)")
    data %>% 
      filter(NeuerTodesfall %in% c(0,1)) %>% 
      group_by(Bundesland, Altersgruppe) %>%
      summarize(Todesfaelle = sum(AnzahlTodesfall)) -> result
  }else if (is.na(federal_state) & is.na(date_start) & is.na(date_end)){
    print("( is.na(federal_state))is.na(date_start) & is.na(date_end)")
    data %>% 
      filter(NeuerTodesfall %in% c(0,1)) %>% 
      group_by(Bundesland, Altersgruppe) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      summarize(Todesfaelle = sum(AnzahlTodesfall)) -> result
  }else {
    print("else")
    data %>%
      filter(NeuerTodesfall %in% c(0,1)) %>% 
      group_by(Bundesland, Altersgruppe) %>%
      summarize(Todesfaelle = sum(AnzahlTodesfall)) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      filter(Bundesland %in% federal_state) -> result
    
    
  }
  return(result) 
}

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



get_deaths_per_federal_states(cov_data,date_start = "dsa", date_end = "sadad", federal_state = "Berlin")
age_group_start = "A05", age_group_end = "A80",


get_deaths_per_district <- function(data, age_group_start = NA, age_group_end = NA, district = NA, date_start = NA, date_end = NA) {
  district_names = c("Schleswig-Holstein",
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
  
  # check if district state is consistent
  if(!is.na(district)){
    stopifnot("district does not exist" = district %in% district_names)
  }
  if(is.na(district)) {
    print("is.na(district")
    data %>%
      filter(NeuerTodesfall %in% c(0,1)) %>% 
      group_by(district, Altersgruppe) %>%
      summarize(Todesfaelle = sum(AnzahlTodesfall)) %>%
      filter_by_age_group(age_group_start, age_group_end) -> result
    #filter_by_date(date_start, date_end) -> result
  } else if (is.na(age_group_start) & is.na(age_group_end)){
    print("is.na(age_group_start) & is.na(age_group_end)")
    data %>%
      filter(NeuerTodesfall %in% c(0,1)) %>% 
      group_by(district, Altersgruppe) %>%
      summarize(Todesfaelle = sum(AnzahlTodesfall)) %>%
      filter(district %in% district_names) -> result
    #filter_by_date(date_start, date_end) -> result
  } else if (is.na(date_start) & is.na(date_end)) {
    print("is.na(date_start) & is.na(date_end))")
    data %>%
      filter(NeuerTodesfall %in% c(0,1)) %>% 
      group_by(district, Altersgruppe) %>%
      summarize(Todesfaelle = sum(AnzahlTodesfall)) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      filter(district %in% district_names) -> result
  }else if ((is.na(age_group_start) & is.na(age_group_end) & is.na(district)){
    print("((is.na(age_group_start) & is.na(age_group_end) & is.na(district))")
    data %>% 
      filter(NeuerTodesfall %in% c(0,1)) %>% 
      group_by(district, Altersgruppe) %>%
      #filter_by_date(date_start, date_end)  %>% 
      summarize(Todesfaelle = sum(AnzahlTodesfall)) -> result
  }else if ((is.na(age_group_start) & is.na(age_group_end) & is.na(district) & is.na(date_start) & is.na(date_end)){
    print("((is.na(age_group_start) & is.na(age_group_end) & is.na(district))is.na(date_start) & is.na(date_end)")
    data %>% 
      filter(NeuerTodesfall %in% c(0,1)) %>% 
      group_by(district, Altersgruppe) %>%
      summarize(Todesfaelle = sum(AnzahlTodesfall)) -> result
  }else if (is.na(district) & is.na(date_start) & is.na(date_end)){
    print("( is.na(district))is.na(date_start) & is.na(date_end)")
    data %>% 
      filter(NeuerTodesfall %in% c(0,1)) %>% 
      group_by(district, Altersgruppe) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      summarize(Todesfaelle = sum(AnzahlTodesfall)) -> result
  }else {
    print("else")
    data %>%
      filter(NeuerTodesfall %in% c(0,1)) %>% 
      group_by(district, Altersgruppe) %>%
      summarize(Todesfaelle = sum(AnzahlTodesfall)) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      filter(district %in% district_names) -> result
    
    
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
                          "Baden-Württemberg",
                          "Bayern",
                          "Saarland",
                          "Berlin",
                          "Brandenburg",
                          "Mecklenburg-Vorpommern",
                          "Sachsen",
                          "Sachsen-Anhalt",
                          "Thüringen")  
  
  # check if federal state is consistent
  if(!is.na(federal_state)){
    stopifnot("federal state does not exist" = federal_state %in% federal_state_names)
  }
  if(is.na(federal_state)) {
    print("is.na(federal_state")
    data %>%
      filter(NeuerFall %in% c(0,1)) %>% 
      group_by(Bundesland, Altersgruppe) %>%
      summarize(Infections = sum(AnzahlFall)) %>%
      filter_by_age_group(age_group_start, age_group_end) -> result
    #filter_by_date(date_start, date_end) -> result
  } else if (is.na(age_group_start) & is.na(age_group_end)){
    print("is.na(age_group_start) & is.na(age_group_end)")
    data %>%
      filter(NeuerFall %in% c(0,1)) %>% 
      group_by(Bundesland, Altersgruppe) %>%
      summarize(Infections = sum(AnzahlFall)) %>%
      filter(Bundesland %in% federal_state) -> result
    #filter_by_date(date_start, date_end) -> result
  } else if (is.na(date_start) & is.na(date_end)) {
    print("is.na(date_start) & is.na(date_end))")
    data %>%
      filter(NeuerFall %in% c(0,1)) %>% 
      group_by(Bundesland, Altersgruppe) %>%
      summarize(Infections = sum(AnzahlFall)) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      filter(Bundesland %in% federal_state) -> result
  }else if ((is.na(age_group_start) & is.na(age_group_end) & is.na(federal_state)){
    print("((is.na(age_group_start) & is.na(age_group_end) & is.na(federal_state))")
    data %>% 
      filter(NeuerFall %in% c(0,1)) %>% 
      group_by(Bundesland, Altersgruppe) %>%
      #filter_by_date(date_start, date_end)  %>% 
      summarize(Infections = sum(AnzahlFall)) -> result
  }else if ((is.na(age_group_start) & is.na(age_group_end) & is.na(federal_state) & is.na(date_start) & is.na(date_end)){
    print("((is.na(age_group_start) & is.na(age_group_end) & is.na(federal_state))is.na(date_start) & is.na(date_end)")
    data %>% 
      filter(NeuerFall %in% c(0,1)) %>% 
      group_by(Bundesland, Altersgruppe) %>%
      summarize(Infections = sum(AnzahlFall)) -> result
  }else if (is.na(federal_state) & is.na(date_start) & is.na(date_end)){
    print("( is.na(federal_state))is.na(date_start) & is.na(date_end)")
    data %>% 
      filter(NeuerFall %in% c(0,1)) %>% 
      group_by(Bundesland, Altersgruppe) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      summarize(Infections = sum(AnzahlFall)) -> result
  }else {
    print("else")
    data %>%
      filter(NeuerFall %in% c(0,1)) %>% 
      group_by(Bundesland, Altersgruppe) %>%
      summarize(Infections = sum(AnzahlFall)) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      filter(Bundesland %in% federal_state) -> result
    
    
  }
  return(result) 
}


get_infections_per_district <- function(data, age_group_start = NA, age_group_end = NA, district = NA, date_start = NA, date_end = NA) {
  district_names = c("Schleswig-Holstein",
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
  
  # check if district state is consistent
  if(!is.na(district)){
    stopifnot("district does not exist" = district %in% district_names)
  }
  if(is.na(district)) {
    print("is.na(district")
    data %>%
      filter(NeuerFall %in% c(0,1)) %>% 
      group_by(district, Altersgruppe) %>%
      summarize(Infections = sum(AnzahlFall)) %>%
      filter_by_age_group(age_group_start, age_group_end) -> result
    #filter_by_date(date_start, date_end) -> result
  } else if (is.na(age_group_start) & is.na(age_group_end)){
    print("is.na(age_group_start) & is.na(age_group_end)")
    data %>%
      filter(NeuerFall %in% c(0,1)) %>% 
      group_by(district, Altersgruppe) %>%
      summarize(Infections = sum(AnzahlFall)) %>%
      filter(district %in% district_names) -> result
    #filter_by_date(date_start, date_end) -> result
  } else if (is.na(date_start) & is.na(date_end)) {
    print("is.na(date_start) & is.na(date_end))")
    data %>%
      filter(NeuerFall %in% c(0,1)) %>% 
      group_by(district, Altersgruppe) %>%
      summarize(Infections = sum(AnzahlFall)) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      filter(district %in% district_names) -> result
  }else if ((is.na(age_group_start) & is.na(age_group_end) & is.na(district)){
    print("((is.na(age_group_start) & is.na(age_group_end) & is.na(district))")
    data %>% 
      filter(NeuerFall %in% c(0,1)) %>% 
      group_by(district, Altersgruppe) %>%
      #filter_by_date(date_start, date_end)  %>% 
      summarize(Infections = sum(AnzahlFall)) -> result
  }else if ((is.na(age_group_start) & is.na(age_group_end) & is.na(district) & is.na(date_start) & is.na(date_end)){
    print("((is.na(age_group_start) & is.na(age_group_end) & is.na(district))is.na(date_start) & is.na(date_end)")
    data %>% 
      filter(NeuerFall %in% c(0,1)) %>% 
      group_by(district, Altersgruppe) %>%
      summarize(Infections = sum(AnzahlFall)) -> result
  }else if (is.na(district) & is.na(date_start) & is.na(date_end)){
    print("( is.na(district))is.na(date_start) & is.na(date_end)")
    data %>% 
      filter(NeuerFall %in% c(0,1)) %>% 
      group_by(district, Altersgruppe) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      summarize(Infections = sum(AnzahlFall)) -> result
  }else {
    print("else")
    data %>%
      filter(NeuerFall %in% c(0,1)) %>% 
      group_by(district, Altersgruppe) %>%
      summarize(Infections = sum(AnzahlFall)) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      filter(district %in% district_names) -> result
    
    
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
                          "Baden-Württemberg",
                          "Bayern",
                          "Saarland",
                          "Berlin",
                          "Brandenburg",
                          "Mecklenburg-Vorpommern",
                          "Sachsen",
                          "Sachsen-Anhalt",
                          "Thüringen")  
  
  # check if federal state is consistent
  if(!is.na(federal_state)){
    stopifnot("federal state does not exist" = federal_state %in% federal_state_names)
  }
  if(is.na(federal_state)) {
    print("is.na(federal_state")
    data %>%
      filter(NeuGenesen %in% c(0,1)) %>% 
      group_by(Bundesland, Altersgruppe) %>%
      summarize(Recovered = sum(AnzahlGenesen)) %>%
      filter_by_age_group(age_group_start, age_group_end) -> result
    #filter_by_date(date_start, date_end) -> result
  } else if (is.na(age_group_start) & is.na(age_group_end)){
    print("is.na(age_group_start) & is.na(age_group_end)")
    data %>%
      filter(NeuGenesen %in% c(0,1)) %>% 
      group_by(Bundesland, Altersgruppe) %>%
      summarize(Recovered = sum(AnzahlGenesen)) %>%
      filter(Bundesland %in% federal_state) -> result
    #filter_by_date(date_start, date_end) -> result
  } else if (is.na(date_start) & is.na(date_end)) {
    print("is.na(date_start) & is.na(date_end))")
    data %>%
      filter(NeuGenesen %in% c(0,1)) %>% 
      group_by(Bundesland, Altersgruppe) %>%
      summarize(Recovered = sum(AnzahlGenesen)) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      filter(Bundesland %in% federal_state) -> result
  }else if ((is.na(age_group_start) & is.na(age_group_end) & is.na(federal_state)){
    print("((is.na(age_group_start) & is.na(age_group_end) & is.na(federal_state))")
    data %>% 
      filter(NeuGenesen %in% c(0,1)) %>% 
      group_by(Bundesland, Altersgruppe) %>%
      #filter_by_date(date_start, date_end)  %>% 
      summarize(Recovered = sum(AnzahlGenesen)) -> result
  }else if ((is.na(age_group_start) & is.na(age_group_end) & is.na(federal_state) & is.na(date_start) & is.na(date_end)){
    print("((is.na(age_group_start) & is.na(age_group_end) & is.na(federal_state))is.na(date_start) & is.na(date_end)")
    data %>% 
      filter(NeuGenesen %in% c(0,1)) %>% 
      group_by(Bundesland, Altersgruppe) %>%
      summarize(Recovered = sum(AnzahlGenesen)) -> result
  }else if (is.na(federal_state) & is.na(date_start) & is.na(date_end)){
    print("( is.na(federal_state))is.na(date_start) & is.na(date_end)")
    data %>% 
      filter(NeuGenesen %in% c(0,1)) %>% 
      group_by(Bundesland, Altersgruppe) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      summarize(Recovered = sum(AnzahlGenesen)) -> result
  }else {
    print("else")
    data %>%
      filter(NeuGenesen %in% c(0,1)) %>% 
      group_by(Bundesland, Altersgruppe) %>%
      summarize(Recovered = sum(AnzahlGenesen)) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      filter(Bundesland %in% federal_state) -> result
    
    
  }
  return(result) 
}



get_infections_per_district <- function(data, age_group_start = NA, age_group_end = NA, district = NA, date_start = NA, date_end = NA) {
  district_names = c("Schleswig-Holstein",
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
  
  # check if district state is consistent
  if(!is.na(district)){
    stopifnot("district does not exist" = district %in% district_names)
  }
  if(is.na(district)) {
    print("is.na(district")
    data %>%
      filter(NeuGenesen %in% c(0,1)) %>% 
      group_by(district, Altersgruppe) %>%
      summarize(Recovered = sum(AnzahlGenesen)) %>%
      filter_by_age_group(age_group_start, age_group_end) -> result
    #filter_by_date(date_start, date_end) -> result
  } else if (is.na(age_group_start) & is.na(age_group_end)){
    print("is.na(age_group_start) & is.na(age_group_end)")
    data %>%
      filter(NeuGenesen %in% c(0,1)) %>% 
      group_by(district, Altersgruppe) %>%
      summarize(Recovered = sum(AnzahlGenesen)) %>%
      filter(district %in% district_names) -> result
    #filter_by_date(date_start, date_end) -> result
  } else if (is.na(date_start) & is.na(date_end)) {
    print("is.na(date_start) & is.na(date_end))")
    data %>%
      filter(NeuGenesen %in% c(0,1)) %>% 
      group_by(district, Altersgruppe) %>%
      summarize(Recovered = sum(AnzahlGenesen)) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      filter(district %in% district_names) -> result
  }else if ((is.na(age_group_start) & is.na(age_group_end) & is.na(district)){
    print("((is.na(age_group_start) & is.na(age_group_end) & is.na(district))")
    data %>% 
      filter(NeuGenesen %in% c(0,1)) %>% 
      group_by(district, Altersgruppe) %>%
      #filter_by_date(date_start, date_end)  %>% 
      summarize(Recovered = sum(AnzahlGenesen)) -> result
  }else if ((is.na(age_group_start) & is.na(age_group_end) & is.na(district) & is.na(date_start) & is.na(date_end)){
    print("((is.na(age_group_start) & is.na(age_group_end) & is.na(district))is.na(date_start) & is.na(date_end)")
    data %>% 
      filter(NeuGenesen %in% c(0,1)) %>% 
      group_by(district, Altersgruppe) %>%
      summarize(Recovered = sum(AnzahlGenesen)) -> result
  }else if (is.na(district) & is.na(date_start) & is.na(date_end)){
    print("( is.na(district))is.na(date_start) & is.na(date_end)")
    data %>% 
      filter(NeuGenesen %in% c(0,1)) %>% 
      group_by(district, Altersgruppe) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      summarize(Recovered = sum(AnzahlGenesen)) -> result
  }else {
    print("else")
    data %>%
      filter(NeuGenesen %in% c(0,1)) %>% 
      group_by(district, Altersgruppe) %>%
      summarize(Recovered = sum(AnzahlGenesen)) %>%
      filter_by_age_group(age_group_start, age_group_end) %>%
      filter(district %in% district_names) -> result
    
    
  }
  return(result) 
}

# delete -1 files

get_unclean_data <- function(data){
  data %>% 
    filter(NeuerFall == -1) %>% 
    filter(NeuerTodesfall == -1) %>% 
    filter(NeuGenesen == -1) -> unclean_data
}

get_unclean_data(data)




















