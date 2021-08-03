library(tidyverse)
data <- read.csv("data.csv")

data %>% 
  filter(NeuerFall == c("1","0"))
data %>% 
  group_by(Bundesland) %>% 
  count(NeuerFall) 
# 0: Fall ist in der Publikation für den aktuellen Tag und in der für den Vortag enthalten
# 1: Fall ist nur in der aktuellen Publikation enthalten
# -1: Fall ist nur in der Publikation des Vortags enthalten

xlabels <- sort(unique(data$Meldedatum))
xlabels[2:100] <- ""
xlabels[102:200] <- ""
xlabels[202:300] <- ""
xlabels[302:400] <- ""
xlabels[402:500] <- ""
xlabels[502:538] <- ""
xlabels

data %>% 
  group_by(Bundesland) %>% 
  count(Meldedatum) %>% 
  ggplot(aes(Meldedatum, n, color = Bundesland, group = 1, )
         )+ theme_minimal() +  geom_line() +ylab("Meldungen")+ xlab("20.01.2020-15.06.2021"
                                                                    ) + scale_x_discrete(labels = xlabels) + theme(axis.text.x = element_text(size=7, angle=90))
# Anzahl an Meldungen pro Tag

data %>%
  filter(IstErkrankungsbeginn == 1) %>% 
  count(Altersgruppe, Bundesland) %>% 
  ggplot(aes(Altersgruppe, n, color = Bundesland, group = 1, )) + geom_bar(stat = "identity", fill = "white") + theme(axis.text.x = element_text(size=7, angle=90))

# Anzahl an Erkrankungsbeginnen von Altersgruppen in der CSV -> Vielleicht dann noch filtern für nur Todesfälle oder so


data %>% 
  filter(IstErkrankungsbeginn == 1) %>% 
  count(Bundesland, Geschlecht) %>% 
  ggplot(aes(Bundesland, n)) + geom_bar(stat = "identity") +  geom_text(aes(label = n), vjust = -0.3, color ="red", size = 2) + theme(axis.text.x = element_text(size=7, angle=90))

data %>% 
  count(Bundesland, Geschlecht) %>% 
  ggplot(aes(Bundesland, n)) + geom_bar(stat = "identity", fill = "steelblue") +  geom_text(aes(label = n), vjust = -0.3, color = "black", size = 2) + theme(axis.text.x = element_text(size=7, angle=90))
# Anzahl an Einträgen gesplittet auf Geschlecht -> Idee Alter + Geschlecht testen



data %>% 
  count(Geschlecht, Altersgruppe) %>% 
  ggplot(aes(Geschlecht, n)) + geom_bar(stat = "identity", fill = "steelblue") + theme_minimal() + geom_text(aes(label = n), vjust = -0.3, color = "black", size = 3.5)

# Plot funktioniert nicht moving on

data %>% 
  count(Bundesland) %>% 
  ggplot(aes(Bundesland, n, color = Bundesland, group = 1, )) + geom_bar(stat = "identity", fill="steelblue") + geom_text(aes(label = n), vjust = -0.3, color = "black", size = 2.5) + theme(axis.text.x = element_text(size=7, angle=90))

# Einfache Anzahl von Fällen in jedem Bundesland -> fehlen halt noch andere Bundeländer















get_deaths_per_federal_states(data, federal_states, age_group_start, age_group_end, date_start, date_end)


if(is.na(federal_state)) {
  data %>%
    group_by(Refdatum, Altersgruppe, Bundesland) %>%
    summarize(Todesfaelle = sum(AnzahlTodesfall)) %>%
    filter(NeuerTodesfall == c("1","0")) %>% 
    filter_by_age_group(age_group_start, age_group_end) %>% 
    filter_by_date(date_start, date_end) -> result
} else if (is.na(age_group_start) & is.na(age_group_end)){
  data %>%
    group_by(Refdatum, Altersgruppe, Bundesland) %>%
    summarize(Todesfaelle = sum(AnzahlTodesfall)) %>%
    filter(NeuerTodesfall == c("1","0")) %>% 
    filter(Bundesland %in% federal_state) %>% 
    filter_by_date(date_start, date_end) -> result
} else if (is.na(date_start) & is.na(date_end)) {
  data %>%
    group_by(Refdatum, Altersgruppe, Bundesland) %>%
    summarize(Todesfaelle = sum(AnzahlTodesfall)) %>%
    filter(NeuerTodesfall == c("1","0")) %>% 
    filter_by_age_group(age_group_start, age_group_end) %>%
    filter(Bundesland %in% federal_state) -> result
} else {
  data %>%
    group_by(Refdatum, Altersgruppe, Landkreis) %>%
    summarize(Todesfaelle = sum(AnzahlTodesfall)) %>%
    filter(NeuerTodesfall == c("1","0")) %>% 
    filter_by_age_group(age_group_start, age_group_end) %>%
    filter_by_date(date_start, date_end) %>% 
    filter(Bundesland %in% federal_state) -> result

return(result)
}


get_deaths_per_districts(data, districts, age_group_start, age_group_end, date_start, date_end) 


if(is.na(district)) {
  data %>%
    group_by(Refdatum, Altersgruppe, Bundesland) %>%
    summarize(Todesfaelle = sum(AnzahlTodesfall)) %>%
    filter(NeuerTodesfall == c("1","0")) %>% 
    filter_by_age_group(age_group_start, age_group_end) %>% 
    filter_by_date(date_start, date_end) -> result
} else if (is.na(age_group_start) & is.na(age_group_end)){
  data %>%
    group_by(Refdatum, Altersgruppe, Bundesland) %>%
    summarize(Todesfaelle = sum(AnzahlTodesfall)) %>%
    filter(NeuerTodesfall == c("1","0")) %>% 
    filter(Landkreis %in% district) %>% 
    filter_by_date(date_start, date_end) -> result
} else if (is.na(date_start) & is.na(date_end)) {
  data %>%
    group_by(Refdatum, Altersgruppe, Bundesland) %>%
    summarize(Todesfaelle = sum(AnzahlTodesfall)) %>%
    filter(NeuerTodesfall == c("1","0")) %>% 
    filter_by_age_group(age_group_start, age_group_end) %>%
    filter(Landkreis %in% district) -> result
} else {
  data %>%
    group_by(Refdatum, Altersgruppe, Landkreis) %>%
    summarize(Todesfaelle = sum(AnzahlTodesfall)) %>%
    filter(NeuerTodesfall == c("1","0")) %>% 
    filter_by_age_group(age_group_start, age_group_end) %>%
    filter_by_date(date_start, date_end) %>% 
    filter(Landkreis %in% district) -> result
  
  return(result)
}



filter_by_gender <- function(data, gender)
  {data %>% 
      filter(Geschlecht %in% gender) -> result
  stopifnot("x has to be M, F or Unknown" = str_detect(gender, regex("[M|W|Unknown]", ignore_case = TRUE)))
  return(result)
}



get_infections_per_federal_states(data, federal_states, age_group_start, age_group_end, date_start, date_end)

if(is.na(federal_state)) {
  data %>%
    group_by(Refdatum, Altersgruppe, Bundesland) %>%
    summarize(Infection = sum(AnzahlFall)) %>%
    filter(NeuerFall == c("1","0")) %>% 
    filter_by_age_group(age_group_start, age_group_end) %>% 
    filter_by_date(date_start, date_end) -> result
} else if (is.na(age_group_start) & is.na(age_group_end)){
  data %>%
    group_by(Refdatum, Altersgruppe, Bundesland) %>%
    filter(NeuerFall == c("1","0")) %>% 
    summarize(Infection = sum(AnzahlFall)) %>%
    filter(Bundesland %in% federal_state) %>% 
    filter_by_date(date_start, date_end) -> result
} else if (is.na(date_start) & is.na(date_end)) {
  data %>%
    group_by(Refdatum, Altersgruppe, Bundesland) %>%
    summarize(Infection = sum(AnzahlFall)) %>%
    filter(NeuerFall == c("1","0")) %>% 
    filter_by_age_group(age_group_start, age_group_end) %>%
    filter(Bundesland %in% federal_state) -> result
} else {
  data %>%
    group_by(Refdatum, Altersgruppe, Landkreis) %>%
    summarize(Infection = sum(AnzahlFall)) %>%
    filter(NeuerFall == c("1","0")) %>% 
    filter_by_age_group(age_group_start, age_group_end) %>%
    filter_by_date(date_start, date_end) %>% 
    filter(Bundesland %in% federal_state) -> result
  
  return(result)
}

get_infections_per_districts(data, districts, age_group_start, age_group_end, date_start, date_end)


if(is.na(district)) {
  data %>%
    group_by(Refdatum, Altersgruppe, Bundesland) %>%
    summarize(Infection = sum(AnzahlFall)) %>%
    filter(NeuerFall == c("1","0")) %>% 
    filter_by_age_group(age_group_start, age_group_end) %>% 
    filter_by_date(date_start, date_end) -> result
} else if (is.na(age_group_start) & is.na(age_group_end)){
  data %>%
    group_by(Refdatum, Altersgruppe, Bundesland) %>%
    summarize(Infection = sum(AnzahlFall)) %>%
    filter(NeuerFall == c("1","0")) %>% 
    filter(Landkreis %in% district) %>% 
    filter_by_date(date_start, date_end) -> result
} else if (is.na(date_start) & is.na(date_end)) {
  data %>%
    group_by(Refdatum, Altersgruppe, Bundesland) %>%
    summarize(Infection = sum(AnzahlFall)) %>%
    filter(NeuerFall == c("1","0")) %>% 
    filter_by_age_group(age_group_start, age_group_end) %>%
    filter(Landkreis %in% district) -> result
} else {
  data %>%
    group_by(Refdatum, Altersgruppe, Landkreis) %>%
    summarize(Infection = sum(AnzahlFall)) %>%
    filter(NeuerFall == c("1","0")) %>% 
    filter_by_age_group(age_group_start, age_group_end) %>%
    filter_by_date(date_start, date_end) %>% 
    filter(Landkreis %in% district) -> result
  
  return(result)
}

get_recovered_per_federal_states(data, federal_states, age_group_start, age_group_end, date_start, date_end)

if(is.na(federal_state)) {
  data %>%
    group_by(Refdatum, Altersgruppe, Bundesland) %>%
    summarize(Infection = sum(AnzahlGenesen)) %>%
    filter(NeuGenesen == c("1","0")) %>% 
    filter_by_age_group(age_group_start, age_group_end) %>% 
    filter_by_date(date_start, date_end) -> result
} else if (is.na(age_group_start) & is.na(age_group_end)){
  data %>%
    group_by(Refdatum, Altersgruppe, Bundesland) %>%
    summarize(Infection = sum(AnzahlGenesen)) %>%
    filter(Bundesland %in% federal_state) %>% 
    filter(NeuGenesen == c("1","0")) %>% 
    filter_by_date(date_start, date_end) -> result
} else if (is.na(date_start) & is.na(date_end)) {
  data %>%
    group_by(Refdatum, Altersgruppe, Bundesland) %>%
    summarize(Infection = sum(AnzahlGenesen)) %>%
    filter(NeuGenesen == c("1","0")) %>% 
    filter_by_age_group(age_group_start, age_group_end) %>%
    filter(Bundesland %in% federal_state) -> result
} else {
  data %>%
    group_by(Refdatum, Altersgruppe, Landkreis) %>%
    summarize(Genesen = sum(AnzahlGenesen)) %>%
    filter(NeuGenesen == c("1","0")) %>% 
    filter_by_age_group(age_group_start, age_group_end) %>%
    filter_by_date(date_start, date_end) %>% 
    filter(Bundesland %in% federal_state) -> result
  
  return(result)
}


get_recovered_per_districts(data, districts, age_group_start, age_group_end, date_start, date_end)


if(is.na(federal_state)) {
  data %>%
    group_by(Refdatum, Altersgruppe, Bundesland) %>%
    summarize(Infection = sum(AnzahlGenesen)) %>%
    filter(NeuGenesen == c("1","0")) %>% 
    filter_by_age_group(age_group_start, age_group_end) %>% 
    filter_by_date(date_start, date_end) -> result
} else if (is.na(age_group_start) & is.na(age_group_end)){
  data %>%
    group_by(Refdatum, Altersgruppe, Bundesland) %>%
    summarize(Infection = sum(AnzahlGenesen)) %>%
    filter(Landkreis %in% district) %>% 
    filter(NeuGenesen == c("1","0")) %>% 
    filter_by_date(date_start, date_end) -> result
} else if (is.na(date_start) & is.na(date_end)) {
  data %>%
    group_by(Refdatum, Altersgruppe, Bundesland) %>%
    summarize(Infection = sum(AnzahlGenesen)) %>%
    filter(NeuGenesen == c("1","0")) %>% 
    filter_by_age_group(age_group_start, age_group_end) %>%
    filter(Landkreis %in% district) -> result
} else {
  data %>%
    group_by(Refdatum, Altersgruppe, Landkreis) %>%
    summarize(Genesen = sum(AnzahlGenesen)) %>%
    filter(NeuGenesen == c("1","0")) %>% 
    filter_by_age_group(age_group_start, age_group_end) %>%
    filter_by_date(date_start, date_end) %>% 
    filter(Landkreis %in% district) -> result
  
  return(result)
}

filter_by_date <- function(data, date_start, date_end){
  data[(as.Date(data$Meldedatum)> date_start & as.Date(data$Meldedatum) < date_end),]
}

as.Date("2021/07/31 00:00:00")
as.Date("2021/07/30 00:00:00")

data %>% 
  filter(Meldedatum >= as.Date("2021/07/30 00:00:00"))

filter_by_date(data, "2020-11-05", "2020-11-08")
data$Meldedatum <- as.Date(data$Meldedatum)
data
