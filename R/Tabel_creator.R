library(tidyverse)
data <- read.csv("data.csv")
data


data %>% 
  count(NeuerFall) 
# 0: Fall ist in der Publikation für den aktuellen Tag und in der für den Vortag enthalten
# 1: Fall ist nur in der aktuellen Publikation enthalten
# -1: Fall ist nur in der Publikation des Vortags enthalten

data %>% 
  count(Meldedatum) %>% 
  ggplot(aes(Meldedatum, n, color = "S-H", group = 1, )) + geom_line()
# Anzahl an Meldungen pro Tag

data %>% 
  count(Altersgruppe) %>% 
  ggplot(aes(Altersgruppe, n, color = "S-H", group = 1, )) + geom_line() + theme(axis.text.x = element_text(size=7, angle=90))

# Anzahl an Einträgen von Altersgruppen in der CSV -> Vielleicht dann noch filtern für nur Todesfälle oder so


data %>% 
  count(Geschlecht) %>% 
  ggplot(aes(Geschlecht, n)) + geom_bar(stat = "identity", fill = "steelblue") + theme_minimal() + geom_text(aes(label = n), vjust = -0.3, color = "black", size = 3.5)

# Anzahl an Einträgen gesplittet auf Geschlecht -> Idee Alter + Geschlecht testen



data %>% 
  count(Geschlecht, Altersgruppe) %>% 
  ggplot(aes(Geschlecht, n)) + geom_bar(stat = "identity", fill = "steelblue") + theme_minimal() + geom_text(aes(label = n), vjust = -0.3, color = "black", size = 3.5)

# Plot funktioniert nicht moving on

data %>% 
  count(Bundesland) %>% 
  ggplot(aes(Bundesland, n, color = "S-H", group = 1, )) + geom_bar(stat = "identity", fill="steelblue") + geom_text(aes(label = n), vjust = -0.3, color = "black", size = 3.5)

# Einfache Anzahl von Fällen in jedem Bundesland -> fehlen halt noch andere Bundeländer



