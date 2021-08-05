fdeaths3 <- get_deaths_per_federal_states(cov_data, date_start = "2020/06/19", date_end = "2021/03/22", federal_state = c("Bayern", "Hessen"))
plot_function(fdeaths3)


p1 <- get_recovered_per_district(cov_data, district = c("SK Flensburg", "LK Oberbergischer Kreis"))
p2 <- get_deaths_per_district(cov_data, district = c("SK Flensburg", "LK Oberbergischer Kreis"))



plot_attempt_01 <- get_deaths_per_district(cov_data, age_group_start = "A15", age_group_end = "A79")
plot_attempt_01

p1
data %>% 
  ggplot(aes(x = Landkreis, y = Recovered, color = Landkreis))



plot_function2 <- function(data){
  if("Landkreis" %in% colnames(data)){
    print("Landkreis")
    if(length(p1$Recovered) != 0) & length(p1$Altersgruppe) != 0)){
      print("Altersgruppen von Recovered pro Landkreis")
      data %>% 
        ggplot(aes(x = Altersgruppe, y = Recovered, color = Landkreis)) + 
        geom_bar(stat= "identity", aes(fill = Landkreis), position = "dodge") + 
        geom_text(aes(label = Recovered),vjust = -0.3, color = "black", size = 3.5)
    }else if (length(p1$Recovered) != 0), length(p1$Altersgruppe) == 0)){
      print("Recovered pro Landkreis")
      data %>% 
        ggplot(aes(x = Landkreis, y = Recovered, color = Landkreis)) + 
        geom_bar(stat= "identity", aes(fill = Landkreis), position = "dodge") + 
        geom_text(aes(label = Recovered),vjust = -0.3, color = "black", size = 3.5)
    }
  }
  else{
    print("Bundesland")
    if(!is.na(data$Recovered[1]) & !is.na(data$Altersgruppe[1])){
      data %>% 
        ggplot(aes(x = Altersgruppe, y = Recovered, color = Bundesland)) + 
        geom_bar(stat= "identity", aes(fill = Bundesland), position = "dodge") + 
        geom_text(aes(label = Recovered),vjust = -0.3, color = "black", size = 3.5)
    }
  }
}
plot_function2(plot_attempt_01)
plot_attempt_02 <- get_recovered_per_district(cov_data, age_group_start = "A15", age_group_end = "A79", district = c("SK Flensburg", "LK Oberbergischer Kreis"))
plot_attempt_02
data %>% 
  ggplot(aes(x = Altersgruppe, y = Recovered, color = district)) + geom_bar(stat= "identity", aes(fill = district), position = "dodge") + geom_text(aes(label = Recovered),vjust = -0.3, color = "black", size = 3.5)
plot_function2(plot_attempt_02)


#Datum Plot hier
plot_agedatum_recovered <- get_recovered_per_district(cov_data, age_group_start = "A15", age_group_end = "A79", date_start = "2020/06/19", date_end = "2021/03/22")
plot_agedatum_recovered %>% 
  group_by(Altersgruppe, Recovered)
Datum_limit <- vector(mode="character", length=length(plot_agedatum_recovered$Meldedatum))
Limiter <- c(floor(length(plot_agedatum_recovered$Meldedatum)/4), floor(length(plot_agedatum_recovered$Meldedatum)/4)*2, floor(length(plot_agedatum_recovered$Meldedatum)/4)*3, floor(length(plot_agedatum_recovered$Meldedatum)/4)*4)
Datum_limit[0] <- plot_agedatum_recovered$Meldedatum[1]
Datum_limit[Limiter] <- plot_agedatum_recovered$Meldedatum[Limiter]
Datum_limit
plot_agedatum_recovered$Meldedatum
plot_agedatum_recovered %>% 
  ggplot(aes(x = as.Date(Meldedatum), y = Recovered, group = Altersgruppe, color = Altersgruppe)) +
  xlab("Meldedatum") +
  geom_line() + 
  stat_smooth(method = "loess", se = FALSE) 



plot_agedatum_recovered %>% 
  ggplot(aes(x = Meldedatum, y = Recovered, color = Altersgruppe)) + 
  geom_point(stat= "identity", group = 1, )  + 
  #geom_smooth(method = "loess", se = FALSE, fullrange = TRUE) +
  #geom_hline(yintercept = mean(plot_agedatum_recovered$Recovered)) +
  #geom_line(stat = "identity", group = 1) +
  #stat_smooth(aes(x = Meldedatum, y = Recovered), method = "lm", formula = (y~x))
  theme(axis.text.x = element_text(size=7, angle=90)) +
  scale_x_discrete(labels = Datum_limit) 


###