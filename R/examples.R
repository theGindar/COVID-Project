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

test1 <- get_deaths_per_federal_states(cov_data, 
                                       age_group_start = "A15", 
                                       age_group_end = "A59", 
                                       date_start = "2020/11/19", 
                                       date_end = "2021/02/19")
plot_function(test1)

test2 <- get_deaths_per_federal_states(cov_data, 
                                       federal_state = c("Hessen", "Berlin"), 
                                       date_start = "2020/11/19", 
                                       date_end = "2021/02/19")
plot_function(test2)

test3 <- get_deaths_per_federal_states(cov_data, 
                                       federal_state = c("Hessen", "Berlin", "Bayern"), 
)
plot_function(test3)

test4 <- get_deaths_per_federal_states(cov_data, 
                                       federal_state = c("Hessen", "Berlin"), 
                                       age_group_start = "A35",
                                       age_group_end = "A79"
)
plot_function(test4)

test5 <- get_deaths_per_federal_states(cov_data, 
                                       date_start = "2020/11/19",
                                       date_end = "2021/02/19"
)
plot_function(test5)

test6 <- get_deaths_per_federal_states(cov_data, 
                                       age_group_start = "A15",
                                       age_group_end = "A59")
plot_function(test6)

test7 <- get_deaths_per_federal_states(cov_data, 
                                       age_group_start = "A15",
                                       age_group_end = "A59",
                                       federal_state = c("Bayern", "Berlin"),
                                       date_start = "2020/11/19",
                                       date_end = "2021/03/19")

test8 <- get_deaths_per_district(cov_data, 
                                 age_group_start = "A15", 
                                 age_group_end = "A59", 
                                 date_start = "2020/11/19", 
                                 date_end = "2021/02/19")
plot_function(test8)

test9 <- get_deaths_per_district(cov_data, 
                                 district = c("SK Flensburg", "SK Krefeld"), 
                                 date_start = "2020/11/19", 
                                 date_end = "2021/02/19")
plot_function(test9)

test10 <- get_deaths_per_district(cov_data, 
                                 district = c("SK Flensburg", "SK Krefeld"))
plot_function(test10)

test11 <- get_deaths_per_district(cov_data, 
                                  district = c("SK Flensburg", "SK Krefeld","LK Mei�en"),
                                  age_group_start = "A35",
                                  age_group_end = "A80")
plot_function(test11)

test12 <- get_deaths_per_district(cov_data, 
                                 age_group_start = "A15",
                                 age_group_end = "A59")
plot_function(test12)

test13 <- get_deaths_per_district(cov_data, 
                                 age_group_start = "A15",
                                 age_group_end = "A59",
                                 district = c("SK Flendsburg", "LK H�xter"),
                                 date_start = "2020/11/19",
                                 date_end = "2021/03/19")


test13


tt1 <- get_infections_overall(cov_data, date_start = "2021/01/02", date_end = "2021/06/02")

plot_function(tt1)



### Meldedatum - Refdatum (-werte zu finden -> Unclean data?)

cov_data2
cov_data2 %>% 
  filter(IstErkrankungsbeginn == "1") %>% 
  group_by(Meldedatum) -> result
attr(result, "flag") <- "f_inf_Datum"
result$date_diff
result %>% 
  ggplot(aes(x = as.Date(Meldedatum), y = date_diff)) +
  xlab("Meldedatum") +
  geom_line()

### Meldedatum - Refdatum (-werte zu finden -> Unclean data?)


xt1 <- get_fallsterblichkeit_overall(cov_data, age_group_start = "A15", age_group_end = "A80")
xt1
xt2 <- get_fallsterblichkeit_overall(cov_data)
plot_Fallsterblichkeit(xt2)


# passt schon 


source("R/filter_functions.R")
source("R/plot_functions.R")

cov_data <- read.csv("data.csv")

# --- plot incidence correlation matrix ---
incidences_df <- get_incidence_per_district(cov_data, 7)
incidence_correlation_pairs <- get_correlation_for_incidence_pairs(incidences_df)

plot_incidence_correlations_matrix(incidence_correlation_pairs,
                                   districts = c("SK Bochum", "SK Dortmund", "LK Esslingen"))

# --- plot incidence correlation barchart ---
incidences_df <- get_incidence_per_district(cov_data, 7)
incidence_correlation_pairs <- get_correlation_for_incidence_pairs(incidences_df)

plot_incidence_correlations_barchart(incidence_correlation_pairs)
