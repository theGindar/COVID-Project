#library("testthat")

#source("R/filter_functions.R")
#source("R/weather.R")
#options(warn = -1)
#cov_data <- read.csv("extdata/data.csv")
# cov_data = cov_data[seq(1, nrow(cov_data), 50), ]

# Test specification of Altersgruppe, Meldedatum

testthat::test_that("Method is taking wrong if case",{
  options(warn = -1)
  cov_data <- read.csv("extdata/data.csv")
  test1 <- get_deaths_per_federal_states(cov_data,
                                         age_group_start = "A15",
                                         age_group_end = "A59",
                                         date_start = "2020/11/19",
                                         date_end = "2021/02/19")
  testthat::expect_equal(attr(test1,"names"), c("Altersgruppe", "Meldedatum", "Deaths"))
})

# Test specification of Bundesland, Meldedatum

testthat::test_that("Method is taking wrong if case",{
  options(warn = -1)
  cov_data <- read.csv("extdata/data.csv")
  test2 <- get_deaths_per_federal_states(cov_data,
                                         federal_state = c("Hessen", "Berlin"),
                                         date_start = "2020/11/19",
                                         date_end = "2021/02/19")
  testthat::expect_equal(attr(test2,"names"), c("Bundesland","Meldedatum", "Deaths"))
})

# Test specification of Bundesland

testthat::test_that("Method is taking wrong if case",{
  options(warn = -1)
  cov_data <- read.csv("extdata/data.csv")
  test3 <- get_deaths_per_federal_states(cov_data,
                                         federal_state = c("Hessen", "Berlin", "Bayern"),
  )
  testthat::expect_equal(attr(test3,"names"), c("Bundesland", "Deaths"))
})

# Test specification of Bundesland, Altersgruppe

testthat::test_that("Method is taking wrong if case",{
  options(warn = -1)
  cov_data <- read.csv("extdata/data.csv")
  test4 <- get_deaths_per_federal_states(cov_data,
                                         federal_state = c("Hessen", "Berlin"),
                                         age_group_start = "A35",
                                         age_group_end = "A79"
  )
  testthat::expect_equal(attr(test4,"names"), c("Bundesland","Altersgruppe", "Deaths"))
})

# Test specification of Meldedatum

testthat::test_that("Method is taking wrong if case",{
  options(warn = -1)
  cov_data <- read.csv("extdata/data.csv")
  test5 <- get_deaths_per_federal_states(cov_data,
                                         date_start = "2020/11/19",
                                         date_end = "2021/02/19"
  )
  testthat::expect_equal(attr(test5,"names"), c("Meldedatum", "Deaths"))
})

# Test specification of Altersgruppe

testthat::test_that("Method is taking wrong if case",{
  options(warn = -1)
  cov_data <- read.csv("extdata/data.csv")
  test6 <- get_deaths_per_federal_states(cov_data,
                                         age_group_start = "A15",
                                         age_group_end = "A59")
  testthat::expect_equal(attr(test6,"names"), c("Bundesland","Altersgruppe", "Deaths"))
})

# Test specification of Altersgruppe, Meldedatum, Bundesland

testthat::test_that("Method is taking wrong if case",{
  options(warn = -1)
  cov_data <- read.csv("extdata/data.csv")
  test7 <- get_deaths_per_federal_states(cov_data,
                                         age_group_start = "A15",
                                         age_group_end = "A59",
                                         federal_state = c("Bayern", "Berlin"),
                                         date_start = "2020/11/19",
                                         date_end = "2021/03/19")
  testthat::expect_equal(attr(test7,"names"), c("Bundesland","Altersgruppe","Meldedatum","Deaths"))
})

### Distrikte testen

# Test specification of Altersgruppe, Meldedatum

testthat::test_that("Method is taking wrong if case",{
  options(warn = -1)
  cov_data <- read.csv("extdata/data.csv")
  test8 <- get_deaths_per_district(sub_data,
                                         age_group_start = "A15",
                                         age_group_end = "A59",
                                         date_start = "2020/11/19",
                                         date_end = "2021/02/19")
  testthat::expect_equal(attr(test8,"names"), c("Altersgruppe", "Meldedatum", "Deaths"))
})

# Test specification of Landkreis, Meldedatum

testthat::test_that("Method is taking wrong if case",{
  options(warn = -1)
  cov_data <- read.csv("extdata/data.csv")
  test9 <- get_deaths_per_district(cov_data,
                                         district = c("SK Flensburg", "SK Krefeld"),
                                         date_start = "2020/11/19",
                                         date_end = "2021/02/19")
  testthat::expect_equal(attr(test9,"names"), c("IdLandkreis", "Landkreis","Meldedatum", "Deaths"))
})

# Test specification of Landkreis

testthat::test_that("Method is taking wrong if case",{
  options(warn = -1)
  cov_data <- read.csv("extdata/data.csv")
  test10 <- get_deaths_per_district(cov_data,
                                    district = c("SK Flensburg", "SK Krefeld"),
  )
  testthat::expect_equal(attr(test10,"names"), c("IdLandkreis", "Landkreis", "Deaths"))
})

# Test specification of Landkreis, Altersgruppe

testthat::test_that("Method is taking wrong if case",{
  options(warn = -1)
  cov_data <- read.csv("extdata/data.csv")
  test11 <- get_deaths_per_district(cov_data,
                                    district = c("SK Flensburg", "SK Krefeld"),
                                         age_group_start = "A35",
                                         age_group_end = "A79"
  )
  testthat::expect_equal(attr(test11,"names"), c("IdLandkreis", "Landkreis","Altersgruppe", "Deaths"))
})

# Test specification of Landkreis

testthat::test_that("Method is taking wrong if case",{
  options(warn = -1)
  cov_data <- read.csv("extdata/data.csv")
  test12 <- get_deaths_per_district(cov_data,
                                         date_start = "2020/11/19",
                                         date_end = "2021/02/19"
  )
  testthat::expect_equal(attr(test12,"names"), c("Meldedatum", "Deaths"))
})

# Test specification of Altersgruppe

testthat::test_that("Method is taking wrong if case",{
  options(warn = -1)
  cov_data <- read.csv("extdata/data.csv")
  test13 <- get_deaths_per_district(cov_data,
                                         age_group_start = "A15",
                                         age_group_end = "A59")
  testthat::expect_equal(attr(test13,"names"), c("Altersgruppe", "Deaths"))
})

# Test specification of Altersgruppe, Meldedatum, Landkreis

testthat::test_that("Method is taking wrong if case",{
  options(warn = -1)
  cov_data <- read.csv("extdata/data.csv")
  test14 <- get_deaths_per_district(cov_data,
                                         age_group_start = "A15",
                                         age_group_end = "A59",
                                    district = c("SK Flensburg", "SK Krefeld"),
                                         date_start = "2020/11/19",
                                         date_end = "2021/03/19")
  testthat::expect_equal(attr(test14,"names"), c("IdLandkreis", "Landkreis","Altersgruppe","Meldedatum","Deaths"))
})

### Infections + Bundesland testen
#
# Test specification of Altersgruppe, Meldedatum

testthat::test_that("Method is taking wrong if case",{
  options(warn = -1)
  cov_data <- read.csv("extdata/data.csv")
  test15 <- get_infections_per_federal_states(sub_data,
                                         age_group_start = "A15",
                                         age_group_end = "A59",
                                         date_start = "2020/11/19",
                                         date_end = "2021/02/19")
  testthat::expect_equal(attr(test15,"names"), c("Altersgruppe", "Meldedatum", "Infections"))
})

# Test specification of Bundesland, Meldedatum

testthat::test_that("Method is taking wrong if case",{
  options(warn = -1)
  cov_data <- read.csv("extdata/data.csv")
  test16 <- get_infections_per_federal_states(cov_data,
                                         federal_state = c("Hessen", "Berlin"),
                                         date_start = "2020/11/19",
                                         date_end = "2021/02/19")
  testthat::expect_equal(attr(test16,"names"), c("Bundesland","Meldedatum", "Infections"))
})

# Test specification of Bundesland

testthat::test_that("Method is taking wrong if case",{
  options(warn = -1)
  cov_data <- read.csv("extdata/data.csv")
  test17 <- get_infections_per_federal_states(cov_data,
                                         federal_state = c("Hessen", "Berlin", "Bayern"),
  )
  testthat::expect_equal(attr(test17,"names"), c("Bundesland", "Infections"))
})

# Test specification of Bundesland, Altersgruppe

testthat::test_that("Method is taking wrong if case",{
  options(warn = -1)
  cov_data <- read.csv("extdata/data.csv")
  test18 <- get_infections_per_federal_states(cov_data,
                                         federal_state = c("Hessen", "Berlin"),
                                         age_group_start = "A35",
                                         age_group_end = "A79"
  )
  testthat::expect_equal(attr(test18,"names"), c("Bundesland","Altersgruppe", "Infections"))
})

# Test specification of Meldedatum

testthat::test_that("Method is taking wrong if case",{
  options(warn = -1)
  cov_data <- read.csv("extdata/data.csv")
  test19 <- get_infections_per_federal_states(cov_data,
                                         date_start = "2020/11/19",
                                         date_end = "2021/02/19"
  )
  testthat::expect_equal(attr(test19,"names"), c("Meldedatum", "Infections"))
})

# Test specification of Altersgruppe

testthat::test_that("Method is taking wrong if case",{
  options(warn = -1)
  cov_data <- read.csv("extdata/data.csv")
  test20 <- get_infections_per_federal_states(cov_data,
                                         age_group_start = "A15",
                                         age_group_end = "A59")
  testthat::expect_equal(attr(test20,"names"), c("Bundesland","Altersgruppe", "Infections"))
})

# Test specification of Altersgruppe, Meldedatum, Bundesland

testthat::test_that("Method is taking wrong if case",{
  options(warn = -1)
  cov_data <- read.csv("extdata/data.csv")
  test21 <- get_infections_per_federal_states(cov_data,
                                         age_group_start = "A15",
                                         age_group_end = "A59",
                                         federal_state = c("Bayern", "Berlin"),
                                         date_start = "2020/11/19",
                                         date_end = "2021/03/19")
  testthat::expect_equal(attr(test21,"names"), c("Bundesland","Altersgruppe","Meldedatum","Infections"))
})


### Distrikte testen mit Infections

# Test specification of Altersgruppe, Meldedatum

testthat::test_that("Method is taking wrong if case",{
  options(warn = -1)
  cov_data <- read.csv("extdata/data.csv")
  test22 <- get_infections_per_district(sub_data,
                                   age_group_start = "A15",
                                   age_group_end = "A59",
                                   date_start = "2020/11/19",
                                   date_end = "2021/02/19")
  testthat::expect_equal(attr(test22,"names"), c("Altersgruppe", "Meldedatum", "Infections"))
})

# Test specification of Landkreis, Meldedatum

testthat::test_that("Method is taking wrong if case",{
  options(warn = -1)
  cov_data <- read.csv("extdata/data.csv")
  test23 <- get_infections_per_district(cov_data,
                                   district = c("SK Flensburg", "SK Krefeld"),
                                   date_start = "2020/11/19",
                                   date_end = "2021/02/19")
  testthat::expect_equal(attr(test23,"names"), c("IdLandkreis", "Landkreis","Meldedatum", "Infections"))
})

# Test specification of Landkreis

testthat::test_that("Method is taking wrong if case",{
  options(warn = -1)
  cov_data <- read.csv("extdata/data.csv")
  test24 <- get_infections_per_district(cov_data,
                                    district = c("SK Flensburg", "SK Krefeld"),
  )
  testthat::expect_equal(attr(test24,"names"), c("IdLandkreis", "Landkreis", "Infections"))
})

# Test specification of Landkreis, Altersgruppe

testthat::test_that("Method is taking wrong if case",{
  options(warn = -1)
  cov_data <- read.csv("extdata/data.csv")
  test25 <- get_infections_per_district(cov_data,
                                    district = c("SK Flensburg", "SK Krefeld"),
                                    age_group_start = "A35",
                                    age_group_end = "A79"
  )
  testthat::expect_equal(attr(test25,"names"), c("IdLandkreis", "Landkreis","Altersgruppe", "Infections"))
})

# Test specification of Meldedatum

testthat::test_that("Method is taking wrong if case",{
  options(warn = -1)
  cov_data <- read.csv("extdata/data.csv")
  test26 <- get_infections_per_district(cov_data,
                                    date_start = "2020/11/19",
                                    date_end = "2021/02/19"
  )
  testthat::expect_equal(attr(test26,"names"), c("Meldedatum", "Infections"))
})

# Test specification of Altersgruppe

testthat::test_that("Method is taking wrong if case",{
  options(warn = -1)
  cov_data <- read.csv("extdata/data.csv")
  test27 <- get_infections_per_district(cov_data,
                                    age_group_start = "A15",
                                    age_group_end = "A59")
  testthat::expect_equal(attr(test27,"names"), c("Altersgruppe", "Infections"))
})

# Test specification of Altersgruppe, Meldedatum, Landkreis

testthat::test_that("Method is taking wrong if case",{
  options(warn = -1)
  cov_data <- read.csv("extdata/data.csv")
  test28 <- get_infections_per_district(cov_data,
                                    age_group_start = "A15",
                                    age_group_end = "A59",
                                    district = c("SK Flensburg", "SK Krefeld"),
                                    date_start = "2020/11/19",
                                    date_end = "2021/03/19")
  testthat::expect_equal(attr(test28,"names"), c("IdLandkreis", "Landkreis","Altersgruppe","Meldedatum","Infections"))
})

### Recovered + Bundesland testen
#
# Test specification of Altersgruppe, Meldedatum

testthat::test_that("Method is taking wrong if case",{
  options(warn = -1)
  cov_data <- read.csv("extdata/data.csv")
  test29 <- get_recovered_per_federal_states(sub_data,
                                              age_group_start = "A15",
                                              age_group_end = "A59",
                                              date_start = "2020/11/19",
                                              date_end = "2021/02/19")
  testthat::expect_equal(attr(test29,"names"), c("Altersgruppe", "Meldedatum", "Recovered"))
})

# Test specification of Bundesland, Meldedatum

testthat::test_that("Method is taking wrong if case",{
  options(warn = -1)
  cov_data <- read.csv("extdata/data.csv")
  test30 <- get_recovered_per_federal_states(cov_data,
                                              federal_state = c("Hessen", "Berlin"),
                                              date_start = "2020/11/19",
                                              date_end = "2021/02/19")
  testthat::expect_equal(attr(test30,"names"), c("Bundesland","Meldedatum", "Recovered"))
})

# Test specification of Bundesland

testthat::test_that("Method is taking wrong if case",{
  options(warn = -1)
  cov_data <- read.csv("extdata/data.csv")
  test31 <- get_recovered_per_federal_states(cov_data,
                                              federal_state = c("Hessen", "Berlin", "Bayern"),
  )
  testthat::expect_equal(attr(test31,"names"), c("Bundesland", "Recovered"))
})

# Test specification of Bundesland, Altersgruppe

testthat::test_that("Method is taking wrong if case",{
  options(warn = -1)
  cov_data <- read.csv("extdata/data.csv")
  test32 <- get_recovered_per_federal_states(cov_data,
                                              federal_state = c("Hessen", "Berlin"),
                                              age_group_start = "A35",
                                              age_group_end = "A79"
  )
  testthat::expect_equal(attr(test32,"names"), c("Bundesland","Altersgruppe", "Recovered"))
})

# Test specification of Meldedatum

testthat::test_that("Method is taking wrong if case",{
  options(warn = -1)
  cov_data <- read.csv("extdata/data.csv")
  test33 <- get_recovered_per_federal_states(cov_data,
                                              date_start = "2020/11/19",
                                              date_end = "2021/02/19"
  )
  testthat::expect_equal(attr(test33,"names"), c("Meldedatum", "Recovered"))
})

# Test specification of Altersgruppe

testthat::test_that("Method is taking wrong if case",{
  options(warn = -1)
  cov_data <- read.csv("extdata/data.csv")
  test34 <- get_recovered_per_federal_states(cov_data,
                                              age_group_start = "A15",
                                              age_group_end = "A59")
  testthat::expect_equal(attr(test34,"names"), c("Bundesland","Altersgruppe", "Recovered"))
})

# Test specification of Altersgruppe, Meldedatum, Bundesland

testthat::test_that("Method is taking wrong if case",{
  options(warn = -1)
  cov_data <- read.csv("extdata/data.csv")
  test35 <- get_recovered_per_federal_states(cov_data,
                                              age_group_start = "A15",
                                              age_group_end = "A59",
                                              federal_state = c("Bayern", "Berlin"),
                                              date_start = "2020/11/19",
                                              date_end = "2021/03/19")
  testthat::expect_equal(attr(test35,"names"), c("Bundesland","Altersgruppe","Meldedatum","Recovered"))
})

### Distrikte testen mit Recovered

# Test specification of Altersgruppe, Meldedatum

testthat::test_that("Method is taking wrong if case",{
  options(warn = -1)
  cov_data <- read.csv("extdata/data.csv")
  test36 <- get_recovered_per_district(sub_data,
                                        age_group_start = "A15",
                                        age_group_end = "A59",
                                        date_start = "2020/11/19",
                                        date_end = "2021/02/19")
  testthat::expect_equal(attr(test36,"names"), c("Altersgruppe", "Meldedatum", "Recovered"))
})

# Test specification of Landkreis, Meldedatum

testthat::test_that("Method is taking wrong if case",{
  options(warn = -1)
  cov_data <- read.csv("extdata/data.csv")
  test37 <- get_recovered_per_district(cov_data,
                                        district = c("SK Flensburg", "SK Krefeld"),
                                        date_start = "2020/11/19",
                                        date_end = "2021/02/19")
  testthat::expect_equal(attr(test37,"names"), c("IdLandkreis", "Landkreis","Meldedatum", "Recovered"))
})

# Test specification of Landkreis

testthat::test_that("Method is taking wrong if case",{
  options(warn = -1)
  cov_data <- read.csv("extdata/data.csv")
  test38 <- get_recovered_per_district(cov_data,
                                        district = c("SK Flensburg", "SK Krefeld"),
  )
  testthat::expect_equal(attr(test38,"names"), c("IdLandkreis", "Landkreis", "Recovered"))
})

# Test specification of Landkreis, Altersgruppe

testthat::test_that("Method is taking wrong if case",{
  options(warn = -1)
  cov_data <- read.csv("extdata/data.csv")
  test39 <- get_recovered_per_district(cov_data,
                                        district = c("SK Flensburg", "SK Krefeld"),
                                        age_group_start = "A35",
                                        age_group_end = "A79"
  )
  testthat::expect_equal(attr(test39,"names"), c("IdLandkreis", "Landkreis","Altersgruppe", "Recovered"))
})

# Test specification of Meldedatum

testthat::test_that("Method is taking wrong if case",{
  options(warn = -1)
  cov_data <- read.csv("extdata/data.csv")
  test40 <- get_recovered_per_district(cov_data,
                                        date_start = "2020/11/19",
                                        date_end = "2021/02/19"
  )
  testthat::expect_equal(attr(test40,"names"), c("Meldedatum", "Recovered"))
})

# Test specification of Altersgruppe

testthat::test_that("Method is taking wrong if case",{
  options(warn = -1)
  cov_data <- read.csv("extdata/data.csv")
  test41 <- get_recovered_per_district(cov_data,
                                        age_group_start = "A15",
                                        age_group_end = "A59")
  testthat::expect_equal(attr(test41,"names"), c("Altersgruppe", "Recovered"))
})

# Test specification of Altersgruppe, Meldedatum, Landkreis

testthat::test_that("Method is taking wrong if case",{
  options(warn = -1)
  cov_data <- read.csv("extdata/data.csv")
  test42 <- get_recovered_per_district(cov_data,
                                        age_group_start = "A15",
                                        age_group_end = "A59",
                                        district = c("SK Flensburg", "SK Krefeld"),
                                        date_start = "2020/11/19",
                                        date_end = "2021/03/19")
  testthat::expect_equal(attr(test42,"names"), c("IdLandkreis", "Landkreis","Altersgruppe","Meldedatum","Recovered"))
})

# Test correct output of get_infections_overall

testthat::test_that("Method is giving wrong output",{
  options(warn = -1)
  cov_data <- read.csv("extdata/data.csv")
  test43 <- get_fallsterblichkeit_overall(cov_data)
  testthat::expect_equal(attr(test43,"names"), c("Meldedatum","Deaths","Infections","Fallsterblichkeit"))
})

testthat::test_that("Method is giving wrong output",{
  options(warn = -1)
  cov_data <- read.csv("extdata/data.csv")
  test44 <- get_fallsterblichkeit_overall(cov_data,age_group_start = "A15", age_group_end = "A59")
  testthat::expect_equal(attr(test44,"names"), c("Altersgruppe","Deaths","Infections","Fallsterblichkeit"))
})

# Test get_incidence_per_district
testthat::test_that("Method is giving wrong output",{
  options(warn = -1)
  cov_data <- read.csv("extdata/data.csv")
  test45 <- get_incidence_per_district(cov_data,
                                       age_group_start = "A15",
                                       district = c("SK Bremerhaven"),
                                       age_group_end = "A59",
                                       date_start = "2020/01/01",
                                       date_end = "2021/06/01")
  testthat::expect_equal(attr(test45,"names"), c("Meldedatum","IdLandkreis","Landkreis","Inzidenz"))
})

# Test get_correlation_for_incidence_pairs
testthat::test_that("Method is giving wrong output",{
  options(warn = -1)
  cov_data <- read.csv("extdata/data.csv")
  test46_1 <- get_incidence_per_district(cov_data,
                                         age_group_start = "A15",
                                         district = c("SK Bremerhaven", "SK Kiel", "SK Flensburg"),
                                         age_group_end = "A59",
                                         date_start = "2020/01/01",
                                         date_end = "2021/06/01")
  test46_2 <- get_correlation_for_incidence_pairs(test46_1)
  testthat::expect_equal(attr(test46_2,"names"), c("IdLandkreis_1","Landkreis_1","IdLandkreis_2", "Landkreis_2", "Correlation"))
})

# Test weather function
testthat::test_that("Method is giving wrong output",{
  options(warn = -1)
  cov_data <- read.csv("extdata/data.csv")
  test47 <- add_weather_data(cov_data)

  testthat::expect_equal(attr(test47,"names"), c("ObjectId","IdBundesland",
                                       "Bundesland", "Landkreis",
                                       "Altersgruppe", "Geschlecht",
                                       "AnzahlFall", "AnzahlTodesfall",
                                       "Meldedatum", "IdLandkreis",
                                       "Datenstand", "NeuerFall",
                                       "NeuerTodesfall", "Refdatum",
                                       "NeuGenesen", "AnzahlGenesen",
                                       "IstErkrankungsbeginn", "Altersgruppe2",
                                       "Temperatur"))
})
