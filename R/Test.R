source("R/filter_functions.R")
library("testthat")

sub_data = cov_data[seq(1, nrow(cov_data), 50), ]


test_that("testing deaths age date",{
  test1 <- get_deaths_per_federal_states(sub_data,
                                         age_group_start = "A15",
                                         age_group_end = "A59",
                                         date_start = "2020/11/19",
                                         date_end = "2021/02/19")
  expect_equal(attr(test,"names"), c("Altersgruppe", "Meldedatum", "Deaths"))
})


test_that("adssad",{
  test <- get_deaths_per_federal_states(cov_data,
                                         federal_state = c("Hessen", "Berlin"),
                                         date_start = "2020/11/19",
                                         date_end = "2021/02/19")
  expect_equal(attr(test,"names"), c("Bundesland","Meldedatum", "Deaths"))
})


test_that("adssad",{
  test <- get_deaths_per_federal_states(cov_data,
                                        federal_state = c("Hessen", "Berlin"),
                                        date_start = "2020/11/19",
                                        date_end = "2021/02/19")
  expect_equal(attr(test,"names"), c("Bundesland","Meldedatum", "Deaths"))
})

test_that("adssad",{
  test <- get_deaths_per_federal_states(cov_data,
                                        federal_state = c("Hessen", "Berlin"),
                                        date_start = "2020/11/19",
                                        date_end = "2021/02/19")
  expect_equal(attr(test,"names"), c("Bundesland","Meldedatum", "Deaths"))
})

test_that("adssad",{
  test <- get_deaths_per_federal_states(cov_data,
                                        federal_state = c("Hessen", "Berlin"),
                                        date_start = "2020/11/19",
                                        date_end = "2021/02/19")
  expect_equal(attr(test,"names"), c("Bundesland","Meldedatum", "Deaths"))
})

test_that("adssad",{
  test <- get_deaths_per_federal_states(cov_data,
                                        federal_state = c("Hessen", "Berlin"),
                                        date_start = "2020/11/19",
                                        date_end = "2021/02/19")
  expect_equal(attr(test,"names"), c("Bundesland","Meldedatum", "Deaths"))
})

test_that("adssad",{
  test <- get_deaths_per_federal_states(cov_data,
                                        federal_state = c("Hessen", "Berlin"),
                                        date_start = "2020/11/19",
                                        date_end = "2021/02/19")
  expect_equal(attr(test,"names"), c("Bundesland","Meldedatum", "Deaths"))
})


test_that("adssad",{
  test <- get_deaths_per_federal_states(cov_data,
                                        federal_state = c("Hessen", "Berlin"),
                                        date_start = "2020/11/19",
                                        date_end = "2021/02/19")
  expect_equal(attr(test,"names"), c("Bundesland","Meldedatum", "Deaths"))
})
