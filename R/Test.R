source("R/filter_functions.R")


test1 <- get_deaths_per_federal_states(cov_data,
                                       age_group_start = "A15",
                                       age_group_end = "A59",
                                       date_start = "2020/11/19",
                                       date_end = "2021/02/19")

# testing get_deaths_per_federal_state for every possible outcome

testthat::test_that("If clauses are not clear",{
  expect_equal(test1 <- get_deaths_per_federal_states(cov_data,
                                                      age_group_start = "A15",
                                                      age_group_end = "A59",
                                                      date_start = "2020/11/19",
                                                      date_end = "2021/02/19"))

})
