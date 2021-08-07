source("R/filter_functions.R")
library("testthat")

sub_data = cov_data[seq(1, nrow(cov_data), 50), ]

# Test specification of Altersgruppe, Meldedatum

test_that("Method is taking wrong if case",{
  test1 <- get_deaths_per_federal_states(sub_data,
                                         age_group_start = "A15",
                                         age_group_end = "A59",
                                         date_start = "2020/11/19",
                                         date_end = "2021/02/19")
  expect_equal(attr(test1,"names"), c("Altersgruppe", "Meldedatum", "Deaths"))
})

# Test specification of Bundesland, Meldedatum

test_that("Method is taking wrong if case",{
  test2 <- get_deaths_per_federal_states(cov_data,
                                         federal_state = c("Hessen", "Berlin"),
                                         date_start = "2020/11/19",
                                         date_end = "2021/02/19")
  expect_equal(attr(test2,"names"), c("Bundesland","Meldedatum", "Deaths"))
})

# Test specification of Bundesland

test_that("Method is taking wrong if case",{
  test3 <- get_deaths_per_federal_states(cov_data,
                                         federal_state = c("Hessen", "Berlin", "Bayern"),
  )
  expect_equal(attr(test3,"names"), c("Bundesland", "Deaths"))
})

# Test specification of Bundesland, Altersgruppe

test_that("Method is taking wrong if case",{
  test4 <- get_deaths_per_federal_states(cov_data,
                                         federal_state = c("Hessen", "Berlin"),
                                         age_group_start = "A35",
                                         age_group_end = "A79"
  )
  expect_equal(attr(test4,"names"), c("Bundesland","Altersgruppe", "Deaths"))
})

# Test specification of Meldedatum

test_that("Method is taking wrong if case",{
  test5 <- get_deaths_per_federal_states(cov_data,
                                         date_start = "2020/11/19",
                                         date_end = "2021/02/19"
  )
  expect_equal(attr(test5,"names"), c("Meldedatum", "Deaths"))
})

# Test specification of Altersgruppe

test_that("Method is taking wrong if case",{
  test6 <- get_deaths_per_federal_states(cov_data,
                                         age_group_start = "A15",
                                         age_group_end = "A59")
  expect_equal(attr(test6,"names"), c("Bundesland","Altersgruppe", "Deaths"))
})

# Test specification of Altersgruppe, Meldedatum, Bundesland

test_that("Method is taking wrong if case",{
  test7 <- get_deaths_per_federal_states(cov_data,
                                         age_group_start = "A15",
                                         age_group_end = "A59",
                                         federal_state = c("Bayern", "Berlin"),
                                         date_start = "2020/11/19",
                                         date_end = "2021/03/19")
  expect_equal(attr(test7,"names"), c("Bundesland","Altersgruppe","Meldedatum","Deaths"))
})

### Distrikte testen

# Test specification of Altersgruppe, Meldedatum

test_that("Method is taking wrong if case",{
  test8 <- get_deaths_per_district(sub_data,
                                         age_group_start = "A15",
                                         age_group_end = "A59",
                                         date_start = "2020/11/19",
                                         date_end = "2021/02/19")
  expect_equal(attr(test8,"names"), c("Altersgruppe", "Meldedatum", "Deaths"))
})

# Test specification of Landkreis, Meldedatum

test_that("Method is taking wrong if case",{
  test9 <- get_deaths_per_district(cov_data,
                                         district = c("SK Flensburg", "SK Krefeld"),
                                         date_start = "2020/11/19",
                                         date_end = "2021/02/19")
  expect_equal(attr(test9,"names"), c("Landkreis","Meldedatum", "Deaths"))
})

# Test specification of Landkreis

test_that("Method is taking wrong if case",{
  test10 <- get_deaths_per_district(cov_data,
                                    district = c("SK Flensburg", "SK Krefeld"),
  )
  expect_equal(attr(test10,"names"), c("Landkreis", "Deaths"))
})

# Test specification of Landkreis, Altersgruppe

test_that("Method is taking wrong if case",{
  test11 <- get_deaths_per_district(cov_data,
                                    district = c("SK Flensburg", "SK Krefeld"),
                                         age_group_start = "A35",
                                         age_group_end = "A79"
  )
  expect_equal(attr(test11,"names"), c("Landkreis","Altersgruppe", "Deaths"))
})

# Test specification of Landkreis

test_that("Method is taking wrong if case",{
  test12 <- get_deaths_per_district(cov_data,
                                         date_start = "2020/11/19",
                                         date_end = "2021/02/19"
  )
  expect_equal(attr(test12,"names"), c("Meldedatum", "Deaths"))
})

# Test specification of Altersgruppe

test_that("Method is taking wrong if case",{
  test13 <- get_deaths_per_district(cov_data,
                                         age_group_start = "A15",
                                         age_group_end = "A59")
  expect_equal(attr(test13,"names"), c("Altersgruppe", "Deaths"))
})

# Test specification of Altersgruppe, Meldedatum, Landkreis

test_that("Method is taking wrong if case",{
  test14 <- get_deaths_per_district(cov_data,
                                         age_group_start = "A15",
                                         age_group_end = "A59",
                                    district = c("SK Flensburg", "SK Krefeld"),
                                         date_start = "2020/11/19",
                                         date_end = "2021/03/19")
  expect_equal(attr(test14,"names"), c("Landkreis","Altersgruppe","Meldedatum","Deaths"))
})

### Infections + Bundesland testen
#
# Test specification of Altersgruppe, Meldedatum

test_that("Method is taking wrong if case",{
  test15 <- get_infections_per_federal_states(sub_data,
                                         age_group_start = "A15",
                                         age_group_end = "A59",
                                         date_start = "2020/11/19",
                                         date_end = "2021/02/19")
  expect_equal(attr(test15,"names"), c("Altersgruppe", "Meldedatum", "Infections"))
})

# Test specification of Bundesland, Meldedatum

test_that("Method is taking wrong if case",{
  test16 <- get_infections_per_federal_states(cov_data,
                                         federal_state = c("Hessen", "Berlin"),
                                         date_start = "2020/11/19",
                                         date_end = "2021/02/19")
  expect_equal(attr(test16,"names"), c("Bundesland","Meldedatum", "Infections"))
})

# Test specification of Bundesland

test_that("Method is taking wrong if case",{
  test17 <- get_infections_per_federal_states(cov_data,
                                         federal_state = c("Hessen", "Berlin", "Bayern"),
  )
  expect_equal(attr(test17,"names"), c("Bundesland", "Infections"))
})

# Test specification of Bundesland, Altersgruppe

test_that("Method is taking wrong if case",{
  test18 <- get_infections_per_federal_states(cov_data,
                                         federal_state = c("Hessen", "Berlin"),
                                         age_group_start = "A35",
                                         age_group_end = "A79"
  )
  expect_equal(attr(test18,"names"), c("Bundesland","Altersgruppe", "Infections"))
})

# Test specification of Meldedatum

test_that("Method is taking wrong if case",{
  test19 <- get_infections_per_federal_states(cov_data,
                                         date_start = "2020/11/19",
                                         date_end = "2021/02/19"
  )
  expect_equal(attr(test19,"names"), c("Meldedatum", "Infections"))
})

# Test specification of Altersgruppe

test_that("Method is taking wrong if case",{
  test20 <- get_infections_per_federal_states(cov_data,
                                         age_group_start = "A15",
                                         age_group_end = "A59")
  expect_equal(attr(test20,"names"), c("Bundesland","Altersgruppe", "Infections"))
})

# Test specification of Altersgruppe, Meldedatum, Bundesland

test_that("Method is taking wrong if case",{
  test21 <- get_infections_per_federal_states(cov_data,
                                         age_group_start = "A15",
                                         age_group_end = "A59",
                                         federal_state = c("Bayern", "Berlin"),
                                         date_start = "2020/11/19",
                                         date_end = "2021/03/19")
  expect_equal(attr(test21,"names"), c("Bundesland","Altersgruppe","Meldedatum","Infections"))
})


### Distrikte testen mit Infections

# Test specification of Altersgruppe, Meldedatum

test_that("Method is taking wrong if case",{
  test22 <- get_infections_per_district(sub_data,
                                   age_group_start = "A15",
                                   age_group_end = "A59",
                                   date_start = "2020/11/19",
                                   date_end = "2021/02/19")
  expect_equal(attr(test22,"names"), c("Altersgruppe", "Meldedatum", "Infections"))
})

# Test specification of Landkreis, Meldedatum

test_that("Method is taking wrong if case",{
  test23 <- get_infections_per_district(cov_data,
                                   district = c("SK Flensburg", "SK Krefeld"),
                                   date_start = "2020/11/19",
                                   date_end = "2021/02/19")
  expect_equal(attr(test23,"names"), c("Landkreis","Meldedatum", "Infections"))
})

# Test specification of Landkreis

test_that("Method is taking wrong if case",{
  test24 <- get_infections_per_district(cov_data,
                                    district = c("SK Flensburg", "SK Krefeld"),
  )
  expect_equal(attr(test24,"names"), c("Landkreis", "Infections"))
})

# Test specification of Landkreis, Altersgruppe

test_that("Method is taking wrong if case",{
  test25 <- get_infections_per_district(cov_data,
                                    district = c("SK Flensburg", "SK Krefeld"),
                                    age_group_start = "A35",
                                    age_group_end = "A79"
  )
  expect_equal(attr(test25,"names"), c("Landkreis","Altersgruppe", "Infections"))
})

# Test specification of Meldedatum

test_that("Method is taking wrong if case",{
  test26 <- get_infections_per_district(cov_data,
                                    date_start = "2020/11/19",
                                    date_end = "2021/02/19"
  )
  expect_equal(attr(test26,"names"), c("Meldedatum", "Infections"))
})

# Test specification of Altersgruppe

test_that("Method is taking wrong if case",{
  test27 <- get_infections_per_district(cov_data,
                                    age_group_start = "A15",
                                    age_group_end = "A59")
  expect_equal(attr(test27,"names"), c("Altersgruppe", "Infections"))
})

# Test specification of Altersgruppe, Meldedatum, Landkreis

test_that("Method is taking wrong if case",{
  test28 <- get_infections_per_district(cov_data,
                                    age_group_start = "A15",
                                    age_group_end = "A59",
                                    district = c("SK Flensburg", "SK Krefeld"),
                                    date_start = "2020/11/19",
                                    date_end = "2021/03/19")
  expect_equal(attr(test28,"names"), c("Landkreis","Altersgruppe","Meldedatum","Infections"))
})

### Recovered + Bundesland testen
#
# Test specification of Altersgruppe, Meldedatum

test_that("Method is taking wrong if case",{
  test29 <- get_recovered_per_federal_states(sub_data,
                                              age_group_start = "A15",
                                              age_group_end = "A59",
                                              date_start = "2020/11/19",
                                              date_end = "2021/02/19")
  expect_equal(attr(test29,"names"), c("Altersgruppe", "Meldedatum", "Recovered"))
})

# Test specification of Bundesland, Meldedatum

test_that("Method is taking wrong if case",{
  test30 <- get_recovered_per_federal_states(cov_data,
                                              federal_state = c("Hessen", "Berlin"),
                                              date_start = "2020/11/19",
                                              date_end = "2021/02/19")
  expect_equal(attr(test30,"names"), c("Bundesland","Meldedatum", "Recovered"))
})

# Test specification of Bundesland

test_that("Method is taking wrong if case",{
  test31 <- get_recovered_per_federal_states(cov_data,
                                              federal_state = c("Hessen", "Berlin", "Bayern"),
  )
  expect_equal(attr(test31,"names"), c("Bundesland", "Recovered"))
})

# Test specification of Bundesland, Altersgruppe

test_that("Method is taking wrong if case",{
  test32 <- get_recovered_per_federal_states(cov_data,
                                              federal_state = c("Hessen", "Berlin"),
                                              age_group_start = "A35",
                                              age_group_end = "A79"
  )
  expect_equal(attr(test32,"names"), c("Bundesland","Altersgruppe", "Recovered"))
})

# Test specification of Meldedatum

test_that("Method is taking wrong if case",{
  test33 <- get_recovered_per_federal_states(cov_data,
                                              date_start = "2020/11/19",
                                              date_end = "2021/02/19"
  )
  expect_equal(attr(test33,"names"), c("Meldedatum", "Recovered"))
})

# Test specification of Altersgruppe

test_that("Method is taking wrong if case",{
  test34 <- get_recovered_per_federal_states(cov_data,
                                              age_group_start = "A15",
                                              age_group_end = "A59")
  expect_equal(attr(test34,"names"), c("Bundesland","Altersgruppe", "Recovered"))
})

# Test specification of Altersgruppe, Meldedatum, Bundesland

test_that("Method is taking wrong if case",{
  test35 <- get_recovered_per_federal_states(cov_data,
                                              age_group_start = "A15",
                                              age_group_end = "A59",
                                              federal_state = c("Bayern", "Berlin"),
                                              date_start = "2020/11/19",
                                              date_end = "2021/03/19")
  expect_equal(attr(test35,"names"), c("Bundesland","Altersgruppe","Meldedatum","Recovered"))
})

### Distrikte testen mit Recovered

# Test specification of Altersgruppe, Meldedatum

test_that("Method is taking wrong if case",{
  test22 <- get_recovered_per_district(sub_data,
                                        age_group_start = "A15",
                                        age_group_end = "A59",
                                        date_start = "2020/11/19",
                                        date_end = "2021/02/19")
  expect_equal(attr(test22,"names"), c("Altersgruppe", "Meldedatum", "Recovered"))
})

# Test specification of Landkreis, Meldedatum

test_that("Method is taking wrong if case",{
  test23 <- get_recovered_per_district(cov_data,
                                        district = c("SK Flensburg", "SK Krefeld"),
                                        date_start = "2020/11/19",
                                        date_end = "2021/02/19")
  expect_equal(attr(test23,"names"), c("Landkreis","Meldedatum", "Recovered"))
})

# Test specification of Landkreis

test_that("Method is taking wrong if case",{
  test24 <- get_recovered_per_district(cov_data,
                                        district = c("SK Flensburg", "SK Krefeld"),
  )
  expect_equal(attr(test24,"names"), c("Landkreis", "Recovered"))
})

# Test specification of Landkreis, Altersgruppe

test_that("Method is taking wrong if case",{
  test25 <- get_recovered_per_district(cov_data,
                                        district = c("SK Flensburg", "SK Krefeld"),
                                        age_group_start = "A35",
                                        age_group_end = "A79"
  )
  expect_equal(attr(test25,"names"), c("Landkreis","Altersgruppe", "Recovered"))
})

# Test specification of Meldedatum

test_that("Method is taking wrong if case",{
  test26 <- get_recovered_per_district(cov_data,
                                        date_start = "2020/11/19",
                                        date_end = "2021/02/19"
  )
  expect_equal(attr(test26,"names"), c("Meldedatum", "Recovered"))
})

# Test specification of Altersgruppe

test_that("Method is taking wrong if case",{
  test27 <- get_recovered_per_district(cov_data,
                                        age_group_start = "A15",
                                        age_group_end = "A59")
  expect_equal(attr(test27,"names"), c("Altersgruppe", "Recovered"))
})

# Test specification of Altersgruppe, Meldedatum, Landkreis

test_that("Method is taking wrong if case",{
  test28 <- get_recovered_per_district(cov_data,
                                        age_group_start = "A15",
                                        age_group_end = "A59",
                                        district = c("SK Flensburg", "SK Krefeld"),
                                        date_start = "2020/11/19",
                                        date_end = "2021/03/19")
  expect_equal(attr(test28,"names"), c("Landkreis","Altersgruppe","Meldedatum","Recovered"))
})

