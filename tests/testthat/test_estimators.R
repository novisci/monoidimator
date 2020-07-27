library(survival, verbose = FALSE)
library(dplyr, verbose = FALSE)

test_that("cumrisk without tied data", {

  zz <- aml %>% filter(!duplicated(time))
  m <- survfit(Surv(time, !status) ~ 1, data = zz)
  zz <- zz %>%
    arrange(time) %>%
    mutate(
      Y    = if_else(status == 1, time, Inf),
      C    = if_else(status == 0, time, Inf),
      PrDel = summary(m, times = time)$surv
    )

  crisk <- make_cumrisk_estimator(purrr::transpose(zz))



  crisk_est <-
    zz %>%
    mutate(hat = crisk()[-1]) %>%
    filter(status == 1) %>%
    group_by(time) %>%
    filter(row_number() == n()) %>%
    pull(hat)


  km    <- summary(survfit(Surv(time, status) ~ 1, data = zz))
  expect_equal(crisk_est, 1 - km$surv, tolerance = 1e-15)
})




test_that("cumrisk with tied data", {

  zz <- aml
  m <- survfit(Surv(time, !status) ~ 1, data = zz)
  zz <- zz %>%
    arrange(time) %>%
    mutate(
      Y    = if_else(status == 1, time, Inf),
      C    = if_else(status == 0, time, Inf)
    )

  zz2 <-
    zz %>%
    group_by(time) %>%
    summarise(
      e = sum(status),
      n = n(),
      .groups = "drop_last"
    ) %>%
    mutate(
      Y = if_else(e > 0, time, Inf),
      C = if_else(e == 0, time, Inf),
      PrDel = summary(m, times = time)$surv
    )

  crisk <- make_cumrisk_estimator2(purrr::transpose(zz2))
  km    <- summary(survfit(Surv(time, status) ~ 1, data = aml))

  crisk_est <-
    zz2 %>%
    mutate(hat = crisk()[-1]) %>%
    filter(e > 0) %>%
    pull(hat)

  crisk_est
  1 - km$surv
  expect_equal(crisk_est, 1 - km$surv, tolerance = 1e-15)


  zz3 <-
  zz2 %>%
    filter(e > 0)
  crisk <- make_cumrisk_estimator2(purrr::transpose(zz3))
  km    <- summary(survfit(Surv(time, status) ~ 1, data = aml))

  crisk_est <-
    zz3 %>%
    mutate(hat = crisk()[-1]*20/23) %>%
    filter(e > 0) %>%
    pull(hat)

  crisk_est
  1 - km$surv
  expect_equal(crisk_est, 1 - km$surv, tolerance = 1e-15)
})

