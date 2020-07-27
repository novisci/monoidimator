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
      C    = if_else(status == 0, time, Inf),
      PrDel = summary(m, times = time)$surv
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

  crisk1 <- make_cumrisk_estimator(purrr::transpose(zz))
  crisk2 <- make_cumrisk_estimator2(purrr::transpose(zz2))
  km    <- summary(survfit(Surv(time, status) ~ 1, data = aml))

  crisk_est1 <-
    zz %>%
    mutate(hat = crisk1()[-1]) %>%
    filter(status == 1) %>%
    group_by(time) %>%
    summarise(hat = max(hat)) %>%
    pull(hat)

  crisk_est2 <-
    zz2 %>%
    mutate(hat = crisk2()[-1]) %>%
    filter(e > 0) %>%
    pull(hat)

  zz3 <- zz2 %>% filter(e > 0)
  crisk3 <- make_cumrisk_estimator2(purrr::transpose(zz3))

  crisk_est3 <-
    zz3 %>%
    mutate(hat = crisk3()[-1]*20/23) %>%
    filter(e > 0) %>%
    pull(hat)


  crisk_est1
  crisk_est2
  crisk_est3

  all.equal(crisk_est1, crisk_est2)
  all.equal(crisk_est2, crisk_est3)
  all.equal(crisk_est1, crisk_est3)

  1 - km$surv
  expect_equal(crisk_est, 1 - km$surv, tolerance = 1e-15)
})

test_that("Another survival example", {

  m <- survfit(Surv(stop, !(event == "death")) ~1,  data=mgus1, subset=(start==0))
  dt <- mgus1 %>%
    filter(start == 0) %>%
    arrange(stop) %>%
    mutate(
      Y    = if_else(event == "death", stop, Inf),
      C    = if_else(event != "death", stop, Inf),
      PrDel = summary(m, times = stop)$surv
    )


  crisk <- make_cumrisk_estimator(purrr::transpose(dt))


  dt1 <- dt %>%
    mutate(hat = crisk()[-1]) %>%
    filter(event == "death") %>%
    group_by(Y) %>%
    summarize(hat = max(hat))
  crisk_est <- pull(dt1, hat)


  km <- survfit(Surv(stop, event == "death") ~1,
                data = mgus1, subset=(start==0))

  km <- summary(km, times = dt1$Y)
  expect_equal(crisk_est, 1 - km$surv)


})

