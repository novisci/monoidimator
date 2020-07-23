## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(monoidalEstimator)

## -----------------------------------------------------------------------------
a1 <- function(vs){
  Ti <- vs$V1
  function(t){
    (Ti < t) * 1
  }
}

a2 <- function(vs){
  del <- vs$V2
  function(t){
    (del < t) * 1
  }
}

b <- function(vs){
  one
}

## -----------------------------------------------------------------------------
as <- list(a1, a2)
cumrisk <- make_estimator(as, b)

## -----------------------------------------------------------------------------
dt <- data.frame(
  V1 = rexp(1000),
  V2 = rexp(1000)
)
dtl <- purrr::transpose(as.list(dt))

