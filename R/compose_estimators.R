#' Creates an individual term in an estimator
#'
#' @param fs a `list` of `function`s of the form `function(vs) { function(...) {}}`,
#'    where `vs` is a `list` of data elements the may be used in the inner function.
#' @importFrom purrr map
#' @return a partial function of a single argument `vs`
#' @export
g <- function(fs){
  function(vs){
    fvs <- purrr::map(fs, ~ .x(vs))
    function(...){
      collect_prod(fs = fvs, ...)
    }
  }
}

#' Creates a linear estimator
#'
#' @param gFUN a function returned by [`g`](`g`)
#' @importFrom purrr map
#' @return a partial function of a single argument `vsl`: a `list` of `list`s
#' @export
h <- function(gFUN){
  function(vsl){
    gvsl <- purrr::map(vsl, ~ gFUN(vs = .x))
    function(...){
      collect_sum(fs = gvsl, ...)/
        collect_sum(replicate(length(vsl), one))
    }
  }
}

#' Creates an estimator
#'
#' @inheritParams g
#' @inheritParams h
#' @export
make_estimator <- function(fs){
  h(g(fs))
}

