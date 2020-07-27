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
#' @param aFUN a function returned by [`g`](`g`)
#' @param bFUN a function returned by [`g`](`g`)
#' @param post a function applied to the final result of the pair returned by
#'            the  `collector` function. Defaults to [`ratio`](`ratio`).
#' @param collector the function used to collapse the monoidal data.
#' @importFrom purrr map
#' @return a partial function of a single argument `vsl`: a `list` of `list`s
#' @export
h <- function(aFUN, bFUN, post = ratio, collector){
  function(vsl){
    gvsl <- purrr::map(vsl, ~ list(aFUN(vs = .x), bFUN(vs = .x)))
    function(...){
      post(collector(fs = gvsl, ...))
    }
  }
}

#' Creates an estimator
#'
#' @param as a `list` of a functions
#' @param bs a `list` of a functions
#' @inheritParams h
#' @export
make_estimator <- function(as, bs, post = ratio, collector = collect2_sum){
  h(g(as), g(bs), post = post, collector = collector)
}

