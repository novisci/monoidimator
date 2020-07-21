#' Utilites for working with monoidal structures
#'
#' @name monoidal_utilities
#' @param op a binary operator
#' @param lift an optional function that lifts the result of `r()` to a different
#'     domain. Defaults to `identity`.
#' @return
#' `make_monoidal_applicator` returns a function of at least 2 arguments:
#'   * `l`: a value of the same type returned by:
#'   * `r`: a function that returns the same type as `l`
#'   * `...`: optional arguments passed to `r`
#'
#' `make_monoidal_collector` returns a function of at least 1 argument:
#'   * `fs`: a `list` of functions
#'   * `...`: optional arguments passed to `r` in the `applicator`
make_monoidal_applicator <- function(op, lift = identity){
  function(l, r, ...){
    op(l, lift(r(...)))
  }
}

#' @rdname monoidal_utilities
#' @param applicator one of the [monoidal_applicators](monoidal_applicators)
#' @param .init the unit of the monoid (e.g. 0 for `+` and numbers)
#' @importFrom purrr reduce
#' @export
make_monoidal_collector <- function(applicator, .init){
  function(fs, ...){
    purrr::reduce(
      .x = fs,
      .f = function(x, y) applicator(l = x, r = y, ...),
      .init = .init)
  }
}

#' Monoidal applicators
#'
#' @name monoidal_applicators
#' @param l a value of the same type returned by:
#' @param r a function that returns the same type as `l`
#' @param ... additional arguments passed to `r(...)`
NULL

#' @rdname monoidal_applicators
#' @export
apply_prod <- make_monoidal_applicator(`*`)

#' @rdname monoidal_applicators
#' @export
apply_sum  <- make_monoidal_applicator(`+`)

#' Monoidal collectors
#'
#' @name monoidal_collectors
#' @param fs a `list` of `function`s
#' @param ... additional arguments passed to `r(...)` of the `applicator`
NULL

#' @rdname monoidal_collectors
#' @export
collect_prod <- make_monoidal_collector(apply_prod, 1)

#' @rdname monoidal_collectors
#' @export
collect_sum  <- make_monoidal_collector(apply_sum, 0)


one <- function() 1
