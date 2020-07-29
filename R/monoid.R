#' Utilites for working with monoidal structures
#'
#' @name monoidal_utilities
#' @param op a binary operator
#' @param lift an optional function that lifts the result of `r()` to a different
#'     domain. Defaults to `identity`.
#' @param collectors a `list` of collector functions
#' @return
#' `make_monoidal_applicator` returns a function of at least 2 arguments:
#'   * `l`: a value of the same type returned by:
#'   * `r`: a function that returns the same type as `l`
#'   * `...`: optional arguments passed to `r`
#'
#' `make_monoidal_collector` returns a function of at least 1 argument:
#'   * `fs`: a `list` of functions
#'   * `...`: optional arguments passed to `r` in the `applicator`
#'
#' `make_sequential_collector` returns a function of at least 1 argument:
#'   * `fs`: a `list` of functions
#'   * `...`: optional arguments passed to `r` in the `applicator`
#'   This function applies each `collector` to each element of `transpose(fs)`
NULL

#' @rdname monoidal_utilities
#' @export
make_monoidal_applicator <- function(op, lift = identity){
  function(l, r, ...){
    op(l, lift(r(...)))
  }
}

#' @rdname monoidal_utilities
#' @export
make_monoidal_2applicator <- function(op, lift = identity){
  function(l, r, ...){
    list(op(l[[1]], lift(r[[1]](...))),
         op(l[[2]], lift(r[[2]](...))))
  }
}

#' @rdname monoidal_utilities
#' @param applicator one of the [monoidal_applicators](monoidal_applicators)
#' @param .init the unit of the monoid (e.g. 0 for `+` and numbers)
#' @param accumulate indicator of whether to accumulate the results
#' @importFrom purrr reduce
#' @export
make_monoidal_collector <- function(applicator, .init, accumulate = FALSE){
  collector <- `if`(accumulate, purrr::accumulate, purrr::reduce)
  function(fs, ...){
    collector(
      .x = fs,
      .f = function(x, y) applicator(l = x, r = y, ...),
      .init = .init)
  }
}

#' @rdname monoidal_utilities
#' @param applicator one of the [monoidal_applicators](monoidal_applicators)
#' @param .init the unit of the monoid (e.g. 0 for `+` and numbers)
#' @param accumulate indicator of whether to accumulate the results
#' @importFrom purrr reduce map2 transpose
#' @export
make_sequential_collector <- function(collectors){
  function(fs, ...){
    purrr::map2(
      .x = collectors,
      # .y = purrr::pmap(fs, list),
      .y = purrr::transpose(fs),
      .f = ~ .x(fs = .y, ...))
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

#' @rdname monoidal_applicators
#' @export
apply2_sum  <- make_monoidal_2applicator(`+`)

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

#' @rdname monoidal_collectors
#' @export
accum_sum  <- make_monoidal_collector(apply_sum, 0, accumulate = TRUE)

#' @rdname monoidal_collectors
#' @export
collect2_sum  <- make_monoidal_collector(apply2_sum, list(0, 0))

#' @rdname monoidal_collectors
#' @export
accumsum_collectsum_seq <- make_sequential_collector(list(accum_sum, collect_sum))

#' @rdname monoidal_collectors
#' @export
one <- function(...) 1

#' @rdname monoidal_collectors
#' @export
zero <- function(...) 0

#' Monoidal helper functions
#'
#' @name monoidal_helpers
#' @param x a `list`
NULL

#' @rdname monoidal_helpers
#' @export
ratio <- function(x) Reduce(`/`, x)
