#' Collapse a character vector into a single character string
#'
#' @param ... Arguments passed to `paste0` to be pasted.
#' @noRd
collapse <- function(...) {
  paste0(..., collapse = "")
}

#' Create a comma separated character string
#'
#' @param ... Arguments passed to `paste0` to be pasted.
#' @noRd
comma_sep <- function(...) {
  paste0(..., collapse = ",")
}

#' Replicate a character as a string
#'
#' @param x A `character` string to replicate.
#' @param n An `integer`, the number of repetitions.
#' @return `x` pasted `n` times.
#' @noRd
rep_char <- function(x, n) {
  paste0(rep(x, n), collapse = "")
}

#' Shorthand for `if (test) yes else no`
#'
#' @param test A `logical` value, the condition to evaluate.
#' @param yes An \R object to return when `test` evaluates to `TRUE`.
#' @param no An \R object to return when `test` evaluates to `FALSE`.
#' @noRd
ifelse_ <- function(test, yes, no) {
  if (test) {
    yes
  } else {
    no
  }
}

seq_int <- seq.int

#' Ascending integer sequence with a step of one.
#'
#' @param from The first value of the sequence.
#' @param to The final value of the sequence.
#' @return An integer sequence `from:to` if `from > 0` and `integer(0L)`
#' otherwise.
#' @noRd
seq_asc <- function(from, to) {
  if (from > to) {
    integer(0L)
  } else {
    seq_int(from, to)
  }
}

#' Set the names of an object
#'
#' @param object An \R object.
#' @param nm A `character` vector of names to assign.
#' @return Modified `object` with names given by `nm`.
#' @noRd
set_names <- function(object, nm) {
  names(object) <- nm
  object
}

#' Try to convert an argument to a specified type
#'
#' @param ... Arguments to convert to `type`.
#' @param type A `character` string naming the type to be converted into.
#' @return `...` converted to `type` if possible.
#' @noRd
try_type <- function(..., type) {
  if (missing(type)) {
    stop_("Argument `type` must be given.")
  }
  dots <- list(...)
  arg_name <- names(dots)[1L]
  out <- try(do.call(paste0("as.", type), list(dots[[1L]])), silent = TRUE)
  if (inherits(out, "try-error")) {
    stop_("Unable to coerce argument `", arg_name, "` to `", type, "`.")
  }
  names(out) <- names(dots[[1L]])
  out
}

#' Stop function execution without displaying the call
#'
#' @param ... Arguments to [base::stop()].
#' @noRd
stop_ <- function(...) {
  stop(..., call. = FALSE)
}

#' Conditionally Stop function execution without displaying the call
#'
#' @param cond A `logical` value. If `TRUE` function execution continues.
#' @param message A `character` string of a message to display if `cond` is
#' `FALSE`.
#' @noRd
stopifnot_ <- function(cond, message) {
  if (!cond) {
    stop_(message)
  }
}
