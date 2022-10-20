collapse <- function(...) {
  paste0(..., collapse = "")
}

comma_sep <- function(...) {
  paste0(..., collapse = ",")
}

rep_char <- function(x, n) {
  paste0(rep(x, n), collapse = "")
}

ifelse_ <- function(test, yes, no) {
  if (test) {
    yes
  } else {
    no
  }
}

seq_int <- seq.int

seq_asc <- function(from, to) {
  if (from > to) {
    integer(0L)
  } else {
    seq_int(from, to)
  }
}

#' Set the names of an object
#'
#' @param object an R object
#' @param nm A `character` vector of names to assign
#' @noRd
set_names <- function(object, nm) {
  names(object) <- nm
  object
}

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

val_consistent <- function(x, y) {
  if (is.null(x) || is.null(y) || identical(x, y)) {
    TRUE
  } else {
    FALSE
  }
}

stop_ <- function(...) {
  stop(..., call. = FALSE)
}

stopifnot_ <- function(cond, message) {
  if (!cond) {
    stop_(message)
  }
}
