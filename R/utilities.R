collapse <- function(...) {
  paste0(..., collapse = "")
}

comma_sep <- function(...) {
  paste0(..., collapse = ",")
}

rep_char <- function(x, n) {
  paste0(rep(x, n), collapse = "")
}

reg_match <- function(x, pattern, perl = TRUE) {
  m <- gregexec(pattern, x, perl)
  regmatches(x, m)[[1L]]
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

check_conflicts <- function(x, y) {
  if (missing(y)) {
    conf <- trivial_conflicts(x)
  } else {
    conf <- trivial_conflict(x, y)
  }
  if (length(conf) > 0L) {
    conf_form <- comma_sep(vapply(conf, format, character(1L)))
    stop_("Inconsistent definitions given for variables: ", conf_form)
  }
}
