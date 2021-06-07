collapse <- function(...) {
    paste0(..., collapse = "")
}

comma_sep <- function(...) {
    paste0(..., collapse = ",")
}

rep_char <- function(x, n) {
    paste0(rep(x, n), collapse = "")
}

rm_ws <- function(x) {
    y <- gsub("[ \t\r\n]", "", x, perl = TRUE)
    dim(y) <- dim(x)
    y
}

reg_match <- function(x, pattern, perl = TRUE) {
    m <- gregexpr(pattern, x, perl)
    regmatches(x, m)[[1L]]
}

reg_named <- function(x, pattern) {
    m <- gregexec(pattern, x, perl = TRUE)
    regmatches(x, m)[[1L]]
}

try_type <- function(..., type) {
    if (missing(type)) {
        stop_("Argument 'type' must be given")
    }
    dots <- list(...)
    arg_name <- names(dots)[1L]
    out <- try(do.call(paste0("as.", type), list(dots[[1L]])), silent = TRUE)
    if ("try-error" %in% class(out)) {
        stop_("Unable to coerce argument '", arg_name, "' to '", type, "'")
    }
    names(out) <- names(dots[[1L]])
    out
}

val_consistent <- function(x, y) {
    if (is.null(x)) {
        return(TRUE)
    }
    if (is.null(y)) {
        return(TRUE)
    }
    if (identical(x, y)) {
        return(TRUE)
    }
    return(FALSE)
}

stop_ <- function(...) {
    stop(..., call. = FALSE)
}

warning_ <- function(...) {
    warning(..., call. = FALSE)
}

any_conflict <- function(x) {
    if (length(x)) {
        conf_form <- comma_sep(sapply(x, format))
        stop_("Inconsistent definitions given for variables: ", conf_form)
    }
}

check_conflicts <- function(x, y) {
    if (missing(y)) out <- trivial_conflicts(x)
    else out <- trivial_conflict(x, y)
    any_conflict(out)
}
