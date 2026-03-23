#' Validate a confoundsens object
#'
#' Internal validator used by constructors and methods. Checks that required
#' fields are present and consistently sized, coerces optional fields, and
#' issues a warning when `lambda` is not nondecreasing.
#'
#' @param x A `confoundsens` object.
#'
#' @return The validated (and possibly coerced) object, invisibly.
#'   Errors if the object is structurally invalid.
#' @keywords internal
validate_confoundsens <- function(x) {
  if (!inherits(x, "confoundsens")) {
    stop("`x` must inherit from class 'confoundsens'.", call. = FALSE)
  }

  # Required fields
  if (is.null(x$lambda) || !is.numeric(x$lambda)) {
    stop("`x$lambda` must be a numeric vector.", call. = FALSE)
  }
  if (is.null(x$theta) || !is.numeric(x$theta)) {
    stop("`x$theta` must be a numeric vector.", call. = FALSE)
  }

  n <- length(x$lambda)
  if (n == 0L) {
    stop("`x$lambda` must not be empty.", call. = FALSE)
  }
  if (length(x$theta) != n) {
    stop("`x$theta` must have the same length as `x$lambda`.", call. = FALSE)
  }
  if (anyNA(x$lambda)) {
    stop("`x$lambda` must not contain NA.", call. = FALSE)
  }

  # Optional: level — coerce and store back
  if (!is.null(x$level)) {
    if (length(x$level) != n) {
      stop("`x$level` must have the same length as `x$lambda`.", call. = FALSE)
    }
    x$level <- as.character(x$level)
  }

  # Optional: se
  if (!is.null(x$se)) {
    if (!is.numeric(x$se) || length(x$se) != n) {
      stop("`x$se` must be a numeric vector with the same length as `x$lambda`.",
           call. = FALSE)
    }
  }

  # Optional: t
  if (!is.null(x$t)) {
    if (!is.numeric(x$t) || length(x$t) != n) {
      stop("`x$t` must be a numeric vector with the same length as `x$lambda`.",
           call. = FALSE)
    }
  }

  # Non-fatal warning for non-monotone lambda
  if (n >= 2L && any(diff(x$lambda) < 0)) {
    warning("`x$lambda` is not nondecreasing; plots may look odd.",
            call. = FALSE)
  }

  invisible(x)
}
