#' Coerce a confoundsens object to a data frame
#'
#' @param x A `confoundsens` object.
#' @param ... Unused; included for S3 method consistency.
#'
#' @return A data.frame with columns `lambda` and `theta`, plus optional
#'   columns `level`, `se`, and `t` when present in `x`.
#' @export
#'
#' @examples
#' x <- new_confoundsens(
#'   lambda = seq(0, 0.2, length.out = 5),
#'   theta  = seq(1, 0.8, length.out = 5),
#'   se     = rep(0.05, 5)
#' )
#' as.data.frame(x)
as.data.frame.confoundsens <- function(x, ...) {
  x <- validate_confoundsens(x)
  n <- length(x$lambda)

  df <- data.frame(
    lambda = as.numeric(x$lambda),
    theta  = as.numeric(x$theta),
    stringsAsFactors = FALSE
  )
  if (!is.null(x$level)) df$level <- as.character(x$level)
  if (!is.null(x$se))    df$se    <- as.numeric(x$se)
  if (!is.null(x$t))     df$t     <- as.numeric(x$t)

  stopifnot(nrow(df) == n)
  df
}
