#' Convert a data frame to a confoundsens object
#'
#' Converts a data frame containing sensitivity analysis results into a
#' `confoundsens` object suitable for use with `confoundvis` plotting functions.
#'
#' @param data A data.frame containing sensitivity analysis results.
#' @param lambda Character string; name of the column containing lambda values.
#'   Default `"lambda"`.
#' @param theta Character string; name of the column containing theta values.
#'   Default `"theta"`.
#' @param level Optional character string; name of the column containing level
#'   identifiers (e.g., `"within"` / `"between"`).
#' @param se Optional character string; name of the column containing standard
#'   errors for `theta`.
#' @param t Optional character string; name of the column containing test
#'   statistics.
#'
#' @return A `confoundsens` object.
#' @export
#'
#' @examples
#' df <- data.frame(
#'   lambda = seq(0, 0.2, length.out = 10),
#'   theta  = seq(1, 0.5, length.out = 10),
#'   se     = rep(0.1, 10),
#'   level  = rep(c("within", "between"), length.out = 10)
#' )
#' x <- as_confoundsens(df)
#' x
as_confoundsens <- function(data,
                            lambda = "lambda",
                            theta  = "theta",
                            level  = NULL,
                            se     = NULL,
                            t      = NULL) {

  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }

  .check_col <- function(nm, arg) {
    if (!is.character(nm) || length(nm) != 1L) {
      stop(sprintf("`%s` must be a single character string.", arg), call. = FALSE)
    }
    if (!nm %in% names(data)) {
      stop(sprintf("Column '%s' not found in `data`.", nm), call. = FALSE)
    }
  }

  .check_col(lambda, "lambda")
  .check_col(theta,  "theta")

  lam <- data[[lambda]]
  th  <- data[[theta]]
  if (!is.numeric(lam)) stop("`lambda` column must be numeric.", call. = FALSE)
  if (!is.numeric(th))  stop("`theta` column must be numeric.",  call. = FALSE)

  lvl <- NULL
  if (!is.null(level)) {
    .check_col(level, "level")
    lvl <- as.character(data[[level]])
  }

  se_ <- NULL
  if (!is.null(se)) {
    .check_col(se, "se")
    se_ <- as.numeric(data[[se]])
  }

  t_ <- NULL
  if (!is.null(t)) {
    .check_col(t, "t")
    t_ <- as.numeric(data[[t]])
  }

  new_confoundsens(lambda = lam, theta = th, level = lvl, se = se_, t = t_)
}
