#' Fit a local quadratic approximation
#'
#' Fits a second-order Taylor (quadratic) approximation of an effect path
#' \eqn{\theta(\delta)} near \eqn{\delta = 0} using ordinary least squares:
#' \deqn{\theta(\delta) \approx a + b\delta + c\delta^2.}
#'
#' You may supply either a data.frame containing columns `delta` and `theta`,
#' or supply numeric vectors `delta` and `theta` directly.
#'
#' @param data Optional data.frame containing columns named `delta` and
#'   `theta`. If supplied, the `delta` and `theta` arguments are ignored.
#' @param delta Optional numeric vector of delta values. Used only when
#'   `data = NULL`.
#' @param theta Optional numeric vector of theta values. Used only when
#'   `data = NULL`.
#' @param local_max_delta Positive numeric scalar giving the half-width of the
#'   local window: only observations with \eqn{|\delta| \le} `local_max_delta`
#'   are used in the fit.
#' @param include_intercept Logical; if `TRUE` (default), include an intercept
#'   term.
#' @param tol Non-negative numeric scalar tolerance used when selecting
#'   observations within the local window.
#'
#' @return A named list with elements:
#'   * `coef` — named coefficient vector from [stats::lm()].
#'   * `intercept`, `slope`, `quad` — individual coefficients (NA when absent).
#'   * `model` — the fitted `lm` object.
#'   * `local_data` — the data.frame used for fitting.
#'   * `local_max_delta` — the window half-width used.
#' @export
#'
#' @examples
#' df <- data.frame(
#'   delta = seq(0, 0.3, length.out = 60),
#'   theta = 0.4 - 0.7 * seq(0, 0.3, length.out = 60) +
#'           0.4 * seq(0, 0.3, length.out = 60)^2
#' )
#' fit_local_quadratic(df, local_max_delta = 0.2)
fit_local_quadratic <- function(data            = NULL,
                                delta           = NULL,
                                theta           = NULL,
                                local_max_delta = 0.2,
                                include_intercept = TRUE,
                                tol             = 1e-12) {

  # ---- parse inputs ----
  if (!is.null(data)) {
    if (!is.data.frame(data)) {
      stop("`data` must be a data.frame when provided.", call. = FALSE)
    }
    if (!all(c("delta", "theta") %in% names(data))) {
      stop("`data` must contain columns named `delta` and `theta`.", call. = FALSE)
    }
    delta <- data[["delta"]]
    theta <- data[["theta"]]
  }

  if (is.null(delta) || is.null(theta)) {
    stop("Provide either `data` or both `delta` and `theta`.", call. = FALSE)
  }
  if (!is.numeric(delta) || !is.numeric(theta)) {
    stop("`delta` and `theta` must be numeric vectors.", call. = FALSE)
  }
  if (length(delta) != length(theta)) {
    stop("`delta` and `theta` must have the same length.", call. = FALSE)
  }
  if (!is.numeric(local_max_delta) || length(local_max_delta) != 1L ||
      is.na(local_max_delta) || local_max_delta <= 0) {
    stop("`local_max_delta` must be a single positive numeric value.", call. = FALSE)
  }
  if (!is.numeric(tol) || length(tol) != 1L || is.na(tol) || tol < 0) {
    stop("`tol` must be a single non-negative numeric value.", call. = FALSE)
  }
  if (!is.logical(include_intercept) || length(include_intercept) != 1L ||
      is.na(include_intercept)) {
    stop("`include_intercept` must be TRUE or FALSE.", call. = FALSE)
  }

  # ---- drop non-finite observations ----
  ok    <- is.finite(delta) & is.finite(theta)
  delta <- delta[ok]
  theta <- theta[ok]
  if (length(delta) < 3L) {
    stop("Need at least 3 finite observations to fit a quadratic.", call. = FALSE)
  }

  # ---- local window ----
  if (min(abs(delta)) > local_max_delta + tol) {
    stop("No observations within `local_max_delta` of 0.", call. = FALSE)
  }
  idx   <- abs(delta) <= local_max_delta + tol
  dloc  <- delta[idx]
  yloc  <- theta[idx]
  if (length(dloc) < 3L) {
    stop("Need at least 3 points in the local window to fit a quadratic.",
         call. = FALSE)
  }

  # ---- fit ----
  df_loc <- data.frame(delta = dloc, theta = yloc)
  fml    <- if (isTRUE(include_intercept)) {
    stats::as.formula("theta ~ delta + I(delta^2)")
  } else {
    stats::as.formula("theta ~ 0 + delta + I(delta^2)")
  }
  mod <- stats::lm(fml, data = df_loc)
  co  <- stats::coef(mod)

  list(
    coef            = co,
    intercept       = unname(if ("(Intercept)" %in% names(co)) co["(Intercept)"] else NA_real_),
    slope           = unname(if ("delta"       %in% names(co)) co["delta"]       else NA_real_),
    quad            = unname(if ("I(delta^2)"  %in% names(co)) co["I(delta^2)"]  else NA_real_),
    model           = mod,
    local_data      = df_loc,
    local_max_delta = local_max_delta
  )
}
