#' Simulate demo confounding paths for Taylor panel figures
#'
#' Generates three synthetic sensitivity paths — linear, concave-down, and
#' convex-up — sharing the same baseline effect \eqn{\theta(0)} and first-order
#' slope, but differing in second-order curvature. These toy paths illustrate
#' the difference between tangent-based (first-order) and local quadratic
#' (second-order) sensitivity approximations.
#'
#' The three regimes are:
#' \itemize{
#'   \item \strong{Linear}: \eqn{\theta(\delta) = \theta_0 + s\delta}
#'   \item \strong{Concave-down}: \eqn{\theta(\delta) = \theta_0 + s\delta - \kappa\delta^2}
#'   \item \strong{Convex-up}: \eqn{\theta(\delta) = \theta_0 + s\delta + \kappa\delta^2}
#' }
#'
#' @param delta_max Single positive numeric scalar. Upper bound of the
#'   \eqn{\delta} grid.
#' @param step Single positive numeric scalar. Step size for the grid.
#' @param theta0 Single numeric scalar. Baseline effect at \eqn{\delta = 0}.
#' @param slope Single numeric scalar. First-order slope at \eqn{\delta = 0}.
#' @param kappa Single non-negative numeric scalar. Curvature magnitude.
#'
#' @return A named list with elements `linear`, `concave`, and `convex`, each
#'   a [new_confoundsens()] object.
#' @export
#'
#' @examples
#' sims <- simulate_taylor_demo(
#'   delta_max = 1, step = 0.05, theta0 = 0.4, slope = -0.7, kappa = 0.4
#' )
#' sims$linear
#' plot_robustness_curve(sims$concave)
simulate_taylor_demo <- function(delta_max = 1.5,
                                 step      = 0.02,
                                 theta0    = 0.40,
                                 slope     = -0.70,
                                 kappa     = 0.40) {

  is_scalar <- function(z) is.numeric(z) && length(z) == 1L && !is.na(z)

  if (!is_scalar(delta_max) || delta_max <= 0)
    stop("`delta_max` must be a single positive numeric value.", call. = FALSE)
  if (!is_scalar(step) || step <= 0)
    stop("`step` must be a single positive numeric value.", call. = FALSE)
  if (!is_scalar(theta0))
    stop("`theta0` must be a single numeric value.", call. = FALSE)
  if (!is_scalar(slope))
    stop("`slope` must be a single numeric value.", call. = FALSE)
  if (!is_scalar(kappa) || kappa < 0)
    stop("`kappa` must be a single non-negative numeric value.", call. = FALSE)

  n     <- floor(delta_max / step) + 1L
  delta <- seq.int(from = 0L, by = step, length.out = n)
  delta[length(delta)] <- delta_max   # guarantee exact endpoint

  lvl <- rep("pooled", length(delta))

  list(
    linear  = new_confoundsens(
      lambda = delta,
      theta  = theta0 + slope * delta,
      level  = lvl
    ),
    concave = new_confoundsens(
      lambda = delta,
      theta  = theta0 + slope * delta - kappa * delta^2,
      level  = lvl
    ),
    convex  = new_confoundsens(
      lambda = delta,
      theta  = theta0 + slope * delta + kappa * delta^2,
      level  = lvl
    )
  )
}
