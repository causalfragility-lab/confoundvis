#' Sensitivity contour plot
#'
#' Draws an ITCV-style hyperbolic boundary in \eqn{(r_{YU}, r_{DU})} space
#' and optionally overlays observed covariate benchmarks as labelled points.
#' The robust region is the interior of the hyperbola where
#' \eqn{|r_{YU} \cdot r_{DU}| < } `threshold`.
#'
#' @param threshold Positive numeric scalar; the ITCV-style product threshold.
#'   The boundary satisfies \eqn{|r_{YU} \cdot r_{DU}| = } `threshold`.
#' @param grid_n Integer >= 50; number of points used per branch of the
#'   boundary curve. Larger values give smoother curves.
#' @param benchmarks Optional data.frame with columns `r_yu` and `r_du`
#'   (numeric) and an optional `label` column (character). Each row is plotted
#'   as a labelled benchmark point.
#'
#' @return A [ggplot2::ggplot()] object.
#' @export
#'
#' @examples
#' b <- data.frame(
#'   r_yu  = c(0.10, 0.15),
#'   r_du  = c(0.20, 0.12),
#'   label = c("SES", "BMI")
#' )
#' plot_sensitivity_contour(threshold = 0.02, benchmarks = b)
plot_sensitivity_contour <- function(threshold,
                                     grid_n     = 200,
                                     benchmarks = NULL) {
  if (!is.numeric(threshold) || length(threshold) != 1L ||
      is.na(threshold) || threshold <= 0) {
    stop("`threshold` must be a single positive numeric value.", call. = FALSE)
  }
  if (!is.numeric(grid_n) || length(grid_n) != 1L || grid_n < 50) {
    stop("`grid_n` must be a single integer >= 50.", call. = FALSE)
  }

  # Build the four branches of the hyperbola r_du = +/- threshold / r_yu
  # and assign a group so geom_path does not connect them.
  eps <- 1e-6
  r   <- seq(eps, 1, length.out = as.integer(grid_n))

  make_branch <- function(r_yu_vals, r_du_vals, grp) {
    df <- data.frame(r_yu = r_yu_vals, r_du = r_du_vals, group = grp,
                     stringsAsFactors = FALSE)
    df[abs(df$r_du) <= 1 & abs(df$r_yu) <= 1, , drop = FALSE]
  }

  boundary <- rbind(
    make_branch( r,  threshold / r,  "Q1"),
    make_branch(-r, -threshold / r,  "Q2"),
    make_branch( r, -threshold / r,  "Q3"),
    make_branch(-r,  threshold / r,  "Q4")
  )

  p <- ggplot2::ggplot(
      boundary,
      ggplot2::aes(x = .data$r_yu, y = .data$r_du, group = .data$group)
    ) +
    ggplot2::geom_path() +
    ggplot2::coord_equal(xlim = c(-1, 1), ylim = c(-1, 1)) +
    ggplot2::labs(
      x = expression(r[YU]),
      y = expression(r[DU])
    )

  if (!is.null(benchmarks)) {
    if (!is.data.frame(benchmarks) ||
        !all(c("r_yu", "r_du") %in% names(benchmarks))) {
      stop("`benchmarks` must be a data.frame with columns `r_yu` and `r_du`.",
           call. = FALSE)
    }
    if (!is.numeric(benchmarks$r_yu) || !is.numeric(benchmarks$r_du)) {
      stop("`benchmarks$r_yu` and `benchmarks$r_du` must be numeric.",
           call. = FALSE)
    }

    p <- p + ggplot2::geom_point(
      data = benchmarks,
      ggplot2::aes(x = .data$r_yu, y = .data$r_du),
      inherit.aes = FALSE,
      size = 2.5
    )

    if ("label" %in% names(benchmarks)) {
      p <- p + ggplot2::geom_text(
        data = benchmarks,
        ggplot2::aes(x = .data$r_yu, y = .data$r_du, label = .data$label),
        inherit.aes = FALSE,
        nudge_y = 0.04,
        size = 3.5
      )
    }
  }

  p
}
