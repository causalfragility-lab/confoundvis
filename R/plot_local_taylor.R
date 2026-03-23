#' Local Taylor diagnostic plot
#'
#' Plots local Taylor series components (or any multi-series decomposition)
#' as a function of `delta` from a long-form data.frame. Lines are
#' distinguished by colour and linetype, keyed by `series`.
#'
#' @param df A data.frame in long form with columns:
#'   * `delta` — numeric; the confounding-strength grid.
#'   * `series` — character or factor; name of each curve
#'     (e.g., `"path"`, `"tangent"`, `"quadratic"`).
#'   * `value` — numeric; the effect value for each (delta, series) pair.
#' @param facet Logical; if `TRUE`, produce a faceted plot with one panel per
#'   `series`. If `FALSE` (default), overlay all series on a single panel with
#'   colour and linetype aesthetics.
#'
#' @return A [ggplot2::ggplot()] object.
#' @export
#'
#' @examples
#' df <- data.frame(
#'   delta  = rep(seq(0, 0.2, length.out = 25), 3),
#'   series = rep(c("path", "tangent", "quadratic"), each = 25),
#'   value  = c(
#'     0.4 - 0.7 * seq(0, 0.2, length.out = 25) - 0.4 * seq(0, 0.2, length.out = 25)^2,
#'     0.4 - 0.7 * seq(0, 0.2, length.out = 25),
#'     0.4 - 0.7 * seq(0, 0.2, length.out = 25) + 0.2 * seq(0, 0.2, length.out = 25)^2
#'   )
#' )
#' plot_local_taylor(df)
#' plot_local_taylor(df, facet = TRUE)
plot_local_taylor <- function(df, facet = FALSE) {
  if (!is.data.frame(df)) {
    stop("`df` must be a data.frame.", call. = FALSE)
  }
  if (!all(c("delta", "series", "value") %in% names(df))) {
    stop("`df` must contain columns `delta`, `series`, and `value`.",
         call. = FALSE)
  }
  if (!is.numeric(df$delta) || !is.numeric(df$value)) {
    stop("`delta` and `value` must be numeric.", call. = FALSE)
  }

  d <- df
  d$series <- factor(as.character(d$series), levels = unique(as.character(d$series)))

  p <- ggplot2::ggplot(
      d,
      ggplot2::aes(
        x        = .data$delta,
        y        = .data$value,
        colour   = .data$series,
        linetype = .data$series
      )
    ) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::labs(
      x      = expression(delta),
      y      = expression(theta(delta)),
      colour   = "Series",
      linetype = "Series"
    )

  if (isTRUE(facet)) {
    p <- p +
      ggplot2::facet_wrap(~ .data$series, scales = "free_y") +
      ggplot2::theme(legend.position = "none")
  }

  p
}
