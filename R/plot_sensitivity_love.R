#' Sensitivity Love plot
#'
#' Benchmarks a sensitivity threshold against the empirical distribution of
#' observed covariate impacts in a "Love plot"-style display. Each covariate
#' appears as a point on a horizontal impact axis; the sensitivity threshold
#' is shown as a vertical reference line. Covariates to the left of the line
#' are weaker than the threshold; those to the right pose a credible threat.
#'
#' @param df A data.frame with at least two columns:
#'   * `covariate` — covariate names (character or factor).
#'   * `impact` — numeric impact values (e.g., ITCV product
#'     \eqn{|r_{YU} \cdot r_{DU}|}, partial \eqn{R^2}, or other
#'     confounding-strength metric).
#' @param threshold Single non-missing numeric value to draw as a vertical
#'   reference line (the sensitivity threshold).
#' @param sort Logical; if `TRUE` (default), sort covariates by `impact` on
#'   the y-axis (ascending).
#' @param top Optional positive integer. If supplied, only the `top` covariates
#'   with the largest absolute impact are displayed.
#'
#' @return A [ggplot2::ggplot()] object.
#' @export
#'
#' @examples
#' df <- data.frame(
#'   covariate = c("SES", "BMI", "Gender", "Race", "Age"),
#'   impact    = c(0.12, 0.05, 0.02, 0.08, 0.03)
#' )
#' plot_sensitivity_love(df, threshold = 0.10)
#' plot_sensitivity_love(df, threshold = 0.10, top = 3)
plot_sensitivity_love <- function(df,
                                  threshold,
                                  sort = TRUE,
                                  top  = NULL) {
  if (!is.data.frame(df)) {
    stop("`df` must be a data.frame.", call. = FALSE)
  }
  if (!all(c("covariate", "impact") %in% names(df))) {
    stop("`df` must contain columns `covariate` and `impact`.", call. = FALSE)
  }
  if (!is.numeric(df$impact)) {
    stop("`df$impact` must be numeric.", call. = FALSE)
  }
  if (!is.numeric(threshold) || length(threshold) != 1L || is.na(threshold)) {
    stop("`threshold` must be a single non-missing numeric value.", call. = FALSE)
  }

  d <- df
  d$covariate <- as.character(d$covariate)

  if (!is.null(top)) {
    if (!is.numeric(top) || length(top) != 1L || is.na(top) || top <= 0) {
      stop("`top` must be a single positive number or NULL.", call. = FALSE)
    }
    ord <- order(abs(d$impact), decreasing = TRUE)
    d   <- d[ord, , drop = FALSE]
    d   <- d[seq_len(min(nrow(d), as.integer(top))), , drop = FALSE]
  }

  if (isTRUE(sort)) {
    d$covariate <- factor(d$covariate,
                          levels = d$covariate[order(d$impact)])
  } else {
    d$covariate <- factor(d$covariate, levels = unique(d$covariate))
  }

  ggplot2::ggplot(d, ggplot2::aes(x = .data$impact, y = .data$covariate)) +
    ggplot2::geom_vline(xintercept = threshold, linetype = "dashed",
                        colour = "grey40") +
    ggplot2::geom_point(size = 2.5) +
    ggplot2::labs(
      x = "Confounding impact strength",
      y = NULL,
      caption = paste0("Dashed line: sensitivity threshold = ", threshold)
    )
}
