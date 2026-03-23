#' confoundvis: Geometric Visualization Tools for Causal Sensitivity Analysis
#'
#' @description
#' Provides visualization tools for sensitivity analysis under unmeasured
#' confounding, including sensitivity contour plots, robustness curves,
#' sensitivity Love plots, and local Taylor diagnostic plots.
#'
#' @name confoundvis-package
#' @aliases confoundvis
#' @docType package
#' @keywords internal
#'
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_ribbon geom_vline
#'   geom_path geom_text coord_equal facet_wrap labs
#' @importFrom stats lm coef
#' @importFrom graphics par
"_PACKAGE"
