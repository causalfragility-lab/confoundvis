 # confoundvis

<!-- badges: start -->
[![R-CMD-check](https://github.com/causalfragility-lab/confoundvis/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/causalfragility-lab/confoundvis/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

**confoundvis** provides visualization tools for sensitivity analysis to
unmeasured confounding in observational studies. It helps researchers assess
how strong omitted confounding would need to be to attenuate, invalidate, or
reverse estimated causal effects — and communicate those findings through
clear, publication-ready graphics.

The package supports three major sensitivity frameworks:

- **Impact threshold** (Frank, 2000)
- **Partial R-squared / robustness value** (Cinelli & Hazlett, 2020)
- **E-value style metrics** (VanderWeele & Ding, 2017)

---

## Installation

Install the development version from GitHub:
```r
# install.packages("pak")
pak::pak("causalfragility-lab/confoundvis")
```

---

## Core Functions

| Function | Purpose |
|---|---|
| `new_confoundsens()` | Construct a sensitivity analysis object |
| `plot_sensitivity_contour()` | Contour plot of robustness values |
| `plot_robustness_curve()` | Robustness curve across effect sizes |
| `plot_sensitivity_love()` | Love-plot style sensitivity display |
| `plot_local_taylor()` | Local Taylor approximation around estimates |
| `fit_local_quadratic()` | Fit local quadratic to sensitivity path |
| `simulate_taylor_demo()` | Simulate data for Taylor approximation demos |

---

## Example: Sensitivity Contour Plot
```r
library(confoundvis)

obj <- new_confoundsens(
  estimate        = 0.5,
  se              = 0.1,
  df              = 200,
  r2dz_x          = seq(0.01, 0.4, by = 0.01),
  r2yz_dx         = seq(0.01, 0.4, by = 0.01),
  benchmark_r2dz  = 0.1,
  benchmark_r2yz  = 0.15
)

plot_sensitivity_contour(obj)
```

---

## Example: Taylor Approximation Bias Across Window Widths

The simulation below illustrates how local quadratic approximation
consistently outperforms simple tangent (first-order) approximation as the
local window widens — especially when the true sensitivity path has curvature.
```r
library(confoundvis)
library(ggplot2)
library(scales)

set.seed(2025)

theta0  <- 0.5
slope   <- -0.7
kappa3  <- 0.6
windows <- c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5)
kappa_vals <- c(-0.4, 0, 0.4)
delta   <- seq(0, 0.5, length.out = 1000)

results <- list()
for (kap in kappa_vals) {
  theta_true <- theta0 + slope * delta + kap * delta^2 + kappa3 * delta^3
  simdat <- data.frame(delta = delta, theta = theta_true)
  for (w in windows) {
    local_dat <- subset(simdat, delta <= w)
    pred_t <- theta0 + slope * local_dat$delta
    mae_t  <- mean(abs(local_dat$theta - pred_t))
    fit_q  <- lm(theta ~ delta + I(delta^2), data = local_dat)
    mae_q  <- mean(abs(local_dat$theta - predict(fit_q, newdata = local_dat)))
    results[[length(results) + 1]] <- data.frame(
      kappa = kap, window = w, mae_tangent = mae_t, mae_quadratic = mae_q
    )
  }
}

df <- do.call(rbind, results)
df_long <- rbind(
  data.frame(kappa = df$kappa, window = df$window,
             type = "Tangent",   mae = df$mae_tangent),
  data.frame(kappa = df$kappa, window = df$window,
             type = "Quadratic", mae = df$mae_quadratic)
)
df_long$kappa_lab <- factor(
  df_long$kappa,
  levels = c(-0.4, 0, 0.4),
  labels = c("Concave (K < 0)", "Linear (K = 0)", "Convex (K > 0)")
)

ggplot(df_long, aes(x = window, y = mae, color = type, linetype = type)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.8) +
  scale_y_log10(labels = label_scientific()) +
  facet_wrap(~ kappa_lab, nrow = 1) +
  labs(
    title    = "Taylor Approximation MAE vs Local Window Width",
    subtitle = "True path includes cubic term (kappa3 = 0.6)",
    x        = "Local window width (w)",
    y        = "Mean Absolute Error (log scale)",
    color    = "Approximation",
    linetype = "Approximation"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position  = "bottom",
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey92", colour = NA),
    plot.title       = element_text(face = "bold")
  )
```

**Key finding:** Quadratic approximation yields substantially lower error than
the tangent at every window width. Approximation error grows with window width,
and curvature accelerates the deterioration of first-order linear approximation.

---

## Example: Zero-Crossing Error Under Linearity Mis-specification

When the true sensitivity path is nonlinear, using a linear approximation to
locate the zero-crossing (the point at which confounding would nullify the
effect) introduces systematic bias.
```r
theta0_vals <- c(0.2, 0.4, 0.6)
slope_vals  <- c(-0.3, -0.6, -0.9)
kappa_vals2 <- seq(-0.6, 0.6, by = 0.2)

find_true <- function(t0, s, k) {
  if (abs(k) < 1e-10) return(-t0 / s)
  disc  <- s^2 - 4 * k * t0
  if (disc < 0) return(NA_real_)
  roots <- c((-s + sqrt(disc)) / (2 * k), (-s - sqrt(disc)) / (2 * k))
  roots <- roots[roots > 0]
  if (length(roots) == 0) return(NA_real_)
  min(roots)
}

res2 <- list()
for (t0 in theta0_vals) {
  for (s in slope_vals) {
    for (k in kappa_vals2) {
      l_lin  <- -t0 / s
      l_true <- find_true(t0, s, k)
      if (is.na(l_true)) next
      res2[[length(res2) + 1]] <- data.frame(
        theta0 = t0, slope = s, kappa = k,
        lambda_linear = l_lin, lambda_true = l_true,
        error = l_lin - l_true
      )
    }
  }
}

df2 <- do.call(rbind, res2)
df2$theta0_lab <- factor(
  df2$theta0,
  levels = c(0.2, 0.4, 0.6),
  labels = c("theta0 = 0.2", "theta0 = 0.4", "theta0 = 0.6")
)

ggplot(df2, aes(x = kappa, y = error,
                color = factor(slope), group = factor(slope))) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.8) +
  facet_wrap(~ theta0_lab, nrow = 1) +
  labs(
    title    = "Signed Error in lambda* Under Linearity Mis-specification",
    subtitle = "Positive = over-estimation by linear approximation",
    x        = "True curvature (kappa)",
    y        = "Estimated lambda* (linear) - true lambda*",
    color    = "Slope"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position  = "bottom",
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey92", colour = NA),
    plot.title       = element_text(face = "bold")
  )
```

**Key finding:** Under concavity, linear approximation over-estimates the
fragility threshold; under convexity, it under-estimates. The bias grows with
both curvature magnitude and initial effect size.

---

## References

- Frank, K. A. (2000). Impact of a confounding variable on a regression
  coefficient. *Sociological Methods & Research*, 29(2), 147–194.
  https://doi.org/10.1177/0049124100029002001

- Cinelli, C., & Hazlett, C. (2020). Making sense of sensitivity: Extending
  omitted variable bias. *Journal of the Royal Statistical Society: Series B*,
  82(1), 39–67. https://doi.org/10.1111/rssb.12348

- VanderWeele, T. J., & Ding, P. (2017). Sensitivity analysis in observational
  research: Introducing the E-value. *Annals of Internal Medicine*, 167(4),
  268–274. https://doi.org/10.7326/M16-2607

---

## Citation
```r
citation("confoundvis")
```

---

## License

GPL-3. See [LICENSE](LICENSE) for details.
