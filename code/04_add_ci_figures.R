#!/usr/bin/env Rscript
# ============================================================
# 04_add_ci_figures.R
#
# Adds confidence interval bands to the causal forest marginal
# return figures (National and Northern Nigeria), addressing
# the referee comment about missing CIs on Figures 1-4.
#
# Requires: outputs/analysis_results.RData (produced by
# code/03_analysis_NLSS_Final.R). Run that script first.
# ============================================================

library(grf)
library(ggplot2)
library(dplyr)
library(tibble)

load("outputs/analysis_results.RData")

add_cf_ci_figure <- function(cf_model, df_cf, region_label, out_prefix) {

  # Build a grid across observed schooling range
  schooling_range <- seq(
    quantile(df_cf$years_of_schooling, 0.02, na.rm = TRUE),
    quantile(df_cf$years_of_schooling, 0.98, na.rm = TRUE),
    length.out = 60
  )

  covariate_names <- colnames(cf_model$X.orig)

  make_grid <- function(cm_value) {
    grid <- df_cf %>%
      summarise(across(all_of(covariate_names), ~ median(.x, na.rm = TRUE)))
    grid <- grid[rep(1, length(schooling_range)), ]
    if ("years_of_schooling" %in% covariate_names) {
      grid$years_of_schooling <- schooling_range
    }
    if ("child_marriage" %in% covariate_names) {
      grid$child_marriage <- cm_value
    }
    grid
  }

  grid_cm0 <- make_grid(0)
  grid_cm1 <- make_grid(1)

  pred_cm0 <- predict(cf_model, newdata = grid_cm0[, covariate_names],
                       estimate.variance = TRUE)
  pred_cm1 <- predict(cf_model, newdata = grid_cm1[, covariate_names],
                       estimate.variance = TRUE)

  build_df <- function(pred, schooling, label) {
    se <- sqrt(pred$variance.estimates)
    tibble(
      years_of_schooling = schooling,
      estimate = pred$predictions,
      ci_lower = pred$predictions - 1.96 * se,
      ci_upper = pred$predictions + 1.96 * se,
      group = label
    )
  }

  plot_df <- bind_rows(
    build_df(pred_cm0, schooling_range, "Not married before 18"),
    build_df(pred_cm1, schooling_range, "Married before 18")
  )

  p <- ggplot(plot_df, aes(x = years_of_schooling, y = estimate,
                            color = group, fill = group)) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.15, color = NA) +
    geom_line(linewidth = 1) +
    scale_color_manual(values = c("Not married before 18" = "#2C3E50",
                                   "Married before 18" = "#C0392B")) +
    scale_fill_manual(values = c("Not married before 18" = "#2C3E50",
                                  "Married before 18" = "#C0392B")) +
    labs(x = "Years of Schooling", y = "Marginal Return to Schooling",
         color = NULL, fill = NULL) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")

  ggsave(paste0("outputs/", out_prefix, "_ci.pdf"), p, width = 8, height = 5.5)
  message("Saved outputs/", out_prefix, "_ci.pdf")

  list(plot = p, data = plot_df)
}

# ============================================================
# IMPORTANT: adjust the object names below (cf_model, plot_cf)
# to match whatever your script actually calls the fitted grf
# model and the plotting data frame. Run:
#   names(results_national)
# to check.
# ============================================================

cf_ci_national <- add_cf_ci_figure(
  cf_model  = results_national$cf_model,
  df_cf     = results_national$plot_cf,
  region_label = "National",
  out_prefix   = "fig01_national"
)

cf_ci_north <- add_cf_ci_figure(
  cf_model  = results_north$cf_model,
  df_cf     = results_north$plot_cf,
  region_label = "Northern Nigeria",
  out_prefix   = "fig02_north"
)

results_national$p_cf_main_ci <- cf_ci_national$plot
results_north$p_cf_main_ci    <- cf_ci_north$plot

save(results_national, results_north, file = "outputs/analysis_results.RData")
message("Updated outputs/analysis_results.RData with CI figures")
