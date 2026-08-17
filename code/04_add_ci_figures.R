#!/usr/bin/env Rscript
# 04_add_ci_figures.R
# Adds confidence interval bands to the causal forest marginal
# return figures using out-of-bag CATE predictions + analytic SE.

library(grf)
library(ggplot2)
library(dplyr)

load("outputs/analysis_results.RData")

add_cf_ci_figure <- function(forest, plot_cf, out_prefix) {

  if (is.null(forest)) {
    stop("forest object is NULL -- check that the causal forest step ",
         "completed successfully in 03_analysis_NLSS_Final.R for this region.")
  }

  pred <- predict(forest, estimate.variance = TRUE)
  tau_hat <- pred$predictions
  se_hat  <- sqrt(pred$variance.estimates)

  if (length(tau_hat) != nrow(plot_cf)) {
    stop("Row count mismatch: forest has ", length(tau_hat),
         " predictions but plot_cf has ", nrow(plot_cf), " rows.")
  }

  df <- plot_cf
  df$tau_hat  <- tau_hat
  df$ci_lower <- tau_hat - 1.96 * se_hat
  df$ci_upper <- tau_hat + 1.96 * se_hat
  df$group <- factor(df$child_marriage, levels = c(0, 1),
                      labels = c("Not married before 18", "Married before 18"))

  p <- ggplot(df, aes(x = years_of_schooling, y = tau_hat,
                       color = group, fill = group)) +
    geom_smooth(method = "loess", se = TRUE, linewidth = 1, alpha = 0.15) +
    scale_color_manual(values = c("Not married before 18" = "#2C3E50",
                                   "Married before 18" = "#C0392B")) +
    scale_fill_manual(values = c("Not married before 18" = "#2C3E50",
                                  "Married before 18" = "#C0392B")) +
    labs(x = "Years of Schooling", y = "Estimated Conditional Return to Schooling",
         color = NULL, fill = NULL) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")

  ggsave(paste0("outputs/", out_prefix, "_ci.pdf"), p, width = 8, height = 5.5)
  message("Saved outputs/", out_prefix, "_ci.pdf")
  p
}

message(">>> Class of results_national$forest: ", class(results_national$forest)[1])
message(">>> Class of results_north$forest: ", class(results_north$forest)[1])

p_nat <- add_cf_ci_figure(results_national$forest, results_national$plot_cf, "fig01_national")
p_nth <- add_cf_ci_figure(results_north$forest, results_north$plot_cf, "fig02_north")

results_national$p_cf_main_ci <- p_nat
results_north$p_cf_main_ci    <- p_nth
save(results_national, results_north, file = "outputs/analysis_results.RData")
message("Updated outputs/analysis_results.RData with CI figures")
