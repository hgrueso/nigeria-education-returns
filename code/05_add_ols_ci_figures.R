#!/usr/bin/env Rscript
library(ggplot2)
library(dplyr)
library(tibble)

load("outputs/analysis_results.RData")

find_flex <- function(coef_names, candidates, label = "") {
  for (cand in candidates) {
    hits <- coef_names
    for (pat in cand$must) hits <- hits[grepl(pat, hits, fixed = TRUE)]
    if (!is.null(cand$mustnot)) for (pat in cand$mustnot) hits <- hits[!grepl(pat, hits, fixed = TRUE)]
    if (length(hits) == 1) return(hits)
  }
  stop("[", label, "] No candidate matched. All coef names: [", paste(coef_names, collapse=", "), "]")
}

build_ols_curves <- function(models_quad, models_lin, df_work, raw_grid) {
  m_quad <- models_quad[["+ Interaction"]]; m_lin <- models_lin[["+ Interaction"]]
  V_quad <- vcov(m_quad); b_quad <- coef(m_quad)
  V_lin  <- vcov(m_lin);  b_lin  <- coef(m_lin)
  cn_quad <- names(b_quad); cn_lin <- names(b_lin)

  years_q  <- find_flex(cn_quad, list(list(must=c("years_c"), mustnot=c("years_c2",":"))), "quad:years")
  years2_q <- find_flex(cn_quad, list(list(must=c("years_c2"), mustnot=c(":"))), "quad:years2")
  inter_q  <- find_flex(cn_quad, list(list(must=c(":","child_marriage","years_c"), mustnot=c("years_c2"))), "quad:inter")
  years_l  <- find_flex(cn_lin, list(
    list(must=c("years_c"), mustnot=c(":","2")),
    list(must=c("years_of_schooling"), mustnot=c(":"))
  ), "lin:years")
  inter_l  <- find_flex(cn_lin, list(
    list(must=c(":","child_marriage","years_c"), mustnot=character(0)),
    list(must=c(":","child_marriage","years_of_schooling"), mustnot=character(0))
  ), "lin:inter")

  quad_is_centered <- grepl("^years_c$", years_q)
  mean_years <- mean(df_work$years_of_schooling, na.rm = TRUE)
  S_grid_quad <- if (quad_is_centered) raw_grid - mean_years else raw_grid

  quad_curve <- function(g) {
    est <- b_quad[years_q] + 2 * b_quad[years2_q] * S_grid_quad + b_quad[inter_q] * g
    se <- sapply(S_grid_quad, function(S) {
      v <- c(1, 2*S, g); idx <- c(years_q, years2_q, inter_q)
      sqrt(as.numeric(t(v) %*% V_quad[idx, idx] %*% v))
    })
    tibble(years_of_schooling = raw_grid, estimate = as.numeric(est), se = se,
           ci_lower = as.numeric(est)-1.96*se, ci_upper = as.numeric(est)+1.96*se,
           group = ifelse(g==1,"Married before 18","Not married before 18"), spec = "Quadratic OLS")
  }
  lin_curve <- function(g) {
    est <- as.numeric(b_lin[years_l] + b_lin[inter_l]*g)
    idx <- c(years_l, inter_l); v <- c(1, g)
    se <- sqrt(as.numeric(t(v) %*% V_lin[idx, idx] %*% v))
    tibble(years_of_schooling = raw_grid, estimate = est, se = se,
           ci_lower = est-1.96*se, ci_upper = est+1.96*se,
           group = ifelse(g==1,"Married before 18","Not married before 18"), spec = "Linear OLS")
  }
  bind_rows(quad_curve(0), quad_curve(1), lin_curve(0), lin_curve(1))
}

make_combined_figure <- function(region_key, out_prefix) {
  res <- if (region_key == "national") results_national else results_north
  raw_grid <- seq(quantile(res$df_work$years_of_schooling, 0.02, na.rm=TRUE),
                   quantile(res$df_work$years_of_schooling, 0.98, na.rm=TRUE), length.out = 60)

  ols_df <- build_ols_curves(res$models_quad, res$models_lin, res$df_work, raw_grid)
  ols_df$spec <- factor(ols_df$spec, levels = c("Quadratic OLS", "Linear OLS"))
  # CRITICAL FIX: give geom_ribbon its own polygon per (group, spec) combination,
  # otherwise ggplot merges bands across specs that share the same color.
  ols_df$grp <- interaction(ols_df$group, ols_df$spec, drop = TRUE)

  p <- ggplot(ols_df, aes(x = years_of_schooling, y = estimate,
                          color = group, fill = group, group = grp)) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.15, color = NA) +
    geom_line(aes(linetype = spec), linewidth = 1) +
    scale_linetype_manual(values = c("Quadratic OLS" = "solid", "Linear OLS" = "dashed")) +
    scale_color_manual(values = c("Not married before 18" = "#2C3E50", "Married before 18" = "#C0392B")) +
    scale_fill_manual(values = c("Not married before 18" = "#2C3E50", "Married before 18" = "#C0392B")) +
    labs(x = "Years of Schooling", y = "Marginal Return to Schooling",
         color = NULL, fill = NULL, linetype = "Specification") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom", legend.box = "vertical")

  ggsave(paste0("outputs/", out_prefix, "_ols_ci.pdf"), p, width = 9, height = 6)
  message("Saved outputs/", out_prefix, "_ols_ci.pdf")
  p
}

p_national <- make_combined_figure("national", "fig03_national")
p_north    <- make_combined_figure("north", "fig04_north")

results_national$p_panel_ci <- p_national
results_north$p_panel_ci    <- p_north
save(results_national, results_north, file = "outputs/analysis_results.RData")
message("Updated outputs/analysis_results.RData")
