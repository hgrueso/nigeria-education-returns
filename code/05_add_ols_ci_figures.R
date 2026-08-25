#!/usr/bin/env Rscript
library(ggplot2)
library(dplyr)
library(tibble)

load("outputs/analysis_results.RData")

find_term <- function(coef_names, must_contain, must_not_contain = NULL) {
  hits <- coef_names
  for (pat in must_contain) hits <- hits[grepl(pat, hits, fixed = TRUE)]
  if (!is.null(must_not_contain)) {
    for (pat in must_not_contain) hits <- hits[!grepl(pat, hits, fixed = TRUE)]
  }
  if (length(hits) != 1) {
    stop("Expected exactly 1 match for pattern (contains: ", paste(must_contain, collapse=", "),
         "; excludes: ", paste(must_not_contain, collapse=", "), ") but found ", length(hits),
         ": [", paste(hits, collapse=", "), "]. All coef names: [",
         paste(coef_names, collapse=", "), "]")
  }
  hits
}

build_ols_ci_curves <- function(models_quad, models_lin, df_work) {
  m_quad <- models_quad[["+ Interaction"]]
  m_lin  <- models_lin[["+ Interaction"]]
  V_quad <- vcov(m_quad); b_quad <- coef(m_quad)
  V_lin  <- vcov(m_lin);  b_lin  <- coef(m_lin)

  cn_quad <- names(b_quad)
  cn_lin  <- names(b_lin)

  years_q   <- find_term(cn_quad, c("years_c"), c("years_c2", ":"))
  years2_q  <- find_term(cn_quad, c("years_c2"), c(":"))
  inter_q   <- find_term(cn_quad, c(":", "child_marriage", "years_c"), c("years_c2"))

  years_l   <- find_term(cn_lin, c("years_c"), c(":"))
  inter_l   <- find_term(cn_lin, c(":", "child_marriage", "years_c"))

  message(">>> Quadratic terms used: years=", years_q, " years2=", years2_q, " interaction=", inter_q)
  message(">>> Linear terms used: years=", years_l, " interaction=", inter_l)

  mean_years <- mean(df_work$years_of_schooling, na.rm = TRUE)
  S_grid <- seq(
    quantile(df_work$years_of_schooling, 0.02, na.rm = TRUE) - mean_years,
    quantile(df_work$years_of_schooling, 0.98, na.rm = TRUE) - mean_years,
    length.out = 60
  )

  quad_curve <- function(g) {
    est <- b_quad[years_q] + 2 * b_quad[years2_q] * S_grid + b_quad[inter_q] * g
    se  <- sapply(S_grid, function(S) {
      v <- c(1, 2 * S, g)
      idx <- c(years_q, years2_q, inter_q)
      sqrt(as.numeric(t(v) %*% V_quad[idx, idx] %*% v))
    })
    tibble(
      years_of_schooling = S_grid + mean_years,
      estimate = as.numeric(est), se = se,
      ci_lower = as.numeric(est) - 1.96 * se, ci_upper = as.numeric(est) + 1.96 * se,
      group = ifelse(g == 1, "Married before 18", "Not married before 18"),
      spec = "Quadratic OLS"
    )
  }

  lin_curve <- function(g) {
    est <- as.numeric(b_lin[years_l] + b_lin[inter_l] * g)
    idx <- c(years_l, inter_l)
    v <- c(1, g)
    se <- sqrt(as.numeric(t(v) %*% V_lin[idx, idx] %*% v))
    tibble(
      years_of_schooling = S_grid + mean_years,
      estimate = est, se = se,
      ci_lower = est - 1.96 * se, ci_upper = est + 1.96 * se,
      group = ifelse(g == 1, "Married before 18", "Not married before 18"),
      spec = "Linear OLS"
    )
  }

  bind_rows(quad_curve(0), quad_curve(1), lin_curve(0), lin_curve(1))
}

make_panel_figure <- function(region_key, out_prefix) {
  res <- if (region_key == "national") results_national else results_north
  ols_df <- build_ols_ci_curves(res$models_quad, res$models_lin, res$df_work)

  gf_df <- NULL
  if (!is.null(res$plot_cf) && "tau_hat" %in% names(res$plot_cf)) {
    gf_df <- res$plot_cf %>%
      transmute(
        years_of_schooling,
        estimate = tau_hat, ci_lower, ci_upper,
        group = ifelse(child_marriage == 1, "Married before 18", "Not married before 18"),
        spec = "Generalized Random Forest"
      )
  }

  p <- ggplot(ols_df, aes(x = years_of_schooling, y = estimate,
                          color = group, fill = group)) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.15, color = NA) +
    geom_line(linewidth = 1) +
    { if (!is.null(gf_df))
        geom_smooth(data = gf_df, aes(ymin = NULL, ymax = NULL),
                     method = "loess", se = TRUE, linetype = "dashed", linewidth = 0.8, alpha = 0.10)
      else NULL } +
    facet_wrap(~ spec, ncol = 1, scales = "free_y") +
    scale_color_manual(values = c("Not married before 18" = "#2C3E50",
                                   "Married before 18" = "#C0392B")) +
    scale_fill_manual(values = c("Not married before 18" = "#2C3E50",
                                  "Married before 18" = "#C0392B")) +
    labs(x = "Years of Schooling", y = "Marginal Return to Schooling",
         color = NULL, fill = NULL) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom", strip.text = element_text(face = "bold"))

  ggsave(paste0("outputs/", out_prefix, "_ols_ci.pdf"), p, width = 8, height = 9)
  message("Saved outputs/", out_prefix, "_ols_ci.pdf")
  p
}

p_national <- make_panel_figure("national", "fig03_national")
p_north    <- make_panel_figure("north", "fig04_north")

results_national$p_panel_ci <- p_national
results_north$p_panel_ci    <- p_north
save(results_national, results_north, file = "outputs/analysis_results.RData")
message("Updated outputs/analysis_results.RData with OLS CI panel figures")
