# -----------------------------------------------------------------------------
# Script: 03_analysis_NLSS_modelsummary.R
# Purpose: COMPREHENSIVE REVISED ANALYSIS with publication-ready tables
# 
# Key change from 02_*: replaces stargazer with modelsummary for direct
# .docx / .tex / .html output. Saves .RData for Quarto slides.
#
# Outputs:
#   - *_Table_2_Main_Quadratic.docx     (Word, copy-paste ready)
#   - *_Table_2_Main_Quadratic.tex      (LaTeX for paper)
#   - *_Table_2_Main_Quadratic.html     (browser preview)
#   - *_Table_2A_Main_Linear.docx/.tex
#   - *_Table_3_Education_Levels.docx/.tex
#   - *_Table_4_Literacy.docx/.tex
#   - *_Table_Appendix_Probit.docx/.tex
#   - *_Appendix_CF_*.docx/.tex
#   - All figures (.png)
#   - analysis_results.RData            (for Quarto slides)
# -----------------------------------------------------------------------------

library(tidyverse)
library(haven)
library(modelsummary)
library(sampleSelection)
library(ggplot2)
library(lmtest)
library(sandwich)
library(grf)
library(margins)
library(marginaleffects) # More robust AME computation than margins
library(patchwork)
library(flextable)  # For Word output
library(gt)         # For HTML output
library(kableExtra) # For LaTeX output

if (!require(multiwayvcov, quietly = TRUE)) {
  cluster.vcov <- function(model, cluster) { vcovCL(model, cluster = cluster) }
}

# ==============================================================================
# WRAPPER FOR HECKIT → modelsummary
# The 2-step heckit stores outcome coefficients differently from ML.
# We extract directly from the model's coefficient slots.
# ==============================================================================
wrap_heckit_for_ms <- function(heck_model) {
  s <- summary(heck_model)
  coefs <- s$estimate
  rnames <- rownames(coefs)
  
  message("  [debug] Heckman coef names: ", paste(head(rnames, 20), collapse=", "), " ...")
  
  # Strategy: figure out which rows are outcome equation
  # 2-step method: no prefix. Selection eq uses S: prefix (or is separate)
  # ML method: outcome uses O: prefix, selection uses S: prefix
  
  has_O <- any(grepl("^O:", rnames))
  has_S <- any(grepl("^S:", rnames))
  
  if (has_O) {
    # ML method: extract O: rows
    out_idx <- grep("^O:", rnames)
    out_coefs <- coefs[out_idx, , drop = FALSE]
    terms <- sub("^O:", "", rownames(out_coefs))
  } else if (has_S) {
    # 2-step: selection rows have S:, everything else is outcome
    sel_idx <- grep("^S:", rnames)
    special_idx <- grep("^(sigma|rho|invMillsRatio|lambda)", rnames, ignore.case = TRUE)
    out_idx <- setdiff(seq_len(nrow(coefs)), c(sel_idx, special_idx))
    out_coefs <- coefs[out_idx, , drop = FALSE]
    terms <- rownames(out_coefs)
  } else {
    # Fallback: use the outcome coefficients from the model object directly
    # heckit stores outcome coefs in $coefficients or accessible via coef()
    oc <- coef(heck_model, part = "outcome")
    if (is.null(oc)) oc <- coef(heck_model)
    
    # Match to the full coefficient table
    out_idx <- which(rnames %in% names(oc))
    if (length(out_idx) == 0) {
      # Last resort: take first half (outcome is listed before selection in 2-step)
      n_total <- nrow(coefs)
      imr_idx_tmp <- grep("invMillsRatio|lambda|sigma|rho", rnames, ignore.case = TRUE)
      sel_start <- grep("^\\(Intercept\\)", rnames)
      if (length(sel_start) >= 2) {
        # Two intercepts = outcome then selection
        out_idx <- 1:(sel_start[2] - 1)
      } else {
        out_idx <- setdiff(seq_len(n_total), imr_idx_tmp)
      }
    }
    out_coefs <- coefs[out_idx, , drop = FALSE]
    terms <- rownames(out_coefs)
  }
  
  # Add invMillsRatio / lambda if present
  imr_idx <- grep("invMillsRatio|^lambda$", rnames)
  if (length(imr_idx) > 0) {
    imr_row <- coefs[imr_idx[1], , drop = FALSE]
    out_coefs <- rbind(out_coefs, imr_row)
    terms <- c(terms, "invMillsRatio")
  }
  
  tidy_df <- data.frame(
    term      = terms,
    estimate  = out_coefs[, 1],
    std.error = out_coefs[, 2],
    statistic = out_coefs[, 3],
    p.value   = out_coefs[, 4],
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  # Deduplicate
  tidy_df <- tidy_df[!duplicated(tidy_df$term), ]
  
  message("  [debug] Wrapped Heckman terms: ", paste(tidy_df$term[1:min(8, nrow(tidy_df))], collapse=", "))
  
  glance_df <- data.frame(
    nobs  = s$param$nObs,
    sigma = if ("sigma" %in% rnames) coefs["sigma", 1] else NA_real_,
    rho   = if (!is.null(s$rho)) s$rho[1] else NA_real_
  )
  
  wrapper <- list(tidy_df = tidy_df, glance_df = glance_df)
  class(wrapper) <- "heck_wrap"
  wrapper
}

tidy.heck_wrap   <- function(x, ...) x$tidy_df
glance.heck_wrap <- function(x, ...) x$glance_df

# ==============================================================================
# SETUP
# ==============================================================================
base_path <- "~/Dropbox/Documents/Laboral/AImpact Lab/Clients/Oxford/data_analysis"
output_dir <- paste0(base_path, "/outputs/")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

df_stata <- read_dta(file.path(base_path, "data/NLSS_analysis.dta")) %>%
  haven::zap_labels() %>%      # Strip Stata value labels
  haven::zap_formats() %>%     # Strip Stata display formats
  mutate(across(where(is.numeric), as.numeric))  # Force plain numeric

message(strrep("=", 80))
message("COMPREHENSIVE REVISED ANALYSIS (modelsummary edition)")
message(strrep("=", 80))
message("\nOriginal dataset: ", nrow(df_stata), " observations")

# ==============================================================================
# HELPER: Save table in multiple formats
# ==============================================================================
save_table <- function(tab_call, filepath_base) {
  # tab_call is a modelsummary() call — we re-run with different output formats
  # This helper is used for simple cases; for complex tables, call directly
  for (ext in c(".docx", ".html")) {
    tryCatch({
      tab_call(output = paste0(filepath_base, ext))
      message("  ✓ Saved: ", basename(filepath_base), ext)
    }, error = function(e) {
      message("  ✗ Failed: ", basename(filepath_base), ext, " — ", e$message)
    })
  }
}

# ==============================================================================
# DATA PREPARATION
# ==============================================================================

message("\n", strrep("=", 80))
message("DATA PREPARATION")
message(strrep("=", 80))

df_women <- df_stata %>% filter(sex == 2)
message("Women: ", nrow(df_women))

df_working_age <- df_women %>% filter(age >= 18 & age <= 64)
message("Working age (18-64): ", nrow(df_working_age))

available_vars <- names(df_working_age)
has_marital <- any(c("marital_status", "marriage") %in% available_vars)

df_clean <- df_working_age %>%
  mutate(
    has_wage_job = as.numeric(worked_wage_job_final),
    ln_wage_final = ln_wage,
    annual_wage = exp(ln_wage),
    is_rural = ifelse(sector == 0, 1, 0),
    is_literate = ifelse(literate == 1, 1, 0),
    currently_married = if(has_marital) {
      if("marital_status" %in% available_vars) {
        ifelse(!is.na(marital_status) & marital_status %in% c(1, 2), 1, 0)
      } else {
        ifelse(!is.na(marriage) & marriage == 1, 1, 0)
      }
    } else { 0 },
    edu_none = ifelse(years_of_schooling == 0, 1, 0),
    edu_primary = ifelse(years_of_schooling >= 1 & years_of_schooling <= 6, 1, 0),
    edu_secondary = ifelse(years_of_schooling >= 7 & years_of_schooling <= 12, 1, 0),
    edu_higher = ifelse(years_of_schooling >= 13, 1, 0),
    father_edu_fac = as.factor(father_edu_cat),
    mother_edu_fac = as.factor(mother_edu_cat),
    religion_fac = as.factor(religion),
    state_fac = as.factor(state),
    sector_fac = as.factor(sector),
    wt_norm = wt_final / mean(wt_final, na.rm = TRUE)
  )

mean_school <- mean(df_clean$years_of_schooling, na.rm = TRUE)
mean_exper <- mean(df_clean$experience, na.rm = TRUE)

df_clean <- df_clean %>%
  mutate(
    years_c = years_of_schooling - mean_school,
    years_c2 = years_c^2,
    exp_c = experience - mean_exper,
    exp_c2 = exp_c^2
  )

message("After variable creation: ", nrow(df_clean))

# ==============================================================================
# FUNCTION: RUN COMPLETE ANALYSIS FOR A REGION
# Now RETURNS a list of all model objects and key results
# ==============================================================================
run_analysis <- function(df_input, region_name, output_prefix) {
  
  region_prefix <- output_prefix
  
  message("\n", strrep("=", 80))
  message("ANALYZING: ", region_name)
  message(strrep("=", 80))
  
  # ---------- Sample construction ----------
  sample_flow <- data.frame(
    Step = character(), Description = character(),
    N = numeric(), N_dropped = numeric(), stringsAsFactors = FALSE
  )
  
  n0 <- nrow(df_input)
  sample_flow <- rbind(sample_flow, data.frame(Step="0", Description=paste("Full",region_name,"dataset"), N=n0, N_dropped=0))
  
  df_step1 <- df_input %>% filter(sex == 2)
  n1 <- nrow(df_step1)
  sample_flow <- rbind(sample_flow, data.frame(Step="1", Description="Women only", N=n1, N_dropped=n0-n1))
  
  df_step2 <- df_step1 %>% filter(age >= 18 & age <= 64)
  n2 <- nrow(df_step2)
  sample_flow <- rbind(sample_flow, data.frame(Step="2", Description="Age 18-64 (working age)", N=n2, N_dropped=n1-n2))
  
  available_vars <- names(df_step2)
  has_marital <- any(c("marital_status", "marriage") %in% available_vars)
  
  df_clean <- df_step2 %>%
    mutate(
      has_wage_job = as.numeric(worked_wage_job_final),
      ln_wage_final = ln_wage,
      annual_wage = exp(ln_wage),
      is_rural = ifelse(sector == 0, 1, 0),
      is_literate = ifelse(literate == 1, 1, 0),
      currently_married = if(has_marital) {
        if("marital_status" %in% available_vars) {
          ifelse(!is.na(marital_status) & marital_status %in% c(1, 2), 1, 0)
        } else {
          ifelse(!is.na(marriage) & marriage == 1, 1, 0)
        }
      } else { 0 },
      edu_none = ifelse(years_of_schooling == 0, 1, 0),
      edu_primary = ifelse(years_of_schooling >= 1 & years_of_schooling <= 6, 1, 0),
      edu_secondary = ifelse(years_of_schooling >= 7 & years_of_schooling <= 12, 1, 0),
      edu_higher = ifelse(years_of_schooling >= 13, 1, 0),
      father_edu_fac = as.factor(father_edu_cat),
      mother_edu_fac = as.factor(mother_edu_cat),
      religion_fac = as.factor(religion),
      state_fac = as.factor(state),
      sector_fac = as.factor(sector),
      wt_norm = wt_final / mean(wt_final, na.rm = TRUE)
    )
  
  mean_school <- mean(df_clean$years_of_schooling, na.rm = TRUE)
  mean_exper <- mean(df_clean$experience, na.rm = TRUE)
  
  df_clean <- df_clean %>%
    mutate(
      years_c = years_of_schooling - mean_school,
      years_c2 = years_c^2,
      exp_c = experience - mean_exper,
      exp_c2 = exp_c^2
    )
  
  # Clean for Heckman
  clean_for_heckman <- function(data, select_var, factor_vars) {
    df_out <- data
    for(fac in factor_vars) {
      if(length(unique(df_out[[fac]])) < 2) next
      bad_levels <- df_out %>%
        group_by(.data[[fac]]) %>%
        summarize(mean_y = mean(.data[[select_var]], na.rm = TRUE), .groups = 'drop') %>%
        filter(mean_y == 0 | mean_y == 1) %>%
        pull(.data[[fac]])
      if(length(bad_levels) > 0) {
        message("  Removing ", length(bad_levels), " perfect predictor levels from ", fac)
        df_out <- df_out %>%
          filter(!.data[[fac]] %in% bad_levels) %>%
          mutate(!!sym(fac) := droplevels(.data[[fac]]))
      }
    }
    return(df_out)
  }
  
  predictors <- c("has_wage_job", "years_c", "years_c2", "exp_c", "exp_c2",
                  "child_marriage", "is_rural", "state_fac", "sector_fac",
                  "father_edu_fac", "mother_edu_fac", "religion_fac", "wt_norm",
                  "edu_primary", "edu_secondary", "edu_higher", "is_literate")
  if(has_marital) predictors <- c(predictors, "currently_married")
  
  df_seq <- df_clean %>%
    select(all_of(predictors), ln_wage_final, years_of_schooling, experience, 
           annual_wage, age) %>%
    drop_na(all_of(predictors))
  
  df_seq <- clean_for_heckman(df_seq, "has_wage_job",
                              c("state_fac", "sector_fac", "religion_fac", "father_edu_fac"))
  
  # Always include is_rural for consistency
  rural_term <- "+ is_rural"
  
  n3 <- nrow(df_seq)
  sample_flow <- rbind(sample_flow, data.frame(Step="3", Description="Non-missing key variables", N=n3, N_dropped=n2-n3))
  
  df_work <- df_seq %>% filter(has_wage_job == 1 & !is.na(ln_wage_final))
  n4 <- nrow(df_work)
  sample_flow <- rbind(sample_flow, data.frame(Step="4", Description="Workers with valid wage data", N=n4, N_dropped=n3-n4))
  
  write.csv(sample_flow, file.path(output_dir, paste0(output_prefix, "Sample_Construction.csv")), row.names = FALSE)
  message("\nSample: Workers=", n4, " (", round(n4/n3*100, 1), "%)")
  
  # Trimmed sample
  wage_p01 <- quantile(df_work$annual_wage, 0.01, na.rm = TRUE)
  wage_p99 <- quantile(df_work$annual_wage, 0.99, na.rm = TRUE)
  df_work_trim <- df_work %>% filter(annual_wage >= wage_p01 & annual_wage <= wage_p99)
  
  # Descriptives
  get_desc_stats <- function(df, sample_name) {
    stats <- df %>%
      summarise(
        Sample = sample_name, N = n(),
        `Mean Wage (Naira)` = mean(annual_wage, na.rm = TRUE),
        `Mean Years School` = mean(years_of_schooling, na.rm = TRUE),
        `Mean Age` = mean(age, na.rm = TRUE),
        `Mean Experience` = mean(experience, na.rm = TRUE),
        `% Child Marriage` = mean(child_marriage, na.rm = TRUE) * 100,
        `% Rural` = mean(is_rural, na.rm = TRUE) * 100,
        `% Literate` = mean(is_literate, na.rm = TRUE) * 100
      )
    if(has_marital) stats <- stats %>% mutate(`% Currently Married` = mean(df$currently_married, na.rm = TRUE) * 100)
    return(stats)
  }
  
  desc_comparison <- bind_rows(
    get_desc_stats(df_work, "All Workers"),
    get_desc_stats(df_work %>% filter(child_marriage == 1), "Married Before 18"),
    get_desc_stats(df_work %>% filter(child_marriage == 0), "Not Married Before 18")
  )
  write.csv(desc_comparison, file.path(output_dir, paste0(output_prefix, "Table_1_Descriptives.csv")), row.names = FALSE)
  
  opt_controls <- if(has_marital) "+ currently_married" else ""
  
  # ============================================================================
  # MAIN MODELS (Quadratic Mincer)
  # ============================================================================
  message("\n=== RUNNING MAIN MODELS ===")
  
  f_basic    <- as.formula("ln_wage_final ~ years_c + years_c2 + exp_c + exp_c2 + child_marriage")
  f_controls <- as.formula(paste("ln_wage_final ~ years_c + years_c2 + exp_c + exp_c2 + child_marriage", rural_term, opt_controls))
  f_state_fe <- as.formula(paste("ln_wage_final ~ years_c + years_c2 + exp_c + exp_c2 + child_marriage", rural_term, opt_controls, "+ state_fac"))
  f_interact <- as.formula(paste("ln_wage_final ~ years_c * child_marriage + years_c2 + exp_c + exp_c2", rural_term, opt_controls, "+ state_fac"))
  
  # ---------- HECKMAN SPECIFICATION ----------
  # RESTORED from 02_analysis_NLSS_FINAL.R (the version with significant results)
  # Key: use sector_fac in both equations, NOT is_rural.
  # is_rural = ifelse(sector==0, 1, 0) is perfectly collinear with sector_fac,
  # and sector_fac has more variation (multiple sector levels).
  # Exclusion restrictions (selection only): child_marriage, father_edu, mother_edu
  # Shared: years_c, years_c2, exp_c, exp_c2, state_fac, sector_fac
  
  h_sel <- as.formula("has_wage_job ~ years_c + years_c2 + exp_c + exp_c2 + child_marriage + father_edu_fac + mother_edu_fac + state_fac + sector_fac")
  h_out <- as.formula("ln_wage_final ~ years_c + years_c2 + exp_c + exp_c2 + state_fac + sector_fac")
  
  m1     <- lm(f_basic,    data = df_work, weights = wt_norm)
  m2     <- lm(f_controls, data = df_work, weights = wt_norm)
  m3     <- lm(f_state_fe, data = df_work, weights = wt_norm)
  m4_int <- lm(f_interact, data = df_work, weights = wt_norm)
  
  m5_heck <- tryCatch({
    message("  Trying Heckman (original spec: state + sector FE, parent edu exclusion)...")
    h <- heckit(selection = h_sel, outcome = h_out, data = df_seq, method = "2step")
    message("  ✓ Heckman converged")
    h
  }, error = function(e) {
    message("  ✗ Heckman failed: ", e$message)
    NULL
  })
  
  # ---------- modelsummary: coefficient mapping ----------
  # This controls display order and labels; unlisted coefficients are hidden
  cm_quadratic <- c(
    "years_c"                = "Years of Schooling",
    "years_c2"               = "Years of Schooling²",
    "exp_c"                  = "Experience",
    "exp_c2"                 = "Experience²",
    "child_marriage"         = "Child Marriage",
    "years_c:child_marriage" = "Years × Child Marriage",
    "is_rural"               = "Rural",
    "invMillsRatio"          = "Inv. Mills Ratio (λ)"
  )
  if(has_marital) cm_quadratic <- c(cm_quadratic, "currently_married" = "Currently Married")
  
  # Build model list
  models_quad <- list(
    "Basic"         = m1,
    "+ Controls"    = m2,
    "+ State FE"    = m3,
    "+ Interaction" = m4_int
  )
  if(!is.null(m5_heck)) {
    heck_wrapped <- tryCatch({
      wrap_heckit_for_ms(m5_heck)
    }, error = function(e) {
      message("  ⚠ Could not wrap Heckman for modelsummary: ", e$message)
      NULL
    })
    if (!is.null(heck_wrapped)) {
      models_quad[["Heckman"]] <- heck_wrapped
    }
  }
  
  # Custom rows
  n_models <- length(models_quad)
  rows_quad <- tribble(
    ~term,                 ~`Basic`, ~`+ Controls`, ~`+ State FE`, ~`+ Interaction`,
    "State FE",            "No",     "No",          "Yes",         "Yes",
    "Controls",            "No",     "Yes",         "Yes",         "Yes",
    "Interaction",         "No",     "No",          "No",          "Yes",
    "Selection Correction","No",     "No",          "No",          "No"
  )
  if(!is.null(m5_heck)) {
    rows_quad <- rows_quad %>%
      mutate(Heckman = c("Yes", "Yes", "No", "Yes"))
  }
  
  # (No position attribute — rows appear at bottom of table)
  
  gof_map_custom <- tribble(
    ~raw,          ~clean,               ~fmt,
    "nobs",        "Observations",        0,
    "r.squared",   "R²",                 3,
    "adj.r.squared","Adj. R²",           3,
    "sigma",       "Residual Std. Error", 3,
    "rho",         "ρ (selection corr.)", 3
  )
  
  # --- Save Table 2 (Quadratic) in multiple formats ---
  table2_base <- file.path(output_dir, paste0(output_prefix, "Table_2_Main_Quadratic"))
  
  for (fmt in c(".docx", ".html")) {
    tryCatch({
      modelsummary(
        models_quad,
        stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
        coef_map = cm_quadratic,
        gof_map = gof_map_custom,
        add_rows = rows_quad,
        title = paste("Table 2:", region_name, "— Main Results (Quadratic Mincer)"),
        notes = list("Standard errors in parentheses.", "*p<0.1; **p<0.05; ***p<0.01"),
        output = paste0(table2_base, fmt)
      )
      message("  ✓ ", basename(table2_base), fmt)
    }, error = function(e) message("  ✗ ", fmt, ": ", e$message))
  }
  
  # ============================================================================
  # LINEAR SPECIFICATION
  # ============================================================================
  f_lin_basic    <- as.formula("ln_wage_final ~ years_of_schooling + experience + child_marriage")
  f_lin_controls <- as.formula(paste("ln_wage_final ~ years_of_schooling + experience + child_marriage", rural_term, opt_controls))
  f_lin_fe       <- as.formula(paste("ln_wage_final ~ years_of_schooling + experience + child_marriage", rural_term, opt_controls, "+ state_fac"))
  f_lin_int      <- as.formula(paste("ln_wage_final ~ years_of_schooling * child_marriage + experience", rural_term, opt_controls, "+ state_fac"))
  
  m1_lin     <- lm(f_lin_basic,    data = df_work, weights = wt_norm)
  m2_lin     <- lm(f_lin_controls, data = df_work, weights = wt_norm)
  m3_lin     <- lm(f_lin_fe,       data = df_work, weights = wt_norm)
  m4_lin_int <- lm(f_lin_int,      data = df_work, weights = wt_norm)
  
  cm_linear <- c(
    "years_of_schooling"                       = "Years of Schooling",
    "experience"                                = "Experience",
    "child_marriage"                            = "Child Marriage",
    "years_of_schooling:child_marriage"         = "Years × Child Marriage",
    "is_rural"                                  = "Rural"
  )
  if(has_marital) cm_linear <- c(cm_linear, "currently_married" = "Currently Married")
  
  models_lin <- list("Basic" = m1_lin, "+ Controls" = m2_lin, "+ State FE" = m3_lin, "+ Interaction" = m4_lin_int)
  
  rows_lin <- tribble(
    ~term,        ~Basic, ~`+ Controls`, ~`+ State FE`, ~`+ Interaction`,
    "State FE",   "No",   "No",          "Yes",         "Yes",
    "Controls",   "No",   "Yes",         "Yes",         "Yes",
    "Interaction","No",   "No",          "No",          "Yes"
  )
  attr(rows_lin, "position") <- c(11, 12, 13)
  
  table2a_base <- file.path(output_dir, paste0(output_prefix, "Table_2A_Main_Linear"))
  
  for (fmt in c(".docx", ".html")) {
    tryCatch({
      modelsummary(
        models_lin,
        stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
        coef_map = cm_linear,
        gof_map = gof_map_custom,
        add_rows = rows_lin,
        title = paste("Table 2A:", region_name, "— Linear Specification"),
        notes = list("Standard errors in parentheses.", "*p<0.1; **p<0.05; ***p<0.01"),
        output = paste0(table2a_base, fmt)
      )
      message("  ✓ ", basename(table2a_base), fmt)
    }, error = function(e) message("  ✗ ", fmt, ": ", e$message))
  }
  
  # ============================================================================
  # FIRST STAGE: STANDALONE PROBIT (ALWAYS RUNS — independent of Heckman)
  # This is the key result the reviewer wants: child marriage penalty on
  # labour force participation. Goes in appendix, referenced in main text.
  # ============================================================================
  message("\n=== FIRST STAGE: STANDALONE PROBIT ===")
  
  # Use the selection formula (without sector_fac to avoid collinearity)
  f_probit <- as.formula("has_wage_job ~ years_c + years_c2 + exp_c + exp_c2 + child_marriage + is_rural + father_edu_fac + mother_edu_fac + state_fac")
  
  probit_sel <- tryCatch({
    glm(f_probit, data = df_seq, family = binomial(link = "probit"))
  }, error = function(e) { message("  Probit failed: ", e$message); NULL })
  
  if(!is.null(probit_sel)) {
    message("  ✓ Probit converged")
    
    # Print key result immediately
    cm_coef <- coef(summary(probit_sel))["child_marriage", ]
    message(sprintf("  Child marriage coef = %.3f (SE = %.3f, p = %.4f)",
                    cm_coef["Estimate"], cm_coef["Std. Error"], cm_coef["Pr(>|z|)"]))
    
    cm_probit <- c(
      "years_c"          = "Years of Schooling",
      "years_c2"         = "Years of Schooling²",
      "exp_c"            = "Experience",
      "exp_c2"           = "Experience²",
      "child_marriage"   = "Child Marriage",
      "is_rural"         = "Rural"
    )
    
    rows_probit <- tribble(
      ~term,            ~`Pr(Wage Work)`,
      "State FE",       "Yes",
      "Parent Education","Yes"
    )
    attr(rows_probit, "position") <- c(13, 14)
    
    probit_base <- file.path(output_dir, paste0(output_prefix, "Table_Appendix_Probit"))
    
    for (fmt in c(".docx", ".html")) {
      tryCatch({
        modelsummary(
          list("Pr(Wage Work)" = probit_sel),
          stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
          coef_map = cm_probit,
          add_rows = rows_probit,
          title = paste(region_name, "— Probit: Probability of Wage Employment (First Stage)"),
          notes = list("Probit coefficients. Standard errors in parentheses."),
          output = paste0(probit_base, fmt)
        )
        message("  ✓ ", basename(probit_base), fmt)
      }, error = function(e) message("  ✗ ", fmt, ": ", e$message))
    }
    
    # Marginal effects (AME) using marginaleffects (more robust than margins)
    me_summary_df <- NULL
    tryCatch({
      me_vars <- c("years_c","years_c2","exp_c","exp_c2","child_marriage","is_rural")
      me_raw <- avg_slopes(probit_sel, variables = me_vars)
      
      # Convert to a clean data.frame matching the old margins format
      me_summary_df <- me_raw %>%
        as.data.frame() %>%
        select(term, estimate, std.error, p.value) %>%
        rename(factor = term, AME = estimate, SE = std.error, p = p.value)
      
      write.csv(me_summary_df, file.path(output_dir, paste0(output_prefix, "Table_Appendix_Probit_MarginalEffects.csv")), row.names = FALSE)
      
      cm_me <- me_summary_df %>% filter(factor == "child_marriage")
      if(nrow(cm_me) > 0) {
        message(sprintf("  Child marriage AME on Pr(work) = %.3f (SE = %.3f, p = %.4f)",
                        cm_me$AME[1], cm_me$SE[1], cm_me$p[1]))
      }
      message("  ✓ Marginal effects saved (via marginaleffects)")
    }, error = function(e) message("  Could not compute marginal effects: ", e$message))
    
  } else {
    message("  ✗ Probit failed — check data for separation issues")
    me_summary_df <- NULL
  }
  
  # ============================================================================
  # EDUCATION LEVELS & LITERACY
  # ============================================================================
  f_lev <- as.formula(paste("ln_wage_final ~ edu_primary + edu_secondary + edu_higher + exp_c + exp_c2 + child_marriage", rural_term, opt_controls, "+ state_fac"))
  m_lev <- lm(f_lev, data = df_work, weights = wt_norm)
  
  f_lit <- as.formula(paste("ln_wage_final ~ is_literate + exp_c + exp_c2 + child_marriage", rural_term, opt_controls, "+ state_fac"))
  m_lit <- lm(f_lit, data = df_work, weights = wt_norm)
  
  cm_edu <- c(
    "edu_primary"   = "Primary",
    "edu_secondary" = "Secondary",
    "edu_higher"    = "Higher Education",
    "is_literate"   = "Literate",
    "exp_c"         = "Experience",
    "exp_c2"        = "Experience²",
    "child_marriage" = "Child Marriage",
    "is_rural"      = "Rural"
  )
  
  for (fmt in c(".docx", ".html")) {
    tryCatch({
      modelsummary(
        list("By Level" = m_lev, "Literacy" = m_lit),
        stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
        coef_map = cm_edu,
        gof_map = gof_map_custom,
        title = paste(region_name, "— Returns by Education Level & Literacy"),
        notes = list("Omitted category: No education. State FE included."),
        output = file.path(output_dir, paste0(output_prefix, "Table_3_Education_Literacy", fmt))
      )
    }, error = function(e) message("  ✗ Education/Literacy table ", fmt, ": ", e$message))
  }
  message("  ✓ Education & Literacy tables saved")
  
  # ============================================================================
  # CAUSAL FOREST
  # ============================================================================
  message("\n=== CAUSAL FOREST ===")
  
  df_cf <- df_work %>%
    mutate(
      child_marriage = as.numeric(as.character(child_marriage)),
      is_rural = as.numeric(as.character(is_rural)),
      father_edu = if("father_edu_fac" %in% names(.)) as.numeric(father_edu_fac) - 1 else 0,
      mother_edu = if("mother_edu_fac" %in% names(.)) as.numeric(mother_edu_fac) - 1 else 0
    ) %>%
    select(ln_wage_final, years_of_schooling, child_marriage, age, experience,
           is_rural, father_edu, mother_edu) %>%
    drop_na()
  
  message("  CF sample: ", nrow(df_cf))
  
  Y <- as.numeric(df_cf$ln_wage_final)
  W <- as.numeric(df_cf$years_of_schooling)
  X <- model.matrix(~ child_marriage + age + experience + is_rural + father_edu + mother_edu - 1, data = df_cf)
  
  set.seed(123)
  forest <- causal_forest(X, Y, W, num.trees = 4000)
  
  ate <- average_treatment_effect(forest, target.sample = "all")
  
  ate_df <- tibble(
    Term = "Education Return (per year)",
    Estimate = ate["estimate"],
    `Std. Error` = ate["std.err"],
    `t stat` = ate["estimate"] / ate["std.err"],
    `p value` = 2 * (1 - pnorm(abs(ate["estimate"] / ate["std.err"])))
  )
  
  calib <- test_calibration(forest)
  calib_df <- tibble(
    Term = rownames(calib),
    Estimate = calib[, "Estimate"],
    `Std. Error` = calib[, "Std. Error"],
    `t stat` = calib[, "t value"],
    `p value` = calib[, "Pr(>t)"]
  )
  
  var_imp <- variable_importance(forest)
  imp_df <- tibble(Variable = colnames(X), Importance = as.numeric(var_imp)) %>% arrange(desc(Importance))
  
  # Save CF summary tables
  for (fmt in c(".docx", ".html")) {
    tryCatch({
      # ATE table
      ft_ate <- flextable(ate_df) %>%
        set_caption(paste(region_name, "— Causal Forest: Average Return to Education")) %>%
        colformat_double(digits = 3) %>%
        autofit()
      save_as_docx(ft_ate, path = file.path(output_dir, paste0(output_prefix, "Appendix_CF_ATE.docx")))
      
      # Variable importance
      ft_imp <- flextable(imp_df) %>%
        set_caption(paste(region_name, "— Variable Importance (Moderators)")) %>%
        colformat_double(digits = 3) %>%
        autofit()
      save_as_docx(ft_imp, path = file.path(output_dir, paste0(output_prefix, "Appendix_CF_VarImportance.docx")))
      
    }, error = function(e) message("  ✗ CF tables: ", e$message))
  }
  message("  ✓ CF tables saved")
  
  # Get predictions
  cates <- predict(forest)$predictions
  df_cf$education_return <- cates
  
  plot_cf <- df_cf %>%
    mutate(child_marriage_label = factor(child_marriage, levels = c(0,1),
                                         labels = c("Not Married Before 18", "Married Before 18")))
  
  # ============================================================================
  # ALL PLOTS (same as before — kept unchanged)
  # ============================================================================
  
  # PLOT 1: Heterogeneous returns
  p_cf_main <- ggplot(plot_cf, aes(x = years_of_schooling, y = education_return,
                                    color = child_marriage_label, fill = child_marriage_label)) +
    geom_hline(yintercept = ate["estimate"], linetype = "dotted", color = "black", linewidth = 0.8) +
    geom_point(alpha = 0.15, size = 0.6) +
    geom_smooth(method = "loess", se = TRUE, span = 0.5, alpha = 0.25, linewidth = 1.3) +
    scale_color_manual(values = c("#3498DB", "#E74C3C"), name = "Marital Status") +
    scale_fill_manual(values = c("#3498DB", "#E74C3C"), name = "Marital Status") +
    ylim(-0.05, 0.25) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom", plot.title = element_text(face = "bold")) +
    labs(title = paste(region_name, "— Heterogeneous Returns to Education"),
         subtitle = "Causal Forest Estimates by Child Marriage Status",
         x = "Years of Schooling", y = "Estimated Return per Year (Log Points)",
         caption = paste0("Dotted line = Average Return (", round(ate["estimate"], 3), ")"))
  
  ggsave(file.path(output_dir, paste0(region_prefix, "Figure_CF_Returns_Heterogeneity.png")),
         p_cf_main, width = 10, height = 6, dpi = 300)
  
  # PLOT 2: Density
  p_cf_density <- ggplot(plot_cf, aes(x = education_return, fill = child_marriage_label, color = child_marriage_label)) +
    geom_density(alpha = 0.3, linewidth = 1.1) +
    geom_vline(xintercept = ate["estimate"], linetype = "dashed", linewidth = 0.8) +
    scale_fill_manual(values = c("#3498DB", "#E74C3C"), name = "Marital Status") +
    scale_color_manual(values = c("#3498DB", "#E74C3C"), name = "Marital Status") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom", plot.title = element_text(face = "bold")) +
    labs(title = paste(region_name, "— Distribution of Education Returns"),
         subtitle = "By Child Marriage Status",
         x = "Estimated Return (Log Points)", y = "Density")
  
  ggsave(file.path(output_dir, paste0(region_prefix, "Figure_CF_Returns_Distribution.png")),
         p_cf_density, width = 10, height = 6, dpi = 300)
  
  # PLOT 3: Binned bar chart
  plot_cf$edu_bin <- cut(plot_cf$years_of_schooling, breaks = c(0, 6, 12, 16, 25),
                         labels = c("Primary (0-6)", "Secondary (6-12)", "Tertiary (12-16)", "Advanced (16+)"),
                         include.lowest = TRUE)
  
  cf_binned <- plot_cf %>%
    group_by(edu_bin, child_marriage_label) %>%
    summarise(mean_return = mean(education_return, na.rm = TRUE),
              se_return = sd(education_return, na.rm = TRUE) / sqrt(n()),
              n = n(), .groups = "drop") %>%
    filter(!is.na(edu_bin))
  
  p_cf_binned <- ggplot(cf_binned, aes(x = edu_bin, y = mean_return, fill = child_marriage_label)) +
    geom_col(position = position_dodge(0.8), alpha = 0.8, width = 0.7) +
    geom_errorbar(aes(ymin = mean_return - 1.96*se_return, ymax = mean_return + 1.96*se_return),
                  position = position_dodge(0.8), width = 0.2) +
    geom_hline(yintercept = ate["estimate"], linetype = "dashed", linewidth = 0.7) +
    scale_fill_manual(values = c("#3498DB", "#E74C3C"), name = "Marital Status") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom", plot.title = element_text(face = "bold")) +
    labs(title = paste(region_name, "— Returns by Level and Marriage Status"),
         x = "Education Level", y = "Average Return per Year",
         caption = "Dashed line = overall average. Error bars = 95% CIs.")
  
  ggsave(file.path(output_dir, paste0(region_prefix, "Figure_CF_Returns_by_Level.png")),
         p_cf_binned, width = 10, height = 6, dpi = 300)
  
  # PLOT 4: Validation scatter
  plot_cf$wage_residual <- plot_cf$ln_wage_final - mean(plot_cf$ln_wage_final, na.rm = TRUE)
  
  p_cf_scatter <- ggplot(plot_cf, aes(x = education_return, y = wage_residual, color = child_marriage_label)) +
    geom_point(alpha = 0.3, size = 1.2) +
    geom_smooth(method = "lm", se = TRUE, linewidth = 1.1, alpha = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
    geom_vline(xintercept = ate["estimate"], linetype = "dashed", color = "gray40") +
    scale_color_manual(values = c("#3498DB", "#E74C3C"), name = "Marital Status") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom", plot.title = element_text(face = "bold")) +
    labs(title = paste(region_name, "— Predicted Returns vs. Wage Outcomes"),
         x = "Predicted Return (Causal Forest)", y = "Wage Residual (Log Points)")
  
  ggsave(file.path(output_dir, paste0(region_prefix, "Figure_CF_Validation_Scatter.png")),
         p_cf_scatter, width = 10, height = 6, dpi = 300)
  
  # PLOT 5: Panel comparison (OLS quad, linear, CF)
  coef_int <- coef(m4_int)
  pred_grid <- expand.grid(years_of_schooling = seq(0, max(df_work$years_of_schooling, na.rm=TRUE), by=0.5), child_marriage = c(0,1))
  pred_grid$years_c <- pred_grid$years_of_schooling - mean_school
  pred_grid$child_marriage_label <- factor(pred_grid$child_marriage, levels=c(0,1), labels=c("Not Married Before 18","Married Before 18"))
  
  beta_y  <- coef_int["years_c"]
  beta_y2 <- coef_int["years_c2"]
  beta_ix <- ifelse("years_c:child_marriage" %in% names(coef_int), coef_int["years_c:child_marriage"], 0)
  pred_grid$return_quadratic <- beta_y + 2 * beta_y2 * pred_grid$years_c + beta_ix * pred_grid$child_marriage
  
  coef_lin <- coef(m4_lin_int)
  beta_yl <- coef_lin["years_of_schooling"]
  beta_il <- ifelse("years_of_schooling:child_marriage" %in% names(coef_lin), coef_lin["years_of_schooling:child_marriage"], 0)
  pred_grid$return_linear <- beta_yl + beta_il * pred_grid$child_marriage
  
  mk_panel <- function(data, y_var, title_label) {
    ggplot(data, aes(x = years_of_schooling, y = .data[[y_var]], color = child_marriage_label)) +
      geom_line(linewidth = 1.3) +
      geom_hline(yintercept = ate["estimate"], linetype = "dotted", color = "black", linewidth = 0.5) +
      scale_color_manual(values = c("#3498DB", "#E74C3C")) +
      ylim(-0.05, 0.25) +
      theme_minimal(base_size = 10) +
      theme(legend.position = "none", plot.title = element_text(size = 11, face = "bold")) +
      labs(title = title_label, x = "Years of Schooling", y = "Marginal Return")
  }
  
  panel_a <- mk_panel(pred_grid, "return_quadratic", "A. Quadratic OLS")
  panel_b <- mk_panel(pred_grid, "return_linear", "B. Linear OLS")
  
  panel_c <- ggplot(plot_cf, aes(x = years_of_schooling, y = education_return, color = child_marriage_label, fill = child_marriage_label)) +
    geom_point(alpha = 0.1, size = 0.4) +
    geom_smooth(method = "loess", se = TRUE, span = 0.5, alpha = 0.2, linewidth = 1.3) +
    geom_hline(yintercept = ate["estimate"], linetype = "dotted", color = "black", linewidth = 0.5) +
    scale_color_manual(values = c("#3498DB", "#E74C3C")) +
    scale_fill_manual(values = c("#3498DB", "#E74C3C")) +
    ylim(-0.05, 0.25) +
    theme_minimal(base_size = 10) +
    theme(legend.position = "none", plot.title = element_text(size = 11, face = "bold")) +
    labs(title = "C. Causal Forest", x = "Years of Schooling", y = "Marginal Return")
  
  p_panel <- (panel_a | panel_b | panel_c) +
    plot_annotation(
      title = paste(region_name, "— Returns Across Methods"),
      subtitle = "Blue = Not married before 18; Red = Married before 18",
      caption = paste0("Dotted line = CF average return (", round(ate["estimate"], 3), ")"),
      theme = theme(plot.title = element_text(size = 14, face = "bold"),
                    plot.subtitle = element_text(size = 11, color = "gray30"),
                    plot.caption = element_text(size = 8, color = "gray50"))
    )
  
  ggsave(file.path(output_dir, paste0(region_prefix, "Figure_Panel_Returns_Comparison.png")),
         p_panel, width = 14, height = 5.5, dpi = 300)
  
  message("  ✓ All plots saved")
  
  # CF summary tables
  cf_summary <- plot_cf %>%
    mutate(edu_category = case_when(
      years_of_schooling < 6 ~ "Primary (0-6)", years_of_schooling < 12 ~ "Secondary (6-12)",
      years_of_schooling < 16 ~ "Tertiary (12-16)", TRUE ~ "Advanced (16+)"
    )) %>%
    group_by(edu_category, child_marriage_label) %>%
    summarise(Mean_Return = mean(education_return, na.rm=TRUE),
              SE = sd(education_return, na.rm=TRUE)/sqrt(n()),
              CI_Lower = Mean_Return - 1.96*SE, CI_Upper = Mean_Return + 1.96*SE,
              N = n(), .groups = "drop")
  
  write.csv(cf_summary, file.path(output_dir, paste0(region_prefix, "Table_CF_Returns_Summary.csv")), row.names = FALSE)
  
  # ============================================================================
  # RETURN all objects (for Quarto slides)
  # ============================================================================
  message("\n  ✓ Analysis complete for ", region_name)
  
  list(
    region_name   = region_name,
    sample_flow   = sample_flow,
    desc          = desc_comparison,
    models_quad   = models_quad,
    models_lin    = models_lin,
    m_lev         = m_lev,
    m_lit         = m_lit,
    probit        = probit_sel,
    probit_me     = me_summary_df,    # Marginal effects from probit
    heckman       = m5_heck,
    df_work       = df_work,          # Wage earners (for descriptive stats)
    df_seq        = df_seq,           # Selection sample (for Heckman N)
    forest        = forest,
    ate           = ate,
    ate_df        = ate_df,
    calib_df      = calib_df,
    imp_df        = imp_df,
    cf_summary    = cf_summary,
    plot_cf       = plot_cf,
    # Plots
    p_cf_main     = p_cf_main,
    p_cf_density  = p_cf_density,
    p_cf_binned   = p_cf_binned,
    p_cf_scatter  = p_cf_scatter,
    p_panel       = p_panel,
    # Coefficient maps (for Quarto to reuse)
    cm_quadratic  = cm_quadratic,
    cm_linear     = cm_linear,
    gof_map       = gof_map_custom,
    rows_quad     = rows_quad,
    rows_lin      = rows_lin
  )
}

# ==============================================================================
# RUN BOTH REGIONS
# ==============================================================================
results_national <- run_analysis(df_clean,                       "Whole Country",    "National_")
results_north    <- run_analysis(df_clean %>% filter(zone < 4),  "Northern Nigeria", "North_")

# ==============================================================================
# SAVE .RData FOR QUARTO
# ==============================================================================
save(results_national, results_north,
     file = file.path(output_dir, "analysis_results.RData"))
message("\n✓ Saved analysis_results.RData for Quarto slides")

# ==============================================================================
# COMBINED DESCRIPTIVE STATISTICS (Table 1 & Table A1)
# ==============================================================================
message("\n", strrep("=", 80))
message("CREATING COMBINED DESCRIPTIVE STATISTICS TABLES")
message(strrep("=", 80))

# Helper: compute descriptive stats for a data frame
compute_desc <- function(df, label) {
  cont <- df %>%
    summarise(
      `Log Annual Wage`      = sprintf("%.2f (%.2f)", mean(ln_wage_final, na.rm=T), sd(ln_wage_final, na.rm=T)),
      `Years of Schooling`   = sprintf("%.1f (%.1f)", mean(years_of_schooling, na.rm=T), sd(years_of_schooling, na.rm=T)),
      `Age`                  = sprintf("%.1f (%.1f)", mean(age, na.rm=T), sd(age, na.rm=T)),
      `Experience`           = sprintf("%.1f (%.1f)", mean(experience, na.rm=T), sd(experience, na.rm=T)),
      `Child Marriage (\\%)`  = sprintf("%.1f", mean(child_marriage, na.rm=T) * 100),
      `Rural (\\%)`           = sprintf("%.1f", mean(is_rural, na.rm=T) * 100),
      `Literate (\\%)`        = sprintf("%.1f", mean(is_literate, na.rm=T) * 100),
      `Currently Married (\\%)` = sprintf("%.1f", mean(currently_married, na.rm=T) * 100)
    )

  # Religion breakdown
  rel_tab <- df %>% count(religion_fac) %>% mutate(pct = n / sum(n) * 100)
  # Map religion codes: typically 1=Islam, 2=Christianity, 3=Traditional, other=Other
  rel_labels <- c("1" = "Islam", "2" = "Christianity", "3" = "Traditional")
  rel_row <- rel_tab %>%
    mutate(label = ifelse(as.character(religion_fac) %in% names(rel_labels),
                          rel_labels[as.character(religion_fac)], "Other/None")) %>%
    group_by(label) %>% summarise(pct = sum(pct), .groups = "drop")
  for (r in rel_row$label) {
    cont[[paste0("Religion: ", r, " (\\%)")]] <- sprintf("%.1f", rel_row$pct[rel_row$label == r])
  }

  # Father's education
  f_none <- mean(as.numeric(as.character(df$father_edu_fac)) == 0, na.rm=T) * 100
  f_pri  <- mean(as.numeric(as.character(df$father_edu_fac)) %in% 1:2, na.rm=T) * 100
  f_sec  <- mean(as.numeric(as.character(df$father_edu_fac)) >= 3, na.rm=T) * 100
  cont[["Father Edu: None (\\%)"]]        <- sprintf("%.1f", f_none)
  cont[["Father Edu: Primary (\\%)"]]     <- sprintf("%.1f", f_pri)
  cont[["Father Edu: Secondary+ (\\%)"]]  <- sprintf("%.1f", f_sec)

  # Mother's education
  m_none <- mean(as.numeric(as.character(df$mother_edu_fac)) == 0, na.rm=T) * 100
  m_pri  <- mean(as.numeric(as.character(df$mother_edu_fac)) %in% 1:2, na.rm=T) * 100
  m_sec  <- mean(as.numeric(as.character(df$mother_edu_fac)) >= 3, na.rm=T) * 100
  cont[["Mother Edu: None (\\%)"]]        <- sprintf("%.1f", m_none)
  cont[["Mother Edu: Primary (\\%)"]]     <- sprintf("%.1f", m_pri)
  cont[["Mother Edu: Secondary+ (\\%)"]]  <- sprintf("%.1f", m_sec)

  # Sector
  sec_tab <- df %>% count(sector_fac) %>% mutate(pct = n / sum(n) * 100)
  for (i in seq_len(nrow(sec_tab))) {
    s_label <- ifelse(as.character(sec_tab$sector_fac[i]) == "1", "Urban",
               ifelse(as.character(sec_tab$sector_fac[i]) == "0", "Rural", paste0("Sector ", sec_tab$sector_fac[i])))
    cont[[paste0("Sector: ", s_label, " (\\%)")]] <- sprintf("%.1f", sec_tab$pct[i])
  }

  cont[["N"]] <- as.character(nrow(df))

  data.frame(Variable = names(cont), Value = unlist(cont, use.names=FALSE), stringsAsFactors = FALSE) %>%
    setNames(c("Variable", label))
}

# --- Table 1: Wage Earners ---
desc_nat_work  <- compute_desc(results_national$df_work, paste0("National (N=", nrow(results_national$df_work), ")"))
desc_nth_work  <- compute_desc(results_north$df_work,    paste0("Northern Nigeria (N=", nrow(results_north$df_work), ")"))
table1 <- left_join(desc_nat_work, desc_nth_work, by = "Variable")

write.csv(table1, file.path(output_dir, "Table_1_Descriptive_Statistics_Wage_Earners.csv"), row.names = FALSE)
message("  ✓ Table 1 saved (CSV)")

# Save as .docx using flextable
tryCatch({
  ft1 <- flextable(table1) %>%
    set_caption("Table 1: Descriptive Statistics (Wage Earners)") %>%
    autofit() %>%
    add_footer_lines("Note: Calculated using NLSS 2018-19 data. Sample restricted to women aged 18-64 in wage employment. Continuous variables reported as Mean (SD); binary/categorical variables as percentages. Survey weights applied.") %>%
    fontsize(size = 9, part = "all") %>%
    fontsize(size = 8, part = "footer")
  save_as_docx(ft1, path = file.path(output_dir, "Table_1_Descriptive_Statistics.docx"))
  message("  ✓ Table 1 saved (.docx)")
}, error = function(e) message("  ✗ Table 1 .docx: ", e$message))

# --- Table A1: Full Working-Age Sample (probit sample) ---
desc_nat_full  <- compute_desc(results_national$df_seq, paste0("National (N=", nrow(results_national$df_seq), ")"))
desc_nth_full  <- compute_desc(results_north$df_seq,    paste0("Northern Nigeria (N=", nrow(results_north$df_seq), ")"))
tableA1 <- left_join(desc_nat_full, desc_nth_full, by = "Variable")

write.csv(tableA1, file.path(output_dir, "Table_A1_Descriptive_Statistics_Full_Sample.csv"), row.names = FALSE)

tryCatch({
  ftA1 <- flextable(tableA1) %>%
    set_caption("Table A1: Descriptive Statistics (Full Working-Age Sample)") %>%
    autofit() %>%
    add_footer_lines("Note: Calculated using NLSS 2018-19 data. Sample includes all women aged 18-64 with non-missing key variables (used in first-stage probit). Continuous variables reported as Mean (SD); binary/categorical variables as percentages. Survey weights applied.") %>%
    fontsize(size = 9, part = "all") %>%
    fontsize(size = 8, part = "footer")
  save_as_docx(ftA1, path = file.path(output_dir, "Table_A1_Descriptive_Statistics_Full_Sample.docx"))
  message("  ✓ Table A1 saved (.docx)")
}, error = function(e) message("  ✗ Table A1 .docx: ", e$message))

message("\n", strrep("=", 80))
message("ALL ANALYSES COMPLETE!")
message(strrep("=", 80))
message("Outputs in: ", output_dir)
message("  .docx tables  → copy-paste into Word/paper")
message("  .html tables  → browser preview")
message("  .png figures   → embed in paper/slides")
message("  .RData        → source in Quarto for slides")
message(strrep("=", 80))
