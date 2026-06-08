pacman::p_load(
  dplyr,
  stringr,
  tidyr,
  purrr,
  sf,
  geobr
)
source("source/fun_census.R")
source("source/fun_census1920.R")
source("source/fun_amc.R")

# ============================================================
# 1. Load AMC crosswalk and geometries
# ============================================================

amc_crosswalk <- load_amc_crosswalk()
amc_sf <- load_amc_sf()

cat("AMC crosswalk:", nrow(amc_crosswalk), "municipality-to-AMC mappings,",
    n_distinct(amc_crosswalk$code_amc), "AMC units\n")

# ============================================================
# 2. Load raw census data
# ============================================================

df_1872 <- fun_load_db_table("brazil/release/brazil_muni.db", table_name = "census", year = 1872)
df_1920 <- fun_load_db_table("brazil/release/brazil_muni.db", table_name = "census", year = 1920)

# ============================================================
# 3. Aggregate 1872 to AMC level
# ============================================================

cols_1872_counts <- names(df_1872)[
  stringr::str_detect(
    norm_name(names(df_1872)),
    "^(h_livres|m_livres|f_livres|h_escr|h_escrv|m_escr|m_escrv|soma_l|soma_e|soma_escr|soma_escrv|soma_g)_"
  )
]

amc_1872 <- aggregate_to_amc(df_1872, amc_crosswalk, cols_1872_counts)

unmatched_1872 <- df_1872 %>%
  mutate(muni_code = as.character(muni_malha_id)) %>%
  anti_join(amc_crosswalk, by = "muni_code")
cat("1872 municipalities not matched to AMC:", nrow(unmatched_1872), "of", nrow(df_1872), "\n")

# ============================================================
# 4. Aggregate 1920 to AMC level
# ============================================================

cols_1920_counts <- names(df_1920)[
  stringr::str_detect(names(df_1920), "^(peaset|popmun20|popalf20|piaalf20|pia20)")
]

amc_1920 <- aggregate_to_amc(df_1920, amc_crosswalk, cols_1920_counts)

unmatched_1920 <- df_1920 %>%
  mutate(muni_code = as.character(muni_malha_id)) %>%
  anti_join(amc_crosswalk, by = "muni_code")
cat("1920 municipalities not matched to AMC:", nrow(unmatched_1920), "of", nrow(df_1920), "\n")

# ============================================================
# 5. Reusable sector variable + transformation metric builders
# ============================================================

logit <- function(p) {
  p_clamped <- pmax(pmin(p, 1 - 1e-8), 1e-8)
  log(p_clamped / (1 - p_clamped))
}

add_1872_sector_vars <- function(df) {
  df %>%
    mutate(
      pop_total_1872 = sum_exact(., "soma_g_almas"),
      pop_enslaved_1872 = sum_exact(., c("soma_e_almas", "soma_escr_almas", "soma_escrv_almas")),
      pop_literate_1872 = sum_match(., prefix_total, "sabemlereescrever"),
      occ_total_1872 = sum_match(., prefix_total, working_occ_patterns),
      agr_total_1872 = sum_match(., prefix_total, agriculture_patterns),
      mfg_total_1872 = sum_match(., prefix_total, manufacturing_patterns),
      serv_total_1872 = pmax(occ_total_1872 - agr_total_1872 - mfg_total_1872, 0),
      agr_enslaved_1872 = sum_match(., prefix_slave, agriculture_patterns),
      mfg_enslaved_1872 = sum_match(., prefix_slave, manufacturing_patterns),
      enslaved_share_1872 = safe_div(pop_enslaved_1872, pop_total_1872),
      enslaved_mfg_share_1872 = safe_div(mfg_enslaved_1872, mfg_total_1872),
      enslaved_agr_share_1872 = safe_div(agr_enslaved_1872, agr_total_1872),
      mfg_share_1872 = safe_div(mfg_total_1872, occ_total_1872),
      agr_share_1872 = safe_div(agr_total_1872, occ_total_1872),
      serv_share_1872 = safe_div(serv_total_1872, occ_total_1872),
      non_agr_share_1872 = 1 - replace_na(agr_share_1872, 1),
      literacy_rate_1872 = safe_div(pop_literate_1872, pop_total_1872)
    )
}

add_1920_sector_vars <- function(df) {
  df %>%
    mutate(
      pop_total_1920 = popmun20,
      pop_literate_1920 = popalf20,
      occ_total_1920 = pia20,
      agr_total_1920 = sum_exact(., agriculture_cols_1920),
      mfg_total_1920 = sum_exact(., manufacturing_cols_1920),
      serv_total_1920 = pmax(occ_total_1920 - agr_total_1920 - mfg_total_1920, 0),
      mfg_share_1920 = safe_div(mfg_total_1920, occ_total_1920),
      agr_share_1920 = safe_div(agr_total_1920, occ_total_1920),
      serv_share_1920 = safe_div(serv_total_1920, occ_total_1920),
      non_agr_share_1920 = 1 - replace_na(agr_share_1920, 1),
      mfg_agr_ratio_1920 = safe_div(mfg_total_1920, agr_total_1920),
      literacy_rate_1920 = safe_div(pop_literate_1920, pop_total_1920),
      mfg_percapita_1920 = safe_div(mfg_total_1920, pop_total_1920)
    )
}

shared_vars_1872 <- c(
  "pop_total_1872", "pop_enslaved_1872", "pop_literate_1872",
  "occ_total_1872", "agr_total_1872", "mfg_total_1872", "serv_total_1872",
  "enslaved_share_1872", "enslaved_mfg_share_1872", "enslaved_agr_share_1872",
  "mfg_share_1872", "agr_share_1872", "serv_share_1872", "non_agr_share_1872",
  "literacy_rate_1872"
)

shared_vars_1920 <- c(
  "pop_total_1920", "pop_literate_1920",
  "occ_total_1920", "agr_total_1920", "mfg_total_1920", "serv_total_1920",
  "mfg_share_1920", "agr_share_1920", "serv_share_1920", "non_agr_share_1920",
  "mfg_agr_ratio_1920", "literacy_rate_1920", "mfg_percapita_1920"
)

add_transformation_metrics <- function(df) {
  df %>%
    mutate(
      mfg_share_change_pp = mfg_share_1920 - mfg_share_1872,
      mfg_share_log_ratio = if_else(
        !is.na(mfg_share_1872) & mfg_share_1872 > 0 &
          !is.na(mfg_share_1920) & mfg_share_1920 > 0,
        log(mfg_share_1920 / mfg_share_1872),
        NA_real_
      ),
      mfg_share_cagr = if_else(
        !is.na(mfg_share_1872) & mfg_share_1872 > 0 &
          !is.na(mfg_share_1920) & mfg_share_1920 > 0,
        (mfg_share_1920 / mfg_share_1872)^(1 / 48) - 1,
        NA_real_
      ),
      mfg_share_logit_change = if_else(
        !is.na(mfg_share_1872) & mfg_share_1872 > 0 & mfg_share_1872 < 1 &
          !is.na(mfg_share_1920) & mfg_share_1920 > 0 & mfg_share_1920 < 1,
        logit(mfg_share_1920) - logit(mfg_share_1872),
        NA_real_
      ),
      non_agr_share_change_pp = non_agr_share_1920 - non_agr_share_1872,
      mfg_agr_ratio_1872 = safe_div(mfg_total_1872, agr_total_1872),
      mfg_agr_ratio_change = mfg_agr_ratio_1920 - mfg_agr_ratio_1872,
      structural_change_index =
        abs(mfg_share_1920 - mfg_share_1872) +
        abs(agr_share_1920 - agr_share_1872) +
        abs(serv_share_1920 - serv_share_1872),
      agr_exit_share = agr_share_1872 - agr_share_1920,
      serv_share_change_pp = serv_share_1920 - serv_share_1872,
      literacy_rate_change_pp = literacy_rate_1920 - literacy_rate_1872,
      pop_growth_rate = if_else(
        !is.na(pop_total_1872) & pop_total_1872 > 0 &
          !is.na(pop_total_1920) & pop_total_1920 > 0,
        log(pop_total_1920 / pop_total_1872) / 48,
        NA_real_
      )
    )
}

# ============================================================
# 6. Build AMC-level transformation panel
# ============================================================

amc_1872 <- add_1872_sector_vars(amc_1872)
amc_1920 <- add_1920_sector_vars(amc_1920)

transformation_panel <- amc_1872 %>%
  select(code_amc, all_of(shared_vars_1872)) %>%
  inner_join(
    amc_1920 %>% select(code_amc, all_of(shared_vars_1920)),
    by = "code_amc"
  ) %>%
  add_transformation_metrics()

cat("Transformation panel:", nrow(transformation_panel), "AMC units\n")

# ============================================================
# 7. Build state-level transformation panel
# ============================================================

aggregate_to_state <- function(df, count_cols) {
  df %>%
    mutate(state_code = substr(as.character(muni_malha_id), 1, 2)) %>%
    group_by(state_code) %>%
    summarise(across(all_of(count_cols), ~ sum(.x, na.rm = TRUE)), .groups = "drop")
}

state_1872 <- aggregate_to_state(df_1872, cols_1872_counts) %>%
  add_1872_sector_vars()

state_1920 <- aggregate_to_state(df_1920, cols_1920_counts) %>%
  add_1920_sector_vars()

state_transformation_panel <- state_1872 %>%
  select(state_code, all_of(shared_vars_1872)) %>%
  inner_join(
    state_1920 %>% select(state_code, all_of(shared_vars_1920)),
    by = "state_code"
  ) %>%
  add_transformation_metrics()

cat("State transformation panel:", nrow(state_transformation_panel), "states\n")

# ============================================================
# 8. AMC-to-state mapping and regression panel
# ============================================================

amc_state_map <- df_1872 %>%
  mutate(muni_code = as.character(muni_malha_id),
         state_code = substr(muni_code, 1, 2)) %>%
  inner_join(amc_crosswalk, by = "muni_code") %>%
  count(code_amc, state_code) %>%
  group_by(code_amc) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(code_amc, state_code)

transformation_panel <- transformation_panel %>%
  left_join(amc_state_map, by = "code_amc")

state_name_lookup_trans <- tribble(
  ~state_code, ~state_name,
  "13", "Amazonas",
  "15", "Para",
  "21", "Maranhao",
  "22", "Piauhy",
  "23", "Ceara",
  "24", "Rio Grande do Norte",
  "25", "Parahyba",
  "26", "Pernambuco",
  "27", "Alagoas",
  "28", "Sergipe",
  "29", "Bahia",
  "30", "Municipio Neutro",
  "31", "Minas Geraes",
  "32", "Espirito Santo",
  "33", "Rio de Janeiro",
  "35", "Sao Paulo",
  "41", "Parana",
  "42", "Santa Catharina",
  "43", "Rio Grande do Sul",
  "51", "Matto Grosso",
  "52", "Goyaz"
)

regression_y_metrics <- c(
  "mfg_share_change_pp", "mfg_share_log_ratio", "mfg_share_cagr",
  "mfg_share_logit_change", "non_agr_share_change_pp",
  "mfg_agr_ratio_change", "structural_change_index",
  "agr_exit_share", "serv_share_change_pp",
  "literacy_rate_change_pp", "pop_growth_rate"
)

min_amc_threshold <- 5

regression_results <- purrr::map_dfr(regression_y_metrics, function(y_col) {
  purrr::map_dfr(
    unique(na.omit(transformation_panel$state_code)),
    function(sc) {
      sub <- transformation_panel %>%
        filter(state_code == sc, !is.na(.data[[y_col]]), !is.na(enslaved_share_1872))

      if (nrow(sub) < min_amc_threshold) return(NULL)
      if (sd(sub$enslaved_share_1872, na.rm = TRUE) == 0) return(NULL)
      if (sd(sub[[y_col]], na.rm = TRUE) == 0) return(NULL)

      sub <- sub %>%
        mutate(
          x_std = as.numeric(scale(enslaved_share_1872)),
          y_std = as.numeric(scale(.data[[y_col]]))
        )

      fit <- lm(y_std ~ x_std, data = sub)
      s <- summary(fit)
      coefs <- coef(s)

      tibble(
        state_code = sc,
        y_metric = y_col,
        n_amc = nrow(sub),
        coef = coefs["x_std", "Estimate"],
        se = coefs["x_std", "Std. Error"],
        t_value = coefs["x_std", "t value"],
        p_value = coefs["x_std", "Pr(>|t|)"],
        ci_lower = confint(fit)["x_std", 1],
        ci_upper = confint(fit)["x_std", 2]
      )
    }
  )
})

regression_results <- regression_results %>%
  left_join(state_name_lookup_trans, by = "state_code")

cat("Regression results:", nrow(regression_results), "state-metric combinations\n")

# ============================================================
# 9. National-level regressions (pooled and with state FEs)
# ============================================================

regression_results_national <- purrr::map_dfr(regression_y_metrics, function(y_col) {
  sub <- transformation_panel %>%
    filter(!is.na(.data[[y_col]]), !is.na(enslaved_share_1872), !is.na(state_code))

  if (nrow(sub) < 10) return(NULL)
  if (sd(sub$enslaved_share_1872, na.rm = TRUE) == 0) return(NULL)
  if (sd(sub[[y_col]], na.rm = TRUE) == 0) return(NULL)

  sub <- sub %>%
    mutate(
      x_std = as.numeric(scale(enslaved_share_1872)),
      y_std = as.numeric(scale(.data[[y_col]]))
    )

  fit_pooled <- lm(y_std ~ x_std, data = sub)
  s_pooled <- summary(fit_pooled)
  ci_pooled <- confint(fit_pooled)

  fit_fe <- lm(y_std ~ x_std + factor(state_code), data = sub)
  s_fe <- summary(fit_fe)
  ci_fe <- confint(fit_fe)

  bind_rows(
    tibble(
      model = "pooled",
      y_metric = y_col,
      n_obs = nrow(sub),
      coef = coef(s_pooled)["x_std", "Estimate"],
      se = coef(s_pooled)["x_std", "Std. Error"],
      t_value = coef(s_pooled)["x_std", "t value"],
      p_value = coef(s_pooled)["x_std", "Pr(>|t|)"],
      ci_lower = ci_pooled["x_std", 1],
      ci_upper = ci_pooled["x_std", 2]
    ),
    tibble(
      model = "state_fe",
      y_metric = y_col,
      n_obs = nrow(sub),
      coef = coef(s_fe)["x_std", "Estimate"],
      se = coef(s_fe)["x_std", "Std. Error"],
      t_value = coef(s_fe)["x_std", "t value"],
      p_value = coef(s_fe)["x_std", "Pr(>|t|)"],
      ci_lower = ci_fe["x_std", 1],
      ci_upper = ci_fe["x_std", 2]
    )
  )
})

cat("National regression results:", nrow(regression_results_national), "model-metric combinations\n")

# ============================================================
# 10. Save outputs
# ============================================================

dir.create("build", showWarnings = FALSE)

saveRDS(transformation_panel, "build/transformation_panel.rds")
saveRDS(state_transformation_panel, "build/state_transformation_panel.rds")
saveRDS(regression_results, "build/regression_results.rds")
saveRDS(regression_results_national, "build/regression_results_national.rds")
saveRDS(amc_sf, "build/amc_sf.rds")

cat("Transformation outputs saved to build/\n")
