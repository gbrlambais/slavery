pacman::p_load(dplyr, sf, jsonlite, ggplot2, scales, tidyr)
source("source/fun_census.R")

# ============================================================
# Load pre-built data
# ============================================================

dashboard_panel_1872 <- readRDS("build/dashboard_panel.rds")
dashboard_panel_1920 <- readRDS("build/dashboard_panel_1920.rds")
transformation_panel <- readRDS("build/transformation_panel.rds")
state_transformation_panel <- readRDS("build/state_transformation_panel.rds")
regression_results <- readRDS("build/regression_results.rds")
regression_results_national <- readRDS("build/regression_results_national.rds")

mun_sf_1872 <- readRDS("build/mun_sf_1872.rds")
state_sf_1872 <- readRDS("build/state_sf_1872.rds")
mun_sf_1920 <- readRDS("build/mun_sf_1920.rds")
state_sf_1920 <- readRDS("build/state_sf_1920.rds")
amc_sf <- readRDS("build/amc_sf.rds")

# ============================================================
# Helpers
# ============================================================

dir.create("docs/data", showWarnings = FALSE, recursive = TRUE)
dir.create("docs/plots", showWarnings = FALSE, recursive = TRUE)

export_columnar <- function(df, path) {
  cols <- lapply(as.list(df), function(x) {
    if (is.numeric(x)) round(x, 6) else as.character(x)
  })
  jsonlite::write_json(cols, path, auto_unbox = FALSE, na = "null", digits = 6)
  cat("  ", basename(path), ":", round(file.size(path) / 1024), "KB\n")
}

export_geojson <- function(sf_obj, path) {
  sf::st_write(sf_obj, path, driver = "GeoJSON", delete_dsn = TRUE, quiet = TRUE)
  cat("  ", basename(path), ":", round(file.size(path) / 1024), "KB\n")
}

regression_y_labels <- c(
  "mfg_share_change_pp" = "Mfg share change (pp)",
  "mfg_share_log_ratio" = "Mfg share log ratio",
  "mfg_share_cagr" = "Mfg share CAGR",
  "mfg_share_logit_change" = "Mfg share logit change",
  "non_agr_share_change_pp" = "Non-agr share change (pp)",
  "mfg_agr_ratio_change" = "Mfg/agr ratio change",
  "structural_change_index" = "Structural change index",
  "agr_exit_share" = "Agr exit share",
  "serv_share_change_pp" = "Services share change (pp)",
  "literacy_rate_change_pp" = "Literacy rate change (pp)",
  "pop_growth_rate" = "Pop growth rate"
)

state_name_lookup_full <- tribble(
  ~state_code, ~state_name,
  "13", "Amazonas", "15", "Para", "21", "Maranhao", "22", "Piauhy",
  "23", "Ceara", "24", "Rio Grande do Norte", "25", "Parahyba",
  "26", "Pernambuco", "27", "Alagoas", "28", "Sergipe", "29", "Bahia",
  "30", "Municipio Neutro", "31", "Minas Geraes", "32", "Espirito Santo",
  "33", "Rio de Janeiro", "35", "Sao Paulo", "41", "Parana",
  "42", "Santa Catharina", "43", "Rio Grande do Sul",
  "51", "Matto Grosso", "52", "Goyaz"
)

# ============================================================
# 1. Export 1872 panel + geographies
# ============================================================

cat("Exporting 1872 data...\n")

dashboard_panel_1872 %>%
  transmute(
    gl = geography_level, gid = geography_id, gn = geography_name,
    gt = group_type, grid = group_id, glab = group_label,
    n_total, n_free, n_enslaved, n_male, n_female,
    pop_total, pop_free, pop_enslaved, pop_literate,
    literacy_rate_population, enslaved_rate_population,
    occ_total, occ_free, occ_enslaved, occ_male, occ_female,
    share_of_all_occupations, share_free_within_group,
    share_enslaved_within_group, share_male_within_group,
    share_female_within_group,
    n_total_national, n_free_national, n_enslaved_national,
    n_male_national, n_female_national,
    pop_literate_national, pop_enslaved_national,
    share_of_national_occupations, share_free_of_national_group,
    share_enslaved_of_national_group, share_male_of_national_group,
    share_female_of_national_group,
    literacy_rate_of_national_pop, enslaved_rate_of_national_pop
  ) %>%
  export_columnar("docs/data/panel_1872.json")

mun_sf_1872 %>% select(code_muni_1872) %>%
  export_geojson("docs/data/mun_1872.geojson")

state_sf_1872 %>% select(code_state_chr) %>%
  export_geojson("docs/data/state_1872.geojson")

# ============================================================
# 2. Export 1920 panel + geographies
# ============================================================

cat("Exporting 1920 data...\n")

dashboard_panel_1920 %>%
  transmute(
    gl = geography_level, gid = geography_id, gn = geography_name,
    gt = group_type, grid = group_id, glab = group_label,
    n_total, n_male, n_female,
    pop_total, pop_literate, literacy_rate_population,
    occ_total, occ_male, occ_female,
    share_of_all_occupations, share_male_within_group,
    share_female_within_group,
    n_total_national, n_male_national, n_female_national,
    pop_literate_national,
    share_of_national_occupations, share_male_of_national_group,
    share_female_of_national_group, literacy_rate_of_national_pop
  ) %>%
  export_columnar("docs/data/panel_1920.json")

mun_sf_1920 %>% select(code_muni_1920) %>%
  export_geojson("docs/data/mun_1920.geojson")

state_sf_1920 %>% select(code_state_chr) %>%
  export_geojson("docs/data/state_1920.geojson")

# ============================================================
# 3. Export transformation panels + AMC geometry
# ============================================================

cat("Exporting transformation data...\n")

trans_cols <- c(
  "mfg_share_1920", "agr_share_1920", "non_agr_share_1920",
  "mfg_agr_ratio_1920", "literacy_rate_1920", "mfg_percapita_1920",
  "mfg_share_change_pp", "mfg_share_log_ratio", "mfg_share_cagr",
  "mfg_share_logit_change", "non_agr_share_change_pp",
  "mfg_agr_ratio_change", "structural_change_index", "agr_exit_share",
  "serv_share_change_pp", "literacy_rate_change_pp", "pop_growth_rate",
  "enslaved_share_1872", "enslaved_mfg_share_1872", "enslaved_agr_share_1872",
  "mfg_share_1872", "literacy_rate_1872",
  "agr_share_1872", "serv_share_1872", "serv_share_1920"
)

transformation_panel %>%
  mutate(code_amc = as.character(code_amc)) %>%
  select(code_amc, state_code, all_of(trans_cols)) %>%
  export_columnar("docs/data/transformation_amc.json")

state_transformation_panel %>%
  left_join(state_name_lookup_full, by = "state_code") %>%
  select(state_code, state_name, all_of(trans_cols)) %>%
  export_columnar("docs/data/transformation_state.json")

amc_sf %>%
  mutate(code_amc = as.character(code_amc)) %>%
  select(code_amc) %>%
  export_geojson("docs/data/amc.geojson")

# ============================================================
# 4. Export regression results
# ============================================================

cat("Exporting regression data...\n")

regression_results %>%
  select(state_code, state_name, y_metric, n_amc, coef, se, ci_lower, ci_upper, p_value) %>%
  mutate(across(c(coef, se, ci_lower, ci_upper, p_value), ~ round(.x, 6))) %>%
  jsonlite::write_json("docs/data/regression_state.json", auto_unbox = TRUE, na = "null")
cat("  regression_state.json:", round(file.size("docs/data/regression_state.json") / 1024), "KB\n")

regression_results_national %>%
  select(model, y_metric, n_obs, coef, se, ci_lower, ci_upper, p_value) %>%
  mutate(across(c(coef, se, ci_lower, ci_upper, p_value), ~ round(.x, 6))) %>%
  jsonlite::write_json("docs/data/regression_national.json", auto_unbox = TRUE, na = "null")
cat("  regression_national.json:", round(file.size("docs/data/regression_national.json") / 1024), "KB\n")

# ============================================================
# 5. Pre-render regression plots
# ============================================================

sig_scale <- scale_color_manual(
  values = c("TRUE" = "#d62728", "FALSE" = "#7f7f7f"),
  labels = c("TRUE" = "p < 0.05", "FALSE" = "p >= 0.05"),
  name = "Significance"
)

forest_theme <- theme_minimal(base_size = 14) +
  theme(panel.grid.major.y = element_blank(), legend.position = "bottom")

# --- State forest plots (1 per Y metric) ---
cat("Generating state regression plots...\n")

for (y_col in names(regression_y_labels)) {
  y_label <- regression_y_labels[y_col]

  plot_data <- regression_results %>%
    filter(y_metric == y_col) %>%
    mutate(
      significant = p_value < 0.05,
      state_label = paste0(state_name, " (n=", n_amc, ")")
    ) %>%
    arrange(coef) %>%
    mutate(state_label = factor(state_label, levels = state_label))

  if (nrow(plot_data) == 0) next

  p <- ggplot(plot_data, aes(x = coef, y = state_label)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0.2, color = "grey40") +
    geom_point(aes(color = significant), size = 3) +
    sig_scale +
    labs(
      x = "Standardized coefficient (effect of enslaved share 1872)", y = NULL,
      title = paste0("Effect of 1872 enslaved share on: ", y_label),
      subtitle = "Within-state standardized regressions at AMC level"
    ) +
    forest_theme

  ggsave(sprintf("docs/plots/state_reg_%s.png", y_col), p,
         width = 10, height = 8, dpi = 150, bg = "white")
}

# --- Scatter plots (1 per Y metric) ---
cat("Generating scatter plots...\n")

for (y_col in names(regression_y_labels)) {
  y_label <- regression_y_labels[y_col]

  plot_data <- transformation_panel %>%
    filter(!is.na(enslaved_share_1872), !is.na(.data[[y_col]])) %>%
    left_join(state_name_lookup_full, by = "state_code")

  if (nrow(plot_data) == 0) next

  p <- ggplot(plot_data, aes(x = enslaved_share_1872, y = .data[[y_col]])) +
    geom_point(aes(color = state_name), alpha = 0.6, size = 1.5) +
    geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 0.8) +
    scale_x_continuous(labels = scales::percent_format()) +
    labs(
      x = "Enslaved share 1872", y = y_label,
      title = paste0("Enslaved share 1872 vs. ", y_label),
      subtitle = paste(nrow(plot_data), "AMCs"),
      color = "State"
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "right")

  ggsave(sprintf("docs/plots/scatter_%s.png", y_col), p,
         width = 10, height = 7, dpi = 150, bg = "white")
}

# --- National pooled plot ---
cat("Generating national regression plots...\n")

plot_data <- regression_results_national %>%
  filter(model == "pooled") %>%
  mutate(
    significant = p_value < 0.05,
    y_label = regression_y_labels[y_metric]
  ) %>%
  arrange(coef) %>%
  mutate(y_label = factor(y_label, levels = y_label))

p <- ggplot(plot_data, aes(x = coef, y = y_label)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0.2, color = "grey40") +
  geom_point(aes(color = significant), size = 3) +
  sig_scale +
  labs(
    x = "Standardized coefficient (effect of enslaved share 1872)", y = NULL,
    title = "Pooled OLS (no state fixed effects)",
    subtitle = paste("n =", plot_data$n_obs[1], "AMCs")
  ) +
  forest_theme

ggsave("docs/plots/national_pooled.png", p, width = 10, height = 6, dpi = 150, bg = "white")

# --- National FE plot ---
plot_data <- regression_results_national %>%
  filter(model == "state_fe") %>%
  mutate(
    significant = p_value < 0.05,
    y_label = regression_y_labels[y_metric]
  ) %>%
  arrange(coef) %>%
  mutate(y_label = factor(y_label, levels = y_label))

p <- ggplot(plot_data, aes(x = coef, y = y_label)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0.2, color = "grey40") +
  geom_point(aes(color = significant), size = 3) +
  sig_scale +
  labs(
    x = "Standardized coefficient (effect of enslaved share 1872)", y = NULL,
    title = "With state fixed effects",
    subtitle = paste("n =", plot_data$n_obs[1], "AMCs")
  ) +
  forest_theme

ggsave("docs/plots/national_fe.png", p, width = 10, height = 6, dpi = 150, bg = "white")

# --- Quartile plot ---
cat("Generating quartile plot...\n")

plot_data <- transformation_panel %>%
  filter(!is.na(enslaved_share_1872), !is.na(mfg_share_change_pp),
         !is.na(serv_share_change_pp)) %>%
  mutate(
    agr_share_change_pp = agr_share_1920 - agr_share_1872,
    slavery_quartile = cut(
      enslaved_share_1872,
      breaks = quantile(enslaved_share_1872, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
      labels = c("Q1 (lowest)", "Q2", "Q3", "Q4 (highest)"),
      include.lowest = TRUE
    )
  )

n_total <- nrow(plot_data)
quartile_n <- plot_data %>% count(slavery_quartile)
n_label <- paste(quartile_n$n, collapse = "/")

baseline_means <- plot_data %>%
  group_by(slavery_quartile) %>%
  summarise(
    Agriculture = mean(agr_share_1872, na.rm = TRUE),
    Manufacturing = mean(mfg_share_1872, na.rm = TRUE),
    Services = mean(serv_share_1872, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(c(Agriculture, Manufacturing, Services),
               names_to = "Sector", values_to = "mean_share") %>%
  mutate(
    Sector = factor(Sector, levels = c("Agriculture", "Manufacturing", "Services")),
    panel = "1872 sector shares (baseline)"
  )

change_means <- plot_data %>%
  group_by(slavery_quartile) %>%
  summarise(
    Agriculture = mean(agr_share_change_pp, na.rm = TRUE),
    Manufacturing = mean(mfg_share_change_pp, na.rm = TRUE),
    Services = mean(serv_share_change_pp, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(c(Agriculture, Manufacturing, Services),
               names_to = "Sector", values_to = "mean_share") %>%
  mutate(
    Sector = factor(Sector, levels = c("Agriculture", "Manufacturing", "Services")),
    panel = "1872-1920 sector share change (pp)"
  )

combined <- bind_rows(baseline_means, change_means) %>%
  mutate(panel = factor(panel, levels = c(
    "1872 sector shares (baseline)", "1872-1920 sector share change (pp)"
  )))

sector_colors <- c("Agriculture" = "#2ca02c", "Manufacturing" = "#1f77b4", "Services" = "#ff7f0e")

p <- ggplot(combined, aes(x = slavery_quartile, y = mean_share, fill = Sector)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_hline(yintercept = 0, linetype = "solid", color = "grey30") +
  facet_wrap(~ panel, scales = "free_y") +
  scale_fill_manual(values = sector_colors) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = "Enslaved share 1872 (quartile)", y = NULL,
    title = "Sector composition and change by slavery intensity",
    subtitle = paste0(n_total, " AMCs (", n_label, " per quartile)")
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

ggsave("docs/plots/quartile.png", p, width = 12, height = 6, dpi = 150, bg = "white")

cat("\nAll static exports complete!\n")
cat("Files in docs/data/:\n")
for (f in list.files("docs/data")) cat("  ", f, "\n")
cat("Files in docs/plots/:\n")
for (f in list.files("docs/plots")) cat("  ", f, "\n")
