pacman::p_load(
  dplyr,
  stringr,
  tidyr,
  readr,
  sf,
  geobr
)
source("source/fun_census.R")

# ============================================================
# 1. Load census data
# ============================================================

df_censo1872 <- fun_load_db_table("brazil/release/brazil_muni.db", table_name = "census", year = 1872)

# ============================================================
# 2. Load 1872 geobr shapefiles
# ============================================================

mun_sf_1872 <- geobr::read_municipality(
  code_muni = "all",
  year = 1872,
  simplified = TRUE,
  showProgress = FALSE
) %>%
  mutate(
    code_muni_1872 = as.character(code_muni),
    code_state = as.integer(code_state)
  )

state_sf_1872 <- geobr::read_state(
  year = 1872,
  simplified = TRUE,
  showProgress = FALSE
) %>%
  mutate(
    code_state = as.integer(code_state),
    code_state_chr = as.character(code_state)
  )

# ============================================================
# 3. Join census to geography
# ============================================================

mun_lookup_1872 <- mun_sf_1872 %>%
  st_drop_geometry() %>%
  transmute(
    code_muni_1872,
    name_muni_map = name_muni,
    code_state,
    abbrev_state,
    name_state
  )

df_mun_geo <- df_censo1872 %>%
  mutate(code_muni_1872 = as.character(muni_malha_id)) %>%
  left_join(mun_lookup_1872, by = "code_muni_1872")

unmatched_munis <- df_mun_geo %>%
  filter(is.na(code_state)) %>%
  select(code_muni_1872, municipio)

cat("Unmatched municipalities (dropped from panels):\n")
print(unmatched_munis)

df_mun_geo <- df_mun_geo %>%
  filter(!is.na(code_state)) %>%
  filter(rowSums(select(., starts_with("soma_g_")), na.rm = TRUE) > 0)

cat("Municipalities after filtering:", nrow(df_mun_geo), "\n")

# ============================================================
# 4. Build municipal panels
# ============================================================

mun_id_vars <- c(
  "code_muni_1872",
  "municipio",
  "name_muni_map",
  "code_state",
  "abbrev_state",
  "name_state"
)

mun_occupation_panel <- build_occupation_panel(df_mun_geo, id_vars = mun_id_vars)
mun_sector_panel <- build_sector_panel(df_mun_geo, id_vars = mun_id_vars)

# ============================================================
# 5. Aggregate to states and build state panels
# ============================================================

census_count_cols <- names(df_mun_geo)[
  stringr::str_detect(
    norm_name(names(df_mun_geo)),
    "^(h_livres|m_livres|f_livres|h_escr|h_escrv|m_escr|m_escrv|soma_l|soma_e|soma_escr|soma_escrv|soma_g)_"
  )
]

state_id_vars <- c("code_state", "abbrev_state", "name_state")

df_state_counts <- df_mun_geo %>%
  filter(!is.na(code_state)) %>%
  group_by(code_state, abbrev_state, name_state) %>%
  summarise(
    across(all_of(census_count_cols), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop"
  )

state_occupation_panel <- build_occupation_panel(df_state_counts, id_vars = state_id_vars)
state_sector_panel <- build_sector_panel(df_state_counts, id_vars = state_id_vars)

# ============================================================
# 6. Wide versions
# ============================================================

mun_occupation_wide <- make_wide(mun_occupation_panel, id_vars = mun_id_vars)
mun_sector_wide <- make_wide(mun_sector_panel, id_vars = mun_id_vars)
state_occupation_wide <- make_wide(state_occupation_panel, id_vars = state_id_vars)
state_sector_wide <- make_wide(state_sector_panel, id_vars = state_id_vars)

# ============================================================
# 7. Dashboard-ready long panel
# ============================================================

dashboard_panel <- bind_rows(
  mun_occupation_panel %>%
    mutate(
      geography_level = "Municipality",
      geography_id = code_muni_1872,
      geography_name = coalesce(municipio, name_muni_map)
    ),

  mun_sector_panel %>%
    mutate(
      geography_level = "Municipality",
      geography_id = code_muni_1872,
      geography_name = coalesce(municipio, name_muni_map)
    ),

  state_occupation_panel %>%
    mutate(
      geography_level = "State",
      geography_id = as.character(code_state),
      geography_name = name_state
    ),

  state_sector_panel %>%
    mutate(
      geography_level = "State",
      geography_id = as.character(code_state),
      geography_name = name_state
    )
) %>%
  select(
    geography_level,
    geography_id,
    geography_name,
    group_type,
    group_id,
    group_label,
    sector,
    n_total,
    n_free,
    n_enslaved,
    n_male,
    n_female,
    pop_total,
    pop_free,
    pop_enslaved,
    pop_literate,
    literacy_rate_population,
    enslaved_rate_population,
    occ_total,
    occ_free,
    occ_enslaved,
    occ_male,
    occ_female,
    share_of_all_occupations,
    share_free_within_group,
    share_enslaved_within_group,
    share_male_within_group,
    share_female_within_group,
    n_literate_within_group,
    share_literate_within_group
  ) %>%
  mutate(across(
    c(share_of_all_occupations, share_free_within_group, share_enslaved_within_group,
      share_male_within_group, share_female_within_group, literacy_rate_population,
      enslaved_rate_population),
    ~ replace_na(.x, 0)
  ))

# ============================================================
# 8. National totals and share of national
# ============================================================

national_denoms <- dashboard_panel %>%
  filter(geography_level == "Municipality") %>%
  group_by(group_type, group_id) %>%
  summarise(
    n_total_national = sum(n_total, na.rm = TRUE),
    n_free_national = sum(n_free, na.rm = TRUE),
    n_enslaved_national = sum(n_enslaved, na.rm = TRUE),
    n_male_national = sum(n_male, na.rm = TRUE),
    n_female_national = sum(n_female, na.rm = TRUE),
    pop_literate_national = sum(pop_literate, na.rm = TRUE),
    pop_enslaved_national = sum(pop_enslaved, na.rm = TRUE),
    .groups = "drop"
  )

dashboard_panel <- dashboard_panel %>%
  left_join(national_denoms, by = c("group_type", "group_id")) %>%
  mutate(
    share_of_national_occupations = replace_na(n_total / n_total_national, 0),
    share_free_of_national_group = replace_na(n_free / n_free_national, 0),
    share_enslaved_of_national_group = replace_na(n_enslaved / n_enslaved_national, 0),
    share_male_of_national_group = replace_na(n_male / n_male_national, 0),
    share_female_of_national_group = replace_na(n_female / n_female_national, 0),
    literacy_rate_of_national_pop = replace_na(pop_literate / pop_literate_national, 0),
    enslaved_rate_of_national_pop = replace_na(pop_enslaved / pop_enslaved_national, 0)
  )

# ============================================================
# 8. Save outputs
# ============================================================

dir.create("build", showWarnings = FALSE)

saveRDS(dashboard_panel, "build/dashboard_panel.rds")
saveRDS(mun_sf_1872, "build/mun_sf_1872.rds")
saveRDS(state_sf_1872, "build/state_sf_1872.rds")

saveRDS(mun_occupation_wide, "build/mun_occupation_wide.rds")
saveRDS(mun_sector_wide, "build/mun_sector_wide.rds")
saveRDS(state_occupation_wide, "build/state_occupation_wide.rds")
saveRDS(state_sector_wide, "build/state_sector_wide.rds")

cat("All outputs saved to build/\n")
