pacman::p_load(
  dplyr,
  stringr,
  tidyr,
  readr,
  sf,
  geobr
)
source("source/fun_census.R")
source("source/fun_census1920.R")

# ============================================================
# 1. Load 1920 census data
# ============================================================

df_censo1920 <- fun_load_db_table("brazil/release/brazil_muni.db", table_name = "census", year = 1920)

# ============================================================
# 2. Load 1920 geobr shapefiles
# ============================================================

mun_sf_1920 <- geobr::read_municipality(
  code_muni = "all",
  year = 1920,
  simplified = TRUE,
  showProgress = FALSE
) %>%
  st_transform("EPSG:4326") %>%
  mutate(
    code_muni_1920 = as.character(code_muni),
    code_state = as.integer(code_state)
  )

state_name_code_1920 <- tribble(
  ~name_state,             ~code_state, ~abbrev_state,
  "Território do Acre",    12L, "AC",
  "Amazonas",              13L, "AM",
  "Pará",                  15L, "PA",
  "Maranhão",              21L, "MA",
  "Piauhy",                22L, "PI",
  "Ceará",                 23L, "CE",
  "Rio Grande do Norte",   24L, "RN",
  "Parahyba do Norte",     25L, "PB",
  "Pernambuco",            26L, "PE",
  "Alagôas",               27L, "AL",
  "Sergipe",               28L, "SE",
  "Bahia",                 29L, "BA",
  "Districto Federal",     30L, "DF",
  "Minas Geraes",          31L, "MG",
  "Espirito Santo",        32L, "ES",
  "Rio de Janeiro",        33L, "RJ",
  "São Paulo",             35L, "SP",
  "Paraná",                41L, "PR",
  "Santa Catharina",       42L, "SC",
  "Rio Grande do Sul",     43L, "RS",
  "Matto Grosso",          51L, "MT",
  "Goyaz",                 52L, "GO"
)

state_sf_1920 <- geobr::read_state(
  year = 1920,
  simplified = TRUE,
  showProgress = FALSE
) %>%
  st_transform("EPSG:4326") %>%
  left_join(state_name_code_1920, by = "name_state") %>%
  mutate(code_state_chr = as.character(code_state))

# ============================================================
# 3. Join census to geography
# ============================================================

state_name_lookup_1920 <- state_sf_1920 %>%
  st_drop_geometry() %>%
  select(code_state, name_state)

mun_lookup_1920 <- mun_sf_1920 %>%
  st_drop_geometry() %>%
  transmute(
    code_muni_1920,
    name_muni_map = name_muni,
    code_state,
    abbrev_state
  ) %>%
  left_join(state_name_lookup_1920, by = "code_state")

df_mun_geo <- df_censo1920 %>%
  mutate(code_muni_1920 = as.character(muni_malha_id)) %>%
  left_join(mun_lookup_1920, by = "code_muni_1920")

unmatched_munis <- df_mun_geo %>%
  filter(is.na(code_state)) %>%
  select(code_muni_1920, nommun20)

cat("1920 unmatched municipalities (dropped from panels):\n")
print(unmatched_munis)

df_mun_geo <- df_mun_geo %>%
  filter(!is.na(code_state)) %>%
  filter(peasettt > 0 | popmun20 > 0)

cat("1920 municipalities after filtering:", nrow(df_mun_geo), "\n")

# ============================================================
# 4. Build municipal panels
# ============================================================

mun_id_vars <- c(
  "code_muni_1920",
  "nommun20",
  "name_muni_map",
  "code_state",
  "abbrev_state",
  "name_state"
)

mun_sector_panel <- build_1920_sector_panel(df_mun_geo, id_vars = mun_id_vars)

# ============================================================
# 5. Aggregate to states and build state panels
# ============================================================

census_count_cols_1920 <- names(df_mun_geo)[
  stringr::str_detect(names(df_mun_geo), "^(peaset|popmun20|popalf20|piaalf20|pibhal20|piehal20|pia20)")
]

state_id_vars <- c("code_state", "abbrev_state", "name_state")

df_state_counts <- df_mun_geo %>%
  filter(!is.na(code_state)) %>%
  group_by(code_state, abbrev_state, name_state) %>%
  summarise(
    across(all_of(census_count_cols_1920), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop"
  )

state_sector_panel <- build_1920_sector_panel(df_state_counts, id_vars = state_id_vars)

# ============================================================
# 6. Dashboard-ready long panel
# ============================================================

dashboard_panel_1920 <- bind_rows(
  mun_sector_panel %>%
    mutate(
      geography_level = "Municipality",
      geography_id = code_muni_1920,
      geography_name = coalesce(nommun20, name_muni_map)
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
    c(share_of_all_occupations, share_male_within_group, share_female_within_group,
      literacy_rate_population),
    ~ replace_na(.x, 0)
  ))

# ============================================================
# 7. National totals
# ============================================================

national_denoms <- dashboard_panel_1920 %>%
  filter(geography_level == "Municipality") %>%
  group_by(group_type, group_id) %>%
  summarise(
    n_total_national = sum(n_total, na.rm = TRUE),
    n_male_national = sum(n_male, na.rm = TRUE),
    n_female_national = sum(n_female, na.rm = TRUE),
    pop_literate_national = sum(pop_literate, na.rm = TRUE),
    .groups = "drop"
  )

dashboard_panel_1920 <- dashboard_panel_1920 %>%
  left_join(national_denoms, by = c("group_type", "group_id")) %>%
  mutate(
    share_of_national_occupations = replace_na(n_total / n_total_national, 0),
    share_male_of_national_group = replace_na(n_male / n_male_national, 0),
    share_female_of_national_group = replace_na(n_female / n_female_national, 0),
    literacy_rate_of_national_pop = replace_na(pop_literate / pop_literate_national, 0)
  )

# ============================================================
# 8. Save outputs
# ============================================================

dir.create("build", showWarnings = FALSE)

saveRDS(dashboard_panel_1920, "build/dashboard_panel_1920.rds")
saveRDS(mun_sf_1920, "build/mun_sf_1920.rds")
saveRDS(state_sf_1920, "build/state_sf_1920.rds")

mun_sector_wide_1920 <- make_wide_1920(mun_sector_panel, id_vars = mun_id_vars)
state_sector_wide_1920 <- make_wide_1920(state_sector_panel, id_vars = state_id_vars)
saveRDS(mun_sector_wide_1920, "build/mun_sector_wide_1920.rds")
saveRDS(state_sector_wide_1920, "build/state_sector_wide_1920.rds")

cat("All 1920 outputs saved to build/\n")
