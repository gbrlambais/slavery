# ============================================================
# AMC (Minimum Comparable Areas) crosswalk utilities
# Uses geobr v1.9.1 read_comparable_areas(); caches to RDS
# ============================================================

amc_cache_path <- "build/amc_crosswalk.rds"
amc_sf_cache_path <- "build/amc_sf.rds"

load_amc_crosswalk <- function() {
  if (file.exists(amc_cache_path)) {
    cat("Loading cached AMC crosswalk from", amc_cache_path, "\n")
    return(readRDS(amc_cache_path))
  }

  cat("Downloading AMC crosswalk from geobr (requires v1.9.1)...\n")
  amc_raw <- geobr::read_comparable_areas(
    start_year = 1872,
    end_year = 2010,
    simplified = TRUE
  )

  amc_sf <- amc_raw %>%
    select(code_amc) %>%
    sf::st_make_valid() %>%
    sf::st_transform("EPSG:4326")

  crosswalk <- amc_raw %>%
    sf::st_drop_geometry() %>%
    select(code_amc, list_code_muni_2010) %>%
    mutate(muni_code = strsplit(as.character(list_code_muni_2010), ",")) %>%
    tidyr::unnest(muni_code) %>%
    mutate(muni_code = trimws(muni_code)) %>%
    select(code_amc, muni_code)

  dir.create("build", showWarnings = FALSE)
  saveRDS(crosswalk, amc_cache_path)
  saveRDS(amc_sf, amc_sf_cache_path)
  cat("Cached AMC crosswalk (", nrow(crosswalk), " mappings) and sf (",
      nrow(amc_sf), " polygons)\n")

  crosswalk
}

load_amc_sf <- function() {
  if (file.exists(amc_sf_cache_path)) {
    return(readRDS(amc_sf_cache_path))
  }
  load_amc_crosswalk()
  readRDS(amc_sf_cache_path)
}

aggregate_to_amc <- function(df, crosswalk, count_cols) {
  df %>%
    mutate(muni_code = as.character(muni_malha_id)) %>%
    inner_join(crosswalk, by = "muni_code") %>%
    group_by(code_amc) %>%
    summarise(
      across(all_of(count_cols), ~ sum(.x, na.rm = TRUE)),
      .groups = "drop"
    )
}
