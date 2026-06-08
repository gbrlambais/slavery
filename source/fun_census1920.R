# ============================================================
# 1920 census sector classification and panel building
# Depends on: fun_census.R (safe_div, sum_exact, fmt_pct, fmt_num)
# ============================================================

agriculture_cols_1920 <- c(
  paste0("peaset", 1:5, "h"),
  paste0("peaset", 1:5, "m")
)

manufacturing_cols_1920 <- c(
  paste0("peaset", 6:19, "h"),
  paste0("peaset", 6:19, "m")
)

transport_cols_1920 <- c(
  paste0("peaset", 20:22, "h"),
  paste0("peaset", 20:22, "m")
)

teachers_cols_1920 <- c("peaset41h", "peaset41m")

# ============================================================
# Denominators for 1920
# ============================================================

add_1920_denominators <- function(.data) {
  .data %>%
    mutate(
      pop_total = popmun20,
      occ_total = pia20,
      occ_male = peasetth,
      occ_female = peasettm,
      pop_literate = popalf20,
      literacy_rate_population = safe_div(pop_literate, pop_total)
    )
}

# ============================================================
# Build 1920 sector panel (long format, one row per geo x sector)
# ============================================================

build_1920_sector_panel <- function(.data, id_vars) {
  base <- .data %>%
    add_1920_denominators() %>%
    mutate(.row_id = row_number())

  context <- base %>%
    select(
      .row_id,
      all_of(id_vars),
      pop_total,
      pop_literate,
      literacy_rate_population,
      occ_total,
      occ_male,
      occ_female
    )

  ag <- tibble(
    .row_id = base$.row_id,
    group_type = "Sector",
    group_id = "agriculture",
    group_label = "Agriculture",
    sector = "agriculture",
    n_total = sum_exact(base, agriculture_cols_1920),
    n_male = sum_exact(base, paste0("peaset", 1:5, "h")),
    n_female = sum_exact(base, paste0("peaset", 1:5, "m"))
  )

  mf <- tibble(
    .row_id = base$.row_id,
    group_type = "Sector",
    group_id = "manufacturing",
    group_label = "Manufacturing",
    sector = "manufacturing",
    n_total = sum_exact(base, manufacturing_cols_1920),
    n_male = sum_exact(base, paste0("peaset", 6:19, "h")),
    n_female = sum_exact(base, paste0("peaset", 6:19, "m"))
  )

  sv <- tibble(
    .row_id = base$.row_id,
    group_type = "Sector",
    group_id = "services",
    group_label = "Services",
    sector = "services",
    n_total = pmax(base$occ_total - ag$n_total - mf$n_total, 0),
    n_male = pmax(base$occ_male - ag$n_male - mf$n_male, 0),
    n_female = pmax(base$occ_female - ag$n_female - mf$n_female, 0)
  )

  bind_rows(ag, mf, sv) %>%
    mutate(
      n_free = NA_real_,
      n_enslaved = NA_real_,
      pop_free = NA_real_,
      pop_enslaved = NA_real_,
      enslaved_rate_population = NA_real_,
      n_literate_within_group = NA_real_,
      share_literate_within_group = NA_real_
    ) %>%
    left_join(context, by = ".row_id") %>%
    mutate(
      occ_free = NA_real_,
      occ_enslaved = NA_real_,
      share_of_all_occupations = safe_div(n_total, occ_total),
      share_free_within_group = NA_real_,
      share_enslaved_within_group = NA_real_,
      share_male_within_group = safe_div(n_male, n_total),
      share_female_within_group = safe_div(n_female, n_total)
    ) %>%
    select(-.row_id)
}

# ============================================================
# Wide panel for 1920 (one row per geography)
# ============================================================

make_wide_1920 <- function(panel, id_vars) {
  panel %>%
    select(
      all_of(id_vars),
      group_id,
      literacy_rate_population,
      n_total,
      share_of_all_occupations,
      share_male_within_group,
      share_female_within_group
    ) %>%
    pivot_wider(
      id_cols = c(all_of(id_vars), literacy_rate_population),
      names_from = group_id,
      values_from = c(
        n_total,
        share_of_all_occupations,
        share_male_within_group,
        share_female_within_group
      ),
      names_glue = "{.value}_{group_id}"
    )
}
