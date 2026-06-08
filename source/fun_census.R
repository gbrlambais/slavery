pacman::p_load(
  dplyr,
  stringr,
  tidyr,
  stringi,
  purrr,
  scales,
  DBI,
  RSQLite
)

# ============================================================
# Data loading
# ============================================================

fun_load_db_table <- function(db_path, path = "/Users/guilherme/Dropbox/", table_name, year = NULL) {
  db_path <- paste0(path, db_path)
  if (!is.null(year)) {
    table_name <- paste0(table_name, year)
  }
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  data <- dplyr::tbl(con, table_name) %>% dplyr::collect()
  DBI::dbDisconnect(con)
  return(data)
}

# ============================================================
# Helper functions
# ============================================================

safe_div <- function(num, den) {
  if_else(!is.na(den) & den > 0, num / den, NA_real_)
}

norm_name <- function(x) {
  x %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("[^a-z0-9_]", "")
}

cols_match <- function(.data, prefixes, patterns) {
  nm <- names(.data)
  nm_norm <- norm_name(nm)

  prefixes_norm <- norm_name(prefixes)
  patterns_norm <- norm_name(patterns)

  prefix_re <- paste0("^(", paste(prefixes_norm, collapse = "|"), ")_")
  pattern_re <- paste0("(", paste(patterns_norm, collapse = "|"), ")")

  nm[
    stringr::str_detect(nm_norm, prefix_re) &
      stringr::str_detect(nm_norm, pattern_re)
  ]
}

sum_match <- function(.data, prefixes, patterns) {
  cols <- cols_match(.data, prefixes = prefixes, patterns = patterns)

  if (length(cols) == 0) {
    return(rep(0, nrow(.data)))
  }

  rowSums(dplyr::select(.data, dplyr::all_of(cols)), na.rm = TRUE)
}

sum_exact <- function(.data, cols) {
  existing <- intersect(cols, names(.data))

  if (length(existing) == 0) {
    return(rep(0, nrow(.data)))
  }

  rowSums(dplyr::select(.data, dplyr::all_of(existing)), na.rm = TRUE)
}

fmt_pct <- function(x) {
  if_else(is.na(x), "No data", scales::percent(x, accuracy = 0.1))
}

fmt_num <- function(x) {
  if_else(is.na(x), "No data", scales::comma(x, accuracy = 1))
}

# ============================================================
# Column prefix constants
# ============================================================

prefix_total        <- "soma_g"
prefix_free         <- "soma_l"
prefix_slave        <- c("soma_e", "soma_escr", "soma_escrv")
prefix_male_free    <- "h_livres"
prefix_female_free  <- c("m_livres", "f_livres")
prefix_male_slave   <- c("h_escr", "h_escrv")
prefix_female_slave <- c("m_escr", "m_escrv")

# ============================================================
# Occupation and sector dictionaries
# ============================================================

occupation_key <- tribble(
  ~occ_id, ~occupation, ~sector, ~patterns,

  "secular_religious",       "Seculares religiosos",                      "services",       list("secularesreligiosos"),
  "regular_religious_men",   "Homens religiosos regulares",               "services",       list("homensreligregular"),
  "regular_religious_women", "Mulheres religiosas regulares",             "services",       list("mulheresreligregular"),
  "judges",                  "Juizes",                                    "services",       list("juizes"),
  "lawyers",                 "Advogados",                                 "services",       list("advogados"),
  "notaries_scribes",        "Notarios e escrivaes",                      "services",       list("notarioseescrivaes"),
  "procurators",             "Procuradores",                              "services",       list("procuradores"),
  "justice_officers",        "Oficiais de justica",                       "services",       list("oficiaisdejustica"),
  "physicians",              "Medicos",                                   "services",       list("medicos"),
  "surgeons",                "Cirurgioes",                                "services",       list("cirurgioes"),
  "pharmacists",             "Farmaceuticos",                             "services",       list("farmaceuticos"),
  "midwives",                "Parteiros",                                 "services",       list("parteiros"),
  "teachers_letters",        "Professores e homens de letras",            "services",       list("professoresehomensdelet"),
  "public_employees",        "Empregados publicos",                       "services",       list("empregadospublicos"),
  "artists",                 "Artistas",                                  "services",       list("artistas"),
  "military",                "Militares",                                 "services",       list("militares"),
  "maritime",                "Maritimos",                                 "services",       list("maritimos"),
  "fishermen",               "Pescadores",                                "agriculture",    list("pescadores"),
  "capitalists_owners",      "Capitalistas e proprietarios",              "services",       list("capitalistasepropriet"),
  "manufacturers",           "Manufatureiros e fabricantes",              "manufacturing",  list("manufatureirosefabrican"),
  "merchants_bookkeepers",   "Comerciantes, guarda-livros e caixeiros",   "services",       list("comerciantesguardalivro"),
  "seamstresses",            "Costureiras",                               "services",       list("costureiras"),
  "stonemasons_miners",      "Canteiros, calceteiros e mineiros",         "agriculture",    list("canteiroscalcoteirosmin"),
  "metals",                  "Em metais",                                 "manufacturing",  list("emmetais"),
  "wood",                    "Em madeiras",                               "manufacturing",  list("emmadeiras"),
  "textiles",                "Em tecidos",                                "manufacturing",  list("emtecidos"),
  "buildings",               "De edificacoes",                            "manufacturing",  list("deedificac"),
  "leather_skins",           "Em couros e peles",                         "manufacturing",  list("emcourosepeles"),
  "dyeing",                  "Em tinturaria",                             "manufacturing",  list("emtinturaria"),
  "clothing",                "De vestuarios",                             "manufacturing",  list("devestuarios"),
  "hats",                    "De chapeus",                                "manufacturing",  list("dechapeus"),
  "footwear",                "De calcado",                                "manufacturing",  list("decalcado"),
  "farmers",                 "Lavradores",                                "agriculture",    list("lavradores"),
  "stockbreeders",           "Criadores",                                 "agriculture",    list("criadores"),
  "servants_day_laborers",   "Criados e jornaleiros",                     "services",       list("criadosejornaleiros"),
  "domestic_service",        "Servico domestico",                         "services",       list("servicodomestico"),
  "other_occupations",       "Outras ocupacoes",                          "services",       list("outrasocupacoes"),
  "without_profession",      "Sem profissao",                             "services",       list("semprofissao")
)

broad_occ_patterns <- c(
  "profliberais",
  "outrasocupacoes",
  "profindustriaisecomerci",
  "profmanuaisemec",
  "profagric",
  "semprofissao"
)

manufacturing_patterns <- c(
  "manufatureirosefabrican",
  "emmetais",
  "emmadeiras",
  "emtecidos",
  "deedificac",
  "emcourosepeles",
  "emtinturaria",
  "devestuarios",
  "dechapeus",
  "decalcado"
)

agriculture_patterns <- c(
  "lavradores",
  "criadores",
  "canteiroscalcoteirosmin",
  "pescadores"
)

# ============================================================
# Denominators and contextual literacy rates
# ============================================================

working_occ_patterns <- setdiff(broad_occ_patterns, "semprofissao")

add_denominators <- function(.data) {
  .data %>%
    mutate(
      pop_total = sum_exact(., "soma_g_almas"),
      pop_free = sum_exact(., "soma_l_almas"),
      pop_enslaved = sum_exact(., c("soma_e_almas", "soma_escr_almas", "soma_escrv_almas")),

      pop_literate = sum_match(., prefix_total, "sabemlereescrever"),
      literacy_rate_population = safe_div(pop_literate, pop_total),
      enslaved_rate_population = safe_div(pop_enslaved, pop_total),

      occ_total = sum_match(., prefix_total, working_occ_patterns),
      occ_free = sum_match(., prefix_free, working_occ_patterns),
      occ_enslaved = sum_match(., prefix_slave, working_occ_patterns),

      occ_male =
        sum_match(., prefix_male_free, working_occ_patterns) +
        sum_match(., prefix_male_slave, working_occ_patterns),

      occ_female =
        sum_match(., prefix_female_free, working_occ_patterns) +
        sum_match(., prefix_female_slave, working_occ_patterns)
    )
}

# ============================================================
# Build occupation panel
# ============================================================

build_occupation_panel <- function(.data, id_vars) {
  base <- .data %>%
    add_denominators() %>%
    mutate(.row_id = row_number())

  context <- base %>%
    select(
      .row_id,
      all_of(id_vars),
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
      occ_female
    )

  purrr::pmap_dfr(
    occupation_key,
    function(occ_id, occupation, sector, patterns) {
      tibble(
        .row_id = base$.row_id,
        group_type = "Occupation",
        group_id = occ_id,
        group_label = occupation,
        sector = sector,

        n_total = sum_match(base, prefix_total, patterns),
        n_free = sum_match(base, prefix_free, patterns),
        n_enslaved = sum_match(base, prefix_slave, patterns),

        n_male_free = sum_match(base, prefix_male_free, patterns),
        n_female_free = sum_match(base, prefix_female_free, patterns),
        n_male_enslaved = sum_match(base, prefix_male_slave, patterns),
        n_female_enslaved = sum_match(base, prefix_female_slave, patterns)
      )
    }
  ) %>%
    mutate(
      n_male = n_male_free + n_male_enslaved,
      n_female = n_female_free + n_female_enslaved,

      n_literate_within_group = NA_real_,
      share_literate_within_group = NA_real_
    ) %>%
    left_join(context, by = ".row_id") %>%
    mutate(
      share_of_all_occupations = safe_div(n_total, occ_total),
      share_free_within_group = safe_div(n_free, n_total),
      share_enslaved_within_group = safe_div(n_enslaved, n_total),
      share_male_within_group = safe_div(n_male, n_total),
      share_female_within_group = safe_div(n_female, n_total)
    ) %>%
    select(-.row_id)
}

# ============================================================
# Build sector panel
# ============================================================

build_sector_panel <- function(.data, id_vars) {
  base <- .data %>%
    add_denominators() %>%
    mutate(.row_id = row_number())

  context <- base %>%
    select(
      .row_id,
      all_of(id_vars),
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
      occ_female
    )

  ag <- tibble(
    .row_id = base$.row_id,
    group_type = "Sector",
    group_id = "agriculture",
    group_label = "Agriculture",
    sector = "agriculture",

    n_total = sum_match(base, prefix_total, agriculture_patterns),
    n_free = sum_match(base, prefix_free, agriculture_patterns),
    n_enslaved = sum_match(base, prefix_slave, agriculture_patterns),

    n_male_free = sum_match(base, prefix_male_free, agriculture_patterns),
    n_female_free = sum_match(base, prefix_female_free, agriculture_patterns),
    n_male_enslaved = sum_match(base, prefix_male_slave, agriculture_patterns),
    n_female_enslaved = sum_match(base, prefix_female_slave, agriculture_patterns)
  )

  mf <- tibble(
    .row_id = base$.row_id,
    group_type = "Sector",
    group_id = "manufacturing",
    group_label = "Manufacturing",
    sector = "manufacturing",

    n_total = sum_match(base, prefix_total, manufacturing_patterns),
    n_free = sum_match(base, prefix_free, manufacturing_patterns),
    n_enslaved = sum_match(base, prefix_slave, manufacturing_patterns),

    n_male_free = sum_match(base, prefix_male_free, manufacturing_patterns),
    n_female_free = sum_match(base, prefix_female_free, manufacturing_patterns),
    n_male_enslaved = sum_match(base, prefix_male_slave, manufacturing_patterns),
    n_female_enslaved = sum_match(base, prefix_female_slave, manufacturing_patterns)
  )

  sv <- tibble(
    .row_id = base$.row_id,
    group_type = "Sector",
    group_id = "services",
    group_label = "Services",
    sector = "services",

    n_total = pmax(base$occ_total - ag$n_total - mf$n_total, 0),
    n_free = pmax(base$occ_free - ag$n_free - mf$n_free, 0),
    n_enslaved = pmax(base$occ_enslaved - ag$n_enslaved - mf$n_enslaved, 0),

    n_male_free = pmax(
      sum_match(base, prefix_male_free, broad_occ_patterns) -
        ag$n_male_free - mf$n_male_free,
      0
    ),

    n_female_free = pmax(
      sum_match(base, prefix_female_free, broad_occ_patterns) -
        ag$n_female_free - mf$n_female_free,
      0
    ),

    n_male_enslaved = pmax(
      sum_match(base, prefix_male_slave, broad_occ_patterns) -
        ag$n_male_enslaved - mf$n_male_enslaved,
      0
    ),

    n_female_enslaved = pmax(
      sum_match(base, prefix_female_slave, broad_occ_patterns) -
        ag$n_female_enslaved - mf$n_female_enslaved,
      0
    )
  )

  bind_rows(ag, mf, sv) %>%
    mutate(
      n_male = n_male_free + n_male_enslaved,
      n_female = n_female_free + n_female_enslaved,

      n_literate_within_group = NA_real_,
      share_literate_within_group = NA_real_
    ) %>%
    left_join(context, by = ".row_id") %>%
    mutate(
      share_of_all_occupations = safe_div(n_total, occ_total),
      share_free_within_group = safe_div(n_free, n_total),
      share_enslaved_within_group = safe_div(n_enslaved, n_total),
      share_male_within_group = safe_div(n_male, n_total),
      share_female_within_group = safe_div(n_female, n_total)
    ) %>%
    select(-.row_id)
}

# ============================================================
# Wide panel (one row per geography)
# ============================================================

make_wide <- function(panel, id_vars) {
  panel %>%
    select(
      all_of(id_vars),
      group_id,
      literacy_rate_population,
      n_total,
      share_of_all_occupations,
      share_free_within_group,
      share_enslaved_within_group,
      share_male_within_group,
      share_female_within_group
    ) %>%
    pivot_wider(
      id_cols = c(all_of(id_vars), literacy_rate_population),
      names_from = group_id,
      values_from = c(
        n_total,
        share_of_all_occupations,
        share_free_within_group,
        share_enslaved_within_group,
        share_male_within_group,
        share_female_within_group
      ),
      names_glue = "{.value}_{group_id}"
    )
}
