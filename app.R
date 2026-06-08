pacman::p_load(
  shiny,
  leaflet,
  sf,
  dplyr,
  scales,
  glue,
  htmltools,
  ggplot2
)
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
# Metric definitions per mode
# ============================================================

metric_defs_1872 <- tribble(
  ~label, ~col, ~num_col, ~den_col, ~num_label, ~den_label,
  "Share of occupations",                "share_of_all_occupations",          "n_total",      "occ_total",            "Total in group",           "Total occupations",
  "Free share within group",             "share_free_within_group",           "n_free",       "n_total",              "Free in group",            "Total in group",
  "Enslaved share within group",         "share_enslaved_within_group",       "n_enslaved",   "n_total",              "Enslaved in group",        "Total in group",
  "Male share within group",             "share_male_within_group",           "n_male",       "n_total",              "Male in group",            "Total in group",
  "Female share within group",           "share_female_within_group",         "n_female",     "n_total",              "Female in group",          "Total in group",
  "Population literacy rate",            "literacy_rate_population",          "pop_literate", "pop_total",            "Literate population",      "Total population",
  "Enslaved population rate",            "enslaved_rate_population",          "pop_enslaved", "pop_total",            "Enslaved population",      "Total population",
  "Share of national occupations",       "share_of_national_occupations",     "n_total",      "n_total_national",     "Total in group",           "National total in group",
  "Free share of national group",        "share_free_of_national_group",      "n_free",       "n_free_national",      "Free in group",            "National free in group",
  "Enslaved share of national group",    "share_enslaved_of_national_group",  "n_enslaved",   "n_enslaved_national",  "Enslaved in group",        "National enslaved in group",
  "Male share of national group",        "share_male_of_national_group",      "n_male",       "n_male_national",      "Male in group",            "National male in group",
  "Female share of national group",      "share_female_of_national_group",    "n_female",     "n_female_national",    "Female in group",          "National female in group",
  "Literacy rate of national population","literacy_rate_of_national_pop",     "pop_literate", "pop_literate_national", "Literate population",      "National literate population",
  "Enslaved rate of national population","enslaved_rate_of_national_pop",     "pop_enslaved", "pop_enslaved_national", "Enslaved population",      "National enslaved population"
)

metric_defs_1920 <- tribble(
  ~label, ~col, ~num_col, ~den_col, ~num_label, ~den_label,
  "Share of occupations",                "share_of_all_occupations",          "n_total",      "occ_total",            "Total in group",           "Total occupations",
  "Male share within group",             "share_male_within_group",           "n_male",       "n_total",              "Male in group",            "Total in group",
  "Female share within group",           "share_female_within_group",         "n_female",     "n_total",              "Female in group",          "Total in group",
  "Population literacy rate",            "literacy_rate_population",          "pop_literate", "pop_total",            "Literate population",      "Total population",
  "Share of national occupations",       "share_of_national_occupations",     "n_total",      "n_total_national",     "Total in group",           "National total in group",
  "Male share of national group",        "share_male_of_national_group",      "n_male",       "n_male_national",      "Male in group",            "National male in group",
  "Female share of national group",      "share_female_of_national_group",    "n_female",     "n_female_national",    "Female in group",          "National female in group",
  "Literacy rate of national population","literacy_rate_of_national_pop",     "pop_literate", "pop_literate_national", "Literate population",      "National literate population"
)

metric_defs_transformation <- tribble(
  ~label, ~col, ~is_pct, ~diverging,
  "Mfg share 1920 (level)",              "mfg_share_1920",             TRUE,  FALSE,
  "Agr share 1920 (level)",              "agr_share_1920",             TRUE,  FALSE,
  "Non-agr share 1920 (level)",          "non_agr_share_1920",         TRUE,  FALSE,
  "Mfg/agr ratio 1920",                  "mfg_agr_ratio_1920",         FALSE, FALSE,
  "Literacy rate 1920",                   "literacy_rate_1920",         TRUE,  FALSE,
  "Mfg per capita 1920",                  "mfg_percapita_1920",        FALSE, FALSE,
  "Mfg share change (pp)",               "mfg_share_change_pp",        TRUE,  TRUE,
  "Mfg share log ratio",                 "mfg_share_log_ratio",        FALSE, TRUE,
  "Mfg share CAGR",                      "mfg_share_cagr",             FALSE, TRUE,
  "Mfg share logit change",              "mfg_share_logit_change",     FALSE, TRUE,
  "Non-agr share change (pp)",           "non_agr_share_change_pp",    TRUE,  TRUE,
  "Mfg/agr ratio change",               "mfg_agr_ratio_change",       FALSE, TRUE,
  "Structural change index",             "structural_change_index",    FALSE, FALSE,
  "Agr exit share",                      "agr_exit_share",             TRUE,  TRUE,
  "Services share change (pp)",          "serv_share_change_pp",       TRUE,  TRUE,
  "Literacy rate change (pp)",           "literacy_rate_change_pp",    TRUE,  TRUE,
  "Pop growth rate (annual)",            "pop_growth_rate",            FALSE, FALSE,
  "Enslaved share 1872 (X)",            "enslaved_share_1872",         TRUE,  FALSE,
  "Enslaved mfg share 1872 (X)",        "enslaved_mfg_share_1872",    TRUE,  FALSE,
  "Enslaved agr share 1872 (X)",        "enslaved_agr_share_1872",    TRUE,  FALSE,
  "Mfg share 1872 (baseline)",          "mfg_share_1872",             TRUE,  FALSE,
  "Literacy rate 1872 (baseline)",       "literacy_rate_1872",         TRUE,  FALSE
)

metric_choices_1872 <- setNames(metric_defs_1872$col, metric_defs_1872$label)
metric_choices_1920 <- setNames(metric_defs_1920$col, metric_defs_1920$label)
metric_choices_transformation <- setNames(metric_defs_transformation$col, metric_defs_transformation$label)

regression_y_choices <- c(
  "Mfg share change (pp)" = "mfg_share_change_pp",
  "Mfg share log ratio" = "mfg_share_log_ratio",
  "Mfg share CAGR" = "mfg_share_cagr",
  "Mfg share logit change" = "mfg_share_logit_change",
  "Non-agr share change (pp)" = "non_agr_share_change_pp",
  "Mfg/agr ratio change" = "mfg_agr_ratio_change",
  "Structural change index" = "structural_change_index",
  "Agr exit share" = "agr_exit_share",
  "Services share change (pp)" = "serv_share_change_pp",
  "Literacy rate change (pp)" = "literacy_rate_change_pp",
  "Pop growth rate" = "pop_growth_rate"
)

# ============================================================
# UI
# ============================================================

ui <- fluidPage(
  titlePanel(textOutput("app_title")),

  sidebarLayout(
    sidebarPanel(
      radioButtons(
        inputId = "census_mode",
        label = "View",
        choices = c(
          "1872 Census" = "1872",
          "1920 Census" = "1920",
          "1872-1920 Transformation" = "transformation",
          "Regression" = "regression"
        ),
        selected = "1872"
      ),

      conditionalPanel(
        condition = "input.census_mode == '1872' || input.census_mode == '1920'",
        radioButtons(
          inputId = "geo_level",
          label = "Geographic level",
          choices = c("Municipality", "State"),
          selected = "Municipality"
        )
      ),

      conditionalPanel(
        condition = "input.census_mode == '1872'",
        radioButtons(
          inputId = "group_type",
          label = "Analysis type",
          choices = c("Occupation", "Sector"),
          selected = "Sector"
        ),
        uiOutput("group_picker_1872"),
        selectInput(
          inputId = "metric_1872",
          label = "Map metric",
          choices = metric_choices_1872,
          selected = "share_of_all_occupations"
        ),
        helpText(
          "Note: literacy and enslaved population rates are mapped as ",
          "population-level contextual rates because the census variables ",
          "do not cross-tab these by occupation or sector."
        )
      ),

      conditionalPanel(
        condition = "input.census_mode == '1920'",
        uiOutput("group_picker_1920"),
        selectInput(
          inputId = "metric_1920",
          label = "Map metric",
          choices = metric_choices_1920,
          selected = "share_of_all_occupations"
        )
      ),

      conditionalPanel(
        condition = "input.census_mode == 'transformation'",
        radioButtons(
          inputId = "geo_level_transformation",
          label = "Geographic level",
          choices = c("AMC", "State"),
          selected = "AMC"
        ),
        selectInput(
          inputId = "metric_transformation",
          label = "Map metric",
          choices = metric_choices_transformation,
          selected = "mfg_share_change_pp"
        ),
        helpText(
          "Sectors harmonized to 3 broad categories (agriculture, manufacturing, ",
          "services) across both censuses. AMC = Minimum Comparable Areas ",
          "(crosswalk for boundary changes). State aggregation needs no crosswalk."
        )
      ),

      conditionalPanel(
        condition = "input.census_mode == 'regression'",
        selectInput(
          inputId = "regression_y_metric",
          label = "Outcome metric (Y)",
          choices = regression_y_choices,
          selected = "mfg_share_change_pp"
        ),
        helpText(
          "X = enslaved share 1872. All regressions use AMC-level data. ",
          "Variables are standardized (z-scored). State regressions use ",
          "within-state standardization; national regressions use global ",
          "standardization."
        )
      )
    ),

    mainPanel(
      conditionalPanel(
        condition = "input.census_mode != 'regression'",
        leafletOutput("map", height = 720),
        br(),
        h4(textOutput("table_title")),
        tableOutput("top_table")
      ),
      conditionalPanel(
        condition = "input.census_mode == 'regression'",
        tabsetPanel(
          id = "regression_tabs",
          tabPanel("Coefficient Map",
            leafletOutput("regression_map", height = 720)
          ),
          tabPanel("State Regressions",
            plotOutput("regression_state_plot", height = 600),
            helpText(
              "Within-state standardized regressions at AMC level. ",
              "Error bars = 95% CI. States with < 5 AMCs excluded."
            )
          ),
          tabPanel("National Regressions",
            plotOutput("regression_national_pooled_plot", height = 400),
            br(),
            plotOutput("regression_national_fe_plot", height = 400),
            helpText(
              "Pooled regressions across all AMCs (n = 465). ",
              "Variables standardized globally. Error bars = 95% CI."
            )
          ),
          tabPanel("Scatter",
            plotOutput("regression_scatter_plot", height = 600)
          ),
          tabPanel("Quartile Comparison",
            plotOutput("quartile_plot", height = 500)
          )
        )
      )
    )
  )
)

# ============================================================
# Server
# ============================================================

server <- function(input, output, session) {

  output$app_title <- renderText({
    switch(input$census_mode,
      "1872" = "Brazil 1872 Census: occupations and sectors",
      "1920" = "Brazil 1920 Census: sectoral employment",
      "transformation" = "Brazil 1872-1920: structural transformation",
      "regression" = "Brazil 1872-1920: slavery and structural change regressions"
    )
  })

  output$table_title <- renderText({
    switch(input$census_mode,
      "1872" = "Top geographies for selected metric",
      "1920" = "Top geographies for selected metric",
      "transformation" = paste("Top units for selected metric -",
                               input$geo_level_transformation, "level")
    )
  })

  # --- 1872 group picker ---
  output$group_picker_1872 <- renderUI({
    groups <- dashboard_panel_1872 %>%
      filter(
        geography_level == input$geo_level,
        group_type == input$group_type
      ) %>%
      distinct(group_id, group_label) %>%
      arrange(group_label)

    selectInput(
      inputId = "group_id_1872",
      label = input$group_type,
      choices = setNames(groups$group_id, groups$group_label),
      selected = groups$group_id[[1]]
    )
  })

  # --- 1920 group picker ---
  output$group_picker_1920 <- renderUI({
    groups <- dashboard_panel_1920 %>%
      filter(geography_level == input$geo_level) %>%
      distinct(group_id, group_label) %>%
      arrange(group_label)

    selectInput(
      inputId = "group_id_1920",
      label = "Sector",
      choices = setNames(groups$group_id, groups$group_label),
      selected = groups$group_id[[1]]
    )
  })

  # --- Active metric definition ---
  cur_metric_def <- reactive({
    if (input$census_mode == "1872") {
      metric_defs_1872 %>% filter(col == input$metric_1872)
    } else if (input$census_mode == "1920") {
      metric_defs_1920 %>% filter(col == input$metric_1920)
    } else {
      NULL
    }
  })

  cur_trans_def <- reactive({
    req(input$census_mode == "transformation")
    metric_defs_transformation %>% filter(col == input$metric_transformation)
  })

  # --- Map data (1872/1920/transformation) ---
  map_data <- reactive({
    req(input$census_mode != "regression")

    if (input$census_mode == "1872") {
      req(input$geo_level, input$group_type, input$group_id_1872, input$metric_1872)
      mdef <- cur_metric_def()

      panel <- dashboard_panel_1872 %>%
        filter(
          geography_level == input$geo_level,
          group_type == input$group_type,
          group_id == input$group_id_1872
        )

      sf_obj <- if (input$geo_level == "Municipality") {
        mun_sf_1872 %>%
          inner_join(panel, by = c("code_muni_1872" = "geography_id"))
      } else {
        state_sf_1872 %>%
          inner_join(panel, by = c("code_state_chr" = "geography_id"))
      }

      sf_obj %>%
        mutate(
          map_value = .data[[input$metric_1872]],
          numerator = .data[[mdef$num_col]],
          denominator = .data[[mdef$den_col]],
          metric_label = mdef$label,
          num_label = mdef$num_label,
          den_label = mdef$den_label,
          is_diverging = FALSE,
          is_pct = TRUE
        )

    } else if (input$census_mode == "1920") {
      req(input$geo_level, input$group_id_1920, input$metric_1920)
      mdef <- cur_metric_def()

      panel <- dashboard_panel_1920 %>%
        filter(
          geography_level == input$geo_level,
          group_id == input$group_id_1920
        )

      sf_obj <- if (input$geo_level == "Municipality") {
        mun_sf_1920 %>%
          inner_join(panel, by = c("code_muni_1920" = "geography_id"))
      } else {
        state_sf_1920 %>%
          inner_join(panel, by = c("code_state_chr" = "geography_id"))
      }

      sf_obj %>%
        mutate(
          map_value = .data[[input$metric_1920]],
          numerator = .data[[mdef$num_col]],
          denominator = .data[[mdef$den_col]],
          metric_label = mdef$label,
          num_label = mdef$num_label,
          den_label = mdef$den_label,
          is_diverging = FALSE,
          is_pct = TRUE
        )

    } else {
      req(input$metric_transformation, input$geo_level_transformation)
      tdef <- cur_trans_def()

      if (input$geo_level_transformation == "State") {
        state_sf_1872 %>%
          inner_join(state_transformation_panel,
                     by = c("code_state_chr" = "state_code")) %>%
          mutate(
            map_value = .data[[input$metric_transformation]],
            metric_label = tdef$label,
            is_diverging = tdef$diverging,
            is_pct = tdef$is_pct,
            geography_name = name_state
          )
      } else {
        amc_sf %>%
          inner_join(transformation_panel, by = "code_amc") %>%
          mutate(
            map_value = .data[[input$metric_transformation]],
            metric_label = tdef$label,
            is_diverging = tdef$diverging,
            is_pct = tdef$is_pct
          )
      }
    }
  })

  # --- Leaflet map (1872/1920/transformation) ---
  output$map <- renderLeaflet({
    dat <- map_data()
    is_trans <- input$census_mode == "transformation"
    is_diverging <- if (is_trans) cur_trans_def()$diverging else FALSE
    is_pct <- if (is_trans) cur_trans_def()$is_pct else TRUE
    metric_label <- if (is_trans) cur_trans_def()$label else cur_metric_def()$label

    if (is_diverging) {
      max_abs <- max(abs(dat$map_value), na.rm = TRUE)
      pal <- colorNumeric(
        palette = "RdBu",
        domain = c(-max_abs, max_abs),
        na.color = "#eeeeee",
        reverse = TRUE
      )
    } else {
      pal <- colorNumeric(
        palette = "YlGnBu",
        domain = dat$map_value,
        na.color = "#eeeeee"
      )
    }

    if (is_trans) {
      fmt_val <- if (is_pct) function(x) fmt_pct(x) else function(x) as.character(round(x, 4))
      is_state_trans <- input$geo_level_transformation == "State"
      if (is_state_trans) {
        dat <- dat %>%
          mutate(
            popup_html = glue(
              "<b>{geography_name}</b><br/>",
              "<b>{metric_label}:</b> {fmt_val(map_value)}"
            )
          )
      } else {
        dat <- dat %>%
          mutate(
            popup_html = glue(
              "<b>AMC {code_amc}</b><br/>",
              "<b>{metric_label}:</b> {fmt_val(map_value)}"
            )
          )
      }
    } else {
      mdef <- cur_metric_def()
      dat <- dat %>%
        mutate(
          popup_html = glue(
            "<b>{geography_name}</b><br/>",
            "<b>{group_type}:</b> {group_label}<br/>",
            "<b>{metric_label}:</b> {fmt_pct(map_value)}<br/>",
            "<b>{mdef$num_label}:</b> {fmt_num(numerator)}<br/>",
            "<b>{mdef$den_label}:</b> {fmt_num(denominator)}"
          )
        )
    }

    map_obj <- leaflet(dat, options = leafletOptions(preferCanvas = TRUE)) %>%
      addPolygons(
        fillColor = ~ pal(map_value),
        fillOpacity = 0.75,
        color = "#444444",
        weight = 0.35,
        opacity = 0.9,
        label = ~ lapply(popup_html, htmltools::HTML),
        popup = ~ popup_html,
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#222222",
          fillOpacity = 0.9,
          bringToFront = TRUE
        )
      )

    if (is_pct) {
      map_obj %>%
        addLegend(
          position = "bottomright",
          pal = pal,
          values = ~ map_value,
          title = metric_label,
          labFormat = labelFormat(
            transform = function(x) 100 * x,
            suffix = "%"
          )
        )
    } else {
      map_obj %>%
        addLegend(
          position = "bottomright",
          pal = pal,
          values = ~ map_value,
          title = metric_label,
          labFormat = labelFormat(digits = 3)
        )
    }
  })

  # --- Top table (1872/1920/transformation) ---
  output$top_table <- renderTable({
    dat <- map_data()
    is_trans <- input$census_mode == "transformation"
    is_pct <- if (is_trans) cur_trans_def()$is_pct else TRUE

    if (is_trans) {
      fmt_val <- if (is_pct) fmt_pct else function(x) as.character(round(x, 4))
      is_state_trans <- input$geo_level_transformation == "State"
      tbl <- dat %>%
        st_drop_geometry() %>%
        filter(!is.na(map_value)) %>%
        arrange(desc(map_value)) %>%
        slice_head(n = 15)
      if (is_state_trans) {
        tbl <- tbl %>%
          transmute(State = geography_name, Metric = fmt_val(map_value))
      } else {
        tbl <- tbl %>%
          transmute(`AMC code` = as.character(code_amc), Metric = fmt_val(map_value))
      }
    } else {
      mdef <- cur_metric_def()
      tbl <- dat %>%
        st_drop_geometry() %>%
        arrange(desc(map_value)) %>%
        slice_head(n = 15) %>%
        transmute(
          Geography = geography_name,
          Metric = fmt_pct(map_value),
          !!mdef$num_label := fmt_num(numerator),
          !!mdef$den_label := fmt_num(denominator)
        )
    }

    tbl
  })

  # --- Regression: coefficient map ---
  output$regression_map <- renderLeaflet({
    req(input$census_mode == "regression", input$regression_y_metric)

    y_label <- names(regression_y_choices[regression_y_choices == input$regression_y_metric])

    plot_data <- regression_results %>%
      filter(y_metric == input$regression_y_metric)

    dat <- state_sf_1872 %>%
      inner_join(plot_data, by = c("code_state_chr" = "state_code"))

    max_abs <- max(abs(dat$coef), na.rm = TRUE)
    pal <- colorNumeric(
      palette = "RdBu",
      domain = c(-max_abs, max_abs),
      na.color = "#eeeeee",
      reverse = TRUE
    )

    dat <- dat %>%
      mutate(
        popup_html = glue(
          "<b>{state_name}</b> (n={n_amc})<br/>",
          "<b>Coefficient:</b> {round(coef, 3)}<br/>",
          "<b>95% CI:</b> [{round(ci_lower, 3)}, {round(ci_upper, 3)}]<br/>",
          "<b>p-value:</b> {round(p_value, 3)}"
        )
      )

    leaflet(dat, options = leafletOptions(preferCanvas = TRUE)) %>%
      addPolygons(
        fillColor = ~ pal(coef),
        fillOpacity = 0.75,
        color = "#444444",
        weight = 0.8,
        opacity = 0.9,
        label = ~ lapply(popup_html, htmltools::HTML),
        popup = ~ popup_html,
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#222222",
          fillOpacity = 0.9,
          bringToFront = TRUE
        )
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = ~ coef,
        title = paste("Std. coef:", y_label),
        labFormat = labelFormat(digits = 2)
      )
  })

  # --- Regression: state-level forest plot ---
  output$regression_state_plot <- renderPlot({
    req(input$census_mode == "regression", input$regression_y_metric)

    plot_data <- regression_results %>%
      filter(y_metric == input$regression_y_metric) %>%
      mutate(
        significant = p_value < 0.05,
        state_label = paste0(state_name, " (n=", n_amc, ")")
      ) %>%
      arrange(coef) %>%
      mutate(state_label = factor(state_label, levels = state_label))

    y_label <- names(regression_y_choices[regression_y_choices == input$regression_y_metric])

    ggplot(plot_data, aes(x = coef, y = state_label)) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
      geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper),
                     height = 0.2, color = "grey40") +
      geom_point(aes(color = significant), size = 3) +
      scale_color_manual(
        values = c("TRUE" = "#d62728", "FALSE" = "#7f7f7f"),
        labels = c("TRUE" = "p < 0.05", "FALSE" = "p >= 0.05"),
        name = "Significance"
      ) +
      labs(
        x = "Standardized coefficient (effect of enslaved share 1872)",
        y = NULL,
        title = paste0("Effect of 1872 enslaved share on: ", y_label),
        subtitle = "Within-state standardized regressions at AMC level"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        panel.grid.major.y = element_blank(),
        legend.position = "bottom"
      )
  })

  # --- Regression: national pooled (no FEs) ---
  output$regression_national_pooled_plot <- renderPlot({
    req(input$census_mode == "regression")

    plot_data <- regression_results_national %>%
      filter(model == "pooled") %>%
      mutate(
        significant = p_value < 0.05,
        y_label = names(regression_y_choices[match(y_metric, regression_y_choices)])
      ) %>%
      arrange(coef) %>%
      mutate(y_label = factor(y_label, levels = y_label))

    ggplot(plot_data, aes(x = coef, y = y_label)) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
      geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper),
                     height = 0.2, color = "grey40") +
      geom_point(aes(color = significant), size = 3) +
      scale_color_manual(
        values = c("TRUE" = "#d62728", "FALSE" = "#7f7f7f"),
        labels = c("TRUE" = "p < 0.05", "FALSE" = "p >= 0.05"),
        name = "Significance"
      ) +
      labs(
        x = "Standardized coefficient (effect of enslaved share 1872)",
        y = NULL,
        title = "Pooled OLS (no state fixed effects)",
        subtitle = paste("n =", plot_data$n_obs[1], "AMCs")
      ) +
      theme_minimal(base_size = 14) +
      theme(
        panel.grid.major.y = element_blank(),
        legend.position = "bottom"
      )
  })

  # --- Regression: national with state FEs ---
  output$regression_national_fe_plot <- renderPlot({
    req(input$census_mode == "regression")

    plot_data <- regression_results_national %>%
      filter(model == "state_fe") %>%
      mutate(
        significant = p_value < 0.05,
        y_label = names(regression_y_choices[match(y_metric, regression_y_choices)])
      ) %>%
      arrange(coef) %>%
      mutate(y_label = factor(y_label, levels = y_label))

    ggplot(plot_data, aes(x = coef, y = y_label)) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
      geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper),
                     height = 0.2, color = "grey40") +
      geom_point(aes(color = significant), size = 3) +
      scale_color_manual(
        values = c("TRUE" = "#d62728", "FALSE" = "#7f7f7f"),
        labels = c("TRUE" = "p < 0.05", "FALSE" = "p >= 0.05"),
        name = "Significance"
      ) +
      labs(
        x = "Standardized coefficient (effect of enslaved share 1872)",
        y = NULL,
        title = "With state fixed effects",
        subtitle = paste("n =", plot_data$n_obs[1], "AMCs")
      ) +
      theme_minimal(base_size = 14) +
      theme(
        panel.grid.major.y = element_blank(),
        legend.position = "bottom"
      )
  })
  # --- Regression: scatter plot ---
  output$regression_scatter_plot <- renderPlot({
    req(input$census_mode == "regression", input$regression_y_metric)

    y_label <- names(regression_y_choices[regression_y_choices == input$regression_y_metric])
    y_col <- input$regression_y_metric

    plot_data <- transformation_panel %>%
      filter(!is.na(enslaved_share_1872), !is.na(.data[[y_col]])) %>%
      left_join(
        regression_results %>%
          filter(y_metric == y_col) %>%
          select(state_code, state_name),
        by = "state_code"
      )

    ggplot(plot_data, aes(x = enslaved_share_1872, y = .data[[y_col]])) +
      geom_point(aes(color = state_name), alpha = 0.6, size = 1.5) +
      geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 0.8) +
      scale_x_continuous(labels = scales::percent_format()) +
      labs(
        x = "Enslaved share 1872",
        y = y_label,
        title = paste0("Enslaved share 1872 vs. ", y_label),
        subtitle = paste(nrow(plot_data), "AMCs"),
        color = "State"
      ) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "right")
  })

  # --- Regression: quartile comparison ---
  output$quartile_plot <- renderPlot({
    req(input$census_mode == "regression")

    plot_data <- transformation_panel %>%
      filter(!is.na(enslaved_share_1872),
             !is.na(mfg_share_change_pp),
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

    # 1872 baseline levels
    baseline_means <- plot_data %>%
      group_by(slavery_quartile) %>%
      summarise(
        Agriculture = mean(agr_share_1872, na.rm = TRUE),
        Manufacturing = mean(mfg_share_1872, na.rm = TRUE),
        Services = mean(serv_share_1872, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      tidyr::pivot_longer(
        cols = c(Agriculture, Manufacturing, Services),
        names_to = "Sector",
        values_to = "mean_share"
      ) %>%
      mutate(
        Sector = factor(Sector, levels = c("Agriculture", "Manufacturing", "Services")),
        panel = "1872 sector shares (baseline)"
      )

    # 1872-1920 changes
    change_means <- plot_data %>%
      group_by(slavery_quartile) %>%
      summarise(
        Agriculture = mean(agr_share_change_pp, na.rm = TRUE),
        Manufacturing = mean(mfg_share_change_pp, na.rm = TRUE),
        Services = mean(serv_share_change_pp, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      tidyr::pivot_longer(
        cols = c(Agriculture, Manufacturing, Services),
        names_to = "Sector",
        values_to = "mean_share"
      ) %>%
      mutate(
        Sector = factor(Sector, levels = c("Agriculture", "Manufacturing", "Services")),
        panel = "1872-1920 sector share change (pp)"
      )

    combined <- bind_rows(baseline_means, change_means) %>%
      mutate(panel = factor(panel, levels = c(
        "1872 sector shares (baseline)",
        "1872-1920 sector share change (pp)"
      )))

    sector_colors <- c(
      "Agriculture" = "#2ca02c",
      "Manufacturing" = "#1f77b4",
      "Services" = "#ff7f0e"
    )

    ggplot(combined, aes(x = slavery_quartile, y = mean_share, fill = Sector)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.7) +
      geom_hline(yintercept = 0, linetype = "solid", color = "grey30") +
      facet_wrap(~ panel, scales = "free_y") +
      scale_fill_manual(values = sector_colors) +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(
        x = "Enslaved share 1872 (quartile)",
        y = NULL,
        title = "Sector composition and change by slavery intensity",
        subtitle = paste0(n_total, " AMCs (", n_label, " per quartile)")
      ) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom")
  })
}

shinyApp(ui, server)
