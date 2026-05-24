pacman::p_load(
  shiny,
  leaflet,
  sf,
  dplyr,
  scales,
  glue,
  htmltools
)
source("source/fun_census.R")

# ============================================================
# Load pre-built data
# ============================================================

dashboard_panel <- readRDS("build/dashboard_panel.rds")
mun_sf_1872 <- readRDS("build/mun_sf_1872.rds")
state_sf_1872 <- readRDS("build/state_sf_1872.rds")

# ============================================================
# Metric definitions: label, column, numerator, denominator
# ============================================================

metric_defs <- tribble(
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

metric_choices <- setNames(metric_defs$col, metric_defs$label)

# ============================================================
# UI
# ============================================================

ui <- fluidPage(
  titlePanel("Brazil 1872 Census: occupations and sectors"),

  sidebarLayout(
    sidebarPanel(
      radioButtons(
        inputId = "geo_level",
        label = "Geographic level",
        choices = c("Municipality", "State"),
        selected = "Municipality"
      ),

      radioButtons(
        inputId = "group_type",
        label = "Analysis type",
        choices = c("Occupation", "Sector"),
        selected = "Sector"
      ),

      uiOutput("group_picker"),

      selectInput(
        inputId = "metric",
        label = "Map metric",
        choices = metric_choices,
        selected = "share_of_all_occupations"
      ),

      helpText(
        "Note: literacy and enslaved population rates are mapped as ",
        "population-level contextual rates because the census variables ",
        "do not cross-tab these by occupation or sector."
      )
    ),

    mainPanel(
      leafletOutput("map", height = 720),
      br(),
      h4("Top geographies for selected metric"),
      tableOutput("top_table")
    )
  )
)

# ============================================================
# Server
# ============================================================

server <- function(input, output, session) {

  cur_metric_def <- reactive({
    metric_defs %>% filter(col == input$metric)
  })

  output$group_picker <- renderUI({
    groups <- dashboard_panel %>%
      filter(
        geography_level == input$geo_level,
        group_type == input$group_type
      ) %>%
      distinct(group_id, group_label) %>%
      arrange(group_label)

    selectInput(
      inputId = "group_id",
      label = input$group_type,
      choices = setNames(groups$group_id, groups$group_label),
      selected = groups$group_id[[1]]
    )
  })

  selected_panel <- reactive({
    req(input$geo_level, input$group_type, input$group_id, input$metric)

    dashboard_panel %>%
      filter(
        geography_level == input$geo_level,
        group_type == input$group_type,
        group_id == input$group_id
      )
  })

  map_data <- reactive({
    panel <- selected_panel()
    mdef <- cur_metric_def()

    if (input$geo_level == "Municipality") {
      mun_sf_1872 %>%
        inner_join(panel, by = c("code_muni_1872" = "geography_id")) %>%
        mutate(
          map_value = .data[[input$metric]],
          numerator = .data[[mdef$num_col]],
          denominator = .data[[mdef$den_col]]
        )
    } else {
      state_sf_1872 %>%
        inner_join(panel, by = c("code_state_chr" = "geography_id")) %>%
        mutate(
          map_value = .data[[input$metric]],
          numerator = .data[[mdef$num_col]],
          denominator = .data[[mdef$den_col]]
        )
    }
  })

  output$map <- renderLeaflet({
    dat <- map_data()
    mdef <- cur_metric_def()

    metric_label <- mdef$label

    pal <- colorNumeric(
      palette = "YlGnBu",
      domain = dat$map_value,
      na.color = "#eeeeee"
    )

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

    leaflet(dat, options = leafletOptions(preferCanvas = TRUE)) %>%
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
      ) %>%
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
  })

  output$top_table <- renderTable({
    dat <- map_data()
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

    tbl
  })
}

shinyApp(ui, server)
