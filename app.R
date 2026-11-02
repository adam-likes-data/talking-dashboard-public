# Seeing who we reach: an NHS Talking Therapies dashboard.
#
# To run: open this file in RStudio or Positron and click Run App,
# or double-click run_dashboard.command (Mac) / run_dashboard.bat (Windows).
# The data comes from data/app_data.rds, made by prepare_data.R.

library(shiny)
library(bslib)
library(ggplot2)
library(ggiraph)
library(dplyr, warn.conflicts = FALSE)

# ---- Load settings and data (runs once, when the app starts) ----------------
settings <- yaml::read_yaml("config/settings.yml")
measures <- read.csv("config/measures.csv", stringsAsFactors = FALSE)

if (!file.exists(settings$output_file)) {
  stop("No prepared data found. Run prepare_data.R first.", call. = FALSE)
}
app_data <- readRDS(settings$output_file)

focus <- settings$focus_area
quarters <- sort(unique(app_data$values$quarter), decreasing = TRUE)
focus_region <- app_data$areas$region[app_data$areas$area_code == focus$code]
sub_icbs <- app_data$areas[app_data$areas$level == "Sub-ICB", ]
similar_n <- settings$area_profile$number_of_similar_areas

compare_choices <- c("england", "region", "similar", "chosen")
names(compare_choices) <- c(
  "All areas in England",
  paste("Areas in", focus_region),
  paste(similar_n, "similar areas"),
  "Areas I choose"
)

# ---- Page layout -------------------------------------------------------------
shared_sidebar <- sidebar(
  title = "Choose what to show",
  width = 300,
  selectInput("quarter", "Quarter", choices = quarters, selected = quarters[1], selectize = FALSE),
  radioButtons("compare", "Compare with", choices = compare_choices, selected = "region"),
  conditionalPanel(
    "input.compare == 'chosen'",
    selectizeInput("chosen", "Areas", multiple = TRUE,
                   choices = setNames(sub_icbs$area_code, sub_icbs$short_name),
                   options = list(
                     placeholder = "Type to search",
                     onInitialize = I("function() { this.$control_input.attr('aria-label', 'Areas to compare with'); }")
                   ))
  ),
  uiOutput("compare_note")
)

ui <- page_navbar(
  title = settings$title,
  theme = app_theme(),
  sidebar = shared_sidebar,
  header = tagList(
    tags$head(
      tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Atkinson+Hyperlegible+Next:wght@400;700&display=swap"),
      tags$link(rel = "stylesheet", href = "styles.css"),
      tags$script(src = "a11y.js")
    ),
    h1(class = "h4 app-subtitle", settings$subtitle)
  ),
  lang = "en",
  fillable = FALSE,
  nav_panel("Overview", overview_ui("overview", measures, settings)),
  nav_panel("Compare areas", compare_ui("compare", measures, settings)),
  nav_panel("Who we reach", reach_ui("reach", app_data, settings)),
  nav_panel("How they benefit", benefit_ui("benefit", app_data, measures, settings)),
  nav_panel("About the data", about_ui("about"))
)

# ---- Server ------------------------------------------------------------------
server <- function(input, output, session) {

  shared <- list(
    quarter = reactive(input$quarter),
    codes = reactive({
      comparison_codes(app_data, input$compare, focus$code, similar_n, input$chosen)
    }),
    label = reactive({
      switch(input$compare,
        england = "all areas in England",
        region  = paste("areas in", focus_region),
        similar = paste(similar_n, "areas most similar to", focus$name),
        chosen  = "the areas you chose")
    })
  )

  output$compare_note <- renderUI({
    if (input$compare == "similar") {
      factors <- c(pct_65_plus = "share of adults aged 65 and over",
                   pct_rural = "share of people living in rural areas",
                   imd_score = "deprivation")[app_data$similar_factors]
      p(class = "small text-muted",
        "Similar areas are the ", similar_n, " Sub-ICBs closest to ", focus$name,
        " on: ", paste(factors, collapse = ", "), ".")
    } else if (input$compare == "chosen" && length(input$chosen) == 0) {
      p(class = "small text-muted", "Choose at least one area to compare with.")
    }
  })

  overview_server("overview", app_data, measures, settings, shared)
  compare_server("compare", app_data, measures, settings, shared)
  reach_server("reach", app_data, settings, shared)
  benefit_server("benefit", app_data, measures, settings, shared)
  about_server("about", app_data, measures, settings)
}

shinyApp(ui, server)
