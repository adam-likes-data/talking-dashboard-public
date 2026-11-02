# About the data page: sources, how figures are worked out, caveats, glossary,
# and which files were loaded.

about_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("page"))
}

about_server <- function(id, app_data, measures, settings) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    focus <- settings$focus_area

    output$page <- shiny::renderUI({
      similar <- app_data$profile[app_data$profile$similarity_rank >= 1 &
                                  app_data$profile$similarity_rank <= settings$area_profile$number_of_similar_areas, ]
      similar <- similar[order(similar$similarity_rank), ]
      similar_names <- app_data$areas$short_name[match(similar$area_code, app_data$areas$area_code)]
      factor_words <- c(pct_65_plus = "share of adults aged 65 and over",
                        pct_rural = "share of people living in rural areas",
                        imd_score = "average deprivation score")[app_data$similar_factors]

      bslib::layout_columns(
        col_widths = bslib::breakpoints(sm = 12, lg = c(6, 6, 6, 6, 12)),
        bslib::card(
          bslib::card_header(shiny::h2("Where the figures come from", class = "h5 mb-0")),
          shiny::p("NHS England publishes NHS Talking Therapies figures every quarter for each Sub-ICB area. ",
                   "A Sub-ICB is the local NHS planning area. This dashboard uses the published 'ICB and Sub-ICB' files."),
          shiny::p("Population figures come from GP registrations and the 2021 Census:"),
          shiny::tableOutput(ns("sources")),
          shiny::p(class = "text-muted small",
                   "Prepared on ", format(app_data$built_at, "%d %B %Y at %H:%M"), ".")
        ),
        bslib::card(
          bslib::card_header(shiny::h2("How the figures are worked out", class = "h5 mb-0")),
          shiny::tags$ul(
            shiny::tags$li(shiny::strong("Per 100,000 adults. "),
              "Counts are divided by the number of adults registered with a GP in the area, then multiplied by 100,000. This lets areas of different sizes be compared."),
            shiny::tags$li(shiny::strong("Reach ratio. "),
              "A group's share of people starting treatment, divided by its share of the population. 1.0 means in line with the population."),
            shiny::tags$li(shiny::strong("Similar areas. "), paste0(
              "The ", length(similar_names), " Sub-ICBs closest to ", focus$name, " on ",
              paste(factor_words, collapse = ", "), ". Each factor counts equally. ",
              "They are: ", paste(similar_names, collapse = ", "), "."))
          )
        ),
        bslib::card(
          bslib::card_header(shiny::h2("Things to bear in mind", class = "h5 mb-0")),
          shiny::tags$ul(
            shiny::tags$li("Population share is a reference point, not a measure of need. Some groups may need support more or less often than others, so a reach ratio of 1.0 is not a target."),
            shiny::tags$li("Ethnicity, religion and sexual orientation population figures come from the 2021 Census and cover all ages. NHS Talking Therapies is for adults, so these comparisons are approximate."),
            shiny::tags$li("NHS England publishes each characteristic separately (for example age, or ethnicity), so two characteristics cannot be combined."),
            shiny::tags$li("NHS England hides small numbers to protect privacy. These show as 'No data'."),
            shiny::tags$li("Groups recorded as not known or not stated are left out of reach comparisons."),
            shiny::tags$li("Sub-ICB areas changed in April 2026. Areas that no longer exist are left out.")
          )
        ),
        bslib::card(
          bslib::card_header(shiny::h2("Data files loaded", class = "h5 mb-0")),
          shiny::tableOutput(ns("files"))
        ),
        bslib::card(
          bslib::card_header(shiny::h2("What each measure means", class = "h5 mb-0")),
          shiny::tableOutput(ns("glossary"))
        )
      )
    })

    output$sources <- shiny::renderTable({
      s <- app_data$population_sources
      s <- s[s$type == "population", c("breakdown", "source", "who")]
      names(s) <- c("Characteristic", "Source", "Who it covers")
      s$Characteristic[s$Characteristic == "Everyone (no breakdown)"] <- "Everyone"
      s
    }, align = "l")

    output$files <- shiny::renderTable({
      f <- app_data$file_log[, c("file", "quarter", "status", "note")]
      names(f) <- c("File", "Quarter", "Status", "Note")
      f
    }, align = "l")

    output$glossary <- shiny::renderTable({
      g <- measures[, c("label", "theme", "help")]
      names(g) <- c("Measure", "Theme", "What it means")
      g
    }, align = "l")
  })
}
