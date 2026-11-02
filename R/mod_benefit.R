# How they benefit page: treatment outcomes by group, for the focus area,
# England and the range across the comparison areas.

benefit_ui <- function(id, app_data, measures, settings) {
  ns <- shiny::NS(id)
  outcome <- measures[measures$theme %in% settings$benefit_themes & measures$unit == "percent", ]
  first <- outcome$measure_id[1]

  shiny::tagList(
    bslib::card(
      class = "mb-3",
      bslib::card_body(
        class = "d-flex flex-row flex-wrap gap-3 align-items-end",
        shiny::selectInput(ns("measure"), "Outcome",
                           choices = stats::setNames(outcome$measure_id, outcome$label),
                           selected = first, width = "22rem", selectize = FALSE),
        shiny::selectInput(ns("breakdown"), "Characteristic",
                           choices = breakdown_choices(app_data, first), selected = "Age Group",
                           width = "20rem", selectize = FALSE),
        bslib::input_switch(ns("as_table"), "Show as table")
      ),
      shiny::uiOutput(ns("note"))
    ),
    shiny::uiOutput(ns("content"))
  )
}

benefit_server <- function(id, app_data, measures, settings, shared) {
  shiny::moduleServer(id, function(input, output, session) {
    focus <- settings$focus_area

    shiny::observeEvent(input$measure, {
      choices <- breakdown_choices(app_data, input$measure)
      keep <- if (input$breakdown %in% choices) input$breakdown else choices[1]
      shiny::updateSelectInput(session, "breakdown", choices = choices, selected = keep)
    })

    benefit_data <- shiny::reactive({
      shiny::req(input$breakdown)
      d <- benefit_table(app_data, input$measure, input$breakdown, shared$quarter(),
                         shared$codes(), focus$code)
      finished <- app_data$values |>
        dplyr::filter(measure_id == "M076", breakdown == input$breakdown,
                      quarter == shared$quarter(), area_code == focus$code) |>
        dplyr::select(category, finished = value)
      dplyr::left_join(d, finished, by = "category")
    })

    output$note <- shiny::renderUI({
      m <- measure_info(measures, input$measure)
      shiny::p(class = "text-muted small px-3",
        m$help,
        if (identical(m$better, "higher")) " Higher is better." else if (identical(m$better, "lower")) " Lower is better." else NULL,
        " Figures for small groups can change a lot between quarters.")
    })

    output$content <- shiny::renderUI({
      ns <- session$ns
      if (isTRUE(input$as_table)) {
        bslib::card(bslib::card_header("Table"), shiny::tableOutput(ns("table")))
      } else {
        bslib::card(full_screen = TRUE,
          bslib::card_header(shiny::h2(
            paste(measure_info(measures, input$measure)$label, "by", tolower(input$breakdown)),
            class = "h5 mb-0")),
          ggiraph::girafeOutput(ns("chart")))
      }
    })

    output$chart <- ggiraph::renderGirafe({
      d <- benefit_data()
      shiny::validate(shiny::need(any(!is.na(d$focus) | !is.na(d$england)),
                                  "No figures for this group in this quarter."))
      to_interactive(benefit_plot(d, measures, input$measure, focus$name, shared$label()),
                     height_in = 1.6 + 0.42 * nrow(d), width_in = 8)
    })

    output$table <- shiny::renderTable({
      benefit_table_view(benefit_data(), measures, input$measure, focus$name)
    }, align = "l")
  })
}

# ---- Building blocks -----------------------------------------------------------

benefit_plot <- function(d, measures, id, focus_name, comparison_label) {
  m <- measure_info(measures, id)
  d$category <- factor(d$category, levels = rev(d$category))
  fmt <- function(x) format_value(x, m$unit)
  d$tip <- paste0(d$category,
                  "\n", focus_name, ": ", fmt(d$focus),
                  ifelse(is.na(d$finished), "", paste0(" (", format_value(d$finished, "count"), " finished treatment)")),
                  "\nEngland: ", fmt(d$england),
                  "\nComparison areas: ", fmt(d$low), " to ", fmt(d$high))

  points <- rbind(
    data.frame(category = d$category, value = d$england, tip = d$tip, series = "England"),
    data.frame(category = d$category, value = d$focus, tip = d$tip, series = focus_name)
  )
  points$series <- factor(points$series, levels = c(focus_name, "England"))

  p <- ggplot2::ggplot(d, ggplot2::aes(y = category)) +
    ggplot2::geom_segment(ggplot2::aes(x = low, xend = high, yend = category),
                          colour = COLOURS$band, linewidth = 5, lineend = "round", na.rm = TRUE)
  if (!is.na(m$target)) {
    p <- p + ggplot2::geom_vline(xintercept = m$target, colour = COLOURS$text_muted, linetype = "dotted") +
      ggplot2::annotate("text", x = m$target, y = Inf, label = "National standard",
                        vjust = -0.5, hjust = 0, size = 3.6, colour = COLOURS$text_muted)
  }
  p +
    ggiraph::geom_point_interactive(
      data = points,
      ggplot2::aes(x = value, colour = series, shape = series, size = series,
                   tooltip = tip, data_id = category),
      na.rm = TRUE
    ) +
    ggplot2::scale_colour_manual(values = stats::setNames(c(COLOURS$focus, COLOURS$england),
                                                          c(focus_name, "England")), name = NULL) +
    ggplot2::scale_shape_manual(values = stats::setNames(c(16, 18), c(focus_name, "England")), name = NULL) +
    ggplot2::scale_size_manual(values = stats::setNames(c(4, 3.6), c(focus_name, "England")), name = NULL) +
    ggplot2::scale_x_continuous(labels = function(x) format_value(x, m$unit)) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(x = m$label, y = NULL,
                  caption = paste("Grey bar: range across", comparison_label)) +
    theme_dashboard() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                   plot.margin = ggplot2::margin(20, 16, 8, 8))
}

benefit_table_view <- function(d, measures, id, focus_name) {
  m <- measure_info(measures, id)
  fmt <- function(x) format_value(x, m$unit)
  out <- data.frame(
    Group = d$category,
    focus = fmt(d$focus),
    finished = format_value(d$finished, "count"),
    England = fmt(d$england),
    `Comparison areas (lowest to highest)` = ifelse(is.na(d$low), "No data",
                                                    paste(fmt(d$low), "to", fmt(d$high))),
    check.names = FALSE
  )
  names(out)[2:3] <- c(focus_name, paste0(focus_name, ": people who finished treatment"))
  out
}
