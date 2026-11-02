# Overview page: headline figures for the focus area, and a trend over time.

overview_ui <- function(id, measures, settings) {
  ns <- shiny::NS(id)
  trend_choices <- stats::setNames(measures$measure_id, measures$label)

  shiny::tagList(
    shiny::uiOutput(ns("headlines")),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        class = "d-flex flex-row flex-wrap gap-3 align-items-end justify-content-between",
        shiny::h2("How it has changed over time", class = "h5 mb-0"),
        shiny::selectInput(ns("measure"), "Measure", choices = trend_choices,
                           selected = settings$reach_measure, width = "22rem", selectize = FALSE)
      ),
      shiny::uiOutput(ns("trend_note")),
      bslib::input_switch(ns("as_table"), "Show as table"),
      shiny::uiOutput(ns("trend"))
    )
  )
}

overview_server <- function(id, app_data, measures, settings, shared) {
  shiny::moduleServer(id, function(input, output, session) {
    focus <- settings$focus_area

    # ---- Headline cards ------------------------------------------------------
    output$headlines <- shiny::renderUI({
      quarter <- shared$quarter()
      before <- previous_quarter(app_data$values$quarter, quarter)
      cards <- lapply(settings$headline_measures, function(id) {
        headline_card(app_data, measures, id, focus, quarter, before)
      })
      do.call(bslib::layout_column_wrap,
              c(list(width = "13rem", fill = FALSE, heights_equal = "row", class = "mb-3"), cards))
    })

    # ---- Trend chart ---------------------------------------------------------
    trend_data <- shiny::reactive({
      d <- get_measure(app_data, measures, input$measure,
                       area_codes = c(shared$codes(), "ENG"))
      list(
        focus = dplyr::filter(d, area_code == focus$code),
        england = dplyr::filter(d, area_code == "ENG"),
        band = summarise_areas(dplyr::filter(d, area_code %in% shared$codes(),
                                             area_code != focus$code))
      )
    })

    output$trend_note <- shiny::renderUI({
      m <- measure_info(measures, input$measure)
      shiny::p(class = "text-muted small mb-1",
        m$help,
        if (m$unit == "count") " Shown per 100,000 adults so areas of different sizes can be compared." else NULL,
        paste0(" The grey band shows the range across ", shared$label(), "."))
    })

    output$trend <- shiny::renderUI({
      if (isTRUE(input$as_table)) {
        shiny::tableOutput(session$ns("trend_table"))
      } else {
        ggiraph::girafeOutput(session$ns("trend_chart"))
      }
    })

    output$trend_chart <- ggiraph::renderGirafe({
      d <- trend_data()
      shiny::validate(shiny::need(nrow(d$focus) > 0, "No data for this measure."))
      to_interactive(trend_plot(d, measures, input$measure, focus$name, shared$label()))
    })

    output$trend_table <- shiny::renderTable({
      trend_table(trend_data(), measures, input$measure, focus$name)
    }, align = "l")
  })
}

# ---- Building blocks used above --------------------------------------------

headline_card <- function(app_data, measures, id, focus, quarter, before) {
  m <- measure_info(measures, id)
  d <- get_measure(app_data, measures, id, quarters = c(quarter, before),
                   area_codes = c(focus$code, "ENG"))
  pick <- function(area, q) {
    v <- d$value[d$area_code == area & d$quarter %in% q]
    if (length(v) == 0) NA_real_ else v[1]
  }
  pick_rate <- function(area, q) {
    v <- d$shown[d$area_code == area & d$quarter %in% q]
    if (length(v) == 0) NA_real_ else v[1]
  }

  now <- pick(focus$code, quarter)
  england_line <- if (m$unit == "count") {
    paste0(format_value(pick_rate(focus$code, quarter), m$unit, rate = TRUE),
           " per 100,000 adults (England ",
           format_value(pick_rate("ENG", quarter), m$unit, rate = TRUE), ")")
  } else {
    paste("England:", format_value(pick("ENG", quarter), m$unit))
  }
  target_line <- if (!is.na(m$target)) paste0("National standard: ", m$target, "%") else NULL

  bslib::value_box(
    title = m$label,
    value = format_value(now, m$unit),
    shiny::p(describe_change(now, pick(focus$code, before), m$unit, before)),
    shiny::p(england_line),
    if (!is.null(target_line)) shiny::p(target_line),
    class = "headline-card"
  )
}

trend_plot <- function(d, measures, id, focus_name, comparison_label) {
  m <- measure_info(measures, id)
  rate <- m$unit == "count"
  tip <- function(area, q, x) paste0(area, "\n", q, ": ", format_value(x, m$unit, rate = rate))

  ggplot2::ggplot(mapping = ggplot2::aes(x = quarter)) +
    ggplot2::geom_ribbon(
      data = d$band, ggplot2::aes(ymin = low, ymax = high, group = 1),
      fill = COLOURS$band
    ) +
    ggiraph::geom_line_interactive(
      data = d$england, ggplot2::aes(y = shown, group = 1, tooltip = "England", data_id = "england"),
      colour = COLOURS$england, linetype = "22", linewidth = 0.7
    ) +
    ggiraph::geom_point_interactive(
      data = d$england, ggplot2::aes(y = shown, tooltip = tip("England", quarter, shown), data_id = "england"),
      colour = COLOURS$england, size = 2.5
    ) +
    ggiraph::geom_line_interactive(
      data = d$focus, ggplot2::aes(y = shown, group = 1, tooltip = focus_name, data_id = "focus"),
      colour = COLOURS$focus, linewidth = 1.2
    ) +
    ggiraph::geom_point_interactive(
      data = d$focus, ggplot2::aes(y = shown, tooltip = tip(focus_name, quarter, shown), data_id = "focus"),
      colour = COLOURS$focus, size = 3.5
    ) +
    ggplot2::geom_text(
      data = utils::tail(d$focus[order(d$focus$quarter), ], 1),
      ggplot2::aes(y = shown, label = focus_name), hjust = -0.15, colour = COLOURS$text, fontface = "bold"
    ) +
    ggplot2::geom_text(
      data = utils::tail(d$england[order(d$england$quarter), ], 1),
      ggplot2::aes(y = shown, label = "England"), hjust = -0.25, colour = COLOURS$text
    ) +
    ggplot2::scale_x_discrete(expand = ggplot2::expansion(add = c(0.3, 1.8))) +
    ggplot2::labs(x = NULL, y = strwrap_label(value_label(measures, id), 30),
                  caption = paste("Grey band: range across", comparison_label)) +
    theme_dashboard()
}

trend_table <- function(d, measures, id, focus_name) {
  m <- measure_info(measures, id)
  rate <- m$unit == "count"
  pick <- function(x) stats::setNames(x$shown, x$quarter)
  quarters <- sort(unique(c(d$focus$quarter, d$band$quarter)))
  f <- pick(d$focus); e <- pick(d$england)
  out <- data.frame(
    Quarter = quarters,
    focus = format_value(unname(f[quarters]), m$unit, rate),
    `Comparison areas (lowest to highest)` = paste(
      format_value(d$band$low[match(quarters, d$band$quarter)], m$unit, rate), "to",
      format_value(d$band$high[match(quarters, d$band$quarter)], m$unit, rate)),
    England = format_value(unname(e[quarters]), m$unit, rate),
    check.names = FALSE
  )
  names(out)[2] <- focus_name
  if (rate) out[[paste(focus_name, "(count as published)")]] <-
    format_value(unname(stats::setNames(d$focus$value, d$focus$quarter)[quarters]), "count")
  out
}
