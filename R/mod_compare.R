# Compare areas page: a map and a ranked chart of the comparison areas.

compare_ui <- function(id, measures, settings) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::card(
      class = "mb-3",
      bslib::card_body(
        class = "d-flex flex-row flex-wrap gap-3 align-items-end",
        shiny::selectInput(ns("measure"), "Measure",
                           choices = stats::setNames(measures$measure_id, measures$label),
                           selected = settings$reach_measure, width = "24rem", selectize = FALSE),
        bslib::input_switch(ns("as_table"), "Show as table")
      ),
      shiny::uiOutput(ns("note"))
    ),
    shiny::uiOutput(ns("content"))
  )
}

compare_server <- function(id, app_data, measures, settings, shared) {
  shiny::moduleServer(id, function(input, output, session) {
    focus <- settings$focus_area

    compare_data <- shiny::reactive({
      get_measure(app_data, measures, input$measure, quarters = shared$quarter(),
                  area_codes = c(shared$codes(), "ENG"))
    })

    output$note <- shiny::renderUI({
      m <- measure_info(measures, input$measure)
      shiny::p(class = "text-muted small px-3",
        m$help,
        if (m$unit == "count") " Shown per 100,000 adults so areas of different sizes can be compared. The table also shows the published count." else NULL,
        if (identical(m$better, "higher")) " Higher is better." else if (identical(m$better, "lower")) " Lower is better." else NULL)
    })

    output$content <- shiny::renderUI({
      ns <- session$ns
      if (isTRUE(input$as_table)) {
        bslib::card(bslib::card_header("Table"), shiny::tableOutput(ns("table")))
      } else {
        bslib::layout_columns(
          col_widths = bslib::breakpoints(sm = 12, lg = c(6, 6)),
          bslib::card(full_screen = TRUE,
            bslib::card_header(shiny::h2("Map", class = "h5 mb-0")),
            ggiraph::girafeOutput(ns("map"))),
          bslib::card(full_screen = TRUE,
            bslib::card_header(shiny::h2("Ranked", class = "h5 mb-0")),
            ggiraph::girafeOutput(ns("ranked")))
        )
      }
    })

    output$map <- ggiraph::renderGirafe({
      d <- dplyr::filter(compare_data(), level == "Sub-ICB")
      shiny::validate(shiny::need(any(!is.na(d$shown)), "No data for this measure and quarter."))
      p <- map_plot(app_data$map, d, measures, input$measure)
      to_interactive(p$plot, height_in = p$height, width_in = p$width)
    })

    output$ranked <- ggiraph::renderGirafe({
      d <- compare_data()
      shiny::validate(shiny::need(any(!is.na(d$shown)), "No data for this measure and quarter."))
      n_areas <- sum(d$level == "Sub-ICB" & !is.na(d$shown))
      p <- ranked_plot(d, measures, input$measure, label_all = n_areas <= 30)
      height <- if (n_areas <= 30) max(3.5, 0.22 * n_areas + 1.2) else 6
      to_interactive(p, height_in = height, width_in = 7)
    })

    output$table <- shiny::renderTable({
      compare_table(compare_data(), measures, input$measure)
    }, align = "l")
  })
}

# ---- Building blocks -----------------------------------------------------------

map_plot <- function(map, d, measures, id) {
  m <- measure_info(measures, id)
  rate <- m$unit == "count"
  shapes <- dplyr::left_join(map, d[, c("area_code", "short_name", "shown", "is_focus")],
                             by = "area_code")
  in_set <- !is.na(shapes$short_name)
  shapes$tip <- ifelse(in_set,
    paste0(shapes$short_name, "\n", format_value(shapes$shown, m$unit, rate = rate)),
    "Not in this comparison")

  box <- shapes[in_set, ]
  pad <- 0.05 * max(diff(range(box$x)), diff(range(box$y)))
  xlim <- range(box$x) + c(-pad, pad)
  ylim <- range(box$y) + c(-pad, pad)

  plot <- ggplot2::ggplot(shapes, ggplot2::aes(x, y, group = piece, subgroup = ring)) +
    ggplot2::geom_polygon(data = shapes[!in_set, ], fill = COLOURS$faded,
                          colour = "#ffffff", linewidth = 0.2) +
    ggiraph::geom_polygon_interactive(
      data = shapes[in_set, ],
      ggplot2::aes(fill = shown, tooltip = tip, data_id = area_code),
      colour = "#ffffff", linewidth = 0.3
    ) +
    ggplot2::geom_polygon(data = shapes[shapes$area_code %in% d$area_code[d$is_focus], ],
                          fill = NA, colour = COLOURS$focus, linewidth = 1.1) +
    ggplot2::scale_fill_gradientn(colours = COLOURS$sequential, na.value = "#ffffff",
                                  name = strwrap_label(value_label(measures, id), 40),
                                  labels = function(x) format_value(x, m$unit, rate = rate),
                                  guide = ggplot2::guide_colourbar(title.position = "top")) +
    ggplot2::coord_equal(xlim = xlim, ylim = ylim) +
    ggplot2::labs(caption = "Orange outline: focus area. White: no published figure.") +
    ggplot2::theme_void(base_size = 13) +
    ggplot2::theme(legend.position = "top",
                   legend.title = ggplot2::element_text(size = 11),
                   legend.key.width = ggplot2::unit(1.6, "cm"))

  aspect <- diff(ylim) / diff(xlim)
  list(plot = plot, width = 6, height = min(8, max(3.5, 6 * aspect + 1)))
}

ranked_plot <- function(d, measures, id, label_all = TRUE) {
  m <- measure_info(measures, id)
  rate <- m$unit == "count"
  areas <- dplyr::filter(d, level == "Sub-ICB", !is.na(shown))
  england <- d$shown[d$area_code == "ENG"]
  areas$label <- ifelse(areas$is_focus, paste0(areas$short_name, "  ▶"), areas$short_name)
  areas$label <- stats::reorder(areas$label, areas$shown)
  areas$tip <- paste0(areas$short_name, "\n", format_value(areas$shown, m$unit, rate = rate))

  p <- ggplot2::ggplot(areas, ggplot2::aes(x = shown, y = label))
  if (label_all) {
    p <- p + ggplot2::geom_segment(ggplot2::aes(x = 0, xend = shown, yend = label),
                                   colour = COLOURS$grid, linewidth = 0.6)
  }
  if (length(england) == 1 && !is.na(england)) {
    p <- p + ggplot2::geom_vline(xintercept = england, colour = COLOURS$england, linetype = "22") +
      ggplot2::annotate("text", x = england, y = Inf, label = "England", vjust = -0.4,
                        hjust = 0, size = 3.6, colour = COLOURS$text)
  }
  if (!rate && !is.na(m$target)) {
    p <- p + ggplot2::geom_vline(xintercept = m$target, colour = COLOURS$text_muted, linetype = "dotted") +
      ggplot2::annotate("text", x = m$target, y = -Inf, label = "National standard",
                        vjust = 1.4, hjust = 0, size = 3.6, colour = COLOURS$text_muted)
  }
  p +
    ggiraph::geom_point_interactive(
      ggplot2::aes(colour = is_focus, size = is_focus, tooltip = tip, data_id = area_code)
    ) +
    ggplot2::scale_colour_manual(values = c(`FALSE` = COLOURS$comparison, `TRUE` = COLOURS$focus),
                                 guide = "none") +
    ggplot2::scale_size_manual(values = c(`FALSE` = if (label_all) 2.6 else 1.6, `TRUE` = 4),
                               guide = "none") +
    ggplot2::scale_y_discrete(labels = function(x) {
      if (label_all) x else ifelse(grepl("\u25B6", x), x, "")
    }) +
    ggplot2::scale_x_continuous(labels = function(x) format_value(x, m$unit, rate = rate),
                                expand = ggplot2::expansion(mult = c(0, 0.08))) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(x = strwrap_label(value_label(measures, id), 30), y = NULL) +
    theme_dashboard() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank(),
                   plot.margin = ggplot2::margin(22, 16, 22, 8))
}

compare_table <- function(d, measures, id) {
  m <- measure_info(measures, id)
  rate <- m$unit == "count"
  d <- d[order(-d$shown, na.last = TRUE), ]
  out <- data.frame(
    Area = ifelse(d$is_focus %in% TRUE, paste(d$short_name, "(focus area)"), d$short_name),
    Region = ifelse(is.na(d$region), "", d$region),
    Value = format_value(d$shown, m$unit, rate = rate),
    check.names = FALSE
  )
  names(out)[3] <- value_label(measures, id)
  if (rate) out[["Count as published"]] <- format_value(d$value, "count")
  out
}
