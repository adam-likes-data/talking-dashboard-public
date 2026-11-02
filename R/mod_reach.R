# Who we reach page: each group's share of people starting treatment compared
# with its share of the population, for the focus area and across areas.

reach_ui <- function(id, app_data, settings) {
  ns <- shiny::NS(id)
  choices <- breakdown_choices(app_data, settings$reach_measure, need_population = TRUE)

  shiny::tagList(
    bslib::card(
      class = "mb-3",
      bslib::card_body(
        class = "d-flex flex-row flex-wrap gap-3 align-items-end",
        shiny::selectInput(ns("breakdown"), "Characteristic", choices = choices,
                           selected = if ("Age Group" %in% choices) "Age Group" else choices[1],
                           width = "20rem", selectize = FALSE),
        bslib::input_switch(ns("as_table"), "Show as table")
      ),
      shiny::p(class = "text-muted small px-3",
        "The reach ratio compares each group's share of people starting treatment with its share of the population. ",
        "1.0 means in line with the population. Below 1.0 means the service reaches this group less often than its share. ",
        "Population share is a reference point, not a measure of need: some groups may need support more or less often than others."),
      shiny::uiOutput(ns("source_note"))
    ),
    shiny::uiOutput(ns("content"))
  )
}

reach_server <- function(id, app_data, settings, shared) {
  shiny::moduleServer(id, function(input, output, session) {
    focus <- settings$focus_area

    reach_data <- shiny::reactive({
      reach_table(app_data, settings$reach_measure, input$breakdown, shared$quarter(),
                  shared$codes(), not_known = settings$not_known_categories)
    })

    output$source_note <- shiny::renderUI({
      src <- app_data$population_sources
      src <- src[src$type == "population" & src$breakdown == input$breakdown, ]
      if (nrow(src) == 0) return(NULL)
      shiny::p(class = "text-muted small px-3",
               paste0("Population figures: ", src$source[1], " (", src$who[1], ")."))
    })

    output$content <- shiny::renderUI({
      ns <- session$ns
      if (isTRUE(input$as_table)) {
        return(bslib::card(bslib::card_header("Table"), shiny::tableOutput(ns("table"))))
      }
      shiny::tagList(
        bslib::card(full_screen = TRUE, class = "mb-3",
          bslib::card_header(shiny::h2(paste("Who the service reaches in", focus$name), class = "h5 mb-0")),
          shiny::uiOutput(ns("summary")),
          ggiraph::girafeOutput(ns("focus_chart"))),
        bslib::card(full_screen = TRUE,
          bslib::card_header(shiny::h2(paste("Reach ratio across", shared$label()), class = "h5 mb-0")),
          shiny::p(class = "text-muted small px-3 mb-0",
            "Red: reached less often than the group's population share. Blue: more often. Grey: in line. White: no published figure."),
          ggiraph::girafeOutput(ns("heatmap")))
      )
    })

    output$summary <- shiny::renderUI({
      d <- dplyr::filter(reach_data(), area_code == focus$code, !is.na(ratio))
      shiny::req(nrow(d) > 0)
      cut_off <- settings$reach_flag_below
      low <- d[d$ratio < cut_off, ]
      low <- low[order(low$ratio), ]
      text <- if (nrow(low) == 0) {
        paste0("In ", shared$quarter(), ", no ", tolower(input$breakdown),
               " had a reach ratio below ", cut_off, ".")
      } else {
        paste0("In ", shared$quarter(), ", groups with a reach ratio below ", cut_off, ": ",
               paste0(low$category, " (", formatC(low$ratio, format = "f", digits = 2), ")",
                      collapse = "; "), ".")
      }
      shiny::p(class = "px-3 mb-0", text)
    })

    output$focus_chart <- ggiraph::renderGirafe({
      d <- dplyr::filter(reach_data(), area_code == focus$code)
      shiny::validate(shiny::need(any(!is.na(d$ratio)), "No figures for this group in this quarter."))
      to_interactive(reach_focus_plot(d), height_in = 1.2 + 0.42 * nrow(d), width_in = 8)
    })

    output$heatmap <- ggiraph::renderGirafe({
      d <- reach_data()
      shiny::validate(shiny::need(any(!is.na(d$ratio)), "No figures for this group in this quarter."))
      n_rows <- length(unique(d$area_code))
      to_interactive(reach_heatmap(d), height_in = 1.8 + 0.3 * n_rows, width_in = 8)
    })

    output$table <- shiny::renderTable({
      reach_table_view(reach_data())
    }, align = "l")
  })
}

# ---- Building blocks -----------------------------------------------------------

pct <- function(x) paste0(formatC(100 * x, format = "f", digits = 1), "%")

reach_focus_plot <- function(d) {
  d$category <- factor(d$category, levels = rev(order_categories(d$category)))
  d$tip <- paste0(d$category,
                  "\nPeople starting treatment: ", pct(d$share_reached), " (", format_value(d$count, "count"), ")",
                  "\nPopulation: ", pct(d$share_population),
                  "\nReach ratio: ", formatC(d$ratio, format = "f", digits = 2))
  long <- rbind(
    data.frame(category = d$category, share = d$share_population, tip = d$tip,
               series = "Share of population"),
    data.frame(category = d$category, share = d$share_reached, tip = d$tip,
               series = "Share of people starting treatment")
  )

  ggplot2::ggplot(long, ggplot2::aes(x = share, y = category)) +
    ggplot2::geom_line(ggplot2::aes(group = category), colour = COLOURS$comparison, linewidth = 0.8) +
    ggiraph::geom_point_interactive(
      ggplot2::aes(shape = series, fill = series, colour = series, tooltip = tip, data_id = category),
      size = 3.6, stroke = 1.2
    ) +
    ggplot2::scale_shape_manual(values = c(21, 21), name = NULL) +
    ggplot2::scale_fill_manual(values = c("Share of population" = "#ffffff",
                                          "Share of people starting treatment" = COLOURS$focus), name = NULL) +
    ggplot2::scale_colour_manual(values = c("Share of population" = COLOURS$text_muted,
                                            "Share of people starting treatment" = COLOURS$focus), name = NULL) +
    ggplot2::scale_x_continuous(labels = function(x) pct(x), limits = c(0, NA),
                                expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::labs(x = "Share of this area's total", y = NULL) +
    theme_dashboard()
}

reach_heatmap <- function(d) {
  focus_name <- unique(d$short_name[d$is_focus %in% TRUE])
  others <- sort(setdiff(unique(d$short_name), focus_name))
  d$area <- factor(d$short_name, levels = rev(c(focus_name, others)))
  d$category <- factor(d$category, levels = order_categories(d$category))
  d$tip <- paste0(d$short_name, "\n", d$category, "\nReach ratio: ",
                  ifelse(is.na(d$ratio), "No data", formatC(d$ratio, format = "f", digits = 2)))
  d$log_ratio <- log2(d$ratio)
  show_numbers <- nrow(d) <= 150
  many <- length(levels(d$category)) > 7
  focus_rows <- d[d$is_focus %in% TRUE, ]

  p <- ggplot2::ggplot(d, ggplot2::aes(x = category, y = area)) +
    ggiraph::geom_tile_interactive(ggplot2::aes(fill = log_ratio, tooltip = tip,
                                                data_id = paste(area_code, category)),
                                   colour = "#ffffff", linewidth = 1) +
    ggplot2::geom_tile(data = focus_rows, fill = NA, colour = COLOURS$focus, linewidth = 1.2) +
    ggplot2::scale_fill_gradient2(
      low = COLOURS$diverging[["under"]], mid = COLOURS$diverging[["even"]],
      high = COLOURS$diverging[["over"]], midpoint = 0, limits = c(-2, 2),
      oob = function(x, range) pmin(pmax(x, range[1]), range[2]),
      breaks = log2(c(0.25, 0.5, 1, 2, 4)), labels = c("0.25", "0.5", "1", "2", "4"),
      na.value = "#ffffff", name = "Reach ratio"
    ) +
    ggplot2::scale_x_discrete(position = "top",
                              labels = function(x) strwrap_label(x, if (many) 26 else 14)) +
    ggplot2::labs(x = NULL, y = NULL) +
    theme_dashboard() +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   legend.position = "bottom",
                   legend.key.width = ggplot2::unit(1.4, "cm"))
  if (many) {
    p <- p + ggplot2::theme(axis.text.x.top = ggplot2::element_text(angle = 50, hjust = 0, vjust = 0))
  }
  if (show_numbers) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = ifelse(is.na(ratio), "", formatC(ratio, format = "f", digits = 2)),
                   colour = abs(log_ratio) > 1.2),
      size = 3.4, show.legend = FALSE) +
      ggplot2::scale_colour_manual(values = c(`FALSE` = COLOURS$text, `TRUE` = "#ffffff"),
                                   na.value = COLOURS$text, guide = "none")
  }
  p
}

reach_table_view <- function(d) {
  d <- d[order(!d$is_focus, d$short_name, match(d$category, order_categories(d$category))), ]
  data.frame(
    Area = d$short_name,
    Group = d$category,
    `People starting treatment` = format_value(d$count, "count"),
    `Share of people starting treatment` = pct(d$share_reached),
    `Share of population` = pct(d$share_population),
    `Reach ratio` = ifelse(is.na(d$ratio), "No data", formatC(d$ratio, format = "f", digits = 2)),
    check.names = FALSE
  )
}
