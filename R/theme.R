# Colours, fonts and chart styling, in one place.

COLOURS <- list(
  focus      = "#eb6834",  # the focus area (Norfolk and Waveney), on every chart
  comparison = "#8a8984",  # other areas in the comparison set
  band       = "#d9d8d4",  # range of the comparison areas
  england    = "#0b0b0b",  # England reference line
  faded      = "#ecebe8",  # areas outside the comparison set (map)
  text       = "#0b0b0b",
  text_muted = "#52514e",
  surface    = "#ffffff",
  grid       = "#e4e3df",
  # One-hue scale for the map (light = low, dark = high)
  sequential = c("#cde2fb", "#86b6ef", "#3987e5", "#1c5cab", "#0d366b"),
  # Two-sided scale for reach: red = fewer than population share, blue = more
  diverging  = c(under = "#c23b3b", even = "#f0efec", over = "#256abf")
)

FONT_STACK <- "'Atkinson Hyperlegible Next', 'Atkinson Hyperlegible', Arial, sans-serif"

# Theme for the Shiny page (Bootstrap 5 via bslib).
app_theme <- function() {
  bslib::bs_theme(
    version = 5,
    bg = COLOURS$surface,
    fg = COLOURS$text,
    primary = "#1c5cab",          # links and buttons: 7:1 contrast on white
    base_font = FONT_STACK,
    heading_font = FONT_STACK,
    "font-size-base" = "1.05rem"  # slightly larger than default, for readability
  )
}

# Theme for every ggplot chart: light grid, no chart border, readable text.
theme_dashboard <- function(base_size = 13) {
  ggplot2::theme_minimal(base_size = base_size, base_family = "sans") +
    ggplot2::theme(
      text = ggplot2::element_text(colour = COLOURS$text),
      axis.text = ggplot2::element_text(colour = COLOURS$text_muted),
      panel.grid.major = ggplot2::element_line(colour = COLOURS$grid, linewidth = 0.4),
      panel.grid.minor = ggplot2::element_blank(),
      plot.title.position = "plot",
      legend.position = "top",
      legend.justification = "left",
      plot.margin = ggplot2::margin(8, 16, 8, 8)
    )
}

# Turns a ggplot into an interactive chart, with the same settings everywhere.
to_interactive <- function(plot, height_in = 4, width_in = 8) {
  ggiraph::girafe(
    ggobj = plot,
    width_svg = width_in,
    height_svg = height_in,
    fonts = list(sans = FONT_STACK),
    options = list(
      ggiraph::opts_tooltip(
        css = paste0("background:#ffffff;color:#0b0b0b;border:1px solid #8a8984;",
                     "border-radius:6px;padding:6px 10px;font-size:15px;"),
        use_fill = FALSE
      ),
      ggiraph::opts_hover(css = "stroke-width:3px;"),
      ggiraph::opts_hover_inv(css = "opacity:0.35;"),  # fade the rest on hover
      ggiraph::opts_selection(type = "none"),
      ggiraph::opts_toolbar(saveaspng = FALSE),
      ggiraph::opts_sizing(rescale = TRUE)
    )
  )
}
