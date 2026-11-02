# Installs the packages this dashboard needs. Run once per computer.
# The run_dashboard launchers run it for you.

app_packages <- c(
  "shiny",    # the web app framework
  "bslib",    # page layout, cards and theme
  "ggplot2",  # charts
  "ggiraph",  # makes ggplot2 charts interactive (hover, tooltips, zoom)
  "dplyr",    # data wrangling
  "yaml"      # reads config/settings.yml
)

prepare_packages <- c(
  "readr",    # reads the large raw CSV files quickly
  "sf",       # reads and simplifies map boundaries
  "testthat"  # runs the automated checks in tests/
)

needed <- c(app_packages, prepare_packages)
missing <- setdiff(needed, rownames(installed.packages()))

if (length(missing) > 0) {
  message("Installing: ", paste(missing, collapse = ", "))
  install.packages(missing, repos = "https://cloud.r-project.org")
} else {
  message("All packages are already installed.")
}

still_missing <- setdiff(needed, rownames(installed.packages()))
if (length(still_missing) > 0) {
  stop("These packages could not be installed: ",
       paste(still_missing, collapse = ", "),
       ". Check your internet connection, or ask IT whether CRAN is blocked.",
       call. = FALSE)
}
