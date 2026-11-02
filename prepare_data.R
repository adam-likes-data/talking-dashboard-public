# Turns the raw files in data-raw/ into one small file, data/app_data.rds,
# which is the only data file the app reads.
#
# Run it after adding a new quarter's CSV or changing the config files:
#   - in RStudio or Positron: open this file and click Source
#   - in a terminal, from this folder: Rscript prepare_data.R

library(dplyr, warn.conflicts = FALSE)

source("prepare/read_talking_therapies.R")
source("prepare/build_reference.R")

settings <- yaml::read_yaml("config/settings.yml")
measures <- read.csv("config/measures.csv", stringsAsFactors = FALSE)
focus_code <- settings$focus_area$code

say <- function(...) message(format(Sys.time(), "%H:%M:%S"), "  ", ...)

say("Reading NHS Talking Therapies files")
tt <- read_all_tt(
  folder = settings$talking_therapies$folder,
  file_pattern = settings$talking_therapies$file_pattern,
  measure_ids = measures$measure_id
)
print(tt$log[, c("file", "quarter", "rows", "status", "note")], row.names = FALSE)

say("Building area names")
areas <- build_areas(settings$reference$area_lookup, settings$reference$area_names, focus_code)

# Keep only areas the dashboard knows about, and add their names.
unknown <- tt$values |>
  filter(!area_code %in% areas$area_code) |>
  distinct(level, area_code, quarter)
if (nrow(unknown) > 0) {
  say("Left out ", n_distinct(unknown$area_code), " area code(s) not in the 2026 lookup: ",
      paste(unique(unknown$area_code), collapse = ", "))
}
values <- tt$values |>
  filter(area_code %in% areas$area_code) |>
  select(quarter, quarter_start, level, area_code, breakdown, category, measure_id, value)

say("Building population figures")
population <- build_population(settings$reference$population, areas)

say("Building map shapes")
map <- build_map(settings$reference$boundaries, areas, settings$map_simplify_metres)

say("Finding similar areas")
profile <- build_area_profile(population, areas, settings$area_profile, focus_code)
say("Similar areas are matched on: ", paste(profile$factors, collapse = ", "))

app_data <- list(
  values = values,
  areas = areas,
  population = population,
  population_sources = read.csv(settings$reference$population_sources, stringsAsFactors = FALSE),
  profile = profile$profile,
  similar_factors = profile$factors,
  map = map,
  file_log = tt$log,
  built_at = Sys.time()
)

dir.create(dirname(settings$output_file), showWarnings = FALSE, recursive = TRUE)
saveRDS(app_data, settings$output_file)
say("Saved ", settings$output_file, " (",
    format(structure(file.size(settings$output_file), class = "object_size"), units = "auto"),
    "). Quarters: ", paste(sort(unique(values$quarter)), collapse = ", "))
