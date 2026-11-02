# Builds the reference tables the app needs alongside the Talking Therapies data:
# area names, population figures, map shapes and the "similar areas" profile.

# ---- Area names ---------------------------------------------------------------

build_areas <- function(lookup_path, names_path, focus_code) {
  lookup <- readr::read_csv(lookup_path, show_col_types = FALSE)
  names <- readr::read_csv(names_path, col_types = readr::cols(.default = "c"))

  sub_icbs <- lookup |>
    dplyr::transmute(
      area_code = SICBL26CDH,
      ons_code = SICBL26CD,
      official_name = SICBL26NM,
      region_code = NHSER26CDH,
      region = NHSER26NM,
      level = "Sub-ICB"
    ) |>
    dplyr::left_join(dplyr::select(names, area_code, short_name), by = "area_code") |>
    dplyr::mutate(
      fallback = sub(" ICB - ", " ", sub("^NHS ", "", official_name)),
      short_name = dplyr::if_else(is.na(short_name) | short_name == "", fallback, short_name),
      is_focus = area_code == focus_code
    ) |>
    dplyr::select(-fallback)

  regions <- sub_icbs |>
    dplyr::distinct(area_code = region_code, official_name = region, region_code, region) |>
    dplyr::mutate(ons_code = NA_character_, short_name = region, level = "Region", is_focus = FALSE)

  england <- data.frame(
    area_code = "ENG", ons_code = NA_character_, official_name = "England",
    region_code = NA_character_, region = NA_character_, level = "England",
    short_name = "England", is_focus = FALSE
  )

  dplyr::bind_rows(sub_icbs, regions, england)
}

# ---- Population figures ------------------------------------------------------

build_population <- function(population_path, areas) {
  pop <- readr::read_csv(population_path, show_col_types = FALSE) |>
    dplyr::filter(type == "population") |>
    dplyr::mutate(breakdown = dplyr::if_else(breakdown == "Everyone (no breakdown)", "Everyone", breakdown)) |>
    dplyr::select(area_code, breakdown, category, population = denominator)

  sub_icbs <- dplyr::filter(areas, level == "Sub-ICB")
  sub_pop <- dplyr::inner_join(pop, dplyr::select(sub_icbs, area_code, region_code), by = "area_code")
  n_in_region <- dplyr::count(sub_icbs, region_code, name = "n_needed")

  region_pop <- sub_pop |>
    dplyr::group_by(area_code = region_code, breakdown, category) |>
    dplyr::summarise(population = sum(population), n = dplyr::n(), .groups = "drop") |>
    dplyr::inner_join(n_in_region, by = c("area_code" = "region_code")) |>
    dplyr::filter(n == n_needed) |>
    dplyr::select(area_code, breakdown, category, population)

  england_sum <- sub_pop |>
    dplyr::group_by(breakdown, category) |>
    dplyr::summarise(population = sum(population), n = dplyr::n(), .groups = "drop") |>
    dplyr::filter(n == nrow(sub_icbs)) |>
    dplyr::transmute(area_code = "ENG", breakdown, category, population)
  england_direct <- dplyr::filter(pop, area_code == "ENG")
  england <- dplyr::bind_rows(
    england_direct,
    dplyr::anti_join(england_sum, england_direct, by = c("breakdown", "category"))
  )

  dplyr::bind_rows(dplyr::select(sub_pop, -region_code), region_pop, england)
}

# ---- Map shapes --------------------------------------------------------------

build_map <- function(boundaries_path, areas, simplify_metres = 300) {
  shapes <- sf::read_sf(boundaries_path) |>
    sf::st_transform(27700) |>  # British National Grid, in metres
    sf::st_simplify(preserveTopology = TRUE, dTolerance = simplify_metres) |>
    sf::st_cast("MULTIPOLYGON")

  points <- as.data.frame(sf::st_coordinates(shapes))
  codes <- areas$area_code[match(shapes$SICBL26CD, areas$ons_code)]

  data.frame(
    area_code = codes[points$L3],
    piece = paste(points$L3, points$L2, sep = "_"),
    ring = paste(points$L3, points$L2, points$L1, sep = "_"),
    x = round(points$X),
    y = round(points$Y)
  )
}

# ---- Similar areas -----------------------------------------------------------

# Finds a file in a folder by a pattern in its name. Returns NULL if none.
find_file <- function(folder, pattern) {
  files <- list.files(folder, full.names = TRUE)
  hit <- files[grepl(pattern, basename(files), perl = TRUE)]
  if (length(hit) == 0) NULL else hit[1]
}

# Finds a column by a pattern in its name, so small renames do not break it.
find_column <- function(data, pattern) {
  hit <- grep(pattern, names(data), value = TRUE, ignore.case = TRUE)
  if (length(hit) == 0) stop("no column matching '", pattern, "'", call. = FALSE)
  hit[1]
}

# Share of adults aged 65 and over, from the population figures.
older_share <- function(population) {
  population |>
    dplyr::filter(breakdown == "Working Age") |>
    dplyr::group_by(area_code) |>
    dplyr::summarise(
      pct_65_plus = 100 * sum(population[category == "65 and over"]) / sum(population),
      .groups = "drop"
    )
}

# Rural share and average deprivation for each Sub-ICB, from neighbourhood
# (LSOA) files. Returns NULL if any of the three files is missing.
rural_and_deprivation <- function(profile_cfg) {
  folder <- profile_cfg$folder
  paths <- list(
    deprivation = find_file(folder, profile_cfg$deprivation_pattern),
    rural = find_file(folder, profile_cfg$rural_pattern),
    lookup = find_file(folder, profile_cfg$lookup_pattern)
  )
  if (any(vapply(paths, is.null, logical(1)))) return(NULL)

  imd <- readr::read_csv(paths$deprivation, show_col_types = FALSE)
  imd <- data.frame(
    lsoa = imd[[find_column(imd, "^LSOA code")]],
    imd_score = imd[[find_column(imd, "^Index of Multiple Deprivation \\(IMD\\) Score")]],
    people = imd[[find_column(imd, "^Total population")]]
  )

  ruc <- readr::read_csv(paths$rural, show_col_types = FALSE)
  ruc_name <- ruc[[find_column(ruc, "^RUC21NM$|Urban_rur")]]
  ruc <- data.frame(
    lsoa = ruc[[find_column(ruc, "^LSOA21CD$")]],
    rural = grepl("rural", ruc_name, ignore.case = TRUE)
  )

  lsoa_lookup <- readr::read_csv(paths$lookup, show_col_types = FALSE)
  lsoa_lookup <- data.frame(
    lsoa = lsoa_lookup[[find_column(lsoa_lookup, "^LSOA21CD$")]],
    area_code = lsoa_lookup[[find_column(lsoa_lookup, "^SICBL\\d\\dCDH$")]]
  )

  lsoa_lookup |>
    dplyr::inner_join(imd, by = "lsoa") |>
    dplyr::inner_join(ruc, by = "lsoa") |>
    dplyr::group_by(area_code) |>
    dplyr::summarise(
      pct_rural = 100 * sum(people[rural]) / sum(people),
      imd_score = sum(imd_score * people) / sum(people),
      .groups = "drop"
    )
}

build_area_profile <- function(population, areas, profile_cfg, focus_code) {
  sub_icb_codes <- areas$area_code[areas$level == "Sub-ICB"]
  profile <- older_share(population) |>
    dplyr::filter(area_code %in% sub_icb_codes)
  extra <- rural_and_deprivation(profile_cfg)
  if (!is.null(extra)) profile <- dplyr::left_join(profile, extra, by = "area_code")

  factors <- intersect(c("pct_65_plus", "pct_rural", "imd_score"), names(profile))
  profile <- profile[stats::complete.cases(profile[factors]), ]
  if (!focus_code %in% profile$area_code) {
    stop("The focus area ", focus_code, " has no area profile.", call. = FALSE)
  }

  z <- scale(as.matrix(profile[factors]))
  focus_row <- z[profile$area_code == focus_code, ]
  profile$distance <- sqrt(rowSums(sweep(z, 2, focus_row)^2))
  profile$similarity_rank <- rank(profile$distance, ties.method = "first") - 1L  # focus = 0

  list(profile = profile, factors = factors)
}
