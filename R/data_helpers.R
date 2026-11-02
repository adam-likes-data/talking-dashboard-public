# Small, pure functions that pick and reshape data for the pages.
# "Pure" means: same inputs, same output, nothing else changed. No Shiny code here.

# One row of config/measures.csv as a list, e.g. measure_info(measures, "M031")$label
measure_info <- function(measures, id) {
  as.list(measures[measures$measure_id == id, ])
}

# Count measures are compared per 100,000 adults; everything else as published.
is_count <- function(measures, id) measure_info(measures, id)$unit == "count"

# Values for one measure, with area names and (for counts) a rate per 100,000
# adults added. Rows with no published value are kept, so gaps show as gaps.
get_measure <- function(app_data, measures, id, breakdown = "Everyone",
                        quarters = NULL, area_codes = NULL) {
  out <- app_data$values |>
    dplyr::filter(measure_id == id, .data$breakdown == !!breakdown)
  if (!is.null(quarters)) out <- dplyr::filter(out, quarter %in% quarters)
  if (!is.null(area_codes)) out <- dplyr::filter(out, area_code %in% area_codes)

  out <- dplyr::left_join(
    out,
    dplyr::select(app_data$areas, area_code, short_name, region, is_focus),
    by = "area_code"
  )

  if (is_count(measures, id) && breakdown == "Everyone") {
    adults <- app_data$population |>
      dplyr::filter(.data$breakdown == "Everyone") |>
      dplyr::select(area_code, adults = population)
    out <- out |>
      dplyr::left_join(adults, by = "area_code") |>
      dplyr::mutate(shown = per_100k(value, adults))
  } else {
    out$shown <- out$value
  }
  out
}

per_100k <- function(count, population) 100000 * count / population

# The quarter before a given one, or NA if it is the first.
previous_quarter <- function(all_quarters, quarter) {
  q <- sort(unique(all_quarters))
  i <- match(quarter, q)
  if (is.na(i) || i == 1) NA_character_ else q[i - 1]
}

# Sub-ICB codes for each "Compare with" choice. Always includes the focus area.
comparison_codes <- function(app_data, choice, focus_code, similar_n = 10, chosen = NULL) {
  sub_icbs <- app_data$areas[app_data$areas$level == "Sub-ICB", ]
  focus_region <- sub_icbs$region[sub_icbs$area_code == focus_code]
  codes <- switch(choice,
    england = sub_icbs$area_code,
    region  = sub_icbs$area_code[sub_icbs$region == focus_region],
    similar = app_data$profile$area_code[
      app_data$profile$similarity_rank <= similar_n],
    chosen  = chosen
  )
  unique(c(focus_code, codes))
}

# Median and range across a set of areas, per quarter, for the grey band.
summarise_areas <- function(d) {
  d <- dplyr::filter(d, !is.na(shown))
  if (nrow(d) == 0) {
    return(data.frame(quarter = character(), quarter_start = as.Date(character()),
                      low = numeric(), middle = numeric(), high = numeric(), n_areas = integer()))
  }
  d |>
    dplyr::group_by(quarter, quarter_start) |>
    dplyr::summarise(
      low = min(shown), middle = stats::median(shown), high = max(shown),
      n_areas = dplyr::n(), .groups = "drop"
    )
}

# Formats numbers for people to read, e.g. 51.2%, 1,234, 34 days.
format_value <- function(x, unit, rate = FALSE) {
  out <- dplyr::case_when(
    is.na(x)            ~ "No data",
    rate                ~ formatC(x, format = "f", digits = 0, big.mark = ","),
    unit == "percent"   ~ paste0(formatC(x, format = "f", digits = 1), "%"),
    unit == "count"     ~ formatC(x, format = "f", digits = 0, big.mark = ","),
    unit == "days"      ~ paste(formatC(x, format = "f", digits = 0), "days"),
    unit == "minutes"   ~ paste(formatC(x, format = "f", digits = 0), "minutes"),
    TRUE                ~ formatC(x, format = "f", digits = 1)
  )
  out
}

# Wraps long labels onto several lines, e.g. for axis and legend titles.
strwrap_label <- function(x, width) {
  unname(vapply(x, function(s) paste(strwrap(s, width), collapse = "\n"), character(1)))
}

# The label for the value axis, which changes when counts become rates.
value_label <- function(measures, id) {
  m <- measure_info(measures, id)
  if (m$unit == "count") paste(m$label, "per 100,000 adults") else m$label
}

# Describes a change between two values in words, e.g. "Up 4.1% since 2025/26 Q4".
describe_change <- function(now, before, unit, before_label) {
  if (is.na(now) || is.na(before) || before == 0) return("No earlier figure to compare")
  if (unit == "percent") {
    diff <- now - before
    size <- paste(formatC(abs(diff), format = "f", digits = 1), "percentage points")
  } else {
    diff <- (now - before) / before
    size <- paste0(formatC(100 * abs(diff), format = "f", digits = 1), "%")
  }
  if (round(diff, 3) == 0) return(paste("No change since", before_label))
  paste(if (diff > 0) "Up" else "Down", size, "since", before_label)
}

# ---- Groups (used by Who we reach and How they benefit) -----------------------

# Puts groups in a sensible order: "Under 18" first, then by the first number
# in the name (so "26 to 64" comes before "65 to 74"), then alphabetically.
order_categories <- function(x) {
  x <- unique(x)
  first_number <- suppressWarnings(as.numeric(sub("^\\D*(\\d+).*$", "\\1", x)))
  first_number[!grepl("\\d", x)] <- NA
  first_number[grepl("^Under", x)] <- -1
  x[order(is.na(first_number), first_number, x)]
}

# Characteristics with published figures for a measure, e.g. "Age Group".
breakdown_choices <- function(app_data, measure_id, need_population = FALSE) {
  have <- unique(app_data$values$breakdown[app_data$values$measure_id == measure_id])
  have <- setdiff(have, "Everyone")
  if (need_population) have <- intersect(have, unique(app_data$population$breakdown))
  sort(have)
}

reach_table <- function(app_data, measure_id, breakdown, quarter, area_codes,
                        not_known = character()) {
  counts <- app_data$values |>
    dplyr::filter(.data$measure_id == !!measure_id, .data$breakdown == !!breakdown,
                  .data$quarter == !!quarter, area_code %in% area_codes,
                  !category %in% not_known) |>
    dplyr::select(area_code, category, count = value)

  pop <- app_data$population |>
    dplyr::filter(.data$breakdown == !!breakdown, area_code %in% area_codes,
                  !category %in% not_known) |>
    dplyr::select(area_code, category, population)

  dplyr::inner_join(counts, pop, by = c("area_code", "category")) |>
    dplyr::group_by(area_code) |>
    dplyr::mutate(
      share_reached = count / sum(count, na.rm = TRUE),
      share_population = population / sum(population[!is.na(count)]),
      ratio = share_reached / share_population
    ) |>
    dplyr::ungroup() |>
    dplyr::left_join(dplyr::select(app_data$areas, area_code, short_name, is_focus),
                     by = "area_code")
}

# One measure by group, for the focus area, England and the comparison areas.
benefit_table <- function(app_data, measure_id, breakdown, quarter, area_codes, focus_code) {
  d <- app_data$values |>
    dplyr::filter(.data$measure_id == !!measure_id, .data$breakdown == !!breakdown,
                  .data$quarter == !!quarter,
                  area_code %in% c(area_codes, "ENG"))
  others <- d |>
    dplyr::filter(area_code %in% area_codes, area_code != focus_code, !is.na(value)) |>
    dplyr::group_by(category) |>
    dplyr::summarise(low = min(value), middle = stats::median(value),
                     high = max(value), n_areas = dplyr::n(), .groups = "drop")
  values_for <- function(code) {
    x <- d[d$area_code == code, c("category", "value")]
    stats::setNames(x$value, x$category)
  }
  categories <- order_categories(d$category)
  data.frame(category = categories) |>
    dplyr::left_join(others, by = "category") |>
    dplyr::mutate(focus = unname(values_for(focus_code)[category]),
                  england = unname(values_for("ENG")[category]))
}
