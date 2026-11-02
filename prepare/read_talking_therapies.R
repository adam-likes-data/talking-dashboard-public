# Reads NHS England's quarterly NHS Talking Therapies "ICB and Sub-ICB" CSV files
# and returns one tidy table: one row per quarter, area, group and measure.

# Columns every file must have. A file without them is skipped, not guessed at.
REQUIRED_COLUMNS <- c(
  "REPORTING_PERIOD_START", "GroupType", "ICB", "SubICB",
  "VariableType", "VariableA", "VariableB",
  "MEASURE_ID", "MEASURE_VALUE_SUPPRESSED"
)

# The levels kept, and the names used for them in the dashboard.
LEVELS <- c(
  England             = "England",
  CommissioningRegion = "Region",
  SubICB              = "Sub-ICB"
)

parse_nhs_date <- function(x) {
  x <- substr(as.character(x), 1, 10)
  out <- as.Date(x, format = "%Y-%m-%d")
  try_uk <- is.na(out)
  out[try_uk] <- as.Date(x[try_uk], format = "%d/%m/%Y")
  out
}

# Financial-year quarter label, e.g. "2026/27 Q1" for April to June 2026.
quarter_label <- function(date) {
  month <- as.integer(format(date, "%m"))
  year <- as.integer(format(date, "%Y"))
  fy_start <- ifelse(month >= 4, year, year - 1L)
  quarter <- ((month - 4L) %% 12L) %/% 3L + 1L
  sprintf("%d/%02d Q%d", fy_start, (fy_start + 1L) %% 100L, quarter)
}

# Turns VariableA / VariableB into one group name.
combine_category <- function(variable_a, variable_b) {
  ifelse(is.na(variable_b) | variable_b == variable_a,
         variable_a,
         paste0(variable_a, ": ", variable_b))
}

read_tt_file <- function(path, measure_ids) {
  raw <- readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
    na = c("", "NA", "null", "NULL"),
    progress = FALSE
  )

  missing <- setdiff(REQUIRED_COLUMNS, names(raw))
  if (length(missing) > 0) {
    stop("missing column(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }

  tidy <- raw |>
    dplyr::filter(GroupType %in% names(LEVELS), MEASURE_ID %in% measure_ids) |>
    dplyr::transmute(
      quarter_start = parse_nhs_date(REPORTING_PERIOD_START),
      level = unname(LEVELS[GroupType]),
      area_code = dplyr::case_when(
        level == "England" ~ "ENG",
        level == "Region"  ~ ICB,
        level == "Sub-ICB" ~ SubICB
      ),
      breakdown = dplyr::if_else(VariableType == "Total", "Everyone", VariableType),
      category = dplyr::if_else(VariableA == "Total", "Everyone",
                                combine_category(VariableA, VariableB)),
      measure_id = MEASURE_ID,
      value = suppressWarnings(as.numeric(MEASURE_VALUE_SUPPRESSED))
    )

  if (anyNA(tidy$quarter_start)) {
    stop("could not read some REPORTING_PERIOD_START dates", call. = FALSE)
  }

  tidy |>
    dplyr::mutate(quarter = quarter_label(quarter_start), .after = quarter_start) |>
    dplyr::mutate(source_file = basename(path))
}

# Reads every matching file in a folder. Returns the combined data and a log
# saying what happened to each file.
read_all_tt <- function(folder, file_pattern, measure_ids) {
  files <- list.files(folder, pattern = file_pattern, full.names = TRUE)
  if (length(files) == 0) {
    stop("No NHS Talking Therapies files found in ", folder,
         ". Add the quarterly ICB and Sub-ICB CSV files there.", call. = FALSE)
  }

  results <- lapply(files, function(f) {
    tryCatch(
      list(data = read_tt_file(f, measure_ids), error = NA_character_),
      error = function(e) list(data = NULL, error = conditionMessage(e))
    )
  })

  log <- data.frame(
    file = basename(files),
    modified = file.mtime(files),
    quarter = vapply(results, function(r) {
      if (is.null(r$data)) NA_character_ else paste(unique(r$data$quarter), collapse = ", ")
    }, character(1)),
    rows = vapply(results, function(r) if (is.null(r$data)) 0L else nrow(r$data), integer(1)),
    status = ifelse(vapply(results, function(r) is.null(r$data), logical(1)), "Skipped", "Loaded"),
    note = vapply(results, function(r) if (is.na(r$error)) "" else r$error, character(1))
  )

  values <- dplyr::bind_rows(lapply(results, `[[`, "data"))
  if (nrow(values) == 0) stop("None of the files could be read. See the log above.", call. = FALSE)

  # If two files cover the same quarter, keep the most recently changed one.
  newest <- log |>
    dplyr::filter(status == "Loaded") |>
    dplyr::group_by(quarter) |>
    dplyr::slice_max(modified, n = 1, with_ties = FALSE) |>
    dplyr::ungroup()
  replaced <- log$status == "Loaded" & !log$file %in% newest$file
  log$status[replaced] <- "Replaced"
  log$note[replaced] <- "Another file covers the same quarter and is newer"

  values <- dplyr::filter(values, source_file %in% newest$file)
  list(values = values, log = log)
}
