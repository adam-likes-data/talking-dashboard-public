# Writes manifest.json, which tells Posit Connect Cloud which files and
# packages the app needs. Run from the seeing-who-we-reach folder:
#   Rscript tools/write_manifest.R
# Run it again whenever you add a package to the app.

if (!requireNamespace("rsconnect", quietly = TRUE)) install.packages("rsconnect")

app_files <- c(
  "app.R",
  list.files("R", pattern = "\\.R$", full.names = TRUE),
  list.files("www", full.names = TRUE),
  "config/settings.yml",
  "config/measures.csv",
  "config/area_names.csv",
  "data/app_data.rds"
)

missing <- app_files[!file.exists(app_files)]
if (length(missing) > 0) {
  stop("Missing: ", paste(missing, collapse = ", "),
       ". Run prepare_data.R first if data/app_data.rds is missing.", call. = FALSE)
}

rsconnect::writeManifest(appDir = ".", appFiles = app_files, appPrimaryDoc = "app.R")
message("Wrote manifest.json. Commit it with the app.")
