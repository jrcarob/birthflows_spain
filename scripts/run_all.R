# Runs the full 5-stage workflow in sequence. Equivalent to running
# each numbered script individually via Rscript, in order.
#
# Usage (run from anywhere):
#   Rscript scripts/run_all.R
#   OUT_FORMAT=png Rscript scripts/run_all.R
#   OUT_FORMAT=svg Rscript scripts/run_all.R
script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- normalizePath(sub("^--file=", "", script_arg))
scripts_dir <- dirname(script_path)

steps <- c(
  "01_load_data.R",
  "02_projection.R",
  "03_series.R",
  "04_annotations.R",
  "05_figure.R"
)

for (step in steps) {
  cat("== Running", step, "==\n")
  source(file.path(scripts_dir, step), chdir = FALSE)
}
