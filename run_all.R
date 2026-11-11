# run_all.R
# Runs the whole project, from raw data to the finished report.
# Open fertiliser-wheat-pass-through.Rproj in RStudio, then run:   source("run_all.R")
source("R/00_setup.R")

steps <- c("R/01_get_data.R",   # download (or find) the raw World Bank file
           "R/02_clean_data.R", # clean + validate
           "R/03_analysis.R",   # models and tests
           "R/04_figures.R")    # charts
for (s in steps) {
  message("\n=== ", s, " ===")
  source(s)
}

message("\n=== report ===")
# The report goes in docs/ (GitHub treats that folder as documentation, and it can be shown
# as a web page). A test run, which changes OUT_DIR, keeps its report with its other outputs.
report_dir <- if (identical(OUT_DIR, "output")) "docs" else OUT_DIR
dir.create(report_dir, showWarnings = FALSE, recursive = TRUE)
rmarkdown::render("report/report.Rmd", output_file = "report.html",
                  output_dir = report_dir, knit_root_dir = normalizePath("."), quiet = TRUE)
writeLines(capture.output(sessionInfo()), file.path(OUT_DIR, "session_info.txt"))
message("\nDone. Open ", file.path(report_dir, "report.html"))
