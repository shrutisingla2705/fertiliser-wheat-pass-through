# 00_setup.R
# Packages, paths and parameters used by every other script.
# Always run scripts from the PROJECT ROOT (open the .Rproj file in RStudio).

# ---- 1. packages -------------------------------------------------------------
required <- c("tidyverse", "readxl", "lubridate", "zoo",
              "sandwich", "lmtest", "tseries", "scales",
              "rmarkdown", "knitr")
missing_pkgs <- required[!vapply(required, requireNamespace,
                                 logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop("Please install these packages first:\n  install.packages(c(",
       paste0('"', missing_pkgs, '"', collapse = ", "), "))", call. = FALSE)
}
suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(lubridate)
  library(sandwich)
  library(lmtest)
  library(tseries)
  library(scales)
})

# ---- 2. paths ----------------------------------------------------------------
# The defaults are what you want. The environment variables exist only so the
# automated test (tests/run_tests.R) can run the whole pipeline on a fake file
# without touching your real data or results.
RAW_FILE <- Sys.getenv("PW_RAW_FILE", unset = "data/raw/CMO-Historical-Data-Monthly.xlsx")
OUT_DIR  <- Sys.getenv("PW_OUT_DIR",  unset = "output")
PROC_DIR <- Sys.getenv("PW_PROC_DIR", unset = "data/processed")
FIG_DIR  <- file.path(OUT_DIR, "figures")
TAB_DIR  <- file.path(OUT_DIR, "tables")
for (p in c(PROC_DIR, FIG_DIR, TAB_DIR)) {
  dir.create(p, recursive = TRUE, showWarnings = FALSE)
}

# ---- 3. analysis parameters (change here, nowhere else) -----------------------
START_DATE  <- as.Date("2015-01-01")  # first month of the analysis sample
SHOCK_START <- as.Date("2021-01-01")  # analyst-defined "price spike" window
SHOCK_END   <- as.Date("2022-12-31")
BASE_DATE   <- as.Date("2019-01-01")  # prices are indexed to 100 in this month
MAIN_LAGS   <- 0:6                    # months of fertiliser price lags in the main model
ROLL_WINDOW <- 24                     # months in the rolling correlation
ROLL_PRED_LAGS <- 1:3                 # rolling chart uses the AVERAGE fertiliser change over these past months
SENS_END    <- as.Date("2025-12-31")  # sensitivity check: sample ends here (drops 2026, which holds a joint oil, gas and urea jump in March)
UREA_BREAK  <- as.Date("2022-03-01")  # World Bank changed the urea benchmark (f.o.b. Black Sea -> f.o.b. Middle East) in March 2022
BURN_IN     <- 13                     # extra months before START_DATE kept only to build lags

# ---- 4. which World Bank columns we use ---------------------------------------
# Columns are matched by pattern on the lower-case column name, not by position,
# because the World Bank changes the layout of the file from time to time.
SERIES_PATTERNS <- list(
  wheat = "^wheat,?\\s*(u\\.?s\\.?,?\\s*)?hrw",  # Wheat, US HRW
  dap   = "^dap\\b",                               # DAP (diammonium phosphate)
  urea  = "^urea",                                 # Urea (E. Europe)
  oil   = "^crude oil,?\\s*average",               # optional control
  gas   = "^natural gas,?\\s*europe"               # optional control
)
REQUIRED_SERIES <- c("wheat", "dap", "urea")     # pipeline stops if any is missing
OPTIONAL_SERIES <- c("oil", "gas")               # used as controls if present
