# tests/run_tests.R
# Runs the WHOLE pipeline on a synthetic fake file (tests/fixture/) and checks that:
#   * the cleaning code reads the awkward World Bank layout correctly,
#   * the model recovers the effect that was deliberately planted in the fake data,
#   * the pipeline STOPS with a clear message when a required series is missing,
#   * every output is stamped as test data.
# Run from the project root:   source("tests/run_tests.R")
# The test writes only to tests/output and tests/processed, never to your real results.

failures <- 0
check <- function(cond, msg) {
  ok <- isTRUE(cond)
  cat(if (ok) "PASS  " else "FAIL  ", msg, "\n", sep = "")
  if (!ok) failures <<- failures + 1
}

fixture <- "tests/fixture/PINK_SHEET_TEST_FIXTURE.xlsx"
stopifnot("Run from the project root" = file.exists(fixture))
unlink(c("tests/output", "tests/processed"), recursive = TRUE)

# ---- Test 1: full pipeline on the fake file ----------------------------------------
Sys.setenv(PW_RAW_FILE = fixture, PW_OUT_DIR = "tests/output", PW_PROC_DIR = "tests/processed")
source("run_all.R")

suppressPackageStartupMessages(library(tidyverse))
used <- read_csv("tests/output/tables/series_used.csv", show_col_types = FALSE)
val  <- read_csv("tests/output/tables/data_validation.csv", show_col_types = FALSE)
get_val <- function(k) val$value[val$check == k]
dat  <- readRDS("tests/processed/prices_monthly.rds")
res  <- readRDS("tests/processed/results.rds")

cat("\n--- cleaning ---\n")
pick <- function(s) used$source_column[used$series == s]
check(pick("wheat") == "Wheat, US HRW",         "wheat column is HRW, not the SRW look-alike")
check(pick("dap")   == "DAP",                   "DAP column is DAP, not TSP")
check(pick("urea")  == "Urea, E. Europe",       "urea column found")
check(pick("oil")   == "Crude oil, average",    "oil is 'average', not Brent")
check(pick("gas")   == "Natural gas, Europe",   "gas is Europe, not US")
month_number <- as.integer(format(dat$date, "%Y")) * 12 + as.integer(format(dat$date, "%m"))
check(!anyDuplicated(dat$date) && all(diff(month_number) == 1),
      "dates form a clean monthly sequence")
check(sum(is.na(dat$oil)) == 2,                 "the two '..' oil values became NA")
check(as.integer(get_val("trailing months dropped (required series missing)")) == 1,
      "the incomplete last month was dropped")
check(get_val("last month") == "2025-05-01",    "last retained month is 2025-05")
check(as.integer(get_val("months in analysis sample")) == 125, "125 months in analysis sample (2015-01 to 2025-05)")
check(is.numeric(dat$wheat) && all(dat$wheat > 0), "prices are numeric and positive")

cat("\n--- estimation ---\n")
truth <- 0.45   # planted: 0.30 (lag 1) + 0.15 (lag 2)
dap <- res$main_cum |> filter(fertiliser == "dap")
check(nrow(dap) == 1,                           "main DAP model estimated")
check(dap$ci_low <= truth && truth <= dap$ci_high,
      sprintf("95%% CI [%.3f, %.3f] covers the planted cumulative effect 0.45", dap$ci_low, dap$ci_high))
check(dap$ci_low > 0,                           "the planted effect is detected (CI excludes zero)")
check(abs(dap$cumulative - truth) < 0.15,
      sprintf("estimate %.3f is close to planted 0.45", dap$cumulative))
lags_dap <- res$main_coefs |> filter(fertiliser == "dap")
est_at <- function(k) lags_dap$estimate[lags_dap$lag_months == k]
check(abs(est_at(1) - 0.30) < 0.12, sprintf("lag-1 coefficient %.3f is near planted 0.30 (fertiliser LEADS wheat)", est_at(1)))
check(abs(est_at(2) - 0.15) < 0.12, sprintf("lag-2 coefficient %.3f is near planted 0.15", est_at(2)))
check(abs(est_at(0)) < 0.12 && abs(est_at(5)) < 0.12,
      "lag-0 and lag-5 coefficients are near zero (no effect planted there)")
check(abs(sum(lags_dap$estimate) - dap$cumulative) < 1e-9,
      "cumulative estimate equals the sum of the lag coefficients")
urea <- res$main_cum |> filter(fertiliser == "urea")
check(nrow(urea) == 1 && is.finite(urea$cumulative), "urea model estimated (planted null)")
check(all(res$stationarity$p_value[res$stationarity$transform == "monthly log change"] < 0.05),
      "monthly log changes pass the ADF stationarity test")
check(nrow(res$robust) >= 6,                    "robustness grid produced")
check(sum(!is.na(res$rolling$rolling_corr)) > 100, "rolling correlation series produced")
check(nrow(res$sens) == 5, "sensitivity table has 5 rows (main and end-date for each fertiliser, plus the urea break)")
n_main  <- res$sens$n[res$sens$fertiliser == "urea" & res$sens$check == "main specification"]
n_blank <- res$sens$n[grepl("urea benchmark", res$sens$check)]
check(n_main - n_blank == 7, sprintf("blanking one month removes the 7 rows whose lag window touches it (%d -> %d)", n_main, n_blank))
check(nrow(res$ext_coefs) == 26, "exploratory 12-month model: 13 lags x 2 fertilisers")
check(nrow(res$granger) == 24,                  "Granger table has 2 fertilisers x 6 orders x 2 directions")

cat("\n--- outputs and test stamp ---\n")
figs <- c("fig1_indexed_prices.png", "fig2_rolling_correlation.png",
          "fig3_lag_coefficients.png", "fig4_lead_lag_correlations.png",
          "fig5_exploratory_12_months.png")
check(all(file.exists(file.path("tests/output/figures", figs))), "all five figures written")
check(file.exists("tests/output/report.html"),  "report rendered")
html <- paste(readLines("tests/output/report.html", warn = FALSE), collapse = "\n")
check(grepl("SYNTHETIC TEST DATA", html, fixed = TRUE), "report carries the SYNTHETIC TEST DATA banner")
n_imgs <- lengths(regmatches(html, gregexpr("data:image/png;base64", html, fixed = TRUE)))
check(n_imgs >= 5, sprintf("all five figures are embedded in the report (%d images found)", n_imgs))
check(isTRUE(res$is_test),                      "results are flagged as test data")

# ---- Test 2: pipeline must STOP when a required series is missing ----------------------
cat("\n--- failure handling ---\n")
Sys.setenv(PW_RAW_FILE = "tests/fixture/PINK_SHEET_TEST_FIXTURE_missing_urea.xlsx")
msg <- tryCatch({ source("R/02_clean_data.R"); "no error" },
                error = function(e) conditionMessage(e))
check(grepl("Required series not found: urea", msg, fixed = TRUE),
      "cleaning stops with a clear message when urea is missing")

Sys.unsetenv(c("PW_RAW_FILE", "PW_OUT_DIR", "PW_PROC_DIR"))
cat("\n", if (failures == 0) "ALL TESTS PASSED" else paste(failures, "TEST(S) FAILED"), "\n", sep = "")
if (failures > 0) stop(failures, " test(s) failed", call. = FALSE)
