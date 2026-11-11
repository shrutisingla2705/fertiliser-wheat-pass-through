# 02_clean_data.R
# Reads the raw World Bank workbook, extracts the series we need, cleans them,
# runs validation checks, and saves a tidy monthly data set.
#
# Design rules:
#  * The raw file is never edited by hand and never overwritten.
#  * Every cleaning decision is checked and written to output/tables/.
#  * If something looks wrong the script STOPS with a clear message,
#    rather than carrying on with bad data.
source("R/00_setup.R")

if (!file.exists(RAW_FILE)) {
  stop("Raw file not found: ", RAW_FILE,
       "\nRun R/01_get_data.R, or download it by hand (see that script).", call. = FALSE)
}

# ---- 1. find the monthly price sheet ------------------------------------------
sheets <- excel_sheets(RAW_FILE)
if ("Monthly Prices" %in% sheets) {
  sheet <- "Monthly Prices"
} else {
  hit <- sheets[grepl("month", sheets, ignore.case = TRUE) &
                grepl("price", sheets, ignore.case = TRUE)]
  if (length(hit) == 0) {
    stop("No monthly prices sheet found. Sheets in file: ",
         paste(sheets, collapse = ", "), call. = FALSE)
  }
  sheet <- hit[1]
}
message("Reading sheet: ", sheet)

# ---- 2. read everything as text so nothing is silently converted ---------------
raw <- suppressMessages(
  read_excel(RAW_FILE, sheet = sheet, col_names = FALSE, col_types = "text")
)
m <- as.matrix(raw)

# Safety flag: the test file is labelled synthetic. If so, every output is stamped.
top <- m[seq_len(min(8, nrow(m))), , drop = FALSE]
is_test <- any(grepl("SYNTHETIC TEST", top, fixed = TRUE))
if (is_test) message("*** SYNTHETIC TEST FILE DETECTED: outputs will be stamped as test data ***")

# When did the World Bank last update this file? (stated in the first rows)
upd_cells  <- m[seq_len(min(8, nrow(m))), 1]
updated_on <- upd_cells[grepl("^Updated on", upd_cells)][1]
message("File status: ", ifelse(is.na(updated_on), "no 'Updated on' line found", updated_on))

# ---- 3. locate the header row and the date rows -------------------------------
scan_rows <- seq_len(min(30, nrow(m)))
is_hdr <- vapply(scan_rows, function(i) {
  any(grepl("^dap\\b", tolower(trimws(m[i, ])), perl = TRUE))
}, logical(1))
if (!any(is_hdr)) {
  stop("Could not find the header row (a cell reading 'DAP') in the first 30 rows of '",
       sheet, "'. The World Bank may have changed the file layout.", call. = FALSE)
}
hdr_row <- which(is_hdr)[1]

col_names <- trimws(m[hdr_row, ])
col_units <- if (hdr_row < nrow(m)) trimws(m[hdr_row + 1, ]) else rep(NA_character_, ncol(m))

first_col    <- trimws(m[, 1])
is_date_row  <- grepl("^\\d{4}M\\d{2}$", first_col)     # e.g. 2021M06
if (sum(is_date_row) < 24) {
  stop("Only ", sum(is_date_row), " rows look like dates (e.g. 1960M01). ",
       "The file layout may have changed.", call. = FALSE)
}

# ---- 4. match the series we want (by name pattern, not by position) -----------
lc <- tolower(col_names)
series_map <- map_dfr(names(SERIES_PATTERNS), function(k) {
  idx <- which(grepl(SERIES_PATTERNS[[k]], lc, perl = TRUE))
  idx <- idx[idx != 1]                                   # never the date column
  tibble(
    series        = k,
    n_matches     = length(idx),
    column_index  = if (length(idx)) idx[1] else NA_integer_,
    source_column = if (length(idx)) col_names[idx[1]] else NA_character_,
    unit          = if (length(idx)) col_units[idx[1]] else NA_character_,
    all_matches   = paste(col_names[idx], collapse = " | ")
  )
})

missing_required <- series_map$series[series_map$series %in% REQUIRED_SERIES &
                                      series_map$n_matches == 0]
if (length(missing_required) > 0) {
  stop("Required series not found: ", paste(missing_required, collapse = ", "),
       "\nColumn names in the file (first 90):\n  ",
       paste(head(col_names[!is.na(col_names)], 90), collapse = " | "),
       "\nAdjust SERIES_PATTERNS in R/00_setup.R.", call. = FALSE)
}
for (i in which(series_map$n_matches > 1)) {
  message("Note: '", series_map$series[i], "' matched several columns (",
          series_map$all_matches[i], "). Using the first: ", series_map$source_column[i])
}
for (i in which(series_map$n_matches == 0)) {
  message("Optional series '", series_map$series[i], "' not found: skipped.")
}
write_csv(series_map, file.path(TAB_DIR, "series_used.csv"))
print(as.data.frame(series_map[, c("series", "source_column", "unit")]), row.names = FALSE)

# ---- 5. extract and convert to numbers -----------------------------------------
parse_num <- function(x, label) {
  x <- trimws(x)
  x[x %in% c("..", "...", "\u2026", "", "n.a.", "N/A", "NA", "-", "--")] <- NA
  out <- suppressWarnings(as.numeric(gsub(",", "", x)))
  unparsed <- sum(!is.na(x) & is.na(out))
  if (unparsed > 0) {
    warning(unparsed, " value(s) in '", label, "' could not be read as numbers and were set to NA.",
            call. = FALSE)
  }
  out
}

dates <- as.Date(paste0(substr(first_col[is_date_row], 1, 4), "-",
                        substr(first_col[is_date_row], 6, 7), "-01"))
dat <- tibble(date = dates)
for (k in series_map$series[series_map$n_matches > 0]) {
  ci <- series_map$column_index[series_map$series == k]
  dat[[k]] <- parse_num(m[is_date_row, ci], k)
}
dat <- dat |>
  arrange(date) |>
  filter(date >= START_DATE %m-% months(BURN_IN))      # keep a burn-in for building lags

# ---- 6. validation checks -----------------------------------------------------
vars <- intersect(c(REQUIRED_SERIES, OPTIONAL_SERIES), names(dat))
checks <- list()
add_check <- function(name, value) checks[[length(checks) + 1]] <<- tibble(check = name, value = as.character(value))

# 6a. duplicates and gaps in the monthly sequence
n_dup <- sum(duplicated(dat$date))
expected_months <- seq(min(dat$date), max(dat$date), by = "month")
gap_months <- expected_months[!expected_months %in% dat$date]
add_check("duplicate months", n_dup)
add_check("missing months in sequence", length(gap_months))
if (n_dup > 0 || length(gap_months) > 0) {
  stop("Dates are not a clean monthly sequence. Duplicates: ", n_dup,
       "; gaps: ", paste(format(gap_months), collapse = ", "), call. = FALSE)
}

# 6b. missing values in required series: allowed only at the very end of the file
complete <- complete.cases(dat[REQUIRED_SERIES])
dropped_tail <- as.Date(character())
if (!all(complete)) {
  bad <- which(!complete)
  last_ok <- max(which(complete))
  if (any(bad < last_ok)) {
    stop("Required series have missing values inside the sample (",
         paste(format(dat$date[bad[bad < last_ok]]), collapse = ", "),
         "). Inspect the raw file before continuing.", call. = FALSE)
  }
  dropped_tail <- dat$date[bad]
  dat <- dat[seq_len(last_ok), ]
  message("Dropped ", length(dropped_tail), " trailing month(s) with missing required values: ",
          paste(format(dropped_tail), collapse = ", "))
}
add_check("trailing months dropped (required series missing)", length(dropped_tail))

# 6c. prices must be positive (we take logs)
n_nonpos <- sum(dat[vars] <= 0, na.rm = TRUE)
add_check("non-positive price values", n_nonpos)
if (n_nonpos > 0) stop("Non-positive prices found; cannot take logs.", call. = FALSE)

# 6d. unusually large one-month moves (flagged for inspection, not removed)
outliers <- dat |>
  mutate(across(all_of(vars), ~ log(.x) - dplyr::lag(log(.x)))) |>
  select(date, all_of(vars)) |>
  pivot_longer(-date, names_to = "series", values_to = "log_change") |>
  filter(!is.na(log_change), abs(log_change) > 0.25)
write_csv(outliers, file.path(TAB_DIR, "outlier_flags.csv"))
add_check("one-month moves larger than 25% (log change), flagged not removed", nrow(outliers))

# 6e. missing values per series (optional series may have gaps; they are only controls)
for (v in vars) add_check(paste0("NA count: ", v), sum(is.na(dat[[v]])))

dat$in_sample <- dat$date >= START_DATE
add_check("first month retained (burn-in for lags)", format(min(dat$date)))
add_check("first month of analysis sample", format(min(dat$date[dat$in_sample])))
add_check("last month", format(max(dat$date)))
add_check("months in analysis sample", sum(dat$in_sample))
add_check("synthetic test file", is_test)
validation <- bind_rows(checks)
write_csv(validation, file.path(TAB_DIR, "data_validation.csv"))

# ---- 7. descriptive statistics (analysis sample only) -------------------------
summary_stats <- dat |>
  filter(in_sample) |>
  select(all_of(vars)) |>
  pivot_longer(everything(), names_to = "series", values_to = "price") |>
  group_by(series) |>
  summarise(n = sum(!is.na(price)), mean = mean(price, na.rm = TRUE),
            sd = sd(price, na.rm = TRUE), min = min(price, na.rm = TRUE),
            max = max(price, na.rm = TRUE), .groups = "drop") |>
  left_join(select(series_map, series, source_column, unit), by = "series")
write_csv(summary_stats, file.path(TAB_DIR, "summary_stats.csv"))

# ---- 8. save --------------------------------------------------------------------
saveRDS(dat, file.path(PROC_DIR, "prices_monthly.rds"))
write_csv(dat, file.path(PROC_DIR, "prices_monthly_clean.csv"))
saveRDS(list(is_test = is_test, sheet = sheet, raw_file = RAW_FILE, updated_on = updated_on,
             series_map = series_map, dropped_tail = dropped_tail),
        file.path(PROC_DIR, "meta.rds"))

message("\nCleaning done: ", sum(dat$in_sample), " months in the analysis sample (",
        format(min(dat$date[dat$in_sample]), "%Y-%m"), " to ",
        format(max(dat$date), "%Y-%m"), "). Checks: ",
        file.path(TAB_DIR, "data_validation.csv"))
