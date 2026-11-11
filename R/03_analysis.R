# 03_analysis.R
# Question: did monthly changes in fertiliser prices (DAP, urea) go together with
# LATER changes in wheat prices, and by how much?
#
# Approach (standard time-series practice):
#  1. Work in monthly log changes (~ monthly % changes). Price LEVELS trend and wander,
#     so regressing levels on levels can give spurious results. We test this with ADF tests.
#  2. Lead-lag correlations: how strongly is wheat this month related to fertiliser k months ago?
#  3. Distributed-lag regression with Newey-West (HAC) standard errors, because monthly
#     price changes are usually autocorrelated and OLS standard errors would be too small.
#  4. Granger tests in BOTH directions (fertiliser -> wheat, wheat -> fertiliser).
#  5. Robustness: different lag lengths, with/without energy controls, sub-periods.
# Everything is an ASSOCIATION. Nothing here proves cause and effect.
source("R/00_setup.R")

d    <- readRDS(file.path(PROC_DIR, "prices_monthly.rds"))
meta <- readRDS(file.path(PROC_DIR, "meta.rds"))

vars_all       <- intersect(c("wheat", "dap", "urea", "oil", "gas"), names(d))
controls_avail <- intersect(OPTIONAL_SERIES, names(d))
message("Energy controls available: ",
        if (length(controls_avail)) paste(controls_avail, collapse = ", ") else "none")

# monthly log changes for every series (computed on the full file incl. burn-in)
dl <- d |>
  arrange(date) |>
  mutate(across(all_of(vars_all), ~ log(.x) - dplyr::lag(log(.x)), .names = "d_{.col}"))

# the core estimation sample (analysis window, complete for the three main series)
core <- dl |> filter(in_sample, !is.na(d_wheat), !is.na(d_dap), !is.na(d_urea))

# ---- 1. stationarity (ADF tests) ---------------------------------------------
adf_row <- function(x, series, transform) {
  x <- x[!is.na(x)]
  t <- suppressWarnings(tseries::adf.test(x))
  tibble(series = series, transform = transform, n = length(x),
         adf_statistic = unname(t$statistic), lag_order = unname(t$parameter),
         p_value = t$p.value)
}
levels_in_sample <- d |> filter(in_sample)
stationarity <- bind_rows(
  map_dfr(c("wheat", "dap", "urea"),
          ~ adf_row(log(levels_in_sample[[.x]]), .x, "log price (level)")),
  map_dfr(c("wheat", "dap", "urea"),
          ~ adf_row(core[[paste0("d_", .x)]], .x, "monthly log change"))
)

# ---- 2. lead-lag correlations --------------------------------------------------
# correlation between wheat change in month t and fertiliser change in month t-k
lead_lag <- map_dfr(c("dap", "urea"), function(x) {
  map_dfr(0:12, function(k) {
    xk   <- dplyr::lag(dl[[paste0("d_", x)]], k)
    keep <- dl$in_sample & !is.na(xk) & !is.na(dl$d_wheat)
    tibble(fertiliser = x, lag_months = k,
           correlation = cor(xk[keep], dl$d_wheat[keep]), n = sum(keep))
  })
}) |> mutate(ci95 = 1.96 / sqrt(n))

# ---- 3. distributed-lag model -------------------------------------------------
#   d_wheat(t) = a + sum_k b_k * d_fert(t-k) + energy controls + error
# b_k = change in wheat (in %) associated with a 1% change in fertiliser k months earlier.
# "cumulative" = sum of the b_k = total association over the lag window.
make_lags <- function(data, var, lags, prefix) {
  out <- lapply(lags, function(k) dplyr::lag(data[[var]], k))
  names(out) <- paste0(prefix, "_l", lags)
  tibble::as_tibble(out)
}

spec_label <- function(lags, controls) {
  paste0("lags ", min(lags), "-", max(lags),
         if (length(controls)) " + energy controls" else ", no controls")
}

fit_dl <- function(data, x, lags = MAIN_LAGS, controls = character(),
                   from = START_DATE, to = as.Date("2100-01-01"), label = "",
                   blank_x = as.Date(character())) {
  # blank_x: months whose fertiliser price CHANGE is set to missing (e.g. a known series break).
  # Every row whose lag window touches such a month is then dropped from the model.
  if (length(blank_x) > 0) data[[paste0("d_", x)]][data$date %in% blank_x] <- NA
  parts <- list(tibble(date = data$date, d_wheat = data$d_wheat),
                make_lags(data, paste0("d_", x), lags, "x"))
  for (cn in controls) {
    parts[[length(parts) + 1]] <- make_lags(data, paste0("d_", cn), 0:1, cn)
  }
  mod_df <- dplyr::bind_cols(parts) |>
    filter(date >= from, date <= to) |>
    tidyr::drop_na()
  rhs <- setdiff(names(mod_df), c("date", "d_wheat"))
  n <- nrow(mod_df)
  if (n < length(rhs) + 8) return(NULL)             # too few observations to trust

  fit <- lm(reformulate(rhs, response = "d_wheat"), data = mod_df)
  b <- coef(fit)
  if (anyNA(b)) return(NULL)

  nw_lag <- max(1, floor(4 * (n / 100)^(2 / 9)))    # standard Newey-West bandwidth rule
  V      <- sandwich::NeweyWest(fit, lag = nw_lag, prewhite = FALSE, adjust = TRUE)
  se     <- sqrt(diag(V))
  tcrit  <- qt(0.975, fit$df.residual)

  is_x  <- startsWith(names(b), "x_l")
  coefs <- tibble(term = names(b), estimate = unname(b), se = unname(se)) |>
    filter(startsWith(term, "x_l")) |>
    mutate(lag_months = as.integer(sub("x_l", "", term)),
           ci_low = estimate - tcrit * se, ci_high = estimate + tcrit * se,
           fertiliser = x, spec = label)

  cvec   <- as.numeric(is_x)                        # picks out the fertiliser lag terms
  cum    <- sum(cvec * b)
  cum_se <- sqrt(drop(t(cvec) %*% V %*% cvec))
  summary_row <- tibble(
    fertiliser = x, spec = label,
    lags = paste0(min(lags), "-", max(lags)),
    controls = if (length(controls)) paste(controls, collapse = "+") else "none",
    sample = paste0(format(min(mod_df$date), "%Y-%m"), " to ", format(max(mod_df$date), "%Y-%m")),
    n = n, cumulative = cum, se = cum_se,
    ci_low = cum - tcrit * cum_se, ci_high = cum + tcrit * cum_se,
    p_value = 2 * pt(-abs(cum / cum_se), fit$df.residual),
    adj_r2 = summary(fit)$adj.r.squared, nw_lag = nw_lag)
  list(coefs = coefs, summary = summary_row)
}

# main specification: lags 0-6, with energy controls when available
main_fits <- map(c("dap", "urea"), function(x) {
  fit_dl(dl, x, MAIN_LAGS, controls_avail, label = spec_label(MAIN_LAGS, controls_avail))
}) |> compact()
main_coefs <- map_dfr(main_fits, "coefs")
main_cum   <- map_dfr(main_fits, "summary")

# ---- 3b. EXPLORATORY longer window (added AFTER seeing the main results) ---------
# The main window (0-6 months) was fixed before looking at the data. This 0-12 version
# only shows what happens beyond month 6. It is not a replacement for the main model.
ext_fits  <- map(c("dap", "urea"), function(x) {
  fit_dl(dl, x, 0:12, controls_avail, label = spec_label(0:12, controls_avail))
}) |> compact()
ext_coefs <- map_dfr(ext_fits, "coefs")

# ---- 4. robustness: other lag lengths, with and without controls ---------------
grid <- expand_grid(x = c("dap", "urea"), lag_max = c(3, 6, 12),
                    use_controls = c(FALSE, TRUE))
if (length(controls_avail) == 0) grid <- filter(grid, !use_controls)
robust <- pmap(grid, function(x, lag_max, use_controls) {
  ctr <- if (use_controls) controls_avail else character()
  fit_dl(dl, x, 0:lag_max, ctr, label = spec_label(0:lag_max, ctr))
}) |> compact()
robust_tbl <- map_dfr(robust, "summary")

# ---- 4b. targeted sensitivity checks on the main specification ------------------
#  (i) end the sample at SENS_END, to see whether recent months drive the result
# (ii) urea only: set the month of the World Bank benchmark change (UREA_BREAK) to missing
main_lab <- spec_label(MAIN_LAGS, controls_avail)
sens_parts <- list()
for (x in c("dap", "urea")) {
  r0 <- fit_dl(dl, x, MAIN_LAGS, controls_avail, label = main_lab)
  r1 <- fit_dl(dl, x, MAIN_LAGS, controls_avail, to = SENS_END, label = main_lab)
  if (!is.null(r0)) sens_parts[[length(sens_parts) + 1]] <- mutate(r0$summary, check = "main specification")
  if (!is.null(r1)) sens_parts[[length(sens_parts) + 1]] <- mutate(r1$summary, check = paste0("sample ends ", format(SENS_END, "%b %Y")))
}
r2 <- fit_dl(dl, "urea", MAIN_LAGS, controls_avail, label = main_lab, blank_x = UREA_BREAK)
if (!is.null(r2)) sens_parts[[length(sens_parts) + 1]] <- mutate(r2$summary, check = "urea benchmark change (Mar 2022) set to missing")
sens_tbl <- bind_rows(sens_parts)

# ---- 5. sub-periods (short samples: treat with caution) ------------------------
periods <- tibble(
  period_name = c("before shock window", "shock window", "after shock window"),
  from = c(START_DATE, SHOCK_START, SHOCK_END + 1),
  to   = c(SHOCK_START - 1, SHOCK_END, as.Date("2100-01-01"))
)
sub_tbl <- expand_grid(x = c("dap", "urea"), idx = seq_len(nrow(periods))) |>
  pmap(function(x, idx) {
    r <- fit_dl(dl, x, 0:3, character(), from = periods$from[idx], to = periods$to[idx],
                label = periods$period_name[idx])
    if (is.null(r)) NULL else mutate(r$summary, period_name = periods$period_name[idx])
  }) |> compact() |> bind_rows()

# ---- 6. Granger-type tests in both directions -----------------------------------
# "x Granger-causes y" means past x helps predict y beyond y's own past.
# It is about PREDICTIVE ORDER, not true causation.
granger <- map_dfr(c("dap", "urea"), function(x) {
  map_dfr(1:6, function(p) {
    f1 <- suppressWarnings(lmtest::grangertest(reformulate(paste0("d_", x), "d_wheat"),
                                               order = p, data = as.data.frame(core)))
    f2 <- suppressWarnings(lmtest::grangertest(reformulate("d_wheat", paste0("d_", x)),
                                               order = p, data = as.data.frame(core)))
    tibble(fertiliser = x, order = p,
           direction = c(paste0(x, " -> wheat"), paste0("wheat -> ", x)),
           F_stat = c(f1$F[2], f2$F[2]),
           p_value = c(f1$`Pr(>F)`[2], f2$`Pr(>F)`[2]))
  })
})

# ---- 7. rolling correlation (24 months) -----------------------------------------
# Wheat change this month vs the AVERAGE fertiliser change over the previous 3 months
# (ROLL_PRED_LAGS). Using past months matches the question: delayed transmission.
avg_lag <- function(x, lags) Reduce(`+`, lapply(lags, function(k) dplyr::lag(x, k))) / length(lags)
roll_fn <- function(a, b) {
  zoo::rollapplyr(cbind(a, b), ROLL_WINDOW, function(z) cor(z[, 1], z[, 2]),
                  by.column = FALSE, fill = NA)
}
rolling <- dl |>
  mutate(f_dap = avg_lag(d_dap, ROLL_PRED_LAGS), f_urea = avg_lag(d_urea, ROLL_PRED_LAGS)) |>
  filter(in_sample, !is.na(d_wheat), !is.na(f_dap), !is.na(f_urea)) |>
  mutate(dap = roll_fn(d_wheat, f_dap), urea = roll_fn(d_wheat, f_urea)) |>
  select(date, dap, urea) |>
  pivot_longer(-date, names_to = "fertiliser", values_to = "rolling_corr")

# ---- 8. save ----------------------------------------------------------------------
write_csv(stationarity, file.path(TAB_DIR, "stationarity_adf.csv"))
write_csv(lead_lag,     file.path(TAB_DIR, "lead_lag_correlations.csv"))
write_csv(main_coefs,   file.path(TAB_DIR, "distributed_lag_coefficients.csv"))
write_csv(main_cum,     file.path(TAB_DIR, "cumulative_association_main.csv"))
write_csv(robust_tbl,   file.path(TAB_DIR, "robustness_specs.csv"))
write_csv(sens_tbl,     file.path(TAB_DIR, "sensitivity_checks.csv"))
write_csv(ext_coefs,    file.path(TAB_DIR, "exploratory_lag_coefficients_0_12.csv"))
write_csv(sub_tbl,      file.path(TAB_DIR, "sub_periods.csv"))
write_csv(granger,      file.path(TAB_DIR, "granger_tests.csv"))
saveRDS(list(is_test = meta$is_test, controls = controls_avail,
             n_core = nrow(core), core = core, rolling = rolling,
             lead_lag = lead_lag, main_coefs = main_coefs, main_cum = main_cum,
             robust = robust_tbl, sens = sens_tbl, ext_coefs = ext_coefs, sub = sub_tbl, granger = granger,
             stationarity = stationarity),
        file.path(PROC_DIR, "results.rds"))

message("\nMain result (cumulative association over lags ",
        min(MAIN_LAGS), "-", max(MAIN_LAGS), "):")
print(as.data.frame(main_cum[, c("fertiliser", "controls", "n", "cumulative", "ci_low", "ci_high", "p_value")]),
      row.names = FALSE, digits = 3)
message("Tables written to ", TAB_DIR)
