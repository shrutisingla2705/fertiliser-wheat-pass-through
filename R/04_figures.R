# 04_figures.R
# Four charts. Titles describe WHAT is plotted, not what it shows, so they stay
# correct whatever the data turn out to say. Colours are colour-blind safe (Okabe-Ito).
source("R/00_setup.R")

d   <- readRDS(file.path(PROC_DIR, "prices_monthly.rds"))
res <- readRDS(file.path(PROC_DIR, "results.rds"))
is_test <- isTRUE(res$is_test)

PAL   <- c(wheat = "#E69F00", dap = "#0072B2", urea = "#009E73")
NAMES <- c(wheat = "Wheat (US HRW)", dap = "DAP", urea = "Urea")
SRC   <- if (is_test) "Source: synthetic test file (NOT World Bank data)." else
  "Source: World Bank Commodity Price Data (Pink Sheet), monthly, nominal US$."
wrap  <- function(x, w = 88) stringr::str_wrap(x, width = w)

mk_caption <- function(extra = NULL) {
  txt <- paste(c(SRC, extra), collapse = "\n")
  if (is_test) txt <- paste0("SYNTHETIC TEST DATA, NOT REAL RESULTS.\n", txt)
  txt
}
mk_title <- function(t) if (is_test) paste0("[TEST] ", t) else t

theme_set(
  theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"),
          plot.caption = element_text(hjust = 0, colour = if (is_test) "#C00000" else "grey30"),
          legend.position = "bottom",
          panel.grid.minor = element_blank())
)
shock_rect <- annotate("rect", xmin = SHOCK_START, xmax = SHOCK_END,
                       ymin = -Inf, ymax = Inf, alpha = 0.12, fill = "grey40")
shock_note <- sprintf("Shaded: %s to %s (analyst-defined window).",
                      format(SHOCK_START, "%b %Y"), format(SHOCK_END, "%b %Y"))
save_fig <- function(p, name) {
  f <- file.path(FIG_DIR, name)
  ggsave(f, p, width = 8, height = 5, dpi = 300, bg = "white")
  message("Saved ", f)
}

# ---- Figure 1: prices indexed to a base month -------------------------------------
base_date <- if (BASE_DATE %in% d$date) BASE_DATE else min(d$date[d$in_sample])
idx <- d |>
  filter(in_sample) |>
  select(date, wheat, dap, urea) |>
  mutate(across(c(wheat, dap, urea), ~ .x / .x[date == base_date] * 100)) |>
  pivot_longer(-date, names_to = "series", values_to = "index")

p1 <- ggplot(idx, aes(date, index, colour = series)) +
  shock_rect +
  geom_hline(yintercept = 100, linetype = 2, colour = "grey60") +
  geom_line(linewidth = 0.8) +
  scale_colour_manual(values = PAL, labels = NAMES, name = NULL) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = mk_title("Wheat and fertiliser prices, indexed"),
       subtitle = wrap(sprintf("Monthly prices, %s = 100. %s", format(base_date, "%b %Y"), shock_note)),
       x = NULL, y = "Price index", caption = mk_caption())
save_fig(p1, "fig1_indexed_prices.png")

# ---- Figure 2: rolling correlation -------------------------------------------------
roll <- filter(res$rolling, !is.na(rolling_corr))
if (nrow(roll) > 0) {
  p2 <- ggplot(roll, aes(date, rolling_corr, colour = fertiliser)) +
    shock_rect +
    geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
    geom_line(linewidth = 0.8) +
    scale_colour_manual(values = PAL[c("dap", "urea")], labels = NAMES[c("dap", "urea")], name = NULL) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    coord_cartesian(ylim = c(-1, 1)) +
    labs(title = mk_title("Rolling correlation: wheat vs earlier fertiliser price changes"),
         subtitle = wrap(sprintf("%d-month window. Wheat change this month vs the average fertiliser change over the previous %d months (log changes). %s",
                                 ROLL_WINDOW, length(ROLL_PRED_LAGS), shock_note)),
         x = NULL, y = "Correlation", caption = mk_caption())
  save_fig(p2, "fig2_rolling_correlation.png")
} else {
  message("Not enough data for the rolling correlation; skipped figure 2.")
}

# ---- Figure 3: distributed-lag coefficients ----------------------------------------
cf <- res$main_coefs
p3 <- ggplot(cf, aes(lag_months, estimate, colour = fertiliser)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.25, colour = "grey35") +
  geom_point(size = 2.4) +
  facet_wrap(~ fertiliser, labeller = as_labeller(c(dap = "DAP", urea = "Urea"))) +
  scale_colour_manual(values = PAL[c("dap", "urea")], guide = "none") +
  scale_x_continuous(breaks = sort(unique(cf$lag_months))) +
  labs(title = mk_title("Association between fertiliser and later wheat price changes"),
       subtitle = wrap(paste0("Distributed-lag estimates with 95% confidence intervals. Model: ", unique(cf$spec)[1])),
       x = "Months since the fertiliser price change",
       y = "Wheat price change (%) per 1% fertiliser change",
       caption = mk_caption("Newey-West (HAC) standard errors. Association, not proof of cause."))
save_fig(p3, "fig3_lag_coefficients.png")

# ---- Figure 4: lead-lag correlations -------------------------------------------------
ll <- res$lead_lag
bands <- ll |> group_by(fertiliser) |> summarise(ci = mean(ci95), .groups = "drop")
p4 <- ggplot(ll, aes(lag_months, correlation, fill = fertiliser)) +
  geom_col(width = 0.7) +
  geom_hline(data = bands, aes(yintercept = ci),  linetype = 3, colour = "grey30") +
  geom_hline(data = bands, aes(yintercept = -ci), linetype = 3, colour = "grey30") +
  geom_hline(yintercept = 0, colour = "grey50") +
  facet_wrap(~ fertiliser, labeller = as_labeller(c(dap = "DAP", urea = "Urea"))) +
  scale_fill_manual(values = PAL[c("dap", "urea")], guide = "none") +
  scale_x_continuous(breaks = 0:12) +
  labs(title = mk_title("Wheat price change vs fertiliser price change k months earlier"),
       subtitle = wrap("Correlation of monthly log changes. Dotted lines: approximate 95% bounds under no correlation."),
       x = "Fertiliser lag k (months)", y = "Correlation", caption = mk_caption())
save_fig(p4, "fig4_lead_lag_correlations.png")

# ---- Figure 5: EXPLORATORY 12-month window -------------------------------------------
ec <- res$ext_coefs
p5 <- ggplot(ec, aes(lag_months, estimate, colour = fertiliser)) +
  annotate("rect", xmin = min(MAIN_LAGS) - 0.5, xmax = max(MAIN_LAGS) + 0.5,
           ymin = -Inf, ymax = Inf, alpha = 0.10, fill = "grey40") +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.25, colour = "grey35") +
  geom_point(size = 2.2) +
  facet_wrap(~ fertiliser, labeller = as_labeller(c(dap = "DAP", urea = "Urea"))) +
  scale_colour_manual(values = PAL[c("dap", "urea")], guide = "none") +
  scale_x_continuous(breaks = sort(unique(ec$lag_months))) +
  labs(title = mk_title("Exploratory: the same model with a 12-month window"),
       subtitle = wrap(paste0("Added after the main results were seen. Shaded: the pre-set main window (months ",
                              min(MAIN_LAGS), " to ", max(MAIN_LAGS), "). 95% confidence intervals.")),
       x = "Months since the fertiliser price change",
       y = "Wheat price change (%) per 1% fertiliser change",
       caption = mk_caption("Newey-West (HAC) standard errors. Exploratory, not a replacement for the main model."))
save_fig(p5, "fig5_exploratory_12_months.png")
