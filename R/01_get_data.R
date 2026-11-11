# 01_get_data.R
# Gets the World Bank "Pink Sheet" monthly price workbook into data/raw/.
#
# The World Bank puts a changing code in the download link every month, so this
# script reads the Commodity Markets page and looks for the current link.
# If that fails (the page layout changes, no internet, a firewall), do it by hand:
#   1. Open https://www.worldbank.org/en/research/commodity-markets
#   2. Click "Monthly prices (XLS)"
#   3. Save the file as data/raw/CMO-Historical-Data-Monthly.xlsx
#   4. Re-run run_all.R
source("R/00_setup.R")

page_url <- "https://www.worldbank.org/en/research/commodity-markets"

manual_help <- function() {
  stop("\nCould not download the file automatically.\n",
       "Please download it by hand:\n",
       "  1. Open ", page_url, "\n",
       "  2. Click 'Monthly prices (XLS)'\n",
       "  3. Save it as ", RAW_FILE, "\n",
       "  4. Run run_all.R again.\n", call. = FALSE)
}

if (file.exists(RAW_FILE)) {
  message("Raw file already present, not downloading again: ", RAW_FILE)
} else {
  # The World Bank CDN rejects requests without a browser-like user agent.
  options(HTTPUserAgent = "Mozilla/5.0 (compatible; research-script)")

  html <- tryCatch(paste(readLines(page_url, warn = FALSE), collapse = "\n"),
                   error = function(e) NA_character_)
  if (is.na(html)) manual_help()

  links <- regmatches(
    html,
    gregexpr("https?://[^\"' ]+CMO-Historical-Data-Monthly\\.xlsx", html)
  )[[1]]
  if (length(links) == 0) manual_help()

  url <- links[1]
  message("Downloading: ", url)
  ok <- tryCatch(
    utils::download.file(url, RAW_FILE, mode = "wb", quiet = TRUE) == 0,
    error = function(e) FALSE, warning = function(w) FALSE
  )
  if (!isTRUE(ok) || !file.exists(RAW_FILE)) manual_help()

  # Record where and when the data came from (provenance).
  writeLines(c(
    paste("Source page:  ", page_url),
    paste("File URL:     ", url),
    paste("Downloaded on:", format(Sys.time(), "%Y-%m-%d %H:%M")),
    "Publisher:     World Bank, Commodity Markets ('Pink Sheet'), monthly nominal US$ prices"
  ), file.path(dirname(RAW_FILE), "SOURCE.txt"))
  message("Saved to ", RAW_FILE)
}
