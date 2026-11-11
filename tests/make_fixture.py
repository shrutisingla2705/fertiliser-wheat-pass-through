"""
Builds SYNTHETIC test workbooks that imitate the layout of the World Bank Pink Sheet
('Monthly Prices' sheet). ALL NUMBERS ARE RANDOM. They exist only so the R code can be
tested without internet access. They are NOT World Bank data and must never be used
for results.

What is planted on purpose (so the tests can check the code recovers it):
  * wheat log-change(t) = 0.30 * DAP(t-1) + 0.15 * DAP(t-2) + 0.02 * oil(t) + noise
    -> the TRUE cumulative DAP association over lags 0-6 is 0.45
  * urea is generated independently of wheat (a planted null)
Awkward details copied from real-world files, to test the cleaning code:
  * title/notes rows above the header, a units row below it
  * '..' used for missing values, including two months inside the sample (oil)
  * look-alike columns (Wheat SRW, Crude oil Brent, Natural gas US, TSP) that must NOT be picked
  * the last month has missing urea and gas (the pipeline must drop it)
Run:  python3 tests/make_fixture.py
"""
import numpy as np
from openpyxl import Workbook
from openpyxl.styles import Font

rng = np.random.default_rng(20260101)
months = [(y, m) for y in range(2012, 2026) for m in range(1, 13)
          if (y, m) <= (2025, 6)]
n = len(months)

def noise(sd):
    return rng.normal(0, sd, n)

r_oil = noise(0.07) + 0.002
r_gas = noise(0.09)
r_dap = noise(0.045) + 0.001
r_urea = noise(0.07) + 0.001

# planted price-spike episode in the fertiliser series
for i, (y, m) in enumerate(months):
    if (2021, 3) <= (y, m) <= (2021, 12):
        r_dap[i] += 0.04
    if (2021, 6) <= (y, m) <= (2022, 3):
        r_urea[i] += 0.05

lag = lambda a, k: np.concatenate([np.zeros(k), a[:-k]])
r_wheat = 0.30 * lag(r_dap, 1) + 0.15 * lag(r_dap, 2) + 0.02 * r_oil + noise(0.03)
r_tsp = noise(0.05)
r_pot = noise(0.04)
r_srw = r_wheat + noise(0.02)
r_brent = r_oil + noise(0.01)
r_gas_us = noise(0.08)

def levels(r, start):
    return start * np.exp(np.cumsum(r))

cols = {
    "Crude oil, average":  (levels(r_oil, 60),   "($/bbl)"),
    "Crude oil, Brent":    (levels(r_brent, 62), "($/bbl)"),
    "Natural gas, US":     (levels(r_gas_us, 3), "($/mmbtu)"),
    "Natural gas, Europe": (levels(r_gas, 8),    "($/mmbtu)"),
    "Wheat, US SRW":       (levels(r_srw, 200),  "($/mt)"),
    "Wheat, US HRW":       (levels(r_wheat, 220), "($/mt)"),
    "DAP":                 (levels(r_dap, 380),  "($/mt)"),
    "TSP":                 (levels(r_tsp, 330),  "($/mt)"),
    "Urea, E. Europe":     (levels(r_urea, 300), "($/mt)"),
    "Potassium chloride":  (levels(r_pot, 350),  "($/mt)"),
}

def build(path, rename=None):
    wb = Workbook()
    desc = wb.active
    desc.title = "Description"
    desc["A1"] = "SYNTHETIC TEST FIXTURE - NOT REAL WORLD BANK DATA"
    ws = wb.create_sheet("Monthly Prices")
    ws["A1"] = "SYNTHETIC TEST FIXTURE - NOT REAL WORLD BANK DATA"
    ws["A2"] = ("Layout imitates the World Bank Pink Sheet. All numbers are random and exist "
                "only to test the code. Nothing here may be quoted as a result.")
    ws["A3"] = "Updated on (synthetic): 2026-01-01"
    names = list(cols.keys())
    if rename:
        names = [rename.get(c, c) for c in names]
    header_row = 5
    for j, name in enumerate(names, start=2):
        ws.cell(header_row, j, name).font = Font(name="Arial", bold=True)
    for j, (_, (_, unit)) in enumerate(cols.items(), start=2):
        ws.cell(header_row + 1, j, unit)
    for i, (y, m) in enumerate(months):
        row = header_row + 2 + i
        ws.cell(row, 1, f"{y}M{m:02d}")
        for j, (_, (series, _)) in enumerate(cols.items(), start=2):
            ws.cell(row, j, round(float(series[i]), 2))
    # planted missing values written the way the World Bank writes them
    def put_missing(label, year, month):
        i = months.index((year, month))
        j = list(cols.keys()).index(label) + 2
        ws.cell(header_row + 2 + i, j, "..")
    put_missing("Crude oil, average", 2016, 3)
    put_missing("Crude oil, average", 2016, 4)
    put_missing("Urea, E. Europe", 2025, 6)       # last month incomplete -> must be dropped
    put_missing("Natural gas, Europe", 2025, 6)
    wb.save(path)
    print("wrote", path)

build("tests/fixture/PINK_SHEET_TEST_FIXTURE.xlsx")
# negative test: a file where the urea column is called something unrecognisable
build("tests/fixture/PINK_SHEET_TEST_FIXTURE_missing_urea.xlsx",
      rename={"Urea, E. Europe": "Nitrogen fertilizer X"})
