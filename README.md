# GDE Equivalence Assessment Shiny Dashboard Prototype v0.23

This version keeps the dashboard style but fixes the run trigger problem from v0.5 by moving file upload, configuration and the Run button into the dashboard body instead of placing inputs inside the `sidebarMenu`.

## Required packages

```r
install.packages(c(
  "shiny",
  "shinydashboard",
  "DT",
  "dplyr", "tidyr", "purrr", "ggplot2", "readxl", "readr",
  "openxlsx", "tibble", "stringr", "rlang", "yaml"
))
```

Quarto is required only if `Render HTML report` is selected:

```r
system2("quarto", "--version")
```

## Run

```r
shiny::runApp("GDE_v023")
```

or from inside the app folder:

```r
shiny::runApp(".")
```

## Notes

- Use the `Data & run` page to upload the dataset and launch the assessment.
- The sidebar is now only for navigation.
- The workflow engine is still `equivalence_workflow_v1_60.R`.


## v0.23 change

Tables displayed in the Shiny app are now formatted for readability:

- ordinary numeric columns are rounded to 2 decimal places;
- count-like columns such as `n`, `n_LV`, `n_pairs`, `n_dates`, `n_observations` are shown as integers.


## v0.23 changes

- Shiny tables are now more report-like: compact column selection, clearer names and status colouring.
- Added dedicated Excel workbook download.
- Added friendlier error messages for common problems.
- The workflow engine is updated to v1.61.
- Daily method-level logic was corrected:
  - a site/campaign FAIL remains dominant;
  - if a CM unit has at least one PASS site/campaign and the remaining site/campaigns are LOW N, its daily result is treated as PASS;
  - if two CM units are present, both CM units must satisfy this CM-level rule for the method-level daily result to be PASS.


## v0.23 changes

- Rebuilt from the stable v0.8 dashboard app.
- Added dynamic dropdown filters for Country, Instrument, Pollutant and Site/Campaign.
- Fixed the UI structure so `header`, `sidebar`, `body` and `ui` are explicitly defined in the correct order.


## v0.23 fix

- Fixed a malformed UI separator line in `app.R` that prevented the `header` object from being created.
- Dynamic filter dropdowns are retained.


## v0.23 changes

- Candidate-method diagnostics no longer display reference-method duplicate plots.
- RM duplicate plots are confined to the Reference screening section.
- Main plots are shown directly within each assessment section instead of requiring a long plot-selection menu.
- The `All generated plots` tab still keeps the full plot selector for manual inspection.


## v0.23 changes

- Dropdown filters now define the actual dataset subset used for the whole workflow run.
- If PM10 is selected, tables, plots, Excel, HTML and ZIP are generated only for PM10.
- If a Site/Campaign is selected, the assessment is run only on that site/campaign subset.
- The app writes a `filtered_input_for_run.csv` file in the run input folder for traceability.
- The run status reports how many rows were retained after filtering.


## v0.23 changes

- Table headers and cell contents are centred in the Shiny app.
- DT tables use centred column definitions and a header callback to keep header/body alignment stable with horizontal scrolling.
- Base Shiny tables are also centred through CSS.


## v0.23 changes

- Fixed the candidate-method site/campaign diagnostic plot selection so it does not repeat the difference-vs-reference plot.
- RM duplicate plots remain excluded from candidate-method diagnostics.
- Improved DT header/body alignment with horizontal scrolling by forcing centred headers, centred cells, column adjustment on draw, and stronger table width handling.


## v0.23 changes

- Candidate-method diagnostic plots are now generated side-by-side for `No correction` and `After correction`.
- The side-by-side plots are generated from `01_clean_data/long_stage_data.csv`.
- Axes are kept coherent across stages:
  - same x/y limits for difference vs reference;
  - same x-axis for histograms;
  - same x/y limits for time series;
  - same x-axis for site/campaign mean-bias diagnostics.
- A `04_plots/plot_manifest.csv` file is generated and used by Shiny to select candidate-method diagnostic plots.
- The plot manifest is shown in the `All generated plots` page for traceability.


## v0.23 changes

- Side-by-side candidate-method diagnostic plot generation is more robust.
- The app now generates `04_plots/plot_manifest.csv` both immediately after the workflow run and on demand if the manifest is missing.
- A diagnostic log is saved to `04_plots/plot_manifest_generation_log.txt`.
- Minor column-name variations in `long_stage_data.csv` are tolerated.
- Date parsing is more robust.


## v0.23 changes

- Removed the non-existing `Correction coefficients` plot box.
- The linearity/correction plot now uses the full page width.
- Fixed Daily LV unit plot selection (`daily_primary_uncertainty.png`).
- Fixed Annual unit plot selection (`annual_primary_uncertainty.png`).
- Rebuilt side-by-side candidate-method manifest generation with simpler ggplot code and explicit logging.
- Manifest path lookup now tries both absolute `path` and `relative_path`.


## v0.23 changes

- Restored informative colours in candidate-method side-by-side plots.
- Difference-vs-reference and time-series plots colour points by diagnostic category.
- Histogram and site/campaign plots use stage colours.
- Manifest lookup now falls back from `plot_id` to `plot_type`, so histogram and time-series plots are displayed even if a manifest naming mismatch occurs.


## v0.23 changes

- Workflow run folders are now written under `tempdir()/GDE_Shiny_runs` instead of inside the app folder.
- This avoids Windows path-length and write-permission issues when the app is extracted inside a deeply nested Downloads/ZIP folder.
- The app still provides ZIP, HTML, Excel and YAML downloads from the interface.
- The run folder path is shown in the Run status box after completion.


## v0.23 changes

- The app folder is now simply `GDE_v023`.
- Extract the folder directly to a short path, for example:

```r
shiny::runApp("C:/Users/claud/Downloads/GDE_v023")
```

- Run outputs are written to:

```text
GDE_v023/runs/run_YYYYMMDD_HHMMSS/output_equivalence
```

- The initial comment block in `app.R` now contains a short changelog for the version.
- This replaces the v0.21 approach of writing runs to `tempdir()`.


## v0.23 changes

- The app folder is now `GDE_v023`.
- Candidate-method side-by-side plots now use the same outlier-category colours as the single diagnostic plots:
  - `OK` = darkgreen
  - `Low (>2 µg/m³)` = orange
  - `High (>25% RM)` = red
- This specifically affects:
  - `Candidate method difference vs reference concentration: raw vs corrected`
  - `Candidate method differences over time: raw vs corrected`
