# ugplot

ugPlot is an interactive Shiny application for **loading tabular omics/biomedical datasets**, **filtering rows and variables**, **building exploratory plots (heatmaps + 2D correlation views)**, **training multiple caret machine learning models**, and **running post-training model analysis from saved `.rds` models**.

Preprint DOI: https://doi.org/10.64898/2026.02.09.704870

## Documentation and installation

For full installation details (system libraries, R dependencies, and troubleshooting), see [`INSTALLATION.md`](INSTALLATION.md).

Quick install in a clean Podman container:

```bash
podman run -it -p 127.0.0.1:8787:8787 -e DISABLE_AUTH=true docker.io/rocker/rstudio:latest bash -lc 'apt-get update && apt-get install -y --no-install-recommends build-essential gfortran pkg-config libcurl4-openssl-dev libssl-dev libxml2-dev libgit2-dev libuv1-dev libfontconfig1-dev libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev libcairo2-dev libharfbuzz-dev libfribidi-dev && R -q -e "install.packages(\"BiocManager\", repos=\"https://cloud.r-project.org\"); options(repos=c(a00s=\"https://a00s.r-universe.dev\", CRAN=\"https://cloud.r-project.org\", BiocManager::repositories())); install.packages(c(\"fs\",\"curl\",\"sass\",\"bslib\",\"rmarkdown\",\"httpuv\",\"httr\",\"htmlwidgets\",\"miniUI\",\"colourpicker\")); BiocManager::install(\"ConsensusClusterPlus\", ask=FALSE, update=FALSE); install.packages(\"ugplot\")"'
```

After installation, run in R:

```r
library(ugplot)
ugPlot()
```

---

# Full User Manual (GitHub edition)

This manual follows the numeric order of screenshots in `man/img/doc*.png` and explains all buttons and controls from each page.

## 0) App startup and general navigation

![doc1 - initial screen](man/img/doc1.png)

- The top menu is organized into six workflow tabs:
  1. **LOAD DATA**
  2. **TABLE**
  3. **HEATMAP PLOT**
  4. **2D PLOT**
  5. **MACHINE LEARNING**
  6. **MODEL ANALYSIS**
- Tabs 2–6 become useful after data is loaded and processed.

---

## 1) LOAD DATA

### Step 1.1 — Select your file

![doc2 - select CSV file](man/img/doc2.png)

Use the controls in this order:

- **Start at line**: chooses the first line to read from the file (skip metadata/header text when needed).
- **Separator**: choose delimiter (`space`, `tab`, `;`, `,`, `|`).
- **Choose a CSV file**: upload your dataset.

### Step 1.2 — Confirm uploaded content and move forward

![doc3 - load tab after upload](man/img/doc3.png)

After upload:

- Left text area shows detected **column names**.
- Right text area shows detected **row/sample IDs**.
- Buttons under each text area:
  - **Add all**: include all listed items.
  - **Remove all**: clear selected list.
  - **Join columns**: merge entered lines into a usable selection block.
- **GO TO STEP 2 (TABLE)**: load selected rows/columns into the table module.
- **Click here to load an example**: loads packaged demo data.

---

## 2) TABLE

![doc4 - table tab controls](man/img/doc4.png)

The TABLE page is the main data curation panel.

### Left panel (Columns)

- Checkbox list for all variables/features.
- **Uncheck all / Check all**: bulk toggle all feature columns.
- Numeric field + **Uncheck variability**:
  - removes (unchecks) low-variability columns according to threshold.
- **Scramble** selector + controls:
  - choose one column,
  - **Scramble column** randomizes sample order for that column,
  - **Restore** brings original values back.

### Middle panel (Rows)

- Checkbox list of samples/rows.
- **Uncheck all / Check all** for sample filtering.

### Right panel (Categories)

- Checkbox list used as category/annotation columns.
- **Transpose table**: swaps rows and columns.
- **Download**: export the currently filtered/transformed dataset.

### Bottom area

- Interactive data table with pagination/search/sort for manual inspection.

---

## 3) HEATMAP PLOT

![doc5 - heatmap plot tab](man/img/doc5.png)

This page supports gallery-driven plotting plus editable code.

- **plot_xy** selector (`ROW x COL`, `COL x COL`, `ROW x ROW`): defines plotting orientation.
- Left image gallery (large icons): selects predefined plot template.
- Left palette gallery (small strips): selects color palette.
- Top code bar:
  - **Play button** executes current plotting code.
  - **Code text area** lets you modify generated plotting command directly.
- Main area: rendered plot output.

---

## 4) 2D PLOT

### Scatter/correlation exploration

![doc6 - 2d correlations](man/img/doc6.png)

Controls:

- Correlation method selector (**pearson**, **spearman**, **kendall**).
- **Spearman Correlation >= x** slider (positive correlation threshold).
- **Negative correlation <= x** slider (negative threshold).
- Left mini-previews each have a button:
  - **Minimalist**: scatter-style compact plot.
  - **Distribution**: histogram/distribution view.
- Main panel lists all variable pairs that pass thresholds.

### Distribution rendering mode

![doc7 - 2d distribution mode](man/img/doc7.png)

When **Distribution** is chosen in a preview, matched pairs are rendered as distribution plots in the main panel.

---

## 5) MACHINE LEARNING

### Model configuration

![doc8 - machine learning seeds](man/img/doc8.png)

- **Target column**: outcome/label to predict.
- **Seeds** collapsible section:
  - Initial/Final Dataset Seed,
  - Initial/Final Training Seed.
  - Use ranges to repeat training across random seeds.
- **Timeout (s)**: max training time per process block.

### Missing data strategy

![doc9 - missing data strategy](man/img/doc9.png)

Inside **Missing Data Strategy** section:

- **Consider as missing**: mark Empty string, `NA`, and/or numeric zero as missing.
- **Zero rule exceptions**: columns where 0 is biologically/technically valid.
- **How to handle missing values**:
  - Do nothing,
  - Replace with zero,
  - KNN imputation,
  - Mean imputation,
  - missForest,
  - methyLImp2.
- **Imputation scope**:
  - impute train/test separately, or
  - impute full dataset once before split.
- Threshold filters:
  - remove columns above X% missingness,
  - remove samples above X% missingness.
- **Run exhaustive threshold scan (0-100%)**: brute-force search for better thresholds.
- Summary tables/plots report how filtering changed data.

### Model lists and execution

![doc10 - installed/missing models](man/img/doc10.png)

- **Download dataset with current thresholds (CSV)** exports the current ML-ready dataset.
- **Models installed** panel:
  - select models,
  - **Uncheck all / Check all**,
  - **RUN** to train all selected models.
- **Models missing** panel:
  - shows unavailable methods,
  - **Install libraries** attempts package installation.

### Training results and diagnostics

![doc11 - machine learning results](man/img/doc11.png)

Output area includes:

- residual plot,
- residual distribution histogram,
- ranked performance table (`R2`, `MAE`, `RMSE`, seed/scope metadata),
- detailed prediction table for selected model row.

---

## 6) MODEL ANALYSIS (from saved `.rds` model)

### Load model and configure analysis

![doc12 - model analysis initial](man/img/doc12.png)

- **Load RDS Model**: upload previously saved model.
- **Target column**: choose ground truth column in current dataset.
- Missingness rules and row-threshold filter (same logic as ML tab).
- **Confidence Threshold**: cut-off used in reliability/decision reporting.
- **Run Analysis** launches evaluation.

### Model metadata after upload

![doc13 - model details loaded](man/img/doc13.png)

After loading an `.rds`, ugPlot displays:

- model call/details,
- preprocessing compatibility info,
- detected model target (when available).

### Summary metrics and consistency checks

![doc14 - analysis summary](man/img/doc14.png)

- Missingness summary table (before/after threshold).
- Count of total/reliable/inconclusive/correct/wrong outputs.
- Accuracy summary for reliable subset.

### Final plots and downloadable table

![doc15 - analysis outputs](man/img/doc15.png)

- **Real vs Predicted** scatter plot with identity/regression lines.
- Statistics overlay (Pearson R, R², MAE, sample size).
- **Download analysis table (CSV)** exports per-sample predictions.
- Final table columns include: sample ID, ground truth, predicted value, confidence metrics, difference, status.

---

## Suggested end-to-end workflow

1. Load file in **1) LOAD DATA**.
2. Curate features/samples in **2) TABLE**.
3. Explore structure in **3) HEATMAP PLOT** and **4) 2D PLOT**.
4. Train/compare models in **5) MACHINE LEARNING**.
5. Re-open best saved model in **6) MODEL ANALYSIS** for external validation/reporting.

