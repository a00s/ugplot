# ugplot

ugPlot is an interactive Shiny application to **prepare omics/biomedical tabular datasets**, **explore patterns visually**, **train and compare machine learning models**, and **analyze saved models on new data**.

Preprint DOI: https://doi.org/10.64898/2026.02.09.704870

---

## Installation

### 1) Recommended (main) installation: prebuilt Podman image

This is the easiest and most reliable way to run ugPlot with all dependencies already configured.

```bash
podman pull ghcr.io/a00s/ugplot:latest
podman run --name ugplot -p 3838:3838 ghcr.io/a00s/ugplot:latest
podman start -a ugplot

---- you are using mac you need to run this first ----
podman machine init
podman machine start
```

Then open in your browser:

- `http://127.0.0.1:3838`

> Why this is the main path: it avoids local R/system library conflicts and is ideal for users who just want to run the app quickly.

### 2) Alternative installation from R (for users who need package-level imports/workflows)

Use this when you want to integrate ugPlot into your own R environment and scripts.

```r
install.packages(
  "ugplot",
  repos = c("https://a00s.r-universe.dev", "https://cloud.r-project.org")
)
```

Run in R:

```r
library(ugplot)
ugPlot()
```
---

# How-To Manual

## What ugPlot is best for

Use ugPlot when you need to:

1. Load a matrix-like dataset (samples × features).
2. Select/filter rows and columns without coding.
3. Explore data structure (heatmaps, correlations, distributions).
4. Train many caret models and compare metrics.
5. Save/load models and validate predictions in a reproducible way.

---

## 0) Interface overview

![doc1 - initial screen](man/img/doc1.png)

The workflow is linear:

1. **LOAD DATA**
2. **TABLE**
3. **HEATMAP PLOT**
4. **2D PLOT**
5. **MACHINE LEARNING**
6. **MODEL ANALYSIS**
7. **DEEP LEARNING**

A practical recommendation: always finish row/column cleanup in **TABLE** before training models.

---

## 1) LOAD DATA — bring your dataset correctly

### 1.1 Choose file format settings first

![doc2 - select CSV file](man/img/doc2.png)

- **Start at line**: if your file has metadata in first lines, start later (e.g., 2, 3, 4...).
- **Separator**: pick the true delimiter of your file (`;`, `,`, tab, etc.).
- **Choose a CSV file**: upload your file.

If separator/line start is wrong, downstream columns may be broken. Fix this here before moving on.

### 1.2 Confirm detected columns and rows

![doc3 - load tab after upload](man/img/doc3.png)

After upload, ugPlot shows:

- Left box: detected **columns/features**.
- Right box: detected **rows/samples**.

Buttons below each box:

- **Add all**: include everything.
- **Remove all**: clear all selections.
- **Join columns**: normalize text list into a merged selection.

Then click **GO TO STEP 2 (TABLE)**.

If you just want to test the app first, click **Click here to load an example**.

---

## 2) TABLE — the most important preprocessing page

![doc4 - table tab controls](man/img/doc4.png)

Think of this tab as your "quality gate" before plotting/training.

### 2.1 Columns panel (features)

- Checkboxes let you keep/remove features.
- **Uncheck all / Check all**: bulk operations.
- **Uncheck variability** with numeric threshold:
  - removes low-information columns.
  - useful to reduce noisy/near-constant features.
- **Scramble column**:
  - selects one feature and randomizes it across samples,
  - useful for quick sanity checks / negative controls.
- **Restore** reverts scrambled data.

### 2.2 Rows panel (samples)

- Select only the samples you want to keep.
- Use **Uncheck all / Check all** for fast cohort filtering.

### 2.3 Categories panel

- Select columns that should behave like category/annotation fields.
- **Transpose table** swaps rows/columns (useful when dataset orientation is opposite of expected).
- **Download** exports exactly what is currently selected.

### 2.4 Bottom data table

Use search/sort to inspect suspicious values before plotting or ML.

---

## 3) HEATMAP PLOT — fast visual diagnostics + editable code

![doc5 - heatmap plot tab](man/img/doc5.png)

- **plot_xy** controls matrix orientation (`ROW x COL`, `COL x COL`, `ROW x ROW`).
- Left gallery (large images): choose plot template.
- Palette strips: choose color scheme.
- Top code area:
  - **Play button** runs the displayed plotting code.
  - editable code box allows fine customization.

Best practice: choose a template close to your goal, then refine code in the text area.

---

## 4) 2D PLOT — discover pairwise relationships

### 4.1 Correlation filtering

![doc6 - 2d correlations](man/img/doc6.png)

- Correlation method: `pearson`, `spearman`, `kendall`.
- Positive threshold slider (`>= x`).
- Negative threshold slider (`<= x`).
- Left mini-buttons:
  - **Minimalist** for scatter-style quick reading.
  - **Distribution** for histogram-like views.

Main panel displays only pairs that pass your thresholds.

### 4.2 Distribution mode example

![doc7 - 2d distribution mode](man/img/doc7.png)

Use distribution mode to check whether correlations may be driven by skewed ranges/outliers.

---

## 5) MACHINE LEARNING — train, compare, and troubleshoot models

### 5.1 Core setup

![doc8 - machine learning seeds](man/img/doc8.png)

- **Target column**: variable to predict (class or numeric).
- **Seeds** section:
  - initial/final dataset seed,
  - initial/final training seed,
  - supports repeatability and robustness checks.
- **Timeout (s)** controls maximum training time.

### 5.2 Missing data strategy (critical for robust models)

![doc9 - missing data strategy](man/img/doc9.png)

You can define what missing means and how to process it:

- Missing definitions: Empty string, `NA`, and/or zero.
- **Zero rule exceptions** for columns where zero is valid.
- Imputation method:
  - none,
  - replace zero,
  - KNN,
  - mean,
  - missForest,
  - methyLImp2.
- Imputation scope:
  - train/test separately,
  - or whole dataset before split.
- Missingness thresholds to remove columns/samples.
- **Run exhaustive threshold scan (0-100%)** to test many threshold combinations automatically.

Read the summary table/plots before running models, to confirm filtering did not distort target distribution.

### 5.3 Installing missing model libraries (step-by-step)

![doc10 - installed/missing models](man/img/doc10.png)

When models appear in **Models missing**, do this:

1. In **Models missing**, click **Check all** (or select specific models).
2. Click **Install libraries**.
3. Wait for installation logs to finish.
4. Re-check the model lists; installed models should move to **Models installed**.
5. In **Models installed**, click **Check all** (or keep a subset).
6. Click **RUN**.

Also available on this page:

- **Download dataset with current thresholds (CSV)** for reproducibility.
- Side-by-side panels for installed vs missing models.

### 5.4 Interpreting output

![doc11 - machine learning results](man/img/doc11.png)

After running:

- residual plot: checks error structure,
- residual histogram: checks bias spread,
- ranking table: compare `R2`, `MAE`, `RMSE`,
- detailed prediction table per selected model.

Tip: prefer models with stable performance across seed combinations, not only best single score.

---

## 6) MODEL ANALYSIS — validate saved `.rds` models

### 6.1 Initial setup

![doc12 - model analysis initial](man/img/doc12.png)

- Upload model with **Load RDS Model**.
- Select dataset **Target column** (ground truth).
- Configure missingness definition + sample threshold.
- Set **Confidence Threshold**.
- Click **Run Analysis**.

### 6.2 Model metadata view

![doc13 - model details loaded](man/img/doc13.png)

ugPlot displays model call/statistics, compatibility/preprocess notes, and inferred target details when available.

### 6.3 Summary and reliability report

![doc14 - analysis summary](man/img/doc14.png)

Inspect:

- missingness before/after threshold,
- reliable/inconclusive counts,
- accuracy summary in reliable subset.

### 6.4 Final outputs and export

![doc15 - analysis outputs](man/img/doc15.png)

- Real vs Predicted scatter (identity + regression lines).
- Correlation/performance stats overlay.
- **Download analysis table (CSV)** for downstream reporting.
- Per-sample table with truth, prediction, confidence, error, and status.

---

## 7) DEEP LEARNING — train neural networks with `torch`

![doc16 - deep learning tab](man/img/doc16.png)

Use this tab when you want a configurable neural network pipeline directly inside ugPlot.

### 7.1 Training configuration (left panel)

- **Target column**: variable to predict.
- **Task type**:
  - **Auto-detect** (recommended): ugPlot infers classification vs regression.
  - **Classification** or **Regression**: force task mode manually.
- **Test split (%)**: holdout size for evaluation.
- **Random seed**: reproducibility for split/training.
- **Epochs** and **Batch size**: training duration and update granularity.
- **Number of hidden layers** + hidden units/dropout controls:
  - define model depth/width,
  - tune regularization to reduce overfitting.
- **Learning rate** and **Weight decay (L2)**: optimizer stability + regularization.
- **Scale numeric target (regression)**: stabilizes regression training for wide target ranges.
- **Auto adjust hidden layer sizes**: quickly generates a reasonable architecture from feature count.
- **Train Deep Learning model**: starts the full preprocessing + training + evaluation cycle.

### 7.2 Outputs and diagnostics (right panel)

After training, ugPlot shows:

- **Training log** with preprocessing/training status messages.
- **Loss curves** (train vs test) across epochs.
- **Metric panel** with task-relevant performance indicators.
- **Tuning tips** to help diagnose underfitting/overfitting.
- **Network view**:
  - model shape summary,
  - path/connection visualization,
  - path weight table,
  - weight distribution plot.
- **Metrics table** and **Predictions table** for per-run and per-sample inspection.

Practical tip: start with auto architecture + moderate epochs, then tune hidden units/dropout and learning rate based on loss gap and prediction quality.

---

## Suggested end-to-end workflow (quick checklist)

1. Install with Podman image (recommended).
2. Load file and verify separator/start line.
3. Clean rows/columns in TABLE.
4. Explore patterns in HEATMAP and 2D tabs.
5. Configure missing data strategy carefully.
6. Install missing libraries (Check all → Install libraries).
7. Train multiple models and compare metrics.
8. Optionally refine results with DEEP LEARNING (`torch`) and inspect network diagnostics.
9. Validate final `.rds` model in MODEL ANALYSIS.
10. Export processed dataset and analysis tables.
