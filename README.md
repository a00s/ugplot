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

### 3) Running a remote job server

Use this when you want the Shiny app on one machine and the model training jobs on another machine, or when you want long jobs to keep running outside the browser session.

Run these commands on the server machine:

```r
library(ugplot)

# Install the packages needed by the HTTP job server.
ugPlotInstallServerDeps()

# Optional: inspect caret model dependencies available on this server.
ugPlotCheckModelDeps()

# Optional: install packages for every caret model that is missing dependencies.
# This can install many R packages, so run it on the server before starting jobs.
ugPlotInstallModelDeps()

# One-command server setup including model dependencies:
# ugPlotInstallServerDeps(install_model_deps = TRUE)

ugPlotServerStart(
  host = "0.0.0.0",
  port = 8080,
  token = "change-this-token"
)
```

Manage the background server from R:

```r
ugPlotServerStatus()
ugPlotServerStop()
```

Then, in the ugPlot app:

1. Open **CONFIGURATIONS**.
2. Add the server URL, for example `http://server-address:8080`.
3. Add the same token used in `ugPlotServerStart()`.
4. In **MACHINE LEARNING**, choose **Run target → Remote server**.
5. Submit the job and monitor it in **JOBS**.
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

For a remote ugPlot server, install the missing model libraries on the server machine, not only in the R session running the browser app:

```r
ugPlotCheckModelDeps()
ugPlotInstallModelDeps()
```

### 5.4 Remote jobs and loading results

![doc18 - remote jobs overview](man/img/doc18.png)

Use **JOBS** to monitor model runs submitted to local or remote ugPlot servers.

- Server cards summarize connection state, active jobs, and version mismatches.
- The job table shows server, job name, type, state, progress, target, model list, timestamps, and actions.
- **Load** imports the selected job preview/results back into the app.
- **Stop** asks a running remote server to stop the active job.
- **Delete** removes finished job records when they are no longer needed.

If a server card shows **VERSION MISMATCH**, update the remote server package so the interface and server use the same ugPlot version.

![doc17 - remote job loaded result](man/img/doc17.png)

After loading a job result, ugPlot displays the best-result summary, metric distribution, stability information, and job logs. For multi-seed jobs, prefer the median and interquartile range over the best single seed when reporting model performance.

### 5.5 Interpreting output

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

## 8) GRAPH MODELS — visualize feature correlation networks

![doc19 - graph models 3d controls](man/img/doc19.png)

Use **GRAPH MODELS** when you want to inspect how selected variables relate to each other as a correlation network.

- **Target column (optional)** can keep the outcome visible while selecting features.
- **Max nodes** limits the graph to the most variable columns, preventing unreadable dense networks.
- **Edge threshold |correlation|** controls how strong a relationship must be before an edge is drawn.
- **Minimum degree** removes isolated or weakly connected nodes.
- **Layout** changes how nodes are positioned.
- **Render in 3D (plotly)** enables interactive rotation/zoom for spatial inspection.

The graph summary reports the number of nodes, edges, average degree, and maximum absolute correlation. Use these values as a quick density check before interpreting individual edges.

![doc20 - graph models outputs](man/img/doc20.png)

When 3D rendering is disabled or when reviewing static output, ugPlot shows a 2D feature graph, node degree distribution, and node metrics table. The download buttons export node metrics and edge lists as CSV files for external network analysis or figure preparation.

---

## 9) GEO IMPORT — methylation pipeline tutorial

![GEO IMPORT overview](man/img/geo-import-01-overview.png)

Use **GEO IMPORT** when you want ugPlot to inspect a GEO methylation accession, prepare CpG matrices, find CpGs correlated with a phenotype, build transcript-level candidate datasets, and run transcript ML models.

This page is organized as a numbered pipeline. Green **DONE** cards mean ugPlot found the required local or remote output for that step. Yellow **PENDING** means the step still needs to run, be loaded from a remote result, or be refreshed after changing a parameter.

### 9.1 Choose local or remote processing

<img src="man/img/geo-import-04-remote-processing.png" alt="GEO IMPORT remote processing controls" width="360">

The **GEO processing location** panel controls where the expensive work runs.

- **Local** runs the GEO pipeline inside the current Shiny/R session.
- **Remote server** sends the pipeline to a configured ugPlot server, keeps large artifacts on that server, and lets you load the lightweight result back into the interface.
- **Start remote GEO pipeline** submits a new remote run using the current accession and settings.
- **Refresh status** checks whether the selected remote job is still running, failed, or finished.
- **Load remote result** imports the finished remote result into the GEO IMPORT tab.

When a remote result is loaded, the blue status banner shows the active remote matrix source and remote cache path. This means downstream GEO tables and transcript ML summaries are being read from the loaded remote job metadata, while large matrices can remain on the server.

### 9.2 Step 1 — Inspect GEO accession

<img src="man/img/geo-import-02-accession.png" alt="GEO IMPORT accession step" width="360">

Enter a GEO accession, for example `GSE87571`, and click **Inspect files** or **Refresh GEO status**.

This step checks the GEO record and supplementary files. If it succeeds, the accession card turns **DONE** and the app can plan which matrix or raw IDAT files are available.

### 9.3 Step 2 — Review sample metadata

<img src="man/img/geo-import-03-metadata.png" alt="GEO IMPORT sample metadata step" width="360">

The **Sample metadata** card summarizes the phenotype table extracted from GEO.

Check:

- total sample count,
- number of metadata columns,
- likely analysis fields such as `age`, `status`, `disease state:ch1`, `gender:ch1`, or `subject_status`,
- the local cache folder used by ugPlot.

These metadata fields are the candidates for the target/correlation variable used later in Step 6 and for class/group comparisons in Step 10.

### 9.4 Step 3 — Choose matrix files

<img src="man/img/geo-import-05-matrix-files.png" alt="GEO IMPORT matrix files step" width="360">

In **Matrix files**, choose the matrix source:

- **Use GEO processed matrix** when GEO already provides a usable beta/intensity table.
- **Recalculate from raw IDAT with sesame** when you want ugPlot to use raw IDAT files and create a sesame beta matrix.

The card reports how many files were found, how much disk space they use, which files are needed for the selected workflow, and the cache folder. If **Still needed** is `0 file(s)`, ugPlot already has the required files for the selected source.

### 9.5 Step 4 — Download progress

<img src="man/img/geo-import-06-download-progress.png" alt="GEO IMPORT download progress step" width="360">

This card tracks GEO file acquisition.

For remote jobs, downloads happen on the selected remote server. For local jobs, this card reports local download/extraction status. If it says the selected files are already local, you can continue without downloading again.

### 9.6 Step 5 — Recalculate beta matrix

<img src="man/img/geo-import-07-beta-matrix.png" alt="GEO IMPORT beta matrix step" width="360">

When using raw IDAT files, Step 5 runs sesame QC/reprocessing and creates the beta matrix used by Spearman analysis.

Key settings:

- **Probe detection p-value cutoff**: default `0.05`.
- **Maximum failed pOOBAH probe fraction per sample**: default `0.05`.
- **Sesame prep code**: default `QCDPB`.

When loaded from a remote run, this card shows the remote beta matrix path, QC report path, and number of processed QC rows. If the beta matrix is available, Step 6 can scan CpGs without rerunning sesame.

### 9.7 Step 6 — Analyze CpGs

<img src="man/img/geo-import-08-analyze-cpgs.png" alt="GEO IMPORT CpG analysis step" width="360">

Step 6 computes CpG-level Spearman correlations against the selected numeric metadata field.

Key settings:

- **Metadata field to predict/correlate**: the phenotype variable, such as `age`.
- **Max CpGs to scan**: use `0` to scan all available CpGs.
- **Minimum samples per CpG for Spearman (%)**: minimum complete samples required per CpG.
- **Transcript CpG threshold |rho|**: minimum absolute Spearman correlation used to keep transcript candidates.

The summary reports how many CpGs were scanned, how many passed the sample filter, and the observed maximum `|rho|`. If the current threshold keeps transcript candidates, the green **Ready to continue transcript pipeline** box appears. If no candidate passes the threshold, lower the threshold or change the target field before continuing.

### 9.8 Step 7 — Load CpG annotation

<img src="man/img/geo-import-09-cpg-annotation.png" alt="GEO IMPORT CpG annotation step" width="360">

Step 7 loads or builds the CpG-to-gene/transcript annotation map for the GEO platform.

The card shows the detected platform, for example `GPL21145`, and the annotation cache path. This annotation is required before ugPlot can group CpGs by transcript and build transcript ML datasets.

### 9.9 Step 8 — Build transcript ML datasets

<img src="man/img/geo-import-10-transcript-datasets.png" alt="GEO IMPORT transcript dataset step" width="360">

Step 8 creates complete-case transcript datasets from the CpGs that passed Step 6 and the annotation from Step 7.

Key setting:

- **Transcript complete-case minimum samples (%)**: minimum sample retention required for a transcript dataset, default `80`.

ugPlot treats empty strings, `NA`/`na` text, true `NA`, and zero as missing for this transcript complete-case step. Transcripts that produce identical final CpG/sample datasets are grouped together to avoid repeated ML runs.

The status box reports processed groups, compatible groups, excluded groups, and the cached group summary path.

### 9.10 Step 9 — Screen transcript ML models

<img src="man/img/geo-import-11-screen-models.png" alt="GEO IMPORT transcript ML screening step" width="360">

Step 9 screens installed caret models for each transcript candidate group.

Key settings:

- **Run transcript groups with trigger |rho| >=**: only runs groups whose strongest CpG correlation reaches this threshold.
- **Limit to top Spearman-ranked groups**: optional cap for quick testing.
- **Use one representative model from four ML families**: faster exploratory mode.
- **Screening seeds per model**: number of seeds used while screening.
- **Timeout per model/seed (s)**: maximum training time for each model/seed attempt.

The model summary shows how many caret models are installed and will be screened. In remote mode, these jobs stay on the selected server until a result is loaded.

### 9.11 Step 10 — Stabilize best transcript ML

<img src="man/img/geo-import-12-stabilize-ml.png" alt="GEO IMPORT transcript ML stability step" width="360">

Step 10 takes the best screened model for each transcript group and reruns seed batches until the metric stabilizes.

Key settings:

- **Minimum stability seeds**: minimum number of seeds before stopping can be considered.
- **Maximum stability seeds**: hard upper limit.
- **Seeds compared for stability**: rolling window used to compare metric changes.
- **Max metric change to stop**: tolerance for considering the metric stable.
- **Optional class/group column for stability seeds**: runs class-aware stability summaries, useful for comparing transcript behavior across phenotype classes.

When complete, the status box reports how many transcript ML summary rows were loaded and where the final summary CSV is cached. Use the transcript ML results tables below the pipeline cards to inspect class rankings, transcript changes between selected classes, CpG-level changes, and ML importance plots.

### 9.12 Recommended GEO workflow

1. Enter the GEO accession and refresh Step 1.
2. Confirm sample metadata and choose the target field you want to analyze.
3. Choose processed matrix or raw IDAT/sesame as the matrix source.
4. Run locally only for small jobs; use a remote server for large IDAT/sesame and transcript ML runs.
5. In Step 6, start with a practical `|rho|` threshold such as `0.7`, then lower it only if no transcript candidates appear.
6. Build transcript ML datasets in Step 8.
7. Screen models in Step 9.
8. Stabilize the best models in Step 10, optionally using a disease/status class column.
9. Open the transcript ML results section to compare transcript ranking changes across classes and inspect the CpGs driving those changes.

### 9.13 GEO report tables and plots

After the pipeline cards, **GEO IMPORT** exposes collapsible report sections. These reports are meant to answer three questions:

1. Which samples, files, CpGs, and transcripts entered the analysis?
2. Which transcript groups are strongest overall?
3. Which transcript groups and CpGs change most between selected classes?

#### Sample metadata table

![GEO report sample metadata](man/img/geo-report-01-sample-metadata.png)

This table shows the parsed phenotype/sample metadata from GEO. Use it to confirm that the accession contains the expected cohorts, disease/status labels, ages, sex, tissue/cell type, and any other candidate target columns.

Important uses:

- choose the numeric field for CpG Spearman analysis, such as `age`,
- choose a class/group column for Step 10 stability summaries,
- verify sample counts and class labels before interpreting ML results.

#### GEO files table

![GEO report files](man/img/geo-report-02-files.png)

This table lists supplementary GEO files and their local/remote status.

Key columns:

- **File**: GEO supplementary file name.
- **Type**: table, archive, IDAT bundle, or other detected file type.
- **MethylationHint**: whether the file looks relevant for methylation.
- **Loadable**: whether ugPlot can load the file directly as a table.
- **LocalStatus** and **LocalSize**: download/extraction state and local cache size.
- **Action**: next useful action, such as extracted or ready to extract.

Use this table to confirm that the selected matrix source really has the files it needs.

#### CpG Spearman table

![GEO report CpG Spearman](man/img/geo-report-03-cpg-spearman.png)

This table ranks CpGs by correlation with the selected metadata field from Step 6.

Key columns:

- **CpG**: probe identifier.
- **SpearmanRho**: signed Spearman correlation with the target field.
- **PValue**: correlation p-value.
- **N**: complete sample count used for that CpG.
- **AbsRho**: absolute correlation, useful for thresholding.

If annotation is already loaded, one CpG can appear in multiple rows because the same CpG may map to multiple transcripts.

#### Transcript ML candidate groups

![GEO report transcript candidate groups](man/img/geo-report-04-candidate-groups.png)

This table lists transcript candidate groups created in Step 8. A group can represent one transcript or several compatible transcripts that produce the same final CpG/sample dataset.

Key columns:

- **Load**: loads that transcript-group dataset into the main ugPlot data workflow.
- **PrincipalTranscript** and **Gene**: representative transcript and gene.
- **Columns**: number of CpGs/features retained.
- **Samples**: complete-case samples retained.
- **TranscriptCount**: number of compatible transcripts in the group.
- **TriggerMaxAbsRho**: strongest absolute Spearman correlation that triggered the group.

Select a row to inspect the CpGs and genomic context for that transcript group.

#### Transcript genomic track

![GEO report transcript genomic track](man/img/geo-report-05-transcript-track.png)

The transcript track shows where the group CpGs fall across genomic position and transcript regions.

How to read it:

- dots are CpGs,
- color/height highlight Spearman `|rho|` and, when available, ML importance,
- shaded regions represent transcript/gene regions such as body, UTR, exon, or TSS windows,
- clustered high-signal CpGs near a regulatory region are often more interpretable than isolated points.

Use this plot to see whether the strongest CpGs are concentrated in promoter/TSS regions, gene body, UTRs, or exon regions.

#### Transcript CpG detail table

![GEO report transcript CpG table](man/img/geo-report-06-transcript-cpg-table.png)

This table gives CpG-level details for the selected transcript group.

Important columns:

- **CpGKeptForML**: whether the CpG was retained in the final ML dataset.
- **GeneRegion**, **Chr**, **Position**, **Strand**: genomic annotation.
- **CpGIslandRelation**, **RegulatoryFeature**, **ProbeType**: platform annotation context.
- **SpearmanRho**, **AbsRho**, **PValue**: association with the selected metadata field.

Use this table when you need exact CpG IDs and coordinates for downstream validation or external annotation.

#### Transcript class ranking plot

![GEO report class ranking plot](man/img/geo-report-07-class-rank-plot.png)

This plot compares transcript group ranking across selected classes. The class tags at the top define the order shown on the x-axis and can be reordered by the user.

Tabs change the ranking rule:

- **R2** ranks by class-specific ML prediction performance.
- **Spearman** ranks by the strongest transcript-triggering CpG correlation.
- **Combined** blends ML performance and Spearman rank.

Each line is a transcript group. Large vertical movement between classes means the group changes rank strongly between those classes.

#### Transcript class ranking table

![GEO report class ranking table](man/img/geo-report-08-class-rank-table.png)

This table is the numeric version of the class ranking plot. Each column is a selected class; each row is a rank position. The cell shows the group, transcript/gene, metric value, trigger `|rho|`, and trigger CpG.

Use it when the plot is crowded and you need the exact order per class.

#### Largest transcript changes

![GEO report largest transcript changes](man/img/geo-report-09-largest-transcript-changes.png)

This table focuses on the selected reference and comparison classes. By default, ugPlot uses the first and last selected class tags, but the dropdowns let you choose any pair.

Key columns:

- **Group**: transcript group ID.
- **Transcript** and **Gene**: representative transcript and gene.
- **Delta**: comparison value minus reference value for the active metric.
- **ReferenceValue** and **ComparisonValue**: metric values in each selected class.

Rows are sorted by absolute delta, so the top rows are the strongest class-dependent transcript changes for the active tab.

#### Selected transcript class comparison

![GEO report selected transcript class comparison](man/img/geo-report-10-class-comparison.png)

After selecting a transcript group, this table summarizes class-level behavior for that group.

It reports:

- class label and interpretation,
- sample count,
- number of CpGs,
- mean transcript beta,
- beta shift against the reference class,
- association rho,
- median R2 and R2 change.

This is the main report for deciding whether the transcript changes because methylation changed, ML prediction changed, or both.

#### Top CpG changes inside selected transcript

![GEO report top CpG changes](man/img/geo-report-11-top-cpg-changes.png)

This table ranks CpGs inside the selected transcript by beta difference between the reference and comparison classes.

Important columns:

- **ReferenceMeanBeta** and **ComparisonMeanBeta**: class-specific methylation means.
- **DeltaBeta** and **AbsDeltaBeta**: signed and absolute methylation shift.
- **ReferenceSamples** and **ComparisonSamples**: complete sample counts.
- **ReferenceR2** and **ComparisonR2**: transcript-level class-specific ML result.
- importance columns, when present, compare ML importance for the same CpG across classes.

Use this table to identify which CpGs are driving the class-level transcript shift.

#### Spearman vs ML importance agreement

![GEO report Spearman vs ML importance](man/img/geo-report-12-rho-importance.png)

This plot compares normalized Spearman `|rho|` with normalized ML importance for CpGs in the selected transcript group.

How to interpret it:

- points near the diagonal are CpGs supported similarly by correlation and ML importance,
- high ML importance with low `|rho|` may indicate a non-linear or model-specific signal,
- high `|rho|` with low ML importance may be redundant once other CpGs are present,
- color shows the difference between ML importance and Spearman signal.

Use this plot after selecting a transcript group to decide whether the model is relying on the same CpGs that drove the Spearman candidate selection.

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
9. Use GRAPH MODELS to inspect feature networks and export edge/node tables.
10. Validate final `.rds` model in MODEL ANALYSIS.
11. Export processed dataset and analysis tables.
