test_that("computationally equivalent transcripts remain group members", {
  build_groups <- ugplot_test_internal("ugplot_geo_build_group_tables_remote")
  progress <- data.frame(
    Transcript = c("ENST_A", "ENST_B", "ENST_C"),
    Gene = c("GENE1", "GENE1", "GENE1"),
    Status = "compatible",
    Columns = c(2L, 2L, 1L),
    Samples = c(3L, 3L, 3L),
    KeptCpGs = c("cg1;cg2", "cg1;cg2", "cg3"),
    CpGKey = c("cg1\rcg2", "cg1\rcg2", "cg3"),
    SampleKey = c("S1\rS2\rS3", "S1\rS2\rS3", "S1\rS2\rS3"),
    TriggerMaxAbsRho = c(0.9, 0.8, 0.7),
    TriggerBestCpG = c("cg1", "cg2", "cg3"),
    TriggerBestRho = c(0.9, 0.8, 0.7),
    DatasetPath = c("a.csv", "b.csv", "c.csv"),
    stringsAsFactors = FALSE
  )
  candidates <- data.frame(
    Transcript = c("ENST_A", "ENST_B", "ENST_C"),
    Gene = "GENE1", CpG = c("cg1", "cg2", "cg3"),
    stringsAsFactors = FALSE
  )

  groups <- build_groups(progress, candidates)

  expect_equal(nrow(groups$summary), 2L)
  shared <- groups$summary[groups$summary$TranscriptCount == 2L, , drop = FALSE]
  expect_equal(shared$TranscriptMembers, "ENST_A;ENST_B")
  expect_equal(shared$GeneMembers, "GENE1")
  expect_equal(shared$PrincipalTranscript, "ENST_A")
  expect_equal(shared$ExtraTranscripts, "ENST_B")
  expect_setequal(
    unique(groups$details$Transcript[groups$details$GroupID == shared$GroupID]),
    c("ENST_A", "ENST_B")
  )
})

test_that("transcripts with different effective CpGs are separate groups", {
  build_groups <- ugplot_test_internal("ugplot_geo_build_group_tables_remote")
  progress <- data.frame(
    Transcript = c("ENST_A", "ENST_B"), Gene = "GENE1", Status = "compatible",
    Columns = 1L, Samples = 2L, KeptCpGs = c("cg1", "cg2"),
    CpGKey = c("cg1", "cg2"), SampleKey = "S1\rS2",
    TriggerMaxAbsRho = c(0.9, 0.8), TriggerBestCpG = c("cg1", "cg2"),
    TriggerBestRho = c(0.9, 0.8), DatasetPath = c("a.csv", "b.csv"),
    stringsAsFactors = FALSE
  )
  candidates <- data.frame(
    Transcript = c("ENST_A", "ENST_B"), Gene = "GENE1", CpG = c("cg1", "cg2"),
    stringsAsFactors = FALSE
  )

  groups <- build_groups(progress, candidates)

  expect_equal(nrow(groups$summary), 2L)
  expect_true(all(groups$summary$TranscriptCount == 1L))
  expect_setequal(groups$summary$TranscriptMembers, c("ENST_A", "ENST_B"))
})

test_that("transcript progress rows reuse a preloaded candidate matrix", {
  build_row <- ugplot_test_internal("ugplot_geo_build_transcript_group_progress_row")
  cache_dir <- tempfile("ugplot-transcript-cache-")
  dir.create(cache_dir, recursive = TRUE)
  on.exit(unlink(cache_dir, recursive = TRUE, force = TRUE), add = TRUE)

  candidates <- data.frame(
    Transcript = c("ENST_A", "ENST_A"), Gene = "GENE1",
    CpG = c("cg1", "cg2"), AbsRho = c(0.9, 0.8),
    TriggerMaxAbsRho = c(0.9, 0.9), TriggerBestCpG = "cg1",
    TriggerBestRho = 0.9, stringsAsFactors = FALSE
  )
  metadata <- data.frame(
    sample_id = paste0("S", 1:5), age = 21:25,
    stringsAsFactors = FALSE
  )
  candidate_matrix <- data.frame(
    sample_id = metadata$sample_id, age = metadata$age,
    cg1 = seq(0.1, 0.5, by = 0.1), cg2 = seq(0.5, 0.1, by = -0.1),
    check.names = FALSE, stringsAsFactors = FALSE
  )

  row <- build_row(
    transcript_id = "ENST_A", candidates = candidates,
    matrix_files = "this-file-must-not-be-read.tsv", metadata = metadata,
    cache_dir = cache_dir, target_column = "age", min_samples_pct = 80,
    candidate_dataset = candidate_matrix
  )

  expect_equal(row$Status, "compatible")
  expect_equal(row$Columns, 2L)
  expect_equal(row$Samples, 5L)
  expect_true(file.exists(row$RawDatasetPath))
  expect_true(file.exists(row$DatasetPath))
})

test_that("transcript dataset progress reports concrete counts and ETA fields", {
  build_groups <- ugplot_test_internal("ugplot_geo_build_transcript_groups_remote")
  dataset_path <- ugplot_test_internal("ugplot_geo_transcript_dataset_path")
  cache_dir <- tempfile("ugplot-transcript-progress-")
  dir.create(cache_dir, recursive = TRUE)
  on.exit(unlink(cache_dir, recursive = TRUE, force = TRUE), add = TRUE)

  candidates <- data.frame(
    Transcript = c("ENST_A", "ENST_B"), Gene = c("GENE1", "GENE2"),
    CpG = c("cg1", "cg2"), AbsRho = c(0.9, 0.8),
    TriggerMaxAbsRho = c(0.9, 0.8), TriggerBestCpG = c("cg1", "cg2"),
    TriggerBestRho = c(0.9, 0.8), stringsAsFactors = FALSE
  )
  metadata <- data.frame(sample_id = paste0("S", 1:5), age = 21:25)
  for (transcript_id in candidates$Transcript) {
    cpg <- candidates$CpG[candidates$Transcript == transcript_id]
    raw <- data.frame(
      sample_id = metadata$sample_id, age = metadata$age,
      value = seq(0.1, 0.5, by = 0.1), stringsAsFactors = FALSE
    )
    names(raw)[[3]] <- cpg
    path <- dataset_path(cache_dir, transcript_id, "age", "processed", raw = TRUE)
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    utils::write.csv(raw, path, row.names = FALSE)
  }
  updates <- list()

  result <- build_groups(
    candidates = candidates, matrix_files = "must-not-be-read.tsv",
    metadata = metadata, cache_dir = cache_dir, target_column = "age",
    threshold = 0.8, min_samples_pct = 80,
    progress_callback = function(value, message, stage_progress) {
      updates[[length(updates) + 1L]] <<- list(
        value = value, message = message, stage = stage_progress
      )
    }
  )

  expect_equal(nrow(result$progress), 2L)
  expect_true(length(updates) >= 2L)
  final <- updates[[length(updates)]]
  expect_match(final$message, "2/2")
  expect_match(final$message, "0 remaining")
  expect_equal(final$stage$name, "transcript_datasets")
  expect_equal(final$stage$completed, 2L)
  expect_equal(final$stage$total, 2L)
  expect_equal(final$stage$remaining, 0L)
  expect_true(is.finite(final$stage$rate_per_min))
  expect_true(is.finite(final$stage$eta_seconds))
})

test_that("Ensembl annotation preserves every overlapping transcript", {
  skip_if_not_installed("GenomicRanges")
  skip_if_not_installed("IRanges")
  expand_transcripts <- ugplot_test_internal("ugplot_geo_expand_ensembl_transcripts")
  probes <- data.frame(
    CpG = c("cg_body_shared", "cg_tx_a_only", "cg_promoter_b"),
    Gene = "OLD_GENE", Transcript = "NM_OLD", EnsemblTranscript = NA_character_,
    GeneRegion = "Body", Chr = "chr6", Position = c(150L, 220L, 390L),
    Strand = "+", CpGIslandRelation = "Island", RegulatoryFeature = "",
    ProbeType = "II", Platform = "GPL_TEST", Genome = "hg19",
    AnnotationSource = "platform", stringsAsFactors = FALSE
  )
  tx <- GenomicRanges::GRanges(
    seqnames = c("6", "6"),
    ranges = IRanges::IRanges(start = c(100L, 100L), end = c(250L, 180L)),
    strand = "+"
  )
  S4Vectors::mcols(tx)$tx_id <- c("ENST_A", "ENST_B")
  S4Vectors::mcols(tx)$gene_name <- c("GENE1", "GENE1")

  expanded <- expand_transcripts(
    probes, tx, "Ensembl test", promoter_upstream = 20L, promoter_downstream = 5L
  )

  expect_setequal(
    expanded$Transcript[expanded$CpG == "cg_body_shared"],
    c("ENST_A", "ENST_B")
  )
  expect_equal(expanded$Transcript[expanded$CpG == "cg_tx_a_only"], "ENST_A")
  expect_true(all(expanded$Gene[expanded$CpG == "cg_body_shared"] == "GENE1"))
  expect_true(all(expanded$TranscriptSource[expanded$CpG == "cg_body_shared"] == "Ensembl test"))
})
