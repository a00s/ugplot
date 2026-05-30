test_that("GEO IDAT Red/Grn files are paired by prefix", {
  cache_dir <- tempfile("geo_idat_")
  dir.create(cache_dir)
  red_path <- file.path(cache_dir, "GSM123456_200001_Red.idat.gz")
  grn_path <- file.path(cache_dir, "GSM123456_200001_Grn.idat.gz")
  incomplete_path <- file.path(cache_dir, "GSM234567_200001_Red.idat.gz")
  file.create(red_path, grn_path, incomplete_path)

  pairs <- ugplot_geo_idat_pairs(cache_dir)

  expect_equal(nrow(pairs), 2)
  complete <- pairs[pairs$Sample == "GSM123456", , drop = FALSE]
  expect_true(complete$Complete[[1]])
  expect_equal(complete$RedPath[[1]], red_path)
  expect_equal(complete$GrnPath[[1]], grn_path)
  expect_false(pairs$Complete[pairs$Sample == "GSM234567"][[1]])
})

test_that("sesame beta matrix cache is recognized as a GEO matrix file", {
  cache_dir <- tempfile("geo_sesame_")
  dir.create(cache_dir)
  beta_matrix <- matrix(
    c(0.1, 0.2, 0.3, 0.4),
    nrow = 2,
    dimnames = list(c("cg00000001", "cg00000002"), c("GSM1", "GSM2"))
  )
  beta_path <- ugplot_geo_write_beta_matrix_tsv(beta_matrix, ugplot_geo_sesame_beta_path(cache_dir))

  matrix_files <- ugplot_geo_matrix_files(cache_dir, source = "raw_sesame")

  expect_true(beta_path %in% matrix_files)
})

test_that("sesame beta matrix GSM columns match GEO sample metadata", {
  cache_dir <- tempfile("geo_sesame_map_")
  dir.create(cache_dir)
  beta_matrix <- matrix(
    c(0.1, 0.2, 0.3, 0.4),
    nrow = 2,
    dimnames = list(c("cg00000001", "cg00000002"), c("GSM1", "GSM2"))
  )
  beta_path <- ugplot_geo_write_beta_matrix_tsv(beta_matrix, ugplot_geo_sesame_beta_path(cache_dir))
  metadata <- data.frame(
    sample_id = c("GSM1", "GSM2"),
    title = c("case 1", "case 2"),
    age = c(10, 20),
    stringsAsFactors = FALSE
  )

  sample_map <- ugplot_geo_matrix_sample_map(beta_path, metadata)

  expect_equal(nrow(sample_map), 2)
  expect_equal(sample_map$MatrixSample, c("GSM1", "GSM2"))
  expect_equal(sample_map$SampleID, c("GSM1", "GSM2"))
})

test_that("sesame IDAT QC defaults to sesame pOOBAH cutoff", {
  expect_equal(formals(ugplot_geo_reprocess_idats_sesame)$detection_p, 0.05)
})

test_that("Spearman scan excludes CpGs below minimum matched samples", {
  cache_dir <- tempfile("geo_spearman_min_")
  dir.create(cache_dir)
  matrix_path <- file.path(cache_dir, "matrix.tsv")
  matrix_df <- data.frame(
    ID_REF = c("cg_keep", "cg_drop"),
    GSM1 = c(0.1, 0.1),
    GSM2 = c(0.2, 0.2),
    GSM3 = c(0.3, 0.3),
    GSM4 = c(0.4, NA),
    GSM5 = c(0.5, NA),
    check.names = FALSE
  )
  utils::write.table(matrix_df, matrix_path, sep = "\t", quote = FALSE, row.names = FALSE, na = "NA")
  metadata <- data.frame(
    sample_id = paste0("GSM", 1:5),
    age = c(10, 20, 30, 40, 50),
    stringsAsFactors = FALSE
  )

  results <- ugplot_geo_spearman_scan(
    matrix_files = matrix_path,
    metadata = metadata,
    target_column = "age",
    min_matched_samples = 4
  )

  expect_equal(results$CpG, "cg_keep")
  expect_equal(results$N, 5)
  expect_equal(attr(results, "min_matched_samples"), 4)
})
