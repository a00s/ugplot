test_that("cached GEO sample metadata is reused without a network request", {
  cache_dir <- tempfile("ugplot-geo-metadata-")
  dir.create(cache_dir, recursive = TRUE)
  on.exit(unlink(cache_dir, recursive = TRUE, force = TRUE), add = TRUE)
  expected <- data.frame(
    sample_id = c("GSM1", "GSM2"), age = c(41, 57),
    stringsAsFactors = FALSE
  )
  saveRDS(
    expected,
    ugplot_test_internal("ugplot_geo_sample_metadata_path")(cache_dir, "rds")
  )

  actual <- ugplot_test_internal("ugplot_geo_fetch_sample_metadata")(
    "THIS_ACCESSION_MUST_NOT_BE_FETCHED", cache_dir
  )

  expect_equal(actual, expected)
})

test_that("cached GEO metadata CSV is used when its RDS is unreadable", {
  cache_dir <- tempfile("ugplot-geo-metadata-csv-")
  dir.create(cache_dir, recursive = TRUE)
  on.exit(unlink(cache_dir, recursive = TRUE, force = TRUE), add = TRUE)
  metadata_path <- ugplot_test_internal("ugplot_geo_sample_metadata_path")
  writeLines("not an rds", metadata_path(cache_dir, "rds"))
  expected <- data.frame(sample_id = c("GSM1", "GSM2"), stage = c("II", "III"))
  utils::write.csv(expected, metadata_path(cache_dir, "csv"), row.names = FALSE)

  actual <- ugplot_test_internal("ugplot_geo_fetch_sample_metadata")(
    "THIS_ACCESSION_MUST_NOT_BE_FETCHED", cache_dir
  )

  expect_equal(actual, expected)
})
