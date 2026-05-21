ugplot_geo_cache_dir <- function(accession) {
  safe_accession <- gsub("[^A-Za-z0-9_.-]", "_", accession)
  file.path(ugplot_geo_cache_root("downloads"), safe_accession)
}

ugplot_geo_manifest_path <- function(cache_dir) {
  file.path(cache_dir, "ugplot_geo_manifest.rds")
}

ugplot_geo_sample_metadata_path <- function(cache_dir, extension = "rds") {
  file.path(cache_dir, paste0("ugplot_geo_sample_metadata.", extension))
}

ugplot_geo_project_root <- function() {
  base_dir <- getwd()
  if (basename(base_dir) == "R" && file.exists(file.path(dirname(base_dir), "DESCRIPTION"))) {
    base_dir <- dirname(base_dir)
  }
  base_dir
}

ugplot_geo_annotation_cache_dir <- function() {
  ugplot_geo_cache_root("annotation")
}

ugplot_geo_cache_root <- function(type = c("downloads", "annotation")) {
  type <- match.arg(type)
  env_var <- if (identical(type, "downloads")) "UGPLOT_GEO_DOWNLOAD_DIR" else "UGPLOT_GEO_ANNOTATION_DIR"
  configured_dir <- Sys.getenv(env_var, unset = "")
  if (nzchar(configured_dir)) {
    return(normalizePath(configured_dir, mustWork = FALSE))
  }
  user_cache <- tryCatch(
    tools::R_user_dir("ugplot", which = "cache"),
    error = function(e) file.path(path.expand("~"), ".cache", "ugplot")
  )
  file.path(user_cache, if (identical(type, "downloads")) "geo_downloads" else "geo_annotation_cache")
}

ugplot_geo_annotation_cache_path <- function(platform_id, extension = "rds") {
  safe_platform <- gsub("[^A-Za-z0-9_.-]", "_", platform_id %||% "unknown_platform")
  file.path(ugplot_geo_annotation_cache_dir(), paste0(safe_platform, "_cpg_gene_transcript_map.", extension))
}

ugplot_geo_append_log <- function(current_log, message) {
  timestamp <- format(Sys.time(), "%H:%M:%S")
  paste(c(current_log, paste0("[", timestamp, "] ", message)), collapse = "\n")
}

ugplot_format_bytes <- function(bytes) {
  bytes <- suppressWarnings(as.numeric(bytes))
  if (length(bytes) == 0 || is.na(bytes) || !is.finite(bytes) || bytes < 0) {
    return("unknown")
  }
  units <- c("B", "KB", "MB", "GB", "TB")
  unit_index <- 1
  while (bytes >= 1024 && unit_index < length(units)) {
    bytes <- bytes / 1024
    unit_index <- unit_index + 1
  }
  paste0(round(bytes, if (unit_index == 1) 0 else 2), " ", units[[unit_index]])
}

ugplot_geo_size_bytes <- function(file_table) {
  if (!is.data.frame(file_table) || nrow(file_table) == 0) {
    return(numeric(0))
  }
  if ("SizeBytes" %in% names(file_table)) {
    size_bytes <- suppressWarnings(as.numeric(file_table$SizeBytes))
  } else {
    size_bytes <- suppressWarnings(as.numeric(file_table$SizeMB) * 1024^2)
  }
  size_bytes
}

ugplot_geo_normalize_url <- function(url) {
  sub("^ftp://ftp\\.ncbi\\.nlm\\.nih\\.gov/", "https://ftp.ncbi.nlm.nih.gov/", url)
}

ugplot_geo_gzip_valid <- function(path) {
  if (!file.exists(path) || !grepl("\\.gz(\\.|$)", basename(path), ignore.case = TRUE)) {
    return(TRUE)
  }
  gzip_path <- Sys.which("gzip")
  if (nzchar(gzip_path)) {
    return(system2(gzip_path, c("-t", path), stdout = FALSE, stderr = FALSE) == 0)
  }
  input <- gzfile(path, "rb")
  on.exit(try(close(input), silent = TRUE), add = TRUE)
  ok <- tryCatch({
    repeat {
      chunk <- readBin(input, what = "raw", n = 1024^2)
      if (length(chunk) == 0) {
        break
      }
    }
    TRUE
  }, warning = function(e) FALSE, error = function(e) FALSE)
  isTRUE(ok)
}

ugplot_geo_remote_file_size <- function(url) {
  url <- ugplot_geo_normalize_url(url)
  if (!requireNamespace("curl", quietly = TRUE)) {
    return(NA_real_)
  }
  tryCatch({
    handle <- curl::new_handle(nobody = TRUE)
    response <- curl::curl_fetch_memory(url, handle = handle)
    headers <- rawToChar(response$headers)
    content_length <- grep("^Content-Length:", strsplit(headers, "\r?\n")[[1]], value = TRUE, ignore.case = TRUE)
    if (length(content_length) == 0) {
      return(NA_real_)
    }
    as.numeric(trimws(sub("^Content-Length:\\s*", "", content_length[[length(content_length)]], ignore.case = TRUE)))
  }, error = function(e) NA_real_)
}

ugplot_append_file <- function(source, destination, chunk_size = 8 * 1024^2) {
  input <- file(source, "rb")
  on.exit(close(input), add = TRUE)
  output <- file(destination, "ab")
  on.exit(close(output), add = TRUE)
  repeat {
    chunk <- readBin(input, what = "raw", n = chunk_size)
    if (length(chunk) == 0) {
      break
    }
    writeBin(chunk, output)
  }
}

ugplot_geo_download_file <- function(url, destination, expected_size = NA_real_, progress_callback = NULL) {
  url <- ugplot_geo_normalize_url(url)
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  temp_destination <- paste0(destination, ".part")
  resume_destination <- paste0(destination, ".resume")
  expected_size <- suppressWarnings(as.numeric(expected_size))

  if (file.exists(destination) && is.finite(expected_size) && file.info(destination)$size == expected_size && ugplot_geo_gzip_valid(destination)) {
    return(list(status = "complete", path = destination, bytes = file.info(destination)$size))
  }
  if (file.exists(temp_destination) && is.finite(expected_size) && file.info(temp_destination)$size == expected_size && ugplot_geo_gzip_valid(temp_destination)) {
    if (file.exists(destination)) {
      unlink(destination)
    }
    file.rename(temp_destination, destination)
    return(list(status = "completed_from_partial", path = destination, bytes = file.info(destination)$size))
  }
  if (file.exists(temp_destination) && is.finite(expected_size) && file.info(temp_destination)$size == expected_size && !ugplot_geo_gzip_valid(temp_destination)) {
    unlink(temp_destination)
  }
  if (file.exists(temp_destination) && is.finite(expected_size) && file.info(temp_destination)$size > expected_size) {
    stop(paste0(
      "Partial file is larger than expected: ", temp_destination,
      " (", ugplot_format_bytes(file.info(temp_destination)$size), " vs ",
      ugplot_format_bytes(expected_size), "). Remove it manually before retrying."
    ))
  }

  partial_start <- if (file.exists(temp_destination)) file.info(temp_destination)$size else 0
  if (file.exists(resume_destination)) {
    unlink(resume_destination)
  }
  if (requireNamespace("curl", quietly = TRUE)) {
    handle_options <- list(
      noprogress = FALSE,
      progressfunction = function(download, upload) {
        if (!is.null(progress_callback) && length(download) >= 2) {
          progress_callback(partial_start + download[[2]], if (is.finite(expected_size)) expected_size else download[[1]])
        }
        TRUE
      }
    )
    if (partial_start > 0) {
      handle_options$resume_from_large <- partial_start
    }
    handle <- do.call(curl::new_handle, handle_options)
    download_target <- if (partial_start > 0) resume_destination else temp_destination
    download_error <- tryCatch({
      curl::curl_fetch_disk(url, download_target, handle = handle)
      NULL
    }, error = function(e) e)
    if (file.exists(resume_destination)) {
      ugplot_append_file(resume_destination, temp_destination)
      unlink(resume_destination)
    }
    if (!is.null(download_error)) {
      if (file.exists(temp_destination) && is.finite(expected_size) && file.info(temp_destination)$size == expected_size && ugplot_geo_gzip_valid(temp_destination)) {
        if (file.exists(destination)) {
          unlink(destination)
        }
        file.rename(temp_destination, destination)
        return(list(status = "completed_after_transfer_warning", path = destination, bytes = file.info(destination)$size))
      }
      partial_size <- if (file.exists(temp_destination)) file.info(temp_destination)$size else 0
      stop(paste0(
        conditionMessage(download_error),
        ". Partial download saved: ", temp_destination,
        " (", ugplot_format_bytes(partial_size), " of ", ugplot_format_bytes(expected_size), "). Click download again to resume."
      ))
    }
  } else {
    utils::download.file(url, temp_destination, mode = "wb", quiet = TRUE)
  }
  if (file.exists(temp_destination) && is.finite(expected_size) && file.info(temp_destination)$size < expected_size) {
    stop(paste0(
      "Partial download saved: ", temp_destination,
      " (", ugplot_format_bytes(file.info(temp_destination)$size), " of ", ugplot_format_bytes(expected_size), "). Click download again to resume."
    ))
  }
  if (file.exists(destination)) {
    unlink(destination)
  }
  file.rename(temp_destination, destination)
  if (!ugplot_geo_gzip_valid(destination)) {
    invalid_size <- file.info(destination)$size
    unlink(destination)
    stop(paste0(
      "Downloaded gzip failed integrity check and was deleted to save disk space: ",
      destination, " (", ugplot_format_bytes(invalid_size),
      "). Click download again to fetch a clean copy."
    ))
  }
  list(status = if (partial_start > 0) "resumed" else "downloaded", path = destination, bytes = file.info(destination)$size)
}

ugplot_geo_decompressed_path <- function(path) {
  if (grepl("\\.gz$", path, ignore.case = TRUE)) {
    return(sub("\\.gz$", "", path, ignore.case = TRUE))
  }
  NA_character_
}

ugplot_geo_extract_gzip <- function(source, destination = ugplot_geo_decompressed_path(source),
                                    chunk_size = 16 * 1024^2, progress_callback = NULL,
                                    delete_source = TRUE) {
  if (is.na(destination) || !nzchar(destination)) {
    stop("Only .gz files can be extracted in this step.")
  }
  if (!file.exists(source)) {
    stop(paste0("Compressed file not found: ", source))
  }
  if (!ugplot_geo_gzip_valid(source)) {
    stop(paste0(
      "Compressed file failed gzip integrity check. Re-download before extracting: ",
      source
    ))
  }
  if (file.exists(destination)) {
    return(list(status = "already_extracted", path = destination, bytes = file.info(destination)$size))
  }
  temp_destination <- paste0(destination, ".part")
  if (file.exists(temp_destination)) {
    unlink(temp_destination)
  }
  input <- gzfile(source, "rb")
  on.exit(try(close(input), silent = TRUE), add = TRUE)
  output <- file(temp_destination, "wb")
  on.exit(try(close(output), silent = TRUE), add = TRUE)
  total_in <- file.info(source)$size
  read_in <- 0
  repeat {
    chunk <- readBin(input, what = "raw", n = chunk_size)
    if (length(chunk) == 0) {
      break
    }
    writeBin(chunk, output)
    read_in <- min(total_in, read_in + length(chunk))
    if (!is.null(progress_callback)) {
      progress_callback(read_in, total_in)
    }
  }
  close(output)
  close(input)
  file.rename(temp_destination, destination)
  if (isTRUE(delete_source) && file.exists(destination) && file.info(destination)$size > 0 && file.exists(source)) {
    unlink(source)
  }
  list(status = "extracted", path = destination, bytes = file.info(destination)$size)
}

ugplot_geo_local_status <- function(file_name, cache_dir, expected_size = NA_real_) {
  final_path <- file.path(cache_dir, file_name)
  partial_path <- paste0(final_path, ".part")
  decompressed_path <- ugplot_geo_decompressed_path(final_path)
  expected_size <- suppressWarnings(as.numeric(expected_size))

  if (!is.na(decompressed_path) && file.exists(decompressed_path)) {
    return(list(
      status = "extracted",
      size = file.info(decompressed_path)$size,
      path = decompressed_path
    ))
  }
  if (file.exists(final_path)) {
    final_size <- file.info(final_path)$size
    if (!ugplot_geo_gzip_valid(final_path)) {
      unlink(final_path)
      return(list(status = "deleted_corrupt", size = NA_real_, path = final_path))
    }
    if (!is.finite(expected_size) || final_size == expected_size) {
      return(list(status = "downloaded", size = final_size, path = final_path))
    }
    return(list(status = "size_mismatch", size = final_size, path = final_path))
  }
  if (file.exists(partial_path)) {
    partial_size <- file.info(partial_path)$size
    if (is.finite(expected_size) && partial_size == expected_size) {
      if (!ugplot_geo_gzip_valid(partial_path)) {
        unlink(partial_path)
        return(list(status = "deleted_corrupt_partial", size = NA_real_, path = final_path))
      }
      if (file.exists(final_path)) {
        unlink(final_path)
      }
      file.rename(partial_path, final_path)
      return(list(status = "downloaded", size = file.info(final_path)$size, path = final_path))
    }
    return(list(status = "partial", size = partial_size, path = partial_path))
  }
  list(status = "missing", size = NA_real_, path = final_path)
}

ugplot_geo_annotate_remote_files <- function(remote_files, cache_dir) {
  if (!is.data.frame(remote_files) || nrow(remote_files) == 0) {
    return(remote_files)
  }
  expected_sizes <- ugplot_geo_size_bytes(remote_files)
  local_info <- lapply(seq_len(nrow(remote_files)), function(i) {
    ugplot_geo_local_status(remote_files$File[[i]], cache_dir, expected_sizes[[i]])
  })
  remote_files$LocalStatus <- vapply(local_info, function(x) x$status, character(1))
  remote_files$LocalSizeBytes <- vapply(local_info, function(x) suppressWarnings(as.numeric(x$size)), numeric(1))
  remote_files$LocalSize <- vapply(local_info, function(x) ugplot_format_bytes(x$size), character(1))
  remote_files$LocalPath <- vapply(local_info, function(x) x$path, character(1))
  remote_files$NeedsDownload <- !(remote_files$LocalStatus %in% c("downloaded", "extracted"))
  remote_files
}

ugplot_geo_write_manifest <- function(cache_dir, accession, remote_files) {
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  manifest <- list(
    accession = accession,
    updated_at = as.character(Sys.time()),
    files = remote_files
  )
  saveRDS(manifest, ugplot_geo_manifest_path(cache_dir))
}

ugplot_geo_remote_supp_files <- function(accession) {
  metadata <- GEOquery::getGEOfile(accession, amount = "quick", destdir = tempdir())
  supp_files <- character(0)
  if (is.character(metadata) && length(metadata) > 0 && file.exists(metadata[[1]])) {
    soft_lines <- readLines(metadata[[1]], warn = FALSE)
    supp_lines <- grep("^!Series_supplementary_file", soft_lines, value = TRUE)
    supp_files <- trimws(sub("^!Series_supplementary_file\\s*=\\s*", "", supp_lines))
  } else {
    supp_files <- tryCatch(
      GEOquery::Meta(metadata)$supplementary_file,
      error = function(e) character(0)
    )
  }
  supp_files <- unique(as.character(supp_files))
  supp_files <- supp_files[nzchar(supp_files) & !is.na(supp_files)]
  if (length(supp_files) == 0) {
    return(data.frame())
  }
  supp_files <- ugplot_geo_normalize_url(supp_files)
  file_names <- basename(supp_files)
  lower_names <- tolower(file_names)
  is_idat <- grepl("\\.idat(\\.gz)?$", lower_names)
  is_table <- grepl("\\.(csv|tsv|txt|csv\\.gz|tsv\\.gz|txt\\.gz)$", lower_names)
  is_metadata <- grepl("\\.(xlsx|xls|soft|soft\\.gz)$", lower_names)
  is_archive <- grepl("\\.(tar|tar\\.gz|tgz|zip)$", lower_names)
  methylation_hint <- grepl("beta|methyl|meth|450k|850k|epic|matrix|processed|normalized|normalised", lower_names)
  size_bytes <- unname(vapply(supp_files, ugplot_geo_remote_file_size, numeric(1)))
  data.frame(
    File = file_names,
    URL = supp_files,
    SizeBytes = size_bytes,
    SizeMB = round(size_bytes / (1024^2), 3),
    Size = vapply(size_bytes, ugplot_format_bytes, character(1)),
    Type = ifelse(is_idat, "IDAT", ifelse(is_table, "table", ifelse(is_metadata, "metadata", ifelse(is_archive, "archive", "other")))),
    MethylationHint = methylation_hint,
    Loadable = is_table,
    stringsAsFactors = FALSE
  )
}

ugplot_geo_clean_metadata_name <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  ifelse(nzchar(x), x, "characteristic")
}

ugplot_geo_parse_characteristics <- function(metadata) {
  characteristic_cols <- grep("^characteristics", names(metadata), value = TRUE, ignore.case = TRUE)
  if (length(characteristic_cols) == 0 || nrow(metadata) == 0) {
    return(metadata)
  }

  parsed <- vector("list", nrow(metadata))
  all_keys <- character(0)
  for (i in seq_len(nrow(metadata))) {
    row_values <- as.character(unlist(metadata[i, characteristic_cols, drop = FALSE], use.names = FALSE))
    row_values <- row_values[nzchar(row_values) & !is.na(row_values)]
    row_values <- unique(row_values)
    row_map <- list()
    for (value in row_values) {
      if (!grepl(":", value, fixed = TRUE)) {
        next
      }
      key <- ugplot_geo_clean_metadata_name(sub(":.*$", "", value))
      val <- trimws(sub("^[^:]+:\\s*", "", value))
      if (!nzchar(key) || !nzchar(val)) {
        next
      }
      if (!is.null(row_map[[key]]) && !identical(row_map[[key]], val)) {
        row_map[[key]] <- paste(unique(c(row_map[[key]], val)), collapse = "; ")
      } else {
        row_map[[key]] <- val
      }
      all_keys <- union(all_keys, key)
    }
    parsed[[i]] <- row_map
  }

  output_keys <- all_keys
  conflicts <- output_keys %in% names(metadata)
  if (any(conflicts)) {
    output_keys[conflicts] <- make.unique(c(names(metadata), output_keys[conflicts]))[-seq_along(names(metadata))]
  }
  names(output_keys) <- all_keys
  for (key in all_keys) {
    output_key <- output_keys[[key]]
    metadata[[output_key]] <- vapply(parsed, function(row_map) {
      value <- row_map[[key]]
      if (is.null(value)) NA_character_ else value
    }, character(1))
  }
  metadata
}

ugplot_geo_fetch_sample_metadata <- function(accession, cache_dir) {
  if (!requireNamespace("GEOquery", quietly = TRUE)) {
    stop("Package 'GEOquery' is required to fetch sample metadata.")
  }
  if (!requireNamespace("Biobase", quietly = TRUE)) {
    stop("Package 'Biobase' is required to read GEO sample metadata.")
  }
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  geo_sets <- GEOquery::getGEO(accession, GSEMatrix = TRUE, getGPL = FALSE, destdir = cache_dir)
  if (!is.list(geo_sets)) {
    geo_sets <- list(geo_sets)
  }
  metadata_list <- lapply(seq_along(geo_sets), function(i) {
    pdata <- as.data.frame(Biobase::pData(geo_sets[[i]]), stringsAsFactors = FALSE, check.names = FALSE)
    if (nrow(pdata) == 0) {
      return(data.frame())
    }
    pdata <- ugplot_geo_parse_characteristics(pdata)
    pdata$sample_id <- rownames(pdata)
    pdata$gse_matrix <- names(geo_sets)[[i]] %||% paste0("matrix_", i)
    pdata <- pdata[, c("sample_id", "gse_matrix", setdiff(names(pdata), c("sample_id", "gse_matrix"))), drop = FALSE]
    rownames(pdata) <- NULL
    pdata
  })
  metadata <- do.call(rbind, metadata_list)
  if (!is.data.frame(metadata) || nrow(metadata) == 0) {
    stop("No sample metadata table was returned by GEOquery.")
  }
  saveRDS(metadata, ugplot_geo_sample_metadata_path(cache_dir, "rds"))
  utils::write.csv(metadata, ugplot_geo_sample_metadata_path(cache_dir, "csv"), row.names = FALSE)
  metadata
}

ugplot_geo_sample_matrix_id <- function(metadata) {
  if (!is.data.frame(metadata) || nrow(metadata) == 0) {
    return(character(0))
  }
  source <- if ("title" %in% names(metadata)) metadata$title else metadata$sample_id
  ids <- trimws(sub("\\s+.*$", "", as.character(source)))
  ids[!nzchar(ids) | is.na(ids)] <- as.character(metadata$sample_id[!nzchar(ids) | is.na(ids)])
  ids
}

ugplot_geo_target_candidates <- function(metadata) {
  if (!is.data.frame(metadata) || nrow(metadata) == 0) {
    return(character(0))
  }
  excluded <- c("sample_id", "gse_matrix", "title", "geo_accession", "status", "submission_date", "last_update_date")
  candidates <- setdiff(names(metadata), excluded)
  candidates <- candidates[vapply(metadata[candidates], function(x) {
    values <- unique(na.omit(as.character(x)))
    length(values) >= 2 && length(values) < nrow(metadata)
  }, logical(1))]
  priority <- grep("age|sex|gender|disease|status|treatment|response|case|control|group|phenotype", candidates, value = TRUE, ignore.case = TRUE)
  unique(c(priority, candidates))
}

ugplot_geo_detect_platform <- function(metadata) {
  if (!is.data.frame(metadata) || nrow(metadata) == 0 || !"platform_id" %in% names(metadata)) {
    return(NA_character_)
  }
  platforms <- unique(na.omit(as.character(metadata$platform_id)))
  platforms <- platforms[nzchar(platforms)]
  if (length(platforms) == 0) NA_character_ else platforms[[1]]
}

ugplot_geo_platform_annotation_package <- function(platform_id) {
  platform_id <- toupper(trimws(platform_id %||% ""))
  switch(platform_id,
    "GPL13534" = list(
      platform = "GPL13534",
      array = "Illumina HumanMethylation450",
      package = "IlluminaHumanMethylation450kanno.ilmn12.hg19",
      object = "IlluminaHumanMethylation450kanno.ilmn12.hg19",
      genome = "hg19"
    ),
    "GPL21145" = list(
      platform = "GPL21145",
      array = "Illumina HumanMethylationEPIC",
      package = "IlluminaHumanMethylationEPICanno.ilm10b4.hg19",
      object = "IlluminaHumanMethylationEPICanno.ilm10b4.hg19",
      genome = "hg19"
    ),
    NULL
  )
}

ugplot_geo_missing_annotation_packages <- function(platform_info) {
  if (is.null(platform_info)) {
    return(character(0))
  }
  missing <- character(0)
  if (!requireNamespace("minfi", quietly = TRUE)) {
    missing <- c(missing, "minfi")
  }
  if (!requireNamespace(platform_info$package, quietly = TRUE)) {
    missing <- c(missing, platform_info$package)
  }
  unique(missing)
}

ugplot_split_semicolon <- function(x) {
  x <- as.character(x %||% "")
  if (!nzchar(x) || is.na(x)) {
    return(character(0))
  }
  values <- trimws(base::strsplit(x, ";", fixed = TRUE)[[1]])
  values[nzchar(values) & !is.na(values)]
}

ugplot_first_existing_col <- function(data, candidates) {
  found <- intersect(candidates, names(data))
  if (length(found) == 0) NA_character_ else found[[1]]
}

ugplot_geo_expand_probe_annotation <- function(annotation, platform_id, source_package, genome) {
  annotation <- as.data.frame(annotation, stringsAsFactors = FALSE, check.names = FALSE)
  cpg_ids <- rownames(annotation)
  if ("Name" %in% names(annotation)) {
    cpg_ids <- as.character(annotation$Name)
  }
  gene_col <- ugplot_first_existing_col(annotation, c("UCSC_RefGene_Name", "Gene", "gene", "gene_symbol"))
  transcript_col <- ugplot_first_existing_col(annotation, c("UCSC_RefGene_Accession", "Transcript", "transcript_id"))
  group_col <- ugplot_first_existing_col(annotation, c("UCSC_RefGene_Group", "Relation_to_Gene", "gene_group"))
  chr_col <- ugplot_first_existing_col(annotation, c("chr", "CHR", "Chromosome"))
  pos_col <- ugplot_first_existing_col(annotation, c("pos", "MAPINFO", "mapinfo", "Position"))
  island_col <- ugplot_first_existing_col(annotation, c("Relation_to_Island", "Relation_to_UCSC_CpG_Island"))
  feature_col <- ugplot_first_existing_col(annotation, c("Regulatory_Feature_Group", "Regulatory_Feature_Name"))
  probe_type_col <- ugplot_first_existing_col(annotation, c("Type", "Probe_Type"))

  split_annotation_col <- function(col_name) {
    if (is.na(col_name)) {
      return(rep(list(character(0)), length(cpg_ids)))
    }
    lapply(annotation[[col_name]], ugplot_split_semicolon)
  }

  genes_split <- split_annotation_col(gene_col)
  transcripts_split <- split_annotation_col(transcript_col)
  groups_split <- split_annotation_col(group_col)
  max_links <- pmax(lengths(genes_split), lengths(transcripts_split), lengths(groups_split), 1L)
  expand_split_values <- function(values_split) {
    base::unlist(Map(function(values, n_links) {
      if (length(values) == 0) {
        values <- NA_character_
      }
      if (length(values) == n_links) {
        return(values)
      }
      rep(values, length.out = n_links)
    }, values_split, max_links), use.names = FALSE)
  }

  annotation_map <- base::data.frame(
    CpG = rep(cpg_ids, max_links),
    Gene = expand_split_values(genes_split),
    Transcript = expand_split_values(transcripts_split),
    GeneRegion = expand_split_values(groups_split),
    Chr = if (!is.na(chr_col)) rep(as.character(annotation[[chr_col]]), max_links) else NA_character_,
    Position = if (!is.na(pos_col)) rep(suppressWarnings(as.numeric(annotation[[pos_col]])), max_links) else NA_real_,
    CpGIslandRelation = if (!is.na(island_col)) rep(as.character(annotation[[island_col]]), max_links) else NA_character_,
    RegulatoryFeature = if (!is.na(feature_col)) rep(as.character(annotation[[feature_col]]), max_links) else NA_character_,
    ProbeType = if (!is.na(probe_type_col)) rep(as.character(annotation[[probe_type_col]]), max_links) else NA_character_,
    Platform = platform_id,
    Genome = genome,
    AnnotationSource = source_package,
    stringsAsFactors = FALSE
  )
  annotation_map <- base::unique(annotation_map)
  rownames(annotation_map) <- NULL
  annotation_map
}

ugplot_geo_build_annotation_cache <- function(platform_id, force = FALSE) {
  platform_info <- ugplot_geo_platform_annotation_package(platform_id)
  if (is.null(platform_info)) {
    stop(paste0("No built-in annotation mapping is configured for platform ", platform_id, "."))
  }
  cache_path <- ugplot_geo_annotation_cache_path(platform_info$platform, "rds")
  if (!isTRUE(force) && base::file.exists(cache_path)) {
    return(base::readRDS(cache_path))
  }
  if (!requireNamespace("minfi", quietly = TRUE)) {
    stop("Package 'minfi' is required to build methylation annotation caches.")
  }
  if (!requireNamespace(platform_info$package, quietly = TRUE)) {
    stop(paste0("Package '", platform_info$package, "' is required for ", platform_info$platform, " annotation."))
  }
  suppressPackageStartupMessages(
    base::library(platform_info$package, character.only = TRUE)
  )
  annotation_object <- base::get(platform_info$object, envir = base::as.environment(paste0("package:", platform_info$package)))
  annotation <- minfi::getAnnotation(annotation_object)
  annotation_map <- ugplot_geo_expand_probe_annotation(
    annotation,
    platform_id = platform_info$platform,
    source_package = platform_info$package,
    genome = platform_info$genome
  )
  base::dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
  base::saveRDS(annotation_map, cache_path)
  utils::write.csv(annotation_map, ugplot_geo_annotation_cache_path(platform_info$platform, "csv"), row.names = FALSE)
  annotation_map
}

ugplot_geo_load_annotation_cache <- function(platform_id) {
  platform_info <- ugplot_geo_platform_annotation_package(platform_id)
  if (is.null(platform_info)) {
    return(data.frame())
  }
  cache_path <- ugplot_geo_annotation_cache_path(platform_info$platform, "rds")
  if (!base::file.exists(cache_path)) {
    return(data.frame())
  }
  base::readRDS(cache_path)
}

ugplot_geo_join_spearman_annotation <- function(results, annotation_map) {
  if (!is.data.frame(results) || nrow(results) == 0 || !is.data.frame(annotation_map) || nrow(annotation_map) == 0) {
    return(results)
  }
  merge(results, annotation_map, by = "CpG", all.x = TRUE, sort = FALSE)
}

ugplot_geo_group_spearman_annotation <- function(annotated_results, group_col) {
  if (!is.data.frame(annotated_results) || nrow(annotated_results) == 0 || !group_col %in% names(annotated_results)) {
    return(data.frame())
  }
  grouped <- annotated_results[!is.na(annotated_results[[group_col]]) & nzchar(as.character(annotated_results[[group_col]])), , drop = FALSE]
  if (nrow(grouped) == 0) {
    return(data.frame())
  }
  split_groups <- split(grouped, grouped[[group_col]])
  summary <- lapply(names(split_groups), function(group_name) {
    df <- split_groups[[group_name]]
    best <- df[order(-df$AbsRho, df$PValue), , drop = FALSE][1, , drop = FALSE]
    data.frame(
      Group = group_name,
      NRows = nrow(df),
      NCpGs = length(unique(df$CpG)),
      MaxAbsRho = best$AbsRho[[1]],
      BestCpG = best$CpG[[1]],
      BestRho = best$SpearmanRho[[1]],
      BestPValue = best$PValue[[1]],
      MeanAbsRho = mean(df$AbsRho, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })
  summary <- do.call(rbind, summary)
  summary <- summary[order(-summary$MaxAbsRho, summary$BestPValue), , drop = FALSE]
  rownames(summary) <- NULL
  summary
}

ugplot_geo_transcript_candidates <- function(results, annotation_map, absrho_threshold = 0.8) {
  if (!is.data.frame(results) || nrow(results) == 0 || !is.data.frame(annotation_map) || nrow(annotation_map) == 0) {
    return(data.frame())
  }
  required_result_cols <- c("CpG", "SpearmanRho", "PValue", "N", "AbsRho")
  if (!all(required_result_cols %in% names(results)) || !"Transcript" %in% names(annotation_map)) {
    return(data.frame())
  }
  absrho_threshold <- suppressWarnings(as.numeric(absrho_threshold))
  if (!is.finite(absrho_threshold) || absrho_threshold < 0) {
    absrho_threshold <- 0.8
  }
  absrho_threshold <- min(1, absrho_threshold)

  raw_results <- unique(results[, required_result_cols, drop = FALSE])
  high_cpgs <- raw_results[is.finite(raw_results$AbsRho) & raw_results$AbsRho >= absrho_threshold, , drop = FALSE]
  if (nrow(high_cpgs) == 0) {
    return(data.frame())
  }

  trigger_links <- merge(
    high_cpgs,
    annotation_map[, intersect(c("CpG", "Gene", "Transcript"), names(annotation_map)), drop = FALSE],
    by = "CpG",
    all.x = FALSE,
    sort = FALSE
  )
  trigger_links <- trigger_links[!is.na(trigger_links$Transcript) & nzchar(as.character(trigger_links$Transcript)), , drop = FALSE]
  if (nrow(trigger_links) == 0) {
    return(data.frame())
  }

  selected_transcripts <- unique(as.character(trigger_links$Transcript))
  transcript_cpgs <- annotation_map[
    !is.na(annotation_map$Transcript) & as.character(annotation_map$Transcript) %in% selected_transcripts,
    ,
    drop = FALSE
  ]
  transcript_cpgs <- unique(transcript_cpgs)
  candidates <- merge(transcript_cpgs, raw_results, by = "CpG", all.x = TRUE, sort = FALSE)
  candidates$CpGInSpearmanScan <- !is.na(candidates$AbsRho)

  trigger_summary <- lapply(selected_transcripts, function(transcript_id) {
    df <- trigger_links[as.character(trigger_links$Transcript) == transcript_id, , drop = FALSE]
    df <- df[order(-df$AbsRho, df$PValue), , drop = FALSE]
    data.frame(
      Transcript = transcript_id,
      TriggerCpGs = paste(unique(df$CpG), collapse = ";"),
      TriggerGenes = paste(unique(stats::na.omit(df$Gene)), collapse = ";"),
      TriggerMaxAbsRho = max(df$AbsRho, na.rm = TRUE),
      TriggerBestCpG = df$CpG[[1]],
      TriggerBestRho = df$SpearmanRho[[1]],
      ThresholdAbsRho = absrho_threshold,
      stringsAsFactors = FALSE
    )
  })
  trigger_summary <- do.call(rbind, trigger_summary)
  candidates <- merge(candidates, trigger_summary, by = "Transcript", all.x = TRUE, sort = FALSE)
  candidates <- candidates[order(-candidates$TriggerMaxAbsRho, candidates$Transcript, candidates$Gene, candidates$CpG), , drop = FALSE]
  rownames(candidates) <- NULL
  candidates
}

ugplot_geo_matrix_files <- function(cache_dir) {
  files <- list.files(cache_dir, pattern = "\\.(txt|tsv|csv)$", full.names = TRUE, recursive = FALSE)
  files <- files[!grepl("series_matrix|sample_metadata|spearman|manifest|metadata|transcript|groups|candidate", basename(files), ignore.case = TRUE)]
  files <- files[file.info(files)$size > 0]
  files[vapply(files, function(path) {
    header <- tryCatch(readLines(path, n = 1, warn = FALSE), error = function(e) character(0))
    length(header) == 1 && grepl("^ID_REF\\t", header)
  }, logical(1))]
}

ugplot_geo_matrix_sample_map <- function(matrix_files, metadata) {
  sample_ids <- ugplot_geo_sample_matrix_id(metadata)
  metadata_lookup <- stats::setNames(seq_len(nrow(metadata)), sample_ids)
  do.call(rbind, lapply(matrix_files, function(path) {
    header <- strsplit(readLines(path, n = 1, warn = FALSE), "\t", fixed = TRUE)[[1]]
    value_cols <- which(header != "ID_REF" & !grepl("\\.1$", header))
    matrix_ids <- header[value_cols]
    metadata_idx <- unname(metadata_lookup[matrix_ids])
    matched <- !is.na(metadata_idx)
    if (!any(matched)) {
      return(data.frame(
        File = character(),
        Path = character(),
        ColumnIndex = integer(),
        MatrixSample = character(),
        MetadataRow = integer(),
        SampleID = character(),
        stringsAsFactors = FALSE
      ))
    }
    data.frame(
      File = basename(path),
      Path = path,
      ColumnIndex = value_cols[matched],
      MatrixSample = matrix_ids[matched],
      MetadataRow = metadata_idx[matched],
      SampleID = metadata$sample_id[metadata_idx[matched]],
      stringsAsFactors = FALSE
    )
  }))
}

ugplot_geo_spearman_scan <- function(matrix_files, metadata, target_column,
                                     max_cpgs = 0,
                                     progress_callback = NULL, result_callback = NULL) {
  if (!target_column %in% names(metadata)) {
    stop("Selected target column is not present in sample metadata.")
  }
  target <- suppressWarnings(as.numeric(as.character(metadata[[target_column]])))
  if (all(is.na(target))) {
    stop("Spearman scan currently requires a numeric target column.")
  }
  sample_map <- ugplot_geo_matrix_sample_map(matrix_files, metadata)
  if (!is.data.frame(sample_map) || nrow(sample_map) == 0) {
    stop("No matrix columns could be matched to sample metadata. Check sample IDs and GEO metadata titles.")
  }
  target_by_matrix <- target[sample_map$MetadataRow]
  if (sum(!is.na(target_by_matrix)) < 3) {
    stop("At least three matched samples with numeric target values are required.")
  }

  file_maps <- split(sample_map, sample_map$Path)
  file_states <- lapply(names(file_maps), function(path) {
    con <- file(path, "r")
    header <- strsplit(readLines(con, n = 1, warn = FALSE), "\t", fixed = TRUE)[[1]]
    list(path = path, con = con, cols = file_maps[[path]]$ColumnIndex, metadata_rows = file_maps[[path]]$MetadataRow, header = header)
  })
  on.exit(lapply(file_states, function(state) try(close(state$con), silent = TRUE)), add = TRUE)

  max_cpgs <- suppressWarnings(as.integer(max_cpgs))
  if (!is.finite(max_cpgs) || max_cpgs < 0) {
    max_cpgs <- 0
  }
  results <- data.frame(CpG = character(), SpearmanRho = numeric(), PValue = numeric(), N = integer(), AbsRho = numeric(), stringsAsFactors = FALSE)
  scanned <- 0L

  repeat {
    lines <- lapply(file_states, function(state) readLines(state$con, n = 1, warn = FALSE))
    if (any(lengths(lines) == 0)) {
      break
    }
    parts <- lapply(lines, function(line) strsplit(line, "\t", fixed = TRUE)[[1]])
    cpg <- parts[[1]][[1]]
    values <- rep(NA_real_, nrow(metadata))
    for (i in seq_along(file_states)) {
      state <- file_states[[i]]
      numeric_values <- suppressWarnings(as.numeric(parts[[i]][state$cols]))
      values[state$metadata_rows] <- numeric_values
    }
    keep <- !is.na(values) & !is.na(target)
    if (sum(keep) >= 3) {
      test <- suppressWarnings(stats::cor.test(values[keep], target[keep], method = "spearman", exact = FALSE))
      results <- rbind(results, data.frame(
        CpG = cpg,
        SpearmanRho = unname(test$estimate),
        PValue = test$p.value,
        N = sum(keep),
        AbsRho = abs(unname(test$estimate)),
        stringsAsFactors = FALSE
      ))
      if (!is.null(result_callback)) {
        result_callback(utils::tail(results, 1))
      }
    }
    scanned <- scanned + 1L
    if (!is.null(progress_callback) && (scanned %% 1000L == 0L)) {
      progress_callback(scanned)
    }
    if (max_cpgs > 0 && scanned >= max_cpgs) {
      break
    }
  }
  if (nrow(results) == 0) {
    stop("No CpG had enough matched numeric values for Spearman correlation.")
  }
  results <- results[order(-results$AbsRho, results$PValue), , drop = FALSE]
  rownames(results) <- NULL
  attr(results, "scanned_cpgs") <- scanned
  attr(results, "matched_samples") <- sum(!is.na(target_by_matrix))
  results
}

ugplot_geo_transcript_dataset <- function(matrix_files, metadata, target_column, cpgs,
                                          progress_callback = NULL) {
  if (!target_column %in% names(metadata)) {
    stop("Selected target column is not present in sample metadata.")
  }
  cpgs <- unique(as.character(stats::na.omit(cpgs)))
  if (length(cpgs) == 0) {
    stop("No CpGs were selected for this transcript.")
  }
  sample_map <- ugplot_geo_matrix_sample_map(matrix_files, metadata)
  if (!is.data.frame(sample_map) || nrow(sample_map) == 0) {
    stop("No matrix columns could be matched to sample metadata. Check sample IDs and GEO metadata titles.")
  }

  values <- matrix(NA_real_, nrow = nrow(metadata), ncol = length(cpgs))
  colnames(values) <- cpgs
  rownames(values) <- metadata$sample_id
  cpg_index <- stats::setNames(seq_along(cpgs), cpgs)
  found <- rep(FALSE, length(cpgs))

  file_maps <- split(sample_map, sample_map$Path)
  scanned <- 0L
  for (path in names(file_maps)) {
    map <- file_maps[[path]]
    file_found <- rep(FALSE, length(cpgs))
    con <- file(path, "r")
    on.exit(try(close(con), silent = TRUE), add = TRUE)
    readLines(con, n = 1, warn = FALSE)
    repeat {
      line <- readLines(con, n = 1, warn = FALSE)
      if (length(line) == 0) {
        break
      }
      parts <- strsplit(line, "\t", fixed = TRUE)[[1]]
      cpg <- parts[[1]]
      if (cpg %in% names(cpg_index)) {
        numeric_values <- suppressWarnings(as.numeric(parts[map$ColumnIndex]))
        values[map$MetadataRow, cpg_index[[cpg]]] <- numeric_values
        found[[cpg_index[[cpg]]]] <- TRUE
        file_found[[cpg_index[[cpg]]]] <- TRUE
      }
      scanned <- scanned + 1L
      if (!is.null(progress_callback) && scanned %% 10000L == 0L) {
        progress_callback(scanned, sum(found), length(cpgs))
      }
      if (all(file_found)) {
        break
      }
    }
    try(close(con), silent = TRUE)
  }

  kept_cpgs <- colnames(values)[found]
  if (length(kept_cpgs) == 0) {
    stop("None of the transcript CpGs were found in the extracted GEO matrix files.")
  }
  values <- values[, kept_cpgs, drop = FALSE]
  data <- data.frame(
    sample_id = metadata$sample_id,
    target = metadata[[target_column]],
    values,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  names(data)[names(data) == "target"] <- target_column
  data
}

ugplot_geo_list_candidate_files <- function(accession, cache_dir) {
  files <- list.files(cache_dir, recursive = TRUE, full.names = TRUE, all.files = FALSE)
  files <- files[basename(files) != "ugplot_geo_manifest.rds"]
  old_corrupt_files <- files[grepl("\\.corrupt$", basename(files), ignore.case = TRUE)]
  if (length(old_corrupt_files) > 0) {
    unlink(old_corrupt_files)
    files <- setdiff(files, old_corrupt_files)
  }
  stale_extract_partials <- files[grepl("\\.(csv|tsv|txt)\\.part$", basename(files), ignore.case = TRUE)]
  if (length(stale_extract_partials) > 0) {
    unlink(stale_extract_partials)
    files <- setdiff(files, stale_extract_partials)
  }
  if (length(files) == 0) {
    return(data.frame())
  }
  info <- file.info(files)
  lower_names <- tolower(basename(files))
  is_partial <- grepl("\\.part$|\\.resume$", lower_names)
  is_idat <- grepl("\\.idat(\\.gz)?$", lower_names)
  is_table <- grepl("\\.(csv|tsv|txt|csv\\.gz|tsv\\.gz|txt\\.gz)$", lower_names)
  is_metadata <- grepl("\\.(xlsx|xls|soft|soft\\.gz)$", lower_names)
  is_archive <- grepl("\\.(tar|tar\\.gz|tgz|zip)$", lower_names)
  methylation_hint <- grepl("beta|methyl|meth|450k|850k|epic|matrix|processed|normalized|normalised", lower_names)
  data.frame(
    File = basename(files),
    Path = files,
    SizeMB = round(info$size / (1024^2), 3),
    Type = ifelse(is_partial, "partial", ifelse(is_idat, "IDAT", ifelse(is_table, "table", ifelse(is_metadata, "metadata", ifelse(is_archive, "archive", "other"))))),
    MethylationHint = methylation_hint,
    Loadable = is_table & !is_partial,
    stringsAsFactors = FALSE
  )
}

ugplot_read_geo_table <- function(path, use_first_column_names = TRUE) {
  lower_path <- tolower(path)
  sep <- if (grepl("\\.csv(\\.gz)?$", lower_path)) "," else "\t"
  data <- utils::read.table(
    path,
    header = TRUE,
    sep = sep,
    row.names = if (isTRUE(use_first_column_names)) 1 else NULL,
    dec = ".",
    stringsAsFactors = FALSE,
    strip.white = TRUE,
    check.names = FALSE,
    comment.char = "",
    quote = "\""
  )
  as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
}
