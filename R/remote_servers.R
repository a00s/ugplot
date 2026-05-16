ugplot_remote_servers_path <- function() {
  config_dir <- file.path(path.expand("~"), ".ugplot")
  ugplot_ensure_dir(config_dir)
  file.path(config_dir, "remote_servers.rds")
}

ugplot_detect_remote_cpu_max <- function() {
  total_cpus <- suppressWarnings(as.integer(parallel::detectCores(logical = TRUE)))
  if (is.na(total_cpus) || total_cpus < 1L) {
    total_cpus <- 1L
  }
  total_cpus
}

ugplot_default_remote_servers <- function() {
  total_cpus <- ugplot_detect_remote_cpu_max()
  data.frame(
    name = "Local 8080",
    url = "http://127.0.0.1:8080",
    token = "",
    cpu_limit = max(1L, total_cpus - 1L),
    cpu_max = total_cpus,
    stringsAsFactors = FALSE
  )
}

ugplot_read_remote_servers <- function() {
  path <- ugplot_remote_servers_path()
  if (!file.exists(path)) {
    return(ugplot_default_remote_servers())
  }
  servers <- tryCatch(readRDS(path), error = function(e) ugplot_default_remote_servers())
  required_columns <- c("name", "url", "token", "cpu_limit", "cpu_max")
  if (!is.data.frame(servers) || !all(required_columns %in% names(servers))) {
    if (!is.data.frame(servers) || !all(c("name", "url", "token") %in% names(servers))) {
      return(ugplot_default_remote_servers())
    }
    total_cpus <- ugplot_detect_remote_cpu_max()
    if (!("cpu_limit" %in% names(servers))) {
      servers$cpu_limit <- max(1L, total_cpus - 1L)
    }
    if (!("cpu_max" %in% names(servers))) {
      servers$cpu_max <- total_cpus
    }
  }
  servers <- servers[, required_columns, drop = FALSE]
  servers$name <- as.character(servers$name)
  servers$url <- as.character(servers$url)
  servers$token <- as.character(servers$token)
  servers$cpu_limit <- suppressWarnings(as.integer(servers$cpu_limit))
  servers$cpu_limit[is.na(servers$cpu_limit) | servers$cpu_limit < 1L] <- 1L
  servers$cpu_max <- suppressWarnings(as.integer(servers$cpu_max))
  invalid_cpu_max <- is.na(servers$cpu_max) | servers$cpu_max < 1L
  servers$cpu_max[invalid_cpu_max] <- servers$cpu_limit[invalid_cpu_max]
  servers$cpu_max <- pmax(servers$cpu_max, servers$cpu_limit)
  servers <- servers[nzchar(servers$name) & nzchar(servers$url), , drop = FALSE]
  if (nrow(servers) == 0) {
    return(ugplot_default_remote_servers())
  }
  servers[!duplicated(servers$name), , drop = FALSE]
}

ugplot_write_remote_servers <- function(servers) {
  required_columns <- c("name", "url", "token", "cpu_limit", "cpu_max")
  servers <- as.data.frame(servers, stringsAsFactors = FALSE)
  for (column_name in setdiff(required_columns, names(servers))) {
    servers[[column_name]] <- if (column_name %in% c("cpu_limit", "cpu_max")) 1L else ""
  }
  servers <- servers[, required_columns, drop = FALSE]
  servers$name <- trimws(as.character(servers$name))
  servers$url <- trimws(as.character(servers$url))
  servers$token <- as.character(servers$token)
  servers$cpu_limit <- suppressWarnings(as.integer(servers$cpu_limit))
  servers$cpu_limit[is.na(servers$cpu_limit) | servers$cpu_limit < 1L] <- 1L
  servers$cpu_max <- suppressWarnings(as.integer(servers$cpu_max))
  invalid_cpu_max <- is.na(servers$cpu_max) | servers$cpu_max < 1L
  servers$cpu_max[invalid_cpu_max] <- servers$cpu_limit[invalid_cpu_max]
  servers$cpu_max <- pmax(servers$cpu_max, servers$cpu_limit)
  servers <- servers[nzchar(servers$name) & nzchar(servers$url), , drop = FALSE]
  if (nrow(servers) == 0) {
    servers <- ugplot_default_remote_servers()
  }
  servers <- servers[!duplicated(servers$name, fromLast = TRUE), , drop = FALSE]
  saveRDS(servers, ugplot_remote_servers_path())
  invisible(servers)
}

ugplot_upsert_remote_server <- function(name, url, token = "", cpu_limit = 1L, cpu_max = cpu_limit) {
  name <- trimws(as.character(name %||% ""))
  url <- trimws(as.character(url %||% ""))
  token <- as.character(token %||% "")
  cpu_limit <- suppressWarnings(as.integer(cpu_limit %||% 1L))
  if (is.na(cpu_limit) || cpu_limit < 1L) {
    cpu_limit <- 1L
  }
  cpu_max <- suppressWarnings(as.integer(cpu_max %||% cpu_limit))
  if (is.na(cpu_max) || cpu_max < 1L) {
    cpu_max <- cpu_limit
  }
  cpu_max <- max(cpu_max, cpu_limit)
  if (!nzchar(name)) {
    stop("Remote server name is required.", call. = FALSE)
  }
  if (!nzchar(url)) {
    stop("Remote server URL is required.", call. = FALSE)
  }
  servers <- ugplot_read_remote_servers()
  servers <- servers[servers$name != name, , drop = FALSE]
  servers <- rbind(
    servers,
    data.frame(name = name, url = url, token = token, cpu_limit = cpu_limit, cpu_max = cpu_max, stringsAsFactors = FALSE)
  )
  ugplot_write_remote_servers(servers)
}

ugplot_remove_remote_server <- function(name) {
  name <- trimws(as.character(name %||% ""))
  servers <- ugplot_read_remote_servers()
  servers <- servers[servers$name != name, , drop = FALSE]
  ugplot_write_remote_servers(servers)
}
