ugplot_remote_servers_path <- function() {
  config_dir <- file.path(path.expand("~"), ".ugplot")
  ugplot_ensure_dir(config_dir)
  file.path(config_dir, "remote_servers.rds")
}

ugplot_default_remote_servers <- function() {
  data.frame(
    name = "Local 8080",
    url = "http://127.0.0.1:8080",
    token = "",
    cpu_limit = max(1L, parallel::detectCores(logical = TRUE) - 1L),
    stringsAsFactors = FALSE
  )
}

ugplot_read_remote_servers <- function() {
  path <- ugplot_remote_servers_path()
  if (!file.exists(path)) {
    return(ugplot_default_remote_servers())
  }
  servers <- tryCatch(readRDS(path), error = function(e) ugplot_default_remote_servers())
  required_columns <- c("name", "url", "token", "cpu_limit")
  if (!is.data.frame(servers) || !all(required_columns %in% names(servers))) {
    if (!is.data.frame(servers) || !all(c("name", "url", "token") %in% names(servers))) {
      return(ugplot_default_remote_servers())
    }
    servers$cpu_limit <- max(1L, parallel::detectCores(logical = TRUE) - 1L)
  }
  servers <- servers[, required_columns, drop = FALSE]
  servers$name <- as.character(servers$name)
  servers$url <- as.character(servers$url)
  servers$token <- as.character(servers$token)
  servers$cpu_limit <- suppressWarnings(as.integer(servers$cpu_limit))
  servers$cpu_limit[is.na(servers$cpu_limit) | servers$cpu_limit < 1L] <- 1L
  servers <- servers[nzchar(servers$name) & nzchar(servers$url), , drop = FALSE]
  if (nrow(servers) == 0) {
    return(ugplot_default_remote_servers())
  }
  servers[!duplicated(servers$name), , drop = FALSE]
}

ugplot_write_remote_servers <- function(servers) {
  required_columns <- c("name", "url", "token", "cpu_limit")
  servers <- as.data.frame(servers, stringsAsFactors = FALSE)
  for (column_name in setdiff(required_columns, names(servers))) {
    servers[[column_name]] <- if (identical(column_name, "cpu_limit")) 1L else ""
  }
  servers <- servers[, required_columns, drop = FALSE]
  servers$name <- trimws(as.character(servers$name))
  servers$url <- trimws(as.character(servers$url))
  servers$token <- as.character(servers$token)
  servers$cpu_limit <- suppressWarnings(as.integer(servers$cpu_limit))
  servers$cpu_limit[is.na(servers$cpu_limit) | servers$cpu_limit < 1L] <- 1L
  servers <- servers[nzchar(servers$name) & nzchar(servers$url), , drop = FALSE]
  if (nrow(servers) == 0) {
    servers <- ugplot_default_remote_servers()
  }
  servers <- servers[!duplicated(servers$name, fromLast = TRUE), , drop = FALSE]
  saveRDS(servers, ugplot_remote_servers_path())
  invisible(servers)
}

ugplot_upsert_remote_server <- function(name, url, token = "", cpu_limit = 1L) {
  name <- trimws(as.character(name %||% ""))
  url <- trimws(as.character(url %||% ""))
  token <- as.character(token %||% "")
  cpu_limit <- suppressWarnings(as.integer(cpu_limit %||% 1L))
  if (is.na(cpu_limit) || cpu_limit < 1L) {
    cpu_limit <- 1L
  }
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
    data.frame(name = name, url = url, token = token, cpu_limit = cpu_limit, stringsAsFactors = FALSE)
  )
  ugplot_write_remote_servers(servers)
}

ugplot_remove_remote_server <- function(name) {
  name <- trimws(as.character(name %||% ""))
  servers <- ugplot_read_remote_servers()
  servers <- servers[servers$name != name, , drop = FALSE]
  ugplot_write_remote_servers(servers)
}
