if (!dir.exists("程序") && basename(getwd()) == "开发脚本") setwd("..")

source_all <- function() {
  files <- list.files("程序", pattern = "[.]R$", full.names = TRUE)
  for (f in files) source(f, encoding = "UTF-8")
  invisible(TRUE)
}

prepare_shiny_app <- function() {
  source_all()
  if (!exists("load_ghs", mode = "function") ||
      !exists("enrich_master", mode = "function") ||
      !exists("build_master_wide", mode = "function")) {
    stop("Required data functions are not available after sourcing 程序/*.R")
  }
  load_ghs_fn <- get("load_ghs", mode = "function")
  enrich_master_fn <- get("enrich_master", mode = "function")
  build_master_wide_fn <- get("build_master_wide", mode = "function")
  cache <- file.path("派生数据", "处理结果", "master_enriched.rds")
  if (file.exists(cache)) {
    master <- readRDS(cache)
  } else {
    ghs <- load_ghs_fn()
    master <- enrich_master_fn(build_master_wide_fn(ghs), with_wdi = TRUE)
    dir.create(dirname(cache), recursive = TRUE, showWarnings = FALSE)
    saveRDS(master, cache)
  }
  dir.create(file.path("仪表盘", "数据快照"), recursive = TRUE, showWarnings = FALSE)
  saveRDS(master, file.path("仪表盘", "数据快照", "snapshot.rds"))
  if (dir.exists(file.path("仪表盘", "程序库"))) {
    unlink(file.path("仪表盘", "程序库"), recursive = TRUE, force = TRUE)
  }
  dir.create(file.path("仪表盘", "程序库"), recursive = TRUE, showWarnings = FALSE)
  file.copy(list.files("程序", pattern = "[.]R$", full.names = TRUE),
            file.path("仪表盘", "程序库"), overwrite = TRUE)
  invisible(TRUE)
}

read_env_or_prompt <- function(name, prompt) {
  value <- Sys.getenv(name, unset = "")
  if (!nzchar(value) && interactive()) {
    value <- readline(prompt)
  }
  value
}

deploy_shinyapps <- function(app_name = "ghs-dashboard-v2",
                             account = Sys.getenv("SHINYAPPS_NAME", unset = "constantine1433223")) {
  if (!requireNamespace("rsconnect", quietly = TRUE)) {
    stop("Package rsconnect is required. Install it with install.packages('rsconnect').")
  }
  prepare_shiny_app()
  token <- read_env_or_prompt("SHINYAPPS_TOKEN", "shinyapps.io credential 1: ")
  secret <- read_env_or_prompt("SHINYAPPS_SECRET", "shinyapps.io credential 2: ")
  if (!nzchar(account) || !nzchar(token) || !nzchar(secret)) {
    stop("Missing shinyapps.io credentials. Set SHINYAPPS_NAME, SHINYAPPS_TOKEN and SHINYAPPS_SECRET in .Renviron or the current R session.")
  }
  rsconnect::setAccountInfo(name = account, token = token, secret = secret)
  app_files <- list.files("仪表盘", recursive = TRUE, full.names = FALSE, all.files = TRUE, no.. = TRUE)
  app_files <- app_files[!grepl("(^|/)rsconnect(/|$)", app_files)]
  rsconnect::deployApp(
    appDir = "仪表盘",
    appName = app_name,
    appTitle = "Global Health Spending Dashboard",
    appFiles = app_files,
    forceUpdate = TRUE,
    launch.browser = TRUE
  )
}

args <- commandArgs(trailingOnly = TRUE)
app_name <- if (length(args) >= 1 && nzchar(args[[1]])) args[[1]] else "ghs-dashboard-v2"
deploy_shinyapps(app_name = app_name)
