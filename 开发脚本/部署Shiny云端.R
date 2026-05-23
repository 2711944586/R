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
  prepare_shiny_widget_manifest()
  invisible(TRUE)
}

prepare_shiny_widget_manifest <- function(widget_dir = file.path("网站发布", "交互组件"),
                                          fallback_dir = file.path("分析输出", "交互组件"),
                                          out = file.path("仪表盘", "www", "widget_manifest.csv"),
                                          shiny_widget_dir = file.path("仪表盘", "www", "交互组件")) {
  if (!dir.exists(widget_dir)) widget_dir <- fallback_dir
  files <- if (dir.exists(widget_dir)) {
    list.files(widget_dir, pattern = "[.]html$", full.names = TRUE)
  } else {
    character()
  }
  dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
  dir.create(shiny_widget_dir, recursive = TRUE, showWarnings = FALSE)
  if (!length(files)) {
    utils::write.csv(data.frame(), out, row.names = FALSE, fileEncoding = "UTF-8")
    return(invisible(out))
  }
  old_local <- list.files(shiny_widget_dir, pattern = "[.]html$", full.names = TRUE)
  stale <- setdiff(basename(old_local), basename(files))
  if (length(stale)) {
    unlink(file.path(shiny_widget_dir, stale), force = TRUE)
  }
  copied <- file.copy(files, shiny_widget_dir, overwrite = TRUE)
  if (any(!copied)) {
    warning("Some standalone widget HTML files were not copied to Shiny www.")
  }
  pretty <- function(file) {
    x <- tools::file_path_sans_ext(basename(file))
    x <- sub("^[0-9]+_", "", x)
    x <- sub("^iadv_", "", x)
    x <- sub("^imap_", "", x)
    x <- sub("^widget_", "", x)
    x <- gsub("_", " ", x)
    x <- tools::toTitleCase(x)
    caps <- c(Che = "CHE", Oops = "OOPS", Gdp = "GDP", U5mr = "U5MR",
              Dt = "DT", Hf = "HF", Hc = "HC", Ext = "EXT",
              Sdg = "SDG", Uhc = "UHC")
    for (nm in names(caps)) x <- gsub(paste0("\\b", nm, "\\b"), caps[[nm]], x)
    x
  }
  type_of <- function(file) {
    x <- tolower(basename(file))
    if (grepl("leaflet|imap|map|choropleth", x)) return("Leaflet")
    if (grepl("reactable|^iadv_rt_|rank", x)) return("Reactable")
    if (grepl("dt_|atlas|master_browse|table", x)) return("DT")
    if (grepl("sankey|network|force|chord|tree|diagonal", x)) return("Network")
    if (grepl("ec_|hc_|gauge|liquid|sunburst|icicle|wheel|packed", x)) return("HTML")
    "Plotly"
  }
  group_of <- function(file) {
    x <- tolower(basename(file))
    if (grepl("leaflet|imap|map|choropleth", x)) return("地图")
    if (grepl("dt_|reactable|^iadv_rt_|rank|table|atlas|browse", x)) return("表格")
    if (grepl("sankey|network|force|chord|tree|diagonal", x)) return("网络")
    if (grepl("sunburst|treemap|icicle|packed|wheel|donut|funnel", x)) return("结构")
    if (grepl("heatmap|matrix|corr|calendar", x)) return("矩阵")
    if (grepl("area|line|trend|race|timeline|stream|river|forecast", x)) return("时间")
    if (grepl("hist|density|box|violin|polar|radar|parcoords|splom", x)) return("分布")
    "专题"
  }
  out_df <- data.frame(
    file = basename(files),
    title = vapply(files, pretty, character(1)),
    group = vapply(files, group_of, character(1)),
    type = vapply(files, type_of, character(1)),
    size_mb = round(file.info(files)$size / 1024^2, 2),
    url = paste0("交互组件/",
                 utils::URLencode(basename(files), reserved = TRUE)),
    stringsAsFactors = FALSE
  )
  out_df <- out_df[order(out_df$group, out_df$type, out_df$title), ]
  utils::write.csv(out_df, out, row.names = FALSE, fileEncoding = "UTF-8")
  invisible(out)
}

read_env_or_prompt <- function(name, prompt) {
  value <- Sys.getenv(name, unset = "")
  if (!nzchar(value) && interactive()) {
    value <- readline(prompt)
  }
  value
}

deploy_shinyapps <- function(app_name = "ghs-dashboard",
                             account = Sys.getenv("SHINYAPPS_NAME", unset = "constantine114514")) {
  if (!requireNamespace("rsconnect", quietly = TRUE)) {
    stop("Package rsconnect is required. Install it with install.packages('rsconnect').")
  }
  prepare_shiny_app()
  token <- read_env_or_prompt("SHINYAPPS_TOKEN", "shinyapps.io credential 1: ")
  secret <- read_env_or_prompt("SHINYAPPS_SECRET", "shinyapps.io credential 2: ")
  if (!nzchar(account)) {
    stop("Missing shinyapps.io account name. Set SHINYAPPS_NAME or pass the account name to deploy_shinyapps().")
  }
  if (nzchar(token) && nzchar(secret)) {
    rsconnect::setAccountInfo(name = account, token = token, secret = secret)
  } else {
    saved_accounts <- rsconnect::accounts()
    has_saved <- nrow(saved_accounts) &&
      any(saved_accounts$name == account & saved_accounts$server == "shinyapps.io")
    if (!has_saved) {
      stop("Missing shinyapps.io credentials. Set SHINYAPPS_TOKEN and SHINYAPPS_SECRET, or add the account with rsconnect::setAccountInfo().")
    }
    message("[deploy] using saved shinyapps.io account: ", account)
  }
  app_files <- list.files("仪表盘", recursive = TRUE, full.names = FALSE, all.files = TRUE, no.. = TRUE)
  app_files <- app_files[!grepl("(^|/)rsconnect(/|$)", app_files)]
  rsconnect::deployApp(
    appDir = "仪表盘",
    appName = app_name,
    appTitle = "Global Health Spending Dashboard",
    appFiles = app_files,
    forceUpdate = TRUE,
    launch.browser = FALSE
  )
}

is_deploy_script_entrypoint <- function() {
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (!length(file_arg)) return(FALSE)
  identical(basename(sub("^--file=", "", file_arg[[length(file_arg)]])),
            "部署Shiny云端.R")
}

if (is_deploy_script_entrypoint()) {
  args <- commandArgs(trailingOnly = TRUE)
  app_name <- if (length(args) >= 1 && nzchar(args[[1]])) args[[1]] else "ghs-dashboard"
  deploy_shinyapps(app_name = app_name)
}
