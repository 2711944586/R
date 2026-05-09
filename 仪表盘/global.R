# =============================================================================
# 仪表盘/global.R  ---  Shiny v2 仪表盘 · 共享对象
# 启动时只执行一次。模块化架构：仪表盘/模块/mod_*.R
# =============================================================================

# ---- 1. 路径自适应（Shiny runApp("仪表盘") 启动时 cwd=仪表盘/） -----------------
# 沿 cwd 向上爬，直到找到含 程序/ 子目录的项目根
.find_proj_root <- function(start = getwd(), max_up = 4) {
  d <- normalizePath(start, mustWork = FALSE)
  for (i in seq_len(max_up + 1)) {
    if (dir.exists(file.path(d, "程序")) &&
        file.exists(file.path(d, "DESCRIPTION"))) return(d)
    if (dir.exists(file.path(d, "程序库")) &&
        file.exists(file.path(d, "global.R"))) return(d)
    d <- dirname(d)
  }
  start  # fallback
}
proj_root_env <- .find_proj_root()
if (!identical(normalizePath(getwd(), mustWork = FALSE),
                normalizePath(proj_root_env, mustWork = FALSE))) {
  setwd(proj_root_env)
}
cat("[global.R] proj root:", proj_root_env, "\n")

# ---- 2. 加载项目函数库（v2: 程序/00–20） --------------------------------------
source_dir <- if (dir.exists(file.path(proj_root_env, "程序"))) {
  file.path(proj_root_env, "程序")
} else {
  file.path(proj_root_env, "程序库")
}
for (f in list.files(source_dir, pattern = "\\.R$", full.names = TRUE)) {
  source(f, encoding = "UTF-8")
}

# ---- 3. 必备包 -------------------------------------------------------------
suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(plotly)
  library(leaflet)
  library(DT)
  library(reactable)
  library(shinyWidgets)
  library(shinycssloaders)
  library(htmltools)
})

# ---- 3b. v2 设计系统 -------------------------------------------------------
# 字体 + theme_ghs2 一并注册，所有模块图表都会自动应用
suppressWarnings(suppressMessages({
  if (exists("register_brand_fonts", mode = "function"))
    register_brand_fonts()
  if (exists("theme_ghs2", mode = "function"))
    ggplot2::theme_set(theme_ghs2(base_size = 11))
}))

# ---- 3c. 加载所有 Shiny 模块（_helpers.R 优先） ---------------------------
mods_dir <- if (dir.exists(file.path(proj_root_env, "仪表盘", "模块"))) {
  file.path(proj_root_env, "仪表盘", "模块")
} else {
  file.path(proj_root_env, "模块")
}
mods_files <- sort(list.files(mods_dir, pattern = "\\.R$", full.names = TRUE))
for (f in mods_files) source(f, encoding = "UTF-8")

# ---- 4. 准备数据（带磁盘缓存以避免每次冷启动 30s） -------------------------
cache_dir <- file.path(proj_root_env, "派生数据", "处理结果")
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
master_cache <- file.path(cache_dir, "master_enriched.rds")
snapshot_cache <- if (dir.exists(file.path(proj_root_env, "仪表盘", "数据快照"))) {
  file.path(proj_root_env, "仪表盘", "数据快照", "snapshot.rds")
} else {
  file.path(proj_root_env, "数据快照", "snapshot.rds")
}
if (file.exists(snapshot_cache)) {
  master_enriched <- readRDS(snapshot_cache)
} else if (file.exists(master_cache)) {
  master_enriched <- readRDS(master_cache)
} else {
  ghs <- load_ghs()
  master_enriched <- enrich_master(build_master_wide(ghs), with_wdi = TRUE)
  saveRDS(master_enriched, master_cache)
}

# 简化下拉菜单选项
country_choices <- master_enriched |>
  dplyr::distinct(.data$iso3_code, .data$country_name) |>
  dplyr::arrange(.data$country_name)
country_choices_named <- stats::setNames(country_choices$iso3_code,
                                          country_choices$country_name)

year_min <- min(master_enriched$year, na.rm = TRUE)
year_max <- max(master_enriched$year, na.rm = TRUE)

continent_choices <- sort(unique(stats::na.omit(master_enriched$continent)))
income_choices    <- c("Low income", "Lower middle income",
                       "Upper middle income", "High income")

# 指标字典（供下拉菜单使用）
indicator_choices <- list(
  "OOPS \u5360\u6bd4 (% of CHE)"          = "hf3_che",
  "\u653f\u5e9c\u5360\u6bd4 GGHE-D (%)"   = "gghed_che",
  "\u79c1\u4eba\u5360\u6bd4 PVT-D (%)"    = "pvtd_che",
  "\u5916\u63f4\u5360\u6bd4 EXT (%)"      = "ext_che",
  "\u4eba\u5747 CHE (USD 2023)"           = "che_pc_usd2023",
  "\u603b CHE (USD 2023)"                  = "che_usd2023",
  "\u9884\u9632\u5360\u6bd4 hc6 (%)"       = "hc6_che",
  "\u6cbb\u7597\u5360\u6bd4 hc1 (%)"       = "hc1_che"
)

# 世界地图（用 sf 简化版本，启动慢，所以也缓存）
world_sf_cache <- file.path(cache_dir, "world_sf_medium.rds")
world_sf_obj <- tryCatch({
  if (file.exists(world_sf_cache)) {
    readRDS(world_sf_cache)
  } else {
    ws <- load_world_sf(scale = "medium", simplify_keep = 0.1)
    saveRDS(ws, world_sf_cache)
    ws
  }
}, error = function(e) NULL)

# ---- 5. UI 主题（v2 bslib v5 + 莫兰迪品牌） --------------------------------
ghs_theme <- bslib::bs_theme(
  version      = 5,
  preset       = "shiny",
  bg           = "#FAF7F2",
  fg           = "#1A1A1F",
  primary      = "#1B5E88",
  secondary    = "#5A5A65",
  success      = "#6B8E5A",
  info         = "#3F88AE",
  warning      = "#E07B00",
  danger       = "#C46B27",
  base_font    = bslib::font_google("Inter",         local = FALSE),
  heading_font = bslib::font_google("Source Serif 4", local = FALSE),
  code_font    = bslib::font_google("JetBrains Mono", local = FALSE),
  "card-border-color" = "#1A1A1F1F",
  "navbar-bg"         = "#FAF7F2",
  "border-radius"     = "10px"
)

# ---- 6. 兼容旧 helpers（kpi_card / with_spinner）-------------------------
# v2 模块用 mod_kpi() / mod_spinner()（在 _helpers.R），保留旧名作为别名以兼容
kpi_card <- function(label, value, icon = NULL, color = "primary") {
  mod_kpi(label, value, color = color, icon = icon)
}
with_spinner <- function(x) mod_spinner(x)
