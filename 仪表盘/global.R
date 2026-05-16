# =============================================================================
# 仪表盘/global.R  ---  Shiny 仪表盘 · 共享对象
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
# setwd() 说明：shinyapps.io 部署时 cwd 被设为 app 目录（仪表盘/），
# 但项目函数库与数据缓存位于上层项目根。此处 setwd() 确保 source() 和
# readRDS() 的相对路径在本地 runApp() 与云端部署中都能正确解析。
if (!identical(normalizePath(getwd(), mustWork = FALSE),
                normalizePath(proj_root_env, mustWork = FALSE))) {
  setwd(proj_root_env)
}
cat("[global.R] proj root:", proj_root_env, "\n")

# ---- 2. 加载项目函数库（程序/00–25） --------------------------------------
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

# ---- 3b. 设计系统 -------------------------------------------------------
suppressWarnings(suppressMessages({
  if (exists("register_brand_fonts", mode = "function"))
    register_brand_fonts()
  if (exists("theme_ghs3", mode = "function")) {
    ggplot2::theme_set(theme_ghs3(base_size = 11, mode = "light",
                                   variant = "default"))
  } else if (exists("theme_ghs2", mode = "function")) {
    ggplot2::theme_set(theme_ghs2(base_size = 11))
  } else if (exists("theme_ghs", mode = "function")) {
    ggplot2::theme_set(theme_ghs(base_size = 11))
  }
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

# ---- 5. UI 主题（bslib + 语义槽） ------------------------------------
# 主题色板与 程序/13_design_system.R 的 palette 对齐，确保 Shiny 与
# 静态 HTML 呈现一致
ghs_theme <- bslib::bs_theme(
  version      = 5,
  preset       = "shiny",
  bg           = "#fbf6ee",     # paper
  fg           = "#0d121b",     # ink
  primary      = "#1d3f5f",     # primary
  secondary    = "#5d667a",     # neutral
  success      = "#2a857a",     # good
  info         = "#3a6e8f",
  warning      = "#c89a3b",     # warn
  danger       = "#a23b3b",     # bad
  base_font    = bslib::font_google("Inter",         local = FALSE),
  heading_font = bslib::font_google("Source Serif 4", local = FALSE),
  code_font    = bslib::font_google("JetBrains Mono", local = FALSE),
  "card-border-color" = "rgba(13,18,27,.10)",
  "card-bg"           = "#fbf6ee",
  "navbar-bg"         = "#0d121b",          # 深色玻璃态导航
  "navbar-fg"         = "#f7eedf",
  "border-radius"     = "14px",
  "body-secondary-color" = "#5d667a"
)

# ---- 5b. 注入 CSS（Shiny 全局样式） -------------------------------------
# 让 Shiny 内的组件类能用
ghs_v3_shiny_css <- htmltools::tags$style(htmltools::HTML(paste(c(
  ":root{",
    "--g3-primary:#1d3f5f;--g3-secondary:#c46327;--g3-good:#2a857a;",
    "--g3-warn:#c89a3b;--g3-bad:#a23b3b;--g3-neutral:#5d667a;",
    "--g3-paper:#fbf6ee;--g3-paper2:#f1e8da;--g3-ink:#0d121b;",
    "--g3-line:rgba(13,18,27,.10);--g3-line-strong:rgba(13,18,27,.18);",
    "--g3-radius:14px;--g3-shadow-sm:0 4px 12px rgba(13,18,27,.06);",
    "--g3-shadow:0 16px 40px rgba(13,18,27,.08);",
  "}",
  # ---- Hero ----
  ".v3-hero{background:linear-gradient(135deg,#0d121b 0%,#1d3f5f 100%);color:#f7eedf;padding:42px 48px;border-radius:0 0 24px 24px;margin:0 0 24px;position:relative;overflow:hidden}",
  ".v3-hero:before{content:'';position:absolute;right:-80px;top:-80px;width:340px;height:340px;border-radius:50%;background:radial-gradient(circle,rgba(247,192,138,.18) 0%,transparent 70%)}",
  ".v3-hero-inner{position:relative;z-index:2;max-width:1200px}",
  ".v3-hero-kicker{display:inline-block;font-size:11.5px;letter-spacing:.18em;text-transform:uppercase;color:rgba(247,238,223,.66);font-weight:800;margin-bottom:10px}",
  ".v3-hero-title{font-family:'Source Serif 4',serif;font-size:clamp(28px,3.5vw,44px);font-weight:700;line-height:1.1;margin:0 0 14px;color:#f7eedf}",
  ".v3-hero-lead{font-size:16px;color:rgba(247,238,223,.78);max-width:760px;margin:0 0 14px;line-height:1.55}",
  ".v3-hero-meta{display:flex;flex-wrap:wrap;gap:8px;font-size:12.5px;color:rgba(247,238,223,.6)}",
  ".v3-hero-meta a{color:#f7c08a;text-decoration:none;border-bottom:1px dotted rgba(247,192,138,.5);padding-bottom:1px}",
  ".v3-hero-meta a:hover{color:#fff;border-color:#fff}",
  ".v3-meta-sep{margin:0 4px;color:rgba(247,238,223,.34)}",
  # ---- Section head ----
  ".v3-section-head{margin:24px 0 18px}",
  ".v3-kicker{display:inline-block;font-size:11.5px;letter-spacing:.18em;text-transform:uppercase;color:#c46327;font-weight:800}",
  ".v3-section-title{font-family:'Source Serif 4',serif;font-size:24px;font-weight:700;color:#0d121b;margin:6px 0 6px}",
  ".v3-section-lead{font-size:14.5px;color:#5d667a;max-width:780px;margin:0;line-height:1.55}",
  # ---- KPI ----
  ".v3-kpi-grid{display:grid;grid-template-columns:repeat(4,minmax(0,1fr));gap:16px;margin:18px 0}",
  ".v3-kpi{position:relative;padding:18px 20px;border:1px solid var(--g3-line);border-radius:var(--g3-radius);background:#fff;box-shadow:var(--g3-shadow-sm);overflow:hidden;min-width:0}",
  ".v3-kpi:before{content:'';position:absolute;left:0;top:0;width:5px;height:100%;background:var(--tone,var(--g3-primary))}",
  ".v3-kpi-value{font-family:'Source Serif 4',serif;font-size:28px;font-weight:700;color:var(--g3-primary);line-height:1.05}",
  ".v3-kpi-label{margin-top:6px;font-weight:700;font-size:12.5px;letter-spacing:.04em;color:var(--g3-ink);display:flex;align-items:center;justify-content:space-between;gap:8px}",
  ".v3-kpi-trend{font-weight:700;font-size:11.5px;font-family:'Source Serif 4',serif}",
  ".v3-kpi-hint{margin-top:6px;font-size:11.5px;color:var(--g3-neutral);line-height:1.5}",
  ".v3-stat-strip{display:grid;grid-template-columns:repeat(4,minmax(0,1fr));gap:14px;margin:18px 0}",
  ".v3-stat-cell{padding:14px 16px;background:#fff;border:1px solid var(--g3-line);border-radius:var(--g3-radius);min-width:0}",
  ".v3-stat-value{font-family:'Source Serif 4',serif;font-size:24px;font-weight:700;color:var(--g3-primary);line-height:1.05}",
  ".v3-stat-label{margin-top:4px;color:var(--g3-neutral);font-size:12.5px}",
  # ---- Callout ----
  ".v3-callout{display:flex;flex-direction:column;gap:6px;padding:12px 16px 14px 20px;border-radius:var(--g3-radius);background:#fff;border-left:4px solid var(--g3-primary);box-shadow:var(--g3-shadow-sm);font-size:13.5px;color:var(--g3-ink);margin:14px 0}",
  ".v3-callout-title{display:block;font-weight:800;font-size:12px;letter-spacing:.04em;text-transform:uppercase;color:var(--g3-primary)}",
  ".v3-callout-good{border-left-color:var(--g3-good)}",
  ".v3-callout-good .v3-callout-title{color:var(--g3-good)}",
  ".v3-callout-warn{border-left-color:var(--g3-warn);background:#fff8ec}",
  ".v3-callout-warn .v3-callout-title{color:var(--g3-warn)}",
  ".v3-callout-bad{border-left-color:var(--g3-bad);background:#fdf0ee}",
  ".v3-callout-bad .v3-callout-title{color:var(--g3-bad)}",
  # ---- Module cards ----
  ".v3-module-grid{display:grid;grid-template-columns:repeat(3,minmax(0,1fr));gap:14px;margin:18px 0 28px}",
  ".v3-module-card{position:relative;text-align:left;padding:18px 18px 22px 22px;background:#fff;border:1px solid var(--g3-line);border-radius:var(--g3-radius);cursor:pointer;display:flex;flex-direction:column;gap:6px;transition:transform .15s ease,box-shadow .15s ease,border-color .15s ease;min-width:0;font-family:inherit}",
  ".v3-module-card:before{content:'';position:absolute;left:0;top:0;width:4px;height:100%;background:var(--tone,var(--g3-primary))}",
  ".v3-module-card:hover{transform:translateY(-2px);box-shadow:var(--g3-shadow);border-color:var(--g3-line-strong)}",
  ".v3-module-icon{font-size:22px;line-height:1;margin-bottom:2px}",
  ".v3-module-kicker{font-size:11px;letter-spacing:.16em;text-transform:uppercase;color:var(--g3-secondary);font-weight:800}",
  ".v3-module-title{font-family:'Source Serif 4',serif;font-size:18px;color:var(--g3-ink);font-weight:700}",
  ".v3-module-desc{font-size:12.5px;color:var(--g3-neutral);line-height:1.55}",
  # ---- Card ----
  ".v3-card{background:#fff;border:1px solid var(--g3-line);border-radius:var(--g3-radius);box-shadow:var(--g3-shadow-sm);overflow:hidden;margin:14px 0}",
  ".v3-card-head{padding:14px 18px 8px;display:flex;flex-direction:column;gap:4px;border-bottom:1px solid var(--g3-line)}",
  ".v3-card-kicker{font-size:11px;letter-spacing:.16em;text-transform:uppercase;color:var(--g3-secondary);font-weight:800}",
  ".v3-card-title{font-family:'Source Serif 4',serif;font-size:18px;font-weight:700;color:var(--g3-ink);margin:0}",
  ".v3-card-body{padding:18px}",
  ".v3-card-footer{padding:10px 18px;border-top:1px solid var(--g3-line);font-size:12.5px;color:var(--g3-neutral);background:var(--g3-paper2)}",
  # ---- Navbar 升级 ----
  ".navbar.bg-primary{background:linear-gradient(90deg,#0d121b 0%,#1d3f5f 100%) !important}",
  ".navbar-brand{font-family:'Source Serif 4',serif;font-weight:700;letter-spacing:.04em}",
  # ---- 响应断点 ----
  "@media(max-width:1024px){.v3-kpi-grid,.v3-stat-strip{grid-template-columns:repeat(2,1fr)}.v3-module-grid{grid-template-columns:repeat(2,1fr)}}",
  "@media(max-width:640px){.v3-kpi-grid,.v3-stat-strip,.v3-module-grid{grid-template-columns:1fr}.v3-hero{padding:28px 22px}.v3-hero-title{font-size:22px}}",
  # ---- Override 旧 panel-content（kpi-card） ----
  ".kpi-card{background:#fff;border:1px solid var(--g3-line);border-radius:var(--g3-radius);padding:16px 18px;box-shadow:var(--g3-shadow-sm);min-width:0}",
  ".kpi-card .kpi-value{font-family:'Source Serif 4',serif;font-size:26px;color:var(--g3-primary);font-weight:700}",
  ".kpi-card .kpi-label{font-size:12px;color:var(--g3-neutral);letter-spacing:.04em;text-transform:uppercase;font-weight:700;margin-bottom:4px}",
  ".kpi-primary{border-left:4px solid var(--g3-primary)}",
  ".kpi-success{border-left:4px solid var(--g3-good)}",
  ".kpi-warning{border-left:4px solid var(--g3-warn)}",
  ".kpi-danger{border-left:4px solid var(--g3-bad)}",
  ".kpi-muted{border-left:4px solid var(--g3-neutral)}",
  ".panel-content{background:#fff;border-radius:var(--g3-radius);padding:18px;border:1px solid var(--g3-line);box-shadow:var(--g3-shadow-sm);margin-bottom:14px;min-width:0}"
), collapse = "")))

# ---- 6. 兼容旧 helpers（kpi_card / with_spinner）-------------------------
# 模块用 mod_kpi() / mod_spinner()（在 _helpers.R），保留旧名作为别名以兼容
kpi_card <- function(label, value, icon = NULL, color = "primary") {
  mod_kpi(label, value, color = color, icon = icon)
}
with_spinner <- function(x) mod_spinner(x)

# ---- 语义别名（移除版本后缀） -----------------------------------------------
ghs_shiny_css <- ghs_v3_shiny_css
