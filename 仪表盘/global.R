
.find_proj_root <- function(start = getwd(), max_up = 4) {
  d <- normalizePath(start, mustWork = FALSE)
  candidates <- character()
  for (i in seq_len(max_up + 1)) {
    candidates <- c(candidates, d)
    if (dir.exists(file.path(d, "程序")) &&
        file.exists(file.path(d, "DESCRIPTION"))) return(d)
    d <- dirname(d)
  }
  for (d in candidates) {
    if (dir.exists(file.path(d, "程序库")) &&
        file.exists(file.path(d, "global.R"))) return(d)
  }
  start
}
proj_root_env <- .find_proj_root()
if (!identical(normalizePath(getwd(), mustWork = FALSE),
                normalizePath(proj_root_env, mustWork = FALSE))) {
  setwd(proj_root_env)
}
cat("[global.R] proj root:", proj_root_env, "\n")

app_dir_env <- if (basename(proj_root_env) == "仪表盘") {
  proj_root_env
} else {
  file.path(proj_root_env, "仪表盘")
}

source_dir <- if (dir.exists(file.path(proj_root_env, "程序库"))) {
  file.path(proj_root_env, "程序库")
} else if (dir.exists(file.path(app_dir_env, "程序库"))) {
  file.path(app_dir_env, "程序库")
} else if (dir.exists(file.path(proj_root_env, "程序"))) {
  file.path(proj_root_env, "程序")
} else {
  file.path(app_dir_env, "程序库")
}
for (f in sort(list.files(source_dir, pattern = "\\.R$", full.names = TRUE))) {
  source(f, encoding = "UTF-8")
}

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

mods_dir <- if (dir.exists(file.path(app_dir_env, "模块"))) {
  file.path(app_dir_env, "模块")
} else if (dir.exists(file.path(proj_root_env, "仪表盘", "模块"))) {
  file.path(proj_root_env, "仪表盘", "模块")
} else {
  file.path(proj_root_env, "模块")
}
mods_files <- sort(list.files(mods_dir, pattern = "\\.R$", full.names = TRUE))
for (f in mods_files) source(f, encoding = "UTF-8")

cache_dir <- if (dir.exists(file.path(proj_root_env, "派生数据", "处理结果"))) {
  file.path(proj_root_env, "派生数据", "处理结果")
} else {
  file.path(app_dir_env, "派生数据", "处理结果")
}
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
master_cache <- file.path(cache_dir, "master_enriched.rds")
snapshot_cache <- if (dir.exists(file.path(app_dir_env, "数据快照"))) {
  file.path(app_dir_env, "数据快照", "snapshot.rds")
} else if (dir.exists(file.path(proj_root_env, "仪表盘", "数据快照"))) {
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

ghs_theme <- bslib::bs_theme(
  version      = 5,
  preset       = "shiny",
  bg           = "#f4f5f0",
  fg           = "#17211f",
  primary      = "#254f5c",
  secondary    = "#5d6965",
  success      = "#587669",
  info         = "#6b6077",
  warning      = "#8f743d",
  danger       = "#8b544e",
  base_font    = bslib::font_google("Inter",         local = FALSE),
  heading_font = bslib::font_google("Source Serif 4", local = FALSE),
  code_font    = bslib::font_google("JetBrains Mono", local = FALSE),
  "card-border-color" = "rgba(23,33,31,.13)",
  "card-bg"           = "#fbfaf6",
  "navbar-bg"         = "#fafaf6",
  "navbar-fg"         = "#17211f",
  "border-radius"     = "8px",
  "body-secondary-color" = "#5d6965",
  "nav-link-font-weight" = "600"
)

ghs_v3_shiny_css <- htmltools::tags$style(htmltools::HTML(paste(c(
  ":root{",
    "--g3-primary:#254f5c;--g3-secondary:#6b6077;--g3-good:#587669;",
    "--g3-warn:#8f743d;--g3-bad:#8b544e;--g3-neutral:#5d6965;",
    "--g3-paper:#f4f5f0;--g3-paper2:#eef3f2;--g3-ink:#17211f;",
    "--g3-line:rgba(23,33,31,.13);--g3-line-strong:rgba(23,33,31,.20);",
    "--g3-radius:8px;--g3-shadow-sm:0 1px 2px rgba(23,33,31,.05),0 10px 26px rgba(23,33,31,.075);",
    "--g3-shadow:0 2px 4px rgba(23,33,31,.06),0 16px 34px rgba(23,33,31,.10);",
  "}",
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
  ".v3-section-head{margin:24px 0 18px}",
  ".v3-kicker{display:inline-block;font-size:11.5px;letter-spacing:.18em;text-transform:uppercase;color:#c46327;font-weight:800}",
  ".v3-section-title{font-family:'Source Serif 4',serif;font-size:24px;font-weight:700;color:#0d121b;margin:6px 0 6px}",
  ".v3-section-lead{font-size:14.5px;color:#5d667a;max-width:780px;margin:0;line-height:1.55}",
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
  ".v3-callout{display:flex;flex-direction:column;gap:6px;padding:12px 16px 14px 20px;border-radius:var(--g3-radius);background:#fff;border-left:4px solid var(--g3-primary);box-shadow:var(--g3-shadow-sm);font-size:13.5px;color:var(--g3-ink);margin:14px 0}",
  ".v3-callout-title{display:block;font-weight:800;font-size:12px;letter-spacing:.04em;text-transform:uppercase;color:var(--g3-primary)}",
  ".v3-callout-good{border-left-color:var(--g3-good)}",
  ".v3-callout-good .v3-callout-title{color:var(--g3-good)}",
  ".v3-callout-warn{border-left-color:var(--g3-warn);background:#fff8ec}",
  ".v3-callout-warn .v3-callout-title{color:var(--g3-warn)}",
  ".v3-callout-bad{border-left-color:var(--g3-bad);background:#fdf0ee}",
  ".v3-callout-bad .v3-callout-title{color:var(--g3-bad)}",
  ".v3-module-grid{display:grid;grid-template-columns:repeat(3,minmax(0,1fr));gap:14px;margin:18px 0 28px}",
  ".v3-module-card{position:relative;text-align:left;padding:18px 18px 22px 22px;background:#fff;border:1px solid var(--g3-line);border-radius:var(--g3-radius);cursor:pointer;display:flex;flex-direction:column;gap:6px;transition:transform .15s ease,box-shadow .15s ease,border-color .15s ease;min-width:0;font-family:inherit}",
  ".v3-module-card:before{content:'';position:absolute;left:0;top:0;width:4px;height:100%;background:var(--tone,var(--g3-primary))}",
  ".v3-module-card:hover{transform:translateY(-2px);box-shadow:var(--g3-shadow);border-color:var(--g3-line-strong)}",
  ".v3-module-icon{font-size:22px;line-height:1;margin-bottom:2px}",
  ".v3-module-kicker{font-size:11px;letter-spacing:.16em;text-transform:uppercase;color:var(--g3-secondary);font-weight:800}",
  ".v3-module-title{font-family:'Source Serif 4',serif;font-size:18px;color:var(--g3-ink);font-weight:700}",
  ".v3-module-desc{font-size:12.5px;color:var(--g3-neutral);line-height:1.55}",
  ".v3-card{background:#fff;border:1px solid var(--g3-line);border-radius:var(--g3-radius);box-shadow:var(--g3-shadow-sm);overflow:hidden;margin:14px 0}",
  ".v3-card-head{padding:14px 18px 8px;display:flex;flex-direction:column;gap:4px;border-bottom:1px solid var(--g3-line)}",
  ".v3-card-kicker{font-size:11px;letter-spacing:.16em;text-transform:uppercase;color:var(--g3-secondary);font-weight:800}",
  ".v3-card-title{font-family:'Source Serif 4',serif;font-size:18px;font-weight:700;color:var(--g3-ink);margin:0}",
  ".v3-card-body{padding:18px}",
  ".v3-card-footer{padding:10px 18px;border-top:1px solid var(--g3-line);font-size:12.5px;color:var(--g3-neutral);background:var(--g3-paper2)}",
  ".navbar.bg-primary{background:linear-gradient(180deg,rgba(255,253,248,.98),rgba(248,241,231,.96)) !important;color:#0d121b !important;border-bottom:1px solid rgba(13,18,27,.10) !important}",
  ".navbar-brand{font-family:'Source Serif 4',serif;font-weight:700;letter-spacing:.04em}",
  "@media(max-width:1024px){.v3-kpi-grid,.v3-stat-strip{grid-template-columns:repeat(2,1fr)}.v3-module-grid{grid-template-columns:repeat(2,1fr)}}",
  "@media(max-width:640px){.v3-kpi-grid,.v3-stat-strip,.v3-module-grid{grid-template-columns:1fr}.v3-hero{padding:28px 22px}.v3-hero-title{font-size:22px}}",
  ".kpi-card{background:#fff;border:1px solid var(--g3-line);border-radius:var(--g3-radius);padding:16px 18px;box-shadow:var(--g3-shadow-sm);min-width:0}",
  ".kpi-card .kpi-value{font-family:'Source Serif 4',serif;font-size:26px;color:var(--g3-primary);font-weight:700}",
  ".kpi-card .kpi-label{font-size:12px;color:var(--g3-neutral);letter-spacing:.04em;text-transform:uppercase;font-weight:700;margin-bottom:4px}",
  ".kpi-primary{border-left:4px solid var(--g3-primary)}",
  ".kpi-success{border-left:4px solid var(--g3-good)}",
  ".kpi-warning{border-left:4px solid var(--g3-warn)}",
  ".kpi-danger{border-left:4px solid var(--g3-bad)}",
  ".kpi-muted{border-left:4px solid var(--g3-neutral)}",
  ".panel-content{background:#fff;border-radius:var(--g3-radius);padding:18px;border:1px solid var(--g3-line);box-shadow:var(--g3-shadow-sm);margin-bottom:14px;min-width:0}",
  ".v3-hero{margin:-16px -12px 26px;border-radius:0 0 26px 26px;border-bottom:1px solid rgba(247,238,223,.14);box-shadow:0 18px 46px rgba(13,18,27,.18)}",
  ".v3-hero:before{inset:0;width:auto;height:auto;border-radius:0;background:linear-gradient(105deg,rgba(247,192,138,.10),transparent 32%,rgba(42,133,122,.10) 72%,transparent),repeating-linear-gradient(90deg,rgba(247,238,223,.06) 0 1px,transparent 1px 74px);opacity:.72}",
  ".ghs-hero::before,.ghs-hero::after{display:none !important}",
  ".ghs-hero{border-bottom:1px solid rgba(247,238,223,.14);box-shadow:0 18px 46px rgba(13,18,27,.18)}",
  ".panel-hero{background:linear-gradient(135deg,#0d121b 0%,#183754 58%,#225d58 100%);color:#f7eedf;padding:38px 44px;border-radius:0 0 24px 24px;margin:-16px -12px 24px;position:relative;overflow:hidden;border-bottom:1px solid rgba(247,238,223,.14);box-shadow:0 18px 46px rgba(13,18,27,.16)}",
  ".panel-hero:before{content:'';position:absolute;inset:0;background:linear-gradient(110deg,rgba(247,192,138,.10),transparent 35%,rgba(255,255,255,.05)),repeating-linear-gradient(90deg,rgba(247,238,223,.05) 0 1px,transparent 1px 72px);pointer-events:none}",
  ".panel-hero>*{position:relative;z-index:1;max-width:960px}",
  ".panel-hero h2{font-family:'Source Serif 4',serif;color:#f7eedf !important;font-size:clamp(26px,3vw,40px);font-weight:780;line-height:1.12;margin:0 0 12px;letter-spacing:0}",
  ".panel-hero .text-muted,.panel-hero p{color:rgba(247,238,223,.78) !important;font-size:15.5px;line-height:1.68;margin:0}",
  ".v3-card.panel-content{padding:0;margin-bottom:18px}",
  ".v3-card.panel-content:hover{transform:translateY(-1px);box-shadow:var(--g3-shadow)}",
  ".legacy-panel-card>.v3-card-body>.card-note:first-child{margin-top:0}",
  ".card-note{font-size:13px;line-height:1.7;color:var(--g3-neutral);border-left:3px solid rgba(196,99,39,.32);padding-left:12px;margin:0 0 14px;font-style:normal}",
  ".v3-page-body{max-width:1180px;margin:0 auto;padding:0 18px 28px}",
  ".v3-page-body-wide{max-width:1360px}",
  ".v3-badge-row{display:flex;flex-wrap:wrap;gap:8px;margin:14px 0 18px}",
  ".v3-badge{display:inline-flex;align-items:center;min-height:26px;padding:5px 10px;border-radius:999px;background:color-mix(in srgb,var(--tone,var(--g3-primary)) 9%,white);border:1px solid color-mix(in srgb,var(--tone,var(--g3-primary)) 24%,white);color:var(--tone,var(--g3-primary));font-size:11.5px;font-weight:800;letter-spacing:.04em}",
  ".v3-chart-guide{position:relative;margin:0 0 14px;padding:14px 16px 15px 18px;border-radius:13px;border:1px solid var(--g3-line);background:linear-gradient(180deg,#fff,#fbf8f2);box-shadow:var(--g3-shadow-sm);overflow:hidden}",
  ".v3-chart-guide:before{content:'';position:absolute;left:0;top:0;width:4px;height:100%;background:#1d3f5f}",
  ".v3-chart-guide-warn{background:#fff8ec}",
  ".v3-chart-guide-good:before{background:#2a857a}.v3-chart-guide-warn:before{background:#c89a3b}.v3-chart-guide-bad:before{background:#a23b3b}",
  ".v3-guide-title{display:block;font-size:12px;letter-spacing:.10em;text-transform:uppercase;color:#1d3f5f;margin-bottom:5px}",
  ".v3-guide-text{margin:0;color:var(--g3-neutral);font-size:13.2px;line-height:1.65}",
  ".v3-guide-list{margin:8px 0 0;padding-left:18px;color:var(--g3-neutral);font-size:12.7px;line-height:1.6}",
  ".v3-sidebar-note{margin:14px 0 4px;padding:14px 15px;border-radius:13px;background:#fff;border:1px solid var(--g3-line);box-shadow:var(--g3-shadow-sm)}",
  ".v3-sidebar-note strong{display:block;color:var(--g3-primary);font-size:11px;letter-spacing:.12em;text-transform:uppercase;margin-bottom:6px}",
  ".v3-sidebar-note p{margin:0;color:var(--g3-neutral);font-size:12.8px;line-height:1.6}",
  ".v3-sidebar-note ul{margin:8px 0 0;padding-left:18px;color:var(--g3-neutral);font-size:12.4px;line-height:1.55}",
  ".v3-rail{display:grid;grid-template-columns:repeat(4,minmax(0,1fr));gap:12px;margin:16px 0 22px}",
  ".v3-rail-item{position:relative;padding:14px 15px 15px;border-radius:13px;background:#fff;border:1px solid var(--g3-line);box-shadow:var(--g3-shadow-sm);min-width:0}",
  ".v3-rail-index{display:inline-flex;align-items:center;justify-content:center;width:32px;height:32px;border-radius:10px;background:#0d121b;color:#f7eedf;font-family:'JetBrains Mono',monospace;font-weight:800;font-size:11px;margin-bottom:9px}",
  ".v3-rail-item strong{display:block;font-size:13.5px;color:var(--g3-ink);margin-bottom:4px}",
  ".v3-rail-item p{margin:0;color:var(--g3-neutral);font-size:12.6px;line-height:1.6}",
  ".v3-story-grid{display:grid;grid-template-columns:repeat(var(--cols,3),minmax(0,1fr));gap:14px;margin:16px 0 22px}",
  ".v3-insight{position:relative;background:linear-gradient(180deg,#fff,#fbf8f2);border:1px solid var(--g3-line);border-radius:var(--g3-radius);padding:16px 17px 17px 19px;box-shadow:var(--g3-shadow-sm);overflow:hidden;min-width:0}",
  ".v3-insight:before{content:'';position:absolute;left:0;top:0;width:4px;height:100%;background:var(--tone,var(--g3-primary))}",
  ".v3-insight-icon{display:inline-flex;align-items:center;justify-content:center;width:28px;height:28px;border-radius:8px;background:color-mix(in srgb,var(--tone,var(--g3-primary)) 13%,white);color:var(--tone,var(--g3-primary));font-weight:800;margin-bottom:9px}",
  ".v3-insight-kicker{display:block;color:var(--tone,var(--g3-primary));font-size:10.5px;font-weight:800;letter-spacing:.16em;text-transform:uppercase;margin-bottom:5px}",
  ".v3-insight-title{display:block;font-family:'Source Serif 4',serif;font-size:17px;line-height:1.22;color:var(--g3-ink);font-weight:750;margin-bottom:6px}",
  ".v3-insight-text{font-size:13.2px;line-height:1.65;color:var(--g3-neutral);margin:0}",
  ".v3-insight-meta{display:block;margin-top:10px;font-size:11.5px;color:rgba(13,18,27,.52);font-weight:700}",
  ".v3-steps{display:grid;gap:12px;margin:12px 0}",
  ".v3-step{display:grid;grid-template-columns:46px minmax(0,1fr);gap:12px;align-items:start;padding:14px 15px;border:1px solid var(--g3-line);border-radius:12px;background:#fff}",
  ".v3-step-index{display:flex;align-items:center;justify-content:center;width:36px;height:36px;border-radius:10px;background:#0d121b;color:#f7eedf;font-family:'JetBrains Mono',monospace;font-size:12px;font-weight:800}",
  ".v3-step-copy strong{display:block;font-size:14px;color:var(--g3-ink);margin:0 0 4px}",
  ".v3-step-copy p{margin:0;color:var(--g3-neutral);font-size:13px;line-height:1.6}",
  ".v3-code-wrap{border-radius:14px;overflow:hidden;border:1px solid rgba(13,18,27,.18);background:#0c1424;margin:12px 0}",
  ".v3-code-title{padding:10px 14px;border-bottom:1px solid rgba(255,255,255,.10);color:#f7c08a;font-size:11px;font-weight:800;letter-spacing:.12em;text-transform:uppercase}",
  ".v3-code-block{margin:0;padding:16px 18px;background:#0c1424;color:#e6efff;font-family:'JetBrains Mono',monospace;font-size:12.5px;line-height:1.65;overflow:auto}",
  ".v3-source-note{display:flex;gap:10px;align-items:flex-start;background:#f4eadc;border:1px solid rgba(196,99,39,.18);border-radius:12px;padding:11px 13px;color:#5d667a;font-size:12.5px;line-height:1.55;margin:14px 0 4px}",
  ".v3-source-note strong{color:#1d3f5f;white-space:nowrap}",
  ".bslib-sidebar-layout>.sidebar{border-right:1px solid var(--g3-line) !important;background:linear-gradient(180deg,#f7efe3,#f1e8da) !important;box-shadow:inset -1px 0 0 rgba(255,255,255,.55)}",
  ".bslib-sidebar-layout>.main{padding:20px 24px 28px !important}",
  ".bslib-sidebar-layout .form-label,.bslib-sidebar-layout label.control-label{font-size:11px;font-weight:850;letter-spacing:.10em;text-transform:uppercase;color:#1d3f5f;margin-bottom:6px}",
  ".form-control,.form-select,.bootstrap-select>.dropdown-toggle,.selectize-input{border-radius:10px !important;border:1px solid var(--g3-line-strong) !important;background:#fff !important;box-shadow:none !important;font-size:13px !important}",
  ".irs--shiny .irs-bar,.irs--shiny .irs-single{background:#1d3f5f !important;border-color:#1d3f5f !important}",
  ".irs--shiny .irs-handle{border-color:#1d3f5f !important;background:#fff !important}",
  ".btn,.btn-default,.btn-primary{border-radius:10px !important;font-weight:750 !important;letter-spacing:.01em}",
  ".btn-primary{background:#1d3f5f !important;border-color:#1d3f5f !important}",
  ".nav-tabs .nav-link{color:#5d667a !important;border-radius:10px 10px 0 0 !important}",
  ".nav-tabs .nav-link.active{color:#0d121b !important;background:#fff !important;border-color:var(--g3-line) var(--g3-line) #fff !important}",
  ".card,.bslib-card{border-color:var(--g3-line) !important;border-radius:var(--g3-radius) !important;box-shadow:var(--g3-shadow-sm) !important}",
  ".table{font-size:13px;color:#0d121b}",
  ".table thead th{font-size:11px;letter-spacing:.08em;text-transform:uppercase;color:#5d667a;border-bottom-color:var(--g3-line-strong)}",
  ".plotly,.leaflet,.html-widget{border-radius:12px;overflow:hidden}",
  ".leaflet-container{background:#eef2ed}",
  ".rt-table{border-radius:12px;overflow:hidden}",
  ".rt-th{background:#f1e8da !important;color:#5d667a !important}",
  ".v3-hero,.ghs-hero,.panel-hero{background:#eef3f2 !important;background-image:none !important;color:#17211f !important;border-radius:0 0 8px 8px !important;box-shadow:0 1px 0 rgba(255,255,255,.85) inset,0 12px 28px rgba(23,33,31,.07) !important}",
  ".v3-hero:before,.v3-hero:after,.ghs-hero:before,.ghs-hero:after,.panel-hero:before,.panel-hero:after,.v3-kpi:before,.v3-insight:before,.v3-chart-guide:before,.v3-module-card:before,.kpi-card:before{content:none !important;display:none !important;background:none !important}",
  ".v3-card,.panel-content,.v3-kpi,.v3-insight,.v3-chart-guide,.v3-sidebar-note,.kpi-card,.card,.bslib-card{background:#fbfaf6 !important;background-image:none !important;border:1px solid rgba(23,33,31,.13) !important;border-left:1px solid rgba(23,33,31,.13) !important;border-radius:8px !important;box-shadow:0 1px 2px rgba(23,33,31,.05),0 10px 26px rgba(23,33,31,.075) !important}",
  ".navbar,.navbar.bg-primary,nav.navbar{background:#fafaf6 !important;background-image:none !important;color:#17211f !important;border-bottom:1px solid rgba(23,33,31,.13) !important;box-shadow:0 1px 0 rgba(255,255,255,.72),0 10px 24px rgba(23,33,31,.065) !important}",
  "@media(max-width:1024px){.v3-story-grid,.v3-rail{grid-template-columns:repeat(2,minmax(0,1fr))}.panel-hero{padding:32px 28px}.v3-page-body{padding-left:8px;padding-right:8px}}",
  "@media(max-width:640px){.v3-story-grid,.v3-rail{grid-template-columns:1fr}.panel-hero{padding:26px 20px}.panel-hero h2{font-size:23px}.bslib-sidebar-layout>.main{padding:16px 14px 24px !important}.v3-page-body{padding-left:0;padding-right:0}}"
), collapse = "")))

kpi_card <- function(label, value, icon = NULL, color = "primary") {
  mod_kpi(label, value, color = color, icon = icon)
}
with_spinner <- function(x) mod_spinner(x)

ghs_shiny_css <- ghs_v3_shiny_css
