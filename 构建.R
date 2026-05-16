# =============================================================================
# 构建.R  ---  统一本地构建脚本
# -----------------------------------------------------------------------------
# 用法：
#   Rscript 构建.R all       # 全量：data + features + models + figures + widgets + submission + shiny
#   Rscript 构建.R data      # 仅生成 master_enriched + external 缓存
#   Rscript 构建.R features  # 生成统一 country-year 特征表 + 变量字典
#   Rscript 构建.R models    # 仅跑统计与机器学习模型 → 分析输出/模型表/
#   Rscript 构建.R figures   # 仅生成静态图 → 分析输出/图表/
#   Rscript 构建.R widgets   # 仅生成交互 widget → 分析输出/交互组件/
#   Rscript 构建.R shinylive # 仅编译 shinylive → 网站发布/仪表盘/
#   Rscript 构建.R submission# 生成课程提交与可转发静态 HTML
#   Rscript 构建.R deploy    # submission + shinylive + copy widgets → 网站发布/（部署用）
#   Rscript 构建.R delivery  # 刷新课程 HTML 与质量报告（默认不打 庄颂_20241334/ 包；
#                           #   设 GHS_BUILD_DELIVERY=TRUE 才会生成最终交付包）
#   Rscript 构建.R clean     # 删除 网站发布/ 与 分析输出/（慎用）
#   Rscript 构建.R audit     # 检查项目主要交付目录是否齐备
# =============================================================================

args <- commandArgs(trailingOnly = TRUE)
target <- if (length(args)) args[[1]] else "audit"

cat("[make] target =", target, "\n")
cat("[make] cwd    =", getwd(), "\n")

# ---- GHS_STRICT 模式：错误即停止，用于 release 构建 --------------------------
.ghs_strict <- isTRUE(as.logical(Sys.getenv("GHS_STRICT", "FALSE")))
if (.ghs_strict) {
  cat("[make] GHS_STRICT=TRUE: errors will stop() instead of continuing\n")
  options(warn = 2)  # warnings → errors
}
.ghs_try <- function(expr, label = "") {

  if (.ghs_strict) {
    tryCatch(expr, error = function(e) {
      stop(sprintf("[STRICT FAIL] %s: %s", label, conditionMessage(e)))
    })
  } else {
    tryCatch(expr, error = function(e) {
      cat(sprintf("[make] %s err: %s\n", label, conditionMessage(e)))
    })
  }
}

# ---- common ---------------------------------------------------------------
source_all_r <- function() {
  for (f in list.files("程序", pattern = "\\.R$", full.names = TRUE)) {
    source(f, encoding = "UTF-8")
  }
}

ensure_cache <- function() {
  source_all_r()
  cache <- file.path("派生数据", "处理结果", "master_enriched.rds")
  if (file.exists(cache)) {
    master <- readRDS(cache)
    cat("[make] cache hit: master_enriched rows =", nrow(master), "\n")
  } else {
    ghs <- load_ghs()
    master <- enrich_master(build_master_wide(ghs), with_wdi = TRUE)
    dir.create(dirname(cache), recursive = TRUE, showWarnings = FALSE)
    saveRDS(master, cache)
    cat("[make] built master_enriched rows =", nrow(master), "\n")
  }
  master
}

# ---- 目标分发 --------------------------------------------------------------

target_data <- function() {
  master <- ensure_cache()
  ext <- tryCatch(load_external_all(), error = function(e) NULL)
  cat("[make] external sources:\n")
  for (nm in names(ext)) cat(sprintf("  - %-6s: %s\n", nm,
    if (is.null(ext[[nm]])) "NULL" else sprintf("%d rows", nrow(ext[[nm]]))))
  invisible(NULL)
}

target_features <- function() {
  source_all_r()
  master <- ensure_cache()
  if (exists("ghs_export_feature_mart", mode = "function")) {
    ghs_export_feature_mart(master_enriched = master)
  } else {
    stop("[make] ghs_export_feature_mart not found")
  }
  invisible(NULL)
}

target_models <- function() {
  master <- ensure_cache()
  if (exists("ghs_export_all_models", mode = "function")) {
    ghs_export_all_models(master,
                          outputs_dir = file.path("分析输出", "模型表"))
  } else {
    cat("[make] ghs_export_all_models not found; skipped\n")
  }
  invisible(NULL)
}

target_figures <- function() {
  master <- ensure_cache()
  world_sf <- tryCatch(load_world_sf("medium", 0.08), error = function(e) NULL)
  if (exists("ghs_export_all_static", mode = "function")) {
    ghs_export_all_static(master, world_sf,
                            outputs_dir = file.path("分析输出", "图表"),
                            verbose = TRUE)
  } else {
    cat("[make] ghs_export_all_static not found; skipped\n")
  }
  if (exists("ghs_export_v2_thematic", mode = "function")) {
    ghs_export_v2_thematic(master,
                           out_dir = file.path("分析输出", "图表"),
                           verbose = TRUE)
  }
  if (exists("ghs_export_v2_dataviz", mode = "function")) {
    ghs_export_v2_dataviz(master, world_sf,
                          out_dir = file.path("分析输出", "图表"),
                          verbose = TRUE)
  }
  if (exists("ghs_export_extra", mode = "function")) {
    ghs_export_extra(master, world_sf,
                     outputs_dir = file.path("分析输出", "图表"),
                     verbose = TRUE)
  }
  if (exists("ghs_export_more", mode = "function")) {
    ghs_export_more(master,
                    outputs_dir = file.path("分析输出", "图表"),
                    verbose = TRUE)
  }
  # B1: 高级静态图集
  if (exists("ghs_export_advanced", mode = "function")) {
    cat("[make] ghs_export_advanced (B1)\n")
    .ghs_try(ghs_export_advanced(master, fig_dir = file.path("分析输出", "图表")), "B1")
  }
  # B2: 地图集
  if (exists("ghs_export_maps_advanced", mode = "function")) {
    cat("[make] ghs_export_maps_advanced (B2)\n")
    .ghs_try(ghs_export_maps_advanced(master, world_sf, fig_dir = file.path("分析输出", "图表")), "B2")
  }
  # B3: 产出图集
  if (exists("ghs_export_outcomes", mode = "function")) {
    cat("[make] ghs_export_outcomes (B3)\n")
    .ghs_try(ghs_export_outcomes(master, world_sf, fig_dir = file.path("分析输出", "图表")), "B3")
  }
  # B4: 不平等图集
  if (exists("ghs_export_equity", mode = "function")) {
    cat("[make] ghs_export_equity (B4)\n")
    .ghs_try(ghs_export_equity(master, fig_dir = file.path("分析输出", "图表")), "B4")
  }
  # B5: 国家专题图集
  if (exists("ghs_export_country", mode = "function")) {
    cat("[make] ghs_export_country (B5)\n")
    .ghs_try(ghs_export_country(master, fig_dir = file.path("分析输出", "图表")), "B5")
  }
  # B6: 冲击图集
  if (exists("ghs_export_shocks", mode = "function")) {
    cat("[make] ghs_export_shocks (B6)\n")
    .ghs_try(ghs_export_shocks(master, fig_dir = file.path("分析输出", "图表")), "B6")
  }
  invisible(NULL)
}

target_widgets <- function() {
  master <- ensure_cache()
  world_sf <- tryCatch(load_world_sf("medium", 0.08), error = function(e) NULL)
  if (exists("ghs_export_all_widgets", mode = "function")) {
    ghs_export_all_widgets(master, world_sf,
                             dir = file.path("分析输出", "交互组件"),
                             verbose = TRUE)
  } else {
    cat("[make] ghs_export_all_widgets not found; skipped\n")
  }
  if (exists("ghs_export_v2_widgets_plotly", mode = "function")) {
    ghs_export_v2_widgets_plotly(master,
                                 out_dir = file.path("分析输出", "交互组件"),
                                 verbose = TRUE)
  }
  if (exists("ghs_export_v2_widgets_other", mode = "function")) {
    ghs_export_v2_widgets_other(master, world_sf,
                                out_dir = file.path("分析输出", "交互组件"),
                                verbose = TRUE)
  }
  if (exists("ghs_export_widgets_more", mode = "function")) {
    ghs_export_widgets_more(master,
                            out_dir = file.path("分析输出", "交互组件"),
                            verbose = TRUE)
  }
  # C1: leaflet/plotly 地图 widget (~20)
  if (exists("ghs_export_widgets_map", mode = "function")) {
    cat("[make] ghs_export_widgets_map (C1)\n")
    .ghs_try(ghs_export_widgets_map(master, world_sf, out_dir = file.path("分析输出", "交互组件")), "C1")
  }
  # C2: 高级 widget (~80)
  if (exists("ghs_export_widgets_advanced", mode = "function")) {
    cat("[make] ghs_export_widgets_advanced (C2)\n")
    .ghs_try(ghs_export_widgets_advanced(master, out_dir = file.path("分析输出", "交互组件")), "C2")
  }
  invisible(NULL)
}

target_shinylive <- function() {
  dir.create("网站发布/仪表盘", recursive = TRUE, showWarnings = FALSE)
  # Snapshot master 到 仪表盘/数据快照/ 供 shinylive 加载
  master <- ensure_cache()
  dir.create("仪表盘/数据快照", recursive = TRUE, showWarnings = FALSE)
  saveRDS(master, "仪表盘/数据快照/snapshot.rds")
  if (dir.exists("仪表盘/程序库")) unlink("仪表盘/程序库", recursive = TRUE, force = TRUE)
  dir.create("仪表盘/程序库", recursive = TRUE, showWarnings = FALSE)
  file.copy(list.files("程序", pattern = "\\.R$", full.names = TRUE),
            "仪表盘/程序库", overwrite = TRUE)
  cat("[make] shinylive snapshot copied to 仪表盘/数据快照/\n")
  if (!requireNamespace("shinylive", quietly = TRUE)) {
    message("[make] shinylive package not available; writing fallback index.html")
    writeLines(c(
      "<!doctype html><html><head><meta charset=\"utf-8\">",
      "<title>Shinylive unavailable</title></head><body>",
      "<p>Shinylive package is not installed locally. Use shinyapps.io or run after installing shinylive.</p>",
      "</body></html>"), "网站发布/仪表盘/index.html")
    return(invisible(NULL))
  }
  tryCatch(
    shinylive::export("仪表盘", "网站发布/仪表盘", quiet = FALSE),
    error = function(e) {
      message("[make] shinylive export failed: ", conditionMessage(e))
      message("[make] writing fallback index.html")
      writeLines(c(
        "<!doctype html><html><head><meta charset=\"utf-8\">",
        "<title>Shinylive unavailable</title></head><body>",
        "<p>Shinylive build failed. Use shinyapps.io link.</p>",
        "</body></html>"), "网站发布/仪表盘/index.html")
    }
  )
  invisible(NULL)
}

target_copy_widgets <- function() {
  if (dir.exists("分析输出/交互组件")) {
    dst <- file.path("网站发布", "交互组件")
    if (dir.exists(dst)) unlink(dst, recursive = TRUE, force = TRUE)
    dir.create("网站发布", recursive = TRUE, showWarnings = FALSE)
    ok <- file.copy(file.path("分析输出", "交互组件"),
                    "网站发布", recursive = TRUE, overwrite = TRUE)
    files <- list.files(dst, full.names = TRUE, recursive = TRUE)
    if (isTRUE(ok)) {
      cat("[make] copied", length(files), "widget assets to 网站发布/交互组件/\n")
    }
  }
  invisible(NULL)
}

target_submission <- function() {
  source_all_r()
  ensure_cache()
  if (length(list.files(file.path("分析输出", "图表"), pattern = "[.]png$")) < 200) {
    target_figures()
  }
  if (length(list.files(file.path("分析输出", "交互组件"), pattern = "[.]html$")) < 100) {
    target_widgets()
  }
  target_copy_widgets()
  if (!exists("generate_static_showcase", mode = "function")) {
    stop("[make] generate_static_showcase not found")
  }
  generate_static_showcase(root = getwd())
  invisible(NULL)
}

target_deploy <- function() {
  # 单一标准：网站发布/index.html 由 21_static_showcase.R 生成的整合页
  # 不再渲染旧的 Quarto book 章节（已弃用）；shinylive + 整合首页 + widget assets 即可。
  target_shinylive()
  target_submission()
  cat("\n[make] deploy bundle ready at 网站发布/\n")
  cat("[make]  - index.html         (整合首页，等同课程提交版)\n")
  cat("[make]  - 交互组件/          (133 个 standalone widget)\n")
  cat("[make]  - 仪表盘/            (shinylive 浏览器版 Shiny)\n")
  invisible(NULL)
}

target_audit <- function() {
  src <- file.path("开发脚本", "审计项目.R")
  if (file.exists(src)) source(src, encoding = "UTF-8")
  else cat("[make] audit script missing at", src, "\n")
  invisible(NULL)
}

target_quality <- function() {
  src <- file.path("开发脚本", "质量门禁.R")
  if (file.exists(src)) source(src, encoding = "UTF-8")
  else stop("[make] quality gate script missing at ", src)
  invisible(NULL)
}

target_delivery <- function() {
  target_submission()
  target_quality()
  build_pkg <- isTRUE(as.logical(Sys.getenv("GHS_BUILD_DELIVERY", "FALSE")))
  if (!build_pkg) {
    cat("[make] delivery package generation is currently disabled.\n")
    cat("[make] submission + quality have been refreshed; 庄颂_20241334/ is NOT created.\n")
    cat("[make] to regenerate the package, run: $env:GHS_BUILD_DELIVERY='TRUE'; Rscript 构建.R delivery\n")
    return(invisible(NULL))
  }
  if (!exists("ghs_build_delivery", mode = "function")) {
    stop("[make] ghs_build_delivery not found")
  }
  ghs_build_delivery(root = getwd(), delivery_dir = file.path(getwd(), "庄颂_20241334"))
  invisible(NULL)
}

target_clean <- function() {
  ans <- readline("really delete 网站发布/ + 分析输出/ ? (yes/no) ")
  if (identical(ans, "yes")) {
    unlink("网站发布", recursive = TRUE, force = TRUE)
    unlink("分析输出/图表", recursive = TRUE, force = TRUE)
    unlink("分析输出/交互组件", recursive = TRUE, force = TRUE)
    unlink("分析输出/模型表",  recursive = TRUE, force = TRUE)
    cat("[make] cleaned.\n")
  } else {
    cat("[make] aborted.\n")
  }
  invisible(NULL)
}

target_all <- function() {
  target_data()
  target_features()
  target_models()
  target_figures()
  target_widgets()
  target_shinylive()
  target_submission()
  cat("\n[make] ALL done.\n")
}

# ---- dispatch -------------------------------------------------------------
switch(
  target,
  all       = target_all(),
  data      = target_data(),
  features  = target_features(),
  models    = target_models(),
  figures   = target_figures(),
  widgets   = target_widgets(),
  shinylive = target_shinylive(),
  submission = target_submission(),
  deploy    = target_deploy(),
  delivery  = target_delivery(),
  audit     = target_audit(),
  quality   = target_quality(),
  clean     = target_clean(),
  {
    cat("unknown target:", target, "\n\n")
    cat("usage: Rscript 构建.R [all|data|features|models|figures|widgets|shinylive|submission|deploy|delivery|audit|quality|clean]\n")
  }
)
