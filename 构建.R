# =============================================================================
# 构建.R  ---  v2 统一本地构建脚本
# -----------------------------------------------------------------------------
# 用法：
#   Rscript 构建.R all       # 全量：data + models + figures + widgets + book + shiny
#   Rscript 构建.R data      # 仅生成 master_enriched + external 缓存
#   Rscript 构建.R models    # 仅跑 6 类统计模型 → 分析输出/模型表/
#   Rscript 构建.R figures   # 仅生成 60+ 静态图 → 分析输出/图表/
#   Rscript 构建.R widgets   # 仅生成 30+ 交互 widget → 分析输出/交互组件/
#   Rscript 构建.R book      # 仅渲染 Quarto Book → 网站发布/
#   Rscript 构建.R shinylive # 仅编译 shinylive → 网站发布/仪表盘/
#   Rscript 构建.R submission# 生成课程提交与可转发静态 HTML
#   Rscript 构建.R deploy    # book + shinylive + copy widgets → 网站发布/（部署用）
#   Rscript 构建.R clean     # 删除 网站发布/ 与 分析输出/（慎用）
#   Rscript 构建.R audit     # 对照 plan v2 检查产物完备性
# =============================================================================

args <- commandArgs(trailingOnly = TRUE)
target <- if (length(args)) args[[1]] else "audit"

cat("[make] target =", target, "\n")
cat("[make] cwd    =", getwd(), "\n")

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
  invisible(NULL)
}

target_book <- function() {
  ensure_cache()
  ok <- system2("quarto", c("render", "报告书"))
  if (ok != 0) stop("[make] quarto render 报告书 failed; check quarto install")
  cat("[make] book rendered to 网站发布/\n")
  invisible(NULL)
}

target_shinylive <- function() {
  if (!requireNamespace("shinylive", quietly = TRUE)) {
    message("[make] installing shinylive ...")
    install.packages("shinylive")
  }
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
  if (length(list.files(file.path("分析输出", "图表"), pattern = "[.]png$")) < 30) {
    target_figures()
  }
  if (length(list.files(file.path("分析输出", "交互组件"), pattern = "[.]html$")) < 20) {
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
  cat("[make]  - 交互组件/          (24 个 standalone widget)\n")
  cat("[make]  - 仪表盘/            (shinylive 浏览器版 Shiny)\n")
  invisible(NULL)
}

target_audit <- function() {
  src <- file.path("开发脚本", "审计项目.R")
  if (file.exists(src)) source(src, encoding = "UTF-8")
  else cat("[make] audit script missing at", src, "\n")
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
  target_models()
  target_figures()
  target_widgets()
  target_book()
  target_copy_widgets()
  target_shinylive()
  cat("\n[make] ALL done.\n")
}

# ---- dispatch -------------------------------------------------------------
switch(
  target,
  all       = target_all(),
  data      = target_data(),
  models    = target_models(),
  figures   = target_figures(),
  widgets   = target_widgets(),
  book      = target_book(),
  shinylive = target_shinylive(),
  submission = target_submission(),
  deploy    = target_deploy(),
  audit     = target_audit(),
  clean     = target_clean(),
  {
    cat("unknown target:", target, "\n\n")
    cat("usage: Rscript 构建.R [all|data|models|figures|widgets|book|shinylive|submission|deploy|audit|clean]\n")
  }
)
