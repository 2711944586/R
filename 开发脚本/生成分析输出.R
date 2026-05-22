## 开发脚本/生成分析输出.R
## 生成静态图、交互 widget 与模型表，写入 分析输出/
## 用法（在仓库根目录）：
##   Rscript 开发脚本/生成分析输出.R [figures|widgets|models|all]
##

if (!dir.exists("程序") && basename(getwd()) == "开发脚本") setwd("..")

mode <- Sys.getenv("GHS_MODE",
                    unset = ifelse(length(commandArgs(trailingOnly = TRUE)),
                                    commandArgs(trailingOnly = TRUE)[1],
                                    "all"))
mode <- match.arg(tolower(mode), c("all", "figures", "widgets", "models"))

cat(sprintf("[run_all_分析输出] mode = %s, cwd = %s\n", mode, getwd()))

# ---- 加载函数库 ------------------------------------------------------------
for (f in list.files("程序", pattern = "\\.R$", full.names = TRUE)) {
  source(f, encoding = "UTF-8")
}
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2)
})

# ---- 主数据 ---------------------------------------------------------------
cache <- file.path("派生数据", "处理结果", "master_enriched.rds")
if (file.exists(cache)) {
  master <- readRDS(cache)
  cat(sprintf("[run_all_分析输出] cache hit  -> %s (rows=%d)\n",
              cache, nrow(master)))
} else {
  cat("[run_all_分析输出] no cache; rebuilding from raw ...\n")
  ghs <- load_ghs()
  master <- enrich_master(build_master_wide(ghs), with_wdi = TRUE)
  dir.create(dirname(cache), recursive = TRUE, showWarnings = FALSE)
  saveRDS(master, cache)
  cat(sprintf("[run_all_分析输出] cache built -> %s (rows=%d)\n",
              cache, nrow(master)))
}

# ---- 世界 sf ---------------------------------------------------------------
world_sf_obj <- NULL
sf_cache <- file.path("派生数据", "处理结果", "world_sf_medium.rds")
if (file.exists(sf_cache)) {
  world_sf_obj <- readRDS(sf_cache)
} else {
  world_sf_obj <- tryCatch(load_world_sf(scale = "medium",
                                          simplify_keep = 0.1),
                            error = function(e) NULL)
  if (!is.null(world_sf_obj)) saveRDS(world_sf_obj, sf_cache)
}
cat(sprintf("[run_all_分析输出] world_sf %s\n",
            if (is.null(world_sf_obj)) "MISSING" else "loaded"))

# ---- 分模式执行 ------------------------------------------------------------
res <- list()
if (mode %in% c("all", "figures")) {
  cat("\n=== STAGE: figures ===\n")
  res$figures <- list()
  if (exists("ghs_export_all_static", mode = "function")) {
    res$figures$base <- ghs_export_all_static(master, world_sf = world_sf_obj)
  }
  if (exists("ghs_export_v2_thematic", mode = "function")) {
    res$figures$thematic <- ghs_export_v2_thematic(master,
                                                   out_dir = file.path("分析输出", "图表"),
                                                   verbose = TRUE)
  }
  if (exists("ghs_export_v2_dataviz", mode = "function")) {
    res$figures$dataviz <- ghs_export_v2_dataviz(master, world_sf_obj,
                                                 out_dir = file.path("分析输出", "图表"),
                                                 verbose = TRUE)
  }
  if (exists("ghs_export_extra", mode = "function")) {
    res$figures$extra <- ghs_export_extra(master, world_sf_obj,
                                          outputs_dir = file.path("分析输出", "图表"),
                                          verbose = TRUE)
  }
  if (exists("ghs_export_more", mode = "function")) {
    res$figures$more <- ghs_export_more(master,
                                        outputs_dir = file.path("分析输出", "图表"),
                                        verbose = TRUE)
  }
  cat(sprintf("[run_all_分析输出] figures saved: %d files\n",
              length(list.files(file.path("分析输出", "图表"),
                                pattern = "[.](png|svg)$"))))
}
if (mode %in% c("all", "widgets")) {
  cat("\n=== STAGE: widgets ===\n")
  res$widgets <- list()
  if (exists("ghs_export_all_widgets", mode = "function")) {
    res$widgets$base <- ghs_export_all_widgets(master, world_sf = world_sf_obj)
  }
  if (exists("ghs_export_v2_widgets_plotly", mode = "function")) {
    res$widgets$plotly <- ghs_export_v2_widgets_plotly(master,
                                                       out_dir = file.path("分析输出", "交互组件"),
                                                       verbose = TRUE)
  }
  if (exists("ghs_export_v2_widgets_other", mode = "function")) {
    res$widgets$other <- ghs_export_v2_widgets_other(master, world_sf_obj,
                                                     out_dir = file.path("分析输出", "交互组件"),
                                                     verbose = TRUE)
  }
  if (exists("ghs_export_widgets_more", mode = "function")) {
    res$widgets$more <- ghs_export_widgets_more(master,
                                                out_dir = file.path("分析输出", "交互组件"),
                                                verbose = TRUE)
  }
  cat(sprintf("[run_all_分析输出] widgets saved: %d files\n",
              length(list.files(file.path("分析输出", "交互组件"),
                                pattern = "[.]html$"))))
}
if (mode %in% c("all", "models")) {
  cat("\n=== STAGE: models ===\n")
  res$models <- ghs_export_all_models(master)
  cat("[run_all_分析输出] model artifacts saved.\n")
}
cat("\n[run_all_分析输出] DONE.\n")
