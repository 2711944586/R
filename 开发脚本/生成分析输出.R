## 开发脚本/生成分析输出.R
## 一键产出 25+ 静态图、12 个交互 widget、所有模型表（写到 分析输出/）
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
  res$figures <- ghs_export_all_static(master, world_sf = world_sf_obj)
  cat(sprintf("[run_all_分析输出] figures saved: %d files\n",
               length(res$figures)))
}
if (mode %in% c("all", "widgets")) {
  cat("\n=== STAGE: widgets ===\n")
  res$widgets <- ghs_export_all_widgets(master, world_sf = world_sf_obj)
  cat(sprintf("[run_all_分析输出] widgets saved: %d files\n",
               length(res$widgets)))
}
if (mode %in% c("all", "models")) {
  cat("\n=== STAGE: models ===\n")
  res$models <- ghs_export_all_models(master)
  cat("[run_all_分析输出] model artifacts saved.\n")
}
cat("\n[run_all_分析输出] DONE.\n")
