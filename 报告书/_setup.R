# =============================================================================
# 报告书/_setup.R  ---  每章 chunk 起头都 source 这个文件
# v2: 加载完整 程序/* + v2 设计系统 + 多源外部数据 + master 缓存
# 使用 rprojroot 锚定 DESCRIPTION，避免 setwd() 触发 knitr in_dir 警告
# =============================================================================

# 1) 锁定项目根（DESCRIPTION 锚点，绕过 quarto _quarto.yml 误导）
.proj_root <- rprojroot::find_root(rprojroot::has_file("DESCRIPTION"))

# 2) Source 所有 R 模块（v1 00-10 + v2 11-20，全用绝对路径）
for (f in list.files(file.path(.proj_root, "程序"),
                      pattern = "\\.R$", full.names = TRUE)) {
  source(f, encoding = "UTF-8")
}

# 3) 必备包（注：library(sf) 必须在 dplyr 之后以注册 S3 方法供 left_join 用）
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(scales)
  library(stringr); library(forcats); library(tibble); library(purrr)
  if (requireNamespace("sf", quietly = TRUE)) library(sf)
})

# 4) 注册 v2 设计系统字体（一次性）+ 设为 ggplot 默认主题
tryCatch(
  ggplot2::theme_set(theme_ghs2(base_size = 13)),
  error = function(e) {
    warning("theme_ghs2() failed; fallback to theme_ghs() — ", conditionMessage(e))
    if (exists("theme_ghs", mode = "function")) ggplot2::theme_set(theme_ghs())
  }
)

# 5) 设为默认色板（替换全局 fill / colour 默认）
options(
  ggplot2.discrete.fill   = brand_palette$continent,
  ggplot2.discrete.colour = brand_palette$continent
)

# 6) 加载 master_enriched（缓存，绝对路径）
cache_path <- file.path(.proj_root, "派生数据", "处理结果", "master_enriched.rds")
if (file.exists(cache_path)) {
  master_enriched <- readRDS(cache_path)
} else {
  ghs <- load_ghs()
  master_enriched <- enrich_master(build_master_wide(ghs), with_wdi = TRUE)
  dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(master_enriched, cache_path)
}

# 7) 尝试加载 v2 外部数据（WDI / GHO / IMF / OECD / GBD）
external_data <- tryCatch(
  if (exists("load_external_all", mode = "function")) load_external_all() else NULL,
  error = function(e) {
    message("[setup] external_data not available: ", conditionMessage(e))
    NULL
  }
)

# 8) 尝试加载 world_sf（绝对路径）
world_sf <- tryCatch({
  cache_w <- file.path(.proj_root, "派生数据", "处理结果", "world_sf_medium.rds")
  if (file.exists(cache_w)) readRDS(cache_w)
  else if (exists("load_world_sf", mode = "function")) load_world_sf("medium", 0.08)
  else NULL
}, error = function(e) NULL)

# 9) 数据源 caption helper（数据新闻风一致 caption）
ghs_news_caption <- function() {
  paste0("\u6570\u636e\u6e90 \u00b7 WHO Global Health Expenditure Database (GHED) 2024",
         "  \u00b7  \u5206\u6790 Analysis: \u5e84\u9882 (20241334)")
}
