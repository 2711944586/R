#!/usr/bin/env Rscript
# =============================================================================
# 安装依赖.R  ·  安装本项目所需的 R 包
# -----------------------------------------------------------------------------
# 用法:
#   Rscript 安装依赖.R           # 命令行
#   source("安装依赖.R")         # 在 R 会话里
# 策略:
#   1. 先装 renv, 如果 renv.lock 存在则 renv::restore()
#   2. 否则对照内置清单逐包检测 + 安装
#   3. 分 "必需 / 推荐 / 可选" 三档, 任一档失败不影响其他档
# =============================================================================

options(Ncpus = max(1L, parallel::detectCores() - 1L), timeout = 600)
repos <- c(CRAN = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/")
options(repos = repos)

cat("\n[setup] R version: ", R.version.string, "\n", sep = "")

# ---- 0. renv ---------------------------------------------------------------
if (!requireNamespace("renv", quietly = TRUE)) {
  cat("[setup] Installing renv ...\n")
  install.packages("renv")
}

if (file.exists("renv.lock")) {
  cat("[setup] Found renv.lock; restoring project library ...\n")
  try(renv::restore(prompt = FALSE), silent = FALSE)
  cat("[setup] renv::restore() done.\n")
  return(invisible(NULL))
}

# ---- 1. 分档依赖清单 --------------------------------------------------------
pkgs_core <- c(
  # 数据与核心
  "tidyverse", "data.table", "janitor", "here", "glue", "rlang",
  "base64enc",
  "arrow", "parquetize", "fs",
  # 国家映射 / 外部数据
  "countrycode", "wbstats",
  # 可视化核心
  "ggplot2", "scales", "ggtext", "ggrepel", "patchwork",
  # 报告
  "rmarkdown", "knitr",
  # 探索
  "skimr", "DataExplorer", "naniar"
)

pkgs_viz <- c(
  # 高级 ggplot 扩展
  "ggridges", "ggstream", "ggbump", "ggdist", "treemapify", "ggtern",
  "ggalluvial", "waffle",
  "gganimate", "gifski", "av", "showtext", "systemfonts", "colorBlindness",
  # 交互可视化
  "plotly", "leaflet", "leaflet.extras", "highcharter", "echarts4r",
  "DT", "reactable", "reactablefmtr", "networkD3", "ggiraph",
  # 地图
  "sf", "rnaturalearth", "rnaturalearthdata", "tmap", "rmapshaper"
)

pkgs_model <- c(
  "broom", "infer", "modelsummary", "fixest",
  "ineq", "changepoint", "strucchange",
  "cluster", "factoextra", "FactoMineR",
  "forecast", "fable", "fabletools", "tsibble", "feasts",
  "prophet"
)

pkgs_app <- c(
  "shiny", "shinyWidgets", "shinyjs", "bslib", "bs4Dash",
  "waiter", "thematic", "golem", "shiny.i18n",
  "shinytest2", "shinycssloaders"
)

pkgs_dev <- c(
  "targets", "tarchetypes", "testthat", "devtools", "usethis",
  "lintr", "styler", "pkgload", "profvis"
)

install_if_missing <- function(pkgs, label) {
  cat("\n[setup] ---- ", label, " (", length(pkgs), " pkgs) ----\n", sep = "")
  to_install <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(to_install) == 0L) {
    cat("[setup] All present.\n")
    return(invisible(character()))
  }
  cat("[setup] Installing: ", paste(to_install, collapse = ", "), "\n", sep = "")
  tryCatch(
    install.packages(to_install, dependencies = TRUE),
    error = function(e) {
      message("[setup] Error while installing ", label, ": ", conditionMessage(e))
      FALSE
    }
  )
  still_missing <- to_install[!vapply(to_install, requireNamespace, logical(1), quietly = TRUE)]
  if (length(still_missing)) {
    message("[setup] Still missing after attempt: ", paste(still_missing, collapse = ", "))
  }
  invisible(still_missing)
}

miss_core  <- install_if_missing(pkgs_core,  "Core")
miss_viz   <- install_if_missing(pkgs_viz,   "Visualisation")
miss_model <- install_if_missing(pkgs_model, "Modeling")
miss_app   <- install_if_missing(pkgs_app,   "Shiny App")
miss_dev   <- install_if_missing(pkgs_dev,   "Dev Tools")

all_miss <- c(miss_core, miss_viz, miss_model, miss_app, miss_dev)
if (length(all_miss)) {
  cat("\n[setup] \u26a0 Some packages failed to install: \n  ",
      paste(all_miss, collapse = ", "), "\n", sep = "")
  cat("[setup] 可稍后手动: install.packages(c(\"",
      paste(all_miss, collapse = "\", \""), "\"))\n", sep = "")
} else {
  cat("\n[setup] \u2713 All packages ready.\n")
}

cat("\n[setup] Done. Next: \n",
    "  1) source('程序/00_utils.R'); source('程序/01_io.R')\n",
    "  2) targets::tar_make()\n",
    "  3) rmarkdown::render('课程提交/\u5e84\u9882_20241334.Rmd')\n",
    sep = "")
