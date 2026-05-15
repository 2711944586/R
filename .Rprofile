# .Rprofile — 本项目的 R 启动脚本
# 1. 设置 CRAN 镜像（国内清华镜像，加速安装）
# 2. 激活 renv（若存在）
# 3. 友好欢迎信息

local({
  # -- CRAN 镜像 -----------------------------------------------------------
  repos <- getOption("repos")
  if (is.null(repos) || identical(repos["CRAN"], c(CRAN = "@CRAN@"))) {
    repos["CRAN"] <- "https://mirrors.tuna.tsinghua.edu.cn/CRAN/"
    options(repos = repos)
  }
  options(
    BioC_mirror = "https://mirrors.tuna.tsinghua.edu.cn/bioconductor",
    download.file.method = "libcurl",
    download.file.extra = "--no-check-certificate",
    Ncpus = max(1L, parallel::detectCores() - 1L),
    timeout = 600
  )

  # -- 中文编码 / 字体 -----------------------------------------------------
  # R 4.2+ 使用 UCRT, 默认 UTF-8 即可; 若强制 936 会导致 saveRDS 写中文路径失败
  tryCatch(Sys.setlocale("LC_CTYPE", "en_US.UTF-8"), warning = function(w) NULL)
  options(encoding = "UTF-8")

  # -- 激活 renv ----------------------------------------------------------
  activate <- file.path("renv", "activate.R")
  if (file.exists(activate)) {
    try(source(activate), silent = TRUE)
  }

  # -- 欢迎信息 -----------------------------------------------------------
  if (interactive()) {
    msg <- paste(
      "================================================",
      "  Global Health Spending Analysis",
      "  Author: 庄颂 (20241334)",
      "  Repo:   https://github.com/2711944586/R",
      "------------------------------------------------",
      "  - 首次运行: source('安装依赖.R') 安装依赖",
      "  - 运行流水线: targets::tar_make()",
      "  - 渲染作业: rmarkdown::render('课程提交/庄颂_20241334.Rmd')",
      "  - 启动 Shiny: shiny::runApp('仪表盘')",
      "================================================",
      sep = "\n"
    )
    packageStartupMessage(msg)
  }
})
