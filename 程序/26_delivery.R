ghs_copy_path <- function(from, to, overwrite = TRUE) {
  if (!file.exists(from) && !dir.exists(from)) return(FALSE)
  dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
  if (dir.exists(from)) {
    if (dir.exists(to)) unlink(to, recursive = TRUE, force = TRUE)
    dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
    ok <- file.copy(from, dirname(to), recursive = TRUE, overwrite = overwrite)
    copied_path <- file.path(dirname(to), basename(from))
    if (!identical(normalizePath(copied_path, winslash = "/", mustWork = FALSE), normalizePath(to, winslash = "/", mustWork = FALSE)) && dir.exists(copied_path)) {
      if (dir.exists(to)) unlink(to, recursive = TRUE, force = TRUE)
      file.rename(copied_path, to)
    }
    isTRUE(ok) || dir.exists(to)
  } else {
    file.copy(from, to, overwrite = overwrite)
  }
}

ghs_count_files <- function(path, pattern = NULL) {
  if (!dir.exists(path)) return(0L)
  length(list.files(path, pattern = pattern, recursive = TRUE, full.names = TRUE))
}

ghs_copy_matching <- function(from_dir, to_dir, pattern, recursive = FALSE) {
  if (!dir.exists(from_dir)) return(invisible(FALSE))
  files <- list.files(from_dir, pattern = pattern, recursive = recursive,
                      full.names = TRUE, all.files = FALSE)
  if (!length(files)) return(invisible(FALSE))
  rel <- ghs_rel(files, from_dir)
  dir.create(to_dir, recursive = TRUE, showWarnings = FALSE)
  for (i in seq_along(files)) {
    dst <- file.path(to_dir, rel[[i]])
    dir.create(dirname(dst), recursive = TRUE, showWarnings = FALSE)
    file.copy(files[[i]], dst, overwrite = TRUE, copy.date = TRUE)
  }
  invisible(TRUE)
}

ghs_dir_size_mb <- function(path) {
  if (!file.exists(path) && !dir.exists(path)) return(NA_real_)
  files <- if (dir.exists(path)) list.files(path, recursive = TRUE, full.names = TRUE) else path
  round(sum(file.info(files)$size, na.rm = TRUE) / 1024^2, 2)
}

ghs_norm <- function(path) {
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

ghs_rel <- function(paths, base) {
  paths <- ghs_norm(paths)
  base <- ghs_norm(base)
  sub(paste0("^", base, "/?"), "", paths, fixed = FALSE)
}

ghs_file_purpose <- function(rel) {
  purpose <- rep("项目辅助文件。", length(rel))
  purpose[rel == "README.md"] <- "最终交付包总说明；说明推荐评阅顺序、目录结构、核心产物、复现命令、质量门禁和注意事项。"
  purpose[rel == file.path("交付索引", "交付清单.csv")] <- "核心交付项核验表；用于快速检查关键文件或目录是否存在、文件数和体积是否符合预期。"
  purpose[rel == file.path("交付索引", "完整文件索引.csv")] <- "交付包内全部文件的机器可读索引；逐文件记录路径、目录、扩展名、体积和用途类别。"
  purpose[rel == "网站发布"] <- "静态发布页入口；提交包仅保留同源首页 HTML。"
  purpose[rel == "分析输出/模型表"] <- "模型、指标、预测、聚类、效率、公平性和情景模拟结果表目录；提交包仅保留 CSV。"
  purpose[rel == "分析输出/质量报告"] <- "质量门禁报告目录；包含 summary、HTML 报告和各模块明细表。"
  purpose[rel == "原始数据"] <- "课程原始数据目录；包含 GHED 三张 CSV 和数据说明文档。"
  purpose[rel == "程序"] <- "R 函数库目录；支撑数据处理、建模、绘图、widget、静态页、Shiny 和交付包生成。"
  purpose[rel == "仪表盘"] <- "Shiny 应用目录；包含 UI、server、模块、前端资源和运行快照。"
  purpose[rel == "项目文档"] <- "项目文档目录；包含方法手册、部署说明、变更记录和课程原始说明。"
  purpose[grepl("^课程提交/.*[.]Rmd$", rel)] <- "课程要求的 RMarkdown 源文档；可在交付包根目录结构下重新 knit 或调用统一生成器。"
  purpose[grepl("^课程提交/.*[.]html$", rel)] <- "课程要求的 HTML 结果文档；作为离线评阅主入口。"
  purpose[rel == "网站发布/index.html"] <- "GitHub Pages 同源首页；与课程提交 HTML 使用同一生成器生成。"
  purpose[grepl("^分析输出/模型表/.*[.]csv$", rel)] <- "模型、指标、预测、聚类、效率、公平性或情景模拟结果 CSV。"
  purpose[grepl("^分析输出/质量报告/", rel)] <- "质量门禁输出；记录语法、图像、链接、widget、文本、secret、Shiny 和 README 一致性检查。"
  purpose[grepl("^原始数据/", rel)] <- "课程原始数据及说明；用于追溯 GHED 三张原始表和数据来源。"
  purpose[grepl("^派生数据/处理结果/master_enriched[.]rds$", rel)] <- "清洗增强后的主面板缓存；支撑图表、模型、静态页和 Shiny。"
  purpose[grepl("^派生数据/处理结果/feature_mart_country_year", rel)] <- "统一 country-year 特征表；用于建模、复核和外部检查。"
  purpose[grepl("^派生数据/处理结果/feature_dictionary[.]csv$", rel)] <- "变量字典；记录变量标签、来源、单位、公式、角色、样本数和缺失率。"
  purpose[grepl("^程序/", rel)] <- "R 函数库；包含数据读取、清洗、特征、建模、绘图、widget、静态页和交付包生成逻辑。"
  purpose[grepl("^仪表盘/(global|server|ui)[.]R$", rel)] <- "本地 Shiny 应用入口文件。"
  purpose[grepl("^仪表盘/模块/", rel)] <- "Shiny 模块源码；对应 39 个页面与交互组件。"
  purpose[grepl("^仪表盘/数据快照/", rel)] <- "Shiny 运行所需数据快照；用于离线启动仪表盘。"
  purpose[grepl("^仪表盘/www/", rel)] <- "Shiny 前端 CSS 与浏览器脚本。"
  purpose[grepl("^项目文档/", rel)] <- "项目说明、方法手册、部署文档、变更记录和课程原始说明。"
  purpose[grepl("^项目入口/", rel)] <- "项目入口或配置文件；用于依赖安装、构建、启动、包元信息、许可证或开发环境复现。"
  purpose
}

ghs_delivery_file_index <- function(delivery_dir) {
  files <- list.files(delivery_dir, recursive = TRUE, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  files <- files[file.exists(files) & !dir.exists(files)]
  rel <- ghs_rel(files, delivery_dir)
  data.frame(
    path = rel,
    directory = dirname(rel),
    file = basename(rel),
    extension = tolower(tools::file_ext(rel)),
    size_mb = round(file.info(files)$size / 1024^2, 4),
    purpose = ghs_file_purpose(rel),
    stringsAsFactors = FALSE
  )
}

ghs_md_table <- function(df, cols = names(df), limit = Inf) {
  if (!nrow(df)) return("| 无 | 无 |\n|---|---|")
  df <- df[, cols, drop = FALSE]
  if (is.finite(limit) && nrow(df) > limit) df <- df[seq_len(limit), , drop = FALSE]
  header <- paste0("| ", paste(names(df), collapse = " | "), " |")
  sep <- paste0("|", paste(rep("---", ncol(df)), collapse = "|"), "|")
  body <- apply(df, 1, function(x) paste0("| ", paste(gsub("[|]", "\\\\|", as.character(x)), collapse = " | "), " |"))
  paste(c(header, sep, body), collapse = "\n")
}

ghs_delivery_manifest <- function(delivery_dir) {
  entries <- c(
    "README.md",
    file.path("交付索引", "交付清单.csv"),
    file.path("交付索引", "完整文件索引.csv"),
    file.path("课程提交", "庄颂_20241334.Rmd"),
    file.path("课程提交", "庄颂_20241334.html"),
    file.path("网站发布", "index.html"),
    file.path("分析输出", "模型表"),
    file.path("分析输出", "质量报告"),
    "原始数据",
    file.path("派生数据", "处理结果", "master_enriched.rds"),
    file.path("派生数据", "处理结果", "feature_mart_country_year.csv"),
    file.path("派生数据", "处理结果", "feature_mart_country_year.rds"),
    file.path("派生数据", "处理结果", "feature_dictionary.csv"),
    "程序",
    "仪表盘",
    "项目文档",
    file.path("项目入口", "构建.R"),
    file.path("项目入口", "安装依赖.R"),
    file.path("项目入口", "启动仪表盘.R"),
    file.path("项目入口", "DESCRIPTION"),
    file.path("项目入口", "LICENSE"),
    file.path("项目入口", "renv.lock")
  )
  rows <- lapply(entries, function(rel) {
    path <- file.path(delivery_dir, rel)
    data.frame(
      path = rel,
      exists = file.exists(path) || dir.exists(path),
      files = if (dir.exists(path)) length(list.files(path, recursive = TRUE, full.names = TRUE)) else as.integer(file.exists(path)),
      size_mb = ghs_dir_size_mb(path),
      purpose = ghs_file_purpose(rel),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

ghs_delivery_readme <- function(root, delivery_dir, manifest, file_index) {
  png_count <- ghs_count_files(file.path(root, "分析输出", "图表"), "[.]png$")
  svg_count <- ghs_count_files(file.path(root, "分析输出", "图表"), "[.]svg$")
  widget_count <- ghs_count_files(file.path(root, "分析输出", "交互组件"), "[.]html$")
  model_count <- ghs_count_files(file.path(delivery_dir, "分析输出", "模型表"), "[.]csv$")
  total_files <- nrow(file_index)
  total_size <- round(sum(file_index$size_mb, na.rm = TRUE), 2)
  top_dir <- ifelse(grepl("/", file_index$path, fixed = TRUE), sub("/.*$", "", file_index$path), ".")
  top_summary <- stats::aggregate(size_mb ~ top_dir, file_index, sum)
  names(top_summary) <- c("顶层路径", "体积MB")
  top_counts <- data.frame(顶层路径 = names(table(top_dir)), 文件数 = as.integer(table(top_dir)), stringsAsFactors = FALSE)
  top_summary <- merge(top_counts, top_summary, by = "顶层路径", all.x = TRUE)
  top_summary$体积MB <- round(top_summary$体积MB, 2)
  q_rows <- tryCatch(read.csv(file.path(root, "分析输出", "质量报告", "quality_gate_summary.csv"), fileEncoding = "UTF-8"), error = function(e) data.frame())
  q_text <- if (nrow(q_rows)) {
    paste(sprintf("| %s | %s | %s/%s |", q_rows$module, q_rows$status, q_rows$pass, q_rows$total), collapse = "\n")
  } else {
    "| 尚未生成 | 请运行 `Rscript 项目入口/构建.R quality` | - |"
  }
  manifest_text <- paste(sprintf("| `%s` | %s | %s | %s MB | %s |", manifest$path, ifelse(manifest$exists, "是", "否"), manifest$files, manifest$size_mb, manifest$purpose), collapse = "\n")
  file_index_text <- ghs_md_table(file_index, c("path", "size_mb", "purpose"))
  c(
    "Shiny 云端版：https://constantine1433223.shinyapps.io/ghs-dashboard/ ｜ 静态发布版：https://2711944586.github.io/R/",
    "",
    "# 庄颂_20241334 · Global Health Spending 交付说明",
    "",
    "> 作者：庄颂（20241334）  ",
    "> 数据：TidyTuesday 2026-04-21 / WHO Global Health Expenditure Database + WDI  ",
    "> Shiny 云端：<https://constantine1433223.shinyapps.io/ghs-dashboard/>  ",
    "> 静态发布：<https://2711944586.github.io/R/>",
    "",
    "## 1. 交付包定位",
    "",
    "本目录名为 `庄颂_20241334/`，与作业提交命名保持一致。最终提交时可直接压缩整个文件夹为 `庄颂_20241334.zip`。根目录只保留一个 `README.md` 文件；安装、启动、构建、依赖和索引类文件均放入专门文件夹，避免评阅入口被杂项文件淹没。",
    "",
    "最终页面遵循单一标准：`课程提交/庄颂_20241334.html` 与 `课程提交/庄颂_20241334.Rmd` 由同一个生成器 `程序/21_static_showcase.R` 生成。Rmd 会从课程 HTML 抽取样式、正文和脚本，保证内容与 HTML 一致。课程 HTML 保留完整正文、图表、右下角 dock、图片放大弹层和章节内交互入口；交互组件不再使用离线预览或内置假预览，而是指向 GitHub Pages 上的真实 standalone widget，并改为展开、点击或进入视口后按需加载，避免本地 HTML 一打开就同时请求大量 iframe。云端 Shiny 已随应用发布完整 standalone widget 缓存，各专题页同样按视口加载真实组件；交付包则保留课程主件、Shiny 源码、核心派生数据、CSV 模型结果、质量报告和复现脚本，不重复打包 `网站发布/`、`项目文档/`、完整图片目录、widget 缓存目录和重复代码副本。",
    "",
    sprintf("本交付包共包含 **%s 个文件**，总大小约 **%s MB**。当前版本保留完整课程 HTML/Rmd 与 Shiny 运行文件，同时删除重复的发布资源副本。", total_files, total_size),
    "",
    "## 2. 两个网页部署入口",
    "",
    "| 网页 | 地址 | 面向老师的用途 | 当前部署方式 |",
    "|---|---|---|---|",
    "| Shiny 云端仪表盘 | <https://constantine1433223.shinyapps.io/ghs-dashboard/> | **课堂汇报首选入口**。用于现场切换模块、演示地图工作台、国家比较、筹资结构、公平性、健康产出、预测情景、政策结论和完整 widget 墙。 | 由 `开发脚本/部署Shiny云端.R ghs-dashboard` 发布到 shinyapps.io；部署包内包含 `仪表盘/www/交互组件/` 的 standalone widget 缓存，组件按视口加载。 |",
    "| GitHub Pages 静态报告 | <https://2711944586.github.io/R/> | **在线报告阅读入口**。适合老师直接在浏览器中按章节阅读，查看 36 个内容章节、14 项核心发现、图表库、交互组件库和复现说明。 | 由 `.github/workflows/deploy.yml` 在 `main` 分支推送后发布 `网站发布/`；`网站发布/index.html` 与课程 HTML 同源生成。 |",
    "",
    "这两个网页承担不同角色：Shiny 云端版负责“可操作、可演示的仪表盘”，GitHub Pages 静态版负责“可阅读、可转发、可留档的网页报告”。课程提交包中的 `课程提交/庄颂_20241334.html` 也会按需加载 GitHub Pages 上的真实 standalone widget；如果老师打开本地 HTML 时没有联网，静态正文和已嵌入图表仍可阅读，但交互 iframe 需要联网访问线上组件。",
    "",
    "## 3. 核心规模",
    "",
    sprintf("- **内容结构**：36 个内容章节，包含数据说明、核心发现、图集、交互组件、复现说明和结论。"),
    sprintf("- **核心发现**：14 项，每项对应研究问题、方法、代码入口、图表、表格和解释。"),
    sprintf("- **静态图集**：完整项目当前生成 %s 张 PNG + %s 张 SVG；课程 HTML/Rmd 已嵌入主要展示图，提交包不再复制独立图片文件。", png_count, svg_count),
    sprintf("- **交互组件**：完整项目当前生成 %s 个 HTML widget；云端 Shiny 已随应用发布完整 widget 缓存，课程 HTML 使用线上绝对地址按需加载真实 widget；提交包不重复保存这批大体积缓存。", widget_count),
    sprintf("- **模型表**：`分析输出/模型表/` 中保留 %s 个 CSV 模型或指标产物，适合快速复核。", model_count),
    "- **Shiny**：39 个页面与交互模块；首页为纯标题封面，已移除额外装饰框和首页自动组件条；总览、地图、国家、筹资、公平、产出、预测、政策和组件库等板块均分布有真实交互组件。",
    "",
    "## 4. 推荐评阅顺序",
    "",
    "1. 打开 Shiny 云端仪表盘：<https://constantine1433223.shinyapps.io/ghs-dashboard/>，用于课堂汇报时演示可交互模块和完整 widget 缓存。",
    "2. 打开 GitHub Pages 静态报告：<https://2711944586.github.io/R/>，用于在线阅读完整报告、核对章节结构和交互组件库。",
    "3. 打开 `课程提交/庄颂_20241334.html`：这是课程要求的本地 HTML 结果文档；联网时会在展开或点击后加载线上真实 standalone widget，初始打开不会一次性拉取所有 iframe。",
    sprintf("4. 打开 `课程提交/庄颂_20241334.Rmd`：这是课程要求的 R Markdown 源文档，样式、正文和脚本与课程 HTML 同源同步。"),
    "5. 查看 `分析输出/质量报告/quality_gate.html`：确认语法、图像、链接、widget、secret、Shiny bundle、README 一致性等质量门禁。",
    "6. 查看 `派生数据/处理结果/feature_dictionary.csv`：复核变量来源、单位、公式、角色和缺失率。",
    "7. 如需逐文件核验，打开 `交付索引/完整文件索引.csv` 或查看本 README 的“完整文件索引”章节。",
    "",
    "## 5. 顶层目录总览",
    "",
    ghs_md_table(top_summary, c("顶层路径", "文件数", "体积MB")),
    "",
    "## 6. 所有顶层路径的作用",
    "",
    "### `课程提交/`",
    "",
    "- `课程提交/庄颂_20241334.Rmd`：课程源文档。它由课程 HTML 同步生成，保留同一套样式、正文、导航、交互入口和结论。",
    sprintf("- `课程提交/庄颂_20241334.html`：课程结果文档。该文件由统一生成器生成，适合老师直接打开；页面包含 36 个内容章节、14 项发现、图表库、交互入口、按需加载的线上真实组件 iframe 和结论。"),
    "",
    "### `分析输出/`",
    "",
    "- `分析输出/模型表/`：保留 CSV 格式的模型、指标、预测、聚类、PCA、效率、公平性和情景模拟结果。",
    "- `分析输出/质量报告/`：质量门禁产物，包括 `quality_gate_summary.csv`、`quality_gate.json`、`quality_gate.html` 和各模块明细表。",
    "",
    "### `原始数据/`",
    "",
    "- `financing_schemes.csv`、`health_spending.csv`、`spending_purpose.csv` 是课程指定的 GHED 原始表。",
    "- 同目录 `.md` 文件解释原始数据结构和字段含义，用于数据来源追溯。",
    "",
    "### `派生数据/处理结果/`",
    "",
    "- `master_enriched.rds`：清洗增强后的主面板缓存，是图表、模型、静态页和 Shiny 的共同数据底座。",
    "- `feature_mart_country_year.csv` / `.rds`：统一 country-year 特征表。",
    "- `feature_dictionary.csv`：变量字典，记录变量来源、单位、公式、角色、有效样本数和缺失率。",
    "- `world_sf_medium.rds`：地图绘制使用的空间数据缓存。",
    "",
    "### `程序/`",
    "",
    "- R 函数库。包含数据读取、清洗、特征工程、指标、模型、绘图主题、静态图导出、widget 导出、静态页生成、Shiny 辅助和交付包生成逻辑；大段解释型注释已清理。",
    "- 关键入口包括 `21_static_showcase.R`（生成课程 HTML 与网站首页）和 `26_delivery.R`（生成本交付包）。",
    "",
    "### `仪表盘/`",
    "",
    "- 本地 Shiny 应用目录。`global.R`、`ui.R`、`server.R` 是入口；`模块/` 保存模块源码；`www/` 保存前端样式与脚本；`数据快照/` 用于可移植启动。",
    "- `www/交互组件/` 是云端 Shiny 使用的 standalone widget 缓存，完整目录已部署到 shinyapps.io；为控制提交包体积，本交付包不复制该缓存，缺失时 Shiny 会回退到 GitHub Pages widget 地址。",
    "- `模块/mod_home.R` 是 Shiny 首页模块；当前首页只承担封面和导航入口，不再堆叠总览指标、路径卡片、结论摘要或自动组件条。",
    "- 已排除 `rsconnect/` 部署元数据，避免把账号部署状态或本机路径放入交付包。",
    "",
    "### `项目入口/` 与 `交付索引/`",
    "",
    "- `README.md`：本交付包说明，也是评阅者进入文件夹后的第一入口。",
    "- `项目入口/构建.R`：统一构建命令入口。",
    "- `项目入口/安装依赖.R`：安装依赖入口。",
    "- `项目入口/启动仪表盘.R`：本地启动 Shiny 仪表盘入口。",
    "- `项目入口/DESCRIPTION`：R 项目元信息和项目根定位锚点。",
    "- `项目入口/LICENSE`、`renv.lock`、`.Rprofile`、`.lintr`、`.gitignore`、`_targets.R`：许可证、依赖锁定、开发环境、风格检查、忽略规则和 targets 配置。",
    "- `交付索引/交付清单.csv`：核心路径核验表。",
    "- `交付索引/完整文件索引.csv`：逐文件索引，本 README 后文也完整列出。",
    "",
    "## 7. 复现命令",
    "",
    "在交付包根目录 `庄颂_20241334/` 内执行：",
    "",
    "```bash",
    "Rscript 项目入口/安装依赖.R",
    "Rscript 项目入口/构建.R data",
    "Rscript 项目入口/构建.R features",
    "Rscript 项目入口/构建.R figures",
    "Rscript 项目入口/构建.R widgets",
    "Rscript 项目入口/构建.R models",
    "Rscript 项目入口/构建.R submission",
    "Rscript 项目入口/构建.R quality",
    "Rscript 项目入口/启动仪表盘.R 4848",
    "```",
    "",
    "如果只想检查课程要求的两个主件，优先打开：",
    "",
    "- `课程提交/庄颂_20241334.Rmd`",
    "- `课程提交/庄颂_20241334.html`",
    "",
    "## 8. 质量门禁摘要",
    "",
    "| 模块 | 状态 | 通过/总数 |",
    "|---|---|---:|",
    q_text,
    "",
    "## 9. 核心交付清单",
    "",
    "| 路径 | 存在 | 文件数 | 体积 | 作用 |",
    "|---|---:|---:|---:|---|",
    manifest_text,
    "",
    "## 10. 注意事项",
    "",
    "- 不要把 shinyapps.io token、secret、`.Renviron`、`.env` 或 `rsconnect/` 提交到仓库。",
    "- `课程提交/庄颂_20241334.html` 使用线上绝对地址按需加载真实 standalone widget；单独转发 HTML 文件也可以查看完整组件，但需要联网访问 GitHub Pages 组件资源。",
    "- `课程提交/庄颂_20241334.Rmd` 依赖交付包内的 `程序/`，因此不要单独移动 Rmd；如需单独提交，仍建议同时提交整个 `庄颂_20241334/` 文件夹。",
    "- 单页 HTML 和同源 Rmd 体积较大是为了课程提交可直接阅读和复核；提交包已避免重复复制 widget、图片目录和重复代码副本。",
    "",
    "## 11. 已知限制",
    "",
    "- 提交包不包含完整 widget 缓存目录、发布图表目录、`网站发布/` 和 `项目文档/`；如需逐个打开 standalone widget 或完整图表库，请使用 shinyapps.io、完整仓库或 GitHub Pages 发布目录。",
    "- 当前质量门禁覆盖语法、图像、链接、widget、文本、secret、Shiny bundle 与 README 一致性，未包含跨断点截图测试。",
    "- `renv.lock` 放在 `项目入口/`；不同 R 与系统库版本下仍可能出现细小渲染差异。",
    "- 多数交互组件是 standalone widget，Shiny 模块之间未共享筛选状态。",
    "- 现有结论以描述性与预测性分析为主，不作为因果识别使用。",
    "",
    "## 12. 完整文件索引",
    "",
    "本节逐文件列出交付包内所有文件及其用途。若需要用表格软件筛选，可直接打开 `交付索引/完整文件索引.csv`。",
    "",
    file_index_text
  )
}

ghs_build_delivery <- function(root = getwd(), delivery_dir = file.path(root, "庄颂_20241334"), clean = TRUE) {
  old <- setwd(root)
  on.exit(setwd(old), add = TRUE)
  if (isTRUE(clean) && dir.exists(delivery_dir)) unlink(delivery_dir, recursive = TRUE, force = TRUE)
  dir.create(delivery_dir, recursive = TRUE, showWarnings = FALSE)
  root_files <- c("构建.R", "安装依赖.R", "启动仪表盘.R", "DESCRIPTION", "LICENSE", "renv.lock", ".Rprofile", ".lintr", ".gitignore", "_targets.R")
  entry_dir <- file.path(delivery_dir, "项目入口")
  index_dir <- file.path(delivery_dir, "交付索引")
  dir.create(entry_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(index_dir, recursive = TRUE, showWarnings = FALSE)
  for (f in root_files) ghs_copy_path(file.path(root, f), file.path(entry_dir, f))
  ghs_copy_path(file.path(root, "课程提交", "庄颂_20241334.html"), file.path(delivery_dir, "课程提交", "庄颂_20241334.html"))
  ghs_copy_path(file.path(root, "课程提交", "庄颂_20241334.Rmd"), file.path(delivery_dir, "课程提交", "庄颂_20241334.Rmd"))
  ghs_copy_matching(file.path(root, "分析输出", "模型表"), file.path(delivery_dir, "分析输出", "模型表"), "[.]csv$", recursive = TRUE)
  ghs_copy_path(file.path(root, "分析输出", "质量报告"), file.path(delivery_dir, "分析输出", "质量报告"))
  ghs_copy_path(file.path(root, "原始数据"), file.path(delivery_dir, "原始数据"))
  derived <- c("master_enriched.rds", "feature_mart_country_year.csv", "feature_mart_country_year.rds", "feature_dictionary.csv", "world_sf_medium.rds")
  for (f in derived) ghs_copy_path(file.path(root, "派生数据", "处理结果", f), file.path(delivery_dir, "派生数据", "处理结果", f))
  ghs_copy_path(file.path(root, "程序"), file.path(delivery_dir, "程序"))
  for (f in c("global.R", "server.R", "ui.R")) ghs_copy_path(file.path(root, "仪表盘", f), file.path(delivery_dir, "仪表盘", f))
  for (d in c("模块", "www", "数据快照", "派生数据")) ghs_copy_path(file.path(root, "仪表盘", d), file.path(delivery_dir, "仪表盘", d))
  unlink(file.path(delivery_dir, "原始数据", ".DS_Store"), force = TRUE)
  unlink(file.path(delivery_dir, "课程提交", ".Rhistory"), force = TRUE)
  unlink(file.path(delivery_dir, "网站发布"), recursive = TRUE, force = TRUE)
  unlink(file.path(delivery_dir, "项目文档"), recursive = TRUE, force = TRUE)
  unlink(file.path(delivery_dir, "仪表盘", "rsconnect"), recursive = TRUE, force = TRUE)
  unlink(file.path(delivery_dir, "仪表盘", "www", "交互组件"), recursive = TRUE, force = TRUE)
  unlink(file.path(delivery_dir, "仪表盘", "www", "widget_manifest.csv"), force = TRUE)
  image_files <- list.files(delivery_dir,
                            pattern = "[.](png|jpe?g|gif|svg|webp|bmp|tiff?)$",
                            recursive = TRUE, full.names = TRUE,
                            ignore.case = TRUE)
  if (length(image_files)) unlink(image_files, force = TRUE)
  writeLines("", file.path(delivery_dir, "README.md"), useBytes = TRUE)
  utils::write.csv(data.frame(), file.path(index_dir, "完整文件索引.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  utils::write.csv(data.frame(), file.path(index_dir, "交付清单.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  for (i in seq_len(2L)) {
    file_index <- ghs_delivery_file_index(delivery_dir)
    manifest <- ghs_delivery_manifest(delivery_dir)
    manifest <- manifest[!(manifest$path %in% c("网站发布/index.html", "项目文档")), , drop = FALSE]
    utils::write.csv(file_index, file.path(index_dir, "完整文件索引.csv"), row.names = FALSE, fileEncoding = "UTF-8")
    utils::write.csv(manifest, file.path(index_dir, "交付清单.csv"), row.names = FALSE, fileEncoding = "UTF-8")
    writeLines(ghs_delivery_readme(root, delivery_dir, manifest, file_index), file.path(delivery_dir, "README.md"), useBytes = TRUE)
  }
  file_index <- ghs_delivery_file_index(delivery_dir)
  manifest <- ghs_delivery_manifest(delivery_dir)
  manifest <- manifest[!(manifest$path %in% c("网站发布/index.html", "项目文档")), , drop = FALSE]
  utils::write.csv(file_index, file.path(index_dir, "完整文件索引.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  utils::write.csv(manifest, file.path(index_dir, "交付清单.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  writeLines(ghs_delivery_readme(root, delivery_dir, manifest, file_index), file.path(delivery_dir, "README.md"), useBytes = TRUE)
  cat("[delivery] final delivery package written to ", normalizePath(delivery_dir, winslash = "/", mustWork = FALSE), "\n", sep = "")
  invisible(manifest)
}
