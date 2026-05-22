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
  purpose[rel == "网站发布/图表"] <- "发布页使用的 PNG 图表目录；比完整图表目录更轻，保留网站首页本地预览能力。"
  purpose[rel == "网站发布/仪表盘"] <- "静态网站中的浏览器版 Shiny 或备用仪表盘入口目录。"
  purpose[rel == "分析输出/模型表"] <- "模型、指标、预测、聚类、效率、公平性和情景模拟结果表目录；轻量包仅保留 CSV。"
  purpose[rel == "分析输出/质量报告"] <- "质量门禁报告目录；包含 summary、HTML 报告和各模块明细表。"
  purpose[rel == "原始数据"] <- "课程原始数据目录；包含 GHED 三张 CSV 和数据说明文档。"
  purpose[rel == "程序"] <- "R 函数库目录；支撑数据处理、建模、绘图、widget、静态页、Shiny 和交付包生成。"
  purpose[rel == "仪表盘"] <- "Shiny 应用目录；包含 UI、server、模块、数据快照和程序库。"
  purpose[rel == "项目文档"] <- "项目文档目录；包含方法手册、部署说明、变更记录和课程原始说明。"
  purpose[grepl("^课程提交/.*[.]Rmd$", rel)] <- "课程要求的 RMarkdown 源文档；可在交付包根目录结构下重新 knit 或调用统一生成器。"
  purpose[grepl("^课程提交/.*[.]html$", rel)] <- "课程要求的 HTML 结果文档；作为离线评阅主入口。"
  purpose[rel == "网站发布/index.html"] <- "GitHub Pages 同源首页；与课程提交 HTML 使用同一生成器生成。"
  purpose[grepl("^网站发布/图表/.*[.]png$", rel)] <- "网站首页使用的 PNG 图表；用于轻量本地预览。"
  purpose[grepl("^网站发布/仪表盘/", rel)] <- "浏览器版 Shiny 或备用发布资源；用于检查静态网站中的仪表盘入口。"
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
  purpose[grepl("^仪表盘/程序库/", rel)] <- "Shiny 部署包中的 R 程序副本；用于云端或可移植部署。"
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
    file.path("网站发布", "图表"),
    file.path("网站发布", "仪表盘"),
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
    "最终页面遵循单一标准：`课程提交/庄颂_20241334.html` 与 `网站发布/index.html` 由同一个生成器 `程序/21_static_showcase.R` 生成，内容同源，只是服务场景不同。交付包保留课程主件、网站首页、Shiny 应用、核心派生数据、CSV 模型结果、质量报告、项目文档和复现脚本；大型图表目录与 widget 依赖不再重复打包，完整交互以首行两个线上地址为准。",
    "",
    sprintf("本交付包共包含 **%s 个文件**，总大小约 **%s MB**。本版为轻量提交包：保留评阅和复现最有用的文件，删除重复的发布资源副本。", total_files, total_size),
    "",
    "## 2. 核心规模",
    "",
    sprintf("- **内容结构**：36 个内容章节，包含数据说明、核心发现、图集、交互组件、复现说明和结论。"),
    sprintf("- **核心发现**：14 项，每项对应研究问题、方法、代码入口、图表、表格和解释。"),
    sprintf("- **静态图集**：完整项目当前生成 %s 张 PNG + %s 张 SVG；课程 HTML 已嵌入主要展示图，轻量包不再复制完整图表目录。", png_count, svg_count),
    sprintf("- **交互组件**：完整项目当前生成 %s 个 HTML widget；课程 HTML 与网站首页通过发布地址按需打开，不在提交包内重复保存 widget 依赖。", widget_count),
    sprintf("- **模型表**：`分析输出/模型表/` 中保留 %s 个 CSV 模型或指标产物，适合快速复核。", model_count),
    "- **Shiny**：39 个页面与交互模块；首页调整为纯标题封面，导航支持点击空白处关闭，交互组件页增加更多代表性快速入口。",
    "",
    "## 3. 推荐评阅顺序",
    "",
    "1. 打开 `课程提交/庄颂_20241334.html`：这是课程要求的 HTML 结果文档，也是最稳妥的离线评阅主入口。",
    sprintf("2. 打开 `课程提交/庄颂_20241334.Rmd`：这是课程要求的 R Markdown 源文档，已同步为当前 14 项核心发现、%s 张 PNG 图、%s 张 SVG 图和 %s 个交互组件口径。", png_count, svg_count, widget_count),
    "3. 打开 `网站发布/index.html`：这是 GitHub Pages 同源首页，适合检查发布路径和交互组件。",
    "4. 查看 `项目文档/方法手册.md`：逐项核查 F1–F14 的研究问题、数据口径、变量、方法、代码入口、主要产物和局限。",
    "5. 查看 `分析输出/质量报告/quality_gate.html`：确认语法、图像、链接、widget、secret、Shiny bundle、README 一致性等质量门禁。",
    "6. 查看 `派生数据/处理结果/feature_dictionary.csv`：复核变量来源、单位、公式、角色和缺失率。",
    "7. 如需逐文件核验，打开 `交付索引/完整文件索引.csv` 或查看本 README 的“完整文件索引”章节。",
    "",
    "## 4. 顶层目录总览",
    "",
    ghs_md_table(top_summary, c("顶层路径", "文件数", "体积MB")),
    "",
    "## 5. 所有顶层路径的作用",
    "",
    "### `课程提交/`",
    "",
    "- `课程提交/庄颂_20241334.Rmd`：课程源文档。它已同步到当前项目口径，包含提交说明、作业要求对照、当前规模、复现命令、质量摘要和 sessionInfo。",
    sprintf("- `课程提交/庄颂_20241334.html`：课程结果文档。该文件由统一生成器生成，适合老师直接打开；页面包含 36 个内容章节、14 项发现、图表库、交互入口和结论。"),
    "",
    "### `网站发布/`",
    "",
    "- `网站发布/index.html`：GitHub Pages 首页，与课程提交 HTML 同源。dock 定位和移动目录关闭逻辑已统一修复。",
    "- 完整发布版的 standalone widgets 位于线上 GitHub Pages 与仓库发布目录，轻量提交包不再复制这批大体积依赖。",
    "- `网站发布/仪表盘/`：浏览器版 Shiny 或备用页面入口，用于静态站中的仪表盘跳转。",
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
    "- R 函数库。包含数据读取、清洗、特征工程、指标、模型、绘图主题、静态图导出、widget 导出、静态页生成、Shiny 辅助和交付包生成逻辑。",
    "- 关键入口包括 `21_static_showcase.R`（生成课程 HTML 与网站首页）和 `26_delivery.R`（生成本交付包）。",
    "",
    "### `仪表盘/`",
    "",
    "- 本地 Shiny 应用目录。`global.R`、`ui.R`、`server.R` 是入口；`模块/` 保存模块源码；`数据快照/` 和 `程序库/` 用于可移植启动。",
    "- `模块/mod_home.R` 是 Shiny 首页模块；当前首页只承担封面和导航入口，不再堆叠总览指标、路径卡片或结论摘要。",
    "- 已排除 `rsconnect/` 部署元数据，避免把账号部署状态或本机路径放入交付包。",
    "",
    "### `项目文档/`",
    "",
    "- `方法手册.md`：14 项核心发现的方法矩阵和局限说明。",
    "- `部署总览.md`、`部署_GitHub_Pages.md`、`部署_Shiny云端.md`、`部署_浏览器仪表盘.md`：不同部署方式的说明。",
    "- `变更记录.md`：重要迭代和 bug 修复记录。",
    "- `2026作业_Global Health Spending 数据集自由分析.docx`：课程原始说明副本。",
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
    "## 6. 复现命令",
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
    "## 7. 质量门禁摘要",
    "",
    "| 模块 | 状态 | 通过/总数 |",
    "|---|---|---:|",
    q_text,
    "",
    "## 8. 核心交付清单",
    "",
    "| 路径 | 存在 | 文件数 | 体积 | 作用 |",
    "|---|---:|---:|---:|---|",
    manifest_text,
    "",
    "## 9. 注意事项",
    "",
    "- 不要把 shinyapps.io token、secret、`.Renviron`、`.env` 或 `rsconnect/` 提交到仓库。",
    "- `网站发布/index.html` 的完整交互体验以线上静态发布版为准；轻量提交包中的课程 HTML 会把 widget 打开到线上发布地址。",
    "- `课程提交/庄颂_20241334.Rmd` 依赖交付包内的 `程序/`，因此不要单独移动 Rmd；如需单独提交，仍建议同时提交整个 `庄颂_20241334/` 文件夹。",
    "- 单页 HTML 体积较大是为了课程提交可直接阅读；提交包已避免重复复制 widget 和完整图表资源。",
    "",
    "## 10. 已知限制",
    "",
    "- 轻量包不包含完整 widget 依赖目录；如需逐个离线打开 standalone widget，请使用完整仓库或 GitHub Pages 发布目录。",
    "- 当前质量门禁覆盖语法、图像、链接、widget、文本、secret、Shiny bundle 与 README 一致性，未包含跨断点截图测试。",
    "- `renv.lock` 放在 `项目入口/`；不同 R 与系统库版本下仍可能出现细小渲染差异。",
    "- 多数交互组件是 standalone widget，Shiny 模块之间未共享筛选状态。",
    "- 现有结论以描述性与预测性分析为主，不作为因果识别使用。",
    "",
    "## 11. 完整文件索引",
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
  ghs_copy_path(file.path(root, "网站发布", "index.html"), file.path(delivery_dir, "网站发布", "index.html"))
  ghs_copy_matching(file.path(root, "网站发布", "图表"), file.path(delivery_dir, "网站发布", "图表"), "[.]png$", recursive = FALSE)
  ghs_copy_path(file.path(root, "网站发布", "仪表盘"), file.path(delivery_dir, "网站发布", "仪表盘"))
  ghs_copy_matching(file.path(root, "分析输出", "模型表"), file.path(delivery_dir, "分析输出", "模型表"), "[.]csv$", recursive = TRUE)
  ghs_copy_path(file.path(root, "分析输出", "质量报告"), file.path(delivery_dir, "分析输出", "质量报告"))
  ghs_copy_path(file.path(root, "原始数据"), file.path(delivery_dir, "原始数据"))
  derived <- c("master_enriched.rds", "feature_mart_country_year.csv", "feature_mart_country_year.rds", "feature_dictionary.csv", "world_sf_medium.rds")
  for (f in derived) ghs_copy_path(file.path(root, "派生数据", "处理结果", f), file.path(delivery_dir, "派生数据", "处理结果", f))
  ghs_copy_path(file.path(root, "程序"), file.path(delivery_dir, "程序"))
  for (f in c("global.R", "server.R", "ui.R")) ghs_copy_path(file.path(root, "仪表盘", f), file.path(delivery_dir, "仪表盘", f))
  for (d in c("模块", "数据快照", "程序库", "派生数据")) ghs_copy_path(file.path(root, "仪表盘", d), file.path(delivery_dir, "仪表盘", d))
  doc_files <- c(
    "../README_项目总览.md",
    "方法手册.md",
    "部署总览.md",
    "部署_GitHub_Pages.md",
    "部署_Shiny云端.md",
    "部署_浏览器仪表盘.md",
    "变更记录.md",
    "2026作业_Global Health Spending 数据集自由分析.docx"
  )
  for (f in doc_files) {
    src <- if (startsWith(f, "../")) file.path(root, sub("^\\.\\./", "", f)) else file.path(root, "项目文档", f)
    dst <- file.path(delivery_dir, "项目文档", basename(f))
    ghs_copy_path(src, dst)
  }
  unlink(file.path(delivery_dir, "原始数据", ".DS_Store"), force = TRUE)
  unlink(file.path(delivery_dir, "课程提交", ".Rhistory"), force = TRUE)
  unlink(file.path(delivery_dir, "仪表盘", "rsconnect"), recursive = TRUE, force = TRUE)
  writeLines("", file.path(delivery_dir, "README.md"), useBytes = TRUE)
  utils::write.csv(data.frame(), file.path(index_dir, "完整文件索引.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  utils::write.csv(data.frame(), file.path(index_dir, "交付清单.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  for (i in seq_len(2L)) {
    file_index <- ghs_delivery_file_index(delivery_dir)
    manifest <- ghs_delivery_manifest(delivery_dir)
    utils::write.csv(file_index, file.path(index_dir, "完整文件索引.csv"), row.names = FALSE, fileEncoding = "UTF-8")
    utils::write.csv(manifest, file.path(index_dir, "交付清单.csv"), row.names = FALSE, fileEncoding = "UTF-8")
    writeLines(ghs_delivery_readme(root, delivery_dir, manifest, file_index), file.path(delivery_dir, "README.md"), useBytes = TRUE)
  }
  file_index <- ghs_delivery_file_index(delivery_dir)
  manifest <- ghs_delivery_manifest(delivery_dir)
  utils::write.csv(file_index, file.path(index_dir, "完整文件索引.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  utils::write.csv(manifest, file.path(index_dir, "交付清单.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  writeLines(ghs_delivery_readme(root, delivery_dir, manifest, file_index), file.path(delivery_dir, "README.md"), useBytes = TRUE)
  cat("[delivery] final delivery package written to ", normalizePath(delivery_dir, winslash = "/", mustWork = FALSE), "\n", sep = "")
  invisible(manifest)
}
