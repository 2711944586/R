# Global Health Spending 2000–2023 · 项目说明

> 作者：庄颂（20241334）
> 数据：TidyTuesday 2026-04-21 / WHO Global Health Expenditure Database，补充 WDI、WHO GHO、IMF 等公开数据
> 在线首页：<https://2711944586.github.io/R/>
> 在线 Shiny：<https://constantine1433223.shinyapps.io/ghs-dashboard/>

本仓库对 WHO 全球卫生支出数据集（GHED）进行端到端分析：从原始数据清洗、特征工程、统计建模，到 300 张静态图、133 个交互组件、36 个 Shiny 模块的完整数据科学流水线。覆盖 195 个国家 2000–2023 年的医疗资金来源、政府/私人/外援构成、人均水平与健康产出。

## 1. 评阅推荐顺序

1. 打开 `课程提交/庄颂_20241334.html`：课程要求的 HTML 结果文档，离线可读。
2. 打开 `课程提交/庄颂_20241334.Rmd`：课程要求的 R Markdown 源文档。
3. 打开 `网站发布/index.html`：与课程 HTML 同源的 GitHub Pages 首页。
4. 查阅 `项目文档/方法手册.md`：36 项核心发现的研究问题、数据口径、方法、代码入口、主要产物与局限。
5. 查阅 `分析输出/质量报告/quality_gate.html`：自动质量门禁报告（语法、图像、链接、widget、secret、Shiny bundle、README 一致性）。
6. 查阅 `派生数据/处理结果/feature_dictionary.csv`：变量字典。

`课程提交/庄颂_20241334.html` 与 `网站发布/index.html` **由同一个生成器** `程序/21_static_showcase.R` 生成，内容一致；区别仅在用途不同——前者用于课程提交、后者用于网站发布。

## 2. 一键复现

在项目根目录依次运行：

```bash
Rscript 安装依赖.R         # 安装所有 R 包依赖
Rscript 构建.R data        # 生成主面板缓存 master_enriched.rds
Rscript 构建.R features    # 生成 country-year 特征表与变量字典
Rscript 构建.R figures     # 生成静态图（PNG/SVG）→ 分析输出/图表/
Rscript 构建.R widgets     # 生成 HTML 交互组件 → 分析输出/交互组件/
Rscript 构建.R models      # 生成模型与指标表 → 分析输出/模型表/
Rscript 构建.R submission  # 生成课程 HTML 与网站首页
Rscript 构建.R quality     # 生成质量门禁报告
Rscript 构建.R shinylive   # 生成 shinylive 浏览器版仪表盘
Rscript 构建.R deploy      # submission + shinylive + 复制 widgets
Rscript 构建.R delivery    # 刷新 submission + quality（默认不打 庄颂_20241334/ 包）
Rscript 启动仪表盘.R 4848  # 本地启动 Shiny（端口 4848，可改）
```

各目标说明：

| 目标 | 命令 | 主要输出 |
|---|---|---|
| 数据缓存 | `data` | `派生数据/处理结果/master_enriched.rds` |
| 特征表 | `features` | `派生数据/处理结果/feature_mart_country_year.csv/.rds`、`feature_dictionary.csv` |
| 静态图 | `figures` | `分析输出/图表/*.png` 和 `*.svg` |
| 交互组件 | `widgets` | `分析输出/交互组件/*.html` 与依赖 `_files/` |
| 模型表 | `models` | `分析输出/模型表/*.csv` 与 `*.rds` |
| 课程 HTML | `submission` | `课程提交/庄颂_20241334.html`、`网站发布/index.html` |
| 质量门禁 | `quality` | `分析输出/质量报告/` |
| 浏览器仪表盘 | `shinylive` | `网站发布/仪表盘/` |
| 部署整合 | `deploy` | `网站发布/` 全套资源 |
| 最终交付包 | `delivery` | 默认刷新 `submission + quality`；设置 `GHS_BUILD_DELIVERY=TRUE` 才生成 `庄颂_20241334/` |
| 项目审计 | `audit` | 控制台输出主要交付目录是否齐备 |
| 清理 | `clean` | 删除 `网站发布/` 与 `分析输出/`（交互式确认） |

要重新生成最终交付包：

```powershell
$env:GHS_BUILD_DELIVERY = 'TRUE'
Rscript 构建.R delivery
$env:GHS_BUILD_DELIVERY = $null
```

## 3. 项目结构总览

```
庄颂_20241334（项目根）
├── README.md                项目说明（本文件）
├── DESCRIPTION              R 项目元信息，定位项目根
├── LICENSE                  许可证
├── 构建.R                   统一构建入口
├── 安装依赖.R               依赖安装脚本
├── 启动仪表盘.R             本地启动 Shiny 入口
├── _targets.R               targets 流水线（可选）
├── .Rprofile                启动信息与 renv 激活
├── .gitignore               git 忽略规则（详注释）
├── .Rbuildignore            R CMD build 忽略规则
├── .dockerignore            Docker 镜像忽略规则
├── .lintr                   lintr 配置
│
├── 原始数据/                课程指定的 GHED 三张原始 CSV 与说明 markdown
├── 派生数据/                清洗 / 缓存 / 特征工程产物
│   ├── 处理结果/            主面板、特征表、变量字典等核心派生表
│   ├── 原始缓存/            WDI / 世界地图 / 收入组等外部抓取缓存
│   └── 外部数据/            GHO / IMF 等可重现外部数据（已入仓供离线使用）
│
├── 程序/                    R 函数库（40 个模块，命名为 NN_xxx.R）
│
├── 分析输出/                所有自动化产物
│   ├── 图表/                300 张 PNG + 300 张 SVG（400 DPI，60+ 种图表类型）
│   ├── 交互组件/            133 个 standalone HTML widget 与依赖
│   ├── 模型表/              81 个 CSV / RDS 模型与指标产物
│   └── 质量报告/            质量门禁报告（HTML / JSON / CSV）
│
├── 课程提交/                课程要求的 Rmd 源文件与 HTML 结果文档
├── 网站发布/                GitHub Pages 静态首页与发布资源
│   ├── 交互组件/            发布版 standalone widgets
│   └── 仪表盘/              shinylive 浏览器版仪表盘（自动生成）
│
├── 仪表盘/                  Shiny 应用源码与部署 bundle
│   ├── global.R / ui.R / server.R   入口
│   ├── 模块/                36 个业务模块 + 1 个 helpers（8 组分类导航）
│   ├── 派生数据/处理结果/   shinyapps.io 部署 bundle 用的 sf 缓存（合规副本）
│   ├── 数据快照/            本地启动用的 master_enriched 快照（自动生成）
│   ├── 程序库/              shinyapps 部署用的 程序/ 副本（自动生成）
│   └── rsconnect/           本地 rsconnect 部署元数据（gitignored）
│
├── 项目文档/                方法手册、部署文档、变更记录、课程原题副本
├── 开发脚本/                审计、测试、质量、部署辅助脚本
├── 自动测试/                testthat 测试套件
├── 部署配置/                Dockerfile 与 docker-compose
└── .github/workflows/       GitHub Actions（Pages 发布、R-CMD 测试）
```

## 4. 顶层路径详解

### 4.1 入口文件（项目根）

| 文件 | 作用 |
|---|---|
| `README.md` | 本文件；项目总入口与目录说明。 |
| `DESCRIPTION` | R 项目元信息（包名、版本、依赖、作者）；同时是 `proj_root()` 的定位锚点。 |
| `LICENSE` | 许可证（MIT）。 |
| `构建.R` | 唯一的构建入口；接受 `data / features / models / figures / widgets / shinylive / submission / deploy / delivery / quality / audit / clean` 等目标。 |
| `安装依赖.R` | 一次性安装所有依赖包，包含 CRAN 与可选 GitHub 包。 |
| `启动仪表盘.R` | 本地启动 Shiny 仪表盘；接受端口号参数（默认 4848）。 |
| `_targets.R` | targets 包流水线声明（可选；当前以 `构建.R` 为主入口）。 |
| `.Rprofile` | 启动横幅、UTF-8 设置、renv 激活；自动跳过非交互模式。 |
| `.gitignore` | git 忽略规则；按"会话状态 / 构建产物 / 缓存 / 部署 / Secret"分组。 |
| `.Rbuildignore` | `R CMD build` 忽略规则；防止把数据和发布产物打入 R 包。 |
| `.dockerignore` | Docker 镜像构建忽略规则；保持镜像精简。 |
| `.lintr` | lintr 配置；配合 `开发脚本/语法检查.R`。 |

### 4.2 `原始数据/` — 课程指定数据集

来自 TidyTuesday 2026-04-21 / WHO Global Health Expenditure Database，未做任何修改。

| 文件 | 内容 |
|---|---|
| `financing_schemes.csv` | 卫生筹资方案表（HF1–HF4 维度）。 |
| `health_spending.csv` | 卫生支出来源表（CHE / GGHED / PVTD / EXT 等）。 |
| `spending_purpose.csv` | 卫生支出用途表（HC1–HC9 维度）。 |
| `*.md` | 上述三表对应的字段说明 markdown。 |
| `intro.md`、`readme.md` | 课程数据集介绍。 |

### 4.3 `派生数据/` — 数据流水线产物

按数据生命周期分三层：

| 子目录 | 作用 | 主要文件 |
|---|---|---|
| `处理结果/` | **核心派生表**，所有图表、模型、Shiny、静态页的共同数据底座。 | `master_enriched.rds`、`feature_mart_country_year.csv/.rds`、`feature_dictionary.csv` |
| `原始缓存/` | **外部数据缓存**，用于离线复现 WDI、世界地图、收入组等。 | `wdi_panel.rds`、`wb_income_group.rds`、`world_sf_medium.rds` |
| `外部数据/` | **入仓的外部数据快照**（GHO、IMF），保证不联网也能跑。 | `gho.parquet/.rds`、`imf.parquet/.rds` |

`处理结果/` 与 `原始缓存/` 中的数据全部由 `Rscript 构建.R data` 与 `features` 自动生成；`外部数据/` 入仓以保证离线复现，按需通过 `程序/11_external_data.R` 刷新。

### 4.4 `程序/` — R 函数库（27 个模块）

按编号严格分层，由低层到高层：

| 模块 | 职责 |
|---|---|
| `00_utils.R` | 项目根定位、缓存、日志、格式化、ggsave 包装。 |
| `01_io.R` | 读取三张原始 CSV。 |
| `02_clean.R` | 类型转换、缺失值处理、宽表整理。 |
| `03_enrich.R` | 国家元数据、收入组、WDI、世界地图增强。 |
| `04_metrics.R` | Gini / Theil / Atkinson、集中度、冲击指标。 |
| `05_models.R` | PCA、聚类、变点、收敛、面板模型、ARIMA 预测。 |
| `06_plot_theme.R` | ggplot 主题与品牌色、字体规范。 |
| `07_plot_static.R` | 主体静态图函数。 |
| `08_plot_interactive.R` | plotly / leaflet / reactable 交互图函数。 |
| `09_maps.R` | 地图绘制辅助。 |
| `10_report_helpers.R` | 批量导出图表、widget、模型表的高阶包装。 |
| `11_external_data.R` | 外部数据（GHO / IMF / IHME / OECD）抓取与缓存。 |
| `12_advanced_models.R` | DEA 效率、灾难性医疗支出、情景模拟、合成控制等。 |
| `13_design_system.R` | 静态页设计系统（卡片、grid、KPI 等）。 |
| `14_plots_thematic.R` | 14 项核心发现专用主题图。 |
| `15_plots_dataviz.R` | 数据新闻类展示型图表。 |
| `16_data_quality.R` | 数据质量诊断函数。 |
| `17_widgets_plotly.R` | plotly widgets。 |
| `18_widgets_other.R` | leaflet、DT、reactable、networkD3 widgets。 |
| `19_narrative.R` | 叙事段落生成辅助。 |
| `20_deploy.R` | widgets / figures 复制、sitemap 生成、缓存预热、体积报告。 |
| `21_static_showcase.R` | 生成课程 HTML 与网站首页（同源生成器）。 |
| `22_plots_extra.R` | 扩展静态图。 |
| `23_feature_mart.R` | 统一 country-year 特征表与变量字典。 |
| `24_plots_more.R` | 补充图集。 |
| `25_widgets_more.R` | 补充交互组件。 |
| `26_delivery.R` | 生成交付目录，并自动写 README、清单与文件索引。 |
| `30_plots_advanced.R` | 高级图集：ridgeline、beeswarm、stream、bump、radial 等 30 种。 |
| `31_maps_advanced.R` | 地图集：choropleth、bivariate、cartogram 等 27 种。 |
| `32_plots_outcomes.R` | 健康产出专题图：DEA 前沿、弹性、SDG-3 等 25 种。 |
| `33_plots_equity.R` | 公平专题图：Lorenz、Gini 分解、集中度等 25 种。 |
| `34_plots_country.R` | 国家专题图：12 国 × 多视角 共 30 种。 |
| `35_plots_shocks.R` | 冲击专题图：GFC、COVID、通胀等 20 种。 |
| `36_widgets_map.R` | 地图交互组件：6 大洲 × 5 指标 leaflet。 |
| `37_widgets_advanced.R` | 高级交互组件：echarts4r、highcharter、networkD3 等。 |
| `38_models_panel.R` | 面板模型扩展：20 种面板回归变体。 |
| `39_models_robust.R` | 稳健性模型：Bootstrap、分位数回归、GAM、RDD 等 20 种。 |
| `40_plots_supplement.R` | 多样化补充图集：donut、polar、waterfall、Lorenz 等 46 种。 |
| `26_delivery.R` | 生成 `庄颂_20241334/` 交付目录，并自动写 README、清单与文件索引。 |

所有函数模块按需 `source()`；`构建.R::source_all_r()` 会一次性加载全部。

### 4.5 `分析输出/` — 自动化产物

| 子目录 | 内容 | 生成命令 |
|---|---|---|
| `图表/` | 300 张 PNG + 300 张 SVG（400 DPI，12 inch，60+ 种图表类型）。 | `Rscript 构建.R figures` |
| `交互组件/` | 133 个 standalone HTML widget；每个 HTML 通常对应一个 `*_files/` 依赖。 | `Rscript 构建.R widgets` |
| `模型表/` | 81 个 CSV / RDS：面板 FE、随机效应、Mundlak、分位数回归、GAM、Bootstrap、Jackknife、DID、RDD、交叉验证等。 | `Rscript 构建.R models` |
| `质量报告/` | 质量门禁产物：`quality_gate.html`、`quality_gate.json`、`quality_gate_summary.csv`，以及各模块明细表。 | `Rscript 构建.R quality` |

### 4.6 `课程提交/`

| 文件 | 作用 |
|---|---|
| `庄颂_20241334.Rmd` | 课程要求的 R Markdown 源文档；包含提交说明、作业要求对照、当前规模、复现命令、质量摘要、sessionInfo。 |
| `庄颂_20241334.html` | 课程要求的 HTML 结果文档；适合老师离线打开。 |

### 4.7 `网站发布/`

GitHub Pages 同源发布目录。

| 路径 | 作用 |
|---|---|
| `index.html` | 网站首页；与课程 HTML 同源。 |
| `交互组件/` | 发布版 standalone widgets；每个 `.html` 与同名 `_files/` 必须整体保留。 |
| `仪表盘/` | shinylive 浏览器版仪表盘入口（由 `Rscript 构建.R shinylive` 生成）。 |

### 4.8 `仪表盘/`

Shiny 应用，分本地与云端两套部署。

| 路径 | 作用 |
|---|---|
| `global.R` | 加载依赖与函数库、磁盘缓存、共享对象、主题。 |
| `ui.R` | 顶部导航与页面布局。 |
| `server.R` | 模块服务端调度。 |
| `模块/_helpers.R` | 共享 UI 帮助函数（KPI 卡片、loading spinner 等）。 |
| `模块/mod_*.R` | 36 个业务模块，分 8 组：总览 / 国家与区域 / 筹资结构 / 公平与效率 / 健康产出 / 冲击与变化 / 分析工具 / 质量与稳健。 |
| `派生数据/处理结果/world_sf_medium.rds` | shinyapps.io 部署 bundle 必需的 sf 地图缓存。`rsconnect::deployApp` 会把整个 `仪表盘/` 上传，云端冷启动若没有它就要重新拉 rnaturalearth 包。 |
| `数据快照/snapshot.rds` | 本地与云端启动时优先读的 master_enriched 快照（`Rscript 构建.R shinylive` 自动生成；gitignored）。 |
| `程序库/` | shinyapps 部署用的 `程序/` 副本（每次部署前自动 rsync；gitignored）。 |
| `rsconnect/` | rsconnect 本地部署元数据（含账号信息；gitignored，不入仓）。 |

### 4.9 `项目文档/`

| 文件 | 内容 |
|---|---|
| `方法手册.md` | 14 项核心发现的方法矩阵、口径、代码入口与局限。 |
| `部署总览.md` | 本地、Docker、GitHub Actions、云端部署总览。 |
| `部署_GitHub_Pages.md` | Pages 发布详细步骤、字体与故障排查。 |
| `部署_Shiny云端.md` | shinyapps.io 凭据、部署、回滚步骤。 |
| `部署_浏览器仪表盘.md` | shinylive 浏览器版部署说明。 |
| `变更记录.md` | 主要迭代与 bug 修复记录。 |
| `2026作业_Global Health Spending 数据集自由分析.docx` | 课程原始题目副本。 |

### 4.10 `开发脚本/`

| 脚本 | 作用 |
|---|---|
| `审计项目.R` | 检查项目主要交付目录是否齐备。 |
| `语法检查.R` | 对 `程序/` 与 `仪表盘/` 跑 lintr。 |
| `运行自动测试.R` | 调用 testthat 跑 `自动测试/` 全部用例。 |
| `测试仪表盘模块.R` | 单独跑 Shiny 模块单元测试。 |
| `生成分析输出.R` | 顺次生成图表、widget、模型表的便捷脚本。 |
| `构建拓展产物.R` | 生成拓展静态图与交互组件。 |
| `质量门禁.R` | 质量门禁主入口（被 `构建.R quality` 调用）。 |
| `部署Shiny云端.R` | 本地准备并 `rsconnect::deployApp` 推到 shinyapps.io。 |
| `启动仪表盘开发版.R` | 本地以开发模式启动 Shiny。 |

### 4.11 `自动测试/`

testthat 测试套件，对应 `程序/` 中的关键函数：

| 测试 | 覆盖 |
|---|---|
| `test-utils.R` | `00_utils.R`：路径、缓存、日志。 |
| `test-clean.R` | `02_clean.R`：清洗与宽表。 |
| `test-metrics.R` | `04_metrics.R`：不平等指标。 |
| `test-models.R` | `05_models.R`：PCA / 聚类 / 面板模型。 |
| `test-plots.R` | `07_plot_static.R`：静态图返回 ggplot 对象。 |
| `test-design.R` | `13_design_system.R`：设计系统。 |
| `test-feature-mart.R` | `23_feature_mart.R`：特征表。 |
| `test-plots-ext.R` / `test-plots-more.R` | `14/15/22/24_*.R`：拓展图。 |
| `test-widgets-ext.R` / `test-widgets-more.R` | `17/18/25_widgets_*.R`：交互组件。 |

运行：

```bash
Rscript 开发脚本/运行自动测试.R
```

### 4.12 `部署配置/`

| 文件 | 作用 |
|---|---|
| `Dockerfile` | 基于 rocker 的 R 4.5 镜像，预装项目依赖。 |
| `docker-compose.yml` | `dashboard`（Shiny 3838）、`site`（静态网站 8080）、可选 `rstudio`（8787）三服务。 |

启动：

```bash
docker compose -f 部署配置/docker-compose.yml up -d
```

### 4.13 `.github/workflows/`

| Workflow | 触发 | 作用 |
|---|---|---|
| `deploy.yml` | push to main 或 manual | 构建 `网站发布/`、shinylive、widgets 并发布到 GitHub Pages。 |
| `R-CMD-tests.yml` | push / PR | 跑 lintr + testthat。 |

## 5. 核心数据契约

| 数据对象 | 含义 | 主要来源 |
|---|---|---|
| `che_usd2023` | 2023 USD 不变价口径下的总卫生支出。 | `health_spending.csv` 经 `02_clean.R` 与 `03_enrich.R`。 |
| `che_pc_usd2023` | 同上，人均口径。 | 同上 + WDI 人口。 |
| `gghed_che` | 政府强制筹资占 CHE 百分比。 | `health_spending.csv`。 |
| `pvtd_che` | 国内私人筹资占 CHE 百分比。 | 同上。 |
| `ext_che` | 外部援助占 CHE 百分比。 | 同上。 |
| `hf3_che` | OOPS（居民自付）占 CHE 百分比；用作财务保护代理。 | 同上。 |
| `hc6_che`、`hc1_che` | 预防 / 治疗占 CHE 百分比。 | `spending_purpose.csv`。 |
| `life_exp`、`u5mr` | 健康产出指标。 | WDI 缓存（`派生数据/原始缓存/wdi_panel.rds`）。 |
| `iso3_code` | 国家三字母 ISO 代码；所有表的 country key。 | 全表。 |
| `year` | 年份；2000–2023。 | 全表。 |

完整字段列表参见 `派生数据/处理结果/feature_dictionary.csv`。

## 6. 质量门禁

`Rscript 构建.R quality` 由 `开发脚本/质量门禁.R` 实现，输出写入 `分析输出/质量报告/`。检查项：

- R 脚本语法解析（`parse_check.csv`）
- PNG / SVG 文件有效性、空白图扫描（`blank_figures.csv`、`svg_placeholder_scan.csv`）
- 图表多样性与图集说明（`figure_diversity.csv`、`gallery_notes.csv`）
- 本地链接、widget 资源、widget 报错扫描（`link_check.csv`、`widget_error_scan.csv`、`interactive_view_inventory.csv`）
- Shiny bundle 完整性（`shiny_bundle.csv`）
- secret / token 扫描（`secret_scan.csv`）
- 文案 AI 痕迹扫描（`text_trace_scan.csv`）
- 静态页面导航与 README 一致性（`static_navigation.csv`、`readme_consistency.csv`、`deliverable_manifest.csv`）
- 产物体积报告（`artifact_size.csv`）
- 科学结论可追溯性（`science_traceability.csv`）

`quality_gate.html` 是浏览器友好版总报告，`quality_gate.json` 是机器可读版。

## 7. 部署

### GitHub Pages

仓库 Settings → Pages → Source 选 **GitHub Actions**；推送到 `main` 后由 `.github/workflows/deploy.yml` 自动构建并发布。详细步骤见 `项目文档/部署_GitHub_Pages.md`。

### shinyapps.io

本地：

```bash
Rscript 开发脚本/部署Shiny云端.R ghs-dashboard
```

GitHub Actions Secrets 需要：

| Secret | 说明 |
|---|---|
| `SHINYAPPS_NAME` | shinyapps.io 账号名 |
| `SHINYAPPS_TOKEN` | rsconnect token |
| `SHINYAPPS_SECRET` | rsconnect secret |

详细步骤见 `项目文档/部署_Shiny云端.md`。

### Docker

```bash
docker build -f 部署配置/Dockerfile -t ghs-dashboard .
docker run -p 3838:3838 ghs-dashboard
```

或 docker compose（自动启动 dashboard + site + rstudio 三服务）：

```bash
docker compose -f 部署配置/docker-compose.yml up -d
```

## 8. 提交注意事项

- 不要把 `.Renviron`、`.env`、`rsconnect/`、shinyapps token 等任何凭据入仓。
- `课程提交/庄颂_20241334.Rmd` 依赖项目根的 `程序/`、`派生数据/`、`分析输出/`；不要单独移动 Rmd。
- `网站发布/index.html` 完整体验依赖同级 `网站发布/交互组件/` 与 `网站发布/仪表盘/`。
- 最终提交只包含 `庄颂_20241334/` 整个文件夹（由 `Rscript 构建.R delivery`，并设置 `GHS_BUILD_DELIVERY=TRUE` 后生成）。
- 单页 HTML 体积较大是为了课程提交离线可读，**这是有意行为**。

## 9. 关键约束

- `financing_schemes`、`health_spending`、`spending_purpose` 三个数据对象名称在所有代码中保留。
- Shiny 应用入口必须保留为 `仪表盘/global.R`、`ui.R`、`server.R`。
- `README.md`、`DESCRIPTION`、`LICENSE`、`_targets.R`、`.github/`、`.gitignore` 等英文命名保留不变。
- 所有派生缓存（`派生数据/处理结果/`、`派生数据/原始缓存/`、`仪表盘/数据快照/`、`仪表盘/程序库/`、`分析输出/质量报告/`）均可由 `Rscript 构建.R` 重新生成；它们都不在 git 索引内（`派生数据/外部数据/` 例外，因为体积小且需要离线复现）。
