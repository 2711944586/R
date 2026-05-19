# Global Health Spending 2000–2023 · 项目说明

> 作者：庄颂（20241334）  
> 数据：TidyTuesday 2026-04-21 / WHO Global Health Expenditure Database + WDI + WHO GHO + IMF  
> 在线首页：<https://2711944586.github.io/R/>  
> Shiny 仪表盘：<https://constantine1433223.shinyapps.io/ghs-dashboard/>  
> GitHub 仓库：<https://github.com/2711944586/R>

本仓库对 WHO 全球卫生支出数据集（GHED）做端到端分析：从原始数据清洗、特征工程、统计建模，到 300 张静态图、133 个交互组件、36 个 Shiny 模块。覆盖 195 个国家 2000–2023 年的医疗资金来源、政府/私人/外援构成、人均水平、健康产出、不平等、效率、聚类、预测、情景模拟。

## 1. 评阅推荐顺序

1. 打开 `课程提交/庄颂_20241334.html`：课程要求的 HTML 结果文档，离线可读。整合首页含 Hero、24 章节、F1–F36 共 36 个核心发现、300 张静态图、133 个交互组件、Glossary、Reproducibility、Conclusion 等完整内容。
2. 打开 `课程提交/庄颂_20241334.Rmd`：课程要求的 R Markdown 源文档。
3. 访问 `https://2711944586.github.io/R/`：与课程 HTML 同源的 GitHub Pages 首页。
4. 访问 `https://constantine1433223.shinyapps.io/ghs-dashboard/`：36 模块 Shiny 仪表盘云端版。
5. 查阅 `项目文档/方法手册.md`：36 项核心发现的研究问题、数据口径、方法、代码入口、主要产物与局限。
6. 查阅 `分析输出/质量报告/quality_gate.html`：自动质量门禁报告（语法、图像、链接、widget、secret、Shiny bundle、README 一致性）。
7. 查阅 `派生数据/处理结果/feature_dictionary.csv`：变量字典。

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

## 3. 项目规模一览

| 维度 | 数量 |
|---|---:|
| R 程序模块 | 41 个（程序/） |
| Shiny 业务模块 | 36 个（仪表盘/模块/） |
| 静态图（PNG） | 300 张（400 DPI / 12 inch） |
| 静态图（SVG） | 300 张 |
| 交互组件（HTML） | 133 个 standalone widgets |
| 模型/指标表 | 81 个（CSV/RDS） |
| 核心发现（findings） | 36 个（F1–F36） |
| 静态页章节 | 24 章 |
| 测试用例 | 21 个 testthat 文件 |
| 总 R 文件数 | 122 个 |
| 图表类型 | 60+ 种（散点 / 折线 / 面积 / 条形 / 箱线 / 小提琴 / 蜂群 / 脊线 / 平行坐标 / 桑基 / 树图 / 网络 / 雷达 / 极坐标 / 日历热图 / 街区地图 / 双变量地图 / cartogram / Lorenz / 集中度曲线 / 斜率图 / 跳跃图 / 流图 / 瀑布图 / 哑铃图 / 漏斗 / 排行 / 瀑布 / 平行集等） |

## 4. 设计系统

### 4.1 静态页面前端

- **设计原则**：参考 NYT / Reuters Graphics / FT / The Pudding / Pew / OECD 视觉密度与排版规范
- **字体栈**：Source Serif 4（衬线标题）/ Inter（无衬线正文）/ JetBrains Mono（等宽代码）/ Noto Sans CJK SC（中文）
- **品牌色板**：5 套语义色（primary 深蓝 / secondary 赭橙 / good 青绿 / warn 暖黄 / bad 朱红 / neutral 中灰）
- **响应断点**：4 个 1280 / 1024 / 768 / 420
- **视觉密度**：96px 章节 padding / 18px 卡片间距 / 14px 圆角 / 4px 色条强调
- **微交互**：hover 上浮 2–4px / 阴影渐深 / 滚动揭示动画（`prefers-reduced-motion` 友好）/ 焦点态可见
- **可访问性**：WCAG 2.1 AA 对比度 / 字号 ≥10pt / 色盲友好色板 / aria-label / 键盘导航
- **印刷品质**：font-feature-settings 启用 ss01/cv11/kern/dlig；letter-spacing 精确到 -0.025em；hyphens auto

### 4.2 Shiny 仪表盘前端

- **顶栏**：bslib 5 + 深色玻璃态（backdrop-filter: blur）+ 36 模块 8 组 nav_menu
- **Hero 区**：每模块顶部 kicker + title + lead + meta strip
- **KPI 卡片**：4 列网格 + 左侧色条 + Source Serif 数值 + 14px 圆角
- **图表卡片**：白底 + card-note 引导句 + spinner 加载状态
- **Sidebar**：暖米白背景 + 260–280px 宽度 + helpText 术语注释
- **模块导航**：overview 页 4×3 卡片网格 + 一键跳转

### 4.3 颜色语义

| 槽位 | 浅色 | 深色 | 语义 |
|---|---|---|---|
| primary | `#1d3f5f` | `#7aa9d6` | 主标题、关键数值、政府支出 |
| secondary | `#c46327` | `#e8a070` | 强调、KPI 高亮、私人支出 |
| good | `#2a857a` | `#5dc4b6` | 改善 / 上升、外援支出 |
| warn | `#c89a3b` | `#e7c46a` | 警示、阈值 |
| bad | `#a23b3b` | `#e08585` | 风险 / 下降、自付支出 |
| neutral | `#5d667a` | `#a8b0c0` | 描述性、次级文本 |
| paper | `#fbf6ee` | `#0d121b` | 页面背景 |
| ink | `#0d121b` | `#f7eedf` | 主文字 |

## 5. 项目结构总览

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
├── .gitignore               git 忽略规则
├── .Rbuildignore            R CMD build 忽略规则
├── .dockerignore            Docker 镜像忽略规则
├── .lintr                   lintr 配置
│
├── 原始数据/                课程指定的 GHED 三张原始 CSV 与说明 markdown
├── 派生数据/                清洗 / 缓存 / 特征工程产物
│   ├── 处理结果/            主面板、特征表、变量字典
│   ├── 原始缓存/            WDI / 世界地图 / 收入组等外部抓取缓存
│   └── 外部数据/            GHO / IMF 等可重现外部数据
│
├── 程序/                    R 函数库（41 个模块，命名 NN_xxx.R）
│
├── 分析输出/                所有自动化产物
│   ├── 图表/                300 PNG + 300 SVG
│   ├── 交互组件/            133 个 standalone HTML widget
│   ├── 模型表/              81 个 CSV/RDS 模型与指标产物
│   └── 质量报告/            quality_gate.html / .json / .csv
│
├── 课程提交/                课程 Rmd 源 + HTML 结果
├── 网站发布/                GitHub Pages 静态首页（0.9 MB HTML + 外链图片）
│   ├── index.html           外链图片版（体积小，可部署）
│   ├── 图表/                300 张 PNG（publish 版图片资源）
│   ├── 交互组件/            发布版 standalone widgets
│   └── 仪表盘/              shinylive 浏览器版（自动生成）
│
├── 仪表盘/                  Shiny 应用源码与部署 bundle
│   ├── global.R / ui.R / server.R   入口
│   ├── 模块/                36 个业务模块 + 1 个 helpers
│   ├── 派生数据/处理结果/   shinyapps.io 部署 bundle 用的 sf 缓存
│   ├── 数据快照/            本地启动用的 master_enriched 快照
│   ├── 程序库/              shinyapps 部署用的 程序/ 副本
│   └── rsconnect/           本地 rsconnect 部署元数据
│
├── 项目文档/                方法手册、部署文档、变更记录、课程原题副本
├── 开发脚本/                审计、测试、质量、部署辅助脚本
├── 自动测试/                testthat 测试套件（21 个测试文件）
├── 部署配置/                Dockerfile 与 docker-compose
└── .github/workflows/       GitHub Actions（Pages 发布、R-CMD 测试）
```

## 6. `程序/` 函数库（41 个模块）

按编号严格分层，由低层到高层：

### 基础层（00–13）

| 模块 | 职责 |
|---|---|
| `00_utils.R` | 项目根定位、缓存、日志、格式化（fmt_usd/fmt_pct）、ggsave 包装、品牌色板（ghs_palette） |
| `01_io.R` | 读取三张原始 CSV |
| `02_clean.R` | 类型转换、缺失值处理、宽表整理 |
| `03_enrich.R` | 国家元数据、收入组、WDI、世界地图增强 |
| `04_metrics.R` | Gini / Theil / Atkinson、集中度、冲击指标 |
| `05_models.R` | PCA、聚类、变点、收敛、面板模型、ARIMA 预测 |
| `06_plot_theme.R` | ggplot 主题（theme_ghs）与品牌色、字体规范 |
| `07_plot_static.R` | 主体静态图函数 |
| `08_plot_interactive.R` | plotly / leaflet / reactable 交互图函数 |
| `09_maps.R` | 地图绘制（leaflet_choropleth） |
| `10_report_helpers.R` | 批量导出图表、widget、模型表的高阶包装 |
| `11_external_data.R` | 外部数据（GHO / IMF / IHME / OECD）抓取与缓存 |
| `12_advanced_models.R` | DEA 效率、灾难性医疗支出、情景模拟、合成控制 |
| `13_design_system.R` | 静态页设计系统（kpi_card_html / labs_news / palette_ghs / ghs_plotly_layout） |

### 表达层（14–26）

| 模块 | 职责 |
|---|---|
| `14_plots_thematic.R` | F1–F14 主题图 |
| `15_plots_dataviz.R` | 数据新闻类展示图 |
| `16_data_quality.R` | 数据质量诊断函数 |
| `17_widgets_plotly.R` | 30 个 plotly widgets |
| `18_widgets_other.R` | 40 个 leaflet/DT/reactable/networkD3 widgets |
| `19_narrative.R` | 叙事段落生成辅助 |
| `20_deploy.R` | widgets / figures 复制、sitemap、缓存预热 |
| `21_static_showcase.R` | **核心**：生成课程 HTML 与网站首页（同源生成器，含 3 层 CSS：基础 + v3 设计系统 + 编辑级精修） |
| `22_plots_extra.R` | 扩展静态图 |
| `23_feature_mart.R` | 统一 country-year 特征表与变量字典 |
| `24_plots_more.R` | 补充图集 |
| `25_widgets_more.R` | 补充交互组件 |
| `26_delivery.R` | 生成 `庄颂_20241334/` 交付目录 |

### 扩展层（30–44）

| 模块 | 职责 |
|---|---|
| `30_plots_advanced.R` | 高级图集：ridgeline、beeswarm、stream、bump、radial 等 30 种 |
| `31_maps_advanced.R` | 地图集：choropleth、bivariate、cartogram 等 27 种 |
| `32_plots_outcomes.R` | 健康产出专题图：DEA 前沿、弹性、SDG-3 等 25 种 |
| `33_plots_equity.R` | 公平专题图：Lorenz、Gini 分解、集中度等 25 种 |
| `34_plots_country.R` | 国家专题图：12 国 × 多视角共 30 种 |
| `35_plots_shocks.R` | 冲击专题图：GFC、COVID、通胀等 20 种 |
| `36_widgets_map.R` | 地图交互组件：6 大洲 × 5 指标 leaflet |
| `37_widgets_advanced.R` | 高级交互组件：echarts4r、highcharter、networkD3 |
| `38_models_panel.R` | 面板模型扩展：20 种面板回归变体 |
| `39_models_robust.R` | 稳健性模型：Bootstrap、分位数回归、GAM、RDD 等 20 种 |
| `40_plots_supplement.R` | 多样化补充图集：donut、polar、waterfall、Lorenz 等 46 种 |
| `42_findings_extra.R` | F15–F36 共 22 个核心发现的内容生成 |
| `43_findings_more.R` | 附加发现（备用） |
| `44_sections_extra.R` | F 阶段 6 个新章节 |

所有函数模块按需 `source()`；`构建.R::source_all_r()` 会一次性加载全部。

## 7. `仪表盘/模块/` Shiny 业务模块（36 个）

按主题分 8 组：

### 总览组（3）
- `mod_overview.R` — 全局 KPI、三源面积、世界地图、模块导航
- `mod_about.R` — 项目说明、数据字典、引用、复现命令
- `mod_methods.R` — 方法手册嵌入版

### 国家与区域（4）
- `mod_country.R` — 单国 24 年面板（CHE/OOPS/GGHED/寿命）
- `mod_regional.R` — 6 大洲 × 4 收入组对比与趋同
- `mod_ranking.R` — 多指标国家排名 + 排名变动追踪
- `mod_benchmark.R` — 基准对标与差距分析（OECD / 高收入 / 全球中位数）

### 筹资结构（5）
- `mod_financing.R` — HF1-HF4 筹资方案分布与趋势
- `mod_spending.R` — 人均 CHE 全球分布、排行、收入梯度
- `mod_purpose.R` — HC1-HC9 卫生支出功能分类
- `mod_aid.R` — 外援依赖度、可持续性、地理分布
- `mod_fiscal.R` — 政府卫生支出占 GDP、四象限分类

### 公平与效率（5）
- `mod_equity.R` — Gini / Theil / Atkinson 三指数演化
- `mod_inequality.R` — 不平等指数详细分解（组间/组内）
- `mod_efficiency.R` — DEA 前沿、CHE→寿命弹性
- `mod_convergence.R` — β-收敛与 σ-收敛分析
- `mod_decomposition.R` — Oaxaca-Blinder 增长分解

### 健康产出（4）
- `mod_outcomes.R` — U5MR / SDG-3 / 寿命与人均 CHE 联动
- `mod_sdg.R` — SDG-3 子指标进展
- `mod_prevention.R` — HC6 预防性支出与健康回报
- `mod_aging.R` — 老龄化与卫生支出关联

### 冲击与变化（5）
- `mod_pandemic.R` — 2019 vs 2020-2022 COVID 冲击
- `mod_growth.R` — 增长率分布、收入弹性、追赶
- `mod_transition.R` — 收入组晋升与筹资转型
- `mod_timeline.R` — 24 年时间线、关键事件、三期对比
- `mod_extremes.R` — IQR 异常检测、极端值识别

### 分析工具（6）
- `mod_compare.R` — 任选 N 国 × N 指标 × 年份滑块
- `mod_cluster.R` — PCA + KMeans 在线调 K
- `mod_forecast.R` — ARIMA / ETS 预测含 CI
- `mod_scenarios.R` — 三源滑块 → 5 年路径
- `mod_correlation.R` — 相关矩阵 + 散点矩阵 + 偏相关
- `mod_distribution.R` — 直方图 / 密度 / QQ / 偏度峰度

### 质量与稳健（4）
- `mod_robustness.R` — Bootstrap / 样本切片 / 异常值
- `mod_dataquality.R` — 缺失热图 / 修订记录浏览
- `mod_policy.R` — 政策建议生成器（按风险给推荐）
- `mod_atlas.R` — 全球 Atlas（choropleth + 双变量上色）

## 8. 静态 HTML 章节顺序（24 章）

```
01 Hero (动态 KPI strip + 渐变品牌字 + Reading Paths)
02 Reading Paths（评阅 / 政策 / 数据科学三条路径）
03 Methodology（数据 / 方法 / 复现 / 代码骨架 4 卡）
04 Codebook（变量字典 + 缺失率条）
05 Findings F1–F14（基础 14 项核心发现）
06 Findings F15–F36（扩展 22 项发现：老龄化 / 城镇化 / UHC / 灾难性支出 / NCD / 母婴 / 卫生人力 / 收入晋升 / 不平等分解 / OECD vs LMIC / 效率象限 / 援助效率 / 数据完整性 / 长尾国 / 综合指数等）
07 Regional Atlas（6 大洲专题）
08 Income Atlas（4 收入组专题）
09 Period Comparison（前危机 / 中间期 / 后疫情三期）
10 SDG-3 Dashboard
11 Outcomes Elasticity Panel
12 Equity Dashboard
13 Efficiency Frontier
14 Cluster Archetypes（PCA + KMeans 4 archetype）
15 Forecast & Scenarios（ARIMA + ETS 五年外推）
16 Country Profiles（12 国画像）
17 Robustness & Sensitivity
18 Limitations & Caveats
19 Policy Implications
20 Data Lineage
21 Figure Index（300 张图分类索引）
22 Widget Gallery（133 个交互组件）
23 Reproducibility（Docker / shinyapps / Pages 三套命令）
24 Conclusion + Glossary + Citations
```

## 9. 部署架构

### 9.1 GitHub Pages（静态首页）

- **触发**：push 到 `main` 分支
- **Workflow**：`.github/workflows/deploy.yml` → job `build` → job `deploy-pages`
- **构建步骤**：
  1. 安装 R 4.5 + 系统依赖（libcurl/sf/cairo/fonts-noto-cjk）
  2. 安装 R 包依赖（含 shinylive、leaflet、plotly 等）
  3. 预构建 master_enriched 缓存
  4. 编译 shinylive 浏览器版仪表盘
  5. 复制 widgets 到发布目录
  6. 生成静态 HTML 首页（`Rscript 构建.R submission`）
  7. 上传 Pages 制品
- **URL**：<https://2711944586.github.io/R/>

### 9.2 shinyapps.io（云端 Shiny）

- **方式 1（推荐）**：本地 `Rscript 开发脚本/部署Shiny云端.R ghs-dashboard`
- **方式 2（CI）**：在 GitHub 仓库 Settings → Secrets 添加：
  - `SHINYAPPS_NAME`：`constantine1433223`
  - `SHINYAPPS_TOKEN`：rsconnect token
  - `SHINYAPPS_SECRET`：rsconnect secret
- **URL**：<https://constantine1433223.shinyapps.io/ghs-dashboard/>

### 9.3 Docker（可选）

```bash
docker compose -f 部署配置/docker-compose.yml up -d
```

启动 dashboard（3838）+ site（8080）+ rstudio（8787）三服务。

## 10. 核心数据契约

| 字段 | 含义 | 来源 |
|---|---|---|
| `che_usd2023` | 2023 USD 不变价口径下的总卫生支出 | `health_spending.csv` 经 `02_clean.R` + `03_enrich.R` |
| `che_pc_usd2023` | 同上，人均口径 | + WDI 人口 |
| `gghed_che` | 政府强制筹资占 CHE 百分比 | `health_spending.csv` |
| `pvtd_che` | 国内私人筹资占 CHE 百分比 | 同上 |
| `ext_che` | 外部援助占 CHE 百分比 | 同上 |
| `hf3_che` | OOPS（居民自付）占 CHE 百分比；财务保护代理 | 同上 |
| `hc6_che` / `hc1_che` | 预防 / 治疗占 CHE 百分比 | `spending_purpose.csv` |
| `life_exp` / `u5mr` | 健康产出指标 | WDI |
| `iso3_code` | 国家三字母 ISO 代码（country key） | 全表 |
| `year` | 年份；2000–2023 | 全表 |

完整字段列表参见 `派生数据/处理结果/feature_dictionary.csv`。

## 11. 质量门禁

`Rscript 构建.R quality` 由 `开发脚本/质量门禁.R` 实现，输出写入 `分析输出/质量报告/`。

| 检查项 | 通过条件 |
|---|---|
| `parse_check` | 所有 R 文件 syntax OK |
| `png` | 所有 PNG 非空白 |
| `svg` | 所有 SVG 非占位符 |
| `figure_diversity` | 每个 finding ≥3 类图 |
| `links` | 所有内部链接有效 |
| `widgets` | 所有 widget HTML 加载无 JS error |
| `interactive` | 关键交互组件 inventory 齐备 |
| `gallery` | Figure index / widget gallery 描述完整 |
| `secrets` | 0 个 token / API key 命中 |
| `text_trace_scan` | 0 AI 痕迹关键词 |
| `sizes` | 单个 widget HTML ≤3 MB |
| `shiny` | Shiny bundle 完整 |
| `navigation` | 静态页 nav 锚一致性 |
| `science` | 科学结论可追溯（每条主张能定位到代码） |
| `deliverables` | 主要交付目录齐备 |
| `readme` | README 与项目结构一致 |

## 12. 关键约束

- `financing_schemes`、`health_spending`、`spending_purpose` 三个数据对象名称在所有代码中保留。
- Shiny 应用入口必须保留为 `仪表盘/global.R`、`ui.R`、`server.R`。
- `README.md`、`DESCRIPTION`、`LICENSE`、`_targets.R`、`.github/`、`.gitignore` 等英文命名保留不变。
- 所有派生缓存（`派生数据/处理结果/`、`派生数据/原始缓存/`、`仪表盘/数据快照/`、`仪表盘/程序库/`、`分析输出/质量报告/`）均可由 `Rscript 构建.R` 重新生成；它们都不在 git 索引内（`派生数据/外部数据/` 例外，因体积小且需离线复现）。
- 静态 HTML / Shiny 中均无版本号字符串、无 AI 痕迹关键词、无 v2/v3 等过渡命名。

## 13. 提交注意事项

- 不要把 `.Renviron`、`.env`、`rsconnect/`、shinyapps token 等任何凭据入仓。
- `课程提交/庄颂_20241334.Rmd` 依赖项目根的 `程序/`、`派生数据/`、`分析输出/`；不要单独移动 Rmd。
- `网站发布/index.html` 完整体验依赖同级 `网站发布/交互组件/` 与 `网站发布/仪表盘/`。
- 最终提交只包含 `庄颂_20241334/` 整个文件夹（由 `Rscript 构建.R delivery`，并设置 `GHS_BUILD_DELIVERY=TRUE` 后生成）。
- 单页 HTML 体积较大（~122 MB）是为了课程提交离线可读（base64 内嵌所有图片），**这是有意行为**。网站发布版仅 0.9 MB（图片通过相对路径引用 `图表/` 目录下的 300 张 PNG）。

## 14. 自动测试

```bash
Rscript 开发脚本/运行自动测试.R
```

测试覆盖：

| 测试 | 覆盖 |
|---|---|
| `test-utils.R` | `00_utils.R`：路径、缓存、日志 |
| `test-clean.R` | `02_clean.R`：清洗与宽表 |
| `test-metrics.R` | `04_metrics.R`：不平等指标 |
| `test-models.R` | `05_models.R`：PCA / 聚类 / 面板模型 |
| `test-plots.R` | `07_plot_static.R`：静态图返回 ggplot 对象 |
| `test-design.R` | `13_design_system.R`：设计系统 |
| `test-feature-mart.R` | `23_feature_mart.R`：特征表 |
| `test-plots-more.R` | `14/15/22/24_*.R`：拓展图 |
| `test-widgets.R` / `test-widgets-more.R` | `17/18/25_widgets_*.R`：交互组件 |
| `test-plots-advanced.R` | `30_plots_advanced.R` |
| `test-maps-advanced.R` | `31_maps_advanced.R` |
| `test-plots-outcomes.R` | `32_plots_outcomes.R` |
| `test-plots-equity.R` | `33_plots_equity.R` |
| `test-plots-country.R` | `34_plots_country.R` |
| `test-plots-shocks.R` | `35_plots_shocks.R` |
| `test-widgets-map.R` | `36_widgets_map.R` |
| `test-widgets-advanced.R` | `37_widgets_advanced.R` |
| `test-shiny-helpers.R` | `仪表盘/模块/_helpers.R` |
| `test-static-showcase.R` | `21_static_showcase.R` |
