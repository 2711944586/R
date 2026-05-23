Shiny 云端版：https://constantine1433223.shinyapps.io/ghs-dashboard/ ｜ GitHub Pages 静态报告：https://2711944586.github.io/R/

# Global Health Spending 2000-2023

作者：庄颂（20241334）

本项目围绕 2000-2023 年全球卫生支出展开，使用 TidyTuesday 2026-04-21 数据集中整理的 WHO Global Health Expenditure Database，并结合 WDI 指标补充人口、GDP、预期寿命、儿童死亡率、收入组等变量。项目产出包括课程 HTML/Rmd、GitHub Pages 静态报告、shinyapps.io 云端仪表盘、本地 Shiny 应用、静态图集、交互组件库、模型表、变量字典、方法手册、变更记录、质量门禁报告和课程提交包。

## 两个网页部署入口

| 网页 | 地址 | 主要用途 | 当前状态 |
|---|---|---|---|
| Shiny 云端仪表盘 | <https://constantine1433223.shinyapps.io/ghs-dashboard/> | 课堂汇报和现场交互演示。可以切换专题模块，查看地图工作台、国家比较、筹资结构、公平性、健康产出、政策结论和完整 widget 缓存。 | 已部署到 shinyapps.io，部署脚本为 `开发脚本/部署Shiny云端.R ghs-dashboard`。 |
| GitHub Pages 静态报告 | <https://2711944586.github.io/R/> | 在线阅读完整网页报告。包含 36 个内容章节、14 项核心发现、图表库、交互组件库、复现说明和质量门禁说明。 | 已部署到 GitHub Pages，由 `.github/workflows/deploy.yml` 发布 `网站发布/`。 |

两者分工不同：Shiny 云端版负责“可操作、可演示”，GitHub Pages 版负责“可阅读、可转发、可留档”。课程提交 HTML 与 GitHub Pages 使用同一套生成逻辑；交互组件 iframe 指向线上真实 standalone widget，并采用按需加载，避免本地 HTML 打开时一次性加载大量远程页面。

## 本轮稳定性与页面升级

- 课程 HTML 不再使用离线预览或内置假预览；所有组件链接都指向 GitHub Pages 上部署的真实 standalone widget。
- 本地课程 HTML 的 iframe 改为按需加载：章节组件在展开后加载，组件库预览在点击卡片或进入视口后加载，显著降低开页卡顿。
- Shiny 各板块保留精选交互组件，但改为进入视口后加载，避免页面切换时同时请求大量 iframe。
- Shiny 首页已回到纯标题封面，只保留项目名称与年份范围；首页不再被自动插入组件条，也删除了两个额外装饰框。
- 静态报告的核心发现中补齐后段交互入口，F1-F14 均可在对应叙事附近打开真实组件，不只集中在最后的组件库。
- README 分为两类：仓库 README 介绍完整项目；提交包 README 面向老师，突出两个已部署网页和评阅顺序。

## 项目内容概览

| 模块 | 内容 |
|---|---|
| 数据底座 | GHED 三张原始表 + WDI 指标，清洗为 `master_enriched.rds` 与 country-year 特征表。 |
| 静态报告 | `课程提交/庄颂_20241334.html` 与 `课程提交/庄颂_20241334.Rmd`，同源生成。 |
| GitHub Pages | `网站发布/index.html`，与课程 HTML 同一生成器生成，在线部署为静态报告。 |
| Shiny 仪表盘 | `仪表盘/`，包含首页、总览、地图、国家、区域、筹资、公平、健康产出、预测、政策、组件库等模块。 |
| 图表库 | `分析输出/图表/` 中保存 PNG/SVG 静态图。 |
| 交互组件 | `分析输出/交互组件/` 中保存 standalone HTML widget，涵盖 Plotly、Leaflet、Reactable、DT、networkD3 等。 |
| 模型表 | `分析输出/模型表/` 中保存面板模型、预测、聚类、效率、公平性、情景模拟等结果 CSV。 |
| 质量报告 | `分析输出/质量报告/quality_gate.html` 汇总语法、图像、链接、widget、导航、Shiny、README、交付包等检查。 |

## 核心研究问题

项目围绕全球卫生支出回答以下问题：

1. 2000-2023 年全球卫生支出总量和资金来源结构如何变化？
2. 政府筹资、私人筹资、外援和自付之间如何共同影响财务保护？
3. 高收入国家与低收入国家的人均卫生支出差距是否收敛？
4. COVID-19、金融危机和通胀冲击是否改变了卫生支出轨迹？
5. 不同国家的筹资 archetype、效率前沿、健康产出和政策风险有何差异？
6. 哪些国家在 SDG-3 指标上进步最快，哪些国家仍面临 OOPS 或外援依赖风险？

## Shiny 仪表盘

Shiny 应用位于 `仪表盘/`，入口文件为 `global.R`、`ui.R`、`server.R`。模块源码在 `仪表盘/模块/`，前端样式与脚本在 `仪表盘/www/`。

常用页面：

| 页面 | 说明 |
|---|---|
| 首页 | 纯标题封面，不放 KPI、路径卡片或额外组件。 |
| 总览 Overview | 全局 KPI、资金来源面积图、OOPS 分布、国家跳转入口。 |
| Map Studio | 年份、指标、收入组和区域可切换的地图工作台。 |
| Country / Compare | 国家画像、国家比较和多国指标轨迹。 |
| Financing / Purpose / Aid | 筹资来源、支出用途和外援依赖专题。 |
| Equity / Inequality / Efficiency | 自付负担、不平等指数、效率前沿和财务保护。 |
| Outcomes / SDG / Prevention / Aging | 健康产出、SDG-3、预防支出和老龄化压力。 |
| Forecast / Scenarios | 预测、情景路径和不确定性展示。 |
| Widgets | 完整交互组件墙和 Shiny 原生组件中枢。 |
| Policy | 结论、政策建议和汇报收束。 |

云端 Shiny 已随应用部署 standalone widget 缓存；本地运行时如果缓存目录不存在，会回退到 GitHub Pages 上的组件地址。为了稳定，组件 iframe 采用懒加载，先显示占位提示，进入视口后再加载真实线上 HTML。

## 静态报告与课程提交

课程报告主文件：

- `课程提交/庄颂_20241334.html`：课程要求的 HTML 成品，正文和核心图片可直接阅读；联网时加载线上真实交互组件。
- `课程提交/庄颂_20241334.Rmd`：课程要求的 R Markdown 源文档，由同一生成器同步产出，便于核对文本、代码块和生成逻辑。
- `网站发布/index.html`：GitHub Pages 使用的静态报告首页，与课程 HTML 同源生成。

重要说明：课程 HTML 为了能单文件转发和离线阅读正文，会内嵌主要文本、样式和静态图，因此体积较大属于预期。交互组件不再内嵌为离线预览，而是按需加载线上真实 widget；没有网络时不会显示 iframe 内容，但正文和静态图仍可读。

## 本地运行

安装依赖：

```bash
Rscript 安装依赖.R
```

启动 Shiny：

```bash
Rscript 启动仪表盘.R 4848
```

常用构建：

```bash
Rscript 构建.R features
Rscript 构建.R figures
Rscript 构建.R widgets
Rscript 构建.R submission
Rscript 构建.R quality
```

生成完整提交包：

```powershell
$env:GHS_BUILD_DELIVERY='TRUE'
Rscript 构建.R delivery
```

部署 Shiny 云端版：

```bash
Rscript 开发脚本/部署Shiny云端.R ghs-dashboard
```

GitHub Pages 静态版由 `main` 分支推送后触发 `.github/workflows/deploy.yml` 发布。

## 目录结构

| 路径 | 说明 |
|---|---|
| `原始数据/` | GHED 原始 CSV 与数据说明。 |
| `派生数据/处理结果/` | 清洗增强后的主面板、特征表、变量字典和地图缓存。 |
| `程序/` | 数据处理、建模、绘图、widget、静态页、质量门禁和交付包生成源码。 |
| `仪表盘/` | Shiny 应用源码、模块、前端资源和运行快照。 |
| `课程提交/` | 课程 HTML 主报告与同源 Rmd。 |
| `网站发布/` | GitHub Pages 静态报告发布目录。 |
| `分析输出/图表/` | 静态 PNG/SVG 图表。 |
| `分析输出/交互组件/` | standalone HTML 交互组件。 |
| `分析输出/模型表/` | 模型、预测、聚类、公平性、效率和情景模拟 CSV。 |
| `分析输出/质量报告/` | 质量门禁结果。 |
| `项目文档/` | 方法手册、部署说明、变更记录和课程说明。 |
| `庄颂_20241334/` | 课程提交包目录。 |

## 推荐评阅顺序

1. 打开 Shiny 云端仪表盘：<https://constantine1433223.shinyapps.io/ghs-dashboard/>。
2. 打开 GitHub Pages 静态报告：<https://2711944586.github.io/R/>。
3. 打开提交包中的 `课程提交/庄颂_20241334.html`。
4. 查看 `课程提交/庄颂_20241334.Rmd` 核对源文档。
5. 查看 `项目文档/方法手册.md`、`项目文档/变更记录.md` 和 `分析输出/质量报告/quality_gate.html`。

## 质量门禁

质量门禁覆盖语法、PNG/SVG 图像、链接、widget、交互视图、图库、文本痕迹、Shiny bundle、静态导航、科学可追溯性、交付项和 README 一致性。正常情况下只保留 HTML 体积提醒，因为课程主 HTML 为可转发的完整报告文件，体积较大是预期行为。
