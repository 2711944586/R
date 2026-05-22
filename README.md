Shiny 云端版：https://constantine1433223.shinyapps.io/ghs-dashboard/ ｜ 静态发布版：https://2711944586.github.io/R/

# Global Health Spending 2000-2023

作者：庄颂（20241334）

本项目围绕 2000-2023 年全球卫生支出展开，使用 TidyTuesday 2026-04-21 数据集中整理的 WHO Global Health Expenditure Database，并结合 WDI 指标补充健康产出、收入组和人口尺度。当前版本包含课程 HTML/Rmd、GitHub Pages 静态页、shinyapps.io 云端仪表盘、本地 Shiny 应用、图表、交互组件、模型表、数据字典、方法手册、变更记录和质量门禁报告。

本轮调整把 Shiny 首页从“总览型信息页”改为“简洁封面页”：首页只保留项目名称、年份范围、作者信息、进入仪表盘、地图工作台和静态报告三个入口，以及一组克制的动态图形。分析内容、KPI、结论和路径卡片均放回对应模块中，避免首页承担过多叙事功能。

## 在线入口

| 入口 | 地址 |
|---|---|
| Shiny 云端版 | <https://constantine1433223.shinyapps.io/ghs-dashboard/> |
| GitHub Pages 静态版 | <https://2711944586.github.io/R/> |
| GitHub 仓库 | <https://github.com/2711944586/R> |

## 推荐查看顺序

1. `课程提交/庄颂_20241334.html`：课程主报告，适合离线评阅。
2. `课程提交/庄颂_20241334.Rmd`：课程源文档，便于核对文本、代码块和生成逻辑。
3. `网站发布/index.html`：静态发布首页，与课程 HTML 同源生成。
4. `仪表盘/` 或云端 Shiny：先进入封面页，再跳转到总览、地图工作台、国家比较、筹资结构、健康产出、交互组件和结论页。
5. `项目文档/方法手册.md`：逐项说明 F1-F14 的研究问题、数据口径、方法、代码入口、主要产物和局限。
6. `项目文档/变更记录.md`：记录本轮前端、导航、Shiny 首页、交互组件、README 和交付包结构调整。
7. `分析输出/质量报告/quality_gate.html`：查看语法、图像、链接、widget、导航、Shiny bundle、交付项和 README 一致性检查。

## Shiny 仪表盘说明

Shiny 应用位于 `仪表盘/`，入口文件为 `global.R`、`ui.R` 和 `server.R`。模块按主题拆分在 `仪表盘/模块/`，前端样式与少量交互脚本放在 `仪表盘/www/`。

当前首页文件是 `仪表盘/模块/mod_home.R`，对应样式在 `仪表盘/www/ghs-shiny.css` 的 `home-cover` 系列规则中。首页定位为封面，不再展示纵览数据、KPI 和长篇结论；这些内容保留在总览、地图、国家、筹资、公平、产出和政策模块里。

常用入口：

| 页面 | 说明 |
|---|---|
| `首页` | 封面与入口页 |
| `总览` | 全局指标、趋势和核心图表 |
| `Map Studio` | 年份、指标、收入组和区域可切换的地图工作台 |
| `交互组件` | Plotly、Leaflet、Reactable、DT 等组件入口 |
| `政策` | 结论与政策含义 |

## 本地运行

安装依赖：

```bash
Rscript 安装依赖.R
```

启动 Shiny：

```bash
Rscript 启动仪表盘.R 4848
```

常用构建命令：

```bash
Rscript 构建.R features
Rscript 构建.R submission
Rscript 构建.R quality
```

完整交付包生成：

```bash
$env:GHS_BUILD_DELIVERY='TRUE'
Rscript 构建.R delivery
```

部署 Shiny 云端版：

```bash
Rscript 开发脚本/部署Shiny云端.R ghs-dashboard
```

部署脚本会优先使用当前环境变量中的 shinyapps.io 账号信息；如果本机已保存 rsconnect 账号，也可以直接复用本机账号记录。

## 关键目录

| 路径 | 说明 |
|---|---|
| `程序/` | 数据读取、清洗、特征、模型、绘图、widget、静态页和交付包生成源码 |
| `仪表盘/` | Shiny 应用源码、模块、前端资源、数据快照和部署用程序库 |
| `课程提交/` | 课程 HTML 与 Rmd |
| `网站发布/` | GitHub Pages 静态发布目录 |
| `分析输出/图表/` | PNG 与 SVG 静态图 |
| `分析输出/交互组件/` | standalone HTML widgets 与依赖资源 |
| `分析输出/模型表/` | 模型、指标、预测、聚类、公平性和情景模拟结果 |
| `派生数据/处理结果/feature_mart_country_year.csv` | country-year 特征表 |
| `派生数据/处理结果/feature_dictionary.csv` | 变量字典 |
| `项目文档/方法手册.md` | 方法矩阵 |
| `项目文档/变更记录.md` | 更新记录 |
| `庄颂_20241334/` | 课程提交包目录，根目录除文件夹外只保留一个 `README.md` |

## 交付说明

提交包由 `程序/26_delivery.R` 生成，根目录只保留 `README.md` 一个文件；安装、启动、构建、依赖和索引文件已移动到 `项目入口/`、`交付索引/` 等文件夹中。`庄颂_20241334.zip` 是当前提交包的压缩版本。

提交包内重点目录：

| 路径 | 内容 |
|---|---|
| `课程提交/` | 课程 HTML 与 Rmd |
| `网站发布/` | 静态发布页与发布版交互组件 |
| `仪表盘/` | Shiny 应用、模块、前端资源和运行快照 |
| `程序/` | 全部分析与构建源码 |
| `分析输出/` | 图表、交互组件、模型表和质量报告 |
| `项目文档/` | 方法手册、部署说明、变更记录 |
| `交付索引/` | 交付清单和完整文件索引 |
| `项目入口/` | 安装、启动、构建和依赖文件 |

当前质量门禁覆盖语法、PNG/SVG 图像、链接、widget、交互视图、图库、文本痕迹、Shiny bundle、静态导航、科学可追溯性、交付项和 README 一致性。正常情况下只保留 HTML 体积警告，因为课程主 HTML 为离线可读文件，体积较大属于预期。

Shiny 云端部署脚本为 `开发脚本/部署Shiny云端.R`。本机已完成最新部署，目标地址为 <https://constantine1433223.shinyapps.io/ghs-dashboard/>。
