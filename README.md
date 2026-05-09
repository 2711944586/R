# Global Health Spending 2000–2023 · 最终交付版

> 作者：庄颂（20241334） · 数据：TidyTuesday 2026-04-21 / WHO GHED + WDI  
> 仓库：<https://github.com/2711944586/R> · 在线：<https://2711944586.github.io/R/>

## 单一标准（Single source of truth）

整个项目只产出 **一份页面**，由唯一生成器 `程序/21_static_showcase.R` 派生为两个等价副本：

| 用途 | 路径 | 大小 | 说明 |
|---|---|---|---|
| **课程提交** | `课程提交/庄颂_20241334.html` | ~26 MB | 自包含图表 base64；交互组件经相对路径或仓库链接打开。 |
| **GitHub Pages 首页** | `网站发布/index.html` | ~26 MB | 与提交版同源；交互组件由 `网站发布/交互组件/` 提供。 |
| **课程提交 Rmd** | `课程提交/庄颂_20241334.Rmd` | < 1 KB | 一键调用生成器；可直接 knit 复现。 |

页面组织（**36 个内容章节**，统一品牌设计）：

1. **Hero** —— 项目元数据与四段导航
2. **01 · Snapshot** —— 8 张 KPI（覆盖国家、CHE 总额、年化增速、OOPS 等）
3. **02 · Data & Methods** —— 真实 R 代码（io / clean / enrich / metrics）
4. **F1–F14 · 十四项核心发现** —— 每项含：研究问题 · 方法 · R 代码 · 原图 · 数据表 · 深度解读 · 数值 chips
5. **11 · Gallery** —— 50 张主体静态图，按主题（时间/分布/地图/结构/模型/指标/综合）筛选
6. **12 · Interactive lab** —— 24 个 standalone widget，懒加载 iframe
7. **13 · Reproducibility** —— 8 条一键命令
8. **14 · Conclusion** —— 6 条政策建议 + 3 条研究局限

不再生成"完整静态展示.html / Quarto book 章节"等冗余产物（已删除）；`网站发布/` 现在只承载 `index.html` + `交互组件/` + `仪表盘/`。

## 一键复现

```bash
Rscript 安装依赖.R
Rscript 构建.R data        # 派生数据/处理结果/master_enriched.rds
Rscript 构建.R figures     # 分析输出/图表/*.png + .svg（50+ 张）
Rscript 构建.R widgets     # 分析输出/交互组件/*.html（24 个）
Rscript 构建.R models      # 分析输出/模型表/*.csv | *.rds（17 张）
Rscript 构建.R submission  # 课程提交/庄颂_20241334.html + 网站发布/index.html
Rscript 启动仪表盘.R 4848  # 本地 Shiny（12 模块）
```

| 目标 | 命令 |
|---|---|
| 全量构建 | `Rscript 构建.R all` |
| 部署包（shinylive + 整合首页） | `Rscript 构建.R deploy` |
| 项目审计 | `Rscript 构建.R audit` |
| 自动测试 | `Rscript 开发脚本/运行自动测试.R` |
| 语法检查 | `Rscript 开发脚本/语法检查.R` |
| Shiny 模块测试 | `Rscript 开发脚本/测试仪表盘模块.R` |

## 项目目录

| 路径 | 用途 |
|---|---|
| `README.md` | 当前项目总说明。 |
| `DESCRIPTION` | R 项目元信息；也是 `rprojroot` 定位项目根目录的锚点。 |
| `LICENSE` | MIT 许可证。 |
| `.gitignore` | Git 忽略规则，排除缓存、日志、临时文件和敏感配置。 |
| `.Rprofile` | R 启动配置：CRAN 镜像、UTF-8 编码、欢迎提示。 |
| `.Rbuildignore` | R 包构建忽略规则。 |
| `.dockerignore` | Docker 构建忽略规则。 |
| `.lintr` | R 代码静态检查配置。 |
| `_targets.R` | `{targets}` 可复现数据流水线入口，文件名必须保留。 |
| `安装依赖.R` | 一键安装 R 包依赖。 |
| `构建.R` | 本地统一构建脚本：数据、模型、图表、交互、提交版、部署。 |
| `启动仪表盘.R` | 本地启动 Shiny 仪表盘的轻量入口。 |
| `.github/workflows/` | GitHub Actions：测试、Pages、shinylive、shinyapps.io。 |
| `原始数据/` | 作业指定的 GHED 三张原始 CSV 与说明文档。 |
| `程序/` | R 函数库，承载数据处理、建模、绘图、部署辅助。 |
| `派生数据/` | 缓存、外部数据和处理结果。 |
| `分析输出/` | 静态图、交互 HTML、模型表和报告输出。 |
| `报告书/` | Quarto Book 源文件。 |
| `网站发布/` | Quarto/shinylive 生成的静态网站发布目录。 |
| `仪表盘/` | Shiny v2 应用。 |
| `探索分析/` | EDA / 探索性分析文档。 |
| `课程提交/` | 作业兜底交付：Rmd + HTML。 |
| `自动测试/` | testthat 测试套件。 |
| `开发脚本/` | 一次性修复、审计、验证和批量构建脚本。 |
| `项目文档/` | 方案、部署教程、变更记录、课程说明文档。 |
| `部署配置/` | Dockerfile 与 docker-compose 配置。 |

## 数据目录

| 路径 | 内容 |
|---|---|
| `原始数据/financing_schemes.csv` | 卫生筹资方案表，变量名 `financing_schemes` 不改。 |
| `原始数据/health_spending.csv` | 卫生支出来源表，变量名 `health_spending` 不改。 |
| `原始数据/spending_purpose.csv` | 卫生支出用途表，变量名 `spending_purpose` 不改。 |
| `原始数据/*.md` | TidyTuesday 数据说明。 |
| `派生数据/原始缓存/` | 外部下载或函数级缓存。 |
| `派生数据/外部数据/` | WDI / GHO / IMF 等外部数据缓存。 |
| `派生数据/中间数据/` | 清洗过程中的临时中间产物。 |
| `派生数据/处理结果/master_enriched.rds` | Shiny、Quarto 和模型共享的主分析表。 |
| `派生数据/处理结果/world_sf_medium.rds` | 世界地图 sf 缓存。 |

## 程序模块

| 路径 | 职责 |
|---|---|
| `程序/00_utils.R` | 项目根定位、缓存、日志、字体、数值格式化、图表保存。 |
| `程序/01_io.R` | 读取 GHED 三张原始表，严格保留三个数据集变量名。 |
| `程序/02_clean.R` | 长表/宽表转换、筹资方案与资金来源加总校验。 |
| `程序/03_enrich.R` | 国家元数据、收入组、WDI、世界地图等增强。 |
| `程序/04_metrics.R` | Gini、Theil、Atkinson、集中率、COVID 冲击指标。 |
| `程序/05_models.R` | PCA、聚类、变点、β 收敛、面板固定效应、预测。 |
| `程序/06_plot_theme.R` | ggplot 主题、色板、图表标签统一。 |
| `程序/07_plot_static.R` | 静态图工厂函数。 |
| `程序/08_plot_interactive.R` | plotly、leaflet、reactable 等交互图工厂函数。 |
| `程序/09_maps.R` | 地图绘制辅助。 |
| `程序/10_report_helpers.R` | 批量导出图表、交互组件和模型表。 |
| `程序/11_external_data.R` | 多源外部数据拉取与缓存。 |
| `程序/12_advanced_models.R` | DEA、灾难性医疗支出、情景模拟等高级模型。 |
| `程序/13_design_system.R` | 数据新闻视觉系统。 |
| `程序/14_plots_thematic.R` | 主题型深度图表。 |
| `程序/15_plots_dataviz.R` | 数据新闻型图表。 |
| `程序/16_data_quality.R` | 数据质量、缺失和一致性报告。 |
| `程序/17_widgets_plotly.R` | plotly 交互组件。 |
| `程序/18_widgets_other.R` | leaflet / DT / reactable / network 等组件。 |
| `程序/19_narrative.R` | Quarto 叙事辅助函数。 |
| `程序/20_deploy.R` | 部署辅助：复制组件、生成 sitemap、体积报告、缓存预热。 |
| `程序/21_static_showcase.R` | 生成新版整合静态展示页和课程提交 HTML。 |
| `程序/22_plots_extra.R` | 扩展图集与高级分析图：分位回归、变点、SDG-3、排名变迁、聚类画像等。 |

## 六、Shiny 仪表盘

| 路径 | 用途 |
|---|---|
| `仪表盘/global.R` | 自动定位项目根，加载 `程序/` 或 shinylive 内置 `程序库/`，读取缓存。 |
| `仪表盘/ui.R` | 12 个模块化导航页的 UI。 |
| `仪表盘/server.R` | 12 个模块 server 调用链。 |
| `仪表盘/模块/_helpers.R` | Shiny 模块共享组件。 |
| `仪表盘/模块/mod_*.R` | 12 个业务模块。 |
| `仪表盘/数据快照/snapshot.rds` | shinylive 打包时使用的数据快照。 |
| `仪表盘/程序库/` | shinylive 打包时复制进去的 R 函数库。 |

本地启动：

```r
shiny::runApp("仪表盘", port = 4848)
```

或：

```bash
Rscript 启动仪表盘.R 4848
```

## 七、Quarto Book、整合首页与发布目录

| 路径 | 用途 |
|---|---|
| `报告书/_quarto.yml` | Quarto Book 配置，输出到 `网站发布/`。 |
| `报告书/_setup.R` | 每章共享 setup：加载函数库、主题、主表、地图。 |
| `报告书/index.qmd` | 书籍首页。 |
| `报告书/01-*.qmd` 至 `报告书/12-*.qmd` | 12 章叙事分析。 |
| `报告书/A0-methodology.qmd` | 方法附录。 |
| `报告书/references.qmd` / `references.bib` | 参考文献。 |
| `网站发布/index.html` | GitHub Pages 入口；当前由 `Rscript 构建.R submission` 生成整合展示页。 |
| `网站发布/仪表盘/` | shinylive 版本 Shiny。 |
| `网站发布/交互组件/` | 复制后的 standalone HTML 交互组件及依赖资源。 |

渲染：

```bash
quarto render 报告书
Rscript 构建.R submission
```

说明：GitHub Actions 会先准备 Shiny / shinylive 资源，再执行 `Rscript 构建.R submission`，因此线上首页会使用最终整合展示页。

## 八、输出目录

| 路径 | 用途 |
|---|---|
| `分析输出/图表/` | PNG/SVG 静态图。 |
| `分析输出/交互组件/` | standalone HTML 交互组件。 |
| `分析输出/模型表/` | CSV/RDS 模型与指标结果。 |
| `分析输出/报告/` | 额外报告输出位置。 |

## 九、课程提交与探索分析

| 路径 | 用途 |
|---|---|
| `课程提交/庄颂_20241334.Rmd` | 课程要求的 RMarkdown 源文件；现在作为最终展示页生成入口。 |
| `课程提交/庄颂_20241334.html` | 课程要求的 HTML 结果文件，图表内嵌，交互组件按相对路径加载。 |
| `探索分析/01_eda.qmd` | 探索性分析：缺失、分布、异常值、相关性。 |

## 十、开发脚本

| 路径 | 用途 |
|---|---|
| `开发脚本/审计项目.R` | 检查各阶段文件与产物是否齐全。 |
| `开发脚本/语法检查.R` | 解析 `程序/*.R`，快速发现语法错误。 |
| `开发脚本/运行自动测试.R` | 运行 `自动测试/testthat/`。 |
| `开发脚本/测试仪表盘模块.R` | 验证 12 个 Shiny 模块、UI 和 server。 |
| `开发脚本/测试报告拓展前四章.R` | 验证报告前四章拓展代码。 |
| `开发脚本/测试报告拓展全书.R` | 验证报告后续章节拓展代码。 |
| `开发脚本/生成分析输出.R` | 批量生成图表、交互组件和模型表。 |
| `开发脚本/构建拓展产物.R` | 拓展产物构建脚本。 |
| `开发脚本/启动仪表盘开发版.R` | 4848 端口启动开发版仪表盘。 |
| `开发脚本/追加报告拓展.R` | 批量追加 Quarto 章节拓展。 |
| `开发脚本/修复报告编码.R` | 修复报告编码问题。 |
| `开发脚本/修复报告项目根.R` | 修复 Quarto 根目录识别问题。 |
| `开发脚本/修复裸中文参数.R` | 修复裸中文参数名。 |
| `开发脚本/修复反引号中文.R` | 修复反引号中的 Unicode 转义。 |

## 十一、测试与验证

```bash
Rscript 开发脚本/语法检查.R
Rscript 开发脚本/运行自动测试.R
Rscript 开发脚本/测试仪表盘模块.R
Rscript 构建.R submission
Rscript 构建.R audit
```

轻量验证优先跑：

```bash
Rscript 开发脚本/语法检查.R
Rscript 构建.R submission
Rscript 构建.R audit
```

## 十二、部署

### GitHub Pages + shinylive

1. GitHub 仓库 Settings → Pages → Source 选择 `GitHub Actions`。
2. 推送到 `main` 或 `master`。
3. `.github/workflows/deploy.yml` 会构建 `网站发布/` 并上传到 Pages。
4. 工作流会执行 `Rscript 构建.R submission`，确保 Pages 首页为新版整合展示页。

目标 URL：

```text
https://2711944586.github.io/R/
https://2711944586.github.io/R/仪表盘/
```

### shinyapps.io

需要在 GitHub Actions Secrets 中配置：

| Secret | 含义 |
|---|---|
| `SHINYAPPS_NAME` | shinyapps.io 账号名。 |
| `SHINYAPPS_TOKEN` | rsconnect token。 |
| `SHINYAPPS_SECRET` | rsconnect secret。 |

详见 `项目文档/部署_Shiny云端.md`。

### Docker

从项目根目录执行：

```bash
docker build -f 部署配置/Dockerfile -t ghs-dashboard .
docker run -p 3838:3838 ghs-dashboard
```

或：

```bash
docker compose -f 部署配置/docker-compose.yml up -d
```

## 十三、项目文档

| 路径 | 用途 |
|---|---|
| `项目文档/项目方案.md` | 20× 项目方案、阶段计划与验收标准。 |
| `项目文档/高标准审核与升级建议.md` | 本轮高标准审核、bug 清单、已修复项、10× 提升建议与复核清单。 |
| `项目文档/变更记录.md` | 历史变更与已修复问题。 |
| `项目文档/部署总览.md` | 本地、Docker、CI/CD 总览。 |
| `项目文档/部署_GitHub_Pages.md` | GitHub Pages 静态站部署教程。 |
| `项目文档/部署_Shiny云端.md` | shinyapps.io 部署教程。 |
| `项目文档/部署_浏览器仪表盘.md` | shinylive 浏览器内运行教程。 |
| `项目文档/2026作业_Global Health Spending 数据集自由分析.docx` | 课程原始说明文档。 |

## 十四、GitHub 上传与当前状态

本项目已初始化 Git，并配置：

```text
branch: main
origin: https://github.com/2711944586/R.git
```

首次推送：

```bash
git add .
git commit -m "升级最终静态展示与部署流程"
git push -u origin main
```

如果远端已有历史，请先 `git pull --rebase origin main` 后再推送。

注意：

- 根目录课程说明 `.docx` 已由 `.gitignore` 排除；项目文档目录内保留课程说明副本。

## 十五、关键约束

- `financing_schemes`、`health_spending`、`spending_purpose` 三个变量名不能改。
- `README.md`、`DESCRIPTION`、`LICENSE`、`_targets.R`、`.github/`、`.gitignore` 等生态入口保留英文命名。
- Shiny 必须保留 `global.R`、`ui.R`、`server.R` 三个文件名。
- Quarto 输出目录为 `网站发布/`，不要把临时 cache 当作源码提交。
- API token、`.Renviron`、`.env`、`rsconnect/` 不应提交到 Git。
