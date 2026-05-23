Shiny 云端版：https://constantine114514.shinyapps.io/ghs-dashboard/ ｜ 静态发布版：https://2711944586.github.io/R/

# 原始数据说明

本目录保留 WHO Global Health Expenditure Database 在 TidyTuesday 2026-04-21 发布的三张核心 CSV。它们是项目清洗、建模、报告和 Shiny 快照的起点，也是提交包中保留的原始数据副本。

## 文件

| 文件 | 内容 | 规模 |
|---|---|---:|
| `health_spending.csv` | 卫生总费用与资金来源，包括 CHE、GGHED、PVTD、EXT | 约 3.4 MB |
| `financing_schemes.csv` | 按筹资方案划分的卫生支出，包括政府方案、自愿保险、居民自付等 | 约 3.9 MB |
| `spending_purpose.csv` | 按卫生服务功能划分的支出，包括治疗、康复、长期护理、预防、治理等 | 约 1.2 MB |
| `health_spending.md` | `health_spending.csv` 字段说明 | 小文件 |
| `financing_schemes.md` | `financing_schemes.csv` 字段说明 | 小文件 |
| `spending_purpose.md` | `spending_purpose.csv` 字段说明 | 小文件 |
| `intro.md` | TidyTuesday 原始介绍 | 小文件 |

## 数据来源

数据来自 WHO Global Health Expenditure Database，经 TidyTuesday 整理发布：

- GHED portal: <https://apps.who.int/nha/database>
- TidyTuesday week: `2026-04-21`
- TidyTuesday repository: <https://github.com/rfordatascience/tidytuesday>

项目分析只使用本目录中的本地 CSV，不在评阅时自动联网抓取数据。联网入口仅用于核对来源或重新下载。

## 关键字段

三张表共享以下基础字段：

| 字段 | 含义 |
|---|---|
| `country_name` | WHO 使用的国家或地区名称 |
| `iso3_code` | ISO 3166-1 alpha-3 代码 |
| `year` | 年份 |
| `indicator_code` | 指标代码 |
| `value` | 指标值 |
| `unit` | 单位 |

`indicator_code` 的后缀用于区分口径：

- `_che`：占 current health expenditure 的百分比。
- `_usd2023`：2023 年不变价美元。

## 项目使用口径

项目在 `程序/01_io.R`、`程序/02_clean.R`、`程序/03_enrich.R` 中完成清洗和宽表整理。主要处理包括：

- 统一字段类型，保留 `country_name`、`iso3_code`、`year` 和指标值。
- 将金额变量整理为 USD 2023 不变价序列。
- 将筹资结构和功能结构整理为占 CHE 的百分比。
- 结合收入组、区域、人口、GDP、人均支出、预期寿命和儿童死亡率等外部变量生成分析主表。

Shiny 提交包默认读取已经整理好的运行快照 `仪表盘/数据快照/snapshot.rds`，因此打开仪表盘不需要重新清洗原始 CSV。需要完整复现时，可从根目录运行：

```bash
Rscript 构建.R data
Rscript 构建.R features
```

## 提交包说明

最终 zip 保留本目录，因为三张 CSV 合计约 8.5 MB，压缩后体积可控，并且能支撑代码复核。相比之下，`分析输出/交互组件/`、`网站发布/` 和重复图表目录体积很大，已从最小提交包中排除；这些内容可由代码重新生成，也可通过线上 Shiny 和 Pages 查看。
