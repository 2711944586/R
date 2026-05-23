Shiny 云端版：https://constantine114514.shinyapps.io/ghs-dashboard/ ｜ 静态发布版：https://2711944586.github.io/R/

# 项目总览

## 交付定位

本项目围绕 2000-2023 年全球卫生支出展开，主体交付由两部分组成：

- `课程提交/庄颂_20241334.html` 与 `课程提交/庄颂_20241334.Rmd`：课程报告与源文件；HTML 是完整主报告，Rmd 与 HTML 同源同步。
- `仪表盘/` 与 `程序/`：Shiny 应用和核心 R 代码，支撑本地运行、云端部署和结果复核。

当前提交包由 `程序/26_delivery.R` 生成，根目录除文件夹外只保留一个 `README.md`。安装、启动、构建、依赖和索引文件统一放入 `项目入口/`、`交付索引/` 等目录，避免根目录变成杂项入口。

Shiny 首页已改为纯标题封面页，只保留项目题名和年份范围。静态 HTML 与同源 Rmd 也完成本轮修复：标题页动画恢复、背景回到统一暖纸色、右下角 dock 可正常开合并支持点空白关闭、图像弹层可查看高清原图、课程提交 HTML 使用线上绝对地址直接载入真实 standalone widget，单独转发 HTML 文件也可以查看完整组件。云端 Shiny 已随应用发布完整 standalone widget 缓存，打开后可以直接看到地图、Plotly、表格、网络图和专题组件；提交包不复制这批大缓存，以保持压缩包体积稳定。

## 给老师汇报的两个网页入口

| 网页 | 地址 | 重点用途 | 部署方式 |
|---|---|---|---|
| Shiny 云端仪表盘 | <https://constantine114514.shinyapps.io/ghs-dashboard/> | 现场汇报主入口。用于演示可切换模块、地图工作台、国家比较、筹资结构、健康产出、交互组件和政策结论。 | `开发脚本/部署Shiny云端.R ghs-dashboard` 发布到 shinyapps.io。 |
| GitHub Pages 静态报告 | <https://2711944586.github.io/R/> | 在线阅读主入口。用于展示完整报告、36 个章节、14 项核心发现、图表库、组件库和复现说明。 | `.github/workflows/deploy.yml` 发布 `网站发布/`。 |

Shiny 云端版偏“演示和操作”，GitHub Pages 静态版偏“阅读和留档”。课程提交 HTML 与静态发布页同源生成，交互 iframe 直接使用线上真实 standalone widget。

## 在线入口

| 入口 | 地址 |
|---|---|
| Shiny 云端版 | <https://constantine114514.shinyapps.io/ghs-dashboard/> |
| GitHub Pages | <https://2711944586.github.io/R/> |
| GitHub 仓库 | <https://github.com/2711944586/R> |

Shiny 最新部署记录以本轮 `rsconnect::deployApp()` 输出为准；应用固定地址为 <https://constantine114514.shinyapps.io/ghs-dashboard/>。

## 提交包主体

| 路径 | 作用 |
|---|---|
| `README.md` | 评阅打开顺序、本地运行和包内容说明 |
| `课程提交/` | 完整 HTML 主报告与同源 Rmd |
| `程序/` | 分析、建模、绘图、组件和报告生成源码 |
| `仪表盘/` | Shiny 应用源码、模块、前端资源、运行快照和地图缓存 |
| `项目入口/` | `DESCRIPTION`、`renv.lock`、安装、启动和构建入口 |

## 本地运行

在提交包根目录执行：

```bash
Rscript 安装依赖.R
Rscript 启动仪表盘.R 4848
```

浏览器打开：

```text
http://127.0.0.1:4848
```

Shiny 默认使用 `仪表盘/数据快照/snapshot.rds` 和 `仪表盘/派生数据/处理结果/world_sf_medium.rds`，分析函数从提交包根目录的 `程序/` 读取，避免在提交包内重复保存一份代码副本。

## 课程文档说明

`课程提交/庄颂_20241334.html` 是完整主报告，保留正文、静态图、图表库、交互入口和结论；`课程提交/庄颂_20241334.Rmd` 由同一份 HTML 抽取样式、正文和脚本写入，内容与 HTML 保持一致。交互组件区直接载入线上绝对地址，完整 Plotly、Leaflet、DT、Reactable 和 networkD3 组件可在单独转发 HTML 时查看，也可通过 shinyapps.io 与线上静态发布地址打开。代码注释已按解析器检查清理，保留必要的文件结构和函数命名，不再保留大段解释型注释。
