# 🖥️ 部署教程 · Shiny → shinyapps.io

> 目标：把 `仪表盘/` 下的 Shiny 仪表盘发布到 shinyapps.io 这个托管服务。
> 最终 URL 形如：`https://<你的账号>.shinyapps.io/ghs-dashboard/`。
> **两条路**：① 手动一次性部署；② GitHub Actions 自动部署（推 push 即上线）。

---

## 一、选路线

| 路线 | 适合场景 | 特点 |
|---|---|---|
| **A · 手动** | 只上线一次，图方便 | 本地一行命令，5 分钟完成 |
| **B · Actions 自动** | 频繁迭代，团队协作 | push 即部署，要配 3 个 secret |
| **C · shinylive (WASM)** | 希望完全零服务器 | 用户浏览器内直接跑 → [`部署_浏览器仪表盘.md`](./部署_浏览器仪表盘.md) |

---

## 二、注册 shinyapps.io（只做一次）

1. 打开 <https://www.shinyapps.io/>，用 GitHub / Google 账号登录。
2. 首次登录会让你选 **账号名**（这就是将来 URL 的 `<你的账号>` 部分，不能改）。
3. 免费计划限制：**25 小时 / 月活跃时长**、**5 个 app**、每 app 1 GB 内存。

---

## 三、获取 token & secret

1. 进 shinyapps.io Dashboard → 右上角头像 → **Tokens**。
2. 点 **Add Token**。
3. 点新 token 行右边的 **Show**，复制里面的 `rsconnect::setAccountInfo(...)` 整段 R 代码。

格式形如：

```r
rsconnect::setAccountInfo(
  name   = 'constantine114514',
  token  = '<TOKEN>',
  secret = '<SECRET>'
)
```

> **安全警告**：`secret` 是敏感凭据，**不要 commit 进 git**。本项目的 `.gitignore` 已排除 `rsconnect/` 文件夹。

---

## 四、路线 A · 手动部署（一次性）

本地 R 控制台：

```r
# 1) 首次：只在本机 .Renviron 设置，不写入项目脚本
usethis::edit_r_environ()

# 写入以下三行后重启 R：
SHINYAPPS_NAME=constantine114514
SHINYAPPS_TOKEN=<TOKEN>
SHINYAPPS_SECRET=<SECRET>

# 2) 部署：脚本会准备 数据快照/ 与 程序库/，再调用 rsconnect::deployApp()
source("开发脚本/部署Shiny云端.R", encoding = "UTF-8")
```

部署前需把 `程序/*.R` 复制到 `仪表盘/程序库/`，并把 `派生数据/处理结果/master_enriched.rds` 复制为 `仪表盘/数据快照/snapshot.rds`。`开发脚本/部署Shiny云端.R` 已自动完成这一步；手动部署前可先运行 `Rscript 构建.R data`。首次会上传 + 构建（约 5-10 分钟）。完成后会自动打开 URL。

---

## 五、路线 B · GitHub Actions 自动部署

### 5.1 把凭据写成 3 个 Repository Secrets

1. GitHub 仓库 → **Settings** → 左侧 **Secrets and variables** → **Actions** → **New repository secret**。
2. 分别新建 3 条：

| Name | Value |
|---|---|
| `SHINYAPPS_NAME` | shinyapps.io 账号名（如 `constantine114514`） |
| `SHINYAPPS_TOKEN` | Token 中的 `token=` 字段值 |
| `SHINYAPPS_SECRET` | Token 中的 `secret=` 字段值 |

![Secrets 截图](https://docs.github.com/assets/cb-21633/mw-1440/images/help/repository/actions-secrets-and-variables.webp)

### 5.2 推送触发

本项目的 `.github/workflows/deploy.yml` 里 `deploy-shinyapps` job 已写好 —— 只要 3 个 secret 齐了、`build` job 成功，就会自动准备可移植 Shiny 包并部署 `仪表盘/`。

```bash
git push origin main
```

### 5.3 观察结果

在 Actions → 最新 run → `deploy-shinyapps` job → **Deploy to shinyapps.io** 步骤，看到 `Application successfully deployed to https://...` 即成功。

---

## 六、优化：压缩启动时间

### 6.1 预计算重数据到 RDS

本项目已在 `仪表盘/数据快照/` 存快照。**不要** 在 app 里算大表，让 RDS 做 fast path：

```r
# 仪表盘/global.R 会优先读取：
master <- readRDS('数据快照/snapshot.rds')
```

### 6.2 裁剪依赖包

shinyapps.io 按"包数量"影响启动。只保留**运行时**必需的：

- 必需：`shiny`, `bslib`, `dplyr`, `tidyr`, `ggplot2`, `scales`, `plotly`, `leaflet`, `DT`, `reactable`, `shinyWidgets`, `shinycssloaders`, `sf`, `countrycode`
- 可删：`renv`, `targets`, `testthat`, `lintr`（部署前确保 仪表盘/global.R 没有 require 它们）

### 6.3 `packrat`/`renv` 警告

部署时 shinyapps.io 会自己生成 `manifest.json`。本地的 `renv.lock` 不会被带上线。

---

## 七、访问与管理

- Dashboard URL：`https://www.shinyapps.io/admin/#/dashboard`
- 活跃时长：右上角 **Usage** 查看本月剩余。
- 关停 / 归档：点 app 名 → Settings → Archive（节省时长）。
- 自定义域名：免费版不支持；Starter 起支持。

---

## 八、常见问题

### ❌ `Error: unable to load shared object ...`

缺系统库。shinyapps.io 基于 Ubuntu 20.04，若你的 app 需要 `rgdal`/`sf` 等本地库，添加显式依赖：

```r
# 在 仪表盘/global.R 顶部
suppressPackageStartupMessages({
  library(sf)    # 触发 sf C++ 库
})
```

### ❌ 部署成功但打开白屏

1. 右上角 **Logs** → 看报错堆栈。
2. 常见：`snapshot.rds` 没打包进去 → 检查 `仪表盘/数据快照/snapshot.rds`。
3. 常见：`程序库/` 没打包进去 → 检查 `仪表盘/程序库/*.R`。

### ❌ CI 里 secret 没生效

- 确保 secret 名称拼写完全一致（大小写敏感）。
- 只能在 **同一仓库的 Actions** 读到；fork 不会继承。

### ❌ "App is too large" (> 500 MB)

删掉大文件：`派生数据/原始缓存/*.parquet`、`分析输出/交互组件/*.html`（这些不是 Shiny 运行必需的）。

```r
appFiles = list.files("仪表盘", recursive = TRUE, full.names = FALSE)
```

---

## 九、下一步

- shinylive（浏览器内 WASM）→ [`部署_浏览器仪表盘.md`](./部署_浏览器仪表盘.md)
- 静态站点 Pages → [`部署_GitHub_Pages.md`](./部署_GitHub_Pages.md)
