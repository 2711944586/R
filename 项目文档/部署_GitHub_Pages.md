# 🚀 部署教程 · GitHub Pages 发布 Quarto Book

> 目标：把本项目的 **Quarto Book（12 章叙事）** 自动发布成一个可公开访问的网站。
> 最终 URL 形如：`https://<你的用户名>.github.io/<仓库名>/`。
> **零本地运行**：用户只需点链接就能看到全部图表和内容。

---

## 一、前置条件

| 项 | 要求 |
|---|---|
| GitHub 账号 | ✅ 已有（本项目 <https://github.com/2711944586/R>） |
| 仓库可见性 | **Public**（Private 仓库的 Pages 需要 GitHub Pro） |
| 分支 | 默认 `main`（或 `master`） |
| 本地能 `git push` | ✅ 代码已推送 |

---

## 二、启用 GitHub Pages（**只做一次**）

1. 打开仓库首页 → 点 **Settings**（⚙ 右上角）。
2. 左侧导航栏点 **Pages**。
3. **Build and deployment** 区域：
   - **Source** 下拉选 **`GitHub Actions`**（不是 Deploy from a branch）。
   - 保存即可（Source 改为 Actions 后不需要选分支）。

![Step 2 示意](https://docs.github.com/assets/cb-47267/images/help/pages/publishing-source-drop-down.png)

---

## 三、推送代码触发部署

本项目 workflow 已写好在 `.github/workflows/deploy.yml`，触发条件：

- **push 到 `main` 且改动了 `程序/` / `报告书/` / `仪表盘/` / `_targets.R`**
- 也可手动在 Actions 页面点 **Run workflow**

```bash
git add .
git commit -m "deploy v2"
git push origin main
```

## 四、等待 build（约 10-15 分钟）

1. 打开仓库 → 点顶部 **Actions** tab。
2. 找到最新的 **Deploy v2 · Pages + shinylive + shinyapps.io** run。
3. 观察 3 个 job：
   - `build` ≈ 8-12 min — 装 R 包 + 渲染 Quarto + shinylive 编译
   - `deploy-pages` ≈ 1 min — 推到 Pages
   - `deploy-shinyapps` ≈ 5 min — 可选（未配 secret 会自动跳过）
4. 全部 ✅ 后，**部署 URL** 会显示在 `deploy-pages` job 的 summary 里。

---

## 五、访问站点

默认 URL：

```
https://<username>.github.io/<repo-name>/
```

例：<https://2711944586.github.io/R/>

打开后你应该看到：

- **Hero** — 封面大标题 + 关键 KPI 卡片
- **12 章叙事** — 左侧目录可跳转
- **顶部 Dashboard 链接** — 通向 `/仪表盘/`（shinylive 编译的 Shiny）
- **各章内嵌交互图** — plotly / leaflet 均可用

---

## 六、自定义域名（可选）

1. **Settings → Pages → Custom domain** 填入你的域名（如 `ghs.example.com`）。
2. 在域名 DNS 处加 **CNAME** 记录指向 `<username>.github.io`。
3. 等 DNS 生效（5 min – 24 h），勾选 **Enforce HTTPS**。

---

## 七、常见问题 / 排错

### ❌ build 失败：`Quarto render` error

打开失败 run 的 `build` job，展开 **Render Quarto Book** 步骤：

- 报错 `could not find function "xxx"` → 在 `deploy.yml` 的 `packages:` 列表里加包名。
- 报错 `cannot find file "..."` → 多半是某个章节引用了不存在的文件，检查 `报告书/*.qmd` 的 `source()` 或 `include` 路径。
- 报错 `No default font family "…"` → 忽略即可，CI 已装 `fonts-noto-cjk` + `fonts-inter` 作兜底。

### ❌ 部署 URL 404

- 确认 Source 已切到 **GitHub Actions**（不是 deploy from branch）。
- 确认最新 run 的 `deploy-pages` 是 ✅。
- 如果刚改 Source，等 5 min 再试。

### ❌ 中文字体方框

`deploy.yml` 已预装 `fonts-noto-cjk`，应该不会。如果仍有问题：

1. 在 `报告书/_setup.R` 开头加 `showtext::showtext_auto()` 与 `register_brand_fonts()`。
2. `knitr::opts_chunk$set(dev = "ragg_png")`。

### ❌ Actions 配额耗尽

- 公开仓库 **免费无限分钟**，私有仓库每月 2000 分钟（免费版）。
- 本项目每次 build ≈ 15 分钟 ≈ 每月可跑 ~130 次。

### ❌ 网站发布/ 被 git 追踪冲突

`网站发布/` 已在 `.gitignore`，CI 生成后不会回写到仓库。
如果你本地跑过 `quarto render 报告书`，`网站发布/` 会生成但不被 commit —— 正常。

---

## 八、一键本地预览（可选）

不推 push 直接看渲染结果：

```bash
quarto render 报告书
quarto preview 报告书     # 本地 http://localhost:xxxx
```

---

## 九、下一步

- 仪表盘部署 → [`项目文档/部署_Shiny云端.md`](./项目文档/部署_Shiny云端.md)
- shinylive 嵌入 Pages → [`项目文档/部署_浏览器仪表盘.md`](./项目文档/部署_浏览器仪表盘.md)
- 所有产物的一键本地构建 → `Rscript 构建.R all`
