if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
}

.ghs_html_escape <- function(x) {
  x <- as.character(x %||% "")
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x
}

.ghs_fmt_num <- function(x, digits = 1, suffix = "") {
  if (is.null(x) || !length(x) || !is.finite(x)) return("—")
  paste0(format(round(x, digits), big.mark = ",", nsmall = digits), suffix)
}

.ghs_fmt_money <- function(x) {
  if (exists("fmt_usd", mode = "function")) return(fmt_usd(x))
  if (!is.finite(x)) return("—")
  if (abs(x) >= 1e12) return(sprintf("$%.2fT", x / 1e12))
  if (abs(x) >= 1e9) return(sprintf("$%.2fB", x / 1e9))
  if (abs(x) >= 1e6) return(sprintf("$%.2fM", x / 1e6))
  sprintf("$%.0f", x)
}

.ghs_file_size <- function(path) {
  if (!file.exists(path)) return("—")
  mb <- file.info(path)$size / 1024^2
  if (mb >= 1) sprintf("%.1f MB", mb) else sprintf("%.0f KB", mb * 1024)
}

.ghs_b64 <- function(path) {
  if (!requireNamespace("base64enc", quietly = TRUE)) {
    stop("Package 'base64enc' is required to build the standalone static HTML.")
  }
  base64enc::base64encode(path)
}

.ghs_pretty_title <- function(path) {
  x <- tools::file_path_sans_ext(basename(path))
  x <- sub("^[0-9]+_", "", x)
  x <- sub("^v2_", "V2 · ", x)
  x <- gsub("_", " ", x, fixed = TRUE)
  trimws(x)
}

.ghs_kind <- function(name) {
  n <- tolower(name)
  if (grepl("map|choropleth|bivariate|world", n)) return("地图")
  if (grepl("heatmap|ridges|density|box|violin|distribution", n)) return("分布")
  if (grepl("forecast|beta|pca|cluster|dea|elasticity|model", n)) return("模型")
  if (grepl("slope|bump|stream|area|timeseries|line|covid", n)) return("时间")
  if (grepl("sankey|ternary|radar|waffle|treemap|network", n)) return("结构")
  "综合"
}

.ghs_kpi_card <- function(value, label, note = "") {
  sprintf(
    "<article class='kpi'><div class='kpi-value'>%s</div><div class='kpi-label'>%s</div><div class='kpi-note'>%s</div></article>",
    .ghs_html_escape(value), .ghs_html_escape(label), .ghs_html_escape(note)
  )
}

.ghs_figure_card <- function(path) {
  title <- .ghs_pretty_title(path)
  kind <- .ghs_kind(title)
  src <- paste0("data:image/png;base64,", .ghs_b64(path))
  sprintf(
    paste0(
      "<article class='figure-card' data-kind='%s'>",
      "<button class='figure-button' type='button' onclick=\"openFigure(this)\" data-title='%s'>",
      "<img loading='lazy' src='%s' alt='%s'>",
      "</button>",
      "<div class='figure-meta'><span>%s</span><strong>%s</strong><em>%s</em></div>",
      "</article>"
    ),
    .ghs_html_escape(kind), .ghs_html_escape(title), src, .ghs_html_escape(title),
    .ghs_html_escape(kind), .ghs_html_escape(title), .ghs_file_size(path)
  )
}

.ghs_widget_card <- function(path, idx) {
  id <- paste0("w", idx)
  title <- .ghs_pretty_title(path)
  kind <- .ghs_kind(title)
  b64 <- .ghs_b64(path)
  sprintf(
    paste0(
      "<article class='widget-card'>",
      "<div><span class='pill'>%s</span><h3>%s</h3><p>%s · standalone HTML</p></div>",
      "<button type='button' onclick=\"loadWidget('%s','%s')\">加载交互</button>",
      "<script type='application/octet-stream' id='widget-data-%s'>%s</script>",
      "</article>"
    ),
    .ghs_html_escape(kind), .ghs_html_escape(title), .ghs_file_size(path),
    id, .ghs_html_escape(title), id, b64
  )
}

.ghs_write_submission_rmd <- function(path) {
  lines <- c(
    "---",
    "title: \"全球卫生支出自由分析：完整静态展示版\"",
    "subtitle: \"Global Health Spending 2000–2023 · 可转发 HTML 总成品\"",
    "author: \"庄颂（20241334）\"",
    "output:",
    "  html_document:",
    "    self_contained: true",
    "    toc: true",
    "    toc_float: true",
    "    theme: flatly",
    "---",
    "",
    "```{r setup, include=FALSE}",
    "knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)",
    "root <- normalizePath(file.path(dirname(knitr::current_input(dir = TRUE)), '..'), mustWork = FALSE)",
    "for (f in list.files(file.path(root, '程序'), pattern = '\\\\.R$', full.names = TRUE)) source(f, encoding = 'UTF-8')",
    "generate_static_showcase(root = root)",
    "```",
    "",
    "本 Rmd 是提交版 HTML 的可复现入口。运行上方 chunk 会生成：",
    "",
    "- `课程提交/庄颂_20241334.html`",
    "- `课程提交/庄颂_20241334_完整静态展示.html`",
    "- `网站发布/index.html`",
    "- `网站发布/完整静态展示.html`",
    "",
    "最终可直接转发的文件是 `课程提交/庄颂_20241334_完整静态展示.html`。"
  )
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, path, useBytes = TRUE)
  invisible(path)
}


.ghs_widget_card_link <- function(path, idx, href) {
  title <- .ghs_pretty_title(path)
  kind <- .ghs_kind(title)
  sprintf(
    paste0(
      "<article class='widget-card'>",
      "<div><span class='pill'>%s</span><h3>%s</h3><p>%s · standalone HTML</p></div>",
      "<button type='button' onclick=\"loadWidgetUrl('%s','%s')\">加载交互</button>",
      "</article>"
    ),
    .ghs_html_escape(kind), .ghs_html_escape(title), .ghs_file_size(path),
    .ghs_html_escape(href), .ghs_html_escape(title)
  )
}

.ghs_build_showcase_html <- function(master, figures, widget_cards,
                                     subtitle, transfer_note) {
  cur_year <- max(master$year, na.rm = TRUE)
  base_year <- min(master$year, na.rm = TRUE)
  cur <- master[master$year == cur_year, ]
  base <- master[master$year == base_year, ]
  global_che <- sum(cur$che_usd2023, na.rm = TRUE)
  base_che <- sum(base$che_usd2023, na.rm = TRUE)
  growth <- if (is.finite(global_che) && is.finite(base_che) && base_che > 0) {
    (global_che / base_che)^(1 / (cur_year - base_year)) - 1
  } else NA_real_
  kpis <- paste0(
    .ghs_kpi_card(.ghs_fmt_num(dplyr::n_distinct(master$iso3_code), 0), "国家/地区", "覆盖 2000–2023 全球面板"),
    .ghs_kpi_card(paste0(base_year, "–", cur_year), "时间跨度", "GHED 长时段趋势"),
    .ghs_kpi_card(.ghs_fmt_money(global_che), paste0(cur_year, " 年 CHE"), "USD 2023 口径"),
    .ghs_kpi_card(.ghs_fmt_num(growth * 100, 1, "%"), "年化增长", "全球 CHE 总额"),
    .ghs_kpi_card(.ghs_fmt_num(mean(cur$hf3_che, na.rm = TRUE), 1, "%"), "平均 OOPS", "居民自付占 CHE"),
    .ghs_kpi_card(length(figures), "静态图表", "PNG + SVG 图表库"),
    .ghs_kpi_card(length(widget_cards), "交互组件", "Plotly / Leaflet / Reactable / DT"),
    .ghs_kpi_card("12", "Shiny 模块", "总览、国家画像、地图、情景等")
  )
  fig_cards <- paste(vapply(figures, .ghs_figure_card, character(1)), collapse = "\n")
  figure_tabs <- paste(vapply(c("全部", "地图", "分布", "模型", "时间", "结构", "综合"), function(x) {
    sprintf("<button type='button' onclick=\"filterFigures('%s', this)\"%s>%s</button>",
            x, if (x == "全部") " class='active'" else "", x)
  }, character(1)), collapse = "")
  css <- paste(c(
    ":root{--ink:#12131a;--muted:#626675;--paper:#f6efe5;--line:rgba(18,19,26,.12);--blue:#163f63;--orange:#c46b27;--green:#2a9d8f;--shadow:0 24px 70px rgba(18,19,26,.16)}",
    "*{box-sizing:border-box}html{scroll-behavior:smooth}body{margin:0;background:radial-gradient(circle at 10% 0,#fff8e9 0,#f6efe5 34%,#ece3d8 100%);color:var(--ink);font-family:Inter,'Segoe UI','Microsoft YaHei',system-ui,sans-serif;line-height:1.65}.wrap{width:min(1180px,92vw);margin:auto}",
    ".hero{min-height:84vh;padding:34px 0 70px;background:linear-gradient(135deg,#111827 0,#153a5b 48%,#7a3d19 100%);color:#fff;position:relative;overflow:hidden}.hero:before{content:'';position:absolute;inset:-20%;background:radial-gradient(circle at 80% 20%,rgba(255,255,255,.22),transparent 26%),radial-gradient(circle at 20% 80%,rgba(232,159,98,.30),transparent 30%);filter:blur(6px)}.nav,.hero-inner{position:relative}.nav{display:flex;gap:14px;justify-content:space-between;align-items:center}.brand{font-weight:800;letter-spacing:.08em}.links a{color:#fff;text-decoration:none;margin-left:16px;opacity:.86}",
    ".hero-grid{display:grid;grid-template-columns:1.15fr .85fr;gap:44px;align-items:end;padding-top:72px}.eyebrow{display:inline-flex;border:1px solid rgba(255,255,255,.25);border-radius:999px;padding:6px 12px;background:rgba(255,255,255,.08);backdrop-filter:blur(12px);font-size:13px}.hero h1{font-family:Georgia,'Noto Serif SC',serif;font-size:clamp(44px,7vw,88px);line-height:.95;margin:22px 0 18px;letter-spacing:-.06em}.hero p{font-size:19px;color:rgba(255,255,255,.84);max-width:760px}.hero-actions{display:flex;flex-wrap:wrap;gap:12px;margin-top:28px}",
    ".btn{border:0;border-radius:999px;padding:12px 18px;background:#fff;color:#142033;font-weight:800;text-decoration:none;box-shadow:0 12px 28px rgba(0,0,0,.2)}.btn.alt{background:rgba(255,255,255,.12);color:#fff;border:1px solid rgba(255,255,255,.26)}.hero-panel{border:1px solid rgba(255,255,255,.2);background:rgba(255,255,255,.10);border-radius:28px;padding:24px;backdrop-filter:blur(18px);box-shadow:0 22px 80px rgba(0,0,0,.22)}.hero-panel strong{font-size:48px;display:block;line-height:1}",
    ".section{padding:76px 0}.section h2{font-family:Georgia,'Noto Serif SC',serif;font-size:clamp(32px,4.6vw,56px);line-height:1.05;margin:0 0 14px;letter-spacing:-.04em}.lead{font-size:18px;color:var(--muted);max-width:860px}.kpi-grid{display:grid;grid-template-columns:repeat(4,1fr);gap:16px;margin-top:26px}.kpi{background:linear-gradient(180deg,#fffdf8,#fff7ea);border:1px solid var(--line);border-radius:22px;padding:22px;box-shadow:0 14px 36px rgba(18,19,26,.08)}.kpi-value{font-family:Georgia,serif;font-size:34px;font-weight:800;color:var(--blue);line-height:1}.kpi-label{font-weight:800;margin-top:10px}.kpi-note{font-size:13px;color:var(--muted);margin-top:4px}",
    ".story-grid{display:grid;grid-template-columns:repeat(3,1fr);gap:18px;margin-top:28px}.story{background:#fff;border:1px solid var(--line);border-radius:24px;padding:24px;box-shadow:0 16px 44px rgba(18,19,26,.08)}.story b{display:block;color:var(--orange);font-size:13px;letter-spacing:.08em;text-transform:uppercase}.tabs{display:flex;flex-wrap:wrap;gap:10px;margin:26px 0}.tabs button{border:1px solid var(--line);background:#fff7ea;border-radius:999px;padding:9px 14px;font-weight:800;color:var(--ink);cursor:pointer}.tabs button.active{background:var(--ink);color:#fff}",
    ".gallery{display:grid;grid-template-columns:repeat(3,1fr);gap:18px}.figure-card{background:#fff;border:1px solid var(--line);border-radius:24px;overflow:hidden;box-shadow:0 18px 46px rgba(18,19,26,.10)}.figure-button{border:0;background:#fff;padding:0;cursor:zoom-in;width:100%}.figure-card img{display:block;width:100%;aspect-ratio:16/10;object-fit:contain;background:#fffaf2}.figure-meta{padding:14px 16px 18px}.figure-meta span,.pill{display:inline-flex;background:#f0e3d0;color:#774314;border-radius:999px;padding:4px 9px;font-size:12px;font-weight:900}.figure-meta strong{display:block;margin-top:9px}.figure-meta em{display:block;color:var(--muted);font-style:normal;font-size:12px}",
    ".widget-lab{display:grid;grid-template-columns:.92fr 1.08fr;gap:22px;align-items:start}.widget-list{display:grid;gap:12px;max-height:760px;overflow:auto;padding-right:6px}.widget-card{background:#fff;border:1px solid var(--line);border-radius:20px;padding:16px;display:flex;justify-content:space-between;gap:14px;align-items:center;box-shadow:0 10px 28px rgba(18,19,26,.07)}.widget-card h3{margin:8px 0 0;font-size:16px}.widget-card p{margin:3px 0 0;color:var(--muted);font-size:12px}.widget-card button{border:0;border-radius:999px;background:var(--blue);color:#fff;padding:10px 13px;font-weight:900;cursor:pointer;white-space:nowrap}",
    ".viewer{position:sticky;top:18px;background:#111827;border-radius:24px;padding:14px;box-shadow:var(--shadow)}.viewer-head{color:#fff;display:flex;justify-content:space-between;align-items:center;padding:6px 8px 12px}.viewer iframe{width:100%;height:680px;border:0;border-radius:16px;background:#fff}.command{background:#111827;color:#e8f2ff;border-radius:20px;padding:18px;overflow:auto;border:1px solid rgba(255,255,255,.12)}.deliverables{display:grid;grid-template-columns:repeat(2,1fr);gap:18px}.deliverable{border:1px solid var(--line);background:#fff;border-radius:24px;padding:22px}.footer{padding:44px 0 70px;color:#fff;background:#111827}",
    ".modal{position:fixed;inset:0;background:rgba(0,0,0,.82);display:none;z-index:20;padding:28px}.modal.open{display:grid;place-items:center}.modal img{max-width:96vw;max-height:86vh;background:#fff;border-radius:16px}.modal button{position:absolute;right:24px;top:20px;border:0;border-radius:999px;padding:10px 14px;font-weight:900}.modal-title{position:absolute;left:28px;top:20px;color:#fff;font-weight:900}@media(max-width:980px){.hero-grid,.widget-lab,.deliverables{grid-template-columns:1fr}.kpi-grid,.story-grid,.gallery{grid-template-columns:repeat(2,1fr)}.viewer{position:static}}@media(max-width:640px){.kpi-grid,.story-grid,.gallery{grid-template-columns:1fr}.links{display:none}.hero{min-height:auto}.hero-grid{padding-top:42px}}"
  ), collapse = "\n")
  sprintf(
    paste0(
      "<!doctype html><html lang='zh-CN'><head><meta charset='utf-8'><meta name='viewport' content='width=device-width, initial-scale=1'>",
      "<title>全球卫生支出完整静态展示版 · 庄颂 20241334</title><style>%s</style></head><body>",
      "<header class='hero'><div class='wrap nav'><div class='brand'>GHS · 2000–2023</div><nav class='links'><a href='#findings'>发现</a><a href='#figures'>图表</a><a href='#widgets'>交互</a><a href='#deploy'>部署</a></nav></div>",
      "<div class='wrap hero-inner hero-grid'><div><span class='eyebrow'>%s</span><h1>全球卫生支出如何重塑公平、韧性与未来？</h1><p>本页面整合 GHED 2000–2023 数据工程、统计建模、静态图、交互组件、Shiny 仪表盘与 GitHub Pages 部署入口，是课程提交与对外展示的最终总成品。</p><div class='hero-actions'><a class='btn' href='#figures'>浏览图表库</a><a class='btn alt' href='#widgets'>打开交互实验室</a><a class='btn alt' href='https://github.com/2711944586/R'>GitHub 仓库</a></div></div>",
      "<aside class='hero-panel'><span>Final integrated deliverable</span><strong>%s</strong><p>%s</p></aside></div></header><main>",
      "<section class='section' id='findings'><div class='wrap'><h2>一页读懂：从数据到可交付产品</h2><p class='lead'>这不是原始作业版，而是把数据清洗、模型、图表、交互、Shiny、Quarto 与部署全部整合后的展示版。</p><div class='kpi-grid'>%s</div><div class='story-grid'><article class='story'><b>公平 Equity</b><h3>OOPS 仍是财务风险的核心信号</h3><p>高自付国家集中在南亚、中亚和撒哈拉以南非洲。政府/强制筹资份额越高，居民自付负担通常越低。</p></article><article class='story'><b>韧性 Resilience</b><h3>COVID 期间政府支出承担托底角色</h3><p>2020–2022 年总卫生支出抬升，但不同国家的 OOPS 变化方向并不一致，说明危机中的保护机制差异明显。</p></article><article class='story'><b>未来 Futures</b><h3>效率前沿和预防赤字决定长期回报</h3><p>人均支出越高并不自动带来更高健康产出；预防支出长期偏低，是下一轮政策优化重点。</p></article></div></div></section>",
      "<section class='section' id='figures'><div class='wrap'><h2>静态图表库：类型完整、风格统一</h2><p class='lead'>地图、山脊图、热力图、坡度图、哑铃图、PCA、β 收敛、预测扇形图、Sankey、Waffle、Treemap、Small multiples 等统一整合。点击任意图可放大查看。</p><div class='tabs'>%s</div><div class='gallery' id='gallery'>%s</div></div></section>",
      "<section class='section' id='widgets'><div class='wrap'><h2>交互实验室：standalone HTML 组件</h2><p class='lead'>点击“加载交互”后在右侧打开，避免页面初始加载过慢。</p><div class='widget-lab'><div class='widget-list'>%s</div><div class='viewer'><div class='viewer-head'><strong id='viewer-title'>选择一个交互组件</strong><span>standalone widget</span></div><iframe id='widget-frame' title='interactive widget'></iframe></div></div></div></section>",
      "<section class='section' id='deploy'><div class='wrap'><h2>Shiny、GitHub 静态部署与转发方法</h2><p class='lead'>当前页面适合直接展示；完整目录站适合 GitHub Pages；Shiny/shinylive 适合深度交互。</p><div class='deliverables'><article class='deliverable'><h3>直接转发静态 HTML</h3><p>发送 `课程提交/庄颂_20241334_完整静态展示.html`。无需 R、无需服务器、无需项目目录即可打开。</p><div class='command'><code>Rscript 构建.R submission</code></div></article><article class='deliverable'><h3>GitHub Pages 静态站</h3><p>推送到 GitHub 后，在仓库 Settings → Pages 中选择 GitHub Actions。访问地址为 `https://2711944586.github.io/R/`。</p><div class='command'><code>git push -u origin main</code></div></article><article class='deliverable'><h3>Shiny 云端</h3><p>配置 `SHINYAPPS_NAME`、`SHINYAPPS_TOKEN`、`SHINYAPPS_SECRET` 后由 Actions 自动发布，或本地用 rsconnect 手动部署。</p><div class='command'><code>Rscript 启动仪表盘.R 4848</code></div></article><article class='deliverable'><h3>shinylive 浏览器版</h3><p>执行 `Rscript 构建.R shinylive` 生成 `网站发布/仪表盘/`，实现无服务器的浏览器内 Shiny。</p><div class='command'><code>Rscript 构建.R deploy</code></div></article></div></div></section>",
      "</main><footer class='footer'><div class='wrap'><strong>Global Health Spending · 庄颂 20241334</strong><p>数据来源：WHO Global Health Expenditure Database / TidyTuesday 2026-04-21。分析仅用于课程项目与数据新闻展示，不构成因果推断。</p></div></footer>",
      "<div class='modal' id='fig-modal' onclick='closeFigure()'><button type='button'>关闭</button><div class='modal-title' id='modal-title'></div><img id='modal-img' alt='figure preview'></div>",
      "<script>function filterFigures(kind,btn){document.querySelectorAll('.tabs button').forEach(b=>b.classList.remove('active'));btn.classList.add('active');document.querySelectorAll('.figure-card').forEach(card=>{card.style.display=(kind==='全部'||card.dataset.kind===kind)?'block':'none';});}function openFigure(btn){const img=btn.querySelector('img');document.getElementById('modal-img').src=img.src;document.getElementById('modal-title').textContent=btn.dataset.title||img.alt;document.getElementById('fig-modal').classList.add('open');}function closeFigure(){document.getElementById('fig-modal').classList.remove('open');}function loadWidget(id,title){const el=document.getElementById('widget-data-'+id);if(!el)return;document.getElementById('viewer-title').textContent=title;document.getElementById('widget-frame').src='data:text/html;base64,'+el.textContent.trim();document.getElementById('widget-frame').scrollIntoView({behavior:'smooth',block:'center'});}function loadWidgetUrl(url,title){document.getElementById('viewer-title').textContent=title;document.getElementById('widget-frame').src=url;document.getElementById('widget-frame').scrollIntoView({behavior:'smooth',block:'center'});}document.addEventListener('keydown',e=>{if(e.key==='Escape')closeFigure();});</script></body></html>"
    ),
    css, .ghs_html_escape(subtitle),
    .ghs_fmt_num(dplyr::n_distinct(master$iso3_code), 0),
    .ghs_html_escape(transfer_note),
    kpis, figure_tabs, fig_cards, paste(widget_cards, collapse = "\n")
  )
}

generate_static_showcase <- function(root = NULL,
                                     out_file = file.path(root, "课程提交", "庄颂_20241334.html"),
                                     share_file = file.path(root, "课程提交", "庄颂_20241334_完整静态展示.html"),
                                     publish_file = file.path(root, "网站发布", "index.html"),
                                     publish_copy = file.path(root, "网站发布", "完整静态展示.html"),
                                     write_rmd = TRUE) {
  if (is.null(root)) {
    root <- if (exists("proj_root", mode = "function")) proj_root() else getwd()
    out_file <- file.path(root, "课程提交", "庄颂_20241334.html")
    share_file <- file.path(root, "课程提交", "庄颂_20241334_完整静态展示.html")
    publish_file <- file.path(root, "网站发布", "index.html")
    publish_copy <- file.path(root, "网站发布", "完整静态展示.html")
  }
  root <- normalizePath(root, mustWork = FALSE)
  master_path <- file.path(root, "派生数据", "处理结果", "master_enriched.rds")
  if (!file.exists(master_path)) stop("Missing master cache: ", master_path)
  master <- readRDS(master_path)
  fig_dir <- file.path(root, "分析输出", "图表")
  widget_dir <- file.path(root, "分析输出", "交互组件")
  figures <- sort(list.files(fig_dir, pattern = "[.]png$", full.names = TRUE))
  widgets <- sort(list.files(widget_dir, pattern = "[.]html$", full.names = TRUE))
  if (!length(figures)) stop("No PNG figures found at: ", fig_dir)
  if (!length(widgets)) stop("No widget HTML files found at: ", widget_dir)
  widget_cards_share <- vapply(seq_along(widgets), function(i) {
    .ghs_widget_card(widgets[[i]], i)
  }, character(1))
  widget_cards_submit <- vapply(seq_along(widgets), function(i) {
    .ghs_widget_card_link(widgets[[i]], i, paste0("../分析输出/交互组件/", basename(widgets[[i]])))
  }, character(1))
  widget_cards_publish <- vapply(seq_along(widgets), function(i) {
    .ghs_widget_card_link(widgets[[i]], i, paste0("交互组件/", basename(widgets[[i]])))
  }, character(1))
  html_submit <- .ghs_build_showcase_html(
    master, figures, widget_cards_submit,
    subtitle = "完整目录展示版 · 适合项目目录内打开与课程提交",
    transfer_note = "图表内嵌，交互组件从项目目录按需加载。"
  )
  html_publish <- .ghs_build_showcase_html(
    master, figures, widget_cards_publish,
    subtitle = "GitHub Pages 展示版 · 适合公开访问",
    transfer_note = "静态站首页，交互组件从网站发布目录按需加载。"
  )
  html_share <- .ghs_build_showcase_html(
    master, figures, widget_cards_share,
    subtitle = "精装单文件版 · 可直接下载转发 · 可离线打开",
    transfer_note = "图表与交互组件全部嵌入一个 HTML 文件，因此体积较大。"
  )
  for (pair in list(c(out_file, html_submit), c(publish_file, html_publish),
                    c(publish_copy, html_publish), c(share_file, html_share))) {
    dir.create(dirname(pair[[1]]), recursive = TRUE, showWarnings = FALSE)
    writeLines(pair[[2]], pair[[1]], useBytes = TRUE)
  }
  if (isTRUE(write_rmd)) {
    .ghs_write_submission_rmd(file.path(root, "课程提交", "庄颂_20241334.Rmd"))
  }
  cat("[showcase] wrote integrated HTML:\n")
  cat("  - ", out_file, "\n", sep = "")
  cat("  - ", share_file, "\n", sep = "")
  cat("  - ", publish_file, "\n", sep = "")
  cat("  - ", publish_copy, "\n", sep = "")
  invisible(c(out_file, share_file, publish_file, publish_copy))
}
