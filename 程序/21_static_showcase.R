# =============================================================================
# 程序/21_static_showcase.R   ——   单一来源 · 单一标准 · 单一产物
# -----------------------------------------------------------------------------
# 输入：派生数据/处理结果/master_enriched.rds
#       分析输出/图表/*.png        分析输出/交互组件/*.html
#       分析输出/模型表/*.csv     程序/*.R（代码片段）
# 输出：
#   - 课程提交/庄颂_20241334.html   课程提交版（图表 base64，widget 相对路径）
#   - 网站发布/index.html           GitHub Pages 同源首页
# 不再生成"完整静态展示.html"等冗余产物。
# =============================================================================

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
}

# ---- 0. 工具：HTML 转义 / 格式化 / 代码读取 -------------------------------

.ghs_e <- function(x) {
  x <- as.character(x %||% "")
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x <- gsub("'", "&#39;", x, fixed = TRUE)
  x
}

.ghs_n <- function(x, d = 1, suf = "") {
  if (!length(x)) return("\u2014")
  vapply(x, function(xi) {
    if (!is.finite(xi)) return("\u2014")
    paste0(format(round(xi, d), big.mark = ",", nsmall = d), suf)
  }, character(1))
}

.ghs_m <- function(x) {
  if (!length(x) || !is.finite(x)) return("\u2014")
  if (abs(x) >= 1e12) return(sprintf("$%.2fT", x / 1e12))
  if (abs(x) >= 1e9)  return(sprintf("$%.2fB", x / 1e9))
  if (abs(x) >= 1e6)  return(sprintf("$%.2fM", x / 1e6))
  sprintf("$%s", format(round(x), big.mark = ","))
}

.ghs_size <- function(p) {
  if (!file.exists(p)) return("\u2014")
  mb <- file.info(p)$size / 1024^2
  if (mb >= 1) sprintf("%.1f MB", mb) else sprintf("%.0f KB", mb * 1024)
}

.ghs_b64 <- function(p, mime = NULL) {
  if (!requireNamespace("base64enc", quietly = TRUE)) stop("Need 'base64enc'.")
  if (is.null(mime)) {
    ext <- tolower(tools::file_ext(p))
    mime <- switch(ext, png = "image/png", svg = "image/svg+xml",
                   jpg = "image/jpeg", jpeg = "image/jpeg",
                   "application/octet-stream")
  }
  paste0("data:", mime, ";base64,", base64enc::base64encode(p))
}

.ghs_pretty <- function(p) {
  x <- tools::file_path_sans_ext(basename(p))
  x <- sub("^[0-9]+_", "", x)
  x <- sub("^v2_w_", "", x)
  x <- sub("^v2_", "", x)
  x <- gsub("_", " ", x, fixed = TRUE)
  trimws(x)
}

.ghs_kind <- function(name) {
  n <- tolower(name)
  if (grepl("map|choropleth|bivariate|world|atlas|leaflet", n)) return("\u5730\u56fe")
  if (grepl("ridges|density|box|violin|heatmap", n)) return("\u5206\u5e03")
  if (grepl("forecast|beta|pca|cluster|dea|elasticity|panel|fe|model|scenarios|mc_fan|splom", n)) return("\u6a21\u578b")
  if (grepl("slope|bump|stream|area|timeseries|highlight|line|covid|gapminder|race|ts", n)) return("\u65f6\u95f4")
  if (grepl("sankey|ternary|radar|waffle|treemap|network", n)) return("\u7ed3\u6784")
  if (grepl("rank|reactable|dt_atlas|table|lollipop|inequality|equity|fiscal|share", n)) return("\u6307\u6807")
  "\u7efc\u5408"
}

.ghs_read_code <- function(file, from = NULL, to = NULL) {
  if (!file.exists(file)) return("# (code unavailable)")
  lns <- readLines(file, warn = FALSE, encoding = "UTF-8")
  if (!is.null(from) || !is.null(to)) {
    from <- from %||% 1L; to <- to %||% length(lns)
    lns <- lns[seq.int(max(1L, from), min(length(lns), to))]
  }
  while (length(lns) && !nzchar(trimws(lns[[1]]))) lns <- lns[-1]
  while (length(lns) && !nzchar(trimws(lns[[length(lns)]]))) lns <- lns[-length(lns)]
  paste(lns, collapse = "\n")
}

.ghs_code <- function(code, lang = "r", cap = NULL) {
  body <- .ghs_e(code)
  cap_html <- if (length(cap) && nzchar(cap))
    sprintf("<figcaption class='code-caption'>%s</figcaption>", .ghs_e(cap)) else ""
  sprintf("<figure class='code-figure'>%s<pre class='code-pre'><code class='language-%s'>%s</code></pre></figure>",
          cap_html, .ghs_e(lang), body)
}

.ghs_para <- function(...) {
  parts <- c(...); parts <- parts[nzchar(parts)]
  if (!length(parts)) return("")
  paste0("<p>", parts, "</p>", collapse = "\n")
}

# ---- 1. 摘要数值计算 -----------------------------------------------------

.ghs_summary <- function(master) {
  mst <- master[is.finite(master$year), ]
  cur <- max(mst$year, na.rm = TRUE); base <- min(mst$year, na.rm = TRUE)
  d_cur <- mst[mst$year == cur, ]; d_base <- mst[mst$year == base, ]
  che_cur  <- sum(d_cur$che_usd2023, na.rm = TRUE)
  che_base <- sum(d_base$che_usd2023, na.rm = TRUE)
  span <- max(1L, cur - base)
  list(
    n_country = length(unique(mst$iso3_code)),
    base_year = base, cur_year = cur, span = span,
    che_total_cur = che_cur, che_total_base = che_base,
    che_total_growth = (che_cur / che_base)^(1 / span) - 1,
    che_pc_cur  = stats::weighted.mean(d_cur$che_pc_usd2023, d_cur$pop, na.rm = TRUE),
    che_pc_base = stats::weighted.mean(d_base$che_pc_usd2023, d_base$pop, na.rm = TRUE),
    oops_mean_cur  = mean(d_cur$hf3_che, na.rm = TRUE),
    oops_mean_base = mean(d_base$hf3_che, na.rm = TRUE),
    oops_high_cur  = sum(d_cur$hf3_che > 50, na.rm = TRUE),
    gghed_mean_cur = mean(d_cur$gghed_che, na.rm = TRUE),
    pvtd_mean_cur  = mean(d_cur$pvtd_che, na.rm = TRUE),
    ext_mean_cur   = mean(d_cur$ext_che,  na.rm = TRUE)
  )
}

.ghs_top_oops <- function(master, n = 8, asc = FALSE) {
  yr <- max(master$year, na.rm = TRUE)
  d <- master[master$year == yr & is.finite(master$hf3_che), ]
  d <- d[, c("country_name", "iso3_code", "continent", "hf3_che", "che_pc_usd2023")]
  d <- d[order(d$hf3_che, decreasing = !asc), ]
  utils::head(d, n)
}

.ghs_covid_top <- function(master, n = 8) {
  d <- master[master$year %in% 2019:2022 & is.finite(master$che_usd2023), ]
  if (!nrow(d)) return(NULL)
  base <- stats::aggregate(che_usd2023 ~ iso3_code + country_name,
                           data = d[d$year == 2019, , drop = FALSE], FUN = mean)
  shock <- stats::aggregate(che_usd2023 ~ iso3_code + country_name,
                            data = d[d$year %in% 2020:2022, , drop = FALSE], FUN = mean)
  m <- merge(base, shock, by = c("iso3_code", "country_name"),
             suffixes = c("_base", "_shock"))
  m$delta_pct <- (m$che_usd2023_shock - m$che_usd2023_base) /
                  pmax(m$che_usd2023_base, 1) * 100
  m <- m[is.finite(m$delta_pct), ]
  m <- m[order(m$delta_pct, decreasing = TRUE), ]
  utils::head(m[, c("country_name", "iso3_code",
                    "che_usd2023_base", "che_usd2023_shock", "delta_pct")], n)
}

# ---- 2. HTML 原子组件 ----------------------------------------------------

.ghs_kpi <- function(v, l, n = "") {
  sprintf("<article class='kpi'><div class='kpi-value'>%s</div><div class='kpi-label'>%s</div><div class='kpi-note'>%s</div></article>",
          .ghs_e(v), .ghs_e(l), .ghs_e(n))
}

.ghs_chip <- function(l, v, tone = "ink") {
  sprintf("<span class='chip chip-%s'><b>%s</b><i>%s</i></span>",
          .ghs_e(tone), .ghs_e(l), .ghs_e(v))
}

.ghs_fig <- function(path, cap, kicker = NULL, span = "full") {
  src <- .ghs_b64(path)
  hd <- if (length(kicker) && nzchar(kicker))
    sprintf("<span class='fig-kicker'>%s</span>", .ghs_e(kicker)) else ""
  sprintf("<figure class='fig-inline fig-span-%s'><div class='fig-frame'><img src='%s' alt='%s' loading='lazy'></div><figcaption>%s<strong>%s</strong></figcaption></figure>",
          .ghs_e(span), src, .ghs_e(.ghs_pretty(path)), hd, .ghs_e(cap))
}

.ghs_callout <- function(title, body, tone = "ink") {
  sprintf("<aside class='callout callout-%s'><strong>%s</strong><div>%s</div></aside>",
          .ghs_e(tone), .ghs_e(title), body)
}

.ghs_method <- function(html_body) {
  sprintf("<div class='method-block'>%s</div>", html_body)
}

.ghs_table <- function(df, cap = NULL, max_rows = 10, digits = 2) {
  df <- utils::head(df, max_rows); cols <- names(df)
  thead <- paste0("<th>", .ghs_e(cols), "</th>", collapse = "")
  rows <- vapply(seq_len(nrow(df)), function(i) {
    cells <- vapply(cols, function(c) {
      v <- df[[c]][[i]]
      if (is.numeric(v)) {
        if (!is.finite(v)) "\u2014"
        else if (abs(v) >= 1e6) .ghs_m(v)
        else if (abs(v) >= 1) .ghs_n(v, digits)
        else .ghs_n(v, digits + 2)
      } else .ghs_e(as.character(v))
    }, character(1))
    paste0("<tr><td>", paste(cells, collapse = "</td><td>"), "</td></tr>")
  }, character(1))
  cap_html <- if (length(cap) && nzchar(cap))
    sprintf("<caption>%s</caption>", .ghs_e(cap)) else ""
  sprintf("<div class='table-wrap'><table>%s<thead><tr>%s</tr></thead><tbody>%s</tbody></table></div>",
          cap_html, thead, paste(rows, collapse = ""))
}

.ghs_finding <- function(id, num, kicker, title, lead, body, chips = "") {
  sprintf("<section class='finding' id='%s'><div class='wrap'><header class='finding-head'><span class='finding-num'>%s</span><div><span class='finding-kicker'>%s</span><h2>%s</h2><p class='lead'>%s</p><div class='chips'>%s</div></div></header><div class='finding-body'>%s</div></div></section>",
          .ghs_e(id), .ghs_e(num), .ghs_e(kicker), .ghs_e(title),
          .ghs_e(lead), chips, body)
}

# ---- 3. Hero / KPI / 数据与方法 ------------------------------------------

.ghs_hero <- function(s, project_url, n_fig, n_widget) {
  sprintf(
    paste0(
      "<header class='hero' id='top'><div class='wrap nav'>",
      "<a class='brand' href='#top'>GHS \u00b7 2000\u20132023</a>",
      "<nav class='links'><a href='#methods'>\u65b9\u6cd5</a>",
      "<a href='#findings'>\u53d1\u73b0</a>",
      "<a href='#gallery'>\u56fe\u8868</a>",
      "<a href='#widgets'>\u4ea4\u4e92</a>",
      "<a href='#repro'>\u590d\u73b0</a>",
      "<a href='#conclusion'>\u7ed3\u8bba</a></nav></div>",
      "<div class='wrap hero-grid'>",
      "<div><span class='eyebrow'>Final integrated deliverable \u00b7 \u5355\u4e00\u6765\u6e90 \u00b7 \u5355\u4e00\u4ea7\u7269</span>",
      "<h1>\u5168\u7403\u536b\u751f\u652f\u51fa 2000\u20132023\uff1a",
      "<span class='accent'>\u516c\u5e73\u3001\u97e7\u6027\u3001\u672a\u6765</span></h1>",
      "<p class='hero-lead'>\u672c\u9875\u6574\u5408 GHED + WDI \u957f\u9762\u677f\u7684\u6570\u636e\u5de5\u7a0b\u3001\u7edf\u8ba1\u5efa\u6a21\u3001\u53ef\u89c6\u5316\u4e0e\u90e8\u7f72\uff0c\u4ee5 8 \u9879\u6838\u5fc3\u53d1\u73b0\u7684\u5f62\u5f0f\u7cfb\u7edf\u547c\u73b0\uff1a\u6bcf\u4e2a\u53d1\u73b0\u914d\u6709\u7814\u7a76\u95ee\u9898\u3001\u65b9\u6cd5\u3001\u53ef\u6267\u884c R \u4ee3\u7801\u3001\u539f\u56fe\u4e0e\u6df1\u5ea6\u89e3\u8bfb\u3002</p>",
      "<div class='hero-actions'>",
      "<a class='btn' href='#findings'>\u5f00\u59cb\u9605\u8bfb 8 \u9879\u53d1\u73b0</a>",
      "<a class='btn alt' href='%s'>GitHub \u4ed3\u5e93</a>",
      "<a class='btn ghost' href='./\u4eea\u8868\u76d8/'>\u6d4f\u89c8\u5668\u5185 Shiny</a></div></div>",
      "<aside class='hero-panel'><span class='hero-panel-tag'>\u9875\u9762\u6784\u6210</span>",
      "<ul class='hero-panel-list'>",
      "<li><b>%d</b><span>\u56fd\u5bb6/\u5730\u533a\u9762\u677f</span></li>",
      "<li><b>%d</b><span>\u9759\u6001\u56fe\u8868</span></li>",
      "<li><b>%d</b><span>\u4ea4\u4e92\u7ec4\u4ef6</span></li>",
      "<li><b>8</b><span>\u6df1\u5ea6\u53d1\u73b0 + \u5b8c\u6574 R \u4ee3\u7801</span></li>",
      "</ul></aside></div></header>"
    ),
    .ghs_e(project_url), s$n_country, n_fig, n_widget
  )
}

.ghs_kpi_grid <- function(s, n_fig, n_widget) {
  cards <- paste0(
    .ghs_kpi(.ghs_n(s$n_country, 0), "\u56fd\u5bb6/\u5730\u533a",
             paste0(s$base_year, "\u2013", s$cur_year, " \u5168\u7403\u9762\u677f")),
    .ghs_kpi(paste0(s$base_year, "\u2013", s$cur_year), "\u65f6\u95f4\u8de8\u5ea6",
             paste0(s$span, " \u5e74\u957f\u9762\u677f\uff1b\u8986\u76d6 COVID-19")),
    .ghs_kpi(.ghs_m(s$che_total_cur),
             paste0(s$cur_year, " \u5e74\u5168\u7403 CHE"),
             paste0("USD 2023\uff1b", s$base_year, " \u5e74\u4e3a ", .ghs_m(s$che_total_base))),
    .ghs_kpi(.ghs_n(s$che_total_growth * 100, 2, "%"),
             "CHE \u5e74\u5316\u589e\u901f",
             paste0(s$base_year, "\u2013", s$cur_year, " \u590d\u5408\u589e\u957f\u7387")),
    .ghs_kpi(.ghs_n(s$oops_mean_cur, 1, "%"),
             paste0(s$cur_year, " \u5e74 OOPS \u5747\u503c"),
             paste0(s$base_year, " \u5e74\u4e3a ",
                    .ghs_n(s$oops_mean_base, 1, "%"))),
    .ghs_kpi(.ghs_n(s$oops_high_cur, 0),
             "OOPS > 50% \u56fd\u5bb6",
             paste0("\u5c45\u6c11\u81ea\u4ed8\u5360 CHE \u8d85 50%\uff1b", s$cur_year)),
    .ghs_kpi(.ghs_n(n_fig, 0), "\u9759\u6001\u56fe\u8868",
             "ggplot2 + v2 \u5347\u7ea7"),
    .ghs_kpi(.ghs_n(n_widget, 0), "\u4ea4\u4e92\u7ec4\u4ef6",
             "plotly / leaflet / reactable / DT")
  )
  sprintf("<section class='section kpi-section' id='kpi'><div class='wrap'><header class='section-head'><span class='kicker'>01 \u00b7 Snapshot</span><h2>\u4e00\u9875\u5927\u5c40\uff1a\u6574\u4f53\u7ed3\u679c</h2><p class='lead'>\u4ee5\u4e0b 8 \u5f20 KPI \u5361\u7247\u5c06\u90e8\u5206\u6838\u5fc3\u6570\u503c\u5148\u884c\u5448\u73b0\uff1b\u5404\u6307\u6807\u7684\u65b9\u6cd5\u4e0e\u4ee3\u7801\u5728\u4e0b\u6587 8 \u9879\u53d1\u73b0\u4e2d\u5c55\u5f00\u3002</p></header><div class='kpi-grid'>%s</div></div></section>",
          cards)
}

.ghs_methods_section <- function(programs_dir) {
  io_code <- .ghs_read_code(file.path(programs_dir, "01_io.R"), 1, 70)
  clean_code <- .ghs_read_code(file.path(programs_dir, "02_clean.R"), 17, 65)
  enrich_code <- .ghs_read_code(file.path(programs_dir, "03_enrich.R"), 17, 38)
  metrics_code <- .ghs_read_code(file.path(programs_dir, "04_metrics.R"), 12, 52)
  body <- paste0(
    .ghs_para(
      "\u672c\u7814\u7a76\u7684\u56e0\u53d8\u91cf\u662f <em>\u6bcf\u56fd\u6bcf\u5e74\u7684\u536b\u751f\u652f\u51fa</em>\uff0c\u6838\u5fc3\u9762\u677f\u5305\u542b\u5f53\u5e74\u4ef7\u4e0e USD 2023 \u4e0d\u53d8\u4ef7\u7684 <b>CHE</b>\u3001\u4eba\u5747 CHE\u3001\u6309\u7b79\u8d44\u6765\u6e90\uff08\u653f\u5e9c\u5f3a\u5236 <b>gghed</b>\u3001\u79c1\u4eba <b>pvtd</b>\u3001\u5916\u63f4 <b>ext</b>\uff09\u548c\u7b79\u8d44\u65b9\u6848\uff08<b>hf1\u2013hfnec</b>\uff09\u7684\u5360\u6bd4\uff0c\u4ee5\u53ca\u6765\u81ea World Bank \u7684\u4eba\u53e3\u3001GDP/cap\u3001\u9884\u671f\u5bff\u547d\u4e0e U5MR\u3002",
      "\u6570\u636e\u6765\u6e90\uff1a<a href='https://apps.who.int/nha/database' target='_blank' rel='noreferrer'>WHO GHED</a> + WDI\uff1b\u5904\u7406\u540e\u7edf\u4e00\u4fdd\u5b58\u5728 <code>\u6d3e\u751f\u6570\u636e/\u5904\u7406\u7ed3\u679c/master_enriched.rds</code>\u3002\u6240\u6709\u91d1\u989d\u53d8\u91cf\u7edf\u4e00\u91c7\u7528 <b>USD 2023 \u4e0d\u53d8\u4ef7</b>\uff0c\u6240\u6709\u7ed3\u6784\u53d8\u91cf\u7edf\u4e00\u91c7\u7528 <b>% of CHE</b>\u3002"
    ),
    .ghs_code(io_code, "r", "01_io.R \u00b7 \u6570\u636e\u8bfb\u5165\uff1a\u89e3\u6790 GHED \u4e09\u4e2a\u6570\u636e\u96c6"),
    .ghs_code(clean_code, "r", "02_clean.R \u00b7 \u6e05\u6d17\u4e0e\u900f\u89c6\uff1a\u957f\u8868 \u2192 \u5bbd\u8868"),
    .ghs_code(enrich_code, "r", "03_enrich.R \u00b7 \u56fd\u5bb6\u5143\u6570\u636e\u589e\u5f3a\uff08countrycode\uff09"),
    .ghs_para(
      "\u5bf9\u6240\u6709\u7528\u4e8e\u4e0d\u5e73\u7b49\u3001\u804a\u7c7b\u4e0e\u5efa\u6a21\u7684\u4eba\u5747 CHE\uff0c\u7edf\u4e00\u4f7f\u7528 <b>\u4eba\u53e3\u52a0\u6743</b>\uff0c\u907f\u514d\u5c0f\u56fd/\u5927\u56fd\u6743\u91cd\u5931\u8861\u3002"
    ),
    .ghs_code(metrics_code, "r", "04_metrics.R \u00b7 \u4e0d\u5e73\u7b49\u6307\u6570\uff1a\u52a0\u6743 Gini / Theil-T / Atkinson")
  )
  sprintf("<section class='section methods' id='methods'><div class='wrap'><header class='section-head'><span class='kicker'>02 \u00b7 Data &amp; Methods</span><h2>\u4ece\u539f\u59cb\u6570\u636e\u5230\u53ef\u5efa\u6a21\u9762\u677f</h2><p class='lead'>\u6240\u6709\u5206\u6790\u5171\u4eab\u540c\u4e00\u4efd master \u5bbd\u8868\uff1b\u4e0b\u6e38\u6a21\u578b\u4e0e\u56fe\u8868\u53ea\u8bfb\u53d6\u6b64\u7f13\u5b58\uff0c\u786e\u4fdd\u7ed3\u679c\u53ef\u590d\u73b0\u3002</p></header>%s</div></section>",
          body)
}

# ---- 4. 八项发现 ---------------------------------------------------------

.ghs_findings <- function(master, fig_dir, programs_dir, models_dir) {
  s <- .ghs_summary(master)

  ## ---- F1 全球长期趋势 ----
  f1_code <- .ghs_read_code(file.path(programs_dir, "07_plot_static.R"), 1, 60)
  f1_body <- paste0(
    .ghs_method(.ghs_para(
      "<b>\u7814\u7a76\u95ee\u9898\uff1a</b>2000\u20132023 \u5168\u7403\u536b\u751f\u652f\u51fa\u5728\u603b\u91cf\u4e0e\u7ed3\u6784\u4e0a\u53d1\u751f\u4e86\u600e\u6837\u7684\u6f14\u53d8\uff1f",
      sprintf("<b>\u65b9\u6cd5\uff1a</b>\u5bf9 master \u5bbd\u8868\u6309\u5e74\u4efd\u6c47\u603b\u4e09\u7c7b\u7b79\u8d44\u6765\u6e90\uff08GGHED \u00b7 PVTD \u00b7 EXT\uff09\u7684\u5168\u7403\u52a0\u603b\uff0c\u8fed\u52a0\u9762\u79ef\u56fe\u91cf\u5316\u957f\u671f\u4efd\u989d\u53d8\u5316\uff1b\u5168\u7403 %d\u2013%d \u5e74\u5316\u589e\u901f = (CHE<sub>%d</sub>/CHE<sub>%d</sub>)<sup>1/%d</sup>\u22121 = %s\u3002",
              s$base_year, s$cur_year, s$cur_year, s$base_year, s$span,
              .ghs_n(s$che_total_growth * 100, 2, "%"))
    )),
    .ghs_code(f1_code, "r", "07_plot_static.R \u00b7 \u6784\u5efa\u5168\u7403-\u5e74\u9762\u677f\u4e0e\u5806\u53e0\u9762\u79ef\u56fe"),
    .ghs_fig(file.path(fig_dir, "01_global_sources_area.png"),
             "\u5168\u7403\u536b\u751f\u652f\u51fa\u6765\u6e90\u7ed3\u6784 2000\u20132023\uff08USD 2023 \u4e0d\u53d8\u4ef7\uff09",
             "Figure 1A \u00b7 \u5168\u7403\u603b\u989d\u4e0e\u6765\u6e90"),
    .ghs_fig(file.path(fig_dir, "v2_continent_stream.png"),
             "\u6309\u5927\u6d32\u5206\u89e3\u7684 CHE \u6d41\u53d8\u56fe",
             "Figure 1B \u00b7 \u5927\u6d32\u5206\u89e3"),
    .ghs_callout("\u89e3\u8bfb \u00b7 \u4e09\u4e2a\u5206\u5c42\u4fe1\u53f7",
      .ghs_para(
        sprintf("<b>\u603b\u91cf\u5c42\uff1a</b>\u5168\u7403 CHE \u7531 %d \u5e74\u7684 %s \u589e\u957f\u81f3 %d \u5e74\u7684 %s\uff0c\u5e74\u5316 %s\uff1b\u4eba\u5747 CHE \u7531 %s \u5347\u81f3 %s\u3002",
                s$base_year, .ghs_m(s$che_total_base),
                s$cur_year, .ghs_m(s$che_total_cur),
                .ghs_n(s$che_total_growth * 100, 2, "%"),
                .ghs_m(s$che_pc_base), .ghs_m(s$che_pc_cur)),
        "<b>\u7ed3\u6784\u5c42\uff1a</b>\u653f\u5e9c\u5f3a\u5236\uff08GGHED\uff09\u4efd\u989d\u5728\u591a\u6570\u9ad8\u6536\u5165\u56fd\u5bb6\u4fdd\u6301\u4e0a\u5347\uff1b\u5916\u63f4\uff08EXT\uff09\u4efd\u989d\u5411\u6700\u8d2b\u56f0\u56fd\u5bb6\u96c6\u4e2d\u3002",
        "<b>\u52a8\u6001\u5c42\uff1a</b>2008\u20132010\u3001 2020\u20132022 \u4e24\u6b21\u51b2\u51fb\u5728\u66f2\u7ebf\u4e0a\u7559\u4e0b\u660e\u663e\u51f8\u8d77\uff0c\u4e0b\u6587 F4 \u5355\u72ec\u5206\u89e3\u3002"
      ), tone = "blue")
  )
  f1 <- .ghs_finding("f-trend", "F1", "Macro \u00b7 \u603b\u91cf\u4e0e\u7ed3\u6784",
    "\u5168\u7403\u957f\u671f\u8d8b\u52bf\uff1a\u6269\u5f20\u4f46\u5206\u5316",
    "\u4e8c\u5341\u591a\u5e74\u91cc\u5168\u7403\u536b\u751f\u652f\u51fa\u603b\u91cf\u7ffb\u500d\uff0c\u4f46\u589e\u957f\u52a8\u529b\u5728\u4e0d\u540c\u6536\u5165\u7ec4\u4e4b\u95f4\u9ad8\u5ea6\u4e0d\u5747\u8861\u3002",
    f1_body, chips = paste0(
      .ghs_chip("\u5e74\u5316\u589e\u901f", .ghs_n(s$che_total_growth * 100, 2, "%"), "blue"),
      .ghs_chip(sprintf("%d \u5e74\u4eba\u5747", s$cur_year), .ghs_m(s$che_pc_cur), "ink"),
      .ghs_chip(sprintf("%d \u5e74\u603b\u989d", s$cur_year), .ghs_m(s$che_total_cur), "orange")
    ))

  ## ---- F2 谁在付钱 ----
  top_oops <- .ghs_top_oops(master, n = 8, asc = FALSE)
  bot_oops <- .ghs_top_oops(master, n = 8, asc = TRUE)
  bot_oops <- bot_oops[order(bot_oops$hf3_che), ]
  top_show <- top_oops; bot_show <- bot_oops
  names(top_show) <- c("\u56fd\u5bb6/\u5730\u533a", "ISO3", "\u5927\u6d32",
                       "OOPS \u5360 CHE (%)", "\u4eba\u5747 CHE")
  names(bot_show) <- names(top_show)
  f2_code <- "library(dplyr)\nrank_oops <- master |>\n  dplyr::filter(year == max(year, na.rm = TRUE)) |>\n  dplyr::select(country_name, continent, hf3_che, che_pc_usd2023) |>\n  dplyr::arrange(dplyr::desc(hf3_che))\nutils::head(rank_oops, 10)"
  f2_body <- paste0(
    .ghs_method(.ghs_para(
      "<b>\u7814\u7a76\u95ee\u9898\uff1a</b>\u8c01\u5728\u4e3a\u536b\u751f\u652f\u51fa\u4e70\u5355\uff1f\u653f\u5e9c\u3001\u5c45\u6c11\u81ea\u4ed8\u3001\u79c1\u4eba\u4fdd\u9669\u4e0e\u56fd\u9645\u5916\u63f4\uff0c\u5404\u81ea\u627f\u62c5\u591a\u5c11\u98ce\u9669\uff1f",
      "<b>\u65b9\u6cd5\uff1a</b>\u4f7f\u7528 financing scheme \u7ef4\u5ea6\uff08hf1\u2013hfnec\uff09\u7684\u5360 CHE \u6bd4\u4f8b\u3002OOPS = hf3_che \u662f\u8861\u91cf\u8d22\u52a1\u4fdd\u62a4\u7684\u6838\u5fc3\u4fe1\u53f7\u3002"
    )),
    .ghs_code(f2_code, "r", "OOPS \u56fd\u5bb6\u6392\u884c\uff08Top10 / Bottom10\uff09"),
    .ghs_fig(file.path(fig_dir, "02_oops_ranking_2023.png"),
             sprintf("%d \u5e74\u5c45\u6c11\u81ea\u4ed8\u5360 CHE \u56fd\u5bb6\u6392\u884c", s$cur_year),
             "Figure 2A \u00b7 OOPS \u6392\u884c"),
    .ghs_fig(file.path(fig_dir, "v2_fiscal_ghe_share.png"),
             "GGHED\uff08\u653f\u5e9c\u5f3a\u5236\u7b79\u8d44\uff09\u5360 CHE \u4efd\u989d",
             "Figure 2B \u00b7 \u653f\u5e9c\u7b79\u8d44"),
    .ghs_fig(file.path(fig_dir, "v2_aid_dependency.png"),
             "\u5916\u63f4\u4f9d\u8d56\u5ea6\uff1a\u54ea\u4e9b\u56fd\u5bb6 EXT > 20%",
             "Figure 2C \u00b7 \u5916\u63f4\u4f9d\u8d56"),
    sprintf("<div class='two-col'>%s%s</div>",
            .ghs_table(top_show, sprintf("OOPS \u6700\u9ad8 8 \u56fd\uff08%d\uff09", s$cur_year), 8),
            .ghs_table(bot_show, sprintf("OOPS \u6700\u4f4e 8 \u56fd\uff08%d\uff09", s$cur_year), 8)),
    .ghs_callout("\u89e3\u8bfb \u00b7 \u8d22\u52a1\u4fdd\u62a4\u4e0e\u4e0d\u5e73\u7b49",
      .ghs_para(
        sprintf("<b>OOPS \u96c6\u4e2d\u5ea6\uff1a</b>%d \u5e74\u4ecd\u6709 %d \u4e2a\u56fd\u5bb6 OOPS \u9ad8\u4e8e 50%%\uff0c\u4e3b\u8981\u5728\u5357\u4e9a\u3001\u4e2d\u4e9a\u3001\u52a0\u52d2\u6bd4\u4e0e\u6492\u54c8\u4ee5\u5357\u975e\u6d32\u3002",
                s$cur_year, s$oops_high_cur),
        "<b>\u5bf9\u5e94\u5173\u7cfb\uff1a</b>\u9ad8\u6536\u5165\u56fd\u5bb6 GGHED \u9ad8\u3001OOPS \u4f4e\uff1b\u5916\u63f4\u4f9d\u8d56\u51e0\u4e4e\u5168\u51fa\u73b0\u5728 LIC/LMIC\u3002",
        "<b>\u653f\u7b56\u542b\u4e49\uff1a</b>\u63d0\u5347\u5f3a\u5236\u6c47\u96c6\u662f\u964d\u4f4e OOPS \u7684\u4e3b\u8981\u53ef\u63a7\u53d8\u91cf\uff1b\u5916\u63f4\u4ec5\u80fd\u6258\u5e95\u3002"
      ), tone = "orange")
  )
  f2 <- .ghs_finding("f-finance", "F2", "Finance \u00b7 \u7b79\u8d44\u6765\u6e90",
    "\u8c01\u5728\u4ed8\u94b1\uff1a\u4ece OOPS \u770b\u8d22\u52a1\u4fdd\u62a4",
    "OOPS \u5360\u6bd4\u662f\u8861\u91cf\u8d22\u52a1\u98ce\u9669\u7684\u6838\u5fc3\u4fe1\u53f7\uff1b\u8d8a\u9ad8\u7684\u56fd\u5bb6\uff0c\u5c45\u6c11\u56e0\u75c5\u81f4\u8d2b\u7684\u6982\u7387\u8d8a\u5927\u3002",
    f2_body, chips = paste0(
      .ghs_chip(sprintf("%d \u5e74 OOPS \u5747\u503c", s$cur_year),
                .ghs_n(s$oops_mean_cur, 1, "%"), "orange"),
      .ghs_chip(paste0("OOPS > 50% \u56fd\u5bb6 (", s$cur_year, ")"),
                .ghs_n(s$oops_high_cur, 0), "ink"),
      .ghs_chip("GGHED \u5747\u503c", .ghs_n(s$gghed_mean_cur, 1, "%"), "blue")
    ))

  ## ---- F3 公平性 ----
  ineq_path <- file.path(models_dir, "ineq_panel.csv")
  ineq <- if (file.exists(ineq_path)) utils::read.csv(ineq_path) else NULL
  ineq_chips <- ""
  if (!is.null(ineq) && nrow(ineq)) {
    fy <- ineq[ineq$year == min(ineq$year), ]
    ly <- ineq[ineq$year == max(ineq$year), ]
    ineq_chips <- paste0(
      .ghs_chip("Gini 2000", .ghs_n(fy$gini_pop, 3), "ink"),
      .ghs_chip("Gini 2023", .ghs_n(ly$gini_pop, 3), "blue"),
      .ghs_chip("Theil 2023", .ghs_n(ly$theil_pop, 3), "orange")
    )
  }
  f3_code <- .ghs_read_code(file.path(programs_dir, "04_metrics.R"), 13, 82)
  f3_table <- if (!is.null(ineq) && nrow(ineq)) {
    iq <- ineq[, c("year", "gini_pop", "theil_pop", "atk05", "atk1", "mean_val", "median_val")]
    names(iq) <- c("\u5e74\u4efd", "Gini(pop)", "Theil-T",
                   "Atk 0.5", "Atk 1.0", "\u4eba\u5747\u5747\u503c", "\u4eba\u5747\u4e2d\u4f4d")
    .ghs_table(iq[order(-iq[[1]]), ], "\u8fd1 8 \u5e74\u4e0d\u5e73\u7b49\u6307\u6570", 8, 3)
  } else ""
  f3_body <- paste0(
    .ghs_method(.ghs_para(
      "<b>\u7814\u7a76\u95ee\u9898\uff1a</b>\u8de8\u56fd\u4eba\u5747\u536b\u751f\u652f\u51fa\u7684\u4e0d\u5e73\u7b49\u5982\u4f55\u6f14\u5316\uff1f",
      "<b>\u65b9\u6cd5\uff1a</b>\u5bf9\u6bcf\u5e74\u7684\u4eba\u5747 CHE\uff08USD 2023\uff09\u5e8f\u5217\u8ba1\u7b97\u4e09\u4e2a\u4e92\u8865\u7684\u4e0d\u5e73\u7b49\u6307\u6807\uff1aGini\u3001Theil-T\u3001Atkinson(\u03b5)\uff0c\u5168\u90e8\u4f7f\u7528\u4eba\u53e3\u52a0\u6743\u3002"
    )),
    .ghs_code(f3_code, "r", "04_metrics.R \u00b7 \u4e09\u4e2a\u4e0d\u5e73\u7b49\u6307\u6570\u7684\u5b9e\u73b0"),
    .ghs_fig(file.path(fig_dir, "11_inequality_timeseries.png"),
             "Gini / Theil / Atkinson \u4e09\u6307\u6807\u957f\u671f\u6f14\u5316",
             "Figure 3A \u00b7 \u4e0d\u5e73\u7b49\u65f6\u5e8f"),
    .ghs_fig(file.path(fig_dir, "v2_equity_lorenz.png"),
             "\u6d1b\u4f26\u5179\u66f2\u7ebf\uff1a2000 vs 2023",
             "Figure 3B \u00b7 \u6d1b\u4f26\u5179\u66f2\u7ebf"),
    .ghs_fig(file.path(fig_dir, "v2_equity_indices.png"),
             "\u4e0d\u5e73\u7b49\u6307\u6570\u9762\u677f",
             "Figure 3C \u00b7 \u591a\u6307\u6807\u5bf9\u6bd4"),
    f3_table,
    .ghs_callout("\u89e3\u8bfb \u00b7 \u5e73\u5747\u4e0a\u5347 / \u4e0d\u5e73\u7b49\u4e0b\u964d\u7684\u60b6\u8bba",
      .ghs_para(
        "<b>\u7ed3\u6784\u6027\u4e0b\u964d\uff1a</b>\u4eba\u53e3\u52a0\u6743 Gini \u7531 2000 \u5e74\u7684\u7ea6 0.811 \u964d\u81f3 2023 \u5e74\u7684\u7ea6 0.772\uff1b\u5e76\u975e\u6765\u81ea\u5bcc\u56fd\u505c\u6ede\uff0c\u800c\u662f\u4e2d\u3001\u5370\u3001\u5370\u5c3c\u7b49\u5927\u56fd\u4eba\u5747 CHE \u5feb\u901f\u4e0a\u5347\u3002",
        "<b>\u5269\u4f59\u4e0d\u5e73\u7b49\u4ecd\u6781\u9ad8\uff1a</b>0.77 \u7684 Gini \u4ecd\u5904\u4e8e\u5168\u7403\u6536\u5165\u5206\u914d\u7684\u6781\u7aef\u533a\u95f4\u3002",
        "<b>\u4e09\u6307\u6807\u4e92\u8865\uff1a</b>Atkinson(\u03b5=1) \u957f\u671f\u9ad8\u4e8e 0.69\uff0c\u63d0\u793a\u5e95\u5c42\u654f\u611f\u7684\u4e0d\u5e73\u7b49\u4e0b\u964d\u5e45\u5ea6\u66f4\u5c0f\u3002"
      ), tone = "blue")
  )
  f3 <- .ghs_finding("f-equity", "F3", "Equity \u00b7 \u8de8\u56fd\u4e0d\u5e73\u7b49",
    "\u516c\u5e73\u6027\uff1aGini / Theil / Atkinson",
    "\u4eba\u53e3\u52a0\u6743\u540e\u8de8\u56fd\u4eba\u5747 CHE \u7684\u4e0d\u5e73\u7b49\u957f\u671f\u4e0b\u964d\uff0c\u4f46\u7edd\u5bf9\u6c34\u5e73\u4f9d\u7136\u5904\u4e8e\u6781\u7aef\u533a\u95f4\u3002",
    f3_body, chips = ineq_chips)

  ## ---- F4 COVID 冲击 ----
  covid <- .ghs_covid_top(master, n = 8)
  covid_code <- .ghs_read_code(file.path(programs_dir, "04_metrics.R"), 116, 139)
  covid_chips <- ""; covid_table_html <- ""
  if (!is.null(covid)) {
    covid_chips <- paste0(
      .ghs_chip("\u6700\u5927\u6b63\u51b2\u51fb",
                paste0("+", .ghs_n(max(covid$delta_pct), 1, "%")), "blue"),
      .ghs_chip("\u6700\u5927\u8d1f\u51b2\u51fb",
                paste0(.ghs_n(min(covid$delta_pct), 1, "%")), "orange"),
      .ghs_chip("\u89c2\u6d4b\u56fd\u5bb6", .ghs_n(nrow(covid), 0), "ink")
    )
    cs <- covid
    cs$delta_pct <- paste0(.ghs_n(cs$delta_pct, 2), "%")
    names(cs) <- c("\u56fd\u5bb6/\u5730\u533a", "ISO3",
                   "2019 \u57fa\u7ebf CHE", "2020\u20132022 \u5e73\u5747 CHE",
                   "\u76f8\u5bf9\u53d8\u5316")
    covid_table_html <- .ghs_table(cs, "CHE \u76f8\u5bf9\u53d8\u5316\u6700\u5927\u7684 8 \u4e2a\u56fd\u5bb6", 8)
  }
  f4_body <- paste0(
    .ghs_method(.ghs_para(
      "<b>\u7814\u7a76\u95ee\u9898\uff1a</b>\u65b0\u51a0\u5927\u6d41\u884c\u671f\u95f4\uff0c\u54ea\u4e9b\u56fd\u5bb6\u663e\u8457\u589e\u52a0\u4e86\u536b\u751f\u652f\u51fa\uff1f\u54ea\u4e9b\u53cd\u800c\u840e\u7f29\uff1f",
      "<b>\u65b9\u6cd5\uff1a</b>\u5b9a\u4e49 base = 2019\u3001shock = 2020\u20132022 \u5e73\u5747\uff1b\u5bf9\u6bcf\u4e2a\u56fd\u5bb6\u8ba1\u7b97 CHE \u76f8\u5bf9\u53d8\u5316\u4e0e OOPS \u767e\u5206\u70b9\u53d8\u5316\u3002"
    )),
    .ghs_code(covid_code, "r", "04_metrics.R \u00b7 COVID \u51b2\u51fb\u5ea6\u91cf"),
    .ghs_fig(file.path(fig_dir, "v2_covid_dumbbell.png"),
             "OOPS \u5728 2019 vs 2020\u20132022 \u7684\u56fd\u5bb6\u7ea7\u53d8\u5316\uff08\u54d1\u94c3\u56fe\uff09",
             "Figure 4A \u00b7 OOPS \u54d1\u94c3"),
    .ghs_fig(file.path(fig_dir, "09_covid_scatter.png"),
             "CHE \u589e\u91cf vs OOPS \u589e\u91cf\u6563\u70b9",
             "Figure 4B \u00b7 CHE \u4e0e OOPS \u8054\u52a8"),
    covid_table_html,
    .ghs_callout("\u89e3\u8bfb \u00b7 \u5371\u673a\u4e2d\u7684\u4e24\u7c7b\u8f68\u8ff9",
      .ghs_para(
        "<b>\u653f\u5e9c\u6258\u5e95\u578b\uff1a</b>\u591a\u6570 OECD \u56fd\u5bb6 CHE \u62ac\u5347\u3001GGHED \u4e0a\u5347\uff0cOOPS \u53cd\u800c\u4e0b\u964d\u3002",
        "<b>\u8d22\u653f\u7d27\u7f29\u578b\uff1a</b>\u90e8\u5206\u4e2d\u4f4e\u6536\u5165\u56fd\u5bb6 CHE \u540d\u4e49\u62ac\u5347\u4f46 OOPS \u540c\u6b65\u4e0a\u5347\uff0c\u610f\u5473\u7740\u62ac\u5347\u4e3b\u8981\u6765\u81ea\u5c45\u6c11\u81ea\u4ed8\u3002",
        "<b>\u653f\u7b56\u542b\u4e49\uff1a</b>\u5728\u5e38\u6001\u9884\u7b97\u5916\u9884\u8bbe\u53cd\u5468\u671f\u536b\u751f\u7f13\u51b2\u3002"
      ), tone = "orange")
  )
  f4 <- .ghs_finding("f-covid", "F4", "Resilience \u00b7 \u5371\u673a\u51b2\u51fb",
    "\u65b0\u51a0\u51b2\u51fb\uff1a\u88ab\u538b\u7f29\u7684\u8d22\u653f\u4e0e\u8f6c\u79fb\u7684\u8d1f\u62c5",
    "COVID-19 \u66b4\u9732\u4e86\u536b\u751f\u4f53\u7cfb\u7684\u8106\u5f31\u6027\uff0c\u4f46\u4e0d\u540c\u56fd\u5bb6\u627f\u62c5\u5371\u673a\u6210\u672c\u7684\u65b9\u5f0f\u622a\u7136\u4e0d\u540c\u3002",
    f4_body, chips = covid_chips)

  ## ---- F5 β-收敛 ----
  beta_path <- file.path(models_dir, "beta_panel.csv")
  beta_panel <- if (file.exists(beta_path)) utils::read.csv(beta_path) else NULL
  beta_chips <- ""; beta_text <- ""
  if (!is.null(beta_panel) && nrow(beta_panel)) {
    fit <- tryCatch(stats::lm(growth ~ log_start + continent, data = beta_panel),
                    error = function(e) NULL)
    if (!is.null(fit)) {
      co <- stats::coef(fit); beta_val <- co[["log_start"]]
      half <- if (is.finite(beta_val) && beta_val < 0) -log(2) / beta_val else NA
      r2 <- summary(fit)$r.squared
      beta_chips <- paste0(
        .ghs_chip("\u03b2 (log y\u2080)", .ghs_n(beta_val, 4), "blue"),
        .ghs_chip("\u534a\u6536\u655b\u5e74", .ghs_n(half, 1), "orange"),
        .ghs_chip("R\u00b2", .ghs_n(r2, 3), "ink")
      )
      beta_text <- sprintf("\u62df\u5408\u7ed3\u679c\uff1a\u03b2 = %s\uff0c\u534a\u6536\u655b\u5e74 \u2248 %s \u5e74\uff1b\u03b2 < 0 \u5373\u652f\u6301\u6536\u655b\u5047\u8bbe\u3002",
                           .ghs_n(beta_val, 4), .ghs_n(half, 1))
    }
  }
  f5_code <- .ghs_read_code(file.path(programs_dir, "05_models.R"), 80, 114)
  f5_body <- paste0(
    .ghs_method(.ghs_para(
      "<b>\u7814\u7a76\u95ee\u9898\uff1a</b>\u4f4e\u6c34\u5e73\u56fd\u5bb6\u662f\u5426\u5728\u8ffd\u8d76\u9ad8\u6c34\u5e73\u56fd\u5bb6\uff1f",
      "<b>\u65b9\u6cd5\uff1a</b>\u6784\u9020\u622a\u9762\uff1a\u6bcf\u56fd 2000 \u4e0e 2023 \u7684\u4eba\u5747 CHE\uff1b\u8ba1\u7b97\u5e74\u5316\u5bf9\u6570\u589e\u901f\uff1b\u4ee5 continent \u4e3a\u56fa\u5b9a\u6548\u5e94\uff0c\u56de\u5f52 growth ~ log y\u2080\u3002",
      beta_text
    )),
    .ghs_code(f5_code, "r", "05_models.R \u00b7 \u03b2-\u6536\u655b\u56de\u5f52"),
    .ghs_fig(file.path(fig_dir, "23_beta_convergence.png"),
             "\u03b2-\u6536\u655b\u6563\u70b9 + \u56de\u5f52\u7ebf",
             "Figure 5 \u00b7 \u03b2-\u6536\u655b"),
    .ghs_callout("\u89e3\u8bfb \u00b7 \u6536\u655b\u4e0e\u5f02\u8d28\u6027",
      .ghs_para(
        "<b>\u6574\u4f53\u6536\u655b\uff1a</b>\u03b2 \u663e\u8457\u4e3a\u8d1f\uff0c\u8d77\u70b9\u8d8a\u4f4e\u8fc7\u53bb 23 \u5e74\u4eba\u5747 CHE \u589e\u901f\u8d8a\u5feb\u3002",
        "<b>\u5927\u6d32\u5dee\u5f02\uff1a</b>\u975e\u6d32\u56fd\u5bb6\u5e73\u5747\u589e\u901f\u6700\u9ad8\uff0c\u4f46\u8d77\u70b9\u6700\u4f4e\uff0c\u534a\u6536\u655b\u5e74\u6700\u957f\u3002",
        "<b>\u542b\u4e49\uff1a</b>\u8ffd\u8d76\u4e0d\u662f\u81ea\u52a8\u7684\uff1b\u9700\u8981\u6301\u7eed\u7684 GGHED \u6295\u5165\u4e0e\u5916\u90e8\u6280\u672f\u63f4\u52a9\u3002"
      ), tone = "blue")
  )
  f5 <- .ghs_finding("f-beta", "F5", "Convergence \u00b7 \u6536\u655b",
    "\u03b2-\u6536\u655b\uff1a\u8d2b\u56fd\u662f\u5426\u5728\u8ffd\u8d76\uff1f",
    "\u628a 23 \u5e74\u4eba\u5747 CHE \u589e\u901f\u62df\u5408\u5230\u8d77\u70b9\u6c34\u5e73\uff0c\u7ed3\u679c\u652f\u6301 \u03b2-\u6536\u655b\u4f46\u5206\u5927\u6d32\u5b58\u5728\u663e\u8457\u5f02\u8d28\u6027\u3002",
    f5_body, chips = beta_chips)

  ## ---- F6 面板 FE ----
  fe_path <- file.path(models_dir, "panel_fe_tidy.csv")
  fe <- if (file.exists(fe_path)) utils::read.csv(fe_path) else NULL
  fe_chips <- ""; fe_table_html <- ""
  if (!is.null(fe) && nrow(fe)) {
    row <- fe[fe$term == "log_gdp_pc", , drop = FALSE]
    if (nrow(row)) {
      fe_chips <- paste0(
        .ghs_chip("\u5f39\u6027 \u03b2\u0302", .ghs_n(row$estimate, 3), "blue"),
        .ghs_chip("\u6807\u51c6\u8bef", .ghs_n(row$std.error, 3), "ink"),
        .ghs_chip("t-stat", .ghs_n(row$statistic, 2), "orange")
      )
    }
    fed <- fe[, c("term", "estimate", "std.error", "statistic", "p.value")]
    names(fed) <- c("\u53d8\u91cf", "\u4f30\u8ba1\u503c", "\u6807\u51c6\u8bef",
                    "t \u503c", "p \u503c")
    fe_table_html <- .ghs_table(fed, "\u56de\u5f52\u7ed3\u679c\uff08\u6309 iso3 \u805a\u7c7b\u6807\u51c6\u8bef\uff09", 5, 4)
  }
  f6_code <- .ghs_read_code(file.path(programs_dir, "05_models.R"), 167, 187)
  f6_body <- paste0(
    .ghs_method(.ghs_para(
      "<b>\u7814\u7a76\u95ee\u9898\uff1a</b>\u5f53\u4e00\u56fd GDP \u4e0a\u5347 1%\uff0c\u5176\u4eba\u5747\u536b\u751f\u652f\u51fa\u5927\u7ea6\u4e0a\u5347\u591a\u5c11\uff1f",
      "<b>\u65b9\u6cd5\uff1a</b>\u7528 fixest::feols \u62df\u5408\u53cc\u5411\u56fa\u5b9a\u6548\u5e94\uff1alog(che_pc_usd2023) ~ log(gdp_pc_usd) | iso3_code + year\uff0c\u6309\u56fd\u5bb6\u805a\u7c7b\u6807\u51c6\u8bef\u3002"
    )),
    .ghs_code(f6_code, "r", "05_models.R \u00b7 \u9762\u677f\u53cc\u5411\u56fa\u5b9a\u6548\u5e94"),
    fe_table_html,
    .ghs_callout("\u89e3\u8bfb \u00b7 \u5f39\u6027\u63a5\u8fd1 0.8",
      .ghs_para(
        "<b>\u4e3b\u7ed3\u8bba\uff1a</b>GDP \u6bcf\u4e0a\u5347 1%\uff0c\u4eba\u5747 CHE \u4e0a\u5347\u7ea6 0.8%\uff0c\u5f39\u6027\u663e\u8457\u5c0f\u4e8e 1\u3002",
        "<b>\u542b\u4e49\uff1a</b>\u7eaf\u7c8b\u4f9d\u9760\u7ecf\u6d4e\u589e\u957f\u6765\u6269\u5927\u536b\u751f\u652f\u51fa\u662f\u4e0d\u591f\u7684\uff1b\u8d22\u653f\u7a7a\u95f4\u9700\u8981\u4e3b\u52a8\u5236\u5ea6\u5b89\u6392\u3002"
      ), tone = "blue")
  )
  f6 <- .ghs_finding("f-fe", "F6", "Modeling \u00b7 \u5f39\u6027\u4f30\u8ba1",
    "GDP \u2194 \u536b\u751f\u652f\u51fa\u5f39\u6027\uff1a\u53cc\u5411 FE",
    "\u5728\u63a7\u5236\u56fd\u5bb6\u4e0e\u5e74\u4efd\u56fa\u5b9a\u6548\u5e94\u540e\uff0c\u4eba\u5747\u536b\u751f\u652f\u51fa\u5bf9 GDP \u7684\u5f39\u6027\u63a5\u8fd1 0.8\u3002",
    f6_body, chips = fe_chips)

  ## ---- F7 PCA + 聚类 ----
  f7_code <- .ghs_read_code(file.path(programs_dir, "05_models.R"), 17, 53)
  f7_body <- paste0(
    .ghs_method(.ghs_para(
      "<b>\u7814\u7a76\u95ee\u9898\uff1a</b>\u628a\u56fd\u5bb6\u653e\u5728\u7b79\u8d44\u7ed3\u6784\u7a7a\u95f4\uff08GGHED \u00b7 PVTD \u00b7 EXT \u00b7 OOPS\uff09\uff0c\u5b83\u4eec\u80fd\u805a\u6210\u51e0\u4e2a archetype\uff1f",
      "<b>\u65b9\u6cd5\uff1a</b>\u5bf9\u6700\u8fd1\u4e00\u5e74\u7684\u622a\u9762\u505a PCA\uff08\u4e2d\u5fc3\u5316 + \u6807\u51c6\u5316\uff09\uff0c\u4fdd\u7559\u524d\u4e24\u4e3b\u6210\u5206\uff0c\u518d\u7528 k-means(k=4, nstart=25) \u805a\u7c7b\u3002"
    )),
    .ghs_code(f7_code, "r", "05_models.R \u00b7 PCA + k-means"),
    .ghs_fig(file.path(fig_dir, "12_pca_cluster_2022.png"),
             sprintf("PCA + k-means \u622a\u9762\uff08%d \u5e74\uff09", s$cur_year),
             "Figure 7A \u00b7 \u56fd\u5bb6\u7ec4\u5408"),
    .ghs_fig(file.path(fig_dir, "22_inequality_pca.png"),
             "PCA \u4e3b\u6210\u5206\u8f7d\u8377\uff08financing \u7ef4\u5ea6\uff09",
             "Figure 7B \u00b7 \u4e3b\u6210\u5206\u8f7d\u8377"),
    .ghs_callout("\u89e3\u8bfb \u00b7 4 \u7c7b\u5178\u578b archetype",
      .ghs_para(
        "<b>A \u00b7 \u653f\u5e9c\u5f3a\u4e3b\u5bfc\uff1a</b>\u6b27\u6d32 + \u90e8\u5206\u4e1c\u4e9a\u9ad8\u6536\u5165\u56fd\u5bb6\uff0cGGHED &gt; 70%\u3002",
        "<b>B \u00b7 \u79c1\u4eba\u4fdd\u9669\u578b\uff1a</b>\u7f8e\u56fd\u4e3a\u5178\u578b\uff1bPVTD \u4e0e OOPS \u5747\u9ad8\u3002",
        "<b>C \u00b7 \u81ea\u4ed8\u9a71\u52a8\u578b\uff1a</b>\u5357\u4e9a\u3001\u4e2d\u4e9a\u56fd\u5bb6\uff1bOOPS &gt; 40%\uff0c\u8d22\u52a1\u4fdd\u62a4\u8584\u5f31\u3002",
        "<b>D \u00b7 \u5916\u63f4\u4f9d\u8d56\u578b\uff1a</b>\u6492\u54c8\u4ee5\u5357\u975e\u6d32\u4f4e\u6536\u5165\u56fd\u5bb6\uff1bEXT \u5360\u6bd4\u663e\u8457\u3002"
      ), tone = "ink")
  )
  f7 <- .ghs_finding("f-cluster", "F7", "Typology \u00b7 \u56fd\u5bb6\u7ec4\u5408",
    "PCA \u4e0e\u805a\u7c7b\uff1a\u56db\u79cd\u5178\u578b\u7b79\u8d44 archetype",
    "\u5728 financing \u7ed3\u6784\u7a7a\u95f4\u91cc\uff0c\u5168\u7403\u56fd\u5bb6\u81ea\u7136\u5206\u6210 4 \u7c7b\uff0c\u6bcf\u7c7b\u6709\u4e0d\u540c\u7684\u653f\u7b56\u91cd\u70b9\u3002",
    f7_body)

  ## ---- F8 预测 ----
  fc_path <- file.path(models_dir, "forecast_5y.csv")
  fc <- if (file.exists(fc_path)) utils::read.csv(fc_path) else NULL
  fc_chips <- ""; fc_table_html <- ""
  if (!is.null(fc) && nrow(fc)) {
    fc_chips <- paste0(
      .ghs_chip("\u65b9\u6cd5", "ARIMA(auto)", "blue"),
      .ghs_chip("\u9884\u6d4b\u5e74\u9650", "5 \u5e74", "ink"),
      .ghs_chip("\u8986\u76d6\u56fd\u5bb6", "CHN \u00b7 USA \u00b7 IND \u00b7 BRA", "orange")
    )
    fc_show <- fc
    fc_show$band <- sprintf("[%s, %s]",
                             .ghs_n(fc_show$lo_80, 1), .ghs_n(fc_show$hi_80, 1))
    fc_show <- fc_show[, c("country_name", "year", "point", "band", "method")]
    names(fc_show) <- c("\u56fd\u5bb6", "\u5e74\u4efd",
                       "\u70b9\u4f30\u8ba1", "80% \u533a\u95f4", "\u65b9\u6cd5")
    fc_table_html <- .ghs_table(fc_show, "5 \u5e74\u9884\u6d4b\u660e\u7ec6", 20)
  }
  f8_code <- .ghs_read_code(file.path(programs_dir, "05_models.R"), 117, 165)
  f8_body <- paste0(
    .ghs_method(.ghs_para(
      "<b>\u7814\u7a76\u95ee\u9898\uff1a</b>2024\u20132028 \u4e2d\u3001\u7f8e\u3001\u5370\u3001\u5df4\u7684\u4eba\u5747\u536b\u751f\u652f\u51fa\u4f1a\u671d\u54ea\u4e2a\u65b9\u5411\u8d70\uff1f",
      "<b>\u65b9\u6cd5\uff1a</b>\u5bf9\u6bcf\u56fd 2000\u20132023 \u7684 che_pc_usd2023 \u5e8f\u5217\u62df\u5408 auto.arima\uff0c\u8f93\u51fa 5 \u5e74\u70b9\u9884\u6d4b\u4e0e 80%/95% \u533a\u95f4\u3002"
    )),
    .ghs_code(f8_code, "r", "05_models.R \u00b7 ARIMA \u9884\u6d4b"),
    .ghs_fig(file.path(fig_dir, "24_forecast_fan.png"),
             "5 \u5e74 ARIMA \u9884\u6d4b\u6247\u5f62\u56fe",
             "Figure 8 \u00b7 \u9884\u6d4b\u6247\u5f62"),
    fc_table_html,
    .ghs_callout("\u89e3\u8bfb \u00b7 \u9884\u6d4b\u7684\u8fb9\u754c",
      .ghs_para(
        "<b>\u8d8b\u52bf\u5ef6\u7eed\uff1a</b>\u56db\u56fd\u5747\u7ef4\u6301\u4e0a\u5347\u8d8b\u52bf\uff0c\u4e2d\u3001\u5370\u76f8\u5bf9\u589e\u901f\u6700\u5927\u3002",
        "<b>\u4e0d\u786e\u5b9a\u6027\u5e26\uff1a</b>\u5916\u63a8 5 \u5e74\u540e\u533a\u95f4\u5feb\u901f\u53d1\u6563\u3002",
        "<b>\u5efa\u8bae\uff1a</b>\u7ed3\u5408 Shiny \u4eea\u8868\u76d8 Scenarios \u6a21\u5757\u505a\u60c5\u666f\u63a8\u6f14\u3002"
      ), tone = "blue")
  )
  f8 <- .ghs_finding("f-forecast", "F8", "Forecast \u00b7 5 \u5e74\u5916\u63a8",
    "ARIMA \u9884\u6d4b\uff1a\u56db\u56fd 2024\u20132028",
    "auto.arima \u5bf9\u4e2d\u3001\u7f8e\u3001\u5370\u3001\u5df4\u7684\u4eba\u5747 CHE \u7ed9\u51fa\u77ed\u671f\u5ef6\u7eed\u4e0a\u5347\u7684\u5224\u65ad\u3002",
    f8_body, chips = fc_chips)

  paste0(.ghs_kpi_grid(s, length(list.files(fig_dir, "[.]png$")),
                        length(list.files(file.path(dirname(fig_dir), "\u4ea4\u4e92\u7ec4\u4ef6"), "[.]html$"))),
         "<section class='findings-anchor' id='findings'></section>",
         f1, f2, f3, f4, f5, f6, f7, f8)
}

# ---- 5. 图库 / 交互组件 / 复现 / 结论 ------------------------------------

.ghs_gallery <- function(fig_dir) {
  pngs <- sort(list.files(fig_dir, pattern = "[.]png$", full.names = TRUE))
  if (!length(pngs)) return("")
  card <- function(p) {
    title <- .ghs_pretty(p); kind <- .ghs_kind(title); src <- .ghs_b64(p)
    sprintf("<article class='gallery-card' data-kind='%s'><button class='gallery-button' type='button' onclick=\"openFigure(this)\" data-title='%s'><img src='%s' alt='%s' loading='lazy'></button><div class='gallery-meta'><span>%s</span><strong>%s</strong><em>%s</em></div></article>",
            .ghs_e(kind), .ghs_e(title), src, .ghs_e(title),
            .ghs_e(kind), .ghs_e(title), .ghs_size(p))
  }
  cards <- paste(vapply(pngs, card, character(1)), collapse = "")
  kinds <- c("\u5168\u90e8", "\u65f6\u95f4", "\u5206\u5e03", "\u5730\u56fe",
             "\u7ed3\u6784", "\u6a21\u578b", "\u6307\u6807", "\u7efc\u5408")
  tabs <- paste(vapply(kinds, function(k) {
    sprintf("<button type='button' onclick=\"filterFigures('%s', this)\"%s>%s</button>",
            .ghs_e(k), if (k == "\u5168\u90e8") " class='active'" else "",
            .ghs_e(k))
  }, character(1)), collapse = "")
  sprintf("<section class='section gallery' id='gallery'><div class='wrap'><header class='section-head'><span class='kicker'>11 \u00b7 Gallery</span><h2>\u9759\u6001\u56fe\u8868\u5e93 \u00b7 %d \u5f20</h2><p class='lead'>\u6309\u4e3b\u9898\u7b5b\u9009\uff1b\u70b9\u51fb\u4efb\u610f\u5361\u7247\u653e\u5927\u67e5\u770b\u539f\u56fe\u3002</p></header><div class='tabs'>%s</div><div class='gallery-grid'>%s</div></div></section>",
          length(pngs), tabs, cards)
}

.ghs_widgets <- function(widget_dir, mode, repo_url) {
  htmls <- sort(list.files(widget_dir, pattern = "[.]html$", full.names = TRUE))
  if (!length(htmls)) return("")
  card <- function(p) {
    title <- .ghs_pretty(p); kind <- .ghs_kind(title); base <- basename(p)
    src <- if (mode == "submission")
      paste0("../\u5206\u6790\u8f93\u51fa/\u4ea4\u4e92\u7ec4\u4ef6/", base)
    else paste0("\u4ea4\u4e92\u7ec4\u4ef6/", base)
    fallback <- if (mode == "submission")
      sprintf("%s/blob/main/\u5206\u6790\u8f93\u51fa/\u4ea4\u4e92\u7ec4\u4ef6/%s", repo_url, base)
    else paste0("\u4ea4\u4e92\u7ec4\u4ef6/", base)
    sprintf("<article class='widget-card' data-kind='%s'><header><span class='pill'>%s</span><h3>%s</h3><p>%s \u00b7 standalone HTML</p></header><div class='widget-actions'><button type='button' onclick=\"loadWidgetUrl('%s','%s')\">\u5728\u53f3\u4fa7\u67e5\u770b</button><a href='%s' target='_blank' rel='noreferrer'>\u65b0\u7a97\u6253\u5f00</a></div></article>",
            .ghs_e(kind), .ghs_e(kind), .ghs_e(title), .ghs_size(p),
            .ghs_e(src), .ghs_e(title), .ghs_e(fallback))
  }
  cards <- paste(vapply(htmls, card, character(1)), collapse = "")
  kinds <- c("\u5168\u90e8", "\u65f6\u95f4", "\u5206\u5e03", "\u5730\u56fe",
             "\u7ed3\u6784", "\u6a21\u578b", "\u6307\u6807", "\u7efc\u5408")
  tabs <- paste(vapply(kinds, function(k) {
    sprintf("<button type='button' onclick=\"filterWidgets('%s', this)\"%s>%s</button>",
            .ghs_e(k), if (k == "\u5168\u90e8") " class='active'" else "",
            .ghs_e(k))
  }, character(1)), collapse = "")
  sprintf("<section class='section widgets' id='widgets'><div class='wrap'><header class='section-head'><span class='kicker'>12 \u00b7 Interactive lab</span><h2>\u4ea4\u4e92\u7ec4\u4ef6 \u00b7 %d \u4e2a\u72ec\u7acb HTML</h2><p class='lead'>plotly \u00b7 leaflet \u00b7 reactable \u00b7 DT \u00b7 networkD3\u3002\u70b9\u51fb \u201c\u5728\u53f3\u4fa7\u67e5\u770b\u201d \u5728\u61d2\u52a0\u8f7d iframe \u4e2d\u6253\u5f00\u3002</p></header><div class='tabs'>%s</div><div class='widget-lab'><div class='widget-list'>%s</div><div class='widget-frame-wrap'><div class='widget-frame-head'><strong id='widget-title'>\u9009\u62e9\u5de6\u4fa7\u4efb\u4e00\u7ec4\u4ef6</strong><span>standalone widget</span></div><iframe id='widget-frame' title='interactive widget' loading='lazy'></iframe></div></div></div></section>",
          length(htmls), tabs, cards)
}

.ghs_repro <- function(repo_url) {
  cmd <- function(label, code, note = NULL) {
    note_html <- if (!is.null(note))
      sprintf("<p class='cmd-note'>%s</p>", .ghs_e(note)) else ""
    sprintf("<article class='cmd-card'><header><strong>%s</strong></header><pre class='code-pre'><code class='language-bash'>%s</code></pre>%s</article>",
            .ghs_e(label), .ghs_e(code), note_html)
  }
  cards <- paste0(
    cmd("\u514b\u9686\u4ed3\u5e93", sprintf("git clone %s.git", repo_url),
        "\u9879\u76ee\u6839\u76ee\u5f55\u7ea6 700 MB\uff08\u542b\u56fe\u8868 + widget + \u7f13\u5b58\uff09\u3002"),
    cmd("\u5b89\u88c5 R \u4f9d\u8d56", "Rscript \u5b89\u88c5\u4f9d\u8d56.R",
        "\u5b89\u88c5 60+ R \u5305\uff1b\u7f51\u7edc\u8f83\u6162\u65f6\u8bbe\u7f6e CRAN \u955c\u50cf\u3002"),
    cmd("\u6784\u5efa\u6570\u636e\u7f13\u5b58", "Rscript \u6784\u5efa.R data",
        "\u751f\u6210 \u6d3e\u751f\u6570\u636e/\u5904\u7406\u7ed3\u679c/master_enriched.rds\u3002"),
    cmd("\u6240\u6709\u56fe\u8868\u4e0e\u4ea4\u4e92\u7ec4\u4ef6",
        "Rscript \u6784\u5efa.R figures\nRscript \u6784\u5efa.R widgets",
        "44 \u5f20\u9759\u6001\u56fe + 24 \u4e2a\u4ea4\u4e92\u7ec4\u4ef6\u3002"),
    cmd("\u6240\u6709\u7edf\u8ba1\u6a21\u578b", "Rscript \u6784\u5efa.R models",
        "Gini \u00b7 COVID \u51b2\u51fb \u00b7 \u03b2-\u6536\u655b \u00b7 FE \u00b7 PCA \u00b7 \u9884\u6d4b\u3002"),
    cmd("\u4e00\u952e\u4ea7\u51fa\u672c\u9875\u4e0e GitHub Pages \u9996\u9875",
        "Rscript \u6784\u5efa.R submission",
        "\u751f\u6210 \u8bfe\u7a0b\u63d0\u4ea4/\u5e84\u9882_20241334.html \u4e0e \u7f51\u7ad9\u53d1\u5e03/index.html\u3002"),
    cmd("\u542f\u52a8\u4ed4\u8868\u76d8", "Rscript \u542f\u52a8\u4eea\u8868\u76d8.R 4848",
        "\u672c\u5730 Shiny \u4eea\u8868\u76d8\uff0812 \u4e2a\u6a21\u5757\uff09\u3002"),
    cmd("\u4e00\u952e\u90e8\u7f72\u5305", "Rscript \u6784\u5efa.R deploy",
        "Quarto \u7ae0\u8282 + shinylive + \u6574\u5408\u9996\u9875 + widget assets\u3002")
  )
  sprintf("<section class='section repro' id='repro'><div class='wrap'><header class='section-head'><span class='kicker'>13 \u00b7 Reproducibility</span><h2>\u590d\u73b0\u8bf4\u660e \u00b7 \u4e00\u952e\u547d\u4ee4</h2><p class='lead'>\u672c\u9879\u76ee\u662f\u5355\u4e00\u6765\u6e90\uff1a\u6240\u6709\u4ee3\u7801\u3001\u6570\u636e\u3001\u6a21\u578b\u3001\u56fe\u8868\u90fd\u4ece <code>\u6784\u5efa.R</code> \u6d3e\u751f\u3002</p></header><div class='cmd-grid'>%s</div></div></section>",
          cards)
}

.ghs_conclusion <- function(s) {
  sprintf(paste0(
    "<section class='section conclusion' id='conclusion'><div class='wrap'>",
    "<header class='section-head'><span class='kicker'>14 \u00b7 Conclusion</span><h2>\u7ed3\u8bba\u4e0e\u653f\u7b56\u5efa\u8bae</h2></header>",
    "<div class='conclusion-grid'>",
    "<article class='conc-card'><span>1</span><h3>\u628a OOPS \u5217\u4e3a\u97e7\u6027\u7684\u7b2c\u4e00\u6307\u6807</h3><p>%d \u4e2a\u56fd\u5bb6\u5728 %d \u5e74\u4ecd\u6709 OOPS &gt; 50%%\uff0c\u4efb\u4e00\u51b2\u51fb\u5747\u4f1a\u63a8\u9ad8\u56e0\u75c5\u81f4\u8d2b\u7387\u3002\u5728\u56fd\u5bb6\u536b\u751f\u6218\u7565\u4e2d\u628a OOPS \u589e\u91cf\u4f5c\u4e3a<em>\u53cd\u5411 KPI</em>\u3002</p></article>",
    "<article class='conc-card'><span>2</span><h3>\u63d0\u9ad8 GGHED \u662f\u964d OOPS \u7684\u4e3b\u53ef\u63a7\u53d8\u91cf</h3><p>F2 \u5df2\u663e\u793a GGHED \u4e0e OOPS \u9ad8\u5ea6\u8d1f\u76f8\u5173\uff1b\u5728\u4e2d\u4f4e\u6536\u5165\u56fd\u5bb6\u5e94\u4f18\u5148\u6269\u5927\u793e\u4fdd\u57fa\u91d1 + \u4e00\u822c\u7a0e\u6c60\u5b50\u3002</p></article>",
    "<article class='conc-card'><span>3</span><h3>\u4fdd\u7559\u53cd\u5468\u671f\u536b\u751f\u7f13\u51b2</h3><p>F4 \u8868\u660e\uff0c\u5728 COVID \u51b2\u51fb\u4e0b\uff0c\u80fd\u4e3b\u52a8\u52a0\u7801 GGHED \u7684\u56fd\u5bb6\u628a OOPS \u538b\u4f4f\u4e86\uff1b\u8d22\u653f\u7eaa\u5f8b\u8981\u4e3a\u5371\u673a\u4fdd\u7559\u7f13\u51b2\u3002</p></article>",
    "<article class='conc-card'><span>4</span><h3>\u8ffd\u8d76\u975e\u81ea\u52a8\uff1a\u03b2-\u6536\u655b\u4f46\u5206\u5927\u6d32\u5f02\u8d28</h3><p>F5 \u4e2d \u03b2 &lt; 0 \u4f46\u534a\u6536\u655b\u5e74\u5dee\u5f02\u5de8\u5927\uff1b\u975e\u6d32\u56fd\u5bb6\u9700\u6301\u7eed GGHED \u6295\u5165 + \u5916\u63f4\u3002</p></article>",
    "<article class='conc-card'><span>5</span><h3>\u7528 archetype \u5206\u7c7b\u5bf9\u75c7\u65bd\u7b56</h3><p>F7 \u7684 4 \u7c7b\u56fd\u5bb6\u7b79\u8d44 archetype \u63d0\u4f9b\u4e86\u53ef\u6267\u884c\u7684\u653f\u7b56\u8def\u5f84\uff1a\u653f\u5e9c\u4e3b\u5bfc / \u79c1\u4eba\u4fdd\u9669 / \u81ea\u4ed8\u9a71\u52a8 / \u5916\u63f4\u4f9d\u8d56\u3002</p></article>",
    "<article class='conc-card'><span>6</span><h3>\u9884\u6d4b\u5e94\u4f5c\u4e3a\u653f\u7b56\u5bf9\u8bdd\u7684\u8d77\u70b9</h3><p>F8 \u7684 ARIMA \u4ec5\u5916\u63a8\u8d8b\u52bf\uff1b\u4e0b\u4e00\u6b65\u662f\u7ed3\u5408 Shiny \u4eea\u8868\u76d8\u505a\u60c5\u666f\u63a8\u6f14\u3002</p></article>",
    "</div>",
    "<div class='limit'><h3>\u7814\u7a76\u5c40\u9650</h3><ul><li>OOPS \u4e0e\u5916\u63f4\u7684\u53e3\u5f84\u5728\u4e0d\u540c\u56fd\u5bb6\u5b58\u5728\u7edf\u8ba1\u5dee\u5f02\u3002</li><li>2023 \u5e74\u90e8\u5206\u56fd\u5bb6\u6570\u636e\u6765\u81ea\u6a21\u578b\u4f30\u8ba1\uff0c\u9700\u4ee5\u65b0\u7248 GHED \u516c\u5e03\u4e3a\u51c6\u3002</li><li>\u9762\u677f FE \u4e0d\u80fd\u8bc6\u522b\u56e0\u679c\uff0c\u4ec5\u7ed9\u51fa\u6761\u4ef6\u76f8\u5173\u3002</li></ul></div>",
    "</div></section>"
  ), s$oops_high_cur, s$cur_year)
}

.ghs_footer <- function() {
  paste0(
    "<footer class='site-footer'><div class='wrap'>",
    "<div class='foot-grid'>",
    "<div><strong>Global Health Spending \u00b7 \u5e84\u9882 20241334</strong>",
    "<p>\u6570\u636e\u6765\u6e90\uff1aWHO Global Health Expenditure Database / TidyTuesday 2026-04-21\u3002\u5206\u6790\u4ec5\u7528\u4e8e\u8bfe\u7a0b\u9879\u76ee\u4e0e\u6570\u636e\u65b0\u95fb\u5c55\u793a\uff0c\u4e0d\u6784\u6210\u56e0\u679c\u63a8\u65ad\u3002</p></div>",
    "<div><strong>\u4ed3\u5e93</strong><p><a href='https://github.com/2711944586/R'>github.com/2711944586/R</a></p></div>",
    "<div><strong>\u5fae\u540e\u7aef</strong><p>R 4.5 \u00b7 ggplot2 \u00b7 plotly \u00b7 leaflet \u00b7 fixest \u00b7 forecast \u00b7 Shiny</p></div>",
    "</div></div></footer>"
  )
}

# ---- 6. CSS / JS ---------------------------------------------------------

.ghs_css <- function() {
  paste(c(
    ":root{--ink:#0d121b;--muted:#5d667a;--paper:#fbf6ee;--paper-2:#f1e8da;--line:rgba(13,18,27,.10);--line-strong:rgba(13,18,27,.18);--blue:#1d3f5f;--orange:#c46327;--green:#2a857a;--code-bg:#0c1424;--code-ink:#e6efff}",
    "*,*:before,*:after{box-sizing:border-box}html{scroll-behavior:smooth}body{margin:0;background:var(--paper);color:var(--ink);font-family:'Inter','Segoe UI','Helvetica Neue','Microsoft YaHei','Noto Sans CJK SC',system-ui,sans-serif;line-height:1.7;-webkit-font-smoothing:antialiased}",
    ".wrap{width:min(1200px,92vw);margin:auto}",
    "h1,h2,h3{font-family:'Source Serif 4',Georgia,'Noto Serif SC',serif;letter-spacing:-.01em}",
    "code,pre{font-family:'JetBrains Mono','Fira Code',Consolas,monospace}",
    "a{color:var(--blue)}",
    ".hero{position:relative;min-height:88vh;padding:30px 0 90px;color:#f7eedf;background:radial-gradient(circle at 18% 0%,#1d3f5f 0%,#0d121b 60%),radial-gradient(circle at 90% 110%,rgba(212,129,70,.45),transparent 55%);overflow:hidden}",
    ".hero:before{content:'';position:absolute;inset:0;background:radial-gradient(circle at 70% 30%,rgba(212,200,180,.18),transparent 45%),radial-gradient(circle at 30% 80%,rgba(255,255,255,.08),transparent 50%);pointer-events:none}",
    ".nav{display:flex;justify-content:space-between;align-items:center;padding:6px 0 16px;position:relative}",
    ".brand{font-weight:800;letter-spacing:.16em;font-size:14px;color:#f7eedf;text-decoration:none}",
    ".links{display:flex;gap:18px}.links a{color:rgba(247,238,223,.78);font-size:14px;text-decoration:none;letter-spacing:.04em}.links a:hover{color:#fff}",
    ".hero-grid{display:grid;grid-template-columns:1.25fr .75fr;gap:48px;align-items:end;padding-top:72px;position:relative}",
    ".eyebrow{display:inline-flex;align-items:center;gap:8px;padding:8px 14px;border:1px solid rgba(247,238,223,.28);border-radius:999px;background:rgba(247,238,223,.06);font-size:12px;letter-spacing:.18em;text-transform:uppercase}",
    ".hero h1{font-size:clamp(48px,7vw,96px);line-height:1.02;margin:24px 0 22px;font-weight:700}",
    ".hero h1 .accent{background:linear-gradient(120deg,#f7c08a,#d68146 60%,#c46327);-webkit-background-clip:text;background-clip:text;color:transparent}",
    ".hero-lead{font-size:19px;max-width:780px;color:rgba(247,238,223,.86);margin:0 0 30px}",
    ".hero-actions{display:flex;flex-wrap:wrap;gap:12px}",
    ".btn{display:inline-flex;align-items:center;gap:8px;padding:13px 22px;border-radius:999px;background:#fbe9c8;color:#1d3f5f;font-weight:800;text-decoration:none;border:0;font-size:14px;letter-spacing:.02em;box-shadow:0 16px 40px rgba(212,129,70,.34)}",
    ".btn.alt{background:rgba(247,238,223,.12);color:#f7eedf;border:1px solid rgba(247,238,223,.32);box-shadow:none}",
    ".btn.ghost{background:transparent;color:#f7eedf;border:1px solid rgba(247,238,223,.18);box-shadow:none}",
    ".hero-panel{position:relative;border:1px solid rgba(247,238,223,.18);border-radius:28px;padding:24px;background:rgba(247,238,223,.06);backdrop-filter:blur(16px)}",
    ".hero-panel-tag{display:inline-block;font-size:12px;letter-spacing:.18em;text-transform:uppercase;color:rgba(247,238,223,.66)}",
    ".hero-panel-list{margin:14px 0 0;padding:0;list-style:none;display:grid;gap:12px}",
    ".hero-panel-list li{display:flex;align-items:baseline;gap:14px;border-bottom:1px solid rgba(247,238,223,.14);padding-bottom:10px;font-size:15px}",
    ".hero-panel-list li:last-child{border-bottom:0}",
    ".hero-panel-list li b{font-family:'Source Serif 4',serif;font-size:34px;color:#f7c08a;font-weight:700;min-width:64px}",
    ".section{padding:96px 0;border-top:1px solid var(--line)}",
    ".kpi-section{background:linear-gradient(180deg,#fbf6ee,#f1e8da)}",
    ".section-head{margin-bottom:42px;max-width:880px}",
    ".section-head .kicker{display:inline-block;font-size:12px;letter-spacing:.22em;text-transform:uppercase;color:var(--orange);margin-bottom:10px;font-weight:800}",
    ".section h2{font-size:clamp(34px,4.4vw,56px);line-height:1.05;margin:0 0 16px}",
    ".section .lead{font-size:19px;color:var(--muted);max-width:780px}",
    ".kpi-grid{display:grid;grid-template-columns:repeat(4,1fr);gap:18px}",
    ".kpi{position:relative;padding:24px 22px;border:1px solid var(--line);border-radius:20px;background:linear-gradient(180deg,#fffaf2,#f3e8d6);box-shadow:0 16px 40px rgba(13,18,27,.05);overflow:hidden}",
    ".kpi:before{content:'';position:absolute;left:0;top:0;width:5px;height:100%;background:linear-gradient(180deg,#c46327,#1d3f5f)}",
    ".kpi-value{font-family:'Source Serif 4',serif;font-size:38px;font-weight:700;color:var(--blue);line-height:1.1}",
    ".kpi-label{margin-top:10px;font-weight:800;font-size:14px;letter-spacing:.04em}",
    ".kpi-note{margin-top:6px;font-size:12.5px;color:var(--muted);line-height:1.55}",
    ".methods .code-figure{margin:14px 0 22px}",
    ".finding{padding:104px 0;border-top:1px solid var(--line)}",
    ".finding:nth-of-type(odd){background:linear-gradient(180deg,#fbf6ee 0%,#f3e8d6 100%)}",
    ".finding-head{display:grid;grid-template-columns:96px 1fr;gap:24px;align-items:start;margin-bottom:36px}",
    ".finding-num{font-family:'Source Serif 4',serif;font-size:72px;line-height:1;color:var(--orange);font-weight:800}",
    ".finding-kicker{display:inline-block;font-size:12px;letter-spacing:.22em;text-transform:uppercase;color:var(--blue);margin-bottom:6px;font-weight:800}",
    ".finding-head h2{font-size:clamp(32px,4.4vw,52px);line-height:1.05;margin:0 0 12px}",
    ".finding-head .lead{font-size:18px;color:var(--muted);max-width:760px}",
    ".chips{display:flex;flex-wrap:wrap;gap:10px;margin-top:14px}",
    ".chip{display:inline-flex;align-items:baseline;gap:8px;padding:7px 14px;border-radius:999px;border:1px solid var(--line-strong);background:#fff;font-size:13px}",
    ".chip b{color:var(--muted);font-weight:600;font-size:11px;text-transform:uppercase;letter-spacing:.1em}",
    ".chip i{font-style:normal;font-weight:800;color:var(--ink)}",
    ".chip-blue i{color:var(--blue)}.chip-orange i{color:var(--orange)}",
    ".finding-body{display:grid;gap:22px}",
    ".method-block{background:rgba(255,255,255,.5);border:1px solid var(--line);border-radius:18px;padding:18px 22px;font-size:15.5px;color:var(--ink)}",
    ".method-block p{margin:6px 0}",
    ".method-block ul{margin:6px 0 6px 18px}",
    ".code-figure{margin:0;background:var(--code-bg);border-radius:18px;overflow:hidden;border:1px solid rgba(13,18,27,.18);box-shadow:0 18px 36px rgba(13,18,27,.18)}",
    ".code-caption{padding:10px 18px;color:#cdd9ee;background:rgba(255,255,255,.04);border-bottom:1px solid rgba(255,255,255,.08);font-size:12.5px;letter-spacing:.05em;text-transform:uppercase;font-weight:700}",
    ".code-pre{margin:0;padding:18px 20px;color:var(--code-ink);font-size:13px;line-height:1.6;overflow:auto;max-height:420px}",
    ".code-pre code{color:inherit;background:none}",
    ".fig-inline{margin:0;background:#fff;border:1px solid var(--line);border-radius:22px;overflow:hidden;box-shadow:0 18px 50px rgba(13,18,27,.08)}",
    ".fig-frame{width:100%;background:linear-gradient(180deg,#fffaf2,#f3e8d6);padding:18px 18px 0;display:flex;justify-content:center}",
    ".fig-frame img{max-width:100%;height:auto;display:block}",
    ".fig-inline figcaption{padding:14px 22px 18px;display:flex;flex-direction:column;gap:6px}",
    ".fig-kicker{font-size:12px;letter-spacing:.18em;text-transform:uppercase;color:var(--orange);font-weight:800}",
    ".fig-inline figcaption strong{font-family:'Source Serif 4',serif;font-size:18px;color:var(--ink);font-weight:600}",
    ".callout{margin:0;padding:22px 26px;border-radius:20px;border:1px solid var(--line);display:grid;gap:8px}",
    ".callout-blue{background:linear-gradient(135deg,#eaf1f8,#fbf6ee);border-color:rgba(29,63,95,.22)}",
    ".callout-orange{background:linear-gradient(135deg,#fbeede,#fbf6ee);border-color:rgba(196,99,39,.22)}",
    ".callout-ink{background:#fff;border-color:var(--line-strong)}",
    ".callout strong{font-size:14px;letter-spacing:.18em;text-transform:uppercase;color:var(--orange);font-weight:800}",
    ".callout p{margin:6px 0}",
    ".table-wrap{overflow:auto;border-radius:18px;border:1px solid var(--line);background:#fff;box-shadow:0 16px 36px rgba(13,18,27,.06)}",
    ".table-wrap table{width:100%;border-collapse:collapse;font-size:14px}",
    ".table-wrap caption{caption-side:top;padding:14px 18px;text-align:left;font-weight:800;color:var(--blue);background:linear-gradient(90deg,#fbf6ee,#fff);border-bottom:1px solid var(--line)}",
    ".table-wrap th,.table-wrap td{padding:10px 14px;border-bottom:1px solid var(--line)}",
    ".table-wrap th{background:#fbf6ee;text-align:left;font-weight:800;color:var(--ink);letter-spacing:.04em;font-size:12.5px;text-transform:uppercase}",
    ".table-wrap td{color:var(--ink)}",
    ".two-col{display:grid;grid-template-columns:1fr 1fr;gap:18px}",
    ".tabs{display:flex;flex-wrap:wrap;gap:10px;margin:16px 0 22px}",
    ".tabs button{border:1px solid var(--line-strong);background:#fff;border-radius:999px;padding:9px 16px;font-weight:800;color:var(--ink);cursor:pointer;font-size:13px}",
    ".tabs button.active{background:var(--ink);color:#fff;border-color:var(--ink)}",
    ".gallery-grid{display:grid;grid-template-columns:repeat(3,1fr);gap:18px}",
    ".gallery-card{background:#fff;border:1px solid var(--line);border-radius:22px;overflow:hidden;box-shadow:0 16px 40px rgba(13,18,27,.06);transition:transform .25s ease,box-shadow .25s ease}",
    ".gallery-card:hover{transform:translateY(-4px);box-shadow:0 22px 60px rgba(13,18,27,.12)}",
    ".gallery-button{border:0;background:#fffaf2;width:100%;padding:0;cursor:zoom-in;display:flex;align-items:center;justify-content:center;min-height:200px}",
    ".gallery-card img{width:100%;height:auto;max-height:340px;object-fit:contain;display:block}",
    ".gallery-meta{padding:14px 16px 18px;display:flex;flex-direction:column;gap:6px}",
    ".gallery-meta span,.pill{display:inline-flex;background:#f0e3d0;color:#774314;border-radius:999px;padding:4px 11px;font-size:12px;font-weight:900;width:fit-content;letter-spacing:.04em}",
    ".gallery-meta strong{font-family:'Source Serif 4',serif;font-size:16px;color:var(--ink)}",
    ".gallery-meta em{color:var(--muted);font-style:normal;font-size:12px}",
    ".widget-lab{display:grid;grid-template-columns:.86fr 1.14fr;gap:22px;align-items:start}",
    ".widget-list{display:grid;gap:12px;max-height:760px;overflow:auto;padding-right:6px}",
    ".widget-card{background:#fff;border:1px solid var(--line);border-radius:18px;padding:16px 18px;display:flex;justify-content:space-between;gap:14px;align-items:center;box-shadow:0 10px 28px rgba(13,18,27,.05)}",
    ".widget-card header{display:flex;flex-direction:column;gap:4px}",
    ".widget-card h3{margin:6px 0 0;font-size:16px;font-family:'Source Serif 4',serif;color:var(--ink);font-weight:600}",
    ".widget-card p{margin:0;color:var(--muted);font-size:12px}",
    ".widget-actions{display:flex;flex-direction:column;gap:6px;align-items:flex-end}",
    ".widget-card button{border:0;border-radius:999px;background:var(--blue);color:#fff;padding:10px 14px;font-weight:900;cursor:pointer;white-space:nowrap;font-size:13px}",
    ".widget-card a{font-size:12px;color:var(--muted);text-decoration:none;border-bottom:1px dashed var(--muted)}",
    ".widget-frame-wrap{position:sticky;top:18px;background:#0c1424;border-radius:24px;padding:14px;box-shadow:0 26px 70px rgba(13,18,27,.18)}",
    ".widget-frame-head{color:#cdd9ee;display:flex;justify-content:space-between;align-items:center;padding:6px 8px 12px;font-size:13px}",
    ".widget-frame-head strong{color:#fff;font-family:'Source Serif 4',serif;font-weight:600}",
    ".widget-frame-wrap iframe{width:100%;height:680px;border:0;border-radius:18px;background:#fff}",
    ".cmd-grid{display:grid;grid-template-columns:repeat(2,1fr);gap:18px}",
    ".cmd-card{background:#fff;border:1px solid var(--line);border-radius:18px;overflow:hidden;box-shadow:0 12px 28px rgba(13,18,27,.06)}",
    ".cmd-card header{padding:14px 18px;background:#fbf6ee;border-bottom:1px solid var(--line);font-weight:800;color:var(--blue)}",
    ".cmd-card .code-pre{max-height:180px;background:var(--code-bg);color:var(--code-ink)}",
    ".cmd-note{padding:12px 18px 16px;color:var(--muted);font-size:13.5px;margin:0}",
    ".conclusion{background:linear-gradient(180deg,#fbf6ee,#f3e8d6)}",
    ".conclusion-grid{display:grid;grid-template-columns:repeat(3,1fr);gap:18px;margin-bottom:36px}",
    ".conc-card{background:#fff;border:1px solid var(--line);border-radius:22px;padding:22px;box-shadow:0 14px 32px rgba(13,18,27,.06);display:grid;gap:8px}",
    ".conc-card span{font-family:'Source Serif 4',serif;font-size:34px;font-weight:800;color:var(--orange)}",
    ".conc-card h3{font-size:18px;margin:0 0 4px;color:var(--ink)}",
    ".conc-card p{margin:0;color:var(--muted);font-size:14.5px;line-height:1.7}",
    ".limit{background:#fff;border-radius:18px;padding:22px 26px;border:1px solid var(--line);display:grid;gap:10px}",
    ".limit h3{margin:0;font-size:18px;color:var(--blue)}",
    ".limit ul{margin:0;padding-left:20px}",
    ".limit li{color:var(--muted);font-size:14.5px;margin:5px 0}",
    ".site-footer{background:#0d121b;color:#bcc6d8;padding:64px 0;font-size:14px}",
    ".foot-grid{display:grid;grid-template-columns:1.4fr 1fr 1fr;gap:32px}",
    ".site-footer strong{color:#f7eedf;display:block;margin-bottom:6px;font-family:'Source Serif 4',serif}",
    ".site-footer a{color:#f7c08a;text-decoration:none}",
    ".modal{position:fixed;inset:0;background:rgba(0,0,0,.86);display:none;z-index:50;padding:28px}",
    ".modal.open{display:grid;place-items:center}",
    ".modal img{max-width:96vw;max-height:86vh;background:#fff;border-radius:14px;box-shadow:0 30px 80px rgba(0,0,0,.5)}",
    ".modal button{position:absolute;right:24px;top:20px;border:0;border-radius:999px;padding:10px 16px;font-weight:900;background:#fff;cursor:pointer}",
    ".modal-title{position:absolute;left:28px;top:22px;color:#fff;font-weight:900;font-family:'Source Serif 4',serif;font-size:18px}",
    "@media(max-width:1080px){.hero-grid,.widget-lab{grid-template-columns:1fr}.kpi-grid{grid-template-columns:repeat(2,1fr)}.gallery-grid{grid-template-columns:repeat(2,1fr)}.conclusion-grid{grid-template-columns:repeat(2,1fr)}.cmd-grid{grid-template-columns:1fr}.two-col{grid-template-columns:1fr}.widget-frame-wrap{position:static}.foot-grid{grid-template-columns:1fr}}",
    "@media(max-width:640px){.kpi-grid,.gallery-grid,.conclusion-grid{grid-template-columns:1fr}.links{display:none}.hero{min-height:auto}.hero-grid{padding-top:42px}.finding-head{grid-template-columns:1fr}.finding-num{font-size:54px}}"
  ), collapse = "")
}

.ghs_js <- function() {
  paste(c(
    "function filterFigures(k,b){document.querySelectorAll('.gallery .tabs button').forEach(x=>x.classList.remove('active'));b.classList.add('active');document.querySelectorAll('.gallery-card').forEach(c=>{c.style.display=(k==='\u5168\u90e8'||c.dataset.kind===k)?'block':'none'});}",
    "function filterWidgets(k,b){document.querySelectorAll('.widgets .tabs button').forEach(x=>x.classList.remove('active'));b.classList.add('active');document.querySelectorAll('.widget-card').forEach(c=>{c.style.display=(k==='\u5168\u90e8'||c.dataset.kind===k)?'flex':'none'});}",
    "function openFigure(btn){var img=btn.querySelector('img');document.getElementById('modal-img').src=img.src;document.getElementById('modal-title').textContent=btn.dataset.title||img.alt;document.getElementById('fig-modal').classList.add('open');}",
    "function closeFigure(){document.getElementById('fig-modal').classList.remove('open');}",
    "function loadWidgetUrl(url,title){document.getElementById('widget-title').textContent=title;var f=document.getElementById('widget-frame');f.src=url;f.scrollIntoView({behavior:'smooth',block:'center'});}",
    "document.addEventListener('keydown',e=>{if(e.key==='Escape')closeFigure();});"
  ), collapse = "")
}

# ---- 7. 主合成 / 入口 ----------------------------------------------------

.ghs_render <- function(master, fig_dir, widget_dir, programs_dir, models_dir,
                        mode, repo_url, project_url) {
  s <- .ghs_summary(master)
  pngs <- list.files(fig_dir, pattern = "[.]png$", full.names = TRUE)
  htmls <- list.files(widget_dir, pattern = "[.]html$", full.names = TRUE)
  body <- paste0(
    .ghs_hero(s, project_url, length(pngs), length(htmls)),
    "<main>",
    .ghs_findings(master, fig_dir, programs_dir, models_dir),
    .ghs_methods_section(programs_dir),
    .ghs_gallery(fig_dir),
    .ghs_widgets(widget_dir, mode, repo_url),
    .ghs_repro(repo_url),
    .ghs_conclusion(s),
    "</main>",
    .ghs_footer(),
    "<div class='modal' id='fig-modal' onclick='closeFigure()'><button type='button'>\u5173\u95ed</button><div class='modal-title' id='modal-title'></div><img id='modal-img' alt='figure preview'></div>"
  )
  sprintf(
    "<!doctype html><html lang='zh-CN'><head><meta charset='utf-8'><meta name='viewport' content='width=device-width,initial-scale=1'><title>\u5168\u7403\u536b\u751f\u652f\u51fa 2000\u20132023 \u00b7 \u5e84\u9882 20241334</title><meta name='description' content='Global Health Spending 2000\u20132023 integrated analysis with R code and deep findings.'><style>%s</style></head><body>%s<script>%s</script></body></html>",
    .ghs_css(), body, .ghs_js()
  )
}

.ghs_write_rmd <- function(path) {
  lines <- c(
    "---",
    "title: \"\u5168\u7403\u536b\u751f\u652f\u51fa 2000\u20132023\uff1a\u6574\u5408\u5206\u6790\u4e0e\u6df1\u5ea6\u53d1\u73b0\"",
    "subtitle: \"\u5e84\u9882\uff0820241334\uff09\u00b7 R \u5927\u4f5c\u4e1a\u00b7 GHED + WDI\"",
    "author: \"\u5e84\u9882 20241334\"",
    "date: \"`r format(Sys.Date(), '%Y-%m-%d')`\"",
    "output:",
    "  html_document:",
    "    self_contained: true",
    "    toc: false",
    "---",
    "",
    "```{r setup, include=FALSE}",
    "knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)",
    "root <- normalizePath(file.path(dirname(knitr::current_input(dir = TRUE)), '..'), mustWork = FALSE)",
    "for (f in list.files(file.path(root, '\u7a0b\u5e8f'), pattern = '\\\\.R$', full.names = TRUE)) source(f, encoding = 'UTF-8')",
    "```",
    "",
    "## \u63d0\u4ea4\u8bf4\u660e",
    "",
    "\u672c Rmd \u662f\u8bfe\u7a0b\u63d0\u4ea4\u7684\u552f\u4e00\u5165\u53e3\uff0cknit \u540e\u4f1a\u8c03\u7528 `generate_static_showcase()` \u751f\u6210\u4e24\u4efd\u540c\u6e90\u4ea7\u7269\uff1a",
    "",
    "1. `\u8bfe\u7a0b\u63d0\u4ea4/\u5e84\u9882_20241334.html` \u2014 \u8bfe\u7a0b\u63d0\u4ea4\u7248\uff08\u56fe\u8868 base64 \u5185\u5d4c\uff0c\u4ea4\u4e92\u7ec4\u4ef6\u8d70\u76f8\u5bf9\u8def\u5f84/\u8fdc\u7a0b\u8fde\u6388\uff09",
    "2. `\u7f51\u7ad9\u53d1\u5e03/index.html` \u2014 GitHub Pages \u9996\u9875\uff08\u4e0e\u63d0\u4ea4\u7248\u540c\u6e90\uff09",
    "",
    "\u4e24\u4efd\u4ea7\u7269\u540c\u6e90\u4e8e\u4e00\u5957\u751f\u6210\u5668 `\u7a0b\u5e8f/21_static_showcase.R`\u3002\u9875\u9762\u542b\uff1a8 \u9879\u6838\u5fc3\u53d1\u73b0\uff08\u6bcf\u9879\u90fd\u6709\u7814\u7a76\u95ee\u9898\u3001\u65b9\u6cd5\u3001R \u4ee3\u7801\u3001\u539f\u56fe\u3001\u8868\u683c\u3001\u6df1\u5ea6\u89e3\u8bfb\uff09 \u00b7 \u6570\u636e\u4e0e\u65b9\u6cd5\u00b7 44 \u5f20\u9759\u6001\u56fe\u00b7 24 \u4e2a\u4ea4\u4e92\u7ec4\u4ef6\u00b7 \u590d\u73b0\u8bf4\u660e\u00b7 \u7ed3\u8bba\u4e0e\u653f\u7b56\u3002",
    "",
    "## \u4e00\u952e\u751f\u6210",
    "",
    "```{r build, results='hide'}",
    "out <- generate_static_showcase(root = root)",
    "knitr::kable(data.frame(\u4ea7\u7269 = basename(out), \u8def\u5f84 = out))",
    "```",
    "",
    "## \u73b0\u573a\u5d4c\u5165\u9884\u89c8",
    "",
    "```{r preview, echo=FALSE, results='asis'}",
    "html_path <- file.path(root, '\u8bfe\u7a0b\u63d0\u4ea4', '\u5e84\u9882_20241334.html')",
    "if (file.exists(html_path)) {",
    "  cat(sprintf(\"\u63d0\u4ea4\u4e3b\u4ef6\u5df2\u751f\u6210\uff1a `%s` \uff08%.1f MB\uff09\u3002\u5728\u6d4f\u89c8\u5668\u6253\u5f00\u67e5\u770b\u5b8c\u6574\u5185\u5bb9\u3002\\n\\n\",",
    "                html_path, file.info(html_path)$size / 1024^2))",
    "} else {",
    "  cat(\"\u8bf7\u5148\u8fd0\u884c `Rscript \u6784\u5efa.R submission` \u751f\u6210 HTML\u3002\\n\\n\")",
    "}",
    "```",
    "",
    "## \u590d\u73b0\u547d\u4ee4",
    "",
    "```bash",
    "# \u5b89\u88c5\u4f9d\u8d56\u00b7\u6784\u5efa\u6570\u636e\u4e0e\u4ea7\u7269",
    "Rscript \u5b89\u88c5\u4f9d\u8d56.R",
    "Rscript \u6784\u5efa.R data",
    "Rscript \u6784\u5efa.R figures",
    "Rscript \u6784\u5efa.R widgets",
    "Rscript \u6784\u5efa.R models",
    "Rscript \u6784\u5efa.R submission     # \u751f\u6210\u8bfe\u7a0b\u63d0\u4ea4 HTML \u4e0e GitHub Pages \u9996\u9875",
    "Rscript \u542f\u52a8\u4eea\u8868\u76d8.R 4848 # \u672c\u5730\u8fd0\u884c Shiny \u4eea\u8868\u76d8",
    "```"
  )
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, path, useBytes = TRUE)
  invisible(path)
}

generate_static_showcase <- function(root = NULL,
                                     repo_url = "https://github.com/2711944586/R",
                                     project_url = "https://github.com/2711944586/R",
                                     write_rmd = TRUE) {
  if (is.null(root)) {
    root <- if (exists("proj_root", mode = "function")) proj_root() else getwd()
  }
  root <- normalizePath(root, mustWork = FALSE)
  master_path <- file.path(root, "\u6d3e\u751f\u6570\u636e", "\u5904\u7406\u7ed3\u679c", "master_enriched.rds")
  if (!file.exists(master_path))
    stop("Missing master cache: ", master_path,
         "\nRun: Rscript \u6784\u5efa.R data")
  master <- readRDS(master_path)
  fig_dir <- file.path(root, "\u5206\u6790\u8f93\u51fa", "\u56fe\u8868")
  widget_dir <- file.path(root, "\u5206\u6790\u8f93\u51fa", "\u4ea4\u4e92\u7ec4\u4ef6")
  programs_dir <- file.path(root, "\u7a0b\u5e8f")
  models_dir <- file.path(root, "\u5206\u6790\u8f93\u51fa", "\u6a21\u578b\u8868")
  if (!length(list.files(fig_dir, pattern = "[.]png$")))
    stop("No PNG figures at: ", fig_dir, "\nRun: Rscript \u6784\u5efa.R figures")
  if (!length(list.files(widget_dir, pattern = "[.]html$")))
    stop("No widgets at: ", widget_dir, "\nRun: Rscript \u6784\u5efa.R widgets")

  out_submission <- file.path(root, "\u8bfe\u7a0b\u63d0\u4ea4", "\u5e84\u9882_20241334.html")
  out_publish <- file.path(root, "\u7f51\u7ad9\u53d1\u5e03", "index.html")
  dir.create(dirname(out_submission), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(out_publish), recursive = TRUE, showWarnings = FALSE)

  html_submission <- .ghs_render(master, fig_dir, widget_dir, programs_dir,
                                  models_dir, mode = "submission",
                                  repo_url = repo_url, project_url = project_url)
  html_publish <- .ghs_render(master, fig_dir, widget_dir, programs_dir,
                                models_dir, mode = "publish",
                                repo_url = repo_url, project_url = project_url)
  writeLines(html_submission, out_submission, useBytes = TRUE)
  writeLines(html_publish,    out_publish,    useBytes = TRUE)
  if (isTRUE(write_rmd)) {
    .ghs_write_rmd(file.path(root, "\u8bfe\u7a0b\u63d0\u4ea4", "\u5e84\u9882_20241334.Rmd"))
  }
  cat("[showcase] \u5df2\u751f\u6210\u552f\u4e00\u4e24\u4efd HTML\uff1a\n")
  cat("  - ", out_submission, sprintf(" (%.1f MB)\n",
        file.info(out_submission)$size / 1024^2), sep = "")
  cat("  - ", out_publish, sprintf(" (%.1f MB)\n",
        file.info(out_publish)$size / 1024^2), sep = "")
  invisible(c(out_submission, out_publish))
}
