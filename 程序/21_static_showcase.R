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

.ghs_w_mean <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (!any(ok)) return(NA_real_)
  stats::weighted.mean(x[ok], w[ok])
}

.ghs_w_quantile <- function(x, w, p = 0.5) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (!any(ok)) return(NA_real_)
  o <- order(x[ok]); xv <- x[ok][o]; wv <- w[ok][o]
  cw <- cumsum(wv) / sum(wv)
  xv[which(cw >= p)[1]]
}

.ghs_summary <- function(master) {
  mst <- master[is.finite(master$year), ]
  cur <- max(mst$year, na.rm = TRUE); base <- min(mst$year, na.rm = TRUE)
  prev <- cur - 1L
  d_cur  <- mst[mst$year == cur,  ]
  d_base <- mst[mst$year == base, ]
  d_prev <- mst[mst$year == prev, ]
  che_cur  <- sum(d_cur$che_usd2023,  na.rm = TRUE)
  che_base <- sum(d_base$che_usd2023, na.rm = TRUE)
  che_prev <- sum(d_prev$che_usd2023, na.rm = TRUE)
  span <- max(1L, cur - base)
  pop_cur  <- sum(d_cur$pop, na.rm = TRUE)
  gdp_cur  <- if ("gdp_usd" %in% names(d_cur)) sum(d_cur$gdp_usd, na.rm = TRUE) else NA_real_
  che_share_gdp_cur <- if (is.finite(gdp_cur) && gdp_cur > 0) che_cur / gdp_cur * 100 else NA_real_
  oops_p10 <- .ghs_w_quantile(d_cur$hf3_che, d_cur$pop, 0.10)
  oops_p90 <- .ghs_w_quantile(d_cur$hf3_che, d_cur$pop, 0.90)
  che_pc_p10 <- .ghs_w_quantile(d_cur$che_pc_usd2023, d_cur$pop, 0.10)
  che_pc_p90 <- .ghs_w_quantile(d_cur$che_pc_usd2023, d_cur$pop, 0.90)
  list(
    n_country = length(unique(mst$iso3_code)),
    base_year = base, cur_year = cur, prev_year = prev, span = span,
    pop_cur = pop_cur, gdp_cur = gdp_cur,
    che_total_cur = che_cur, che_total_base = che_base, che_total_prev = che_prev,
    che_total_growth = (che_cur / che_base)^(1 / span) - 1,
    che_yoy = if (is.finite(che_prev) && che_prev > 0) che_cur / che_prev - 1 else NA_real_,
    che_share_gdp_cur = che_share_gdp_cur,
    che_pc_cur  = .ghs_w_mean(d_cur$che_pc_usd2023, d_cur$pop),
    che_pc_base = .ghs_w_mean(d_base$che_pc_usd2023, d_base$pop),
    che_pc_p10 = che_pc_p10, che_pc_p90 = che_pc_p90,
    che_pc_ratio_p90_p10 = if (is.finite(che_pc_p10) && che_pc_p10 > 0)
      che_pc_p90 / che_pc_p10 else NA_real_,
    oops_mean_cur  = mean(d_cur$hf3_che,  na.rm = TRUE),
    oops_mean_base = mean(d_base$hf3_che, na.rm = TRUE),
    oops_w_cur     = .ghs_w_mean(d_cur$hf3_che, d_cur$pop),
    oops_p10 = oops_p10, oops_p90 = oops_p90,
    oops_high_cur  = sum(d_cur$hf3_che > 50, na.rm = TRUE),
    oops_low_cur   = sum(d_cur$hf3_che < 15, na.rm = TRUE),
    gghed_mean_cur = mean(d_cur$gghed_che, na.rm = TRUE),
    gghed_w_cur    = .ghs_w_mean(d_cur$gghed_che, d_cur$pop),
    pvtd_mean_cur  = mean(d_cur$pvtd_che, na.rm = TRUE),
    ext_mean_cur   = mean(d_cur$ext_che,  na.rm = TRUE),
    ext_high_cur   = sum(d_cur$ext_che > 20, na.rm = TRUE),
    n_continent    = length(unique(stats::na.omit(d_cur$continent)))
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

.ghs_continent_panel <- function(master) {
  yr <- max(master$year, na.rm = TRUE)
  d <- master[master$year == yr, ]
  if (!nrow(d) || !"continent" %in% names(d)) return(NULL)
  d <- d[is.finite(d$che_usd2023) & nzchar(d$continent), ]
  spl <- split(d, d$continent)
  rows <- lapply(names(spl), function(k) {
    x <- spl[[k]]
    out_row <- data.frame(
      v_continent  = k,
      v_n          = nrow(x),
      v_pop        = sum(x$pop, na.rm = TRUE),
      v_che_pc     = .ghs_w_mean(x$che_pc_usd2023, x$pop),
      v_oops       = .ghs_w_mean(x$hf3_che,        x$pop),
      v_gghed      = .ghs_w_mean(x$gghed_che,      x$pop),
      v_ext        = .ghs_w_mean(x$ext_che,        x$pop),
      stringsAsFactors = FALSE
    )
    names(out_row) <- c("\u5927\u6d32", "\u56fd\u5bb6\u6570",
                        "\u4eba\u53e3", "\u4eba\u5747 CHE",
                        "OOPS \u5747\u503c", "GGHED \u5747\u503c",
                        "EXT \u5747\u503c")
    out_row
  })
  out <- do.call(rbind, rows)
  out[order(-out[["\u4eba\u5747 CHE"]]), ]
}

.ghs_country_panel <- function(master, iso3, years = NULL) {
  d <- master[master$iso3_code == iso3, ]
  if (!nrow(d)) return(NULL)
  if (!is.null(years)) d <- d[d$year %in% years, ]
  cols <- c("year", "che_pc_usd2023", "che_usd2023",
            "gghed_che", "pvtd_che", "ext_che", "hf3_che", "pop")
  d <- d[, intersect(cols, names(d)), drop = FALSE]
  d <- d[order(d$year), ]
  d
}

.ghs_country_brief <- function(master, iso3) {
  d <- master[master$iso3_code == iso3, ]
  if (!nrow(d)) return(NULL)
  cur <- max(d$year, na.rm = TRUE); base <- min(d$year, na.rm = TRUE)
  span <- max(1L, cur - base)
  cur_row <- d[d$year == cur, ][1, , drop = FALSE]
  base_row <- d[d$year == base, ][1, , drop = FALSE]
  che_g <- if (is.finite(cur_row$che_usd2023) && is.finite(base_row$che_usd2023) &&
               base_row$che_usd2023 > 0)
    (cur_row$che_usd2023 / base_row$che_usd2023)^(1 / span) - 1 else NA_real_
  list(
    iso3 = iso3,
    name = cur_row$country_name,
    continent = cur_row$continent,
    pop = cur_row$pop,
    che_pc_cur = cur_row$che_pc_usd2023,
    che_pc_base = base_row$che_pc_usd2023,
    che_total_cur = cur_row$che_usd2023,
    che_g = che_g,
    oops_cur = cur_row$hf3_che,
    oops_base = base_row$hf3_che,
    gghed_cur = cur_row$gghed_che,
    pvtd_cur = cur_row$pvtd_che,
    ext_cur  = cur_row$ext_che,
    cur_year = cur, base_year = base
  )
}

.ghs_dq_table <- function(models_dir, file, max_rows = 10) {
  p <- file.path(models_dir, file)
  if (!file.exists(p)) return(NULL)
  utils::read.csv(p, check.names = FALSE)
}

.ghs_oops_quintile <- function(master) {
  yr <- max(master$year, na.rm = TRUE)
  d <- master[master$year == yr & is.finite(master$hf3_che) & is.finite(master$pop), ]
  if (!nrow(d)) return(NULL)
  d <- d[order(d$hf3_che), ]
  d$cum_pop <- cumsum(d$pop) / sum(d$pop)
  d$quintile <- cut(d$cum_pop, breaks = seq(0, 1, 0.2),
                    include.lowest = TRUE, labels = paste0("Q", 1:5))
  agg <- stats::aggregate(
    cbind(hf3_che, gghed_che, ext_che, che_pc_usd2023, pop) ~ quintile,
    data = d, FUN = function(x) mean(x, na.rm = TRUE))
  agg
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
      "<div class='top-bar'><div id='read-progress'></div></div>",
      "<header class='hero' id='top'><div class='wrap nav'>",
      "<a class='brand' href='#top'>GHS \u00b7 2000\u20132023</a>",
      "<nav class='links'>",
      "<a href='#executive'>\u6458\u8981</a>",
      "<a href='#kpi'>KPI</a>",
      "<a href='#methods'>\u65b9\u6cd5</a>",
      "<a href='#findings'>\u53d1\u73b0</a>",
      "<a href='#countries'>\u56fd\u5bb6</a>",
      "<a href='#regional'>\u533a\u57df</a>",
      "<a href='#simulator'>\u4eff\u771f</a>",
      "<a href='#gallery'>\u56fe\u5e93</a>",
      "<a href='#widgets'>\u4ea4\u4e92</a>",
      "<a href='#repro'>\u590d\u73b0</a>",
      "<a href='#conclusion'>\u7ed3\u8bba</a>",
      "</nav>",
      "<button class='theme-toggle' type='button' onclick='toggleTheme()' title='\u5207\u6362\u4e3b\u9898'>\u2600</button>",
      "</div>",
      "<div class='wrap hero-grid'>",
      "<div><span class='eyebrow'>Final integrated deliverable \u00b7 22 \u8282 \u00b7 10 \u9879\u53d1\u73b0 \u00b7 \u5355\u4e00\u6807\u51c6</span>",
      "<h1>\u5168\u7403\u536b\u751f\u652f\u51fa 2000\u20132023\uff1a",
      "<span class='accent'>\u516c\u5e73\u3001\u97e7\u6027\u3001\u672a\u6765</span></h1>",
      "<p class='hero-lead'>\u672c\u9875\u6574\u5408 GHED + WDI \u957f\u9762\u677f\u7684\u6570\u636e\u5de5\u7a0b\u3001\u7edf\u8ba1\u5efa\u6a21\u3001\u53ef\u89c6\u5316\u3001\u4eff\u771f\u4e0e\u90e8\u7f72\uff1a22 \u8282\u5185\u5bb9\u6db5\u76d6 \u6458\u8981 \u00b7 KPI \u00b7 \u65b9\u6cd5 \u00b7 \u6570\u636e\u8d28\u91cf \u00b7 \u53d8\u91cf\u5b57\u5178 \u00b7 10 \u9879\u53d1\u73b0 \u00b7 \u56fd\u5bb6\u6863\u6848 \u00b7 \u533a\u57df\u805a\u7126 \u00b7 \u4e0d\u5e73\u7b49\u56fe\u518c \u00b7 \u653f\u7b56\u4eff\u771f\u5668 \u00b7 \u7a33\u5065\u6027 \u00b7 44 \u5f20\u9759\u6001\u56fe \u00b7 24 \u4ea4\u4e92\u7ec4\u4ef6 \u00b7 \u8bcd\u6c47 \u00b7 \u590d\u73b0\u00b7\u7ed3\u8bba\u3002</p>",
      "<div class='hero-actions'>",
      "<a class='btn' href='#executive'>90 \u79d2\u8bfb\u6458\u8981 \u2192</a>",
      "<a class='btn alt' href='#findings'>\u8df3\u5230 10 \u9879\u53d1\u73b0</a>",
      "<a class='btn ghost' href='%s'>GitHub \u4ed3\u5e93</a>",
      "<a class='btn ghost' href='./\u4eea\u8868\u76d8/'>\u6d4f\u89c8\u5668\u5185 Shiny</a></div></div>",
      "<aside class='hero-panel'><span class='hero-panel-tag'>\u9875\u9762\u6784\u6210</span>",
      "<ul class='hero-panel-list'>",
      "<li><b>%d</b><span>\u56fd\u5bb6/\u5730\u533a \u00b7 %d \u5e74\u9762\u677f</span></li>",
      "<li><b>%d</b><span>\u9759\u6001\u56fe\u8868</span></li>",
      "<li><b>%d</b><span>\u4ea4\u4e92\u7ec4\u4ef6</span></li>",
      "<li><b>10</b><span>\u6df1\u5ea6\u53d1\u73b0 + \u5b8c\u6574 R \u4ee3\u7801</span></li>",
      "<li><b>22</b><span>\u8282\u00b7\u4ece\u6458\u8981\u5230\u4eff\u771f</span></li>",
      "</ul></aside></div></header>"
    ),
    .ghs_e(project_url), s$n_country, s$span, n_fig, n_widget
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
             paste0("USD 2023\uff1b", s$base_year, " \u4e3a ", .ghs_m(s$che_total_base))),
    .ghs_kpi(.ghs_n(s$che_total_growth * 100, 2, "%"),
             "CHE \u5e74\u5316\u589e\u901f",
             paste0(s$base_year, "\u2013", s$cur_year, " \u590d\u5408\u589e\u957f\u7387")),
    .ghs_kpi(.ghs_n(s$che_yoy * 100, 2, "%"),
             paste0(s$cur_year, " \u5e74\u540c\u6bd4"),
             paste0("\u4e0e ", s$prev_year, " \u5e74\u603b\u989d\u5bf9\u6bd4")),
    .ghs_kpi(.ghs_m(s$che_pc_cur),
             paste0(s$cur_year, " \u4eba\u5747 CHE"),
             paste0(s$base_year, " \u4e3a ", .ghs_m(s$che_pc_base),
                    "\uff1bP90/P10 = ", .ghs_n(s$che_pc_ratio_p90_p10, 1, "x"))),
    .ghs_kpi(.ghs_n(s$oops_w_cur, 1, "%"),
             paste0(s$cur_year, " \u4eba\u53e3\u52a0\u6743 OOPS"),
             paste0("P10 = ", .ghs_n(s$oops_p10, 1, "%"),
                    "\uff1bP90 = ", .ghs_n(s$oops_p90, 1, "%"))),
    .ghs_kpi(.ghs_n(s$oops_high_cur, 0),
             "OOPS > 50% \u56fd\u5bb6",
             paste0(.ghs_n(s$oops_low_cur, 0),
                    " \u4e2a\u56fd\u5bb6 OOPS < 15%\uff1b", s$cur_year)),
    .ghs_kpi(.ghs_n(s$gghed_w_cur, 1, "%"),
             paste0(s$cur_year, " \u4eba\u53e3\u52a0\u6743 GGHED"),
             "\u653f\u5e9c\u5f3a\u5236\u7b79\u8d44\u5360 CHE"),
    .ghs_kpi(.ghs_n(s$ext_high_cur, 0),
             "EXT > 20% \u56fd\u5bb6",
             paste0("\u5916\u63f4\u9ad8\u4f9d\u8d56\u4e2d/\u4f4e\u6536\u5165\uff1b", s$cur_year)),
    .ghs_kpi(.ghs_n(n_fig, 0), "\u9759\u6001\u56fe\u8868",
             "ggplot2 + v2 \u5347\u7ea7"),
    .ghs_kpi(.ghs_n(n_widget, 0), "\u4ea4\u4e92\u7ec4\u4ef6",
             "plotly / leaflet / reactable / DT")
  )
  sprintf("<section class='section kpi-section' id='kpi'><div class='wrap'><header class='section-head'><span class='kicker'>02 \u00b7 Snapshot</span><h2>\u4e00\u9875\u5927\u5c40\uff1a12 \u5f20 KPI \u5361\u7247</h2><p class='lead'>\u4ee5\u4e0b 12 \u5f20\u5361\u7247\u4ece\u603b\u91cf\u3001\u589e\u901f\u3001\u4eba\u5747\u3001\u8d22\u52a1\u4fdd\u62a4\u3001\u8d22\u653f\u7a7a\u95f4\u3001\u5916\u90e8\u4f9d\u8d56\u4e94\u4e2a\u7ef4\u5ea6\u7ed9\u51fa\u6240\u6709\u53d1\u73b0\u7684\u5f00\u573a\u6570\u503c\uff1b\u4e0b\u6587 F1\u2013F10 \u4f1a\u9010\u4e00\u63ed\u793a\u5176\u80cc\u540e\u7684\u65b9\u6cd5\u4e0e\u4ee3\u7801\u3002</p></header><div class='kpi-grid'>%s</div></div></section>",
          cards)
}

# ---- 3b. Executive summary (S01) ----------------------------------------

.ghs_executive <- function(s) {
  big <- function(num, label, hint, tone = "blue") {
    sprintf("<article class='exec-big tone-%s'><div class='exec-num'>%s</div><div class='exec-label'>%s</div><div class='exec-hint'>%s</div></article>",
            .ghs_e(tone), .ghs_e(num), .ghs_e(label), .ghs_e(hint))
  }
  bigs <- paste0(
    big(.ghs_m(s$che_total_cur),
        sprintf("%d \u5e74\u5168\u7403\u536b\u751f\u603b\u652f\u51fa", s$cur_year),
        sprintf("\u5e74\u5316 %s\uff1b\u4eba\u5747 %s",
                .ghs_n(s$che_total_growth * 100, 2, "%"),
                .ghs_m(s$che_pc_cur)), "blue"),
    big(.ghs_n(s$oops_w_cur, 1, "%"),
        "\u4eba\u53e3\u52a0\u6743 OOPS \u5747\u503c",
        sprintf("P90/P10 \u4eba\u5747 CHE \u8d2b\u5bcc\u6bd4 = %s",
                .ghs_n(s$che_pc_ratio_p90_p10, 1, "x")), "orange"),
    big(.ghs_n(s$oops_high_cur, 0),
        sprintf("%d \u5e74\u4ecd\u6709 OOPS &gt; 50%% \u56fd\u5bb6", s$cur_year),
        sprintf("\u53ea\u6709 %s \u4e2a\u56fd\u5bb6 OOPS &lt; 15%%",
                .ghs_n(s$oops_low_cur, 0)), "ink"),
    big(.ghs_n(s$ext_high_cur, 0),
        "\u9ad8\u5916\u63f4\u4f9d\u8d56\u56fd\u5bb6 (EXT &gt; 20%)",
        "\u51e0\u4e4e\u5168\u90e8\u96c6\u4e2d\u5728 LIC \u4e0e LMIC", "blue")
  )
  tldr <- paste0(
    "<ol class='tldr-list'>",
    "<li><b>\u603b\u91cf\u4e0a\u96c6\u4e2d\u5728\u9ad8\u6536\u5165\u56fd\u5bb6</b>\uff1a\u5168\u7403 ", s$cur_year, " \u5e74 CHE \u8fbe ", .ghs_m(s$che_total_cur),
    "\uff0c\u4f46\u4eba\u5747 P90/P10 \u5dee\u5f02\u9ad8\u8fbe ", .ghs_n(s$che_pc_ratio_p90_p10, 1, "x"), "\u3002</li>",
    "<li><b>\u8d22\u52a1\u4fdd\u62a4\u4ecd\u4e0d\u5145\u5206</b>\uff1a", .ghs_n(s$oops_high_cur, 0),
    " \u4e2a\u56fd\u5bb6\u5c45\u6c11\u81ea\u4ed8\u5360 CHE \u8d85 50%\uff1b OOPS \u4e0e GGHED \u9ad8\u5ea6\u8d1f\u76f8\u5173\uff08F2\uff09\u3002</li>",
    "<li><b>\u4e0d\u5e73\u7b49\u4e0b\u964d\u4f46\u7edd\u5bf9\u6c34\u5e73\u4ecd\u9ad8</b>\uff1a\u4eba\u53e3\u52a0\u6743 Gini \u7531 0.81 \u964d\u81f3 0.77\uff0c\u4f46 0.77 \u5728\u8de8\u56fd\u6536\u5165\u5206\u914d\u4e2d\u5904\u4e8e\u6781\u7aef\u533a\u95f4\uff08F3\uff09\u3002</li>",
    "<li><b>COVID-19 \u51b2\u51fb\u4e0b\u4e24\u7c7b\u8f68\u8ff9</b>\uff1a\u591a\u6570 OECD CHE \u62ac\u5347 + OOPS \u4e0b\u964d\uff1b\u90e8\u5206\u4e2d\u4f4e\u6536\u5165\u56fd\u5bb6 CHE \u62ac\u5347\u4f46 OOPS \u540c\u6b65\u4e0a\u5347\uff08F4\uff09\u3002</li>",
    "<li><b>\u8ffd\u8d76\u4e0d\u662f\u81ea\u52a8\u7684</b>\uff1a\u03b2-\u6536\u655b\u6210\u7acb\u4f46\u534a\u6536\u655b\u5e74\u8de8\u5927\u6d32\u5dee\u5f02\u663e\u8457\uff1b\u53cc\u5411 FE \u5f39\u6027\u7ea6 0.8 < 1\uff0c\u4ec5\u9760 GDP \u589e\u957f\u4e0d\u8db3\u4ee5\u9a71\u52a8 UHC\uff08F5\u3001F6\uff09\u3002</li>",
    "</ol>"
  )
  sprintf("<section class='section executive' id='executive'><div class='wrap'><header class='section-head'><span class='kicker'>01 \u00b7 Executive Summary</span><h2>\u6458\u8981 \u00b7 \u4e94\u53e5\u8bdd\u8bfb\u5b8c\u672c\u9879\u76ee</h2><p class='lead'>\u672c\u8282\u4ee5 4 \u5f20\u5927\u5b57\u6570\u5b57\u5361\u7247 + 5 \u6761 TLDR \u7ed3\u8bba\u63d0\u4f9b\u4e00\u4e2a 90 \u79d2\u5185\u53ef\u8bfb\u5b8c\u7684\u9879\u76ee\u603b\u89c8\uff1b\u4e0b\u6587\u662f\u5176\u80cc\u540e\u7684\u4ee3\u7801\u4e0e\u8bc1\u636e\u3002</p></header><div class='exec-big-grid'>%s</div><div class='exec-tldr'><span class='kicker'>TLDR \u00b7 \u6838\u5fc3\u53d1\u73b0</span>%s<a class='btn-light' href='#findings'>\u8df3\u5230 10 \u9879\u53d1\u73b0 \u2193</a></div></div></section>",
          bigs, tldr)
}

# ---- 3c. Data Quality (S04) + Codebook (S05) ----------------------------

.ghs_data_quality <- function(models_dir) {
  smry <- .ghs_dq_table(models_dir, "data_quality_summary.csv")
  by_var <- .ghs_dq_table(models_dir, "data_quality_missing_by_var.csv")
  by_inc <- .ghs_dq_table(models_dir, "data_quality_missing_by_income.csv")
  consis <- .ghs_dq_table(models_dir, "data_quality_consistency.csv")
  cards <- ""
  if (!is.null(smry) && nrow(smry))
    cards <- paste0(cards, .ghs_table(smry, "\u603b\u4f53\u8d28\u91cf\u6307\u6807", 8))
  if (!is.null(by_var) && nrow(by_var))
    cards <- paste0(cards, .ghs_table(by_var,
      "\u5404\u53d8\u91cf\u7f3a\u5931\u7387 (\u524d 12)", 12))
  if (!is.null(by_inc) && nrow(by_inc))
    cards <- paste0(cards, .ghs_table(by_inc,
      "\u6309\u6536\u5165\u7ec4\u7684\u7f3a\u5931", 8))
  if (!is.null(consis) && nrow(consis))
    cards <- paste0(cards, .ghs_table(consis,
      "\u4e00\u81f4\u6027\u68c0\u67e5\uff08\u603b\u989d \u2261 \u6765\u6e90\u4e4b\u548c\uff09", 8))
  if (!nzchar(cards))
    cards <- "<p class='muted'>\u672a\u68c0\u6d4b\u5230 data_quality_*.csv\uff0c\u8bf7\u5148 Rscript \u6784\u5efa.R models\u3002</p>"
  sprintf("<section class='section dq' id='dq'><div class='wrap'><header class='section-head'><span class='kicker'>04 \u00b7 Data Quality</span><h2>\u6570\u636e\u8d28\u91cf\u00b7\u7f3a\u5931\u00b7\u4e00\u81f4\u6027</h2><p class='lead'>\u6240\u6709\u540e\u7eed\u53d1\u73b0\u8865\u5145\u4e8e\u540c\u4e00\u4efd\u9762\u677f\uff1b\u672c\u8282\u4ee5 4 \u5f20\u8868\u5448\u73b0\u539f\u59cb\u8d28\u91cf\u8bca\u65ad\uff08\u51fa\u81ea <code>\u5206\u6790\u8f93\u51fa/\u6a21\u578b\u8868/data_quality_*.csv</code>\uff09\u3002</p></header><div class='dq-grid'>%s</div></div></section>",
          cards)
}

.ghs_codebook <- function() {
  rows <- list(
    c("iso3_code",        "ISO 3166-1 alpha-3",       "—",     "GHED + countrycode", "\u56fd\u5bb6\u4e3b\u952e"),
    c("country_name",     "\u56fd\u5bb6\u540d",         "—",     "GHED",                "\u4e2d\u82f1\u540d\u79f0"),
    c("continent",        "\u5927\u6d32",               "—",     "countrycode",         "\u806b\u5408\u533a\u57df\u805a\u5408"),
    c("year",             "\u5e74\u4efd",               "year",  "GHED panel",          "2000\u20132023"),
    c("che_usd2023",      "\u5f53\u5e74\u603b CHE",     "USD",   "GHED USD2023",        "\u4e0d\u53d8\u4ef7\u3001\u539f\u59cb\u603b\u989d"),
    c("che_pc_usd2023",   "\u4eba\u5747 CHE",            "USD",   "GHED + WDI pop",      "\u4e0d\u5e73\u7b49\u4e3b\u8981\u5e94\u53d8\u91cf"),
    c("hf3_che",          "OOPS \u5360 CHE",             "%",     "GHED HF",             "\u8d22\u52a1\u4fdd\u62a4\u53cd\u5411 KPI"),
    c("gghed_che",        "\u653f\u5e9c\u5f3a\u5236\u5360 CHE", "%", "GHED FS",          "GGHED \u5360\u603b\u989d\u4efd\u989d"),
    c("pvtd_che",         "\u79c1\u4eba\u575a\u6301\u5360 CHE", "%", "GHED FS",          "\u4ee5 PHI \u4e3a\u4e3b"),
    c("ext_che",          "\u5916\u63f4\u5360 CHE",       "%",     "GHED FS",             "\u4f9d\u8d56\u95e8\u69db"),
    c("hf1_che \u2026 hfnec_che","7 \u7c7b\u7b79\u8d44\u65b9\u6848","%","GHED HF",      "\u603b\u548c \u2261 100%"),
    c("pop",              "\u4eba\u53e3",                 "people","WDI",                 "\u52a0\u6743\u4f7f\u7528"),
    c("gdp_pc_usd",       "\u4eba\u5747 GDP",             "USD",   "WDI",                 "F6 \u53d8\u91cf"),
    c("life_exp",         "\u9884\u671f\u5bff\u547d",     "years", "WDI",                 "F\u8865 (\u672a\u4f7f\u7528)"),
    c("u5mr",             "5\u5c81\u4ee5\u4e0b\u513f\u7ae5\u6b7b\u4ea1\u7387", "/1000", "WDI", "\u7ed3\u679c\u53d8\u91cf")
  )
  df <- do.call(rbind, lapply(rows, function(r) {
    out <- data.frame(c1 = r[1], c2 = r[2], c3 = r[3],
                      c4 = r[4], c5 = r[5], stringsAsFactors = FALSE)
    names(out) <- c("\u53d8\u91cf", "\u63cf\u8ff0", "\u5355\u4f4d",
                    "\u6765\u6e90", "\u5907\u6ce8")
    out
  }))
  body <- .ghs_table(df,
    "master_enriched \u4e3b\u8981\u53d8\u91cf\u5b57\u5178", 20)
  sprintf("<section class='section codebook' id='codebook'><div class='wrap'><header class='section-head'><span class='kicker'>05 \u00b7 Codebook</span><h2>\u53d8\u91cf\u5b57\u5178\u4e0e\u53e3\u5f84\u8bf4\u660e</h2><p class='lead'>\u4ee5\u4e0b\u662f master \u5bbd\u8868\u4e3b\u8981\u5b57\u6bb5\u7684\u63cf\u8ff0\u3001\u5355\u4f4d\u4e0e\u6765\u6e90\uff1b\u6240\u6709\u540e\u7eed\u53d1\u73b0\u53ea\u8bfb\u53d6\u8be5\u8868\u3002</p></header>%s</div></section>",
          body)
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

# ---- 4. 十项发现 ---------------------------------------------------------

.ghs_widget_anchor <- function(widget_dir, base, label, mode = "submission",
                               repo_url = "https://github.com/2711944586/R") {
  p <- file.path(widget_dir, base)
  if (!file.exists(p)) return("")
  href_local <- if (mode == "submission")
    paste0("../\u5206\u6790\u8f93\u51fa/\u4ea4\u4e92\u7ec4\u4ef6/", base)
  else paste0("\u4ea4\u4e92\u7ec4\u4ef6/", base)
  href_remote <- sprintf(
    "%s/blob/main/\u5206\u6790\u8f93\u51fa/\u4ea4\u4e92\u7ec4\u4ef6/%s", repo_url, base)
  sprintf("<aside class='widget-link'><strong>\u4ea4\u4e92\u7ec4\u4ef6 \u00b7 %s</strong><div><a href='%s' target='_blank' rel='noreferrer'>\u672c\u5730\u9884\u89c8</a><a href='%s' target='_blank' rel='noreferrer'>GitHub \u9884\u89c8</a><a href='#widgets'>\u7ec4\u4ef6\u4e2d\u5fc3</a></div></aside>",
          .ghs_e(label), .ghs_e(href_local), .ghs_e(href_remote))
}

.ghs_limit <- function(...) {
  parts <- c(...); parts <- parts[nzchar(parts)]
  if (!length(parts)) return("")
  body <- paste0("<li>", parts, "</li>", collapse = "")
  sprintf("<aside class='limit-note'><strong>\u5c40\u9650 \u00b7 \u5c0f\u5fc3\u4e0d\u8981\u8bfb\u8fc7\u91cf</strong><ul>%s</ul></aside>", body)
}

.ghs_findings <- function(master, fig_dir, programs_dir, models_dir,
                          widget_dir = NULL, mode = "submission",
                          repo_url = "https://github.com/2711944586/R") {
  s <- .ghs_summary(master)
  if (is.null(widget_dir)) widget_dir <- file.path(dirname(fig_dir), "\u4ea4\u4e92\u7ec4\u4ef6")

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
      ), tone = "blue"),
    .ghs_widget_anchor(widget_dir, "02_highlight_ts.html",
      "跨国人均 CHE 高亮时序线", mode, repo_url),
    .ghs_limit(
      "全球加总受 USD2023 不变价调整，与各国本币发布口径存在领域差异。",
      "GGHED / PVTD / EXT 占比年代间不严格合计 100%（PVD 与 PHI 定义调整）。",
      "该趋势不识别因果，仅描述总量与结构的联动变化。"
    )
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
      ), tone = "orange"),
    .ghs_widget_anchor(widget_dir, "12_reactable_rank.html",
      "OOPS / GGHED / EXT 可排序表格", mode, repo_url),
    .ghs_limit(
      "OOPS 口径依赖于成本分摄调查，部分 LIC 可能偏下。",
      "外援 EXT 在人道紧急期会出现年度剧烈跳动。",
      "财务保护还受财政货币、医保覆盖面等未入面板变量影响。"
    )
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
      ), tone = "blue"),
    .ghs_widget_anchor(widget_dir, "07_inequality.html",
      "不平等三指标交互趋势", mode, repo_url),
    .ghs_limit(
      "Gini 与 Theil 在极端重尾分布下估计偏差受样本大小影响。",
      "Atkinson 取 ε = 0.5 与 ε = 1.0 反映不同偏好，需同时报呈。",
      "跨国不平等 ≠ 国内不平等；后者需 LSMS / DHS 微观样本。"
    )
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
      ), tone = "orange"),
    .ghs_widget_anchor(widget_dir, "v2_w_oops_heatmap.html",
      "OOPS 2019 vs 2020-2022 热力图", mode, repo_url),
    .ghs_limit(
      "2020–2022 均值与 2019 基线对比仅捕捉短期冲击。",
      "部分国家 2022–2023 数据仍为初估，存在修正风险。",
      "未及 COVID 主动付费后续调整（如报销、补贴、交叉补偿）。"
    )
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
      ), tone = "blue"),
    .ghs_widget_anchor(widget_dir, "v2_w_gapminder_bubble.html",
      "GDP × CHE_pc × 人口气泡 (gapminder)", mode, repo_url),
    .ghs_limit(
      "经典 β-收敛只描述平均趋势，心中与低位间距仍有开。",
      "continent 固定效应只控制领域面，未控制个体 / 制度面变量。",
      "起点 2000 年部分 LIC 存在数据缺失，需以 enrich 补齐。"
    )
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
      ), tone = "blue"),
    .ghs_widget_anchor(widget_dir, "v2_w_splom.html",
      "多变量散点矩阵 (SPLOM)", mode, repo_url),
    .ghs_limit(
      "FE 仅控面不可观测且不随时间变化的项，不代表因果。",
      "未加入人口老龄化、默克尔指数等可能控变量。",
      "聚类标准误与年代限制为推断设计选择，可能偏低。"
    )
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
      ), tone = "ink"),
    .ghs_widget_anchor(widget_dir, "v2_w_country_network.html",
      "PCA 附近联网·国家 archetype", mode, repo_url),
    .ghs_limit(
      "PCA 主成分载荷依赖变量选择；加入 / 去除 EXT 与 PVTD 会改变双众 plot。",
      "k = 4 为使用 elbow + silhouette 联合选取；不代表唯一能选择。",
      "聚类仅描述截面状态，动态转移路径需 longitudinal cluster。"
    )
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
      ), tone = "blue"),
    .ghs_widget_anchor(widget_dir, "06_forecast_subplot.html",
      "4 国 ARIMA 交互子图", mode, repo_url),
    .ghs_limit(
      "ARIMA 是单变量时序，不控 GDP · 财政 · 老龄化等驱动项。",
      "5 年以上外推使 80% 区间快速发散，需谨慎引用。",
      "未插入重大冲击（另一大流行、金融危机）的场景损动。"
    )
  )
  f8 <- .ghs_finding("f-forecast", "F8", "Forecast \u00b7 5 \u5e74\u5916\u63a8",
    "ARIMA \u9884\u6d4b\uff1a\u56db\u56fd 2024\u20132028",
    "auto.arima \u5bf9\u4e2d\u3001\u7f8e\u3001\u5370\u3001\u5df4\u7684\u4eba\u5747 CHE \u7ed9\u51fa\u77ed\u671f\u5ef6\u7eed\u4e0a\u5347\u7684\u5224\u65ad\u3002",
    f8_body, chips = fc_chips)

  ## ---- F9 援助依赖 ----
  yr <- s$cur_year
  d_aid <- master[master$year == yr & is.finite(master$ext_che), ]
  ext_top <- if (nrow(d_aid)) {
    o <- d_aid[order(-d_aid$ext_che), ][1:8, c("country_name", "iso3_code",
                                                "continent", "ext_che",
                                                "che_pc_usd2023", "gghed_che")]
    names(o) <- c("\u56fd\u5bb6/\u5730\u533a", "ISO3", "\u5927\u6d32",
                  "EXT \u5360 CHE (%)", "\u4eba\u5747 CHE",
                  "GGHED \u5360 CHE (%)")
    o
  } else NULL
  ext_chips <- paste0(
    .ghs_chip("EXT > 20% \u56fd\u5bb6", .ghs_n(s$ext_high_cur, 0), "orange"),
    .ghs_chip("EXT \u5747\u503c", .ghs_n(s$ext_mean_cur, 1, "%"), "blue"),
    .ghs_chip("\u5927\u6d32\u8986\u76d6", .ghs_n(s$n_continent, 0), "ink")
  )
  f9_code <- paste0(
    "library(dplyr)\n",
    "aid_rank <- master |>\n",
    "  dplyr::filter(year == max(year, na.rm = TRUE)) |>\n",
    "  dplyr::select(country_name, continent, ext_che, gghed_che, hf3_che, che_pc_usd2023) |>\n",
    "  dplyr::arrange(dplyr::desc(ext_che))\n",
    "head(aid_rank, 10)"
  )
  f9_body <- paste0(
    .ghs_method(.ghs_para(
      "<b>\u7814\u7a76\u95ee\u9898\uff1a</b>\u54ea\u4e9b\u56fd\u5bb6\u7684\u536b\u751f\u8d22\u52a1\u96be\u4ee5\u8131\u79bb\u5916\u63f4\uff1f\u5916\u63f4\u5360\u6bd4\u4e0e\u8d22\u52a1\u4fdd\u62a4\uff08OOPS\uff09\u662f\u5982\u4f55\u8054\u52a8\u7684\uff1f",
      "<b>\u65b9\u6cd5\uff1a</b>\u53d6 GHED financing source \u4e2d\u7684 EXT (External transfer schemes) \u5360 CHE \u4efd\u989d\uff0c\u8de8\u8054 OOPS / GGHED \u4e0e\u4eba\u5747 CHE\uff0c\u91cd\u70b9\u8bc6\u522b\u54ea\u4e9b LIC \u4ecd\u5904\u4e8e\u8f93\u8840\u578b\u9636\u6bb5\u3002"
    )),
    .ghs_code(f9_code, "r", "EXT > 20% \u9ad8\u4f9d\u8d56\u56fd\u5bb6\u6392\u884c"),
    .ghs_fig(file.path(fig_dir, "v2_aid_dependency.png"),
             sprintf("\u5916\u63f4\u4f9d\u8d56\u5ea6\u9762\u677f\uff08%d\uff09", yr),
             "Figure 9A \u00b7 EXT \u4f9d\u8d56\u5ea6"),
    .ghs_fig(file.path(fig_dir, "17_ext_density.png"),
             "EXT \u5360\u6bd4\u8de8\u56fd\u5206\u5e03\u5bc6\u5ea6",
             "Figure 9B \u00b7 EXT \u5206\u5e03"),
    if (!is.null(ext_top)) .ghs_table(ext_top,
      sprintf("EXT \u4f9d\u8d56\u5ea6\u6700\u9ad8\u7684 8 \u4e2a\u56fd\u5bb6 (%d)", yr), 8) else "",
    .ghs_callout("\u89e3\u8bfb \u00b7 \u5916\u63f4\u4ec5\u80fd\u6258\u5e95",
      .ghs_para(
        sprintf("<b>\u96c6\u4e2d\u4e8e\u4f4e\u6536\u5165\uff1a</b>%d \u5e74\u4ecd\u6709 %d \u4e2a\u56fd\u5bb6 EXT > 20%%\uff0c\u5168\u90e8\u4e3a LIC/LMIC\u3002",
                yr, s$ext_high_cur),
        "<b>\u4e0e OOPS \u5e76\u5b58\uff1a</b>\u9ad8 EXT \u56fd\u5bb6\u591a\u6570 OOPS \u540c\u6837\u504f\u9ad8\uff0c\u610f\u5473\u7740\u5916\u90e8\u8f93\u8840\u672a\u80fd\u6709\u6548\u51cf\u8f7b\u5c45\u6c11\u8d22\u52a1\u8d1f\u62c5\u3002",
        "<b>\u8def\u5f84\uff1a</b>\u9700\u540c\u6b65\u63a8\u52a8\u672c\u571f\u8d22\u653f\u5f3a\u5236\u6c47\u96c6\u3001\u793e\u533b\u9690\u6027\u4ef7\u3001PHC \u521d\u8bca\u8865\u507f\u3002"
      ), tone = "orange"),
    .ghs_widget_anchor(widget_dir, "v2_w_sankey_flows.html",
      "GGHED / PVTD / EXT / OOPS \u8d44\u91d1\u6d41 sankey", mode, repo_url),
    .ghs_limit(
      "EXT \u8c03\u67e5\u5b58\u5728\u8df3\u53d8\uff1b2020\u20132022 \u4eba\u9053\u54cd\u5e94\u62ac\u5347\u4f7f\u90e8\u5206\u56fd\u5bb6\u5916\u63f4\u4efd\u989d\u4e34\u65f6\u9ad8\u4f30\u3002",
      "GHED \u672a\u80fd\u5b8c\u5168\u8986\u76d6\u53cc\u8fb9\u00b7\u5168\u7403\u57fa\u91d1\u4e0e\u9886\u5907\u62a5\u9500\u4e0d\u5bf9\u79f0\u3002",
      "EXT \u5360\u6bd4\u4ec5\u53cd\u6620\u8d44\u91d1\u6765\u6e90\uff0c\u4e0d\u4ee3\u8868\u8d44\u91d1\u6548\u7387\u3002"
    )
  )
  f9 <- .ghs_finding("f-aid", "F9", "Aid \u00b7 \u5916\u63f4\u4f9d\u8d56",
    "\u8c01\u9760\u5916\u63f4\uff1a\u5916\u90e8\u4f9d\u8d56\u4e0e\u672c\u571f\u8d22\u653f\u7684\u4e8c\u5206\u6cd5",
    "EXT \u5360 CHE \u8d85\u8fc7 20% \u610f\u5473\u7740\u672c\u571f\u8d22\u653f\u96be\u4ee5\u72ec\u7acb\u652f\u6491\u7cfb\u7edf\uff1b\u8be5\u4fe1\u53f7\u4e0e OOPS \u5e76\u5b58\u63d0\u793a\u5c45\u6c11\u4ecd\u9700\u6309\u6b21\u4ed8\u8d39\u3002",
    f9_body, chips = ext_chips)

  ## ---- F10 效率前沿 ----
  d_eff <- master[master$year == yr &
                    is.finite(master$che_pc_usd2023) &
                    is.finite(master$life_exp), ]
  eff_chips <- ""
  eff_text <- ""
  eff_table_html <- ""
  if (nrow(d_eff)) {
    d_eff <- d_eff[, c("country_name", "iso3_code", "continent",
                       "che_pc_usd2023", "life_exp")]
    fit_eff <- tryCatch(stats::lm(life_exp ~ log(che_pc_usd2023), data = d_eff),
                        error = function(e) NULL)
    if (!is.null(fit_eff)) {
      co_eff <- stats::coef(fit_eff)
      r2_eff <- summary(fit_eff)$r.squared
      eff_chips <- paste0(
        .ghs_chip("\u62df\u5408\u659c\u7387",
                  .ghs_n(co_eff[2], 2), "blue"),
        .ghs_chip("R\u00b2", .ghs_n(r2_eff, 3), "ink"),
        .ghs_chip("\u53ef\u7528\u56fd\u5bb6", .ghs_n(nrow(d_eff), 0), "orange"))
      eff_text <- sprintf(
        "\u62df\u5408\u9884\u671f\u5bff\u547d ~ log(\u4eba\u5747 CHE)\uff1a\u4eba\u5747\u8d44\u91d1\u6bcf\u53d8\u4e3a e \u500d (\u22482.72 \u500d) \u8054\u52a8\u9884\u671f\u5bff\u547d\u589e\u52a0 %s \u5e74\uff0cR\u00b2 = %s\u3002",
        .ghs_n(co_eff[2], 2), .ghs_n(r2_eff, 3))
    }
    d_eff$resid <- if (!is.null(fit_eff)) stats::residuals(fit_eff) else NA_real_
    d_eff_show <- utils::head(d_eff[order(-d_eff$resid), ], 8)
    names(d_eff_show) <- c("\u56fd\u5bb6", "ISO3", "\u5927\u6d32",
                           "\u4eba\u5747 CHE", "\u9884\u671f\u5bff\u547d", "\u6b8b\u5dee")
    eff_table_html <- .ghs_table(d_eff_show,
      "\u9884\u671f\u5bff\u547d\u9ad8\u4e8e\u4eba\u5747 CHE \u9884\u6d4b\u6700\u591a\u7684 8 \u4e2a\u56fd\u5bb6\uff08\u9ad8\u6548\u7387\u8c61\u9650\uff09", 8, 3)
  }
  f10_code <- paste0(
    "library(dplyr)\n",
    "fit <- master |>\n",
    "  dplyr::filter(year == max(year, na.rm = TRUE),\n",
    "                is.finite(che_pc_usd2023), is.finite(life_exp)) |>\n",
    "  lm(life_exp ~ log(che_pc_usd2023), data = _)\n",
    "summary(fit)"
  )
  f10_body <- paste0(
    .ghs_method(.ghs_para(
      "<b>\u7814\u7a76\u95ee\u9898\uff1a</b>\u540c\u6837\u4eba\u5747 CHE \u4e0b\uff0c\u54ea\u4e9b\u56fd\u5bb6\u201c\u82b1\u5f97\u66f4\u52a0\u5212\u7b97\u201d\uff08\u9884\u671f\u5bff\u547d\u9ad8\u4e8e\u9884\u6d4b\u503c\uff09\uff1f",
      paste0("<b>\u65b9\u6cd5\uff1a</b>\u62df\u5408 \u9884\u671f\u5bff\u547d ~ log(\u4eba\u5747 CHE)\uff0c\u53d6\u6b63\u6b8b\u5dee\u4f5c\u4e3a\u201c\u8d77\u52a8\u6548\u7387\u9886\u5148\u8005\u201d\u4ee3\u7406\uff1b\u4e0e DEA \u8f93\u51fa\u4e0d\u540c\u4f46\u601d\u8def\u4e92\u8865\u3002 ",
             eff_text)
    )),
    .ghs_code(f10_code, "r", "\u201c\u82b1\u591a\u8d8a\u591a\u3001\u4f46\u4e0d\u4e00\u5b9a\u4e70\u5230\u5bff\u547d\u201d \u00b7 \u7b80\u6790\u62df\u5408"),
    .ghs_fig(file.path(fig_dir, "v2_efficiency_dea.png"),
             "DEA \u6548\u7387\u524d\u6cbf\uff1a\u4eba\u5747 CHE \u00b7 U5MR / \u9884\u671f\u5bff\u547d",
             "Figure 10A \u00b7 \u6548\u7387\u524d\u6cbf"),
    .ghs_fig(file.path(fig_dir, "v2_outcomes_elasticity.png"),
             "\u4eba\u5747 CHE \u4e0e \u9884\u671f\u5bff\u547d / U5MR \u7684\u5f39\u6027",
             "Figure 10B \u00b7 \u8f93\u5165 \u00b7 \u8f93\u51fa"),
    eff_table_html,
    .ghs_callout("\u89e3\u8bfb \u00b7 \u8d44\u91d1\u4e0d\u7b49\u4e8e\u6210\u6548",
      .ghs_para(
        "<b>\u53d8\u8861\uff1a</b>\u4eba\u5747 CHE \u53cc\u500d \u2248 \u589e\u52a0 4-5 \u5e74\u9884\u671f\u5bff\u547d\uff0c\u4f46\u8de8\u8d44\u91d1\u533a\u95f4\u9012\u51cf\u3002",
        "<b>\u9ad8\u6548\u7387\u9886\u5148\u8005\uff1a</b>\u53e4\u5df4\u3001\u8d8a\u5357\u3001\u6cf0\u56fd\u3001\u613e\u5f00\u8d77\u3001\u52a0\u52d2\u6bd4\u90e8\u5206\u5c0f\u56fd\u201c\u4f4e\u6295\u5165\u9ad8\u4ea7\u51fa\u201d\u3002",
        "<b>\u4f4e\u6548\u7387\u8001\u8db3\u4e0d\u624d\uff1a</b>\u90e8\u5206\u4e2d\u4e1c \u00b7 \u4e2d\u4e9a \u00b7 \u62c9\u7f8e \u4eba\u5747 CHE \u8f83\u9ad8\u4f46\u9884\u671f\u5bff\u547d\u4f4e\u4e8e\u9884\u6d4b\u3002"
      ), tone = "blue"),
    .ghs_widget_anchor(widget_dir, "v2_w_scenarios.html",
      "DEA \u00b7 ARIMA \u00b7 Lorenz \u8054\u52a8\u573a\u666f", mode, repo_url),
    .ghs_limit(
      "\u9884\u671f\u5bff\u547d\u662f\u591a\u56e0\u7d20\u7ed3\u679c\uff0c\u4e0d\u4ec5\u53d6\u51b3\u4e8e\u536b\u751f\u652f\u51fa\u3002",
      "\u8de8\u56fd\u5907\u4ef7\u53d7\u5230 PPP / \u7269\u4ef7\u00b7\u4eba\u53e3\u6784\u00b7\u96c6\u8fbe\u4ee3\u4ef7\u5dee\u5f02\u5f71\u54cd\u3002",
      "\u6298\u4e0d\u4f7f\u7528\u591a\u8f93\u5165 DEA\uff08\u4ec5 1 \u8f93\u5165 \u00b7 1 \u8f93\u51fa\uff09\u3002"
    )
  )
  f10 <- .ghs_finding("f-efficiency", "F10", "Efficiency \u00b7 \u8d44\u91d1 \u00b7 \u5bff\u547d",
    "\u540c\u6837\u4eba\u5747 CHE\uff0c\u8c01\u8d70\u5728\u6548\u7387\u524d\u6cbf\uff1f",
    "\u5728\u7ed9\u5b9a\u4eba\u5747\u8d44\u91d1\u4e0b\uff0c\u5404\u56fd\u9884\u671f\u5bff\u547d / U5MR \u7684\u5dee\u5f02\u53f3\u53cd\u6620\u4f53\u7cfb\u8d28\u91cf\u3001PHC \u3001\u793e\u533b\u8986\u76d6\u9762\u7b49\u975e\u8d44\u91d1\u56e0\u7d20\u3002",
    f10_body, chips = eff_chips)

  paste0(.ghs_kpi_grid(s, length(list.files(fig_dir, "[.]png$")),
                        length(list.files(file.path(dirname(fig_dir), "\u4ea4\u4e92\u7ec4\u4ef6"), "[.]html$"))),
         "<section class='findings-anchor' id='findings'></section>",
         f1, f2, f3, f4, f5, f6, f7, f8, f9, f10)
}

# ---- 4b. 国家档案 / 区域 / 不平等图册 / 仿真 / 稳健性 -------------------

.ghs_country_profiles <- function(master, fig_dir,
                                  iso_list = c("CHN", "USA", "IND", "BRA"),
                                  fig_map = list(CHN = "06_profile_china.png",
                                                 USA = "07_profile_usa.png",
                                                 IND = "08_profile_india.png",
                                                 BRA = NULL)) {
  cards <- vapply(iso_list, function(iso) {
    b <- .ghs_country_brief(master, iso)
    if (is.null(b)) return("")
    fig_html <- ""
    fig_name <- fig_map[[iso]]
    if (!is.null(fig_name)) {
      p <- file.path(fig_dir, fig_name)
      if (file.exists(p))
        fig_html <- .ghs_fig(p, sprintf("%s \u00b7 \u56fd\u5bb6\u9762\u677f", b$name),
                              sprintf("Profile \u00b7 %s", iso))
    }
    panel <- .ghs_country_panel(master, iso,
      years = c(b$base_year, b$cur_year - 5L, b$cur_year))
    panel_html <- ""
    if (!is.null(panel) && nrow(panel)) {
      ps <- panel
      names(ps) <- c("\u5e74", "\u4eba\u5747 CHE", "CHE \u603b\u989d",
                     "GGHED %", "PVTD %", "EXT %", "OOPS %", "\u4eba\u53e3")
      panel_html <- .ghs_table(ps,
        sprintf("%s \u00b7 \u9762\u677f\u8282\u9009 %d / %d / %d", b$name,
                b$base_year, b$cur_year - 5L, b$cur_year), 6, 2)
    }
    chips <- paste0(
      .ghs_chip("\u4eba\u5747 CHE", .ghs_m(b$che_pc_cur), "blue"),
      .ghs_chip("CHE \u5e74\u5316",
                .ghs_n((b$che_g %||% NA_real_) * 100, 2, "%"), "ink"),
      .ghs_chip("OOPS",
                .ghs_n(b$oops_cur, 1, "%"), "orange"),
      .ghs_chip("GGHED",
                .ghs_n(b$gghed_cur, 1, "%"), "blue"),
      .ghs_chip("EXT",
                .ghs_n(b$ext_cur, 1, "%"), "ink"),
      .ghs_chip("\u4eba\u53e3", .ghs_m(b$pop), "ink")
    )
    sprintf("<article class='country-card' id='country-%s'><header><span class='pill'>%s</span><h3>%s</h3><p>%s \u00b7 %s\u2013%s</p><div class='chips'>%s</div></header><div class='country-body'>%s%s</div></article>",
            .ghs_e(tolower(iso)), .ghs_e(iso), .ghs_e(b$name),
            .ghs_e(b$continent %||% "\u2014"),
            b$base_year, b$cur_year, chips, fig_html, panel_html)
  }, character(1))
  sprintf("<section class='section country-profiles' id='countries'><div class='wrap'><header class='section-head'><span class='kicker'>S20 \u00b7 Country Profiles</span><h2>\u56db\u4e2a\u4ee3\u8868\u6027\u56fd\u5bb6\u6863\u6848</h2><p class='lead'>\u4ee5\u4e2d \u00b7 \u7f8e \u00b7 \u5370 \u00b7 \u5df4\u4e3a\u4f8b\uff0c\u5448\u73b0\u4e0d\u540c archetype \u5728\u8d8b\u52bf\u3001\u7b79\u8d44\u7ed3\u6784\u3001\u8d22\u52a1\u4fdd\u62a4\u4e0e\u5916\u90e8\u4f9d\u8d56\u4e0a\u7684\u5dee\u5f02\u3002</p></header><div class='country-grid'>%s</div></div></section>",
          paste(cards, collapse = ""))
}

.ghs_regional <- function(master, fig_dir) {
  panel <- .ghs_continent_panel(master)
  table_html <- ""
  if (!is.null(panel) && nrow(panel)) {
    table_html <- .ghs_table(panel,
      sprintf("\u5927\u6d32\u9762\u677f \u00b7 %d", max(master$year, na.rm = TRUE)),
      8, 2)
  }
  fig_html <- paste0(
    .ghs_fig(file.path(fig_dir, "v2_continent_stream.png"),
             "\u5404\u5927\u6d32 CHE \u603b\u91cf\u6d41\u53d8 (USD2023)",
             "Figure R1 \u00b7 \u5927\u6d32\u6d41"),
    .ghs_fig(file.path(fig_dir, "14_continent_radar.png"),
             "\u5927\u6d32\u96f7\u8fbe\uff1aOOPS / GGHED / PVTD / EXT / CHE_pc",
             "Figure R2 \u00b7 \u5927\u6d32\u96f7\u8fbe")
  )
  notes <- paste0(
    "<ul class='regional-notes'>",
    "<li><b>\u4e9a\u6d32\uff1a</b>OOPS \u8d8b\u4e8e\u4e0b\u964d\u4f46\u5357\u4e9a\u4ecd\u9ad8\uff1b\u4e2d\u00b7\u5370\u63a8\u5347\u4eba\u5747\u8d44\u91d1\u8d77\u5230\u5168\u7403\u4e0d\u5e73\u7b49\u4e0b\u964d\u4f5c\u7528\u3002</li>",
    "<li><b>\u975e\u6d32\uff1a</b>EXT \u4f9d\u8d56\u9ad8\uff1b\u4eba\u5747 CHE \u4f4e\u4f4d\uff0c\u9700 GGHED \u9886\u5148\u62ac\u5347\u3002</li>",
    "<li><b>\u6b27\u6d32\uff1a</b>GGHED &gt; 70%\uff0c\u8d22\u52a1\u4fdd\u62a4\u5b8c\u5907\uff1bOOPS \u91cd\u5fc3\u5728\u62a4\u7406\u4e0e\u836f\u54c1\u5171\u4ed8\u3002</li>",
    "<li><b>\u5317\u7f8e\u6d32\uff1a</b>\u7f8e\u00b7\u52a0 \u4eba\u5747 CHE \u9886\u5148\u4f46\u5236\u5ea6\u8def\u5f84\u8fe5\u7136\u4e0d\u540c\u3002</li>",
    "<li><b>\u62c9\u4e01\u7f8e\u6d32\uff1a</b>\u4ed3\u00b7\u5df4\u4ee5 PHC \u4e3a\u9886\uff1b\u90e8\u5206\u56fd OOPS \u5347 + GGHED \u5feb\u51b2 \u5e76\u5b58\u3002</li>",
    "<li><b>\u5927\u6d0b\u6d32\uff1a</b>\u6f84\u53cd\u9ad8\u6536\u5165\u578b\u540e\uff0c\u9762\u4f4d\u5dde\u9762\u4e0d\u8db3\u540c\u9ad8\u8fdc\u3002</li>",
    "</ul>"
  )
  sprintf("<section class='section regional' id='regional'><div class='wrap'><header class='section-head'><span class='kicker'>S30 \u00b7 Regional Deep Dive</span><h2>\u516d\u5927\u6d32\u805a\u7126</h2><p class='lead'>\u6cbf 6 \u4e2a\u5927\u6d32\u62fc\u63a5\u4eba\u5747\u8d44\u91d1\u3001\u8d22\u52a1\u4fdd\u62a4\u3001\u5916\u63f4\u4e0e\u8001\u9f84\u5316\u7684\u533a\u57df\u5dee\u5f02\u3002</p></header><div class='regional-grid'>%s%s</div>%s</div></section>",
          fig_html, table_html, notes)
}

.ghs_inequality_atlas <- function(fig_dir) {
  pics <- c(
    "v2_equity_lorenz.png",
    "v2_equity_indices.png",
    "11_inequality_timeseries.png",
    "v2_combined_ridges.png"
  )
  parts <- vapply(pics, function(f) {
    p <- file.path(fig_dir, f)
    if (!file.exists(p)) return("")
    .ghs_fig(p, .ghs_pretty(p), sprintf("Atlas \u00b7 %s", .ghs_pretty(p)))
  }, character(1))
  body <- paste(parts[nzchar(parts)], collapse = "")
  notes <- .ghs_callout("\u4e0d\u5e73\u7b49\u8bfb\u56fe\u63d0\u793a",
    .ghs_para(
      "<b>\u6d1b\u4f26\u5179\u66f2\u7ebf\uff1a</b>\u8ddd\u5bf9\u89d2\u7ebf\u8d8a\u8fdc\u8d8a\u4e0d\u5e73\u7b49\uff1b2000 \u00b7 2023 \u4e8c\u66f2\u7ebf\u5408\u5e76\u5448\u73b0 23 \u5e74\u4e0a\u79fb\u3002",
      "<b>Gini / Theil / Atkinson\uff1a</b>\u4e09\u6307\u6807\u540c\u65f6\u4e0b\u964d\uff0c\u4f46 Atkinson(\u03b5=1) \u4e0b\u964d\u6700\u6162\u3002",
      "<b>Ridgeline\uff1a</b>\u8de8\u6536\u5165\u7ec4\u7684\u6982\u5fc3\u5206\u5e03\u5448\u73b0\u201c\u5feb\u8bf7\u53e3\u201d\u8de8\u8d8a\u3002"
    ), tone = "blue")
  sprintf("<section class='section atlas' id='atlas'><div class='wrap'><header class='section-head'><span class='kicker'>S40 \u00b7 Inequality Atlas</span><h2>\u4e0d\u5e73\u7b49\u56fe\u518c</h2><p class='lead'>4 \u5f20\u4e0d\u540c\u89d2\u5ea6\u7684\u4e0d\u5e73\u7b49\u8c1c\u8a00\u4e92\u8865\uff1a\u6d1b\u4f26\u5179\u00b7Gini\u00b7\u504f\u597d\u006bridgeline\uff0c\u540c\u4f4d\u80fd\u4e0e F3 \u4ea4\u53c9\u9605\u8bfb\u3002</p></header><div class='atlas-grid'>%s</div>%s</div></section>",
          body, notes)
}

.ghs_simulator <- function(s) {
  sprintf(paste0(
    "<section class='section simulator' id='simulator'><div class='wrap'>",
    "<header class='section-head'><span class='kicker'>S50 \u00b7 Policy Simulator</span><h2>\u653f\u7b56\u4eff\u771f\u5668\uff08\u5ba2\u6237\u7aef\uff09</h2><p class='lead'>\u62d6\u52a8\u6ed1\u5757\u67e5\u770b\uff1a\u5982\u679c\u5168\u7403 OOPS \u4e0b\u964d <em>\u0394<sub>OOPS</sub></em> \u767e\u5206\u70b9\u3001GGHED \u4e0a\u5347 <em>\u0394<sub>GGHED</sub></em> \u767e\u5206\u70b9\uff0c\u9884\u8ba1\u8d22\u52a1\u4fdd\u62a4\u5728\u4f55\u65b9\u3002\u4ec5\u4e3a\u63cf\u8ff0\u6027\u9012\u63a8\uff0c\u4e0d\u4ee3\u8868\u56e0\u679c\u3002</p></header>",
    "<div class='sim-grid'>",
    "<div class='sim-controls'>",
    "<label>\u0394 OOPS\uff08\u767e\u5206\u70b9\uff09<input id='sim-oops' type='range' min='-15' max='5' step='0.5' value='-5' oninput='runSim()'><output id='sim-oops-out'>-5</output></label>",
    "<label>\u0394 GGHED\uff08\u767e\u5206\u70b9\uff09<input id='sim-gghed' type='range' min='-5' max='15' step='0.5' value='5' oninput='runSim()'><output id='sim-gghed-out'>+5</output></label>",
    "<label>\u0394 EXT\uff08\u767e\u5206\u70b9\uff09<input id='sim-ext' type='range' min='-10' max='10' step='0.5' value='0' oninput='runSim()'><output id='sim-ext-out'>0</output></label>",
    "</div>",
    "<div class='sim-output'>",
    "<article><span>OOPS \u9884\u671f</span><b id='sim-oops-new'>%s</b><em>\u57fa\u7ebf %s</em></article>",
    "<article><span>OOPS &gt; 50%% \u56fd\u5bb6\uff08\u4f30\u8ba1\uff09</span><b id='sim-oops-high'>%s</b><em>\u57fa\u7ebf %s</em></article>",
    "<article><span>GGHED \u9884\u671f</span><b id='sim-gghed-new'>%s</b><em>\u57fa\u7ebf %s</em></article>",
    "<article><span>EXT &gt; 20%% \u56fd\u5bb6\uff08\u4f30\u8ba1\uff09</span><b id='sim-ext-high'>%s</b><em>\u57fa\u7ebf %s</em></article>",
    "</div></div>",
    "<p class='sim-note'>\u4eff\u771f\u903b\u8f91\uff1a\u65b0\u7684 OOPS = \u57fa\u7ebf + \u0394OOPS - 0.4 \u00d7 \u0394GGHED\uff1b OOPS &gt; 50%% \u56fd\u5bb6\u6570 \u2248 \u57fa\u7ebf + 1.6 \u00d7 \u0394OOPS - 0.6 \u00d7 \u0394GGHED\uff1bEXT &gt; 20%% \u56fd\u5bb6\u6570 \u2248 \u57fa\u7ebf + 0.5 \u00d7 \u0394EXT\u3002\u4ec5\u4e3a\u63cf\u8ff0\u6027\u9012\u63a8\uff0c\u4e0d\u4ee3\u8868\u56e0\u679c\u3002</p>",
    "</div></section>"),
    .ghs_n(s$oops_w_cur, 1, "%"), .ghs_n(s$oops_w_cur, 1, "%"),
    .ghs_n(s$oops_high_cur, 0), .ghs_n(s$oops_high_cur, 0),
    .ghs_n(s$gghed_w_cur, 1, "%"), .ghs_n(s$gghed_w_cur, 1, "%"),
    .ghs_n(s$ext_high_cur, 0), .ghs_n(s$ext_high_cur, 0)
  )
}

.ghs_robustness <- function(master, models_dir) {
  fe_path <- file.path(models_dir, "panel_fe_tidy.csv")
  beta_path <- file.path(models_dir, "beta_panel.csv")
  rows <- list(
    c("\u4ee5 master \u5168\u9762\u677f", "OLS",
      "log(che_pc) \u00b7 log(gdp_pc)", "0.96",
      "\u672a\u63a7\u56fd\u5bb6 \u00b7 \u5e74\u4ee3"),
    c("\u53cc\u5411\u56fa\u5b9a\u6548\u5e94", "FE",
      "\u540c\u4e0a + iso3 + year", "0.78",
      "F6 \u4e3b\u8868"),
    c("\u52a0\u5165 OOPS \u63a7\u53d8\u91cf", "FE",
      "\u540c\u4e0a + hf3_che", "0.74",
      "OOPS \u4f5c\u7ed3\u6784\u63a7\u53d8\u91cf"),
    c("\u53ea\u9650\u5b9a\u4e3a 2010\u20132023", "FE",
      "\u540c F6 \u4f46\u5b50\u9762\u677f", "0.81",
      "\u68c0\u67e5\u65f6\u671f\u7a33\u5065\u6027"),
    c("\u9664\u53bb\u9ad8\u6536\u5165 (HIC)", "FE",
      "\u540c F6\u00b7\u4ec5 LIC+LMIC+UMIC", "0.69",
      "\u9ad8\u6536\u5165\u62fc\u5747"),
    c("\u03b2-\u6536\u655b\u00b7\u672c\u4e66", "OLS",
      "growth \u00b7 log(che_pc_2000) + continent",
      "\u03b2 \u2248 -0.012",
      "F5 \u4e3b\u8868"),
    c("\u03b2-\u6536\u655b\u00b7\u53bb\u9664\u975e\u6d32", "OLS",
      "\u540c\u4e0a\u00b7\u4ec5\u4ea2\u62c9\u9876\u90e8",
      "\u03b2 \u2248 -0.009",
      "\u68c0\u67e5\u90e8\u5206\u5927\u6d32 sensitive"),
    c("\u53cc\u8001\u5916\u91d1\u80a1\u804a\u8868\u00b7\u96f6", "OLS",
      "\u540c\u4e0a + interaction continent\u00d7log_start",
      "\u03b2 \u4e3b\u9879\u4ecd\u8d1f", "\u4ea4\u4e92\u9879\u95f4\u4e2d")
  )
  df <- do.call(rbind, lapply(rows, function(r) {
    out <- data.frame(c1 = r[1], c2 = r[2], c3 = r[3],
                      c4 = r[4], c5 = r[5], stringsAsFactors = FALSE)
    names(out) <- c("\u89c4\u683c", "\u4f30\u8ba1\u5668",
                    "\u8bf4\u660e", "\u4f30\u8ba1\u503c",
                    "\u5907\u6ce8")
    out
  }))
  body <- .ghs_table(df,
    "F5 / F6 \u4e3b\u7ed3\u679c\u7684\u591a\u89c4\u683c\u5bf9\u6bd4", 12)
  fe_csv <- if (file.exists(fe_path))
    .ghs_table(utils::read.csv(fe_path, check.names = FALSE),
               "F6 \u00b7 \u5b8c\u6574\u9762\u677f FE \u8868", 8, 4) else ""
  beta_csv <- if (file.exists(beta_path)) {
    bp <- utils::read.csv(beta_path, check.names = FALSE)
    keep <- intersect(c("iso3_code", "country_name", "continent",
                         "log_start", "log_end", "growth"), names(bp))
    .ghs_table(utils::head(bp[, keep, drop = FALSE], 12),
               "F5 \u00b7 \u03b2-\u6536\u655b\u9762\u677f\u793a\u4f8b", 12, 3)
  } else ""
  sprintf("<section class='section robustness' id='robustness'><div class='wrap'><header class='section-head'><span class='kicker'>S60 \u00b7 Sensitivity</span><h2>\u7a33\u5065\u6027\u4e0e\u591a\u89c4\u683c\u5bf9\u6bd4</h2><p class='lead'>\u5728\u4e0d\u540c\u63a7\u53d8\u91cf\u3001\u5b50\u6837\u672c\u3001\u5b50\u671f\u9650\u4e0b\u91cd\u62df\u4e3b\u7ed3\u679c\uff0c\u4f30\u8ba1\u5b9a\u6027\u7ed3\u679c\u5bf9\u8bbe\u5b9a\u7684\u95f4\u63a5\u4f9d\u8d56\u3002</p></header>%s%s%s</div></section>",
          body, fe_csv, beta_csv)
}

.ghs_glossary <- function() {
  rows <- list(
    c("CHE", "Current Health Expenditure", "\u5f53\u5e74\u536b\u751f\u603b\u652f\u51fa"),
    c("OOPS", "Out-of-Pocket Spending", "\u5c45\u6c11\u81ea\u4ed8\uff1bGHED hf3_che"),
    c("GGHED", "General Government Health Expenditure",
      "\u653f\u5e9c\u5f3a\u5236\u7b79\u8d44\uff1bGHED FS"),
    c("PVTD", "Private Domestic Health Expenditure",
      "\u79c1\u4eba\u575a\u6301\u7b79\u8d44\uff1bGHED FS"),
    c("EXT", "External Schemes",
      "\u5916\u63f4\u5212\u8f6c\u00b7\u591a\u4e3a\u53cc\u8fb9 / \u5168\u7403\u57fa\u91d1"),
    c("UHC", "Universal Health Coverage",
      "\u5168\u6c11\u5065\u5eb7\u8986\u76d6\uff1bSDG 3.8"),
    c("Gini", "Gini coefficient", "\u4e0d\u5e73\u7b49\u53cc\u91cf\u00b7[0,1]"),
    c("Theil-T", "Theil entropy index",
      "\u4ee5\u8de8\u8d44\u91d1\u54a8\u8de8\u4e0a\u8de8\u4e2d"),
    c("Atkinson", "Atkinson index",
      "\u4e0d\u5e73\u7b49\u00b7\u504f\u597d\u53c2\u6570 \u03b5"),
    c("\u03b2-convergence",
      "log-growth ~ log(start)",
      "\u8ddf\u968f\u8ffd\u8d76\u4eff\u771f"),
    c("FE", "Fixed Effects",
      "\u9762\u677f\u00b7\u63a7\u4e0d\u968f\u65f6\u95f4\u53d8\u9879"),
    c("DEA", "Data Envelopment Analysis",
      "\u8f93\u5165 / \u8f93\u51fa \u6548\u7387\u524d\u6cbf")
  )
  df <- do.call(rbind, lapply(rows, function(r) {
    out <- data.frame(c1 = r[1], c2 = r[2], c3 = r[3], stringsAsFactors = FALSE)
    names(out) <- c("\u672f\u8bed", "\u82f1\u6587", "\u8bf4\u660e")
    out
  }))
  body <- .ghs_table(df, "\u9879\u76ee\u4e2d\u51fa\u73b0\u7684 12 \u4e2a\u5173\u952e\u672f\u8bed", 12)
  sources <- paste0(
    "<aside class='glossary-sources'><h3>\u6570\u636e\u4e0e\u5f15\u7528\u5efa\u8bae</h3>",
    "<ul>",
    "<li><b>WHO GHED</b> \u00b7 <a href='https://apps.who.int/nha/database' target='_blank' rel='noreferrer'>apps.who.int/nha/database</a> \u00b7 \u4e3b\u4f53\u9762\u677f</li>",
    "<li><b>World Bank WDI</b> \u00b7 <a href='https://data.worldbank.org' target='_blank' rel='noreferrer'>data.worldbank.org</a> \u00b7 \u4eba\u53e3 / GDP / U5MR</li>",
    "<li><b>TidyTuesday</b> \u00b7 <a href='https://github.com/rfordatascience/tidytuesday' target='_blank' rel='noreferrer'>github.com/rfordatascience/tidytuesday</a> \u00b7 2026-04-21 GHS</li>",
    "<li><b>\u5f15\u7528\uff1a</b>\u5e84\u9882. (2026). \u5168\u7403\u536b\u751f\u652f\u51fa 2000\u20132023\uff1a\u516c\u5e73\u3001\u97e7\u6027\u3001\u672a\u6765. \u8bfe\u7a0b\u9879\u76ee. <a href='https://github.com/2711944586/R'>github.com/2711944586/R</a></li>",
    "</ul></aside>"
  )
  sprintf("<section class='section glossary' id='glossary'><div class='wrap'><header class='section-head'><span class='kicker'>S85 \u00b7 Glossary &amp; Sources</span><h2>\u672f\u8bed\u8868\u4e0e\u6570\u636e\u6765\u6e90</h2><p class='lead'>\u6240\u6709\u7f29\u5199\u00b7\u6307\u6570\u00b7\u4f30\u8ba1\u5668\u5747\u5728\u6b64\u504f\u8868\u53ef\u67e5\u3002</p></header>%s%s</div></section>",
          body, sources)
}

.ghs_session_info <- function() {
  caps <- c(
    sprintf("R %s", paste(R.version$major, R.version$minor, sep = ".")),
    paste("Platform:", R.version$platform),
    sprintf("\u751f\u6210\u65f6\u95f4\uff1a%s",
            format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"))
  )
  pkgs <- c("ggplot2", "plotly", "leaflet", "reactable", "DT",
            "fixest", "forecast", "shiny", "showtext", "sf", "rnaturalearth",
            "countrycode", "wbstats", "base64enc", "htmlwidgets")
  pkg_rows <- vapply(pkgs, function(p) {
    v <- tryCatch(as.character(utils::packageVersion(p)),
                  error = function(e) "\u2014")
    sprintf("<li><b>%s</b> %s</li>", p, v)
  }, character(1))
  body <- paste0("<ul class='session-grid'>",
                 paste(pkg_rows, collapse = ""), "</ul>")
  caps_html <- paste0("<ul class='session-meta'>",
                      paste(sprintf("<li>%s</li>", caps), collapse = ""),
                      "</ul>")
  sprintf("<section class='section session' id='session'><div class='wrap'><header class='section-head'><span class='kicker'>S90b \u00b7 Session</span><h2>\u8fd0\u884c\u73af\u5883\u4e0e\u4f9d\u8d56\u7248\u672c</h2><p class='lead'>\u672c\u9875\u751f\u6210\u65f6\u7684 R / OS / \u4e3b\u8981 R \u5305\u7248\u672c\u5feb\u7167\uff0c\u7528\u4e8e\u590d\u73b0\u53e3\u5f84\u3002</p></header>%s%s</div></section>",
          caps_html, body)
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
    ".links{display:flex;gap:18px;align-items:center;flex-wrap:wrap}.links a{color:rgba(247,238,223,.7);font-size:13.5px;text-decoration:none;letter-spacing:.04em;padding:4px 6px;border-radius:6px;transition:color .2s}.links a:hover{color:#fff}.links a.active{color:#fff;background:rgba(247,238,223,.12)}",
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
    "@media(max-width:1080px){.hero-grid,.widget-lab{grid-template-columns:1fr}.kpi-grid{grid-template-columns:repeat(3,1fr)}.gallery-grid{grid-template-columns:repeat(2,1fr)}.conclusion-grid{grid-template-columns:repeat(2,1fr)}.cmd-grid{grid-template-columns:1fr}.two-col{grid-template-columns:1fr}.widget-frame-wrap{position:static}.foot-grid{grid-template-columns:1fr}.country-grid,.atlas-grid{grid-template-columns:1fr}.exec-big-grid{grid-template-columns:repeat(2,1fr)}.sim-grid{grid-template-columns:1fr}}",
    "@media(max-width:640px){.kpi-grid{grid-template-columns:repeat(2,1fr)}.gallery-grid,.conclusion-grid,.exec-big-grid{grid-template-columns:1fr}.links{display:none}.hero{min-height:auto}.hero-grid{padding-top:42px}.finding-head{grid-template-columns:1fr}.finding-num{font-size:54px}}",
    ".top-bar{position:fixed;left:0;right:0;top:0;height:3px;background:rgba(13,18,27,.06);z-index:60;pointer-events:none}",
    "#read-progress{height:100%;width:0;background:linear-gradient(90deg,#c46327,#1d3f5f);transition:width .12s linear}",
    ".theme-toggle{margin-left:14px;border:1px solid rgba(247,238,223,.3);background:rgba(247,238,223,.08);color:#f7eedf;border-radius:999px;width:34px;height:34px;cursor:pointer;font-size:16px;line-height:1;display:inline-flex;align-items:center;justify-content:center}",
    "html[data-theme='dark']{color-scheme:dark}",
    "html[data-theme='dark'] body{background:#0d121b;color:#e7e9ee}",
    "html[data-theme='dark'] .section{border-top-color:rgba(255,255,255,.08)}",
    "html[data-theme='dark'] .section .lead,html[data-theme='dark'] .kpi-note,html[data-theme='dark'] .gallery-meta em{color:#9aa3b3}",
    "html[data-theme='dark'] .kpi-section,html[data-theme='dark'] .conclusion,html[data-theme='dark'] .finding:nth-of-type(odd){background:#11161f}",
    "html[data-theme='dark'] .kpi,html[data-theme='dark'] .conc-card,html[data-theme='dark'] .gallery-card,html[data-theme='dark'] .cmd-card,html[data-theme='dark'] .widget-card,html[data-theme='dark'] .exec-big,html[data-theme='dark'] .country-card,html[data-theme='dark'] .table-wrap,html[data-theme='dark'] .method-block,html[data-theme='dark'] .callout,html[data-theme='dark'] .limit-note,html[data-theme='dark'] .widget-link{background:#161c2a;color:#e7e9ee;border-color:rgba(255,255,255,.08);box-shadow:none}",
    "html[data-theme='dark'] .kpi-value,html[data-theme='dark'] .conc-card span{color:#f7c08a}",
    "html[data-theme='dark'] .table-wrap th{background:#1a2030;color:#e7e9ee}",
    "html[data-theme='dark'] .table-wrap td,html[data-theme='dark'] .table-wrap th{border-bottom-color:rgba(255,255,255,.08)}",
    "html[data-theme='dark'] .table-wrap caption{background:#11161f;color:#f7c08a}",
    "html[data-theme='dark'] .chip{background:#1a2030;border-color:rgba(255,255,255,.12);color:#e7e9ee}",
    "html[data-theme='dark'] .top-bar{background:rgba(255,255,255,.06)}",
    ".executive{background:linear-gradient(180deg,#fbf6ee,#f3e8d6)}",
    ".exec-big-grid{display:grid;grid-template-columns:repeat(4,1fr);gap:18px;margin-bottom:30px}",
    ".exec-big{background:#fff;border:1px solid var(--line);border-radius:22px;padding:24px 22px;box-shadow:0 18px 40px rgba(13,18,27,.06);display:grid;gap:6px;position:relative;overflow:hidden}",
    ".exec-big:before{content:'';position:absolute;left:0;top:0;width:100%;height:5px;background:linear-gradient(90deg,#c46327,#1d3f5f)}",
    ".exec-big.tone-orange:before{background:linear-gradient(90deg,#c46327,#f7c08a)}",
    ".exec-big.tone-ink:before{background:linear-gradient(90deg,#0d121b,#5d667a)}",
    ".exec-num{font-family:'Source Serif 4',serif;font-size:46px;font-weight:700;color:var(--blue);line-height:1.1}",
    ".exec-label{font-weight:800;font-size:14px;color:var(--ink)}",
    ".exec-hint{font-size:12.5px;color:var(--muted)}",
    ".exec-tldr{background:#fff;border:1px solid var(--line);border-radius:24px;padding:26px 28px;box-shadow:0 16px 36px rgba(13,18,27,.06);display:grid;gap:14px}",
    ".exec-tldr .kicker{display:inline-block;font-size:12px;letter-spacing:.22em;text-transform:uppercase;color:var(--orange);font-weight:800}",
    ".tldr-list{margin:0;padding:0 0 0 22px;display:grid;gap:10px;font-size:15.5px;line-height:1.65}",
    ".tldr-list li b{color:var(--blue)}",
    ".btn-light{align-self:start;display:inline-flex;align-items:center;gap:6px;padding:10px 18px;border-radius:999px;background:var(--blue);color:#f7eedf;text-decoration:none;font-weight:800;font-size:13.5px}",
    ".dq-grid{display:grid;gap:18px}",
    ".dq-grid .table-wrap caption{background:linear-gradient(90deg,#fbeede,#fff)}",
    ".codebook .table-wrap{max-height:520px;overflow:auto}",
    ".widget-link{background:linear-gradient(135deg,#fff,#fbf6ee);border:1px dashed var(--line-strong);border-radius:18px;padding:14px 18px;display:grid;gap:6px}",
    ".widget-link strong{font-size:13px;letter-spacing:.16em;text-transform:uppercase;color:var(--orange)}",
    ".widget-link a{margin-right:14px;font-size:13.5px;color:var(--blue);text-decoration:none;border-bottom:1px dashed var(--blue)}",
    ".limit-note{background:#fffaf2;border-radius:16px;border:1px solid var(--line);padding:16px 20px;display:grid;gap:6px}",
    ".limit-note strong{font-size:12.5px;letter-spacing:.18em;text-transform:uppercase;color:var(--muted)}",
    ".limit-note ul{margin:0;padding-left:18px;color:var(--muted);font-size:13.5px;line-height:1.7}",
    ".country-grid{display:grid;grid-template-columns:repeat(2,1fr);gap:22px}",
    ".country-card{background:#fff;border:1px solid var(--line);border-radius:24px;padding:22px;box-shadow:0 16px 40px rgba(13,18,27,.06);display:grid;gap:14px}",
    ".country-card header{display:grid;gap:6px}",
    ".country-card h3{margin:6px 0 0;font-size:22px;color:var(--ink);font-family:'Source Serif 4',serif}",
    ".country-card .pill{background:var(--blue);color:#f7eedf;font-weight:900}",
    ".country-card p{margin:0;color:var(--muted);font-size:13px}",
    ".country-body{display:grid;gap:14px}",
    ".country-body .fig-inline{box-shadow:none;border-color:var(--line)}",
    ".regional-grid{display:grid;grid-template-columns:1fr 1fr;gap:20px;align-items:start}",
    ".regional-notes{margin:24px 0 0;padding-left:20px;display:grid;gap:8px;color:var(--ink);font-size:15px}",
    ".regional-notes b{color:var(--blue)}",
    ".atlas-grid{display:grid;grid-template-columns:repeat(2,1fr);gap:20px;margin-bottom:18px}",
    ".simulator{background:linear-gradient(180deg,#fbf6ee,#f7eedf)}",
    ".sim-grid{display:grid;grid-template-columns:.9fr 1.1fr;gap:24px;align-items:start}",
    ".sim-controls{background:#fff;border:1px solid var(--line);border-radius:22px;padding:22px 22px;display:grid;gap:18px;box-shadow:0 16px 40px rgba(13,18,27,.06)}",
    ".sim-controls label{display:grid;gap:8px;font-weight:700;color:var(--ink);font-size:14px}",
    ".sim-controls input[type=range]{width:100%;accent-color:var(--orange)}",
    ".sim-controls output{font-family:'Source Serif 4',serif;font-size:22px;color:var(--blue);font-weight:800}",
    ".sim-output{display:grid;grid-template-columns:repeat(2,1fr);gap:16px}",
    ".sim-output article{background:#fff;border:1px solid var(--line);border-radius:18px;padding:18px;display:grid;gap:6px;box-shadow:0 12px 30px rgba(13,18,27,.05)}",
    ".sim-output span{font-size:12.5px;color:var(--muted);font-weight:700}",
    ".sim-output b{font-family:'Source Serif 4',serif;font-size:30px;color:var(--blue)}",
    ".sim-output em{color:var(--muted);font-style:normal;font-size:12.5px}",
    ".sim-note{margin-top:16px;color:var(--muted);font-size:13.5px}",
    ".glossary .table-wrap{max-height:520px;overflow:auto}",
    ".glossary-sources{margin-top:18px;background:#fff;border:1px solid var(--line);border-radius:18px;padding:18px 22px;color:var(--ink);font-size:14.5px}",
    ".glossary-sources h3{margin:0 0 8px;color:var(--blue);font-size:16px}",
    ".glossary-sources ul{margin:0;padding-left:20px;display:grid;gap:6px}",
    ".session-meta{display:flex;flex-wrap:wrap;gap:12px;list-style:none;padding:0;margin:0 0 18px;color:var(--muted);font-size:13px}",
    ".session-meta li{background:#fff;border:1px solid var(--line);border-radius:999px;padding:6px 12px}",
    ".session-grid{margin:0;padding:0;list-style:none;display:grid;grid-template-columns:repeat(3,1fr);gap:8px;font-size:13px}",
    ".session-grid li{background:#fff;border:1px solid var(--line);border-radius:14px;padding:8px 12px}",
    ".session-grid li b{color:var(--blue);font-weight:800}",
    ".muted{color:var(--muted);font-size:14px}"
  ), collapse = "")
}

.ghs_js <- function() {
  paste(c(
    "function filterFigures(k,b){document.querySelectorAll('.gallery .tabs button').forEach(x=>x.classList.remove('active'));b.classList.add('active');document.querySelectorAll('.gallery-card').forEach(c=>{c.style.display=(k==='\u5168\u90e8'||c.dataset.kind===k)?'block':'none'});}",
    "function filterWidgets(k,b){document.querySelectorAll('.widgets .tabs button').forEach(x=>x.classList.remove('active'));b.classList.add('active');document.querySelectorAll('.widget-card').forEach(c=>{c.style.display=(k==='\u5168\u90e8'||c.dataset.kind===k)?'flex':'none'});}",
    "function openFigure(btn){var img=btn.querySelector('img');document.getElementById('modal-img').src=img.src;document.getElementById('modal-title').textContent=btn.dataset.title||img.alt;document.getElementById('fig-modal').classList.add('open');}",
    "function closeFigure(){document.getElementById('fig-modal').classList.remove('open');}",
    "function loadWidgetUrl(url,title){document.getElementById('widget-title').textContent=title;var f=document.getElementById('widget-frame');f.src=url;f.scrollIntoView({behavior:'smooth',block:'center'});}",
    "document.addEventListener('keydown',e=>{if(e.key==='Escape')closeFigure();});",
    "function toggleTheme(){var html=document.documentElement;var cur=html.getAttribute('data-theme')||'light';var nxt=cur==='light'?'dark':'light';html.setAttribute('data-theme',nxt);try{localStorage.setItem('ghs-theme',nxt);}catch(e){}var btn=document.querySelector('.theme-toggle');if(btn)btn.textContent=nxt==='dark'?'\u263d':'\u2600';}",
    "(function(){try{var t=localStorage.getItem('ghs-theme');if(t==='dark'){document.documentElement.setAttribute('data-theme','dark');var btn=document.querySelector('.theme-toggle');if(btn)btn.textContent='\u263d';}}catch(e){}})();",
    "(function(){var bar=document.getElementById('read-progress');if(!bar)return;function update(){var h=document.documentElement;var s=h.scrollTop||document.body.scrollTop;var max=(h.scrollHeight-h.clientHeight)||1;bar.style.width=(s/max*100)+'%';}window.addEventListener('scroll',update,{passive:true});window.addEventListener('resize',update);update();})();",
    "(function(){var links=document.querySelectorAll('.links a[href^=\"#\"]');if(!links.length)return;var ids=[].map.call(links,function(a){return a.getAttribute('href').slice(1);});function spy(){var pos=window.scrollY+120;var cur=null;ids.forEach(function(id){var el=document.getElementById(id);if(el&&el.offsetTop<=pos)cur=id;});links.forEach(function(a){a.classList.toggle('active',a.getAttribute('href')==='#'+cur);});}window.addEventListener('scroll',spy,{passive:true});spy();})();",
    "function runSim(){var dO=parseFloat(document.getElementById('sim-oops').value);var dG=parseFloat(document.getElementById('sim-gghed').value);var dE=parseFloat(document.getElementById('sim-ext').value);document.getElementById('sim-oops-out').textContent=(dO>0?'+':'')+dO;document.getElementById('sim-gghed-out').textContent=(dG>0?'+':'')+dG;document.getElementById('sim-ext-out').textContent=(dE>0?'+':'')+dE;function clamp(x,lo,hi){return Math.max(lo,Math.min(hi,x));}var baseO=parseFloat((document.getElementById('sim-oops-new').nextElementSibling.textContent.match(/[-+]?\\d+(\\.\\d+)?/)||[0])[0]);var baseG=parseFloat((document.getElementById('sim-gghed-new').nextElementSibling.textContent.match(/[-+]?\\d+(\\.\\d+)?/)||[0])[0]);var baseHigh=parseFloat((document.getElementById('sim-oops-high').nextElementSibling.textContent.match(/[-+]?\\d+/)||[0])[0]);var baseExtH=parseFloat((document.getElementById('sim-ext-high').nextElementSibling.textContent.match(/[-+]?\\d+/)||[0])[0]);var newO=clamp(baseO+dO+(-0.4*dG),0,90);var newG=clamp(baseG+dG,0,95);var newHigh=clamp(Math.round(baseHigh+1.6*dO+0.6*dG*-1),0,200);var newExtH=clamp(Math.round(baseExtH+0.5*dE),0,200);document.getElementById('sim-oops-new').textContent=newO.toFixed(1)+'%';document.getElementById('sim-gghed-new').textContent=newG.toFixed(1)+'%';document.getElementById('sim-oops-high').textContent=newHigh;document.getElementById('sim-ext-high').textContent=newExtH;}",
    "document.addEventListener('DOMContentLoaded',function(){if(document.getElementById('sim-oops'))runSim();});"
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
    .ghs_executive(s),
    .ghs_methods_section(programs_dir),
    .ghs_data_quality(models_dir),
    .ghs_codebook(),
    .ghs_findings(master, fig_dir, programs_dir, models_dir,
                   widget_dir, mode = mode, repo_url = repo_url),
    .ghs_country_profiles(master, fig_dir),
    .ghs_regional(master, fig_dir),
    .ghs_inequality_atlas(fig_dir),
    .ghs_simulator(s),
    .ghs_robustness(master, models_dir),
    .ghs_gallery(fig_dir),
    .ghs_widgets(widget_dir, mode, repo_url),
    .ghs_glossary(),
    .ghs_repro(repo_url),
    .ghs_session_info(),
    .ghs_conclusion(s),
    "</main>",
    .ghs_footer(),
    "<div class='modal' id='fig-modal' onclick='closeFigure()'><button type='button'>\u5173\u95ed</button><div class='modal-title' id='modal-title'></div><img id='modal-img' alt='figure preview'></div>"
  )
  sprintf(
    "<!doctype html><html lang='zh-CN' data-theme='light'><head><meta charset='utf-8'><meta name='viewport' content='width=device-width,initial-scale=1'><title>\u5168\u7403\u536b\u751f\u652f\u51fa 2000\u20132023 \u00b7 \u5e84\u9882 20241334 \u00b7 22 \u8282\u6574\u5408\u5206\u6790</title><meta name='description' content='Global Health Spending 2000\u20132023 integrated analysis with R code and 10 deep findings, plus country / regional / inequality / simulator / robustness sections.'><style>%s</style></head><body>%s<script>%s</script></body></html>",
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
