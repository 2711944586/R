# =============================================================================
# 仪表盘/模块/_helpers.R
# Shiny 模块共享工具：KPI 卡片 / spinner / 全局过滤器 / 数据切片
# =============================================================================

#' KPI 卡（与 程序/13 kpi_card_html 保持设计一致，但带颜色变体）
mod_kpi <- function(label, value, sublabel = NULL,
                    color = c("primary", "danger", "success", "warning", "muted"),
                    icon = NULL) {
  color <- match.arg(color)
  htmltools::div(
    class = sprintf("kpi-card kpi-%s", color),
    htmltools::div(class = "kpi-label", label),
    htmltools::div(class = "kpi-value", value),
    if (!is.null(sublabel))
      htmltools::div(class = "kpi-sublabel", sublabel),
    if (!is.null(icon))
      htmltools::div(class = "kpi-icon", icon)
  )
}

#' 标准 spinner（颜色与品牌主色一致）
mod_spinner <- function(x, color = "#1B5E88") {
  shinycssloaders::withSpinner(x, type = 6, color = color, size = 0.6)
}

#' 标准卡片容器（panel-content 风格）
mod_card <- function(..., title = NULL, class = NULL) {
  htmltools::div(
    class = paste("panel-content", class),
    if (!is.null(title)) htmltools::h4(title),
    ...
  )
}

#' 全局过滤器应用于 master 数据
#' @param master master_enriched 数据
#' @param filters list(years, continents, incomes, isos)
apply_global_filter <- function(master, filters) {
  d <- master
  if (!is.null(filters$years) && length(filters$years) == 2) {
    d <- d[d$year >= filters$years[1] & d$year <= filters$years[2], , drop = FALSE]
  }
  if (!is.null(filters$continents) && length(filters$continents) > 0) {
    d <- d[d$continent %in% filters$continents, , drop = FALSE]
  }
  if (!is.null(filters$incomes) && length(filters$incomes) > 0) {
    d <- d[d$income_group %in% filters$incomes, , drop = FALSE]
  }
  if (!is.null(filters$isos) && length(filters$isos) > 0) {
    d <- d[d$iso3_code %in% filters$isos, , drop = FALSE]
  }
  d
}

#' 显示数据为空的占位
mod_empty_message <- function(msg = "数据不足") {
  htmltools::div(
    class = "panel-content text-muted text-center",
    style = "padding: 60px 20px;",
    htmltools::h4(msg),
    htmltools::p("调整左侧过滤器或选择不同时段")
  )
}

#' 安全的 plotly 渲染（捕获错误）
safe_plotly <- function(expr, fallback_msg = "图表渲染失败") {
  tryCatch(force(expr),
            error = function(e) {
              plotly::plotly_empty(type = "scatter", mode = "markers") |>
                plotly::layout(title = fallback_msg,
                                annotations = list(
                                  list(text = conditionMessage(e),
                                       x = 0.5, y = 0.5, showarrow = FALSE,
                                       font = list(color = "#999"))
                                ))
            })
}

#' 通用 KPI 重组 — 返回一个 4 列 fluidRow
mod_kpi_row <- function(...) {
  cards <- list(...)
  bslib::layout_columns(
    col_widths = rep(3, length(cards)),
    !!!cards
  )
}

# =============================================================================
# 共用组件
# -----------------------------------------------------------------------------
# 与 程序/13_design_system.R 的 helpers 配合使用，保证 Shiny 与静态 HTML
# 共享同一套语义槽 / KPI 风格 / callout / 章节头。
# =============================================================================

#' hero（顶部标题区，含 kicker / 大标题 / lead / meta strip）
mod_v3_hero <- function(kicker, title, lead, meta = NULL) {
  meta_html <- if (!is.null(meta) && length(meta)) {
    pieces <- vapply(meta, function(m) {
      if (is.list(m) && !is.null(m$href)) {
        sprintf("<a href='%s' target='_blank' rel='noreferrer'>%s</a>",
                htmltools::htmlEscape(m$href),
                htmltools::htmlEscape(m$label))
      } else {
        sprintf("<span>%s</span>", htmltools::htmlEscape(m))
      }
    }, character(1))
    paste0("<div class='v3-hero-meta'>",
           paste(pieces, collapse = "<span class='v3-meta-sep'>\u00b7</span>"),
           "</div>")
  } else ""
  htmltools::HTML(sprintf(
    paste0("<section class='v3-hero'><div class='v3-hero-inner'>",
           "<span class='v3-hero-kicker'>%s</span>",
           "<h1 class='v3-hero-title'>%s</h1>",
           "<p class='v3-hero-lead'>%s</p>%s</div></section>"),
    htmltools::htmlEscape(kicker),
    htmltools::htmlEscape(title),
    htmltools::htmlEscape(lead),
    meta_html))
}

#' 章节头（kicker + h3 + lead）
mod_v3_section_head <- function(kicker, title, lead = NULL) {
  lead_html <- if (!is.null(lead) && nchar(lead))
    sprintf("<p class='v3-section-lead'>%s</p>",
            htmltools::htmlEscape(lead))
  else ""
  htmltools::HTML(sprintf(
    paste0("<header class='v3-section-head'>",
           "<span class='v3-kicker'>%s</span>",
           "<h3 class='v3-section-title'>%s</h3>%s</header>"),
    htmltools::htmlEscape(kicker),
    htmltools::htmlEscape(title),
    lead_html))
}

#' v3 KPI 卡（替代 mod_kpi 的更精致版本，与静态页 .ghs_v3_kpi 一致）
mod_v3_kpi <- function(label, value, hint = NULL, trend = NULL,
                       tone = c("primary", "secondary", "good",
                                 "warn", "bad", "neutral")) {
  tone <- match.arg(tone)
  tone_color <- switch(tone,
    primary = "#1d3f5f", secondary = "#c46327",
    good = "#2a857a", warn = "#c89a3b", bad = "#a23b3b",
    "#5d667a")
  trend_html <- ""
  if (!is.null(trend) && is.finite(trend)) {
    arr <- if (trend > 0) "\u2197" else if (trend < 0) "\u2198" else "\u2192"
    col <- if (trend > 0) "#2a857a"
           else if (trend < 0) "#a23b3b"
           else "#5d667a"
    trend_html <- sprintf(
      "<span class='v3-kpi-trend' style='color:%s'>%s %+0.1f%%</span>",
      col, arr, trend * 100)
  }
  hint_html <- if (!is.null(hint) && nchar(hint))
    sprintf("<div class='v3-kpi-hint'>%s</div>",
            htmltools::htmlEscape(hint))
  else ""
  htmltools::HTML(sprintf(
    paste0("<div class='v3-kpi' style='--tone:%s'>",
           "<div class='v3-kpi-value'>%s</div>",
           "<div class='v3-kpi-label'>%s%s</div>%s</div>"),
    tone_color,
    htmltools::htmlEscape(value),
    htmltools::htmlEscape(label),
    trend_html, hint_html))
}

#' KPI 网格（4 列，自动响应到 2 / 1 列）
mod_v3_kpi_grid <- function(...) {
  cards <- list(...)
  htmltools::tagList(
    htmltools::div(class = "v3-kpi-grid",
                   lapply(cards, function(c) c)))
}

#' callout（4 tones：info/good/warn/bad）
mod_v3_callout <- function(text, tone = c("info", "good", "warn", "bad"),
                            title = NULL) {
  tone <- match.arg(tone)
  title_html <- if (!is.null(title) && nchar(title))
    sprintf("<strong class='v3-callout-title'>%s</strong>",
            htmltools::htmlEscape(title))
  else ""
  htmltools::HTML(sprintf(
    paste0("<aside class='v3-callout v3-callout-%s'>",
           "%s<div class='v3-callout-body'>%s</div></aside>"),
    tone, title_html,
    if (inherits(text, "html") || inherits(text, "shiny.tag"))
      as.character(text) else htmltools::htmlEscape(text)))
}

#' 模块卡（用于 overview 内的"模块导航"）
#' @param target 目标 nav id（用于 nav_select）
#' @param kicker eyebrow 文字
#' @param title 模块名
#' @param desc 一句话描述
#' @param icon 简短 icon（emoji 或 unicode 字符）
#' @param tone 主色：primary/secondary/good/warn/bad
mod_v3_module_card <- function(ns, target, kicker, title, desc,
                                icon = "\u25b8",
                                tone = "primary") {
  tone_color <- switch(tone,
    primary = "#1d3f5f", secondary = "#c46327",
    good = "#2a857a", warn = "#c89a3b", bad = "#a23b3b",
    "#5d667a")
  htmltools::tags$button(
    class = "v3-module-card",
    type  = "button",
    style = sprintf("--tone:%s", tone_color),
    onclick = sprintf(
      "Shiny.setInputValue('%s', '%s', {priority:'event'});",
      ns("nav_to"), target),
    htmltools::span(class = "v3-module-icon", htmltools::HTML(icon)),
    htmltools::span(class = "v3-module-kicker", kicker),
    htmltools::strong(class = "v3-module-title", title),
    htmltools::span(class = "v3-module-desc", desc)
  )
}

#' stat strip（横向数据条；适合长 KPI 行）
mod_v3_stat_strip <- function(items) {
  if (!length(items)) return(htmltools::HTML(""))
  cards <- vapply(seq_along(items), function(i) {
    it <- items[[i]]
    sprintf(
      paste0("<div class='v3-stat-cell'>",
             "<div class='v3-stat-value'>%s</div>",
             "<div class='v3-stat-label'>%s</div></div>"),
      htmltools::htmlEscape(it$value %||% "\u2014"),
      htmltools::htmlEscape(it$label %||% ""))
  }, character(1))
  htmltools::HTML(sprintf("<div class='v3-stat-strip'>%s</div>",
                          paste(cards, collapse = "")))
}

#' 卡片容器（升级版 mod_card；带可选 kicker / footer）
mod_v3_card <- function(..., title = NULL, kicker = NULL, footer = NULL,
                          class = NULL) {
  head_html <- ""
  if (!is.null(kicker) || !is.null(title)) {
    head_html <- htmltools::div(
      class = "v3-card-head",
      if (!is.null(kicker))
        htmltools::span(class = "v3-card-kicker",
                         htmltools::htmlEscape(kicker)),
      if (!is.null(title))
        htmltools::h4(class = "v3-card-title",
                       htmltools::htmlEscape(title))
    )
  }
  htmltools::div(
    class = paste("v3-card", class),
    head_html,
    htmltools::div(class = "v3-card-body", ...),
    if (!is.null(footer))
      htmltools::div(class = "v3-card-footer", footer)
  )
}

#' 应用 plotly 主题（在 server 端 plotly 输出前调用）
mod_v3_plotly <- function(p, dark = FALSE) {
  if (exists("ghs_plotly_layout", mode = "function")) {
    return(ghs_plotly_layout(p, theme = if (dark) "dark" else "light"))
  }
  p
}

#' 应用 leaflet 品牌底图
mod_v3_leaflet <- function(map = NULL, dark = FALSE) {
  if (exists("ghs_leaflet_provider", mode = "function")) {
    return(ghs_leaflet_provider(map, theme = if (dark) "dark" else "light"))
  }
  if (is.null(map)) leaflet::leaflet() else map
}

#' reactable 默认主题包装
mod_v3_reactable <- function(data, ..., dark = FALSE) {
  args <- list(data = data, ...)
  if (exists("ghs_reactable_theme", mode = "function") &&
      !"theme" %in% names(args)) {
    args$theme <- ghs_reactable_theme(if (dark) "dark" else "light")
  }
  do.call(reactable::reactable, args)
}

#' 简易 fmt 助手（USD / 百分比 / 大数）
fmt_v3_usd <- function(x, digits = 0) {
  if (!length(x) || !is.finite(x)) return("\u2014")
  if (abs(x) >= 1e12) return(sprintf("$%.2fT", x / 1e12))
  if (abs(x) >= 1e9)  return(sprintf("$%.2fB", x / 1e9))
  if (abs(x) >= 1e6)  return(sprintf("$%.1fM", x / 1e6))
  if (abs(x) >= 1e3)  return(sprintf("$%s",
                                    format(round(x, digits), big.mark = ",")))
  sprintf("$%s", format(round(x, digits), big.mark = ","))
}

fmt_v3_pct <- function(x, digits = 1) {
  if (!length(x) || !is.finite(x)) return("\u2014")
  paste0(format(round(x, digits), nsmall = digits), "%")
}

fmt_v3_num <- function(x, digits = 0, suffix = "") {
  if (!length(x) || !is.finite(x)) return("\u2014")
  paste0(format(round(x, digits), big.mark = ",", nsmall = digits), suffix)
}
