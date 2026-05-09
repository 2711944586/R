# =============================================================================
# 仪表盘/模块/_helpers.R
# Shiny v2 模块共享工具：KPI 卡片 / spinner / 全局过滤器 / 数据切片
# =============================================================================

#' v2 KPI 卡（与 程序/13 kpi_card_html 保持设计一致，但带颜色变体）
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
