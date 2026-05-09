# =============================================================================
# 程序/19_narrative.R  ·  Quarto 叙事 helper（KPI / callout / scrollytell）
# -----------------------------------------------------------------------------
# 用法：在 Quarto chunk 中调用，输出 HTML 直接嵌入文档
# =============================================================================

#' KPI 卡片网格（任意数量）
#' @param ... 名值对 list(value=..., label=..., delta=NULL, sublabel=NULL)
#' @export
narrative_kpi_grid <- function(...) {
  args <- list(...)
  if (!length(args)) return("")
  cards <- vapply(args, function(a) {
    do.call(kpi_card_html, a)
  }, character(1))
  htmltools::HTML(paste0("<div class=\"kpi-grid\">",
                         paste(cards, collapse = ""), "</div>"))
}

#' 编辑式 callout（衬线大字 + 左竖线）
#' @export
narrative_callout <- function(text, source = NULL,
                               variant = c("default","warn","tip","note")) {
  variant <- match.arg(variant)
  htmltools::HTML(news_callout_html(text, source = source, variant = variant))
}

#' 数据来源块（页脚或图下方）
#' @export
narrative_data_source <- function(...) {
  args <- list(...)
  items <- vapply(args, function(a) {
    if (is.list(a)) {
      sprintf("<li><b>%s</b>: %s</li>",
               htmltools::htmlEscape(a$name %||% ""),
               htmltools::htmlEscape(a$desc %||% ""))
    } else {
      sprintf("<li>%s</li>", htmltools::htmlEscape(a))
    }
  }, character(1))
  htmltools::HTML(sprintf(
    "<aside class=\"data-source-panel\"><h6>\u6570\u636e\u6765\u6e90</h6><ul>%s</ul></aside>",
    paste(items, collapse = "")
  ))
}

#' scrollytell 容器（左 sticky 图，右 step 文字）
#' @param figure_html 图（HTML 字符串）
#' @param steps list of character: 每段 step 文字
#' @export
narrative_scrolly <- function(figure_html, steps) {
  if (!length(steps)) return("")
  steps_html <- paste(vapply(seq_along(steps), function(i) {
    sprintf("<div class=\"scrolly-step\" data-step=\"%d\">%s</div>",
             i, htmltools::HTML(steps[[i]]))
  }, character(1)), collapse = "\n")
  htmltools::HTML(sprintf(
    "<section class=\"scrolly-container\">\n  <div class=\"scrolly-figure\">%s</div>\n  <div class=\"scrolly-steps\">%s</div>\n</section>",
    figure_html, steps_html
  ))
}

#' 章末政策含义 box
#' @export
narrative_policy_box <- function(...) {
  args <- list(...)
  items <- vapply(args, function(s) sprintf("<li>%s</li>",
                                              htmltools::htmlEscape(s)),
                   character(1))
  htmltools::HTML(sprintf(
    "<div class=\"news-callout tip\"><h6>\u653f\u7b56\u542b\u4e49</h6><ol>%s</ol></div>",
    paste(items, collapse = "")
  ))
}

#' 简洁数字摘要（文本流内用）
#' @export
narrative_inline_number <- function(value, unit = "", fmt = "%s") {
  cls <- "inline-number"
  txt <- htmltools::htmlEscape(sprintf(fmt, value))
  if (nchar(unit))
    txt <- paste0(txt, " <span class=\"unit\">",
                   htmltools::htmlEscape(unit), "</span>")
  htmltools::HTML(sprintf("<span class=\"%s\">%s</span>", cls, txt))
}

# tiny operator
`%||%` <- function(a, b) if (!is.null(a)) a else b
