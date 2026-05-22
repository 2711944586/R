
narrative_kpi_grid <- function(...) {
  args <- list(...)
  if (!length(args)) return("")
  cards <- vapply(args, function(a) {
    do.call(kpi_card_html, a)
  }, character(1))
  htmltools::HTML(paste0("<div class=\"kpi-grid\">",
                         paste(cards, collapse = ""), "</div>"))
}

narrative_callout <- function(text, source = NULL,
                               variant = c("default","warn","tip","note")) {
  variant <- match.arg(variant)
  htmltools::HTML(news_callout_html(text, source = source, variant = variant))
}

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

narrative_inline_number <- function(value, unit = "", fmt = "%s") {
  cls <- "inline-number"
  txt <- htmltools::htmlEscape(sprintf(fmt, value))
  if (nchar(unit))
    txt <- paste0(txt, " <span class=\"unit\">",
                   htmltools::htmlEscape(unit), "</span>")
  htmltools::HTML(sprintf("<span class=\"%s\">%s</span>", cls, txt))
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
