




local({
  proj_root <- (function() {
    cwd <- getwd()
    for (up in 0:5) {
      candidate <- do.call(file.path, c(list(cwd), as.list(rep("..", up))))
      candidate <- normalizePath(candidate, mustWork = FALSE)
      if (file.exists(file.path(candidate, "DESCRIPTION"))) return(candidate)
    }
    cwd
  })()
  helpers <- file.path(proj_root, "\u4eea\u8868\u76d8", "\u6a21\u5757", "_helpers.R")
  if (file.exists(helpers)) source(helpers, encoding = "UTF-8")
})

test_that("mod_v3_hero 返回 hero HTML 包含 kicker / title / lead", {
  h <- mod_v3_hero(
    kicker = "Global Health Spending",
    title = "\u5168\u7403\u8d8b\u52bf",
    lead = "2000\u20132023 24 \u5e74\u9762\u677f",
    meta = list("\u5e84\u9882", list(label = "GitHub", href = "https://example.com")))
  expect_true(inherits(h, "html"))
  expect_match(as.character(h), "v3-hero")
  expect_match(as.character(h), "Global Health Spending")
  expect_match(as.character(h), "GitHub")
})

test_that("mod_v3_section_head / mod_v3_kpi 返回正确 HTML", {
  s <- mod_v3_section_head("01 Overview", "\u5168\u7403\u603b\u89c8",
                            "\u5546\u4e1a\u533b\u7597\u9762\u677f")
  expect_match(as.character(s), "v3-section-head")
  expect_match(as.character(s), "Overview")

  k <- mod_v3_kpi("Global CHE 2023", "$9.8T", hint = "USD2023",
                  trend = 0.045, tone = "primary")
  expect_match(as.character(k), "v3-kpi")
  expect_match(as.character(k), "9.8T")
  expect_match(as.character(k), "v3-kpi-trend")
})

test_that("mod_v3_callout / mod_v3_stat_strip / mod_v3_card 工作", {
  c1 <- mod_v3_callout("\u9ad8 OOPS \u63d0\u793a\u8d22\u52a1\u4fdd\u62a4\u538b\u529b",
                        tone = "warn", title = "Caveat")
  expect_match(as.character(c1), "v3-callout-warn")

  s <- mod_v3_stat_strip(list(
    list(value = "195", label = "\u56fd\u5bb6"),
    list(value = "$9.8T", label = "CHE")))
  expect_match(as.character(s), "v3-stat-strip")
  expect_match(as.character(s), "195")

  card <- mod_v3_card(htmltools::p("body content"),
                       title = "\u5361\u7247\u6807\u9898",
                       kicker = "DEMO",
                       footer = "\u9875\u811a")
  expect_match(as.character(card), "v3-card")
  expect_match(as.character(card), "v3-card-head")
  expect_match(as.character(card), "v3-card-body")
  expect_match(as.character(card), "v3-card-footer")
})

test_that("mod_v3_module_card 渲染按钮含 onclick 命名空间", {
  ns_fn <- shiny::NS("overview")
  btn <- mod_v3_module_card(ns_fn, target = "country",
                             kicker = "01 Country",
                             title = "\u56fd\u5bb6\u753b\u50cf",
                             desc = "\u5355\u56fd\u9762\u677f",
                             icon = "\u272a", tone = "primary")
  expect_true(inherits(btn, "shiny.tag"))
  html <- as.character(btn)
  expect_match(html, "v3-module-card")
  expect_match(html, "overview-nav_to")
  expect_match(html, "country")
})

test_that("fmt_v3 系列在常见数值上输出预期格式", {
  expect_equal(fmt_v3_usd(9.8e12), "$9.80T")
  expect_equal(fmt_v3_usd(2.5e9),  "$2.50B")
  expect_equal(fmt_v3_usd(1.234e6, digits = 1), "$1.2M")
  expect_equal(fmt_v3_usd(1234), "$1,234")
  expect_equal(fmt_v3_pct(33.456, digits = 1), "33.5%")
  expect_equal(fmt_v3_num(195000), "195,000")
  expect_equal(fmt_v3_usd(NA), "\u2014")
  expect_equal(fmt_v3_pct(NaN), "\u2014")
})

test_that("mod_v3_plotly / mod_v3_leaflet / mod_v3_reactable 安全包装", {
  if (requireNamespace("plotly", quietly = TRUE)) {
    p <- plotly::plot_ly(x = 1:5, y = 1:5, type = "scatter", mode = "lines")
    p2 <- mod_v3_plotly(p)
    expect_s3_class(p2, "plotly")
  }
  if (requireNamespace("leaflet", quietly = TRUE)) {
    m <- mod_v3_leaflet()
    expect_s3_class(m, "leaflet")
  }
  if (requireNamespace("reactable", quietly = TRUE)) {
    rt <- mod_v3_reactable(data.frame(a = 1:3, b = letters[1:3]))
    expect_true(inherits(rt, "reactable") || inherits(rt, "htmlwidget"))
  }
})

test_that("既有 mod_kpi / mod_card / mod_spinner 不被破坏", {
  k <- mod_kpi("test", "1234", color = "primary")
  expect_true(inherits(k, "shiny.tag"))
  expect_match(as.character(k), "kpi-primary")

  ca <- mod_card(htmltools::p("hi"), title = "T")
  expect_true(inherits(ca, "shiny.tag"))
  expect_match(as.character(ca), "panel-content")
})
