# 自动测试/testthat/test-v3-static-showcase.R
# 静态页扩展：CSS overlay + 章节/卡片渲染助手

test_that("ghs_css 输出包含全部语义 token", {
  css <- .ghs_css_v3()
  expect_match(css, "--g3-primary:#1d3f5f")
  expect_match(css, "--g3-good:#2a857a")
  expect_match(css, "--g3-paper:#fbf6ee")
  expect_match(css, "ghs-finding-num")
  expect_match(css, "ghs-deep-grid")
  expect_match(css, "ghs-paths")
  expect_match(css, "ghs-lineage")
  expect_match(css, "@media\\(max-width:1280px\\)")
  expect_match(css, "@media\\(max-width:1024px\\)")
  expect_match(css, "@media\\(max-width:768px\\)")
  expect_match(css, "@media\\(max-width:420px\\)")
})

test_that("ghs_v3_section_head / ghs_v3_section 返回标准结构", {
  h <- .ghs_v3_section_head("01 Overview", "\u5168\u7403\u8d8b\u52bf",
                             "\u8d44\u91d1\u53d8\u52a8")
  expect_match(h, "ghs-section-head")
  expect_match(h, "ghs-kicker")
  expect_match(h, "ghs-section-title")

  s <- .ghs_v3_section("test-id", "01", "\u6807\u9898", "<p>body</p>",
                       lead = "\u5bfc\u8bed")
  expect_match(s, "id='test-id'")
  expect_match(s, "section ghs-v3")
})

test_that("ghs_v3_kpi / ghs_v3_kpi_grid 返回正确卡片结构", {
  k1 <- .ghs_v3_kpi("$9.8T", "Global CHE", hint = "USD2023",
                    trend = 0.045, tone = "primary")
  expect_match(k1, "ghs-kpi")
  expect_match(k1, "ghs-kpi-trend")

  k2 <- .ghs_v3_kpi("32%", "OOPS\u5747\u503c", tone = "warn")
  expect_match(k2, "var\\(--g3-warn\\)")

  grid <- .ghs_v3_kpi_grid(c(k1, k2))
  expect_match(grid, "ghs-kpi-grid")
})

test_that("ghs_v3_stat_strip / chips / callout 工作", {
  strip <- .ghs_v3_stat_strip(list(
    list(value = "195", label = "\u56fd\u5bb6\u6570"),
    list(value = "$9.8T", label = "CHE")))
  expect_match(strip, "ghs-stat-strip")
  expect_match(strip, "195")
  expect_match(strip, "9.8T")

  chips <- .ghs_v3_chips(list(
    list(label = "n", value = "195", tone = "primary"),
    list(label = "year", value = "2023", tone = "good")))
  expect_match(chips, "ghs-chip-row")
  expect_match(chips, "data-tone='primary'")

  c <- .ghs_v3_callout("\u9ad8 OOPS \u63d0\u793a\u8d22\u52a1\u4fdd\u62a4\u538b\u529b",
                       tone = "warn", title = "Caveat")
  expect_match(c, "ghs-callout-warn")
  expect_match(c, "Caveat")
})

test_that("ghs_v3_deep_dive 返回 6 卡", {
  d <- .ghs_v3_deep_dive(
    data = "GHED 2024-12 + WDI 2024-10",
    method = "\u53cc\u5411 FE \u9762\u677f",
    assume = "\u5e74\u5ea6\u9700\u6c42\u8fde\u7eed",
    limit = "\u5916\u63f4\u53e3\u5f84\u5dee\u5f02",
    sens = "Bootstrap 1000 \u00d7",
    policy = "\u4f4e CHE \u56fd\u4f18\u5148 GGHED")
  expect_match(d, "ghs-deep-grid")
  expect_match(d, "tone-data")
  expect_match(d, "tone-method")
  expect_match(d, "tone-assume")
  expect_match(d, "tone-limit")
  expect_match(d, "tone-sens")
  expect_match(d, "tone-policy")
})

test_that("ghs_v3_fig_card / ghs_v3_fig_grid 处理缺失文件", {
  fig_dir <- file.path(tempdir(), "ghs-test-figs")
  dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)
  good_png <- file.path(fig_dir, "demo.png")
  png(good_png, width = 480, height = 320); plot(1:5, main = "demo"); dev.off()

  card <- .ghs_v3_fig_card(good_png, "demo caption", mode = "submission")
  expect_match(card, "ghs-fig-card")
  expect_match(card, "demo caption")
  expect_match(card, "data:image/png;base64")

  card_pub <- .ghs_v3_fig_card(good_png, "demo caption", mode = "publish")
  expect_match(card_pub, "demo.png")  # 外链仅文件名

  miss <- .ghs_v3_fig_card("/nonexistent/foo.png", "x")
  expect_match(miss, "missing")

  grid <- .ghs_v3_fig_grid(c(card, card), ncol = 3)
  expect_match(grid, "ghs-fig-grid-3")
})

test_that("ghs_v3_finding 返回完整 finding 结构", {
  f <- .ghs_v3_finding(
    id = "f15", num = "F15",
    kicker = "Aging \u00b7 \u8001\u9f84\u5316",
    title = "\u8001\u9f84\u5316\u4e0e\u536b\u751f\u652f\u51fa",
    lead = "\u4e2d\u4f4d\u5e74\u9f84 +1 \u5c81\u5bf9\u4eba\u5747 CHE \u5f39\u6027",
    chips_html = .ghs_v3_chips(list(list(label = "n", value = "180"))),
    figs_html = "<div>fig</div>",
    deep_html = .ghs_v3_deep_dive(data = "WDI"))
  expect_match(f, "id='f15'")
  expect_match(f, "ghs-finding-num")
  expect_match(f, "F15")
  expect_match(f, "ghs-deep-grid")
})

test_that("ghs_v3_reading_paths / lineage / sources / sens / bullets 渲染", {
  paths <- .ghs_v3_reading_paths(list(
    list(tone = "primary", icon = "\u00b7",
         title = "\u8bc4\u9605", lead = "5 \u6b65\u8bfb\u5b8c",
         items = list(list(anchor = "exec", label = "\u6458\u8981"),
                       list(anchor = "f1", label = "F1"))),
    list(tone = "good", icon = "\u00b7",
         title = "\u653f\u7b56", lead = "5 \u8df3\u8f6c",
         items = list(list(anchor = "policy", label = "\u653f\u7b56")))))
  expect_match(paths, "ghs-paths")
  expect_match(paths, "data-tone='primary'")
  expect_match(paths, "data-tone='good'")

  lin <- .ghs_v3_lineage(list(
    list(stage = "01 Raw", name = "GHED", meta = "3 CSV"),
    list(stage = "02 Clean", name = "master", meta = "195 \u00d7 24"),
    list(stage = "03 Enrich", name = "WDI", meta = "65 vars"),
    list(stage = "04 Analysis", name = "F1\u2013F36", meta = "300+ figs")))
  expect_match(lin, "ghs-lineage")
  expect_match(lin, "ghs-lineage-step")

  src <- .ghs_v3_sources(list(
    list(name = "WHO GHED 2024-12", desc = "Health expenditure"),
    list(name = "WDI 2024-10", desc = "Population, GDP")))
  expect_match(src, "ghs-sources")

  sg <- .ghs_v3_sens_grid(list(
    list(title = "\u4e3b\u8981\u5f39\u6027", value = "0.78",
         note = "GDP\u4eba\u5747 1% \u2192 CHE 0.78%")))
  expect_match(sg, "ghs-sens-grid")

  bl <- .ghs_v3_bullets(list(
    list(tone = "good",
         title = "\u8d22\u52a1\u4fdd\u62a4\u63d0\u5347",
         body = "OOPS \u5747\u503c \u4e0b\u964d 6.4 pp"),
    list(tone = "warn",
         title = "\u8001\u9f84\u5316\u538b\u529b",
         body = "\u9ad8\u6536\u5165\u56fd\u5bbf\u4f4f\u9762\u4e34")))
  expect_match(bl, "ghs-bullet-list")
  expect_match(bl, "data-tone='good'")
  expect_match(bl, "data-tone='warn'")
})

test_that("既有 .ghs_render() 调用方式仍然可用（不破坏）", {
  expect_true(exists(".ghs_render", mode = "function"))
  expect_true(exists(".ghs_css", mode = "function"))
  expect_true(exists(".ghs_js", mode = "function"))
})
