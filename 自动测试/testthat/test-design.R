# 自动测试/testthat/test-design.R
# 设计系统扩展层的烟雾测试
# helper-source.R 已自动加载 程序/13_design_system.R

test_that("palette_ghs3 语义槽返回正确颜色", {
  expect_equal(palette_ghs3("primary"), "#1d3f5f")
  expect_equal(palette_ghs3("good", "dark"), "#5dc4b6")
  expect_error(palette_ghs3("nonexistent"), "Unknown slot")
})

test_that("palette_ghs3_discrete / sequential / diverging 输出长度正确", {
  expect_length(palette_ghs3_discrete(5), 5)
  expect_length(palette_ghs3_discrete(20), 20)
  expect_length(palette_ghs3_sequential(7, "ember"), 7)
  expect_length(palette_ghs3_diverging(11), 11)
  for (col in palette_ghs3_discrete(12)) {
    expect_match(col, "^#[0-9a-fA-F]{6}$")
  }
})

test_that("theme_ghs3 三种 mode + 三种 variant 都返回 ggplot theme", {
  for (mode in c("light", "dark", "print")) {
    for (variant in c("default", "data", "editorial")) {
      th <- theme_ghs3(mode = mode, variant = variant)
      expect_s3_class(th, "theme")
    }
  }
})

test_that("ghs3 HTML 卡片返回非空字符串", {
  k <- ghs_kpi_card("$9.8T", "Global CHE 2023", hint = "USD2023",
                    trend = 0.045, tone = "primary")
  expect_match(k, "ghs-kpi")
  expect_match(k, "9.8T")

  s <- ghs_section_head("01 Overview", "\u5168\u7403\u8d8b\u52bf",
                        "\u5546\u4e1a\u533b\u7597\u9762\u677f")
  expect_match(s, "ghs-section-head")

  strip <- ghs_stat_strip(list(
    list(value = "195", label = "\u56fd\u5bb6\u6570"),
    list(value = "$9.8T", label = "CHE")))
  expect_match(strip, "ghs-stat-strip")

  c1 <- ghs_callout("\u9ad8 OOPS \u63d0\u793a\u8d22\u52a1\u4fdd\u62a4\u538b\u529b",
                    tone = "warn", title = "Caveat")
  expect_match(c1, "ghs-callout-warn")
})

test_that("ghs3 scale 助手返回正确 ggproto 类", {
  expect_s3_class(scale_fill_ghs3(), "ScaleDiscrete")
  expect_s3_class(scale_fill_ghs3_seq(), "ScaleContinuous")
  expect_s3_class(scale_fill_ghs3_div(midpoint = 0), "ScaleContinuous")
})

test_that("不破坏旧 API（向后兼容）", {
  expect_true(is.list(brand_palette))
  expect_s3_class(theme_ghs2(), "theme")
  expect_s3_class(scale_fill_brand_continent(), "ScaleDiscrete")
})

test_that("ghs_plotly_layout / ghs_leaflet_provider / ghs_reactable_theme 在依赖可用时正常", {
  if (requireNamespace("plotly", quietly = TRUE)) {
    p <- plotly::plot_ly(x = 1:5, y = 1:5, type = "scatter", mode = "lines")
    p2 <- ghs_plotly_layout(p)
    expect_s3_class(p2, "plotly")
  }
  if (requireNamespace("leaflet", quietly = TRUE)) {
    m <- ghs_leaflet_provider()
    expect_s3_class(m, "leaflet")
  }
  if (requireNamespace("reactable", quietly = TRUE)) {
    th <- ghs_reactable_theme()
    expect_s3_class(th, "reactableTheme")
  }
})

test_that("GHS3_VERSION / GHS3_META 元数据完整", {
  expect_equal(GHS3_VERSION, "3.0.0")
  expect_true("ggplot2" %in% GHS3_META$required_pkgs)
  expect_true("echarts4r" %in% GHS3_META$optional_pkgs)
})
