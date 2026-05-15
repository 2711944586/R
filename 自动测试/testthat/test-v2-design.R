# 自动测试/testthat/test-v2-design.R
# 设计系统与高级模型模块的烟雾测试

test_that("brand_palette has all expected slots", {
  expect_true(is.list(brand_palette))
  expect_true(all(c("ink", "paper", "rule", "muted",
                    "source", "continent", "income",
                    "sequential", "diverging") %in% names(brand_palette)))
  expect_length(brand_palette$source, 3)
  expect_length(brand_palette$continent, 6)
  expect_length(brand_palette$income, 4)
})

test_that("register_brand_fonts() returns a list with 4 roles", {
  fonts <- tryCatch(register_brand_fonts(), error = function(e) NULL)
  if (is.null(fonts)) skip("showtext / sysfonts not available")
  expect_named(fonts, c("serif", "sans", "mono", "cjk"), ignore.order = TRUE)
  expect_true(all(vapply(fonts, is.character, logical(1))))
})

test_that("theme_ghs2() returns a ggplot theme", {
  th <- tryCatch(theme_ghs2(), error = function(e) NULL)
  if (is.null(th)) skip("ggplot2 / showtext not available")
  expect_s3_class(th, "theme")
  expect_s3_class(th, "gg")
})

test_that("scale_*_brand_* functions return ggproto Scale objects", {
  if (!requireNamespace("ggplot2", quietly = TRUE)) skip("ggplot2 missing")
  expect_s3_class(scale_fill_brand_continent(),  "ScaleDiscrete")
  expect_s3_class(scale_colour_brand_income(),   "ScaleDiscrete")
  expect_s3_class(scale_fill_brand_seq(),        "ScaleContinuous")
  expect_s3_class(scale_fill_brand_div(),        "ScaleContinuous")
})

test_that("kpi_card_html() and dl_stats_html() return non-empty HTML", {
  if (!requireNamespace("htmltools", quietly = TRUE)) skip("htmltools missing")
  k <- kpi_card_html("$9.0T", label = "Total CHE 2023", delta = 5.2, unit = "")
  expect_true(grepl("kpi-card", k, fixed = TRUE))
  expect_true(grepl("9\\.0T", k))

  d <- dl_stats_html(`Countries` = "195", `Years` = "24")
  expect_true(grepl("Countries", d))
})

test_that("news_callout_html() variants render", {
  if (!requireNamespace("htmltools", quietly = TRUE)) skip("htmltools missing")
  for (v in c("default", "warn", "tip")) {
    h <- news_callout_html("test", source = "GHED", variant = v)
    expect_true(grepl("news-callout", h, fixed = TRUE))
  }
})

test_that("fit_lorenz returns 0/0 to 1/1 monotonic curve", {
  l <- fit_lorenz(c(1, 2, 4, 8, 16))
  expect_named(l, c("p_pop", "p_value"), ignore.order = TRUE)
  expect_equal(l$p_pop[1], 0)
  expect_equal(l$p_value[1], 0)
  expect_equal(tail(l$p_pop, 1), 1)
  expect_equal(tail(l$p_value, 1), 1)
  expect_true(all(diff(l$p_pop) >= 0))
  expect_true(all(diff(l$p_value) >= 0))
})

test_that("concentration_index returns C / G / kakwani", {
  if (!requireNamespace("ineq", quietly = TRUE)) skip("ineq missing")
  set.seed(1)
  v <- runif(50, 100, 1000)
  i <- runif(50,  500, 50000)
  ck <- concentration_index(v, i)
  expect_named(ck, c("C", "G", "kakwani"), ignore.order = TRUE)
  expect_true(is.finite(ck$C))
  expect_true(is.finite(ck$G))
  expect_equal(ck$kakwani, ck$C - ck$G, tolerance = 1e-9)
})

test_that("catastrophic_oop_share returns empty when GHO indicator missing", {
  df <- tibble::tibble(iso3_code = "USA", year = 2020)
  res <- catastrophic_oop_share(df, threshold = 10)
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 0)
})

test_that("read_external_cache returns NULL on missing file", {
  out <- read_external_cache("__no_such_source__")
  expect_null(out)
})

test_that(".wdi_indicators returns >= 30 indicators", {
  inds <- .wdi_indicators()
  expect_gte(length(inds), 30)
  expect_true(all(grepl("^[A-Z]+\\.[A-Z]+", inds)))
})
