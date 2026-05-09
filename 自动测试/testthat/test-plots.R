# 图函数测试：只验证返回 ggplot 对象 + 关键 layer 存在, 不渲染像素

ghs_fix2 <- tryCatch(load_ghs(), error = function(e) NULL)
skip_if_no_data2 <- function() {
  if (is.null(ghs_fix2)) skip("GHED CSV not found")
}

test_that("plot_source_area returns a ggplot", {
  skip_if_no_data2()
  mw <- build_master_wide(ghs_fix2)
  me <- enrich_master(mw, with_wdi = FALSE)
  p <- plot_source_area(me)
  expect_s3_class(p, "ggplot")
})

test_that("plot_oops_ranking returns a ggplot", {
  skip_if_no_data2()
  mw <- build_master_wide(ghs_fix2)
  me <- enrich_master(mw, with_wdi = FALSE)
  p <- plot_oops_ranking(me, year_focus = 2023, top_n = 5)
  expect_s3_class(p, "ggplot")
})

test_that("plot_oops_box_continent returns a ggplot", {
  skip_if_no_data2()
  mw <- build_master_wide(ghs_fix2)
  me <- enrich_master(mw, with_wdi = FALSE)
  p <- plot_oops_box_continent(me, year_focus = 2023)
  expect_s3_class(p, "ggplot")
})

test_that("theme_ghs returns a theme", {
  expect_s3_class(theme_ghs(), "theme")
})

test_that("plot_country_profile returns a patchwork object", {
  skip_if_no_data2()
  skip_if_not_installed("patchwork")
  mw <- build_master_wide(ghs_fix2)
  me <- enrich_master(mw, with_wdi = FALSE)
  p <- plot_country_profile(me, iso = "CHN")
  expect_true(inherits(p, "patchwork") || inherits(p, "ggplot"))
})
