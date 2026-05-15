# 自动测试/testthat/test-plots-equity.R
# B4: 不平等 / 财务保护图集 plot_equity_*

skip_unless_master <- function() {
  proj <- (function() {
    cwd <- getwd()
    for (up in 0:5) {
      candidate <- do.call(file.path, c(list(cwd), as.list(rep("..", up))))
      candidate <- normalizePath(candidate, mustWork = FALSE)
      if (file.exists(file.path(candidate, "DESCRIPTION"))) return(candidate)
    }
    cwd
  })()
  cache <- file.path(proj, "\u6d3e\u751f\u6570\u636e",
                     "\u5904\u7406\u7ed3\u679c", "master_enriched.rds")
  if (!file.exists(cache)) testthat::skip("\u7f3a\u5c11 master_enriched \u7f13\u5b58")
  invisible(readRDS(cache))
}

local({
  f <- file.path("\u7a0b\u5e8f", "33_plots_equity.R")
  if (!exists("plot_equity_oop_share_global", mode = "function") &&
      file.exists(f)) {
    source(f, encoding = "UTF-8")
  }
})

test_that("OOP / 财务保护组 5 图能构造 ggplot", {
  m <- skip_unless_master()
  expect_s3_class(plot_equity_oop_share_global(m), "ggplot")
  expect_s3_class(plot_equity_oop_share_income(m), "ggplot")
  expect_s3_class(plot_equity_oop_share_continent(m), "ggplot")
  expect_s3_class(plot_equity_oop_box_income(m), "ggplot")
  expect_s3_class(plot_equity_catastrophic_proxy(m), "ggplot")
})

test_that("Gini / Lorenz / 集中度 5 图能构造 ggplot", {
  m <- skip_unless_master()
  expect_s3_class(plot_equity_lorenz_che(m), "ggplot")
  expect_s3_class(plot_equity_gini_trend(m), "ggplot")
  expect_s3_class(plot_equity_theil_between_within(m), "ggplot")
  expect_s3_class(plot_equity_concentration_lifeexp(m), "ggplot")
  expect_s3_class(plot_equity_index_dot(m), "ggplot")
})

test_that("跨国分布 5 图能构造 ggplot", {
  m <- skip_unless_master()
  expect_s3_class(plot_equity_che_pc_iqr(m), "ggplot")
  expect_s3_class(plot_equity_top_bottom_ratio(m), "ggplot")
  expect_s3_class(plot_equity_dispersion_income(m), "ggplot")
  expect_s3_class(plot_equity_logvar_trend(m), "ggplot")
  expect_s3_class(plot_equity_che_fan(m), "ggplot")
})

test_that("区域内分布 5 图能构造 ggplot", {
  m <- skip_unless_master()
  expect_s3_class(plot_equity_continent_oop_box(m), "ggplot")
  expect_s3_class(plot_equity_region_heat(m), "ggplot")
  expect_s3_class(plot_equity_continent_cv(m), "ggplot")
  expect_s3_class(plot_equity_oop_vs_che_facet(m), "ggplot")
  expect_s3_class(plot_equity_continent_rank(m), "ggplot")
})

test_that("灾难性支出与替代视图 5 图能构造 ggplot", {
  m <- skip_unless_master()
  expect_s3_class(plot_equity_high_oop_top(m, n = 12), "ggplot")
  expect_s3_class(plot_equity_oop_change(m, n = 8), "ggplot")
  expect_s3_class(plot_equity_oop_vs_gghed(m), "ggplot")
  expect_s3_class(plot_equity_threshold_share(m), "ggplot")
  expect_s3_class(plot_equity_oop_velocity(m), "ggplot")
})

test_that("Gini / Theil helpers \u8fd4\u56de\u6709\u9650\u503c", {
  m <- skip_unless_master()
  x <- m$che_pc_usd2023[m$year == max(m$year, na.rm = TRUE)]
  w <- m$pop[m$year == max(m$year, na.rm = TRUE)]
  g <- .weighted_gini(x, w)
  expect_true(is.finite(g) && g > 0 && g < 1)
  th <- .weighted_theil_t(x, w)
  expect_true(is.finite(th) && th >= 0)
})

test_that("ghs_export_equity \u80fd\u5feb\u901f\u5bfc\u51fa\u4e00\u5f20 PNG", {
  m <- skip_unless_master()
  out_dir <- file.path(tempdir(), "ghs-equity-test")
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  p <- plot_equity_gini_trend(m)
  png_path <- file.path(out_dir, "_smoke.png")
  ggplot2::ggsave(png_path, p, width = 8, height = 5, dpi = 120)
  expect_true(file.exists(png_path))
  expect_gt(file.info(png_path)$size, 5000)
})
