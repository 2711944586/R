# 自动测试/testthat/test-v3-plots-shocks.R
# B6: 冲击与变点图集 plot_shock_*

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
  f <- file.path("\u7a0b\u5e8f", "35_plots_shocks.R")
  if (!exists("plot_shock_covid_chepc", mode = "function") &&
      file.exists(f)) {
    source(f, encoding = "UTF-8")
  }
})

test_that("\u51b2\u51fb\u4e8b\u4ef6\u8868 ghs_shock_events \u5b9a\u4e49\u5b8c\u6574", {
  expect_true(is.list(ghs_shock_events))
  expect_true(all(c("gfc", "covid", "recovery") %in% names(ghs_shock_events)))
})

test_that("COVID-19 \u51b2\u51fb 5 \u56fe\u80fd\u6784\u9020 ggplot", {
  m <- skip_unless_master()
  expect_s3_class(plot_shock_covid_chepc(m), "ggplot")
  expect_s3_class(plot_shock_covid_oop(m), "ggplot")
  expect_s3_class(plot_shock_covid_dot_income(m), "ggplot")
  expect_s3_class(plot_shock_covid_lifeexp(m), "ggplot")
  expect_s3_class(plot_shock_covid_track(m), "ggplot")
})

test_that("GFC 5 \u56fe\u80fd\u6784\u9020 ggplot", {
  m <- skip_unless_master()
  expect_s3_class(plot_shock_gfc_chepc(m), "ggplot")
  expect_s3_class(plot_shock_gfc_oop(m), "ggplot")
  expect_s3_class(plot_shock_gfc_trend_gap(m), "ggplot")
  expect_s3_class(plot_shock_gfc_recovery_years(m), "ggplot")
  expect_s3_class(plot_shock_gfc_sensitivity(m), "ggplot")
})

test_that("\u53d8\u70b9\u68c0\u6d4b 5 \u56fe\u80fd\u6784\u9020 ggplot", {
  m <- skip_unless_master()
  expect_s3_class(plot_shock_segmented_chepc(m, "USA", 2008), "ggplot")
  expect_s3_class(plot_shock_segmented_multi(m), "ggplot")
  expect_s3_class(plot_shock_segmented_dot(m), "ggplot")
  expect_s3_class(plot_shock_pre_post_slope(m), "ggplot")
  expect_s3_class(plot_shock_segment_fit(m, "CHN"), "ggplot")
})

test_that("\u5168\u7403\u51b2\u51fb\u4e8b\u4ef6 5 \u56fe\u80fd\u6784\u9020 ggplot", {
  m <- skip_unless_master()
  expect_s3_class(plot_shock_global_trend(m), "ggplot")
  expect_s3_class(plot_shock_growth_rate_box(m), "ggplot")
  expect_s3_class(plot_shock_volatility_envelope(m), "ggplot")
  expect_s3_class(plot_shock_anomaly_heat(m), "ggplot")
  expect_s3_class(plot_shock_freq_rank(m), "ggplot")
})

test_that("\u6062\u590d\u8bca\u65ad 5 \u56fe\u80fd\u6784\u9020 ggplot", {
  m <- skip_unless_master()
  expect_s3_class(plot_shock_recovery_2022(m), "ggplot")
  expect_s3_class(plot_shock_recovery_scatter(m), "ggplot")
  expect_s3_class(plot_shock_catchup(m), "ggplot")
  expect_s3_class(plot_shock_residual_2022(m), "ggplot")
  expect_s3_class(plot_shock_recovery_dashboard(m), "ggplot")
})

test_that(".segmented_slopes helper \u8fd4\u56de\u5217\u8868\u4e0e fit", {
  yrs <- 2000:2022
  vals <- log(yrs - 1999) + rnorm(length(yrs), 0, 0.05)
  seg <- .segmented_slopes(yrs, vals, 2010)
  expect_true(is.list(seg))
  expect_s3_class(seg$fit, "lm")
  expect_equal(nrow(seg$pred), length(yrs))
})

test_that("ghs_export_shocks \u80fd\u5feb\u901f\u5bfc\u51fa PNG", {
  m <- skip_unless_master()
  out_dir <- file.path(tempdir(), "ghs-shocks-test")
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  p <- plot_shock_global_trend(m)
  png_path <- file.path(out_dir, "_smoke.png")
  ggplot2::ggsave(png_path, p, width = 8, height = 5, dpi = 120)
  expect_true(file.exists(png_path))
  expect_gt(file.info(png_path)$size, 5000)
})
