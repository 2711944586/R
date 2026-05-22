


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
  f <- file.path("\u7a0b\u5e8f", "34_plots_country.R")
  if (!exists("plot_country_trend_chepc", mode = "function") &&
      file.exists(f)) {
    source(f, encoding = "UTF-8")
  }
})

test_that("\u9884\u8bbe\u56fd\u5bb6\u96c6 ghs_country_sets \u4e0d\u4e3a\u7a7a", {
  expect_true(is.list(ghs_country_sets))
  expect_true(length(ghs_country_sets$brics) == 5)
  expect_true(length(ghs_country_sets$g7) == 7)
})

test_that("\u5355\u56fd\u65f6\u5e8f 5 \u56fe\u80fd\u6784\u9020 ggplot", {
  m <- skip_unless_master()
  expect_s3_class(plot_country_trend_chepc(m, "CHN"), "ggplot")
  expect_s3_class(plot_country_hf_stack(m, "CHN"), "ggplot")
  expect_s3_class(plot_country_hc_bars(m, "CHN"), "ggplot")
  expect_s3_class(plot_country_outcome_dual(m, "CHN"), "ggplot")
  expect_s3_class(plot_country_dashboard(m, "CHN"), "ggplot")
})

test_that("\u56fd\u5bb6\u5bf9\u6bd4 5 \u56fe\u80fd\u6784\u9020 ggplot", {
  m <- skip_unless_master()
  expect_s3_class(plot_country_smallmult_chepc(m), "ggplot")
  expect_s3_class(plot_country_compare_chepc(m), "ggplot")
  expect_s3_class(plot_country_compare_heat(m), "ggplot")
  expect_s3_class(plot_country_pair_hc(m), "ggplot")
  expect_s3_class(plot_country_cagr_compare(m), "ggplot")
})

test_that("\u540c\u4fa9 / \u533a\u57df\u5bf9\u6bd4 5 \u56fe\u80fd\u6784\u9020 ggplot", {
  m <- skip_unless_master()
  expect_s3_class(plot_country_zscore_income(m), "ggplot")
  expect_s3_class(plot_country_oop_gap(m), "ggplot")
  expect_s3_class(plot_country_lifeexp_gap(m), "ggplot")
  expect_s3_class(plot_country_multi_vs_global(m), "ggplot")
  expect_s3_class(plot_country_rank_trend(m), "ggplot")
})

test_that("\u6392\u540d / Top / Bottom 5 \u56fe\u80fd\u6784\u9020 ggplot", {
  m <- skip_unless_master()
  expect_s3_class(plot_country_topbot_chepc(m, n = 8), "ggplot")
  expect_s3_class(plot_country_topbot_lifeexp(m, n = 8), "ggplot")
  expect_s3_class(plot_country_start_progress(m), "ggplot")
  expect_s3_class(plot_country_growth_dist(m), "ggplot")
  expect_s3_class(plot_country_topn_grid(m, n = 8), "ggplot")
})

test_that("\u4e13\u9898\u56fd\u5bb6\u96c6 5 \u56fe\u80fd\u6784\u9020 ggplot", {
  m <- skip_unless_master()
  expect_s3_class(plot_country_brics_dual(m), "ggplot")
  expect_s3_class(plot_country_g7_oop(m), "ggplot")
  expect_s3_class(plot_country_eu_vs_asean(m), "ggplot")
  expect_s3_class(plot_country_nordic_combo(m), "ggplot")
  expect_s3_class(plot_country_lowle_highle(m), "ggplot")
})

test_that("ghs_export_country \u80fd\u5feb\u901f\u5bfc\u51fa PNG", {
  m <- skip_unless_master()
  out_dir <- file.path(tempdir(), "ghs-country-test")
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  p <- plot_country_trend_chepc(m, "USA")
  png_path <- file.path(out_dir, "_smoke.png")
  ggplot2::ggsave(png_path, p, width = 8, height = 5, dpi = 120)
  expect_true(file.exists(png_path))
  expect_gt(file.info(png_path)$size, 5000)
})
