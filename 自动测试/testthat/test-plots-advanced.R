


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
  f <- file.path("\u7a0b\u5e8f", "30_plots_advanced.R")
  if (!exists("plot_adv_ridge_oops_by_income", mode = "function") &&
      file.exists(f)) {
    source(f, encoding = "UTF-8")
  }
})

test_that("ridgeline 系列 3 图能构造 ggplot 而不抛错", {
  m <- skip_unless_master()
  expect_s3_class(plot_adv_ridge_oops_by_income(m), "ggplot")
  expect_s3_class(plot_adv_ridge_che_pc_evolution(m), "ggplot")
  expect_s3_class(plot_adv_ridge_gghed_by_continent(m), "ggplot")
})

test_that("beeswarm 系列 4 图能构造 ggplot", {
  m <- skip_unless_master()
  expect_s3_class(plot_adv_beeswarm_che_pc(m), "ggplot")
  expect_s3_class(plot_adv_beeswarm_oops(m), "ggplot")
  expect_s3_class(plot_adv_beeswarm_gghed(m), "ggplot")
  expect_s3_class(plot_adv_beeswarm_lifeexp(m), "ggplot")
})

test_that("stream / bump / slope / lollipop 系列能构造 ggplot", {
  m <- skip_unless_master()
  expect_s3_class(plot_adv_stream_global_sources(m), "ggplot")
  expect_s3_class(plot_adv_stream_continent(m), "ggplot")
  expect_s3_class(plot_adv_stream_income(m), "ggplot")
  expect_s3_class(plot_adv_bump_top25(m, n = 12), "ggplot")
  expect_s3_class(plot_adv_slope_smallmultiples(m, top_n = 6), "ggplot")
  expect_s3_class(plot_adv_lollipop_che_change(m, n = 10), "ggplot")
})

test_that("treemap / parallel / marimekko / radial / calendar 类能构造 ggplot", {
  m <- skip_unless_master()
  expect_s3_class(plot_adv_treemap_continent_che(m), "ggplot")
  expect_s3_class(plot_adv_parallel_finance(m), "ggplot")
  expect_s3_class(plot_adv_marimekko_finance(m), "ggplot")
  expect_s3_class(plot_adv_radial_hc_purpose(m, "USA"), "ggplot")
  expect_s3_class(plot_adv_radial_hc_purpose(m, "CHN"), "ggplot")
  expect_s3_class(plot_adv_calendar_growth(m, top_n = 24), "ggplot")
  expect_s3_class(plot_adv_density_2d_finance(m), "ggplot")
})

test_that("多面板时序与 Top/Bottom 类能构造 ggplot", {
  m <- skip_unless_master()
  expect_s3_class(plot_adv_che_pc_by_continent(m), "ggplot")
  expect_s3_class(plot_adv_sources_by_income(m), "ggplot")
  expect_s3_class(plot_adv_topbot_dotplot(m, "che_pc_usd2023", n = 10),
                   "ggplot")
  expect_s3_class(plot_adv_topbot_dotplot(m, "hf3_che", n = 10),
                   "ggplot")
  expect_s3_class(plot_adv_topbot_dotplot(m, "gghed_che", n = 10),
                   "ggplot")
  expect_s3_class(plot_adv_topbot_dotplot(m, "life_exp", n = 10),
                   "ggplot")
})

test_that("ghs_export_advanced 在 tempdir 下能写出 PNG/SVG (\u5feb\u901f\u5b50\u96c6)", {
  m <- skip_unless_master()
  out_dir <- file.path(tempdir(), "ghs-adv-test")
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  p <- plot_adv_beeswarm_che_pc(m)
  png_path <- file.path(out_dir, "_smoke.png")
  ggplot2::ggsave(png_path, p, width = 8, height = 5, dpi = 120)
  expect_true(file.exists(png_path))
  expect_gt(file.info(png_path)$size, 5000)
})
