# 自动测试/testthat/test-v3-plots-outcomes.R
# B3: 产出 / 寿命专题图集 plot_outcome_*

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

skip_unless_worldsf <- function() {
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
                     "\u5904\u7406\u7ed3\u679c", "world_sf_medium.rds")
  if (!file.exists(cache)) testthat::skip("\u7f3a\u5c11 world_sf \u7f13\u5b58")
  invisible(readRDS(cache))
}

local({
  f <- file.path("\u7a0b\u5e8f", "32_plots_outcomes.R")
  if (!exists("plot_outcome_lexis_lifeexp", mode = "function") &&
      file.exists(f)) {
    source(f, encoding = "UTF-8")
  }
})

test_that("Lexis 系列 3 图能构造 ggplot 而不抛错", {
  m <- skip_unless_master()
  expect_s3_class(plot_outcome_lexis_lifeexp(m), "ggplot")
  expect_s3_class(plot_outcome_lexis_u5mr(m), "ggplot")
  expect_s3_class(plot_outcome_lexis_dual(m), "ggplot")
})

test_that("SDG-3 趋势 6 图能构造 ggplot", {
  m <- skip_unless_master()
  expect_s3_class(plot_outcome_lifeexp_trend_global(m), "ggplot")
  expect_s3_class(plot_outcome_lifeexp_trend_income(m), "ggplot")
  expect_s3_class(plot_outcome_lifeexp_trend_continent(m), "ggplot")
  expect_s3_class(plot_outcome_u5mr_trend_global(m), "ggplot")
  expect_s3_class(plot_outcome_u5mr_trend_income(m), "ggplot")
  expect_s3_class(plot_outcome_u5mr_trend_continent(m), "ggplot")
})

test_that("差距与增长系列 4 图能构造 ggplot", {
  m <- skip_unless_master()
  expect_s3_class(plot_outcome_lifeexp_gap_to_frontier(m), "ggplot")
  expect_s3_class(plot_outcome_u5mr_gap_to_frontier(m), "ggplot")
  expect_s3_class(plot_outcome_lifeexp_gain(m, n = 8), "ggplot")
  expect_s3_class(plot_outcome_u5mr_reduction(m, n = 8), "ggplot")
})

test_that("前沿 / 效率系列 5 图能构造 ggplot", {
  m <- skip_unless_master()
  expect_s3_class(plot_outcome_frontier_lifeexp(m), "ggplot")
  expect_s3_class(plot_outcome_frontier_u5mr(m), "ggplot")
  expect_s3_class(plot_outcome_frontier_composite(m), "ggplot")
  expect_s3_class(plot_outcome_efficiency_score(m, n = 10), "ggplot")
  expect_s3_class(plot_outcome_frontier_panel_income(m), "ggplot")
})

test_that("弹性与残差 4 图能构造 ggplot", {
  m <- skip_unless_master()
  expect_s3_class(plot_outcome_elasticity_lifeexp(m), "ggplot")
  expect_s3_class(plot_outcome_elasticity_u5mr(m), "ggplot")
  expect_s3_class(plot_outcome_residual_lifeexp(m, n = 8), "ggplot")
  expect_s3_class(plot_outcome_residual_u5mr(m, n = 8), "ggplot")
})

test_that("耦合视图 2 图能构造 ggplot", {
  m <- skip_unless_master()
  expect_s3_class(plot_outcome_decoupling_track(m), "ggplot")
  expect_s3_class(plot_outcome_u5mr_velocity(m), "ggplot")
})

test_that("残差地理 choropleth 能构造 (\u9700 world_sf)", {
  m  <- skip_unless_master()
  sf <- skip_unless_worldsf()
  expect_s3_class(plot_outcome_residual_map_lifeexp(m, sf), "ggplot")
})

test_that("ghs_export_outcomes \u80fd\u5feb\u901f\u5bfc\u51fa\u4e00\u5f20 PNG (\u5b50\u96c6)", {
  m <- skip_unless_master()
  out_dir <- file.path(tempdir(), "ghs-outcomes-test")
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  p <- plot_outcome_lifeexp_trend_global(m)
  png_path <- file.path(out_dir, "_smoke.png")
  ggplot2::ggsave(png_path, p, width = 8, height = 5, dpi = 120)
  expect_true(file.exists(png_path))
  expect_gt(file.info(png_path)$size, 5000)
})
