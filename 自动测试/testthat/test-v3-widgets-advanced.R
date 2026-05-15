# 自动测试/testthat/test-v3-widgets-advanced.R
# C2: 高级交互组件 iadv_*

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
  f <- file.path("\u7a0b\u5e8f", "37_widgets_advanced.R")
  if (!exists("iadv_che_pc_lines", mode = "function") && file.exists(f)) {
    source(f, encoding = "UTF-8")
  }
})

test_that("A \u00b7 plotly time/series 5 widget", {
  m <- skip_unless_master()
  testthat::skip_if_not_installed("plotly")
  expect_s3_class(iadv_che_pc_lines(m), "plotly")
  expect_s3_class(iadv_oop_lines(m), "plotly")
  expect_s3_class(iadv_che_total_stacked(m), "plotly")
  expect_s3_class(iadv_global_weighted_avg(m), "plotly")
  expect_s3_class(iadv_lifeexp_byinc(m), "plotly")
})

test_that("A \u00b7 plotly bar/heatmap/ribbon 5 widget", {
  m <- skip_unless_master()
  testthat::skip_if_not_installed("plotly")
  expect_s3_class(iadv_bar_race(m), "plotly")
  expect_s3_class(iadv_hf_share_area(m), "plotly")
  expect_s3_class(iadv_waterfall_che(m), "plotly")
  expect_s3_class(iadv_ribbon_quantiles(m), "plotly")
  expect_s3_class(iadv_heatmap_year_inc(m), "plotly")
})

test_that("B \u00b7 distribution/structure 5 widget", {
  m <- skip_unless_master()
  testthat::skip_if_not_installed("plotly")
  expect_s3_class(iadv_violin_inc(m), "plotly")
  expect_s3_class(iadv_box_continent(m), "plotly")
  expect_s3_class(iadv_sunburst_che(m), "plotly")
  expect_s3_class(iadv_treemap_che(m), "plotly")
  expect_s3_class(iadv_3d_scatter(m), "plotly")
})

test_that("C \u00b7 reactable 5 widget", {
  m <- skip_unless_master()
  testthat::skip_if_not_installed("reactable")
  expect_s3_class(iadv_rt_top_che_pc(m), "reactable")
  expect_s3_class(iadv_rt_oop_extremes(m), "reactable")
  expect_s3_class(iadv_rt_continent_summary(m), "reactable")
  expect_s3_class(iadv_rt_income_summary(m), "reactable")
  expect_s3_class(iadv_rt_finprot(m), "reactable")
})

test_that("D \u00b7 DT 5 widget", {
  m <- skip_unless_master()
  testthat::skip_if_not_installed("DT")
  expect_s3_class(iadv_dt_master_browse(m), "datatables")
  expect_s3_class(iadv_dt_threeyear(m), "datatables")
  expect_s3_class(iadv_dt_yoy(m), "datatables")
  expect_s3_class(iadv_dt_lifeexp_u5mr(m), "datatables")
  expect_s3_class(iadv_dt_growth_desc(m), "datatables")
})

test_that("E \u00b7 networkD3 4 widget", {
  m <- skip_unless_master()
  testthat::skip_if_not_installed("networkD3")
  expect_s3_class(iadv_sankey_3stage(m), "sankeyNetwork")
  expect_s3_class(iadv_sankey_continent_oop(m), "sankeyNetwork")
  expect_s3_class(iadv_force_country_sim(m), "forceNetwork")
  expect_s3_class(iadv_diagonal_tree(m), "diagonalNetwork")
})

test_that("F \u00b7 crosstalk 3 widget", {
  m <- skip_unless_master()
  testthat::skip_if_not_installed("crosstalk")
  testthat::skip_if_not_installed("plotly")
  expect_s3_class(iadv_ctk_dual_scatter(m), "shiny.tag")
  expect_s3_class(iadv_ctk_brushable(m), "shiny.tag")
  expect_s3_class(iadv_ctk_panel_combo(m), "shiny.tag")
})

test_that("G \u00b7 htmltools KPI 5 widget", {
  m <- skip_unless_master()
  testthat::skip_if_not_installed("htmltools")
  expect_s3_class(iadv_kpi_global(m), "shiny.tag")
  expect_s3_class(iadv_kpi_country(m, "USA"), "shiny.tag")
  expect_s3_class(iadv_kpi_change(m), "shiny.tag")
  expect_s3_class(iadv_progress_sdg3(m), "shiny.tag")
  expect_s3_class(iadv_rank_strip(m), "shiny.tag")
})

test_that("H \u00b7 plotly extras 5 widget", {
  m <- skip_unless_master()
  testthat::skip_if_not_installed("plotly")
  expect_s3_class(iadv_area_smooth_che(m), "plotly")
  expect_s3_class(iadv_slope_chart(m), "plotly")
  expect_s3_class(iadv_dumbbell(m), "plotly")
  expect_s3_class(iadv_lollipop_oop(m), "plotly")
  expect_s3_class(iadv_continent_ribbon(m), "plotly")
})

test_that("I \u00b7 hc / ec \u964d\u7ea7 4 widget", {
  m <- skip_unless_master()
  testthat::skip_if_not_installed("plotly")
  expect_s3_class(iadv_hc_country_lines(m), "plotly")
  expect_s3_class(iadv_hc_stream(m), "plotly")
  expect_s3_class(iadv_hc_packed(m), "plotly")
  expect_s3_class(iadv_hc_item(m), "plotly")
})

test_that("J \u00b7 htmltools dashboards 5 widget", {
  m <- skip_unless_master()
  testthat::skip_if_not_installed("htmltools")
  expect_s3_class(iadv_country_card_grid(m), "shiny.tag")
  expect_s3_class(iadv_spark_continents(m), "shiny.tag")
  expect_s3_class(iadv_extreme_pill(m), "shiny.tag")
  expect_s3_class(iadv_top_banner(m), "shiny.tag")
  expect_s3_class(iadv_table_plus_plot(m), "shiny.tag")
})

test_that("ghs_validate_widgets_advanced \u00b7 \u6279\u91cf\u9a8c\u8bc1\u8fd0\u884c", {
  m <- skip_unless_master()
  res <- ghs_validate_widgets_advanced(m)
  expect_gte(length(res), 95)
  ok_n <- sum(vapply(res, function(r) isTRUE(r$ok), logical(1)))
  expect_gte(ok_n, 85)
})

test_that("iadv \u603b\u51fd\u6570\u6570\u91cf\u00b7\u8d85 60", {
  fn <- ls(envir = globalenv(), pattern = "^iadv_[a-z]")
  expect_gte(length(fn), 60)
})
