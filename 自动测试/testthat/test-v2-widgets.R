# tests/testthat/自动测试/testthat/test-v2-widgets.R
# v2 程序/17-18 widget 工厂烟雾测试

read_master_local <- function() {
  for (p in c("派生数据/处理结果/master_enriched.rds",
              "../派生数据/处理结果/master_enriched.rds",
              "../../派生数据/处理结果/master_enriched.rds",
              "../../../派生数据/处理结果/master_enriched.rds")) {
    if (file.exists(p)) return(readRDS(p))
  }
  NULL
}

test_that("widget_v2_gapminder_bubble returns a plotly object", {
  m <- read_master_local()
  skip_if(is.null(m), "no master")
  skip_if_not_installed("plotly")
  w <- widget_v2_gapminder_bubble(m)
  expect_s3_class(w, "plotly")
})

test_that("widget_v2_highlight_lines returns a plotly object", {
  m <- read_master_local()
  skip_if(is.null(m), "no master")
  skip_if_not_installed("plotly")
  w <- widget_v2_highlight_lines(m, countries = c("USA", "CHN", "BRA"))
  expect_s3_class(w, "plotly")
})

test_that("widget_v2_oops_heatmap returns a plotly object", {
  m <- read_master_local()
  skip_if(is.null(m), "no master")
  skip_if_not_installed("plotly")
  skip_if_not_installed("reshape2")
  w <- widget_v2_oops_heatmap(m, top_n = 10)
  expect_s3_class(w, "plotly")
})

test_that("widget_v2_ternary returns a plotly object", {
  m <- read_master_local()
  skip_if(is.null(m), "no master")
  skip_if_not_installed("plotly")
  w <- widget_v2_ternary(m, year = 2022)
  expect_s3_class(w, "plotly")
})

test_that("widget_v2_income_violin returns a plotly object", {
  m <- read_master_local()
  skip_if(is.null(m), "no master")
  skip_if_not_installed("plotly")
  w <- widget_v2_income_violin(m)
  expect_s3_class(w, "plotly")
})

test_that("widget_v2_splom returns a plotly object", {
  m <- read_master_local()
  skip_if(is.null(m), "no master")
  skip_if_not_installed("plotly")
  w <- widget_v2_splom(m, year = 2022)
  expect_s3_class(w, "plotly")
})

test_that("widget_v2_scenarios returns a plotly object", {
  m <- read_master_local()
  skip_if(is.null(m), "no master")
  skip_if_not_installed("plotly")
  w <- widget_v2_scenarios(m, country_iso = "CHN")
  expect_s3_class(w, "plotly")
})

test_that("widget_v2_reactable_rank returns a reactable object", {
  m <- read_master_local()
  skip_if(is.null(m), "no master")
  skip_if_not_installed("reactable")
  w <- widget_v2_reactable_rank(m, year = 2022)
  expect_s3_class(w, "htmlwidget")
})

test_that("widget_v2_dt_atlas returns a DT/htmlwidget", {
  m <- read_master_local()
  skip_if(is.null(m), "no master")
  skip_if_not_installed("DT")
  w <- widget_v2_dt_atlas(m)
  expect_s3_class(w, "htmlwidget")
})

test_that("widget_v2_country_network returns a forceNetwork", {
  m <- read_master_local()
  skip_if(is.null(m), "no master")
  skip_if_not_installed("networkD3")
  w <- widget_v2_country_network(m, year = 2022, k = 3)
  expect_true(is.null(w) || inherits(w, "htmlwidget"))
})

test_that("widget_v2_sankey_flows returns a sankeyNetwork", {
  m <- read_master_local()
  skip_if(is.null(m), "no master")
  skip_if_not_installed("networkD3")
  w <- widget_v2_sankey_flows(m, year = 2022)
  expect_true(is.null(w) || inherits(w, "htmlwidget"))
})

test_that("widget_v2_kpi_grid returns a div tag", {
  m <- read_master_local()
  skip_if(is.null(m), "no master")
  skip_if_not_installed("htmltools")
  w <- widget_v2_kpi_grid(m, year = 2022)
  expect_true(inherits(w, "shiny.tag") || is.character(w))
})

test_that("narrative_kpi_grid returns HTML", {
  skip_if_not_installed("htmltools")
  h <- narrative_kpi_grid(
    list(value = "$9T", label = "Total CHE"),
    list(value = "182", label = "Countries"))
  expect_true(grepl("kpi-grid", as.character(h), fixed = TRUE))
})

test_that("narrative_callout returns news-callout HTML", {
  skip_if_not_installed("htmltools")
  h <- narrative_callout("test text", source = "GHED", variant = "tip")
  expect_true(grepl("news-callout", as.character(h), fixed = TRUE))
})

test_that("deploy_size_report works on existing 网站发布/", {
  if (!dir.exists("网站发布")) skip("no 网站发布/ to scan")
  rep <- deploy_size_report("网站发布")
  expect_s3_class(rep, "data.frame")
  expect_true(all(c("path", "n_files", "size_mb") %in% names(rep)))
})
