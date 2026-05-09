# 自动测试/testthat/test-v2-plots.R
# v2 程序/14-15 静态图工厂的烟雾测试

skip_if_no_master <- function() {
  if (!file.exists("派生数据/处理结果/master_enriched.rds") &&
      !file.exists("../../派生数据/处理结果/master_enriched.rds") &&
      !file.exists("../../../派生数据/处理结果/master_enriched.rds"))
    skip("master_enriched not built")
}

read_master <- function() {
  for (p in c("派生数据/处理结果/master_enriched.rds",
              "../派生数据/处理结果/master_enriched.rds",
              "../../派生数据/处理结果/master_enriched.rds",
              "../../../派生数据/处理结果/master_enriched.rds")) {
    if (file.exists(p)) return(readRDS(p))
  }
  NULL
}

test_that("plot_v2_equity_indices builds a ggplot", {
  skip_if_no_master()
  m <- read_master()
  skip_if(is.null(m), "no master")
  p <- plot_v2_equity_indices(m)
  expect_s3_class(p, "ggplot")
})

test_that("plot_v2_equity_lorenz builds a ggplot", {
  skip_if_no_master()
  m <- read_master()
  skip_if(is.null(m), "no master")
  p <- plot_v2_equity_lorenz(m, years = c(2000, 2023))
  expect_s3_class(p, "ggplot")
})

test_that("plot_v2_fiscal_ghe_share builds a ggplot", {
  skip_if_no_master()
  m <- read_master()
  skip_if(is.null(m), "no master")
  p <- plot_v2_fiscal_ghe_share(m, year = 2022)
  expect_s3_class(p, "ggplot")
})

test_that("plot_v2_efficiency_dea builds a ggplot", {
  skip_if_no_master()
  m <- read_master()
  skip_if(is.null(m), "no master")
  p <- plot_v2_efficiency_dea(m, year = 2021)
  expect_s3_class(p, "ggplot")
})

test_that("plot_v2_outcomes_elasticity builds a ggplot", {
  skip_if_no_master()
  m <- read_master()
  skip_if(is.null(m), "no master")
  p <- plot_v2_outcomes_elasticity(m)
  expect_s3_class(p, "ggplot")
})

test_that("plot_v2_aid_dependency builds a ggplot", {
  skip_if_no_master()
  m <- read_master()
  skip_if(is.null(m), "no master")
  p <- plot_v2_aid_dependency(m, year = 2021)
  expect_s3_class(p, "ggplot")
})

test_that("plot_v2_combined_ridges builds a ggplot", {
  skip_if_no_master()
  m <- read_master()
  skip_if(is.null(m), "no master")
  p <- plot_v2_combined_ridges(m)
  expect_s3_class(p, "ggplot")
})

test_that("plot_v2_covid_dumbbell builds a ggplot", {
  skip_if_no_master()
  m <- read_master()
  skip_if(is.null(m), "no master")
  p <- plot_v2_covid_dumbbell(m)
  expect_s3_class(p, "ggplot")
})

test_that("plot_v2_slope_oops builds a ggplot", {
  skip_if_no_master()
  m <- read_master()
  skip_if(is.null(m), "no master")
  p <- plot_v2_slope_oops(m)
  expect_s3_class(p, "ggplot")
})

test_that("plot_v2_continent_stream builds a ggplot", {
  skip_if_no_master()
  m <- read_master()
  skip_if(is.null(m), "no master")
  p <- plot_v2_continent_stream(m)
  expect_s3_class(p, "ggplot")
})

test_that("ghs_export_v2_thematic returns count > 0", {
  skip_if_no_master()
  m <- read_master()
  skip_if(is.null(m), "no master")
  tmp <- tempfile()
  dir.create(tmp)
  n <- ghs_export_v2_thematic(m, out_dir = tmp, verbose = FALSE)
  expect_gte(n, 6)   # 允许部分失败
  pngs <- list.files(tmp, pattern = "\\.png$", full.names = TRUE)
  expect_true(length(pngs) >= 6)
  unlink(tmp, recursive = TRUE)
})

test_that("data_quality_missing returns expected structure", {
  skip_if_no_master()
  m <- read_master()
  skip_if(is.null(m), "no master")
  res <- data_quality_missing(m)
  expect_named(res, c("by_var", "by_year", "by_income", "total_pct_missing"),
                ignore.order = TRUE)
  expect_gte(nrow(res$by_var), 10)
  expect_true(is.numeric(res$total_pct_missing))
})

test_that("data_quality_consistency returns hf+source checks", {
  skip_if_no_master()
  m <- read_master()
  skip_if(is.null(m), "no master")
  res <- data_quality_consistency(m, tol = 5)
  expect_true("hf" %in% names(res) || "source" %in% names(res))
  if (!is.null(res$source))
    expect_gte(res$source$pct_within_tol, 50)
})
