library(testthat)

test_that("feature mart exporter sanity", {
  skip_if_not(exists("ghs_export_feature_mart", mode = "function"),
              "ghs_export_feature_mart 未加载")

  cache <- file.path("派生数据", "处理结果", "master_enriched.rds")
  skip_if_not(file.exists(cache), "缺少 master_enriched 缓存")
  master <- readRDS(cache)

  out_dir <- tempfile("feature_mart_")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  res <- suppressWarnings(
    ghs_export_feature_mart(master,
                            out_dir = out_dir,
                            verbose = FALSE)
  )

  csv_path <- file.path(out_dir, "feature_mart_country_year.csv")
  dict_path <- file.path(out_dir, "feature_dictionary.csv")
  expect_true(file.exists(csv_path))
  expect_true(file.exists(dict_path))

  dict <- read.csv(dict_path, stringsAsFactors = FALSE)
  expect_true(all(c("variable", "type", "role", "missingness") %in%
                    names(dict)))
  expect_gt(nrow(dict), 20L)

  mart <- read.csv(csv_path, stringsAsFactors = FALSE)
  expect_true(all(c("iso3_code", "year", "country_name") %in% names(mart)))
  expect_gt(nrow(mart), 3000L)
})
