library(testthat)

test_that("widgets_more 导出器能在临时目录下运行并生成 HTML", {
  skip_if_not(exists("ghs_export_widgets_more", mode = "function"),
              "ghs_export_widgets_more 未加载")
  skip_if_not(requireNamespace("htmlwidgets", quietly = TRUE),
              "htmlwidgets 未安装")
  skip_if_not(requireNamespace("plotly", quietly = TRUE),
              "plotly 未安装")

  cache <- file.path("派生数据", "处理结果", "master_enriched.rds")
  skip_if_not(file.exists(cache), "缺少 master_enriched 缓存")
  master <- readRDS(cache)

  out_dir <- tempfile("widgets_more_")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  res <- suppressWarnings(tryCatch(
    ghs_export_widgets_more(master,
                            out_dir = out_dir,
                            verbose = FALSE),
    error = function(e) e
  ))
  expect_false(inherits(res, "error"),
               info = "ghs_export_widgets_more 抛出了错误")

  htmls <- list.files(out_dir, pattern = "[.]html$", full.names = TRUE)
  expect_gt(length(htmls), 5L)

  sizes <- file.size(htmls)
  expect_true(all(sizes > 1000),
              info = "某些 widget HTML 文件过小（< 1KB），可能是空 shell")
})
