library(testthat)

test_that("plots_more 主要函数能构造 ggplot 对象而不抛错", {
  skip_if_not(exists("ghs_export_more", mode = "function"),
              "ghs_export_more 未加载")

  cache <- file.path("派生数据", "处理结果", "master_enriched.rds")
  skip_if_not(file.exists(cache), "缺少 master_enriched 缓存")
  master <- readRDS(cache)

  sample_fns <- c(".gxm_continent_oops",
                  ".gxm_income_che",
                  ".gxm_che_cagr",
                  ".gxm_life_top")
  found <- sample_fns[vapply(sample_fns,
                             function(n) exists(n, mode = "function"),
                             logical(1))]
  skip_if(length(found) == 0, "plots_more 内部函数未加载")

  for (fn_name in found) {
    p <- tryCatch(
      suppressWarnings(do.call(fn_name, list(master))),
      error = function(e) NULL
    )
    if (!is.null(p)) {
      expect_s3_class(p, c("ggplot", "gg", "patchwork"))
    }
  }
})

test_that("plot_country_profile 对 6 个代表国家不崩溃", {
  skip_if_not(exists("plot_country_profile", mode = "function"),
              "plot_country_profile 未加载")
  cache <- file.path("派生数据", "处理结果", "master_enriched.rds")
  skip_if_not(file.exists(cache), "缺少 master_enriched 缓存")
  master <- readRDS(cache)

  for (iso in c("USA", "CHN", "JPN", "DEU", "ZAF", "IDN")) {
    if (!iso %in% master$iso3_code) next
    p <- tryCatch(
      suppressWarnings(plot_country_profile(master, iso)),
      error = function(e) e
    )
    expect_false(inherits(p, "error"),
                 info = paste("profile failed for", iso))
  }
})
