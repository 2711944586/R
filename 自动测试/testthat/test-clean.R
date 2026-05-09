## 共享 fixture：load_ghs 一次, 后面所有测试都用
ghs_fix <- tryCatch(load_ghs(), error = function(e) NULL)
skip_if_no_data <- function() {
  if (is.null(ghs_fix)) skip("GHED CSV not found")
}

test_that("load_ghs() returns the three expected datasets", {
  skip_if_no_data()
  expect_named(ghs_fix, c("financing_schemes", "health_spending", "spending_purpose"),
               ignore.order = TRUE)
  expect_s3_class(ghs_fix$financing_schemes, "data.frame")
  expect_gt(nrow(ghs_fix$financing_schemes), 1000)
})

test_that("each raw df has the expected required columns", {
  skip_if_no_data()
  for (nm in names(ghs_fix)) {
    expect_true(all(c("country_name", "iso3_code", "year",
                      "indicator_code", "value") %in% names(ghs_fix[[nm]])),
                info = paste("dataset =", nm))
  }
})

test_that("hf1+hf2+hf3+hf4+hfnec ~= 100 within tol", {
  skip_if_no_data()
  ck <- check_scheme_sum(ghs_fix$financing_schemes, tol = 5)
  expect_true(mean(ck$within_tol, na.rm = TRUE) > 0.95)
})

test_that("build_master_wide() has expected key columns", {
  skip_if_no_data()
  mw <- build_master_wide(ghs_fix)
  expect_true(all(c("iso3_code", "country_name", "year",
                    "hf1_che", "hf3_che",
                    "gghed_che", "pvtd_che", "ext_che",
                    "che_usd2023") %in% names(mw)))
  expect_equal(anyDuplicated(mw[, c("iso3_code", "year")]), 0L)
})

test_that("master_wide year range is 2000-2023", {
  skip_if_no_data()
  mw <- build_master_wide(ghs_fix)
  expect_equal(min(mw$year, na.rm = TRUE), 2000)
  expect_equal(max(mw$year, na.rm = TRUE), 2023)
})

test_that("enrich_master() adds continent + income_group", {
  skip_if_no_data()
  mw <- build_master_wide(ghs_fix)
  me <- enrich_master(mw, with_wdi = FALSE)  # 跳过 WDI 网络以加速
  expect_true("continent" %in% names(me))
  expect_true("income_group" %in% names(me))
  expect_true(all(unique(na.omit(me$continent)) %in%
                    c("Africa", "Americas", "Asia", "Europe", "Oceania", "Antarctica")))
})
