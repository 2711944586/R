# 自动测试/testthat/test-v3-models.R
# D1 + D2: 面板与稳健模型扩展

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
  for (f in c("\u7a0b\u5e8f/38_models_panel.R",
              "\u7a0b\u5e8f/39_models_robust.R"))
    if (file.exists(f)) source(f, encoding = "UTF-8")
})

test_that("D1 \u00b7 \u9762\u677f\u6a21\u578b \u00b7 5 \u6837\u672c", {
  m <- skip_unless_master()
  testthat::skip_if_not_installed("fixest")
  expect_s3_class(mp_panel_twoway_fe(m), "fixest")
  expect_s3_class(mp_panel_oop_fe(m), "fixest")
  expect_s3_class(mp_panel_lifeexp(m), "fixest")
  expect_s3_class(mp_panel_u5mr(m), "fixest")
  expect_s3_class(mp_panel_pc1_trend(m), "fixest")
})

test_that("D1 \u00b7 \u968f\u673a\u6548\u5e94 + Mundlak", {
  m <- skip_unless_master()
  testthat::skip_if_not_installed("lme4")
  testthat::skip_if_not_installed("fixest")
  expect_s4_class(mp_panel_re(m), "lmerMod")
  expect_s3_class(mp_panel_mundlak(m), "fixest")
})

test_that("D1 \u00b7 \u9762\u677f \u6536\u655b/IV/\u6743\u91cd", {
  m <- skip_unless_master()
  expect_s3_class(mp_panel_growth_init(m), "lm")
  expect_s3_class(mp_panel_first_diff(m), "lm")
  expect_s3_class(mp_panel_pop_weighted(m), "lm")
})

test_that("D2 \u00b7 \u5206\u4f4d\u56de\u5f52", {
  m <- skip_unless_master()
  testthat::skip_if_not_installed("quantreg")
  fit <- mr_quantile_reg(m)
  expect_s3_class(fit, "rqs")
})

test_that("D2 \u00b7 \u9c81\u68d2 OLS / Theil-Sen", {
  m <- skip_unless_master()
  testthat::skip_if_not_installed("MASS")
  expect_s3_class(mr_rlm(m), "rlm")
})

test_that("D2 \u00b7 GAM \u4e0e\u591a\u9879\u5f0f", {
  m <- skip_unless_master()
  testthat::skip_if_not_installed("mgcv")
  expect_s3_class(mr_gam_lifeexp(m), "gam")
  expect_s3_class(mr_gam_oop(m), "gam")
  expect_s3_class(mr_poly_lifeexp(m), "lm")
})

test_that("D2 \u00b7 Bootstrap / Jackknife", {
  m <- skip_unless_master()
  res_b <- mr_boot_elasticity(m, B = 50)
  expect_named(res_b, c("beta", "se", "ci", "B"))
  expect_true(is.numeric(res_b$beta))
  res_j <- mr_jackknife(m)
  expect_true(is.numeric(res_j$mean))
})

test_that("D2 \u00b7 DiD \u4e0e segmented \u4e0e Tukey", {
  m <- skip_unless_master()
  testthat::skip_if_not_installed("fixest")
  expect_s3_class(mr_did_covid(m), "fixest")
  testthat::skip_if_not_installed("segmented")
  expect_s3_class(mr_segmented_che(m), "segmented")
  expect_s3_class(mr_tukey_oop(m), "TukeyHSD")
})

test_that("D2 \u00b7 \u9c81\u68d2\u6807\u51c6\u8bef HC3 \u00b7 cluster", {
  m <- skip_unless_master()
  testthat::skip_if_not_installed("sandwich")
  testthat::skip_if_not_installed("lmtest")
  expect_s3_class(mr_robust_se(m), "coeftest")
  expect_s3_class(mr_cluster_se(m), "coeftest")
})

test_that("\u6a21\u578b\u603b\u51fd\u6570\u6570\u00b7\u8d85 45", {
  fn <- c(ls(envir = globalenv(), pattern = "^fit_"),
           ls(envir = globalenv(), pattern = "^mp_[a-z]"),
           ls(envir = globalenv(), pattern = "^mr_[a-z]"))
  expect_gte(length(fn), 45)
})

test_that("\u6279\u91cf\u9a8c\u8bc1\u5668\u00b7\u9762\u677f", {
  m <- skip_unless_master()
  res <- ghs_validate_models_panel(m)
  ok_n <- sum(vapply(res, function(r) isTRUE(r[["ok"]]) &&
                                       !isTRUE(r[["is_skip"]]), logical(1)))
  expect_gte(ok_n, 15)
})

test_that("\u6279\u91cf\u9a8c\u8bc1\u5668\u00b7\u9c81\u68d2", {
  m <- skip_unless_master()
  res <- ghs_validate_models_robust(m)
  ok_n <- sum(vapply(res, function(r) isTRUE(r[["ok"]]) &&
                                       !isTRUE(r[["is_skip"]]), logical(1)))
  expect_gte(ok_n, 15)
})
