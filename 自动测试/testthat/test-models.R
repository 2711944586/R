test_that("fit_pca returns NULL on insufficient data", {
  df <- tibble::tibble(iso3_code = letters[1:3], country_name = letters[1:3],
                       a = 1:3, b = 4:6)
  expect_warning(p <- fit_pca(df, vars = c("a", "b")))
  expect_null(p)
})

test_that("fit_pca returns expected structure", {
  set.seed(4)
  df <- tibble::tibble(
    iso3_code   = paste0("C", 1:30),
    country_name = paste0("Country", 1:30),
    a = rnorm(30), b = rnorm(30), c = rnorm(30)
  )
  p <- fit_pca(df, vars = c("a", "b", "c"))
  expect_named(p, c("pca", "scores", "loadings", "var_explained"),
               ignore.order = TRUE)
  expect_s3_class(p$pca, "prcomp")
  expect_true(all(c("PC1", "PC2", "PC3") %in% names(p$scores)))
})

test_that("fit_cluster adds 'cluster' factor", {
  set.seed(5)
  df <- tibble::tibble(
    iso3_code   = paste0("C", 1:40),
    country_name = paste0("Country", 1:40),
    a = rnorm(40), b = rnorm(40), c = rnorm(40), d = rnorm(40)
  )
  pca <- fit_pca(df, vars = c("a", "b", "c", "d"))
  clu <- fit_cluster(pca, k = 3)
  expect_true("cluster" %in% names(clu$scores))
  expect_s3_class(clu$scores$cluster, "factor")
  expect_equal(length(levels(clu$scores$cluster)), 3)
})

test_that("detect_changepoints returns NULL when changepoint pkg missing or short", {
  set.seed(6)
  res <- detect_changepoints(rnorm(5), 2000:2004)
  expect_null(res)
})

test_that("fit_forecast works when forecast pkg available", {
  skip_if_not_installed("forecast")
  set.seed(7)
  res <- fit_forecast(cumsum(rnorm(20, 0.5, 1)), years = 2000:2019, h = 3)
  if (!is.null(res)) {
    expect_true(all(c("year", "point", "lo_80", "hi_80") %in% names(res)))
    expect_equal(nrow(res), 3)
  }
})
