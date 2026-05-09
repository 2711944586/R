test_that("gini_weighted equals 0 for equal vector", {
  expect_equal(gini_weighted(rep(10, 100)), 0, tolerance = 1e-8)
})

test_that("gini_weighted is monotonic in dispersion", {
  set.seed(1)
  small <- runif(100, 9, 11)         # 几乎均匀
  big   <- c(rep(0.01, 99), 100)     # 极不平等
  expect_lt(gini_weighted(small), gini_weighted(big))
  expect_gte(gini_weighted(big), 0.9)
})

test_that("gini_weighted handles weights", {
  x <- c(1, 100); w <- c(99, 1)
  # 大多数人是低收入：基尼应较高
  g <- gini_weighted(x, w)
  expect_true(is.finite(g) && g > 0.4)
})

test_that("theil_t is non-negative", {
  set.seed(2)
  expect_gte(theil_t(runif(50, 1, 100)), 0)
})

test_that("atkinson is in [0, 1) for positive vec", {
  set.seed(3)
  x <- runif(50, 1, 50)
  for (eps in c(0.5, 1, 2)) {
    a <- atkinson(x, eps = eps)
    expect_gte(a, 0)
    expect_lt(a, 1)
  }
})

test_that("top_k_share returns share between 0 and 1", {
  df <- tibble::tibble(v = c(rep(1, 100), 100))
  s <- top_k_share(df, "v", k = 1)
  expect_equal(s$top_k_sum, 100)
  expect_true(s$share > 0 && s$share < 1)
})

test_that("struct_change is 0 for identical vectors", {
  v <- c(0.5, 0.3, 0.2)
  expect_equal(struct_change(v, v), 0, tolerance = 1e-10)
})

test_that("struct_change is positive for different vectors", {
  expect_gt(struct_change(c(1, 0, 0), c(0, 1, 0)), 0.5)
})
