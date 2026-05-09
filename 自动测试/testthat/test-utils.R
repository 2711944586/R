test_that("ghs_indicator_prefix() splits codes correctly", {
  expect_equal(ghs_indicator_prefix("hf1_che"),     "hf1")
  expect_equal(ghs_indicator_prefix("gghed_usd2023"), "gghed")
  expect_equal(ghs_indicator_prefix("hc6_che"),     "hc6")
  expect_equal(ghs_indicator_prefix("ext_che"),     "ext")
})

test_that("ghs_indicator_unit() classifies units", {
  expect_equal(ghs_indicator_unit("hf1_che"),       "%_che")
  expect_equal(ghs_indicator_unit("hf1_usd2023"),   "usd2023")
  expect_true(is.na(ghs_indicator_unit("foobar")))
})

test_that("formatters work", {
  expect_match(fmt_usd(1234567890), "^\\$")
  expect_match(fmt_pct(45.67), "%$")
  expect_equal(fmt_pct(NA), "—")
})

test_that("`%||%` returns rhs only when lhs is NULL", {
  expect_equal(NULL %||% "fallback", "fallback")
  expect_equal("primary" %||% "fallback", "primary")
  expect_equal(0   %||% 99, 0)
  expect_equal(NA  %||% 99, NA)
})

test_that("proj_root() returns a directory", {
  r <- proj_root()
  expect_true(is.character(r) && length(r) == 1)
  expect_true(dir.exists(r))
})
