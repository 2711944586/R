# 自动测试/testthat/test-maps-advanced.R
# B2 高级地图集 plot_map_*

.proj_root <- (function() {
  cwd <- getwd()
  for (up in 0:5) {
    candidate <- do.call(file.path, c(list(cwd), as.list(rep("..", up))))
    candidate <- normalizePath(candidate, mustWork = FALSE)
    if (file.exists(file.path(candidate, "DESCRIPTION"))) return(candidate)
  }
  cwd
})()

skip_unless_master_and_sf <- function() {
  if (!requireNamespace("sf", quietly = TRUE))
    testthat::skip("sf \u4e0d\u53ef\u7528")
  m_path <- file.path(.proj_root, "\u6d3e\u751f\u6570\u636e",
                       "\u5904\u7406\u7ed3\u679c", "master_enriched.rds")
  s_path <- file.path(.proj_root, "\u6d3e\u751f\u6570\u636e",
                       "\u5904\u7406\u7ed3\u679c", "world_sf_medium.rds")
  if (!file.exists(m_path) || !file.exists(s_path))
    testthat::skip("\u7f3a\u5c11 master / world_sf \u7f13\u5b58")
  list(master = readRDS(m_path), sf = readRDS(s_path))
}

# 显式 source 31_maps_advanced.R（若 helper 未加载）
local({
  f <- file.path(.proj_root, "\u7a0b\u5e8f", "31_maps_advanced.R")
  if (!exists("plot_map_world_var", mode = "function") && file.exists(f)) {
    source(f, encoding = "UTF-8")
  }
})

test_that("5 \u4e2a\u5168\u7403 choropleth \u5305\u88c5\u80fd\u8fd4\u56de ggplot", {
  d <- skip_unless_master_and_sf()
  expect_s3_class(plot_map_che_pc(d$master, d$sf), "ggplot")
  expect_s3_class(plot_map_gghed(d$master, d$sf), "ggplot")
  expect_s3_class(plot_map_oops(d$master, d$sf), "ggplot")
  expect_s3_class(plot_map_lifeexp(d$master, d$sf), "ggplot")
  expect_s3_class(plot_map_u5mr(d$master, d$sf), "ggplot")
})

test_that("\u5927\u6d32\u7f29\u653e 6 \u56fe\u80fd\u8fd4\u56de ggplot", {
  d <- skip_unless_master_and_sf()
  for (cnt in c("Africa", "Asia", "Europe", "Americas", "Oceania")) {
    p <- plot_map_continent(d$master, d$sf, cnt, "che_pc_usd2023",
                              trans = "log10")
    expect_s3_class(p, "ggplot")
  }
})

test_that("Bivariate \u4e09\u56fe\u8fd4\u56de ggplot \u6216 cowplot \u7ec4\u5408", {
  d <- skip_unless_master_and_sf()
  p1 <- plot_map_bivariate_che_life(d$master, d$sf)
  p2 <- plot_map_bivariate_gghed_oops(d$master, d$sf)
  p3 <- plot_map_bivariate_gghed_u5mr(d$master, d$sf)
  # ggdraw 返回 ggplot 子类
  expect_true(inherits(p1, "ggplot"))
  expect_true(inherits(p2, "ggplot"))
  expect_true(inherits(p3, "ggplot"))
})

test_that("\u53d8\u5316 3 \u56fe\u80fd\u8fd4\u56de ggplot", {
  d <- skip_unless_master_and_sf()
  expect_s3_class(plot_map_che_pc_change(d$master, d$sf), "ggplot")
  expect_s3_class(plot_map_oops_change(d$master, d$sf), "ggplot")
  expect_s3_class(plot_map_lifeexp_change(d$master, d$sf), "ggplot")
})

test_that("\u5206\u4f4d\u4e0e\u6c14\u6ce1\u56fe\u80fd\u8fd4\u56de ggplot", {
  d <- skip_unless_master_and_sf()
  expect_s3_class(plot_map_quintile(d$master, d$sf, "che_pc_usd2023"),
                   "ggplot")
  expect_s3_class(plot_map_quintile(d$master, d$sf, "gghed_che",
                                       palette = "ocean"), "ggplot")
  expect_s3_class(plot_map_bubble(d$master, d$sf, "che_usd2023"),
                   "ggplot")
})

test_that("\u591a\u65f6\u70b9 small multiples \u8fd4\u56de ggplot", {
  d <- skip_unless_master_and_sf()
  p <- plot_map_smallmultiples(d$master, d$sf, "che_pc_usd2023",
                                  years = c(2000, 2010, 2018, 2022),
                                  trans = "log10")
  expect_s3_class(p, "ggplot")
})

test_that(".bivariate_class \u8fd4\u56de 9 \u4e2a\u53ef\u80fd\u503c", {
  x <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)
  y <- c(9, 8, 7, 6, 5, 4, 3, 2, 1)
  cls <- .bivariate_class(x, y)
  expect_true(all(nchar(cls) == 2))
  expect_true(all(substr(cls, 1, 1) %in% c("A", "B", "C")))
  expect_true(all(substr(cls, 2, 2) %in% c("1", "2", "3")))
})
