# 自动测试/testthat/test-v3-widgets-map.R
# C1: 交互地图 widget imap_*

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

skip_unless_world <- function() {
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
                     "\u5904\u7406\u7ed3\u679c", "world_sf_medium.rds")
  if (!file.exists(cache)) testthat::skip("\u7f3a\u5c11 world_sf \u7f13\u5b58")
  invisible(readRDS(cache))
}

local({
  f <- file.path("\u7a0b\u5e8f", "36_widgets_map.R")
  if (!exists("imap_che_pc", mode = "function") && file.exists(f)) {
    source(f, encoding = "UTF-8")
  }
})

test_that("leaflet \u5355\u6307\u6807 5 \u4e2a widget \u80fd\u8fd4\u56de leaflet", {
  m  <- skip_unless_master()
  sf <- skip_unless_world()
  expect_s3_class(imap_che_pc(m, sf), "leaflet")
  expect_s3_class(imap_oop(m, sf), "leaflet")
  expect_s3_class(imap_gghed(m, sf), "leaflet")
  expect_s3_class(imap_lifeexp(m, sf), "leaflet")
  expect_s3_class(imap_u5mr(m, sf), "leaflet")
})

test_that("\u4e94\u5206\u4f4d / bivariate / \u53d8\u5316 5 widget \u80fd\u8fd4\u56de leaflet", {
  m  <- skip_unless_master()
  sf <- skip_unless_world()
  expect_s3_class(imap_quintile_che_pc(m, sf), "leaflet")
  expect_s3_class(imap_bivariate(m, sf), "leaflet")
  expect_s3_class(imap_change_che_pc(m, sf), "leaflet")
  expect_s3_class(imap_change_oop(m, sf), "leaflet")
  expect_s3_class(imap_efficiency(m, sf), "leaflet")
})

test_that("\u6c14\u6ce1 / \u70b9 / \u9ad8\u4eae 5 widget \u80fd\u8fd4\u56de leaflet", {
  m  <- skip_unless_master()
  sf <- skip_unless_world()
  expect_s3_class(imap_bubble_che_total(m, sf), "leaflet")
  expect_s3_class(imap_bubble_oop_continent(m, sf), "leaflet")
  expect_s3_class(imap_top10_oop(m, sf), "leaflet")
  expect_s3_class(imap_country_points(m, sf), "leaflet")
  expect_s3_class(imap_highlight_country(m, sf, "USA"), "leaflet")
})

test_that("plotly \u00b7 \u53cc\u9762 \u00b7 \u5e74 layer 5 widget \u80fd\u751f\u6210", {
  m  <- skip_unless_master()
  sf <- skip_unless_world()
  testthat::skip_if_not_installed("plotly")
  expect_s3_class(imap_plotly_animation(m, sf), "plotly")
  expect_s3_class(imap_plotly_choropleth(m, sf), "plotly")
  expect_s3_class(imap_plotly_density(m, sf), "plotly")
  expect_s3_class(imap_dual_compare(m, sf), "shiny.tag")
  expect_s3_class(imap_layer_years(m, sf), "leaflet")
})

test_that("ghs_validate_widgets_map \u8fd4\u56de 20 \u6761\u8bb0\u5f55", {
  m  <- skip_unless_master()
  sf <- skip_unless_world()
  res <- ghs_validate_widgets_map(m, sf)
  expect_equal(length(res), 20)
  ok_n <- sum(vapply(res, function(r) isTRUE(r$ok), logical(1)))
  expect_gte(ok_n, 18)
})
