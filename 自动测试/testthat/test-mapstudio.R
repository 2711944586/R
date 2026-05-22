


skip_unless_master_mapstudio <- function() {
  proj <- (function() {
    cwd <- getwd()
    for (up in 0:5) {
      candidate <- do.call(file.path, c(list(cwd), as.list(rep("..", up))))
      candidate <- normalizePath(candidate, mustWork = FALSE)
      if (file.exists(file.path(candidate, "DESCRIPTION"))) return(candidate)
    }
    cwd
  })()
  cache <- file.path(proj, "派生数据", "处理结果", "master_enriched.rds")
  if (!file.exists(cache)) testthat::skip("缺少 master_enriched 缓存")
  readRDS(cache)
}

skip_unless_world_mapstudio <- function() {
  proj <- (function() {
    cwd <- getwd()
    for (up in 0:5) {
      candidate <- do.call(file.path, c(list(cwd), as.list(rep("..", up))))
      candidate <- normalizePath(candidate, mustWork = FALSE)
      if (file.exists(file.path(candidate, "DESCRIPTION"))) return(candidate)
    }
    cwd
  })()
  cache <- file.path(proj, "派生数据", "处理结果", "world_sf_medium.rds")
  if (!file.exists(cache)) testthat::skip("缺少 world_sf 缓存")
  readRDS(cache)
}

local({
  proj_root <- (function() {
    cwd <- getwd()
    for (up in 0:5) {
      candidate <- do.call(file.path, c(list(cwd), as.list(rep("..", up))))
      candidate <- normalizePath(candidate, mustWork = FALSE)
      if (file.exists(file.path(candidate, "DESCRIPTION"))) return(candidate)
    }
    cwd
  })()
  helpers <- file.path(proj_root, "仪表盘", "模块", "_helpers.R")
  if (file.exists(helpers)) source(helpers, encoding = "UTF-8")
})

mapstudio_default_spec <- function(master) {
  list(
    mode = "choropleth",
    indicator = "hf3_che",
    year = max(master$year, na.rm = TRUE),
    year_start = min(master$year, na.rm = TRUE),
    year_end = max(master$year, na.rm = TRUE),
    scale = "auto",
    basemap = "positron",
    continents = character(),
    incomes = character(),
    iso = "CHN"
  )
}

leaflet_has_layer_id <- function(map) {
  calls <- map$x$calls
  poly <- calls[vapply(calls, function(x) identical(x$method, "addPolygons"), logical(1))]
  if (!length(poly)) return(FALSE)
  any(vapply(poly, function(x) {
    layer_ids <- x$args[[2]]
    is.character(layer_ids) && any(grepl("^[A-Z0-9-]{2,4}$", layer_ids))
  }, logical(1)))
}

test_that("Map Studio 单指标地图返回 leaflet 且包含 layerId", {
  testthat::skip_if_not_installed("leaflet")
  testthat::skip_if_not_installed("sf")
  m <- skip_unless_master_mapstudio()
  sf <- skip_unless_world_mapstudio()
  map <- mod_mapstudio_leaflet(mapstudio_default_spec(m), m, sf)
  expect_s3_class(map, "leaflet")
  expect_true(leaflet_has_layer_id(map))
})

test_that("Map Studio 变化、气泡、图层模式可生成 leaflet", {
  testthat::skip_if_not_installed("leaflet")
  testthat::skip_if_not_installed("sf")
  m <- skip_unless_master_mapstudio()
  sf <- skip_unless_world_mapstudio()
  spec <- mapstudio_default_spec(m)

  spec$mode <- "change_oop"
  expect_s3_class(mod_mapstudio_leaflet(spec, m, sf), "leaflet")

  spec$mode <- "bubble_oop"
  expect_s3_class(mod_mapstudio_leaflet(spec, m, sf), "leaflet")

  spec$mode <- "year_layers"
  expect_s3_class(mod_mapstudio_leaflet(spec, m, sf), "leaflet")
})

test_that("Map Studio 窄筛选下的特殊模式保持稳定", {
  testthat::skip_if_not_installed("leaflet")
  testthat::skip_if_not_installed("sf")
  m <- skip_unless_master_mapstudio()
  sf <- skip_unless_world_mapstudio()
  spec <- mapstudio_default_spec(m)
  spec$continents <- "Asia"
  spec$incomes <- "High income"

  for (mode in c("change_che_pc", "change_oop", "bivariate", "efficiency")) {
    spec$mode <- mode
    expect_s3_class(mod_mapstudio_leaflet(spec, m, sf), "leaflet")
  }
})

test_that("Map Studio Plotly 地图和动画可生成 plotly", {
  testthat::skip_if_not_installed("plotly")
  m <- skip_unless_master_mapstudio()
  spec <- mapstudio_default_spec(m)

  spec$mode <- "plotly_choropleth"
  expect_s3_class(mod_mapstudio_plotly(spec, m), "plotly")

  spec$mode <- "plotly_animation"
  expect_s3_class(mod_mapstudio_plotly(spec, m), "plotly")
})

test_that("Map Studio 指标标签和格式化函数稳定", {
  expect_equal(mod_mapstudio_metric_label("hf3_che"), "OOPS / CHE")
  expect_match(mod_mapstudio_metric_value(36.45, "hf3_che"), "36.5%")
  expect_match(mod_mapstudio_metric_value(1328, "che_pc_usd2023"), "\\$")
})
