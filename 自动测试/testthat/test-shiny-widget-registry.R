


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
  widgets <- file.path(proj_root, "仪表盘", "模块", "mod_widgets.R")
  if (file.exists(widgets)) source(widgets, encoding = "UTF-8")
})

test_that("mod_widget_registry 返回完整且唯一的组件目录", {
  reg <- mod_widget_registry()
  expect_s3_class(reg, "data.frame")
  expect_true(all(c("id", "title", "desc", "source", "fn", "type",
                    "deps", "needs_world", "group") %in% names(reg)))
  expect_equal(anyDuplicated(reg$id), 0)
  expect_true(all(nzchar(reg$id)))
  expect_true(all(nzchar(reg$title)))
  expect_true(all(nzchar(reg$fn)))
  expect_true(all(reg$type %in% c("plotly", "leaflet", "reactable", "dt", "ui")))
  expect_gte(nrow(reg), 100)
})

test_that("registry 覆盖既有四类交互函数前缀", {
  reg <- mod_widget_registry()
  expect_true(any(grepl("^widget_v2_", reg$fn)))
  expect_true(any(grepl("^widget_more_", reg$fn)))
  expect_true(any(grepl("^imap_", reg$fn)))
  expect_true(any(grepl("^iadv_", reg$fn)))
})

test_that("registry 中登记的函数在当前环境可发现", {
  reg <- mod_widget_registry()
  exists_vec <- vapply(reg$fn, exists, logical(1), mode = "function")
  expect_true(all(exists_vec))
})

test_that("非 Plotly htmlwidget 与必填文本组件有安全登记", {
  reg <- mod_widget_registry()
  type_of <- function(fn) reg$type[match(fn, reg$fn)]

  expect_equal(type_of("iadv_hc_country_lines"), "ui")
  expect_equal(type_of("iadv_ec_calendar"), "ui")
  expect_equal(type_of("iadv_finding_card"), "ui")
  expect_equal(type_of("iadv_quote_callout"), "ui")

  finding <- reg$params[[match("iadv_finding_card", reg$fn)]]
  quote <- reg$params[[match("iadv_quote_callout", reg$fn)]]
  expect_true(all(c("title", "abstract", "evidence_text") %in% names(finding)))
  expect_true(all(c("quote_text", "attrib") %in% names(quote)))
})

test_that("组件输出 UI 根据类型生成正确容器", {
  ns <- shiny::NS("widgets")
  is_tagish <- function(x) inherits(x, "shiny.tag") || inherits(x, "shiny.tag.list")
  expect_true(is_tagish(mod_widget_output_ui(ns, "demo_plot", "plotly")))
  expect_true(is_tagish(mod_widget_output_ui(ns, "demo_map", "leaflet")))
  expect_true(is_tagish(mod_widget_output_ui(ns, "demo_table", "reactable")))
  expect_true(is_tagish(mod_widget_output_ui(ns, "demo_dt", "dt")))
  expect_true(is_tagish(mod_widget_output_ui(ns, "demo_ui", "ui")))
})

test_that("Widgets 组件中枢在筛选为空时不回退到无关组件", {
  testthat::skip_if_not_installed("shiny")
  master <- data.frame(
    iso3_code = "CHN", country_name = "China", year = 2023,
    continent = "Asia", income_group = "Upper middle income",
    che_pc_usd2023 = 1000, hf3_che = 20, gghed_che = 55,
    stringsAsFactors = FALSE
  )
  suppressWarnings(shiny::testServer(
    mod_widgets_server,
    args = list(master_r = shiny::reactive(master), world_sf = NULL),
    {
      session$setInputs(query = "no-such-widget-token-xyz")
      expect_equal(nrow(filtered_r()), 0)
      expect_null(current_spec())
    }
  ))
})
