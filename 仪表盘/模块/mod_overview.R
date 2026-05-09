# =============================================================================
# 仪表盘/模块/mod_overview.R
# Tab 1 · 总览：4 KPI + 全球三源面积 + leaflet 世界地图 + OOPS 排行
# =============================================================================

mod_overview_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = htmltools::HTML("&#127759; 总览 Overview"),
    icon  = NULL,
    htmltools::div(
      class = "panel-hero",
      htmltools::h2("Global Health Expenditure Database (GHED) · 仪表盘"),
      htmltools::p(class = "text-muted",
                    "WHO GHED 2024-12 release · 195 国家 · 2000–2023 · 庄颂 (20241334)")
    ),
    bslib::layout_columns(
      col_widths = c(3, 3, 3, 3),
      shiny::uiOutput(ns("kpi_n_countries")),
      shiny::uiOutput(ns("kpi_year_range")),
      shiny::uiOutput(ns("kpi_global_oops")),
      shiny::uiOutput(ns("kpi_global_che_pc"))
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      mod_card(
        title = "全球三源结构演化（2000–latest）",
        mod_spinner(plotly::plotlyOutput(ns("source_area"), height = 360))
      ),
      mod_card(
        title = "各大洲 OOPS 分布（最新年）",
        mod_spinner(plotly::plotlyOutput(ns("oops_box"), height = 360))
      )
    ),
    mod_card(
      title = "世界 OOPS 地图（点击查看详情）",
      mod_spinner(leaflet::leafletOutput(ns("world_map"), height = 480))
    )
  )
}

mod_overview_server <- function(id, master_r, world_sf_obj, year_max) {
  shiny::moduleServer(id, function(input, output, session) {
    output$kpi_n_countries <- shiny::renderUI({
      m <- master_r()
      mod_kpi("覆盖国家 Countries",
              format(length(unique(m$iso3_code)), big.mark = ","),
              color = "primary")
    })
    output$kpi_year_range <- shiny::renderUI({
      m <- master_r()
      mod_kpi("年份范围 Years",
              sprintf("%d – %d", min(m$year, na.rm = TRUE),
                                  max(m$year, na.rm = TRUE)),
              color = "success")
    })
    output$kpi_global_oops <- shiny::renderUI({
      m <- master_r()
      val <- mean(m$hf3_che[m$year == year_max & is.finite(m$hf3_che)],
                  na.rm = TRUE)
      mod_kpi(sprintf("%d 全球 OOPS 均值", year_max),
              fmt_pct(val, 1), color = "danger")
    })
    output$kpi_global_che_pc <- shiny::renderUI({
      m <- master_r()
      val <- stats::median(m$che_pc_usd2023[m$year == year_max &
                                              is.finite(m$che_pc_usd2023)],
                            na.rm = TRUE)
      mod_kpi(sprintf("%d 人均 CHE 中位数", year_max),
              fmt_usd(val), color = "warning")
    })

    output$source_area <- plotly::renderPlotly({
      m <- master_r()
      safe_plotly({
        p <- plot_source_area(m)
        plotly::ggplotly(p, tooltip = c("x", "y", "fill")) |>
          plotly::config(displaylogo = FALSE) |>
          plotly::layout(legend = list(orientation = "h", y = -0.15))
      })
    })

    output$oops_box <- plotly::renderPlotly({
      m <- master_r()
      safe_plotly({
        p <- plot_oops_box_continent(m, year_focus = year_max)
        plotly::ggplotly(p) |> plotly::config(displaylogo = FALSE)
      })
    })

    output$world_map <- leaflet::renderLeaflet({
      m <- master_r()
      if (is.null(world_sf_obj)) {
        return(leaflet::leaflet() |>
                  leaflet::addProviderTiles("CartoDB.Positron") |>
                  leaflet::addLabelOnlyMarkers(0, 0, label = "world_sf 不可用"))
      }
      tryCatch(
        leaflet_choropleth(m, world_sf_obj,
                            indicator_col = "hf3_che",
                            year_focus = year_max,
                            title = sprintf("OOPS %% · %d", year_max)),
        error = function(e) {
          leaflet::leaflet() |>
            leaflet::addProviderTiles("CartoDB.Positron") |>
            leaflet::addLabelOnlyMarkers(0, 0,
              label = paste("Error:", conditionMessage(e)))
        }
      )
    })
  })
}
