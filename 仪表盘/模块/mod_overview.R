# =============================================================================
# 仪表盘/模块/mod_overview.R
# Tab 1 · 总览：4 KPI + 全球三源面积 + leaflet 世界地图 + OOPS 排行
# =============================================================================

mod_overview_ui <- function(id) {
  ns <- shiny::NS(id)

  module_card <- function(target, title, desc, kicker) {
    htmltools::tags$button(
      class = "module-card",
      type  = "button",
      onclick = sprintf(
        "Shiny.setInputValue('%s', '%s', {priority:'event'});",
        ns("nav_to"), target),
      htmltools::span(class = "module-kicker", kicker),
      htmltools::strong(class = "module-title", title),
      htmltools::span(class = "module-desc", desc)
    )
  }

  bslib::nav_panel(
    title = htmltools::HTML("&#127759; \u603b\u89c8 Overview"),
    icon  = NULL,
    htmltools::div(
      class = "v2-hero",
      htmltools::div(
        class = "v2-hero-inner",
        htmltools::span(class = "v2-hero-kicker",
                        "Global Health Expenditure Database 2024-12"),
        htmltools::h1(class = "v2-hero-title",
                      "全球卫生支出 · 资金、结构与结果的 24 年长卷"),
        htmltools::p(class = "v2-hero-lead",
                     paste("基于 WHO GHED + WDI，对 195 个国家 2000–2023",
                           "年的医疗资金来源、政府/私人/外援构成、人均水平",
                           "和健康产出进行系统化建模与可视化。")),
        htmltools::div(
          class = "v2-hero-meta",
          htmltools::span("\u4f5c\u8005 \u5e84\u9882 (20241334)"),
          htmltools::span("\u00b7"),
          htmltools::span("12 \u4e2a Shiny \u6a21\u5757 \u00b7 94 \u5f20\u9759\u6001\u56fe \u00b7 42 \u4e2a\u4ea4\u4e92\u7ec4\u4ef6"),
          htmltools::span("\u00b7"),
          htmltools::tags$a(
            href = "https://2711944586.github.io/R/", target = "_blank",
            "\u9759\u6001\u9996\u9875"),
          htmltools::span("\u00b7"),
          htmltools::tags$a(
            href = "https://github.com/2711944586/R", target = "_blank",
            "GitHub")
        )
      )
    ),
    htmltools::div(
      class = "v2-kpi-grid",
      shiny::uiOutput(ns("kpi_n_countries")),
      shiny::uiOutput(ns("kpi_year_range")),
      shiny::uiOutput(ns("kpi_global_oops")),
      shiny::uiOutput(ns("kpi_global_che_pc")),
      shiny::uiOutput(ns("kpi_global_gghed")),
      shiny::uiOutput(ns("kpi_global_ext"))
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      mod_card(
        title = "\u5168\u7403\u4e09\u6e90\u7ed3\u6784\u6f14\u5316 (2000\u2013latest)",
        htmltools::p(class = "card-note",
                     paste("\u653f\u5e9c (GGHE-D) \u00b7 \u79c1\u4eba (PVT-D) \u00b7 \u5916\u63f4 (EXT)",
                           "\u00b7 \u4ee5\u5168\u7403\u603b CHE \u4e3a\u5206\u6bcd\uff0c",
                           "\u53ef\u89c2\u5bdf\u516c\u79c1\u3001\u5916\u63f4\u4e09\u8005\u7684\u53cd\u8f6c\u4e0e\u8d8b\u52bf\u3002")),
        mod_spinner(plotly::plotlyOutput(ns("source_area"), height = 360))
      ),
      mod_card(
        title = "\u5404\u5927\u6d32 OOPS \u5206\u5e03 (\u6700\u65b0\u5e74)",
        htmltools::p(class = "card-note",
                     paste("\u7bb1\u7ebf+\u6563\u70b9\u5c55\u793a\u5404\u5927\u6d32\u5e74\u5ea6\u4e2d\u4f4d\u6570",
                           "\u3001\u56db\u5206\u4f4d\u6570\u3001\u6781\u503c\u56fd\uff0c",
                           "\u4ee5\u53ca\u9ad8 OOPS \u5c3e\u90e8\u56fd\u5bb6\u5206\u5e03\u3002")),
        mod_spinner(plotly::plotlyOutput(ns("oops_box"), height = 360))
      )
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      mod_card(
        title = "\u4eba\u5747 CHE \u9ad8\u4f4e\u56fd\u5bf9\u6bd4 (Slope chart)",
        htmltools::p(class = "card-note",
                     paste("\u7eff\u8272 = 2000 \u00b7 \u6a59\u8272 = \u6700\u65b0\u5e74\u3002",
                           "\u659c\u7387\u5c55\u793a\u4ee3\u8868\u56fd\u5bb6\u4e0a\u5347/\u4e0b\u964d\u7684\u8d8b\u52bf\uff0c",
                           "\u5feb\u901f\u8bc6\u522b\u8ffd\u8d76\u8005\u4e0e\u8131\u9698\u8005\u3002")),
        mod_spinner(plotly::plotlyOutput(ns("slope_che"), height = 360))
      ),
      mod_card(
        title = "OOPS \u9ad8\u8d1f\u62c5 Top 15 (\u6700\u65b0\u5e74)",
        htmltools::p(class = "card-note",
                     paste("\u9ad8 OOPS \u610f\u5473\u7740\u5c45\u6c11\u533b\u7597\u8d1f\u62c5\u4ee5",
                           "\u73b0\u91d1\u5f62\u5f0f\u76f4\u63a5\u627f\u62c5\uff0c",
                           "\u7ed3\u5408\u5404\u56fd GGHED \u5360\u6bd4\u53ef\u8bc4\u4f30\u8d22\u52a1\u4fdd\u62a4\u8983\u53d1\u3002")),
        mod_spinner(plotly::plotlyOutput(ns("oops_top"), height = 360))
      )
    ),
    mod_card(
      title = "\u4e16\u754c OOPS \u5730\u56fe (\u70b9\u51fb\u67e5\u770b\u8be6\u60c5)",
      htmltools::p(class = "card-note",
                   paste("\u989c\u8272\u8d8a\u6df1\u8868\u793a OOPS \u5360\u6bd4\u8d8a\u9ad8\u3002",
                         "\u5730\u56fe\u9762\u79ef\u4e0d\u4ee3\u8868\u4eba\u53e3\u89c4\u6a21\uff0c",
                         "\u8bf7\u540c\u65f6\u53c2\u8003\u5de6\u4fa7 Top 15 \u6392\u884c\u3002")),
      mod_spinner(leaflet::leafletOutput(ns("world_map"), height = 480))
    ),
    htmltools::div(
      class = "v2-section-head",
      htmltools::h3("\u9009\u62e9\u4f60\u611f\u5174\u8da3\u7684\u4e3b\u9898 \u00b7 12 \u4e2a\u4ea4\u4e92\u6a21\u5757"),
      htmltools::p(class = "v2-section-lead",
                   paste("\u70b9\u51fb\u5361\u7247\u53ef\u76f4\u63a5\u8df3\u8f6c\u3002",
                         "\u6bcf\u4e2a\u6a21\u5757\u90fd\u53ef\u72ec\u7acb\u4ea4\u4e92\u3001",
                         "\u9884\u8bbe\u53ef\u590d\u5236\u3001\u4e0e\u9759\u6001\u56fe\u3001",
                         "\u62a5\u544a\u5171\u7528\u4e00\u5957\u6307\u6807\u4f53\u7cfb\u3002"))
    ),
    htmltools::div(
      class = "module-grid",
      module_card("country", "\u56fd\u5bb6\u753b\u50cf",
                  "\u5355\u56fd CHE / OOPS / GGHED / \u9884\u671f\u5bff\u547d 24 \u5e74\u9762\u677f\u4e0e\u9009\u9879\u3002",
                  "01 Country"),
      module_card("equity", "\u516c\u5e73\u4e0e\u4e0d\u5e73\u7b49",
                  "Gini / Theil / Atkinson / Lorenz / \u5728\u7ebf\u91cd\u7b97\uff0c\u8de8\u5e74\u8de8\u6307\u6807\u3002",
                  "02 Equity"),
      module_card("efficiency", "\u6548\u7387\u4e0e\u4ea7\u51fa",
                  "DEA \u524d\u6cbf\u3001CHE\u2192HALE \u5f39\u6027\u3001\u8d56 \u88ab\u88ab\u95f4\u63a5\u6210\u672c\u3002",
                  "03 Efficiency"),
      module_card("pandemic", "\u75ab\u60c5\u51b2\u51fb",
                  "2019 vs 2020\u20132022 \u53d8\u5316\u3001COVID dumbbell\u3001\u540c\u6bd4\u70ed\u56fe\u3002",
                  "04 Pandemic"),
      module_card("outcomes", "\u8d44\u91d1 \u00b7 \u7ed3\u679c",
                  "U5MR / SDG-3 / \u9884\u671f\u5bff\u547d \u4e0e\u4eba\u5747 CHE \u8054\u52a8\u753b\u50cf\u3002",
                  "05 Outcomes"),
      module_card("compare", "\u591a\u56fd\u591a\u6307\u6807\u5bf9\u6bd4",
                  "\u4efb\u610f\u9009 N \u4e2a\u56fd\u5bb6 \u00d7 N \u4e2a\u6307\u6807\uff0c\u5728\u7ebf\u751f\u6210\u70ed\u56fe\u4e0e\u8868\u683c\u3002",
                  "06 Compare"),
      module_card("cluster", "\u805a\u7c7b\u4e0e\u8c61\u9650",
                  "PCA + KMeans \u5728\u7ebf\u8c03 K\u3001\u5c55\u793a\u8c61\u9650\u4e0e\u4ee3\u8868\u56fd\u3002",
                  "07 Cluster"),
      module_card("forecast", "\u9884\u6d4b",
                  "\u6307\u6807 ARIMA / ETS \u9884\u6d4b\uff0c\u542b\u4e0d\u786e\u5b9a\u6027\u533a\u95f4\u4e0e\u8001\u9a8c\u8bc1\u70b9\u3002",
                  "08 Forecast"),
      module_card("scenarios", "\u60c5\u666f\u4eff\u771f",
                  "OOPS / GGHED / \u5916\u63f4 \u4e09\u6e90\u53ef\u4eff\u771f\u8c03\u6574\u540e\u7684\u8d22\u52a1\u4fdd\u62a4\u672a\u6765\u3002",
                  "09 Scenario"),
      module_card("atlas", "\u5168\u7403 Atlas",
                  "Choropleth \u5730\u56fe + \u53cc\u53d8\u91cf\u4e0a\u8272 + \u63d0\u793a\u6846\u3002",
                  "10 Atlas"),
      module_card("about", "\u9879\u76ee\u8bf4\u660e",
                  "\u6570\u636e\u53d1\u5e03\u6e90 \u00b7 \u53d8\u91cf\u5b57\u5178 \u00b7 \u590d\u73b0\u547d\u4ee4 \u00b7 \u5f15\u7528\u3002",
                  "12 About")
    )
  )
}

mod_overview_server <- function(id, master_r, world_sf_obj, year_max,
                                parent_session = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    output$kpi_n_countries <- shiny::renderUI({
      m <- master_r()
      mod_kpi("\u8986\u76d6\u56fd\u5bb6 Countries",
              format(length(unique(m$iso3_code)), big.mark = ","),
              color = "primary")
    })
    output$kpi_year_range <- shiny::renderUI({
      m <- master_r()
      mod_kpi("\u5e74\u4efd\u8303\u56f4 Years",
              sprintf("%d \u2013 %d", min(m$year, na.rm = TRUE),
                                       max(m$year, na.rm = TRUE)),
              color = "success")
    })
    output$kpi_global_oops <- shiny::renderUI({
      m <- master_r()
      val <- mean(m$hf3_che[m$year == year_max & is.finite(m$hf3_che)],
                  na.rm = TRUE)
      mod_kpi(sprintf("%d \u5168\u7403 OOPS \u5747\u503c", year_max),
              fmt_pct(val, 1), color = "danger")
    })
    output$kpi_global_che_pc <- shiny::renderUI({
      m <- master_r()
      val <- stats::median(m$che_pc_usd2023[m$year == year_max &
                                              is.finite(m$che_pc_usd2023)],
                            na.rm = TRUE)
      mod_kpi(sprintf("%d \u4eba\u5747 CHE \u4e2d\u4f4d\u6570", year_max),
              fmt_usd(val), color = "warning")
    })
    output$kpi_global_gghed <- shiny::renderUI({
      m <- master_r()
      val <- mean(m$gghed_che[m$year == year_max & is.finite(m$gghed_che)],
                  na.rm = TRUE)
      mod_kpi(sprintf("%d \u653f\u5e9c GGHE-D \u5747\u503c", year_max),
              fmt_pct(val, 1), color = "primary")
    })
    output$kpi_global_ext <- shiny::renderUI({
      m <- master_r()
      val <- mean(m$ext_che[m$year == year_max & is.finite(m$ext_che)],
                  na.rm = TRUE)
      mod_kpi(sprintf("%d \u5916\u63f4 EXT \u5747\u503c", year_max),
              fmt_pct(val, 1), color = "muted")
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

    output$slope_che <- plotly::renderPlotly({
      m <- master_r()
      safe_plotly({
        p <- plot_slope_chart(m, value_col = "che_pc_usd2023",
                              year_a = min(m$year, na.rm = TRUE),
                              year_b = year_max, top_n = 18)
        plotly::ggplotly(p) |> plotly::config(displaylogo = FALSE)
      })
    })

    output$oops_top <- plotly::renderPlotly({
      m <- master_r()
      safe_plotly({
        p <- plot_oops_ranking(m, year_focus = year_max, top_n = 15)
        plotly::ggplotly(p) |> plotly::config(displaylogo = FALSE)
      })
    })

    output$world_map <- leaflet::renderLeaflet({
      m <- master_r()
      if (is.null(world_sf_obj)) {
        return(leaflet::leaflet() |>
                  leaflet::addProviderTiles("CartoDB.Positron") |>
                  leaflet::addLabelOnlyMarkers(0, 0,
                                                label = "world_sf \u4e0d\u53ef\u7528"))
      }
      tryCatch(
        leaflet_choropleth(m, world_sf_obj,
                            indicator_col = "hf3_che",
                            year_focus = year_max,
                            title = sprintf("OOPS %% \u00b7 %d", year_max)),
        error = function(e) {
          leaflet::leaflet() |>
            leaflet::addProviderTiles("CartoDB.Positron") |>
            leaflet::addLabelOnlyMarkers(0, 0,
              label = paste("Error:", conditionMessage(e)))
        }
      )
    })

    shiny::observeEvent(input$nav_to, {
      target <- input$nav_to
      if (!is.null(parent_session) && nzchar(target)) {
        bslib::nav_select(id = "main_nav", selected = target,
                           session = parent_session)
      }
    }, ignoreInit = TRUE)
  })
}
