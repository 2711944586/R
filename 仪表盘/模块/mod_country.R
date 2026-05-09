# =============================================================================
# 仪表盘/模块/mod_country.R
# Tab 2 · 国家画像：国家选择 + 4 KPI + 4 panel plotly
# =============================================================================

mod_country_ui <- function(id, country_choices_named, year_min, year_max) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = htmltools::HTML("&#128205; \u56fd\u5bb6 Country"),
    htmltools::div(
      class = "panel-hero",
      htmltools::h2("\u56fd\u5bb6\u753b\u50cf"),
      htmltools::p(class = "text-muted",
                   paste("\u9009\u62e9\u4e00\u4e2a\u56fd\u5bb6\u67e5\u770b\u5176 24 \u5e74\u603b\u989d / \u4eba\u5747 / \u4e09\u6e90\u7ed3\u6784 / \u7b79\u8d44\u65b9\u6848 / \u9884\u9632 vs \u6cbb\u7597\u3002",
                         "\u56db\u4e2a Tab \u4e3b\u9898\u9762\u677f\u53ef\u5212\u8fc7\u6bd4\u8f83\u3002",
                         "\u4e0a\u65b9 4 \u4e2a KPI \u662f\u9009\u4e2d\u533a\u95f4\u672b\u5e74\u4ee3\u8868\u503c\u3002"))
    ),
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 280,
        shinyWidgets::pickerInput(
          ns("country"), "选择国家 Country",
          choices = country_choices_named, selected = "CHN",
          options = list(`live-search` = TRUE)),
        shiny::sliderInput(
          ns("year_range"), "年份区间",
          min = year_min, max = year_max,
          value = c(year_min, year_max), step = 1, sep = ""),
        shiny::helpText("拖动滑块以聚焦不同时段。")
      ),
      bslib::layout_columns(
        col_widths = c(3, 3, 3, 3),
        shiny::uiOutput(ns("kpi_che")),
        shiny::uiOutput(ns("kpi_oops")),
        shiny::uiOutput(ns("kpi_gghed")),
        shiny::uiOutput(ns("kpi_ext"))
      ),
      mod_card(
        bslib::navset_card_tab(
          bslib::nav_panel("总量与人均",
            mod_spinner(plotly::plotlyOutput(ns("total_che"), height = 320)),
            mod_spinner(plotly::plotlyOutput(ns("pc_che"), height = 320))),
          bslib::nav_panel("来源结构",
            mod_spinner(plotly::plotlyOutput(ns("sources"), height = 360))),
          bslib::nav_panel("筹资方案",
            mod_spinner(plotly::plotlyOutput(ns("schemes"), height = 360))),
          bslib::nav_panel("预防 vs 治疗",
            mod_spinner(plotly::plotlyOutput(ns("purpose"), height = 360)))
        )
      )
    )
  )
}

mod_country_server <- function(id, master_r) {
  shiny::moduleServer(id, function(input, output, session) {
    cp_data <- shiny::reactive({
      shiny::req(input$country)
      m <- master_r()
      m[m$iso3_code == input$country &
        m$year >= input$year_range[1] &
        m$year <= input$year_range[2], , drop = FALSE]
    })

    output$kpi_che <- shiny::renderUI({
      d <- cp_data()
      shiny::req(nrow(d) > 0)
      latest <- d[d$year == max(d$year, na.rm = TRUE), , drop = FALSE]
      mod_kpi(sprintf("CHE (%d)", latest$year[1]),
              fmt_usd(latest$che_usd2023[1]), color = "primary")
    })
    output$kpi_oops <- shiny::renderUI({
      d <- cp_data()
      shiny::req(nrow(d) > 0)
      latest <- d[d$year == max(d$year, na.rm = TRUE), , drop = FALSE]
      mod_kpi("OOPS %", fmt_pct(latest$hf3_che[1]), color = "danger")
    })
    output$kpi_gghed <- shiny::renderUI({
      d <- cp_data()
      shiny::req(nrow(d) > 0)
      latest <- d[d$year == max(d$year, na.rm = TRUE), , drop = FALSE]
      mod_kpi("GGHE-D %", fmt_pct(latest$gghed_che[1]), color = "success")
    })
    output$kpi_ext <- shiny::renderUI({
      d <- cp_data()
      shiny::req(nrow(d) > 0)
      latest <- d[d$year == max(d$year, na.rm = TRUE), , drop = FALSE]
      mod_kpi("EXT %", fmt_pct(latest$ext_che[1]), color = "warning")
    })

    output$total_che <- plotly::renderPlotly({
      d <- cp_data(); shiny::req(nrow(d) > 0)
      plotly::plot_ly(d, x = ~year, y = ~che_usd2023 / 1e9,
                       type = "scatter", mode = "lines+markers",
                       line = list(color = "#1B5E88", width = 3),
                       marker = list(color = "#1B5E88", size = 6),
                       hovertemplate = "%{x}: $%{y:.2f}B<extra></extra>") |>
        plotly::layout(title = "总卫生支出 CHE (十亿 USD 2023)",
                       xaxis = list(title = ""), yaxis = list(title = "USD (B)")) |>
        plotly::config(displaylogo = FALSE)
    })

    output$pc_che <- plotly::renderPlotly({
      d <- cp_data(); shiny::req(nrow(d) > 0)
      plotly::plot_ly(d, x = ~year, y = ~che_pc_usd2023,
                       type = "scatter", mode = "lines+markers",
                       line = list(color = "#C46B27", width = 3),
                       marker = list(color = "#C46B27", size = 6),
                       hovertemplate = "%{x}: $%{y:,.0f}<extra></extra>") |>
        plotly::layout(title = "人均 CHE (USD 2023)",
                       xaxis = list(title = ""),
                       yaxis = list(title = "USD per capita")) |>
        plotly::config(displaylogo = FALSE)
    })

    output$sources <- plotly::renderPlotly({
      d <- cp_data(); shiny::req(nrow(d) > 0)
      d2 <- d[, c("year", "gghed_che", "pvtd_che", "ext_che")]
      d2 <- tidyr::pivot_longer(d2, c("gghed_che", "pvtd_che", "ext_che"),
                                 names_to = "src", values_to = "pct")
      d2$src <- factor(d2$src,
                       levels = c("gghed_che", "pvtd_che", "ext_che"),
                       labels = c("政府 GGHE-D", "私人 PVT-D", "外援 EXT"))
      p <- ggplot2::ggplot(d2, ggplot2::aes(year, pct, fill = src)) +
        ggplot2::geom_area(alpha = 0.85) +
        ggplot2::scale_fill_manual(values = c("政府 GGHE-D" = "#1B5E88",
                                               "私人 PVT-D" = "#C46B27",
                                               "外援 EXT"  = "#6B8E5A"),
                                    name = NULL) +
        ggplot2::scale_y_continuous(labels = scales::label_percent(scale = 1)) +
        ggplot2::labs(title = "来源结构", x = NULL, y = NULL) +
        ggplot2::theme_minimal(base_size = 11)
      plotly::ggplotly(p) |> plotly::config(displaylogo = FALSE)
    })

    output$schemes <- plotly::renderPlotly({
      d <- cp_data(); shiny::req(nrow(d) > 0)
      cols <- intersect(c("hf1_che","hf2_che","hf3_che","hf4_che","hfnec_che"),
                        names(d))
      d2 <- d[, c("year", cols)]
      d2 <- tidyr::pivot_longer(d2, dplyr::all_of(cols),
                                 names_to = "sch", values_to = "pct")
      p <- ggplot2::ggplot(d2, ggplot2::aes(year, pct, fill = sch)) +
        ggplot2::geom_area(alpha = 0.85) +
        ggplot2::scale_y_continuous(labels = scales::label_percent(scale = 1)) +
        ggplot2::labs(title = "筹资方案 HF", x = NULL, y = NULL) +
        ggplot2::theme_minimal(base_size = 11)
      plotly::ggplotly(p) |> plotly::config(displaylogo = FALSE)
    })

    output$purpose <- plotly::renderPlotly({
      d <- cp_data(); shiny::req(nrow(d) > 0)
      if (!all(c("hc1_che", "hc6_che") %in% names(d))) {
        return(plotly::plotly_empty() |>
                  plotly::layout(title = "缺少 hc 系列字段"))
      }
      d2 <- d[, c("year", "hc1_che", "hc6_che")]
      d2 <- tidyr::drop_na(d2)
      d2 <- tidyr::pivot_longer(d2, c("hc1_che", "hc6_che"),
                                 names_to = "purpose", values_to = "pct")
      d2$purpose <- factor(d2$purpose,
                           levels = c("hc1_che", "hc6_che"),
                           labels = c("hc1 治疗", "hc6 预防"))
      if (nrow(d2) < 2) {
        return(plotly::plotly_empty() |>
                  plotly::layout(title = "数据不足 (hc 系列仅 2016+)"))
      }
      plotly::plot_ly(d2, x = ~year, y = ~pct, color = ~purpose,
                       type = "scatter", mode = "lines+markers",
                       hovertemplate = "%{x}: %{y:.1f}%<extra></extra>") |>
        plotly::layout(title = "预防 hc6 vs 治疗 hc1",
                       yaxis = list(title = "% of CHE"),
                       xaxis = list(title = "")) |>
        plotly::config(displaylogo = FALSE)
    })
  })
}
