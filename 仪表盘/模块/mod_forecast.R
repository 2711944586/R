# =============================================================================
# 仪表盘/模块/mod_forecast.R
# Tab 9 · 预测：ARIMA fan + 80%/95% PI
# =============================================================================

mod_forecast_ui <- function(id, country_choices_named, indicator_choices) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = htmltools::HTML("&#128201; 预测 Forecast"),
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 280,
        shinyWidgets::pickerInput(ns("country"), "国家",
                                   choices = country_choices_named,
                                   selected = "CHN",
                                   options  = list(`live-search` = TRUE)),
        shiny::selectInput(ns("indicator"), "指标",
                            choices = indicator_choices,
                            selected = "che_pc_usd2023"),
        shiny::sliderInput(ns("h"), "预测年数 (h)",
                            min = 1, max = 10, value = 5)
      ),
      mod_card(
        title = "ARIMA fan plot",
        mod_spinner(plotly::plotlyOutput(ns("fc_plot"), height = 480)),
        shiny::helpText("模型: forecast::auto.arima。阴影 = 80% 与 95% 预测区间。")
      )
    )
  )
}

mod_forecast_server <- function(id, master_r) {
  shiny::moduleServer(id, function(input, output, session) {
    output$fc_plot <- plotly::renderPlotly({
      shiny::req(input$country, input$indicator, input$h)
      m <- master_r()
      val <- input$indicator
      d <- m[m$iso3_code == input$country & is.finite(m[[val]]), , drop = FALSE]
      d <- d[order(d$year), , drop = FALSE]
      if (nrow(d) < 5) {
        return(plotly::plotly_empty() |>
                  plotly::layout(title = "数据不足, 无法预测"))
      }
      fc <- tryCatch(fit_forecast(d[[val]], d$year, h = input$h),
                      error = function(e) NULL)
      if (is.null(fc)) {
        return(plotly::plotly_empty() |>
                  plotly::layout(title = "ARIMA 训练失败"))
      }
      hist_df <- data.frame(year = d$year, value = d[[val]])
      fc_df   <- data.frame(year = fc$year, value = fc$point,
                             lo80 = fc$lo_80, hi80 = fc$hi_80,
                             lo95 = fc$lo_95, hi95 = fc$hi_95)
      plotly::plot_ly() |>
        plotly::add_lines(data = hist_df, x = ~year, y = ~value, name = "History",
                           line = list(color = "#1B5E88", width = 3)) |>
        plotly::add_ribbons(data = fc_df, x = ~year, ymin = ~lo95, ymax = ~hi95,
                             name = "95% PI",
                             fillcolor = "rgba(196,107,39,0.18)",
                             line = list(color = "transparent")) |>
        plotly::add_ribbons(data = fc_df, x = ~year, ymin = ~lo80, ymax = ~hi80,
                             name = "80% PI",
                             fillcolor = "rgba(196,107,39,0.32)",
                             line = list(color = "transparent")) |>
        plotly::add_lines(data = fc_df, x = ~year, y = ~value, name = "Forecast",
                           line = list(color = "#C46B27", width = 3, dash = "dash")) |>
        plotly::layout(title = sprintf("ARIMA 预测: %s · h=%d 年",
                                        input$indicator, input$h),
                       xaxis = list(title = ""),
                       yaxis = list(title = input$indicator)) |>
        plotly::config(displaylogo = FALSE)
    })
  })
}
