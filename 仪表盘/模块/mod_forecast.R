# =============================================================================
# 仪表盘/模块/mod_forecast.R
# Tab 9 · 预测：ARIMA fan + 80%/95% PI
# =============================================================================

mod_forecast_ui <- function(id, country_choices_named, indicator_choices) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = htmltools::HTML("&#128201; \u9884\u6d4b Forecast"),
    value = "forecast",
    mod_v3_hero(
      kicker = "FORECAST LAB",
      title = "\u5355\u56fd\u5355\u6307\u6807\u9884\u6d4b\uff1aARIMA / ETS \u70b9\u9884\u6d4b\u4e0e\u533a\u95f4",
      lead = paste(
        "\u9009\u62e9\u4efb\u610f\u56fd\u5bb6\u548c\u6307\u6807\uff0c\u7528\u5386\u53f2\u5e74\u5ea6\u5e8f\u5217\u81ea\u52a8\u62df\u5408\u9884\u6d4b\u6a21\u578b\uff0c",
        "\u540c\u65f6\u8f93\u51fa\u70b9\u9884\u6d4b\u300180% \u548c 95% \u9884\u6d4b\u533a\u95f4\u3002",
        "\u8fd9\u4e00\u9875\u5b9a\u4f4d\u4e8e\u57fa\u7ebf\u5916\u63a8\uff0c\u60c5\u666f\u5e72\u9884\u548c\u653f\u7b56\u53c2\u6570\u6539\u53d8\u8bf7\u5728 Scenarios \u9875\u8fdb\u884c\u3002"
      ),
      meta = list("auto.arima / ETS backend", "80% & 95% prediction intervals", "Single-country history")
    ),
    mod_v3_page_body(
      wide = TRUE,
      mod_v3_badge_row(
        "\u5355\u56fd\u65f6\u5e8f",
        "\u81ea\u52a8\u9009\u9636",
        "\u9884\u6d4b\u533a\u95f4",
        "\u8d8b\u52bf\u5916\u63a8",
        tone = "secondary"
      ),
      mod_v3_story_grid(
        columns = 3,
        mod_v3_insight(
          kicker = "Baseline",
          title = "\u9884\u6d4b\u662f\u201c\u5982\u679c\u8def\u5f84\u5ef6\u7eed\u201d",
          text = "\u6a21\u578b\u53ea\u4f7f\u7528\u5df2\u9009\u56fd\u5bb6\u7684\u5386\u53f2\u6307\u6807\u5e8f\u5217\uff0c\u9002\u5408\u751f\u6210\u57fa\u7ebf\u8def\u5f84\u800c\u4e0d\u662f\u653f\u7b56\u5b9e\u9a8c\u3002",
          tone = "primary"
        ),
        mod_v3_insight(
          kicker = "Uncertainty",
          title = "\u533a\u95f4\u5bbd\u5ea6\u662f\u98ce\u9669\u4fe1\u53f7",
          text = "95% \u533a\u95f4\u8d8a\u5bbd\uff0c\u8bf4\u660e\u5386\u53f2\u6ce2\u52a8\u8d8a\u5927\u6216\u6837\u672c\u8d8a\u8584\uff0c\u4e0d\u5b9c\u53ea\u8bfb\u4e2d\u4f4d\u7ebf\u3002",
          tone = "warn"
        ),
        mod_v3_insight(
          kicker = "Use case",
          title = "\u53ef\u7528\u4e8e\u627e\u9700\u8981\u60c5\u666f\u5206\u6790\u7684\u6307\u6807",
          text = "\u5f53\u9884\u6d4b\u57fa\u7ebf\u4e0e\u653f\u7b56\u76ee\u6807\u660e\u663e\u8131\u8282\u65f6\uff0c\u518d\u8fdb\u5165\u60c5\u666f\u9875\u8c03\u6574\u7b79\u8d44\u6216\u81ea\u4ed8\u53c2\u6570\u3002",
          tone = "good"
        )
      ),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 292,
          shinyWidgets::pickerInput(ns("country"), "\u56fd\u5bb6",
                                     choices = country_choices_named,
                                     selected = "CHN",
                                     options  = list(`live-search` = TRUE)),
          shiny::selectInput(ns("indicator"), "\u6307\u6807",
                              choices = indicator_choices,
                              selected = "che_pc_usd2023"),
          shiny::sliderInput(ns("h"), "\u9884\u6d4b\u5e74\u6570 (h)",
                              min = 1, max = 10, value = 5),
          mod_v3_sidebar_note(
            "Model scope",
            "\u672c\u9875\u7684\u9884\u6d4b\u662f\u5355\u53d8\u91cf\u65f6\u5e8f\u57fa\u7ebf\uff0c\u4e0d\u52a0\u5165\u653f\u7b56\u51b2\u51fb\u3001\u4eba\u53e3\u7ed3\u6784\u6216\u5b8f\u89c2\u5916\u751f\u53d8\u91cf\u3002",
            bullets = c("\u5386\u53f2\u89c2\u6d4b\u8fc7\u5c11\u65f6\u4e0d\u4f1a\u8bad\u7ec3", "\u70b9\u9884\u6d4b\u5e94\u548c\u533a\u95f4\u540c\u65f6\u89e3\u8bfb", "\u5bf9\u7a81\u53d1\u51b2\u51fb\u7684\u5904\u7406\u8bf7\u7528\u60c5\u666f\u9875")
          )
        ),
        shiny::uiOutput(ns("kpi_strip")),
        mod_v3_rail(list(
          list(title = "\u9009\u5e8f\u5217", text = "\u56fd\u5bb6\u548c\u6307\u6807\u5171\u540c\u51b3\u5b9a\u8bad\u7ec3\u6837\u672c\u3002"),
          list(title = "\u5b9a\u89c6\u7a97", text = "\u9884\u6d4b\u5e74\u6570\u8d8a\u957f\uff0c\u4e0d\u786e\u5b9a\u6027\u901a\u5e38\u8d8a\u9ad8\u3002"),
          list(title = "\u770b\u533a\u95f4", text = "80% \u662f\u4e2d\u5fc3\u98ce\u9669\u5e26\uff0c95% \u8868\u8fbe\u66f4\u5bbd\u7684\u5916\u5ef6\u98ce\u9669\u3002"),
          list(title = "\u505a\u5bf9\u7167", text = "\u5c06\u57fa\u7ebf\u7ed3\u679c\u548c\u653f\u7b56\u76ee\u6807\u6216\u60c5\u666f\u6a21\u62df\u5bf9\u7167\u3002")
        )),
        mod_card(
          kicker = "FORECAST FAN",
          title = "ARIMA fan plot",
          mod_v3_chart_guide(
            "\u8bfb\u56fe\u65b9\u6cd5",
            "\u84dd\u8272\u7ebf\u662f\u5386\u53f2\u89c2\u6d4b\uff0c\u6a59\u8272\u865a\u7ebf\u662f\u70b9\u9884\u6d4b\uff1b\u6df1\u6a59\u8272\u548c\u6d45\u6a59\u8272\u9634\u5f71\u5206\u522b\u8868\u793a 80% \u4e0e 95% \u9884\u6d4b\u533a\u95f4\u3002",
            bullets = c("\u533a\u95f4\u4e0d\u662f\u653f\u7b56\u76ee\u6807", "\u5f02\u5e38\u5e74\u53ef\u80fd\u62c9\u5bbd\u4e0d\u786e\u5b9a\u6027", "\u5e94\u548c\u60c5\u666f\u6a21\u62df\u7ed3\u679c\u4e92\u76f8\u6821\u9a8c")
          ),
          mod_spinner(plotly::plotlyOutput(ns("fc_plot"), height = 520)),
          footer = "\u6a21\u578b\u7531 fit_forecast() \u5c01\u88c5\uff1b\u5f53\u53ef\u7528\u5e8f\u5217\u5c11\u4e8e 5 \u5e74\u65f6\u8fd4\u56de\u6570\u636e\u4e0d\u8db3\u3002"
        )
      )
    )
  )
}

mod_forecast_server <- function(id, master_r) {
  shiny::moduleServer(id, function(input, output, session) {
    output$kpi_strip <- shiny::renderUI({
      shiny::req(input$country, input$indicator, input$h)
      m <- master_r()
      val <- input$indicator
      d <- m[m$iso3_code == input$country & is.finite(m[[val]]), , drop = FALSE]
      d <- d[order(d$year), , drop = FALSE]
      last_value <- if (nrow(d)) d[[val]][nrow(d)] else NA_real_
      year_span <- if (nrow(d)) {
        sprintf("%d-%d", min(d$year, na.rm = TRUE), max(d$year, na.rm = TRUE))
      } else "\u2014"
      mod_v3_kpi_grid(
        mod_v3_kpi("\u5386\u53f2\u89c2\u6d4b", fmt_v3_num(nrow(d)),
                   hint = "\u53ef\u7528\u5e74\u5ea6\u70b9\u6570",
                   tone = "primary"),
        mod_v3_kpi("\u5e74\u4efd\u8303\u56f4", year_span,
                   hint = "\u5f53\u524d\u5e8f\u5217\u7684\u8bad\u7ec3\u7a97\u53e3",
                   tone = "neutral"),
        mod_v3_kpi("\u6700\u65b0\u89c2\u6d4b", fmt_v3_num(last_value, 2),
                   hint = "\u9884\u6d4b\u8d77\u70b9\u524d\u7684\u6700\u540e\u5b9e\u9645\u503c",
                   tone = "secondary"),
        mod_v3_kpi("\u9884\u6d4b\u671f", sprintf("%d \u5e74", input$h),
                   hint = "\u5411\u524d\u5916\u63a8\u7684\u5e74\u6570",
                   tone = "good")
      )
    })

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
