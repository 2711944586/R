# =============================================================================
# 仪表盘/模块/mod_scenarios.R
# Tab 10 · 情景 ★：MC 1000 次情景 + ARIMA fan + 概率云
# =============================================================================

mod_scenarios_ui <- function(id, country_choices_named) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = htmltools::HTML("&#127919; 情景 Scenarios"),
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 280,
        shinyWidgets::pickerInput(ns("country"), "国家",
          choices = country_choices_named, selected = "CHN",
          options = list(`live-search` = TRUE)),
        shiny::selectInput(ns("indicator"), "指标",
          choices = c("人均 CHE (USD2023)" = "che_pc_usd2023",
                       "OOPS %" = "hf3_che",
                       "GGHE-D %" = "gghed_che"),
          selected = "che_pc_usd2023"),
        shiny::sliderInput(ns("h"), "预测年数", min = 3, max = 15, value = 10),
        shiny::sliderInput(ns("n_sim"), "MC 模拟次数",
          min = 100, max = 2000, value = 500, step = 100),
        shiny::numericInput(ns("seed"), "随机种子", value = 42),
        shiny::tags$hr(),
        shiny::helpText(
          "蒙特卡洛在 ", htmltools::strong("ARIMA 残差分布"),
          " 上重采样，得到", htmltools::strong("非参数概率扇"), "。")
      ),
      mod_card(
        title = "MC 概率扇 + ARIMA 中位数",
        mod_spinner(plotly::plotlyOutput(ns("mc_fan"), height = 540))
      ),
      mod_card(
        title = "情景比较：5%/25%/50%/75%/95% 分位",
        mod_spinner(reactable::reactableOutput(ns("scenario_table")))
      )
    )
  )
}

mod_scenarios_server <- function(id, master_r) {
  shiny::moduleServer(id, function(input, output, session) {
    sim_obj <- shiny::reactive({
      shiny::req(input$country, input$indicator, input$h)
      m <- master_r()
      d <- m[m$iso3_code == input$country &
              is.finite(m[[input$indicator]]), , drop = FALSE]
      d <- d[order(d$year), , drop = FALSE]
      shiny::req(nrow(d) >= 8)
      set.seed(input$seed %||% 42)
      fc <- tryCatch(fit_forecast(d[[input$indicator]], d$year, h = input$h),
                      error = function(e) NULL)
      shiny::req(!is.null(fc))

      sd_resid <- if (!is.null(fc$residual_sd) && is.finite(fc$residual_sd))
                     fc$residual_sd
                   else stats::sd(diff(d[[input$indicator]]), na.rm = TRUE)
      if (!is.finite(sd_resid) || sd_resid == 0) sd_resid <- 1

      sims <- matrix(NA_real_, nrow = input$n_sim, ncol = input$h)
      for (i in seq_len(input$n_sim)) {
        cum_shock <- 0
        for (j in seq_len(input$h)) {
          cum_shock <- cum_shock + stats::rnorm(1, 0, sd_resid)
          sims[i, j] <- fc$point[j] + cum_shock
        }
      }
      list(history = data.frame(year = d$year, value = d[[input$indicator]]),
            forecast = data.frame(year = fc$year, point = fc$point,
                                  lo80 = fc$lo_80, hi80 = fc$hi_80,
                                  lo95 = fc$lo_95, hi95 = fc$hi_95),
            sims = sims, sim_years = fc$year)
    })

    output$mc_fan <- plotly::renderPlotly({
      s <- sim_obj(); shiny::req(s)
      qtl <- t(apply(s$sims, 2, stats::quantile,
                      probs = c(0.05, 0.25, 0.5, 0.75, 0.95), na.rm = TRUE))
      qdf <- data.frame(year = s$sim_years,
                         q05 = qtl[, 1], q25 = qtl[, 2], q50 = qtl[, 3],
                         q75 = qtl[, 4], q95 = qtl[, 5])
      plotly::plot_ly() |>
        plotly::add_lines(data = s$history, x = ~year, y = ~value,
                           name = "History",
                           line = list(color = "#1B5E88", width = 3)) |>
        plotly::add_ribbons(data = qdf, x = ~year, ymin = ~q05, ymax = ~q95,
                             name = "5–95%",
                             fillcolor = "rgba(196,107,39,0.15)",
                             line = list(color = "transparent")) |>
        plotly::add_ribbons(data = qdf, x = ~year, ymin = ~q25, ymax = ~q75,
                             name = "25–75%",
                             fillcolor = "rgba(196,107,39,0.30)",
                             line = list(color = "transparent")) |>
        plotly::add_lines(data = qdf, x = ~year, y = ~q50, name = "Median",
                           line = list(color = "#C46B27", width = 3,
                                        dash = "dash")) |>
        plotly::layout(title = sprintf("MC fan · %s · %s · h=%d · sims=%d",
                                        input$country, input$indicator,
                                        input$h, input$n_sim),
                       xaxis = list(title = ""),
                       yaxis = list(title = input$indicator)) |>
        plotly::config(displaylogo = FALSE)
    })

    output$scenario_table <- reactable::renderReactable({
      s <- sim_obj(); shiny::req(s)
      qtl <- t(apply(s$sims, 2, stats::quantile,
                      probs = c(0.05, 0.25, 0.5, 0.75, 0.95), na.rm = TRUE))
      tab <- data.frame(
        年份 = s$sim_years,
        `5%`  = round(qtl[, 1], 2),
        `25%` = round(qtl[, 2], 2),
        `50%` = round(qtl[, 3], 2),
        `75%` = round(qtl[, 4], 2),
        `95%` = round(qtl[, 5], 2),
        check.names = FALSE
      )
      reactable::reactable(tab, defaultPageSize = 12,
        pagination = FALSE, highlight = TRUE,
        defaultColDef = reactable::colDef(headerStyle = list(background = "#f1f3f7")))
    })
  })
}

`%||%` <- function(a, b) if (is.null(a)) b else a
