# =============================================================================
# 仪表盘/模块/mod_scenarios.R
# Tab 10 · 情景 ★：MC 1000 次情景 + ARIMA fan + 概率云
# =============================================================================

mod_scenarios_ui <- function(id, country_choices_named) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = htmltools::HTML("&#127919; \u60c5\u666f Scenarios"),
    value = "scenarios",
    htmltools::div(
      class = "panel-hero",
      htmltools::h2("\u4e09\u6e90\u8c03\u6574 \u00b7 \u8d22\u52a1\u4fdd\u62a4\u4eff\u771f"),
      htmltools::p(class = "text-muted",
                   paste("\u8c03\u8282 OOPS / GGHE-D / EXT \u7684\u5047\u60f3\u53d8\u5316\uff08\u767e\u5206\u70b9\uff09\uff0c",
                         "\u89c2\u5bdf\u672a\u6765\u65b0\u503c\u3001\u91cd\u65b0\u5206\u7ec4\u4e0e\u6781\u503c\u56fd\u6570\u91cf\u7684\u53d8\u52a8\u3002",
                         "\u4eff\u771f\u662f\u201c\u4f20\u9012\u63a8\u65ad\u201d\uff0c\u4e0d\u5305\u542b\u884c\u4e3a / \u7740\u529b / \u5468\u671f\u6548\u5e94\u3002"))
    ),
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
          " 上重采样，得到", htmltools::strong("非参数概率扇"), "。"),
        shiny::tags$hr(),
        shiny::h5("Policy Simulator"),
        shiny::sliderInput(ns("policy_years"), "政策兑现期（年）",
          min = 1, max = 10, value = 5),
        shiny::sliderInput(ns("gghed_boost"), "GGHED 提升（百分点）",
          min = 0, max = 30, value = 8, step = 1),
        shiny::sliderInput(ns("oop_cut"), "OOPS 下降（百分点）",
          min = 0, max = 30, value = 8, step = 1),
        shiny::sliderInput(ns("che_growth"), "人均 CHE 额外增长（%）",
          min = 0, max = 80, value = 15, step = 5),
        shiny::helpText("政策模拟是透明的 what-if 计算，不是因果估计。")
      ),
      mod_card(
        title = "MC 概率扇 + ARIMA 中位数",
        mod_spinner(plotly::plotlyOutput(ns("mc_fan"), height = 540))
      ),
      mod_card(
        title = "情景比较：5%/25%/50%/75%/95% 分位",
        mod_spinner(reactable::reactableOutput(ns("scenario_table")))
      ),
      bslib::layout_columns(
        col_widths = c(6, 6),
        mod_card(
          title = "Policy Simulator：政府筹资与自付下降",
          mod_spinner(plotly::plotlyOutput(ns("policy_plot"), height = 430))
        ),
        mod_card(
          title = "政策情景指标对照",
          mod_spinner(reactable::reactableOutput(ns("policy_table"))),
          shiny::helpText("寿命变化来自最近年份截面 life_exp ~ log(CHE_pc) 的描述性斜率。")
        )
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

    policy_obj <- shiny::reactive({
      shiny::req(input$country, input$policy_years)
      m <- master_r()
      needed <- c("iso3_code", "country_name", "year", "che_pc_usd2023",
                  "gghed_che", "hf3_che", "life_exp")
      shiny::validate(shiny::need(all(needed %in% names(m)),
        "当前数据缺少政策模拟所需字段。"))
      yr <- max(m$year, na.rm = TRUE)
      base <- m[m$iso3_code == input$country & m$year == yr, needed, drop = FALSE]
      if (!nrow(base)) {
        base <- m[m$iso3_code == input$country, needed, drop = FALSE]
        base <- base[order(base$year, decreasing = TRUE), , drop = FALSE]
        base <- utils::head(base, 1)
      }
      shiny::validate(shiny::need(nrow(base) == 1, "未找到所选国家的最近年份观测。"))
      shiny::validate(shiny::need(is.finite(base$che_pc_usd2023) && base$che_pc_usd2023 > 0,
        "所选国家缺少可用人均 CHE。"))
      cross <- m[m$year == base$year &
                   is.finite(m$che_pc_usd2023) &
                   is.finite(m$life_exp) &
                   m$che_pc_usd2023 > 0, , drop = FALSE]
      elastic <- tryCatch({
        fit <- stats::lm(life_exp ~ log(che_pc_usd2023), data = cross)
        as.numeric(stats::coef(fit)[["log(che_pc_usd2023)"]])
      }, error = function(e) NA_real_)
      if (!is.finite(elastic)) elastic <- 2
      base_che <- as.numeric(base$che_pc_usd2023)
      scenario_che <- base_che * (1 + input$che_growth / 100)
      base_life <- as.numeric(base$life_exp)
      life_gain <- elastic * log(scenario_che / base_che)
      scenario_life <- if (is.finite(base_life)) base_life + life_gain else NA_real_
      base_gghed <- as.numeric(base$gghed_che)
      base_oop <- as.numeric(base$hf3_che)
      scenario_gghed <- pmin(100, base_gghed + input$gghed_boost)
      scenario_oop <- pmax(0, base_oop - input$oop_cut)
      public_add_pc <- base_che * input$gghed_boost / 100
      household_relief_pc <- base_che * input$oop_cut / 100
      data.frame(
        country_name = base$country_name,
        iso3_code = base$iso3_code,
        year = base$year,
        metric = c("GGHED 占 CHE (%)", "OOPS 占 CHE (%)",
                   "人均 CHE (USD2023)", "预期寿命（年）",
                   "公共筹资增加（USD/人）", "居民自付减负（USD/人）"),
        baseline = c(base_gghed, base_oop, base_che, base_life, 0, 0),
        scenario = c(scenario_gghed, scenario_oop, scenario_che,
                     scenario_life, public_add_pc, household_relief_pc),
        stringsAsFactors = FALSE
      )
    })

    output$policy_plot <- plotly::renderPlotly({
      d <- policy_obj()
      shiny::req(nrow(d) > 0)
      show <- d[d$metric %in% c("GGHED 占 CHE (%)", "OOPS 占 CHE (%)",
                                "人均 CHE (USD2023)", "预期寿命（年）"), ]
      long <- data.frame(
        metric = rep(show$metric, 2),
        value = c(show$baseline, show$scenario),
        scenario = rep(c("baseline", "policy"), each = nrow(show)),
        stringsAsFactors = FALSE
      )
      plotly::plot_ly(long, x = ~metric, y = ~value, color = ~scenario,
                      type = "bar",
                      colors = c(baseline = "#5A5A65", policy = "#C46B27"),
                      text = ~round(value, 2), textposition = "auto") |>
        plotly::layout(
          title = sprintf("%s · %s 年政策兑现期",
                          unique(d$country_name), input$policy_years),
          barmode = "group",
          xaxis = list(title = ""),
          yaxis = list(title = ""),
          legend = list(orientation = "h", x = 0, y = -0.18),
          margin = list(b = 95)
        ) |>
        plotly::config(displaylogo = FALSE)
    })

    output$policy_table <- reactable::renderReactable({
      d <- policy_obj()
      d$change <- d$scenario - d$baseline
      tab <- data.frame(
        指标 = d$metric,
        基线 = round(d$baseline, 2),
        政策情景 = round(d$scenario, 2),
        变化 = round(d$change, 2),
        check.names = FALSE
      )
      reactable::reactable(tab, pagination = FALSE, highlight = TRUE,
        defaultColDef = reactable::colDef(headerStyle = list(background = "#f1f3f7")),
        columns = list(
          变化 = reactable::colDef(style = function(value) {
            if (is.na(value)) return(NULL)
            color <- if (value >= 0) "#1B5E88" else "#C46B27"
            list(color = color, fontWeight = "700")
          })
        ))
    })
  })
}

`%||%` <- function(a, b) if (is.null(a)) b else a
