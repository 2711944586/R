
mod_scenarios_ui <- function(id, country_choices_named) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = "\u60c5\u666f Scenarios",
    value = "scenarios",
    mod_v3_hero(
      kicker = "\u60c5\u666f\u6a21\u62df",
      title = "\u60c5\u666f\u63a8\u6f14\uff1a\u6982\u7387\u6247\u5f62\u3001\u7b79\u8d44\u8c03\u6574\u4e0e\u8d22\u52a1\u4fdd\u62a4",
      lead = paste(
        "\u672c\u9875\u5c06\u4e24\u7c7b\u201c\u672a\u6765\u201d\u653e\u5728\u4e00\u8d77\uff1a",
        "\u4e00\u7c7b\u662f\u57fa\u4e8e\u5386\u53f2\u6b8b\u5dee\u91cd\u91c7\u6837\u7684\u6982\u7387\u6247\u5f62\uff0c\u53e6\u4e00\u7c7b\u662f\u5bf9 GGHED\u3001OOPS \u548c\u4eba\u5747 CHE \u8fdb\u884c\u900f\u660e what-if \u8c03\u6574\u3002",
        "\u5b83\u4e0d\u4ee3\u66ff\u56e0\u679c\u8bc6\u522b\uff0c\u4f46\u53ef\u5e2e\u52a9\u8bf4\u6e05\u76ee\u6807\u5dee\u8ddd\u3001\u98ce\u9669\u533a\u95f4\u548c\u653f\u7b56\u53c2\u6570\u654f\u611f\u6027\u3002"
      ),
      meta = list("\u8499\u7279\u5361\u6d1b\u6247\u5f62", "\u653f\u7b56\u53c2\u6570\u8bd5\u7b97", "\u6b8b\u5dee\u4e0d\u786e\u5b9a\u6027", "\u8d22\u52a1\u4fdd\u62a4")
    ),
    mod_v3_page_body(
      wide = TRUE,
      mod_v3_badge_row(
        "\u5386\u53f2\u6b8b\u5dee\u91cd\u91c7\u6837",
        "\u6982\u7387\u5206\u4f4d\u6570",
        "\u7b79\u8d44\u63d0\u5347",
        "\u81ea\u4ed8\u51cf\u8d1f",
        "\u5bff\u547d\u63cf\u8ff0\u6027\u659c\u7387",
        tone = "warn"
      ),
      mod_v3_story_grid(
        columns = 3,
        mod_v3_insight(
          kicker = "Fan",
          title = "\u6982\u7387\u6247\u5f62\u663e\u793a\u57fa\u7ebf\u98ce\u9669",
          text = "\u8499\u7279\u5361\u6d1b\u6a21\u62df\u5728 ARIMA \u6b8b\u5dee\u5206\u5e03\u4e0a\u91cd\u91c7\u6837\uff0c\u7ed9\u51fa\u4e0d\u540c\u5206\u4f4d\u7684\u672a\u6765\u8def\u5f84\u3002",
          tone = "primary"
        ),
        mod_v3_insight(
          kicker = "Policy",
          title = "\u653f\u7b56\u8c03\u6574\u662f\u900f\u660e\u53c2\u6570\u8bd5\u7b97",
          text = "\u7528\u6237\u76f4\u63a5\u8bbe\u5b9a\u653f\u5e9c\u7b79\u8d44\u589e\u52a0\u3001\u5c45\u6c11\u81ea\u4ed8\u4e0b\u964d\u548c\u4eba\u5747 CHE \u989d\u5916\u589e\u957f\u3002",
          tone = "secondary"
        ),
        mod_v3_insight(
          kicker = "Boundary",
          title = "\u8f93\u51fa\u662f\u8fd1\u4f3c\u4f20\u9012\uff0c\u4e0d\u662f\u56e0\u679c\u7ed3\u8bba",
          text = "\u5bff\u547d\u53d8\u5316\u6765\u81ea\u6700\u8fd1\u5e74\u622a\u9762\u63cf\u8ff0\u6027\u659c\u7387\uff0c\u4e0d\u5305\u542b\u884c\u4e3a\u53cd\u5e94\u3001\u75be\u75c5\u51b2\u51fb\u6216\u5236\u5ea6\u6267\u884c\u5dee\u5f02\u3002",
          tone = "bad"
        )
      ),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 304,
          shinyWidgets::pickerInput(ns("country"), "\u56fd\u5bb6",
            choices = country_choices_named, selected = "CHN",
            options = list(`live-search` = TRUE)),
          shiny::selectInput(ns("indicator"), "\u6982\u7387\u6247\u5f62\u6307\u6807",
            choices = c("\u4eba\u5747 CHE (USD2023)" = "che_pc_usd2023",
                         "OOPS %" = "hf3_che",
                         "GGHE-D %" = "gghed_che"),
            selected = "che_pc_usd2023"),
          shiny::sliderInput(ns("h"), "\u9884\u6d4b\u5e74\u6570", min = 3, max = 15, value = 10),
          shiny::sliderInput(ns("n_sim"), "\u8499\u7279\u5361\u6d1b\u6a21\u62df\u6b21\u6570",
            min = 100, max = 2000, value = 500, step = 100),
          shiny::numericInput(ns("seed"), "\u968f\u673a\u79cd\u5b50", value = 42),
          mod_v3_sidebar_note(
            "\u6982\u7387\u6247\u5f62",
            "\u6a21\u62df\u4f7f\u7528\u5386\u53f2\u6b8b\u5dee\u6ce2\u52a8\u6765\u6784\u9020\u672a\u6765\u8def\u5f84\uff0c\u9002\u5408\u8868\u8fbe\u4e0d\u786e\u5b9a\u6027\u800c\u4e0d\u662f\u653f\u7b56\u6548\u679c\u3002",
            bullets = c("\u6a21\u62df\u6b21\u6570\u8d8a\u9ad8\uff0c\u5206\u4f4d\u6570\u66f4\u7a33\u5b9a", "\u5e74\u6570\u8d8a\u957f\uff0c\u6247\u5f62\u901a\u5e38\u8d8a\u5bbd", "\u968f\u673a\u79cd\u5b50\u4fdd\u8bc1\u53ef\u590d\u73b0")
          ),
          shiny::tags$hr(),
          htmltools::tags$div(
            style = "font-size:11px;font-weight:850;letter-spacing:.10em;text-transform:uppercase;color:#1d3f5f;margin:18px 0 8px;",
            "\u653f\u7b56\u6a21\u62df\u5668"
          ),
          shiny::sliderInput(ns("policy_years"), "\u653f\u7b56\u5151\u73b0\u671f\uff08\u5e74\uff09",
            min = 1, max = 10, value = 5),
          shiny::sliderInput(ns("gghed_boost"), "GGHED \u63d0\u5347\uff08\u767e\u5206\u70b9\uff09",
            min = 0, max = 30, value = 8, step = 1),
          shiny::sliderInput(ns("oop_cut"), "OOPS \u4e0b\u964d\uff08\u767e\u5206\u70b9\uff09",
            min = 0, max = 30, value = 8, step = 1),
          shiny::sliderInput(ns("che_growth"), "\u4eba\u5747 CHE \u989d\u5916\u589e\u957f\uff08%\uff09",
            min = 0, max = 80, value = 15, step = 5),
          mod_v3_sidebar_note(
            "\u653f\u7b56\u8fb9\u754c",
            "\u653f\u7b56\u6a21\u62df\u662f\u7b80\u5316\u5bf9\u7167\uff1a\u8ba1\u7b97\u57fa\u7ebf\u503c\u3001\u8c03\u6574\u540e\u503c\u548c\u5dee\u989d\uff0c\u5e76\u5c06 CHE \u589e\u957f\u901a\u8fc7\u622a\u9762\u659c\u7387\u6295\u5c04\u5230\u5bff\u547d\u3002",
            bullets = c(
              "\u4e0d\u542b\u9884\u7b97\u7ea6\u675f\u548c\u6267\u884c\u65f6\u6ede\u3002",
              "\u4e0d\u5904\u7406\u60a3\u8005\u884c\u4e3a\u6216\u670d\u52a1\u4f9b\u7ed9\u53cd\u5e94\u3002",
              "\u7528\u4e8e\u53c2\u6570\u654f\u611f\u6027\u5c55\u793a\u800c\u975e\u56e0\u679c\u4f30\u8ba1\u3002"
            )
          )
        ),
        shiny::uiOutput(ns("kpi_strip")),
        mod_v3_rail(list(
          list(title = "\u9009\u57fa\u7ebf", text = "\u786e\u5b9a\u56fd\u5bb6\u3001\u6307\u6807\u548c\u9884\u6d4b\u5e74\u6570\u3002"),
          list(title = "\u8bfb\u6982\u7387", text = "\u7528 5/25/50/75/95% \u5206\u4f4d\u89c2\u5bdf\u4e0a\u4e0b\u884c\u98ce\u9669\u3002"),
          list(title = "\u8c03\u53c2\u6570", text = "\u6539\u53d8 GGHED\u3001OOPS \u548c\u4eba\u5747 CHE \u7684\u5047\u60f3\u7ec4\u5408\u3002"),
          list(title = "\u6bd4\u5dee\u989d", text = "\u7528\u56fe\u8868\u548c\u8868\u683c\u540c\u65f6\u5ba1\u89c6\u53d8\u5316\u65b9\u5411\u4e0e\u5e45\u5ea6\u3002")
        )),
        mod_card(
          kicker = "\u6982\u7387\u6247\u5f62",
          title = "\u8499\u7279\u5361\u6d1b\u6982\u7387\u6247 + ARIMA \u4e2d\u4f4d\u6570",
          mod_v3_chart_guide(
            "\u8bfb\u56fe\u65b9\u6cd5",
            "\u5386\u53f2\u7ebf\u4e4b\u540e\u7684\u6247\u5f62\u4ee3\u8868\u6a21\u62df\u8def\u5f84\u5206\u4f4d\uff1b\u4e2d\u4f4d\u7ebf\u4e0d\u662f\u552f\u4e00\u7ed3\u8bba\uff0c\u5e94\u4e0e\u5c3e\u90e8\u5206\u4f4d\u4e00\u8d77\u89e3\u8bfb\u3002",
            bullets = c("5-95% \u533a\u95f4\u8868\u793a\u5bbd\u98ce\u9669\u5e26", "25-75% \u533a\u95f4\u8868\u793a\u4e2d\u5fc3\u60c5\u666f", "\u6247\u5f62\u6269\u5f20\u5feb\u8bf4\u660e\u672a\u6765\u8def\u5f84\u66f4\u4e0d\u7a33\u5b9a")
          ),
          mod_spinner(plotly::plotlyOutput(ns("mc_fan"), height = 540))
        ),
        mod_card(
          kicker = "\u5206\u4f4d\u8868",
          title = "\u60c5\u666f\u6bd4\u8f83\uff1a5% / 25% / 50% / 75% / 95% \u5206\u4f4d",
          mod_v3_chart_guide(
            "\u8868\u683c\u7528\u9014",
            "\u5206\u4f4d\u6570\u8868\u53ef\u7528\u4e8e\u62a5\u544a\u4e2d\u7684\u6570\u503c\u5f15\u7528\uff0c\u4e5f\u4fbf\u4e8e\u5c06\u672a\u6765\u5e74\u4efd\u7684\u4e0a\u4e0b\u884c\u98ce\u9669\u62c6\u5f00\u6bd4\u8f83\u3002"
          ),
          mod_spinner(reactable::reactableOutput(ns("scenario_table")))
        ),
        shiny::uiOutput(ns("policy_delta_strip")),
        bslib::layout_columns(
          col_widths = c(6, 6),
          mod_card(
            kicker = "\u653f\u7b56\u8bd5\u7b97",
            title = "\u653f\u7b56\u6a21\u62df\u5668\uff1a\u653f\u5e9c\u7b79\u8d44\u4e0e\u81ea\u4ed8\u4e0b\u964d",
            mod_v3_chart_guide(
              "\u5bf9\u7167\u903b\u8f91",
              "\u56fe\u4e2d\u5c06\u57fa\u7ebf\u548c\u653f\u7b56\u60c5\u666f\u5e76\u6392\u663e\u793a\uff0c\u7528\u4e8e\u76f4\u89c2\u5224\u65ad\u653f\u7b56\u53c2\u6570\u5bf9\u6838\u5fc3\u6307\u6807\u7684\u5373\u65f6\u63a8\u6f14\u5f71\u54cd\u3002",
              tone = "good"
            ),
            mod_spinner(plotly::plotlyOutput(ns("policy_plot"), height = 430))
          ),
          mod_card(
            kicker = "\u653f\u7b56\u8868",
            title = "\u653f\u7b56\u60c5\u666f\u6307\u6807\u5bf9\u7167",
            mod_v3_chart_guide(
              "\u8bfb\u8868\u65b9\u6cd5",
              "\u53d8\u5316\u5217\u8868\u793a\u653f\u7b56\u60c5\u666f\u4e0e\u57fa\u7ebf\u7684\u5dee\u989d\uff1b\u5bff\u547d\u53d8\u5316\u6765\u81ea\u6700\u8fd1\u5e74\u622a\u9762 life_exp ~ log(CHE_pc) \u7684\u63cf\u8ff0\u6027\u659c\u7387\u3002",
              tone = "warn"
            ),
            mod_spinner(reactable::reactableOutput(ns("policy_table"))),
            footer = "\u653f\u7b56\u60c5\u666f\u4e0d\u5305\u542b\u884c\u4e3a\u53cd\u5e94\u3001\u6267\u884c\u65f6\u6ede\u6216\u533b\u7597\u4f9b\u7ed9\u7ea6\u675f\uff0c\u9002\u5408\u4f5c\u4e3a\u53c2\u6570\u654f\u611f\u6027\u5c55\u793a\u3002"
          )
        )
      )
    )
  )
}

mod_scenarios_server <- function(id, master_r) {
  shiny::moduleServer(id, function(input, output, session) {
    indicator_label <- function(x) {
      switch(x,
        che_pc_usd2023 = "\u4eba\u5747 CHE\uff08USD 2023\uff09",
        hf3_che = "OOPS / CHE\uff08%\uff09",
        gghed_che = "GGHE-D / CHE\uff08%\uff09",
        x
      )
    }

    output$kpi_strip <- shiny::renderUI({
      shiny::req(input$country, input$indicator, input$h, input$n_sim)
      mod_v3_kpi_grid(
        mod_v3_kpi("\u5f53\u524d\u56fd\u5bb6", input$country,
                   hint = "\u6982\u7387\u6247\u548c\u653f\u7b56\u6a21\u62df\u5171\u7528\u7684\u56fd\u5bb6",
                   tone = "primary"),
        mod_v3_kpi("\u6a21\u62df\u6b21\u6570", fmt_v3_num(input$n_sim),
                   hint = "\u8499\u7279\u5361\u6d1b\u8def\u5f84\u6570",
                   tone = "secondary"),
        mod_v3_kpi("\u9884\u6d4b\u671f", sprintf("%d \u5e74", input$h),
                   hint = "\u6982\u7387\u6247\u5411\u524d\u5ef6\u4f38\u7684\u65f6\u957f",
                   tone = "good"),
        mod_v3_kpi("\u81ea\u4ed8\u4e0b\u964d", sprintf("%d pp", input$oop_cut %||% 0),
                   hint = "\u653f\u7b56\u6a21\u62df\u4e2d OOPS \u964d\u5e45",
                   tone = "bad")
      )
    })

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
                           name = "\u5386\u53f2\u89c2\u6d4b",
                           line = list(color = "#1B5E88", width = 3)) |>
        plotly::add_ribbons(data = qdf, x = ~year, ymin = ~q05, ymax = ~q95,
                             name = "5-95% \u5206\u4f4d\u5e26",
                             fillcolor = "rgba(196,107,39,0.15)",
                             line = list(color = "transparent")) |>
        plotly::add_ribbons(data = qdf, x = ~year, ymin = ~q25, ymax = ~q75,
                             name = "25-75% \u5206\u4f4d\u5e26",
                             fillcolor = "rgba(196,107,39,0.30)",
                             line = list(color = "transparent")) |>
        plotly::add_lines(data = qdf, x = ~year, y = ~q50, name = "\u4e2d\u4f4d\u8def\u5f84",
                           line = list(color = "#C46B27", width = 3,
                                        dash = "dash")) |>
        ghs_plotly_layout() |>
        plotly::layout(title = sprintf("\u8499\u7279\u5361\u6d1b\u6247\u5f62 \u00b7 %s \u00b7 %s \u00b7 h=%d \u00b7 %d \u6b21",
                                        input$country, indicator_label(input$indicator),
                                        input$h, input$n_sim),
                       xaxis = list(title = ""),
                       yaxis = list(title = indicator_label(input$indicator)),
                       legend = list(orientation = "h", y = -0.15)) |>
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

    output$policy_delta_strip <- shiny::renderUI({
      d <- policy_obj()
      shiny::req(nrow(d) > 0)
      change <- stats::setNames(d$scenario - d$baseline, d$metric)
      mod_v3_kpi_grid(
        mod_v3_kpi(
          "\u516c\u5171\u7b79\u8d44\u589e\u52a0",
          paste0(fmt_v3_num(change[["GGHED 占 CHE (%)"]], 1), " pp"),
          hint = "\u653f\u5e9c\u7b79\u8d44\u5360 CHE \u7684\u8bd5\u7b97\u63d0\u5347",
          tone = "primary"
        ),
        mod_v3_kpi(
          "\u5c45\u6c11\u81ea\u4ed8\u51cf\u8d1f",
          paste0(fmt_v3_num(abs(change[["OOPS 占 CHE (%)"]]), 1), " pp"),
          hint = "OOPS \u5360 CHE \u7684\u8bd5\u7b97\u4e0b\u964d",
          tone = "good"
        ),
        mod_v3_kpi(
          "\u4eba\u5747 CHE \u589e\u91cf",
          fmt_v3_usd(change[["人均 CHE (USD2023)"]], 0),
          hint = "\u6309\u5f53\u524d CHE \u589e\u957f\u53c2\u6570\u6298\u7b97",
          tone = "secondary"
        ),
        mod_v3_kpi(
          "\u5bff\u547d\u63cf\u8ff0\u6027\u53d8\u5316",
          paste0(fmt_v3_num(change[["预期寿命（年）"]], 2), " \u5e74"),
          hint = "\u7531\u6700\u8fd1\u5e74\u622a\u9762\u659c\u7387\u8fd1\u4f3c\u6295\u5c04",
          tone = "warn"
        )
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
        scenario = rep(c("\u57fa\u7ebf", "\u653f\u7b56\u60c5\u666f"), each = nrow(show)),
        stringsAsFactors = FALSE
      )
      plotly::plot_ly(long, x = ~metric, y = ~value, color = ~scenario,
                      type = "bar",
                      colors = c("\u57fa\u7ebf" = "#5A5A65",
                                 "\u653f\u7b56\u60c5\u666f" = "#C46B27"),
                      text = ~round(value, 2), textposition = "auto") |>
        ghs_plotly_layout() |>
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
