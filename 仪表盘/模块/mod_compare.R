
mod_compare_ui <- function(id, country_choices_named, indicator_choices) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = "\u5bf9\u6bd4 Compare",
    value = "compare",
    mod_v3_hero(
      kicker = "\u56fd\u5bb6\u5bf9\u6bd4",
      title = "\u591a\u56fd\u591a\u6307\u6807\u5bf9\u6bd4",
      lead = paste(
        "\u4ece\u4efb\u610f\u56fd\u5bb6\u7ec4\u5408\u51fa\u53d1\uff0c\u540c\u65f6\u67e5\u770b\u957f\u65f6\u5e8f\u3001",
        "\u6700\u65b0\u5e74\u5168\u7403\u6392\u540d\u3001\u8d77\u6b62\u5e74\u4efd dumbbell \u548c CAGR\u3002",
        "\u5b83\u9002\u5408\u505a\u540c\u4f34\u56fd\u5bf9\u6807\u3001\u653f\u7b56\u7a97\u53e3\u590d\u76d8\u548c\u8ffd\u8d76\u8005\u8bc6\u522b\u3002"
      ),
      meta = list("N-country selection", "Time series + ranking + CAGR", "Latest-year benchmark")
    ),
    mod_v3_page_body(
      wide = TRUE,
      mod_v3_badge_row(
        "\u591a\u56fd\u9009\u62e9",
        "\u4efb\u610f\u6307\u6807",
        "\u6392\u540d\u53ef\u641c\u7d22",
        "\u8d77\u6b62\u5e74\u4efd\u53ef\u8c03",
        tone = "secondary"
      ),
      mod_v3_story_grid(
        columns = 3,
        mod_v3_insight(
          kicker = "Trend",
          title = "\u8d8b\u52bf\u56fe\u56de\u7b54\u201c\u8c01\u5728\u6539\u53d8\u201d",
          text = "\u65f6\u5e8f\u7ebf\u628a\u56fd\u5bb6\u653e\u5230\u540c\u4e00\u6307\u6807\u5750\u6807\u4e0b\uff0c\u5bf9\u6570\u5c3a\u5ea6\u9002\u5408\u91d1\u989d\u5dee\u8ddd\u6781\u5927\u7684\u573a\u666f\u3002",
          tone = "primary"
        ),
        mod_v3_insight(
          kicker = "Rank",
          title = "\u6392\u540d\u8868\u56de\u7b54\u201c\u73b0\u5728\u5728\u54ea\u201d",
          text = "\u5168\u4e16\u754c\u6700\u65b0\u5e74\u964d\u5e8f\u6392\u540d\uff0c\u4e0d\u53ea\u770b\u9009\u4e2d\u56fd\u5bb6\uff0c\u4e5f\u80fd\u5feb\u901f\u5b9a\u4f4d\u76f8\u90bb\u5bf9\u6807\u56fd\u3002",
          tone = "secondary"
        ),
        mod_v3_insight(
          kicker = "Momentum",
          title = "Dumbbell \u548c CAGR \u56de\u7b54\u201c\u53d8\u5316\u591a\u5feb\u201d",
          text = "\u8d77\u70b9\u4e0e\u7ec8\u70b9\u540c\u65f6\u5c55\u793a\u65b9\u5411\u548c\u5e45\u5ea6\uff0cCAGR \u628a\u4e0d\u540c\u533a\u95f4\u957f\u5ea6\u6807\u51c6\u5316\u3002",
          tone = "good"
        )
      ),
      bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 300,
        shinyWidgets::pickerInput(
          ns("isos"), "\u9009\u62e9\u56fd\u5bb6\uff08\u591a\u9009\uff09",
          choices  = country_choices_named,
          selected = c("CHN", "USA", "IND", "BRA", "ZAF"),
          multiple = TRUE,
          options  = list(`actions-box` = TRUE,
                          `live-search` = TRUE,
                          `selected-text-format` = "count > 3")),
        shiny::selectInput(ns("indicator"), "\u6307\u6807",
                           choices = indicator_choices,
                           selected = "che_pc_usd2023"),
        shiny::radioButtons(ns("log"), "Y \u8f74\u5c3a\u5ea6",
                            choices = c("\u7ebf\u6027" = "linear",
                                         "\u5bf9\u6570" = "log"),
                            selected = "linear", inline = TRUE),
        shiny::sliderInput(ns("year_a"), "\u5bf9\u6bd4\u8d77\u70b9\u5e74",
                           min = 2000, max = 2020, value = 2000, step = 1,
                           sep = ""),
        shiny::sliderInput(ns("year_b"), "\u5bf9\u6bd4\u7ec8\u70b9\u5e74",
                           min = 2005, max = 2023, value = 2023, step = 1,
                           sep = ""),
        mod_v3_sidebar_note(
          "\u5bf9\u6bd4\u903b\u8f91",
          "\u5efa\u8bae\u5148\u786e\u8ba4\u6307\u6807\u5355\u4f4d\uff0c\u518d\u9009\u62e9\u7ebf\u6027\u6216\u5bf9\u6570\u5c3a\u5ea6\uff1b\u91d1\u989d\u578b\u6307\u6807\u66f4\u5bb9\u6613\u53d7\u5c3a\u5ea6\u5f71\u54cd\u3002",
          bullets = c("\u70b9\u51fb\u56fe\u4f8b\u53ef\u9690\u85cf\u56fd\u5bb6", "\u8d77\u6b62\u5e74\u9700\u4fdd\u6301\u987a\u5e8f", "CAGR \u53ea\u5bf9\u6b63\u503c\u8ba1\u7b97")
        )
      ),
      shiny::uiOutput(ns("summary_strip")),
      bslib::layout_columns(
        col_widths = c(7, 5),
        mod_card(
          kicker = "\u65f6\u95f4\u8f68\u8ff9",
          title = "\u65f6\u5e8f\u5bf9\u6bd4",
          mod_v3_chart_guide(
            "\u8bfb\u56fe\u65b9\u6cd5",
            "\u628a\u9009\u4e2d\u56fd\u5bb6\u7684\u5386\u53f2\u8def\u5f84\u653e\u5728\u540c\u4e00\u5750\u6807\u4e0a\u3002\u5bf9\u6570\u5c3a\u5ea6\u9002\u5408\u4eba\u5747 CHE \u8fd9\u7c7b\u8de8\u6570\u91cf\u7ea7\u7684\u6307\u6807\uff0c\u7ebf\u6027\u5c3a\u5ea6\u66f4\u76f4\u89c2\u3002"
          ),
          mod_spinner(plotly::plotlyOutput(ns("lines"), height = 420))
        ),
        mod_card(
          kicker = "\u6700\u65b0\u6392\u540d",
          title = "\u6700\u65b0\u5e74\u6392\u540d",
          mod_v3_chart_guide(
            "\u8868\u683c\u7528\u9014",
            "\u6392\u540d\u8868\u4fdd\u7559\u5168\u7403\u6837\u672c\uff0c\u4fbf\u4e8e\u5224\u65ad\u9009\u4e2d\u56fd\u5bb6\u7684\u76f8\u5bf9\u4f4d\u7f6e\u548c\u53ef\u6bd4\u5bf9\u8c61\u3002"
          ),
          mod_spinner(reactable::reactableOutput(ns("rank_table")))
        )
      ),
      bslib::layout_columns(
        col_widths = c(7, 5),
        mod_card(
          kicker = "\u7aef\u70b9\u53d8\u5316",
          title = "\u5e74\u521d vs \u5e74\u672b dumbbell",
          mod_v3_chart_guide(
            "\u7aef\u70b9\u5bf9\u7167",
            "\u8fde\u7ebf\u4e24\u7aef\u5206\u522b\u662f\u8d77\u70b9\u5e74\u548c\u7ec8\u70b9\u5e74\uff1b\u8ddd\u79bb\u8868\u793a\u7edd\u5bf9\u53d8\u5316\uff0c\u989c\u8272\u8868\u793a\u65b9\u5411\u3002"
          ),
          mod_spinner(plotly::plotlyOutput(ns("dumbbell"), height = 360))
        ),
        mod_card(
          kicker = "\u5e74\u5316\u901f\u5ea6",
          title = "\u5e74\u5316\u589e\u901f CAGR",
          mod_v3_chart_guide(
            "\u5e74\u5316\u53e3\u5f84",
            "CAGR = (\u672b\u503c/\u521d\u503c)^(1/n) - 1\uff0c\u7528\u4e8e\u6bd4\u8f83\u4e0d\u540c\u56fd\u5bb6\u5728\u540c\u4e00\u65f6\u6bb5\u5185\u7684\u5e73\u5747\u6bcf\u5e74\u53d8\u5316\u901f\u5ea6\u3002",
            tone = "warn"
          ),
          mod_spinner(plotly::plotlyOutput(ns("cagr"), height = 360))
        )
      ),
      mod_card(
        kicker = "\u5bf9\u6bd4\u6458\u8981",
        title = "\u9009\u4e2d\u56fd\u5bb6\u7684\u7aef\u70b9\u660e\u7ec6",
        mod_v3_chart_guide(
          "\u8868\u683c\u7528\u9014",
          "\u8868\u683c\u5c06\u8d77\u70b9\u503c\u3001\u7ec8\u70b9\u503c\u3001\u7edd\u5bf9\u53d8\u5316\u548c CAGR \u653e\u5230\u540c\u4e00\u884c\uff0c\u4fbf\u4e8e\u628a\u56fe\u4e2d\u7684\u8def\u5f84\u8f6c\u6210\u62a5\u544a\u4e2d\u7684\u6570\u5b57\u8868\u8ff0\u3002",
          tone = "good"
        ),
        mod_spinner(reactable::reactableOutput(ns("endpoint_table"))),
        footer = "\u4ec5\u5305\u542b\u8d77\u70b9\u5e74\u548c\u7ec8\u70b9\u5e74\u90fd\u6709\u6709\u6548\u503c\u7684\u9009\u4e2d\u56fd\u5bb6\u3002"
      )
      )
    )
  )
}

mod_compare_server <- function(id, master_r, year_max) {
  shiny::moduleServer(id, function(input, output, session) {
    indicator_label <- function(x) {
      switch(x,
        hf3_che = "OOPS / CHE (%)",
        gghed_che = "GGHE-D / CHE (%)",
        pvtd_che = "PVT-D / CHE (%)",
        ext_che = "EXT / CHE (%)",
        che_pc_usd2023 = "\u4eba\u5747 CHE\uff08USD 2023\uff09",
        che_usd2023 = "\u603b CHE\uff08USD 2023\uff09",
        hc6_che = "HC6 \u9884\u9632\u5360 CHE\uff08%\uff09",
        hc1_che = "HC1 \u6cbb\u7597\u5360 CHE\uff08%\uff09",
        x
      )
    }

    endpoint_data <- shiny::reactive({
      shiny::req(input$isos, input$indicator,
                 input$year_a, input$year_b,
                 input$year_a < input$year_b)
      m <- master_r()
      val <- input$indicator
      d <- m[m$iso3_code %in% input$isos &
              m$year %in% c(input$year_a, input$year_b) &
              is.finite(m[[val]]),
            c("iso3_code", "country_name", "continent", "income_group",
              "year", val), drop = FALSE]
      colnames(d)[6] <- "value"
      d_a <- d[d$year == input$year_a,
               c("iso3_code", "country_name", "continent",
                 "income_group", "value")]
      d_b <- d[d$year == input$year_b,
               c("iso3_code", "country_name", "continent",
                 "income_group", "value")]
      names(d_a)[5] <- "v_a"
      names(d_b)[5] <- "v_b"
      dd <- merge(d_a, d_b,
                  by = c("iso3_code", "country_name", "continent",
                         "income_group"))
      if (!nrow(dd)) return(dd)
      dd$delta <- dd$v_b - dd$v_a
      n <- max(input$year_b - input$year_a, 1)
      dd$cagr <- ifelse(is.finite(dd$v_a) & dd$v_a > 0 &
                          is.finite(dd$v_b) & dd$v_b > 0,
                        (dd$v_b / dd$v_a)^(1 / n) - 1,
                        NA_real_)
      dd
    })

    bounds <- shiny::reactive({
      m <- master_r()
      list(min = min(m$year, na.rm = TRUE),
           max = max(m$year, na.rm = TRUE))
    })

    shiny::observe({
      b <- bounds()
      shiny::updateSliderInput(session, "year_a",
                                min = b$min, max = b$max - 1,
                                value = b$min)
      shiny::updateSliderInput(session, "year_b",
                                min = b$min + 1, max = b$max,
                                value = b$max)
    })

    output$summary_strip <- shiny::renderUI({
      shiny::req(input$isos, input$indicator, input$year_a, input$year_b)
      mod_v3_kpi_grid(
        mod_v3_kpi("已选国家", as.character(length(input$isos)),
                   hint = "当前对比组合中的国家数量",
                   tone = "primary"),
        mod_v3_kpi("对比指标", indicator_label(input$indicator),
                   hint = "趋势图、排名表和端点变化共享同一字段",
                   tone = "secondary"),
        mod_v3_kpi("起止年份", sprintf("%d-%d", input$year_a, input$year_b),
                   hint = "Dumbbell 与 CAGR 的计算窗口",
                   tone = "good"),
        mod_v3_kpi("排名年份", as.character(year_max),
                   hint = "全局排名使用最新可用年份",
                   tone = "neutral")
      )
    })

    output$lines <- plotly::renderPlotly({
      shiny::req(input$isos, input$indicator)
      m <- master_r()
      safe_plotly({
        p <- highlight_country_ts(m, isos = input$isos,
                                   value_col = input$indicator,
                                   title = sprintf("%s \u8d70\u52bf\u5bf9\u6bd4",
                                                   input$indicator))
        if (input$log == "log") p <- p |>
          plotly::layout(yaxis = list(type = "log"))
        p
      })
    })

    output$rank_table <- reactable::renderReactable({
      shiny::req(input$indicator)
      m <- master_r()
      val <- input$indicator
      d <- m[m$year == year_max & is.finite(m[[val]]), , drop = FALSE]
      d <- d[order(-d[[val]]), , drop = FALSE]
      d <- data.frame(
        "\u6392\u540d"        = seq_len(nrow(d)),
        "\u56fd\u5bb6"     = d$country_name,
        "\u5927\u6d32"   = d$continent,
        "\u6536\u5165\u7ec4" = d$income_group,
        "\u6307\u6807\u503c"       = round(d[[val]], 2),
        check.names = FALSE
      )
      reactable::reactable(d, searchable = TRUE, defaultPageSize = 15,
        pagination = TRUE, highlight = TRUE,
        defaultColDef = reactable::colDef(
          headerStyle = list(background = "#f1f3f7")))
    })

    output$dumbbell <- plotly::renderPlotly({
      dd <- endpoint_data()
      shiny::req(nrow(dd) > 0)
      dd$dir <- ifelse(dd$delta >= 0, "rise", "fall")
      dd <- dd[order(dd$v_b), ]
      dd$country_name <- factor(dd$country_name, levels = dd$country_name)
      pal <- c(rise = "#6B8E5A", fall = "#C46B27")
      p <- ggplot2::ggplot(dd) +
        ggplot2::geom_segment(ggplot2::aes(x = v_a, xend = v_b,
                                            y = country_name,
                                            yend = country_name,
                                            color = dir),
                               linewidth = 1.6) +
        ggplot2::geom_point(ggplot2::aes(x = v_a, y = country_name),
                             color = "#5A5A65", size = 3) +
        ggplot2::geom_point(ggplot2::aes(x = v_b, y = country_name,
                                          color = dir), size = 3.4) +
        ggplot2::scale_color_manual(values = pal, guide = "none") +
        ggplot2::labs(x = sprintf("%d \u2192 %d\uff08%s\uff09",
                                   input$year_a, input$year_b,
                                   indicator_label(input$indicator)),
                      y = NULL) +
        ggplot2::theme_minimal(base_size = 12)
      plotly::ggplotly(p) |> plotly::config(displaylogo = FALSE)
    })

    output$cagr <- plotly::renderPlotly({
      dd <- endpoint_data()
      dd <- dd[is.finite(dd$cagr), , drop = FALSE]
      shiny::req(nrow(dd) > 0)
      dd <- dd[order(dd$cagr), ]
      dd$country_name <- factor(dd$country_name, levels = dd$country_name)
      p <- ggplot2::ggplot(dd, ggplot2::aes(cagr, country_name,
                                              fill = cagr >= 0)) +
        ggplot2::geom_col() +
        ggplot2::geom_vline(xintercept = 0, linewidth = 0.5,
                             color = "#1A1A1F33") +
        ggplot2::scale_fill_manual(
          values = c("TRUE" = "#1B5E88", "FALSE" = "#C46B27"),
          guide = "none") +
        ggplot2::scale_x_continuous(
          labels = scales::percent_format(accuracy = 0.1)) +
        ggplot2::labs(x = sprintf("%d\u2013%d \u5e74\u5316\u589e\u901f",
                                   input$year_a, input$year_b),
                      y = NULL) +
        ggplot2::theme_minimal(base_size = 12)
      plotly::ggplotly(p) |> plotly::config(displaylogo = FALSE)
    })

    output$endpoint_table <- reactable::renderReactable({
      dd <- endpoint_data()
      shiny::req(nrow(dd) > 0)
      dd <- dd[order(-abs(dd$delta)), , drop = FALSE]
      tab <- data.frame(
        "ISO3" = dd$iso3_code,
        "\u56fd\u5bb6" = dd$country_name,
        "\u5927\u6d32" = dd$continent,
        "\u6536\u5165\u7ec4" = dd$income_group,
        "\u8d77\u70b9\u503c" = round(dd$v_a, 2),
        "\u7ec8\u70b9\u503c" = round(dd$v_b, 2),
        "\u7edd\u5bf9\u53d8\u5316" = round(dd$delta, 2),
        "CAGR" = ifelse(is.finite(dd$cagr),
                         paste0(round(dd$cagr * 100, 2), "%"),
                         "\u2014"),
        check.names = FALSE
      )
      names(tab)[5:6] <- c(
        sprintf("%d \u503c", input$year_a),
        sprintf("%d \u503c", input$year_b)
      )
      reactable::reactable(tab, searchable = TRUE, defaultPageSize = 12,
        pagination = TRUE, highlight = TRUE,
        defaultColDef = reactable::colDef(headerStyle = list(background = "#f1f3f7")),
        columns = list(
          "ISO3" = reactable::colDef(width = 70,
            style = list(fontFamily = "'JetBrains Mono', monospace",
                         fontWeight = 700)),
          "\u7edd\u5bf9\u53d8\u5316" = reactable::colDef(align = "right",
            style = function(value) {
              col <- if (!is.na(value) && value >= 0) "#2a857a" else "#a23b3b"
              list(color = col, fontWeight = 700)
            })
        ))
    })
  })
}
