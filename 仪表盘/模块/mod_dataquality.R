# =============================================================================
# 仪表盘/模块/mod_dataquality.R
# 数据质量浏览：缺失模式、覆盖率、修订记录
# =============================================================================

mod_dataquality_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = htmltools::HTML("&#128202; \u6570\u636e\u8d28\u91cf Data Quality"),
    value = "dataquality",
    mod_v3_hero(
      kicker = "DATA QUALITY",
      title = "\u6570\u636e\u8d28\u91cf\u4e0e\u5b8c\u6574\u6027",
      lead = paste(
        "\u901a\u8fc7\u56fd\u5bb6-\u5e74\u4efd\u7c92\u5ea6\u68c0\u67e5\u6838\u5fc3\u5b57\u6bb5\u7684\u8986\u76d6\u7a33\u5b9a\u6027\u3001\u7f3a\u5931\u805a\u96c6\u533a\u548c\u5e74\u5ea6\u65ad\u70b9\u3002",
        "\u70ed\u56fe\u3001\u8d8b\u52bf\u7ebf\u548c\u4e24\u5f20\u8d28\u91cf\u8868\u5171\u540c\u56de\u7b54\uff1a\u54ea\u4e9b\u6307\u6807\u53ef\u4ee5\u652f\u6491\u7a33\u5b9a\u6bd4\u8f83\uff0c\u54ea\u4e9b\u53ea\u9002\u5408\u8f85\u52a9\u89e3\u91ca\u3002"
      ),
      meta = list("\u5b57\u6bb5\u8986\u76d6\u7387", "\u56fd\u5bb6-\u5e74\u4efd\u70ed\u56fe", "\u7f3a\u5931\u8bca\u65ad")
    ),
    mod_v3_page_body(
      wide = TRUE,
      mod_v3_badge_row(
        "\u6307\u6807\u5207\u6362",
        "\u5e74\u4efd\u7a97\u53e3",
        "\u8986\u76d6\u7387\u6392\u540d",
        "\u56fd\u5bb6\u7ea7\u7f3a\u5931",
        tone = "primary"
      ),
      mod_v3_story_grid(
        columns = 3,
        mod_v3_insight(
          kicker = "Coverage",
          title = "\u8986\u76d6\u7387\u5148\u4e8e\u7ed3\u8bba",
          text = "\u5f53\u67d0\u4e2a\u6307\u6807\u5728\u7279\u5b9a\u5e74\u4efd\u6216\u5730\u533a\u6301\u7eed\u7f3a\u5931\uff0c\u8be5\u6a21\u5757\u4f1a\u628a\u5b83\u663e\u6027\u66b4\u9732\uff0c\u907f\u514d\u56fe\u8868\u88ab\u6837\u672c\u504f\u79fb\u8bef\u5bfc\u3002",
          tone = "primary",
          icon = "C"
        ),
        mod_v3_insight(
          kicker = "Pattern",
          title = "\u770b\u7f3a\u5931\u662f\u968f\u673a\u8fd8\u662f\u6210\u7247",
          text = "\u70ed\u56fe\u628a\u56fd\u5bb6\u4e0e\u5e74\u4efd\u5c55\u5f00\uff0c\u53ef\u533a\u5206\u5355\u5e74\u7f3a\u53e3\u3001\u8fde\u7eed\u65ad\u6863\u548c\u957f\u671f\u65e0\u8bb0\u5f55\u56fd\u5bb6\u3002",
          tone = "secondary",
          icon = "M"
        ),
        mod_v3_insight(
          kicker = "Audit",
          title = "\u4e0e\u65b9\u6cd5\u9875\u4e92\u76f8\u6821\u9a8c",
          text = "\u5b57\u6bb5\u8986\u76d6\u8868\u548c\u56fd\u5bb6\u8986\u76d6\u8868\u53ef\u76f4\u63a5\u4f5c\u4e3a\u65b9\u6cd5\u9644\u5f55\u7684\u8d28\u91cf\u8bf4\u660e\u3002",
          tone = "good",
          icon = "A"
        )
      ),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 300,
          shiny::selectInput(ns("indicator"), "\u9009\u62e9\u6307\u6807",
            choices = c(
              "\u4eba\u5747 CHE" = "che_pc_usd2023",
              "OOPS %" = "hf3_che",
              "GGHE-D %" = "gghed_che",
              "\u9884\u671f\u5bff\u547d" = "life_exp",
              "U5MR" = "u5mr",
              "\u4eba\u5747 GDP" = "gdp_pc_usd",
              "\u4eba\u53e3" = "pop"
            ), selected = "che_pc_usd2023"),
          shiny::sliderInput(ns("year_range"), "\u5e74\u4efd\u8303\u56f4",
            min = 2000, max = 2023, value = c(2000, 2023), step = 1, sep = ""),
          mod_v3_sidebar_note(
            title = "\u56fe\u4f8b",
            text = "\u70ed\u56fe\u4e2d\u6df1\u7ea2\u8868\u793a\u7f3a\u5931\uff0c\u7eff\u8272\u8868\u793a\u6709\u6548\u8bb0\u5f55\u3002",
            bullets = c(
              "\u6bcf\u4e2a\u5355\u5143\u683c\u4ee3\u8868\u4e00\u4e2a\u56fd\u5bb6-\u5e74\u4efd\u3002",
              "\u53f3\u4fa7\u8d8b\u52bf\u56fe\u6309\u5e74\u6c47\u603b\u6709\u6570\u636e\u56fd\u5bb6\u5360\u6bd4\u3002",
              "\u8868\u683c\u53ef\u7528\u4e8e\u5b9a\u4f4d\u8986\u76d6\u8584\u5f31\u7684\u6307\u6807\u548c\u56fd\u5bb6\u3002"
            )
          )
        ),
        shiny::uiOutput(ns("kpi_strip")),
        mod_v3_rail(list(
          list(title = "\u9009\u6307\u6807", text = "\u5728 CHE\u3001OOPS\u3001GGHE-D\u3001\u5065\u5eb7\u4ea7\u51fa\u548c\u5b8f\u89c2\u53d8\u91cf\u95f4\u5207\u6362\u3002"),
          list(title = "\u7f29\u5e74\u4efd", text = "\u8c03\u6574\u5e74\u4efd\u7a97\u53e3\uff0c\u68c0\u67e5\u65e9\u671f\u6570\u636e\u4e0e\u8fd1\u5e74\u6570\u636e\u662f\u5426\u4e00\u81f4\u3002"),
          list(title = "\u8bfb\u70ed\u56fe", text = "\u627e\u51fa\u7f3a\u5931\u662f\u56fd\u5bb6\u805a\u96c6\u3001\u5e74\u4efd\u805a\u96c6\uff0c\u8fd8\u662f\u96f6\u6563\u6ce2\u52a8\u3002"),
          list(title = "\u5b9a\u7ed3\u8bba", text = "\u628a\u8986\u76d6\u8584\u5f31\u7684\u6307\u6807\u964d\u4e3a\u8f85\u52a9\u8bc1\u636e\uff0c\u4f18\u5148\u89e3\u91ca\u9ad8\u8986\u76d6\u6307\u6807\u3002")
        )),
        bslib::layout_columns(
          col_widths = c(7, 5),
          mod_card(
            kicker = "MISSINGNESS MAP",
            title = "\u7f3a\u5931\u6a21\u5f0f\u70ed\u529b\u56fe",
            mod_v3_chart_guide(
              title = "\u70ed\u56fe\u89e3\u8bfb",
              text = "\u7ad6\u5411\u770b\u5355\u56fd\u8fde\u7eed\u6027\uff0c\u6a2a\u5411\u770b\u67d0\u4e00\u5e74\u662f\u5426\u51fa\u73b0\u666e\u904d\u65ad\u70b9\u3002",
              bullets = c(
                "\u957f\u6761\u7ea2\u8272\uff1a\u67d0\u56fd\u6301\u7eed\u7f3a\u5931\u3002",
                "\u7ad6\u5411\u7ea2\u8272\u5e26\uff1a\u67d0\u5e74\u6570\u636e\u53ef\u80fd\u7cfb\u7edf\u6027\u4e0d\u5b8c\u6574\u3002",
                "\u96f6\u6563\u7ea2\u70b9\uff1a\u66f4\u53ef\u80fd\u662f\u5c40\u90e8\u8bb0\u5f55\u95ee\u9898\u3002"
              )
            ),
            mod_spinner(plotly::plotlyOutput(ns("missing_heatmap"), height = 500)),
            footer = "\u70ed\u56fe\u4ec5\u5c55\u793a\u8986\u76d6\u8f83\u9ad8\u7684\u524d 40 \u4e2a ISO3\uff0c\u7528\u4e8e\u4fdd\u6301\u53ef\u8bfb\u6027\u3002"
          ),
          mod_card(
            kicker = "COVERAGE TREND",
            title = "\u8986\u76d6\u7387\u968f\u5e74\u53d8\u5316",
            mod_v3_chart_guide(
              title = "\u8d8b\u52bf\u89e3\u8bfb",
              text = "\u6298\u7ebf\u8868\u793a\u5f53\u5e74\u6709\u6548\u8bb0\u5f55\u5728\u56fd\u5bb6-\u5e74\u4efd\u89c2\u6d4b\u4e2d\u7684\u5360\u6bd4\u3002",
              tone = "good"
            ),
            mod_spinner(plotly::plotlyOutput(ns("coverage_trend"), height = 500)),
            footer = "\u8d8b\u52bf\u7a81\u7136\u4e0b\u964d\u901a\u5e38\u9700\u8981\u56de\u5230\u6570\u636e\u6e90\u6216\u6e05\u6d17\u811a\u672c\u6838\u5bf9\u3002"
          )
        ),
        bslib::layout_columns(
          col_widths = c(6, 6),
          mod_card(
            kicker = "FIELD RANKING",
            title = "\u5404\u6307\u6807\u8986\u76d6\u6982\u89c8",
            mod_v3_chart_guide(
              title = "\u5b57\u6bb5\u5c42\u9762",
              text = "\u6309\u6709\u6548\u8bb0\u5f55\u6570\u964d\u5e8f\u6392\u5217\uff0c\u5feb\u901f\u8bc6\u522b\u53ef\u7528\u6027\u6700\u597d\u548c\u6700\u9700\u8c28\u614e\u7684\u6307\u6807\u3002"
            ),
            mod_spinner(reactable::reactableOutput(ns("var_coverage_table")))
          ),
          mod_card(
            kicker = "COUNTRY RANKING",
            title = "\u6309\u56fd\u5bb6\u67e5\u770b\u8986\u76d6\u7387",
            mod_v3_chart_guide(
              title = "\u56fd\u5bb6\u5c42\u9762",
              text = "\u6309\u8986\u76d6\u7387\u4ece\u4f4e\u5230\u9ad8\u6392\u5217\uff0c\u4fbf\u4e8e\u5148\u5b9a\u4f4d\u6837\u672c\u8d28\u91cf\u98ce\u9669\u3002",
              tone = "warn"
            ),
            mod_spinner(reactable::reactableOutput(ns("country_coverage_table")))
          )
        )
      )
    )
  )
}

mod_dataquality_server <- function(id, master_r) {
  shiny::moduleServer(id, function(input, output, session) {

    filtered <- shiny::reactive({
      m <- master_r()
      m[m$year >= input$year_range[1] & m$year <= input$year_range[2], , drop = FALSE]
    })

    output$kpi_strip <- shiny::renderUI({
      m <- filtered()
      ind <- input$indicator
      total <- nrow(m)
      has_data <- sum(is.finite(m[[ind]]))
      coverage <- if (total > 0) has_data / total * 100 else 0
      mod_v3_kpi_grid(
        mod_v3_kpi(format(total, big.mark = ","), "\u603b\u89c2\u6d4b", tone = "primary"),
        mod_v3_kpi(format(has_data, big.mark = ","), "\u6709\u6570\u636e", tone = "good"),
        mod_v3_kpi(sprintf("%.1f%%", coverage), "\u8986\u76d6\u7387", tone = "secondary"),
        mod_v3_kpi(format(total - has_data, big.mark = ","), "\u7f3a\u5931", tone = "bad")
      )
    })

    output$missing_heatmap <- plotly::renderPlotly({
      m <- filtered()
      ind <- input$indicator
      # Take a sample of countries (top 50 by coverage)
      coverage_by_iso <- tapply(m[[ind]], m$iso3_code, function(x) sum(is.finite(x)))
      top_isos <- names(sort(coverage_by_iso, decreasing = TRUE))[1:40]
      d <- m[m$iso3_code %in% top_isos, c("iso3_code", "year", ind)]
      d$has <- as.integer(is.finite(d[[ind]]))
      safe_plotly({
        plotly::plot_ly(d, x = ~year, y = ~iso3_code, z = ~has,
                        type = "heatmap",
                        colorscale = list(c(0, "#a23b3b"), c(1, "#2a857a")),
                        showscale = FALSE) |>
          ghs_plotly_layout() |>
          plotly::layout(xaxis = list(title = ""), yaxis = list(title = ""))
      })
    })

    output$coverage_trend <- plotly::renderPlotly({
      m <- filtered()
      ind <- input$indicator
      agg <- do.call(rbind, lapply(split(m, m$year), function(ch) {
        data.frame(year = ch$year[1],
                   n_total = nrow(ch),
                   n_has = sum(is.finite(ch[[ind]])))
      }))
      agg$pct <- agg$n_has / agg$n_total * 100
      safe_plotly({
        plotly::plot_ly(agg, x = ~year, y = ~pct,
                        type = "scatter", mode = "lines+markers",
                        line = list(color = "#1d3f5f", width = 3),
                        marker = list(color = "#1d3f5f", size = 6)) |>
          ghs_plotly_layout() |>
          plotly::layout(xaxis = list(title = ""),
                         yaxis = list(title = "\u8986\u76d6\u7387 (%)", range = c(0, 100)))
      })
    })

    output$var_coverage_table <- reactable::renderReactable({
      m <- filtered()
      vars <- c("che_pc_usd2023", "che_usd2023", "gghed_che", "pvtd_che",
                "ext_che", "hf3_che", "life_exp", "u5mr", "gdp_pc_usd", "pop")
      vars <- vars[vars %in% names(m)]
      total <- nrow(m)
      df <- data.frame(
        variable = vars,
        valid_records = sapply(vars, function(v) sum(is.finite(m[[v]]))),
        coverage_pct = sapply(vars, function(v) round(sum(is.finite(m[[v]])) / total * 100, 1))
      )
      df <- df[order(-df$valid_records), ]
      names(df) <- c("\u6307\u6807", "\u6709\u6548\u8bb0\u5f55", "\u8986\u76d6\u7387(%)")
      reactable::reactable(df, defaultPageSize = 10, highlight = TRUE,
        defaultColDef = reactable::colDef(headerStyle = list(background = "#f1f3f7")),
        columns = list(
          "\u6307\u6807" = reactable::colDef(minWidth = 150,
            style = list(fontFamily = "'JetBrains Mono', monospace", fontWeight = 600)),
          "\u6709\u6548\u8bb0\u5f55" = reactable::colDef(align = "right",
            format = reactable::colFormat(separators = TRUE)),
          "\u8986\u76d6\u7387(%)" = reactable::colDef(align = "right")
        ))
    })

    output$country_coverage_table <- reactable::renderReactable({
      m <- filtered()
      ind <- input$indicator
      agg <- do.call(rbind, lapply(split(m, m$iso3_code), function(ch) {
        data.frame(iso = ch$iso3_code[1],
                   country = ch$country_name[1],
                   n_years = sum(is.finite(ch[[ind]])),
                   total_years = nrow(ch))
      }))
      agg$pct <- round(agg$n_years / agg$total_years * 100, 1)
      agg <- agg[order(agg$pct), ]
      names(agg) <- c("ISO", "\u56fd\u5bb6", "\u6709\u6570\u636e\u5e74\u4efd",
                       "\u603b\u5e74\u4efd", "\u8986\u76d6\u7387(%)")
      reactable::reactable(agg, defaultPageSize = 12, searchable = TRUE,
        defaultColDef = reactable::colDef(headerStyle = list(background = "#f1f3f7")),
        columns = list(
          "\u56fd\u5bb6" = reactable::colDef(minWidth = 170),
          "\u6709\u6570\u636e\u5e74\u4efd" = reactable::colDef(align = "right"),
          "\u603b\u5e74\u4efd" = reactable::colDef(align = "right"),
          "\u8986\u76d6\u7387(%)" = reactable::colDef(align = "right")
        ))
    })
  })
}
