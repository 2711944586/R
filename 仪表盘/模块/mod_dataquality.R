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
        "\u67e5\u770b\u6bcf\u4e2afield\u5728\u5404\u56fd\u00b7\u5404\u5e74\u7684\u8986\u76d6\u60c5\u51b5\u3002",
        "\u5305\u542b\u7f3a\u5931\u6a21\u5f0f\u70ed\u56fe\u3001coverage\u8d8b\u52bf\u3001\u5f02\u5e38\u503c\u8bca\u65ad\u4e0e\u4e00\u81f4\u6027\u68c0\u67e5\u3002"
      ),
      meta = list("field: 40+", "coverage\u9762\u677f", "\u5f02\u5e38\u503c\u68c0\u6d4b")
    ),
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 260,
        shiny::selectInput(ns("indicator"), "\u9009\u62e9field",
          choices = c(
            "\u4eba\u5747 CHE" = "che_pc_usd2023",
            "OOPS %" = "hf3_che",
            "GGHE-D %" = "gghed_che",
            "\u9884\u671f\u5bff\u547d" = "life_exp",
            "U5MR" = "u5mr",
            "\u4eba\u5747 GDP" = "gdp_pc_usd",
            "\u4eba\u53e3" = "pop"
          ), selected = "che_pc_usd2023"),
        shiny::sliderInput(ns("year_range"), "year_col\u8303\u56f4",
          min = 2000, max = 2023, value = c(2000, 2023), step = 1, sep = ""),
        shiny::tags$hr(),
        shiny::helpText(
          "\u7eff\u8272 = col_x = \u7f3a\u5931\u3002",
          "\u4f7f\u7528\u4e0a\u65b9\u4e0b\u62c9\u83dc\u5355\u5207\u6362\u4e0d\u540cfield\u3002"
        )
      ),
      shiny::uiOutput(ns("kpi_strip")),
      bslib::layout_columns(
        col_widths = c(7, 5),
        mod_card(
          title = "\u7f3a\u5931\u6a21\u5f0f\u70ed\u529b\u56fe",
          htmltools::p(class = "card-note",
            "\u6bcf\u4e2a\u5355\u5143\u683c = \u4e00\u4e2acountry\u00b7\u5e74\u3002"),
          mod_spinner(plotly::plotlyOutput(ns("missing_heatmap"), height = 500))
        ),
        mod_card(
          title = "coverage\u968f\u5e74\u53d8\u5316",
          htmltools::p(class = "card-note",
            "\u6709\u6570\u636e\u7684country\u6570\u968fyear_col\u53d8\u5316\u3002"),
          mod_spinner(plotly::plotlyOutput(ns("coverage_trend"), height = 500))
        )
      ),
      bslib::layout_columns(
        col_widths = c(6, 6),
        mod_card(
          title = "\u5404fieldcoverage\u6982\u89c8",
          mod_spinner(reactable::reactableOutput(ns("var_coverage_table")))
        ),
        mod_card(
          title = "\u6309countrycoverage",
          mod_spinner(reactable::reactableOutput(ns("country_coverage_table")))
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
        mod_v3_kpi(sprintf("%.1f%%", coverage), "coverage", tone = "secondary"),
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
                         yaxis = list(title = "coverage (%)", range = c(0, 100)))
      })
    })

    output$var_coverage_table <- reactable::renderReactable({
      m <- filtered()
      vars <- c("che_pc_usd2023", "che_usd2023", "gghed_che", "pvtd_che",
                "ext_che", "hf3_che", "life_exp", "u5mr", "gdp_pc_usd", "pop")
      vars <- vars[vars %in% names(m)]
      total <- nrow(m)
      df <- data.frame(
        `field` = vars,
        `coverage` = sapply(vars, function(v) sum(is.finite(m[[v]]))),
        `pct` = sapply(vars, function(v) round(sum(is.finite(m[[v]])) / total * 100, 1)),
        check.names = FALSE
      )
      df <- df[order(-df$coverage), ]
      reactable::reactable(df, defaultPageSize = 10, highlight = TRUE,
        defaultColDef = reactable::colDef(headerStyle = list(background = "#f1f3f7")))
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
      names(agg) <- c("ISO", "country", "\u6709\u6570\u636eyear_col",
                       "\u603byear_col", "coverage(%)")
      reactable::reactable(agg, defaultPageSize = 12, searchable = TRUE,
        defaultColDef = reactable::colDef(headerStyle = list(background = "#f1f3f7")))
    })
  })
}
