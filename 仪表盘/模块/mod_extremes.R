# =============================================================================
# 仪表盘/模块/mod_extremes.R
# 极端值分析：异常高/低支出国家的识别与特征
# =============================================================================

mod_extremes_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = htmltools::HTML("&#9888; \u6781\u503c Extremes"),
    mod_v3_hero(
      kicker = "EXTREME VALUES & OUTLIERS",
      title = "\u6781\u7aef\u503c\u8bc6\u522b\u4e0e\u5f02\u5e38\u56fd\u5bb6\u5206\u6790",
      lead = paste(
        "\u5168\u7403\u536b\u751f\u652f\u51fa\u5206\u5e03\u5448\u73b0\u6781\u7aef\u504f\u659c\uff1a\u4eba\u5747 CHE \u6700\u9ad8\u4e0e\u6700\u4f4e\u56fd\u5bb6\u76f8\u5dee 100 \u500d\u3002",
        "\u672c\u6a21\u5757\u8bc6\u522b\u7edf\u8ba1\u5f02\u5e38\u503c\u3001\u5206\u6790\u5176\u7279\u5f81\u4e0e\u6210\u56e0\u3002"
      ),
      meta = list("IQR \u65b9\u6cd5", "Z-score", "195 \u56fd\u5bb6")
    ),
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 280,
        shiny::sliderInput(ns("year"), "\u5e74\u4efd",
          min = 2000, max = 2023, value = 2023, step = 1, sep = ""),
        shiny::selectInput(ns("indicator"), "\u68c0\u6d4b\u6307\u6807",
          choices = c(
            "\u4eba\u5747 CHE (USD)" = "che_pc_usd2023",
            "OOPS (%)" = "hf3_che",
            "GGHE-D (%)" = "gghed_che",
            "EXT (%)" = "ext_che"
          ), selected = "che_pc_usd2023"),
        shiny::sliderInput(ns("iqr_mult"), "IQR \u500d\u6570\u9608\u503c",
          min = 1.0, max = 3.0, value = 1.5, step = 0.25),
        shiny::tags$hr(),
        shiny::helpText(
          "\u5f02\u5e38\u503c\u5b9a\u4e49\uff1a\u8d85\u8fc7 Q3 + k\u00d7IQR \u6216\u4f4e\u4e8e Q1 - k\u00d7IQR\u3002",
          "\u8c03\u6574 k \u503c\u53ef\u6539\u53d8\u5f02\u5e38\u68c0\u6d4b\u7684\u4e25\u683c\u7a0b\u5ea6\u3002"
        )
      ),
      shiny::uiOutput(ns("kpi_strip")),
      # Row 1
      bslib::layout_columns(
        col_widths = c(7, 5),
        mod_card(
          title = "\u5206\u5e03\u4e0e\u5f02\u5e38\u503c\u6807\u8bb0",
          htmltools::p(class = "card-note",
            "\u7ea2\u8272\u70b9 = \u8d85\u8fc7\u9608\u503c\u7684\u5f02\u5e38\u56fd\u5bb6\u3002\u7070\u8272 = \u6b63\u5e38\u8303\u56f4\u3002"),
          mod_spinner(plotly::plotlyOutput(ns("outlier_scatter"), height = 440))
        ),
        mod_card(
          title = "\u5f02\u5e38\u503c\u5206\u5e03\u7bb1\u7ebf\u56fe",
          htmltools::p(class = "card-note",
            "\u6309\u5927\u6d32\u5206\u7ec4\u7684\u7bb1\u7ebf\u56fe\uff0c\u7ea2\u8272\u865a\u7ebf = \u5f02\u5e38\u9608\u503c\u3002"),
          mod_spinner(plotly::plotlyOutput(ns("box_outliers"), height = 440))
        )
      ),
      # Row 2
      bslib::layout_columns(
        col_widths = c(6, 6),
        mod_card(
          title = "\u5f02\u5e38\u56fd\u5bb6\u6570\u91cf\u8d8b\u52bf",
          htmltools::p(class = "card-note",
            "\u6bcf\u5e74\u88ab\u8bc6\u522b\u4e3a\u5f02\u5e38\u503c\u7684\u56fd\u5bb6\u6570\u91cf\u53d8\u5316\u3002"),
          mod_spinner(plotly::plotlyOutput(ns("outlier_trend"), height = 380))
        ),
        mod_card(
          title = "\u6781\u7aef\u56fd\u5bb6\u7684\u5171\u540c\u7279\u5f81",
          htmltools::p(class = "card-note",
            "\u5f02\u5e38\u56fd\u5bb6\u5728\u5176\u4ed6\u6307\u6807\u4e0a\u7684\u5747\u503c vs \u6b63\u5e38\u56fd\u5bb6\u3002"),
          mod_spinner(plotly::plotlyOutput(ns("feature_compare"), height = 380))
        )
      ),
      # Table
      mod_card(
        title = "\u5f02\u5e38\u56fd\u5bb6\u8be6\u8868",
        htmltools::p(class = "card-note",
          "\u5f53\u5e74\u88ab\u8bc6\u522b\u4e3a\u5f02\u5e38\u503c\u7684\u56fd\u5bb6\u53ca\u5176\u6307\u6807\u3002"),
        mod_spinner(reactable::reactableOutput(ns("outlier_table")))
      )
    )
  )
}

mod_extremes_server <- function(id, master_r) {
  shiny::moduleServer(id, function(input, output, session) {

    detect_outliers <- function(d, col, k = 1.5) {
      vals <- d[[col]]
      q1 <- stats::quantile(vals, 0.25, na.rm = TRUE)
      q3 <- stats::quantile(vals, 0.75, na.rm = TRUE)
      iqr <- q3 - q1
      lower <- q1 - k * iqr
      upper <- q3 + k * iqr
      d$is_outlier <- !is.na(vals) & (vals < lower | vals > upper)
      d$outlier_type <- ifelse(vals > upper, "high",
                        ifelse(vals < lower, "low", "normal"))
      d$outlier_type[is.na(vals)] <- NA_character_
      attr(d, "bounds") <- c(lower = lower, upper = upper)
      d
    }

    output$kpi_strip <- shiny::renderUI({
      m <- master_r(); yr <- input$year
      d <- m[m$year == yr & is.finite(m[[input$indicator]]), ]
      d <- detect_outliers(d, input$indicator, input$iqr_mult)
      n_out <- sum(d$is_outlier, na.rm = TRUE)
      pct_out <- n_out / nrow(d) * 100
      mod_v3_kpi_grid(
        mod_v3_kpi(format(nrow(d), big.mark = ","),
                   "\u56fd\u5bb6\u6570", tone = "primary"),
        mod_v3_kpi(as.character(n_out),
                   "\u5f02\u5e38\u56fd\u5bb6", tone = "bad"),
        mod_v3_kpi(fmt_v3_pct(pct_out),
                   "\u5f02\u5e38\u6bd4\u4f8b", tone = "warn"),
        mod_v3_kpi(sprintf("%.1f", input$iqr_mult),
                   "IQR \u500d\u6570", tone = "neutral")
      )
    })

    output$outlier_scatter <- plotly::renderPlotly({
      m <- master_r(); yr <- input$year
      d <- m[m$year == yr & is.finite(m[[input$indicator]]) &
             !is.na(m$country_name), ]
      d <- detect_outliers(d, input$indicator, input$iqr_mult)
      d <- d[order(d[[input$indicator]]), ]
      d$rank <- seq_len(nrow(d))
      bounds <- attr(d, "bounds")
      safe_plotly({
        plotly::plot_ly(d, x = ~rank, y = stats::as.formula(paste0("~", input$indicator)),
                        color = ~is_outlier, text = ~country_name,
                        type = "scatter", mode = "markers",
                        colors = c("FALSE" = "#5d667a", "TRUE" = "#a23b3b"),
                        marker = list(size = 7, opacity = 0.7)) |>
          ghs_plotly_layout() |>
          plotly::layout(
            xaxis = list(title = "\u56fd\u5bb6\u6392\u5e8f"),
            yaxis = list(title = input$indicator),
            showlegend = FALSE,
            shapes = list(
              list(type = "line", x0 = 0, x1 = nrow(d),
                   y0 = bounds["upper"], y1 = bounds["upper"],
                   line = list(color = "#a23b3b", width = 1.5, dash = "dash")),
              list(type = "line", x0 = 0, x1 = nrow(d),
                   y0 = bounds["lower"], y1 = bounds["lower"],
                   line = list(color = "#1d3f5f", width = 1.5, dash = "dash"))
            )
          )
      })
    })

    output$box_outliers <- plotly::renderPlotly({
      m <- master_r(); yr <- input$year
      d <- m[m$year == yr & is.finite(m[[input$indicator]]) &
             !is.na(m$continent), ]
      safe_plotly({
        plotly::plot_ly(d, x = ~continent,
                        y = stats::as.formula(paste0("~", input$indicator)),
                        type = "box", color = ~continent,
                        colors = unname(brand_palette$continent),
                        boxpoints = "outliers") |>
          ghs_plotly_layout() |>
          plotly::layout(showlegend = FALSE,
                         xaxis = list(title = ""),
                         yaxis = list(title = input$indicator))
      })
    })

    output$outlier_trend <- plotly::renderPlotly({
      m <- master_r()
      years <- sort(unique(m$year))
      counts <- vapply(years, function(yr) {
        d <- m[m$year == yr & is.finite(m[[input$indicator]]), ]
        if (nrow(d) < 10) return(NA_real_)
        d <- detect_outliers(d, input$indicator, input$iqr_mult)
        sum(d$is_outlier, na.rm = TRUE)
      }, numeric(1))
      df <- data.frame(year = years, n_outliers = counts)
      df <- df[is.finite(df$n_outliers), ]
      safe_plotly({
        plotly::plot_ly(df, x = ~year, y = ~n_outliers,
                        type = "scatter", mode = "lines+markers",
                        line = list(color = "#a23b3b", width = 2.5),
                        marker = list(color = "#a23b3b", size = 6),
                        fill = "tozeroy",
                        fillcolor = "rgba(162,59,59,0.1)") |>
          ghs_plotly_layout() |>
          plotly::layout(
            xaxis = list(title = ""),
            yaxis = list(title = "\u5f02\u5e38\u56fd\u5bb6\u6570")
          )
      })
    })

    output$feature_compare <- plotly::renderPlotly({
      m <- master_r(); yr <- input$year
      d <- m[m$year == yr & is.finite(m[[input$indicator]]), ]
      d <- detect_outliers(d, input$indicator, input$iqr_mult)
      compare_vars <- c("che_pc_usd2023", "hf3_che", "gghed_che", "life_exp")
      compare_labels <- c("CHE/cap", "OOPS%", "GGHED%", "Life exp")
      outlier_means <- vapply(compare_vars, function(v) {
        mean(d[[v]][d$is_outlier], na.rm = TRUE)
      }, numeric(1))
      normal_means <- vapply(compare_vars, function(v) {
        mean(d[[v]][!d$is_outlier], na.rm = TRUE)
      }, numeric(1))
      # Normalize for comparison
      ratio <- outlier_means / pmax(normal_means, 0.01)
      df <- data.frame(variable = compare_labels, ratio = ratio)
      safe_plotly({
        plotly::plot_ly(df, x = ~variable, y = ~ratio,
                        type = "bar",
                        marker = list(color = ifelse(df$ratio > 1, "#a23b3b", "#1d3f5f"))) |>
          ghs_plotly_layout() |>
          plotly::layout(
            xaxis = list(title = ""),
            yaxis = list(title = "\u5f02\u5e38/\u6b63\u5e38 \u6bd4\u503c"),
            shapes = list(
              list(type = "line", x0 = -0.5, x1 = 3.5, y0 = 1, y1 = 1,
                   line = list(color = "#5d667a", width = 1, dash = "dash"))
            )
          )
      })
    })

    output$outlier_table <- reactable::renderReactable({
      m <- master_r(); yr <- input$year
      d <- m[m$year == yr & is.finite(m[[input$indicator]]), ]
      d <- detect_outliers(d, input$indicator, input$iqr_mult)
      d <- d[d$is_outlier, ]
      d <- d[order(-abs(d[[input$indicator]])), ]
      tab <- data.frame(
        "\u56fd\u5bb6" = d$country_name,
        "\u5927\u6d32" = d$continent,
        "\u6536\u5165\u7ec4" = d$income_group,
        "\u6307\u6807\u503c" = round(d[[input$indicator]], 1),
        "\u7c7b\u578b" = d$outlier_type,
        "CHE/cap" = round(d$che_pc_usd2023, 0),
        check.names = FALSE
      )
      reactable::reactable(tab, defaultPageSize = 15, searchable = TRUE,
        highlight = TRUE, striped = TRUE,
        defaultColDef = reactable::colDef(
          headerStyle = list(background = "#f1f3f7", fontWeight = 700)))
    })
  })
}
