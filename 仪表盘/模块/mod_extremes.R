
mod_extremes_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = "\u6781\u503c Extremes",
    value = "extremes",
    mod_v3_hero(
      kicker = "\u6781\u7aef\u503c\u4e0e\u5f02\u5e38",
      title = "\u6781\u7aef\u503c\u8bc6\u522b\u4e0e\u5f02\u5e38\u56fd\u5bb6\u5206\u6790",
      lead = paste(
        "\u5168\u7403\u536b\u751f\u652f\u51fa\u5206\u5e03\u5448\u73b0\u6781\u7aef\u504f\u659c\uff1a\u4eba\u5747 CHE \u6700\u9ad8\u4e0e\u6700\u4f4e\u56fd\u5bb6\u76f8\u5dee 100 \u500d\u3002",
        "\u672c\u6a21\u5757\u8bc6\u522b\u7edf\u8ba1\u5f02\u5e38\u503c\u3001\u5206\u6790\u5176\u7279\u5f81\u4e0e\u6210\u56e0\u3002"
      ),
      meta = list("IQR \u65b9\u6cd5", "Z-score", "195 \u56fd\u5bb6")
    ),
    mod_v3_page_body(
      wide = TRUE,
      mod_v3_topic_brief("extremes"),
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
          mod_v3_sidebar_note(
            "\u5f02\u5e38\u68c0\u6d4b",
            "\u5f02\u5e38\u503c\u5b9a\u4e49\uff1a\u8d85\u8fc7 Q3 + k\u00d7IQR \u6216\u4f4e\u4e8e Q1 - k\u00d7IQR\u3002k \u8d8a\u5927\uff0c\u53ea\u6709\u66f4\u6781\u7aef\u7684\u56fd\u5bb6\u4f1a\u88ab\u6807\u8bb0\u3002",
            bullets = c(
              "\u4eba\u5747 CHE \u901a\u5e38\u53ea\u6709\u9ad8\u7aef\u5f02\u5e38\uff0cOOPS \u53ef\u80fd\u540c\u65f6\u5b58\u5728\u9ad8\u4f4e\u5c3e\u90e8\u3002",
              "\u5f02\u5e38\u503c\u4e0d\u7b49\u4e8e\u9519\u8bef\uff0c\u9700\u8981\u7ed3\u5408\u56fd\u5bb6\u89c4\u6a21\u3001\u4ef7\u683c\u548c\u5236\u5ea6\u80cc\u666f\u89e3\u91ca\u3002",
              "\u8c03\u6574 IQR \u9608\u503c\u53ef\u4ee5\u5728\u654f\u611f\u53d1\u73b0\u548c\u7a33\u5065\u7b5b\u9009\u4e4b\u95f4\u5207\u6362\u3002"
            )
          )
        ),
        shiny::uiOutput(ns("kpi_strip")),
        bslib::layout_columns(
          col_widths = c(7, 5),
          mod_card(
            kicker = "F1 · Ranked outliers",
            title = "\u5206\u5e03\u4e0e\u5f02\u5e38\u503c\u6807\u8bb0",
            htmltools::p(class = "card-note",
              "\u7ea2\u8272\u70b9 = \u8d85\u8fc7\u9608\u503c\u7684\u5f02\u5e38\u56fd\u5bb6\u3002\u7070\u8272 = \u6b63\u5e38\u8303\u56f4\u3002"),
            mod_v3_chart_guide(
              "\u6392\u5e8f\u56fe\u76f4\u63a5\u663e\u793a\u5c3e\u90e8\u65ad\u70b9",
              "\u70b9\u4f4d\u8d8a\u9760\u53f3\u8868\u793a\u6307\u6807\u503c\u8d8a\u9ad8\uff1b\u865a\u7ebf\u4e4b\u5916\u7684\u70b9\u4ee3\u8868\u5f53\u524d\u9608\u503c\u4e0b\u7684\u5f02\u5e38\u6837\u672c\u3002",
              bullets = c("\u4f4e\u7aef\u5f02\u5e38\u5e94\u4e0e\u6570\u636e\u7f3a\u5931\u6216\u53e3\u5f84\u53d8\u5316\u533a\u5206\u3002", "\u9ad8\u7aef\u5f02\u5e38\u9700\u8981\u7ed3\u5408\u6536\u5165\u7ec4\u548c\u8d22\u653f\u7ed3\u6784\u590d\u6838\u3002")
            ),
            mod_spinner(plotly::plotlyOutput(ns("outlier_scatter"), height = 440))
          ),
          mod_card(
            kicker = "F2 · Group context",
            title = "\u5f02\u5e38\u503c\u5206\u5e03\u7bb1\u7ebf\u56fe",
            htmltools::p(class = "card-note",
              "\u6309\u5927\u6d32\u5206\u7ec4\u7684\u7bb1\u7ebf\u56fe\uff0c\u7ea2\u8272\u865a\u7ebf = \u5f02\u5e38\u9608\u503c\u3002"),
            mod_v3_chart_guide(
              "\u7bb1\u7ebf\u56fe\u5224\u65ad\u5f02\u5e38\u662f\u5168\u5c40\u8fd8\u662f\u7ec4\u5185",
              "\u5982\u679c\u67d0\u4e2a\u7ec4\u7684\u6574\u4f53\u7bb1\u4f53\u5df2\u63a5\u8fd1\u9608\u503c\uff0c\u8bf4\u660e\u5f02\u5e38\u53ef\u80fd\u662f\u533a\u57df\u7ed3\u6784\u7279\u5f81\uff0c\u800c\u4e0d\u662f\u4e2a\u522b\u56fd\u5bb6\u5f02\u52a8\u3002",
              tone = "warn"
            ),
            mod_spinner(plotly::plotlyOutput(ns("box_outliers"), height = 440)),
            footer = "IQR \u9608\u503c\u57fa\u4e8e\u5f53\u5e74\u5168\u6837\u672c\u8ba1\u7b97\uff0c\u4e0d\u968f\u5927\u6d32\u5206\u7ec4\u91cd\u7b97\u3002"
          )
        ),
        bslib::layout_columns(
          col_widths = c(6, 6),
          mod_card(
            kicker = "F3 · Persistence",
            title = "\u5f02\u5e38\u56fd\u5bb6\u6570\u91cf\u8d8b\u52bf",
            htmltools::p(class = "card-note",
              "\u6bcf\u5e74\u88ab\u8bc6\u522b\u4e3a\u5f02\u5e38\u503c\u7684\u56fd\u5bb6\u6570\u91cf\u53d8\u5316\u3002"),
            mod_v3_chart_guide(
              "\u6301\u7eed\u6027\u6bd4\u5355\u5e74\u6807\u8bb0\u66f4\u91cd\u8981",
              "\u5f02\u5e38\u6570\u91cf\u5728\u591a\u5e74\u6301\u7eed\u589e\u52a0\uff0c\u8868\u793a\u5206\u5e03\u5c3e\u90e8\u53ef\u80fd\u6b63\u5728\u62c9\u957f\uff1b\u53ea\u6709\u5355\u5e74\u5c16\u5cf0\u5219\u53ef\u80fd\u662f\u51b2\u51fb\u6216\u6570\u636e\u66f4\u65b0\u3002"
            ),
            mod_spinner(plotly::plotlyOutput(ns("outlier_trend"), height = 380))
          ),
          mod_card(
            kicker = "F4 · Profile contrast",
            title = "\u6781\u7aef\u56fd\u5bb6\u7684\u5171\u540c\u7279\u5f81",
            htmltools::p(class = "card-note",
              "\u5f02\u5e38\u56fd\u5bb6\u5728\u5176\u4ed6\u6307\u6807\u4e0a\u7684\u5747\u503c vs \u6b63\u5e38\u56fd\u5bb6\u3002"),
            mod_v3_chart_guide(
              "\u7528\u7279\u5f81\u6bd4\u503c\u5224\u65ad\u5f02\u5e38\u7684\u80cc\u666f",
              "\u6bd4\u503c\u9ad8\u4e8e 1 \u8868\u793a\u5f02\u5e38\u56fd\u5bb6\u5728\u8be5\u6307\u6807\u4e0a\u9ad8\u4e8e\u6b63\u5e38\u7ec4\uff1b\u8fd9\u80fd\u5e2e\u52a9\u533a\u5206\u8d22\u5bcc\u9a71\u52a8\u3001\u7b79\u8d44\u7ed3\u6784\u9a71\u52a8\u548c\u5065\u5eb7\u7ed3\u679c\u5dee\u5f02\u3002",
              tone = "good"
            ),
            mod_spinner(plotly::plotlyOutput(ns("feature_compare"), height = 380))
          )
        ),
        mod_card(
          kicker = "F5 · Outlier audit",
          title = "\u5f02\u5e38\u56fd\u5bb6\u8be6\u8868",
          htmltools::p(class = "card-note",
            "\u5f53\u5e74\u88ab\u8bc6\u522b\u4e3a\u5f02\u5e38\u503c\u7684\u56fd\u5bb6\u53ca\u5176\u6307\u6807\u3002"),
          mod_v3_chart_guide(
            "\u8868\u683c\u7528\u4e8e\u6700\u540e\u6838\u9a8c",
            "\u4ece\u6563\u70b9\u3001\u7bb1\u7ebf\u548c\u7279\u5f81\u5bf9\u6bd4\u4e2d\u627e\u5230\u7684\u56fd\u5bb6\uff0c\u5728\u8fd9\u91cc\u6838\u5bf9\u5927\u6d32\u3001\u6536\u5165\u7ec4\u3001\u6307\u6807\u503c\u548c CHE/cap\u3002"
          ),
          mod_spinner(reactable::reactableOutput(ns("outlier_table"))),
          footer = "\u5f02\u5e38\u6807\u8bb0\u662f\u5206\u6790\u5165\u53e3\uff0c\u4e0d\u662f\u8d28\u91cf\u5224\u5b9a\uff1b\u5e94\u7ed3\u5408\u539f\u59cb\u6570\u636e\u53e3\u5f84\u8fdb\u884c\u590d\u6838\u3002"
        )
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
