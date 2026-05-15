# =============================================================================
# 仪表盘/模块/mod_ranking.R
# 排行榜：多指标国家排名与排名变动追踪
# =============================================================================

mod_ranking_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = htmltools::HTML("&#127942; \u6392\u884c Ranking"),
    mod_v3_hero(
      kicker = "COUNTRY RANKINGS",
      title = "\u591a\u6307\u6807\u56fd\u5bb6\u6392\u540d",
      lead = paste(
        "\u6309\u4eba\u5747 CHE\u3001OOPS\u3001GGHE-D\u3001\u9884\u671f\u5bff\u547d\u7b49\u6307\u6807\u5bf9 195 \u56fd\u5bb6\u6392\u540d\uff0c",
        "\u8ffd\u8e2a\u6392\u540d\u53d8\u52a8\uff0c\u8bc6\u522b\u4e0a\u5347\u8005\u4e0e\u4e0b\u964d\u8005\u3002"
      ),
      meta = list("195 \u56fd\u5bb6", "\u591a\u6307\u6807", "2000\u20132023")
    ),
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 280,
        shiny::sliderInput(ns("year"), "\u5e74\u4efd",
          min = 2000, max = 2023, value = 2023, step = 1, sep = ""),
        shiny::selectInput(ns("indicator"), "\u6392\u540d\u6307\u6807",
          choices = c(
            "\u4eba\u5747 CHE (USD)" = "che_pc_usd2023",
            "OOPS (%)" = "hf3_che",
            "GGHE-D (%)" = "gghed_che",
            "\u9884\u671f\u5bff\u547d" = "life_exp",
            "EXT (%)" = "ext_che"
          ), selected = "che_pc_usd2023"),
        shiny::selectInput(ns("direction"), "\u6392\u5e8f\u65b9\u5411",
          choices = c("\u964d\u5e8f (\u9ad8\u2192\u4f4e)" = "desc",
                      "\u5347\u5e8f (\u4f4e\u2192\u9ad8)" = "asc"),
          selected = "desc"),
        shiny::numericInput(ns("top_n"), "\u663e\u793a\u6570\u91cf",
          value = 25, min = 10, max = 50, step = 5),
        shiny::tags$hr(),
        shiny::helpText(
          "\u6392\u540d\u53d8\u52a8 = \u5f53\u5e74\u6392\u540d - 5 \u5e74\u524d\u6392\u540d\u3002",
          "\u8d1f\u503c = \u6392\u540d\u4e0a\u5347\uff08\u6539\u5584\uff09\u3002"
        )
      ),
      shiny::uiOutput(ns("kpi_strip")),
      # Row 1
      bslib::layout_columns(
        col_widths = c(7, 5),
        mod_card(
          title = "\u56fd\u5bb6\u6392\u540d\u6761\u5f62\u56fe",
          htmltools::p(class = "card-note",
            "\u6309\u9009\u5b9a\u6307\u6807\u6392\u5e8f\u7684\u56fd\u5bb6\u6392\u884c\u3002"),
          mod_spinner(plotly::plotlyOutput(ns("rank_bar"), height = 520))
        ),
        mod_card(
          title = "\u6392\u540d\u53d8\u52a8 Top 15",
          htmltools::p(class = "card-note",
            "\u8fc7\u53bb 5 \u5e74\u6392\u540d\u4e0a\u5347\u6700\u5feb\u7684\u56fd\u5bb6\u3002"),
          mod_spinner(plotly::plotlyOutput(ns("rank_change"), height = 520))
        )
      ),
      # Row 2
      bslib::layout_columns(
        col_widths = c(6, 6),
        mod_card(
          title = "\u6392\u540d\u5206\u5e03\uff08\u6309\u6536\u5165\u7ec4\uff09",
          htmltools::p(class = "card-note",
            "\u5404\u6536\u5165\u7ec4\u56fd\u5bb6\u5728\u6392\u540d\u4e2d\u7684\u4f4d\u7f6e\u5206\u5e03\u3002"),
          mod_spinner(plotly::plotlyOutput(ns("rank_by_income"), height = 380))
        ),
        mod_card(
          title = "\u6307\u6807\u503c\u5206\u5e03",
          htmltools::p(class = "card-note",
            "\u9009\u5b9a\u6307\u6807\u7684\u76f4\u65b9\u56fe\uff0c\u7ea2\u7ebf = \u4e2d\u4f4d\u6570\u3002"),
          mod_spinner(plotly::plotlyOutput(ns("value_hist"), height = 380))
        )
      ),
      mod_card(
        title = "\u5b8c\u6574\u6392\u540d\u8868",
        mod_spinner(reactable::reactableOutput(ns("rank_table")))
      )
    )
  )
}

mod_ranking_server <- function(id, master_r) {
  shiny::moduleServer(id, function(input, output, session) {

    ranked <- shiny::reactive({
      m <- master_r(); yr <- input$year; ind <- input$indicator
      d <- m[m$year == yr & is.finite(m[[ind]]), ]
      d <- d[order(if (input$direction == "desc") -d[[ind]] else d[[ind]]), ]
      d$rank <- seq_len(nrow(d))
      # Get rank from 5 years ago
      yr_prev <- yr - 5
      d_prev <- m[m$year == yr_prev & is.finite(m[[ind]]), ]
      d_prev <- d_prev[order(if (input$direction == "desc") -d_prev[[ind]] else d_prev[[ind]]), ]
      d_prev$rank_prev <- seq_len(nrow(d_prev))
      d <- merge(d, d_prev[, c("iso3_code", "rank_prev")], by = "iso3_code", all.x = TRUE)
      d$rank_change <- d$rank_prev - d$rank  # positive = improved
      d[order(d$rank), ]
    })

    output$kpi_strip <- shiny::renderUI({
      d <- ranked()
      mod_v3_kpi_grid(
        mod_v3_kpi(format(nrow(d), big.mark = ","),
                   "\u56fd\u5bb6\u6570", tone = "primary"),
        mod_v3_kpi(sprintf("%.1f", median(d[[input$indicator]], na.rm = TRUE)),
                   "\u4e2d\u4f4d\u6570", tone = "secondary"),
        mod_v3_kpi(as.character(sum(d$rank_change > 0, na.rm = TRUE)),
                   "\u6392\u540d\u4e0a\u5347", tone = "good"),
        mod_v3_kpi(as.character(sum(d$rank_change < 0, na.rm = TRUE)),
                   "\u6392\u540d\u4e0b\u964d", tone = "bad")
      )
    })

    output$rank_bar <- plotly::renderPlotly({
      d <- ranked()
      top <- utils::head(d, input$top_n)
      top$country_name <- factor(top$country_name, levels = rev(top$country_name))
      safe_plotly({
        plotly::plot_ly(top, x = stats::as.formula(paste0("~", input$indicator)),
                        y = ~country_name, type = "bar", orientation = "h",
                        marker = list(color = "#1d3f5f"),
                        text = ~sprintf("#%d", rank), textposition = "outside") |>
          ghs_plotly_layout() |>
          plotly::layout(
            xaxis = list(title = input$indicator),
            yaxis = list(title = ""),
            margin = list(l = 130)
          )
      })
    })

    output$rank_change <- plotly::renderPlotly({
      d <- ranked()
      d <- d[is.finite(d$rank_change), ]
      d <- d[order(-d$rank_change), ]
      top <- utils::head(d, 15)
      top$country_name <- factor(top$country_name, levels = rev(top$country_name))
      safe_plotly({
        plotly::plot_ly(top, x = ~rank_change, y = ~country_name,
                        type = "bar", orientation = "h",
                        marker = list(
                          color = ifelse(top$rank_change > 0, "#2a857a", "#a23b3b")
                        ),
                        text = ~sprintf("%+d", rank_change), textposition = "outside") |>
          ghs_plotly_layout() |>
          plotly::layout(
            xaxis = list(title = "\u6392\u540d\u53d8\u52a8 (\u6b63 = \u4e0a\u5347)"),
            yaxis = list(title = ""),
            margin = list(l = 130)
          )
      })
    })

    output$rank_by_income <- plotly::renderPlotly({
      d <- ranked()
      d <- d[!is.na(d$income_group), ]
      safe_plotly({
        plotly::plot_ly(d, x = ~income_group, y = ~rank, type = "box",
                        color = ~income_group,
                        colors = unname(brand_palette$income)) |>
          ghs_plotly_layout() |>
          plotly::layout(
            showlegend = FALSE,
            xaxis = list(title = ""),
            yaxis = list(title = "\u6392\u540d\u4f4d\u7f6e", autorange = "reversed")
          )
      })
    })

    output$value_hist <- plotly::renderPlotly({
      d <- ranked()
      vals <- d[[input$indicator]]
      safe_plotly({
        plotly::plot_ly(x = vals, type = "histogram",
                        marker = list(color = "rgba(29,63,95,0.6)",
                                      line = list(color = "#1d3f5f", width = 1)),
                        nbinsx = 20) |>
          ghs_plotly_layout() |>
          plotly::layout(
            xaxis = list(title = input$indicator),
            yaxis = list(title = "\u56fd\u5bb6\u6570"),
            shapes = list(
              list(type = "line",
                   x0 = median(vals, na.rm = TRUE), x1 = median(vals, na.rm = TRUE),
                   y0 = 0, y1 = 1, yref = "paper",
                   line = list(color = "#a23b3b", width = 2, dash = "dash"))
            )
          )
      })
    })

    output$rank_table <- reactable::renderReactable({
      d <- ranked()
      tab <- data.frame(
        "\u6392\u540d" = d$rank,
        "\u56fd\u5bb6" = d$country_name,
        "\u6536\u5165\u7ec4" = d$income_group,
        "\u6307\u6807\u503c" = round(d[[input$indicator]], 1),
        "\u6392\u540d\u53d8\u52a8" = ifelse(is.finite(d$rank_change),
                                             sprintf("%+d", d$rank_change), "\u2014"),
        check.names = FALSE
      )
      reactable::reactable(tab, defaultPageSize = 20, searchable = TRUE,
        highlight = TRUE, striped = TRUE,
        defaultColDef = reactable::colDef(
          headerStyle = list(background = "#f1f3f7", fontWeight = 700)))
    })
  })
}
