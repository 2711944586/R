# =============================================================================
# 仪表盘/模块/mod_ranking.R
# 排行榜：多指标国家排名与排名变动追踪
# =============================================================================

mod_ranking_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = "\u6392\u884c Ranking",
    value = "ranking",
    mod_v3_hero(
      kicker = "\u56fd\u5bb6\u6392\u540d",
      title = "\u591a\u6307\u6807\u56fd\u5bb6\u6392\u540d",
      lead = paste(
        "\u6309\u4eba\u5747 CHE\u3001OOPS\u3001GGHE-D\u3001\u9884\u671f\u5bff\u547d\u7b49\u6307\u6807\u5bf9 195 \u56fd\u5bb6\u6392\u540d\uff0c",
        "\u8ffd\u8e2a\u6392\u540d\u53d8\u52a8\uff0c\u8bc6\u522b\u4e0a\u5347\u8005\u4e0e\u4e0b\u964d\u8005\u3002"
      ),
      meta = list("195 \u56fd\u5bb6", "\u591a\u6307\u6807", "2000\u20132023")
    ),
    mod_v3_page_body(
      wide = TRUE,
      mod_v3_topic_brief("ranking"),
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
          mod_v3_sidebar_note(
            "\u6392\u540d\u8bfb\u6cd5",
            "\u6392\u540d\u9002\u5408\u5feb\u901f\u5b9a\u4f4d\u5934\u90e8\u3001\u5c3e\u90e8\u548c\u53d8\u52a8\u8f83\u5927\u7684\u56fd\u5bb6\uff0c\u4f46\u4e0d\u5e94\u5355\u72ec\u4f5c\u4e3a\u653f\u7b56\u7ed3\u8bba\u3002",
            bullets = c(
              "\u6392\u540d\u53d8\u52a8 = 5 \u5e74\u524d\u6392\u540d - \u5f53\u5e74\u6392\u540d\uff0c\u6b63\u503c\u8868\u793a\u4f4d\u6b21\u4e0a\u5347\u3002",
              "\u9ad8\u503c\u662f\u5426\u201c\u66f4\u597d\u201d\u53d6\u51b3\u4e8e\u6307\u6807\uff1a\u5bff\u547d\u548c GGHE-D \u4e0e OOPS/EXT \u9700\u8981\u5206\u5f00\u89e3\u91ca\u3002",
              "\u4f4d\u6b21\u5dee\u8ddd\u5927\u4e0d\u4ee3\u8868\u6570\u503c\u5dee\u8ddd\u5927\uff0c\u76f4\u65b9\u56fe\u548c\u8868\u683c\u7528\u6765\u590d\u6838\u5b9e\u9645\u95f4\u8ddd\u3002"
            )
          )
        ),
      shiny::uiOutput(ns("kpi_strip")),
      # Row 1
      bslib::layout_columns(
        col_widths = c(7, 5),
        mod_card(
          kicker = "R1 \u00b7 Ranked list",
          title = "\u56fd\u5bb6\u6392\u540d\u6761\u5f62\u56fe",
          htmltools::p(class = "card-note",
            "\u6309\u9009\u5b9a\u6307\u6807\u6392\u5e8f\u7684\u56fd\u5bb6\u6392\u884c\u3002"),
          mod_v3_chart_guide(
            "\u6761\u5f62\u56fe\u8d1f\u8d23\u201c\u5b9a\u4f4d\u201d\uff0c\u4e0d\u8d1f\u8d23\u201c\u89e3\u91ca\u201d",
            "\u8fd9\u5f20\u56fe\u6700\u9002\u5408\u627e\u51fa\u5f53\u5e74\u5934\u90e8\u6216\u5c3e\u90e8\u56fd\u5bb6\u3002\u5982\u679c\u76f8\u90bb\u56fd\u5bb6\u6761\u5f62\u957f\u5ea6\u63a5\u8fd1\uff0c\u5373\u4f7f\u4f4d\u6b21\u4e0d\u540c\uff0c\u5b9e\u9645\u5dee\u8ddd\u4e5f\u53ef\u80fd\u5f88\u5c0f\u3002",
            bullets = c("\u5207\u6362\u5347/\u964d\u5e8f\u524d\uff0c\u5148\u786e\u8ba4\u8be5\u6307\u6807\u9ad8\u503c\u662f\u5426\u4ee3\u8868\u66f4\u597d\u7ed3\u679c\u3002", "\u6392\u540d\u56fe\u7684\u5de6\u4fa7\u56fd\u540d\u4fbf\u4e8e\u4e0e\u8868\u683c\u4ea4\u53c9\u68c0\u67e5\u3002")
          ),
          mod_spinner(plotly::plotlyOutput(ns("rank_bar"), height = 520)),
          footer = "\u663e\u793a\u6570\u91cf\u53ef\u5728\u4fa7\u680f\u8c03\u6574\uff0c\u4fbf\u4e8e\u5728\u6982\u89c8\u548c\u957f\u699c\u5355\u4e4b\u95f4\u5207\u6362\u3002"
        ),
        mod_card(
          kicker = "R2 \u00b7 Five-year movement",
          title = "\u6392\u540d\u53d8\u52a8 Top 15",
          htmltools::p(class = "card-note",
            "\u8fc7\u53bb 5 \u5e74\u6392\u540d\u4e0a\u5347\u6700\u5feb\u7684\u56fd\u5bb6\u3002"),
          mod_v3_chart_guide(
            "\u4f4d\u6b21\u53d8\u52a8\u662f\u4e00\u4e2a\u201c\u8ffd\u95ee\u6309\u94ae\u201d",
            "\u4f4d\u6b21\u4e0a\u5347\u53ef\u80fd\u6765\u81ea\u672c\u56fd\u6307\u6807\u6539\u5584\uff0c\u4e5f\u53ef\u80fd\u6765\u81ea\u5176\u4ed6\u56fd\u5bb6\u4e0b\u964d\u6216\u6837\u672c\u53d8\u5316\u3002\u56e0\u6b64\u9700\u8981\u56de\u5230\u8868\u683c\u786e\u8ba4\u6307\u6807\u503c\u7684\u771f\u5b9e\u53d8\u5316\u3002",
            tone = "warn"
          ),
          mod_spinner(plotly::plotlyOutput(ns("rank_change"), height = 520))
        )
      ),
      # Row 2
      bslib::layout_columns(
        col_widths = c(6, 6),
        mod_card(
          kicker = "R3 \u00b7 Income context",
          title = "\u6392\u540d\u5206\u5e03\uff08\u6309\u6536\u5165\u7ec4\uff09",
          htmltools::p(class = "card-note",
            "\u5404\u6536\u5165\u7ec4\u56fd\u5bb6\u5728\u6392\u540d\u4e2d\u7684\u4f4d\u7f6e\u5206\u5e03\u3002"),
          mod_v3_chart_guide(
            "\u5206\u7ec4\u6392\u540d\u53ef\u4ee5\u964d\u4f4e\u5168\u7403\u699c\u5355\u7684\u7ed3\u6784\u504f\u5dee",
            "\u5982\u679c\u4e00\u4e2a\u56fd\u5bb6\u5728\u5168\u7403\u6392\u540d\u4e0d\u9ad8\uff0c\u4f46\u5728\u540c\u6536\u5165\u7ec4\u4e2d\u5904\u4e8e\u524d\u5217\uff0c\u653f\u7b56\u542b\u4e49\u4f1a\u5b8c\u5168\u4e0d\u540c\u3002",
            tone = "good"
          ),
          mod_spinner(plotly::plotlyOutput(ns("rank_by_income"), height = 380))
        ),
        mod_card(
          kicker = "R4 \u00b7 Value spread",
          title = "\u6307\u6807\u503c\u5206\u5e03",
          htmltools::p(class = "card-note",
            "\u9009\u5b9a\u6307\u6807\u7684\u76f4\u65b9\u56fe\uff0c\u7ea2\u7ebf = \u4e2d\u4f4d\u6570\u3002"),
          mod_v3_chart_guide(
            "\u76f4\u65b9\u56fe\u544a\u8bc9\u4f60\u6392\u540d\u662f\u5426\u771f\u7684\u62c9\u5f00",
            "\u5982\u679c\u5206\u5e03\u96c6\u4e2d\uff0c\u5219\u591a\u6570\u56fd\u5bb6\u5b9e\u9645\u503c\u5dee\u8ddd\u5f88\u5c0f\uff1b\u5982\u679c\u53f3\u504f\u6216\u5de6\u504f\u660e\u663e\uff0c\u5219\u5c3e\u90e8\u56fd\u5bb6\u5bf9\u699c\u5355\u89c6\u89c9\u5f71\u54cd\u5f88\u5927\u3002"
          ),
          mod_spinner(plotly::plotlyOutput(ns("value_hist"), height = 380)),
          footer = "\u7ea2\u8272\u865a\u7ebf\u662f\u5f53\u5e74\u6837\u672c\u4e2d\u4f4d\u6570\uff0c\u53ef\u7528\u4e8e\u5feb\u901f\u533a\u5206\u9ad8\u4e8e/\u4f4e\u4e8e\u5178\u578b\u6c34\u5e73\u7684\u56fd\u5bb6\u3002"
        )
      ),
      mod_card(
        kicker = "R5 \u00b7 Ranking audit",
        title = "\u5b8c\u6574\u6392\u540d\u8868",
        mod_v3_chart_guide(
          "\u8868\u683c\u662f\u6392\u540d\u89e3\u91ca\u7684\u6700\u540e\u4e00\u9053\u95e8",
          "\u5728\u8fd9\u91cc\u540c\u65f6\u67e5\u770b\u56fd\u5bb6\u3001\u6536\u5165\u7ec4\u3001\u6307\u6807\u503c\u548c 5 \u5e74\u6392\u540d\u53d8\u52a8\uff0c\u907f\u514d\u53ea\u6839\u636e\u4f4d\u6b21\u505a\u8fc7\u5ea6\u89e3\u91ca\u3002"
        ),
        mod_spinner(reactable::reactableOutput(ns("rank_table"))),
        footer = "\u6392\u540d\u53d8\u52a8\u4e2d\u7684 \u2014 \u8868\u793a 5 \u5e74\u524d\u7f3a\u5c11\u53ef\u6bd4\u6392\u540d\u3002"
      )
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
