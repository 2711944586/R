# =============================================================================
# 仪表盘/模块/mod_financing.R
# 筹资结构：公共、私人、自付与外部援助的制度分担
# =============================================================================

mod_financing_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = htmltools::HTML("&#128176; \u7b79\u8d44 Financing"),
    value = "financing",
    mod_v3_hero(
      kicker = "FINANCING STRUCTURE",
      title = "\u536b\u751f\u7b79\u8d44\u7ed3\u6784\u4e0e\u98ce\u9669\u5206\u62c5",
      lead = paste(
        "HF1-HF4 \u7b79\u8d44\u65b9\u6848\u63cf\u8ff0\u533b\u7597\u8d39\u7528\u6700\u7ec8\u7531\u8c01\u627f\u62c5\uff1a",
        "\u653f\u5e9c\u548c\u5f3a\u5236\u6027\u9884\u4ed8\u673a\u5236\u80fd\u63d0\u4f9b\u98ce\u9669\u5171\u62c5\uff0c",
        "\u5c45\u6c11\u81ea\u4ed8\u548c\u5916\u90e8\u63f4\u52a9\u5219\u5bf9\u5bb6\u5ead\u4fdd\u62a4\u4e0e\u8d22\u653f\u53ef\u6301\u7eed\u6027\u63d0\u51fa\u4e0d\u540c\u538b\u529b\u3002"
      ),
      meta = list("WHO GHED 2024-12", "195 \u56fd\u5bb6", "2000\u20132023", "GGHE-D / PVT-D / EXT / OOPS")
    ),
    mod_v3_page_body(
      wide = TRUE,
      mod_v3_topic_brief("financing"),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 280,
          shiny::sliderInput(ns("year"), "\u5e74\u4efd",
            min = 2000, max = 2023, value = 2023, step = 1, sep = ""),
          shiny::selectInput(ns("group"), "\u5206\u7ec4\u53e3\u5f84",
            choices = c("\u5927\u6d32" = "continent", "\u6536\u5165\u7ec4" = "income_group"),
            selected = "continent"),
          mod_v3_sidebar_note(
            "\u7b79\u8d44\u8bfb\u6cd5",
            "\u8fd9\u4e00\u9875\u5173\u6ce8\u98ce\u9669\u5728\u653f\u5e9c\u3001\u5bb6\u5ead\u548c\u5916\u90e8\u8d44\u91d1\u4e4b\u95f4\u5982\u4f55\u5206\u914d\u3002",
            bullets = c(
              "\u5806\u53e0\u56fe\u770b\u5236\u5ea6\u4e3b\u4f53\u3002",
              "OOPS \u5206\u5e03\u770b\u5bb6\u5ead\u76f4\u63a5\u627f\u62c5\u538b\u529b\u3002",
              "\u65f6\u5e8f\u56fe\u5224\u65ad\u7ed3\u6784\u662f\u957f\u671f\u8f6c\u578b\u8fd8\u662f\u5355\u5e74\u6ce2\u52a8\u3002"
            )
          )
        ),
        shiny::uiOutput(ns("kpi_strip")),
        bslib::layout_columns(
          col_widths = c(6, 6),
          mod_card(
            kicker = "F1 · Source mix",
            title = "\u5206\u7ec4\u7b79\u8d44\u7ed3\u6784",
            htmltools::p(class = "card-note", "GGHE-D + PVT-D + EXT \u4e09\u6e90\u5360\u6bd4\uff0c\u7528\u4e8e\u5224\u65ad\u4e0d\u540c\u7ec4\u522b\u7684\u4e3b\u5bfc\u8d44\u91d1\u6765\u6e90\u3002"),
            mod_v3_chart_guide(
              "\u5148\u770b\u4e3b\u5bfc\u6765\u6e90",
              "\u653f\u5e9c\u5360\u6bd4\u9ad8\u901a\u5e38\u4ee3\u8868\u66f4\u5f3a\u7684\u98ce\u9669\u5171\u62c5\uff0cEXT \u5360\u6bd4\u9ad8\u5219\u9700\u8981\u8fdb\u4e00\u6b65\u8bc4\u4f30\u63f4\u52a9\u9000\u51fa\u98ce\u9669\u3002"
            ),
            mod_spinner(plotly::plotlyOutput(ns("stacked_bar"), height = 420)),
            footer = "\u6570\u503c\u4e3a\u7ec4\u5185\u56fd\u5bb6\u7b80\u5355\u5e73\u5747\uff0c\u4e0d\u662f\u4eba\u53e3\u6216 CHE \u52a0\u6743\u5e73\u5747\u3002"
          ),
          mod_card(
            kicker = "F2 · Household pressure",
            title = "OOPS \u5206\u5e03",
            htmltools::p(class = "card-note", "\u5c45\u6c11\u81ea\u4ed8\u5360\u6bd4\u7684\u5206\u7ec4\u5206\u5e03\uff0c\u540c\u65f6\u5c55\u793a\u4e2d\u4f4d\u6570\u548c\u5c3e\u90e8\u56fd\u5bb6\u3002"),
            mod_v3_chart_guide(
              "\u5c3e\u90e8\u4ee3\u8868\u4fdd\u62a4\u7f3a\u53e3",
              "OOPS \u9ad8\u503c\u56fd\u5bb6\u9700\u8981\u7ed3\u5408\u707e\u96be\u6027\u652f\u51fa\u3001\u8d2b\u56f0\u652f\u51fa\u548c\u653f\u7b56\u6a21\u5757\u8fdb\u4e00\u6b65\u68c0\u67e5\u3002",
              tone = "warn"
            ),
            mod_spinner(plotly::plotlyOutput(ns("oops_violin"), height = 420))
          )
        ),
        bslib::layout_columns(
          col_widths = c(6, 6),
          mod_card(
            kicker = "F3 · Time path",
            title = "\u7b79\u8d44\u7ed3\u6784\u6f14\u5316",
            mod_v3_chart_guide(
              "\u8ffd\u8e2a\u957f\u671f\u66ff\u4ee3",
              "\u5982\u679c EXT \u4e0b\u964d\u540c\u65f6 GGHE-D \u4e0a\u5347\uff0c\u66f4\u50cf\u53ef\u6301\u7eed\u7684\u56fd\u5185\u66ff\u4ee3\uff1b\u5982\u679c OOPS \u4e0a\u5347\uff0c\u5219\u53ef\u80fd\u4f34\u968f\u5bb6\u5ead\u538b\u529b\u8f6c\u79fb\u3002"
            ),
            mod_spinner(plotly::plotlyOutput(ns("area_trend"), height = 380))
          ),
          mod_card(
            kicker = "F4 · Public buffer",
            title = "\u516c\u5171\u7b79\u8d44 / \u81ea\u4ed8\u6bd4",
            mod_v3_chart_guide(
              "\u9608\u503c\u7ebf\u662f\u76f4\u89c2\u53c2\u7167",
              "\u6bd4\u503c\u5927\u4e8e 1 \u8868\u793a\u5e73\u5747 GGHE-D \u9ad8\u4e8e OOPS\uff0c\u901a\u5e38\u662f\u8d22\u52a1\u4fdd\u62a4\u66f4\u5f3a\u7684\u4fe1\u53f7\u3002",
              tone = "good"
            ),
            mod_spinner(plotly::plotlyOutput(ns("ratio_trend"), height = 380))
          )
        ),
        mod_card(
          kicker = "F5 · Country audit",
          title = "\u56fd\u5bb6\u7b79\u8d44\u660e\u7ec6",
          mod_v3_chart_guide(
            "\u4ece\u56fe\u8868\u56de\u5230\u6570\u503c",
            "\u8868\u683c\u6309 OOPS \u964d\u5e8f\u6392\u5217\uff0c\u9002\u5408\u5feb\u901f\u5b9a\u4f4d\u5bb6\u5ead\u627f\u62c5\u538b\u529b\u8f83\u9ad8\u7684\u56fd\u5bb6\u3002"
          ),
          mod_spinner(reactable::reactableOutput(ns("fin_table"))),
          footer = "GGHE-D、PVT-D、EXT \u4e0e OOPS \u5747\u4e3a\u5360 CHE \u6bd4\u4f8b\u3002"
        )
      )
    )
  )
}


mod_financing_server <- function(id, master_r) {
  shiny::moduleServer(id, function(input, output, session) {
    
    output$kpi_strip <- shiny::renderUI({
      m <- master_r(); yr <- input$year
      d <- m[m$year == yr, ]
      n <- sum(is.finite(d$che_pc_usd2023))
      mod_v3_kpi_grid(
        mod_v3_kpi(format(n, big.mark = ","), "Countries", tone = "primary"),
        mod_v3_kpi(as.character(yr), "Year", tone = "neutral"),
        mod_v3_kpi(fmt_usd(median(d$che_pc_usd2023, na.rm = TRUE)), "Median CHE/cap", tone = "secondary"),
        mod_v3_kpi(fmt_pct(mean(d$hf3_che, na.rm = TRUE)), "Mean OOPS", tone = "warn")
      )
    })
    
    output$stacked_bar <- plotly::renderPlotly({
      m <- master_r(); yr <- input$year; grp <- input$group
      d <- m[m$year == yr & is.finite(m$gghed_che) & !is.na(m[[grp]]), ]
      agg <- stats::aggregate(cbind(gghed_che, pvtd_che, ext_che) ~ get(grp), data = d, FUN = mean)
      names(agg)[1] <- "group"
      safe_plotly({
        plotly::plot_ly(agg, x = ~group, y = ~gghed_che, type = "bar", name = "GGHE-D",
                        marker = list(color = "#1d3f5f")) |>
          plotly::add_trace(y = ~pvtd_che, name = "PVT-D", marker = list(color = "#c46327")) |>
          plotly::add_trace(y = ~ext_che, name = "EXT", marker = list(color = "#2a857a")) |>
          ghs_plotly_layout() |>
          plotly::layout(barmode = "stack", xaxis = list(title = ""), yaxis = list(title = "%% of CHE"))
      })
    })
    output$oops_violin <- plotly::renderPlotly({
      m <- master_r(); yr <- input$year; grp <- input$group
      d <- m[m$year == yr & is.finite(m$hf3_che) & !is.na(m[[grp]]), ]
      safe_plotly({
        plotly::plot_ly(d, x = stats::as.formula(paste0("~", grp)), y = ~hf3_che,
                        type = "violin", box = list(visible = TRUE),
                        meanline = list(visible = TRUE)) |>
          ghs_plotly_layout() |>
          plotly::layout(xaxis = list(title = ""), yaxis = list(title = "OOPS (%%)"))
      })
    })
    output$area_trend <- plotly::renderPlotly({
      m <- master_r()
      d <- m[is.finite(m$gghed_che) & is.finite(m$pvtd_che) & is.finite(m$ext_che), ]
      agg <- stats::aggregate(cbind(gghed_che, pvtd_che, ext_che) ~ year, data = d, FUN = mean)
      safe_plotly({
        plotly::plot_ly(agg, x = ~year) |>
          plotly::add_trace(y = ~gghed_che, name = "GGHE-D", type = "scatter", mode = "lines",
                            fill = "tozeroy", fillcolor = "rgba(29,63,95,0.3)", line = list(color = "#1d3f5f")) |>
          plotly::add_trace(y = ~pvtd_che, name = "PVT-D", type = "scatter", mode = "lines",
                            line = list(color = "#c46327", width = 2)) |>
          plotly::add_trace(y = ~ext_che, name = "EXT", type = "scatter", mode = "lines",
                            line = list(color = "#2a857a", width = 2)) |>
          ghs_plotly_layout() |>
          plotly::layout(xaxis = list(title = ""), yaxis = list(title = "%% of CHE"))
      })
    })
    output$ratio_trend <- plotly::renderPlotly({
      m <- master_r()
      d <- m[is.finite(m$gghed_che) & is.finite(m$hf3_che) & m$hf3_che > 0, ]
      agg <- stats::aggregate(cbind(gghed_che, hf3_che) ~ year, data = d, FUN = mean)
      agg$ratio <- agg$gghed_che / agg$hf3_che
      safe_plotly({
        plotly::plot_ly(agg, x = ~year, y = ~ratio, type = "scatter", mode = "lines+markers",
                        line = list(color = "#1d3f5f", width = 3)) |>
          ghs_plotly_layout() |>
          plotly::layout(xaxis = list(title = ""), yaxis = list(title = "GGHED / OOPS ratio"),
                         shapes = list(list(type = "line", x0 = 2000, x1 = 2023, y0 = 1, y1 = 1,
                                            line = list(color = "#a23b3b", dash = "dash"))))
      })
    })
    output$fin_table <- reactable::renderReactable({
      m <- master_r(); yr <- input$year
      d <- m[m$year == yr & is.finite(m$gghed_che), ]
      d <- d[order(-d$hf3_che), ]
      tab <- data.frame(Country = d$country_name, GGHED = round(d$gghed_che, 1),
                        PVTD = round(d$pvtd_che, 1), EXT = round(d$ext_che, 1),
                        OOPS = round(d$hf3_che, 1), check.names = FALSE)
      reactable::reactable(tab, defaultPageSize = 15, searchable = TRUE, highlight = TRUE,
        defaultColDef = reactable::colDef(headerStyle = list(background = "#f1f3f7")))
    })
  })
}

