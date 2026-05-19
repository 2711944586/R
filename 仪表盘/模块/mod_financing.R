# =============================================================================
# 仪表盘/模块/mod_financing.R
# FINANCING STRUCTURE
# =============================================================================

mod_financing_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = htmltools::HTML("&#128176; Financing"),
    value = "financing",
    mod_v3_hero(
      kicker = "FINANCING STRUCTURE",
      title = "FINANCING STRUCTURE",
      lead = "HF1-HF4 筹资方案的全球分布、时序演化与收入组差异。政府强制 vs 自愿 vs 自付 vs 境外。",
      meta = list("WHO GHED 2024-12", "195 countries", "2000-2023")
    ),
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 260,
        shiny::sliderInput(ns("year"), "Year",
          min = 2000, max = 2023, value = 2023, step = 1, sep = ""),
        shiny::selectInput(ns("group"), "Group by",
          choices = c("Continent" = "continent", "Income" = "income_group"), selected = "continent")
      ),
      shiny::uiOutput(ns("kpi_strip")),
      bslib::layout_columns(
        col_widths = c(6, 6),
        mod_card(title = "Financing structure by group",
          htmltools::p(class = "card-note", "GGHE-D + PVT-D + EXT 三源占比。"),
          mod_spinner(plotly::plotlyOutput(ns("stacked_bar"), height = 420))),
        mod_card(title = "OOPS distribution",
          htmltools::p(class = "card-note", "居民自付占比的分组分布。"),
          mod_spinner(plotly::plotlyOutput(ns("oops_violin"), height = 420)))
      ),
      bslib::layout_columns(
        col_widths = c(6, 6),
        mod_card(title = "Structure evolution",
          mod_spinner(plotly::plotlyOutput(ns("area_trend"), height = 380))),
        mod_card(title = "Public vs Private ratio",
          mod_spinner(plotly::plotlyOutput(ns("ratio_trend"), height = 380)))
      ),
      mod_card(title = "Country detail",
        mod_spinner(reactable::reactableOutput(ns("fin_table"))))
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

