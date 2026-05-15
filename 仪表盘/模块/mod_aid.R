# =============================================================================
# 仪表盘/模块/mod_aid.R
# EXTERNAL AID
# =============================================================================

mod_aid_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = htmltools::HTML("&#127760; Aid"),
    mod_v3_hero(
      kicker = "外援与依赖",
      title = "EXTERNAL AID",
      lead = "外部援助卫生支出的地理分布、趋势与可持续性分析。",
      meta = list("WHO GHED 2024-12", "195 countries", "2000-2023")
    ),
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 260,
        shiny::sliderInput(ns("year"), "Year",
          min = 2000, max = 2023, value = 2023, step = 1, sep = "")
      ),
      shiny::uiOutput(ns("kpi_strip")),
      bslib::layout_columns(
        col_widths = c(7, 5),
        mod_card(title = "Main visualization",
          htmltools::p(class = "card-note", "Primary analytical view."),
          mod_spinner(plotly::plotlyOutput(ns("main_plot"), height = 460))),
        mod_card(title = "Distribution",
          htmltools::p(class = "card-note", "Cross-sectional distribution."),
          mod_spinner(plotly::plotlyOutput(ns("dist_plot"), height = 460)))
      ),
      bslib::layout_columns(
        col_widths = c(6, 6),
        mod_card(title = "Trend",
          mod_spinner(plotly::plotlyOutput(ns("trend_plot"), height = 380))),
        mod_card(title = "Comparison",
          mod_spinner(plotly::plotlyOutput(ns("compare_plot"), height = 380)))
      ),
      mod_card(title = "Data table",
        mod_spinner(reactable::reactableOutput(ns("data_table"))))
    )
  )
}


mod_aid_server <- function(id, master_r) {
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
    
    output$main_plot <- plotly::renderPlotly({
      m <- master_r(); yr <- input$year
      d <- m[m$year == yr & is.finite(m$che_pc_usd2023) & !is.na(m$continent), ]
      shiny::req(nrow(d) > 5)
      safe_plotly({
        plotly::plot_ly(d, x = ~che_pc_usd2023, y = ~life_exp, color = ~continent,
                        text = ~country_name, type = "scatter", mode = "markers",
                        colors = unname(brand_palette$continent),
                        marker = list(size = 8, opacity = 0.7)) |>
          ghs_plotly_layout() |>
          plotly::layout(xaxis = list(title = "CHE per capita (USD)", type = "log"),
                         yaxis = list(title = "Life expectancy"))
      })
    })
    output$dist_plot <- plotly::renderPlotly({
      m <- master_r(); yr <- input$year
      d <- m[m$year == yr & is.finite(m$che_pc_usd2023) & !is.na(m$income_group), ]
      safe_plotly({
        plotly::plot_ly(d, x = ~income_group, y = ~che_pc_usd2023, type = "box",
                        color = ~income_group, colors = unname(brand_palette$income)) |>
          ghs_plotly_layout() |>
          plotly::layout(showlegend = FALSE, yaxis = list(type = "log", title = "CHE/cap (USD)"))
      })
    })
    output$trend_plot <- plotly::renderPlotly({
      m <- master_r()
      agg <- stats::aggregate(che_pc_usd2023 ~ year, data = m[is.finite(m$che_pc_usd2023),], FUN = median)
      safe_plotly({
        plotly::plot_ly(agg, x = ~year, y = ~che_pc_usd2023, type = "scatter", mode = "lines+markers",
                        line = list(color = "#1d3f5f", width = 3)) |>
          ghs_plotly_layout() |>
          plotly::layout(xaxis = list(title = ""), yaxis = list(title = "Median CHE/cap"))
      })
    })
    output$compare_plot <- plotly::renderPlotly({
      m <- master_r(); yr <- input$year
      d <- m[m$year == yr & is.finite(m$gghed_che) & !is.na(m$continent), ]
      agg <- stats::aggregate(gghed_che ~ continent, data = d, FUN = mean)
      agg <- agg[order(-agg$gghed_che), ]
      safe_plotly({
        plotly::plot_ly(agg, x = ~gghed_che, y = ~reorder(continent, gghed_che),
                        type = "bar", orientation = "h", marker = list(color = "#1d3f5f")) |>
          ghs_plotly_layout() |>
          plotly::layout(xaxis = list(title = "GGHE-D (%%)"), yaxis = list(title = ""))
      })
    })
    output$data_table <- reactable::renderReactable({
      m <- master_r(); yr <- input$year
      d <- m[m$year == yr & is.finite(m$che_pc_usd2023), ]
      d <- d[order(-d$che_pc_usd2023), ]
      tab <- data.frame(Country = d$country_name, CHE_pc = round(d$che_pc_usd2023),
                        OOPS = round(d$hf3_che, 1), GGHED = round(d$gghed_che, 1),
                        Life = round(d$life_exp, 1), check.names = FALSE)
      reactable::reactable(tab, defaultPageSize = 12, searchable = TRUE, highlight = TRUE,
        defaultColDef = reactable::colDef(headerStyle = list(background = "#f1f3f7")))
    })
  })
}

