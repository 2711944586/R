# =============================================================================
# 仪表盘/模块/mod_spending.R
# 支出水平：人均卫生支出的全球分布、排行与收入梯度
# =============================================================================

mod_spending_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = htmltools::HTML("&#128178; \u652f\u51fa Spending"),
    value = "spending",
    mod_v3_hero(
      kicker = "SPENDING LEVELS & PATTERNS",
      title = "\u536b\u751f\u652f\u51fa\u6c34\u5e73\u4e0e\u6a21\u5f0f",
      lead = paste(
        "\u4eba\u5747 CHE \u5728\u5168\u7403\u5448\u73b0 100 \u500d\u5dee\u8ddd\uff1a",
        "\u9ad8\u6536\u5165\u56fd\u5bb6\u8d85 $5,000\uff0c\u4f4e\u6536\u5165\u56fd\u5bb6\u4ec5 $30\u201350\u3002",
        "\u672c\u6a21\u5757\u5c55\u793a\u652f\u51fa\u6c34\u5e73\u7684\u5206\u5e03\u3001\u6392\u884c\u3001\u6536\u5165\u68af\u5ea6\u4e0e\u65f6\u5e8f\u8d8b\u52bf\u3002"
      ),
      meta = list("USD 2023 \u4e0d\u53d8\u4ef7", "195 \u56fd\u5bb6", "2000\u20132023")
    ),
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 260,
        shiny::sliderInput(ns("year"), "\u5e74\u4efd",
          min = 2000, max = 2023, value = 2023, step = 1, sep = "",
          animate = shiny::animationOptions(interval = 900)),
        shiny::selectInput(ns("color_by"), "\u7740\u8272",
          choices = c("\u5927\u6d32" = "continent",
                      "\u6536\u5165\u7ec4" = "income_group"),
          selected = "continent"),
        shiny::checkboxInput(ns("log_scale"), "\u5bf9\u6570\u5750\u6807", value = TRUE),
        shiny::tags$hr(),
        shiny::helpText(
          "\u5bf9\u6570\u5750\u6807\u66f4\u9002\u5408\u5c55\u793a\u8de8\u6536\u5165\u7ec4\u7684\u652f\u51fa\u5dee\u5f02\uff0c",
          "\u56e0\u4e3a\u652f\u51fa\u5206\u5e03\u5448\u5f3a\u70c8\u53f3\u504f\u3002"
        )
      ),
      shiny::uiOutput(ns("kpi_strip")),
      # Row 1
      bslib::layout_columns(
        col_widths = c(7, 5),
        mod_card(
          title = "\u4eba\u5747 CHE vs GDP \u6563\u70b9\u56fe",
          htmltools::p(class = "card-note",
            "\u536b\u751f\u652f\u51fa\u4e0e\u7ecf\u6d4e\u6c34\u5e73\u7684\u5f3a\u6b63\u76f8\u5173\u3002\u504f\u79bb\u62df\u5408\u7ebf\u7684\u56fd\u5bb6\u503c\u5f97\u5173\u6ce8\u3002"),
          mod_spinner(plotly::plotlyOutput(ns("che_gdp_scatter"), height = 440))
        ),
        mod_card(
          title = "\u4eba\u5747 CHE \u5206\u5e03\uff08\u6309\u7ec4\uff09",
          htmltools::p(class = "card-note",
            "\u5404\u7ec4\u7684\u652f\u51fa\u6c34\u5e73\u5206\u5e03\uff0c\u7bb1\u7ebf + \u6563\u70b9\u3002"),
          mod_spinner(plotly::plotlyOutput(ns("che_box"), height = 440))
        )
      ),
      # Row 2
      bslib::layout_columns(
        col_widths = c(6, 6),
        mod_card(
          title = "\u4eba\u5747 CHE \u6392\u884c Top 20",
          htmltools::p(class = "card-note",
            "\u5f53\u5e74\u4eba\u5747\u536b\u751f\u652f\u51fa\u6700\u9ad8\u7684 20 \u4e2a\u56fd\u5bb6\u3002"),
          mod_spinner(plotly::plotlyOutput(ns("che_top"), height = 420))
        ),
        mod_card(
          title = "\u5168\u7403\u4eba\u5747 CHE \u65f6\u5e8f\u8d8b\u52bf",
          htmltools::p(class = "card-note",
            "\u5168\u7403\u4e2d\u4f4d\u6570\u3001\u5747\u503c\u3001\u56db\u5206\u4f4d\u6570\u7684 24 \u5e74\u6f14\u5316\u3002"),
          mod_spinner(plotly::plotlyOutput(ns("che_trend"), height = 420))
        )
      ),
      # Row 3: map + table
      bslib::layout_columns(
        col_widths = c(7, 5),
        mod_card(
          title = "\u4eba\u5747 CHE \u4e16\u754c\u5730\u56fe",
          htmltools::p(class = "card-note",
            "\u989c\u8272\u8d8a\u6df1 = \u4eba\u5747\u652f\u51fa\u8d8a\u9ad8\u3002"),
          mod_spinner(leaflet::leafletOutput(ns("spending_map"), height = 400))
        ),
        mod_card(
          title = "\u652f\u51fa\u6570\u636e\u8868",
          mod_spinner(reactable::reactableOutput(ns("spending_table")))
        )
      )
    )
  )
}

mod_spending_server <- function(id, master_r) {
  shiny::moduleServer(id, function(input, output, session) {

    output$kpi_strip <- shiny::renderUI({
      m <- master_r(); yr <- input$year
      d <- m[m$year == yr & is.finite(m$che_pc_usd2023), ]
      mod_v3_kpi_grid(
        mod_v3_kpi(format(nrow(d), big.mark = ","),
                   "\u56fd\u5bb6\u6570", tone = "primary"),
        mod_v3_kpi(fmt_v3_usd(median(d$che_pc_usd2023, na.rm = TRUE)),
                   "\u4e2d\u4f4d\u6570", tone = "secondary"),
        mod_v3_kpi(fmt_v3_usd(max(d$che_pc_usd2023, na.rm = TRUE)),
                   "\u6700\u9ad8\u503c", tone = "good"),
        mod_v3_kpi(fmt_v3_usd(min(d$che_pc_usd2023, na.rm = TRUE)),
                   "\u6700\u4f4e\u503c", tone = "bad")
      )
    })

    output$che_gdp_scatter <- plotly::renderPlotly({
      m <- master_r(); yr <- input$year; grp <- input$color_by
      d <- m[m$year == yr & is.finite(m$che_pc_usd2023) &
             is.finite(m$gdp_pc_usd) & !is.na(m[[grp]]), ]
      shiny::req(nrow(d) > 5)
      pal <- if (grp == "continent") unname(brand_palette$continent)
             else unname(brand_palette$income)
      ax_type <- if (input$log_scale) "log" else "linear"
      safe_plotly({
        plotly::plot_ly(d, x = ~gdp_pc_usd, y = ~che_pc_usd2023,
                        color = stats::as.formula(paste0("~", grp)),
                        text = ~country_name, type = "scatter", mode = "markers",
                        colors = pal,
                        marker = list(size = 8, opacity = 0.7)) |>
          ghs_plotly_layout() |>
          plotly::layout(
            xaxis = list(title = "GDP per capita (USD)", type = ax_type),
            yaxis = list(title = "CHE per capita (USD)", type = ax_type),
            legend = list(orientation = "h", y = -0.15)
          )
      })
    })

    output$che_box <- plotly::renderPlotly({
      m <- master_r(); yr <- input$year; grp <- input$color_by
      d <- m[m$year == yr & is.finite(m$che_pc_usd2023) & !is.na(m[[grp]]), ]
      shiny::req(nrow(d) > 5)
      pal <- if (grp == "continent") unname(brand_palette$continent)
             else unname(brand_palette$income)
      safe_plotly({
        plotly::plot_ly(d, x = stats::as.formula(paste0("~", grp)),
                        y = ~che_pc_usd2023, type = "box",
                        color = stats::as.formula(paste0("~", grp)),
                        colors = pal, boxpoints = "outliers") |>
          ghs_plotly_layout() |>
          plotly::layout(
            showlegend = FALSE,
            xaxis = list(title = ""),
            yaxis = list(title = "CHE/cap (USD)",
                         type = if (input$log_scale) "log" else "linear")
          )
      })
    })

    output$che_top <- plotly::renderPlotly({
      m <- master_r(); yr <- input$year
      d <- m[m$year == yr & is.finite(m$che_pc_usd2023), ]
      d <- d[order(-d$che_pc_usd2023), ]
      top <- utils::head(d, 20)
      top$country_name <- factor(top$country_name, levels = rev(top$country_name))
      safe_plotly({
        plotly::plot_ly(top, x = ~che_pc_usd2023, y = ~country_name,
                        type = "bar", orientation = "h",
                        marker = list(color = "#1d3f5f")) |>
          ghs_plotly_layout() |>
          plotly::layout(
            xaxis = list(title = "CHE per capita (USD)"),
            yaxis = list(title = ""),
            margin = list(l = 120)
          )
      })
    })

    output$che_trend <- plotly::renderPlotly({
      m <- master_r()
      d <- m[is.finite(m$che_pc_usd2023), ]
      agg <- stats::aggregate(che_pc_usd2023 ~ year, data = d,
        FUN = function(x) c(med = median(x), q25 = unname(stats::quantile(x, .25)),
                            q75 = unname(stats::quantile(x, .75))))
      agg <- do.call(data.frame, agg)
      names(agg) <- c("year", "median", "q25", "q75")
      safe_plotly({
        plotly::plot_ly(agg, x = ~year) |>
          plotly::add_ribbons(ymin = ~q25, ymax = ~q75, name = "IQR",
                              fillcolor = "rgba(29,63,95,0.12)",
                              line = list(color = "transparent")) |>
          plotly::add_lines(y = ~median, name = "\u4e2d\u4f4d\u6570",
                            line = list(color = "#1d3f5f", width = 3)) |>
          ghs_plotly_layout() |>
          plotly::layout(
            xaxis = list(title = ""),
            yaxis = list(title = "CHE per capita (USD)"),
            legend = list(orientation = "h", y = -0.15)
          )
      })
    })

    output$spending_map <- leaflet::renderLeaflet({
      m <- master_r(); yr <- input$year
      if (is.null(world_sf_obj)) {
        return(leaflet::leaflet() |> leaflet::addProviderTiles("CartoDB.Positron"))
      }
      tryCatch(
        leaflet_choropleth(m, world_sf_obj, indicator_col = "che_pc_usd2023",
                           year_focus = yr,
                           title = sprintf("CHE/cap \u00b7 %d", yr)),
        error = function(e) {
          leaflet::leaflet() |> leaflet::addProviderTiles("CartoDB.Positron")
        }
      )
    })

    output$spending_table <- reactable::renderReactable({
      m <- master_r(); yr <- input$year
      d <- m[m$year == yr & is.finite(m$che_pc_usd2023), ]
      d <- d[order(-d$che_pc_usd2023), ]
      tab <- data.frame(
        "\u56fd\u5bb6" = d$country_name,
        "\u6536\u5165\u7ec4" = d$income_group,
        "CHE/cap" = round(d$che_pc_usd2023, 0),
        "GDP/cap" = round(d$gdp_pc_usd, 0),
        "CHE/GDP%" = round(d$che_pc_usd2023 / pmax(d$gdp_pc_usd, 1) * 100, 1),
        check.names = FALSE
      )
      reactable::reactable(tab, defaultPageSize = 15, searchable = TRUE,
        highlight = TRUE, striped = TRUE,
        defaultColDef = reactable::colDef(
          headerStyle = list(background = "#f1f3f7", fontWeight = 700)))
    })
  })
}
