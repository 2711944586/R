# =============================================================================
# 仪表盘/模块/mod_prevention.R
# 预防性支出：HC6 预防性护理在卫生总支出中的占比与效果
# =============================================================================

mod_prevention_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = htmltools::HTML("&#128137; \u9884\u9632 Prevention"),
    value = "prevention",
    mod_v3_hero(
      kicker = "PREVENTIVE CARE SPENDING",
      title = "\u9884\u9632\u6027\u62a4\u7406\u652f\u51fa\u4e0e\u5065\u5eb7\u56de\u62a5",
      lead = paste(
        "HC6 \u9884\u9632\u6027\u62a4\u7406\u652f\u51fa\u5360 CHE \u6bd4\u91cd\u5728\u5168\u7403\u4ec5 3\u20135%\uff0c",
        "\u4f46\u5176\u8fb9\u9645\u5065\u5eb7\u56de\u62a5\u8fdc\u9ad8\u4e8e\u6cbb\u7597\u6027\u652f\u51fa\u3002",
        "\u672c\u6a21\u5757\u5206\u6790\u9884\u9632\u652f\u51fa\u7684\u8de8\u56fd\u5dee\u5f02\u3001\u4e0e NCD \u8d1f\u62c5\u7684\u5173\u8054\u3002"
      ),
      meta = list("WHO GHED HC6", "195 \u56fd\u5bb6", "2000\u20132023")
    ),
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 260,
        shiny::sliderInput(ns("year"), "\u5e74\u4efd",
          min = 2000, max = 2023, value = 2023, step = 1, sep = "",
          animate = shiny::animationOptions(interval = 900)),
        shiny::selectInput(ns("color_by"), "\u7740\u8272\u7ef4\u5ea6",
          choices = c("\u5927\u6d32" = "continent",
                      "\u6536\u5165\u7ec4" = "income_group"),
          selected = "continent"),
        shiny::tags$hr(),
        shiny::helpText(
          "HC6 = \u9884\u9632\u6027\u62a4\u7406\u652f\u51fa\uff0c\u5305\u542b\u514d\u75ab\u3001\u7b5b\u67e5\u3001",
          "\u5065\u5eb7\u4fc3\u8fdb\u3001\u6d41\u884c\u75c5\u5b66\u76d1\u6d4b\u7b49\u3002"
        )
      ),
      shiny::uiOutput(ns("kpi_strip")),
      # Row 1
      bslib::layout_columns(
        col_widths = c(7, 5),
        mod_card(
          title = "\u9884\u9632\u652f\u51fa\u4e0e\u9884\u671f\u5bff\u547d",
          htmltools::p(class = "card-note",
            "\u6a2a\u8f74 = HC6 \u5360 CHE%\uff0c\u7eb5\u8f74 = \u9884\u671f\u5bff\u547d\u3002\u9884\u9632\u6295\u5165\u8f83\u9ad8\u7684\u56fd\u5bb6\u5bff\u547d\u504f\u9ad8\u3002"),
          mod_spinner(plotly::plotlyOutput(ns("hc6_life_scatter"), height = 440))
        ),
        mod_card(
          title = "HC6 \u5360\u6bd4\u8d8b\u52bf\uff08\u6309\u7ec4\uff09",
          htmltools::p(class = "card-note",
            "\u5404\u7ec4\u9884\u9632\u652f\u51fa\u5360\u6bd4\u7684 24 \u5e74\u6f14\u5316\u3002"),
          mod_spinner(plotly::plotlyOutput(ns("hc6_trend"), height = 440))
        )
      ),
      # Row 2
      bslib::layout_columns(
        col_widths = c(6, 6),
        mod_card(
          title = "HC6 \u5360\u6bd4\u5206\u5e03\uff08\u6700\u65b0\u5e74\uff09",
          htmltools::p(class = "card-note",
            "\u5404\u7ec4\u56fd\u5bb6\u7684\u9884\u9632\u652f\u51fa\u5360\u6bd4\u5206\u5e03\u3002"),
          mod_spinner(plotly::plotlyOutput(ns("hc6_box"), height = 380))
        ),
        mod_card(
          title = "\u9884\u9632 vs \u6cbb\u7597\u652f\u51fa\u6bd4\u4f8b",
          htmltools::p(class = "card-note",
            "HC6 (\u9884\u9632) \u4e0e HC1 (\u6cbb\u7597) \u7684\u6bd4\u503c\u3002\u6bd4\u503c\u8d8a\u9ad8 = \u9884\u9632\u5bfc\u5411\u8d8a\u5f3a\u3002"),
          mod_spinner(plotly::plotlyOutput(ns("prevention_treatment"), height = 380))
        )
      ),
      # Row 3
      bslib::layout_columns(
        col_widths = c(5, 7),
        mod_card(
          title = "HC6 Top 20 \u56fd\u5bb6",
          htmltools::p(class = "card-note",
            "\u9884\u9632\u652f\u51fa\u5360\u6bd4\u6700\u9ad8\u7684\u56fd\u5bb6\u3002"),
          mod_spinner(plotly::plotlyOutput(ns("hc6_top"), height = 440))
        ),
        mod_card(
          title = "\u9884\u9632\u652f\u51fa\u5730\u56fe",
          htmltools::p(class = "card-note",
            "HC6 \u5360 CHE \u6bd4\u91cd\u7684\u5168\u7403\u5206\u5e03\u3002"),
          mod_spinner(leaflet::leafletOutput(ns("hc6_map"), height = 440))
        )
      ),
      mod_card(
        title = "\u9884\u9632\u652f\u51fa\u8be6\u8868",
        mod_spinner(reactable::reactableOutput(ns("hc6_table")))
      )
    )
  )
}

mod_prevention_server <- function(id, master_r) {
  shiny::moduleServer(id, function(input, output, session) {

    output$kpi_strip <- shiny::renderUI({
      m <- master_r(); yr <- input$year
      d <- m[m$year == yr & is.finite(m$hc6_che), ]
      mod_v3_kpi_grid(
        mod_v3_kpi(format(nrow(d), big.mark = ","),
                   "\u56fd\u5bb6\u6570", tone = "primary"),
        mod_v3_kpi(fmt_v3_pct(median(d$hc6_che, na.rm = TRUE)),
                   "HC6 \u4e2d\u4f4d\u6570", tone = "good"),
        mod_v3_kpi(fmt_v3_pct(max(d$hc6_che, na.rm = TRUE)),
                   "HC6 \u6700\u9ad8\u503c", tone = "secondary"),
        mod_v3_kpi(as.character(yr), "\u5e74\u4efd", tone = "neutral")
      )
    })

    output$hc6_life_scatter <- plotly::renderPlotly({
      m <- master_r(); yr <- input$year
      d <- m[m$year == yr & is.finite(m$hc6_che) & is.finite(m$life_exp) &
             !is.na(m[[input$color_by]]), ]
      shiny::req(nrow(d) > 5)
      pal <- if (input$color_by == "continent") unname(brand_palette$continent)
             else unname(brand_palette$income)
      safe_plotly({
        plotly::plot_ly(d, x = ~hc6_che, y = ~life_exp,
                        color = stats::as.formula(paste0("~", input$color_by)),
                        text = ~country_name, type = "scatter", mode = "markers",
                        colors = pal,
                        marker = list(size = 9, opacity = 0.7)) |>
          ghs_plotly_layout() |>
          plotly::layout(
            xaxis = list(title = "HC6 \u9884\u9632\u652f\u51fa / CHE (%)"),
            yaxis = list(title = "\u9884\u671f\u5bff\u547d (\u5c81)"),
            legend = list(orientation = "h", y = -0.15)
          )
      })
    })

    output$hc6_trend <- plotly::renderPlotly({
      m <- master_r()
      grp <- input$color_by
      d <- m[is.finite(m$hc6_che) & !is.na(m[[grp]]), ]
      agg <- stats::aggregate(
        stats::as.formula(paste("hc6_che ~ year +", grp)),
        data = d, FUN = mean, na.rm = TRUE
      )
      pal <- if (grp == "continent") unname(brand_palette$continent)
             else unname(brand_palette$income)
      safe_plotly({
        plotly::plot_ly(agg, x = ~year, y = ~hc6_che,
                        color = stats::as.formula(paste0("~", grp)),
                        type = "scatter", mode = "lines+markers",
                        colors = pal) |>
          ghs_plotly_layout() |>
          plotly::layout(
            xaxis = list(title = ""),
            yaxis = list(title = "HC6 / CHE (%)"),
            legend = list(orientation = "h", y = -0.18)
          )
      })
    })

    output$hc6_box <- plotly::renderPlotly({
      m <- master_r(); yr <- input$year
      grp <- input$color_by
      d <- m[m$year == yr & is.finite(m$hc6_che) & !is.na(m[[grp]]), ]
      shiny::req(nrow(d) > 5)
      pal <- if (grp == "continent") unname(brand_palette$continent)
             else unname(brand_palette$income)
      safe_plotly({
        plotly::plot_ly(d, x = stats::as.formula(paste0("~", grp)),
                        y = ~hc6_che, type = "box",
                        color = stats::as.formula(paste0("~", grp)),
                        colors = pal) |>
          ghs_plotly_layout() |>
          plotly::layout(showlegend = FALSE,
                         xaxis = list(title = ""),
                         yaxis = list(title = "HC6 / CHE (%)"))
      })
    })

    output$prevention_treatment <- plotly::renderPlotly({
      m <- master_r(); yr <- input$year
      d <- m[m$year == yr & is.finite(m$hc6_che) & is.finite(m$hc1_che) &
             !is.na(m[[input$color_by]]), ]
      shiny::req(nrow(d) > 5)
      d$ratio <- d$hc6_che / pmax(d$hc1_che, 0.1)
      pal <- if (input$color_by == "continent") unname(brand_palette$continent)
             else unname(brand_palette$income)
      safe_plotly({
        plotly::plot_ly(d, x = ~hc1_che, y = ~hc6_che,
                        color = stats::as.formula(paste0("~", input$color_by)),
                        text = ~country_name, type = "scatter", mode = "markers",
                        colors = pal,
                        marker = list(size = 8, opacity = 0.7)) |>
          ghs_plotly_layout() |>
          plotly::layout(
            xaxis = list(title = "HC1 \u6cbb\u7597\u652f\u51fa / CHE (%)"),
            yaxis = list(title = "HC6 \u9884\u9632\u652f\u51fa / CHE (%)"),
            legend = list(orientation = "h", y = -0.15)
          )
      })
    })

    output$hc6_top <- plotly::renderPlotly({
      m <- master_r(); yr <- input$year
      d <- m[m$year == yr & is.finite(m$hc6_che), ]
      d <- d[order(-d$hc6_che), ]
      top <- utils::head(d, 20)
      top$country_name <- factor(top$country_name, levels = rev(top$country_name))
      safe_plotly({
        plotly::plot_ly(top, x = ~hc6_che, y = ~country_name,
                        type = "bar", orientation = "h",
                        marker = list(color = "#2a857a")) |>
          ghs_plotly_layout() |>
          plotly::layout(
            xaxis = list(title = "HC6 / CHE (%)"),
            yaxis = list(title = ""),
            margin = list(l = 120)
          )
      })
    })

    output$hc6_map <- leaflet::renderLeaflet({
      m <- master_r(); yr <- input$year
      if (is.null(world_sf_obj)) {
        return(leaflet::leaflet() |> leaflet::addProviderTiles("CartoDB.Positron"))
      }
      tryCatch(
        leaflet_choropleth(m, world_sf_obj, indicator_col = "hc6_che",
                           year_focus = yr,
                           title = sprintf("HC6 %% \u00b7 %d", yr)),
        error = function(e) {
          leaflet::leaflet() |> leaflet::addProviderTiles("CartoDB.Positron")
        }
      )
    })

    output$hc6_table <- reactable::renderReactable({
      m <- master_r(); yr <- input$year
      d <- m[m$year == yr & is.finite(m$hc6_che), ]
      d <- d[order(-d$hc6_che), ]
      tab <- data.frame(
        "\u56fd\u5bb6" = d$country_name,
        "\u5927\u6d32" = d$continent,
        "HC6%" = round(d$hc6_che, 1),
        "HC1%" = round(d$hc1_che, 1),
        "CHE/cap" = round(d$che_pc_usd2023, 0),
        "\u5bff\u547d" = round(d$life_exp, 1),
        check.names = FALSE
      )
      reactable::reactable(tab, defaultPageSize = 15, searchable = TRUE,
        highlight = TRUE, striped = TRUE,
        defaultColDef = reactable::colDef(
          headerStyle = list(background = "#f1f3f7", fontWeight = 700)))
    })
  })
}
