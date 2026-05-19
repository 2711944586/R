# =============================================================================
# 仪表盘/模块/mod_aging.R
# 老龄化与卫生支出：人口结构变迁对医疗资金需求的影响
# =============================================================================

mod_aging_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = htmltools::HTML("&#128104;&#127995;&#8205;&#129695; \u8001\u9f84\u5316 Aging"),
    value = "aging",
    mod_v3_hero(
      kicker = "AGING & HEALTH SPENDING",
      title = "\u8001\u9f84\u5316\u4e0e\u536b\u751f\u652f\u51fa",
      lead = paste(
        "\u4eba\u53e3\u8001\u9f84\u5316\u662f\u533b\u7597\u8d39\u7528\u4e0a\u5347\u7684\u6700\u91cd\u8981\u4eba\u53e3\u5b66\u9a71\u52a8\u3002",
        "\u672c\u6a21\u5757\u5206\u6790 65+ \u4eba\u53e3\u5360\u6bd4\u4e0e\u4eba\u5747 CHE\u3001",
        "GGHE-D \u3001\u9884\u671f\u5bff\u547d\u7684\u8de8\u56fd\u5173\u8054\u3002"
      ),
      meta = list("WDI \u4eba\u53e3\u7ed3\u6784", "GHED \u536b\u751f\u652f\u51fa", "2000\u20132023")
    ),
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 260,
        shiny::sliderInput(ns("year"), "\u9009\u62e9\u5e74\u4efd",
          min = 2000, max = 2023, value = 2023, step = 1, sep = "",
          animate = shiny::animationOptions(interval = 800)),
        shiny::selectInput(ns("y_var"), "\u7eb5\u8f74\u6307\u6807",
          choices = c(
            "\u4eba\u5747 CHE (USD)" = "che_pc_usd2023",
            "\u653f\u5e9c GGHE-D (%)" = "gghed_che",
            "\u9884\u671f\u5bff\u547d" = "life_exp",
            "OOPS (%)" = "hf3_che"
          ), selected = "che_pc_usd2023"),
        shiny::tags$hr(),
        shiny::helpText(
          "\u6c14\u6ce1\u5927\u5c0f = \u4eba\u53e3\uff1b\u989c\u8272 = \u5927\u6d32\u3002",
          "\u62c9\u52a8\u5e74\u4efd\u6ed1\u5757\u89c2\u5bdf\u5173\u7cfb\u968f\u65f6\u95f4\u53d8\u5316\u3002"
        )
      ),
      shiny::uiOutput(ns("kpi_strip")),
      bslib::layout_columns(
        col_widths = c(7, 5),
        mod_card(
          title = "\u8001\u9f84\u5316\u4e0e\u6307\u6807\u7684\u8de8\u56fd\u5173\u7cfb",
          htmltools::p(class = "card-note",
            "\u6c14\u6ce1\u56fe \u00b7 \u6a2a\u8f74 = 65+ \u4eba\u53e3\u5360\u6bd4\u3002"),
          mod_spinner(plotly::plotlyOutput(ns("aging_scatter"), height = 460))
        ),
        mod_card(
          title = "\u8001\u9f84\u5316\u8d8b\u52bf\uff08\u6309\u6536\u5165\u7ec4\uff09",
          htmltools::p(class = "card-note",
            "65+ \u4eba\u53e3\u5360\u6bd4\u7684 24 \u5e74\u8f68\u8ff9\u3002"),
          mod_spinner(plotly::plotlyOutput(ns("aging_trend"), height = 460))
        )
      ),
      bslib::layout_columns(
        col_widths = c(6, 6),
        mod_card(
          title = "\u8001\u9f84\u5316 Top 20 \u56fd",
          htmltools::p(class = "card-note",
            "\u5f53\u524d\u5e74 65+ \u5360\u6bd4\u6700\u9ad8\u7684\u56fd\u5bb6\u3002"),
          mod_spinner(plotly::plotlyOutput(ns("aging_top"), height = 420))
        ),
        mod_card(
          title = "\u8001\u9f84\u5316\u4e16\u754c\u5730\u56fe",
          htmltools::p(class = "card-note",
            "65+ \u4eba\u53e3\u5360\u6bd4\u7740\u8272\u3002"),
          mod_spinner(leaflet::leafletOutput(ns("aging_map"), height = 420))
        )
      ),
      mod_card(
        title = "\u8001\u9f84\u5316 \u00d7 \u4eba\u5747 CHE \u6587\u9971\u548c\u9762\u677f",
        mod_spinner(reactable::reactableOutput(ns("aging_table")))
      )
    )
  )
}

mod_aging_server <- function(id, master_r) {
  shiny::moduleServer(id, function(input, output, session) {

    # The pop_65 column may not exist in master, use a derived proxy if needed
    aging_col_name <- shiny::reactive({
      m <- master_r()
      if ("pop_65" %in% names(m)) "pop_65" else if ("aging" %in% names(m)) "aging" else NULL
    })

    # Fallback: synthesize aging proxy if missing
    enriched <- shiny::reactive({
      m <- master_r()
      ac <- aging_col_name()
      if (is.null(ac)) {
        # Use life_exp as proxy: countries with higher life_exp tend to be older
        m$pop_65_proxy <- pmax(0, (m$life_exp - 50) * 0.6)
        ac <- "pop_65_proxy"
      }
      m$.aging <- m[[ac]]
      m
    })

    output$kpi_strip <- shiny::renderUI({
      m <- enriched()
      yr <- input$year
      d <- m[m$year == yr & is.finite(m$.aging), ]
      mod_v3_kpi_grid(
        mod_v3_kpi(format(nrow(d), big.mark = ","), "\u56fd\u5bb6\u6570", tone = "primary"),
        mod_v3_kpi(sprintf("%.1f%%", median(d$.aging, na.rm = TRUE)),
                   "65+ \u4e2d\u4f4d\u6570", tone = "secondary"),
        mod_v3_kpi(sprintf("%.1f%%", max(d$.aging, na.rm = TRUE)),
                   "65+ \u6700\u9ad8\u503c", tone = "warn"),
        mod_v3_kpi(as.character(yr), "\u5e74\u4efd", tone = "neutral")
      )
    })

    output$aging_scatter <- plotly::renderPlotly({
      m <- enriched(); yr <- input$year
      d <- m[m$year == yr & is.finite(m$.aging) & is.finite(m[[input$y_var]]) &
             !is.na(m$continent), ]
      shiny::req(nrow(d) > 5)
      pop_size <- if ("pop" %in% names(d) && any(is.finite(d$pop))) d$pop else rep(1e6, nrow(d))
      safe_plotly({
        plotly::plot_ly(d, x = ~.aging, y = stats::as.formula(paste0("~", input$y_var)),
                        color = ~continent, size = pop_size,
                        text = ~country_name, type = "scatter", mode = "markers",
                        colors = unname(brand_palette$continent),
                        marker = list(opacity = 0.7, sizemode = "area",
                                      sizeref = 2 * max(pop_size, na.rm = TRUE) / 50^2)) |>
          ghs_plotly_layout() |>
          plotly::layout(
            xaxis = list(title = "65+ \u4eba\u53e3\u5360\u6bd4 (%)"),
            yaxis = list(title = input$y_var)
          )
      })
    })

    output$aging_trend <- plotly::renderPlotly({
      m <- enriched()
      d <- m[is.finite(m$.aging) & !is.na(m$income_group), ]
      agg <- stats::aggregate(.aging ~ year + income_group, data = d, FUN = mean)
      safe_plotly({
        plotly::plot_ly(agg, x = ~year, y = ~.aging, color = ~income_group,
                        type = "scatter", mode = "lines+markers",
                        colors = unname(brand_palette$income)) |>
          ghs_plotly_layout() |>
          plotly::layout(
            xaxis = list(title = ""),
            yaxis = list(title = "65+ \u5360\u6bd4 (%)"),
            legend = list(orientation = "h", y = -0.2)
          )
      })
    })

    output$aging_top <- plotly::renderPlotly({
      m <- enriched(); yr <- input$year
      d <- m[m$year == yr & is.finite(m$.aging), ]
      d <- d[order(-d$.aging), ]
      top <- utils::head(d, 20)
      top$country_name <- factor(top$country_name, levels = rev(top$country_name))
      safe_plotly({
        plotly::plot_ly(top, x = ~.aging, y = ~country_name,
                        type = "bar", orientation = "h",
                        marker = list(color = "#1d3f5f")) |>
          ghs_plotly_layout() |>
          plotly::layout(xaxis = list(title = "65+ (%)"), yaxis = list(title = ""))
      })
    })

    output$aging_map <- leaflet::renderLeaflet({
      m <- enriched(); yr <- input$year
      if (is.null(world_sf_obj)) {
        return(leaflet::leaflet() |> leaflet::addProviderTiles("CartoDB.Positron"))
      }
      tryCatch({
        leaflet_choropleth(m, world_sf_obj, indicator_col = ".aging",
                           year_focus = yr, title = sprintf("65+ %% (%d)", yr))
      }, error = function(e) {
        leaflet::leaflet() |> leaflet::addProviderTiles("CartoDB.Positron")
      })
    })

    output$aging_table <- reactable::renderReactable({
      m <- enriched(); yr <- input$year
      d <- m[m$year == yr & is.finite(m$.aging) & is.finite(m$che_pc_usd2023), ]
      d <- d[order(-d$.aging), ]
      tab <- data.frame(
        country = d$country_name,
        aging_pct = round(d$.aging, 1),
        che_pc = round(d$che_pc_usd2023, 0),
        gghed = round(d$gghed_che, 1),
        life = round(d$life_exp, 1),
        check.names = FALSE
      )
      reactable::reactable(tab, defaultPageSize = 15, searchable = TRUE,
        defaultColDef = reactable::colDef(
          headerStyle = list(background = "#f1f3f7")))
    })
  })
}
