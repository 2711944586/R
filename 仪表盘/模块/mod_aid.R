
mod_aid_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = "\u5916\u63f4 Aid",
    value = "aid",
    mod_v3_hero(
      kicker = "\u5916\u90e8\u536b\u751f\u652f\u51fa",
      title = "\u5916\u90e8\u63f4\u52a9\u4e0e\u536b\u751f\u7b79\u8d44\u4f9d\u8d56",
      lead = paste(
        "\u5916\u90e8\u536b\u751f\u63f4\u52a9 (EXT) \u5728\u4f4e\u6536\u5165\u56fd\u5bb6\u5360 CHE \u6bd4\u91cd\u53ef\u8fbe 30\u2013",
        "50%\u3002\u672c\u6a21\u5757\u5206\u6790\u63f4\u52a9\u4f9d\u8d56\u5ea6\u7684\u5730\u7406\u5206\u5e03\u3001\u65f6\u5e8f\u6f14\u5316\u3001",
        "\u4e0e\u5065\u5eb7\u4ea7\u51fa\u7684\u5173\u8054\uff0c\u4ee5\u53ca\u63f4\u52a9\u9000\u51fa\u540e\u7684\u8d22\u52a1\u53ef\u6301\u7eed\u6027\u98ce\u9669\u3002"
      ),
      meta = list("WHO GHED 2024-12", "WDI", "195 \u56fd\u5bb6 \u00b7 2000\u20132023")
    ),
    mod_v3_page_body(
      wide = TRUE,
      mod_v3_topic_brief("aid"),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 280,
          shiny::sliderInput(ns("year"), "\u5e74\u4efd",
            min = 2000, max = 2023, value = 2023, step = 1, sep = "",
            animate = shiny::animationOptions(interval = 900)),
          shiny::selectInput(ns("region_filter"), "\u5927\u6d32\u7b5b\u9009",
            choices = c("\u5168\u90e8" = "all", "Africa", "Asia", "Americas",
                        "Europe", "Oceania"),
            selected = "all"),
          shiny::sliderInput(ns("ext_threshold"), "\u9ad8\u4f9d\u8d56\u9608\u503c (%)",
            min = 5, max = 50, value = 20, step = 5),
          mod_v3_sidebar_note(
            "\u5916\u63f4\u53e3\u5f84",
            "EXT = External Health Expenditure\uff0c\u5305\u542b\u53cc\u8fb9\u63f4\u52a9\u3001\u591a\u8fb9\u673a\u6784\u8d44\u91d1\u548c NGO \u8f6c\u79fb\u7b49\u5916\u90e8\u6765\u6e90\u3002",
            bullets = c(
              "\u9608\u503c\u7528\u4e8e\u5b9a\u4e49\u9ad8\u4f9d\u8d56\u56fd\u5bb6\uff0c\u4e0d\u662f\u7edf\u4e00\u653f\u7b56\u8b66\u6212\u7ebf\u3002",
              "\u770b\u5916\u63f4\u4f9d\u8d56\u65f6\u9700\u540c\u65f6\u68c0\u67e5 GGHE-D \u662f\u5426\u6709\u8db3\u591f\u66ff\u4ee3\u80fd\u529b\u3002",
              "\u533a\u57df\u7b5b\u9009\u4ec5\u5f71\u54cd\u4e3b\u8981\u56fe\u8868\u548c\u8868\u683c\uff0c\u5168\u7403\u8d8b\u52bf\u4fdd\u6301\u5b8c\u6574\u6837\u672c\u3002"
            )
          )
        ),
        shiny::uiOutput(ns("kpi_strip")),
      bslib::layout_columns(
        col_widths = c(7, 5),
        mod_card(
          kicker = "F1 \u00b7 Aid dependence",
          title = "\u63f4\u52a9\u4f9d\u8d56\u4e0e\u5065\u5eb7\u4ea7\u51fa",
          htmltools::p(class = "card-note",
            "\u6a2a\u8f74 = EXT \u5360 CHE \u6bd4\u91cd\uff0c\u7eb5\u8f74 = \u9884\u671f\u5bff\u547d\u3002\u6c14\u6ce1\u5927\u5c0f = \u4eba\u53e3\u3002"),
          mod_v3_chart_guide(
            "\u9ad8 EXT \u4e0d\u4e00\u5b9a\u5e26\u6765\u9ad8\u4ea7\u51fa",
            "\u5982\u679c\u5916\u63f4\u5360\u6bd4\u9ad8\u4f46\u5bff\u547d\u4ecd\u7136\u504f\u4f4e\uff0c\u53ef\u80fd\u8bf4\u660e\u63f4\u52a9\u6d41\u5411\u66f4\u591a\u662f\u8865\u7f3a\u53e3\uff0c\u800c\u4e0d\u662f\u5df2\u8f6c\u5316\u4e3a\u7a33\u5b9a\u7684\u57fa\u5c42\u670d\u52a1\u80fd\u529b\u3002",
            tone = "warn"
          ),
          mod_spinner(plotly::plotlyOutput(ns("scatter_ext_life"), height = 450))
        ),
        mod_card(
          kicker = "F2 \u00b7 Long-run exit",
          title = "\u5168\u7403 EXT \u5360\u6bd4\u8d8b\u52bf\uff08\u6309\u6536\u5165\u7ec4\uff09",
          htmltools::p(class = "card-note",
            "\u4f4e\u6536\u5165\u56fd\u5bb6\u7684\u63f4\u52a9\u4f9d\u8d56\u5728 2010 \u5e74\u540e\u6301\u7eed\u4e0b\u964d\u3002"),
          mod_v3_chart_guide(
            "\u770b\u9000\u51fa\u8f68\u8ff9\u662f\u5426\u5e73\u6ed1",
            "EXT \u4e0b\u964d\u672c\u8eab\u4e0d\u7b49\u4e8e\u98ce\u9669\u4e0b\u964d\uff1b\u53ea\u6709\u5728\u516c\u5171\u7b79\u8d44\u80fd\u63a5\u4f4f\u7f3a\u53e3\u65f6\uff0c\u9000\u51fa\u624d\u66f4\u50cf\u53ef\u6301\u7eed\u8f6c\u578b\u3002"
          ),
          mod_spinner(plotly::plotlyOutput(ns("trend_by_income"), height = 450))
        )
      ),
      bslib::layout_columns(
        col_widths = c(5, 7),
        mod_card(
          kicker = "F3 \u00b7 Top tail",
          title = "\u63f4\u52a9\u4f9d\u8d56\u5ea6 Top 20",
          htmltools::p(class = "card-note",
            "\u5f53\u5e74 EXT \u5360 CHE \u6bd4\u91cd\u6700\u9ad8\u7684\u56fd\u5bb6\u3002"),
          mod_v3_chart_guide(
            "\u6392\u884c\u7528\u4e8e\u5b9a\u4f4d\u5c3e\u90e8\u98ce\u9669",
            "\u8fd9\u4e9b\u56fd\u5bb6\u5728\u63f4\u52a9\u53d8\u52a8\u65f6\u66f4\u5bb9\u6613\u51fa\u73b0\u670d\u52a1\u7f3a\u53e3\uff0c\u5efa\u8bae\u540c\u65f6\u68c0\u67e5\u8d22\u653f\u548c\u653f\u7b56\u6a21\u5757\u3002",
            tone = "bad"
          ),
          mod_spinner(plotly::plotlyOutput(ns("top_dependent"), height = 480))
        ),
        mod_card(
          kicker = "F4 \u00b7 Spatial concentration",
          title = "\u63f4\u52a9\u4f9d\u8d56\u5730\u7406\u5206\u5e03",
          htmltools::p(class = "card-note",
            "\u989c\u8272\u8d8a\u6df1 = EXT \u5360\u6bd4\u8d8a\u9ad8\u3002\u6492\u54c8\u62c9\u4ee5\u5357\u975e\u6d32\u4e3a\u4e3b\u8981\u53d7\u63f4\u533a\u3002"),
          mod_v3_chart_guide(
            "\u5730\u56fe\u663e\u793a\u533a\u57df\u6027\u4f9d\u8d56",
            "\u76f8\u90bb\u56fd\u5bb6\u540c\u65f6\u9ad8 EXT \u65f6\uff0c\u9700\u5173\u6ce8\u63f4\u52a9\u8d44\u91d1\u7684\u533a\u57df\u6027\u5468\u671f\u548c\u5916\u90e8\u9879\u76ee\u9000\u51fa\u7684\u540c\u6b65\u98ce\u9669\u3002"
          ),
          mod_spinner(leaflet::leafletOutput(ns("aid_map"), height = 480))
        )
      ),
      bslib::layout_columns(
        col_widths = c(6, 6),
        mod_card(
          kicker = "F5 \u00b7 Replacement capacity",
          title = "\u63f4\u52a9\u9000\u51fa\u98ce\u9669\uff1a\u9ad8\u4f9d\u8d56\u56fd\u7684 GGHED \u66ff\u4ee3\u80fd\u529b",
          htmltools::p(class = "card-note",
            "\u6a2a\u8f74 = EXT%\uff0c\u7eb5\u8f74 = GGHED%\u3002\u53f3\u4e0b\u8c61\u9650 = \u9ad8\u4f9d\u8d56\u4f4e\u81ea\u4e3b\u3002"),
          mod_v3_chart_guide(
            "\u53f3\u4e0b\u8c61\u9650\u662f\u4e3b\u8981\u9884\u8b66\u533a",
            "\u5916\u63f4\u9ad8\u3001\u653f\u5e9c\u5360\u6bd4\u4f4e\u7684\u56fd\u5bb6\u4e00\u65e6\u63f4\u52a9\u6536\u7f29\uff0c\u66f4\u53ef\u80fd\u5c06\u6210\u672c\u8f6c\u79fb\u7ed9\u5bb6\u5ead\u6216\u538b\u7f29\u670d\u52a1\u4f9b\u7ed9\u3002",
            tone = "warn"
          ),
          mod_spinner(plotly::plotlyOutput(ns("sustainability_scatter"), height = 380))
        ),
        mod_card(
          kicker = "F6 \u00b7 Regional structure",
          title = "\u63f4\u52a9\u6d41\u5411\u7ed3\u6784\uff08\u6309\u5927\u6d32\uff09",
          htmltools::p(class = "card-note",
            "\u5404\u5927\u6d32\u63a5\u53d7\u7684 EXT \u603b\u91cf\u5360\u6bd4\u3002"),
          mod_v3_chart_guide(
            "\u7ec4\u95f4\u6bd4\u8f83\u4f7f\u7528\u5e73\u5747\u5360\u6bd4",
            "\u56fe\u8868\u5c55\u793a\u7684\u662f\u56fd\u5bb6\u5c42\u9762 EXT/CHE \u5747\u503c\uff0c\u66f4\u9002\u5408\u8bfb\u4f9d\u8d56\u5ea6\uff0c\u4e0d\u4ee3\u8868\u63f4\u52a9\u8d44\u91d1\u7edd\u5bf9\u89c4\u6a21\u3002",
            tone = "good"
          ),
          mod_spinner(plotly::plotlyOutput(ns("aid_by_continent"), height = 380))
        )
      ),
      mod_card(
        kicker = "F7 \u00b7 \u56fd\u5bb6\u660e\u7ec6",
        title = "\u63f4\u52a9\u4f9d\u8d56\u56fd\u5bb6\u8be6\u8868",
        htmltools::p(class = "card-note",
          "\u7b5b\u9009\u6761\u4ef6\uff1aEXT \u5360\u6bd4\u8d85\u8fc7\u5de6\u4fa7\u9608\u503c\u7684\u56fd\u5bb6\u3002"),
        mod_v3_chart_guide(
          "\u8868\u683c\u7528\u4e8e\u590d\u6838\u9608\u503c\u6837\u672c",
          "\u70b9\u51fb\u6392\u5e8f\u53ef\u5bf9\u6bd4 EXT\u3001GGHE-D\u3001CHE/cap \u548c\u5bff\u547d\uff0c\u5224\u65ad\u9ad8\u4f9d\u8d56\u662f\u6682\u65f6\u6027\u63f4\u52a9\u6ce2\u52a8\u8fd8\u662f\u7ed3\u6784\u6027\u8d22\u653f\u7f3a\u53e3\u3002"
        ),
        mod_spinner(reactable::reactableOutput(ns("aid_table"))),
        footer = "\u9608\u503c\u7531\u4fa7\u680f\u63a7\u5236\uff1b\u53ea\u5c55\u793a\u5f53\u5e74\u8d85\u8fc7\u9608\u503c\u4e14 EXT \u6709\u6548\u7684\u56fd\u5bb6\u3002"
      )
      )
    )
  )
}

mod_aid_server <- function(id, master_r) {
  shiny::moduleServer(id, function(input, output, session) {

    filtered <- shiny::reactive({
      m <- master_r()
      if (input$region_filter != "all") {
        m <- m[m$continent == input$region_filter, , drop = FALSE]
      }
      m
    })

    output$kpi_strip <- shiny::renderUI({
      m <- filtered(); yr <- input$year
      d <- m[m$year == yr & is.finite(m$ext_che), ]
      high_dep <- sum(d$ext_che > input$ext_threshold, na.rm = TRUE)
      mod_v3_kpi_grid(
        mod_v3_kpi(format(nrow(d), big.mark = ","),
                   "\u56fd\u5bb6\u6570", tone = "primary"),
        mod_v3_kpi(fmt_v3_pct(median(d$ext_che, na.rm = TRUE)),
                   "EXT \u4e2d\u4f4d\u6570", tone = "secondary"),
        mod_v3_kpi(as.character(high_dep),
                   "\u9ad8\u4f9d\u8d56\u56fd", tone = "warn"),
        mod_v3_kpi(fmt_v3_pct(max(d$ext_che, na.rm = TRUE)),
                   "EXT \u6700\u9ad8\u503c", tone = "bad")
      )
    })

    output$scatter_ext_life <- plotly::renderPlotly({
      m <- filtered(); yr <- input$year
      d <- m[m$year == yr & is.finite(m$ext_che) & is.finite(m$life_exp) &
             !is.na(m$continent), ]
      shiny::req(nrow(d) > 5)
      pop_col <- if ("pop" %in% names(d) && any(is.finite(d$pop))) d$pop
                 else rep(1e6, nrow(d))
      safe_plotly({
        plotly::plot_ly(d, x = ~ext_che, y = ~life_exp,
                        color = ~continent, size = pop_col,
                        text = ~country_name, type = "scatter", mode = "markers",
                        colors = unname(brand_palette$continent),
                        marker = list(opacity = 0.7, sizemode = "area",
                                      sizeref = 2 * max(pop_col, na.rm = TRUE) / 45^2)) |>
          ghs_plotly_layout() |>
          plotly::layout(
            xaxis = list(title = "EXT \u5360 CHE (%)"),
            yaxis = list(title = "\u9884\u671f\u5bff\u547d (\u5c81)"),
            legend = list(orientation = "h", y = -0.15)
          )
      })
    })

    output$trend_by_income <- plotly::renderPlotly({
      m <- master_r()
      d <- m[is.finite(m$ext_che) & !is.na(m$income_group), ]
      agg <- stats::aggregate(ext_che ~ year + income_group, data = d, FUN = mean)
      safe_plotly({
        plotly::plot_ly(agg, x = ~year, y = ~ext_che, color = ~income_group,
                        type = "scatter", mode = "lines+markers",
                        colors = unname(brand_palette$income)) |>
          ghs_plotly_layout() |>
          plotly::layout(
            xaxis = list(title = ""),
            yaxis = list(title = "EXT \u5360 CHE \u5747\u503c (%)"),
            legend = list(orientation = "h", y = -0.18)
          )
      })
    })

    output$top_dependent <- plotly::renderPlotly({
      m <- filtered(); yr <- input$year
      d <- m[m$year == yr & is.finite(m$ext_che), ]
      d <- d[order(-d$ext_che), ]
      top <- utils::head(d, 20)
      top$country_name <- factor(top$country_name, levels = rev(top$country_name))
      safe_plotly({
        plotly::plot_ly(top, x = ~ext_che, y = ~country_name,
                        type = "bar", orientation = "h",
                        marker = list(color = "#2a857a"),
                        text = ~sprintf("%.1f%%", ext_che),
                        textposition = "outside") |>
          ghs_plotly_layout() |>
          plotly::layout(
            xaxis = list(title = "EXT \u5360 CHE (%)"),
            yaxis = list(title = ""),
            margin = list(l = 120)
          )
      })
    })

    output$aid_map <- leaflet::renderLeaflet({
      m <- master_r(); yr <- input$year
      if (is.null(world_sf_obj)) {
        return(leaflet::leaflet() |> leaflet::addProviderTiles("CartoDB.Positron"))
      }
      tryCatch(
        leaflet_choropleth(m, world_sf_obj, indicator_col = "ext_che",
                           year_focus = yr,
                           title = sprintf("EXT %% \u00b7 %d", yr)),
        error = function(e) {
          leaflet::leaflet() |> leaflet::addProviderTiles("CartoDB.Positron")
        }
      )
    })

    output$sustainability_scatter <- plotly::renderPlotly({
      m <- filtered(); yr <- input$year
      d <- m[m$year == yr & is.finite(m$ext_che) & is.finite(m$gghed_che) &
             !is.na(m$income_group), ]
      shiny::req(nrow(d) > 5)
      safe_plotly({
        plotly::plot_ly(d, x = ~ext_che, y = ~gghed_che,
                        color = ~income_group, text = ~country_name,
                        type = "scatter", mode = "markers",
                        colors = unname(brand_palette$income),
                        marker = list(size = 9, opacity = 0.75)) |>
          ghs_plotly_layout() |>
          plotly::layout(
            xaxis = list(title = "EXT \u5360 CHE (%)"),
            yaxis = list(title = "GGHE-D \u5360 CHE (%)"),
            shapes = list(
              list(type = "line", x0 = input$ext_threshold,
                   x1 = input$ext_threshold, y0 = 0, y1 = 100,
                   line = list(color = "#a23b3b", width = 1.5, dash = "dash"))
            ),
            legend = list(orientation = "h", y = -0.18)
          )
      })
    })

    output$aid_by_continent <- plotly::renderPlotly({
      m <- master_r(); yr <- input$year
      d <- m[m$year == yr & is.finite(m$ext_che) & !is.na(m$continent), ]
      agg <- stats::aggregate(ext_che ~ continent, data = d, FUN = mean)
      agg <- agg[order(-agg$ext_che), ]
      safe_plotly({
        plotly::plot_ly(agg, x = ~reorder(continent, ext_che), y = ~ext_che,
                        type = "bar",
                        marker = list(color = unname(brand_palette$continent[agg$continent]))) |>
          ghs_plotly_layout() |>
          plotly::layout(
            xaxis = list(title = ""),
            yaxis = list(title = "EXT \u5360 CHE \u5747\u503c (%)")
          )
      })
    })

    output$aid_table <- reactable::renderReactable({
      m <- filtered(); yr <- input$year
      d <- m[m$year == yr & is.finite(m$ext_che) &
             m$ext_che > input$ext_threshold, ]
      d <- d[order(-d$ext_che), ]
      tab <- data.frame(
        "\u56fd\u5bb6" = d$country_name,
        "\u5927\u6d32" = d$continent,
        "EXT%" = round(d$ext_che, 1),
        "GGHED%" = round(d$gghed_che, 1),
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
