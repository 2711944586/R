
mod_aging_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = "\u8001\u9f84\u5316 Aging",
    value = "aging",
    mod_v3_hero(
      kicker = "\u8001\u9f84\u5316\u4e0e\u536b\u751f\u652f\u51fa",
      title = "\u8001\u9f84\u5316\u4e0e\u536b\u751f\u652f\u51fa",
      lead = paste(
        "\u4eba\u53e3\u8001\u9f84\u5316\u662f\u533b\u7597\u8d39\u7528\u4e0a\u5347\u7684\u6700\u91cd\u8981\u4eba\u53e3\u5b66\u9a71\u52a8\u3002",
        "\u672c\u6a21\u5757\u5206\u6790 65+ \u4eba\u53e3\u5360\u6bd4\u4e0e\u4eba\u5747 CHE\u3001",
        "GGHE-D \u3001\u9884\u671f\u5bff\u547d\u7684\u8de8\u56fd\u5173\u8054\u3002"
      ),
      meta = list("WDI \u4eba\u53e3\u7ed3\u6784", "GHED \u536b\u751f\u652f\u51fa", "2000\u20132023")
    ),
    mod_v3_page_body(
      wide = TRUE,
      mod_v3_topic_brief("aging"),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 280,
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
          mod_v3_sidebar_note(
            "\u4eba\u53e3\u7ed3\u6784\u8bfb\u6cd5",
            "\u6c14\u6ce1\u5927\u5c0f\u4ee3\u8868\u4eba\u53e3\uff0c\u989c\u8272\u4ee3\u8868\u5927\u6d32\u3002\u5efa\u8bae\u5148\u56fa\u5b9a\u5e74\u4efd\u770b\u6a2a\u622a\u9762\uff0c\u518d\u62d6\u52a8\u65f6\u95f4\u68c0\u67e5\u8d8b\u52bf\u662f\u5426\u7a33\u5b9a\u3002",
            bullets = c(
              "65+ \u5360\u6bd4\u9ad8\u4f46 CHE/cap \u4f4e\u7684\u56fd\u5bb6\u53ef\u80fd\u5b58\u5728\u672a\u6ee1\u8db3\u9700\u6c42\u3002",
              "OOPS \u968f\u8001\u9f84\u5316\u4e0a\u5347\u65f6\uff0c\u8981\u8fdb\u4e00\u6b65\u68c0\u67e5\u957f\u671f\u62a4\u7406\u548c\u6162\u75c5\u62a5\u9500\u3002",
              "\u5982\u679c\u7f3a\u5c11 65+ \u5b57\u6bb5\uff0c\u9875\u9762\u4f1a\u4f7f\u7528\u5bff\u547d\u4ee3\u7406\u53d8\u91cf\u3002"
            )
          )
        ),
        shiny::uiOutput(ns("kpi_strip")),
        bslib::layout_columns(
          col_widths = c(7, 5),
          mod_card(
            kicker = "F1 \u00b7 Demographic pressure",
            title = "\u8001\u9f84\u5316\u4e0e\u6307\u6807\u7684\u8de8\u56fd\u5173\u7cfb",
            htmltools::p(class = "card-note",
              "\u6c14\u6ce1\u56fe \u00b7 \u6a2a\u8f74 = 65+ \u4eba\u53e3\u5360\u6bd4\uff0c\u7eb5\u8f74\u53ef\u5207\u6362\u652f\u51fa\u3001\u8d22\u52a1\u4fdd\u62a4\u6216\u5bff\u547d\u3002"),
            mod_v3_chart_guide(
              "\u540c\u4e00\u8001\u9f84\u5316\u6c34\u5e73\u4e0b\u7684\u7eb5\u5411\u5dee\u5f02\u6700\u503c\u5f97\u770b",
              "\u5982\u679c\u8001\u9f84\u5316\u6c34\u5e73\u63a5\u8fd1\uff0c\u4f46 CHE/cap\u3001OOPS \u6216\u5bff\u547d\u660e\u663e\u4e0d\u540c\uff0c\u8bf4\u660e\u652f\u4ed8\u5236\u5ea6\u3001\u670d\u52a1\u4f9b\u7ed9\u6216\u4ef7\u683c\u6c34\u5e73\u6b63\u5728\u6539\u53d8\u8001\u9f84\u5316\u7684\u8d22\u52a1\u540e\u679c\u3002",
              tone = "warn"
            ),
            mod_spinner(plotly::plotlyOutput(ns("aging_scatter"), height = 470))
          ),
          mod_card(
            kicker = "F2 \u00b7 Long-run drift",
            title = "\u8001\u9f84\u5316\u8d8b\u52bf\uff08\u6309\u6536\u5165\u7ec4\uff09",
            htmltools::p(class = "card-note",
              "65+ \u4eba\u53e3\u5360\u6bd4\u7684 24 \u5e74\u8f68\u8ff9\uff0c\u7528\u4e8e\u5224\u65ad\u54ea\u4e9b\u6536\u5165\u7ec4\u5df2\u7ecf\u8fdb\u5165\u957f\u671f\u9ad8\u538b\u9636\u6bb5\u3002"),
            mod_v3_chart_guide(
              "\u8d8b\u52bf\u6bd4\u5355\u5e74\u6392\u540d\u66f4\u80fd\u89e3\u91ca\u53ef\u6301\u7eed\u6027",
              "\u9ad8\u6536\u5165\u7ec4\u901a\u5e38\u6709\u66f4\u65e9\u7684\u8001\u9f84\u5316\u8fdb\u7a0b\uff1b\u4e2d\u6536\u5165\u7ec4\u82e5\u5feb\u901f\u4e0a\u884c\uff0c\u5219\u9700\u8981\u66f4\u65e9\u914d\u7f6e\u9884\u4ed8\u548c\u957f\u671f\u62a4\u7406\u673a\u5236\u3002"
            ),
            mod_spinner(plotly::plotlyOutput(ns("aging_trend"), height = 470))
          )
        ),
        bslib::layout_columns(
          col_widths = c(6, 6),
          mod_card(
            kicker = "F3 \u00b7 Top tail",
            title = "\u8001\u9f84\u5316 Top 20 \u56fd\u5bb6",
            htmltools::p(class = "card-note",
              "\u5f53\u524d\u5e74 65+ \u5360\u6bd4\u6700\u9ad8\u7684\u56fd\u5bb6\uff0c\u4fbf\u4e8e\u5b9a\u4f4d\u9ad8\u9700\u6c42\u538b\u529b\u6837\u672c\u3002"),
            mod_v3_chart_guide(
              "\u9ad8\u5360\u6bd4\u4e0d\u7b49\u4e8e\u9ad8\u652f\u51fa",
              "\u6392\u884c\u53ea\u662f\u4eba\u53e3\u7ed3\u6784\u4fe1\u53f7\uff0c\u9700\u8981\u56de\u5230\u6563\u70b9\u56fe\u548c\u8868\u683c\u68c0\u67e5\u8d22\u653f\u627f\u62c5\u548c\u5bb6\u5ead\u73b0\u91d1\u538b\u529b\u3002",
              tone = "good"
            ),
            mod_spinner(plotly::plotlyOutput(ns("aging_top"), height = 420))
          ),
          mod_card(
            kicker = "F4 \u00b7 Spatial pattern",
            title = "\u8001\u9f84\u5316\u4e16\u754c\u5730\u56fe",
            htmltools::p(class = "card-note",
              "65+ \u4eba\u53e3\u5360\u6bd4\u7740\u8272\uff0c\u7528\u4e8e\u8bc6\u522b\u533a\u57df\u6027\u4eba\u53e3\u538b\u529b\u96c6\u7fa4\u3002"),
            mod_v3_chart_guide(
              "\u5730\u56fe\u8d1f\u8d23\u53d1\u73b0\u7a7a\u95f4\u76f8\u90bb\u6a21\u5f0f",
              "\u8fde\u7eed\u533a\u57df\u540c\u65f6\u8fdb\u5165\u9ad8\u8001\u9f84\u5316\u65f6\uff0c\u653f\u7b56\u95ee\u9898\u5f80\u5f80\u4e0d\u662f\u5355\u56fd\u8d22\u653f\uff0c\u800c\u662f\u533a\u57df\u533b\u7597\u52b3\u52a8\u529b\u548c\u7167\u62a4\u80fd\u529b\u914d\u7f6e\u3002"
            ),
            mod_spinner(leaflet::leafletOutput(ns("aging_map"), height = 420))
          )
        ),
        mod_card(
          kicker = "F5 \u00b7 Country panel",
          title = "\u8001\u9f84\u5316 \u00d7 \u4eba\u5747 CHE \u660e\u7ec6\u9762\u677f",
          mod_v3_chart_guide(
            "\u7528\u8868\u683c\u590d\u6838\u56fe\u4e2d\u7684\u56fd\u5bb6",
            "\u8868\u683c\u540c\u65f6\u4fdd\u7559 65+ \u5360\u6bd4\u3001CHE/cap\u3001GGHE-D \u548c\u5bff\u547d\uff0c\u4fbf\u4e8e\u8bc6\u522b\u9ad8\u8001\u9f84\u5316\u4f46\u652f\u51fa\u6216\u4ea7\u51fa\u4e0d\u5339\u914d\u7684\u56fd\u5bb6\u3002"
          ),
          mod_spinner(reactable::reactableOutput(ns("aging_table"))),
          footer = "\u82e5\u672c\u5730\u6570\u636e\u96c6\u7f3a\u5c11 65+ \u5b57\u6bb5\uff0c\u7cfb\u7edf\u4f7f\u7528\u9884\u671f\u5bff\u547d\u4ee3\u7406\u8001\u9f84\u5316\u538b\u529b\u3002"
        )
      )
    )
  )
}

mod_aging_server <- function(id, master_r) {
  shiny::moduleServer(id, function(input, output, session) {

    aging_col_name <- shiny::reactive({
      m <- master_r()
      if ("pop_65" %in% names(m)) "pop_65" else if ("aging" %in% names(m)) "aging" else NULL
    })

    enriched <- shiny::reactive({
      m <- master_r()
      ac <- aging_col_name()
      if (is.null(ac)) {
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
      y_label <- switch(input$y_var,
        che_pc_usd2023 = "\u4eba\u5747 CHE\uff08USD 2023\uff09",
        gghed_che = "GGHE-D / CHE\uff08%\uff09",
        life_exp = "\u9884\u671f\u5bff\u547d\uff08\u5c81\uff09",
        hf3_che = "OOPS / CHE\uff08%\uff09",
        input$y_var
      )
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
            yaxis = list(title = y_label)
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
        "\u56fd\u5bb6" = d$country_name,
        "65+%" = round(d$.aging, 1),
        "CHE/cap" = round(d$che_pc_usd2023, 0),
        "GGHE-D%" = round(d$gghed_che, 1),
        "\u5bff\u547d" = round(d$life_exp, 1),
        check.names = FALSE
      )
      reactable::reactable(tab, defaultPageSize = 15, searchable = TRUE,
        defaultColDef = reactable::colDef(
          headerStyle = list(background = "#f1f3f7")))
    })
  })
}
