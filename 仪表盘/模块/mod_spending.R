
mod_spending_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = "\u652f\u51fa Spending",
    value = "spending",
    mod_v3_hero(
      kicker = "\u652f\u51fa\u6c34\u5e73\u4e0e\u6a21\u5f0f",
      title = "\u536b\u751f\u652f\u51fa\u6c34\u5e73\u4e0e\u6a21\u5f0f",
      lead = paste(
        "\u4eba\u5747 CHE \u5728\u5168\u7403\u5448\u73b0 100 \u500d\u5dee\u8ddd\uff1a",
        "\u9ad8\u6536\u5165\u56fd\u5bb6\u8d85 $5,000\uff0c\u4f4e\u6536\u5165\u56fd\u5bb6\u4ec5 $30\u201350\u3002",
        "\u672c\u6a21\u5757\u5c55\u793a\u652f\u51fa\u6c34\u5e73\u7684\u5206\u5e03\u3001\u6392\u884c\u3001\u6536\u5165\u68af\u5ea6\u4e0e\u65f6\u5e8f\u8d8b\u52bf\u3002"
      ),
      meta = list("USD 2023 \u4e0d\u53d8\u4ef7", "195 \u56fd\u5bb6", "2000\u20132023")
    ),
    mod_v3_page_body(
      wide = TRUE,
      mod_v3_topic_brief("spending"),
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
        mod_v3_sidebar_note(
          "\u9605\u8bfb\u8bbe\u7f6e",
          "\u652f\u51fa\u53d8\u91cf\u8de8\u5ea6\u5f88\u5927\uff0c\u5efa\u8bae\u5148\u7528\u5bf9\u6570\u5750\u6807\u770b\u7ed3\u6784\uff0c\u518d\u5207\u56de\u7ebf\u6027\u5750\u6807\u5224\u65ad\u7edd\u5bf9\u7f8e\u5143\u5dee\u3002",
          bullets = c(
            "\u5e74\u4efd\u6ed1\u5757\u56fa\u5b9a\u4e00\u4e2a\u6a2a\u622a\u9762\u3002",
            "\u5927\u6d32\u7528\u4e8e\u7a7a\u95f4\u8bc6\u522b\uff0c\u6536\u5165\u7ec4\u7528\u4e8e\u89e3\u91ca\u652f\u4ed8\u80fd\u529b\u68af\u5ea6\u3002",
            "Top\u3001\u7bb1\u7ebf\u56fe\u3001\u5730\u56fe\u548c\u8868\u683c\u5e94\u914d\u5957\u9605\u8bfb\u3002"
          )
        )
      ),
      shiny::uiOutput(ns("kpi_strip")),
      bslib::layout_columns(
        col_widths = c(7, 5),
        mod_card(
          kicker = "F1 · Income gradient",
          title = "\u4eba\u5747 CHE vs GDP \u6563\u70b9\u56fe",
          htmltools::p(class = "card-note",
            "\u536b\u751f\u652f\u51fa\u4e0e\u7ecf\u6d4e\u6c34\u5e73\u7684\u5f3a\u6b63\u76f8\u5173\u3002\u504f\u79bb\u62df\u5408\u7ebf\u7684\u56fd\u5bb6\u503c\u5f97\u5173\u6ce8\u3002"),
          mod_v3_chart_guide(
            "\u8bfb\u79bb\u6563\u800c\u4e0d\u662f\u53ea\u8bfb\u659c\u7387",
            "\u540c\u6837 GDP/cap \u4e0b\u660e\u663e\u9ad8\u4e8e\u6216\u4f4e\u4e8e\u4e91\u56e2\u7684\u56fd\u5bb6\uff0c\u901a\u5e38\u6bd4\u62df\u5408\u7ebf\u672c\u8eab\u66f4\u6709\u89e3\u91ca\u4ef7\u503c\u3002",
            bullets = c("\u6a2a\u7eb5\u8f74\u540c\u5f00\u5bf9\u6570\u65f6\uff0c\u8ddd\u79bb\u4ee3\u8868\u76f8\u5bf9\u500d\u6570\u5dee\u3002", "\u60ac\u505c\u70b9\u4f4d\u53ef\u56de\u5230\u56fd\u5bb6\u753b\u50cf\u7ee7\u7eed\u8ffd\u8e2a\u3002")
          ),
          mod_spinner(plotly::plotlyOutput(ns("che_gdp_scatter"), height = 440))
        ),
        mod_card(
          kicker = "F2 · Group spread",
          title = "\u4eba\u5747 CHE \u5206\u5e03\uff08\u6309\u7ec4\uff09",
          htmltools::p(class = "card-note",
            "\u5404\u7ec4\u7684\u652f\u51fa\u6c34\u5e73\u5206\u5e03\uff0c\u7bb1\u7ebf + \u6563\u70b9\u3002"),
          mod_v3_chart_guide(
            "\u7bb1\u4f53\u663e\u793a\u7ec4\u5185\u5dee\u5f02",
            "\u4e2d\u4f4d\u7ebf\u8868\u793a\u5178\u578b\u56fd\u5bb6\uff0c\u7bb1\u4f53\u548c\u79bb\u7fa4\u70b9\u63d0\u793a\u540c\u7ec4\u5185\u90e8\u7684\u5236\u5ea6\u3001\u4ef7\u683c\u548c\u89c4\u6a21\u5dee\u5f02\u3002",
            tone = "warn"
          ),
          mod_spinner(plotly::plotlyOutput(ns("che_box"), height = 440)),
          footer = "\u91d1\u989d\u53e3\u5f84\u4e3a 2023 \u5e74\u4e0d\u53d8\u7f8e\u5143\uff1b\u5bf9\u6570\u8f74\u4f1a\u538b\u7f29\u9ad8\u503c\u5c3e\u90e8\u3002"
        )
      ),
      bslib::layout_columns(
        col_widths = c(6, 6),
        mod_card(
          kicker = "F3 · Top tail",
          title = "\u4eba\u5747 CHE \u6392\u884c Top 20",
          htmltools::p(class = "card-note",
            "\u5f53\u5e74\u4eba\u5747\u536b\u751f\u652f\u51fa\u6700\u9ad8\u7684 20 \u4e2a\u56fd\u5bb6\u3002"),
          mod_v3_chart_guide(
            "\u6392\u884c\u7528\u4e8e\u5b9a\u4f4d\u5c3e\u90e8",
            "\u9ad8\u652f\u51fa\u56fd\u5bb6\u5f80\u5f80\u96c6\u4e2d\u5728\u9ad8\u6536\u5165\u3001\u5c0f\u56fd\u6216\u4ef7\u683c\u6c34\u5e73\u8f83\u9ad8\u7684\u7cfb\u7edf\uff0c\u89e3\u91ca\u65f6\u9700\u8981\u7ed3\u5408 GDP/cap \u548c\u4eba\u53e3\u89c4\u6a21\u3002",
            tone = "good"
          ),
          mod_spinner(plotly::plotlyOutput(ns("che_top"), height = 420))
        ),
        mod_card(
          kicker = "F4 · Global drift",
          title = "\u5168\u7403\u4eba\u5747 CHE \u65f6\u5e8f\u8d8b\u52bf",
          htmltools::p(class = "card-note",
            "\u5168\u7403\u4e2d\u4f4d\u6570\u3001\u5747\u503c\u3001\u56db\u5206\u4f4d\u6570\u7684 24 \u5e74\u6f14\u5316\u3002"),
          mod_v3_chart_guide(
            "\u770b\u4e2d\u4f4d\u6570\u548c IQR \u7684\u5171\u540c\u79fb\u52a8",
            "\u4e2d\u4f4d\u6570\u4e0a\u5347\u8bf4\u660e\u5178\u578b\u56fd\u5bb6\u652f\u51fa\u62ac\u5347\uff1bIQR \u6269\u5927\u5219\u63d0\u793a\u8de8\u56fd\u5dee\u8ddd\u6216\u7ec4\u5185\u5dee\u5f02\u4ecd\u5728\u6269\u5f20\u3002"
          ),
          mod_spinner(plotly::plotlyOutput(ns("che_trend"), height = 420)),
          footer = "IQR = \u7b2c 25 \u5230\u7b2c 75 \u767e\u5206\u4f4d\uff0c\u4e0d\u53d7\u6781\u7aef\u9ad8\u503c\u4e3b\u5bfc\u3002"
        )
      ),
      bslib::layout_columns(
        col_widths = c(7, 5),
        mod_card(
          kicker = "F5 · Spatial pattern",
          title = "\u4eba\u5747 CHE \u4e16\u754c\u5730\u56fe",
          htmltools::p(class = "card-note",
            "\u989c\u8272\u8d8a\u6df1 = \u4eba\u5747\u652f\u51fa\u8d8a\u9ad8\u3002"),
          mod_v3_chart_guide(
            "\u5730\u56fe\u8d1f\u8d23\u53d1\u73b0\u533a\u57df\u96c6\u7fa4",
            "\u8fde\u7eed\u90bb\u8fd1\u56fd\u5bb6\u540c\u65f6\u504f\u9ad8\u6216\u504f\u4f4e\u65f6\uff0c\u901a\u5e38\u8bf4\u660e\u6536\u5165\u3001\u5236\u5ea6\u6216\u533a\u57df\u4ef7\u683c\u6c34\u5e73\u5b58\u5728\u5171\u540c\u80cc\u666f\u3002",
            tone = "good"
          ),
          mod_spinner(leaflet::leafletOutput(ns("spending_map"), height = 400))
        ),
        mod_card(
          kicker = "F6 \u00b7 \u5ba1\u8ba1\u8868",
          title = "\u652f\u51fa\u6570\u636e\u8868",
          mod_v3_chart_guide(
            "\u8868\u683c\u7528\u4e8e\u590d\u6838",
            "\u56fe\u8868\u53d1\u73b0\u7684\u56fd\u5bb6\u53ef\u5728\u8fd9\u91cc\u6309\u56fd\u5bb6\u540d\u641c\u7d22\uff0c\u6838\u5bf9\u6536\u5165\u7ec4\u3001CHE/cap\u3001GDP/cap \u548c CHE/GDP \u6bd4\u4f8b\u3002"
          ),
          mod_spinner(reactable::reactableOutput(ns("spending_table"))),
          footer = "CHE/GDP% \u7531 CHE/cap \u4e0e GDP/cap \u8fd1\u4f3c\u8ba1\u7b97\uff0c\u7528\u4e8e\u5feb\u901f\u6bd4\u8f83\u3002"
        )
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
            xaxis = list(title = "\u4eba\u5747 GDP\uff08USD\uff09", type = ax_type),
            yaxis = list(title = "\u4eba\u5747 CHE\uff08USD 2023\uff09", type = ax_type),
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
            yaxis = list(title = "\u4eba\u5747 CHE\uff08USD 2023\uff09",
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
            xaxis = list(title = "\u4eba\u5747 CHE\uff08USD 2023\uff09"),
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
            yaxis = list(title = "\u4eba\u5747 CHE\uff08USD 2023\uff09"),
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
