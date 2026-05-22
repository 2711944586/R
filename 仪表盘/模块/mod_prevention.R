# =============================================================================
# 仪表盘/模块/mod_prevention.R
# 预防性支出：HC6 预防性护理在卫生总支出中的占比与效果
# =============================================================================

mod_prevention_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = "\u9884\u9632 Prevention",
    value = "prevention",
    mod_v3_hero(
      kicker = "\u9884\u9632\u670d\u52a1\u652f\u51fa",
      title = "\u9884\u9632\u6027\u62a4\u7406\u652f\u51fa\u4e0e\u5065\u5eb7\u56de\u62a5",
      lead = paste(
        "HC6 \u9884\u9632\u6027\u62a4\u7406\u652f\u51fa\u5360 CHE \u6bd4\u91cd\u5728\u5168\u7403\u4ec5 3\u20135%\uff0c",
        "\u4f46\u5176\u8fb9\u9645\u5065\u5eb7\u56de\u62a5\u8fdc\u9ad8\u4e8e\u6cbb\u7597\u6027\u652f\u51fa\u3002",
        "\u672c\u6a21\u5757\u5206\u6790\u9884\u9632\u652f\u51fa\u7684\u8de8\u56fd\u5dee\u5f02\u3001\u4e0e NCD \u8d1f\u62c5\u7684\u5173\u8054\u3002"
      ),
      meta = list("WHO GHED HC6", "195 \u56fd\u5bb6", "2000\u20132023")
    ),
    mod_v3_page_body(
      wide = TRUE,
      mod_v3_topic_brief("prevention"),
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
          mod_v3_sidebar_note(
            "HC6 \u53e3\u5f84",
            "HC6 \u4ee3\u8868\u9884\u9632\u6027\u62a4\u7406\u652f\u51fa\uff0c\u5305\u62ec\u514d\u75ab\u3001\u7b5b\u67e5\u3001\u5065\u5eb7\u4fc3\u8fdb\u3001\u75be\u75c5\u76d1\u6d4b\u548c\u516c\u5171\u536b\u751f\u5e72\u9884\u7b49\u3002",
            bullets = c(
              "\u9884\u9632\u5360\u6bd4\u8f83\u9ad8\u5e76\u4e0d\u7b49\u4e8e\u7acb\u5373\u4ea7\u51fa\u66f4\u597d\uff0c\u56e0\u4e3a\u5065\u5eb7\u56de\u62a5\u6709\u660e\u663e\u6ede\u540e\u3002",
              "HC6/HC1 \u6bd4\u503c\u53ef\u4ee5\u63d0\u793a\u7cfb\u7edf\u662f\u66f4\u504f\u524d\u7aef\u9632\u63a7\u8fd8\u662f\u540e\u7aef\u6cbb\u7597\u3002",
              "\u56fd\u5bb6\u95f4 HC6 \u5206\u7c7b\u53e3\u5f84\u53ef\u80fd\u4e0d\u5b8c\u5168\u4e00\u81f4\uff0c\u6781\u7aef\u503c\u9700\u8981\u56de\u8868\u683c\u590d\u6838\u3002"
            )
          )
        ),
      shiny::uiOutput(ns("kpi_strip")),
      # Row 1
      bslib::layout_columns(
        col_widths = c(7, 5),
        mod_card(
          kicker = "P1 \u00b7 Outcome link",
          title = "\u9884\u9632\u652f\u51fa\u4e0e\u9884\u671f\u5bff\u547d",
          htmltools::p(class = "card-note",
            "\u6a2a\u8f74 = HC6 \u5360 CHE%\uff0c\u7eb5\u8f74 = \u9884\u671f\u5bff\u547d\u3002\u9884\u9632\u6295\u5165\u8f83\u9ad8\u7684\u56fd\u5bb6\u5bff\u547d\u504f\u9ad8\u3002"),
          mod_v3_chart_guide(
            "\u628a\u6563\u70b9\u5f53\u4f5c\u7ed3\u6784\u7ebf\u7d22\uff0c\u4e0d\u5f53\u4f5c\u56e0\u679c\u8bc1\u660e",
            "\u53f3\u4e0a\u533a\u57df\u8868\u793a\u9884\u9632\u6295\u5165\u548c\u5bff\u547d\u4ea7\u51fa\u540c\u65f6\u8f83\u9ad8\uff1b\u5de6\u4e0b\u533a\u57df\u5219\u66f4\u9700\u8981\u7ed3\u5408\u6536\u5165\u3001\u75be\u75c5\u8d1f\u62c5\u548c\u670d\u52a1\u53ef\u53ca\u6027\u89e3\u91ca\u3002",
            bullets = c("\u540c\u989c\u8272\u5185\u7684\u504f\u79bb\u70b9\u6bd4\u8de8\u7ec4\u6bd4\u8f83\u66f4\u6709\u653f\u7b56\u542b\u4e49\u3002", "\u5bff\u547d\u662f\u7d2f\u79ef\u7ed3\u679c\uff0c\u4e0d\u5e94\u53ea\u7528\u5f53\u5e74 HC6 \u89e3\u91ca\u3002")
          ),
          mod_spinner(plotly::plotlyOutput(ns("hc6_life_scatter"), height = 440))
        ),
        mod_card(
          kicker = "P2 \u00b7 Prevention trend",
          title = "HC6 \u5360\u6bd4\u8d8b\u52bf\uff08\u6309\u7ec4\uff09",
          htmltools::p(class = "card-note",
            "\u5404\u7ec4\u9884\u9632\u652f\u51fa\u5360\u6bd4\u7684 24 \u5e74\u6f14\u5316\u3002"),
          mod_v3_chart_guide(
            "\u8d8b\u52bf\u6bd4\u5355\u5e74\u6392\u540d\u66f4\u80fd\u5224\u65ad\u4f53\u7cfb\u8f6c\u5411",
            "\u5982\u679c HC6 \u5360\u6bd4\u957f\u671f\u4e0a\u884c\uff0c\u8bf4\u660e\u9884\u9632\u6295\u5165\u5728\u536b\u751f\u8d44\u6e90\u4e2d\u83b7\u5f97\u66f4\u9ad8\u6743\u91cd\uff1b\u5982\u679c\u53ea\u662f\u5355\u5e74\u5c16\u5cf0\uff0c\u53ef\u80fd\u4e0e\u75ab\u60c5\u6216\u9879\u76ee\u6027\u652f\u51fa\u6709\u5173\u3002",
            tone = "good"
          ),
          mod_spinner(plotly::plotlyOutput(ns("hc6_trend"), height = 440)),
          footer = "\u6298\u7ebf\u4e3a\u7ec4\u5185\u56fd\u5bb6\u5747\u503c\uff0c\u7528\u4e8e\u5224\u65ad\u5927\u65b9\u5411\u800c\u975e\u5355\u56fd\u8def\u5f84\u3002"
        )
      ),
      # Row 2
      bslib::layout_columns(
        col_widths = c(6, 6),
        mod_card(
          kicker = "P3 \u00b7 Group spread",
          title = "HC6 \u5360\u6bd4\u5206\u5e03\uff08\u6700\u65b0\u5e74\uff09",
          htmltools::p(class = "card-note",
            "\u5404\u7ec4\u56fd\u5bb6\u7684\u9884\u9632\u652f\u51fa\u5360\u6bd4\u5206\u5e03\u3002"),
          mod_v3_chart_guide(
            "\u7bb1\u7ebf\u56fe\u7528\u6765\u6293\u7ec4\u5185\u5dee\u5f02",
            "\u7ec4\u95f4\u5e73\u5747\u503c\u53ef\u80fd\u76f8\u8fd1\uff0c\u4f46\u7ec4\u5185\u5c3e\u90e8\u56fd\u5bb6\u7684\u9884\u9632\u6295\u5165\u5dee\u8ddd\u4f1a\u5f88\u5927\uff1b\u8fd9\u662f\u5bfb\u627e\u5b66\u4e60\u6837\u672c\u548c\u4f4e\u6295\u5165\u6837\u672c\u7684\u5165\u53e3\u3002"
          ),
          mod_spinner(plotly::plotlyOutput(ns("hc6_box"), height = 380))
        ),
        mod_card(
          kicker = "P4 \u00b7 Functional balance",
          title = "\u9884\u9632 vs \u6cbb\u7597\u652f\u51fa\u6bd4\u4f8b",
          htmltools::p(class = "card-note",
            "HC6 (\u9884\u9632) \u4e0e HC1 (\u6cbb\u7597) \u7684\u6bd4\u503c\u3002\u6bd4\u503c\u8d8a\u9ad8 = \u9884\u9632\u5bfc\u5411\u8d8a\u5f3a\u3002"),
          mod_v3_chart_guide(
            "\u4e0d\u53ea\u770b HC6\uff0c\u8981\u770b\u5b83\u548c\u6cbb\u7597\u652f\u51fa\u7684\u5173\u7cfb",
            "\u53f3\u4e0b\u70b9\u901a\u5e38\u4ee3\u8868\u6cbb\u7597\u5360\u6bd4\u9ad8\u3001\u9884\u9632\u5360\u6bd4\u4f4e\uff1b\u5de6\u4e0a\u70b9\u5219\u8868\u793a\u529f\u80fd\u7ed3\u6784\u66f4\u504f\u524d\u7aef\u3002",
            tone = "warn"
          ),
          mod_spinner(plotly::plotlyOutput(ns("prevention_treatment"), height = 380)),
          footer = "HC1 \u548c HC6 \u90fd\u662f\u5360 CHE \u6bd4\u4f8b\uff0c\u5c0f\u7cfb\u7edf\u7684\u5206\u7c7b\u53d8\u52a8\u53ef\u80fd\u653e\u5927\u6bd4\u4f8b\u6ce2\u52a8\u3002"
        )
      ),
      # Row 3
      bslib::layout_columns(
        col_widths = c(5, 7),
        mod_card(
          kicker = "P5 \u00b7 High prevention share",
          title = "HC6 Top 20 \u56fd\u5bb6",
          htmltools::p(class = "card-note",
            "\u9884\u9632\u652f\u51fa\u5360\u6bd4\u6700\u9ad8\u7684\u56fd\u5bb6\u3002"),
          mod_v3_chart_guide(
            "\u5934\u90e8\u56fd\u5bb6\u662f\u201c\u4e3a\u4ec0\u4e48\u9ad8\u201d\u7684\u6837\u672c",
            "\u6392\u540d\u9760\u524d\u53ef\u80fd\u6765\u81ea\u771f\u5b9e\u9884\u9632\u6295\u5165\u3001\u9879\u76ee\u6027\u652f\u51fa\uff0c\u6216\u8005\u529f\u80fd\u5206\u7c7b\u53e3\u5f84\u5dee\u5f02\uff1b\u9700\u8981\u4e0e\u5730\u56fe\u548c\u8868\u683c\u4e00\u8d77\u8bfb\u3002",
            tone = "good"
          ),
          mod_spinner(plotly::plotlyOutput(ns("hc6_top"), height = 440))
        ),
        mod_card(
          kicker = "P6 \u00b7 Spatial pattern",
          title = "\u9884\u9632\u652f\u51fa\u5730\u56fe",
          htmltools::p(class = "card-note",
            "HC6 \u5360 CHE \u6bd4\u91cd\u7684\u5168\u7403\u5206\u5e03\u3002"),
          mod_v3_chart_guide(
            "\u5730\u56fe\u628a\u9884\u9632\u6295\u5165\u653e\u56de\u533a\u57df\u8bed\u5883",
            "\u989c\u8272\u96c6\u4e2d\u7684\u533a\u57df\u53ef\u80fd\u53cd\u6620\u516c\u5171\u536b\u751f\u9879\u76ee\u6295\u5165\u3001\u6536\u5165\u6c34\u5e73\u6216\u7edf\u8ba1\u53e3\u5f84\u5171\u6027\u3002"
          ),
          mod_spinner(leaflet::leafletOutput(ns("hc6_map"), height = 440))
        )
      ),
      mod_card(
        kicker = "P7 \u00b7 Prevention audit",
        title = "\u9884\u9632\u652f\u51fa\u8be6\u8868",
        mod_v3_chart_guide(
          "\u8868\u683c\u662f\u56fe\u8868\u7ed3\u8bba\u7684\u56de\u68c0\u533a",
          "\u4ece\u6563\u70b9\u3001Top \u699c\u5355\u6216\u5730\u56fe\u4e2d\u9009\u51fa\u7684\u56fd\u5bb6\uff0c\u5728\u8fd9\u91cc\u540c\u65f6\u6838\u5bf9 HC6\u3001HC1\u3001CHE/cap \u548c\u5bff\u547d\u3002"
        ),
        mod_spinner(reactable::reactableOutput(ns("hc6_table"))),
        footer = "\u8868\u683c\u6309 HC6 \u5360\u6bd4\u964d\u5e8f\u6392\u5217\uff0c\u9002\u5408\u5feb\u901f\u590d\u6838\u9ad8\u9884\u9632\u5360\u6bd4\u56fd\u5bb6\u3002"
      )
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
