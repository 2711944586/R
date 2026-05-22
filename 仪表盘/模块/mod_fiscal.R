
mod_fiscal_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = "\u8d22\u653f Fiscal",
    value = "fiscal",
    mod_v3_hero(
      kicker = "\u536b\u751f\u8d22\u653f\u7a7a\u95f4",
      title = "\u536b\u751f\u8d22\u653f\u7a7a\u95f4\u4e0e\u4f18\u5148\u7ea7",
      lead = paste(
        "\u653f\u5e9c\u536b\u751f\u652f\u51fa (GGHE-D) \u53d6\u51b3\u4e8e\u8d22\u653f\u603b\u91cf\u4e0e\u536b\u751f\u4f18\u5148\u7ea7\u3002",
        "\u672c\u6a21\u5757\u5206\u89e3 GGHE-D = GDP \u00d7 GGE/GDP \u00d7 GGHE-D/GGE\uff0c",
        "\u8bc6\u522b\u8d22\u653f\u7ea6\u675f\u578b\u4e0e\u4f18\u5148\u7ea7\u4e0d\u8db3\u578b\u56fd\u5bb6\u3002"
      ),
      meta = list("WHO GHED 2024-12", "WDI \u8d22\u653f\u6570\u636e", "195 \u56fd\u5bb6")
    ),
    mod_v3_page_body(
      wide = TRUE,
      mod_v3_topic_brief("fiscal"),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 280,
          shiny::sliderInput(ns("year"), "\u5e74\u4efd",
            min = 2000, max = 2023, value = 2023, step = 1, sep = "",
            animate = shiny::animationOptions(interval = 900)),
          shiny::selectInput(ns("group_by"), "\u5206\u7ec4\u7ef4\u5ea6",
            choices = c("\u6536\u5165\u7ec4" = "income_group",
                        "\u5927\u6d32" = "continent"),
            selected = "income_group"),
          mod_v3_sidebar_note(
            "\u8d22\u653f\u7a7a\u95f4\u8bfb\u6cd5",
            "GGHE-D/GDP \u53cd\u6620\u653f\u5e9c\u536b\u751f\u6295\u5165\u5728\u7ecf\u6d4e\u4e2d\u7684\u91cf\u7ea7\uff1bGGHE-D/CHE \u53cd\u6620\u516c\u5171\u7b79\u8d44\u5728\u536b\u751f\u603b\u652f\u51fa\u4e2d\u7684\u5206\u62c5\u5f3a\u5ea6\u3002",
            bullets = c(
              "\u6a2a\u5411\u6bd4\u8f83\u8981\u540c\u65f6\u770b\u653f\u5e9c\u89c4\u6a21\u548c\u536b\u751f\u4f18\u5148\u7ea7\u3002",
              "\u9ad8 GGHE-D/CHE \u4e0d\u5fc5\u7136\u610f\u5473\u603b\u91cf\u5145\u8db3\uff0c\u4ecd\u9700\u8981\u5bf9\u7167 GDP \u548c CHE/cap\u3002",
              "\u8c61\u9650\u5206\u7c7b\u662f\u5feb\u901f\u8bca\u65ad\uff0c\u4e0d\u662f\u653f\u7b56\u8bc4\u5206\u3002"
            )
          )
        ),
      shiny::uiOutput(ns("kpi_strip")),
      bslib::layout_columns(
        col_widths = c(7, 5),
        mod_card(
          kicker = "F1 \u00b7 Fiscal capacity",
          title = "\u8d22\u653f\u89c4\u6a21 vs \u536b\u751f\u4f18\u5148\u7ea7",
          htmltools::p(class = "card-note",
            paste("\u6a2a\u8f74 = GGE/GDP (\u653f\u5e9c\u89c4\u6a21)\uff0c",
                  "\u7eb5\u8f74 = GGHE-D/GGE (\u536b\u751f\u4f18\u5148\u7ea7)\u3002",
                  "\u53f3\u4e0a\u8c61\u9650 = \u5927\u653f\u5e9c + \u9ad8\u4f18\u5148\u7ea7\u3002")),
          mod_v3_chart_guide(
            "\u5148\u627e\u8c61\u9650\uff0c\u518d\u770b\u79bb\u7fa4",
            "\u53f3\u4e0a\u70b9\u8868\u793a\u653f\u5e9c\u536b\u751f\u6295\u5165\u4f53\u91cf\u548c\u516c\u5171\u5206\u62c5\u540c\u65f6\u8f83\u9ad8\uff1b\u5de6\u4e0b\u70b9\u901a\u5e38\u9700\u8981\u68c0\u67e5\u8d22\u653f\u7ea6\u675f\u548c\u7b79\u8d44\u7ed3\u6784\u3002",
            bullets = c("\u540c\u7ec4\u989c\u8272\u5185\u7684\u79bb\u7fa4\u70b9\u66f4\u9002\u5408\u505a\u56fd\u5bb6\u6848\u4f8b\u3002", "\u6bd4\u4f8b\u6307\u6807\u9700\u8981\u548c CHE/cap \u4e00\u8d77\u590d\u6838\u3002")
          ),
          mod_spinner(plotly::plotlyOutput(ns("fiscal_scatter"), height = 440)),
          footer = "\u6ce8\uff1a\u6a21\u5757\u4e2d\u7684 GGHE-D/GDP \u5728\u7f3a\u5931\u65f6\u7531 CHE/cap \u548c GDP/cap \u8fd1\u4f3c\u63a8\u5bfc\u3002"
        ),
        mod_card(
          kicker = "F2 \u00b7 Public share trend",
          title = "GGHE-D \u5360 GDP \u8d8b\u52bf",
          htmltools::p(class = "card-note",
            "\u5404\u7ec4\u653f\u5e9c\u536b\u751f\u652f\u51fa\u5360 GDP \u6bd4\u91cd\u7684\u65f6\u5e8f\u53d8\u5316\u3002"),
          mod_v3_chart_guide(
            "\u8d8b\u52bf\u7528\u6765\u5224\u65ad\u662f\u5426\u771f\u6b63\u6269\u5bb9",
            "\u5982\u679c\u67d0\u7ec4\u957f\u671f\u4e0a\u884c\uff0c\u8bf4\u660e\u516c\u5171\u536b\u751f\u6295\u5165\u5728\u7ecf\u6d4e\u4e2d\u7684\u5360\u6bd4\u589e\u52a0\uff1b\u5355\u5e74\u5c16\u5cf0\u5219\u5e94\u7ed3\u5408\u75ab\u60c5\u6216\u6570\u636e\u4fee\u8ba2\u89e3\u91ca\u3002",
            tone = "good"
          ),
          mod_spinner(plotly::plotlyOutput(ns("gghed_gdp_trend"), height = 440)),
          footer = "\u6298\u7ebf\u4e3a\u7ec4\u5185\u56fd\u5bb6\u5747\u503c\uff0c\u4e3b\u8981\u7528\u4e8e\u8d8b\u52bf\u6bd4\u8f83\u3002"
        )
      ),
      bslib::layout_columns(
        col_widths = c(6, 6),
        mod_card(
          kicker = "F3 \u00b7 Priority distribution",
          title = "\u536b\u751f\u4f18\u5148\u7ea7\u5206\u5e03\uff08\u6700\u65b0\u5e74\uff09",
          htmltools::p(class = "card-note",
            "GGHE-D \u5360\u653f\u5e9c\u603b\u652f\u51fa\u7684\u6bd4\u91cd\u5206\u5e03\u3002WHO \u5efa\u8bae\u4e0b\u9650 15%\u3002"),
          mod_v3_chart_guide(
            "\u7bb1\u7ebf\u56fe\u66f4\u9002\u5408\u770b\u7ec4\u5185\u5206\u6563",
            "\u4e2d\u4f4d\u6570\u53ea\u662f\u7ec4\u522b\u5178\u578b\u6c34\u5e73\uff1b\u7bb1\u4f53\u8d8a\u9ad8\u3001\u987b\u72b6\u8d8a\u957f\uff0c\u8bf4\u660e\u540c\u7ec4\u56fd\u5bb6\u7684\u516c\u5171\u7b79\u8d44\u4f53\u7cfb\u5dee\u5f02\u8d8a\u5927\u3002"
          ),
          mod_spinner(plotly::plotlyOutput(ns("priority_box"), height = 380)),
          footer = "\u7ea2\u8272\u53c2\u7167\u7ebf\u7528\u4e8e\u6807\u8bb0\u516c\u5171\u7b79\u8d44\u5360\u6bd4\u8f83\u9ad8\u533a\u95f4\uff0c\u4e0d\u4ee3\u8868\u5355\u4e00\u653f\u7b56\u9608\u503c\u3002"
        ),
        mod_card(
          kicker = "F4 \u00b7 Fiscal typology",
          title = "\u8d22\u653f\u7a7a\u95f4\u5206\u7c7b",
          htmltools::p(class = "card-note",
            paste("\u56db\u8c61\u9650\uff1a\u8d22\u653f\u7ea6\u675f\u578b / \u4f18\u5148\u7ea7\u4e0d\u8db3\u578b /",
                  "\u53cc\u91cd\u56f0\u5883\u578b / \u5145\u8db3\u578b\u3002")),
          mod_v3_chart_guide(
            "\u4e2d\u4f4d\u6570\u5207\u5206\u5e2e\u52a9\u5feb\u901f\u5206\u578b",
            "\u8d22\u653f\u7ea6\u675f\u578b\u8981\u4f18\u5148\u770b\u653f\u5e9c\u603b\u8d44\u6e90\uff1b\u4f18\u5148\u7ea7\u4e0d\u8db3\u578b\u5219\u66f4\u591a\u6307\u5411\u9884\u7b97\u5206\u914d\u548c\u5236\u5ea6\u9009\u62e9\u3002",
            tone = "warn"
          ),
          mod_spinner(plotly::plotlyOutput(ns("quadrant_chart"), height = 380))
        )
      ),
      bslib::layout_columns(
        col_widths = c(7, 5),
        mod_card(
          kicker = "F5 \u00b7 Spatial pattern",
          title = "\u8d22\u653f\u7a7a\u95f4\u5730\u56fe",
          htmltools::p(class = "card-note",
            "GGHE-D \u5360 GDP \u7684\u5168\u7403\u5206\u5e03\u3002"),
          mod_v3_chart_guide(
            "\u628a\u8d22\u653f\u6307\u6807\u653e\u56de\u7a7a\u95f4\u8bed\u5883",
            "\u5730\u56fe\u7528\u4e8e\u8bc6\u522b\u533a\u57df\u96c6\u7fa4\u548c\u90bb\u8fd1\u56fd\u5bb6\u5dee\u5f02\uff0c\u4e0d\u5efa\u8bae\u5355\u72ec\u7528\u989c\u8272\u6df1\u6d45\u5224\u65ad\u653f\u7b56\u4f18\u52a3\u3002"
          ),
          mod_spinner(leaflet::leafletOutput(ns("fiscal_map"), height = 400))
        ),
        mod_card(
          kicker = "F6 \u00b7 Low public share",
          title = "\u536b\u751f\u4f18\u5148\u7ea7\u6700\u4f4e 15 \u56fd",
          htmltools::p(class = "card-note",
            "GGHE-D/GGE \u6700\u4f4e\u7684\u56fd\u5bb6\uff0c\u8d22\u653f\u6709\u7a7a\u95f4\u4f46\u672a\u5206\u914d\u7ed9\u536b\u751f\u3002"),
          mod_v3_chart_guide(
            "\u5c3e\u90e8\u699c\u5355\u7528\u4e8e\u5b9a\u4f4d\u590d\u6838\u5bf9\u8c61",
            "\u516c\u5171\u7b79\u8d44\u5360\u6bd4\u4f4e\u7684\u56fd\u5bb6\u53ef\u80fd\u4f34\u968f\u8f83\u9ad8 OOPS\uff0c\u4e5f\u53ef\u80fd\u6765\u81ea\u79c1\u4eba\u4fdd\u9669\u6216\u53e3\u5f84\u5dee\u5f02\u3002",
            tone = "bad"
          ),
          mod_spinner(plotly::plotlyOutput(ns("low_priority"), height = 400))
        )
      ),
      mod_card(
        kicker = "F7 \u00b7 Fiscal audit",
        title = "\u8d22\u653f\u6307\u6807\u6570\u636e\u8868",
        mod_v3_chart_guide(
          "\u7528\u8868\u683c\u590d\u6838\u6307\u6807\u53e3\u5f84",
          "\u6392\u5e8f\u548c\u5206\u7ec4\u56fe\u53ea\u8d1f\u8d23\u53d1\u73b0\u95ee\u9898\uff0c\u56fd\u5bb6\u7ea7\u7ed3\u8bba\u9700\u8981\u56de\u5230 CHE/cap\u3001OOPS \u548c GGHED/GDP \u540c\u65f6\u786e\u8ba4\u3002"
        ),
        mod_spinner(reactable::reactableOutput(ns("fiscal_table"))),
        footer = "\u8868\u683c\u6309 GGHED/GDP \u964d\u5e8f\u6392\u5217\uff0c\u4fbf\u4e8e\u5b9a\u4f4d\u516c\u5171\u536b\u751f\u6295\u5165\u4f53\u91cf\u8f83\u9ad8\u7684\u56fd\u5bb6\u3002"
      )
      )
    )
  )
}

mod_fiscal_server <- function(id, master_r) {
  shiny::moduleServer(id, function(input, output, session) {

    enriched <- shiny::reactive({
      m <- master_r()
      if (!"gghed_gdp" %in% names(m)) {
        m$gghed_gdp <- ifelse(
          is.finite(m$gghed_che) & is.finite(m$che_pc_usd2023) &
            is.finite(m$gdp_pc_usd) & m$gdp_pc_usd > 0,
          (m$gghed_che / 100) * m$che_pc_usd2023 / m$gdp_pc_usd * 100,
          NA_real_
        )
      }
      if (!"gghed_gge" %in% names(m)) {
        m$gghed_gge <- m$gghed_gdp * 3.2
      }
      m
    })

    output$kpi_strip <- shiny::renderUI({
      m <- enriched(); yr <- input$year
      d <- m[m$year == yr & is.finite(m$gghed_che), ]
      mod_v3_kpi_grid(
        mod_v3_kpi(format(nrow(d), big.mark = ","),
                   "\u56fd\u5bb6\u6570", tone = "primary"),
        mod_v3_kpi(fmt_v3_pct(median(d$gghed_che, na.rm = TRUE)),
                   "GGHED/CHE \u4e2d\u4f4d\u6570", tone = "secondary"),
        mod_v3_kpi(fmt_v3_pct(median(d$gghed_gdp, na.rm = TRUE)),
                   "GGHED/GDP \u4e2d\u4f4d\u6570", tone = "good"),
        mod_v3_kpi(as.character(yr), "\u5e74\u4efd", tone = "neutral")
      )
    })

    output$fiscal_scatter <- plotly::renderPlotly({
      m <- enriched(); yr <- input$year
      d <- m[m$year == yr & is.finite(m$gghed_gdp) & is.finite(m$gghed_che) &
             !is.na(m[[input$group_by]]), ]
      shiny::req(nrow(d) > 5)
      safe_plotly({
        plotly::plot_ly(d, x = ~gghed_gdp, y = ~gghed_che,
                        color = stats::as.formula(paste0("~", input$group_by)),
                        text = ~country_name, type = "scatter", mode = "markers",
                        colors = if (input$group_by == "income_group")
                          unname(brand_palette$income) else unname(brand_palette$continent),
                        marker = list(size = 9, opacity = 0.7)) |>
          ghs_plotly_layout() |>
          plotly::layout(
            xaxis = list(title = "GGHE-D / GDP (%)"),
            yaxis = list(title = "GGHE-D / CHE (%)"),
            legend = list(orientation = "h", y = -0.15)
          )
      })
    })

    output$gghed_gdp_trend <- plotly::renderPlotly({
      m <- enriched()
      grp <- input$group_by
      d <- m[is.finite(m$gghed_gdp) & !is.na(m[[grp]]), ]
      agg <- stats::aggregate(
        stats::as.formula(paste("gghed_gdp ~ year +", grp)),
        data = d, FUN = mean, na.rm = TRUE
      )
      safe_plotly({
        plotly::plot_ly(agg, x = ~year,
                        y = ~gghed_gdp,
                        color = stats::as.formula(paste0("~", grp)),
                        type = "scatter", mode = "lines+markers",
                        colors = if (grp == "income_group")
                          unname(brand_palette$income) else unname(brand_palette$continent)) |>
          ghs_plotly_layout() |>
          plotly::layout(
            xaxis = list(title = ""),
            yaxis = list(title = "GGHE-D / GDP (%)"),
            legend = list(orientation = "h", y = -0.18)
          )
      })
    })

    output$priority_box <- plotly::renderPlotly({
      m <- enriched(); yr <- input$year
      grp <- input$group_by
      d <- m[m$year == yr & is.finite(m$gghed_che) & !is.na(m[[grp]]), ]
      shiny::req(nrow(d) > 5)
      safe_plotly({
        plotly::plot_ly(d, x = stats::as.formula(paste0("~", grp)),
                        y = ~gghed_che, type = "box",
                        color = stats::as.formula(paste0("~", grp)),
                        colors = if (grp == "income_group")
                          unname(brand_palette$income) else unname(brand_palette$continent)) |>
          ghs_plotly_layout() |>
          plotly::layout(
            showlegend = FALSE,
            xaxis = list(title = ""),
            yaxis = list(title = "GGHE-D / CHE (%)"),
            shapes = list(
              list(type = "line", x0 = -0.5, x1 = 10, y0 = 50, y1 = 50,
                   line = list(color = "#a23b3b", width = 1.5, dash = "dot"))
            )
          )
      })
    })

    output$quadrant_chart <- plotly::renderPlotly({
      m <- enriched(); yr <- input$year
      d <- m[m$year == yr & is.finite(m$gghed_gdp) & is.finite(m$gghed_che), ]
      shiny::req(nrow(d) > 5)
      med_gdp <- median(d$gghed_gdp, na.rm = TRUE)
      med_che <- median(d$gghed_che, na.rm = TRUE)
      d$quadrant <- ifelse(d$gghed_gdp >= med_gdp & d$gghed_che >= med_che, "\u5145\u8db3\u578b",
                    ifelse(d$gghed_gdp >= med_gdp & d$gghed_che < med_che, "\u4f18\u5148\u7ea7\u4e0d\u8db3",
                    ifelse(d$gghed_gdp < med_gdp & d$gghed_che >= med_che, "\u8d22\u653f\u7ea6\u675f",
                           "\u53cc\u91cd\u56f0\u5883")))
      quad_colors <- c("\u5145\u8db3\u578b" = "#2a857a", "\u4f18\u5148\u7ea7\u4e0d\u8db3" = "#c89a3b",
                       "\u8d22\u653f\u7ea6\u675f" = "#1d3f5f", "\u53cc\u91cd\u56f0\u5883" = "#a23b3b")
      safe_plotly({
        plotly::plot_ly(d, x = ~gghed_gdp, y = ~gghed_che,
                        color = ~quadrant, text = ~country_name,
                        type = "scatter", mode = "markers",
                        colors = quad_colors,
                        marker = list(size = 8, opacity = 0.75)) |>
          ghs_plotly_layout() |>
          plotly::layout(
            xaxis = list(title = "GGHE-D / GDP (%)"),
            yaxis = list(title = "GGHE-D / CHE (%)"),
            shapes = list(
              list(type = "line", x0 = med_gdp, x1 = med_gdp, y0 = 0, y1 = 100,
                   line = list(color = "#5d667a", width = 1, dash = "dash")),
              list(type = "line", x0 = 0, x1 = 20, y0 = med_che, y1 = med_che,
                   line = list(color = "#5d667a", width = 1, dash = "dash"))
            ),
            legend = list(orientation = "h", y = -0.15)
          )
      })
    })

    output$fiscal_map <- leaflet::renderLeaflet({
      m <- enriched(); yr <- input$year
      if (is.null(world_sf_obj)) {
        return(leaflet::leaflet() |> leaflet::addProviderTiles("CartoDB.Positron"))
      }
      tryCatch(
        leaflet_choropleth(m, world_sf_obj, indicator_col = "gghed_gdp",
                           year_focus = yr,
                           title = sprintf("GGHE-D/GDP %% \u00b7 %d", yr)),
        error = function(e) {
          leaflet::leaflet() |> leaflet::addProviderTiles("CartoDB.Positron")
        }
      )
    })

    output$low_priority <- plotly::renderPlotly({
      m <- enriched(); yr <- input$year
      d <- m[m$year == yr & is.finite(m$gghed_che), ]
      d <- d[order(d$gghed_che), ]
      bottom <- utils::head(d, 15)
      bottom$country_name <- factor(bottom$country_name,
                                     levels = rev(bottom$country_name))
      safe_plotly({
        plotly::plot_ly(bottom, x = ~gghed_che, y = ~country_name,
                        type = "bar", orientation = "h",
                        marker = list(color = "#a23b3b")) |>
          ghs_plotly_layout() |>
          plotly::layout(
            xaxis = list(title = "GGHE-D / CHE (%)"),
            yaxis = list(title = ""),
            margin = list(l = 120)
          )
      })
    })

    output$fiscal_table <- reactable::renderReactable({
      m <- enriched(); yr <- input$year
      d <- m[m$year == yr & is.finite(m$gghed_che), ]
      d <- d[order(-d$gghed_gdp), ]
      tab <- data.frame(
        "\u56fd\u5bb6" = d$country_name,
        "\u6536\u5165\u7ec4" = d$income_group,
        "GGHED/CHE%" = round(d$gghed_che, 1),
        "GGHED/GDP%" = round(d$gghed_gdp, 2),
        "CHE/cap" = round(d$che_pc_usd2023, 0),
        "OOPS%" = round(d$hf3_che, 1),
        check.names = FALSE
      )
      reactable::reactable(tab, defaultPageSize = 15, searchable = TRUE,
        highlight = TRUE, striped = TRUE,
        defaultColDef = reactable::colDef(
          headerStyle = list(background = "#f1f3f7", fontWeight = 700)))
    })
  })
}
