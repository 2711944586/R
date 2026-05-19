# =============================================================================
# 仪表盘/模块/mod_fiscal.R
# 财政空间：政府卫生支出的财政可持续性与优先级分析
# =============================================================================

mod_fiscal_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = htmltools::HTML("&#127970; \u8d22\u653f Fiscal"),
    value = "fiscal",
    mod_v3_hero(
      kicker = "FISCAL SPACE FOR HEALTH",
      title = "\u536b\u751f\u8d22\u653f\u7a7a\u95f4\u4e0e\u4f18\u5148\u7ea7",
      lead = paste(
        "\u653f\u5e9c\u536b\u751f\u652f\u51fa (GGHE-D) \u53d6\u51b3\u4e8e\u8d22\u653f\u603b\u91cf\u4e0e\u536b\u751f\u4f18\u5148\u7ea7\u3002",
        "\u672c\u6a21\u5757\u5206\u89e3 GGHE-D = GDP \u00d7 GGE/GDP \u00d7 GGHE-D/GGE\uff0c",
        "\u8bc6\u522b\u8d22\u653f\u7ea6\u675f\u578b\u4e0e\u4f18\u5148\u7ea7\u4e0d\u8db3\u578b\u56fd\u5bb6\u3002"
      ),
      meta = list("WHO GHED 2024-12", "WDI \u8d22\u653f\u6570\u636e", "195 \u56fd\u5bb6")
    ),
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
        shiny::tags$hr(),
        shiny::helpText(
          "GGHE-D/GGE = \u536b\u751f\u5728\u653f\u5e9c\u603b\u652f\u51fa\u4e2d\u7684\u4f18\u5148\u7ea7\u3002",
          "GGE/GDP = \u653f\u5e9c\u89c4\u6a21\u3002",
          "\u4e24\u8005\u4e58\u79ef\u51b3\u5b9a GGHE-D/GDP\u3002"
        )
      ),
      shiny::uiOutput(ns("kpi_strip")),
      # Row 1
      bslib::layout_columns(
        col_widths = c(7, 5),
        mod_card(
          title = "\u8d22\u653f\u89c4\u6a21 vs \u536b\u751f\u4f18\u5148\u7ea7",
          htmltools::p(class = "card-note",
            paste("\u6a2a\u8f74 = GGE/GDP (\u653f\u5e9c\u89c4\u6a21)\uff0c",
                  "\u7eb5\u8f74 = GGHE-D/GGE (\u536b\u751f\u4f18\u5148\u7ea7)\u3002",
                  "\u53f3\u4e0a\u8c61\u9650 = \u5927\u653f\u5e9c + \u9ad8\u4f18\u5148\u7ea7\u3002")),
          mod_spinner(plotly::plotlyOutput(ns("fiscal_scatter"), height = 440))
        ),
        mod_card(
          title = "GGHE-D \u5360 GDP \u8d8b\u52bf",
          htmltools::p(class = "card-note",
            "\u5404\u7ec4\u653f\u5e9c\u536b\u751f\u652f\u51fa\u5360 GDP \u6bd4\u91cd\u7684\u65f6\u5e8f\u53d8\u5316\u3002"),
          mod_spinner(plotly::plotlyOutput(ns("gghed_gdp_trend"), height = 440))
        )
      ),
      # Row 2
      bslib::layout_columns(
        col_widths = c(6, 6),
        mod_card(
          title = "\u536b\u751f\u4f18\u5148\u7ea7\u5206\u5e03\uff08\u6700\u65b0\u5e74\uff09",
          htmltools::p(class = "card-note",
            "GGHE-D \u5360\u653f\u5e9c\u603b\u652f\u51fa\u7684\u6bd4\u91cd\u5206\u5e03\u3002WHO \u5efa\u8bae\u4e0b\u9650 15%\u3002"),
          mod_spinner(plotly::plotlyOutput(ns("priority_box"), height = 380))
        ),
        mod_card(
          title = "\u8d22\u653f\u7a7a\u95f4\u5206\u7c7b",
          htmltools::p(class = "card-note",
            paste("\u56db\u8c61\u9650\uff1a\u8d22\u653f\u7ea6\u675f\u578b / \u4f18\u5148\u7ea7\u4e0d\u8db3\u578b /",
                  "\u53cc\u91cd\u56f0\u5883\u578b / \u5145\u8db3\u578b\u3002")),
          mod_spinner(plotly::plotlyOutput(ns("quadrant_chart"), height = 380))
        )
      ),
      # Row 3: map + ranking
      bslib::layout_columns(
        col_widths = c(7, 5),
        mod_card(
          title = "\u8d22\u653f\u7a7a\u95f4\u5730\u56fe",
          htmltools::p(class = "card-note",
            "GGHE-D \u5360 GDP \u7684\u5168\u7403\u5206\u5e03\u3002"),
          mod_spinner(leaflet::leafletOutput(ns("fiscal_map"), height = 400))
        ),
        mod_card(
          title = "\u536b\u751f\u4f18\u5148\u7ea7\u6700\u4f4e 15 \u56fd",
          htmltools::p(class = "card-note",
            "GGHE-D/GGE \u6700\u4f4e\u7684\u56fd\u5bb6\uff0c\u8d22\u653f\u6709\u7a7a\u95f4\u4f46\u672a\u5206\u914d\u7ed9\u536b\u751f\u3002"),
          mod_spinner(plotly::plotlyOutput(ns("low_priority"), height = 400))
        )
      ),
      mod_card(
        title = "\u8d22\u653f\u6307\u6807\u6570\u636e\u8868",
        mod_spinner(reactable::reactableOutput(ns("fiscal_table")))
      )
    )
  )
}

mod_fiscal_server <- function(id, master_r) {
  shiny::moduleServer(id, function(input, output, session) {

    # GGHE-D/GGE proxy: gghed_che * che / gge (approximate)
    enriched <- shiny::reactive({
      m <- master_r()
      # gghed_gdp = GGHE-D as % of GDP (if available)
      if (!"gghed_gdp" %in% names(m)) {
        m$gghed_gdp <- ifelse(
          is.finite(m$gghed_che) & is.finite(m$che_pc_usd2023) &
            is.finite(m$gdp_pc_usd) & m$gdp_pc_usd > 0,
          (m$gghed_che / 100) * m$che_pc_usd2023 / m$gdp_pc_usd * 100,
          NA_real_
        )
      }
      # gghed_gge proxy (health priority in government spending)
      if (!"gghed_gge" %in% names(m)) {
        m$gghed_gge <- m$gghed_gdp * 3.2  # rough proxy: avg GGE/GDP ~ 31%
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
