# =============================================================================
# 仪表盘/模块/mod_sdg.R
# SDG-3 健康目标追踪：U5MR、MMR、UHC 覆盖率与卫生支出的关联
# =============================================================================

mod_sdg_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = htmltools::HTML("&#127919; SDG-3"),
    value = "sdg",
    mod_v3_hero(
      kicker = "SDG-3 HEALTH TARGETS",
      title = "SDG-3 \u5065\u5eb7\u76ee\u6807\u8ffd\u8e2a",
      lead = paste(
        "\u8054\u5408\u56fd\u53ef\u6301\u7eed\u53d1\u5c55\u76ee\u6807 3 \u7684\u5173\u952e\u6307\u6807\u4e0e\u536b\u751f\u652f\u51fa\u7684\u5173\u8054\u3002",
        "\u5305\u542b U5MR \u4e0b\u964d\u8f68\u8ff9\u3001\u9884\u671f\u5bff\u547d\u589e\u957f\u3001\u4eba\u5747 CHE \u5f39\u6027\u3002"
      ),
      meta = list("WHO GHED + WDI", "U5MR / Life expectancy", "2000\u20132023")
    ),
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 260,
        shiny::selectInput(ns("outcome"), "\u5065\u5eb7\u4ea7\u51fa\u6307\u6807",
          choices = c(
            "U5MR (\u2030)" = "u5mr",
            "\u9884\u671f\u5bff\u547d (\u5c81)" = "life_exp"
          ), selected = "u5mr"),
        shiny::sliderInput(ns("year"), "\u5e74\u4efd",
          min = 2000, max = 2023, value = 2023, step = 1, sep = "",
          animate = shiny::animationOptions(interval = 600)),
        shiny::selectInput(ns("color_by"), "\u7740\u8272\u7ef4\u5ea6",
          choices = c("\u5927\u6d32" = "continent", "\u6536\u5165\u7ec4" = "income_group"),
          selected = "continent"),
        shiny::tags$hr(),
        shiny::helpText(
          "U5MR = \u4e94\u5c81\u4ee5\u4e0b\u6b7b\u4ea1\u7387\uff08\u6bcf\u5343\u6d3b\u4ea7\uff09\u3002",
          "\u6c14\u6ce1\u5927\u5c0f = \u4eba\u53e3\u89c4\u6a21\u3002"
        )
      ),
      shiny::uiOutput(ns("kpi_strip")),
      bslib::layout_columns(
        col_widths = c(7, 5),
        mod_card(
          title = "\u4eba\u5747 CHE \u4e0e\u5065\u5eb7\u4ea7\u51fa",
          htmltools::p(class = "card-note",
            "\u6a2a\u8f74 = \u4eba\u5747 CHE\uff08\u5bf9\u6570\uff09\uff0c\u7eb5\u8f74 = \u5065\u5eb7\u4ea7\u51fa\u6307\u6807\u3002"),
          mod_spinner(plotly::plotlyOutput(ns("scatter_main"), height = 480))
        ),
        mod_card(
          title = "\u5168\u7403\u8d8b\u52bf",
          htmltools::p(class = "card-note",
            "\u5168\u7403\u5747\u503c\u7684 24 \u5e74\u8d8b\u52bf\u3002"),
          mod_spinner(plotly::plotlyOutput(ns("trend_global"), height = 480))
        )
      ),
      bslib::layout_columns(
        col_widths = c(6, 6),
        mod_card(
          title = "\u6309\u6536\u5165\u7ec4\u5206\u5e03",
          mod_spinner(plotly::plotlyOutput(ns("box_income"), height = 380))
        ),
        mod_card(
          title = "\u6539\u5584\u6700\u5927\u7684 15 \u56fd",
          mod_spinner(plotly::plotlyOutput(ns("top_improvers"), height = 380))
        )
      ),
      mod_card(
        title = "\u56fd\u5bb6\u660e\u7ec6\u8868",
        mod_spinner(reactable::reactableOutput(ns("detail_table")))
      )
    )
  )
}

mod_sdg_server <- function(id, master_r) {
  shiny::moduleServer(id, function(input, output, session) {

    output$kpi_strip <- shiny::renderUI({
      m <- master_r(); yr <- input$year
      d <- m[m$year == yr, ]
      outcome <- input$outcome
      val <- median(d[[outcome]], na.rm = TRUE)
      n <- sum(is.finite(d[[outcome]]))
      mod_v3_kpi_grid(
        mod_v3_kpi(format(n, big.mark = ","), "\u56fd\u5bb6\u6570", tone = "primary"),
        mod_v3_kpi(sprintf("%.1f", val),
                   if (outcome == "u5mr") "U5MR \u4e2d\u4f4d\u6570 (\u2030)" else "\u5bff\u547d\u4e2d\u4f4d\u6570",
                   tone = if (outcome == "u5mr") "bad" else "good"),
        mod_v3_kpi(as.character(yr), "\u5e74\u4efd", tone = "neutral"),
        mod_v3_kpi(sprintf("%.0f", median(d$che_pc_usd2023, na.rm = TRUE)),
                   "\u4eba\u5747 CHE \u4e2d\u4f4d", tone = "secondary")
      )
    })

    output$scatter_main <- plotly::renderPlotly({
      m <- master_r(); yr <- input$year; outcome <- input$outcome; color_by <- input$color_by
      d <- m[m$year == yr & is.finite(m$che_pc_usd2023) & is.finite(m[[outcome]]) &
             !is.na(m[[color_by]]), ]
      shiny::req(nrow(d) > 10)
      pop_vals <- if ("pop" %in% names(d) && any(is.finite(d$pop))) d$pop else rep(1e6, nrow(d))
      colors <- if (color_by == "continent") unname(brand_palette$continent) else unname(brand_palette$income)
      safe_plotly({
        plotly::plot_ly(d, x = ~che_pc_usd2023, y = stats::as.formula(paste0("~", outcome)),
                        color = stats::as.formula(paste0("~", color_by)),
                        size = pop_vals, text = ~country_name,
                        type = "scatter", mode = "markers", colors = colors,
                        marker = list(opacity = 0.7, sizemode = "area",
                                      sizeref = 2 * max(pop_vals, na.rm = TRUE) / 40^2)) |>
          ghs_plotly_layout() |>
          plotly::layout(
            xaxis = list(title = "\u4eba\u5747 CHE (USD)", type = "log"),
            yaxis = list(title = outcome,
                         autorange = if (outcome == "u5mr") "reversed" else TRUE)
          )
      })
    })

    output$trend_global <- plotly::renderPlotly({
      m <- master_r(); outcome <- input$outcome
      d <- m[is.finite(m[[outcome]]), ]
      agg <- stats::aggregate(stats::as.formula(paste(outcome, "~ year")), data = d, FUN = median)
      safe_plotly({
        plotly::plot_ly(agg, x = ~year, y = stats::as.formula(paste0("~", outcome)),
                        type = "scatter", mode = "lines+markers",
                        line = list(color = "#1d3f5f", width = 3),
                        marker = list(color = "#1d3f5f", size = 6)) |>
          ghs_plotly_layout() |>
          plotly::layout(xaxis = list(title = ""), yaxis = list(title = outcome))
      })
    })

    output$box_income <- plotly::renderPlotly({
      m <- master_r(); yr <- input$year; outcome <- input$outcome
      d <- m[m$year == yr & is.finite(m[[outcome]]) & !is.na(m$income_group), ]
      safe_plotly({
        plotly::plot_ly(d, x = ~income_group, y = stats::as.formula(paste0("~", outcome)),
                        type = "box", color = ~income_group,
                        colors = unname(brand_palette$income)) |>
          ghs_plotly_layout() |>
          plotly::layout(showlegend = FALSE, xaxis = list(title = ""), yaxis = list(title = outcome))
      })
    })

    output$top_improvers <- plotly::renderPlotly({
      m <- master_r(); outcome <- input$outcome
      yr_a <- min(m$year, na.rm = TRUE); yr_b <- max(m$year, na.rm = TRUE)
      da <- m[m$year == yr_a & is.finite(m[[outcome]]), c("iso3_code", "country_name", outcome)]
      db <- m[m$year == yr_b & is.finite(m[[outcome]]), c("iso3_code", outcome)]
      names(da)[3] <- "val_a"; names(db)[2] <- "val_b"
      mg <- merge(da, db, by = "iso3_code")
      mg$change <- if (outcome == "u5mr") mg$val_a - mg$val_b else mg$val_b - mg$val_a
      mg <- utils::head(mg[order(-mg$change), ], 15)
      mg$country_name <- factor(mg$country_name, levels = rev(mg$country_name))
      safe_plotly({
        plotly::plot_ly(mg, x = ~change, y = ~country_name, type = "bar",
                        orientation = "h", marker = list(color = "#2a857a")) |>
          ghs_plotly_layout() |>
          plotly::layout(xaxis = list(title = "\u6539\u5584\u5e45\u5ea6"), yaxis = list(title = ""))
      })
    })

    output$detail_table <- reactable::renderReactable({
      m <- master_r(); yr <- input$year; outcome <- input$outcome
      d <- m[m$year == yr & is.finite(m[[outcome]]), ]
      d <- d[order(d[[outcome]], decreasing = (outcome == "life_exp")), ]
      tab <- data.frame(
        country = d$country_name,
        value = round(d[[outcome]], 1),
        che_pc = round(d$che_pc_usd2023, 0),
        gghed = round(d$gghed_che, 1),
        continent = d$continent,
        check.names = FALSE
      )
      names(tab) <- c("Country", outcome, "CHE/cap", "GGHED%", "Continent")
      reactable::reactable(tab, defaultPageSize = 15, searchable = TRUE, highlight = TRUE,
        defaultColDef = reactable::colDef(headerStyle = list(background = "#f1f3f7")))
    })
  })
}
