# =============================================================================
# 仪表盘/模块/mod_purpose.R
# 支出用途：HC1-HC9 卫生支出功能分类的结构与演化
# =============================================================================

mod_purpose_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = htmltools::HTML("&#128202; \u7528\u9014 Purpose"),
    mod_v3_hero(
      kicker = "SPENDING BY PURPOSE (HC)",
      title = "\u536b\u751f\u652f\u51fa\u529f\u80fd\u5206\u7c7b",
      lead = paste(
        "HC1 \u6cbb\u7597\u3001HC6 \u9884\u9632\u3001HC7 \u6cbb\u7406\u7b49 9 \u5927\u7c7b\u652f\u51fa\u7528\u9014\u7684\u7ed3\u6784\u5206\u6790\u3002",
        "\u6cbb\u7597\u652f\u51fa\u5360\u6bd4\u901a\u5e38\u8d85\u8fc7 60%\uff0c\u9884\u9632\u4ec5 3\u20135%\u3002"
      ),
      meta = list("HC1\u2013HC9", "WHO GHED", "195 \u56fd\u5bb6")
    ),
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 260,
        shiny::sliderInput(ns("year"), "\u5e74\u4efd",
          min = 2000, max = 2023, value = 2023, step = 1, sep = "",
          animate = shiny::animationOptions(interval = 900)),
        shiny::selectInput(ns("group_by"), "\u5206\u7ec4",
          choices = c("\u6536\u5165\u7ec4" = "income_group",
                      "\u5927\u6d32" = "continent"),
          selected = "income_group"),
        shiny::tags$hr(),
        shiny::helpText(
          "HC1 = \u6cbb\u7597\u6027\u62a4\u7406\uff0cHC6 = \u9884\u9632\u6027\u62a4\u7406\u3002",
          "\u6570\u636e\u53e3\u5f84\uff1a\u5404 HC \u5360 CHE \u7684\u767e\u5206\u6bd4\u3002"
        )
      ),
      shiny::uiOutput(ns("kpi_strip")),
      # Row 1
      bslib::layout_columns(
        col_widths = c(7, 5),
        mod_card(
          title = "\u652f\u51fa\u7528\u9014\u7ed3\u6784\uff08\u5168\u7403\u5747\u503c\uff09",
          htmltools::p(class = "card-note",
            "HC1 \u6cbb\u7597\u3001HC6 \u9884\u9632\u7b49\u5404\u7c7b\u5360 CHE \u6bd4\u91cd\u3002"),
          mod_spinner(plotly::plotlyOutput(ns("purpose_bar"), height = 440))
        ),
        mod_card(
          title = "HC1 \u6cbb\u7597 vs HC6 \u9884\u9632\u6563\u70b9",
          htmltools::p(class = "card-note",
            "\u6a2a\u8f74 = \u6cbb\u7597\u5360\u6bd4\uff0c\u7eb5\u8f74 = \u9884\u9632\u5360\u6bd4\u3002\u53f3\u4e0b = \u91cd\u6cbb\u7597\u8f7b\u9884\u9632\u3002"),
          mod_spinner(plotly::plotlyOutput(ns("hc1_hc6_scatter"), height = 440))
        )
      ),
      # Row 2
      bslib::layout_columns(
        col_widths = c(6, 6),
        mod_card(
          title = "\u7528\u9014\u7ed3\u6784\u6309\u7ec4\u5bf9\u6bd4",
          htmltools::p(class = "card-note",
            "\u5404\u7ec4\u7684 HC1/HC6 \u5360\u6bd4\u5bf9\u6bd4\u3002"),
          mod_spinner(plotly::plotlyOutput(ns("purpose_by_group"), height = 380))
        ),
        mod_card(
          title = "HC6 \u9884\u9632\u5360\u6bd4\u8d8b\u52bf",
          htmltools::p(class = "card-note",
            "\u5404\u7ec4\u9884\u9632\u652f\u51fa\u5360\u6bd4\u7684\u65f6\u5e8f\u53d8\u5316\u3002"),
          mod_spinner(plotly::plotlyOutput(ns("hc6_trend"), height = 380))
        )
      ),
      mod_card(
        title = "\u652f\u51fa\u7528\u9014\u6570\u636e\u8868",
        mod_spinner(reactable::reactableOutput(ns("purpose_table")))
      )
    )
  )
}

mod_purpose_server <- function(id, master_r) {
  shiny::moduleServer(id, function(input, output, session) {

    output$kpi_strip <- shiny::renderUI({
      m <- master_r(); yr <- input$year
      d <- m[m$year == yr, ]
      mod_v3_kpi_grid(
        mod_v3_kpi(fmt_v3_pct(mean(d$hc1_che, na.rm = TRUE)),
                   "HC1 \u6cbb\u7597\u5747\u503c", tone = "primary"),
        mod_v3_kpi(fmt_v3_pct(mean(d$hc6_che, na.rm = TRUE)),
                   "HC6 \u9884\u9632\u5747\u503c", tone = "good"),
        mod_v3_kpi(sprintf("%.1f", mean(d$hc1_che, na.rm = TRUE) / pmax(mean(d$hc6_che, na.rm = TRUE), 0.1)),
                   "HC1/HC6 \u6bd4\u503c", tone = "warn"),
        mod_v3_kpi(as.character(yr), "\u5e74\u4efd", tone = "neutral")
      )
    })

    output$purpose_bar <- plotly::renderPlotly({
      m <- master_r(); yr <- input$year
      hc_cols <- c("hc1_che", "hc6_che")
      hc_labels <- c("HC1 \u6cbb\u7597", "HC6 \u9884\u9632")
      d <- m[m$year == yr, ]
      vals <- vapply(hc_cols, function(col) mean(d[[col]], na.rm = TRUE), numeric(1))
      df <- data.frame(category = hc_labels, value = vals)
      df <- df[order(-df$value), ]
      df$category <- factor(df$category, levels = rev(df$category))
      safe_plotly({
        plotly::plot_ly(df, x = ~value, y = ~category,
                        type = "bar", orientation = "h",
                        marker = list(color = c("#1d3f5f", "#2a857a")[seq_len(nrow(df))])) |>
          ghs_plotly_layout() |>
          plotly::layout(
            xaxis = list(title = "\u5360 CHE (%)"),
            yaxis = list(title = "")
          )
      })
    })

    output$hc1_hc6_scatter <- plotly::renderPlotly({
      m <- master_r(); yr <- input$year; grp <- input$group_by
      d <- m[m$year == yr & is.finite(m$hc1_che) & is.finite(m$hc6_che) &
             !is.na(m[[grp]]), ]
      shiny::req(nrow(d) > 5)
      pal <- if (grp == "income_group") unname(brand_palette$income)
             else unname(brand_palette$continent)
      safe_plotly({
        plotly::plot_ly(d, x = ~hc1_che, y = ~hc6_che,
                        color = stats::as.formula(paste0("~", grp)),
                        text = ~country_name, type = "scatter", mode = "markers",
                        colors = pal,
                        marker = list(size = 8, opacity = 0.7)) |>
          ghs_plotly_layout() |>
          plotly::layout(
            xaxis = list(title = "HC1 \u6cbb\u7597 / CHE (%)"),
            yaxis = list(title = "HC6 \u9884\u9632 / CHE (%)"),
            legend = list(orientation = "h", y = -0.15)
          )
      })
    })

    output$purpose_by_group <- plotly::renderPlotly({
      m <- master_r(); yr <- input$year; grp <- input$group_by
      d <- m[m$year == yr & is.finite(m$hc1_che) & is.finite(m$hc6_che) &
             !is.na(m[[grp]]), ]
      agg <- stats::aggregate(
        cbind(hc1_che, hc6_che) ~ get(grp),
        data = d, FUN = mean, na.rm = TRUE
      )
      names(agg)[1] <- "group"
      safe_plotly({
        plotly::plot_ly(agg, x = ~group, y = ~hc1_che, type = "bar",
                        name = "HC1 \u6cbb\u7597", marker = list(color = "#1d3f5f")) |>
          plotly::add_trace(y = ~hc6_che, name = "HC6 \u9884\u9632",
                            marker = list(color = "#2a857a")) |>
          ghs_plotly_layout() |>
          plotly::layout(
            barmode = "group",
            xaxis = list(title = ""),
            yaxis = list(title = "\u5360 CHE (%)"),
            legend = list(orientation = "h", y = -0.15)
          )
      })
    })

    output$hc6_trend <- plotly::renderPlotly({
      m <- master_r(); grp <- input$group_by
      d <- m[is.finite(m$hc6_che) & !is.na(m[[grp]]), ]
      agg <- stats::aggregate(
        stats::as.formula(paste("hc6_che ~ year +", grp)),
        data = d, FUN = mean, na.rm = TRUE
      )
      pal <- if (grp == "income_group") unname(brand_palette$income)
             else unname(brand_palette$continent)
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

    output$purpose_table <- reactable::renderReactable({
      m <- master_r(); yr <- input$year
      d <- m[m$year == yr & is.finite(m$hc1_che), ]
      d <- d[order(-d$hc6_che), ]
      tab <- data.frame(
        "\u56fd\u5bb6" = d$country_name,
        "\u6536\u5165\u7ec4" = d$income_group,
        "HC1\u6cbb\u7597%" = round(d$hc1_che, 1),
        "HC6\u9884\u9632%" = round(d$hc6_che, 1),
        "HC1/HC6" = round(d$hc1_che / pmax(d$hc6_che, 0.1), 1),
        "CHE/cap" = round(d$che_pc_usd2023, 0),
        check.names = FALSE
      )
      reactable::reactable(tab, defaultPageSize = 15, searchable = TRUE,
        highlight = TRUE, striped = TRUE,
        defaultColDef = reactable::colDef(
          headerStyle = list(background = "#f1f3f7", fontWeight = 700)))
    })
  })
}
