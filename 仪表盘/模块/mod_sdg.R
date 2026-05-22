# =============================================================================
# 仪表盘/模块/mod_sdg.R
# SDG-3 健康目标追踪：U5MR、MMR、UHC 覆盖率与卫生支出的关联
# =============================================================================

mod_sdg_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = "SDG-3",
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
    mod_v3_page_body(
      wide = TRUE,
      mod_v3_topic_brief("sdg"),
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
          mod_v3_sidebar_note(
            "\u76ee\u6807\u53e3\u5f84",
            "U5MR = \u4e94\u5c81\u4ee5\u4e0b\u6b7b\u4ea1\u7387\uff08\u6bcf\u5343\u6d3b\u4ea7\uff09\uff0c\u6570\u503c\u8d8a\u4f4e\u8d8a\u597d\uff1b\u9884\u671f\u5bff\u547d\u6570\u503c\u8d8a\u9ad8\u8d8a\u597d\u3002\u6c14\u6ce1\u9762\u79ef\u8868\u793a\u4eba\u53e3\u89c4\u6a21\u3002",
            bullets = c(
              "\u652f\u51fa\u4e0e\u7ed3\u679c\u4e0d\u662f\u7ebf\u6027\u4ea4\u6362\uff0c\u540c\u7b49 CHE/cap \u4e0b\u7684\u6b8b\u5dee\u66f4\u503c\u5f97\u770b\u3002",
              "\u9009 U5MR \u65f6\u7eb5\u8f74\u53cd\u5411\uff0c\u4f4d\u7f6e\u8d8a\u4e0a\u4ee3\u8868\u6b7b\u4ea1\u7387\u8d8a\u4f4e\u3002",
              "\u6536\u5165\u7ec4\u7740\u8272\u7528\u6765\u533a\u5206\u7ed3\u6784\u68af\u5ea6\uff0c\u5927\u6d32\u7740\u8272\u7528\u6765\u770b\u533a\u57df\u805a\u96c6\u3002"
            )
          )
        ),
        shiny::uiOutput(ns("kpi_strip")),
        bslib::layout_columns(
          col_widths = c(7, 5),
          mod_card(
            kicker = "F1 · Spending-outcome frontier",
            title = "\u4eba\u5747 CHE \u4e0e\u5065\u5eb7\u4ea7\u51fa",
            htmltools::p(class = "card-note",
              "\u6a2a\u8f74 = \u4eba\u5747 CHE\uff08\u5bf9\u6570\uff09\uff0c\u7eb5\u8f74 = \u5065\u5eb7\u4ea7\u51fa\u6307\u6807\u3002"),
            mod_v3_chart_guide(
              "\u5148\u770b\u524d\u6cbf\uff0c\u518d\u770b\u504f\u79bb",
              "\u9ad8\u652f\u51fa\u672a\u5fc5\u5bf9\u5e94\u6700\u4f18\u7ed3\u679c\uff1b\u540c\u6837 CHE/cap \u4e0b\u66f4\u597d\u7684\u56fd\u5bb6\uff0c\u53ef\u80fd\u4ee3\u8868\u66f4\u9ad8\u7684\u521d\u7ea7\u533b\u7597\u6548\u7387\u6216\u66f4\u5f3a\u7684\u516c\u5171\u536b\u751f\u57fa\u7840\u3002",
              bullets = c("\u5bf9\u6570\u6a2a\u8f74\u538b\u7f29\u9ad8\u652f\u51fa\u5c3e\u90e8\u3002", "\u5927\u6c14\u6ce1\u5bf9\u5168\u7403\u5e73\u5747\u5f71\u54cd\u66f4\u5927\uff0c\u4f46\u4e0d\u4ee3\u8868\u6392\u540d\u66f4\u9ad8\u3002")
            ),
            mod_spinner(plotly::plotlyOutput(ns("scatter_main"), height = 480))
          ),
          mod_card(
            kicker = "F2 · Global drift",
            title = "\u5168\u7403\u8d8b\u52bf",
            htmltools::p(class = "card-note",
              "\u5168\u7403\u4e2d\u4f4d\u6570\u7684 24 \u5e74\u8d8b\u52bf\u3002"),
            mod_v3_chart_guide(
              "\u7528\u957f\u8d8b\u52bf\u5224\u65ad SDG \u8fdb\u5ea6",
              "U5MR \u6301\u7eed\u4e0b\u884c\u6216\u5bff\u547d\u6301\u7eed\u4e0a\u884c\u8868\u793a\u5168\u7403\u5178\u578b\u56fd\u5bb6\u5728\u6539\u5584\uff1b\u5982\u679c\u8d8b\u52bf\u653e\u7f13\uff0c\u9700\u8981\u7ed3\u5408\u6536\u5165\u7ec4\u5206\u5e03\u627e\u5230\u74f6\u9888\u3002",
              tone = "good"
            ),
            mod_spinner(plotly::plotlyOutput(ns("trend_global"), height = 480)),
            footer = "\u8d8b\u52bf\u56fe\u4f7f\u7528\u5168\u7403\u4e2d\u4f4d\u6570\uff0c\u76f8\u6bd4\u5747\u503c\u66f4\u4e0d\u5bb9\u6613\u88ab\u5927\u56fd\u6216\u6781\u7aef\u503c\u4e3b\u5bfc\u3002"
          )
        ),
        bslib::layout_columns(
          col_widths = c(6, 6),
          mod_card(
            kicker = "F3 · Income spread",
            title = "\u6309\u6536\u5165\u7ec4\u5206\u5e03",
            mod_v3_chart_guide(
              "\u7bb1\u7ebf\u56fe\u8bf4\u660e\u7ec4\u5185\u5dee\u8ddd",
              "\u540c\u4e00\u6536\u5165\u7ec4\u5185\u4ecd\u53ef\u80fd\u5b58\u5728\u663e\u8457\u5dee\u5f02\uff1b\u7bb1\u4f53\u8d8a\u9ad8\uff0c\u8bf4\u660e\u8be5\u7ec4\u7684\u7ed3\u679c\u5206\u5e03\u8d8a\u4e0d\u5747\u8861\u3002",
              tone = "warn"
            ),
            mod_spinner(plotly::plotlyOutput(ns("box_income"), height = 380))
          ),
          mod_card(
            kicker = "F4 · Fast movers",
            title = "\u6539\u5584\u6700\u5927\u7684 15 \u56fd",
            mod_v3_chart_guide(
              "\u6392\u884c\u7528\u4e8e\u627e\u6539\u5584\u6837\u672c",
              "\u8fd9\u91cc\u6bd4\u8f83\u671f\u521d\u4e0e\u671f\u672b\u7684\u7ed3\u679c\u6539\u5584\u5e45\u5ea6\uff0c\u9002\u5408\u6311\u51fa\u53ef\u7ee7\u7eed\u505a\u56fd\u5bb6\u4e2a\u6848\u7684\u5feb\u901f\u8fdb\u6b65\u8005\u3002"
            ),
            mod_spinner(plotly::plotlyOutput(ns("top_improvers"), height = 380))
          )
        ),
        mod_card(
          kicker = "F5 \u00b7 \u56fd\u5bb6\u660e\u7ec6",
          title = "\u56fd\u5bb6\u660e\u7ec6\u8868",
          mod_v3_chart_guide(
            "\u8868\u683c\u8d1f\u8d23\u628a\u70b9\u4f4d\u843d\u56de\u56fd\u5bb6",
            "\u5bf9\u6563\u70b9\u56fe\u4e2d\u7684\u9ad8\u6548\u6216\u4f4e\u6548\u56fd\u5bb6\uff0c\u53ef\u5728\u8868\u683c\u4e2d\u6838\u5bf9 CHE/cap\u3001GGHE-D \u548c\u5927\u6d32\u80cc\u666f\u3002"
          ),
          mod_spinner(reactable::reactableOutput(ns("detail_table"))),
          footer = "U5MR \u7684\u6539\u5584\u5e45\u5ea6\u6309\u4e0b\u964d\u503c\u8ba1\u7b97\uff1b\u9884\u671f\u5bff\u547d\u6309\u4e0a\u5347\u503c\u8ba1\u7b97\u3002"
        )
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
        "\u56fd\u5bb6" = d$country_name,
        "\u6307\u6807\u503c" = round(d[[outcome]], 1),
        "\u4eba\u5747 CHE" = round(d$che_pc_usd2023, 0),
        "GGHE-D%" = round(d$gghed_che, 1),
        "\u5927\u6d32" = d$continent,
        check.names = FALSE
      )
      reactable::reactable(tab, defaultPageSize = 15, searchable = TRUE, highlight = TRUE,
        defaultColDef = reactable::colDef(headerStyle = list(background = "#f1f3f7")))
    })
  })
}
