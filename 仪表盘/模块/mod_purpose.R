
mod_purpose_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = "\u7528\u9014 Purpose",
    value = "purpose",
    mod_v3_hero(
      kicker = "SPENDING BY PURPOSE (HC)",
      title = "\u536b\u751f\u652f\u51fa\u529f\u80fd\u5206\u7c7b",
      lead = paste(
        "HC1 \u6cbb\u7597\u3001HC6 \u9884\u9632\u3001HC7 \u6cbb\u7406\u7b49 9 \u5927\u7c7b\u652f\u51fa\u7528\u9014\u7684\u7ed3\u6784\u5206\u6790\u3002",
        "\u6cbb\u7597\u652f\u51fa\u5360\u6bd4\u901a\u5e38\u8d85\u8fc7 60%\uff0c\u9884\u9632\u4ec5 3\u20135%\u3002"
      ),
      meta = list("HC1\u2013HC9", "WHO GHED", "195 \u56fd\u5bb6")
    ),
    mod_v3_page_body(
      wide = TRUE,
      mod_v3_topic_brief("purpose"),
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
          mod_v3_sidebar_note(
            "HC \u529f\u80fd\u53e3\u5f84",
            "HC1 \u4ee3\u8868\u6cbb\u7597\u6027\u62a4\u7406\uff0cHC6 \u4ee3\u8868\u9884\u9632\u6027\u62a4\u7406\u3002\u672c\u9875\u5173\u6ce8\u536b\u751f\u652f\u51fa\u4ece\u201c\u8d44\u91d1\u6765\u6e90\u201d\u8f6c\u5230\u201c\u670d\u52a1\u7528\u9014\u201d\u540e\u5448\u73b0\u7684\u7ed3\u6784\u3002",
            bullets = c(
              "\u5404 HC \u6307\u6807\u4e3a\u5360 CHE \u6bd4\u4f8b\uff0c\u9002\u5408\u6bd4\u8f83\u529f\u80fd\u91cd\u5fc3\u3002",
              "\u6cbb\u7597\u5360\u6bd4\u9ad8\u4e0d\u4e00\u5b9a\u7b49\u4e8e\u4f4e\u6548\uff0c\u9700\u8981\u7ed3\u5408\u75be\u75c5\u8d1f\u62c5\u548c\u670d\u52a1\u4ef7\u683c\u3002",
              "\u9884\u9632\u5360\u6bd4\u957f\u671f\u504f\u4f4e\u65f6\uff0c\u66f4\u9002\u5408\u8fdb\u5165\u9884\u9632\u6a21\u5757\u7ee7\u7eed\u62c6\u89e3\u3002"
            )
          )
        ),
      shiny::uiOutput(ns("kpi_strip")),
      bslib::layout_columns(
        col_widths = c(7, 5),
        mod_card(
          kicker = "U1 \u00b7 Functional mix",
          title = "\u652f\u51fa\u7528\u9014\u7ed3\u6784\uff08\u5168\u7403\u5747\u503c\uff09",
          htmltools::p(class = "card-note",
            "HC1 \u6cbb\u7597\u3001HC6 \u9884\u9632\u7b49\u5404\u7c7b\u5360 CHE \u6bd4\u91cd\u3002"),
          mod_v3_chart_guide(
            "\u7528\u9014\u7ed3\u6784\u5148\u770b\u4e3b\u5bfc\u9879",
            "\u5982\u679c HC1 \u660e\u663e\u9ad8\u4e8e HC6\uff0c\u8bf4\u660e\u536b\u751f\u652f\u51fa\u66f4\u96c6\u4e2d\u5728\u6cbb\u7597\u6027\u670d\u52a1\uff1b\u8fd9\u4e0d\u76f4\u63a5\u7b49\u4e8e\u95ee\u9898\uff0c\u4f46\u4f1a\u63d0\u9192\u6211\u4eec\u7ee7\u7eed\u68c0\u67e5\u9884\u9632\u548c\u521d\u7ea7\u536b\u751f\u80fd\u529b\u3002",
            bullets = c("\u672c\u56fe\u5f3a\u8c03\u529f\u80fd\u9879\u76ee\u7684\u76f8\u5bf9\u6743\u91cd\u3002", "\u5e73\u5747\u503c\u4e0d\u4ee3\u8868\u56fd\u5bb6\u7ea7\u5dee\u5f02\uff0c\u9700\u8981\u548c\u5206\u7ec4\u56fe\u5bf9\u7167\u3002")
          ),
          mod_spinner(plotly::plotlyOutput(ns("purpose_bar"), height = 440)),
          footer = "\u5f53\u524d\u5b9e\u73b0\u805a\u7126 HC1 \u4e0e HC6 \u4e24\u4e2a\u4ee3\u8868\u6027\u529f\u80fd\u9879\u3002"
        ),
        mod_card(
          kicker = "U2 \u00b7 Treatment-prevention balance",
          title = "HC1 \u6cbb\u7597 vs HC6 \u9884\u9632\u6563\u70b9",
          htmltools::p(class = "card-note",
            "\u6a2a\u8f74 = \u6cbb\u7597\u5360\u6bd4\uff0c\u7eb5\u8f74 = \u9884\u9632\u5360\u6bd4\u3002\u53f3\u4e0b = \u91cd\u6cbb\u7597\u8f7b\u9884\u9632\u3002"),
          mod_v3_chart_guide(
            "\u6563\u70b9\u5e2e\u52a9\u8bc6\u522b\u529f\u80fd\u7ed3\u6784\u7684\u504f\u5411",
            "\u53f3\u4e0b\u8c61\u9650\u5f80\u5f80\u8868\u793a\u652f\u51fa\u66f4\u504f\u540e\u7aef\u6cbb\u7597\uff1b\u5de6\u4e0a\u533a\u57df\u5219\u63d0\u793a\u9884\u9632\u5360\u6bd4\u66f4\u7a81\u51fa\u3002\u540c\u7ec4\u5185\u7684\u504f\u79bb\u70b9\u503c\u5f97\u5f53\u6210\u6848\u4f8b\u770b\u3002",
            tone = "warn"
          ),
          mod_spinner(plotly::plotlyOutput(ns("hc1_hc6_scatter"), height = 440))
        )
      ),
      bslib::layout_columns(
        col_widths = c(6, 6),
        mod_card(
          kicker = "U3 \u00b7 Group contrast",
          title = "\u7528\u9014\u7ed3\u6784\u6309\u7ec4\u5bf9\u6bd4",
          htmltools::p(class = "card-note",
            "\u5404\u7ec4\u7684 HC1/HC6 \u5360\u6bd4\u5bf9\u6bd4\u3002"),
          mod_v3_chart_guide(
            "\u5206\u7ec4\u56fe\u628a\u529f\u80fd\u7ed3\u6784\u548c\u53d1\u5c55\u9636\u6bb5\u8fde\u8d77\u6765",
            "\u6536\u5165\u7ec4\u6216\u5927\u6d32\u95f4\u7684 HC1/HC6 \u5dee\u5f02\uff0c\u5e38\u5e38\u540c\u65f6\u53cd\u6620\u75be\u75c5\u8d1f\u62c5\u3001\u652f\u4ed8\u5236\u5ea6\u548c\u516c\u5171\u536b\u751f\u80fd\u529b\u3002",
            tone = "good"
          ),
          mod_spinner(plotly::plotlyOutput(ns("purpose_by_group"), height = 380)),
          footer = "\u67f1\u5f62\u4e3a\u7ec4\u5185\u56fd\u5bb6\u5747\u503c\uff0c\u4e0d\u662f\u4eba\u53e3\u6216 CHE \u52a0\u6743\u503c\u3002"
        ),
        mod_card(
          kicker = "U4 \u00b7 Prevention path",
          title = "HC6 \u9884\u9632\u5360\u6bd4\u8d8b\u52bf",
          htmltools::p(class = "card-note",
            "\u5404\u7ec4\u9884\u9632\u652f\u51fa\u5360\u6bd4\u7684\u65f6\u5e8f\u53d8\u5316\u3002"),
          mod_v3_chart_guide(
            "\u8d8b\u52bf\u56fe\u7528\u6765\u533a\u5206\u7ed3\u6784\u6027\u8f6c\u5411\u548c\u5355\u5e74\u9879\u76ee\u51b2\u51fb",
            "\u5982\u679c\u9884\u9632\u5360\u6bd4\u5728\u591a\u5e74\u5185\u7a33\u5b9a\u4e0a\u884c\uff0c\u66f4\u53ef\u80fd\u662f\u670d\u52a1\u7ed3\u6784\u6539\u53d8\uff1b\u5982\u679c\u53ea\u6709\u5355\u4e2a\u5e74\u4efd\u8df3\u5347\uff0c\u5219\u9700\u8981\u56de\u5230\u6570\u636e\u53e3\u5f84\u6216\u75ab\u60c5\u80cc\u666f\u68c0\u67e5\u3002"
          ),
          mod_spinner(plotly::plotlyOutput(ns("hc6_trend"), height = 380))
        )
      ),
      mod_card(
        kicker = "U5 \u00b7 Purpose audit",
        title = "\u652f\u51fa\u7528\u9014\u6570\u636e\u8868",
        mod_v3_chart_guide(
          "\u8868\u683c\u7528\u4e8e\u628a\u529f\u80fd\u7ed3\u6784\u843d\u5230\u56fd\u5bb6",
          "\u4ece\u6563\u70b9\u56fe\u6216\u5206\u7ec4\u56fe\u4e2d\u770b\u5230\u7684\u7ed3\u6784\u504f\u79bb\uff0c\u53ef\u4ee5\u5728\u8fd9\u91cc\u540c\u65f6\u6838\u5bf9 HC1\u3001HC6\u3001HC1/HC6 \u548c CHE/cap\u3002"
        ),
        mod_spinner(reactable::reactableOutput(ns("purpose_table"))),
        footer = "\u8868\u683c\u6309 HC6 \u9884\u9632\u5360\u6bd4\u964d\u5e8f\u6392\u5217\u3002"
      )
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
