
mod_regional_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = "\u533a\u57df Regional",
    value = "regional",
    mod_v3_hero(
      kicker = "\u533a\u57df\u5bf9\u6bd4",
      title = "\u533a\u57df\u5bf9\u6bd4\u4e0e\u8d8b\u540c",
      lead = paste(
        "\u6bd4\u8f83 6 \u5927\u6d32\u4e0e 4 \u4e2a\u6536\u5165\u7ec4\u7684\u536b\u751f\u652f\u51fa\u7ed3\u6784\u3001",
        "\u4eba\u5747\u6c34\u5e73\u3001\u8d22\u52a1\u4fdd\u62a4\u4e0e\u5065\u5eb7\u4ea7\u51fa\u5dee\u5f02\u3002"
      ),
      meta = list(
        "\u6570\u636e: WHO GHED 2024-12 + WDI",
        "\u8986\u76d6: 195 \u56fd\u5bb6 \u00b7 2000\u20132023"
      )
    ),
    mod_v3_page_body(
      wide = TRUE,
      mod_v3_topic_brief("regional"),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 260,
          shiny::selectInput(ns("group_by"), "\u5206\u7ec4\u7ef4\u5ea6",
            choices = c("\u5927\u6d32 Continent" = "continent",
                        "\u6536\u5165\u7ec4 Income" = "income_group"),
            selected = "continent"),
          shiny::sliderInput(ns("year_range"), "\u5e74\u4efd\u8303\u56f4",
            min = 2000, max = 2023, value = c(2000, 2023),
            step = 1, sep = ""),
          shiny::selectInput(ns("indicator"), "\u6307\u6807",
            choices = c(
              "\u4eba\u5747 CHE (USD)" = "che_pc_usd2023",
              "OOPS \u5360\u6bd4 (%)" = "hf3_che",
              "\u653f\u5e9c GGHE-D (%)" = "gghed_che",
              "\u5916\u63f4 EXT (%)" = "ext_che",
              "\u9884\u671f\u5bff\u547d" = "life_exp"
            ), selected = "che_pc_usd2023"),
          mod_v3_sidebar_note(
            "\u533a\u57df\u6bd4\u8f83\u8bfb\u6cd5",
            "\u672c\u9875\u5148\u7528\u5206\u7ec4\u8d8b\u52bf\u5efa\u7acb\u5168\u5c40\u5370\u8c61\uff0c\u518d\u7528\u7bb1\u7ebf\u56fe\u3001\u5dee\u8ddd\u6bd4\u503c\u548c\u5730\u56fe\u68c0\u67e5\u7ec4\u5185\u5f02\u8d28\u6027\u3002",
            bullets = c(
              "\u5927\u6d32\u66f4\u9002\u5408\u770b\u7a7a\u95f4\u96c6\u7fa4\uff0c\u6536\u5165\u7ec4\u66f4\u9002\u5408\u770b\u53d1\u5c55\u9636\u6bb5\u3002",
              "\u8d8b\u52bf\u7ebf\u4e3a\u7ec4\u5185\u56fd\u5bb6\u5747\u503c\uff0c\u7bb1\u7ebf\u56fe\u624d\u4f1a\u66b4\u9732\u7ec4\u5185\u5dee\u5f02\u3002",
              "\u7ed3\u6784\u5806\u53e0\u56fe\u56fa\u5b9a\u5c55\u793a GGHE-D/PVT-D/EXT\uff0c\u7528\u6765\u5bf9\u7167\u7b79\u8d44\u5206\u62c5\u3002"
            )
          )
        ),
      shiny::uiOutput(ns("kpi_strip")),
      bslib::layout_columns(
        col_widths = c(7, 5),
        mod_card(
          kicker = "Z1 \u00b7 Group trajectory",
          title = "\u5206\u7ec4\u8d8b\u52bf\u6f14\u5316",
          htmltools::p(class = "card-note",
            "\u5404\u7ec4\u56fd\u5bb6\u5747\u503c\u7684\u65f6\u5e8f\u53d8\u5316\u3002"),
          mod_v3_chart_guide(
            "\u8d8b\u52bf\u5148\u56de\u7b54\u7ec4\u4e0e\u7ec4\u662f\u5426\u5728\u6536\u655b",
            "\u7ebf\u8ddd\u9010\u6e10\u53d8\u5c0f\u8bf4\u660e\u533a\u57df\u6216\u6536\u5165\u7ec4\u4e4b\u95f4\u5dee\u5f02\u5728\u6536\u7a84\uff1b\u7ebf\u8ddd\u62c9\u5f00\u5219\u63d0\u793a\u5206\u5316\u52a0\u6df1\u3002",
            bullets = c("\u5982\u679c\u6307\u6807\u662f CHE/cap\uff0c\u8bf7\u540c\u65f6\u8003\u8651\u4ef7\u683c\u548c\u6536\u5165\u5dee\u5f02\u3002", "\u5982\u679c\u6307\u6807\u662f OOPS\uff0c\u4e0a\u884c\u901a\u5e38\u610f\u5473\u5bb6\u5ead\u538b\u529b\u52a0\u91cd\u3002")
          ),
          mod_spinner(plotly::plotlyOutput(ns("trend_line"), height = 400)),
          footer = "\u8d8b\u52bf\u56fe\u4f7f\u7528\u7ec4\u5185\u56fd\u5bb6\u5747\u503c\uff0c\u7528\u4e8e\u6bd4\u8f83\u65b9\u5411\u800c\u975e\u7cbe\u786e\u4f30\u8ba1\u4eba\u53e3\u52a0\u6743\u5747\u503c\u3002"
        ),
        mod_card(
          kicker = "Z2 \u00b7 Within-group spread",
          title = "\u7ec4\u5185\u5206\u5e03\uff08\u6700\u65b0\u5e74\uff09",
          htmltools::p(class = "card-note",
            "\u7bb1\u7ebf + \u6563\u70b9\u5c55\u793a\u7ec4\u5185\u56fd\u5bb6\u7684\u5206\u5e03\u4e0e\u79bb\u7fa4\u503c\u3002"),
          mod_v3_chart_guide(
            "\u7bb1\u7ebf\u56fe\u662f\u9632\u6b62\u5e73\u5747\u503c\u8bef\u5bfc\u7684\u5de5\u5177",
            "\u540c\u4e00\u7ec4\u5185\u5982\u679c\u7bb1\u4f53\u5f88\u9ad8\u6216\u79bb\u7fa4\u70b9\u5f88\u591a\uff0c\u8bf4\u660e\u8be5\u7ec4\u4e0d\u5e94\u88ab\u4e00\u4e2a\u5747\u503c\u6982\u62ec\u3002",
            tone = "warn"
          ),
          mod_spinner(plotly::plotlyOutput(ns("box_latest"), height = 400))
        )
      ),
      bslib::layout_columns(
        col_widths = c(6, 6),
        mod_card(
          kicker = "Z3 \u00b7 Gap ratio",
          title = "\u7ec4\u95f4\u5dee\u8ddd\u6f14\u5316",
          htmltools::p(class = "card-note",
            "\u6700\u9ad8\u7ec4\u4e0e\u6700\u4f4e\u7ec4\u7684\u6bd4\u503c\u968f\u65f6\u95f4\u53d8\u5316\u3002"),
          mod_v3_chart_guide(
            "\u6bd4\u503c\u628a\u7ec4\u95f4\u5dee\u8ddd\u538b\u7f29\u6210\u4e00\u6761\u7ebf",
            "\u6bd4\u503c\u4e0a\u5347\u8868\u793a\u6700\u9ad8\u7ec4\u548c\u6700\u4f4e\u7ec4\u4e4b\u95f4\u7684\u76f8\u5bf9\u5dee\u8ddd\u6269\u5927\uff1b\u6bd4\u503c\u4e0b\u964d\u5219\u66f4\u63a5\u8fd1\u6536\u655b\u3002",
            tone = "good"
          ),
          mod_spinner(plotly::plotlyOutput(ns("gap_ratio"), height = 360)),
          footer = "\u6bd4\u503c\u5bf9\u6781\u5c0f\u5206\u6bcd\u654f\u611f\uff0c\u4f4e\u503c\u7ec4\u63a5\u8fd1 0 \u65f6\u9700\u8981\u8c28\u614e\u89e3\u91ca\u3002"
        ),
        mod_card(
          kicker = "Z4 \u00b7 Financing mix",
          title = "\u7ed3\u6784\u5806\u53e0\u5bf9\u6bd4",
          htmltools::p(class = "card-note",
            "\u5404\u7ec4\u7684\u653f\u5e9c/\u79c1\u4eba/\u5916\u63f4\u4e09\u6e90\u7ed3\u6784\u5bf9\u6bd4\u3002"),
          mod_v3_chart_guide(
            "\u7ed3\u6784\u5806\u53e0\u8bfb\u7684\u662f\u98ce\u9669\u7531\u8c01\u627f\u62c5",
            "GGHE-D \u5360\u6bd4\u9ad8\u901a\u5e38\u8868\u793a\u516c\u5171\u5206\u62c5\u66f4\u5f3a\uff1bPVT-D \u548c EXT \u5360\u6bd4\u9ad8\u5219\u9700\u8981\u5206\u522b\u68c0\u67e5\u5bb6\u5ead/\u79c1\u4eba\u652f\u4ed8\u548c\u5916\u63f4\u4f9d\u8d56\u3002",
            tone = "warn"
          ),
          mod_spinner(plotly::plotlyOutput(ns("stacked_bar"), height = 360)),
          footer = "\u5806\u53e0\u56fe\u4f7f\u7528\u6700\u65b0\u5e74\u7684\u7ec4\u5185\u56fd\u5bb6\u5747\u503c\uff0c\u4e09\u9879\u4e0d\u4e00\u5b9a\u52a0\u603b\u4e3a 100\u3002"
        )
      ),
      mod_card(
        kicker = "Z5 \u00b7 Spatial audit",
        title = "\u533a\u57df\u5730\u56fe",
        htmltools::p(class = "card-note",
          "\u6309\u5f53\u524d\u6307\u6807\u7740\u8272\u7684\u4e16\u754c\u5730\u56fe\uff0c\u70b9\u51fb\u67e5\u770b\u56fd\u5bb6\u8be6\u60c5\u3002"),
        mod_v3_chart_guide(
          "\u5730\u56fe\u628a\u5206\u7ec4\u5747\u503c\u843d\u56de\u56fd\u5bb6\u7a7a\u95f4",
          "\u8d8b\u52bf\u548c\u7bb1\u7ebf\u56fe\u544a\u8bc9\u4f60\u7ec4\u522b\u4e0a\u7684\u6a21\u5f0f\uff0c\u5730\u56fe\u5219\u7528\u6765\u627e\u90bb\u8fd1\u56fd\u5bb6\u662f\u5426\u5448\u73b0\u7c7b\u4f3c\u6a21\u5f0f\u6216\u660e\u663e\u53cd\u5dee\u3002"
        ),
        mod_spinner(leaflet::leafletOutput(ns("region_map"), height = 420))
      ),
      mod_card(
        kicker = "Z6 \u00b7 Regional summary",
        title = "\u6570\u636e\u8868",
        mod_v3_chart_guide(
          "\u8868\u683c\u8ba9\u533a\u57df\u5bf9\u6bd4\u53ef\u590d\u6838",
          "\u8868\u683c\u6309\u5f53\u524d\u5206\u7ec4\u53e3\u5f84\u6c47\u603b\u6700\u65b0\u5e74\u5747\u503c\u548c\u6837\u672c\u56fd\u5bb6\u6570\uff0c\u7528\u4e8e\u786e\u8ba4\u56fe\u4e0a\u6bcf\u6761\u7ebf\u548c\u6bcf\u4e2a\u7bb1\u4f53\u80cc\u540e\u7684\u6837\u672c\u91cf\u3002"
        ),
        mod_spinner(reactable::reactableOutput(ns("data_table"))),
        footer = "\u6837\u672c\u56fd\u5bb6\u6570\u8f83\u5c11\u7684\u7ec4\u522b\u5e94\u8c28\u614e\u89e3\u91ca\u5747\u503c\u548c\u5dee\u8ddd\u6bd4\u503c\u3002"
      )
      )
    )
  )
}

mod_regional_server <- function(id, master_r) {
  shiny::moduleServer(id, function(input, output, session) {

    filtered <- shiny::reactive({
      m <- master_r()
      shiny::req(input$year_range)
      m[m$year >= input$year_range[1] & m$year <= input$year_range[2], ,
        drop = FALSE]
    })

    output$kpi_strip <- shiny::renderUI({
      m <- filtered()
      yr <- max(m$year, na.rm = TRUE)
      latest <- m[m$year == yr, , drop = FALSE]
      ind <- input$indicator
      val <- if (is.numeric(latest[[ind]])) {
        stats::median(latest[[ind]], na.rm = TRUE)
      } else NA_real_
      n_countries <- length(unique(latest$iso3_code))
      n_groups <- length(unique(stats::na.omit(latest[[input$group_by]])))
      mod_v3_kpi_grid(
        mod_v3_kpi(format(n_countries, big.mark = ","),
                   "\u56fd\u5bb6\u6570", tone = "primary"),
        mod_v3_kpi(as.character(n_groups),
                   "\u5206\u7ec4\u6570", tone = "neutral"),
        mod_v3_kpi(if (grepl("che|usd", ind)) fmt_usd(val) else fmt_pct(val),
                   "\u4e2d\u4f4d\u6570", tone = "secondary"),
        mod_v3_kpi(sprintf("%d\u2013%d", input$year_range[1], input$year_range[2]),
                   "\u65f6\u6bb5", tone = "neutral")
      )
    })

    output$trend_line <- plotly::renderPlotly({
      m <- filtered()
      ind <- input$indicator
      grp <- input$group_by
      shiny::req(ind %in% names(m), grp %in% names(m))
      agg <- m[is.finite(m[[ind]]), , drop = FALSE]
      agg <- stats::aggregate(
        stats::as.formula(paste(ind, "~ year +", grp)),
        data = agg, FUN = mean, na.rm = TRUE
      )
      safe_plotly({
        plotly::plot_ly(agg, x = ~year, y = stats::as.formula(paste0("~", ind)),
                        color = stats::as.formula(paste0("~", grp)),
                        type = "scatter", mode = "lines+markers",
                        colors = if (grp == "continent") unname(brand_palette$continent)
                                 else unname(brand_palette$income)) |>
          ghs_plotly_layout() |>
          plotly::layout(
            xaxis = list(title = ""),
            yaxis = list(title = ind),
            legend = list(orientation = "h", y = -0.15)
          )
      })
    })

    output$box_latest <- plotly::renderPlotly({
      m <- filtered()
      ind <- input$indicator
      grp <- input$group_by
      yr <- max(m$year, na.rm = TRUE)
      d <- m[m$year == yr & is.finite(m[[ind]]), , drop = FALSE]
      shiny::req(nrow(d) > 5)
      safe_plotly({
        plotly::plot_ly(d, y = stats::as.formula(paste0("~", ind)),
                        x = stats::as.formula(paste0("~", grp)),
                        type = "box",
                        color = stats::as.formula(paste0("~", grp)),
                        colors = if (grp == "continent") unname(brand_palette$continent)
                                 else unname(brand_palette$income)) |>
          ghs_plotly_layout() |>
          plotly::layout(showlegend = FALSE,
                         xaxis = list(title = ""),
                         yaxis = list(title = ind))
      })
    })

    output$gap_ratio <- plotly::renderPlotly({
      m <- filtered()
      ind <- input$indicator
      grp <- input$group_by
      shiny::req(ind %in% names(m))
      agg <- m[is.finite(m[[ind]]), , drop = FALSE]
      agg <- stats::aggregate(
        stats::as.formula(paste(ind, "~ year +", grp)),
        data = agg, FUN = mean, na.rm = TRUE
      )
      ratio_df <- do.call(rbind, lapply(split(agg, agg$year), function(chunk) {
        vals <- chunk[[ind]]
        if (length(vals) < 2 || all(!is.finite(vals))) return(NULL)
        data.frame(year = chunk$year[1],
                   ratio = max(vals, na.rm = TRUE) / max(min(vals[vals > 0], na.rm = TRUE), 0.01))
      }))
      shiny::req(nrow(ratio_df) > 3)
      safe_plotly({
        plotly::plot_ly(ratio_df, x = ~year, y = ~ratio,
                        type = "scatter", mode = "lines+markers",
                        line = list(color = palette_ghs3("secondary"), width = 3),
                        marker = list(color = palette_ghs3("secondary"), size = 6)) |>
          ghs_plotly_layout() |>
          plotly::layout(
            xaxis = list(title = ""),
            yaxis = list(title = "\u6700\u9ad8/\u6700\u4f4e\u7ec4\u6bd4\u503c")
          )
      })
    })

    output$stacked_bar <- plotly::renderPlotly({
      m <- filtered()
      grp <- input$group_by
      yr <- max(m$year, na.rm = TRUE)
      d <- m[m$year == yr & is.finite(m$gghed_che) & is.finite(m$pvtd_che), ,
             drop = FALSE]
      shiny::req(nrow(d) > 5)
      agg <- stats::aggregate(
        cbind(gghed_che, pvtd_che, ext_che) ~ get(grp),
        data = d, FUN = mean, na.rm = TRUE
      )
      names(agg)[1] <- "group"
      safe_plotly({
        plotly::plot_ly(agg, x = ~group, y = ~gghed_che, type = "bar",
                        name = "GGHE-D", marker = list(color = "#1d3f5f")) |>
          plotly::add_trace(y = ~pvtd_che, name = "PVT-D",
                            marker = list(color = "#c46327")) |>
          plotly::add_trace(y = ~ext_che, name = "EXT",
                            marker = list(color = "#2a857a")) |>
          ghs_plotly_layout() |>
          plotly::layout(barmode = "stack",
                         xaxis = list(title = ""),
                         yaxis = list(title = "\u5360 CHE \u6bd4\u4f8b (%)"),
                         legend = list(orientation = "h", y = -0.15))
      })
    })

    output$region_map <- leaflet::renderLeaflet({
      m <- master_r()
      yr <- max(m$year, na.rm = TRUE)
      ind <- input$indicator
      if (is.null(world_sf_obj)) {
        return(leaflet::leaflet() |>
                 leaflet::addProviderTiles("CartoDB.Positron"))
      }
      tryCatch(
        leaflet_choropleth(m, world_sf_obj,
                           indicator_col = ind,
                           year_focus = yr,
                           title = sprintf("%s \u00b7 %d", ind, yr)),
        error = function(e) {
          leaflet::leaflet() |>
            leaflet::addProviderTiles("CartoDB.Positron")
        }
      )
    })

    output$data_table <- reactable::renderReactable({
      m <- filtered()
      ind <- input$indicator
      grp <- input$group_by
      yr <- max(m$year, na.rm = TRUE)
      d <- m[m$year == yr & is.finite(m[[ind]]), , drop = FALSE]
      agg <- stats::aggregate(
        stats::as.formula(paste(ind, "~", grp)),
        data = d, FUN = function(x) round(mean(x, na.rm = TRUE), 2)
      )
      agg$n <- as.integer(table(d[[grp]])[agg[[grp]]])
      names(agg) <- c("\u5206\u7ec4", "\u5747\u503c", "\u56fd\u5bb6\u6570")
      reactable::reactable(agg, pagination = FALSE, highlight = TRUE,
        defaultColDef = reactable::colDef(
          headerStyle = list(background = "#f1f3f7")))
    })
  })
}
