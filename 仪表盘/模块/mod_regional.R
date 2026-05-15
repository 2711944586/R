# =============================================================================
# 仪表盘/模块/mod_regional.R
# Tab · 区域对比：6 大洲 × 4 收入组的卫生支出结构与趋势
# =============================================================================

mod_regional_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = htmltools::HTML("&#127758; \u533a\u57df Regional"),
    mod_v3_hero(
      kicker = "REGIONAL COMPARISON",
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
        shiny::tags$hr(),
        shiny::helpText(
          "\u5206\u7ec4\u5747\u503c\u4e3a\u4eba\u53e3\u52a0\u6743\u5e73\u5747\uff1b",
          "\u7bb1\u7ebf\u56fe\u5c55\u793a\u7ec4\u5185\u56fd\u5bb6\u5206\u5e03\u3002"
        )
      ),
      # KPI row
      shiny::uiOutput(ns("kpi_strip")),
      # Main charts
      bslib::layout_columns(
        col_widths = c(7, 5),
        mod_card(
          title = "\u5206\u7ec4\u8d8b\u52bf\u6f14\u5316",
          htmltools::p(class = "card-note",
            "\u5404\u7ec4\u4eba\u53e3\u52a0\u6743\u5e73\u5747\u503c\u7684\u65f6\u5e8f\u53d8\u5316\u3002"),
          mod_spinner(plotly::plotlyOutput(ns("trend_line"), height = 400))
        ),
        mod_card(
          title = "\u7ec4\u5185\u5206\u5e03\uff08\u6700\u65b0\u5e74\uff09",
          htmltools::p(class = "card-note",
            "\u7bb1\u7ebf + \u6563\u70b9\u5c55\u793a\u7ec4\u5185\u56fd\u5bb6\u7684\u5206\u5e03\u4e0e\u79bb\u7fa4\u503c\u3002"),
          mod_spinner(plotly::plotlyOutput(ns("box_latest"), height = 400))
        )
      ),
      bslib::layout_columns(
        col_widths = c(6, 6),
        mod_card(
          title = "\u7ec4\u95f4\u5dee\u8ddd\u6f14\u5316",
          htmltools::p(class = "card-note",
            "\u6700\u9ad8\u7ec4\u4e0e\u6700\u4f4e\u7ec4\u7684\u6bd4\u503c\u968f\u65f6\u95f4\u53d8\u5316\u3002"),
          mod_spinner(plotly::plotlyOutput(ns("gap_ratio"), height = 360))
        ),
        mod_card(
          title = "\u7ed3\u6784\u5806\u53e0\u5bf9\u6bd4",
          htmltools::p(class = "card-note",
            "\u5404\u7ec4\u7684\u653f\u5e9c/\u79c1\u4eba/\u5916\u63f4\u4e09\u6e90\u7ed3\u6784\u5bf9\u6bd4\u3002"),
          mod_spinner(plotly::plotlyOutput(ns("stacked_bar"), height = 360))
        )
      ),
      mod_card(
        title = "\u533a\u57df\u5730\u56fe",
        htmltools::p(class = "card-note",
          "\u6309\u5f53\u524d\u6307\u6807\u7740\u8272\u7684\u4e16\u754c\u5730\u56fe\uff0c\u70b9\u51fb\u67e5\u770b\u56fd\u5bb6\u8be6\u60c5\u3002"),
        mod_spinner(leaflet::leafletOutput(ns("region_map"), height = 420))
      ),
      mod_card(
        title = "\u6570\u636e\u8868",
        mod_spinner(reactable::reactableOutput(ns("data_table")))
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
                         yaxis = list(title = "% of CHE"),
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
