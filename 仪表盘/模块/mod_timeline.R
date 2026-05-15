# =============================================================================
# 仪表盘/模块/mod_timeline.R
# 时间线：全球卫生支出 24 年关键事件与结构转折
# =============================================================================

mod_timeline_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = htmltools::HTML("&#128197; \u65f6\u95f4\u7ebf Timeline"),
    mod_v3_hero(
      kicker = "24-YEAR TIMELINE",
      title = "\u5168\u7403\u536b\u751f\u652f\u51fa 24 \u5e74\u65f6\u95f4\u7ebf",
      lead = paste(
        "2000\u20132023 \u5e74\u95f4\uff0c\u5168\u7403\u536b\u751f\u652f\u51fa\u7ecf\u5386\u4e86 2008 \u91d1\u878d\u5371\u673a\u3001",
        "2014 \u6cb9\u4ef7\u5d29\u76d8\u30012020 COVID-19 \u4e09\u6b21\u91cd\u5927\u51b2\u51fb\u3002",
        "\u672c\u6a21\u5757\u5c55\u793a\u5173\u952e\u6307\u6807\u7684\u65f6\u5e8f\u6f14\u5316\u4e0e\u7ed3\u6784\u8f6c\u6298\u3002"
      ),
      meta = list("2000\u20132023", "\u4e09\u6b21\u91cd\u5927\u51b2\u51fb", "195 \u56fd\u5bb6")
    ),
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 260,
        shiny::selectInput(ns("indicator"), "\u6307\u6807",
          choices = c(
            "\u4eba\u5747 CHE (USD)" = "che_pc_usd2023",
            "OOPS (%)" = "hf3_che",
            "GGHE-D (%)" = "gghed_che",
            "EXT (%)" = "ext_che",
            "\u9884\u671f\u5bff\u547d" = "life_exp"
          ), selected = "che_pc_usd2023"),
        shiny::selectInput(ns("group_by"), "\u5206\u7ec4",
          choices = c("\u6536\u5165\u7ec4" = "income_group",
                      "\u5927\u6d32" = "continent"),
          selected = "income_group"),
        shiny::tags$hr(),
        shiny::helpText(
          "\u7070\u8272\u7ad6\u7ebf\u6807\u8bb0\u5173\u952e\u4e8b\u4ef6\uff1a",
          "2008 GFC \u00b7 2014 \u6cb9\u4ef7 \u00b7 2020 COVID\u3002"
        )
      ),
      shiny::uiOutput(ns("kpi_strip")),
      # Row 1: main timeline
      mod_card(
        title = "\u5168\u7403\u65f6\u5e8f\u6f14\u5316\uff08\u5206\u7ec4\uff09",
        htmltools::p(class = "card-note",
          "\u5404\u7ec4\u5e73\u5747\u503c\u7684 24 \u5e74\u8f68\u8ff9\uff0c\u7070\u8272\u7ad6\u7ebf = \u5173\u952e\u4e8b\u4ef6\u5e74\u4efd\u3002"),
        mod_spinner(plotly::plotlyOutput(ns("main_timeline"), height = 420))
      ),
      # Row 2
      bslib::layout_columns(
        col_widths = c(6, 6),
        mod_card(
          title = "\u5e74\u5ea6\u53d8\u5316\u7387",
          htmltools::p(class = "card-note",
            "\u5168\u7403\u4e2d\u4f4d\u6570\u7684\u5e74\u5ea6\u53d8\u5316\u7387\uff0c\u8d1f\u503c = \u4e0b\u964d\u3002"),
          mod_spinner(plotly::plotlyOutput(ns("yoy_bar"), height = 380))
        ),
        mod_card(
          title = "\u7ed3\u6784\u6f14\u5316\uff08\u4e09\u6e90\u5360\u6bd4\uff09",
          htmltools::p(class = "card-note",
            "\u653f\u5e9c/\u79c1\u4eba/\u5916\u63f4\u4e09\u6e90\u5360\u6bd4\u7684 24 \u5e74\u53d8\u5316\u3002"),
          mod_spinner(plotly::plotlyOutput(ns("structure_area"), height = 380))
        )
      ),
      # Row 3
      bslib::layout_columns(
        col_widths = c(6, 6),
        mod_card(
          title = "\u6ce2\u52a8\u6027\u5206\u6790",
          htmltools::p(class = "card-note",
            "\u5404\u7ec4\u7684\u5e74\u5ea6\u6807\u51c6\u5dee\u6f14\u5316\uff0c\u53cd\u6620\u7ec4\u5185\u5dee\u5f02\u53d8\u5316\u3002"),
          mod_spinner(plotly::plotlyOutput(ns("volatility"), height = 380))
        ),
        mod_card(
          title = "\u4e09\u671f\u5bf9\u6bd4",
          htmltools::p(class = "card-note",
            "\u524d\u5371\u673a (2000\u20132007) / \u4e2d\u95f4\u671f (2008\u20132019) / \u540e\u75ab\u60c5 (2020\u20132023) \u4e09\u671f\u5747\u503c\u3002"),
          mod_spinner(plotly::plotlyOutput(ns("period_compare"), height = 380))
        )
      ),
      mod_card(
        title = "\u5e74\u5ea6\u6570\u636e\u8868",
        mod_spinner(reactable::reactableOutput(ns("timeline_table")))
      )
    )
  )
}

mod_timeline_server <- function(id, master_r) {
  shiny::moduleServer(id, function(input, output, session) {

    event_years <- c(2008, 2014, 2020)
    event_labels <- c("GFC", "\u6cb9\u4ef7\u5d29\u76d8", "COVID-19")

    output$kpi_strip <- shiny::renderUI({
      m <- master_r(); ind <- input$indicator
      latest <- max(m$year, na.rm = TRUE)
      earliest <- min(m$year, na.rm = TRUE)
      val_latest <- median(m[[ind]][m$year == latest], na.rm = TRUE)
      val_earliest <- median(m[[ind]][m$year == earliest], na.rm = TRUE)
      change <- (val_latest / pmax(val_earliest, 0.01) - 1) * 100
      mod_v3_kpi_grid(
        mod_v3_kpi(sprintf("%d\u2013%d", earliest, latest),
                   "\u65f6\u95f4\u8de8\u5ea6", tone = "primary"),
        mod_v3_kpi(sprintf("%.1f", val_latest),
                   sprintf("%d \u4e2d\u4f4d\u6570", latest), tone = "secondary"),
        mod_v3_kpi(sprintf("%+.1f%%", change),
                   "\u7d2f\u8ba1\u53d8\u5316", tone = if (change > 0) "good" else "bad"),
        mod_v3_kpi("3", "\u91cd\u5927\u51b2\u51fb", tone = "warn")
      )
    })

    output$main_timeline <- plotly::renderPlotly({
      m <- master_r(); ind <- input$indicator; grp <- input$group_by
      d <- m[is.finite(m[[ind]]) & !is.na(m[[grp]]), ]
      agg <- stats::aggregate(
        stats::as.formula(paste(ind, "~ year +", grp)),
        data = d, FUN = median, na.rm = TRUE
      )
      pal <- if (grp == "income_group") unname(brand_palette$income)
             else unname(brand_palette$continent)
      shapes <- lapply(seq_along(event_years), function(i) {
        list(type = "line", x0 = event_years[i], x1 = event_years[i],
             y0 = 0, y1 = 1, yref = "paper",
             line = list(color = "rgba(93,102,122,0.4)", width = 1.5, dash = "dash"))
      })
      safe_plotly({
        plotly::plot_ly(agg, x = ~year, y = stats::as.formula(paste0("~", ind)),
                        color = stats::as.formula(paste0("~", grp)),
                        type = "scatter", mode = "lines+markers",
                        colors = pal) |>
          ghs_plotly_layout() |>
          plotly::layout(
            xaxis = list(title = ""),
            yaxis = list(title = ind),
            shapes = shapes,
            legend = list(orientation = "h", y = -0.15)
          )
      })
    })

    output$yoy_bar <- plotly::renderPlotly({
      m <- master_r(); ind <- input$indicator
      years <- sort(unique(m$year))
      yoy <- vapply(years[-1], function(yr) {
        v_now <- median(m[[ind]][m$year == yr], na.rm = TRUE)
        v_prev <- median(m[[ind]][m$year == yr - 1], na.rm = TRUE)
        if (is.finite(v_prev) && v_prev != 0) (v_now / v_prev - 1) * 100 else NA_real_
      }, numeric(1))
      df <- data.frame(year = years[-1], yoy = yoy)
      df <- df[is.finite(df$yoy), ]
      safe_plotly({
        plotly::plot_ly(df, x = ~year, y = ~yoy, type = "bar",
                        marker = list(
                          color = ifelse(df$yoy >= 0, "#2a857a", "#a23b3b")
                        )) |>
          ghs_plotly_layout() |>
          plotly::layout(
            xaxis = list(title = ""),
            yaxis = list(title = "\u5e74\u5ea6\u53d8\u5316\u7387 (%)"),
            shapes = list(
              list(type = "line", x0 = min(df$year), x1 = max(df$year),
                   y0 = 0, y1 = 0,
                   line = list(color = "#5d667a", width = 1))
            )
          )
      })
    })

    output$structure_area <- plotly::renderPlotly({
      m <- master_r()
      d <- m[is.finite(m$gghed_che) & is.finite(m$pvtd_che) & is.finite(m$ext_che), ]
      agg <- stats::aggregate(cbind(gghed_che, pvtd_che, ext_che) ~ year,
                              data = d, FUN = mean, na.rm = TRUE)
      safe_plotly({
        plotly::plot_ly(agg, x = ~year) |>
          plotly::add_trace(y = ~gghed_che, name = "GGHE-D", type = "scatter",
                            mode = "lines", stackgroup = "one",
                            fillcolor = "rgba(29,63,95,0.6)",
                            line = list(color = "#1d3f5f")) |>
          plotly::add_trace(y = ~pvtd_che, name = "PVT-D", type = "scatter",
                            mode = "lines", stackgroup = "one",
                            fillcolor = "rgba(196,99,39,0.6)",
                            line = list(color = "#c46327")) |>
          plotly::add_trace(y = ~ext_che, name = "EXT", type = "scatter",
                            mode = "lines", stackgroup = "one",
                            fillcolor = "rgba(42,133,122,0.6)",
                            line = list(color = "#2a857a")) |>
          ghs_plotly_layout() |>
          plotly::layout(
            xaxis = list(title = ""),
            yaxis = list(title = "\u5360 CHE (%)"),
            legend = list(orientation = "h", y = -0.15)
          )
      })
    })

    output$volatility <- plotly::renderPlotly({
      m <- master_r(); ind <- input$indicator; grp <- input$group_by
      d <- m[is.finite(m[[ind]]) & !is.na(m[[grp]]), ]
      agg <- stats::aggregate(
        stats::as.formula(paste(ind, "~ year +", grp)),
        data = d, FUN = stats::sd, na.rm = TRUE
      )
      pal <- if (grp == "income_group") unname(brand_palette$income)
             else unname(brand_palette$continent)
      safe_plotly({
        plotly::plot_ly(agg, x = ~year, y = stats::as.formula(paste0("~", ind)),
                        color = stats::as.formula(paste0("~", grp)),
                        type = "scatter", mode = "lines",
                        colors = pal) |>
          ghs_plotly_layout() |>
          plotly::layout(
            xaxis = list(title = ""),
            yaxis = list(title = "\u7ec4\u5185\u6807\u51c6\u5dee"),
            legend = list(orientation = "h", y = -0.15)
          )
      })
    })

    output$period_compare <- plotly::renderPlotly({
      m <- master_r(); ind <- input$indicator; grp <- input$group_by
      periods <- list(
        "\u524d\u5371\u673a (00\u201307)" = c(2000, 2007),
        "\u4e2d\u95f4\u671f (08\u201319)" = c(2008, 2019),
        "\u540e\u75ab\u60c5 (20\u201323)" = c(2020, 2023)
      )
      results <- lapply(names(periods), function(pname) {
        rng <- periods[[pname]]
        d <- m[m$year >= rng[1] & m$year <= rng[2] & is.finite(m[[ind]]) &
               !is.na(m[[grp]]), ]
        agg <- stats::aggregate(
          stats::as.formula(paste(ind, "~", grp)),
          data = d, FUN = median, na.rm = TRUE
        )
        agg$period <- pname
        agg
      })
      df <- do.call(rbind, results)
      safe_plotly({
        plotly::plot_ly(df, x = stats::as.formula(paste0("~", grp)),
                        y = stats::as.formula(paste0("~", ind)),
                        color = ~period, type = "bar",
                        colors = c("#1d3f5f", "#c46327", "#2a857a")) |>
          ghs_plotly_layout() |>
          plotly::layout(
            barmode = "group",
            xaxis = list(title = ""),
            yaxis = list(title = ind),
            legend = list(orientation = "h", y = -0.15)
          )
      })
    })

    output$timeline_table <- reactable::renderReactable({
      m <- master_r(); ind <- input$indicator
      agg <- stats::aggregate(
        stats::as.formula(paste(ind, "~ year")),
        data = m[is.finite(m[[ind]]), ], FUN = median, na.rm = TRUE
      )
      names(agg)[2] <- "median"
      agg$n <- vapply(agg$year, function(yr) sum(m$year == yr & is.finite(m[[ind]])), integer(1))
      agg$median <- round(agg$median, 1)
      reactable::reactable(agg, pagination = FALSE, highlight = TRUE,
        striped = TRUE,
        defaultColDef = reactable::colDef(
          headerStyle = list(background = "#f1f3f7", fontWeight = 700)))
    })
  })
}
