# =============================================================================
# 仪表盘/模块/mod_growth.R
# 增长分析：卫生支出增长率的跨国比较与收入弹性
# =============================================================================

mod_growth_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = htmltools::HTML("&#128200; \u589e\u957f Growth"),
    mod_v3_hero(
      kicker = "GROWTH & ELASTICITY",
      title = "\u536b\u751f\u652f\u51fa\u589e\u957f\u4e0e\u6536\u5165\u5f39\u6027",
      lead = paste(
        "\u4eba\u5747 CHE \u7684\u5e74\u5747\u589e\u957f\u7387\u5728\u4e0d\u540c\u6536\u5165\u7ec4\u95f4\u5dee\u5f02\u663e\u8457\u3002",
        "\u672c\u6a21\u5757\u5206\u6790\u589e\u957f\u7387\u5206\u5e03\u3001\u6536\u5165\u5f39\u6027\u3001\u8ffd\u8d76\u6548\u5e94\u4e0e\u589e\u957f\u6301\u7eed\u6027\u3002"
      ),
      meta = list("CAGR", "\u6536\u5165\u5f39\u6027", "195 \u56fd\u5bb6 \u00b7 2000\u20132023")
    ),
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 280,
        shiny::sliderInput(ns("period"), "\u8ba1\u7b97\u65f6\u6bb5",
          min = 2000, max = 2023, value = c(2010, 2023), step = 1, sep = ""),
        shiny::selectInput(ns("indicator"), "\u589e\u957f\u6307\u6807",
          choices = c(
            "\u4eba\u5747 CHE (USD)" = "che_pc_usd2023",
            "GGHE-D (%)" = "gghed_che",
            "OOPS (%)" = "hf3_che"
          ), selected = "che_pc_usd2023"),
        shiny::selectInput(ns("color_by"), "\u7740\u8272",
          choices = c("\u6536\u5165\u7ec4" = "income_group",
                      "\u5927\u6d32" = "continent"),
          selected = "income_group"),
        shiny::tags$hr(),
        shiny::helpText(
          "CAGR = \u5e74\u5747\u590d\u5408\u589e\u957f\u7387\u3002",
          "\u6536\u5165\u5f39\u6027 = %\u0394CHE / %\u0394GDP\uff0c>1 \u8868\u793a\u536b\u751f\u652f\u51fa\u589e\u901f\u5feb\u4e8e\u7ecf\u6d4e\u3002"
        )
      ),
      shiny::uiOutput(ns("kpi_strip")),
      bslib::layout_columns(
        col_widths = c(7, 5),
        mod_card(
          title = "\u56fd\u5bb6\u7ea7 CAGR \u5206\u5e03",
          htmltools::p(class = "card-note",
            "\u5404\u56fd\u5728\u9009\u5b9a\u65f6\u6bb5\u5185\u7684\u5e74\u5747\u590d\u5408\u589e\u957f\u7387\u5206\u5e03\u3002"),
          mod_spinner(plotly::plotlyOutput(ns("cagr_box"), height = 440))
        ),
        mod_card(
          title = "\u589e\u957f\u7387 Top/Bottom 15",
          htmltools::p(class = "card-note",
            "\u589e\u957f\u6700\u5feb\u4e0e\u6700\u6162\u7684\u56fd\u5bb6\u3002"),
          mod_spinner(plotly::plotlyOutput(ns("cagr_ranking"), height = 440))
        )
      ),
      bslib::layout_columns(
        col_widths = c(6, 6),
        mod_card(
          title = "\u6536\u5165\u5f39\u6027\u6563\u70b9\u56fe",
          htmltools::p(class = "card-note",
            "\u6a2a\u8f74 = GDP CAGR\uff0c\u7eb5\u8f74 = CHE CAGR\u3002\u5bf9\u89d2\u7ebf\u4e0a\u65b9 = \u5f39\u6027 > 1\u3002"),
          mod_spinner(plotly::plotlyOutput(ns("elasticity_scatter"), height = 400))
        ),
        mod_card(
          title = "\u5206\u7ec4\u5e74\u5ea6\u589e\u957f\u7387\u65f6\u5e8f",
          htmltools::p(class = "card-note",
            "\u5404\u7ec4\u7684\u5e74\u5ea6\u589e\u957f\u7387\u6f14\u5316\u3002"),
          mod_spinner(plotly::plotlyOutput(ns("yoy_trend"), height = 400))
        )
      ),
      mod_card(
        title = "\u589e\u957f\u7387\u8be6\u8868",
        mod_spinner(reactable::reactableOutput(ns("growth_table")))
      )
    )
  )
}

mod_growth_server <- function(id, master_r) {
  shiny::moduleServer(id, function(input, output, session) {

    growth_data <- shiny::reactive({
      m <- master_r()
      ind <- input$indicator
      yr1 <- input$period[1]; yr2 <- input$period[2]
      d1 <- m[m$year == yr1 & is.finite(m[[ind]]) & m[[ind]] > 0,
              c("iso3_code", "country_name", "continent", "income_group", ind, "gdp_pc_usd")]
      d2 <- m[m$year == yr2 & is.finite(m[[ind]]) & m[[ind]] > 0,
              c("iso3_code", ind, "gdp_pc_usd")]
      names(d1)[5:6] <- c("val_t0", "gdp_t0")
      names(d2)[2:3] <- c("val_t1", "gdp_t1")
      merged <- merge(d1[, c("iso3_code", "country_name", "continent",
                              "income_group", "val_t0", "gdp_t0")],
                      d2, by = "iso3_code")
      n_years <- yr2 - yr1
      merged$cagr <- ((merged$val_t1 / merged$val_t0)^(1 / n_years) - 1) * 100
      merged$gdp_cagr <- ifelse(
        is.finite(merged$gdp_t0) & merged$gdp_t0 > 0 & is.finite(merged$gdp_t1),
        ((merged$gdp_t1 / merged$gdp_t0)^(1 / n_years) - 1) * 100,
        NA_real_
      )
      merged$elasticity <- merged$cagr / pmax(merged$gdp_cagr, 0.01)
      merged[is.finite(merged$cagr), ]
    })

    output$kpi_strip <- shiny::renderUI({
      gd <- growth_data()
      shiny::req(nrow(gd) > 0)
      mod_v3_kpi_grid(
        mod_v3_kpi(format(nrow(gd), big.mark = ","),
                   "\u56fd\u5bb6\u6570", tone = "primary"),
        mod_v3_kpi(sprintf("%.2f%%", median(gd$cagr, na.rm = TRUE)),
                   "CAGR \u4e2d\u4f4d\u6570", tone = "good"),
        mod_v3_kpi(sprintf("%.2f", median(gd$elasticity[is.finite(gd$elasticity)], na.rm = TRUE)),
                   "\u5f39\u6027\u4e2d\u4f4d\u6570", tone = "secondary"),
        mod_v3_kpi(sprintf("%d\u2013%d", input$period[1], input$period[2]),
                   "\u65f6\u6bb5", tone = "neutral")
      )
    })

    output$cagr_box <- plotly::renderPlotly({
      gd <- growth_data()
      grp <- input$color_by
      shiny::req(nrow(gd) > 5, grp %in% names(gd))
      pal <- if (grp == "income_group") unname(brand_palette$income)
             else unname(brand_palette$continent)
      safe_plotly({
        plotly::plot_ly(gd, x = stats::as.formula(paste0("~", grp)),
                        y = ~cagr, type = "box",
                        color = stats::as.formula(paste0("~", grp)),
                        colors = pal, boxpoints = "outliers") |>
          ghs_plotly_layout() |>
          plotly::layout(
            showlegend = FALSE,
            xaxis = list(title = ""),
            yaxis = list(title = "CAGR (%)"),
            shapes = list(
              list(type = "line", x0 = -0.5, x1 = 10, y0 = 0, y1 = 0,
                   line = list(color = "#5d667a", width = 1, dash = "dash"))
            )
          )
      })
    })

    output$cagr_ranking <- plotly::renderPlotly({
      gd <- growth_data()
      shiny::req(nrow(gd) > 10)
      gd <- gd[order(-gd$cagr), ]
      top <- utils::head(gd, 8)
      bottom <- utils::tail(gd, 7)
      show <- rbind(top, bottom)
      show$country_name <- factor(show$country_name, levels = rev(show$country_name))
      safe_plotly({
        plotly::plot_ly(show, x = ~cagr, y = ~country_name,
                        type = "bar", orientation = "h",
                        marker = list(
                          color = ifelse(show$cagr > 0, "#2a857a", "#a23b3b")
                        )) |>
          ghs_plotly_layout() |>
          plotly::layout(
            xaxis = list(title = "CAGR (%)"),
            yaxis = list(title = ""),
            margin = list(l = 120)
          )
      })
    })

    output$elasticity_scatter <- plotly::renderPlotly({
      gd <- growth_data()
      gd <- gd[is.finite(gd$gdp_cagr) & is.finite(gd$cagr), ]
      shiny::req(nrow(gd) > 5)
      grp <- input$color_by
      pal <- if (grp == "income_group") unname(brand_palette$income)
             else unname(brand_palette$continent)
      safe_plotly({
        plotly::plot_ly(gd, x = ~gdp_cagr, y = ~cagr,
                        color = stats::as.formula(paste0("~", grp)),
                        text = ~country_name, type = "scatter", mode = "markers",
                        colors = pal,
                        marker = list(size = 8, opacity = 0.7)) |>
          ghs_plotly_layout() |>
          plotly::layout(
            xaxis = list(title = "GDP CAGR (%)"),
            yaxis = list(title = "CHE CAGR (%)"),
            shapes = list(
              list(type = "line", x0 = -5, x1 = 15, y0 = -5, y1 = 15,
                   line = list(color = "#5d667a", width = 1.5, dash = "dash"))
            ),
            legend = list(orientation = "h", y = -0.15)
          )
      })
    })

    output$yoy_trend <- plotly::renderPlotly({
      m <- master_r(); ind <- input$indicator; grp <- input$color_by
      years <- sort(unique(m$year))
      results <- list()
      for (yr in years[-1]) {
        yr_prev <- yr - 1
        d_now <- m[m$year == yr & is.finite(m[[ind]]) & m[[ind]] > 0, ]
        d_prev <- m[m$year == yr_prev & is.finite(m[[ind]]) & m[[ind]] > 0, ]
        merged <- merge(
          d_now[, c("iso3_code", grp, ind)],
          d_prev[, c("iso3_code", ind)],
          by = "iso3_code", suffixes = c("_now", "_prev")
        )
        if (nrow(merged) > 10) {
          col_now <- paste0(ind, "_now"); col_prev <- paste0(ind, "_prev")
          merged$yoy <- (merged[[col_now]] / merged[[col_prev]] - 1) * 100
          agg <- stats::aggregate(
            stats::as.formula(paste("yoy ~", grp)),
            data = merged[is.finite(merged$yoy), ], FUN = median
          )
          agg$year <- yr
          results[[length(results) + 1]] <- agg
        }
      }
      df <- do.call(rbind, results)
      shiny::req(nrow(df) > 5)
      pal <- if (grp == "income_group") unname(brand_palette$income)
             else unname(brand_palette$continent)
      safe_plotly({
        plotly::plot_ly(df, x = ~year, y = ~yoy,
                        color = stats::as.formula(paste0("~", grp)),
                        type = "scatter", mode = "lines+markers",
                        colors = pal) |>
          ghs_plotly_layout() |>
          plotly::layout(
            xaxis = list(title = ""),
            yaxis = list(title = "\u5e74\u5ea6\u589e\u957f\u7387\u4e2d\u4f4d\u6570 (%)"),
            legend = list(orientation = "h", y = -0.18),
            shapes = list(
              list(type = "line", x0 = min(df$year), x1 = max(df$year),
                   y0 = 0, y1 = 0,
                   line = list(color = "#5d667a", width = 1, dash = "dash"))
            )
          )
      })
    })

    output$growth_table <- reactable::renderReactable({
      gd <- growth_data()
      shiny::req(nrow(gd) > 0)
      gd <- gd[order(-gd$cagr), ]
      tab <- data.frame(
        "\u56fd\u5bb6" = gd$country_name,
        "\u6536\u5165\u7ec4" = gd$income_group,
        "CAGR%" = round(gd$cagr, 2),
        "GDP_CAGR%" = round(gd$gdp_cagr, 2),
        "\u5f39\u6027" = round(gd$elasticity, 2),
        "\u8d77\u59cb\u503c" = round(gd$val_t0, 0),
        "\u7ec8\u6b62\u503c" = round(gd$val_t1, 0),
        check.names = FALSE
      )
      reactable::reactable(tab, defaultPageSize = 15, searchable = TRUE,
        highlight = TRUE, striped = TRUE,
        defaultColDef = reactable::colDef(
          headerStyle = list(background = "#f1f3f7", fontWeight = 700)))
    })
  })
}
