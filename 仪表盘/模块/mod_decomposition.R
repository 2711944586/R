# =============================================================================
# 仪表盘/模块/mod_decomposition.R
# 分解分析：卫生支出增长的来源分解与结构变迁
# =============================================================================

mod_decomposition_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = htmltools::HTML("&#128300; \u5206\u89e3 Decompose"),
    value = "decomposition",
    mod_v3_hero(
      kicker = "GROWTH DECOMPOSITION",
      title = "\u536b\u751f\u652f\u51fa\u589e\u957f\u7684\u6765\u6e90\u5206\u89e3",
      lead = paste(
        "\u4eba\u5747 CHE \u589e\u957f\u53ef\u5206\u89e3\u4e3a\uff1aGDP \u589e\u957f\u6548\u5e94 + \u536b\u751f\u4f18\u5148\u7ea7\u6548\u5e94 + \u4ea4\u4e92\u9879\u3002",
        "\u672c\u6a21\u5757\u5c55\u793a\u4e0d\u540c\u56fd\u5bb6\u7ec4\u522b\u7684\u589e\u957f\u9a71\u52a8\u529b\u5dee\u5f02\u3002"
      ),
      meta = list("Oaxaca-Blinder \u5206\u89e3", "Shapley \u5f52\u56e0", "2000\u20132023")
    ),
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 280,
        shiny::sliderInput(ns("period"), "\u5bf9\u6bd4\u65f6\u6bb5",
          min = 2000, max = 2023, value = c(2005, 2020), step = 1, sep = ""),
        shiny::selectInput(ns("group_by"), "\u5206\u7ec4",
          choices = c("\u6536\u5165\u7ec4" = "income_group",
                      "\u5927\u6d32" = "continent"),
          selected = "income_group"),
        shiny::tags$hr(),
        shiny::helpText(
          "\u5206\u89e3\u516c\u5f0f\uff1a\u0394CHE/GDP = \u0394GDP\u00d7(CHE/GDP)_0 + GDP_0\u00d7\u0394(CHE/GDP) + \u0394GDP\u00d7\u0394(CHE/GDP)",
          "\u5373\uff1a\u7ecf\u6d4e\u589e\u957f\u6548\u5e94 + \u4f18\u5148\u7ea7\u6548\u5e94 + \u4ea4\u4e92\u9879\u3002"
        )
      ),
      shiny::uiOutput(ns("kpi_strip")),
      # Row 1
      bslib::layout_columns(
        col_widths = c(7, 5),
        mod_card(
          title = "\u589e\u957f\u5206\u89e3\u5806\u53e0\u56fe",
          htmltools::p(class = "card-note",
            "\u5404\u7ec4\u7684 CHE/cap \u589e\u957f\u5206\u89e3\u4e3a GDP \u6548\u5e94\u3001\u4f18\u5148\u7ea7\u6548\u5e94\u3001\u4ea4\u4e92\u9879\u3002"),
          mod_spinner(plotly::plotlyOutput(ns("decomp_bar"), height = 440))
        ),
        mod_card(
          title = "\u589e\u957f\u7387\u5bf9\u6bd4",
          htmltools::p(class = "card-note",
            "\u5404\u7ec4\u5728\u9009\u5b9a\u65f6\u6bb5\u5185\u7684\u5e74\u5747\u590d\u5408\u589e\u957f\u7387 (CAGR)\u3002"),
          mod_spinner(plotly::plotlyOutput(ns("cagr_bar"), height = 440))
        )
      ),
      # Row 2
      bslib::layout_columns(
        col_widths = c(6, 6),
        mod_card(
          title = "\u7ed3\u6784\u53d8\u8fc1\uff1a\u4e09\u6e90\u5360\u6bd4\u53d8\u5316",
          htmltools::p(class = "card-note",
            "\u5404\u7ec4\u7684 GGHED/PVT-D/EXT \u5360\u6bd4\u5728\u4e24\u4e2a\u65f6\u70b9\u7684\u53d8\u5316\u3002"),
          mod_spinner(plotly::plotlyOutput(ns("structure_change"), height = 380))
        ),
        mod_card(
          title = "\u56fd\u5bb6\u7ea7\u589e\u957f\u5206\u5e03",
          htmltools::p(class = "card-note",
            "\u5404\u56fd CHE/cap \u5e74\u5747\u589e\u957f\u7387\u7684\u5206\u5e03\uff0c\u6309\u7ec4\u7740\u8272\u3002"),
          mod_spinner(plotly::plotlyOutput(ns("growth_dist"), height = 380))
        )
      ),
      # Row 3
      mod_card(
        title = "\u5206\u89e3\u7ed3\u679c\u8868",
        htmltools::p(class = "card-note",
          "\u5404\u7ec4\u7684\u589e\u957f\u5206\u89e3\u6570\u503c\u3002"),
        mod_spinner(reactable::reactableOutput(ns("decomp_table")))
      )
    )
  )
}

mod_decomposition_server <- function(id, master_r) {
  shiny::moduleServer(id, function(input, output, session) {

    decomp_data <- shiny::reactive({
      m <- master_r()
      yr1 <- input$period[1]; yr2 <- input$period[2]
      grp <- input$group_by
      d1 <- m[m$year == yr1 & is.finite(m$che_pc_usd2023) &
              is.finite(m$gdp_pc_usd) & !is.na(m[[grp]]), ]
      d2 <- m[m$year == yr2 & is.finite(m$che_pc_usd2023) &
              is.finite(m$gdp_pc_usd) & !is.na(m[[grp]]), ]
      # Aggregate by group
      agg1 <- stats::aggregate(
        cbind(che_pc_usd2023, gdp_pc_usd, gghed_che, pvtd_che, ext_che) ~ get(grp),
        data = d1, FUN = mean, na.rm = TRUE
      )
      agg2 <- stats::aggregate(
        cbind(che_pc_usd2023, gdp_pc_usd, gghed_che, pvtd_che, ext_che) ~ get(grp),
        data = d2, FUN = mean, na.rm = TRUE
      )
      names(agg1)[1] <- names(agg2)[1] <- "group"
      merged <- merge(agg1, agg2, by = "group", suffixes = c("_t0", "_t1"))
      # Decompose: CHE growth = GDP effect + priority effect + interaction
      merged$delta_che <- merged$che_pc_usd2023_t1 - merged$che_pc_usd2023_t0
      merged$delta_gdp <- merged$gdp_pc_usd_t1 - merged$gdp_pc_usd_t0
      # CHE/GDP ratio
      merged$ratio_t0 <- merged$che_pc_usd2023_t0 / pmax(merged$gdp_pc_usd_t0, 1)
      merged$ratio_t1 <- merged$che_pc_usd2023_t1 / pmax(merged$gdp_pc_usd_t1, 1)
      merged$delta_ratio <- merged$ratio_t1 - merged$ratio_t0
      # Decomposition
      merged$gdp_effect <- merged$delta_gdp * merged$ratio_t0
      merged$priority_effect <- merged$gdp_pc_usd_t0 * merged$delta_ratio
      merged$interaction <- merged$delta_gdp * merged$delta_ratio
      # CAGR
      n_years <- yr2 - yr1
      merged$cagr <- ((merged$che_pc_usd2023_t1 / pmax(merged$che_pc_usd2023_t0, 1))^(1/n_years) - 1) * 100
      merged
    })

    output$kpi_strip <- shiny::renderUI({
      dd <- decomp_data()
      shiny::req(nrow(dd) > 0)
      mod_v3_kpi_grid(
        mod_v3_kpi(sprintf("%d\u2013%d", input$period[1], input$period[2]),
                   "\u5bf9\u6bd4\u65f6\u6bb5", tone = "primary"),
        mod_v3_kpi(as.character(nrow(dd)),
                   "\u5206\u7ec4\u6570", tone = "neutral"),
        mod_v3_kpi(sprintf("%.1f%%", mean(dd$cagr, na.rm = TRUE)),
                   "\u5e73\u5747 CAGR", tone = "good"),
        mod_v3_kpi(fmt_v3_usd(mean(dd$delta_che, na.rm = TRUE)),
                   "\u5e73\u5747\u0394CHE", tone = "secondary")
      )
    })

    output$decomp_bar <- plotly::renderPlotly({
      dd <- decomp_data()
      shiny::req(nrow(dd) > 0)
      safe_plotly({
        plotly::plot_ly(dd, x = ~group, y = ~gdp_effect, type = "bar",
                        name = "GDP \u6548\u5e94", marker = list(color = "#1d3f5f")) |>
          plotly::add_trace(y = ~priority_effect, name = "\u4f18\u5148\u7ea7\u6548\u5e94",
                            marker = list(color = "#2a857a")) |>
          plotly::add_trace(y = ~interaction, name = "\u4ea4\u4e92\u9879",
                            marker = list(color = "#c89a3b")) |>
          ghs_plotly_layout() |>
          plotly::layout(
            barmode = "stack",
            xaxis = list(title = ""),
            yaxis = list(title = "\u0394CHE/cap (USD)"),
            legend = list(orientation = "h", y = -0.15)
          )
      })
    })

    output$cagr_bar <- plotly::renderPlotly({
      dd <- decomp_data()
      shiny::req(nrow(dd) > 0)
      dd <- dd[order(-dd$cagr), ]
      safe_plotly({
        plotly::plot_ly(dd, x = ~reorder(group, cagr), y = ~cagr,
                        type = "bar",
                        marker = list(color = ifelse(dd$cagr > 0, "#2a857a", "#a23b3b"))) |>
          ghs_plotly_layout() |>
          plotly::layout(
            xaxis = list(title = ""),
            yaxis = list(title = "CAGR (%)")
          )
      })
    })

    output$structure_change <- plotly::renderPlotly({
      dd <- decomp_data()
      shiny::req(nrow(dd) > 0)
      dd$d_gghed <- dd$gghed_che_t1 - dd$gghed_che_t0
      dd$d_pvtd <- dd$pvtd_che_t1 - dd$pvtd_che_t0
      dd$d_ext <- dd$ext_che_t1 - dd$ext_che_t0
      safe_plotly({
        plotly::plot_ly(dd, x = ~group, y = ~d_gghed, type = "bar",
                        name = "\u0394GGHED%", marker = list(color = "#1d3f5f")) |>
          plotly::add_trace(y = ~d_pvtd, name = "\u0394PVT-D%",
                            marker = list(color = "#c46327")) |>
          plotly::add_trace(y = ~d_ext, name = "\u0394EXT%",
                            marker = list(color = "#2a857a")) |>
          ghs_plotly_layout() |>
          plotly::layout(
            barmode = "group",
            xaxis = list(title = ""),
            yaxis = list(title = "\u5360\u6bd4\u53d8\u5316 (pp)"),
            legend = list(orientation = "h", y = -0.15)
          )
      })
    })

    output$growth_dist <- plotly::renderPlotly({
      m <- master_r()
      yr1 <- input$period[1]; yr2 <- input$period[2]
      grp <- input$group_by
      d1 <- m[m$year == yr1 & is.finite(m$che_pc_usd2023), c("iso3_code", grp, "che_pc_usd2023")]
      d2 <- m[m$year == yr2 & is.finite(m$che_pc_usd2023), c("iso3_code", "che_pc_usd2023")]
      names(d1)[3] <- "che_t0"; names(d2)[2] <- "che_t1"
      merged <- merge(d1, d2, by = "iso3_code")
      n_years <- yr2 - yr1
      merged$cagr <- ((merged$che_t1 / pmax(merged$che_t0, 1))^(1/n_years) - 1) * 100
      merged <- merged[is.finite(merged$cagr) & !is.na(merged[[grp]]), ]
      shiny::req(nrow(merged) > 10)
      pal <- if (grp == "income_group") unname(brand_palette$income)
             else unname(brand_palette$continent)
      safe_plotly({
        plotly::plot_ly(merged, x = stats::as.formula(paste0("~", grp)),
                        y = ~cagr, type = "box",
                        color = stats::as.formula(paste0("~", grp)),
                        colors = pal) |>
          ghs_plotly_layout() |>
          plotly::layout(
            showlegend = FALSE,
            xaxis = list(title = ""),
            yaxis = list(title = "CAGR (%)")
          )
      })
    })

    output$decomp_table <- reactable::renderReactable({
      dd <- decomp_data()
      shiny::req(nrow(dd) > 0)
      tab <- data.frame(
        "\u5206\u7ec4" = dd$group,
        "CHE_t0" = round(dd$che_pc_usd2023_t0, 0),
        "CHE_t1" = round(dd$che_pc_usd2023_t1, 0),
        "\u0394CHE" = round(dd$delta_che, 0),
        "GDP\u6548\u5e94" = round(dd$gdp_effect, 0),
        "\u4f18\u5148\u7ea7" = round(dd$priority_effect, 0),
        "\u4ea4\u4e92\u9879" = round(dd$interaction, 0),
        "CAGR%" = round(dd$cagr, 2),
        check.names = FALSE
      )
      reactable::reactable(tab, pagination = FALSE, highlight = TRUE,
        striped = TRUE,
        defaultColDef = reactable::colDef(
          headerStyle = list(background = "#f1f3f7", fontWeight = 700)))
    })
  })
}
