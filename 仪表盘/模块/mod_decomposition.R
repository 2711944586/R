# =============================================================================
# 仪表盘/模块/mod_decomposition.R
# 分解分析：卫生支出增长的来源分解与结构变迁
# =============================================================================

mod_decomposition_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = "\u5206\u89e3 Decompose",
    value = "decomposition",
    mod_v3_hero(
      kicker = "\u589e\u957f\u5206\u89e3",
      title = "\u536b\u751f\u652f\u51fa\u589e\u957f\u7684\u6765\u6e90\u5206\u89e3",
      lead = paste(
        "\u4eba\u5747 CHE \u589e\u957f\u53ef\u5206\u89e3\u4e3a\uff1aGDP \u589e\u957f\u6548\u5e94 + \u536b\u751f\u4f18\u5148\u7ea7\u6548\u5e94 + \u4ea4\u4e92\u9879\u3002",
        "\u672c\u6a21\u5757\u5c55\u793a\u4e0d\u540c\u56fd\u5bb6\u7ec4\u522b\u7684\u589e\u957f\u9a71\u52a8\u529b\u5dee\u5f02\u3002"
      ),
      meta = list("Oaxaca-Blinder \u5206\u89e3", "Shapley \u5f52\u56e0", "2000\u20132023")
    ),
    mod_v3_page_body(
      wide = TRUE,
      mod_v3_topic_brief("decomposition"),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 280,
          shiny::sliderInput(ns("period"), "\u5bf9\u6bd4\u65f6\u6bb5",
            min = 2000, max = 2023, value = c(2005, 2020), step = 1, sep = ""),
          shiny::selectInput(ns("group_by"), "\u5206\u7ec4",
            choices = c("\u6536\u5165\u7ec4" = "income_group",
                        "\u5927\u6d32" = "continent"),
            selected = "income_group"),
          mod_v3_sidebar_note(
            "\u5206\u89e3\u516c\u5f0f",
            "\u0394CHE/cap \u88ab\u62c6\u6210 GDP \u6548\u5e94\u3001\u536b\u751f\u4f18\u5148\u7ea7\u6548\u5e94\u548c\u4ea4\u4e92\u9879\uff1a\u4e00\u90e8\u5206\u6765\u81ea\u7ecf\u6d4e\u57fa\u6570\u589e\u957f\uff0c\u4e00\u90e8\u5206\u6765\u81ea CHE/GDP \u6bd4\u4f8b\u53d8\u5316\u3002",
            bullets = c(
              "\u9009\u62e9\u8f83\u957f\u65f6\u6bb5\u53ef\u51cf\u5c11\u5355\u5e74\u6ce2\u52a8\u5e72\u6270\u3002",
              "\u6309\u6536\u5165\u7ec4\u66f4\u9002\u5408\u8bfb\u589e\u957f\u9636\u6bb5\uff0c\u6309\u5927\u6d32\u66f4\u9002\u5408\u8bfb\u533a\u57df\u8def\u5f84\u3002",
              "\u4ea4\u4e92\u9879\u4e3a\u6b63\u65f6\uff0c\u8868\u793a GDP \u589e\u957f\u4e0e\u536b\u751f\u4f18\u5148\u7ea7\u62ac\u5347\u540c\u65f6\u53d1\u751f\u3002"
            )
          )
        ),
        shiny::uiOutput(ns("kpi_strip")),
        bslib::layout_columns(
          col_widths = c(7, 5),
          mod_card(
            kicker = "F1 · Driver stack",
            title = "\u589e\u957f\u5206\u89e3\u5806\u53e0\u56fe",
            htmltools::p(class = "card-note",
              "\u5404\u7ec4\u7684 CHE/cap \u589e\u957f\u5206\u89e3\u4e3a GDP \u6548\u5e94\u3001\u4f18\u5148\u7ea7\u6548\u5e94\u3001\u4ea4\u4e92\u9879\u3002"),
            mod_v3_chart_guide(
              "\u770b\u589e\u957f\u6765\u81ea\u89c4\u6a21\u8fd8\u662f\u4f18\u5148\u7ea7",
              "\u5982\u679c GDP \u6548\u5e94\u5360\u4e3b\u5bfc\uff0c\u536b\u751f\u652f\u51fa\u589e\u957f\u4e3b\u8981\u968f\u7ecf\u6d4e\u6269\u5f20\u800c\u6765\uff1b\u5982\u679c\u4f18\u5148\u7ea7\u6548\u5e94\u66f4\u5927\uff0c\u8868\u793a\u536b\u751f\u5728\u7ecf\u6d4e\u4e2d\u7684\u5206\u914d\u6743\u91cd\u4e0a\u5347\u3002",
              bullets = c("\u5806\u53e0\u987a\u5e8f\u4e0d\u4ee3\u8868\u56e0\u679c\u987a\u5e8f\u3002", "\u8d1f\u503c\u5206\u91cf\u4ee3\u8868\u5bf9\u603b\u589e\u957f\u7684\u62d6\u7d2f\u3002")
            ),
            mod_spinner(plotly::plotlyOutput(ns("decomp_bar"), height = 440))
          ),
          mod_card(
            kicker = "F2 · CAGR contrast",
            title = "\u589e\u957f\u7387\u5bf9\u6bd4",
            htmltools::p(class = "card-note",
              "\u5404\u7ec4\u5728\u9009\u5b9a\u65f6\u6bb5\u5185\u7684\u5e74\u5747\u590d\u5408\u589e\u957f\u7387 (CAGR)\u3002"),
            mod_v3_chart_guide(
              "\u589e\u957f\u7387\u4f7f\u4e0d\u540c\u57fa\u6570\u53ef\u6bd4",
              "CAGR \u628a\u591a\u5e74\u7d2f\u8ba1\u53d8\u5316\u8f6c\u4e3a\u5e74\u5316\u901f\u5ea6\uff0c\u9002\u5408\u6bd4\u8f83\u4f4e\u57fa\u6570\u5feb\u589e\u957f\u548c\u9ad8\u57fa\u6570\u7a33\u5b9a\u589e\u957f\u3002",
              tone = "warn"
            ),
            mod_spinner(plotly::plotlyOutput(ns("cagr_bar"), height = 440)),
            footer = "CAGR \u5bf9\u671f\u521d\u503c\u654f\u611f\uff0c\u4f4e\u57fa\u6570\u56fd\u5bb6\u7684\u9ad8\u589e\u901f\u9700\u8981\u4e0e\u7edd\u5bf9\u589e\u91cf\u4e00\u8d77\u89e3\u91ca\u3002"
          )
        ),
        bslib::layout_columns(
          col_widths = c(6, 6),
          mod_card(
            kicker = "F3 · Financing shift",
            title = "\u7ed3\u6784\u53d8\u8fc1\uff1a\u4e09\u6e90\u5360\u6bd4\u53d8\u5316",
            htmltools::p(class = "card-note",
              "\u5404\u7ec4\u7684 GGHED/PVT-D/EXT \u5360\u6bd4\u5728\u4e24\u4e2a\u65f6\u70b9\u7684\u53d8\u5316\u3002"),
            mod_v3_chart_guide(
              "\u589e\u957f\u8fd8\u8981\u770b\u8c01\u5728\u4ed8\u8d39",
              "\u516c\u5171\u652f\u51fa\u5360\u6bd4\u4e0a\u5347\u548c OOPS \u5360\u6bd4\u4e0b\u964d\uff0c\u901a\u5e38\u4ee3\u8868\u7b79\u8d44\u4fdd\u62a4\u7ed3\u6784\u6539\u5584\uff1b\u5916\u63f4\u5360\u6bd4\u53d8\u5316\u5219\u63d0\u793a\u5bf9\u5916\u90e8\u8d44\u91d1\u7684\u4f9d\u8d56\u7a0b\u5ea6\u3002",
              tone = "good"
            ),
            mod_spinner(plotly::plotlyOutput(ns("structure_change"), height = 380))
          ),
          mod_card(
            kicker = "F4 · Country dispersion",
            title = "\u56fd\u5bb6\u7ea7\u589e\u957f\u5206\u5e03",
            htmltools::p(class = "card-note",
              "\u5404\u56fd CHE/cap \u5e74\u5747\u589e\u957f\u7387\u7684\u5206\u5e03\uff0c\u6309\u7ec4\u7740\u8272\u3002"),
            mod_v3_chart_guide(
              "\u7ec4\u5747\u503c\u4e4b\u540e\u8981\u770b\u7ec4\u5185\u79bb\u6563",
              "\u540c\u4e00\u7ec4\u5185\u56fd\u5bb6\u7684\u5dee\u5f02\u4f1a\u51b3\u5b9a\u653f\u7b56\u7ed3\u8bba\u80fd\u5426\u63a8\u5e7f\u5230\u6574\u4e2a\u7ec4\u522b\uff1b\u5bbd\u7bb1\u4f53\u8bf4\u660e\u7ec4\u5185\u8def\u5f84\u5e76\u4e0d\u4e00\u81f4\u3002"
            ),
            mod_spinner(plotly::plotlyOutput(ns("growth_dist"), height = 380))
          )
        ),
        mod_card(
          kicker = "F5 · Decomposition audit",
          title = "\u5206\u89e3\u7ed3\u679c\u8868",
          htmltools::p(class = "card-note",
            "\u5404\u7ec4\u7684\u589e\u957f\u5206\u89e3\u6570\u503c\u3002"),
          mod_v3_chart_guide(
            "\u8868\u683c\u4fdd\u7559\u589e\u91cf\u548c\u5206\u91cf",
            "\u56fe\u8868\u7528\u4e8e\u5feb\u901f\u5224\u65ad\u4e3b\u5bfc\u56e0\u7d20\uff0c\u8868\u683c\u5219\u7528\u6765\u6838\u5bf9\u6bcf\u4e2a\u7ec4\u7684 \u0394CHE\u3001GDP \u6548\u5e94\u3001\u4f18\u5148\u7ea7\u6548\u5e94\u548c\u4ea4\u4e92\u9879\u3002"
          ),
          mod_spinner(reactable::reactableOutput(ns("decomp_table"))),
          footer = "\u5206\u89e3\u662f\u7b97\u672f\u5f52\u56e0\uff0c\u5e76\u975e\u56e0\u679c\u8bc6\u522b\uff1b\u89e3\u91ca\u65f6\u5e94\u7ed3\u5408\u8d22\u653f\u548c\u7b79\u8d44\u6a21\u5757\u3002"
        )
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
