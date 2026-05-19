# =============================================================================
# 仪表盘/模块/mod_transition.R
# 转型分析：国家收入组晋升与卫生筹资结构转型
# =============================================================================

mod_transition_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = htmltools::HTML("&#128260; \u8f6c\u578b Transition"),
    value = "transition",
    mod_v3_hero(
      kicker = "FINANCING TRANSITION",
      title = "\u536b\u751f\u7b79\u8d44\u8f6c\u578b\u4e0e\u6536\u5165\u664b\u5347",
      lead = paste(
        "\u968f\u7740\u56fd\u5bb6\u7ecf\u6d4e\u589e\u957f\uff0c\u536b\u751f\u7b79\u8d44\u7ed3\u6784\u4ece\u5916\u63f4\u4f9d\u8d56\u578b\u8f6c\u5411\u653f\u5e9c\u4e3b\u5bfc\u578b\u3002",
        "\u672c\u6a21\u5757\u8ffd\u8e2a\u56fd\u5bb6\u6536\u5165\u7ec4\u664b\u5347\u4e0e\u7b79\u8d44\u7ed3\u6784\u8f6c\u53d8\u7684\u5173\u8054\u3002"
      ),
      meta = list("\u6536\u5165\u7ec4\u664b\u5347", "\u7b79\u8d44\u8f6c\u578b", "2000\u20132023")
    ),
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 260,
        shiny::sliderInput(ns("year_range"), "\u65f6\u6bb5",
          min = 2000, max = 2023, value = c(2000, 2023), step = 1, sep = ""),
        shiny::selectInput(ns("focus_transition"), "\u805a\u7126\u8f6c\u578b",
          choices = c(
            "\u5168\u90e8" = "all",
            "\u4f4e\u2192\u4e2d\u4f4e" = "low_to_lm",
            "\u4e2d\u4f4e\u2192\u4e2d\u9ad8" = "lm_to_um",
            "\u4e2d\u9ad8\u2192\u9ad8" = "um_to_high"
          ), selected = "all"),
        shiny::tags$hr(),
        shiny::helpText(
          "\u6536\u5165\u7ec4\u664b\u5347\u57fa\u4e8e\u4e16\u754c\u94f6\u884c\u5e74\u5ea6\u5206\u7c7b\u3002",
          "\u7b5b\u8d44\u8f6c\u578b = EXT \u4e0b\u964d + GGHED \u4e0a\u5347\u3002"
        )
      ),
      shiny::uiOutput(ns("kpi_strip")),
      # Row 1
      bslib::layout_columns(
        col_widths = c(7, 5),
        mod_card(
          title = "\u7b79\u8d44\u8f6c\u578b\u8f68\u8ff9",
          htmltools::p(class = "card-note",
            "\u6a2a\u8f74 = EXT \u5360\u6bd4\uff0c\u7eb5\u8f74 = GGHED \u5360\u6bd4\u3002\u7bad\u5934 = \u65f6\u95f4\u65b9\u5411\u3002"),
          mod_spinner(plotly::plotlyOutput(ns("transition_path"), height = 440))
        ),
        mod_card(
          title = "\u6536\u5165\u7ec4\u53d8\u52a8\u7edf\u8ba1",
          htmltools::p(class = "card-note",
            "\u5404\u7c7b\u664b\u5347/\u964d\u7ea7\u7684\u56fd\u5bb6\u6570\u91cf\u3002"),
          mod_spinner(plotly::plotlyOutput(ns("transition_sankey"), height = 440))
        )
      ),
      # Row 2
      bslib::layout_columns(
        col_widths = c(6, 6),
        mod_card(
          title = "\u664b\u5347\u56fd\u5bb6\u7684\u7b5b\u8d44\u53d8\u5316",
          htmltools::p(class = "card-note",
            "\u664b\u5347\u56fd\u5bb6\u5728\u664b\u5347\u524d\u540e\u7684 GGHED/EXT/OOPS \u53d8\u5316\u3002"),
          mod_spinner(plotly::plotlyOutput(ns("pre_post_bar"), height = 380))
        ),
        mod_card(
          title = "GDP \u4e0e\u7b5b\u8d44\u7ed3\u6784\u7684\u5173\u8054",
          htmltools::p(class = "card-note",
            "\u6a2a\u8f74 = GDP/cap\uff0c\u7eb5\u8f74 = GGHED%\u3002\u968f\u6536\u5165\u4e0a\u5347\uff0c\u653f\u5e9c\u5360\u6bd4\u8d8b\u5347\u3002"),
          mod_spinner(plotly::plotlyOutput(ns("gdp_gghed_scatter"), height = 380))
        )
      ),
      mod_card(
        title = "\u8f6c\u578b\u56fd\u5bb6\u8be6\u8868",
        mod_spinner(reactable::reactableOutput(ns("transition_table")))
      )
    )
  )
}

mod_transition_server <- function(id, master_r) {
  shiny::moduleServer(id, function(input, output, session) {

    transition_countries <- shiny::reactive({
      m <- master_r()
      yr1 <- input$year_range[1]; yr2 <- input$year_range[2]
      d1 <- m[m$year == yr1 & !is.na(m$income_group),
              c("iso3_code", "country_name", "continent", "income_group")]
      d2 <- m[m$year == yr2 & !is.na(m$income_group),
              c("iso3_code", "income_group")]
      names(d1)[4] <- "income_t0"; names(d2)[2] <- "income_t1"
      merged <- merge(d1, d2, by = "iso3_code")
      merged$changed <- merged$income_t0 != merged$income_t1
      # Determine direction
      levels_order <- c("Low income", "Lower middle income",
                        "Upper middle income", "High income")
      merged$level_t0 <- match(merged$income_t0, levels_order)
      merged$level_t1 <- match(merged$income_t1, levels_order)
      merged$direction <- ifelse(merged$level_t1 > merged$level_t0, "upgrade",
                          ifelse(merged$level_t1 < merged$level_t0, "downgrade", "stable"))
      merged
    })

    output$kpi_strip <- shiny::renderUI({
      tc <- transition_countries()
      n_up <- sum(tc$direction == "upgrade", na.rm = TRUE)
      n_down <- sum(tc$direction == "downgrade", na.rm = TRUE)
      mod_v3_kpi_grid(
        mod_v3_kpi(format(nrow(tc), big.mark = ","),
                   "\u56fd\u5bb6\u6570", tone = "primary"),
        mod_v3_kpi(as.character(n_up),
                   "\u664b\u5347\u56fd\u5bb6", tone = "good"),
        mod_v3_kpi(as.character(n_down),
                   "\u964d\u7ea7\u56fd\u5bb6", tone = "bad"),
        mod_v3_kpi(sprintf("%d\u2013%d", input$year_range[1], input$year_range[2]),
                   "\u65f6\u6bb5", tone = "neutral")
      )
    })

    output$transition_path <- plotly::renderPlotly({
      m <- master_r()
      yr1 <- input$year_range[1]; yr2 <- input$year_range[2]
      # Get start and end points for each country
      d1 <- m[m$year == yr1 & is.finite(m$ext_che) & is.finite(m$gghed_che),
              c("iso3_code", "country_name", "income_group", "ext_che", "gghed_che")]
      d2 <- m[m$year == yr2 & is.finite(m$ext_che) & is.finite(m$gghed_che),
              c("iso3_code", "ext_che", "gghed_che")]
      names(d1)[4:5] <- c("ext_t0", "gghed_t0")
      names(d2)[2:3] <- c("ext_t1", "gghed_t1")
      merged <- merge(d1, d2, by = "iso3_code")
      # Filter to countries with significant change
      merged$ext_change <- merged$ext_t1 - merged$ext_t0
      merged$gghed_change <- merged$gghed_t1 - merged$gghed_t0
      merged <- merged[abs(merged$ext_change) > 3 | abs(merged$gghed_change) > 3, ]
      shiny::req(nrow(merged) > 3)
      safe_plotly({
        p <- plotly::plot_ly()
        for (i in seq_len(min(nrow(merged), 30))) {
          row <- merged[i, ]
          p <- plotly::add_trace(p,
            x = c(row$ext_t0, row$ext_t1),
            y = c(row$gghed_t0, row$gghed_t1),
            type = "scatter", mode = "lines+markers",
            line = list(color = "rgba(29,63,95,0.3)", width = 1),
            marker = list(size = c(4, 8), color = c("#5d667a", "#1d3f5f")),
            text = row$country_name, name = row$country_name,
            showlegend = FALSE
          )
        }
        p |> ghs_plotly_layout() |>
          plotly::layout(
            xaxis = list(title = "EXT \u5360 CHE (%)"),
            yaxis = list(title = "GGHE-D \u5360 CHE (%)")
          )
      })
    })

    output$transition_sankey <- plotly::renderPlotly({
      tc <- transition_countries()
      # Count transitions
      trans_counts <- as.data.frame(table(tc$income_t0, tc$income_t1))
      names(trans_counts) <- c("from", "to", "n")
      trans_counts <- trans_counts[trans_counts$n > 0, ]
      trans_counts <- trans_counts[order(-trans_counts$n), ]
      top_trans <- utils::head(trans_counts, 10)
      top_trans$label <- sprintf("%s \u2192 %s", top_trans$from, top_trans$to)
      top_trans$label <- factor(top_trans$label, levels = rev(top_trans$label))
      safe_plotly({
        plotly::plot_ly(top_trans, x = ~n, y = ~label,
                        type = "bar", orientation = "h",
                        marker = list(color = ifelse(
                          as.character(top_trans$from) == as.character(top_trans$to),
                          "#5d667a", "#1d3f5f"
                        ))) |>
          ghs_plotly_layout() |>
          plotly::layout(
            xaxis = list(title = "\u56fd\u5bb6\u6570"),
            yaxis = list(title = ""),
            margin = list(l = 200)
          )
      })
    })

    output$pre_post_bar <- plotly::renderPlotly({
      m <- master_r(); tc <- transition_countries()
      upgraded <- tc[tc$direction == "upgrade", "iso3_code"]
      if (length(upgraded) < 3) {
        return(safe_plotly(plotly::plotly_empty()))
      }
      yr1 <- input$year_range[1]; yr2 <- input$year_range[2]
      d1 <- m[m$year == yr1 & m$iso3_code %in% upgraded, ]
      d2 <- m[m$year == yr2 & m$iso3_code %in% upgraded, ]
      indicators <- c("gghed_che", "hf3_che", "ext_che")
      labels <- c("GGHED%", "OOPS%", "EXT%")
      before <- vapply(indicators, function(ind) mean(d1[[ind]], na.rm = TRUE), numeric(1))
      after <- vapply(indicators, function(ind) mean(d2[[ind]], na.rm = TRUE), numeric(1))
      df <- data.frame(indicator = labels, before = before, after = after)
      safe_plotly({
        plotly::plot_ly(df, x = ~indicator, y = ~before, type = "bar",
                        name = sprintf("%d", yr1), marker = list(color = "#5d667a")) |>
          plotly::add_trace(y = ~after, name = sprintf("%d", yr2),
                            marker = list(color = "#1d3f5f")) |>
          ghs_plotly_layout() |>
          plotly::layout(
            barmode = "group",
            xaxis = list(title = ""),
            yaxis = list(title = "\u5360 CHE (%)"),
            legend = list(orientation = "h", y = -0.15)
          )
      })
    })

    output$gdp_gghed_scatter <- plotly::renderPlotly({
      m <- master_r(); yr <- input$year_range[2]
      d <- m[m$year == yr & is.finite(m$gdp_pc_usd) & is.finite(m$gghed_che) &
             !is.na(m$income_group), ]
      shiny::req(nrow(d) > 5)
      safe_plotly({
        plotly::plot_ly(d, x = ~gdp_pc_usd, y = ~gghed_che,
                        color = ~income_group, text = ~country_name,
                        type = "scatter", mode = "markers",
                        colors = unname(brand_palette$income),
                        marker = list(size = 8, opacity = 0.7)) |>
          ghs_plotly_layout() |>
          plotly::layout(
            xaxis = list(title = "GDP per capita (USD)", type = "log"),
            yaxis = list(title = "GGHE-D / CHE (%)"),
            legend = list(orientation = "h", y = -0.15)
          )
      })
    })

    output$transition_table <- reactable::renderReactable({
      tc <- transition_countries()
      tc <- tc[tc$changed, ]
      tc <- tc[order(-tc$level_t1), ]
      tab <- data.frame(
        "\u56fd\u5bb6" = tc$country_name,
        "\u5927\u6d32" = tc$continent,
        "\u8d77\u59cb\u7ec4" = tc$income_t0,
        "\u5f53\u524d\u7ec4" = tc$income_t1,
        "\u65b9\u5411" = ifelse(tc$direction == "upgrade", "\u2191 \u664b\u5347", "\u2193 \u964d\u7ea7"),
        check.names = FALSE
      )
      if (nrow(tab) == 0) {
        tab <- data.frame("\u4fe1\u606f" = "\u65e0\u6536\u5165\u7ec4\u53d8\u52a8\u56fd\u5bb6", check.names = FALSE)
      }
      reactable::reactable(tab, defaultPageSize = 15, searchable = TRUE,
        highlight = TRUE, striped = TRUE,
        defaultColDef = reactable::colDef(
          headerStyle = list(background = "#f1f3f7", fontWeight = 700)))
    })
  })
}
