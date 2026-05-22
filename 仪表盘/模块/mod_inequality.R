
mod_inequality_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = "\u4e0d\u5e73\u7b49 Inequality",
    value = "inequality",
    mod_v3_hero(
      kicker = "\u4e0d\u5e73\u7b49\u6307\u6570",
      title = "\u536b\u751f\u652f\u51fa\u4e0d\u5e73\u7b49\u6307\u6570",
      lead = paste(
        "\u5168\u7403\u4eba\u5747 CHE \u7684 Gini \u7cfb\u6570\u7ea6 0.55\uff0c\u8fdc\u9ad8\u4e8e\u6536\u5165\u4e0d\u5e73\u7b49\u3002",
        "\u672c\u6a21\u5757\u8ba1\u7b97 Gini\u3001Theil-T\u3001Atkinson \u4e09\u79cd\u6307\u6570\uff0c",
        "\u5e76\u5c06 Theil \u5206\u89e3\u4e3a\u7ec4\u95f4\u4e0e\u7ec4\u5185\u4e24\u4e2a\u5206\u91cf\u3002"
      ),
      meta = list("Gini / Theil-T / Atkinson", "\u7ec4\u95f4-\u7ec4\u5185\u5206\u89e3", "2000\u20132023")
    ),
    mod_v3_page_body(
      wide = TRUE,
      mod_v3_topic_brief("inequality"),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 280,
          shiny::selectInput(ns("indicator"), "\u4e0d\u5e73\u7b49\u6307\u6807",
            choices = c(
              "\u4eba\u5747 CHE (USD)" = "che_pc_usd2023",
              "OOPS (%)" = "hf3_che",
              "GGHE-D (%)" = "gghed_che"
            ), selected = "che_pc_usd2023"),
          shiny::selectInput(ns("group_by"), "\u5206\u89e3\u7ef4\u5ea6",
            choices = c("\u6536\u5165\u7ec4" = "income_group",
                        "\u5927\u6d32" = "continent"),
            selected = "income_group"),
          shiny::sliderInput(ns("atkinson_e"), "Atkinson \u4e0d\u5e73\u7b49\u538c\u6076\u53c2\u6570 \u03b5",
            min = 0.5, max = 2.0, value = 1.0, step = 0.25),
          mod_v3_sidebar_note(
            "\u6307\u6570\u89e3\u91ca",
            "Gini \u770b\u603b\u4f53\u96c6\u4e2d\u5ea6\uff0cTheil-T \u53ef\u4ee5\u5206\u89e3\u4e3a\u7ec4\u95f4\u548c\u7ec4\u5185\u6765\u6e90\uff0cAtkinson \u901a\u8fc7 \u03b5 \u8c03\u8282\u5bf9\u5e95\u90e8\u56fd\u5bb6\u7684\u654f\u611f\u5ea6\u3002",
            bullets = c(
              "\u03b5 \u8d8a\u5927\uff0cAtkinson \u8d8a\u5173\u6ce8\u4f4e\u652f\u51fa\u6216\u4f4e\u4fdd\u62a4\u7aef\u3002",
              "\u7ec4\u95f4\u5360\u6bd4\u9ad8\uff0c\u8bf4\u660e\u6536\u5165\u7ec4/\u533a\u57df\u5206\u5c42\u89e3\u91ca\u529b\u5f3a\u3002",
              "\u7ec4\u5185\u5360\u6bd4\u9ad8\uff0c\u5e73\u5747\u7ec4\u522b\u5dee\u5f02\u4e0d\u8db3\u4ee5\u6982\u62ec\u56fd\u5bb6\u95f4\u5dee\u8ddd\u3002"
            )
          )
        ),
      shiny::uiOutput(ns("kpi_strip")),
      bslib::layout_columns(
        col_widths = c(7, 5),
        mod_card(
          kicker = "I1 \u00b7 Index path",
          title = "\u4e0d\u5e73\u7b49\u6307\u6570\u65f6\u5e8f\u6f14\u5316",
          htmltools::p(class = "card-note",
            "Gini\u3001Theil-T\u3001Atkinson \u4e09\u79cd\u6307\u6570\u7684 24 \u5e74\u8f68\u8ff9\u3002"),
          mod_v3_chart_guide(
            "\u5148\u770b\u65b9\u5411\uff0c\u518d\u770b\u54ea\u4e2a\u6307\u6570\u53d8\u5f97\u66f4\u5feb",
            "\u5982\u679c Gini \u7a33\u5b9a\u4f46 Theil \u4e0a\u5347\uff0c\u5f80\u5f80\u8868\u793a\u5c3e\u90e8\u6216\u7ec4\u522b\u95f4\u7684\u6781\u7aef\u5dee\u5f02\u5728\u589e\u5f3a\uff1bAtkinson \u5bf9\u53c2\u6570 \u03b5 \u7684\u53cd\u5e94\u5219\u63d0\u793a\u5e95\u90e8\u56fd\u5bb6\u7684\u60c5\u51b5\u3002",
            bullets = c("\u4e0b\u884c\u4ee3\u8868\u5206\u5e03\u66f4\u63a5\u8fd1\u5e73\u7b49\u3002", "\u6307\u6570\u6c34\u5e73\u4e0d\u5e94\u8de8\u6307\u6807\u76f4\u63a5\u6bd4\u8f83\u3002")
          ),
          mod_spinner(plotly::plotlyOutput(ns("indices_trend"), height = 440)),
          footer = "\u65f6\u5e8f\u6307\u6570\u57fa\u4e8e\u5f53\u5e74\u6709\u6548\u6837\u672c\u8ba1\u7b97\uff0c\u6837\u672c\u8986\u76d6\u53d8\u5316\u4f1a\u5f71\u54cd\u5e74\u9645\u53ef\u6bd4\u6027\u3002"
        ),
        mod_card(
          kicker = "I2 \u00b7 Lorenz shape",
          title = "Lorenz \u66f2\u7ebf\uff08\u6700\u65b0\u5e74\uff09",
          htmltools::p(class = "card-note",
            "\u66f2\u7ebf\u8d8a\u8fdc\u79bb\u5bf9\u89d2\u7ebf = \u4e0d\u5e73\u7b49\u8d8a\u4e25\u91cd\u3002"),
          mod_v3_chart_guide(
            "\u66f2\u7ebf\u5f2f\u66f2\u5904\u662f\u5206\u5e03\u6545\u4e8b\u7684\u5f00\u53e3",
            "\u5de6\u4e0b\u6bb5\u8d34\u8fd1\u6a2a\u8f74\uff0c\u8868\u793a\u5e95\u90e8\u56fd\u5bb6\u5360\u6709\u7684\u6307\u6807\u4efd\u989d\u5f88\u4f4e\uff1b\u53f3\u4e0a\u6bb5\u7a81\u7136\u62ac\u5347\uff0c\u8868\u793a\u5c11\u6570\u9ad8\u503c\u56fd\u5bb6\u8d21\u732e\u4e86\u5927\u90e8\u5206\u603b\u91cf\u3002",
            tone = "warn"
          ),
          mod_spinner(plotly::plotlyOutput(ns("lorenz_curve"), height = 440)),
          footer = "Lorenz \u66f2\u7ebf\u6309\u56fd\u5bb6\u7b49\u6743\u6392\u5e8f\uff0c\u4e0d\u662f\u6309\u4eba\u53e3\u6743\u91cd\u6392\u5e8f\u3002"
        )
      ),
      bslib::layout_columns(
        col_widths = c(6, 6),
        mod_card(
          kicker = "I3 \u00b7 Decomposition",
          title = "Theil \u5206\u89e3\uff1a\u7ec4\u95f4 vs \u7ec4\u5185",
          htmltools::p(class = "card-note",
            "\u7ec4\u95f4\u4e0d\u5e73\u7b49\u53cd\u6620\u6536\u5165\u7ec4/\u5927\u6d32\u95f4\u5dee\u5f02\uff0c\u7ec4\u5185\u53cd\u6620\u540c\u7ec4\u56fd\u5bb6\u95f4\u5dee\u5f02\u3002"),
          mod_v3_chart_guide(
            "\u8fd9\u5f20\u56fe\u56de\u7b54\u201c\u5dee\u8ddd\u6765\u81ea\u54ea\u91cc\u201d",
            "\u7ec4\u95f4\u9762\u79ef\u8d8a\u5927\uff0c\u8bf4\u660e\u6536\u5165\u7ec4\u6216\u5927\u6d32\u7684\u5e73\u5747\u5dee\u8ddd\u662f\u4e3b\u8981\u6765\u6e90\uff1b\u7ec4\u5185\u9762\u79ef\u8d8a\u5927\uff0c\u5219\u8bf4\u660e\u540c\u4e00\u7ec4\u5185\u56fd\u5bb6\u8fd8\u6709\u5f88\u5f3a\u5f02\u8d28\u6027\u3002",
            tone = "good"
          ),
          mod_spinner(plotly::plotlyOutput(ns("theil_decomp"), height = 380)),
          footer = "Theil \u5206\u89e3\u4f7f\u7528\u5f53\u524d\u9009\u62e9\u7684\u5206\u7ec4\u53e3\u5f84\uff0c\u5207\u6362\u5206\u7ec4\u4f1a\u6539\u53d8\u7ec4\u95f4/\u7ec4\u5185\u5360\u6bd4\u3002"
        ),
        mod_card(
          kicker = "I4 \u00b7 Within-group Gini",
          title = "\u5206\u7ec4 Gini \u5bf9\u6bd4",
          htmltools::p(class = "card-note",
            "\u5404\u7ec4\u5185\u90e8\u7684 Gini \u7cfb\u6570\u5bf9\u6bd4\u3002"),
          mod_v3_chart_guide(
            "\u7528\u7ec4\u5185 Gini \u627e\u51fa\u201c\u5e73\u5747\u503c\u6700\u4f1a\u9a97\u4eba\u201d\u7684\u7ec4",
            "\u5982\u679c\u67d0\u7ec4\u7684\u7ec4\u5185 Gini \u9ad8\uff0c\u90a3\u4e48\u8be5\u7ec4\u7684\u5e73\u5747\u503c\u5f88\u53ef\u80fd\u63a9\u76d6\u4e86\u5c11\u6570\u9ad8\u503c\u6216\u4f4e\u503c\u56fd\u5bb6\u3002",
            tone = "warn"
          ),
          mod_spinner(plotly::plotlyOutput(ns("group_gini"), height = 380))
        )
      ),
      mod_card(
        kicker = "I5 \u00b7 Inequality audit",
        title = "\u4e0d\u5e73\u7b49\u6307\u6570\u8be6\u8868",
        mod_v3_chart_guide(
          "\u8868\u683c\u7528\u4e8e\u8ddf\u8e2a\u6bcf\u5e74\u8ba1\u7b97\u7ed3\u679c",
          "\u5f53\u56fe\u4e0a\u770b\u5230\u67d0\u4e2a\u5e74\u4efd\u6307\u6570\u7a81\u7136\u53d8\u5316\u65f6\uff0c\u5148\u56de\u5230\u8868\u683c\u68c0\u67e5 N \u662f\u5426\u53d8\u5316\uff0c\u518d\u5224\u65ad\u662f\u771f\u5b9e\u5206\u5e03\u53d8\u5316\u8fd8\u662f\u6837\u672c\u8986\u76d6\u53d8\u5316\u3002"
        ),
        mod_spinner(reactable::reactableOutput(ns("ineq_table"))),
        footer = "\u6240\u6709\u6307\u6570\u4ec5\u4f7f\u7528\u5927\u4e8e 0 \u4e14\u6709\u6548\u7684\u5f53\u5e74\u89c2\u6d4b\u503c\u8ba1\u7b97\u3002"
      )
      )
    )
  )
}

mod_inequality_server <- function(id, master_r) {
  shiny::moduleServer(id, function(input, output, session) {

    calc_gini <- function(x) {
      x <- sort(x[is.finite(x) & x > 0])
      n <- length(x)
      if (n < 3) return(NA_real_)
      2 * sum(seq_len(n) * x) / (n * sum(x)) - (n + 1) / n
    }

    calc_theil <- function(x) {
      x <- x[is.finite(x) & x > 0]
      n <- length(x)
      if (n < 3) return(NA_real_)
      mu <- mean(x)
      sum((x / mu) * log(x / mu)) / n
    }

    calc_atkinson <- function(x, e = 1) {
      x <- x[is.finite(x) & x > 0]
      n <- length(x)
      if (n < 3) return(NA_real_)
      mu <- mean(x)
      if (abs(e - 1) < 0.01) {
        1 - exp(mean(log(x))) / mu
      } else {
        1 - (mean(x^(1 - e))^(1 / (1 - e))) / mu
      }
    }

    output$kpi_strip <- shiny::renderUI({
      m <- master_r(); ind <- input$indicator
      latest <- max(m$year, na.rm = TRUE)
      vals <- m[[ind]][m$year == latest]
      g <- calc_gini(vals)
      th <- calc_theil(vals)
      at <- calc_atkinson(vals, input$atkinson_e)
      mod_v3_kpi_grid(
        mod_v3_kpi(sprintf("%.3f", g), "Gini", tone = "primary"),
        mod_v3_kpi(sprintf("%.3f", th), "Theil-T", tone = "secondary"),
        mod_v3_kpi(sprintf("%.3f", at), "Atkinson", tone = "warn"),
        mod_v3_kpi(as.character(latest), "\u5e74\u4efd", tone = "neutral")
      )
    })

    output$indices_trend <- plotly::renderPlotly({
      m <- master_r(); ind <- input$indicator
      years <- sort(unique(m$year))
      ginis <- vapply(years, function(yr) calc_gini(m[[ind]][m$year == yr]), numeric(1))
      theils <- vapply(years, function(yr) calc_theil(m[[ind]][m$year == yr]), numeric(1))
      atks <- vapply(years, function(yr) calc_atkinson(m[[ind]][m$year == yr], input$atkinson_e), numeric(1))
      df <- data.frame(year = years, Gini = ginis, Theil = theils, Atkinson = atks)
      safe_plotly({
        plotly::plot_ly(df, x = ~year) |>
          plotly::add_lines(y = ~Gini, name = "Gini",
                            line = list(color = "#1d3f5f", width = 3)) |>
          plotly::add_lines(y = ~Theil, name = "Theil-T",
                            line = list(color = "#c46327", width = 3)) |>
          plotly::add_lines(y = ~Atkinson, name = "Atkinson",
                            line = list(color = "#2a857a", width = 3)) |>
          ghs_plotly_layout() |>
          plotly::layout(
            xaxis = list(title = ""),
            yaxis = list(title = "\u6307\u6570\u503c"),
            legend = list(orientation = "h", y = -0.15)
          )
      })
    })

    output$lorenz_curve <- plotly::renderPlotly({
      m <- master_r(); ind <- input$indicator
      latest <- max(m$year, na.rm = TRUE)
      vals <- sort(m[[ind]][m$year == latest & is.finite(m[[ind]]) & m[[ind]] > 0])
      n <- length(vals)
      shiny::req(n > 5)
      cum_pop <- seq_len(n) / n
      cum_val <- cumsum(vals) / sum(vals)
      safe_plotly({
        plotly::plot_ly() |>
          plotly::add_lines(x = c(0, cum_pop), y = c(0, cum_val),
                            name = "Lorenz",
                            line = list(color = "#1d3f5f", width = 3)) |>
          plotly::add_lines(x = c(0, 1), y = c(0, 1),
                            name = "\u5b8c\u5168\u5e73\u7b49",
                            line = list(color = "#5d667a", width = 1.5, dash = "dash")) |>
          ghs_plotly_layout() |>
          plotly::layout(
            xaxis = list(title = "\u7d2f\u8ba1\u56fd\u5bb6\u6bd4\u4f8b"),
            yaxis = list(title = "\u7d2f\u8ba1\u652f\u51fa\u6bd4\u4f8b"),
            legend = list(orientation = "h", y = -0.15)
          )
      })
    })

    output$theil_decomp <- plotly::renderPlotly({
      m <- master_r(); ind <- input$indicator; grp <- input$group_by
      years <- sort(unique(m$year))
      results <- lapply(years, function(yr) {
        d <- m[m$year == yr & is.finite(m[[ind]]) & m[[ind]] > 0 & !is.na(m[[grp]]), ]
        if (nrow(d) < 10) return(NULL)
        total_theil <- calc_theil(d[[ind]])
        group_means <- tapply(d[[ind]], d[[grp]], mean, na.rm = TRUE)
        group_ns <- tapply(d[[ind]], d[[grp]], length)
        mu <- mean(d[[ind]])
        n_total <- nrow(d)
        between <- sum((group_ns / n_total) * (group_means / mu) * log(group_means / mu), na.rm = TRUE)
        within_t <- total_theil - between
        data.frame(year = yr, total = total_theil, between = between, within = within_t)
      })
      df <- do.call(rbind, results)
      shiny::req(nrow(df) > 3)
      safe_plotly({
        plotly::plot_ly(df, x = ~year) |>
          plotly::add_trace(y = ~between, name = "\u7ec4\u95f4", type = "scatter",
                            mode = "lines", stackgroup = "one",
                            fillcolor = "rgba(29,63,95,0.5)",
                            line = list(color = "#1d3f5f")) |>
          plotly::add_trace(y = ~within, name = "\u7ec4\u5185", type = "scatter",
                            mode = "lines", stackgroup = "one",
                            fillcolor = "rgba(196,99,39,0.5)",
                            line = list(color = "#c46327")) |>
          ghs_plotly_layout() |>
          plotly::layout(
            xaxis = list(title = ""),
            yaxis = list(title = "Theil-T"),
            legend = list(orientation = "h", y = -0.15)
          )
      })
    })

    output$group_gini <- plotly::renderPlotly({
      m <- master_r(); ind <- input$indicator; grp <- input$group_by
      latest <- max(m$year, na.rm = TRUE)
      d <- m[m$year == latest & is.finite(m[[ind]]) & m[[ind]] > 0 & !is.na(m[[grp]]), ]
      groups <- unique(d[[grp]])
      ginis <- vapply(groups, function(g) calc_gini(d[[ind]][d[[grp]] == g]), numeric(1))
      df <- data.frame(group = groups, gini = ginis)
      df <- df[is.finite(df$gini), ]
      df <- df[order(-df$gini), ]
      safe_plotly({
        plotly::plot_ly(df, x = ~reorder(group, gini), y = ~gini,
                        type = "bar",
                        marker = list(color = "#1d3f5f")) |>
          ghs_plotly_layout() |>
          plotly::layout(
            xaxis = list(title = ""),
            yaxis = list(title = "Gini (\u7ec4\u5185)")
          )
      })
    })

    output$ineq_table <- reactable::renderReactable({
      m <- master_r(); ind <- input$indicator
      years <- sort(unique(m$year))
      tab <- data.frame(
        "\u5e74\u4efd" = years,
        "Gini" = vapply(years, function(yr) round(calc_gini(m[[ind]][m$year == yr]), 4), numeric(1)),
        "Theil" = vapply(years, function(yr) round(calc_theil(m[[ind]][m$year == yr]), 4), numeric(1)),
        "Atkinson" = vapply(years, function(yr) round(calc_atkinson(m[[ind]][m$year == yr], input$atkinson_e), 4), numeric(1)),
        "N" = vapply(years, function(yr) sum(m$year == yr & is.finite(m[[ind]]) & m[[ind]] > 0), integer(1)),
        check.names = FALSE
      )
      reactable::reactable(tab, pagination = FALSE, highlight = TRUE,
        striped = TRUE,
        defaultColDef = reactable::colDef(
          headerStyle = list(background = "#f1f3f7", fontWeight = 700)))
    })
  })
}
