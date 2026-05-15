# =============================================================================
# 仪表盘/模块/mod_inequality.R
# 不平等指数：Gini、Theil、Atkinson 的时序演化与分解
# =============================================================================

mod_inequality_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = htmltools::HTML("&#9878; \u4e0d\u5e73\u7b49 Inequality"),
    mod_v3_hero(
      kicker = "INEQUALITY INDICES",
      title = "\u536b\u751f\u652f\u51fa\u4e0d\u5e73\u7b49\u6307\u6570",
      lead = paste(
        "\u5168\u7403\u4eba\u5747 CHE \u7684 Gini \u7cfb\u6570\u7ea6 0.55\uff0c\u8fdc\u9ad8\u4e8e\u6536\u5165\u4e0d\u5e73\u7b49\u3002",
        "\u672c\u6a21\u5757\u8ba1\u7b97 Gini\u3001Theil-T\u3001Atkinson \u4e09\u79cd\u6307\u6570\uff0c",
        "\u5e76\u5c06 Theil \u5206\u89e3\u4e3a\u7ec4\u95f4\u4e0e\u7ec4\u5185\u4e24\u4e2a\u5206\u91cf\u3002"
      ),
      meta = list("Gini / Theil-T / Atkinson", "\u7ec4\u95f4-\u7ec4\u5185\u5206\u89e3", "2000\u20132023")
    ),
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
        shiny::sliderInput(ns("atkinson_e"), "Atkinson \u4e0d\u5e73\u7b49\u53a8\u6076\u53c2\u6570 \u03b5",
          min = 0.5, max = 2.0, value = 1.0, step = 0.25),
        shiny::tags$hr(),
        shiny::helpText(
          "Gini \u2208 [0,1]\uff0c0 = \u5b8c\u5168\u5e73\u7b49\u3002",
          "Theil-T \u2265 0\uff0c\u53ef\u52a0\u6027\u5206\u89e3\u3002",
          "Atkinson \u53c2\u6570 \u03b5 \u8d8a\u5927\u8d8a\u5173\u6ce8\u5e95\u90e8\u3002"
        )
      ),
      shiny::uiOutput(ns("kpi_strip")),
      # Row 1: Gini trend + Lorenz
      bslib::layout_columns(
        col_widths = c(7, 5),
        mod_card(
          title = "\u4e0d\u5e73\u7b49\u6307\u6570\u65f6\u5e8f\u6f14\u5316",
          htmltools::p(class = "card-note",
            "Gini\u3001Theil-T\u3001Atkinson \u4e09\u79cd\u6307\u6570\u7684 24 \u5e74\u8f68\u8ff9\u3002"),
          mod_spinner(plotly::plotlyOutput(ns("indices_trend"), height = 440))
        ),
        mod_card(
          title = "Lorenz \u66f2\u7ebf\uff08\u6700\u65b0\u5e74\uff09",
          htmltools::p(class = "card-note",
            "\u66f2\u7ebf\u8d8a\u8fdc\u79bb\u5bf9\u89d2\u7ebf = \u4e0d\u5e73\u7b49\u8d8a\u4e25\u91cd\u3002"),
          mod_spinner(plotly::plotlyOutput(ns("lorenz_curve"), height = 440))
        )
      ),
      # Row 2: decomposition + between/within
      bslib::layout_columns(
        col_widths = c(6, 6),
        mod_card(
          title = "Theil \u5206\u89e3\uff1a\u7ec4\u95f4 vs \u7ec4\u5185",
          htmltools::p(class = "card-note",
            "\u7ec4\u95f4\u4e0d\u5e73\u7b49\u53cd\u6620\u6536\u5165\u7ec4/\u5927\u6d32\u95f4\u5dee\u5f02\uff0c\u7ec4\u5185\u53cd\u6620\u540c\u7ec4\u56fd\u5bb6\u95f4\u5dee\u5f02\u3002"),
          mod_spinner(plotly::plotlyOutput(ns("theil_decomp"), height = 380))
        ),
        mod_card(
          title = "\u5206\u7ec4 Gini \u5bf9\u6bd4",
          htmltools::p(class = "card-note",
            "\u5404\u7ec4\u5185\u90e8\u7684 Gini \u7cfb\u6570\u5bf9\u6bd4\u3002"),
          mod_spinner(plotly::plotlyOutput(ns("group_gini"), height = 380))
        )
      ),
      # Row 3
      mod_card(
        title = "\u4e0d\u5e73\u7b49\u6307\u6570\u8be6\u8868",
        mod_spinner(reactable::reactableOutput(ns("ineq_table")))
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
        # Between-group Theil
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
