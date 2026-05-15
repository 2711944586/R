# =============================================================================
# 仪表盘/模块/mod_distribution.R
# 分布形态：核心指标的统计分布、偏度、峰度与正态性检验
# =============================================================================

mod_distribution_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = htmltools::HTML("&#128202; \u5206\u5e03 Distribution"),
    mod_v3_hero(
      kicker = "STATISTICAL DISTRIBUTIONS",
      title = "\u6307\u6807\u5206\u5e03\u5f62\u6001\u4e0e\u7edf\u8ba1\u7279\u5f81",
      lead = paste(
        "\u5168\u7403\u536b\u751f\u652f\u51fa\u6307\u6807\u5448\u73b0\u663e\u8457\u7684\u53f3\u504f\u5206\u5e03\uff0c",
        "\u5c3e\u90e8\u56fd\u5bb6\u4e0e\u4e2d\u4f4d\u6570\u5dee\u8ddd\u5de8\u5927\u3002",
        "\u672c\u6a21\u5757\u63d0\u4f9b\u76f4\u65b9\u56fe\u3001\u5bc6\u5ea6\u66f2\u7ebf\u3001QQ \u56fe\u3001\u504f\u5ea6/\u5cf0\u5ea6\u5206\u6790\u3002"
      ),
      meta = list("\u6b63\u6001\u6027\u68c0\u9a8c", "Shapiro-Wilk", "195 \u56fd\u5bb6")
    ),
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 280,
        shiny::sliderInput(ns("year"), "\u5e74\u4efd",
          min = 2000, max = 2023, value = 2023, step = 1, sep = ""),
        shiny::selectInput(ns("indicator"), "\u6307\u6807",
          choices = c(
            "\u4eba\u5747 CHE (USD)" = "che_pc_usd2023",
            "OOPS (%)" = "hf3_che",
            "GGHE-D (%)" = "gghed_che",
            "EXT (%)" = "ext_che",
            "\u9884\u671f\u5bff\u547d" = "life_exp",
            "GDP/cap" = "gdp_pc_usd"
          ), selected = "che_pc_usd2023"),
        shiny::checkboxInput(ns("log_transform"), "\u5bf9\u6570\u53d8\u6362", value = FALSE),
        shiny::tags$hr(),
        shiny::helpText(
          "\u5bf9\u6570\u53d8\u6362\u53ef\u4f7f\u53f3\u504f\u5206\u5e03\u63a5\u8fd1\u6b63\u6001\uff0c",
          "\u4fbf\u4e8e\u53c2\u6570\u68c0\u9a8c\u3002"
        )
      ),
      shiny::uiOutput(ns("kpi_strip")),
      # Row 1: histogram + density
      bslib::layout_columns(
        col_widths = c(6, 6),
        mod_card(
          title = "\u76f4\u65b9\u56fe",
          htmltools::p(class = "card-note",
            "\u6307\u6807\u7684\u9891\u7387\u5206\u5e03\uff0c\u7ea2\u8272\u7ebf = \u6838\u5bc6\u5ea6\u4f30\u8ba1\u3002"),
          mod_spinner(plotly::plotlyOutput(ns("histogram"), height = 380))
        ),
        mod_card(
          title = "\u6309\u7ec4\u5bc6\u5ea6\u66f2\u7ebf",
          htmltools::p(class = "card-note",
            "\u4e0d\u540c\u6536\u5165\u7ec4\u7684\u5206\u5e03\u5f62\u6001\u5bf9\u6bd4\u3002"),
          mod_spinner(plotly::plotlyOutput(ns("density_by_group"), height = 380))
        )
      ),
      # Row 2: QQ + box
      bslib::layout_columns(
        col_widths = c(6, 6),
        mod_card(
          title = "Q-Q \u56fe\uff08\u6b63\u6001\u6027\u68c0\u9a8c\uff09",
          htmltools::p(class = "card-note",
            "\u70b9\u8d8a\u63a5\u8fd1\u5bf9\u89d2\u7ebf = \u8d8a\u63a5\u8fd1\u6b63\u6001\u5206\u5e03\u3002"),
          mod_spinner(plotly::plotlyOutput(ns("qq_plot"), height = 380))
        ),
        mod_card(
          title = "\u5206\u5e03\u6f14\u5316\uff08\u591a\u5e74\u7bb1\u7ebf\uff09",
          htmltools::p(class = "card-note",
            "\u6bcf 5 \u5e74\u4e00\u4e2a\u7bb1\u7ebf\uff0c\u89c2\u5bdf\u5206\u5e03\u5f62\u6001\u968f\u65f6\u95f4\u7684\u53d8\u5316\u3002"),
          mod_spinner(plotly::plotlyOutput(ns("box_evolution"), height = 380))
        )
      ),
      # Row 3: stats table
      mod_card(
        title = "\u63cf\u8ff0\u6027\u7edf\u8ba1\u91cf",
        htmltools::p(class = "card-note",
          "\u5404\u7ec4\u7684\u5747\u503c\u3001\u4e2d\u4f4d\u6570\u3001\u6807\u51c6\u5dee\u3001\u504f\u5ea6\u3001\u5cf0\u5ea6\u3001Shapiro-Wilk p \u503c\u3002"),
        mod_spinner(reactable::reactableOutput(ns("stats_table")))
      )
    )
  )
}

mod_distribution_server <- function(id, master_r) {
  shiny::moduleServer(id, function(input, output, session) {

    get_vals <- function(d, ind, do_log = FALSE) {
      v <- d[[ind]]
      v <- v[is.finite(v)]
      if (do_log && all(v > 0)) v <- log(v)
      v
    }

    output$kpi_strip <- shiny::renderUI({
      m <- master_r(); yr <- input$year; ind <- input$indicator
      d <- m[m$year == yr & is.finite(m[[ind]]), ]
      vals <- d[[ind]]
      skew <- if (length(vals) > 3) {
        n <- length(vals); mu <- mean(vals); s <- stats::sd(vals)
        sum(((vals - mu) / s)^3) / n
      } else NA_real_
      sw_p <- tryCatch(stats::shapiro.test(vals[seq_len(min(length(vals), 5000))])$p.value,
                       error = function(e) NA_real_)
      mod_v3_kpi_grid(
        mod_v3_kpi(format(length(vals), big.mark = ","),
                   "\u89c2\u6d4b\u6570", tone = "primary"),
        mod_v3_kpi(sprintf("%.2f", skew),
                   "\u504f\u5ea6", tone = if (!is.na(skew) && abs(skew) > 1) "warn" else "good"),
        mod_v3_kpi(sprintf("%.4f", sw_p),
                   "Shapiro p", tone = if (!is.na(sw_p) && sw_p < 0.05) "bad" else "good"),
        mod_v3_kpi(sprintf("%.1f", stats::sd(vals, na.rm = TRUE)),
                   "\u6807\u51c6\u5dee", tone = "neutral")
      )
    })

    output$histogram <- plotly::renderPlotly({
      m <- master_r(); yr <- input$year; ind <- input$indicator
      d <- m[m$year == yr & is.finite(m[[ind]]), ]
      vals <- get_vals(d, ind, input$log_transform)
      shiny::req(length(vals) > 5)
      safe_plotly({
        plotly::plot_ly(x = vals, type = "histogram",
                        marker = list(color = "rgba(29,63,95,0.6)",
                                      line = list(color = "#1d3f5f", width = 1)),
                        nbinsx = 25) |>
          ghs_plotly_layout() |>
          plotly::layout(
            xaxis = list(title = if (input$log_transform) paste0("log(", ind, ")") else ind),
            yaxis = list(title = "\u9891\u6b21"),
            shapes = list(
              list(type = "line",
                   x0 = stats::median(vals), x1 = stats::median(vals),
                   y0 = 0, y1 = 1, yref = "paper",
                   line = list(color = "#a23b3b", width = 2, dash = "dash"))
            )
          )
      })
    })

    output$density_by_group <- plotly::renderPlotly({
      m <- master_r(); yr <- input$year; ind <- input$indicator
      d <- m[m$year == yr & is.finite(m[[ind]]) & !is.na(m$income_group), ]
      shiny::req(nrow(d) > 10)
      groups <- unique(d$income_group)
      p <- plotly::plot_ly()
      colors <- unname(brand_palette$income)
      for (i in seq_along(groups)) {
        g <- groups[i]
        vals <- get_vals(d[d$income_group == g, ], ind, input$log_transform)
        if (length(vals) > 3) {
          dens <- stats::density(vals, n = 64)
          p <- plotly::add_trace(p, x = dens$x, y = dens$y,
                                  type = "scatter", mode = "lines",
                                  name = g,
                                  line = list(color = colors[min(i, length(colors))],
                                              width = 2.5),
                                  fill = "tozeroy",
                                  fillcolor = paste0(colors[min(i, length(colors))], "22"))
        }
      }
      safe_plotly({
        p |> ghs_plotly_layout() |>
          plotly::layout(
            xaxis = list(title = if (input$log_transform) paste0("log(", ind, ")") else ind),
            yaxis = list(title = "\u5bc6\u5ea6"),
            legend = list(orientation = "h", y = -0.15)
          )
      })
    })

    output$qq_plot <- plotly::renderPlotly({
      m <- master_r(); yr <- input$year; ind <- input$indicator
      d <- m[m$year == yr & is.finite(m[[ind]]), ]
      vals <- get_vals(d, ind, input$log_transform)
      shiny::req(length(vals) > 5)
      n <- length(vals)
      theoretical <- stats::qnorm(stats::ppoints(n))
      empirical <- sort(vals)
      safe_plotly({
        plotly::plot_ly(x = theoretical, y = empirical,
                        type = "scatter", mode = "markers",
                        marker = list(color = "#1d3f5f", size = 5, opacity = 0.6)) |>
          ghs_plotly_layout() |>
          plotly::layout(
            xaxis = list(title = "\u7406\u8bba\u5206\u4f4d\u6570"),
            yaxis = list(title = "\u6837\u672c\u5206\u4f4d\u6570"),
            shapes = list(
              list(type = "line",
                   x0 = min(theoretical), x1 = max(theoretical),
                   y0 = mean(empirical) + stats::sd(empirical) * min(theoretical),
                   y1 = mean(empirical) + stats::sd(empirical) * max(theoretical),
                   line = list(color = "#a23b3b", width = 2, dash = "dash"))
            )
          )
      })
    })

    output$box_evolution <- plotly::renderPlotly({
      m <- master_r(); ind <- input$indicator
      # Every 5 years
      years_show <- seq(2000, 2023, by = 5)
      if (!2023 %in% years_show) years_show <- c(years_show, 2023)
      d <- m[m$year %in% years_show & is.finite(m[[ind]]), ]
      d$year_f <- factor(d$year)
      shiny::req(nrow(d) > 20)
      safe_plotly({
        plotly::plot_ly(d, x = ~year_f,
                        y = stats::as.formula(paste0("~", ind)),
                        type = "box",
                        marker = list(color = "#1d3f5f"),
                        line = list(color = "#1d3f5f"),
                        fillcolor = "rgba(29,63,95,0.15)") |>
          ghs_plotly_layout() |>
          plotly::layout(
            xaxis = list(title = ""),
            yaxis = list(title = ind)
          )
      })
    })

    output$stats_table <- reactable::renderReactable({
      m <- master_r(); yr <- input$year; ind <- input$indicator
      d <- m[m$year == yr & is.finite(m[[ind]]) & !is.na(m$income_group), ]
      groups <- c("All", sort(unique(d$income_group)))
      results <- lapply(groups, function(g) {
        vals <- if (g == "All") d[[ind]] else d[[ind]][d$income_group == g]
        vals <- vals[is.finite(vals)]
        n <- length(vals)
        if (n < 3) return(NULL)
        mu <- mean(vals); med <- stats::median(vals); s <- stats::sd(vals)
        skew <- sum(((vals - mu) / s)^3) / n
        kurt <- sum(((vals - mu) / s)^4) / n - 3
        sw_p <- tryCatch(stats::shapiro.test(vals[seq_len(min(n, 5000))])$p.value,
                         error = function(e) NA_real_)
        data.frame(
          "\u5206\u7ec4" = g, N = n,
          "\u5747\u503c" = round(mu, 1), "\u4e2d\u4f4d\u6570" = round(med, 1),
          "SD" = round(s, 1), "\u504f\u5ea6" = round(skew, 2),
          "\u5cf0\u5ea6" = round(kurt, 2),
          "Shapiro_p" = sprintf("%.4f", sw_p),
          check.names = FALSE
        )
      })
      tab <- do.call(rbind, results)
      reactable::reactable(tab, pagination = FALSE, highlight = TRUE,
        striped = TRUE,
        defaultColDef = reactable::colDef(
          headerStyle = list(background = "#f1f3f7", fontWeight = 700)))
    })
  })
}
