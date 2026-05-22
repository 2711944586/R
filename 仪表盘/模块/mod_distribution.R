
mod_distribution_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = "\u5206\u5e03 Distribution",
    value = "distribution",
    mod_v3_hero(
      kicker = "\u7edf\u8ba1\u5206\u5e03",
      title = "\u6307\u6807\u5206\u5e03\u5f62\u6001\u4e0e\u7edf\u8ba1\u7279\u5f81",
      lead = paste(
        "\u5168\u7403\u536b\u751f\u652f\u51fa\u6307\u6807\u5448\u73b0\u663e\u8457\u7684\u53f3\u504f\u5206\u5e03\uff0c",
        "\u5c3e\u90e8\u56fd\u5bb6\u4e0e\u4e2d\u4f4d\u6570\u5dee\u8ddd\u5de8\u5927\u3002",
        "\u672c\u6a21\u5757\u63d0\u4f9b\u76f4\u65b9\u56fe\u3001\u5bc6\u5ea6\u66f2\u7ebf\u3001QQ \u56fe\u3001\u504f\u5ea6/\u5cf0\u5ea6\u5206\u6790\u3002"
      ),
      meta = list("\u6b63\u6001\u6027\u68c0\u9a8c", "Shapiro-Wilk", "195 \u56fd\u5bb6")
    ),
    mod_v3_page_body(
      wide = TRUE,
      mod_v3_topic_brief("distribution"),
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
          mod_v3_sidebar_note(
            "\u5206\u5e03\u8bbe\u7f6e",
            "\u5bf9\u6570\u53d8\u6362\u53ef\u4f7f CHE/cap \u548c GDP/cap \u8fd9\u7c7b\u53f3\u504f\u53d8\u91cf\u66f4\u63a5\u8fd1\u5bf9\u79f0\u5206\u5e03\uff0c\u4fbf\u4e8e\u6bd4\u8f83\u5c3e\u90e8\u548c\u505a\u53c2\u6570\u68c0\u9a8c\u3002",
            bullets = c(
              "\u539f\u59cb\u5c3a\u5ea6\u66f4\u9002\u5408\u8bfb\u7edd\u5bf9\u5dee\u8ddd\u3002",
              "\u5bf9\u6570\u5c3a\u5ea6\u66f4\u9002\u5408\u8bfb\u500d\u6570\u5dee\u548c\u5206\u5e03\u5f62\u72b6\u3002",
              "Shapiro-Wilk p \u503c\u8f83\u5c0f\u8868\u793a\u4e0e\u6b63\u6001\u5206\u5e03\u5b58\u5728\u7edf\u8ba1\u5dee\u5f02\uff0c\u4e0d\u4ee3\u8868\u5206\u6790\u4e0d\u53ef\u7528\u3002"
            )
          )
        ),
        shiny::uiOutput(ns("kpi_strip")),
        bslib::layout_columns(
          col_widths = c(6, 6),
          mod_card(
            kicker = "F1 · Shape",
            title = "\u76f4\u65b9\u56fe",
            htmltools::p(class = "card-note",
              "\u6307\u6807\u7684\u9891\u7387\u5206\u5e03\uff0c\u7ea2\u8272\u7ebf = \u4e2d\u4f4d\u6570\u53c2\u7167\u7ebf\u3002"),
            mod_v3_chart_guide(
              "\u76f4\u65b9\u56fe\u8bfb\u4e09\u4ef6\u4e8b",
              "\u5148\u770b\u4e3b\u4f53\u96c6\u4e2d\u5728\u54ea\u91cc\uff0c\u518d\u770b\u53f3\u5c3e\u6216\u5de6\u5c3e\u6709\u591a\u957f\uff0c\u6700\u540e\u770b\u4e2d\u4f4d\u6570\u662f\u5426\u504f\u79bb\u5206\u5e03\u4e2d\u5fc3\u3002",
              bullets = c("\u957f\u53f3\u5c3e\u8868\u793a\u5c11\u6570\u56fd\u5bb6\u663e\u8457\u9ad8\u4e8e\u5178\u578b\u503c\u3002", "\u5207\u6362\u5bf9\u6570\u53d8\u6362\u53ef\u68c0\u67e5\u5dee\u5f02\u662f\u5426\u4e3b\u8981\u6765\u81ea\u500d\u6570\u5dee\u3002")
            ),
            mod_spinner(plotly::plotlyOutput(ns("histogram"), height = 380))
          ),
          mod_card(
            kicker = "F2 · Group density",
            title = "\u6309\u7ec4\u5bc6\u5ea6\u66f2\u7ebf",
            htmltools::p(class = "card-note",
              "\u4e0d\u540c\u6536\u5165\u7ec4\u7684\u5206\u5e03\u5f62\u6001\u5bf9\u6bd4\u3002"),
            mod_v3_chart_guide(
              "\u5bc6\u5ea6\u66f2\u7ebf\u8bfb\u91cd\u53e0\u548c\u5206\u79bb",
              "\u66f2\u7ebf\u9ad8\u5ea6\u8868\u793a\u8be5\u533a\u95f4\u7684\u56fd\u5bb6\u96c6\u4e2d\u5ea6\uff1b\u4e0d\u540c\u7ec4\u7684\u91cd\u53e0\u8d8a\u591a\uff0c\u8bf4\u660e\u5355\u4e00\u6536\u5165\u7ec4\u5bf9\u6307\u6807\u7684\u89e3\u91ca\u8d8a\u6709\u9650\u3002",
              tone = "good"
            ),
            mod_spinner(plotly::plotlyOutput(ns("density_by_group"), height = 380))
          )
        ),
        bslib::layout_columns(
          col_widths = c(6, 6),
          mod_card(
            kicker = "F3 · Normality check",
            title = "Q-Q \u56fe\uff08\u6b63\u6001\u6027\u68c0\u9a8c\uff09",
            htmltools::p(class = "card-note",
              "\u70b9\u8d8a\u63a5\u8fd1\u5bf9\u89d2\u7ebf = \u8d8a\u63a5\u8fd1\u6b63\u6001\u5206\u5e03\u3002"),
            mod_v3_chart_guide(
              "\u4e24\u7aef\u504f\u79bb\u6bd4\u4e2d\u95f4\u504f\u79bb\u66f4\u5173\u952e",
              "\u5982\u679c\u70b9\u5728\u4e24\u7aef\u660e\u663e\u5f2f\u79bb\u5bf9\u89d2\u7ebf\uff0c\u8bf4\u660e\u6781\u7aef\u56fd\u5bb6\u4f1a\u5f71\u54cd\u5747\u503c\u548c\u7ebf\u6027\u6a21\u578b\uff0c\u9700\u8981\u4f7f\u7528\u4e2d\u4f4d\u6570\u3001\u5206\u4f4d\u6570\u6216\u7a33\u5065\u65b9\u6cd5\u3002",
              tone = "warn"
            ),
            mod_spinner(plotly::plotlyOutput(ns("qq_plot"), height = 380))
          ),
          mod_card(
            kicker = "F4 · Time evolution",
            title = "\u5206\u5e03\u6f14\u5316\uff08\u591a\u5e74\u7bb1\u7ebf\uff09",
            htmltools::p(class = "card-note",
              "\u6bcf 5 \u5e74\u4e00\u4e2a\u7bb1\u7ebf\uff0c\u89c2\u5bdf\u5206\u5e03\u5f62\u6001\u968f\u65f6\u95f4\u7684\u53d8\u5316\u3002"),
            mod_v3_chart_guide(
              "\u591a\u5e74\u7bb1\u7ebf\u770b\u4e2d\u4f4d\u6570\u548c IQR \u540c\u65f6\u79fb\u52a8",
              "\u4e2d\u4f4d\u6570\u4e0a\u5347\u8bf4\u660e\u5178\u578b\u56fd\u5bb6\u63d0\u5347\uff1bIQR \u6269\u5927\u8868\u793a\u56fd\u5bb6\u95f4\u5dee\u8ddd\u540c\u65f6\u53d8\u5927\u3002"
            ),
            mod_spinner(plotly::plotlyOutput(ns("box_evolution"), height = 380)),
            footer = "IQR = \u7b2c 25 \u5230\u7b2c 75 \u767e\u5206\u4f4d\uff1b\u7bb1\u5916\u70b9\u8868\u793a\u5206\u5e03\u5c3e\u90e8\u56fd\u5bb6\u3002"
          )
        ),
        mod_card(
          kicker = "F5 · Statistical audit",
          title = "\u63cf\u8ff0\u6027\u7edf\u8ba1\u91cf",
          htmltools::p(class = "card-note",
            "\u5404\u7ec4\u7684\u5747\u503c\u3001\u4e2d\u4f4d\u6570\u3001\u6807\u51c6\u5dee\u3001\u504f\u5ea6\u3001\u5cf0\u5ea6\u3001Shapiro-Wilk p \u503c\u3002"),
          mod_v3_chart_guide(
            "\u7528\u8868\u683c\u628a\u56fe\u5f62\u5224\u65ad\u6570\u503c\u5316",
            "\u504f\u5ea6\u63cf\u8ff0\u5c3e\u90e8\u65b9\u5411\uff0c\u5cf0\u5ea6\u63cf\u8ff0\u5c3e\u90e8\u539a\u5ea6\uff0cShapiro-Wilk p \u503c\u5219\u7528\u6765\u5feb\u901f\u5224\u65ad\u662f\u5426\u504f\u79bb\u6b63\u6001\u5047\u8bbe\u3002",
            bullets = c("\u5e73\u5747\u503c\u660e\u663e\u9ad8\u4e8e\u4e2d\u4f4d\u6570\u65f6\uff0c\u901a\u5e38\u5b58\u5728\u53f3\u504f\u3002", "\u6807\u51c6\u5dee\u9ad8\u7684\u7ec4\u9700\u8981\u914d\u5408\u7bb1\u7ebf\u56fe\u590d\u6838\u79bb\u7fa4\u70b9\u3002")
          ),
          mod_spinner(reactable::reactableOutput(ns("stats_table"))),
          footer = "\u5bf9\u4e8e\u56fd\u5bb6\u6570\u5f88\u591a\u7684\u6a2a\u622a\u9762\uff0c\u6b63\u6001\u6027\u68c0\u9a8c\u5f88\u5bb9\u6613\u663e\u8457\uff1b\u5b9e\u52a1\u89e3\u91ca\u5e94\u7ed3\u5408\u56fe\u5f62\u5f62\u72b6\u3002"
        )
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
