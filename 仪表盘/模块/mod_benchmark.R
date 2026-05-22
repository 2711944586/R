
mod_benchmark_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = "\u5bf9\u6807 Benchmark",
    value = "benchmark",
    mod_v3_hero(
      kicker = "\u57fa\u51c6\u4e0e\u5dee\u8ddd",
      title = "\u57fa\u51c6\u5bf9\u6807\u4e0e\u5dee\u8ddd\u5206\u6790",
      lead = paste(
        "\u9009\u62e9\u53c2\u7167\u7ec4\uff08\u5982 OECD \u5747\u503c\u3001\u540c\u6536\u5165\u7ec4\u4e2d\u4f4d\u6570\uff09\uff0c",
        "\u91cf\u5316\u5404\u56fd\u4e0e\u57fa\u51c6\u7684\u5dee\u8ddd\uff0c\u8bc6\u522b\u8ffd\u8d76\u8005\u4e0e\u843d\u540e\u8005\u3002"
      ),
      meta = list("WHO GHED 2024-12", "\u591a\u6307\u6807\u5bf9\u6807", "195 \u56fd\u5bb6")
    ),
    mod_v3_page_body(
      wide = TRUE,
      mod_v3_topic_brief("benchmark"),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 280,
          shiny::sliderInput(ns("year"), "\u5e74\u4efd",
            min = 2000, max = 2023, value = 2023, step = 1, sep = ""),
          shiny::selectInput(ns("benchmark"), "\u53c2\u7167\u57fa\u51c6",
            choices = c(
              "OECD \u5747\u503c" = "oecd",
              "\u9ad8\u6536\u5165\u56fd\u4e2d\u4f4d\u6570" = "high_income",
              "\u5168\u7403\u4e2d\u4f4d\u6570" = "global_median",
              "\u540c\u6536\u5165\u7ec4\u4e2d\u4f4d\u6570" = "peer_income"
            ), selected = "high_income"),
          shiny::selectInput(ns("indicator"), "\u5bf9\u6807\u6307\u6807",
            choices = c(
              "\u4eba\u5747 CHE (USD)" = "che_pc_usd2023",
              "GGHE-D (%)" = "gghed_che",
              "OOPS (%)" = "hf3_che",
              "\u9884\u671f\u5bff\u547d" = "life_exp"
            ), selected = "che_pc_usd2023"),
          shiny::selectInput(ns("focus_group"), "\u805a\u7126\u7ec4\u522b",
            choices = c("\u5168\u90e8" = "all",
                        "Low income" = "Low income",
                        "Lower middle income" = "Lower middle income",
                        "Upper middle income" = "Upper middle income"),
            selected = "all"),
          mod_v3_sidebar_note(
            "\u5dee\u8ddd\u8ba1\u7b97",
            "\u5dee\u8ddd = (\u56fd\u5bb6\u503c - \u57fa\u51c6\u503c) / \u57fa\u51c6\u503c \u00d7 100%\u3002\u8d1f\u503c\u8868\u793a\u4f4e\u4e8e\u57fa\u51c6\uff0c\u6b63\u503c\u8868\u793a\u8d85\u8fc7\u57fa\u51c6\u3002",
            bullets = c(
              "\u4e0d\u540c\u57fa\u51c6\u4f1a\u6539\u53d8\u7ed3\u8bba\uff0c\u5efa\u8bae\u56fa\u5b9a\u6307\u6807\u540e\u5207\u6362\u57fa\u51c6\u6bd4\u8f83\u3002",
              "\u540c\u6536\u5165\u7ec4\u57fa\u51c6\u5728\u56fd\u5bb6\u5c42\u9762\u5355\u72ec\u8ba1\u7b97\uff0c\u66f4\u9002\u5408\u5bfb\u627e\u53ef\u884c\u8ffd\u8d76\u76ee\u6807\u3002",
              "\u8ffd\u8d76\u901f\u5ea6\u56fe\u4f7f\u7528\u8fc7\u53bb 10 \u5e74\u7684\u5dee\u8ddd\u53d8\u5316\u3002"
            )
          )
        ),
        shiny::uiOutput(ns("kpi_strip")),
      bslib::layout_columns(
        col_widths = c(7, 5),
        mod_card(
          kicker = "F1 \u00b7 Gap distribution",
          title = "\u4e0e\u57fa\u51c6\u7684\u5dee\u8ddd\u5206\u5e03",
          htmltools::p(class = "card-note",
            "\u5404\u56fd\u4e0e\u53c2\u7167\u57fa\u51c6\u7684\u767e\u5206\u6bd4\u5dee\u8ddd\u3002\u7ea2\u8272 = \u4f4e\u4e8e\u57fa\u51c6\u3002"),
          mod_v3_chart_guide(
            "\u5148\u770b\u8d1f\u5411\u7f3a\u53e3\uff0c\u518d\u770b\u6b63\u5411\u504f\u79bb",
            "\u8d1f\u5411\u6761\u5f62\u5c06\u6700\u9700\u8981\u8ffd\u8d76\u7684\u56fd\u5bb6\u63a8\u5230\u524d\u53f0\uff1b\u6b63\u5411\u56fd\u5bb6\u5219\u53ef\u4f5c\u4e3a\u5236\u5ea6\u6216\u8d44\u6e90\u914d\u7f6e\u7684\u53c2\u7167\u6837\u672c\u3002",
            tone = "warn"
          ),
          mod_spinner(plotly::plotlyOutput(ns("gap_bar"), height = 480))
        ),
        mod_card(
          kicker = "F2 \u00b7 Gap closure",
          title = "\u5dee\u8ddd\u6536\u7f29\u8d8b\u52bf",
          htmltools::p(class = "card-note",
            "\u5404\u7ec4\u4e0e\u57fa\u51c6\u7684\u5e73\u5747\u5dee\u8ddd\u968f\u65f6\u95f4\u7684\u53d8\u5316\u3002"),
          mod_v3_chart_guide(
            "\u8d8b\u52bf\u56fe\u56de\u7b54\u662f\u5426\u771f\u5728\u8ffd\u8d76",
            "\u5982\u679c\u7ebf\u6761\u9010\u6b65\u63a5\u8fd1 0\uff0c\u8868\u793a\u8be5\u7ec4\u6b63\u5728\u7f29\u5c0f\u4e0e\u57fa\u51c6\u7684\u5dee\u8ddd\uff1b\u957f\u671f\u8fdc\u79bb 0 \u5219\u63d0\u793a\u7ed3\u6784\u6027\u7f3a\u53e3\u3002"
          ),
          mod_spinner(plotly::plotlyOutput(ns("gap_trend"), height = 480))
        )
      ),
      bslib::layout_columns(
        col_widths = c(6, 6),
        mod_card(
          kicker = "F3 \u00b7 Catch-up speed",
          title = "\u8ffd\u8d76\u901f\u5ea6\u6392\u884c",
          htmltools::p(class = "card-note",
            "\u8fc7\u53bb 10 \u5e74\u5dee\u8ddd\u7f29\u5c0f\u6700\u5feb\u7684\u56fd\u5bb6\u3002"),
          mod_v3_chart_guide(
            "\u53ea\u770b\u539f\u672c\u4f4e\u4e8e\u57fa\u51c6\u7684\u56fd\u5bb6",
            "\u8ffd\u8d76\u6392\u884c\u6392\u9664\u539f\u672c\u5df2\u9ad8\u4e8e\u57fa\u51c6\u7684\u6837\u672c\uff0c\u907f\u514d\u628a\u9ad8\u57fa\u6570\u6ce2\u52a8\u8bef\u8bfb\u4e3a\u8ffd\u8d76\u80fd\u529b\u3002",
            tone = "good"
          ),
          mod_spinner(plotly::plotlyOutput(ns("catch_up"), height = 390))
        ),
        mod_card(
          kicker = "F4 \u00b7 Multi-indicator",
          title = "\u591a\u6307\u6807\u96f7\u8fbe\u5bf9\u6807",
          htmltools::p(class = "card-note",
            "\u9009\u5b9a\u7ec4\u522b\u5728 4 \u4e2a\u6307\u6807\u4e0a\u4e0e\u57fa\u51c6\u7684\u5bf9\u6bd4\u3002"),
          mod_v3_chart_guide(
            "\u96f7\u8fbe\u56fe\u8865\u8db3\u5355\u6307\u6807\u7684\u76f2\u70b9",
            "\u4e00\u4e2a\u56fd\u5bb6\u6216\u7ec4\u522b\u53ef\u80fd\u5728 CHE/cap \u4e0a\u843d\u540e\uff0c\u4f46\u5728\u5bff\u547d\u6216 OOPS \u4e0a\u8868\u73b0\u66f4\u7a33\u5065\uff1b\u8fd9\u7c7b\u7ed3\u6784\u5dee\u5f02\u8981\u6bd4\u5355\u4e00\u6392\u540d\u66f4\u91cd\u8981\u3002"
          ),
          mod_spinner(plotly::plotlyOutput(ns("radar_benchmark"), height = 390))
        )
      ),
      mod_card(
        kicker = "F5 \u00b7 \u5ba1\u8ba1\u8868",
        title = "\u5bf9\u6807\u8be6\u8868",
        mod_v3_chart_guide(
          "\u8868\u683c\u7528\u4e8e\u67e5\u627e\u5177\u4f53\u56fd\u5bb6",
          "\u8868\u683c\u6309\u5dee\u8ddd\u4ece\u4f4e\u5230\u9ad8\u6392\u5217\uff0c\u53ef\u7528\u641c\u7d22\u5feb\u901f\u5b9a\u4f4d\u67d0\u4e2a\u56fd\u5bb6\u7684\u5b9e\u9645\u503c\u3001\u57fa\u51c6\u503c\u548c\u5dee\u8ddd\u767e\u5206\u6bd4\u3002"
        ),
        mod_spinner(reactable::reactableOutput(ns("benchmark_table"))),
        footer = "\u5bf9\u6807\u662f\u76f8\u5bf9\u53e3\u5f84\uff1b\u5f53\u6307\u6807\u4e3a OOPS \u65f6\uff0c\u8d85\u8fc7\u57fa\u51c6\u5e76\u4e0d\u4ee3\u8868\u66f4\u597d\u8868\u73b0\u3002"
      )
      )
    )
  )
}

mod_benchmark_server <- function(id, master_r) {
  shiny::moduleServer(id, function(input, output, session) {

    get_benchmark_value <- function(m, yr, ind, bench_type) {
      d <- m[m$year == yr & is.finite(m[[ind]]), ]
      switch(bench_type,
        "oecd" = {
          oecd_isos <- c("AUS","AUT","BEL","CAN","CHE","CHL","COL","CRI","CZE",
                         "DEU","DNK","ESP","EST","FIN","FRA","GBR","GRC","HUN",
                         "IRL","ISL","ISR","ITA","JPN","KOR","LTU","LUX","LVA",
                         "MEX","NLD","NOR","NZL","POL","PRT","SVK","SVN","SWE",
                         "TUR","USA")
          oecd_d <- d[d$iso3_code %in% oecd_isos, ]
          if (nrow(oecd_d) > 5) mean(oecd_d[[ind]], na.rm = TRUE) else NA_real_
        },
        "high_income" = {
          hi <- d[d$income_group == "High income", ]
          if (nrow(hi) > 5) stats::median(hi[[ind]], na.rm = TRUE) else NA_real_
        },
        "global_median" = stats::median(d[[ind]], na.rm = TRUE),
        "peer_income" = NA_real_
      )
    }

    output$kpi_strip <- shiny::renderUI({
      m <- master_r(); yr <- input$year; ind <- input$indicator
      bench_val <- get_benchmark_value(m, yr, ind, input$benchmark)
      d <- m[m$year == yr & is.finite(m[[ind]]), ]
      n_below <- sum(d[[ind]] < bench_val, na.rm = TRUE)
      mod_v3_kpi_grid(
        mod_v3_kpi(format(nrow(d), big.mark = ","),
                   "\u56fd\u5bb6\u6570", tone = "primary"),
        mod_v3_kpi(if (grepl("che|usd|gdp", ind)) fmt_v3_usd(bench_val)
                   else fmt_v3_pct(bench_val),
                   "\u57fa\u51c6\u503c", tone = "good"),
        mod_v3_kpi(as.character(n_below),
                   "\u4f4e\u4e8e\u57fa\u51c6", tone = "warn"),
        mod_v3_kpi(fmt_v3_pct(n_below / nrow(d) * 100),
                   "\u4f4e\u4e8e\u6bd4\u4f8b", tone = "bad")
      )
    })

    output$gap_bar <- plotly::renderPlotly({
      m <- master_r(); yr <- input$year; ind <- input$indicator
      bench_val <- get_benchmark_value(m, yr, ind, input$benchmark)
      shiny::req(is.finite(bench_val))
      d <- m[m$year == yr & is.finite(m[[ind]]) & !is.na(m$country_name), ]
      if (input$focus_group != "all") {
        d <- d[d$income_group == input$focus_group, ]
      }
      d$gap_pct <- (d[[ind]] - bench_val) / abs(bench_val) * 100
      d <- d[order(d$gap_pct), ]
      below <- utils::head(d[d$gap_pct < 0, ], 15)
      above <- utils::tail(d[d$gap_pct > 0, ], 5)
      show <- rbind(below, above)
      show$country_name <- factor(show$country_name, levels = show$country_name)
      safe_plotly({
        plotly::plot_ly(show, x = ~gap_pct, y = ~country_name,
                        type = "bar", orientation = "h",
                        marker = list(
                          color = ifelse(show$gap_pct < 0, "#a23b3b", "#2a857a")
                        )) |>
          ghs_plotly_layout() |>
          plotly::layout(
            xaxis = list(title = "\u4e0e\u57fa\u51c6\u5dee\u8ddd (%)"),
            yaxis = list(title = ""),
            margin = list(l = 120),
            shapes = list(
              list(type = "line", x0 = 0, x1 = 0, y0 = -0.5, y1 = nrow(show) - 0.5,
                   line = list(color = "#5d667a", width = 1.5))
            )
          )
      })
    })

    output$gap_trend <- plotly::renderPlotly({
      m <- master_r(); ind <- input$indicator
      grp <- "income_group"
      years <- sort(unique(m$year))
      results <- lapply(years, function(yr) {
        bench_val <- get_benchmark_value(m, yr, ind, input$benchmark)
        if (!is.finite(bench_val)) return(NULL)
        d <- m[m$year == yr & is.finite(m[[ind]]) & !is.na(m[[grp]]), ]
        agg <- stats::aggregate(
          stats::as.formula(paste(ind, "~", grp)),
          data = d, FUN = mean, na.rm = TRUE
        )
        agg$gap <- (agg[[ind]] - bench_val) / abs(bench_val) * 100
        agg$year <- yr
        agg[, c("year", grp, "gap")]
      })
      df <- do.call(rbind, results)
      shiny::req(nrow(df) > 5)
      safe_plotly({
        plotly::plot_ly(df, x = ~year, y = ~gap,
                        color = stats::as.formula(paste0("~", grp)),
                        type = "scatter", mode = "lines+markers",
                        colors = unname(brand_palette$income)) |>
          ghs_plotly_layout() |>
          plotly::layout(
            xaxis = list(title = ""),
            yaxis = list(title = "\u4e0e\u57fa\u51c6\u5dee\u8ddd (%)"),
            legend = list(orientation = "h", y = -0.18),
            shapes = list(
              list(type = "line", x0 = min(df$year), x1 = max(df$year),
                   y0 = 0, y1 = 0,
                   line = list(color = "#5d667a", width = 1, dash = "dash"))
            )
          )
      })
    })

    output$catch_up <- plotly::renderPlotly({
      m <- master_r(); ind <- input$indicator; yr <- input$year
      yr_start <- max(yr - 10, min(m$year, na.rm = TRUE))
      bench_now <- get_benchmark_value(m, yr, ind, input$benchmark)
      bench_then <- get_benchmark_value(m, yr_start, ind, input$benchmark)
      shiny::req(is.finite(bench_now), is.finite(bench_then))
      d_now <- m[m$year == yr & is.finite(m[[ind]]), c("iso3_code", "country_name", ind)]
      d_then <- m[m$year == yr_start & is.finite(m[[ind]]), c("iso3_code", ind)]
      names(d_now)[3] <- "val_now"; names(d_then)[2] <- "val_then"
      merged <- merge(d_now, d_then, by = "iso3_code")
      merged$gap_now <- (merged$val_now - bench_now) / abs(bench_now) * 100
      merged$gap_then <- (merged$val_then - bench_then) / abs(bench_then) * 100
      merged$gap_change <- merged$gap_now - merged$gap_then
      catchers <- merged[merged$gap_then < 0, ]
      catchers <- catchers[order(-catchers$gap_change), ]
      top <- utils::head(catchers, 15)
      top$country_name <- factor(top$country_name, levels = rev(top$country_name))
      safe_plotly({
        plotly::plot_ly(top, x = ~gap_change, y = ~country_name,
                        type = "bar", orientation = "h",
                        marker = list(color = "#2a857a")) |>
          ghs_plotly_layout() |>
          plotly::layout(
            xaxis = list(title = "\u5dee\u8ddd\u7f29\u5c0f (pp)"),
            yaxis = list(title = ""),
            margin = list(l = 120)
          )
      })
    })

    output$radar_benchmark <- plotly::renderPlotly({
      m <- master_r(); yr <- input$year
      indicators <- c("che_pc_usd2023", "gghed_che", "hf3_che", "life_exp")
      labels <- c("CHE/cap", "GGHED%", "OOPS%", "Life exp")
      bench_vals <- vapply(indicators, function(ind) {
        get_benchmark_value(m, yr, ind, input$benchmark)
      }, numeric(1))
      grp_val <- if (input$focus_group != "all") {
        d <- m[m$year == yr & m$income_group == input$focus_group, ]
        vapply(indicators, function(ind) mean(d[[ind]], na.rm = TRUE), numeric(1))
      } else {
        d <- m[m$year == yr, ]
        vapply(indicators, function(ind) mean(d[[ind]], na.rm = TRUE), numeric(1))
      }
      bench_norm <- rep(100, length(indicators))
      grp_norm <- grp_val / pmax(bench_vals, 0.01) * 100
      safe_plotly({
        plotly::plot_ly(type = "scatterpolar", mode = "lines+markers") |>
          plotly::add_trace(
            r = c(bench_norm, bench_norm[1]),
            theta = c(labels, labels[1]),
            name = "\u57fa\u51c6",
            line = list(color = "#1d3f5f", width = 2),
            fill = "toself", fillcolor = "rgba(29,63,95,0.1)"
          ) |>
          plotly::add_trace(
            r = c(grp_norm, grp_norm[1]),
            theta = c(labels, labels[1]),
            name = "\u5f53\u524d\u7ec4",
            line = list(color = "#c46327", width = 2),
            fill = "toself", fillcolor = "rgba(196,99,39,0.1)"
          ) |>
          ghs_plotly_layout() |>
          plotly::layout(
            polar = list(radialaxis = list(visible = TRUE, range = c(0, max(c(grp_norm, 120))))),
            legend = list(orientation = "h", y = -0.1)
          )
      })
    })

    output$benchmark_table <- reactable::renderReactable({
      m <- master_r(); yr <- input$year; ind <- input$indicator
      bench_val <- get_benchmark_value(m, yr, ind, input$benchmark)
      d <- m[m$year == yr & is.finite(m[[ind]]), ]
      if (input$focus_group != "all") {
        d <- d[d$income_group == input$focus_group, ]
      }
      d$gap_pct <- round((d[[ind]] - bench_val) / abs(bench_val) * 100, 1)
      d <- d[order(d$gap_pct), ]
      tab <- data.frame(
        "\u56fd\u5bb6" = d$country_name,
        "\u6536\u5165\u7ec4" = d$income_group,
        "\u5b9e\u9645\u503c" = round(d[[ind]], 1),
        "\u57fa\u51c6\u503c" = round(bench_val, 1),
        "\u5dee\u8ddd%" = d$gap_pct,
        check.names = FALSE
      )
      reactable::reactable(tab, defaultPageSize = 15, searchable = TRUE,
        highlight = TRUE, striped = TRUE,
        defaultColDef = reactable::colDef(
          headerStyle = list(background = "#f1f3f7", fontWeight = 700),
          style = function(value) {
            if (!is.numeric(value)) return(list())
            if (is.na(value)) return(list())
            list()
          }
        ),
        columns = list(
          "\u5dee\u8ddd%" = reactable::colDef(
            style = function(value) {
              if (!is.numeric(value) || is.na(value)) return(list())
              color <- if (value < -50) "#a23b3b"
                       else if (value < 0) "#c89a3b"
                       else "#2a857a"
              list(color = color, fontWeight = 700)
            }
          )
        ))
    })
  })
}
