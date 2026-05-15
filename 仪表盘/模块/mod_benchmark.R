# =============================================================================
# 仪表盘/模块/mod_benchmark.R
# 基准对标：选择参照国组，量化差距与追赶路径
# =============================================================================

mod_benchmark_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = htmltools::HTML("&#127919; \u5bf9\u6807 Benchmark"),
    mod_v3_hero(
      kicker = "BENCHMARKING & GAP ANALYSIS",
      title = "\u57fa\u51c6\u5bf9\u6807\u4e0e\u5dee\u8ddd\u5206\u6790",
      lead = paste(
        "\u9009\u62e9\u53c2\u7167\u7ec4\uff08\u5982 OECD \u5747\u503c\u3001\u540c\u6536\u5165\u7ec4\u4e2d\u4f4d\u6570\uff09\uff0c",
        "\u91cf\u5316\u5404\u56fd\u4e0e\u57fa\u51c6\u7684\u5dee\u8ddd\uff0c\u8bc6\u522b\u8ffd\u8d76\u8005\u4e0e\u843d\u540e\u8005\u3002"
      ),
      meta = list("WHO GHED 2024-12", "\u591a\u6307\u6807\u5bf9\u6807", "195 \u56fd\u5bb6")
    ),
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
        shiny::tags$hr(),
        shiny::helpText(
          "\u5dee\u8ddd = (\u56fd\u5bb6\u503c - \u57fa\u51c6\u503c) / \u57fa\u51c6\u503c \u00d7 100%\u3002",
          "\u8d1f\u503c = \u4f4e\u4e8e\u57fa\u51c6\uff0c\u6b63\u503c = \u8d85\u8fc7\u57fa\u51c6\u3002"
        )
      ),
      shiny::uiOutput(ns("kpi_strip")),
      # Row 1
      bslib::layout_columns(
        col_widths = c(7, 5),
        mod_card(
          title = "\u4e0e\u57fa\u51c6\u7684\u5dee\u8ddd\u5206\u5e03",
          htmltools::p(class = "card-note",
            "\u5404\u56fd\u4e0e\u53c2\u7167\u57fa\u51c6\u7684\u767e\u5206\u6bd4\u5dee\u8ddd\u3002\u7ea2\u8272 = \u4f4e\u4e8e\u57fa\u51c6\u3002"),
          mod_spinner(plotly::plotlyOutput(ns("gap_bar"), height = 480))
        ),
        mod_card(
          title = "\u5dee\u8ddd\u6536\u7f29\u8d8b\u52bf",
          htmltools::p(class = "card-note",
            "\u5404\u7ec4\u4e0e\u57fa\u51c6\u7684\u5e73\u5747\u5dee\u8ddd\u968f\u65f6\u95f4\u7684\u53d8\u5316\u3002"),
          mod_spinner(plotly::plotlyOutput(ns("gap_trend"), height = 480))
        )
      ),
      # Row 2
      bslib::layout_columns(
        col_widths = c(6, 6),
        mod_card(
          title = "\u8ffd\u8d76\u901f\u5ea6\u6392\u884c",
          htmltools::p(class = "card-note",
            "\u8fc7\u53bb 10 \u5e74\u5dee\u8ddd\u7f29\u5c0f\u6700\u5feb\u7684\u56fd\u5bb6\u3002"),
          mod_spinner(plotly::plotlyOutput(ns("catch_up"), height = 380))
        ),
        mod_card(
          title = "\u591a\u6307\u6807\u96f7\u8fbe\u5bf9\u6807",
          htmltools::p(class = "card-note",
            "\u9009\u5b9a\u7ec4\u522b\u5728 4 \u4e2a\u6307\u6807\u4e0a\u4e0e\u57fa\u51c6\u7684\u5bf9\u6bd4\u3002"),
          mod_spinner(plotly::plotlyOutput(ns("radar_benchmark"), height = 380))
        )
      ),
      mod_card(
        title = "\u5bf9\u6807\u8be6\u8868",
        mod_spinner(reactable::reactableOutput(ns("benchmark_table")))
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
        "peer_income" = NA_real_  # handled per-country
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
      # Show top 15 below + top 5 above
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
      # Countries that were below and caught up most
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
      # Get benchmark values
      bench_vals <- vapply(indicators, function(ind) {
        get_benchmark_value(m, yr, ind, input$benchmark)
      }, numeric(1))
      # Get group average
      grp_val <- if (input$focus_group != "all") {
        d <- m[m$year == yr & m$income_group == input$focus_group, ]
        vapply(indicators, function(ind) mean(d[[ind]], na.rm = TRUE), numeric(1))
      } else {
        d <- m[m$year == yr, ]
        vapply(indicators, function(ind) mean(d[[ind]], na.rm = TRUE), numeric(1))
      }
      # Normalize to benchmark = 100
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
