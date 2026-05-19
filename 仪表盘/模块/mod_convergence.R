# =============================================================================
# 仪表盘/模块/mod_convergence.R
# 收敛分析：beta 收敛、sigma 收敛与收入组异质性
# =============================================================================

mod_convergence_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = htmltools::HTML("&#128200; \u6536\u655b Convergence"),
    value = "convergence",
    mod_v3_hero(
      kicker = "CONVERGENCE DIAGNOSTICS",
      title = "\u536b\u751f\u652f\u51fa\u6536\u655b\u4e0e\u8ffd\u8d76\u901f\u5ea6",
      lead = paste(
        "\u68c0\u9a8c\u5168\u7403\u4eba\u5747 CHE \u662f\u5426\u5b58\u5728 beta \u6536\u655b\u548c sigma \u6536\u655b\uff1a",
        "\u4f4e\u8d77\u70b9\u56fd\u5bb6\u662f\u5426\u66f4\u5feb\u589e\u957f\uff0c",
        "\u800c\u5168\u4f53\u5dee\u8ddd\u662f\u5426\u5728\u65f6\u95f4\u4e0a\u771f\u6b63\u6536\u7a84\u3002"
      ),
      meta = list("WHO GHED 2024-12", "195 \u56fd\u5bb6", "2005\u20132023 \u8ffd\u8d76\u7a97\u53e3")
    ),
    mod_v3_page_body(
      wide = TRUE,
      mod_v3_topic_brief("convergence"),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 280,
          shiny::sliderInput(ns("year"), "\u5bf9\u6bd4\u7ec8\u70b9\u5e74",
            min = 2006, max = 2023, value = 2023, step = 1, sep = ""),
          mod_v3_sidebar_note(
            "\u6536\u655b\u8bfb\u6cd5",
            "\u672c\u9875\u628a 2005 \u5e74\u4f5c\u4e3a\u8d77\u70b9\uff0c\u4f60\u53ef\u4ee5\u62d6\u52a8\u7ec8\u70b9\u5e74\u89c2\u5bdf\u8ffd\u8d76\u7a97\u53e3\u7684\u7a33\u5b9a\u6027\u3002",
            bullets = c(
              "beta \u56fe\u770b\u4f4e\u8d77\u70b9\u56fd\u5bb6\u662f\u5426\u66f4\u5feb\u589e\u957f\u3002",
              "sigma \u56fe\u770b\u5168\u4f53\u5206\u5e03\u662f\u5426\u771f\u6b63\u53d8\u7a84\u3002",
              "\u6536\u5165\u7ec4\u7bb1\u7ebf\u56fe\u7528\u4e8e\u5224\u65ad\u6536\u655b\u662f\u5168\u5c40\u8fd8\u662f\u5c40\u90e8\u73b0\u8c61\u3002"
            )
          )
        ),
        shiny::uiOutput(ns("kpi_strip")),
        bslib::layout_columns(
          col_widths = c(6, 6),
          mod_card(
            kicker = "F1 \u00b7 Beta convergence",
            title = "\u521d\u59cb\u6c34\u5e73 vs \u5e74\u5747\u589e\u901f",
            htmltools::p(class = "card-note", "\u6a2a\u8f74\u4e3a 2005 \u5e74 log(CHE/cap)\uff0c\u7eb5\u8f74\u4e3a\u5230\u7ec8\u70b9\u5e74\u7684\u5e74\u5747\u5bf9\u6570\u589e\u901f\u3002"),
            mod_v3_chart_guide(
              "\u8d1f\u65b9\u5411\u659c\u7387\u624d\u8868\u793a\u8ffd\u8d76",
              "\u5982\u679c\u4f4e\u8d77\u70b9\u56fd\u5bb6\u666e\u904d\u589e\u957f\u66f4\u5feb\uff0c\u70b9\u4e91\u4f1a\u5448\u73b0\u5411\u4e0b\u5173\u7cfb\uff1b\u53cd\u4e4b\u5219\u8868\u793a\u652f\u51fa\u5dee\u8ddd\u53ef\u80fd\u7ee7\u7eed\u6269\u5927\u3002",
              bullets = c("\u60ac\u505c\u67e5\u770b\u56fd\u5bb6\uff0c\u4f18\u5148\u5173\u6ce8\u4f4e\u8d77\u70b9\u9ad8\u589e\u901f\u548c\u9ad8\u8d77\u70b9\u8d1f\u589e\u901f\u56fd\u5bb6\u3002"),
              tone = "warn"
            ),
            mod_spinner(plotly::plotlyOutput(ns("beta_scatter"), height = 440)),
            footer = "\u5e74\u5747\u589e\u901f\u4f7f\u7528 log(end) - log(start) \u9664\u4ee5\u5e74\u6570\uff0c\u66f4\u9002\u5408\u8de8\u5dee\u8ddd\u6bd4\u8f83\u3002"
          ),
          mod_card(
            kicker = "F2 \u00b7 Sigma convergence",
            title = "\u4eba\u5747 CHE \u5dee\u5f02\u5ea6\u65f6\u5e8f",
            htmltools::p(class = "card-note", "\u4ee5 log(CHE/cap) \u6807\u51c6\u5dee\u8861\u91cf\u5168\u7403\u5206\u5e03\u662f\u5426\u6536\u7a84\u3002"),
            mod_v3_chart_guide(
              "\u4e0b\u884c\u624d\u662f\u5dee\u8ddd\u6536\u7a84",
              "sigma \u6536\u655b\u5173\u6ce8\u5206\u5e03\u5bbd\u5ea6\uff1a\u5373\u4f7f\u6709\u4e00\u6279\u4f4e\u8d77\u70b9\u56fd\u5bb6\u5feb\u901f\u8ffd\u8d76\uff0c\u53ea\u8981\u6574\u4f53\u6807\u51c6\u5dee\u4e0d\u964d\uff0c\u5168\u5c40\u8ddd\u79bb\u5c31\u672a\u5fc5\u7f29\u5c0f\u3002"
            ),
            mod_spinner(plotly::plotlyOutput(ns("sigma_trend"), height = 440)),
            footer = "\u5bf9\u6570\u53e3\u5f84\u51cf\u5c11\u6781\u7aef\u9ad8\u652f\u51fa\u56fd\u5bb6\u5bf9\u79bb\u6563\u5ea6\u7684\u4e3b\u5bfc\u3002"
          )
        ),
        mod_card(
          kicker = "F3 \u00b7 Income heterogeneity",
          title = "\u6309\u6536\u5165\u7ec4\u6bd4\u8f83\u8ffd\u8d76\u901f\u5ea6",
          htmltools::p(class = "card-note", "\u7bb1\u7ebf\u56fe\u5c55\u793a\u4e0d\u540c\u6536\u5165\u7ec4\u5728\u540c\u4e00\u8ffd\u8d76\u7a97\u53e3\u4e2d\u7684\u589e\u901f\u5206\u5e03\u3002"),
          mod_v3_chart_guide(
            "\u770b\u7ec4\u5185\u5dee\u5f02\u548c\u79bb\u7fa4\u70b9",
            "\u6536\u5165\u7ec4\u4e2d\u4f4d\u7ebf\u63d0\u793a\u5178\u578b\u8ffd\u8d76\u901f\u5ea6\uff0c\u79bb\u7fa4\u70b9\u53ef\u80fd\u662f\u653f\u7b56\u6269\u5f20\u3001\u4ef7\u683c\u53d8\u52a8\u6216\u6570\u636e\u53e3\u5f84\u6539\u53d8\u7684\u4fe1\u53f7\u3002",
            tone = "good"
          ),
          mod_spinner(plotly::plotlyOutput(ns("beta_by_income"), height = 400))
        ),
        mod_card(
          kicker = "F4 \u00b7 Country audit",
          title = "\u56fd\u5bb6\u8ffd\u8d76\u660e\u7ec6\u8868",
          mod_v3_chart_guide(
            "\u8868\u683c\u4fdd\u7559\u6392\u540d\u4e0e\u6570\u503c",
            "\u6309 CAGR \u964d\u5e8f\u5217\u51fa\u56fd\u5bb6\u3001\u8d77\u70b9\u652f\u51fa\u3001\u7ec8\u70b9\u652f\u51fa\u548c\u5e74\u590d\u5408\u589e\u901f\uff0c\u4fbf\u4e8e\u5bf9\u7167\u56fe\u4e2d\u7684\u8ffd\u8d76\u8005\u3002"
          ),
          mod_spinner(reactable::reactableOutput(ns("conv_table"))),
          footer = "\u8d77\u70b9\u56fa\u5b9a\u4e3a 2005 \u5e74\uff1b\u7ec8\u70b9\u968f\u4fa7\u680f\u5e74\u4efd\u53d8\u5316\u3002"
        )
      )
    )
  )
}


mod_convergence_server <- function(id, master_r) {
  shiny::moduleServer(id, function(input, output, session) {
    
    output$kpi_strip <- shiny::renderUI({
      m <- master_r(); yr <- input$year
      d <- m[m$year == yr, ]
      n <- sum(is.finite(d$che_pc_usd2023))
      mod_v3_kpi_grid(
        mod_v3_kpi(format(n, big.mark = ","), "\u56fd\u5bb6\u6570", tone = "primary"),
        mod_v3_kpi(as.character(yr), "\u7ec8\u70b9\u5e74", tone = "neutral"),
        mod_v3_kpi(fmt_usd(median(d$che_pc_usd2023, na.rm = TRUE)), "\u4eba\u5747 CHE \u4e2d\u4f4d\u6570", tone = "secondary"),
        mod_v3_kpi(fmt_pct(mean(d$hf3_che, na.rm = TRUE)), "\u5e73\u5747 OOPS", tone = "warn")
      )
    })
    
    output$beta_scatter <- plotly::renderPlotly({
      m <- master_r()
      yr_a <- 2005; yr_b <- input$year
      shiny::req(yr_b > yr_a)
      da <- m[m$year == yr_a & is.finite(m$che_pc_usd2023) & m$che_pc_usd2023 > 0, c("iso3_code","country_name","continent","che_pc_usd2023")]
      db <- m[m$year == yr_b & is.finite(m$che_pc_usd2023) & m$che_pc_usd2023 > 0, c("iso3_code","che_pc_usd2023")]
      names(da)[4] <- "start"; names(db)[2] <- "end"
      mg <- merge(da, db, by = "iso3_code")
      mg$growth <- (log(mg$end) - log(mg$start)) / (yr_b - yr_a)
      mg$log_start <- log(mg$start)
      safe_plotly({
        plotly::plot_ly(mg, x = ~log_start, y = ~growth, color = ~continent,
                        text = ~country_name, type = "scatter", mode = "markers",
                        colors = unname(brand_palette$continent),
                        marker = list(size = 8, opacity = 0.7)) |>
          ghs_plotly_layout() |>
          plotly::layout(xaxis = list(title = "log(2005 \u4eba\u5747 CHE)"),
                         yaxis = list(title = "\u5e74\u5747\u5bf9\u6570\u589e\u901f"))
      })
    })
    output$sigma_trend <- plotly::renderPlotly({
      m <- master_r()
      d <- m[is.finite(m$che_pc_usd2023) & m$che_pc_usd2023 > 0, ]
      agg <- do.call(rbind, lapply(split(d, d$year), function(ch)
        data.frame(year = ch$year[1], sd_log = sd(log(ch$che_pc_usd2023), na.rm = TRUE))))
      safe_plotly({
        plotly::plot_ly(agg, x = ~year, y = ~sd_log, type = "scatter", mode = "lines+markers",
                        line = list(color = "#1d3f5f", width = 3)) |>
          ghs_plotly_layout() |>
          plotly::layout(xaxis = list(title = ""), yaxis = list(title = "log(CHE/cap) \u6807\u51c6\u5dee"))
      })
    })
    output$beta_by_income <- plotly::renderPlotly({
      m <- master_r()
      yr_a <- 2005; yr_b <- input$year
      shiny::req(yr_b > yr_a)
      da <- m[m$year == yr_a & is.finite(m$che_pc_usd2023) & m$che_pc_usd2023 > 0, c("iso3_code","income_group","che_pc_usd2023")]
      db <- m[m$year == yr_b & is.finite(m$che_pc_usd2023) & m$che_pc_usd2023 > 0, c("iso3_code","che_pc_usd2023")]
      names(da)[3] <- "start"; names(db)[2] <- "end"
      mg <- merge(da, db, by = "iso3_code")
      mg$growth <- (log(mg$end) - log(mg$start)) / (yr_b - yr_a)
      mg <- mg[!is.na(mg$income_group), ]
      safe_plotly({
        plotly::plot_ly(mg, x = ~income_group, y = ~growth, type = "box",
                        color = ~income_group, colors = unname(brand_palette$income)) |>
          ghs_plotly_layout() |>
          plotly::layout(showlegend = FALSE, xaxis = list(title = ""), yaxis = list(title = "\u5e74\u5747\u5bf9\u6570\u589e\u901f"))
      })
    })
    output$conv_table <- reactable::renderReactable({
      m <- master_r()
      yr_a <- 2005; yr_b <- input$year
      shiny::req(yr_b > yr_a)
      da <- m[m$year == yr_a & is.finite(m$che_pc_usd2023) & m$che_pc_usd2023 > 0, c("iso3_code","country_name","che_pc_usd2023")]
      db <- m[m$year == yr_b & is.finite(m$che_pc_usd2023), c("iso3_code","che_pc_usd2023")]
      names(da)[3] <- "start"; names(db)[2] <- "end"
      mg <- merge(da, db, by = "iso3_code")
      mg$cagr <- ((mg$end / mg$start)^(1/(yr_b - yr_a)) - 1) * 100
      mg <- mg[order(-mg$cagr), ]
      tab <- data.frame("\u56fd\u5bb6" = mg$country_name,
                        "\u8d77\u70b9 CHE/cap" = round(mg$start),
                        "\u7ec8\u70b9 CHE/cap" = round(mg$end),
                        "CAGR%" = round(mg$cagr, 2),
                        check.names = FALSE)
      reactable::reactable(tab, defaultPageSize = 15, searchable = TRUE, highlight = TRUE,
        defaultColDef = reactable::colDef(headerStyle = list(background = "#f1f3f7")))
    })
  })
}

