# =============================================================================
# 仪表盘/模块/mod_convergence.R
# CONVERGENCE
# =============================================================================

mod_convergence_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = htmltools::HTML("&#128200; Convergence"),
    mod_v3_hero(
      kicker = "收敛与趋同分析",
      title = "CONVERGENCE",
      lead = "检验全球卫生支出是否存在 beta-收敛和 sigma-收敛趋势。初始水平低的国家是否增长更快？",
      meta = list("WHO GHED 2024-12", "195 countries", "2000-2023")
    ),
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 260,
        shiny::sliderInput(ns("year"), "Year",
          min = 2000, max = 2023, value = 2023, step = 1, sep = "")
      ),
      shiny::uiOutput(ns("kpi_strip")),
      bslib::layout_columns(
        col_widths = c(6, 6),
        mod_card(title = "Beta-convergence scatter",
          htmltools::p(class = "card-note", "初始水平 vs 增速：负斜率 = 收敛。"),
          mod_spinner(plotly::plotlyOutput(ns("beta_scatter"), height = 420))),
        mod_card(title = "Sigma-convergence trend",
          htmltools::p(class = "card-note", "人均 CHE 标准差随时间变化。"),
          mod_spinner(plotly::plotlyOutput(ns("sigma_trend"), height = 420)))
      ),
      mod_card(title = "Convergence by income group",
        mod_spinner(plotly::plotlyOutput(ns("beta_by_income"), height = 380))),
      mod_card(title = "Detail table",
        mod_spinner(reactable::reactableOutput(ns("conv_table"))))
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
        mod_v3_kpi(format(n, big.mark = ","), "Countries", tone = "primary"),
        mod_v3_kpi(as.character(yr), "Year", tone = "neutral"),
        mod_v3_kpi(fmt_usd(median(d$che_pc_usd2023, na.rm = TRUE)), "Median CHE/cap", tone = "secondary"),
        mod_v3_kpi(fmt_pct(mean(d$hf3_che, na.rm = TRUE)), "Mean OOPS", tone = "warn")
      )
    })
    
    output$beta_scatter <- plotly::renderPlotly({
      m <- master_r()
      yr_a <- 2005; yr_b <- max(m$year, na.rm = TRUE)
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
          plotly::layout(xaxis = list(title = "log(initial CHE/cap)"),
                         yaxis = list(title = "Annual growth rate"))
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
          plotly::layout(xaxis = list(title = ""), yaxis = list(title = "SD of log(CHE/cap)"))
      })
    })
    output$beta_by_income <- plotly::renderPlotly({
      m <- master_r()
      yr_a <- 2005; yr_b <- max(m$year, na.rm = TRUE)
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
          plotly::layout(showlegend = FALSE, xaxis = list(title = ""), yaxis = list(title = "Growth rate"))
      })
    })
    output$conv_table <- reactable::renderReactable({
      m <- master_r()
      yr_a <- 2005; yr_b <- max(m$year, na.rm = TRUE)
      da <- m[m$year == yr_a & is.finite(m$che_pc_usd2023) & m$che_pc_usd2023 > 0, c("iso3_code","country_name","che_pc_usd2023")]
      db <- m[m$year == yr_b & is.finite(m$che_pc_usd2023), c("iso3_code","che_pc_usd2023")]
      names(da)[3] <- "start"; names(db)[2] <- "end"
      mg <- merge(da, db, by = "iso3_code")
      mg$cagr <- ((mg$end / mg$start)^(1/(yr_b - yr_a)) - 1) * 100
      mg <- mg[order(-mg$cagr), ]
      tab <- data.frame(Country = mg$country_name, Start = round(mg$start), End = round(mg$end),
                        CAGR_pct = round(mg$cagr, 2), check.names = FALSE)
      reactable::reactable(tab, defaultPageSize = 15, searchable = TRUE, highlight = TRUE,
        defaultColDef = reactable::colDef(headerStyle = list(background = "#f1f3f7")))
    })
  })
}

