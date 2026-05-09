# =============================================================================
# 仪表盘/模块/mod_efficiency.R
# Tab 4 · 效率 ★：DEA 散点 + 排名 reactable
# =============================================================================

mod_efficiency_ui <- function(id, year_min, year_max) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = htmltools::HTML("&#9881; 效率 Efficiency"),
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 280,
        shiny::sliderInput(ns("year"), "年份",
                            min = year_min, max = year_max, value = year_max,
                            step = 1, sep = ""),
        shiny::tags$hr(),
        shiny::helpText(
          "前沿曲线（DEA 简化版）显示每个 CHE 区间的 ",
          htmltools::strong("效率上限"),
          "。曲线下方的国家未充分把支出转化为预期寿命。")
      ),
      mod_card(
        title = "CHE × 寿命散点 + DEA 前沿",
        mod_spinner(plotly::plotlyOutput(ns("dea_plot"), height = 540))
      ),
      mod_card(
        title = "效率排名（残差越正越优）",
        mod_spinner(reactable::reactableOutput(ns("eff_rank")))
      )
    )
  )
}

mod_efficiency_server <- function(id, master_r) {
  shiny::moduleServer(id, function(input, output, session) {
    eff_data <- shiny::reactive({
      m <- master_r()
      d <- m[m$year == input$year &
              is.finite(m$che_pc_usd2023) &
              is.finite(m$life_exp), , drop = FALSE]
      shiny::req(nrow(d) >= 30)

      # 简化 DEA：log 寿命 vs log CHE 的回归残差作为效率分
      d$log_che  <- log(d$che_pc_usd2023)
      d$log_life <- log(d$life_exp)
      lm_fit <- stats::lm(log_life ~ log_che, data = d)
      d$pred  <- stats::fitted(lm_fit)
      d$resid <- stats::resid(lm_fit)
      d$score <- round(d$resid * 100, 2)
      d
    })

    output$dea_plot <- plotly::renderPlotly({
      d <- eff_data()
      pal <- c(Africa = "#C0504D", Americas = "#1B5E88", Asia = "#E8833C",
               Europe = "#2A9D8F", Oceania = "#7B4B94", Antarctica = "#9C9C9C")
      p <- ggplot2::ggplot(d, ggplot2::aes(che_pc_usd2023, life_exp,
                                            colour = continent,
                                            text = paste0(country_name,
                                                          "<br>Score: ", score))) +
        ggplot2::geom_point(size = 2.4, alpha = 0.85) +
        ggplot2::geom_smooth(ggplot2::aes(group = 1), method = "loess",
                              se = TRUE, colour = "#1A1A1F", linewidth = 0.6,
                              fill = "#1A1A1F18") +
        ggplot2::scale_x_log10(labels = scales::label_dollar()) +
        ggplot2::scale_colour_manual(values = pal, name = NULL) +
        ggplot2::labs(x = "CHE per capita (USD 2023, log)",
                      y = "Life expectancy at birth") +
        ggplot2::theme_minimal(base_size = 12)
      plotly::ggplotly(p, tooltip = "text") |>
        plotly::config(displaylogo = FALSE)
    })

    output$eff_rank <- reactable::renderReactable({
      d <- eff_data()
      d <- d[order(-d$score), , drop = FALSE]
      tab <- data.frame(
        Rank = seq_len(nrow(d)),
        Country = d$country_name,
        Continent = d$continent,
        `CHE pc (USD)` = round(d$che_pc_usd2023, 0),
        `Life Exp` = round(d$life_exp, 1),
        `Eff Score` = d$score,
        check.names = FALSE
      )
      reactable::reactable(tab, searchable = TRUE, defaultPageSize = 15,
        pagination = TRUE, highlight = TRUE,
        defaultColDef = reactable::colDef(headerStyle = list(background = "#f1f3f7")),
        columns = list(
          `Eff Score` = reactable::colDef(
            style = function(value) {
              col <- if (value > 0) "#3F8F4A" else "#C0504D"
              list(color = col, fontWeight = "bold")
            })
        ))
    })
  })
}
