# =============================================================================
# 仪表盘/模块/mod_outcomes.R
# Tab 6 · 产出 ★：CHE-寿命弹性 + 收入组对比
# =============================================================================

mod_outcomes_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = htmltools::HTML("&#127973; \u4ea7\u51fa Outcomes"),
    value = "outcomes",
    htmltools::div(
      class = "panel-hero",
      htmltools::h2("\u8d44\u91d1 \u2192 \u5bff\u547d\u7684\u9650\u8fb9\u95ee\u9898"),
      htmltools::p(class = "text-muted",
                   paste("\u5728\u4e0d\u540c\u6536\u5165\u7ec4\u91cc\u3001\u591a\u4ed8 1% CHE/cap \u5bf9\u9884\u671f\u5bff\u547d\u7684\u9650\u8fb9\u8d21\u732e\u4e0d\u540c\u3002",
                         "\u5de6\u56fe = \u70b9\u4e0e\u5206\u6bb5 OLS\uff0c\u53f3\u8868 = \u6bcf\u4e2a\u6536\u5165\u7ec4\u7684\u5f39\u6027\u53c2\u6570 \u00b1 95% CI\u3002",
                         "\u5916\u751f\u6027\u53d7\u8bb8\u591a\u9009\u9879\u5f71\u54cd\uff0c\u5b9c\u4f5c\u4e3a\u5173\u8054\u4fe1\u53f7\u3002"))
    ),
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 280,
        shiny::checkboxGroupInput(ns("groups"), "显示收入组",
          choices = c("Low income", "Lower middle income",
                       "Upper middle income", "High income"),
          selected = c("Low income", "Lower middle income",
                        "Upper middle income", "High income")),
        shiny::tags$hr(),
        shiny::helpText(
          "弹性 = ", htmltools::tags$em("\u0394 寿命 / \u0394 log CHE/cap"),
          "。收入越低，每多 1% CHE/cap 带来的寿命收益越大。")
      ),
      mod_card(
        title = "log CHE × 寿命 × 收入组",
        mod_spinner(plotly::plotlyOutput(ns("elasticity_plot"), height = 540))
      ),
      mod_card(
        title = "弹性系数（按收入组分段 OLS）",
        mod_spinner(reactable::reactableOutput(ns("elasticity_table")))
      )
    )
  )
}

mod_outcomes_server <- function(id, master_r, year_max) {
  shiny::moduleServer(id, function(input, output, session) {
    out_data <- shiny::reactive({
      m <- master_r()
      d <- m[m$year == year_max &
              is.finite(m$che_pc_usd2023) &
              is.finite(m$life_exp) &
              !is.na(m$income_group), , drop = FALSE]
      d$log_che <- log(d$che_pc_usd2023)
      d <- d[d$income_group %in% input$groups, , drop = FALSE]
      d$income_group <- factor(d$income_group,
        levels = c("Low income", "Lower middle income",
                    "Upper middle income", "High income"))
      d
    })

    output$elasticity_plot <- plotly::renderPlotly({
      d <- out_data()
      shiny::req(nrow(d) > 0)
      pal <- c("Low income" = "#A03B27", "Lower middle income" = "#D89B5B",
               "Upper middle income" = "#4F8FBF", "High income" = "#0B3D5C")
      p <- ggplot2::ggplot(d, ggplot2::aes(log_che, life_exp,
                                            colour = income_group,
                                            text = country_name)) +
        ggplot2::geom_point(size = 2.4, alpha = 0.85) +
        ggplot2::geom_smooth(method = "lm", se = TRUE, linewidth = 1,
                              ggplot2::aes(group = income_group)) +
        ggplot2::scale_colour_manual(values = pal, name = "Income group") +
        ggplot2::labs(x = "log CHE / cap (USD 2023)",
                      y = "Life expectancy at birth (years)") +
        ggplot2::theme_minimal(base_size = 12)
      plotly::ggplotly(p, tooltip = "text") |>
        plotly::config(displaylogo = FALSE)
    })

    output$elasticity_table <- reactable::renderReactable({
      d <- out_data()
      shiny::req(nrow(d) > 0)
      out_tab <- d |>
        split(d$income_group) |>
        Filter(f = function(x) nrow(x) >= 5) |>
        lapply(function(x) {
          fit <- stats::lm(life_exp ~ log_che, data = x)
          ci <- tryCatch(stats::confint(fit)[2, ],
                          error = function(e) c(NA, NA))
          data.frame(
            n = nrow(x),
            beta = round(stats::coef(fit)[2], 3),
            ci_low = round(ci[1], 3),
            ci_high = round(ci[2], 3),
            r2 = round(summary(fit)$r.squared, 3)
          )
        }) |>
        do.call(rbind, args = _)
      out_tab$IncomeGroup <- rownames(out_tab)
      out_tab <- out_tab[, c("IncomeGroup", "n", "beta", "ci_low", "ci_high", "r2")]
      names(out_tab) <- c("收入组", "n", "弹性 \u03b2",
                          "CI 下", "CI 上", "R\u00b2")
      reactable::reactable(out_tab, defaultPageSize = 4,
        pagination = FALSE, highlight = TRUE,
        defaultColDef = reactable::colDef(headerStyle = list(background = "#f1f3f7")))
    })
  })
}
