# =============================================================================
# 仪表盘/模块/mod_equity.R
# Tab 3 · 公平 ★：Gini/Theil/Atkinson 多线 + Lorenz 曲线 + 灾难性 OOP
# =============================================================================

mod_equity_ui <- function(id, year_min, year_max) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = htmltools::HTML("&#9878; \u516c\u5e73 Equity"),
    htmltools::div(
      class = "panel-hero",
      htmltools::h2("\u516c\u5e73\u4e0e\u4e0d\u5e73\u7b49"),
      htmltools::p(class = "text-muted",
                   paste("\u8de8\u56fd\u4eba\u5747 CHE \u7684 Gini / Theil-T / Atkinson \u4e09\u6307\u6570\u540c\u671f\u4e0b\u884c\uff0c",
                         "\u4f46 Theil-T \u4e0e Atkinson \u6536\u655b\u66f4\u6162\u8868\u660e\u6781\u503c\u56fd\u5bb6\u5dee\u8ddd\u4ecd\u5728\u3002",
                         "\u5de6\u4fa7\u53ef\u9009\u8981\u663e\u793a\u7684\u6307\u6570\uff0c\u53f3\u4fa7 Lorenz \u53ef\u8c03\u5e74\u4efd\u5bf9\u6bd4\u3002"))
    ),
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 280,
        shiny::sliderInput(ns("lorenz_years"), "Lorenz 对比年份",
                            min = year_min, max = year_max,
                            value = c(2000, year_max), step = 1, sep = ""),
        shiny::checkboxGroupInput(ns("indices"), "显示指数",
                                   choices = c("Gini", "Theil-T",
                                                "Atkinson(\u03b5=0.5)",
                                                "Atkinson(\u03b5=1)",
                                                "Atkinson(\u03b5=2)"),
                                   selected = c("Gini", "Theil-T",
                                                 "Atkinson(\u03b5=1)")),
        shiny::tags$hr(),
        shiny::helpText(
          "三大不平等指数同期下行，但 ", htmltools::strong("Theil-T 与 Atkinson"),
          " 比 Gini 收敛更慢 — 极值国家差距仍很大。")
      ),
      bslib::layout_columns(
        col_widths = c(7, 5),
        mod_card(
          title = "不平等指数时序（2000–latest）",
          mod_spinner(plotly::plotlyOutput(ns("indices_ts"), height = 460))
        ),
        mod_card(
          title = "Lorenz 曲线对比（人口加权）",
          mod_spinner(plotly::plotlyOutput(ns("lorenz_plot"), height = 460))
        )
      ),
      mod_card(
        title = "灾难性 OOP 排行（最新年）",
        mod_spinner(reactable::reactableOutput(ns("catastrophic_table")))
      )
    )
  )
}

mod_equity_server <- function(id, master_r, year_max) {
  shiny::moduleServer(id, function(input, output, session) {
    ineq_data <- shiny::reactive({
      m <- master_r()
      tryCatch(inequality_by_year(m, value_col = "che_pc_usd2023"),
                error = function(e) NULL)
    })

    output$indices_ts <- plotly::renderPlotly({
      d <- ineq_data()
      shiny::req(d, nrow(d) > 0)
      cols_map <- c("Gini" = "gini_pop", "Theil-T" = "theil_pop",
                    "Atkinson(\u03b5=0.5)" = "atk05",
                    "Atkinson(\u03b5=1)"   = "atk1",
                    "Atkinson(\u03b5=2)"   = "atk2")
      sel <- intersect(input$indices, names(cols_map))
      shiny::req(length(sel) > 0)
      use_cols <- cols_map[sel]
      use_cols <- use_cols[use_cols %in% names(d)]
      d2 <- d[, c("year", use_cols)]
      d2 <- tidyr::pivot_longer(d2, dplyr::all_of(unname(use_cols)),
                                 names_to = "key", values_to = "value")
      lab_lookup <- stats::setNames(names(use_cols), use_cols)
      d2$index <- factor(lab_lookup[d2$key], levels = names(cols_map))

      pal <- c("Gini" = "#1B5E88", "Theil-T" = "#C46B27",
               "Atkinson(\u03b5=0.5)" = "#6B8E5A",
               "Atkinson(\u03b5=1)" = "#7B4B94",
               "Atkinson(\u03b5=2)" = "#E8833C")
      p <- ggplot2::ggplot(d2, ggplot2::aes(year, value, colour = index)) +
        ggplot2::geom_line(linewidth = 1) +
        ggplot2::geom_point(size = 1.6) +
        ggplot2::scale_colour_manual(values = pal, name = NULL) +
        ggplot2::scale_x_continuous(breaks = scales::breaks_pretty(7)) +
        ggplot2::labs(x = NULL, y = "Inequality index (0–1)") +
        ggplot2::theme_minimal(base_size = 12)
      plotly::ggplotly(p) |> plotly::config(displaylogo = FALSE)
    })

    output$lorenz_plot <- plotly::renderPlotly({
      m <- master_r()
      yrs <- input$lorenz_years
      shiny::req(length(yrs) == 2)
      yrs <- sort(unique(yrs))
      curves <- lapply(yrs, function(y) {
        d <- m[m$year == y &
                is.finite(m$che_pc_usd2023) &
                is.finite(m$pop), , drop = FALSE]
        if (nrow(d) < 5) return(NULL)
        l <- tryCatch(fit_lorenz(d$che_pc_usd2023, weights = d$pop),
                       error = function(e) NULL)
        if (is.null(l)) return(NULL)
        data.frame(p_pop = l$p_pop, p_value = l$p_value, year = as.character(y))
      })
      curves <- Filter(Negate(is.null), curves)
      if (!length(curves)) return(plotly::plotly_empty())
      df <- do.call(rbind, curves)
      pal <- c("#1B5E88", "#C46B27", "#6B8E5A", "#7B4B94")[seq_along(unique(df$year))]
      p <- ggplot2::ggplot(df, ggplot2::aes(p_pop, p_value, colour = year)) +
        ggplot2::geom_abline(slope = 1, intercept = 0,
                              linetype = "dashed", colour = "#1A1A1F40") +
        ggplot2::geom_line(linewidth = 1) +
        ggplot2::scale_colour_manual(values = pal, name = "Year") +
        ggplot2::scale_x_continuous(labels = scales::percent_format(1)) +
        ggplot2::scale_y_continuous(labels = scales::percent_format(1)) +
        ggplot2::labs(x = "Population (cumulative)",
                      y = "CHE per capita (cumulative)") +
        ggplot2::theme_minimal(base_size = 12)
      plotly::ggplotly(p) |> plotly::config(displaylogo = FALSE)
    })

    output$catastrophic_table <- reactable::renderReactable({
      m <- master_r()
      d <- m[m$year == year_max & is.finite(m$hf3_che), , drop = FALSE]
      d <- d[order(-d$hf3_che), , drop = FALSE]
      d <- utils::head(d, 30)
      d <- data.frame(
        Rank        = seq_len(nrow(d)),
        Country     = d$country_name,
        Continent   = d$continent,
        IncomeGroup = d$income_group,
        `OOPS %`    = round(d$hf3_che, 1),
        `CHE per capita (USD)` = round(d$che_pc_usd2023, 0),
        check.names = FALSE
      )
      reactable::reactable(d, searchable = TRUE, defaultPageSize = 15,
        pagination = TRUE, highlight = TRUE,
        defaultColDef = reactable::colDef(headerStyle = list(background = "#f1f3f7")),
        columns = list(
          `OOPS %` = reactable::colDef(
            style = function(value) {
              col <- if (value > 50) "#C0504D"
                      else if (value > 30) "#E07B00"
                      else "#3F8F4A"
              list(color = col, fontWeight = "bold")
            })
        ))
    })
  })
}
