# =============================================================================
# 仪表盘/模块/mod_equity.R
# Tab 3 · 公平 ★：Gini/Theil/Atkinson 多线 + Lorenz 曲线 + 灾难性 OOP
# =============================================================================

mod_equity_ui <- function(id, year_min, year_max) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = htmltools::HTML("&#9878; \u516c\u5e73 Equity"),
    value = "equity",
    mod_v3_hero(
      kicker = "EQUITY & DISTRIBUTION",
      title = "\u516c\u5e73\u4e0e\u4e0d\u5e73\u7b49",
      lead = paste(
        "\u5c06\u8de8\u56fd\u4eba\u5747 CHE \u653e\u5165 Gini\u3001Theil-T\u3001Atkinson \u548c Lorenz \u66f2\u7ebf\u6846\u67b6\uff0c",
        "\u540c\u65f6\u89c2\u5bdf\u6574\u4f53\u6536\u655b\u3001\u5c3e\u90e8\u6781\u503c\u548c\u5bb6\u5ead\u81ea\u4ed8\u538b\u529b\u3002",
        "\u8fd9\u4e00\u9875\u9002\u5408\u628a\u201c\u6295\u5165\u589e\u52a0\u201d\u548c\u201c\u8d22\u52a1\u4fdd\u62a4\u201d\u5206\u5f00\u8ba8\u8bba\u3002"
      ),
      meta = list("Population-weighted inequality", "Lorenz comparison", "OOPS burden ranking")
    ),
    mod_v3_page_body(
      wide = TRUE,
      mod_v3_badge_row(
        "\u4eba\u53e3\u52a0\u6743",
        "\u591a\u6307\u6570\u5207\u6362",
        "Lorenz \u5e74\u4efd\u5bf9\u7167",
        "\u9ad8 OOPS \u5c3e\u90e8\u8bc6\u522b",
        tone = "warn"
      ),
      mod_v3_story_grid(
        columns = 3,
        mod_v3_insight(
          kicker = "Level",
          title = "Gini \u9002\u5408\u770b\u603b\u4f53\u5206\u5e03",
          text = "Gini \u7ed9\u51fa\u4e00\u4e2a\u7b80\u6d01\u7684\u8de8\u56fd\u79bb\u6563\u5ea6\u4fe1\u53f7\uff0c\u4f46\u5bf9\u6700\u9ad8\u548c\u6700\u4f4e\u7aef\u53d8\u5316\u4e0d\u5982 Theil \u654f\u611f\u3002",
          tone = "primary"
        ),
        mod_v3_insight(
          kicker = "Tail",
          title = "Theil-T \u548c Atkinson \u63a2\u6d4b\u6781\u503c",
          text = "\u5f53 Gini \u4e0b\u884c\u4f46 Atkinson \u4ecd\u9ad8\u65f6\uff0c\u8bf4\u660e\u5c11\u6570\u56fd\u5bb6\u4ecd\u5360\u636e\u8fc7\u5927\u5dee\u8ddd\u3002",
          tone = "secondary"
        ),
        mod_v3_insight(
          kicker = "Protection",
          title = "OOPS \u6392\u884c\u628a\u5206\u5e03\u62c9\u56de\u5bb6\u5ead",
          text = "\u4eba\u5747\u536b\u751f\u652f\u51fa\u66f4\u5e73\u7b49\u5e76\u4e0d\u5fc5\u7136\u610f\u5473\u5bb6\u5ead\u73b0\u91d1\u652f\u4ed8\u538b\u529b\u4e0b\u964d\u3002",
          tone = "bad"
        )
      ),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 292,
          shiny::sliderInput(ns("lorenz_years"), "Lorenz \u5bf9\u6bd4\u5e74\u4efd",
                             min = year_min, max = year_max,
                             value = c(2000, year_max), step = 1, sep = ""),
          shiny::checkboxGroupInput(ns("indices"), "\u663e\u793a\u6307\u6570",
                                    choices = c("Gini", "Theil-T",
                                                 "Atkinson(\u03b5=0.5)",
                                                 "Atkinson(\u03b5=1)",
                                                 "Atkinson(\u03b5=2)"),
                                    selected = c("Gini", "Theil-T",
                                                  "Atkinson(\u03b5=1)")),
          mod_v3_sidebar_note(
            "Reading sequence",
            "\u5148\u770b\u6307\u6570\u662f\u5426\u540c\u5411\u6536\u655b\uff0c\u518d\u7528 Lorenz \u66f2\u7ebf\u786e\u8ba4\u4e0d\u540c\u5e74\u4efd\u7684\u5206\u5e03\u5f62\u72b6\u3002",
            bullets = c("Theil \u66f4\u654f\u611f\u4e8e\u6781\u503c", "Atkinson \u53ef\u8c03\u5bf9\u4f4e\u7aef\u7684\u654f\u611f\u5ea6", "OOPS \u8868\u683c\u7528\u4e8e\u627e\u9ad8\u98ce\u9669\u56fd\u5bb6")
          )
        ),
        shiny::uiOutput(ns("kpi_strip")),
        mod_v3_rail(list(
          list(title = "\u9009\u6307\u6570", text = "\u7528\u591a\u6307\u6570\u68c0\u67e5\u6536\u655b\u662f\u5426\u7a33\u5b9a\u3002"),
          list(title = "\u5bf9\u5e74\u4efd", text = "Lorenz \u66f2\u7ebf\u663e\u793a\u4eba\u53e3\u548c\u652f\u51fa\u7684\u7d2f\u79ef\u4efd\u989d\u3002"),
          list(title = "\u770b\u5c3e\u90e8", text = "OOPS Top \u56fd\u5bb6\u63d0\u793a\u8d22\u52a1\u4fdd\u62a4\u8584\u5f31\u70b9\u3002"),
          list(title = "\u533a\u5206\u542b\u4e49", text = "\u5206\u5e03\u6539\u5584\u548c\u5bb6\u5ead\u4fdd\u62a4\u9700\u5206\u522b\u5224\u65ad\u3002")
        )),
        bslib::layout_columns(
          col_widths = c(7, 5),
          mod_card(
            kicker = "INEQUALITY TREND",
            title = "\u4e0d\u5e73\u7b49\u6307\u6570\u65f6\u5e8f\uff082000-latest\uff09",
            mod_v3_chart_guide(
              "\u8bfb\u56fe\u65b9\u6cd5",
              "\u4e09\u7c7b\u6307\u6570\u82e5\u540c\u6b65\u4e0b\u884c\uff0c\u8bf4\u660e\u8de8\u56fd\u4eba\u5747 CHE \u5206\u5e03\u6536\u655b\uff1b\u82e5\u5206\u5316\uff0c\u5219\u9700\u5173\u6ce8\u6781\u503c\u56fd\u5bb6\u7684\u62c9\u52a8\u3002"
            ),
            mod_spinner(plotly::plotlyOutput(ns("indices_ts"), height = 460))
          ),
          mod_card(
            kicker = "LORENZ SHAPE",
            title = "Lorenz \u66f2\u7ebf\u5bf9\u6bd4\uff08\u4eba\u53e3\u52a0\u6743\uff09",
            mod_v3_chart_guide(
              "\u66f2\u7ebf\u542b\u4e49",
              "\u66f2\u7ebf\u8d8a\u63a5\u8fd1\u5bf9\u89d2\u7ebf\uff0c\u4eba\u5747 CHE \u5206\u5e03\u8d8a\u5747\u8861\uff1b\u4e24\u5e74\u66f2\u7ebf\u7684\u8ddd\u79bb\u53ef\u76f4\u89c2\u5c55\u793a\u5206\u5e03\u6539\u5584\u5e45\u5ea6\u3002",
              tone = "good"
            ),
            mod_spinner(plotly::plotlyOutput(ns("lorenz_plot"), height = 460))
          )
        ),
        mod_card(
          kicker = "HOUSEHOLD BURDEN",
          title = "\u707e\u96be\u6027 OOP \u6392\u884c\uff08\u6700\u65b0\u5e74\uff09",
          mod_v3_chart_guide(
            "\u8868\u683c\u7528\u9014",
            "\u8868\u683c\u4f18\u5148\u5c55\u793a OOPS \u5360 CHE \u6700\u9ad8\u7684\u56fd\u5bb6\uff0c\u5e76\u540c\u65f6\u7ed9\u51fa\u5927\u6d32\u3001\u6536\u5165\u7ec4\u548c\u4eba\u5747 CHE\uff0c\u4fbf\u4e8e\u533a\u5206\u4f4e\u6295\u5165\u548c\u9ad8\u81ea\u4ed8\u4e24\u7c7b\u95ee\u9898\u3002",
            tone = "bad"
          ),
          mod_spinner(reactable::reactableOutput(ns("catastrophic_table"))),
          footer = "\u4e0d\u5e73\u7b49\u6307\u6570\u57fa\u4e8e\u4eba\u5747 CHE\uff1bOOPS \u6392\u884c\u57fa\u4e8e\u6700\u65b0\u5e74\u5c45\u6c11\u73b0\u91d1\u81ea\u4ed8\u5360 CHE \u6bd4\u4f8b\u3002"
        )
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

    output$kpi_strip <- shiny::renderUI({
      d <- ineq_data()
      m <- master_r()
      shiny::req(d, nrow(d) > 0)
      latest <- d[d$year == max(d$year, na.rm = TRUE), , drop = FALSE]
      oops_d <- m[m$year == year_max & is.finite(m$hf3_che), , drop = FALSE]
      mod_v3_kpi_grid(
        mod_v3_kpi("最近年份", as.character(latest$year[1]),
                   hint = "\u4e0d\u5e73\u7b49\u6307\u6570\u53ef\u7528\u7684\u6700\u65b0\u5e74",
                   tone = "neutral"),
        mod_v3_kpi("Gini", fmt_v3_num(latest$gini_pop[1], 3),
                   hint = "\u4eba\u53e3\u52a0\u6743\u8de8\u56fd\u4eba\u5747 CHE \u4e0d\u5e73\u7b49",
                   tone = "primary"),
        mod_v3_kpi("Theil-T", fmt_v3_num(latest$theil_pop[1], 3),
                   hint = "\u5bf9\u5206\u5e03\u5c3e\u90e8\u548c\u6781\u503c\u66f4\u654f\u611f",
                   tone = "secondary"),
        mod_v3_kpi("OOPS P90", fmt_v3_pct(stats::quantile(oops_d$hf3_che, 0.9, na.rm = TRUE), 1),
                   hint = "\u6700\u65b0\u5e74\u5bb6\u5ead\u81ea\u4ed8\u9ad8\u5206\u4f4d\u538b\u529b",
                   tone = "bad")
      )
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
