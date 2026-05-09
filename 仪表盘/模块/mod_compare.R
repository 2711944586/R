# =============================================================================
# 仪表盘/模块/mod_compare.R
# Tab 7 · 多国对比：5 国 × 8 指标对比 + 排名表
# =============================================================================

mod_compare_ui <- function(id, country_choices_named, indicator_choices) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = htmltools::HTML("&#128209; \u5bf9\u6bd4 Compare"),
    htmltools::div(
      class = "panel-hero",
      htmltools::h2("\u591a\u56fd\u591a\u6307\u6807\u5728\u7ebf\u5bf9\u6bd4"),
      htmltools::p(class = "text-muted",
                   paste("\u9009\u4efb\u610f N \u4e2a\u56fd\u5bb6 \u00d7 1 \u4e2a\u6307\u6807\uff0c",
                         "\u540c\u65f6\u5f97\u5230\u65f6\u5e8f\u8d70\u52bf\u3001\u6700\u65b0\u5e74\u6392\u540d\u3001",
                         "\u5e74\u521d vs \u5e74\u672b dumbbell \u4e0e\u5e74\u5316\u589e\u901f\u3002",
                         "\u9ed8\u8ba4 5 \u56fd\u5bf9\u6bd4\uff0c\u53ef\u968f\u610f\u6dfb\u51cf\u3002"))
    ),
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 300,
        shinyWidgets::pickerInput(
          ns("isos"), "\u9009\u62e9\u56fd\u5bb6\uff08\u591a\u9009\uff09",
          choices  = country_choices_named,
          selected = c("CHN", "USA", "IND", "BRA", "ZAF"),
          multiple = TRUE,
          options  = list(`actions-box` = TRUE,
                          `live-search` = TRUE,
                          `selected-text-format` = "count > 3")),
        shiny::selectInput(ns("indicator"), "\u6307\u6807",
                           choices = indicator_choices,
                           selected = "che_pc_usd2023"),
        shiny::radioButtons(ns("log"), "Y \u8f74\u5c3a\u5ea6",
                            choices = c("\u7ebf\u6027" = "linear",
                                         "\u5bf9\u6570" = "log"),
                            selected = "linear", inline = TRUE),
        shiny::sliderInput(ns("year_a"), "\u5bf9\u6bd4\u8d77\u70b9\u5e74",
                           min = 2000, max = 2020, value = 2000, step = 1,
                           sep = ""),
        shiny::sliderInput(ns("year_b"), "\u5bf9\u6bd4\u7ec8\u70b9\u5e74",
                           min = 2005, max = 2023, value = 2023, step = 1,
                           sep = "")
      ),
      bslib::layout_columns(
        col_widths = c(7, 5),
        mod_card(
          title = "\u65f6\u5e8f\u5bf9\u6bd4",
          htmltools::p(class = "card-note",
                       paste("\u9ad8\u4eae = \u5f53\u524d\u9009\u4e2d\u56fd\uff0c",
                             "\u7070\u8272\u80cc\u666f = \u5168\u90e8\u56fd\u5bb6\u3002",
                             "\u67d0\u4e9b\u6307\u6807\u5bf9\u6570\u5c3a\u5ea6\u4e0b\u53ef\u8bc6\u522b\u9ad8\u8fb9\u9645\u53d8\u5316\u3002")),
          mod_spinner(plotly::plotlyOutput(ns("lines"), height = 420))
        ),
        mod_card(
          title = "\u6700\u65b0\u5e74\u6392\u540d",
          htmltools::p(class = "card-note",
                       paste("\u5168\u4e16\u754c\u6309\u9009\u4e2d\u6307\u6807\u964d\u5e8f\u3002",
                             "\u53ef\u641c\u7d22\u5feb\u901f\u5b9a\u4f4d\u67d0\u56fd\u3002")),
          mod_spinner(reactable::reactableOutput(ns("rank_table")))
        )
      ),
      bslib::layout_columns(
        col_widths = c(7, 5),
        mod_card(
          title = "\u5e74\u521d vs \u5e74\u672b dumbbell",
          htmltools::p(class = "card-note",
                       paste("\u8fde\u7ebf\u4e24\u70b9\u5206\u522b\u662f\u8d77\u70b9\u5e74\u4e0e\u7ec8\u70b9\u5e74\u7684\u6307\u6807\u503c\u3002",
                             "\u7eff\u8272 = \u4e0a\u5347\u3001\u6a59\u8272 = \u4e0b\u964d\u3002",
                             "\u9ed8\u8ba4\u4e0a\u4e0b 6 \u56fd\u4ee5\u5185\u3002")),
          mod_spinner(plotly::plotlyOutput(ns("dumbbell"), height = 360))
        ),
        mod_card(
          title = "\u5e74\u5316\u589e\u901f CAGR",
          htmltools::p(class = "card-note",
                       paste("CAGR = (\u672b\u503c/\u521d\u503c)^(1/n) - 1\uff0c",
                             "\u4ee3\u8868\u5728\u9009\u5b9a\u533a\u95f4\u5185\u7684\u5e74\u5316\u53d8\u5316\u901f\u5ea6\u3002",
                             "\u6839\u636e\u8d77\u70b9 / \u7ec8\u70b9\u5747\u9700>0\u3002")),
          mod_spinner(plotly::plotlyOutput(ns("cagr"), height = 360))
        )
      )
    )
  )
}

mod_compare_server <- function(id, master_r, year_max) {
  shiny::moduleServer(id, function(input, output, session) {
    bounds <- shiny::reactive({
      m <- master_r()
      list(min = min(m$year, na.rm = TRUE),
           max = max(m$year, na.rm = TRUE))
    })

    shiny::observe({
      b <- bounds()
      shiny::updateSliderInput(session, "year_a",
                                min = b$min, max = b$max - 1,
                                value = b$min)
      shiny::updateSliderInput(session, "year_b",
                                min = b$min + 1, max = b$max,
                                value = b$max)
    })

    output$lines <- plotly::renderPlotly({
      shiny::req(input$isos, input$indicator)
      m <- master_r()
      safe_plotly({
        p <- highlight_country_ts(m, isos = input$isos,
                                   value_col = input$indicator,
                                   title = sprintf("%s \u8d70\u52bf\u5bf9\u6bd4",
                                                   input$indicator))
        if (input$log == "log") p <- p |>
          plotly::layout(yaxis = list(type = "log"))
        p
      })
    })

    output$rank_table <- reactable::renderReactable({
      shiny::req(input$indicator)
      m <- master_r()
      val <- input$indicator
      d <- m[m$year == year_max & is.finite(m[[val]]), , drop = FALSE]
      d <- d[order(-d[[val]]), , drop = FALSE]
      d <- data.frame(
        Rank        = seq_len(nrow(d)),
        Country     = d$country_name,
        Continent   = d$continent,
        IncomeGroup = d$income_group,
        Value       = round(d[[val]], 2)
      )
      reactable::reactable(d, searchable = TRUE, defaultPageSize = 15,
        pagination = TRUE, highlight = TRUE,
        defaultColDef = reactable::colDef(
          headerStyle = list(background = "#f1f3f7")))
    })

    output$dumbbell <- plotly::renderPlotly({
      shiny::req(input$isos, input$indicator,
                 input$year_a, input$year_b,
                 input$year_a < input$year_b)
      m <- master_r()
      val <- input$indicator
      d <- m[m$iso3_code %in% input$isos &
              m$year %in% c(input$year_a, input$year_b) &
              is.finite(m[[val]]), c("iso3_code", "country_name",
                                       "year", val), drop = FALSE]
      shiny::req(nrow(d) > 0)
      colnames(d)[4] <- "value"
      d_a <- d[d$year == input$year_a, c("iso3_code", "country_name", "value")]
      d_b <- d[d$year == input$year_b, c("iso3_code", "country_name", "value")]
      names(d_a)[3] <- "v_a"; names(d_b)[3] <- "v_b"
      dd <- merge(d_a, d_b, by = c("iso3_code", "country_name"))
      shiny::req(nrow(dd) > 0)
      dd$delta <- dd$v_b - dd$v_a
      dd$dir <- ifelse(dd$delta >= 0, "rise", "fall")
      dd <- dd[order(dd$v_b), ]
      dd$country_name <- factor(dd$country_name, levels = dd$country_name)
      pal <- c(rise = "#6B8E5A", fall = "#C46B27")
      p <- ggplot2::ggplot(dd) +
        ggplot2::geom_segment(ggplot2::aes(x = v_a, xend = v_b,
                                            y = country_name,
                                            yend = country_name,
                                            color = dir),
                               linewidth = 1.6) +
        ggplot2::geom_point(ggplot2::aes(x = v_a, y = country_name),
                             color = "#5A5A65", size = 3) +
        ggplot2::geom_point(ggplot2::aes(x = v_b, y = country_name,
                                          color = dir), size = 3.4) +
        ggplot2::scale_color_manual(values = pal, guide = "none") +
        ggplot2::labs(x = sprintf("%d \u2192 %d (%s)",
                                   input$year_a, input$year_b, val),
                      y = NULL) +
        ggplot2::theme_minimal(base_size = 12)
      plotly::ggplotly(p) |> plotly::config(displaylogo = FALSE)
    })

    output$cagr <- plotly::renderPlotly({
      shiny::req(input$isos, input$indicator,
                 input$year_a, input$year_b,
                 input$year_a < input$year_b)
      m <- master_r()
      val <- input$indicator
      d <- m[m$iso3_code %in% input$isos &
              m$year %in% c(input$year_a, input$year_b) &
              is.finite(m[[val]]), c("iso3_code", "country_name",
                                       "year", val), drop = FALSE]
      colnames(d)[4] <- "value"
      d_a <- d[d$year == input$year_a, c("iso3_code", "country_name", "value")]
      d_b <- d[d$year == input$year_b, c("iso3_code", "country_name", "value")]
      names(d_a)[3] <- "v_a"; names(d_b)[3] <- "v_b"
      dd <- merge(d_a, d_b, by = c("iso3_code", "country_name"))
      dd <- dd[is.finite(dd$v_a) & dd$v_a > 0 &
                is.finite(dd$v_b) & dd$v_b > 0, , drop = FALSE]
      shiny::req(nrow(dd) > 0)
      n <- max(input$year_b - input$year_a, 1)
      dd$cagr <- (dd$v_b / dd$v_a)^(1 / n) - 1
      dd <- dd[order(dd$cagr), ]
      dd$country_name <- factor(dd$country_name, levels = dd$country_name)
      p <- ggplot2::ggplot(dd, ggplot2::aes(cagr, country_name,
                                              fill = cagr >= 0)) +
        ggplot2::geom_col() +
        ggplot2::geom_vline(xintercept = 0, linewidth = 0.5,
                             color = "#1A1A1F33") +
        ggplot2::scale_fill_manual(
          values = c("TRUE" = "#1B5E88", "FALSE" = "#C46B27"),
          guide = "none") +
        ggplot2::scale_x_continuous(
          labels = scales::percent_format(accuracy = 0.1)) +
        ggplot2::labs(x = sprintf("%d\u2013%d \u5e74\u5316\u589e\u901f",
                                   input$year_a, input$year_b),
                      y = NULL) +
        ggplot2::theme_minimal(base_size = 12)
      plotly::ggplotly(p) |> plotly::config(displaylogo = FALSE)
    })
  })
}
