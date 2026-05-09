# =============================================================================
# 仪表盘/模块/mod_pandemic.R
# Tab 5 · 韧性 ★：COVID 冲击散点 + dumbbell + 韧性排名
# =============================================================================

mod_pandemic_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = htmltools::HTML("&#129516; \u97e7\u6027 Pandemic"),
    htmltools::div(
      class = "panel-hero",
      htmltools::h2("COVID \u51b2\u51fb\u4e0b\u7684\u97e7\u6027\u8bc4\u4ef7"),
      htmltools::p(class = "text-muted",
                   paste("\u5bf9\u6bd4 2019 \u4e0e 2022 \u7684\u4e09\u6e90\u7ed3\u6784\u53d8\u5316\uff1a",
                         "\u5de6\u56fe\u662f \u0394 GGHE-D % vs \u0394 OOPS % \u6563\u70b9\uff0c\u53f3\u56fe\u662f Top N \u53d8\u5316 dumbbell\u3002",
                         "\u4e0b\u65b9\u8868\u683c\u4ee5 \u0394GGHE - \u0394OOPS \u4e3a\u97e7\u6027\u5f97\u5206\uff08\u8d8a\u9ad8\u8d8a\u80fd\u516c\u5171\u8d22\u653f\u6258\u5e95\uff09\u3002"))
    ),
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 280,
        shiny::numericInput(ns("top_n"), "Dumbbell Top N", value = 25,
                             min = 10, max = 60, step = 5),
        shiny::tags$hr(),
        shiny::helpText(
          "比较 ", htmltools::strong("2019 vs 2022"),
          " 的 OOPS 占比变化：",
          htmltools::tags$br(),
          htmltools::tags$span(style = "color: #C46B27", "橙红"),
          " 表示 OOP 恶化（家庭压力上升）；",
          htmltools::tags$br(),
          htmltools::tags$span(style = "color: #6B8E5A", "橄榄"),
          " 表示改善。")
      ),
      bslib::layout_columns(
        col_widths = c(6, 6),
        mod_card(
          title = "COVID 前后 GGHE-D 变化",
          mod_spinner(plotly::plotlyOutput(ns("covid_scatter"), height = 480))
        ),
        mod_card(
          title = "OOPS 变化 dumbbell",
          mod_spinner(plotly::plotlyOutput(ns("dumbbell"), height = 480))
        )
      ),
      mod_card(
        title = "韧性评分（GGHE-D 上升 - OOPS 上升）",
        mod_spinner(reactable::reactableOutput(ns("resilience_table")))
      )
    )
  )
}

mod_pandemic_server <- function(id, master_r) {
  shiny::moduleServer(id, function(input, output, session) {
    covid_panel <- shiny::reactive({
      m <- master_r()
      tryCatch(covid_shock(m), error = function(e) NULL)
    })

    output$covid_scatter <- plotly::renderPlotly({
      d <- covid_panel()
      shiny::req(d, nrow(d) > 0)
      pal <- c(Africa = "#C0504D", Americas = "#1B5E88", Asia = "#E8833C",
               Europe = "#2A9D8F", Oceania = "#7B4B94", Antarctica = "#9C9C9C")
      x_col <- if ("delta_gghe_che" %in% names(d)) "delta_gghe_che"
                else if ("delta_gghed_che" %in% names(d)) "delta_gghed_che"
                else names(d)[grepl("delta", names(d), ignore.case = TRUE)][1]
      y_col <- if ("delta_oop" %in% names(d)) "delta_oop"
                else if ("delta_hf3_che" %in% names(d)) "delta_hf3_che"
                else names(d)[grepl("oop", names(d), ignore.case = TRUE)][1]
      shiny::req(!is.null(x_col), !is.null(y_col))
      d2 <- d[is.finite(d[[x_col]]) & is.finite(d[[y_col]]), , drop = FALSE]
      p <- ggplot2::ggplot(d2, ggplot2::aes(.data[[x_col]], .data[[y_col]],
                                              colour = continent,
                                              text = country_name)) +
        ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                             colour = "#1A1A1F40") +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                             colour = "#1A1A1F40") +
        ggplot2::geom_point(size = 2.6, alpha = 0.85) +
        ggplot2::scale_colour_manual(values = pal, name = NULL) +
        ggplot2::labs(x = "\u0394 GGHE-D %", y = "\u0394 OOPS %") +
        ggplot2::theme_minimal(base_size = 12)
      plotly::ggplotly(p, tooltip = "text") |>
        plotly::config(displaylogo = FALSE)
    })

    output$dumbbell <- plotly::renderPlotly({
      m <- master_r()
      shiny::req(input$top_n)
      d19 <- m[m$year == 2019 &
                is.finite(m$hf3_che),
                c("iso3_code", "country_name", "continent", "hf3_che")]
      d22 <- m[m$year == 2022 &
                is.finite(m$hf3_che),
                c("iso3_code", "hf3_che")]
      names(d19)[4] <- "y2019"; names(d22)[2] <- "y2022"
      d <- merge(d19, d22, by = "iso3_code")
      d$delta <- d$y2022 - d$y2019
      d <- d[order(-abs(d$delta)), ][seq_len(min(input$top_n, nrow(d))), ]
      d$country_name <- factor(d$country_name,
        levels = rev(d$country_name[order(-abs(d$delta))]))
      d$dir <- ifelse(d$delta > 0, "up", "down")
      p <- ggplot2::ggplot(d) +
        ggplot2::geom_segment(ggplot2::aes(x = y2019, xend = y2022,
                                            y = country_name, yend = country_name,
                                            colour = dir), linewidth = 1) +
        ggplot2::geom_point(ggplot2::aes(y2019, country_name),
                             size = 2.6, colour = "#5A5A65") +
        ggplot2::geom_point(ggplot2::aes(y2022, country_name),
                             size = 2.6, colour = "#C46B27") +
        ggplot2::scale_colour_manual(
          values = c(up = "#C46B27", down = "#6B8E5A"),
          labels = c(up = "OOPS \u4e0a\u5347", down = "OOPS \u4e0b\u964d"),
          name = NULL) +
        ggplot2::labs(x = "OOPS / CHE  (%)", y = NULL) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(legend.position = "top")
      plotly::ggplotly(p) |> plotly::config(displaylogo = FALSE)
    })

    output$resilience_table <- reactable::renderReactable({
      m <- master_r()
      d19 <- m[m$year == 2019, c("iso3_code", "country_name", "continent",
                                  "income_group", "gghed_che", "hf3_che")]
      d22 <- m[m$year == 2022, c("iso3_code", "gghed_che", "hf3_che")]
      names(d19)[5:6] <- c("gghed19", "oops19")
      names(d22)[2:3] <- c("gghed22", "oops22")
      d <- merge(d19, d22, by = "iso3_code")
      d$delta_gghe <- d$gghed22 - d$gghed19
      d$delta_oops <- d$oops22  - d$oops19
      d$resilience <- round(d$delta_gghe - d$delta_oops, 2)
      d <- d[order(-d$resilience), ]
      tab <- data.frame(
        Rank        = seq_len(nrow(d)),
        Country     = d$country_name,
        Continent   = d$continent,
        IncomeGroup = d$income_group,
        delta_GGHE  = round(d$delta_gghe, 1),
        delta_OOPS  = round(d$delta_oops, 1),
        Resilience  = d$resilience
      )
      names(tab) <- c("Rank", "Country", "Continent", "IncomeGroup",
                       "\u0394 GGHE-D %", "\u0394 OOPS %", "Resilience")
      reactable::reactable(tab, searchable = TRUE, defaultPageSize = 15,
        pagination = TRUE, highlight = TRUE,
        defaultColDef = reactable::colDef(headerStyle = list(background = "#f1f3f7")),
        columns = list(
          Resilience = reactable::colDef(style = function(value) {
            col <- if (value > 0) "#3F8F4A" else "#C0504D"
            list(color = col, fontWeight = "bold")
          })
        ))
    })
  })
}
