# =============================================================================
# 仪表盘/模块/mod_compare.R
# Tab 7 · 多国对比：5 国 × 8 指标对比 + 排名表
# =============================================================================

mod_compare_ui <- function(id, country_choices_named, indicator_choices) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = htmltools::HTML("&#128209; 对比 Compare"),
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 280,
        shinyWidgets::pickerInput(
          ns("isos"), "选择国家（多选）",
          choices  = country_choices_named,
          selected = c("CHN", "USA", "IND", "BRA", "ZAF"),
          multiple = TRUE,
          options  = list(`actions-box` = TRUE,
                          `live-search` = TRUE,
                          `selected-text-format` = "count > 3")),
        shiny::selectInput(ns("indicator"), "指标",
                           choices = indicator_choices,
                           selected = "che_pc_usd2023"),
        shiny::radioButtons(ns("log"), "Y 轴尺度",
                            choices = c("线性" = "linear", "对数" = "log"),
                            selected = "linear", inline = TRUE)
      ),
      mod_card(
        title = "时序对比",
        mod_spinner(plotly::plotlyOutput(ns("lines"), height = 480))
      ),
      mod_card(
        title = "最新年排名",
        mod_spinner(reactable::reactableOutput(ns("rank_table")))
      )
    )
  )
}

mod_compare_server <- function(id, master_r, year_max) {
  shiny::moduleServer(id, function(input, output, session) {
    output$lines <- plotly::renderPlotly({
      shiny::req(input$isos, input$indicator)
      m <- master_r()
      safe_plotly({
        p <- highlight_country_ts(m, isos = input$isos,
                                   value_col = input$indicator,
                                   title = sprintf("%s 走势对比", input$indicator))
        if (input$log == "log") p <- p |> plotly::layout(yaxis = list(type = "log"))
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
        defaultColDef = reactable::colDef(headerStyle = list(background = "#f1f3f7")))
    })
  })
}
