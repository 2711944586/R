# =============================================================================
# 仪表盘/模块/mod_atlas.R
# Tab 11 · Atlas：DT 全字段 + 多格式下载（CSV/Parquet/RDS）
# =============================================================================

mod_atlas_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = htmltools::HTML("&#128218; Atlas"),
    value = "atlas",
    htmltools::div(
      class = "panel-hero",
      htmltools::h2("\u5168\u7403 Atlas \u00b7 \u7a7a\u95f4\u89c6\u89d2"),
      htmltools::p(class = "text-muted",
                   paste("\u4ee5 choropleth / \u53cc\u53d8\u91cf\u5730\u56fe\u5de1\u89c8 OOPS \u00b7 GGHE-D \u00b7 EXT \u00b7 CHE \u7684\u5168\u7403\u5206\u5e03\u3002",
                         "\u70b9\u51fb\u56fd\u5bb6\u5373\u53ef\u67e5\u770b\u5173\u952e\u6307\u6807\u5361\u7247\u3002",
                         "\u6240\u6709\u5730\u56fe\u9762\u79ef\u4e0d\u4ee3\u8868\u4eba\u53e3\u89c4\u6a21\uff0c\u8bf7\u7ed3\u5408\u53f3\u4fa7\u56fd\u5bb6\u8868\u3002"))
    ),
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 280,
        shiny::radioButtons(ns("dataset"), "选择数据集",
                             choices = c(
                               "Master 完整宽表"     = "master",
                               "不平等面板"          = "inequality",
                               "COVID 冲击"          = "covid"
                             )),
        shiny::selectInput(ns("format"), "下载格式",
                            choices = c("CSV (.csv)" = "csv",
                                        "Parquet (.parquet)" = "parquet",
                                        "RDS (.rds)" = "rds")),
        shiny::downloadButton(ns("download"), "下载",
                               class = "btn-primary"),
        shiny::tags$hr(),
        shiny::helpText("Atlas 涵盖全部 master 字段 (~30 列) 和派生指标。")
      ),
      mod_card(
        title = "数据预览（前 200 行 · 可搜索 · 可排序）",
        mod_spinner(reactable::reactableOutput(ns("preview")))
      )
    )
  )
}

mod_atlas_server <- function(id, master_r) {
  shiny::moduleServer(id, function(input, output, session) {
    dl_data <- shiny::reactive({
      m <- master_r()
      switch(input$dataset,
              master     = m,
              inequality = inequality_by_year(m),
              covid      = covid_shock(m))
    })

    output$preview <- reactable::renderReactable({
      d <- dl_data()
      reactable::reactable(utils::head(d, 200),
        searchable = TRUE, pagination = TRUE,
        defaultPageSize = 15, highlight = TRUE,
        defaultColDef = reactable::colDef(maxWidth = 160,
          headerStyle = list(background = "#f1f3f7")))
    })

    output$download <- shiny::downloadHandler(
      filename = function() {
        sprintf("ghed_%s_%s.%s",
                input$dataset, format(Sys.Date(), "%Y%m%d"),
                if (input$format == "rds") "rds"
                else if (input$format == "parquet") "parquet"
                else "csv")
      },
      content = function(file) {
        d <- dl_data()
        switch(input$format,
                csv = readr::write_csv(d, file),
                parquet = if (requireNamespace("arrow", quietly = TRUE)) {
                  arrow::write_parquet(d, file)
                } else readr::write_csv(d, file),
                rds = saveRDS(d, file))
      }
    )
  })
}
