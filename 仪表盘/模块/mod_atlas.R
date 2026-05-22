# =============================================================================
# 仪表盘/模块/mod_atlas.R
# Tab 11 · Atlas：DT 全字段 + 多格式下载（CSV/Parquet/RDS）
# =============================================================================

mod_atlas_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = "Atlas",
    value = "atlas",
    mod_v3_hero(
      kicker = "\u6570\u636e\u56fe\u8c31",
      title = "全球 Atlas · 数据资产与导出中枢",
      lead = paste(
        "把 Shiny 中使用的完整国家年度宽表、不平等年度面板和 COVID 冲击表集中到一个可检索的数据台账。",
        "这里不只是下载入口，也用于核对字段、覆盖范围、变量类型和可复现导出口径。"
      ),
      meta = list("Master 宽表", "派生面板", "CSV / Parquet / RDS")
    ),
    mod_v3_page_body(
      wide = TRUE,
      mod_v3_badge_row(
        "国家年度主键：ISO3 + year",
        "前 200 行交互预览",
        "字段库存与覆盖率",
        "多格式导出",
        tone = "secondary"
      ),
      mod_v3_story_grid(
        columns = 3,
        mod_v3_insight(
          kicker = "\u5b57\u6bb5\u76d8\u70b9",
          title = "把变量先变成可审计资产",
          text = "每个数据集都会同步展示字段类型、非缺失记录数和覆盖比例，避免只下载文件而不了解字段质量。",
          tone = "primary",
          icon = "A"
        ),
        mod_v3_insight(
          kicker = "\u7edf\u4e00\u5bfc\u51fa",
          title = "同一口径服务 Shiny 与静态报告",
          text = "Master、inequality 和 COVID 三类表与分析模块共用函数生成，导出的数据就是图表和模型正在读取的数据。",
          tone = "secondary",
          icon = "D"
        ),
        mod_v3_insight(
          kicker = "\u4e0b\u8f7d\u524d\u590d\u6838",
          title = "先预览，再下载",
          text = "预览表保留搜索、排序、分页能力，适合快速检查某个国家、年份或变量是否按预期进入当前数据集。",
          tone = "good",
          icon = "Q"
        )
      ),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 300,
          shiny::radioButtons(ns("dataset"), "选择数据集",
            choices = c(
              "Master 完整宽表" = "master",
              "不平等年度面板" = "inequality",
              "COVID 冲击表" = "covid"
            )
          ),
          shiny::selectInput(ns("format"), "下载格式",
            choices = c(
              "CSV (.csv)" = "csv",
              "Parquet (.parquet)" = "parquet",
              "RDS (.rds)" = "rds"
            )
          ),
          shiny::downloadButton(ns("download"), "下载当前数据集",
            class = "btn-primary"),
          mod_v3_sidebar_note(
            title = "导出说明",
            text = "CSV 便于跨软件查看，RDS 保留 R 类型信息，Parquet 适合列式存储和大数据流程。",
            bullets = c(
              "文件名自动包含数据集名称与导出日期。",
              "Parquet 需要本地安装 arrow 包。",
              "字段库存表可用于下载前的质量核对。"
            )
          )
        ),
        shiny::uiOutput(ns("asset_strip")),
        mod_v3_rail(list(
          list(title = "选择数据集", text = "切换 master、年度不平等面板或疫情冲击数据。"),
          list(title = "核对字段", text = "查看字段类型、非缺失记录与覆盖比例。"),
          list(title = "预览记录", text = "用搜索和排序定位国家、年份或异常值。"),
          list(title = "导出复用", text = "下载后可直接进入课程报告、复现脚本或二次建模。")
        )),
        bslib::layout_columns(
          col_widths = c(8, 4),
          mod_card(
            kicker = "\u6570\u636e\u9884\u89c8",
            title = "数据预览（前 200 行 · 可搜索 · 可排序）",
            mod_v3_chart_guide(
              title = "如何检查",
              text = "先用搜索框定位国家或变量，再按关键字段排序；预览限制在前 200 行以保持界面响应。",
              bullets = c(
                "Master 表适合核对国家年度主记录。",
                "不平等面板适合复核年度指标口径。",
                "COVID 表适合查看疫情前后冲击变量。"
              )
            ),
            mod_spinner(reactable::reactableOutput(ns("preview"))),
            footer = "预览不会改变下载内容；导出文件包含当前数据集的完整记录。"
          ),
          mod_card(
            kicker = "\u5b57\u6bb5\u5e93\u5b58",
            title = "字段库存与覆盖率",
            mod_v3_chart_guide(
              title = "读表提示",
              text = "覆盖率按当前数据集全量行数计算，字符字段使用非空值，数值字段使用有限值。",
              tone = "good"
            ),
            mod_spinner(reactable::reactableOutput(ns("field_inventory"))),
            footer = "低覆盖字段更适合作为解释性线索，而不是单独支撑强结论。"
          )
        )
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

    dataset_label <- shiny::reactive({
      switch(input$dataset,
        master = "Master 完整宽表",
        inequality = "不平等年度面板",
        covid = "COVID 冲击表",
        "当前数据集"
      )
    })

    non_missing <- function(x) {
      if (is.numeric(x)) {
        is.finite(x)
      } else if (is.character(x)) {
        !is.na(x) & nzchar(x)
      } else {
        !is.na(x)
      }
    }

    output$asset_strip <- shiny::renderUI({
      d <- dl_data()
      years <- if ("year" %in% names(d)) {
        rng <- range(d$year, na.rm = TRUE)
        if (all(is.finite(rng))) sprintf("%d-%d", rng[1], rng[2]) else "NA"
      } else {
        "NA"
      }
      countries <- if ("iso3_code" %in% names(d)) {
        length(unique(d$iso3_code[!is.na(d$iso3_code)]))
      } else if ("country_name" %in% names(d)) {
        length(unique(d$country_name[!is.na(d$country_name)]))
      } else {
        NA_integer_
      }
      mod_v3_kpi_grid(
        mod_v3_kpi("当前数据集", dataset_label(), hint = "侧栏可切换", tone = "primary"),
        mod_v3_kpi("记录数", format(nrow(d), big.mark = ","), hint = "下载包含全量行", tone = "secondary"),
        mod_v3_kpi("字段数", format(ncol(d), big.mark = ","), hint = "含派生指标", tone = "good"),
        mod_v3_kpi("国家 / 年份", sprintf("%s · %s",
          if (is.finite(countries)) format(countries, big.mark = ",") else "NA",
          years), hint = "由可用字段自动识别", tone = "warn")
      )
    })

    output$preview <- reactable::renderReactable({
      d <- dl_data()
      reactable::reactable(utils::head(d, 200),
        searchable = TRUE, pagination = TRUE,
        defaultPageSize = 15, highlight = TRUE,
        defaultColDef = reactable::colDef(maxWidth = 160,
          headerStyle = list(background = "#f1f3f7")))
    })

    output$field_inventory <- reactable::renderReactable({
      d <- dl_data()
      total <- max(nrow(d), 1)
      inventory <- data.frame(
        `字段` = names(d),
        `类型` = vapply(d, function(x) class(x)[1], character(1)),
        `非缺失记录` = vapply(d, function(x) sum(non_missing(x)), numeric(1)),
        `覆盖率(%)` = vapply(d, function(x) round(sum(non_missing(x)) / total * 100, 1), numeric(1)),
        check.names = FALSE
      )
      inventory <- inventory[order(-inventory$`覆盖率(%)`, inventory$字段), ]
      reactable::reactable(inventory,
        searchable = TRUE, pagination = TRUE, defaultPageSize = 12,
        highlight = TRUE,
        defaultColDef = reactable::colDef(headerStyle = list(background = "#f1f3f7")),
        columns = list(
          `字段` = reactable::colDef(minWidth = 150,
            style = list(fontFamily = "'JetBrains Mono', monospace", fontWeight = 600)),
          `类型` = reactable::colDef(width = 90),
          `非缺失记录` = reactable::colDef(width = 110, align = "right",
            format = reactable::colFormat(separators = TRUE)),
          `覆盖率(%)` = reactable::colDef(width = 110, align = "right")
        ))
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
