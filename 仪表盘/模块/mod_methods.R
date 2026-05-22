# =============================================================================
# 仪表盘/模块/mod_methods.R
# 方法手册嵌入版：数据来源、统计方法、变量字典、复现命令
# =============================================================================

mod_methods_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = "\u65b9\u6cd5 Methods",
    value = "methods",
    mod_v3_hero(
      kicker = "\u65b9\u6cd5\u624b\u518c",
      title = "\u65b9\u6cd5\u4e0e\u6570\u636e\u8bf4\u660e",
      lead = paste(
        "\u672c\u9879\u76ee\u5c06 WHO GHED \u536b\u751f\u652f\u51fa\u8868\u3001WDI \u5b8f\u89c2\u6307\u6807\u548c\u591a\u4e2a\u8f85\u52a9\u6570\u636e\u6e90",
        "\u6574\u5408\u4e3a country-year \u5bbd\u8868\uff0c\u518d\u5728\u7edf\u4e00\u53e3\u5f84\u4e0b\u8fdb\u884c\u4e0d\u5e73\u7b49\u3001\u6536\u655b\u3001",
        "\u805a\u7c7b\u3001\u9884\u6d4b\u3001\u7a33\u5065\u6027\u548c\u653f\u7b56\u60c5\u666f\u63a8\u6f14\u3002"
      ),
      meta = list(
        "WHO GHED 2024-12",
        "WDI 2024-10",
        "195 \u4e2a\u56fd\u5bb6 \u00b7 2000\u20132023"
      )
    ),
    htmltools::div(
      style = "max-width: 1000px; margin: 0 auto; padding: 0 24px;",
      mod_v3_kpi_grid(
        mod_v3_kpi("面板范围", "2000-2023", hint = "以年度为最小时间单位"),
        mod_v3_kpi("核心主键", "ISO3 + year", hint = "跨源合并与质量检查使用",
                   tone = "secondary"),
        mod_v3_kpi("方法模块", "8 类", hint = "描述、分解、预测、情景、稳健性",
                   tone = "good"),
        mod_v3_kpi("输出层", "图表 + 表格", hint = "Shiny 与静态报告共享口径",
                   tone = "warn")
      ),
      mod_v3_story_grid(
        columns = 3,
        mod_v3_insight(
          kicker = "\u8bbe\u8ba1\u539f\u5219",
          title = "\u5148\u7edf\u4e00\u53e3\u5f84\uff0c\u518d\u505a\u6a21\u578b",
          text = "\u6240\u6709\u7edf\u8ba1\u91cf\u90fd\u5148\u5728 master_enriched \u5bbd\u8868\u4e2d\u786e\u5b9a\u5355\u4f4d\u3001\u5e74\u4efd\u3001\u5206\u7ec4\u548c\u7f3a\u5931\u89c4\u5219\u3002",
          tone = "primary"
        ),
        mod_v3_insight(
          kicker = "\u89e3\u91ca\u539f\u5219",
          title = "\u533a\u5206\u63cf\u8ff0\u4e0e\u56e0\u679c",
          text = "\u672c\u4eea\u8868\u76d8\u4e2d\u7684\u5f39\u6027\u3001\u6b8b\u5dee\u548c\u60c5\u666f\u7ed3\u679c\u662f\u53ef\u89e3\u91ca\u7684\u4fe1\u53f7\uff0c\u4e0d\u76f4\u63a5\u7b49\u540c\u4e8e\u56e0\u679c\u6548\u5e94\u3002",
          tone = "secondary"
        ),
        mod_v3_insight(
          kicker = "\u590d\u73b0\u6027",
          title = "\u811a\u672c\u5316\u590d\u73b0",
          text = "\u6570\u636e\u6784\u5efa\u3001\u7279\u5f81\u5de5\u7a0b\u3001\u56fe\u8868\u8f93\u51fa\u548c Shiny \u542f\u52a8\u5747\u4fdd\u7559\u547d\u4ee4\u5165\u53e3\u3002",
          tone = "good"
        )
      ),

      # ---- 数据来源 ----
      mod_v3_card(
        kicker = "\u6570\u636e\u6765\u6e90",
        title = "\u6570\u636e\u6765\u6e90\u4e0e\u7528\u9014",
        htmltools::tags$table(
          class = "table table-sm",
          htmltools::tags$thead(htmltools::tags$tr(
            htmltools::tags$th("\u6570\u636e\u96c6"),
            htmltools::tags$th("\u6765\u6e90"),
            htmltools::tags$th("\u65f6\u6bb5"),
            htmltools::tags$th("\u7528\u9014")
          )),
          htmltools::tags$tbody(
            htmltools::tags$tr(
              htmltools::tags$td("financing_schemes.csv"),
              htmltools::tags$td("WHO GHED via TidyTuesday 2026-04-21"),
              htmltools::tags$td("2000\u20132023"),
              htmltools::tags$td("\u7b79\u8d44\u65b9\u6848\u5206\u7c7b (HF1\u2013HF4)")
            ),
            htmltools::tags$tr(
              htmltools::tags$td("health_spending.csv"),
              htmltools::tags$td("WHO GHED"),
              htmltools::tags$td("2000\u20132023"),
              htmltools::tags$td("\u652f\u51fa\u6765\u6e90\u4e0e\u603b\u91cf (CHE/GGHED/PVTD/EXT)")
            ),
            htmltools::tags$tr(
              htmltools::tags$td("spending_purpose.csv"),
              htmltools::tags$td("WHO GHED"),
              htmltools::tags$td("2000\u20132023"),
              htmltools::tags$td("\u652f\u51fa\u7528\u9014 (HC1\u2013HC9)")
            ),
            htmltools::tags$tr(
              htmltools::tags$td("WDI"),
              htmltools::tags$td("World Bank"),
              htmltools::tags$td("2000\u20132023"),
              htmltools::tags$td("\u4eba\u53e3\u3001GDP\u3001\u5bff\u547d\u3001U5MR")
            ),
            htmltools::tags$tr(
              htmltools::tags$td("GHO"),
              htmltools::tags$td("WHO"),
              htmltools::tags$td("\u591a\u5e74"),
              htmltools::tags$td("HALE\u3001UHC SCI\u3001\u533b\u5e08\u5bc6\u5ea6")
            )
          )
        ),
        mod_v3_source_note(
          title = "\u53e3\u5f84",
          "GHED \u4e2d\u7684\u5360\u6bd4\u6307\u6807\u4ee5 CHE \u4e3a\u5206\u6bcd\uff1bUSD \u7c7b\u6307\u6807\u5728\u9879\u76ee\u5185\u7edf\u4e00\u8f6c\u4e3a 2023 \u4e0d\u53d8\u4ef7\u3002"
        )
      ),

      # ---- 统计方法 ----
      mod_v3_card(
        kicker = "\u7edf\u8ba1\u65b9\u6cd5",
        title = "\u7edf\u8ba1\u65b9\u6cd5\u7ba1\u7ebf",
        mod_v3_steps(list(
          list(title = "\u6570\u636e\u6e05\u7406\u4e0e\u5bbd\u8868\u5316",
               body = "GHED \u4e09\u5f20\u4e3b\u8868\u6309 ISO3-year \u5bf9\u9f50\uff0c\u4e0e WDI/GHO/IMF/OECD/IHME \u8f85\u52a9\u6307\u6807\u5408\u5e76\u3002"),
          list(title = "\u4e0d\u5e73\u7b49\u5ea6\u91cf",
               body = "Gini\u3001Theil-T \u548c Atkinson \u6309\u4eba\u53e3\u6743\u91cd\u8ba1\u7b97\uff0cTheil \u53ef\u8fdb\u4e00\u6b65\u5206\u89e3\u7ec4\u95f4/\u7ec4\u5185\u8d21\u732e\u3002"),
          list(title = "\u6536\u655b\u4e0e\u589e\u957f",
               body = "\u03b2-\u6536\u655b\u4f7f\u7528\u521d\u59cb\u6c34\u5e73\u89e3\u91ca\u540e\u7eed\u589e\u901f\uff0c\u03c3-\u6536\u655b\u8ddf\u8e2a\u8de8\u56fd\u5206\u5e03\u79bb\u6563\u5ea6\u3002"),
          list(title = "\u805a\u7c7b\u4e0e\u8c61\u9650",
               body = "\u5bf9 GGHED\u3001PVT-D\u3001EXT\u3001OOPS \u7b49\u7b79\u8d44\u7ef4\u5ea6\u6807\u51c6\u5316\u540e\u8fdb\u884c PCA \u548c k-means\u3002"),
          list(title = "\u9884\u6d4b\u4e0e\u60c5\u666f",
               body = "ARIMA/ETS \u7528\u4e8e\u5355\u56fd\u65f6\u5e8f\u9884\u6d4b\uff0c\u8499\u7279\u5361\u6d1b\u6247\u5f62\u56fe\u7528\u5386\u53f2\u6b8b\u5dee\u8868\u8fbe\u4e0d\u786e\u5b9a\u6027\u3002"),
          list(title = "\u7a33\u5065\u6027\u68c0\u67e5",
               body = "Bootstrap\u3001Jackknife\u3001\u95e8\u69db\u654f\u611f\u6027\u548c\u7f3a\u5931\u8986\u76d6\u68c0\u67e5\u7528\u4e8e\u8bc4\u4f30\u7ed3\u8bba\u662f\u5426\u53d7\u5c11\u6570\u89c2\u6d4b\u9a71\u52a8\u3002")
        ))
      ),

      # ---- 变量字典 ----
      mod_v3_card(
        kicker = "\u53d8\u91cf\u5b57\u5178",
        title = "\u6838\u5fc3\u53d8\u91cf\u5b57\u5178",
        htmltools::p(
          class = "card-note",
          "\u8868\u4e2d\u53d8\u91cf\u662f Shiny \u6a21\u5757\u548c\u9759\u6001\u62a5\u544a\u5171\u7528\u7684\u6700\u5c0f\u53e3\u5f84\u96c6\uff1b\u66f4\u591a\u6d3e\u751f\u6307\u6807\u5728\u5404\u4e13\u9898\u6a21\u5757\u5185\u5373\u65f6\u8ba1\u7b97\u3002"
        ),
        reactable::reactableOutput(ns("codebook_table"))
      ),

      # ---- 复现命令 ----
      mod_v3_card(
        kicker = "\u590d\u73b0\u547d\u4ee4",
        title = "\u590d\u73b0\u547d\u4ee4",
        mod_v3_code_block(
          paste(
          "# \u5b89\u88c5\u4f9d\u8d56",
          "Rscript \u5b89\u88c5\u4f9d\u8d56.R",
          "",
          "# \u6570\u636e\u7f13\u5b58",
          "Rscript \u6784\u5efa.R data",
          "",
          "# \u7279\u5f81\u5de5\u7a0b",
          "Rscript \u6784\u5efa.R features",
          "",
          "# \u9759\u6001\u56fe",
          "Rscript \u6784\u5efa.R figures",
          "",
          "# \u4ea4\u4e92\u7ec4\u4ef6",
          "Rscript \u6784\u5efa.R widgets",
          "",
          "# \u6a21\u578b",
          "Rscript \u6784\u5efa.R models",
          "",
          "# \u8bfe\u7a0b HTML",
          "Rscript \u6784\u5efa.R submission",
          "",
          "# \u542f\u52a8 Shiny",
          "Rscript \u542f\u52a8\u4eea\u8868\u76d8.R 4848",
          sep = "\n"),
          title = "R \u547d\u4ee4\u884c"
        )
      ),

      # ---- 引用 ----
      mod_v3_card(
        kicker = "\u5f15\u7528",
        title = "\u5f15\u7528",
        htmltools::tags$ul(
          style = "line-height: 1.8;",
          htmltools::tags$li("WHO (2024). Global Health Expenditure Database. Geneva: World Health Organization."),
          htmltools::tags$li("World Bank (2024). World Development Indicators. Washington, DC."),
          htmltools::tags$li("TidyTuesday (2026-04-21). Global Health Spending dataset."),
          htmltools::tags$li("R Core Team (2025). R: A Language and Environment for Statistical Computing."),
          htmltools::tags$li("Wickham H et al. (2019). Welcome to the tidyverse. JOSS, 4(43), 1686.")
        )
      )
    )
  )
}

mod_methods_server <- function(id, master_r = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    output$codebook_table <- reactable::renderReactable({
      codebook <- data.frame(
        variable = c("iso3_code", "year", "che_pc_usd2023", "gghed_che",
                     "pvtd_che", "ext_che", "hf3_che", "life_exp", "u5mr",
                     "gdp_pc_usd", "pop", "continent", "income_group"),
        definition = c(
          "ISO 3166-1 \u4e09\u5b57\u6bcd\u56fd\u5bb6\u4ee3\u7801",
          "\u5e74\u4efd (2000\u20132023)",
          "\u4eba\u5747\u536b\u751f\u603b\u652f\u51fa (USD 2023 \u4e0d\u53d8\u4ef7)",
          "\u56fd\u5185\u653f\u5e9c\u536b\u751f\u652f\u51fa\u5360 CHE (%)",
          "\u56fd\u5185\u79c1\u4eba\u536b\u751f\u652f\u51fa\u5360 CHE (%)",
          "\u5916\u90e8\u63f4\u52a9\u536b\u751f\u652f\u51fa\u5360 CHE (%)",
          "\u5c45\u6c11\u81ea\u4ed8 (OOPS) \u5360 CHE (%)",
          "\u51fa\u751f\u65f6\u9884\u671f\u5bff\u547d (\u5c81)",
          "\u4e94\u5c81\u4ee5\u4e0b\u6b7b\u4ea1\u7387 (\u2030)",
          "\u4eba\u5747 GDP (USD \u4e0d\u53d8\u4ef7)",
          "\u603b\u4eba\u53e3",
          "\u5927\u6d32\uff08\u7531 ISO3 \u6807\u51c6\u6620\u5c04\uff09",
          "World Bank \u6536\u5165\u7ec4\u5206\u7c7b"
        ),
        source = c("GHED", "GHED", "GHED", "GHED", "GHED", "GHED",
                   "GHED", "WDI", "WDI", "WDI", "WDI", "countrycode", "World Bank")
      )
      names(codebook) <- c("\u53d8\u91cf", "\u5b9a\u4e49\u4e0e\u53e3\u5f84", "\u6765\u6e90")
      reactable::reactable(codebook, pagination = FALSE, highlight = TRUE,
        defaultColDef = reactable::colDef(headerStyle = list(background = "#f1f3f7")),
        columns = list(
          "\u53d8\u91cf" = reactable::colDef(width = 150,
            style = list(fontFamily = "'JetBrains Mono', monospace", fontWeight = 600)),
          "\u5b9a\u4e49\u4e0e\u53e3\u5f84" = reactable::colDef(minWidth = 300),
          "\u6765\u6e90" = reactable::colDef(width = 110)
        ))
    })
  })
}
