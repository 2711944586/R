# =============================================================================
# 仪表盘/模块/mod_methods.R
# 方法手册嵌入版：数据来源、统计方法、变量字典、复现命令
# =============================================================================

mod_methods_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = htmltools::HTML("&#128218; \u65b9\u6cd5 Methods"),
    mod_v3_hero(
      kicker = "METHODOLOGY",
      title = "\u65b9\u6cd5\u4e0e\u6570\u636e\u8bf4\u660e",
      lead = paste(
        "\u672c\u9879\u76ee\u7684\u6570\u636esource\u3001\u7edf\u8ba1\u65b9\u6cd5\u3001variabledefinition\u4e0e\u590d\u73b0\u547d\u4ee4\u3002",
        "\u6240\u6709\u5206\u6790\u5747\u53ef\u901a\u8fc7\u4e0b\u65b9\u547d\u4ee4\u4e00\u952e\u590d\u73b0\u3002"
      ),
      meta = list(
        "WHO GHED 2024-12",
        "WDI 2024-10",
        "195 country \u00b7 2000\u20132023"
      )
    ),
    htmltools::div(
      style = "max-width: 1000px; margin: 0 auto; padding: 0 24px;",

      # ---- 数据来源 ----
      mod_v3_card(
        kicker = "DATA SOURCES",
        title = "\u6570\u636esource",
        htmltools::tags$table(
          class = "table table-sm",
          htmltools::tags$thead(htmltools::tags$tr(
            htmltools::tags$th("\u6570\u636e\u96c6"),
            htmltools::tags$th("source"),
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
              htmltools::tags$td("\u652f\u51fasource (CHE/GGHED/PVTD/EXT)")
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
        )
      ),

      # ---- 统计方法 ----
      mod_v3_card(
        kicker = "STATISTICAL METHODS",
        title = "\u7edf\u8ba1\u65b9\u6cd5",
        htmltools::tags$dl(
          htmltools::tags$dt("\u4e0d\u5e73\u7b49\u5ea6\u91cf"),
          htmltools::tags$dd("Gini (\u52a0\u6743)\u3001Theil-T (\u53ef\u5206\u89e3)\u3001Atkinson (\u03b5=0.5, 1)"),
          htmltools::tags$dt("\u9762\u677f\u56de\u5f52"),
          htmltools::tags$dd("log(CHE_pc) ~ log(GDP_pc) | country + year\uff0c\u53cc\u5411\u56fa\u5b9a\u6548\u5e94\uff0c\u805a\u7c7b\u7a33\u5065se"),
          htmltools::tags$dt("\u03b2-\u6536\u655b"),
          htmltools::tags$dd("\u589e\u901f ~ log(\u521d\u59cb\u6c34\u5e73) + \u5927\u6d32 FE\uff0c\u534a\u8870\u671f = -ln(2)/\u03b2"),
          htmltools::tags$dt("\u805a\u7c7b"),
          htmltools::tags$dd("PCA \u964d\u7ef4 + k-means (k=4)\uff0c\u57fa\u4e8e GGHED/PVTD/EXT/OOPS \u56db\u7ef4"),
          htmltools::tags$dt("\u53d8\u70b9\u68c0\u6d4b"),
          htmltools::tags$dd("PELT \u7b97\u6cd5\uff0c\u6700\u5c0f\u6bb5\u957f 4 \u5e74"),
          htmltools::tags$dt("\u9884\u6d4b"),
          htmltools::tags$dd("auto.arima + 80%/95% \u7f6e\u4fe1\u533a\u95f4\uff0ch=5 \u5e74"),
          htmltools::tags$dt("\u7a33\u5065\u6027"),
          htmltools::tags$dd("Bootstrap (B=200)\u3001\u5206\u4f4d\u6570\u56de\u5f52\u3001Jackknife\u3001\u6392\u5217\u68c0\u9a8c")
        )
      ),

      # ---- 变量字典 ----
      mod_v3_card(
        kicker = "CODEBOOK",
        title = "\u6838\u5fc3variable\u5b57\u5178",
        reactable::reactableOutput(ns("codebook_table"))
      ),

      # ---- 复现命令 ----
      mod_v3_card(
        kicker = "REPRODUCIBILITY",
        title = "\u590d\u73b0\u547d\u4ee4",
        htmltools::tags$pre(
          style = "background: #0c1424; color: #e6efff; padding: 18px; border-radius: 10px; font-size: 13px; overflow-x: auto;",
          htmltools::HTML(paste(
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
            sep = "\n"
          ))
        )
      ),

      # ---- 引用 ----
      mod_v3_card(
        kicker = "CITATIONS",
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
        `variable` = c("iso3_code", "year", "che_pc_usd2023", "gghed_che",
                         "pvtd_che", "ext_che", "hf3_che", "life_exp", "u5mr",
                         "gdp_pc_usd", "pop", "continent", "income_group"),
        `definition` = c(
          "ISO 3166-1 \u4e09\u5b57\u6bcdcountry\u4ee3\u7801",
          "year_col (2000\u20132023)",
          "\u4eba\u5747\u536b\u751f\u603b\u652f\u51fa (USD 2023 \u4e0d\u53d8\u4ef7)",
          "\u56fd\u5185\u653f\u5e9c\u536b\u751f\u652f\u51fa\u5360 CHE (%)",
          "\u56fd\u5185\u79c1\u4eba\u536b\u751f\u652f\u51fa\u5360 CHE (%)",
          "\u5916\u90e8\u63f4\u52a9\u536b\u751f\u652f\u51fa\u5360 CHE (%)",
          "\u5c45\u6c11\u81ea\u4ed8 (OOPS) \u5360 CHE (%)",
          "\u51fa\u751f\u65f6\u9884\u671f\u5bff\u547d (\u5c81)",
          "\u4e94\u5c81\u4ee5\u4e0b\u6b7b\u4ea1\u7387 (\u2030)",
          "\u4eba\u5747 GDP (USD \u4e0d\u53d8\u4ef7)",
          "\u603b\u4eba\u53e3",
          "\u5927\u6d32 (countrycode \u6620\u5c04)",
          "World Bank \u6536\u5165\u7ec4\u5206\u7c7b"
        ),
        `source` = c("GHED", "GHED", "GHED", "GHED", "GHED", "GHED",
                         "GHED", "WDI", "WDI", "WDI", "WDI", "countrycode", "World Bank"),
        check.names = FALSE
      )
      reactable::reactable(codebook, pagination = FALSE, highlight = TRUE,
        defaultColDef = reactable::colDef(headerStyle = list(background = "#f1f3f7")),
        columns = list(
          `variable` = reactable::colDef(width = 140,
            style = list(fontFamily = "'JetBrains Mono', monospace", fontWeight = 600)),
          `definition` = reactable::colDef(minWidth = 250),
          `source` = reactable::colDef(width = 100)
        ))
    })
  })
}
