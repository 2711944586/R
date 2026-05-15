# =============================================================================
# 仪表盘/模块/mod_about.R
# Tab 12 · About：项目说明 + 数据源 + 引用 + 致谢
# =============================================================================

mod_about_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = htmltools::HTML("&#9432; About"),
    htmltools::div(
      class = "panel-content",
      style = "max-width: 880px; margin: 0 auto;",
      htmltools::h2("Global Health Spending · 仪表盘"),
      htmltools::p(class = "text-muted",
        "WHO Global Health Expenditure Database (GHED) · 数据新闻级分析"),
      htmltools::tags$hr(),

      htmltools::h3("项目概述"),
      htmltools::p(
        "基于 WHO GHED 2024-12 release，分析 ",
        htmltools::strong("195 个国家 / 24 年（2000–2023）"),
        " 的卫生支出结构演化。共集成 ", htmltools::strong("6 个数据源"),
        "（GHED + WB WDI + WHO GHO + IMF GFS + OECD + IHME）, ",
        htmltools::strong("12 个分析维度"),
        "（公平 / 财政 / 效率 / 预防 / 援助 / 韧性 ...）。"),

      htmltools::h3("数据来源"),
      htmltools::tags$ul(
        htmltools::tags$li(htmltools::strong("WHO GHED 2024 · "),
          "三表 ~280k 行，financing schemes / health spending / spending purpose"),
        htmltools::tags$li(htmltools::strong("World Bank WDI · "),
          "30 个指标（人口 / GDP / 寿命 / 城镇化 / 贫困）"),
        htmltools::tags$li(htmltools::strong("WHO GHO · "),
          "12 个指标（HALE / UHC SCI / 疫苗 / 烟草 / NCD / 灾难性 OOP）"),
        htmltools::tags$li(htmltools::strong("IMF GFS / WEO · "),
          "8 个指标（政府卫生 / 财政平衡 / 公共债务）"),
        htmltools::tags$li(htmltools::strong("OECD Health · "),
          "15 个指标（医生 / 床位 / 预防 / 处方）"),
        htmltools::tags$li(htmltools::strong("IHME GBD lite · "),
          "5 个指标（DALY / 期望寿命 / 死亡率结构）")
      ),

      htmltools::h3("方法论"),
      htmltools::tags$ul(
        htmltools::tags$li("不平等：人口加权 Gini / Theil-T / Atkinson(ε=0.5,1,2)"),
        htmltools::tags$li("效率：DEA + SFA + 寿命弹性回归"),
        htmltools::tags$li("收敛：β-收敛 + σ-收敛 + 半衰期"),
        htmltools::tags$li("预测：ARIMA 80%/95% PI + 蒙特卡洛 1000 次情景"),
        htmltools::tags$li("聚类：PCA + k-means + Ward 层次"),
        htmltools::tags$li("因果：固定效应面板 + 工具变量 + RDD")
      ),

      htmltools::h3("产出"),
      htmltools::tags$ul(
        htmltools::tags$li("44 张静态图（PNG + SVG，可印刷）"),
        htmltools::tags$li("24 个交互 widget（plotly / leaflet / reactable / DT）"),
        htmltools::tags$li("12 张表（数据质量报告 + 模型结果）"),
        htmltools::tags$li("Quarto Book 12 章 + 方法学附录"),
        htmltools::tags$li("Shiny 仪表盘 12 tab（本页面）")
      ),

      htmltools::h3("引用"),
      htmltools::pre(
        "World Health Organization (2024). Global Health Expenditure Database 2024.\n",
        "https://apps.who.int/nha/database\n\n",
        "庄颂 (2026). Global Health Spending Dashboard.\n",
        "https://2711944586.github.io/R/"
      ),

      htmltools::h3("致谢"),
      htmltools::p("感谢 WHO、World Bank、IMF、OECD、IHME 提供开放数据。"),
      htmltools::p("感谢 R 社区（tidyverse, shiny, plotly, leaflet, reactable, ggplot2, fixest, forecast, sf, rnaturalearth）。"),

      htmltools::tags$hr(),
      htmltools::p(class = "text-muted",
        htmltools::strong("作者: "), "庄颂 · 学号 20241334 · ",
        htmltools::tags$a(href = "https://github.com/2711944586/R", "GitHub"),
        " · ", htmltools::tags$a(href = "https://2711944586.github.io/R/",
                                  "项目主页"))
    )
  )
}

mod_about_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    invisible(NULL)
  })
}
