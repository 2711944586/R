# =============================================================================
# 仪表盘/模块/mod_about.R
# Tab 12 · 关于（About）：项目说明 + 数据源 + 引用 + 致谢
# =============================================================================

mod_about_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = "\u5173\u4e8e About",
    value = "about",
    mod_v3_hero(
      kicker = "\u9879\u76ee\u8bf4\u660e",
      title = "Global Health Spending · 交互仪表盘说明",
      lead = paste(
        "本 Shiny 应用将静态报告中的全球卫生支出研究转译为可筛选、可追踪、",
        "可下载的交互工作台。它覆盖国家画像、区域对标、筹资结构、公平效率、",
        "健康产出、冲击韧性、预测情景和稳健性检查等完整分析链路。"
      ),
      meta = list(
        "WHO GHED 2024-12",
        "195 \u4e2a\u56fd\u5bb6",
        "2000-2023 \u5e74\u5ea6\u9762\u677f",
        list(label = "静态报告", href = "https://2711944586.github.io/R/"),
        list(label = "GitHub", href = "https://github.com/2711944586/R")
      )
    ),
    htmltools::div(
      class = "about-page",
      style = "max-width: 1180px; margin: 0 auto; padding: 0 22px;",
      mod_v3_kpi_grid(
        mod_v3_kpi("国家与地区", "195", hint = "以 ISO3 国家代码为主键整合"),
        mod_v3_kpi("年度面板", "24 年", hint = "2000-2023，按年度宽表建模",
                   tone = "good"),
        mod_v3_kpi("交互模块", "36", hint = "导航分为 8 个主题群",
                   tone = "secondary"),
        mod_v3_kpi("核心输出", "300+ 图表",
                   hint = "静态图、交互组件、模型表和复现缓存", tone = "warn")
      ),
      mod_v3_section_head(
        "\u5e94\u7528\u5b9a\u4f4d",
        "项目定位",
        paste(
          "这个应用不是静态报告的缩略版，而是一个面向探索、复核和展示的交互层：",
          "用户可以把同一套指标口径切换到国家、区域、收入组、时间窗口和政策情景。"
        )
      ),
      mod_v3_story_grid(
        columns = 3,
        mod_v3_insight(
          kicker = "\u5206\u6790\u8303\u56f4",
          title = "从总额到结构",
          text = paste(
            "CHE 总量、人均 CHE、GGHE-D、PVT-D、EXT、OOPS 和 HC 功能项",
            "共同描述支出的规模、来源和用途。"
          ),
          tone = "primary"
        ),
        mod_v3_insight(
          kicker = "\u653f\u7b56\u89c6\u89d2",
          title = "从公平到保护",
          text = paste(
            "不平等指数、灾难性自付、公共筹资占比和外援依赖度共同指向",
            "财政托底、家庭风险和制度韧性。"
          ),
          tone = "secondary"
        ),
        mod_v3_insight(
          kicker = "\u6a21\u578b\u5c42",
          title = "从观察到情景",
          text = paste(
            "PCA 聚类、收敛检验、趋势预测、蒙特卡洛情景和稳健性模块",
            "提供可解释的二级分析。"
          ),
          tone = "good"
        )
      ),
      bslib::layout_columns(
        col_widths = c(6, 6),
        mod_v3_card(
          kicker = "\u6570\u636e\u8d44\u4ea7",
          title = "数据来源与角色",
          htmltools::tags$table(
            class = "table table-sm v3-table",
            htmltools::tags$thead(htmltools::tags$tr(
              htmltools::tags$th("数据源"),
              htmltools::tags$th("主要内容"),
              htmltools::tags$th("在本项目中的用途")
            )),
            htmltools::tags$tbody(
              htmltools::tags$tr(
                htmltools::tags$td(htmltools::strong("WHO GHED")),
                htmltools::tags$td("卫生支出、筹资方案、资金来源、用途分类"),
                htmltools::tags$td("构建 CHE、GGHE-D、PVT-D、EXT、OOPS 与 HC 系列")
              ),
              htmltools::tags$tr(
                htmltools::tags$td(htmltools::strong("World Bank WDI")),
                htmltools::tags$td("人口、GDP、收入组、预期寿命、U5MR 等"),
                htmltools::tags$td("补充宏观背景、健康产出与分组变量")
              ),
              htmltools::tags$tr(
                htmltools::tags$td(htmltools::strong("WHO GHO")),
                htmltools::tags$td("HALE、UHC SCI、疫苗、NCD 等健康指标"),
                htmltools::tags$td("用于产出、预防、SDG-3 和效率视角")
              ),
              htmltools::tags$tr(
                htmltools::tags$td(htmltools::strong("IMF / OECD / IHME")),
                htmltools::tags$td("财政、服务供给和疾病负担辅助指标"),
                htmltools::tags$td("用于财政空间、稳健性和扩展解释")
              )
            )
          ),
          footer = "所有数据在进入模块前统一到 country-year 宽表，并保留原始字段口径。"
        ),
        mod_v3_card(
          kicker = "\u4ea4\u4ed8\u7269",
          title = "交付物与阅读方式",
          mod_v3_steps(list(
            list(title = "静态报告",
                 body = "用于完整叙事、图表编排、章节推导和课程提交。"),
            list(title = "Shiny 仪表盘",
                 body = "用于参数筛选、即时复算、国家追踪和现场展示。"),
            list(title = "派生数据与模型缓存",
                 body = "用于复现图表、避免重复计算，并支持 Atlas 下载。"),
            list(title = "自动测试",
                 body = "覆盖设计系统、Shiny helper、数据构建和关键统计函数。")
          ))
        )
      ),
      mod_v3_section_head(
        "\u9605\u8bfb\u8def\u5f84",
        "推荐阅读路径",
        "按目标选择模块，而不是线性浏览。每条路径都能从总览进入，再落到具体国家、分组或模型。"
      ),
      mod_v3_story_grid(
        columns = 4,
        mod_v3_insight(
          kicker = "\u4e09\u5206\u949f",
          title = "快速掌握全局",
          text = "先看总览 KPI、全球三源面积图、OOPS 地图和 Top 15 排行，形成第一层判断。",
          tone = "primary"
        ),
        mod_v3_insight(
          kicker = "\u653f\u7b56\u590d\u76d8",
          title = "政策分析线",
          text = "从公平、效率、财政和政策模块进入，重点观察高自付、低公共筹资和产出不足的组合。",
          tone = "secondary"
        ),
        mod_v3_insight(
          kicker = "\u56fd\u522b\u5907\u5fd8\u5f55",
          title = "国家备忘录线",
          text = "先用国家画像定位时间序列，再用对比、预测和情景模块输出可解释的国家摘要。",
          tone = "good"
        ),
        mod_v3_insight(
          kicker = "\u590d\u6838\u4e0e\u4e0b\u8f7d",
          title = "复核与下载线",
          text = "用数据质量、稳健性和 Atlas 模块检查缺失、异常值、模型敏感性和原始表格。",
          tone = "warn"
        )
      ),
      bslib::layout_columns(
        col_widths = c(6, 6),
        mod_v3_card(
          kicker = "\u89e3\u91ca\u8fb9\u754c",
          title = "解释边界",
          htmltools::tags$ul(
            htmltools::tags$li("跨国截面关系不自动等同于因果关系，尤其是 CHE 与寿命、OOPS 与制度保护之间。"),
            htmltools::tags$li("GHED 指标受国民卫生账户口径、价格折算、修订版本和报告完整度影响。"),
            htmltools::tags$li("预测和情景模块用于透明 what-if 推演，不替代正式财政预测或政策评估。"),
            htmltools::tags$li("地图展示空间分布，不按人口加权；涉及人口含义时需同时查看表格或加权指标。")
          )
        ),
        mod_v3_card(
          kicker = "\u5f15\u7528\u4e0e\u94fe\u63a5",
          title = "引用与链接",
          mod_v3_code_block(
            paste(
              "World Health Organization (2024). Global Health Expenditure Database.",
              "https://apps.who.int/nha/database",
              "",
              "World Bank (2024). World Development Indicators.",
              "https://databank.worldbank.org/source/world-development-indicators",
              "",
              "庄颂 (2026). Global Health Spending Dashboard.",
              "https://2711944586.github.io/R/",
              sep = "\n"
            ),
            title = "\u5efa\u8bae\u5f15\u7528"
          ),
          mod_v3_source_note(
            title = "\u4f5c\u8005",
            "庄颂 · 学号 20241334 · ",
            htmltools::tags$a(href = "https://github.com/2711944586/R",
                              target = "_blank", "GitHub"),
            " · ",
            htmltools::tags$a(href = "https://2711944586.github.io/R/",
                              target = "_blank", "项目主页")
          )
        )
      ),
      mod_v3_card(
        kicker = "\u81f4\u8c22",
        title = "致谢",
        htmltools::p(
          "感谢 WHO、World Bank、IMF、OECD、IHME 等机构提供开放数据；感谢 R 社区中 tidyverse、",
          "shiny、bslib、plotly、leaflet、reactable、DT、ggplot2、sf、forecast、fixest 等工具链。"
        ),
        htmltools::p(
          "本项目的 Shiny 层与静态 HTML 共用同一组变量语义、品牌色板和图表主题，确保课程展示、",
          "交互探索和复现脚本之间保持口径一致。"
        )
      )
    )
  )
}

mod_about_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    invisible(NULL)
  })
}
