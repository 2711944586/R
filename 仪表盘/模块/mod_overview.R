# =============================================================================
# 仪表盘/模块/mod_overview.R
# Tab 1 · 总览：KPI + 全球三源面积 + leaflet 世界地图 + OOPS 排行
# =============================================================================

mod_overview_ui <- function(id) {
  ns <- shiny::NS(id)

  module_card <- function(target, title, desc, kicker) {
    tone <- switch(target,
      about = "primary", methods = "secondary",
      country = "primary", regional = "good", ranking = "warn",
      benchmark = "secondary",
      financing = "primary", spending = "secondary", purpose = "good",
      aid = "warn", fiscal = "bad",
      equity = "good", inequality = "secondary", efficiency = "primary",
      convergence = "warn", decomposition = "bad",
      outcomes = "good", sdg = "primary", prevention = "secondary",
      aging = "warn",
      pandemic = "bad", growth = "secondary", transition = "good",
      timeline = "neutral", extremes = "bad",
      compare = "primary", cluster = "secondary", forecast = "warn",
      scenarios = "good", correlation = "primary", distribution = "secondary",
      robustness = "warn", dataquality = "primary", policy = "good",
      atlas = "secondary",
      "primary"
    )
    mod_v3_module_card(
      ns = ns, target = target, kicker = kicker, title = title, desc = desc,
      icon = sub("\\s.*$", "", kicker), tone = tone
    )
  }

  bslib::nav_panel(
    title = htmltools::HTML("&#127759; \u603b\u89c8 Overview"),
    value = "overview",
    icon  = NULL,
    mod_v3_hero(
      kicker = "Global Health Expenditure Database 2024-12",
      title = "全球卫生支出 · 资金、结构与结果的 24 年长卷",
      lead = paste(
        "基于 WHO GHED + WDI，对 195 个国家 2000-2023 年的医疗资金来源、",
        "政府/私人/外援构成、人均支出与健康产出进行系统建模；首页把静态报告的",
        "总体叙事压缩为可筛选、可悬停、可跳转的交互入口。"
      ),
      meta = list(
        "\u4f5c\u8005 \u5e84\u9882 (20241334)",
        "36 \u4e2a Shiny \u6a21\u5757 \u00b7 300 \u5f20\u9759\u6001\u56fe \u00b7 133 \u4e2a\u4ea4\u4e92\u7ec4\u4ef6",
        list(label = "\u9759\u6001\u9996\u9875",
             href = "https://2711944586.github.io/R/"),
        list(label = "GitHub", href = "https://github.com/2711944586/R")
      )
    ),
    mod_v3_page_body(
    wide = TRUE,
    shiny::uiOutput(ns("overview_kpi_strip")),
    mod_v3_badge_row(
      "WHO GHED + WDI",
      "195 个国家",
      "2000-2023",
      "36 个交互模块",
      "静态报告同口径",
      tone = "primary"
    ),
    mod_v3_story_grid(
      columns = 3,
      mod_v3_insight(
        kicker = "Orientation",
        title = "先看资金结构，再看负担分布",
        text = "首页把全球资金来源、人均支出、OOPS 负担和空间分布放在同一屏，让读者先建立总体坐标。",
        tone = "primary",
        icon = "01"
      ),
      mod_v3_insight(
        kicker = "Navigation",
        title = "模块不是目录，而是分析路径",
        text = "下方 35 张专题卡按国家、筹资、公平、产出、冲击、工具和质量分组，可直接跳转到对应交互页面。",
        tone = "secondary",
        icon = "02"
      ),
      mod_v3_insight(
        kicker = "Consistency",
        title = "Shiny 与 HTML 共用叙事口径",
        text = "这里展示的 KPI、图表和后续专题页均读取同一套 master_enriched 数据，便于和静态报告逐项对照。",
        tone = "good",
        icon = "03"
      )
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      mod_card(
        kicker = "F1 · Source mix",
        title = "\u5168\u7403\u4e09\u6e90\u7ed3\u6784\u6f14\u5316 (2000\u2013latest)",
        htmltools::p(class = "card-note",
                     paste("\u653f\u5e9c (GGHE-D) \u00b7 \u79c1\u4eba (PVT-D) \u00b7 \u5916\u63f4 (EXT)",
                           "\u00b7 \u4ee5\u5168\u7403\u603b CHE \u4e3a\u5206\u6bcd\uff0c",
                           "\u53ef\u89c2\u5bdf\u516c\u79c1\u3001\u5916\u63f4\u4e09\u8005\u7684\u53cd\u8f6c\u4e0e\u8d8b\u52bf\u3002")),
        mod_v3_chart_guide(
          "如何阅读",
          "面积图回答的是“谁在支付医疗账单”。如果政府占比上升且 OOPS 回落，通常意味着风险共担能力增强；如果私人现金支付长期偏高，则财务保护仍偏脆弱。",
          bullets = c("观察长期方向，不只看单年尖峰。",
                      "EXT 占比应结合援助模块判断外部依赖。"),
          tone = "info"
        ),
        mod_spinner(plotly::plotlyOutput(ns("source_area"), height = 380)),
        footer = "分母为全球 CHE 汇总值；分子分别为 GGHE-D、PVT-D、EXT。"
      ),
      mod_card(
        kicker = "F2 · Burden spread",
        title = "\u5404\u5927\u6d32 OOPS \u5206\u5e03 (\u6700\u65b0\u5e74)",
        htmltools::p(class = "card-note",
                     paste("\u7bb1\u7ebf+\u6563\u70b9\u5c55\u793a\u5404\u5927\u6d32\u5e74\u5ea6\u4e2d\u4f4d\u6570",
                           "\u3001\u56db\u5206\u4f4d\u6570\u3001\u6781\u503c\u56fd\uff0c",
                           "\u4ee5\u53ca\u9ad8 OOPS \u5c3e\u90e8\u56fd\u5bb6\u5206\u5e03\u3002")),
        mod_v3_chart_guide(
          "比较逻辑",
          "箱体宽度不表达样本量，重点看中位线、四分位距和离群点。区域内部离散度越高，说明同一洲内制度与收入差异越值得拆开分析。",
          bullets = c("散点悬停可查看具体国家。",
                      "高端离群国家通常需要结合政策模块解释。"),
          tone = "warn"
        ),
        mod_spinner(plotly::plotlyOutput(ns("oops_box"), height = 380)),
        footer = "OOPS = Out-of-pocket spending as share of CHE。"
      )
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      mod_card(
        kicker = "F3 · Catch-up path",
        title = "\u4eba\u5747 CHE \u9ad8\u4f4e\u56fd\u5bf9\u6bd4 (Slope chart)",
        htmltools::p(class = "card-note",
                     paste("\u7eff\u8272 = 2000 \u00b7 \u6a59\u8272 = \u6700\u65b0\u5e74\u3002",
                           "\u659c\u7387\u5c55\u793a\u4ee3\u8868\u56fd\u5bb6\u4e0a\u5347/\u4e0b\u964d\u7684\u8d8b\u52bf\uff0c",
                           "\u5feb\u901f\u8bc6\u522b\u8ffd\u8d76\u8005\u4e0e\u8131\u9698\u8005\u3002")),
        mod_v3_chart_guide(
          "读斜率而非读排名",
          "向上斜率代表人均支出扩张，向下斜率代表实际支出回落。它更适合识别追赶、停滞或异常收缩，而不是单纯给国家排座次。",
          bullets = c("高收入国家常占据绝对水平上沿。",
                      "低起点高增速国家需要回到国家画像查看基数。"),
          tone = "good"
        ),
        mod_spinner(plotly::plotlyOutput(ns("slope_che"), height = 380)),
        footer = "金额统一到 2023 年美元口径，便于跨年比较。"
      ),
      mod_card(
        kicker = "F4 · High-risk list",
        title = "OOPS \u9ad8\u8d1f\u62c5 Top 15 (\u6700\u65b0\u5e74)",
        htmltools::p(class = "card-note",
                     paste("\u9ad8 OOPS \u610f\u5473\u7740\u5c45\u6c11\u533b\u7597\u8d1f\u62c5\u4ee5",
                           "\u73b0\u91d1\u5f62\u5f0f\u76f4\u63a5\u627f\u62c5\uff0c",
                           "\u7ed3\u5408\u5404\u56fd GGHED \u5360\u6bd4\u53ef\u8bc4\u4f30\u8d22\u52a1\u4fdd\u62a4\u8983\u53d1\u3002")),
        mod_v3_chart_guide(
          "预警用途",
          "Top 15 不是价值判断，而是把现金支付风险最高的一组国家先推到前台，方便继续检查政府支出、外援依赖和收入组背景。",
          bullets = c("高 OOPS 可能来自保障不足，也可能来自服务利用变化。",
                      "建议与筹资结构、公共财政和政策模块联读。"),
          tone = "bad"
        ),
        mod_spinner(plotly::plotlyOutput(ns("oops_top"), height = 380)),
        footer = "排序使用最新年份国家观测值，缺失国家自动剔除。"
      )
    ),
    mod_card(
      kicker = "F5 · Spatial scan",
      title = "\u4e16\u754c OOPS \u5730\u56fe (\u70b9\u51fb\u67e5\u770b\u8be6\u60c5)",
      htmltools::p(class = "card-note",
                   paste("\u989c\u8272\u8d8a\u6df1\u8868\u793a OOPS \u5360\u6bd4\u8d8a\u9ad8\u3002",
                         "\u5730\u56fe\u9762\u79ef\u4e0d\u4ee3\u8868\u4eba\u53e3\u89c4\u6a21\uff0c",
                         "\u8bf7\u540c\u65f6\u53c2\u8003\u5de6\u4fa7 Top 15 \u6392\u884c\u3002")),
      mod_v3_chart_guide(
        "空间视角",
        "地图用于发现区域聚集和邻近差异。由于行政面积会放大部分国家的视觉权重，结论应回到排行、箱线图和国家画像交叉确认。",
        bullets = c("点击国家查看标签信息。",
                    "缺失区域不强行插补，避免制造虚假连续性。"),
        tone = "info"
      ),
      mod_spinner(leaflet::leafletOutput(ns("world_map"), height = 520)),
      footer = "Leaflet 底图仅用于定位；统计口径来自 master_enriched。"
    ),
    mod_v3_section_head(
      "Guided paths",
      "\u63a8\u8350\u9605\u8bfb\u8def\u5f84",
      "\u6839\u636e\u4f60\u7684\u65f6\u95f4\u548c\u5173\u6ce8\u70b9\uff0c\u9009\u62e9\u4e00\u6761\u8def\u5f84\u8fdb\u5165\u5168\u90e8\u5206\u6790\uff1b\u9996\u9875\u4e0d\u53ea\u662f\u76ee\u5f55\uff0c\u800c\u662f\u9759\u6001 HTML \u53d9\u4e8b\u7684\u4ea4\u4e92\u7248\u7d22\u5f15\u3002"
    ),
    mod_v3_story_grid(
      columns = 3,
      mod_v3_insight(
        kicker = "3 minutes",
        title = "\u5feb\u901f\u4e86\u89e3\u5168\u5c40",
        text = "\u5148\u770b KPI \u548c\u4e09\u6e90\u9762\u79ef\u56fe\uff0c\u518d\u7528 OOPS Top 15 \u548c\u4e16\u754c\u5730\u56fe\u9501\u5b9a\u9ad8\u8d1f\u62c5\u56fd\u5bb6\uff0c\u6700\u540e\u56de\u5230\u7ed3\u8bba\u4e0e\u653f\u7b56\u5efa\u8bae\u3002",
        tone = "primary",
        icon = "A"
      ),
      mod_v3_insight(
        kicker = "Policy route",
        title = "\u516c\u5e73\u4e0e\u6548\u7387\u4e13\u9898",
        text = "\u4ece\u516c\u5e73\u6307\u6807\u3001Theil \u5206\u89e3\u3001DEA \u6548\u7387\u5230\u653f\u7b56\u63a8\u8350\uff0c\u8fde\u7eed\u8FFD\u8E2A\u8D44\u91D1\u5206\u914D\u3001\u8D22\u52A1\u4FDD\u62A4\u548C\u4EA7\u51FA\u56DE\u62A5\u3002",
        tone = "good",
        icon = "B"
      ),
      mod_v3_insight(
        kicker = "Research route",
        title = "\u5168\u90e8\u6a21\u5757\u6df1\u5165\u63a2\u7d22",
        text = "\u4f9d\u6b21\u68c0\u67e5\u56fd\u5bb6\u753b\u50cf\u3001\u7b79\u8d44\u7ed3\u6784\u3001\u51b2\u51fb\u53d8\u5316\u3001\u9884\u6d4b\u60c5\u666f\u548c\u7a33\u5065\u6027\uff0c\u9002\u5408\u5199\u4f5c\u8bba\u6587\u6216\u590d\u73b0\u62a5\u544a\u3002",
        tone = "secondary",
        icon = "C"
      )
    ),
    mod_v3_section_head(
      "Module atlas",
      "\u9009\u62e9\u4f60\u611f\u5174\u8da3\u7684\u4e3b\u9898 \u00b7 36 \u4e2a\u4ea4\u4e92\u6a21\u5757",
      paste("\u70b9\u51fb\u5361\u7247\u53ef\u76f4\u63a5\u8df3\u8f6c\u3002",
            "\u6bcf\u4e2a\u6a21\u5757\u90fd\u53ef\u72ec\u7acb\u4ea4\u4e92\u3001\u9884\u8bbe\u53ef\u590d\u5236\uff0c",
            "\u5e76\u4e0e\u9759\u6001\u56fe\u3001\u62a5\u544a\u6b63\u6587\u5171\u7528\u4e00\u5957\u6307\u6807\u4f53\u7cfb\u3002")
    ),
    # ---- Group 1: 总览 -----------------------------------------------------
    htmltools::div(
      class = "ghs-section-head-nav",
      style = "margin-top:24px;",
      htmltools::h3(style = "font-size:18px;",
                    "\u603b\u89c8 Overview \u00b7 3 \u6a21\u5757")
    ),
    htmltools::div(
      class = "v3-module-grid",
      module_card("about", "\u9879\u76ee\u8bf4\u660e",
                  "\u6570\u636e\u53d1\u5e03\u6e90 \u00b7 \u53d8\u91cf\u5b57\u5178 \u00b7 \u590d\u73b0\u547d\u4ee4 \u00b7 \u5f15\u7528\u3002",
                  "01 About"),
      module_card("methods", "\u65b9\u6cd5\u624b\u518c",
                  "\u8be6\u7ec6\u7684\u5206\u6790\u65b9\u6cd5\u3001\u6307\u6807\u53e3\u5f84\u4e0e\u8d28\u91cf\u63a7\u5236\u8bf4\u660e\u3002",
                  "02 Methods")
    ),
    # ---- Group 2: 国家与区域 ----------------------------------------------
    htmltools::div(
      class = "ghs-section-head-nav",
      style = "margin-top:24px;",
      htmltools::h3(style = "font-size:18px;",
                    "\u56fd\u5bb6\u4e0e\u533a\u57df Country & Regional \u00b7 4 \u6a21\u5757")
    ),
    htmltools::div(
      class = "v3-module-grid",
      module_card("country", "\u56fd\u5bb6\u753b\u50cf",
                  "\u5355\u56fd CHE / OOPS / GGHED / \u9884\u671f\u5bff\u547d 24 \u5e74\u9762\u677f\u3002",
                  "03 Country"),
      module_card("regional", "\u533a\u57df\u5bf9\u6bd4",
                  "6 \u5927\u6d32 \u00d7 4 \u6536\u5165\u7ec4 \u00b7 \u533a\u57df\u95f4\u8de8\u671f\u5dee\u5f02\u4e0e\u8d8b\u540c\u3002",
                  "04 Regional"),
      module_card("ranking", "\u5168\u7403\u6392\u884c",
                  "\u4eba\u5747 / \u603b\u989d / \u589e\u901f / \u8d8b\u52bf \u00b7 \u591a\u6307\u6807\u6392\u540d\u4e0e\u53d8\u52a8\u3002",
                  "05 Ranking"),
      module_card("benchmark", "\u540c\u4f34\u5bf9\u6807",
                  "\u4ee5\u540c\u6536\u5165\u7ec4 / \u540c\u533a\u57df / \u540c\u4eba\u53e3\u89c4\u6a21\u4f5c\u5750\u6807\u53c2\u7167\u3002",
                  "06 Benchmark")
    ),
    # ---- Group 3: 筹资结构 ------------------------------------------------
    htmltools::div(
      class = "ghs-section-head-nav",
      style = "margin-top:24px;",
      htmltools::h3(style = "font-size:18px;",
                    "\u7b79\u8d44\u7ed3\u6784 Financing \u00b7 5 \u6a21\u5757")
    ),
    htmltools::div(
      class = "v3-module-grid",
      module_card("financing", "\u7b79\u8d44\u4f53\u7cfb",
                  "\u793e\u4fdd / \u5546\u4fdd / \u9884\u4ed8 / \u73b0\u91d1 \u00b7 \u5236\u5ea6\u578b\u6001\u8de8\u5e74\u6f14\u5316\u3002",
                  "07 Financing"),
      module_card("spending", "\u652f\u51fa\u603b\u91cf",
                  "\u603b CHE / \u4eba\u5747 / GDP \u5360\u6bd4 \u00b7 \u4e09\u4e2a\u91cf\u7ea7\u540c\u6b65\u8003\u5bdf\u3002",
                  "08 Spending"),
      module_card("purpose", "\u652f\u51fa\u7528\u9014",
                  "HC1\u2013HC9 \u4e5d\u5927\u529f\u80fd\u7ec4 \u00b7 \u6cbb\u7597 / \u9884\u9632 / \u7ba1\u7406\u5360\u6bd4\u3002",
                  "09 Purpose"),
      module_card("aid", "\u5916\u63f4\u4e0e\u63f4\u52a9",
                  "EXT \u4f9d\u8d56\u5ea6 \u00b7 ODA \u6d41\u5411 \u00b7 \u9ad8\u4f9d\u8d56\u56fd\u753b\u50cf\u3002",
                  "10 Aid"),
      module_card("fiscal", "\u516c\u5171\u8d22\u653f",
                  "GGHED / GGE / GDP \u00b7 \u8d22\u653f\u7a7a\u95f4\u4e0e\u501f\u8d37\u6210\u672c\u4ea4\u4e92\u3002",
                  "11 Fiscal")
    ),
    # ---- Group 4: 公平与效率 ----------------------------------------------
    htmltools::div(
      class = "ghs-section-head-nav",
      style = "margin-top:24px;",
      htmltools::h3(style = "font-size:18px;",
                    "\u516c\u5e73\u4e0e\u6548\u7387 Equity & Efficiency \u00b7 5 \u6a21\u5757")
    ),
    htmltools::div(
      class = "v3-module-grid",
      module_card("equity", "\u516c\u5e73\u4e0e\u4e0d\u5e73\u7b49",
                  "Gini / Theil / Atkinson / Lorenz \u00b7 \u5728\u7ebf\u91cd\u7b97\u3002",
                  "12 Equity"),
      module_card("inequality", "\u4e0d\u5e73\u7b49\u5206\u89e3",
                  "Theil-T \u7ec4\u95f4 / \u7ec4\u5185 \u00b7 between vs within \u8d8b\u52bf\u3002",
                  "13 Inequality"),
      module_card("efficiency", "\u6548\u7387\u4e0e\u4ea7\u51fa",
                  "DEA \u524d\u6cbf \u00b7 CHE\u2192HALE \u5f39\u6027 \u00b7 \u540c\u8d44\u91d1\u4f4d\u6b8b\u5dee\u3002",
                  "14 Efficiency"),
      module_card("convergence", "\u6536\u655b\u5206\u6790",
                  "\u03b2-\u6536\u655b / \u03c3-\u6536\u655b \u00b7 \u533a\u57df\u4e0e\u6536\u5165\u7ec4\u5185\u8ddf\u5347\u3002",
                  "15 Convergence"),
      module_card("decomposition", "\u8d21\u732e\u5206\u89e3",
                  "Shapley / Theil \u5206\u89e3 \u00b7 \u53d8\u91cf\u8d21\u732e\u4e0e\u589e\u91cf\u53e3\u5f84\u3002",
                  "16 Decompose")
    ),
    # ---- Group 5: 健康产出 ------------------------------------------------
    htmltools::div(
      class = "ghs-section-head-nav",
      style = "margin-top:24px;",
      htmltools::h3(style = "font-size:18px;",
                    "\u5065\u5eb7\u4ea7\u51fa Health Outcomes \u00b7 4 \u6a21\u5757")
    ),
    htmltools::div(
      class = "v3-module-grid",
      module_card("outcomes", "\u8d44\u91d1 \u00b7 \u7ed3\u679c",
                  "U5MR / SDG-3 / \u9884\u671f\u5bff\u547d \u4e0e\u4eba\u5747 CHE \u8054\u52a8\u753b\u50cf\u3002",
                  "17 Outcomes"),
      module_card("sdg", "SDG-3 \u8fdb\u5c55",
                  "5 \u4e2a SDG-3 \u5b50\u6307\u6807 \u00b7 \u8de8\u671f\u8f68\u8ff9\u4e0e\u9694\u53e3\u3002",
                  "18 SDG-3"),
      module_card("prevention", "\u9884\u9632\u4e0e\u6cbb\u7597",
                  "HC6 \u9884\u9632\u4e0e HALE/DALY \u00b7 \u9884\u9632\u6027\u62a4\u7406\u8fb9\u9645\u6536\u76ca\u3002",
                  "19 Prevention"),
      module_card("aging", "\u8001\u9f84\u5316",
                  "65+ \u4eba\u53e3\u5360\u6bd4\u4e0e CHE \u538b\u529b \u00b7 OECD vs LMIC \u5bf9\u6bd4\u3002",
                  "20 Aging")
    ),
    # ---- Group 6: 冲击与变化 ----------------------------------------------
    htmltools::div(
      class = "ghs-section-head-nav",
      style = "margin-top:24px;",
      htmltools::h3(style = "font-size:18px;",
                    "\u51b2\u51fb\u4e0e\u53d8\u5316 Shocks & Change \u00b7 5 \u6a21\u5757")
    ),
    htmltools::div(
      class = "v3-module-grid",
      module_card("pandemic", "\u75ab\u60c5\u51b2\u51fb",
                  "2019 vs 2020\u20132022 \u00b7 COVID dumbbell \u00b7 \u540c\u6bd4\u70ed\u56fe\u3002",
                  "21 Pandemic"),
      module_card("growth", "\u589e\u957f\u52a8\u529b",
                  "CAGR / \u52a8\u91cf\u00b7\u80fd\u91cf \u00b7 \u589e\u901f\u8de8\u671f\u8de8\u533a\u57df\u3002",
                  "22 Growth"),
      module_card("transition", "\u8f6c\u578b\u8def\u5f84",
                  "OOP\u2192\u9884\u4ed8 / GGHED \u589e\u957f \u00b7 \u8d22\u52a1\u4fdd\u62a4\u8f6c\u578b\u3002",
                  "23 Transition"),
      module_card("timeline", "\u65f6\u95f4\u7ebf\u4e8b\u4ef6",
                  "GFC / COVID / \u901a\u80c0 \u00b7 \u5168\u7403\u4e8b\u4ef6\u4e0e CHE \u53cd\u5e94\u3002",
                  "24 Timeline"),
      module_card("extremes", "\u6781\u503c\u4e8b\u4ef6",
                  "\u9ad8 OOP \u00b7 \u4f4e GGHED \u00b7 \u9ad8\u589e\u957f \u00b7 \u8de8\u95e8\u69db\u8bc6\u522b\u3002",
                  "25 Extremes")
    ),
    # ---- Group 7: 分析工具 ------------------------------------------------
    htmltools::div(
      class = "ghs-section-head-nav",
      style = "margin-top:24px;",
      htmltools::h3(style = "font-size:18px;",
                    "\u5206\u6790\u5de5\u5177 Analysis Tools \u00b7 6 \u6a21\u5757")
    ),
    htmltools::div(
      class = "v3-module-grid",
      module_card("compare", "\u591a\u56fd\u591a\u6307\u6807\u5bf9\u6bd4",
                  "\u4efb\u610f\u9009 N \u4e2a\u56fd\u5bb6 \u00d7 N \u4e2a\u6307\u6807 \u00b7 \u70ed\u56fe\u4e0e\u8868\u683c\u3002",
                  "26 Compare"),
      module_card("cluster", "\u805a\u7c7b\u4e0e\u8c61\u9650",
                  "PCA + KMeans \u5728\u7ebf\u8c03 K \u00b7 \u8c61\u9650\u4e0e\u4ee3\u8868\u56fd\u3002",
                  "27 Cluster"),
      module_card("forecast", "\u9884\u6d4b",
                  "ARIMA / ETS \u9884\u6d4b \u00b7 \u4e0d\u786e\u5b9a\u6027\u533a\u95f4\u4e0e\u8001\u9a8c\u8bc1\u3002",
                  "28 Forecast"),
      module_card("scenarios", "\u60c5\u666f\u4eff\u771f",
                  "OOPS / GGHED / \u5916\u63f4 \u4e09\u6e90\u4eff\u771f \u00b7 \u672a\u6765\u6f14\u53d8\u3002",
                  "29 Scenario"),
      module_card("correlation", "\u76f8\u5173\u6027\u5206\u6790",
                  "\u591a\u53d8\u91cf\u76f8\u5173 \u00b7 \u504f\u76f8\u5173 \u00b7 \u70ed\u56fe\u4e0e\u7f51\u7edc\u3002",
                  "30 Correlation"),
      module_card("distribution", "\u5206\u5e03\u53ef\u89c6\u5316",
                  "\u5bc6\u5ea6\u00b7\u5206\u4f4d\u6570\u00b7\u8108\u7eb9 \u00b7 \u8de8\u5e74\u5206\u5e03\u6f14\u5316\u3002",
                  "31 Distribution")
    ),
    # ---- Group 8: 质量与稳健 ----------------------------------------------
    htmltools::div(
      class = "ghs-section-head-nav",
      style = "margin-top:24px;",
      htmltools::h3(style = "font-size:18px;",
                    "\u8d28\u91cf\u4e0e\u7a33\u5065 Quality & Robustness \u00b7 4 \u6a21\u5757")
    ),
    htmltools::div(
      class = "v3-module-grid",
      module_card("robustness", "\u7a33\u5065\u6027",
                  "\u591a\u6837\u672c\u00b7\u591a\u95e8\u69db\u00b7\u591a\u53d8\u91cf \u00b7 \u7ed3\u8bba\u7a33\u5065\u6027\u68c0\u9a8c\u3002",
                  "32 Robustness"),
      module_card("dataquality", "\u6570\u636e\u8d28\u91cf",
                  "\u7f3a\u5931\u70ed\u56fe \u00b7 \u4fee\u8ba2\u8bb0\u5f55 \u00b7 \u4e00\u81f4\u6027\u8bca\u65ad\u3002",
                  "33 Data Quality"),
      module_card("policy", "\u653f\u7b56\u63a8\u8350",
                  "\u6309\u98ce\u9669\u67e5\u8be2 \u00b7 \u751f\u6210\u4e2a\u6027\u5316\u653f\u7b56\u5efa\u8bae\u3002",
                  "34 Policy"),
      module_card("atlas", "\u5168\u7403 Atlas",
                  "Choropleth + \u53cc\u53d8\u91cf\u4e0a\u8272 \u00b7 6 \u5927\u6d32\u00d75 \u6307\u6807\u3002",
                  "35 Atlas")
    )
    )
  )
}

mod_overview_server <- function(id, master_r, world_sf_obj, year_max,
                                parent_session = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    output$overview_kpi_strip <- shiny::renderUI({
      m <- master_r()
      years <- m$year[is.finite(m$year)]
      year_label <- if (length(years)) {
        sprintf("%d-%d", min(years, na.rm = TRUE), max(years, na.rm = TRUE))
      } else {
        "\u2014"
      }
      latest <- m[m$year == year_max, , drop = FALSE]
      latest_countries <- unique(stats::na.omit(latest$iso3_code))
      n_countries <- length(unique(stats::na.omit(m$iso3_code)))
      oops <- mean(latest$hf3_che[is.finite(latest$hf3_che)], na.rm = TRUE)
      che_pc <- stats::median(
        latest$che_pc_usd2023[is.finite(latest$che_pc_usd2023)],
        na.rm = TRUE
      )
      gghed <- mean(latest$gghed_che[is.finite(latest$gghed_che)],
                    na.rm = TRUE)
      ext <- mean(latest$ext_che[is.finite(latest$ext_che)], na.rm = TRUE)

      mod_v3_kpi_grid(
        mod_v3_kpi(
          "\u8986\u76d6\u56fd\u5bb6",
          fmt_v3_num(n_countries),
          hint = "master_enriched 中可识别 ISO3 的国家/地区。",
          tone = "primary"
        ),
        mod_v3_kpi(
          "\u5e74\u4efd\u8303\u56f4",
          year_label,
          hint = "\u5168\u90e8\u6a21\u5757\u5171\u7528\u8fd9\u4e00\u65f6\u95f4\u9762\u677f\u3002",
          tone = "good"
        ),
        mod_v3_kpi(
          sprintf("%d \u6700\u65b0\u6837\u672c", year_max),
          fmt_v3_num(length(latest_countries)),
          hint = "\u6700\u65b0\u5e74\u53ef\u7528\u56fd\u5bb6\u6570\uff0c\u53d7\u7f3a\u5931\u503c\u5f71\u54cd\u3002",
          tone = "secondary"
        ),
        mod_v3_kpi(
          sprintf("%d OOPS \u5747\u503c", year_max),
          fmt_v3_pct(oops, 1),
          hint = "\u5c45\u6c11\u73b0\u91d1\u81ea\u4ed8\u5360 CHE \u7684\u5e73\u5747\u6bd4\u91cd\u3002",
          tone = "bad"
        ),
        mod_v3_kpi(
          sprintf("%d \u4eba\u5747 CHE \u4e2d\u4f4d\u6570", year_max),
          fmt_v3_usd(che_pc),
          hint = "\u6309 2023 \u5e74\u7f8e\u5143\u53e3\u5f84\u8ba1\u7b97\uff0c\u964d\u4f4e\u6781\u503c\u5f71\u54cd\u3002",
          tone = "warn"
        ),
        mod_v3_kpi(
          sprintf("%d GGHE-D / EXT", year_max),
          paste0(fmt_v3_pct(gghed, 1), " / ", fmt_v3_pct(ext, 1)),
          hint = "\u653f\u5e9c\u4e0e\u5916\u63f4\u4e24\u4e2a\u516c\u5171\u6027\u8d44\u91d1\u6765\u6e90\u7684\u5bf9\u7167\u3002",
          tone = "neutral"
        )
      )
    })

    output$source_area <- plotly::renderPlotly({
      m <- master_r()
      safe_plotly({
        p <- plot_source_area(m)
        plotly::ggplotly(p, tooltip = c("x", "y", "fill")) |>
          plotly::config(displaylogo = FALSE) |>
          plotly::layout(legend = list(orientation = "h", y = -0.15))
      })
    })

    output$oops_box <- plotly::renderPlotly({
      m <- master_r()
      safe_plotly({
        p <- plot_oops_box_continent(m, year_focus = year_max)
        plotly::ggplotly(p) |> plotly::config(displaylogo = FALSE)
      })
    })

    output$slope_che <- plotly::renderPlotly({
      m <- master_r()
      safe_plotly({
        p <- plot_slope_chart(m, value_col = "che_pc_usd2023",
                              year_a = min(m$year, na.rm = TRUE),
                              year_b = year_max, top_n = 18)
        plotly::ggplotly(p) |> plotly::config(displaylogo = FALSE)
      })
    })

    output$oops_top <- plotly::renderPlotly({
      m <- master_r()
      safe_plotly({
        p <- plot_oops_ranking(m, year_focus = year_max, top_n = 15)
        plotly::ggplotly(p) |> plotly::config(displaylogo = FALSE)
      })
    })

    output$world_map <- leaflet::renderLeaflet({
      m <- master_r()
      if (is.null(world_sf_obj)) {
        return(leaflet::leaflet() |>
                  leaflet::addProviderTiles("CartoDB.Positron") |>
                  leaflet::addLabelOnlyMarkers(0, 0,
                                                label = "world_sf \u4e0d\u53ef\u7528"))
      }
      tryCatch(
        leaflet_choropleth(m, world_sf_obj,
                            indicator_col = "hf3_che",
                            year_focus = year_max,
                            title = sprintf("OOPS %% \u00b7 %d", year_max)),
        error = function(e) {
          leaflet::leaflet() |>
            leaflet::addProviderTiles("CartoDB.Positron") |>
            leaflet::addLabelOnlyMarkers(0, 0,
              label = paste("Error:", conditionMessage(e)))
        }
      )
    })

    shiny::observeEvent(input$nav_to, {
      target <- input$nav_to
      if (!is.null(parent_session) && nzchar(target)) {
        bslib::nav_select(id = "main_nav", selected = target,
                           session = parent_session)
      }
    }, ignoreInit = TRUE)
  })
}
