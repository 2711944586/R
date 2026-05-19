# =============================================================================
# 仪表盘/模块/_helpers.R
# Shiny 模块共享工具：KPI 卡片 / spinner / 全局过滤器 / 数据切片
# =============================================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

#' KPI 卡（与 程序/13 kpi_card_html 保持设计一致，但带颜色变体）
mod_kpi <- function(label, value, sublabel = NULL,
                    color = c("primary", "danger", "success", "warning", "muted"),
                    icon = NULL) {
  color <- match.arg(color)
  htmltools::div(
    class = sprintf("kpi-card kpi-%s", color),
    htmltools::div(class = "kpi-label", label),
    htmltools::div(class = "kpi-value", value),
    if (!is.null(sublabel))
      htmltools::div(class = "kpi-sublabel", sublabel),
    if (!is.null(icon))
      htmltools::div(class = "kpi-icon", icon)
  )
}

#' 标准 spinner（颜色与品牌主色一致）
mod_spinner <- function(x, color = "#1B5E88") {
  shinycssloaders::withSpinner(x, type = 6, color = color, size = 0.6)
}

#' 合并 CSS class，自动丢弃 NULL / 空字符串
mod_class <- function(...) {
  x <- unlist(list(...), use.names = FALSE)
  x <- x[!is.na(x) & nzchar(x)]
  paste(unique(x), collapse = " ")
}

#' 标准卡片容器（panel-content 风格）
mod_card <- function(..., title = NULL, class = NULL,
                     kicker = NULL, footer = NULL) {
  mod_v3_card(...,
              title = title,
              kicker = kicker,
              footer = footer,
              class = mod_class("panel-content", "legacy-panel-card", class))
}

#' 全局过滤器应用于 master 数据
#' @param master master_enriched 数据
#' @param filters list(years, continents, incomes, isos)
apply_global_filter <- function(master, filters) {
  d <- master
  if (!is.null(filters$years) && length(filters$years) == 2) {
    d <- d[d$year >= filters$years[1] & d$year <= filters$years[2], , drop = FALSE]
  }
  if (!is.null(filters$continents) && length(filters$continents) > 0) {
    d <- d[d$continent %in% filters$continents, , drop = FALSE]
  }
  if (!is.null(filters$incomes) && length(filters$incomes) > 0) {
    d <- d[d$income_group %in% filters$incomes, , drop = FALSE]
  }
  if (!is.null(filters$isos) && length(filters$isos) > 0) {
    d <- d[d$iso3_code %in% filters$isos, , drop = FALSE]
  }
  d
}

#' 显示数据为空的占位
mod_empty_message <- function(msg = "数据不足") {
  htmltools::div(
    class = "panel-content text-muted text-center",
    style = "padding: 60px 20px;",
    htmltools::h4(msg),
    htmltools::p("调整左侧过滤器或选择不同时段")
  )
}

#' 安全的 plotly 渲染（捕获错误）
safe_plotly <- function(expr, fallback_msg = "图表渲染失败") {
  tryCatch(force(expr),
            error = function(e) {
              plotly::plotly_empty(type = "scatter", mode = "markers") |>
                plotly::layout(title = fallback_msg,
                                annotations = list(
                                  list(text = conditionMessage(e),
                                       x = 0.5, y = 0.5, showarrow = FALSE,
                                       font = list(color = "#999"))
                                ))
            })
}

#' 通用 KPI 重组 — 返回一个 4 列 fluidRow
mod_kpi_row <- function(...) {
  cards <- list(...)
  bslib::layout_columns(
    col_widths = rep(3, length(cards)),
    !!!cards
  )
}

# =============================================================================
# 共用组件
# -----------------------------------------------------------------------------
# 与 程序/13_design_system.R 的 helpers 配合使用，保证 Shiny 与静态 HTML
# 共享同一套语义槽 / KPI 风格 / callout / 章节头。
# =============================================================================

#' hero（顶部标题区，含 kicker / 大标题 / lead / meta strip）
mod_v3_hero <- function(kicker, title, lead, meta = NULL) {
  meta_html <- if (!is.null(meta) && length(meta)) {
    pieces <- vapply(meta, function(m) {
      if (is.list(m) && !is.null(m$href)) {
        sprintf("<a href='%s' target='_blank' rel='noreferrer'>%s</a>",
                htmltools::htmlEscape(m$href),
                htmltools::htmlEscape(m$label))
      } else {
        sprintf("<span>%s</span>", htmltools::htmlEscape(m))
      }
    }, character(1))
    paste0("<div class='v3-hero-meta'>",
           paste(pieces, collapse = "<span class='v3-meta-sep'>\u00b7</span>"),
           "</div>")
  } else ""
  htmltools::HTML(sprintf(
    paste0("<section class='v3-hero'><div class='v3-hero-inner'>",
           "<span class='v3-hero-kicker'>%s</span>",
           "<h1 class='v3-hero-title'>%s</h1>",
           "<p class='v3-hero-lead'>%s</p>%s</div></section>"),
    htmltools::htmlEscape(kicker),
    htmltools::htmlEscape(title),
    htmltools::htmlEscape(lead),
    meta_html))
}

#' 章节头（kicker + h3 + lead）
mod_v3_section_head <- function(kicker, title, lead = NULL) {
  lead_html <- if (!is.null(lead) && nchar(lead))
    sprintf("<p class='v3-section-lead'>%s</p>",
            htmltools::htmlEscape(lead))
  else ""
  htmltools::HTML(sprintf(
    paste0("<header class='v3-section-head'>",
           "<span class='v3-kicker'>%s</span>",
           "<h3 class='v3-section-title'>%s</h3>%s</header>"),
    htmltools::htmlEscape(kicker),
    htmltools::htmlEscape(title),
    lead_html))
}

#' 判断字符串是否更像 KPI 值，用于兼容 label/value 与 value/label 两种调用
mod_v3_is_value_like <- function(x) {
  if (is.null(x) || length(x) == 0) return(FALSE)
  txt <- trimws(as.character(x[[1]]))
  if (!nzchar(txt)) return(FALSE)
  if (txt %in% c("\u2014", "-", "NA")) return(TRUE)
  has_digit <- grepl("[0-9]", txt)
  has_value_mark <- grepl("[$%]|[0-9][,.0-9]*\\s*(T|B|M|K|USD|yrs?|年|次)?$",
                          txt, ignore.case = TRUE)
  short_value <- nchar(txt) <= 18 && has_digit
  has_digit && (has_value_mark || short_value)
}

#' v3 KPI 卡（替代 mod_kpi 的更精致版本，与静态页 .ghs_v3_kpi 一致）
mod_v3_kpi <- function(label, value, hint = NULL, trend = NULL,
                       tone = c("primary", "secondary", "good",
                                 "warn", "bad", "neutral")) {
  tone <- match.arg(tone)
  # 早期模块大量使用 mod_v3_kpi(value, label)，测试与新代码使用
  # mod_v3_kpi(label, value)。这里按显示特征兼容，避免逐页破坏性迁移。
  if (mod_v3_is_value_like(label) && !mod_v3_is_value_like(value)) {
    tmp <- label
    label <- value
    value <- tmp
  }
  tone_color <- switch(tone,
    primary = "#1d3f5f", secondary = "#c46327",
    good = "#2a857a", warn = "#c89a3b", bad = "#a23b3b",
    "#5d667a")
  trend_html <- ""
  if (!is.null(trend) && is.finite(trend)) {
    arr <- if (trend > 0) "\u2197" else if (trend < 0) "\u2198" else "\u2192"
    col <- if (trend > 0) "#2a857a"
           else if (trend < 0) "#a23b3b"
           else "#5d667a"
    trend_html <- sprintf(
      "<span class='v3-kpi-trend' style='color:%s'>%s %+0.1f%%</span>",
      col, arr, trend * 100)
  }
  hint_html <- if (!is.null(hint) && nchar(hint))
    sprintf("<div class='v3-kpi-hint'>%s</div>",
            htmltools::htmlEscape(hint))
  else ""
  htmltools::HTML(sprintf(
    paste0("<div class='v3-kpi' style='--tone:%s'>",
           "<div class='v3-kpi-value'>%s</div>",
           "<div class='v3-kpi-label'>%s%s</div>%s</div>"),
    tone_color,
    htmltools::htmlEscape(value),
    htmltools::htmlEscape(label),
    trend_html, hint_html))
}

#' KPI 网格（4 列，自动响应到 2 / 1 列）
mod_v3_kpi_grid <- function(...) {
  cards <- list(...)
  htmltools::tagList(
    htmltools::div(class = "v3-kpi-grid",
                   lapply(cards, function(c) c)))
}

#' 响应式叙事网格
mod_v3_story_grid <- function(..., columns = 3, class = NULL) {
  htmltools::div(
    class = mod_class("v3-story-grid", class),
    style = sprintf("--cols:%s", columns),
    ...
  )
}

#' 页面内容外层，限制宽度并统一 Shiny 模块间距
mod_v3_page_body <- function(..., wide = FALSE, class = NULL) {
  htmltools::div(
    class = mod_class("v3-page-body", if (wide) "v3-page-body-wide", class),
    ...
  )
}

#' 胶囊徽章行，用于标注数据口径、模型类型和交互能力
mod_v3_badge_row <- function(..., tone = c("primary", "secondary", "good",
                                           "warn", "bad", "neutral")) {
  tone <- match.arg(tone)
  tone_color <- switch(tone,
    primary = "#1d3f5f", secondary = "#c46327",
    good = "#2a857a", warn = "#c89a3b", bad = "#a23b3b",
    "#5d667a")
  labels <- unlist(list(...), use.names = FALSE)
  labels <- labels[!is.na(labels) & nzchar(labels)]
  htmltools::div(
    class = "v3-badge-row",
    style = sprintf("--tone:%s", tone_color),
    lapply(labels, function(x) htmltools::span(class = "v3-badge", x))
  )
}

#' 小型洞察卡：用于补足静态 HTML 中的叙事密度
mod_v3_insight <- function(title, text, kicker = NULL,
                           tone = c("primary", "secondary", "good",
                                    "warn", "bad", "neutral"),
                           icon = NULL, meta = NULL) {
  tone <- match.arg(tone)
  tone_color <- switch(tone,
    primary = "#1d3f5f", secondary = "#c46327",
    good = "#2a857a", warn = "#c89a3b", bad = "#a23b3b",
    "#5d667a")
  htmltools::div(
    class = "v3-insight",
    style = sprintf("--tone:%s", tone_color),
    if (!is.null(icon)) htmltools::span(class = "v3-insight-icon", icon),
    if (!is.null(kicker)) htmltools::span(class = "v3-insight-kicker", kicker),
    htmltools::strong(class = "v3-insight-title", title),
    htmltools::p(class = "v3-insight-text", text),
    if (!is.null(meta)) htmltools::span(class = "v3-insight-meta", meta)
  )
}

#' 读图提示 / 解释卡，放在图表前后补足“怎么看”
mod_v3_chart_guide <- function(title, text, bullets = NULL,
                               tone = c("info", "good", "warn", "bad")) {
  tone <- match.arg(tone)
  bullet_html <- if (!is.null(bullets) && length(bullets)) {
    htmltools::tags$ul(
      class = "v3-guide-list",
      lapply(bullets, htmltools::tags$li)
    )
  } else NULL
  htmltools::div(
    class = mod_class("v3-chart-guide", paste0("v3-chart-guide-", tone)),
    htmltools::strong(class = "v3-guide-title", title),
    htmltools::p(class = "v3-guide-text", text),
    bullet_html
  )
}

#' 侧栏说明块，替代零散 helpText，增强控制区质感
mod_v3_sidebar_note <- function(title, text, bullets = NULL) {
  htmltools::div(
    class = "v3-sidebar-note",
    htmltools::strong(title),
    htmltools::p(text),
    if (!is.null(bullets) && length(bullets)) {
      htmltools::tags$ul(lapply(bullets, htmltools::tags$li))
    }
  )
}

#' 横向解释轨道：把“输入-处理-输出”或“读图顺序”压缩成一行
mod_v3_rail <- function(items) {
  if (!length(items)) return(htmltools::HTML(""))
  htmltools::div(
    class = "v3-rail",
    lapply(seq_along(items), function(i) {
      it <- items[[i]]
      htmltools::div(
        class = "v3-rail-item",
        htmltools::span(class = "v3-rail-index", sprintf("%02d", i)),
        htmltools::strong(it$title %||% ""),
        htmltools::p(it$text %||% it$body %||% "")
      )
    })
  )
}

#' Topic brief registry used by Shiny modules.
#' Each spec supplies the narrative density that used to exist only in the
#' static HTML report: badges, insight cards, reading order and interpretation
#' note. Keep the copy compact because it appears above interactive controls.
mod_v3_topic_registry <- list(
  spending = list(
    badge_tone = "primary",
    badges = c("支出规模", "收入梯度", "地图 + 排行", "USD 2023"),
    insights = list(
      list(kicker = "Scale", title = "先看支出量级",
           text = "人均 CHE 的跨国差距通常达到两个数量级，直接决定后续图表需要使用对数轴和分组比较。",
           tone = "primary"),
      list(kicker = "Gradient", title = "收入组是第一解释层",
           text = "GDP/cap 与 CHE/cap 的共同移动揭示了支付能力、财政空间和服务价格共同构成的支出梯度。",
           tone = "secondary"),
      list(kicker = "Outliers", title = "偏离拟合线才是重点",
           text = "同等经济水平下支出明显偏高或偏低的国家，更适合继续进入效率、财政和产出模块复核。",
           tone = "good")
    ),
    rail = list(
      list(title = "选择年份", text = "固定横截面，避免把时序变化和国家差异混在一起。"),
      list(title = "切换着色", text = "大洲用于空间阅读，收入组用于制度和支付能力比较。"),
      list(title = "检查极端值", text = "Top 排行、箱线图和地图一起确认是否为真实高支出。"),
      list(title = "回到表格", text = "表格保留国家、收入组和核心比率，便于导出复核。")
    ),
    note = "读图时先判断分布跨度，再看同组内部差异；对数坐标下的距离代表相对差距，不是绝对美元差。"
  ),
  financing = list(
    badge_tone = "secondary",
    badges = c("筹资结构", "公共/私人/外援", "OOPS", "制度保护"),
    insights = list(
      list(kicker = "Structure", title = "结构比总量更接近制度",
           text = "公共筹资、私人支出和外部援助的相对权重，反映卫生系统风险如何在政府、家庭和捐助方之间分配。",
           tone = "primary"),
      list(kicker = "Protection", title = "OOPS 是保护压力信号",
           text = "自付比例高并不一定等于灾难性支出高，但二者同时偏高时，家庭财务风险通常更集中。",
           tone = "bad"),
      list(kicker = "Transition", title = "观察替代而不是孤立变化",
           text = "外援下降只有在公共筹资或预付机制同步补位时，才更可能代表可持续转型。",
           tone = "good")
    ),
    rail = list(
      list(title = "先看堆叠", text = "确认各组资金来源的主导结构。"),
      list(title = "再看 OOPS", text = "识别家庭承担压力是否随结构变化同步下降。"),
      list(title = "追踪趋势", text = "判断转型是长期变化还是单年波动。"),
      list(title = "查国家表", text = "定位结构异常和样本口径。")
    ),
    note = "筹资结构是百分比口径，组内比较要同时关注 CHE 总量，避免把小规模系统的比例变化误读为财政能力提升。"
  ),
  aging = list(
    badge_tone = "warn",
    badges = c("人口结构", "65+ 占比", "支出压力", "长期需求"),
    insights = list(
      list(kicker = "Demography", title = "年龄结构改变需求曲线",
           text = "65+ 人口占比升高会把医疗利用推向慢病、长期照护和更高强度的服务组合。",
           tone = "warn"),
      list(kicker = "Spending", title = "支出响应并非线性",
           text = "老龄化相近的国家可能因为支付制度、价格水平和服务供给不同，呈现完全不同的人均 CHE。",
           tone = "secondary"),
      list(kicker = "Policy", title = "关注可持续而非单年高低",
           text = "趋势图比截面排名更能说明财政是否正在提前吸收老龄化带来的支出压力。",
           tone = "good")
    ),
    rail = list(
      list(title = "固定年份", text = "先看当前老龄化与支出或产出的横截面关系。"),
      list(title = "切换纵轴", text = "比较 CHE、GGHE-D、寿命和 OOPS 对年龄结构的响应。"),
      list(title = "看分组趋势", text = "确认高收入组是否已进入稳定老龄化阶段。"),
      list(title = "查看地图", text = "把人口结构差异映射到区域格局。")
    ),
    note = "如果缺少正式 65+ 字段，模块会用寿命构造代理变量；这类结果适合趋势提示，不应当作为人口学精确估计。"
  ),
  aid = list(
    badge_tone = "warn",
    badges = c("外部援助", "依赖阈值", "可持续性", "退出风险"),
    insights = list(
      list(kicker = "Dependence", title = "高 EXT 是脆弱性线索",
           text = "外援占 CHE 比重越高，卫生系统越需要评估资金中断、项目退出和国内财政替代能力。",
           tone = "warn"),
      list(kicker = "Output", title = "援助与产出要一起读",
           text = "外援高并不自动表示产出差，散点图需要结合寿命、收入组和人口规模判断背景。",
           tone = "primary"),
      list(kicker = "Sustainability", title = "右下象限最需要关注",
           text = "高 EXT 且低 GGHED 的国家，在援助退出时更容易出现服务覆盖和预算连续性压力。",
           tone = "bad")
    ),
    rail = list(
      list(title = "设定阈值", text = "定义高依赖国家集合。"),
      list(title = "看空间分布", text = "识别援助集中区域和跨区域差异。"),
      list(title = "对照 GGHED", text = "评估国内公共筹资替代能力。"),
      list(title = "导出清单", text = "用表格复核高依赖国家的关键指标。")
    ),
    note = "EXT 是资金来源口径，不等同于项目绩效；政策解释需要结合国内财政、疾病负担和服务供给。"
  ),
  benchmark = list(
    badge_tone = "good",
    badges = c("参照组", "差距百分比", "追赶速度", "雷达对标"),
    insights = list(
      list(kicker = "Reference", title = "基准决定结论方向",
           text = "OECD、全球中位数和同收入组中位数回答的是不同问题，不能混用为单一排名。",
           tone = "primary"),
      list(kicker = "Gap", title = "差距要看正负也看幅度",
           text = "负值代表低于基准，正值代表高于基准；对于 OOPS 这类低值更好的指标需反向解释。",
           tone = "warn"),
      list(kicker = "Catch-up", title = "追赶路径比当前差距更重要",
           text = "当前仍落后的国家，如果十年差距持续收窄，政策含义不同于长期停滞的国家。",
           tone = "good")
    ),
    rail = list(
      list(title = "选基准", text = "先明确参照对象。"),
      list(title = "选指标", text = "确认该指标是高值更好还是低值更好。"),
      list(title = "看差距", text = "用条形图定位偏离最大的国家。"),
      list(title = "看趋势", text = "判断差距是在扩大还是收敛。")
    ),
    note = "对标模块用于形成比较问题，不直接给出政策目标；目标值应结合国家收入、人口结构和财政空间校准。"
  ),
  correlation = list(
    badge_tone = "primary",
    badges = c("Spearman", "变量关系", "残差", "条件相关"),
    insights = list(
      list(kicker = "Association", title = "相关不是因果",
           text = "本页用于发现变量之间的形态和异常点，不把截面相关直接解释为政策效果。",
           tone = "warn"),
      list(kicker = "Robustness", title = "秩相关更适合非线性",
           text = "Spearman 对极端值和曲线关系更稳健，适合 CHE、GDP 等右偏分布变量。",
           tone = "primary"),
      list(kicker = "Residual", title = "残差指出特殊国家",
           text = "在给定 X 后仍显著高于或低于拟合预期的国家，值得进入国家画像或政策模块深挖。",
           tone = "good")
    ),
    rail = list(
      list(title = "选择 X/Y", text = "构造你要检验的变量关系。"),
      list(title = "打开对数轴", text = "处理右偏金额变量。"),
      list(title = "对照矩阵", text = "确认该关系是否只是更大相关网络的一部分。"),
      list(title = "看残差", text = "识别偏离常规关系的国家。")
    ),
    note = "当变量存在共同收入梯度时，简单相关可能被 GDP/cap 驱动；需要结合分组相关和残差图判断。"
  ),
  convergence = list(
    badge_tone = "good",
    badges = c("Beta 收敛", "Sigma 收敛", "初始水平", "增长路径"),
    insights = list(
      list(kicker = "Beta", title = "低起点是否增长更快",
           text = "如果初始 CHE/cap 较低的国家后续增长率更高，散点关系会呈现负斜率。",
           tone = "good"),
      list(kicker = "Sigma", title = "离散度是否缩小",
           text = "即使存在 beta 收敛，整体分布也可能因为冲击或高收入组继续扩张而没有明显 sigma 收敛。",
           tone = "primary"),
      list(kicker = "Groups", title = "分收入组检查异质性",
           text = "不同收入组的收敛机制不同，低收入组更常受到外援、基数和数据缺失影响。",
           tone = "secondary")
    ),
    rail = list(
      list(title = "看 beta 图", text = "判断起点和增长率的方向。"),
      list(title = "看 sigma 线", text = "验证跨国离散度是否真的下降。"),
      list(title = "拆分收入组", text = "确认收敛是否只来自某一组。"),
      list(title = "查明细表", text = "定位高增长和低增长国家。")
    ),
    note = "收敛分析对起止年份敏感，短窗口可能捕捉到冲击恢复而不是长期制度趋同。"
  ),
  distribution = list(
    badge_tone = "secondary",
    badges = c("分布形态", "偏度", "分位数", "异常尾部"),
    insights = list(
      list(kicker = "Shape", title = "先判断分布形状",
           text = "卫生支出和 GDP 常呈右偏分布，均值容易被极端高值拉动，中位数和分位数更稳健。",
           tone = "primary"),
      list(kicker = "Tail", title = "尾部决定政策关注点",
           text = "高端尾部反映高支出系统，低端尾部则指向资源不足和数据覆盖问题。",
           tone = "warn"),
      list(kicker = "Evolution", title = "分布移动比单点更有信息",
           text = "跨年箱线图能同时展示整体抬升、离散度变化和极端值扩张。",
           tone = "good")
    ),
    rail = list(
      list(title = "选变量", text = "决定观察金额、比例还是健康产出。"),
      list(title = "选年份", text = "固定分布横截面。"),
      list(title = "切换分组", text = "比较收入组或大洲内部分布。"),
      list(title = "看统计表", text = "用分位数和偏度补充视觉判断。")
    ),
    note = "对金额变量建议结合对数视角阅读；原始尺度适合看绝对差距，对数尺度适合看相对结构。"
  ),
  decomposition = list(
    badge_tone = "primary",
    badges = c("增长分解", "人口/价格/强度", "CAGR", "结构变化"),
    insights = list(
      list(kicker = "Drivers", title = "把总增长拆开",
           text = "CHE 总量变化通常来自人口规模、经济水平、卫生支出强度和价格口径的共同作用。",
           tone = "primary"),
      list(kicker = "Intensity", title = "强度变化是政策信号",
           text = "当 GDP 与人口不能解释支出增速时，剩余部分更可能反映制度扩张、价格或服务利用变化。",
           tone = "secondary"),
      list(kicker = "CAGR", title = "增长率需要和基数一起看",
           text = "低基数国家可能出现高 CAGR，但绝对支出水平仍然有限。",
           tone = "warn")
    ),
    rail = list(
      list(title = "设定窗口", text = "定义分解起止年份。"),
      list(title = "读贡献条", text = "比较各驱动项对总增长的贡献。"),
      list(title = "看 CAGR", text = "区分高速增长和高水平。"),
      list(title = "查表", text = "复核国家级贡献和起止值。")
    ),
    note = "分解结果是会计恒等或近似贡献，不等同于因果归因；适合回答增长来自哪里，而不是为什么发生。"
  ),
  extremes = list(
    badge_tone = "bad",
    badges = c("异常值", "稳健阈值", "尾部国家", "特征对照"),
    insights = list(
      list(kicker = "Detection", title = "异常值先分类再解释",
           text = "高支出、高自付、低寿命和高外援属于不同异常机制，需要分别进入对应模块复核。",
           tone = "bad"),
      list(kicker = "Method", title = "阈值只负责发现",
           text = "箱线、分位数或 z-score 能发现尾部，但不能判断异常是错误、真实冲击还是制度特征。",
           tone = "warn"),
      list(kicker = "Profile", title = "特征对照给出线索",
           text = "把异常国家与同组国家比较，可以快速判断偏离来自经济水平、筹资结构还是产出缺口。",
           tone = "good")
    ),
    rail = list(
      list(title = "选指标", text = "决定异常定义。"),
      list(title = "调阈值", text = "平衡发现数量与噪声。"),
      list(title = "看趋势", text = "区分长期异常与一次性冲击。"),
      list(title = "查清单", text = "对具体国家做二次核验。")
    ),
    note = "异常值不是坏数据的同义词；卫生支出中很多极端点来自真实制度差异、冲突冲击或小国规模效应。"
  ),
  fiscal = list(
    badge_tone = "secondary",
    badges = c("财政优先度", "GGHE/GDP", "政府占比", "预算空间"),
    insights = list(
      list(kicker = "Priority", title = "财政优先度是政策选择",
           text = "GGHE-D 与 GDP 中政府卫生支出的比例共同反映卫生在公共预算中的相对位置。",
           tone = "primary"),
      list(kicker = "Space", title = "空间不足常表现为双低",
           text = "低 GDP、低公共占比和高 OOPS 的组合，通常意味着财政托底能力不足。",
           tone = "bad"),
      list(kicker = "Quadrant", title = "象限图适合快速分类",
           text = "高财政优先度但低产出、低财政优先度但高自付等组合，指向不同政策问题。",
           tone = "good")
    ),
    rail = list(
      list(title = "选择年份", text = "固定财政横截面。"),
      list(title = "看散点", text = "定位财政优先度与支出水平关系。"),
      list(title = "看象限", text = "把国家归入政策类型。"),
      list(title = "看地图/表", text = "输出重点国家清单。")
    ),
    note = "财政指标受政府会计口径和卫生账户分类影响，比较时应优先看相对位置和同组差异。"
  ),
  growth = list(
    badge_tone = "good",
    badges = c("CAGR", "收入弹性", "追赶效应", "年度波动"),
    insights = list(
      list(kicker = "Growth", title = "增长率要避开单年噪声",
           text = "CAGR 用起止值概括窗口内变化，比单年同比更适合跨国比较。",
           tone = "primary"),
      list(kicker = "Elasticity", title = "弹性揭示卫生支出是否跑赢经济",
           text = "弹性大于 1 表示 CHE 增速快于 GDP，可能来自覆盖扩张、价格上升或服务利用增加。",
           tone = "secondary"),
      list(kicker = "Volatility", title = "持续性需要看年度轨迹",
           text = "高 CAGR 如果来自少数年份跳升，政策含义不同于平稳长期增长。",
           tone = "warn")
    ),
    rail = list(
      list(title = "设窗口", text = "定义增长比较期间。"),
      list(title = "看分布", text = "确认各组增长中位数和离散度。"),
      list(title = "看弹性", text = "比较 CHE 与 GDP 的同步性。"),
      list(title = "查排名", text = "定位增长最快和最慢国家。")
    ),
    note = "增长率对起止值质量非常敏感；如果某国起始值接近零，需回到表格确认是否存在缺失或口径突变。"
  ),
  inequality = list(
    badge_tone = "bad",
    badges = c("Gini", "Theil", "Lorenz", "人口加权"),
    insights = list(
      list(kicker = "Equity", title = "不平等不是平均差距",
           text = "Gini 和 Theil 关注分布整体形状，能揭示少数国家或群组对差距的贡献。",
           tone = "bad"),
      list(kicker = "Weighting", title = "人口加权改变结论",
           text = "按国家等权和按人口加权回答不同问题：国家系统差异与全球人口实际暴露并不相同。",
           tone = "warn"),
      list(kicker = "Decomposition", title = "Theil 可拆分组间和组内",
           text = "如果组内贡献高，说明同一收入组或区域内部仍存在显著差距。",
           tone = "good")
    ),
    rail = list(
      list(title = "选指标", text = "确定不平等对象。"),
      list(title = "看趋势", text = "判断差距是否收敛。"),
      list(title = "看 Lorenz", text = "理解分布集中程度。"),
      list(title = "看分解", text = "区分组间和组内来源。")
    ),
    note = "不平等指标适合比较分布结构，不直接说明谁的政策更有效；解释时要结合收入、人口和数据覆盖。"
  ),
  policy = list(
    badge_tone = "secondary",
    badges = c("国家诊断", "同伴对标", "优先级", "建议生成"),
    insights = list(
      list(kicker = "Diagnosis", title = "先形成四维画像",
           text = "财务保护、自主筹资、预防投入和寿命产出共同描述国家卫生筹资的政策短板。",
           tone = "primary"),
      list(kicker = "Peers", title = "同收入组比全球平均更有用",
           text = "政策建议优先与同收入组国家对照，避免用不可达的高收入基准误导判断。",
           tone = "good"),
      list(kicker = "Action", title = "建议是可讨论的优先级",
           text = "生成结果用于组织政策备忘录，不替代具体预算测算或制度设计。",
           tone = "warn")
    ),
    rail = list(
      list(title = "选国家", text = "读取最新可用年份。"),
      list(title = "选优先域", text = "决定建议生成范围。"),
      list(title = "读雷达", text = "识别相对短板。"),
      list(title = "看趋势", text = "确认问题是长期存在还是近期变化。")
    ),
    note = "政策建议使用启发式规则生成，重点是透明和可解释；正式政策评估仍需结合本国制度文本与预算数据。"
  ),
  prevention = list(
    badge_tone = "good",
    badges = c("HC6", "预防支出", "寿命关联", "治疗结构"),
    insights = list(
      list(kicker = "Prevention", title = "预防占比反映前端投入",
           text = "HC6 高低提示卫生系统是否把资源配置到免疫、筛查、健康促进和公共卫生能力。",
           tone = "good"),
      list(kicker = "Balance", title = "预防和治疗需要一起看",
           text = "低预防、高治疗占比可能说明系统更偏向事后服务，但也可能来自疾病负担和记账口径。",
           tone = "warn"),
      list(kicker = "Outcome", title = "寿命关联不是即时效果",
           text = "预防投入对寿命的影响有滞后，截面散点更适合作为结构提示而非因果估计。",
           tone = "primary")
    ),
    rail = list(
      list(title = "固定年份", text = "看当前预防投入水平。"),
      list(title = "对照寿命", text = "寻找低投入低产出组合。"),
      list(title = "看趋势", text = "判断预防是否逐步抬升。"),
      list(title = "看地图", text = "识别区域集中模式。")
    ),
    note = "HC6 统计口径在国家间可能存在分类差异，尤其是公共卫生项目和初级保健边界。"
  ),
  purpose = list(
    badge_tone = "primary",
    badges = c("HC 功能项", "治疗/预防", "用途结构", "服务重心"),
    insights = list(
      list(kicker = "Function", title = "用途结构回答钱花在哪里",
           text = "HC1、HC6 等功能项把卫生支出从资金来源转到服务用途，是解释结构效率的重要入口。",
           tone = "primary"),
      list(kicker = "Mix", title = "治疗和预防的比例需要平衡",
           text = "治疗支出高并不必然低效，但若预防长期偏低，可能提示服务体系过度后端化。",
           tone = "warn"),
      list(kicker = "Groups", title = "分组比较揭示制度重心",
           text = "不同收入组的功能结构差异，常常反映疾病谱、价格、支付制度和服务供给能力。",
           tone = "good")
    ),
    rail = list(
      list(title = "选年份", text = "固定用途结构。"),
      list(title = "看柱图", text = "比较主要 HC 类别。"),
      list(title = "看散点", text = "定位治疗与预防关系。"),
      list(title = "查表", text = "导出国家级功能项比例。")
    ),
    note = "功能项之和受 GHED 分类完整度影响，缺失类别较多的国家需要结合数据质量模块复核。"
  ),
  ranking = list(
    badge_tone = "secondary",
    badges = c("排名", "Top/Bottom", "位次变化", "分组排行"),
    insights = list(
      list(kicker = "Rank", title = "排名适合定位，不适合单独解释",
           text = "位次能快速找出头部和尾部国家，但相邻排名之间可能只有很小数值差异。",
           tone = "primary"),
      list(kicker = "Change", title = "排名变化要结合值变化",
           text = "位次上升可能来自本国改善，也可能来自其他国家下降或数据缺失。",
           tone = "warn"),
      list(kicker = "Context", title = "分组排名降低结构偏差",
           text = "同收入组内的排名通常比全球排名更适合做政策比较。",
           tone = "good")
    ),
    rail = list(
      list(title = "选指标", text = "确定排名对象。"),
      list(title = "选方向", text = "确认高值好还是低值好。"),
      list(title = "看变化", text = "比较当年和基准年位次。"),
      list(title = "查表", text = "复核值、组别和变动幅度。")
    ),
    note = "排名视觉很强，但政策含义应以实际数值差距和趋势为准。"
  ),
  regional = list(
    badge_tone = "primary",
    badges = c("区域比较", "大洲/收入组", "趋势", "空间分布"),
    insights = list(
      list(kicker = "Region", title = "区域是第一层聚合",
           text = "大洲和收入组能快速揭示卫生支出的空间格局与发展阶段差异。",
           tone = "primary"),
      list(kicker = "Within", title = "组内差异同样重要",
           text = "箱线图展示同一地区内部的离散度，避免用单个均值代表整组国家。",
           tone = "warn"),
      list(kicker = "Map", title = "地图帮助发现邻近模式",
           text = "空间分布能暴露区域集群、边界效应和小国高波动现象。",
           tone = "good")
    ),
    rail = list(
      list(title = "选指标", text = "确定区域比较维度。"),
      list(title = "看趋势", text = "识别长期分化或收敛。"),
      list(title = "看箱线", text = "判断组内离散度。"),
      list(title = "看地图", text = "把聚合结果落回国家空间。")
    ),
    note = "区域平均可能掩盖人口规模和国家数量差异；必要时结合国家表和人口加权指标。"
  ),
  sdg = list(
    badge_tone = "good",
    badges = c("SDG-3", "寿命/U5MR", "支出关联", "改善速度"),
    insights = list(
      list(kicker = "Outcome", title = "产出指标衡量卫生结果",
           text = "寿命、儿童死亡和 UHC 相关指标把支出问题转化为健康结果问题。",
           tone = "good"),
      list(kicker = "Efficiency", title = "高支出不自动等于高产出",
           text = "散点图中同等支出下产出更好的国家，值得进入效率模块进一步分析。",
           tone = "primary"),
      list(kicker = "Progress", title = "改善速度能提示追赶",
           text = "Top improvers 揭示哪些国家在资源约束下仍实现了健康产出快速改善。",
           tone = "secondary")
    ),
    rail = list(
      list(title = "选结果", text = "决定 SDG 阅读对象。"),
      list(title = "看支出关联", text = "判断投入和产出是否同步。"),
      list(title = "看趋势", text = "确认全球进展方向。"),
      list(title = "看改善者", text = "定位高进步国家。")
    ),
    note = "SDG 结果指标存在滞后和多因素决定，不能只用当年支出解释当年产出。"
  ),
  timeline = list(
    badge_tone = "primary",
    badges = c("时间序列", "同比", "结构面积", "波动率"),
    insights = list(
      list(kicker = "Path", title = "时间线保留变化顺序",
           text = "国家或分组的年度轨迹能区分平稳增长、平台期、突变和冲击后恢复。",
           tone = "primary"),
      list(kicker = "YoY", title = "同比突出短期冲击",
           text = "年度增长率能发现疫情、财政危机或口径变化带来的跳变。",
           tone = "warn"),
      list(kicker = "Structure", title = "面积图说明结构替代",
           text = "公共、私人和外援占比的时序变化能揭示筹资责任转移。",
           tone = "good")
    ),
    rail = list(
      list(title = "选国家/组", text = "定义追踪对象。"),
      list(title = "选指标", text = "决定主时间线口径。"),
      list(title = "看同比", text = "识别短期冲击年份。"),
      list(title = "看结构", text = "判断资金来源是否发生替代。")
    ),
    note = "时间序列中的突变可能来自真实冲击，也可能来自数据修订；异常年份需结合表格和原始来源复核。"
  ),
  transition = list(
    badge_tone = "secondary",
    badges = c("收入组晋升", "筹资转型", "EXT -> GGHED", "路径追踪"),
    insights = list(
      list(kicker = "Upgrade", title = "收入晋升改变筹资格局",
           text = "随着收入组上移，外援通常下降，国内公共筹资和预付机制需要承担更多责任。",
           tone = "secondary"),
      list(kicker = "Path", title = "轨迹比前后差值更清楚",
           text = "EXT-GGHED 平面上的路径能展示国家是否从依赖外援转向政府主导。",
           tone = "primary"),
      list(kicker = "Risk", title = "转型期最怕保护缺口",
           text = "如果外援下降快于公共筹资补位，家庭自付和服务覆盖可能承受压力。",
           tone = "warn")
    ),
    rail = list(
      list(title = "设定时段", text = "定义收入组变化窗口。"),
      list(title = "筛转型类型", text = "聚焦特定晋升路径。"),
      list(title = "看轨迹", text = "判断筹资责任转移方向。"),
      list(title = "看前后变化", text = "确认 OOPS 是否同步改善。")
    ),
    note = "收入组分类来自年度口径，短期上下跳动不一定代表真实制度转型，应结合多年轨迹判断。"
  )
)

mod_v3_topic_specs <- function(topic) {
  spec <- mod_v3_topic_registry[[topic]]
  if (!is.null(spec)) return(spec)
  list(
    badge_tone = "neutral",
    badges = c("交互分析", "筛选", "图表", "明细表"),
    insights = list(
      list(kicker = "Read", title = "先看全局，再进细节",
           text = "从 KPI 和主图判断总体方向，再用分组图、地图和表格复核国家差异。",
           tone = "primary"),
      list(kicker = "Compare", title = "比较需要固定口径",
           text = "年份、指标和分组口径改变时，结论也会随之改变。",
           tone = "warn"),
      list(kicker = "Export", title = "表格用于复核",
           text = "图表负责发现问题，表格负责确认数值、国家和样本范围。",
           tone = "good")
    ),
    rail = list(
      list(title = "设置参数", text = "先明确时间、国家和指标。"),
      list(title = "阅读主图", text = "判断主要模式。"),
      list(title = "查看分组", text = "识别异质性。"),
      list(title = "复核明细", text = "用表格确认样本。")
    ),
    note = "交互结果会随筛选器实时变化，截图或引用结论时请同时记录年份、指标和筛选条件。"
  )
}

mod_v3_topic_brief <- function(topic, columns = 3, include_rail = TRUE,
                               include_note = TRUE) {
  spec <- mod_v3_topic_specs(topic)
  badges <- do.call(
    mod_v3_badge_row,
    c(as.list(spec$badges %||% character(0)),
      list(tone = spec$badge_tone %||% "primary"))
  )
  insight_tags <- lapply(spec$insights %||% list(), function(it) {
    do.call(mod_v3_insight, it)
  })
  story <- do.call(mod_v3_story_grid,
                   c(insight_tags, list(columns = columns,
                                        class = "v3-topic-brief")))
  htmltools::tagList(
    badges,
    story,
    if (include_rail) mod_v3_rail(spec$rail %||% list()),
    if (include_note) {
      mod_v3_source_note(title = "阅读提示", spec$note %||% "")
    }
  )
}

#' 有编号的方法步骤
mod_v3_steps <- function(items) {
  if (!length(items)) return(htmltools::HTML(""))
  item_names <- names(items)
  htmltools::div(
    class = "v3-steps",
    lapply(seq_along(items), function(i) {
      it <- items[[i]]
      title <- if (is.list(it)) {
        it$title %||% ""
      } else if (!is.null(item_names) && length(item_names) >= i &&
                 nzchar(item_names[[i]])) {
        item_names[[i]]
      } else {
        ""
      }
      body <- if (is.list(it)) it$body %||% it$text %||% "" else it
      htmltools::div(
        class = "v3-step",
        htmltools::span(class = "v3-step-index", sprintf("%02d", i)),
        htmltools::div(
          class = "v3-step-copy",
          htmltools::strong(title),
          htmltools::p(body)
        )
      )
    })
  )
}

#' 代码块
mod_v3_code_block <- function(code, title = NULL) {
  htmltools::div(
    class = "v3-code-wrap",
    if (!is.null(title)) htmltools::div(class = "v3-code-title", title),
    htmltools::tags$pre(
      class = "v3-code-block",
      htmltools::tags$code(code)
    )
  )
}

#' 来源 / 注释脚注
mod_v3_source_note <- function(..., title = "Source note") {
  htmltools::div(
    class = "v3-source-note",
    htmltools::strong(title),
    htmltools::span(...)
  )
}

#' callout（4 tones：info/good/warn/bad）
mod_v3_callout <- function(text, tone = c("info", "good", "warn", "bad"),
                            title = NULL) {
  tone <- match.arg(tone)
  title_html <- if (!is.null(title) && nchar(title))
    sprintf("<strong class='v3-callout-title'>%s</strong>",
            htmltools::htmlEscape(title))
  else ""
  htmltools::HTML(sprintf(
    paste0("<aside class='v3-callout v3-callout-%s'>",
           "%s<div class='v3-callout-body'>%s</div></aside>"),
    tone, title_html,
    if (inherits(text, "html") || inherits(text, "shiny.tag"))
      as.character(text) else htmltools::htmlEscape(text)))
}

#' 模块卡（用于 overview 内的"模块导航"）
#' @param target 目标 nav id（用于 nav_select）
#' @param kicker eyebrow 文字
#' @param title 模块名
#' @param desc 一句话描述
#' @param icon 简短 icon（emoji 或 unicode 字符）
#' @param tone 主色：primary/secondary/good/warn/bad
mod_v3_module_card <- function(ns, target, kicker, title, desc,
                                icon = "\u25b8",
                                tone = "primary") {
  tone_color <- switch(tone,
    primary = "#1d3f5f", secondary = "#c46327",
    good = "#2a857a", warn = "#c89a3b", bad = "#a23b3b",
    "#5d667a")
  htmltools::tags$button(
    class = "v3-module-card",
    type  = "button",
    style = sprintf("--tone:%s", tone_color),
    onclick = sprintf(
      "Shiny.setInputValue('%s', '%s', {priority:'event'});",
      ns("nav_to"), target),
    htmltools::span(class = "v3-module-icon", htmltools::HTML(icon)),
    htmltools::span(class = "v3-module-kicker", kicker),
    htmltools::strong(class = "v3-module-title", title),
    htmltools::span(class = "v3-module-desc", desc)
  )
}

#' stat strip（横向数据条；适合长 KPI 行）
mod_v3_stat_strip <- function(items) {
  if (!length(items)) return(htmltools::HTML(""))
  cards <- vapply(seq_along(items), function(i) {
    it <- items[[i]]
    sprintf(
      paste0("<div class='v3-stat-cell'>",
             "<div class='v3-stat-value'>%s</div>",
             "<div class='v3-stat-label'>%s</div></div>"),
      htmltools::htmlEscape(it$value %||% "\u2014"),
      htmltools::htmlEscape(it$label %||% ""))
  }, character(1))
  htmltools::HTML(sprintf("<div class='v3-stat-strip'>%s</div>",
                          paste(cards, collapse = "")))
}

#' 卡片容器（升级版 mod_card；带可选 kicker / footer）
mod_v3_card <- function(..., title = NULL, kicker = NULL, footer = NULL,
                          class = NULL) {
  head_html <- ""
  if (!is.null(kicker) || !is.null(title)) {
    head_html <- htmltools::div(
      class = "v3-card-head",
      if (!is.null(kicker))
        htmltools::span(class = "v3-card-kicker",
                         htmltools::htmlEscape(kicker)),
      if (!is.null(title))
        htmltools::h4(class = "v3-card-title",
                       htmltools::htmlEscape(title))
    )
  }
  htmltools::div(
    class = mod_class("v3-card", class),
    head_html,
    htmltools::div(class = "v3-card-body", ...),
    if (!is.null(footer))
      htmltools::div(class = "v3-card-footer", footer)
  )
}

#' 应用 plotly 主题（在 server 端 plotly 输出前调用）
mod_v3_plotly <- function(p, dark = FALSE) {
  if (exists("ghs_plotly_layout", mode = "function")) {
    return(ghs_plotly_layout(p, theme = if (dark) "dark" else "light"))
  }
  p
}

#' 应用 leaflet 品牌底图
mod_v3_leaflet <- function(map = NULL, dark = FALSE) {
  if (exists("ghs_leaflet_provider", mode = "function")) {
    return(ghs_leaflet_provider(map, theme = if (dark) "dark" else "light"))
  }
  if (is.null(map)) leaflet::leaflet() else map
}

#' reactable 默认主题包装
mod_v3_reactable <- function(data, ..., dark = FALSE) {
  args <- list(data = data, ...)
  if (exists("ghs_reactable_theme", mode = "function") &&
      !"theme" %in% names(args)) {
    args$theme <- ghs_reactable_theme(if (dark) "dark" else "light")
  }
  do.call(reactable::reactable, args)
}

#' 简易 fmt 助手（USD / 百分比 / 大数）
fmt_v3_usd <- function(x, digits = 0) {
  if (!length(x) || !is.finite(x)) return("\u2014")
  if (abs(x) >= 1e12) return(sprintf("$%.2fT", x / 1e12))
  if (abs(x) >= 1e9)  return(sprintf("$%.2fB", x / 1e9))
  if (abs(x) >= 1e6)  return(sprintf("$%.1fM", x / 1e6))
  if (abs(x) >= 1e3)  return(sprintf("$%s",
                                    format(round(x, digits), big.mark = ",")))
  sprintf("$%s", format(round(x, digits), big.mark = ","))
}

fmt_v3_pct <- function(x, digits = 1) {
  if (!length(x) || !is.finite(x)) return("\u2014")
  paste0(format(round(x, digits), nsmall = digits), "%")
}

fmt_v3_num <- function(x, digits = 0, suffix = "") {
  if (!length(x) || !is.finite(x)) return("\u2014")
  paste0(format(round(x, digits), big.mark = ",", nsmall = digits), suffix)
}

# =============================================================================
# 语义别名（移除版本后缀，保持向后兼容）
# =============================================================================
mod_hero <- mod_v3_hero
mod_section_head <- mod_v3_section_head
mod_kpi_card <- mod_v3_kpi
mod_kpi_grid <- mod_v3_kpi_grid
mod_callout <- mod_v3_callout
mod_module_card <- mod_v3_module_card
mod_stat_strip <- mod_v3_stat_strip
mod_plotly <- mod_v3_plotly
mod_leaflet <- mod_v3_leaflet
mod_reactable_themed <- mod_v3_reactable
fmt_usd_v <- fmt_v3_usd
fmt_pct_v <- fmt_v3_pct
fmt_num_v <- fmt_v3_num
