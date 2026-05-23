
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

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

mod_spinner <- function(x, color = "#1B5E88") {
  shinycssloaders::withSpinner(x, type = 6, color = color, size = 0.6)
}

mod_class <- function(...) {
  x <- unlist(list(...), use.names = FALSE)
  x <- x[!is.na(x) & nzchar(x)]
  paste(unique(x), collapse = " ")
}

mod_card <- function(..., title = NULL, class = NULL,
                     kicker = NULL, footer = NULL) {
  mod_v3_card(...,
              title = title,
              kicker = kicker,
              footer = footer,
              class = mod_class("panel-content", "legacy-panel-card", class))
}

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

mod_empty_message <- function(msg = "数据不足") {
  htmltools::div(
    class = "panel-content text-muted text-center",
    style = "padding: 60px 20px;",
    htmltools::h4(msg),
    htmltools::p("调整左侧过滤器或选择不同时段")
  )
}

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

mod_kpi_row <- function(...) {
  cards <- list(...)
  bslib::layout_columns(
    col_widths = rep(3, length(cards)),
    !!!cards
  )
}


mod_v3_widget_url <- function(file) {
  paste0(
    "https://2711944586.github.io/R/交互组件/",
    utils::URLencode(file, reserved = TRUE)
  )
}

mod_v3_widget_placeholder <- function(title) {
  paste0(
    "<!doctype html><html><head><meta charset='utf-8'>",
    "<style>body{margin:0;min-height:100vh;display:grid;place-items:center;",
    "background:#fffaf2;color:#5d667a;font-family:system-ui,'Microsoft YaHei',sans-serif}",
    "main{text-align:center;padding:24px}b{display:block;color:#254f5c;margin-bottom:8px}</style>",
    "</head><body><main><b>", htmltools::htmlEscape(title),
    "</b><span>滚动到这里后加载完整交互图</span></main></body></html>"
  )
}

mod_v3_widget_topic <- function(kicker, title, lead) {
  primary <- paste(kicker, title)
  txt <- paste(primary, lead)
  match_topic <- function(source) {
    rules <- list(
      c("widgets", "交互组件|\\bWidgets\\b|INTERACTION LIBRARY"),
      c("overview", "Global Health Expenditure|总览|\\bOverview\\b|长卷"),
      c("about", "项目说明|交互仪表盘说明|关于|\\bAbout\\b"),
      c("methods", "方法|数据说明|\\bMethods\\b|手册"),
      c("forecast", "预测|\\bForecast\\b|ARIMA|\\bETS\\b|预测实验室"),
      c("scenarios", "情景|\\bScenarios\\b|蒙特卡洛"),
      c("country", "国家画像|单国|\\bCountry\\b"),
      c("regional", "区域对比|区域|大洲|\\bRegional\\b"),
      c("ranking", "排行|排名|\\bRanking\\b"),
      c("benchmark", "对标|基准|\\bBenchmark\\b"),
      c("purpose", "功能分类|用途|\\bPurpose\\b|HC1|SPENDING BY PURPOSE"),
      c("financing", "筹资结构|风险分担|\\bFinancing\\b|HF1|HF4"),
      c("spending", "支出水平|人均 CHE|支出规模|\\bSpending\\b"),
      c("aid", "外援|援助|外部援助|\\bAid\\b|EXT"),
      c("fiscal", "财政|\\bFiscal\\b|政府"),
      c("equity", "公平|\\bEquity\\b|财务保护|灾难性"),
      c("inequality", "不平等|\\bInequality\\b|Theil|Gini"),
      c("efficiency", "效率|\\bEfficiency\\b|DEA|效率前沿|残差"),
      c("convergence", "收敛|\\bConvergence\\b|beta|sigma"),
      c("decomposition", "分解|\\bDecompose\\b|\\bDecomposition\\b"),
      c("outcomes", "健康产出|\\bOutcomes\\b|资金.*寿命|寿命.*产出|life_exp"),
      c("sdg", "SDG|UHC|U5MR|健康目标"),
      c("prevention", "预防|\\bPrevention\\b|HC6"),
      c("aging", "老龄化|\\bAging\\b|人口结构"),
      c("pandemic", "疫情|\\bPandemic\\b|韧性|COVID|冲击"),
      c("growth", "增长|\\bGrowth\\b|CAGR|弹性"),
      c("transition", "转型|\\bTransition\\b|收入晋升"),
      c("timeline", "时间线|\\bTimeline\\b|24-YEAR"),
      c("extremes", "极值|异常国家|\\bExtremes\\b|极端值"),
      c("compare", "多国多指标对比|国家对比|\\bCompare\\b"),
      c("cluster", "聚类|\\bCluster\\b|PCA|k-means|类型发现"),
      c("mapstudio", "Map Studio|地图工作台|SPATIAL ANALYTICS"),
      c("correlation", "相关|\\bCorrelation\\b|Spearman|变量关联"),
      c("distribution", "指标分布|统计特征|\\bDistribution\\b|偏度|统计分布"),
      c("robustness", "稳健|\\bRobustness\\b|Bootstrap|敏感度"),
      c("dataquality", "数据质量|\\bData Quality\\b|缺失|覆盖"),
      c("policy", "政策|\\bPolicy\\b|政策顾问"),
      c("atlas", "\\bAtlas\\b|数据图谱|字段|资产")
    )
    for (rule in rules) {
      if (grepl(rule[[2]], source, ignore.case = TRUE, perl = TRUE)) {
        return(rule[[1]])
      }
    }
    NULL
  }
  topic <- match_topic(primary)
  if (!is.null(topic)) return(topic)
  topic <- match_topic(txt)
  if (!is.null(topic)) return(topic)
  "overview"
}

mod_v3_widget_specs <- function(topic) {
  spec <- function(file, title, kicker, desc) {
    list(file = file, title = title, kicker = kicker, desc = desc)
  }
  common <- list(
    spec("01_gapminder_animated.html", "动态气泡：支出、GDP 与寿命", "Plotly animation",
         "用年份动画把经济水平、健康产出和人均卫生支出放在同一张图里。"),
    spec("11_world_leaflet.html", "世界地图：空间分布", "Leaflet map",
         "悬停国家查看指标标签，先把全球差异落到地理位置。")
  )
  specs <- switch(topic,
    widgets = list(
      spec("iadv_bar_race.html", "动态排行：高支出国家变化", "Motion ranking",
           "用年份帧观察排行重排，适合快速进入完整组件库。"),
      spec("iadv_sankey_3stage.html", "三段资金流", "Network flow",
           "把来源、筹资方案和用途串起来看，补足单图比例的阅读。")
    ),
    overview = common,
    about = list(
      spec("iadv_gauge_grid.html", "指标仪表组", "HTML gauges",
           "把关键指标和风险信号整合为可悬停的仪表化面板。"),
      spec("13_reactable_rank.html", "国家排行表", "Reactable",
           "用可搜索、可排序的表格把图上的模式落回国家明细。")
    ),
    methods = list(
      spec("iadv_dt_master_browse.html", "主面板浏览", "DT table",
           "按国家和年份复核宽表字段，检查每张图背后的记录。"),
      spec("31_corr_matrix.html", "指标相关矩阵", "Plotly matrix",
           "查看核心变量的相关结构，辅助判断后续模型设定。")
    ),
    country = list(
      spec("33_country_compare.html", "国家对比：核心指标轨迹", "Plotly compare",
           "把多个国家的支出和保护指标放在同一屏里比较。"),
      spec("13_highlight_lines.html", "多国高亮时序", "Plotly lines",
           "点击图例隔离国家，观察长期距离和追赶路径。")
    ),
    regional = list(
      spec("iadv_rt_continent_summary.html", "大洲摘要表", "Reactable",
           "从区域表格读取大洲差异，再回到趋势和箱线图。"),
      spec("iadv_continent_ribbon.html", "大洲带状趋势", "Plotly ribbon",
           "用带状图展示区域均值和波动范围。")
    ),
    ranking = list(
      spec("iadv_oop_rank_latest.html", "OOPS 最新排行", "Reactable",
           "锁定自付压力最高和最低的国家，便于继续追踪。"),
      spec("iadv_bar_race.html", "动态排行重排", "Plotly frame",
           "用动画查看排名是否只是单年结果。")
    ),
    benchmark = list(
      spec("iadv_polar_radar.html", "多维雷达画像", "Plotly radar",
           "把充足性、公平性和效率放到同一张剖面图里。"),
      spec("iadv_rt_compare_years.html", "年份对标表", "Reactable",
           "用表格核查基准差距和跨期变化。")
    ),
    financing = list(
      spec("09_sankey_sources.html", "资金来源 Sankey", "Sankey",
           "把公共、私人、外援和自付的流向关系直接画出来。"),
      spec("iadv_donut_finance.html", "筹资结构环图", "Plotly donut",
           "用结构图快速判断风险主要落在政府、家庭还是外部资金。")
    ),
    spending = list(
      spec("iadv_che_pc_lines.html", "人均 CHE 趋势线", "Plotly lines",
           "对比主要国家长期支出距离和增长斜率。"),
      spec("13_leaflet_choropleth.html", "人均支出世界地图", "Leaflet",
           "把支出梯度放回全球空间格局中阅读。")
    ),
    purpose = list(
      spec("iadv_hc_stream.html", "HC 功能流图", "HTML stream",
           "观察治疗、预防和管理等功能项随时间的结构流动。"),
      spec("iadv_hc_wheel.html", "功能用途轮图", "HTML wheel",
           "用环形层级图展示 HC 功能项的组成关系。")
    ),
    aid = list(
      spec("35_ext_top_bar.html", "外援依赖 Top 国家", "Plotly bar",
           "快速识别 EXT 占比高的国家和潜在退出风险。"),
      spec("iadv_dt_extdep.html", "外援依赖明细表", "DT table",
           "用可筛选表格核查高依赖国家的支出与产出背景。")
    ),
    fiscal = list(
      spec("iadv_dt_gghed_oop.html", "公共筹资与自付表", "DT table",
           "并列检查 GGHED 和 OOPS，定位财政托底不足的国家。"),
      spec("36_global_hf.html", "全球筹资结构趋势", "Plotly",
           "观察公共、私人和外援份额的长期替代关系。")
    ),
    equity = list(
      spec("07_inequality.html", "不平等指数交互图", "Plotly",
           "追踪支出不平等和人口权重下的分布变化。"),
      spec("13_oops_heatmap.html", "OOPS 国家年份热力图", "Heatmap",
           "找出自付压力持续高位或发生突变的国家。")
    ),
    inequality = list(
      spec("iadv_ribbon_quantiles.html", "分位数带状图", "Plotly ribbon",
           "用分位数带查看分布是否正在收窄或拉开。"),
      spec("iadv_box_continent.html", "大洲箱线分布", "Plotly box",
           "比较组内离散度和离群点。")
    ),
    efficiency = list(
      spec("28_che_life.html", "支出与寿命关系", "Plotly scatter",
           "在同等支出水平下识别健康产出偏高或偏低的国家。"),
      spec("iadv_density2d.html", "二维密度分布", "Plotly density",
           "用密度层识别主体国家群和异常点。")
    ),
    convergence = list(
      spec("iadv_che_pc_lines.html", "支出收敛趋势", "Plotly lines",
           "观察国家间人均支出距离是否随时间缩小。"),
      spec("iadv_rt_growth_champions.html", "增长冠军表", "Reactable",
           "定位低起点高增长或长期停滞国家。")
    ),
    decomposition = list(
      spec("iadv_waterfall_che.html", "CHE 增长瀑布图", "Plotly waterfall",
           "把总变化拆成可解释的增量结构。"),
      spec("iadv_waterfall_continents.html", "大洲贡献瀑布图", "Plotly waterfall",
           "查看全球变化主要由哪些区域推动。")
    ),
    outcomes = list(
      spec("28_che_life.html", "CHE 与预期寿命", "Plotly scatter",
           "比较支出投入和健康产出是否同步改善。"),
      spec("29_che_u5.html", "CHE 与儿童死亡率", "Plotly scatter",
           "把卫生支出和 U5MR 改善放在同一横截面里。")
    ),
    sdg = list(
      spec("iadv_dt_sdg38_alarm.html", "SDG 3.8 风险表", "DT table",
           "筛选需要关注的 UHC 和风险保护国家。"),
      spec("iadv_lifeexp_byinc.html", "收入组寿命趋势", "Plotly",
           "对照收入组间健康产出距离。")
    ),
    prevention = list(
      spec("iadv_hc_item.html", "预防功能项组件", "HTML widget",
           "把 HC 预防支出拆到功能项层级。"),
      spec("iadv_lifeexp_byinc.html", "预防与产出参照", "Plotly",
           "把投入结构和寿命结果放到同一分析线索中。")
    ),
    aging = list(
      spec("38_life_top_bar.html", "寿命 Top 国家", "Plotly bar",
           "先看长寿国家，再回到老龄化压力和支出响应。"),
      spec("iadv_dualaxis_oop_lifeexp.html", "OOPS 与寿命双轴", "Plotly dual axis",
           "同屏查看财务压力与健康产出变化。")
    ),
    pandemic = list(
      spec("iadv_area_shock_bands.html", "冲击期带状面积图", "Plotly bands",
           "突出 2020 前后卫生支出的异常波动。"),
      spec("iadv_rt_shock_response.html", "冲击响应表", "Reactable",
           "用国家明细确认疫情期变化幅度。")
    ),
    growth = list(
      spec("26_che_cagr_bar.html", "CHE CAGR 排行", "Plotly bar",
           "用复合增速识别长期增长最快的国家。"),
      spec("iadv_dt_growth_desc.html", "增长明细表", "DT table",
           "筛选增长、基数和收入组信息。")
    ),
    transition = list(
      spec("iadv_sankey_continent_oop.html", "区域 OOPS 转型 Sankey", "Sankey",
           "把区域结构变化转化为流向关系。"),
      spec("iadv_funnel_sources.html", "筹资转型漏斗图", "Plotly funnel",
           "从资金来源到制度结构逐层观察转型。")
    ),
    timeline = list(
      spec("iadv_area_smooth_che.html", "全球 CHE 平滑面积", "Plotly area",
           "用长期面积图观察总量扩张节奏。"),
      spec("43_yoy_heatmap.html", "同比变化热力图", "Heatmap",
           "把年度变化压缩成矩阵，快速定位断点。")
    ),
    extremes = list(
      spec("iadv_lollipop_oop.html", "OOPS 极值棒棒糖图", "Plotly lollipop",
           "突出自付压力最高的一组国家。"),
      spec("iadv_rt_oop_extremes.html", "OOPS 极值表", "Reactable",
           "用表格复核极端值是否稳定。")
    ),
    compare = list(
      spec("33_country_compare.html", "多国指标对比", "Plotly compare",
           "把选中国家的关键指标轨迹放在同一视图。"),
      spec("iadv_dt_threeyear.html", "三年窗口表", "DT table",
           "检查短期变化是否受单年波动影响。")
    ),
    cluster = list(
      spec("iadv_parcoords.html", "多指标平行坐标", "Plotly parcoords",
           "查看各类国家在多维指标上的剖面差异。"),
      spec("iadv_force_country_sim.html", "国家相似网络", "Network",
           "用指标距离寻找同伴国家。")
    ),
    forecast = list(
      spec("06_forecast_subplot.html", "预测子图", "Plotly forecast",
           "用多面板查看未来路径和历史趋势。"),
      spec("13_mc_fan.html", "蒙特卡洛扇形图", "Plotly fan",
           "用不确定性区间表达预测风险。")
    ),
    scenarios = list(
      spec("13_scenarios.html", "政策情景曲线", "Plotly scenarios",
           "比较不同假设下的未来轨迹。"),
      spec("13_mc_fan.html", "情景不确定性扇形图", "Plotly fan",
           "把情景结果从单点扩展到区间。")
    ),
    mapstudio = list(
      spec("11_world_leaflet.html", "世界底图", "Leaflet",
           "空间工作台的基础入口，支持国家悬停和定位。"),
      spec("iadv_heatmap_year_inc.html", "年份收入组热力", "Heatmap",
           "把时间和收入组维度压缩到一张矩阵里。")
    ),
    correlation = list(
      spec("31_corr_matrix.html", "相关矩阵", "Plotly matrix",
           "快速查看核心变量之间的相关结构。"),
      spec("32_corr_overtime.html", "相关随时间变化", "Plotly overtime",
           "判断相关关系是否稳定。")
    ),
    distribution = list(
      spec("iadv_density_che.html", "CHE 密度分布", "Plotly density",
           "查看卫生支出的右偏和尾部。"),
      spec("13_income_violin.html", "收入组小提琴图", "Plotly violin",
           "比较收入组间分布差异和组内离散度。")
    ),
    robustness = list(
      spec("iadv_bar_ci.html", "置信区间条形图", "Plotly CI",
           "用区间而非单点呈现估计稳定性。"),
      spec("iadv_rt_below_threshold.html", "阈值敏感表", "Reactable",
           "筛选低于关键阈值的国家和年份。")
    ),
    dataquality = list(
      spec("iadv_dt_master_browse.html", "主表审计", "DT table",
           "检查缺失、异常和字段覆盖。"),
      spec("iadv_ec_calendar.html", "覆盖日历热力", "HTML calendar",
           "用矩阵化组件观察数据覆盖状态。")
    ),
    policy = list(
      spec("iadv_polar_radar.html", "政策雷达画像", "Plotly radar",
           "把财政、保护、效率和产出维度合成一张画像。"),
      spec("iadv_gauge_grid.html", "风险仪表组", "HTML gauges",
           "用仪表化组件呈现关键风险等级。")
    ),
    atlas = list(
      spec("iadv_dt_full_panel.html", "全字段浏览表", "DT table",
           "集中浏览国家年面板的所有核心字段。"),
      spec("iadv_dt_master_browse.html", "主面板索引", "DT table",
           "按国家、年份、收入组快速定位记录。")
    ),
    common
  )
  specs[seq_len(min(2, length(specs)))]
}

mod_v3_widget_card <- function(spec, index) {
  url <- mod_v3_widget_url(spec$file)
  htmltools::tags$article(
    class = "section-widget-card widget-gallery-card",
    htmltools::tags$header(
      class = "section-widget-card-head",
      htmltools::span(class = "section-widget-index", sprintf("%02d", index)),
      htmltools::div(
        htmltools::span(class = "section-widget-kicker", spec$kicker),
        htmltools::h3(spec$title),
        htmltools::p(spec$desc)
      )
    ),
    htmltools::div(
      class = "section-widget-frame widget-gallery-frame",
      htmltools::tags$iframe(
        title = paste("Section widget", spec$title),
        `data-widget-src` = url,
        srcdoc = mod_v3_widget_placeholder(spec$title),
        loading = "lazy",
        referrerpolicy = "no-referrer",
        allowfullscreen = NA
      )
    ),
    htmltools::tags$footer(
      class = "section-widget-card-foot",
      htmltools::span(spec$file),
      htmltools::tags$a("新窗打开", href = url, target = "_blank",
                        rel = "noreferrer")
    )
  )
}

mod_v3_section_widgets <- function(kicker, title, lead) {
  topic <- mod_v3_widget_topic(kicker, title, lead)
  specs <- mod_v3_widget_specs(topic)
  htmltools::tags$section(
    class = "section-widget-strip",
    `data-widget-topic` = topic,
    htmltools::div(
      class = "section-widget-head",
      htmltools::span("Interactive pair"),
      htmltools::strong("本板块精选交互组件"),
      htmltools::p("先用这两张交互图建立直觉，再向下阅读本页的分析、表格和模型结果。")
    ),
    htmltools::div(
      class = "section-widget-grid",
      lapply(seq_along(specs), function(i) mod_v3_widget_card(specs[[i]], i))
    )
  )
}

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
  hero_html <- sprintf(
    paste0("<section class='v3-hero'><div class='v3-hero-inner'>",
           "<span class='v3-hero-kicker'>%s</span>",
           "<h1 class='v3-hero-title'>%s</h1>",
           "<p class='v3-hero-lead'>%s</p>%s</div></section>"),
    htmltools::htmlEscape(kicker),
    htmltools::htmlEscape(title),
    htmltools::htmlEscape(lead),
    meta_html)
  widget_html <- as.character(mod_v3_section_widgets(kicker, title, lead))
  htmltools::HTML(paste0(hero_html, widget_html))
}

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

mod_v3_kpi <- function(label, value, hint = NULL, trend = NULL,
                       tone = c("primary", "secondary", "good",
                                 "warn", "bad", "neutral")) {
  tone <- match.arg(tone)
  if (mod_v3_is_value_like(label) && !mod_v3_is_value_like(value)) {
    tmp <- label
    label <- value
    value <- tmp
  }
  tone_color <- switch(tone,
    primary = "#254f5c", secondary = "#6b6077",
    good = "#587669", warn = "#8f743d", bad = "#8b544e",
    "#5d6965")
  trend_html <- ""
  if (!is.null(trend) && is.finite(trend)) {
    arr <- if (trend > 0) "\u2197" else if (trend < 0) "\u2198" else "\u2192"
    col <- if (trend > 0) "#587669"
           else if (trend < 0) "#8b544e"
           else "#5d6965"
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

mod_v3_kpi_grid <- function(...) {
  cards <- list(...)
  htmltools::tagList(
    htmltools::div(class = "v3-kpi-grid",
                   lapply(cards, function(c) c)))
}

mod_v3_story_grid <- function(..., columns = 3, class = NULL) {
  htmltools::div(
    class = mod_class("v3-story-grid", class),
    style = sprintf("--cols:%s", columns),
    ...
  )
}

mod_v3_page_body <- function(..., wide = FALSE, class = NULL) {
  htmltools::div(
    class = mod_class("v3-page-body", if (wide) "v3-page-body-wide", class),
    ...
  )
}

mod_v3_badge_row <- function(..., tone = c("primary", "secondary", "good",
                                           "warn", "bad", "neutral")) {
  tone <- match.arg(tone)
  tone_color <- switch(tone,
    primary = "#254f5c", secondary = "#6b6077",
    good = "#587669", warn = "#8f743d", bad = "#8b544e",
    "#5d6965")
  labels <- unlist(list(...), use.names = FALSE)
  labels <- labels[!is.na(labels) & nzchar(labels)]
  htmltools::div(
    class = "v3-badge-row",
    style = sprintf("--tone:%s", tone_color),
    lapply(labels, function(x) htmltools::span(class = "v3-badge", x))
  )
}

mod_v3_insight <- function(title, text, kicker = NULL,
                           tone = c("primary", "secondary", "good",
                                    "warn", "bad", "neutral"),
                           icon = NULL, meta = NULL) {
  tone <- match.arg(tone)
  tone_color <- switch(tone,
    primary = "#254f5c", secondary = "#6b6077",
    good = "#587669", warn = "#8f743d", bad = "#8b544e",
    "#5d6965")
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
      list(kicker = "异常点", title = "偏离拟合线才是重点",
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
      list(kicker = "保护", title = "OOPS 是保护压力信号",
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
      list(kicker = "尾部", title = "尾部决定政策关注点",
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

mod_v3_source_note <- function(..., title = "Source note") {
  htmltools::div(
    class = "v3-source-note",
    htmltools::strong(title),
    htmltools::span(...)
  )
}

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

mod_v3_module_card <- function(ns, target, kicker, title, desc,
                                icon = "\u25b8",
                                tone = "primary") {
  tone_color <- switch(tone,
    primary = "#254f5c", secondary = "#6b6077",
    good = "#587669", warn = "#8f743d", bad = "#8b544e",
    "#5d6965")
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

mod_v3_plotly <- function(p, dark = FALSE) {
  if (exists("ghs_plotly_layout", mode = "function")) {
    return(ghs_plotly_layout(p, theme = if (dark) "dark" else "light"))
  }
  p
}

mod_v3_leaflet <- function(map = NULL, dark = FALSE) {
  if (exists("ghs_leaflet_provider", mode = "function")) {
    return(ghs_leaflet_provider(map, theme = if (dark) "dark" else "light"))
  }
  if (is.null(map)) leaflet::leaflet() else map
}

mod_v3_reactable <- function(data, ..., dark = FALSE) {
  args <- list(data = data, ...)
  if (exists("ghs_reactable_theme", mode = "function") &&
      !"theme" %in% names(args)) {
    args$theme <- ghs_reactable_theme(if (dark) "dark" else "light")
  }
  do.call(reactable::reactable, args)
}

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


mod_widget_project_root <- function(start = getwd(), max_up = 6) {
  d <- normalizePath(start, mustWork = FALSE)
  for (i in seq_len(max_up + 1)) {
    if (dir.exists(file.path(d, "程序")) ||
        file.exists(file.path(d, "DESCRIPTION"))) {
      return(d)
    }
    parent <- dirname(d)
    if (identical(parent, d)) break
    d <- parent
  }
  normalizePath(start, mustWork = FALSE)
}

mod_widget_ensure_interactive_fns <- function() {
  sentinels <- c(
    "widget_v2_gapminder_bubble",
    "widget_more_continent_oops",
    "imap_che_pc",
    "iadv_che_pc_lines"
  )
  if (all(vapply(sentinels, exists, logical(1),
                 envir = .GlobalEnv, mode = "function"))) {
    return(invisible(TRUE))
  }
  root <- mod_widget_project_root()
  source_dir <- if (dir.exists(file.path(root, "程序"))) {
    file.path(root, "程序")
  } else {
    file.path(root, "程序库")
  }
  files <- file.path(source_dir, c(
    "17_widgets_plotly.R",
    "18_widgets_other.R",
    "25_widgets_more.R",
    "36_widgets_map.R",
    "37_widgets_advanced.R"
  ))
  files <- files[file.exists(files)]
  if (!length(files)) return(invisible(FALSE))
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(root)
  for (f in files) {
    suppressWarnings(suppressMessages(
      source(f, encoding = "UTF-8", local = .GlobalEnv)
    ))
  }
  invisible(TRUE)
}

mod_widget_registry <- function() {
  mod_widget_ensure_interactive_fns()
  mk <- function(id, title, desc, source, fn, type,
                 deps = character(), needs_world = FALSE,
                 params = list(), group = "交互组件") {
    data.frame(
      id = id, title = title, desc = desc, source = source, fn = fn,
      type = type, deps = paste(deps, collapse = ","),
      needs_world = isTRUE(needs_world), group = group,
      stringsAsFactors = FALSE
    ) |>
      transform(params = I(list(params)))
  }
  rows <- list(
    mk("widget_v2_gapminder_bubble", "动态气泡：GDP、寿命与人均支出", "年份滑块展示经济水平、预期寿命与人均卫生支出的共同移动，适合作为全局结构的第一张交互图。", "程序/17_widgets_plotly.R", "widget_v2_gapminder_bubble", "plotly", "plotly", FALSE, group = "Plotly 基础"),
    mk("widget_v2_highlight_lines", "多国人均 CHE 高亮时序", "点击图例可隔离国家，观察高收入经济体与主要新兴经济体在人均支出上的长期距离。", "程序/17_widgets_plotly.R", "widget_v2_highlight_lines", "plotly", "plotly", FALSE, group = "Plotly 基础"),
    mk("widget_v2_oops_heatmap", "OOPS 国家年份热力图", "把自付比例波动最大的国家排成热力矩阵，快速识别家庭现金支付压力的持续高位和结构性转折。", "程序/17_widgets_plotly.R", "widget_v2_oops_heatmap", "plotly", c("plotly", "reshape2"), FALSE, group = "Plotly 基础"),
    mk("widget_v2_ternary", "HF1/HF2/HF3 三元结构", "用三元坐标呈现政府、保险与自付三类筹资方案的相对位置，适合比较制度型态。", "程序/17_widgets_plotly.R", "widget_v2_ternary", "plotly", "plotly", FALSE, list(year = 2022), group = "Plotly 基础"),
    mk("widget_v2_income_violin", "收入组人均支出分布动画", "按收入组展示人均 CHE 分布随年份移动，重点看组间距离和组内离散度是否同步变化。", "程序/17_widgets_plotly.R", "widget_v2_income_violin", "plotly", "plotly", FALSE, group = "Plotly 基础"),
    mk("widget_v2_splom", "多指标散点矩阵", "在同一年横截面中查看人均 CHE、GDP、寿命和 OOPS 的相关结构，适合寻找非线性和异常国家。", "程序/17_widgets_plotly.R", "widget_v2_splom", "plotly", "plotly", FALSE, list(year = 2022), group = "Plotly 基础"),
    mk("widget_v2_mc_fan", "蒙特卡洛情景扇形图", "用区间带展示未来情景的不确定性，强调预测不是单条线，而是一组可能路径。", "程序/17_widgets_plotly.R", "widget_v2_mc_fan", "plotly", "plotly", FALSE, group = "Plotly 基础"),
    mk("widget_v2_scenarios", "政策情景比较", "围绕单国政策参数构造未来轨迹，适合把历史走势转化为可讨论的假设空间。", "程序/17_widgets_plotly.R", "widget_v2_scenarios", "plotly", "plotly", FALSE, group = "Plotly 基础"),

    mk("widget_v2_leaflet_choropleth", "世界地图：多指标切换", "在同一张 Leaflet 地图中切换 OOPS、GGHED、EXT、人均 CHE 与预防支出占比。", "程序/18_widgets_other.R", "widget_v2_leaflet_choropleth", "leaflet", c("leaflet", "sf"), TRUE, list(year = 2022), group = "地图"),
    mk("widget_v2_reactable_rank", "国家排行表", "带搜索、排序和趋势条的国家排行，用于把图上的空间模式落回国家明细。", "程序/18_widgets_other.R", "widget_v2_reactable_rank", "reactable", "reactable", FALSE, list(year = 2023), group = "表格"),
    mk("widget_v2_dt_atlas", "DT 全字段浏览", "以可筛选表格浏览主面板核心字段，适合复核国家、年份和变量口径。", "程序/18_widgets_other.R", "widget_v2_dt_atlas", "dt", "DT", FALSE, group = "表格"),
    mk("widget_v2_country_network", "国家相似网络", "基于筹资和支出指标的距离构造相似国家网络，帮助发现同类制度组合。", "程序/18_widgets_other.R", "widget_v2_country_network", "ui", "networkD3", FALSE, list(year = 2022), group = "网络"),
    mk("widget_v2_sankey_flows", "三段资金流 Sankey", "把来源、筹资方案和用途连接为一张流向图，用于说明结构不是单一比例，而是多级分配。", "程序/18_widgets_other.R", "widget_v2_sankey_flows", "ui", "networkD3", FALSE, list(year = 2022), group = "网络"),
    mk("widget_v2_kpi_grid", "指标卡片组", "以 HTML 原生卡片呈现总量、人均、自付和外援依赖等关键数值。", "程序/18_widgets_other.R", "widget_v2_kpi_grid", "ui", "htmltools", FALSE, list(year = 2022), group = "HTML 摘要")
  )

  more_fns <- ls(envir = .GlobalEnv, pattern = "^widget_more_[a-z]")
  rows <- c(rows, lapply(more_fns, function(fn) {
    label <- gsub("_", " ", sub("^widget_more_", "", fn))
    type <- if (grepl("_dt_", fn)) "dt" else "plotly"
    deps <- if (identical(type, "dt")) "DT" else "plotly"
    mk(fn, paste("扩展图表", label), "第二批 Plotly 扩展组件，覆盖排行、趋势、热力图、相关矩阵和国家比较等常用分析视角。", "程序/25_widgets_more.R", fn, type, deps, FALSE, group = "Plotly 扩展")
  }))

  imap_fns <- ls(envir = .GlobalEnv, pattern = "^imap_[a-z]")
  rows <- c(rows, lapply(imap_fns, function(fn) {
    label <- gsub("_", " ", sub("^imap_", "", fn))
    type <- if (fn %in% c("imap_plotly_animation", "imap_plotly_choropleth", "imap_plotly_density")) "plotly"
            else if (fn == "imap_dual_compare") "ui"
            else "leaflet"
    deps <- switch(type,
      plotly = "plotly",
      leaflet = c("leaflet", "sf"),
      c("leaflet", "sf", "htmltools")
    )
    mk(fn, paste("地图", label), "地图工作台同源函数，可作为独立原生组件渲染；适合对照 Map Studio 的联动版本。", "程序/36_widgets_map.R", fn, type, deps, TRUE, group = "地图")
  }))

  iadv_fns <- ls(envir = .GlobalEnv, pattern = "^iadv_[a-z]")
  iadv_fns <- setdiff(iadv_fns, c("iadv_callout"))
  rows <- c(rows, lapply(iadv_fns, function(fn) {
    label <- gsub("_", " ", sub("^iadv_", "", fn))
    type <- if (grepl("^iadv_rt_", fn)) "reactable"
            else if (grepl("^iadv_dt_", fn)) "dt"
            else if (grepl("^iadv_hc_|^iadv_ec_", fn)) "ui"
            else if (grepl("^iadv_kpi_|^iadv_progress_|^iadv_rank_strip|^iadv_swatch|^iadv_alert|^iadv_dashboard|^iadv_country_card|^iadv_spark|^iadv_extreme|^iadv_finding|^iadv_milestone|^iadv_quote|^iadv_stats|^iadv_top_banner|^iadv_table_plus|^iadv_ctk_", fn)) "ui"
            else if (grepl("^iadv_sankey|^iadv_force|^iadv_chord|^iadv_diagonal", fn)) "ui"
            else "plotly"
    deps <- switch(type,
      plotly = "plotly",
      reactable = "reactable",
      dt = "DT",
      "htmltools"
    )
    params <- switch(fn,
      iadv_finding_card = list(
        title = "从图表回到证据",
        abstract = "这个卡片用于承载专题发现的摘要，让组件中枢能够直接预览叙事型 HTML 组件。",
        evidence_text = "组件中枢会为需要文本输入的 HTML 组件补充默认文案；正式页面仍应使用专题模块中的实际证据。"
      ),
      iadv_quote_callout = list(
        quote_text = "卫生支出比较的重点不只是高低，而是资金结构、风险保护和健康产出是否彼此支撑。",
        attrib = "Global Health Spending Dashboard"
      ),
      list()
    )
    mk(fn, paste("高级组件", label), "高级交互组件库中的原生输出，覆盖分布、结构、网络、表格、KPI 与专题摘要。", "程序/37_widgets_advanced.R", fn, type, deps, FALSE, params, group = "高级组件")
  }))

  reg <- do.call(rbind, rows)
  reg <- reg[!duplicated(reg$id), , drop = FALSE]
  featured <- c(
    "widget_v2_gapminder_bubble",
    "widget_v2_leaflet_choropleth",
    "widget_v2_reactable_rank",
    "widget_v2_dt_atlas",
    "widget_v2_oops_heatmap",
    "iadv_che_pc_lines",
    "iadv_oop_lines",
    "iadv_bar_race",
    "iadv_sunburst_che",
    "iadv_parcoords",
    "iadv_force_country_sim",
    "iadv_dashboard_overview",
    "iadv_dt_full_panel",
    "iadv_heatmap_year_inc",
    "iadv_polar_radar",
    "iadv_treemap_che",
    "iadv_hc_stream",
    "iadv_ec_liquid",
    "iadv_dt_master_browse"
  )
  group_rank <- match(
    reg$group,
    c("Plotly 基础", "地图", "表格", "Plotly 扩展", "高级组件",
      "网络", "HTML 摘要"),
    nomatch = 99
  )
  type_rank <- match(reg$type, c("plotly", "leaflet", "reactable", "dt", "ui"),
                     nomatch = 99)
  featured_rank <- match(reg$id, featured, nomatch = length(featured) + 1L)
  reg <- reg[order(group_rank, featured_rank, type_rank, reg$title), ,
             drop = FALSE]
  rownames(reg) <- NULL
  reg
}

mod_widget_output_ui <- function(ns, widget_id, type, height = "620px") {
  output_id <- ns(paste0("widget_", widget_id))
  switch(type,
    plotly = plotly::plotlyOutput(output_id, height = height),
    leaflet = leaflet::leafletOutput(output_id, height = height),
    reactable = reactable::reactableOutput(output_id),
    dt = DT::DTOutput(output_id),
    ui = shiny::uiOutput(output_id),
    shiny::uiOutput(output_id)
  )
}

mod_render_native_widget <- function(output, output_id, builder, type) {
  fallback_plotly <- function(x) {
    msg <- if (inherits(x, "shiny.tag") || inherits(x, "html")) {
      gsub("<[^>]+>", " ", as.character(x))
    } else {
      as.character(x %||% "组件不可用")
    }
    plotly::plotly_empty(type = "scatter", mode = "markers") |>
      plotly::layout(
        annotations = list(list(
          text = msg, x = 0.5, y = 0.5, showarrow = FALSE,
          font = list(color = "#5d667a", size = 13)
        )),
        paper_bgcolor = "#ffffff",
        plot_bgcolor = "#ffffff"
      )
  }
  fallback_leaflet <- function(x) {
    msg <- if (inherits(x, "shiny.tag") || inherits(x, "html")) {
      gsub("<[^>]+>", " ", as.character(x))
    } else {
      as.character(x %||% "组件不可用")
    }
    leaflet::leaflet() |>
      leaflet::addProviderTiles("CartoDB.Positron") |>
      leaflet::addControl(html = htmltools::htmlEscape(msg), position = "topright")
  }
  fallback_table <- function(x) {
    msg <- if (inherits(x, "shiny.tag") || inherits(x, "html")) {
      gsub("<[^>]+>", " ", as.character(x))
    } else {
      as.character(x %||% "组件不可用")
    }
    data.frame(message = msg, stringsAsFactors = FALSE)
  }
  switch(type,
    plotly = { output[[output_id]] <- plotly::renderPlotly({
      obj <- builder()
      if (inherits(obj, "plotly")) obj else fallback_plotly(obj)
    }) },
    leaflet = { output[[output_id]] <- leaflet::renderLeaflet({
      obj <- builder()
      if (inherits(obj, "leaflet")) obj else fallback_leaflet(obj)
    }) },
    reactable = { output[[output_id]] <- reactable::renderReactable({
      obj <- builder()
      if (inherits(obj, "reactable") || inherits(obj, "htmlwidget")) obj
      else reactable::reactable(fallback_table(obj), pagination = FALSE)
    }) },
    dt = { output[[output_id]] <- DT::renderDT({
      obj <- builder()
      if (inherits(obj, "datatables") || inherits(obj, "htmlwidget")) obj
      else DT::datatable(fallback_table(obj), options = list(dom = "t"), rownames = FALSE)
    }) },
    ui = { output[[output_id]] <- shiny::renderUI(builder()) },
    { output[[output_id]] <- shiny::renderUI(builder()) }
  )
  invisible(output_id)
}

mod_widget_missing <- function(title, text) {
  htmltools::div(
    class = "v3-empty-state",
    htmltools::strong(title),
    htmltools::p(text)
  )
}

mod_mapstudio_indicator_specs <- function() {
  list(
    che_pc_usd2023 = list(label = "人均 CHE", unit = "USD 2023", palette = "Blues", transform = "log", fmt = fmt_v3_usd),
    che_usd2023 = list(label = "总 CHE", unit = "USD 2023", palette = "Blues", transform = "log", fmt = fmt_v3_usd),
    hf3_che = list(label = "OOPS / CHE", unit = "%", palette = "YlOrRd", transform = "linear", fmt = fmt_v3_pct),
    gghed_che = list(label = "GGHED / CHE", unit = "%", palette = "GnBu", transform = "linear", fmt = fmt_v3_pct),
    pvtd_che = list(label = "PVT-D / CHE", unit = "%", palette = "PuRd", transform = "linear", fmt = fmt_v3_pct),
    ext_che = list(label = "EXT / CHE", unit = "%", palette = "YlGn", transform = "linear", fmt = fmt_v3_pct),
    hc6_che = list(label = "预防支出占比", unit = "%", palette = "BuGn", transform = "linear", fmt = fmt_v3_pct),
    life_exp = list(label = "预期寿命", unit = "年", palette = "Viridis", transform = "linear", fmt = function(x) fmt_v3_num(x, 1, " 年")),
    u5mr = list(label = "5 岁以下死亡率", unit = "/1000", palette = "OrRd", transform = "log", fmt = function(x) fmt_v3_num(x, 1))
  )
}

mod_mapstudio_metric_label <- function(var) {
  specs <- mod_mapstudio_indicator_specs()
  if (!is.null(specs[[var]])) specs[[var]]$label else var
}

mod_mapstudio_metric_value <- function(x, var) {
  specs <- mod_mapstudio_indicator_specs()
  fmt <- if (!is.null(specs[[var]])) specs[[var]]$fmt else fmt_v3_num
  fmt(x)
}

mod_mapstudio_point_data <- function(master, world_sf, year,
                                     vars = c("che_usd2023", "che_pc_usd2023",
                                              "hf3_che", "gghed_che",
                                              "life_exp", "u5mr")) {
  if (is.null(world_sf) || !requireNamespace("sf", quietly = TRUE)) return(NULL)
  keep <- intersect(c("iso3_code", "country_name", "continent", "income_group", "pop", vars), names(master))
  d <- master[master$year == year, keep, drop = FALSE]
  g <- merge(world_sf, d, by = "iso3_code", all.x = TRUE)
  g <- suppressWarnings(sf::st_centroid(sf::st_make_valid(g)))
  g
}

mod_mapstudio_leaflet <- function(spec, master, world_sf) {
  if (is.null(world_sf) || !requireNamespace("leaflet", quietly = TRUE) ||
      !requireNamespace("sf", quietly = TRUE)) {
    return(leaflet::leaflet() |> leaflet::addTiles())
  }
  mode <- spec$mode %||% "choropleth"
  var <- spec$indicator %||% "hf3_che"
  year <- spec$year %||% max(master$year, na.rm = TRUE)
  y1 <- spec$year_start %||% min(master$year, na.rm = TRUE)
  y2 <- spec$year_end %||% year
  continents <- spec$continents %||% character()
  incomes <- spec$incomes %||% character()
  selected_iso <- spec$iso %||% NULL

  base_provider <- switch(spec$basemap %||% "positron",
    voyager = "CartoDB.Voyager",
    dark = "CartoDB.DarkMatter",
    terrain = "Esri.WorldTopoMap",
    "CartoDB.Positron"
  )
  add_base <- function(m) {
    leaflet::addProviderTiles(m, base_provider, options = leaflet::providerTileOptions(opacity = 0.92))
  }
  filter_slice <- function(d) {
    if (length(continents)) d <- d[d$continent %in% continents | is.na(d$continent), , drop = FALSE]
    if (length(incomes)) d <- d[d$income_group %in% incomes | is.na(d$income_group), , drop = FALSE]
    d
  }
  join_year <- function(y, value_var = var) {
    keep <- intersect(c("iso3_code", "country_name", "continent", "income_group", "pop",
                        value_var, "che_pc_usd2023", "hf3_che", "gghed_che", "life_exp", "u5mr"),
                      names(master))
    d <- master[master$year == y, keep, drop = FALSE]
    d <- filter_slice(d)
    merge(world_sf, d, by = "iso3_code", all.x = TRUE)
  }
  label_for <- function(g, value_col = var, extra = NULL) {
    vals <- g[[value_col]]
    nm <- if ("country_name" %in% names(g)) g$country_name else g$iso3_code
    vapply(seq_len(nrow(g)), function(i) {
      paste0(
        "<strong>", htmltools::htmlEscape(nm[i] %||% g$iso3_code[i]), "</strong> (", g$iso3_code[i], ")<br>",
        htmltools::htmlEscape(mod_mapstudio_metric_label(value_col)), ": ",
        htmltools::htmlEscape(mod_mapstudio_metric_value(vals[i], value_col)),
        if (!is.null(extra)) paste0("<br>", extra(g, i)) else ""
      )
    }, character(1))
  }

  if (mode == "dual_compare") {
    g1 <- join_year(y1)
    g2 <- join_year(y2)
    vals <- c(g1[[var]], g2[[var]])
    vals <- vals[is.finite(vals)]
    pal <- leaflet::colorNumeric("YlOrRd", domain = vals, na.color = "#d9d4ca")
    m <- leaflet::leaflet(options = leaflet::leafletOptions(minZoom = 1.5, worldCopyJump = FALSE)) |> add_base()
    g1$map_value <- g1[[var]]
    g2$map_value <- g2[[var]]
    m <- leaflet::addPolygons(m, data = g1, group = paste0(y1),
      layerId = ~iso3_code, fillColor = ~pal(map_value), fillOpacity = 0.82,
      color = "#ffffff", weight = 0.35, label = lapply(label_for(g1), htmltools::HTML),
      highlightOptions = leaflet::highlightOptions(weight = 1.5, color = "#1d3f5f", bringToFront = TRUE))
    m <- leaflet::addPolygons(m, data = g2, group = paste0(y2),
      layerId = ~iso3_code, fillColor = ~pal(map_value), fillOpacity = 0.82,
      color = "#ffffff", weight = 0.35, label = lapply(label_for(g2), htmltools::HTML),
      highlightOptions = leaflet::highlightOptions(weight = 1.5, color = "#1d3f5f", bringToFront = TRUE))
    return(m |>
      leaflet::addLayersControl(baseGroups = c(paste0(y1), paste0(y2)),
        options = leaflet::layersControlOptions(collapsed = FALSE)) |>
      leaflet::hideGroup(paste0(y1)) |>
      leaflet::addLegend(pal = pal, values = vals, title = mod_mapstudio_metric_label(var)))
  }

  if (mode == "year_layers") {
    years <- unique(round(seq(y1, y2, length.out = min(5, max(1, y2 - y1 + 1)))))
    vals <- master[[var]]
    vals <- vals[is.finite(vals)]
    pal <- leaflet::colorNumeric("YlOrRd", domain = vals, na.color = "#d9d4ca")
    m <- leaflet::leaflet(options = leaflet::leafletOptions(minZoom = 1.5, worldCopyJump = FALSE)) |> add_base()
    for (yy in years) {
      gy <- join_year(yy)
      gy$map_value <- gy[[var]]
      m <- leaflet::addPolygons(m, data = gy, group = paste0(yy), layerId = ~iso3_code,
        fillColor = ~pal(map_value), fillOpacity = 0.82, color = "#ffffff", weight = 0.35,
        label = lapply(label_for(gy), htmltools::HTML),
        highlightOptions = leaflet::highlightOptions(weight = 1.5, color = "#1d3f5f", bringToFront = TRUE))
    }
    return(m |>
      leaflet::addLayersControl(baseGroups = paste0(years),
        options = leaflet::layersControlOptions(collapsed = FALSE)) |>
      leaflet::hideGroup(paste0(years[-length(years)])) |>
      leaflet::addLegend(pal = pal, values = vals, title = mod_mapstudio_metric_label(var)))
  }

  if (mode %in% c("change_che_pc", "change_oop")) {
    value_var <- if (mode == "change_oop") "hf3_che" else "che_pc_usd2023"
    keep_change <- intersect(c("iso3_code", "country_name", "continent",
                               "income_group", value_var), names(master))
    d1 <- master[master$year == y1, keep_change, drop = FALSE]
    d1 <- filter_slice(d1)
    d2 <- master[master$year == y2,
                 intersect(c("iso3_code", value_var), names(master)),
                 drop = FALSE]
    mdat <- merge(d1, d2, by = "iso3_code", suffixes = c(".y1", ".y2"))
    mdat$delta <- if (mode == "change_oop") {
      mdat[[paste0(value_var, ".y2")]] - mdat[[paste0(value_var, ".y1")]]
    } else {
      log(mdat[[paste0(value_var, ".y2")]] / mdat[[paste0(value_var, ".y1")]])
    }
    g <- merge(world_sf, mdat, by = "iso3_code", all.x = TRUE)
    finite_delta <- g$delta[is.finite(g$delta)]
    max_abs <- if (length(finite_delta)) max(abs(finite_delta), na.rm = TRUE) else 1
    if (!is.finite(max_abs) || max_abs <= 0) max_abs <- 1
    pal <- leaflet::colorNumeric(c("#a23b3b", "#fbf6ee", "#2a857a"),
      domain = c(-max_abs, max_abs), na.color = "#d9d4ca")
    labs <- vapply(seq_len(nrow(g)), function(i) {
      paste0("<strong>", htmltools::htmlEscape(g$country_name[i] %||% g$iso3_code[i]), "</strong><br>",
             y1, " 到 ", y2, "：", ifelse(is.finite(g$delta[i]), sprintf("%+.2f", g$delta[i]), "NA"))
    }, character(1))
    return(leaflet::leaflet(g, options = leaflet::leafletOptions(minZoom = 1.5)) |>
      add_base() |>
      leaflet::addPolygons(layerId = ~iso3_code, fillColor = ~pal(delta), fillOpacity = 0.84,
        color = "#ffffff", weight = 0.35, label = lapply(labs, htmltools::HTML),
        highlightOptions = leaflet::highlightOptions(weight = 1.5, color = "#1d3f5f", bringToFront = TRUE)) |>
      leaflet::addLegend(pal = pal, values = c(-1, 0, 1), title = if (mode == "change_oop") "OOPS 变化 pp" else "log 倍数变化"))
  }

  if (mode %in% c("bubble_che_total", "bubble_oop", "country_points", "top10_oop")) {
    g <- mod_mapstudio_point_data(master, world_sf, year)
    if (is.null(g)) return(leaflet::leaflet() |> leaflet::addTiles())
    if (length(continents)) g <- g[g$continent %in% continents, ]
    if (length(incomes)) g <- g[g$income_group %in% incomes, ]
    if (mode == "top10_oop") {
      g <- g[is.finite(g$hf3_che), ]
      g <- utils::head(g[order(-g$hf3_che), ], 10)
    }
    rad <- switch(mode,
      bubble_che_total = sqrt(pmax(if ("che_usd2023" %in% names(g)) g$che_usd2023 else 0, 0)) / 900000,
      bubble_oop = pmax(4, pmin(18, g$hf3_che / 4)),
      top10_oop = rep(11, nrow(g)),
      rep(5, nrow(g))
    )
    rad[!is.finite(rad)] <- 5
    fill <- if (mode == "top10_oop") "#a23b3b" else "#1d3f5f"
    labs <- vapply(seq_len(nrow(g)), function(i) {
      paste0("<strong>", htmltools::htmlEscape(g$country_name[i] %||% g$iso3_code[i]), "</strong> (", g$iso3_code[i], ")<br>",
             "人均 CHE: ", mod_mapstudio_metric_value(g$che_pc_usd2023[i], "che_pc_usd2023"), "<br>",
             "OOPS: ", mod_mapstudio_metric_value(g$hf3_che[i], "hf3_che"), "<br>",
             "GGHED: ", mod_mapstudio_metric_value(g$gghed_che[i], "gghed_che"))
    }, character(1))
    return(leaflet::leaflet(world_sf, options = leaflet::leafletOptions(minZoom = 1.5)) |>
      add_base() |>
      leaflet::addPolygons(fillColor = "#f2eadf", fillOpacity = 0.72, color = "#ffffff", weight = 0.25) |>
      leaflet::addCircleMarkers(data = g, layerId = ~iso3_code, radius = rad, color = "#0d121b",
        weight = 0.7, fillColor = fill, fillOpacity = 0.72, label = lapply(labs, htmltools::HTML)))
  }

  g <- join_year(year)
  vals <- g[[var]]
  scale_mode <- spec$scale %||% "auto"
  z <- vals
  legend_values <- z
  legend_title <- mod_mapstudio_metric_label(var)
  spec_var <- mod_mapstudio_indicator_specs()[[var]]
  default_transform <- if (is.null(spec_var)) "" else spec_var$transform %||% ""
  if (scale_mode == "log" || (scale_mode == "auto" && default_transform == "log")) {
    z <- log10(pmax(vals, 1))
    legend_title <- paste0("log10 ", legend_title)
  }
  if (mode == "quintile" || scale_mode == "quantile") {
    bins <- unique(stats::quantile(vals, probs = seq(0, 1, 0.2), na.rm = TRUE))
    if (length(bins) < 3) bins <- pretty(vals[is.finite(vals)], n = 5)
    pal <- leaflet::colorBin("YlOrRd", domain = vals, bins = bins, na.color = "#d9d4ca")
    g$map_value <- vals
    fill_expr <- ~pal(map_value)
    legend_values <- vals
  } else if (mode == "bivariate") {
    d <- master[master$year == year & is.finite(master$che_pc_usd2023) & is.finite(master$life_exp), ]
    d <- filter_slice(d)
    if (nrow(d) < 3) {
      d <- d[FALSE, , drop = FALSE]
      d$cell <- character()
    } else {
      qx <- unique(as.numeric(stats::quantile(d$che_pc_usd2023,
        c(1/3, 2/3), na.rm = TRUE)))
      qy <- unique(as.numeric(stats::quantile(d$life_exp,
        c(1/3, 2/3), na.rm = TRUE)))
      if (length(qx) < 2 || length(qy) < 2) {
        d$cell <- NA_character_
      } else {
        d$cell <- paste0(
          cut(d$che_pc_usd2023, c(-Inf, qx[1:2], Inf), labels = 1:3),
          cut(d$life_exp, c(-Inf, qy[1:2], Inf), labels = 1:3)
        )
      }
    }
    cell_pal <- c("11" = "#e8e8e8", "12" = "#aac4d1", "13" = "#6c9fbf",
                  "21" = "#e5b099", "22" = "#b29ab2", "23" = "#7090b8",
                  "31" = "#d97539", "32" = "#b56c6c", "33" = "#7d4a72")
    g <- merge(world_sf, d[, c("iso3_code", "country_name", "continent", "income_group", "che_pc_usd2023", "life_exp", "cell")], by = "iso3_code", all.x = TRUE)
    g$fill_col <- cell_pal[g$cell]
    g$fill_col[is.na(g$fill_col)] <- "#d9d4ca"
    pal <- NULL
    fill_expr <- ~fill_col
    legend_values <- NULL
    legend_title <- "CHE x 寿命"
  } else if (mode == "efficiency") {
    d <- master[master$year == year & is.finite(master$life_exp) & is.finite(master$che_pc_usd2023) & master$che_pc_usd2023 > 0, ]
    d <- filter_slice(d)
    if (nrow(d) >= 3 && length(unique(log(d$che_pc_usd2023))) >= 2) {
      fit <- stats::lm(life_exp ~ log(che_pc_usd2023), data = d)
      d$res <- stats::residuals(fit)
    } else {
      d <- d[FALSE, , drop = FALSE]
      d$res <- numeric()
    }
    g <- merge(world_sf, d[, c("iso3_code", "country_name", "continent", "income_group", "res")], by = "iso3_code", all.x = TRUE)
    finite_res <- d$res[is.finite(d$res)]
    max_abs_res <- if (length(finite_res)) max(abs(finite_res), na.rm = TRUE) else 1
    if (!is.finite(max_abs_res) || max_abs_res <= 0) max_abs_res <- 1
    pal <- leaflet::colorNumeric(c("#a23b3b", "#fbf6ee", "#2a857a"),
      domain = c(-max_abs_res, max_abs_res), na.color = "#d9d4ca")
    g$map_value <- g$res
    fill_expr <- ~pal(map_value)
    legend_values <- g$map_value
    legend_title <- "同支出水平寿命残差"
  } else {
    g$z_value <- z
    pal <- leaflet::colorNumeric("YlOrRd", domain = z, na.color = "#d9d4ca")
    fill_expr <- ~pal(z_value)
    legend_values <- z
  }
  labs <- if (mode == "efficiency") {
    vapply(seq_len(nrow(g)), function(i) paste0("<strong>", htmltools::htmlEscape(g$country_name[i] %||% g$iso3_code[i]), "</strong><br>寿命残差: ", fmt_v3_num(g$res[i], 1, " 年")), character(1))
  } else if (mode == "bivariate") {
    vapply(seq_len(nrow(g)), function(i) paste0("<strong>", htmltools::htmlEscape(g$country_name[i] %||% g$iso3_code[i]), "</strong><br>人均 CHE: ", mod_mapstudio_metric_value(g$che_pc_usd2023[i], "che_pc_usd2023"), "<br>寿命: ", mod_mapstudio_metric_value(g$life_exp[i], "life_exp"), "<br>格: ", g$cell[i]), character(1))
  } else {
    label_for(g)
  }
  m <- leaflet::leaflet(g, options = leaflet::leafletOptions(minZoom = 1.5, worldCopyJump = FALSE)) |>
    add_base() |>
    leaflet::addPolygons(layerId = ~iso3_code, fillColor = fill_expr, fillOpacity = 0.84,
      color = "#ffffff", weight = 0.35, label = lapply(labs, htmltools::HTML),
      highlightOptions = leaflet::highlightOptions(weight = 1.6, color = "#0d121b", bringToFront = TRUE))
  if (!is.null(selected_iso) && selected_iso %in% g$iso3_code) {
    sg <- g[g$iso3_code == selected_iso, ]
    m <- leaflet::addPolygons(m, data = sg, fillColor = "transparent", fillOpacity = 0,
      color = "#0d121b", weight = 2.6, group = "selected")
  }
  if (!is.null(pal)) {
    m <- leaflet::addLegend(m, pal = pal, values = legend_values,
      title = legend_title, position = "bottomright")
  }
  m
}

mod_mapstudio_plotly <- function(spec, master) {
  if (!requireNamespace("plotly", quietly = TRUE)) return(NULL)
  var <- spec$indicator %||% "hf3_che"
  mode <- spec$mode %||% "choropleth"
  year <- spec$year %||% max(master$year, na.rm = TRUE)
  d <- master
  if (length(spec$continents %||% character())) d <- d[d$continent %in% spec$continents, ]
  if (length(spec$incomes %||% character())) d <- d[d$income_group %in% spec$incomes, ]
  d <- d[is.finite(d[[var]]), ]
  if (!nrow(d)) return(plotly::plotly_empty())
  if (mode == "plotly_animation") {
    z <- if (var %in% c("che_pc_usd2023", "u5mr", "che_usd2023")) log10(pmax(d[[var]], 1)) else d[[var]]
    hover_text <- paste(d$country_name, "<br>", mod_mapstudio_metric_label(var), "=", signif(d[[var]], 4))
    p <- plotly::plot_ly(d, type = "choropleth", locations = ~iso3_code,
      z = z, frame = ~year, text = hover_text,
      colorscale = "YlOrRd", colorbar = list(title = mod_mapstudio_metric_label(var))) |>
      plotly::layout(geo = list(projection = list(type = "robinson"), showcountries = TRUE, countrycolor = "#ffffff", showframe = FALSE),
        margin = list(t = 40, b = 10, l = 10, r = 10), paper_bgcolor = "#fbf6ee")
    return(p)
  }
  d <- d[d$year == year, ]
  z <- if (var %in% c("che_pc_usd2023", "u5mr", "che_usd2023")) log10(pmax(d[[var]], 1)) else d[[var]]
  plotly::plot_ly(d, type = "choropleth", locations = ~iso3_code,
    z = z, text = ~country_name, colorscale = "YlOrRd",
    colorbar = list(title = mod_mapstudio_metric_label(var))) |>
    plotly::layout(geo = list(projection = list(type = "mollweide"), showframe = FALSE),
      title = list(text = paste0(mod_mapstudio_metric_label(var), " · ", year), x = 0),
      margin = list(t = 60, b = 10, l = 10, r = 10), paper_bgcolor = "#fbf6ee")
}
