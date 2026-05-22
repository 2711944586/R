mod_widget_feature_button <- function(ns, id, icon, kicker, title, text,
                                      meta, tone = "primary") {
  shiny::actionButton(
    ns(id),
    label = htmltools::tagList(
      htmltools::div(
        class = "widget-feature-head",
        htmltools::span(class = "widget-feature-icon", shiny::icon(icon)),
        htmltools::span(class = "widget-feature-kicker", kicker)
      ),
      htmltools::strong(title),
      htmltools::p(text),
      htmltools::span(class = "widget-feature-meta", meta)
    ),
    class = paste("widget-feature-btn", paste0("widget-feature-", tone)),
    title = paste("切换到", title)
  )
}

mod_widgets_ui <- function(id, country_choices, year_min, year_max) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = "交互组件 Widgets",
    value = "widgets",
    mod_v3_hero(
      kicker = "NATIVE INTERACTION LIBRARY",
      title = "Widgets · 交互组件原生中枢",
      lead = paste(
        "这里把静态报告中分散的交互组件还原为 Shiny 原生输出。",
        "目录只负责检索与选择，真正的图表、地图、表格和网络会在选中后懒加载，",
        "既保留组件完整度，也避免一次性加载全部组件造成页面迟滞。"
      ),
      meta = list("Plotly", "Leaflet", "Reactable", "DT", "Network / HTML")
    ),
    mod_v3_page_body(
      wide = TRUE,
      mod_v3_badge_row(
        "原生渲染",
        "Plotly / Leaflet / Reactable / DT / Network",
        "按需加载",
        "高级组件可直接切换",
        tone = "secondary"
      ),
      htmltools::div(
        class = "widget-feature-grid",
        mod_widget_feature_button(
          ns, "feature_bubble", "chart-area", "Overview",
          "动态气泡",
          "年份推进时观察 GDP、寿命与人均卫生支出的共同移动。",
          "Plotly animation", "primary"
        ),
        mod_widget_feature_button(
          ns, "feature_map", "globe", "Spatial",
          "世界地图",
          "切换指标和年份，快速定位自付、公共筹资和外援的空间差异。",
          "Leaflet map", "good"
        ),
        mod_widget_feature_button(
          ns, "feature_race", "ranking-star", "Motion",
          "排行动画",
          "用 year-frame 看高支出国家排序如何变化，适合做动态比较。",
          "Plotly frame", "warn"
        ),
        mod_widget_feature_button(
          ns, "feature_sunburst", "circle-nodes", "Structure",
          "旭日结构",
          "按大洲、收入组和国家展开总 CHE，读出全球资金层级。",
          "Sunburst", "secondary"
        ),
        mod_widget_feature_button(
          ns, "feature_parcoords", "sliders", "Profile",
          "平行坐标",
          "把 CHE、OOPS、公共筹资、寿命和儿童死亡率放在同一剖面。",
          "Parallel coordinates", "neutral"
        ),
        mod_widget_feature_button(
          ns, "feature_force", "diagram-project", "Network",
          "相似网络",
          "用筹资和结果指标寻找相近国家，适合识别同伴比较对象。",
          "networkD3", "primary"
        ),
        mod_widget_feature_button(
          ns, "feature_dashboard", "gauge-high", "HTML",
          "专题看板",
          "把 KPI、风险提醒和 SDG 进度条合并为一块原生信息板。",
          "HTML widgets", "good"
        ),
        mod_widget_feature_button(
          ns, "feature_full_dt", "database", "Table",
          "全字段表",
          "保留字段浏览、筛选和逐项复核能力，用于从图回到数据。",
          "DT table", "secondary"
        ),
        mod_widget_feature_button(
          ns, "feature_heatmap", "border-all", "Matrix",
          "年份热力",
          "把年份、收入组和指标强度压缩成一张矩阵，适合快速识别断点。",
          "Heatmap", "warn"
        ),
        mod_widget_feature_button(
          ns, "feature_radar", "bullseye", "Profile",
          "雷达画像",
          "用多维评分同时看充足性、公平性和效率，适合做国家画像。",
          "Radar", "primary"
        ),
        mod_widget_feature_button(
          ns, "feature_treemap", "sitemap", "Hierarchy",
          "树状结构",
          "按区域和国家分解 CHE 总量，把规模差异和层级关系放在同一屏。",
          "Treemap", "good"
        ),
        mod_widget_feature_button(
          ns, "feature_stream", "wave-square", "Flow",
          "功能流图",
          "观察 HC 功能项随年份变化的结构流动，补足静态堆叠图的阅读。",
          "Highcharter", "neutral"
        ),
        mod_widget_feature_button(
          ns, "feature_gauge", "gauge", "Gauge",
          "液体仪表",
          "用仪表化组件呈现覆盖率和风险状态，适合做专题页的视觉锚点。",
          "Gauge UI", "secondary"
        ),
        mod_widget_feature_button(
          ns, "feature_master", "table-list", "Audit",
          "主表浏览",
          "筛选 year-country 主面板，直接核查图表背后的国家和年份记录。",
          "DT audit", "primary"
        )
      ),
      htmltools::div(
        class = "widget-workbench-bar",
        htmltools::div(
          class = "widget-workbench-title",
          htmltools::span("Switchboard"),
          htmltools::strong("常用组件快速切换")
        ),
        htmltools::div(
          class = "widget-quick-actions",
          shiny::actionButton(
            ns("quick_bubble"),
            label = htmltools::tagList(shiny::icon("chart-area"), "动态气泡"),
            class = "widget-quick-btn",
            title = "切换到 Plotly 动态气泡"
          ),
          shiny::actionButton(
            ns("quick_map"),
            label = htmltools::tagList(shiny::icon("globe"), "世界地图"),
            class = "widget-quick-btn",
            title = "切换到 Leaflet 世界地图"
          ),
          shiny::actionButton(
            ns("quick_rank"),
            label = htmltools::tagList(shiny::icon("table"), "排行表"),
            class = "widget-quick-btn",
            title = "切换到 Reactable 国家排行"
          ),
          shiny::actionButton(
            ns("quick_heatmap"),
            label = htmltools::tagList(shiny::icon("border-all"), "OOPS 热力"),
            class = "widget-quick-btn",
            title = "切换到 OOPS 国家年份热力图"
          ),
          shiny::actionButton(
            ns("quick_lines"),
            label = htmltools::tagList(shiny::icon("chart-line"), "CHE 趋势"),
            class = "widget-quick-btn",
            title = "切换到高级 CHE 趋势线"
          ),
          shiny::actionButton(
            ns("quick_race"),
            label = htmltools::tagList(shiny::icon("ranking-star"), "排行动画"),
            class = "widget-quick-btn",
            title = "切换到动态排行组件"
          ),
          shiny::actionButton(
            ns("quick_sunburst"),
            label = htmltools::tagList(shiny::icon("circle-nodes"), "旭日结构"),
            class = "widget-quick-btn",
            title = "切换到支出结构旭日图"
          ),
          shiny::actionButton(
            ns("quick_parcoords"),
            label = htmltools::tagList(shiny::icon("sliders"), "平行坐标"),
            class = "widget-quick-btn",
            title = "切换到多指标平行坐标"
          ),
          shiny::actionButton(
            ns("quick_sankey"),
            label = htmltools::tagList(shiny::icon("diagram-project"), "Sankey"),
            class = "widget-quick-btn",
            title = "切换到资金流 Sankey"
          ),
          shiny::actionButton(
            ns("quick_full_dt"),
            label = htmltools::tagList(shiny::icon("database"), "全字段表"),
            class = "widget-quick-btn",
            title = "切换到全字段浏览表"
          ),
          shiny::actionButton(
            ns("quick_area"),
            label = htmltools::tagList(shiny::icon("chart-simple"), "总量面积"),
            class = "widget-quick-btn",
            title = "切换到全球总 CHE 平滑面积图"
          ),
          shiny::actionButton(
            ns("quick_dashboard"),
            label = htmltools::tagList(shiny::icon("gauge-high"), "专题看板"),
            class = "widget-quick-btn",
            title = "切换到高级专题看板"
          ),
          shiny::actionButton(
            ns("quick_treemap"),
            label = htmltools::tagList(shiny::icon("sitemap"), "树状结构"),
            class = "widget-quick-btn",
            title = "切换到 CHE 树状结构"
          ),
          shiny::actionButton(
            ns("quick_radar"),
            label = htmltools::tagList(shiny::icon("bullseye"), "雷达画像"),
            class = "widget-quick-btn",
            title = "切换到综合雷达画像"
          ),
          shiny::actionButton(
            ns("quick_stream"),
            label = htmltools::tagList(shiny::icon("water"), "功能流图"),
            class = "widget-quick-btn",
            title = "切换到 HC 功能流图"
          ),
          shiny::actionButton(
            ns("quick_gauge"),
            label = htmltools::tagList(shiny::icon("gauge"), "液体仪表"),
            class = "widget-quick-btn",
            title = "切换到液体仪表组件"
          ),
          shiny::actionButton(
            ns("quick_master"),
            label = htmltools::tagList(shiny::icon("table-list"), "主表浏览"),
            class = "widget-quick-btn",
            title = "切换到主面板浏览表"
          )
        )
      ),
      shiny::uiOutput(ns("widget_type_deck")),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 310,
          shiny::textInput(ns("query"), "搜索组件",
            placeholder = "输入地图、排行、OOPS、Sankey、DT 等关键词"),
          shiny::selectInput(ns("group"), "组件分组",
            choices = c("全部" = "all")),
          shiny::selectInput(ns("type"), "输出类型",
            choices = c("全部" = "all", "Plotly" = "plotly",
                        "Leaflet" = "leaflet", "Reactable" = "reactable",
                        "DT" = "dt", "HTML / Network" = "ui")),
          shiny::selectizeInput(ns("widget"), "选择组件",
            choices = character(),
            options = list(
              placeholder = "先用上方条件缩小范围",
              maxOptions = 120
            )),
          shiny::sliderInput(ns("year"), "年份参数",
            min = year_min, max = year_max, value = min(year_max, 2023),
            step = 1, sep = ""),
          shiny::selectizeInput(ns("country"), "国家参数",
            choices = country_choices, selected = "CHN",
            options = list(placeholder = "部分组件会使用该国家")),
          mod_v3_sidebar_note(
            title = "组件加载策略",
            text = "目录每次只渲染当前选中的一个组件；切换组件会重新调用对应 R 函数。",
            bullets = c(
              "Plotly/Leaflet/表格组件走专用输出通道。",
              "HTML、networkD3、crosstalk 等组合组件走 UI 输出。",
              "若函数需要 world_sf，会自动传入全局地图对象。"
            )
          )
        ),
        shiny::uiOutput(ns("registry_kpis")),
        bslib::layout_columns(
          col_widths = c(3, 9),
          mod_card(
            kicker = "Registry",
            title = "组件目录",
            mod_v3_chart_guide(
              title = "如何选择",
              text = "先用精选卡片进入典型组件，再按分组、类型和关键词缩小目录。函数名保留在表中，便于回到源码复核。",
              tone = "info"
            ),
            mod_spinner(reactable::reactableOutput(ns("registry_table"))),
            footer = "目录用于检索和复核；实际切换使用侧栏选择框或上方按钮。",
            class = "widget-registry-card"
          ),
          mod_card(
            kicker = "Live widget",
            title = "原生组件预览",
            shiny::uiOutput(ns("widget_header")),
            shiny::uiOutput(ns("widget_slot")),
            footer = "输出在 Shiny 运行时由对应 render 函数生成，不读取 standalone HTML 文件。",
            class = "widget-preview-card"
          )
        )
      )
    )
  )
}

mod_widgets_server <- function(id, master_r, world_sf) {
  shiny::moduleServer(id, function(input, output, session) {
    default_widget_id <- "widget_v2_gapminder_bubble"
    quick_target <- shiny::reactiveVal(NULL)

    registry_r <- shiny::reactive({
      mod_widget_registry()
    })

    shiny::observe({
      reg <- registry_r()
      groups <- sort(unique(reg$group))
      shiny::updateSelectInput(session, "group",
        choices = c("全部" = "all", stats::setNames(groups, groups)),
        selected = input$group %||% "all")
    })

    filtered_r <- shiny::reactive({
      reg <- registry_r()
      if (!is.null(input$group) && input$group != "all") {
        reg <- reg[reg$group == input$group, , drop = FALSE]
      }
      if (!is.null(input$type) && input$type != "all") {
        reg <- reg[reg$type == input$type, , drop = FALSE]
      }
      q <- trimws(input$query %||% "")
      if (nzchar(q)) {
        hay <- paste(reg$title, reg$desc, reg$fn, reg$source, reg$group)
        reg <- reg[grepl(q, hay, ignore.case = TRUE), , drop = FALSE]
      }
      reg
    })

    shiny::observe({
      reg <- filtered_r()
      choices <- if (nrow(reg)) {
        stats::setNames(reg$id, paste0(reg$title, " · ", reg$type))
      } else {
        character()
      }
      target <- quick_target()
      selected <- if (!is.null(target) && length(target) && target %in% reg$id) {
                    target
                  } else if (!is.null(input$widget) && input$widget %in% reg$id) input$widget
                  else if (default_widget_id %in% reg$id) default_widget_id
                  else if (length(choices)) unname(choices[[1]]) else character(0)
      shiny::updateSelectizeInput(session, "widget",
        choices = choices, selected = selected, server = FALSE)
    })

    switch_to_widget <- function(widget_id, group, type) {
      shiny::updateTextInput(session, "query", value = "")
      shiny::updateSelectInput(session, "group", selected = group)
      shiny::updateSelectInput(session, "type", selected = type)
      quick_target(widget_id)
      shiny::updateSelectizeInput(session, "widget", selected = widget_id)
      shiny::onFlushed(function() quick_target(NULL), once = TRUE)
    }

    shiny::observeEvent(input$quick_bubble, {
      switch_to_widget("widget_v2_gapminder_bubble", "Plotly 基础", "plotly")
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$quick_map, {
      switch_to_widget("widget_v2_leaflet_choropleth", "地图", "leaflet")
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$quick_rank, {
      switch_to_widget("widget_v2_reactable_rank", "表格", "reactable")
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$quick_heatmap, {
      switch_to_widget("widget_v2_oops_heatmap", "Plotly 基础", "plotly")
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$quick_lines, {
      switch_to_widget("iadv_che_pc_lines", "高级组件", "plotly")
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$quick_race, {
      switch_to_widget("iadv_bar_race", "高级组件", "plotly")
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$quick_sunburst, {
      switch_to_widget("iadv_sunburst_che", "高级组件", "plotly")
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$quick_parcoords, {
      switch_to_widget("iadv_parcoords", "高级组件", "plotly")
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$quick_sankey, {
      switch_to_widget("iadv_sankey_3stage", "高级组件", "ui")
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$quick_full_dt, {
      switch_to_widget("iadv_dt_full_panel", "高级组件", "dt")
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$quick_area, {
      switch_to_widget("iadv_area_smooth_che", "高级组件", "plotly")
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$quick_dashboard, {
      switch_to_widget("iadv_dashboard_overview", "高级组件", "ui")
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$quick_treemap, {
      switch_to_widget("iadv_treemap_che", "高级组件", "plotly")
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$quick_radar, {
      switch_to_widget("iadv_polar_radar", "高级组件", "plotly")
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$quick_stream, {
      switch_to_widget("iadv_hc_stream", "高级组件", "ui")
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$quick_gauge, {
      switch_to_widget("iadv_ec_liquid", "高级组件", "ui")
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$quick_master, {
      switch_to_widget("iadv_dt_master_browse", "高级组件", "dt")
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$feature_bubble, {
      switch_to_widget("widget_v2_gapminder_bubble", "Plotly 基础", "plotly")
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$feature_map, {
      switch_to_widget("widget_v2_leaflet_choropleth", "地图", "leaflet")
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$feature_race, {
      switch_to_widget("iadv_bar_race", "高级组件", "plotly")
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$feature_sunburst, {
      switch_to_widget("iadv_sunburst_che", "高级组件", "plotly")
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$feature_parcoords, {
      switch_to_widget("iadv_parcoords", "高级组件", "plotly")
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$feature_force, {
      switch_to_widget("iadv_force_country_sim", "高级组件", "ui")
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$feature_dashboard, {
      switch_to_widget("iadv_dashboard_overview", "高级组件", "ui")
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$feature_full_dt, {
      switch_to_widget("iadv_dt_full_panel", "高级组件", "dt")
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$feature_heatmap, {
      switch_to_widget("iadv_heatmap_year_inc", "高级组件", "plotly")
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$feature_radar, {
      switch_to_widget("iadv_polar_radar", "高级组件", "plotly")
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$feature_treemap, {
      switch_to_widget("iadv_treemap_che", "高级组件", "plotly")
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$feature_stream, {
      switch_to_widget("iadv_hc_stream", "高级组件", "ui")
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$feature_gauge, {
      switch_to_widget("iadv_ec_liquid", "高级组件", "ui")
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$feature_master, {
      switch_to_widget("iadv_dt_master_browse", "高级组件", "dt")
    }, ignoreInit = TRUE)

    current_spec <- shiny::reactive({
      reg <- filtered_r()
      wid <- input$widget
      if (is.null(wid) || !wid %in% reg$id) {
        if (!nrow(reg)) return(NULL)
        wid <- if (default_widget_id %in% reg$id) default_widget_id else reg$id[1]
      }
      reg[match(wid, reg$id), , drop = FALSE]
    })

    output$widget_type_deck <- shiny::renderUI({
      reg <- registry_r()
      filt <- filtered_r()
      type_counts <- sort(table(reg$type), decreasing = TRUE)
      group_counts <- sort(table(reg$group), decreasing = TRUE)
      top_group <- if (length(group_counts)) names(group_counts)[1] else "—"

      type_card <- function(type, label, note, tone = "primary") {
        count <- if (type %in% names(type_counts)) unname(type_counts[[type]]) else 0
        pct <- if (nrow(reg)) count / nrow(reg) * 100 else 0
        htmltools::div(
          class = paste("widget-type-card", paste0("widget-type-", tone)),
          htmltools::div(
            class = "widget-type-card-head",
            htmltools::span(label),
            htmltools::strong(fmt_v3_num(count))
          ),
          htmltools::div(
            class = "widget-type-meter",
            htmltools::span(style = sprintf("width:%s%%", max(5, min(100, pct))))
          ),
          htmltools::p(note)
        )
      }

      htmltools::div(
        class = "widget-type-deck",
        htmltools::div(
          class = "widget-type-summary",
          htmltools::span("Asset radar"),
          htmltools::strong("组件资产覆盖雷达"),
          htmltools::p(paste0(
            "当前运行环境登记 ", fmt_v3_num(nrow(reg)), " 个组件，",
            "筛选后保留 ", fmt_v3_num(nrow(filt)), " 个；最大分组为 ",
            top_group, "。"
          ))
        ),
        type_card("plotly", "Plotly", "动态图、趋势线、热力图和分布图的主力输出。", "primary"),
        type_card("leaflet", "Leaflet", "地图类组件支持空间定位、图层切换和区域扫描。", "good"),
        type_card("reactable", "Reactable", "表格资产承担排序、搜索、明细复核和导出前检查。", "warn"),
        type_card("dt", "DT", "适合大字段浏览和快速筛选。", "secondary"),
        type_card("ui", "HTML / Network", "组合型 HTML、网络图和专题摘要进入 UI 渲染通道。", "neutral")
      )
    })

    output$registry_kpis <- shiny::renderUI({
      reg <- registry_r()
      filt <- filtered_r()
      mod_v3_kpi_grid(
        mod_v3_kpi("登记组件", fmt_v3_num(nrow(reg)), hint = "当前运行环境可见函数", tone = "primary"),
        mod_v3_kpi("筛选结果", fmt_v3_num(nrow(filt)), hint = "目录实时过滤", tone = "secondary"),
        mod_v3_kpi("输出类型", fmt_v3_num(length(unique(reg$type))), hint = paste(sort(unique(reg$type)), collapse = " / "), tone = "good"),
        mod_v3_kpi("地图组件", fmt_v3_num(sum(reg$needs_world)), hint = "自动传入 world_sf", tone = "warn")
      )
    })

    output$registry_table <- reactable::renderReactable({
      reg <- filtered_r()
      show <- reg[, c("group", "title", "type", "fn", "source", "deps", "needs_world"), drop = FALSE]
      names(show) <- c("分组", "标题", "类型", "函数", "来源", "依赖", "地图对象")
      reactable::reactable(show,
        searchable = FALSE, highlight = TRUE, compact = TRUE,
        defaultPageSize = 12,
        columns = list(
          `标题` = reactable::colDef(minWidth = 220),
          `函数` = reactable::colDef(minWidth = 180,
            style = list(fontFamily = "'JetBrains Mono', monospace")),
          `来源` = reactable::colDef(minWidth = 150),
          `地图对象` = reactable::colDef(width = 80)
        ))
    })

    output$widget_header <- shiny::renderUI({
      spec <- current_spec()
      if (is.null(spec)) return(mod_widget_missing("没有可用组件", "当前筛选条件下没有匹配项。"))
      htmltools::div(
        class = "widget-native-head",
        htmltools::div(
          htmltools::span(class = "v3-card-kicker", spec$group),
          htmltools::h3(spec$title)
        ),
        htmltools::p(spec$desc),
        htmltools::div(
          class = "widget-native-meta",
          htmltools::span(spec$type),
          htmltools::span(spec$fn),
          htmltools::span(if (nzchar(spec$deps)) spec$deps else "base")
        )
      )
    })

    output$widget_slot <- shiny::renderUI({
      spec <- current_spec()
      if (is.null(spec)) return(NULL)
      mod_widget_output_ui(ns = session$ns, widget_id = spec$id, type = spec$type)
    })

    call_widget <- function(spec) {
      fn_name <- spec$fn
      if (!exists(fn_name, mode = "function")) {
        return(mod_widget_missing("函数未加载", paste("当前环境找不到", fn_name, "。")))
      }
      deps <- trimws(unlist(strsplit(spec$deps %||% "", ",", fixed = TRUE)))
      deps <- deps[nzchar(deps)]
      missing <- deps[!vapply(deps, requireNamespace, logical(1), quietly = TRUE)]
      if (length(missing)) {
        return(mod_widget_missing("缺少可选依赖",
          paste("该组件需要安装：", paste(missing, collapse = ", "))))
      }
      f <- get(fn_name, mode = "function")
      formals_names <- names(formals(f))
      defaults <- spec$params[[1]]
      if (is.null(defaults) || !is.list(defaults)) defaults <- list()
      args <- defaults[names(defaults) %in% formals_names]
      if ("master" %in% formals_names) args$master <- master_r()
      if ("world_sf" %in% formals_names) args$world_sf <- world_sf
      if ("year" %in% formals_names) args$year <- input$year
      if ("y1" %in% formals_names) args$y1 <- min(master_r()$year, na.rm = TRUE)
      if ("y2" %in% formals_names) args$y2 <- input$year
      if ("iso" %in% formals_names) args$iso <- input$country %||% "CHN"
      if ("country_iso" %in% formals_names) args$country_iso <- input$country %||% "CHN"
      if ("countries" %in% formals_names) args$countries <- unique(c(input$country %||% "CHN", "USA", "IND", "BRA", "DEU", "JPN"))
      if ("isos" %in% formals_names) args$isos <- unique(c(input$country %||% "CHN", "USA", "IND", "BRA", "DEU", "JPN"))
      if ("var" %in% formals_names) args$var <- "hf3_che"
      missing_required <- setdiff(
        formals_names[vapply(formals(f), identical, logical(1), quote(expr = ))],
        names(args)
      )
      if (length(missing_required)) {
        return(mod_widget_missing("缺少组件参数",
          paste("该组件还需要：", paste(missing_required, collapse = ", "), "。")))
      }
      obj <- tryCatch(do.call(f, args), error = function(e) {
        mod_widget_missing("组件渲染失败", conditionMessage(e))
      })
      if (is.null(obj)) {
        mod_widget_missing("组件返回为空", "函数存在，但在当前数据、年份或依赖条件下没有生成可显示对象。")
      } else {
        obj
      }
    }

    rendered_id <- shiny::reactiveVal(NULL)
    shiny::observeEvent(current_spec(), {
      spec <- current_spec()
      if (is.null(spec)) return()
      out_id <- paste0("widget_", spec$id)
      rendered_id(out_id)
      mod_render_native_widget(
        output = output,
        output_id = out_id,
        type = spec$type,
        builder = function() call_widget(spec)
      )
    }, ignoreInit = FALSE)
  })
}
