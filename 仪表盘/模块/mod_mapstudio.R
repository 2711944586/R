
mod_mapstudio_ui <- function(id, country_choices, indicator_choices,
                             year_min, year_max) {
  ns <- shiny::NS(id)
  map_indicator_choices <- c(
    "人均 CHE (USD 2023)" = "che_pc_usd2023",
    "总 CHE (USD 2023)" = "che_usd2023",
    "OOPS / CHE (%)" = "hf3_che",
    "GGHED / CHE (%)" = "gghed_che",
    "PVT-D / CHE (%)" = "pvtd_che",
    "EXT / CHE (%)" = "ext_che",
    "预防支出 HC6 (%)" = "hc6_che",
    "预期寿命" = "life_exp",
    "U5MR" = "u5mr"
  )
  map_indicator_choices <- unique(c(map_indicator_choices, indicator_choices))

  bslib::nav_panel(
    title = "Map Studio",
    value = "mapstudio",
    mod_v3_hero(
      kicker = "SPATIAL ANALYTICS",
      title = "Map Studio · 全球卫生支出的空间工作台",
      lead = paste(
        "地图在这里不是装饰图，而是一套可切换、可比较、可联动的分析界面。",
        "你可以固定年份观察横截面，也可以拉开起止年份查看变化；点击国家后，右侧 KPI、趋势线和同组排行会同步更新，",
        "从空间模式直接进入国家证据。"
      ),
      meta = list("Leaflet 联动", "Plotly 投影地图", "国家点击", "双年比较")
    ),
    mod_v3_page_body(
      wide = TRUE,
      mod_v3_badge_row(
        "20 类地图能力",
        "polygon layerId = ISO3",
        "点击联动国家画像",
        "双年与动画视角",
        tone = "primary"
      ),
      mod_v3_story_grid(
        columns = 3,
        mod_v3_insight(
          kicker = "地图阅读",
          title = "先看聚集，再查国家",
          text = "颜色聚集只能提示区域模式，不能替代表格复核；点击国家后，右侧会给出同一指标的时间轨迹和同组位置。",
          tone = "primary",
          icon = "01"
        ),
        mod_v3_insight(
          kicker = "尺度选择",
          title = "对数轴用于量级，分位数用于分层",
          text = "人均支出和总额跨度很大，自动尺度会优先使用对数；若关注相对等级，分位数模式更适合比较国家所处层级。",
          tone = "secondary",
          icon = "02"
        ),
        mod_v3_insight(
          kicker = "变化比较",
          title = "双年差值比单年颜色更接近政策问题",
          text = "同一国家在 2000 年与最新年份之间的变化，能揭示财政扩张、自付压力下降或健康产出改善是否同步发生。",
          tone = "good",
          icon = "03"
        )
      ),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 330,
          shiny::selectInput(ns("mode"), "地图模式",
            choices = c(
              "单指标分布" = "choropleth",
              "五分位分层" = "quintile",
              "CHE × 寿命双变量" = "bivariate",
              "CHE 变化 2000→最新" = "change_che_pc",
              "OOPS 变化 2000→最新" = "change_oop",
              "同支出寿命残差" = "efficiency",
              "总 CHE 气泡" = "bubble_che_total",
              "OOPS 气泡" = "bubble_oop",
              "高 OOPS Top 10" = "top10_oop",
              "国家点图" = "country_points",
              "双年图层比较" = "dual_compare",
              "跨年图层切换" = "year_layers",
              "Plotly 年份动画" = "plotly_animation",
              "Plotly 投影地图" = "plotly_choropleth"
            )
          ),
          shiny::selectInput(ns("indicator"), "指标",
            choices = map_indicator_choices, selected = "hf3_che"),
          shiny::sliderInput(ns("year"), "年份",
            min = year_min, max = year_max, value = year_max,
            step = 1, sep = ""),
          shiny::sliderInput(ns("year_window"), "起止年份",
            min = year_min, max = year_max, value = c(year_min, year_max),
            step = 1, sep = ""),
          shiny::radioButtons(ns("scale"), "色阶尺度",
            choices = c("自动" = "auto", "线性" = "linear",
                        "对数" = "log", "分位数" = "quantile"),
            selected = "auto", inline = TRUE),
          shiny::selectInput(ns("basemap"), "底图",
            choices = c("Positron" = "positron", "Voyager" = "voyager",
                        "Dark Matter" = "dark", "Terrain" = "terrain"),
            selected = "positron"),
          shiny::selectizeInput(ns("continents"), "区域筛选",
            choices = continent_choices, multiple = TRUE),
          shiny::selectizeInput(ns("incomes"), "收入组筛选",
            choices = income_choices, multiple = TRUE),
          shiny::selectizeInput(ns("country"), "国家联动",
            choices = country_choices, selected = "CHN",
            options = list(placeholder = "搜索国家或 ISO3")),
          mod_v3_sidebar_note(
            title = "操作逻辑",
            text = "先选择地图模式和指标，再用年份窗口决定横截面或变化范围。点击地图上的国家会覆盖国家选择器，右侧所有证据随之刷新。",
            bullets = c(
              "单指标、五分位、双变量和残差使用 polygon 点击。",
              "气泡与 Top10 模式使用国家点点击。",
              "Plotly 模式保留动画和投影视角，国家联动以选择器为准。"
            )
          )
        ),
        shiny::uiOutput(ns("mapstudio_kpis")),
        bslib::layout_columns(
          col_widths = c(8, 4),
          mod_card(
            kicker = "Primary map",
            title = "Leaflet 空间扫描",
            mod_v3_chart_guide(
              title = "读图顺序",
              text = "先判断颜色是否形成区域聚集，再点击具体国家查看右侧联动。面积大的国家容易被视觉放大，最终判断应回到同组排行和时间线。",
              bullets = c("缺失值保留为空色，不做空间插补。",
                          "变化地图中的颜色表示起止年份差值或 log 倍数。"),
              tone = "info"
            ),
            shiny::conditionalPanel(
              condition = sprintf("input['%s'] != 'plotly_animation' && input['%s'] != 'plotly_choropleth'", ns("mode"), ns("mode")),
              mod_spinner(leaflet::leafletOutput(ns("map"), height = 650))
            ),
            shiny::conditionalPanel(
              condition = sprintf("input['%s'] == 'plotly_animation' || input['%s'] == 'plotly_choropleth'", ns("mode"), ns("mode")),
              mod_spinner(plotly::plotlyOutput(ns("plotly_map"), height = 650))
            ),
            footer = "所有地图读取 master_enriched 与 world_sf_obj；地图点击只改变当前页面的国家联动状态。"
          ),
          htmltools::div(
            class = "mapstudio-side-stack",
            mod_card(
              kicker = "Selected country",
              title = "国家时间线",
              shiny::uiOutput(ns("country_note")),
              mod_spinner(plotly::plotlyOutput(ns("country_trend"), height = 310)),
              footer = "同一指标的历史轨迹帮助判断当前颜色来自长期状态还是单年波动。"
            ),
            mod_card(
              kicker = "Peer rank",
              title = "同组排行与数值复核",
              mod_spinner(reactable::reactableOutput(ns("peer_rank"))),
              footer = "同组优先使用当前国家收入组；若收入组缺失则回到当前筛选范围。"
            )
          )
        ),
        mod_card(
          kicker = "Projection view",
          title = "Plotly 投影地图备查",
          mod_v3_chart_guide(
            title = "为什么保留第二张地图",
            text = "Leaflet 更适合点击和空间定位，Plotly 投影图更适合动画、截图和整体构图。两者使用同一筛选口径，但承担不同阅读任务。",
            tone = "good"
          ),
          mod_spinner(plotly::plotlyOutput(ns("secondary_plotly"), height = 440))
        )
      )
    )
  )
}

mod_mapstudio_server <- function(id, master_r, world_sf, year_min, year_max) {
  shiny::moduleServer(id, function(input, output, session) {
    selected_iso <- shiny::reactiveVal("CHN")

    shiny::observeEvent(input$country, {
      if (!is.null(input$country) && nzchar(input$country)) selected_iso(input$country)
    }, ignoreInit = FALSE)

    shiny::observeEvent(input$map_shape_click$id, {
      if (!is.null(input$map_shape_click$id) && nzchar(input$map_shape_click$id)) {
        selected_iso(input$map_shape_click$id)
        shiny::updateSelectizeInput(session, "country", selected = input$map_shape_click$id)
      }
    })

    shiny::observeEvent(input$map_marker_click$id, {
      if (!is.null(input$map_marker_click$id) && nzchar(input$map_marker_click$id)) {
        selected_iso(input$map_marker_click$id)
        shiny::updateSelectizeInput(session, "country", selected = input$map_marker_click$id)
      }
    })

    spec_r <- shiny::reactive({
      yrs <- input$year_window %||% c(year_min, year_max)
      list(
        mode = input$mode %||% "choropleth",
        indicator = input$indicator %||% "hf3_che",
        year = input$year %||% year_max,
        year_start = yrs[1],
        year_end = yrs[2],
        scale = input$scale %||% "auto",
        basemap = input$basemap %||% "positron",
        continents = input$continents %||% character(),
        incomes = input$incomes %||% character(),
        iso = selected_iso()
      )
    })

    output$map <- leaflet::renderLeaflet({
      mod_mapstudio_leaflet(spec_r(), master_r(), world_sf)
    })

    output$plotly_map <- plotly::renderPlotly({
      mod_mapstudio_plotly(spec_r(), master_r())
    })

    output$secondary_plotly <- plotly::renderPlotly({
      sp <- spec_r()
      sp$mode <- if (sp$mode == "plotly_animation") "plotly_animation" else "plotly_choropleth"
      mod_mapstudio_plotly(sp, master_r())
    })

    selected_row <- shiny::reactive({
      d <- master_r()
      iso <- selected_iso()
      year <- input$year %||% year_max
      out <- d[d$iso3_code == iso & d$year == year, , drop = FALSE]
      if (!nrow(out)) out <- d[d$iso3_code == iso, , drop = FALSE]
      if (!nrow(out)) return(NULL)
      out[which.max(out$year), , drop = FALSE]
    })

    output$mapstudio_kpis <- shiny::renderUI({
      d <- selected_row()
      var <- input$indicator %||% "hf3_che"
      if (is.null(d)) {
        return(mod_widget_missing("未选择国家", "请在地图上点击国家，或使用侧栏搜索框选择 ISO3。"))
      }
      mod_v3_kpi_grid(
        mod_v3_kpi("国家", d$country_name[1] %||% d$iso3_code[1], hint = paste(d$iso3_code[1], d$year[1]), tone = "primary"),
        mod_v3_kpi(mod_mapstudio_metric_label(var), mod_mapstudio_metric_value(d[[var]][1], var), hint = "当前指标", tone = "secondary"),
        mod_v3_kpi("OOPS / CHE", mod_mapstudio_metric_value(d$hf3_che[1], "hf3_che"), hint = "家庭现金支付压力", tone = "bad"),
        mod_v3_kpi("GGHED / CHE", mod_mapstudio_metric_value(d$gghed_che[1], "gghed_che"), hint = "公共筹资占比", tone = "good")
      )
    })

    output$country_note <- shiny::renderUI({
      d <- selected_row()
      if (is.null(d)) return(NULL)
      htmltools::p(
        class = "card-note",
        sprintf(
          "%s 在 %s 年属于 %s / %s。下方时间线只展示当前指标，因此可以直接判断地图上的空间位置是否由长期趋势支撑。",
          d$country_name[1] %||% d$iso3_code[1],
          d$year[1],
          d$continent[1] %||% "未知区域",
          d$income_group[1] %||% "未分组"
        )
      )
    })

    output$country_trend <- plotly::renderPlotly({
      d <- master_r()
      iso <- selected_iso()
      var <- input$indicator %||% "hf3_che"
      dd <- d[d$iso3_code == iso & is.finite(d[[var]]), , drop = FALSE]
      if (!nrow(dd)) return(plotly::plotly_empty())
      p <- plotly::plot_ly(dd, x = ~year, y = dd[[var]],
        type = "scatter", mode = "lines+markers",
        line = list(color = "#1d3f5f", width = 3),
        marker = list(color = "#c46327", size = 7),
        text = ~country_name,
        hovertemplate = paste0("<b>%{text}</b><br>%{x}<br>",
                               mod_mapstudio_metric_label(var), ": %{y:.2f}<extra></extra>")) |>
        plotly::layout(
          xaxis = list(title = NULL),
          yaxis = list(title = mod_mapstudio_metric_label(var)),
          margin = list(t = 18, r = 12, b = 42, l = 58),
          paper_bgcolor = "#ffffff",
          plot_bgcolor = "#ffffff"
        )
      mod_v3_plotly(p)
    })

    output$peer_rank <- reactable::renderReactable({
      d <- master_r()
      var <- input$indicator %||% "hf3_che"
      year <- input$year %||% year_max
      iso <- selected_iso()
      slice <- d[d$year == year & is.finite(d[[var]]), , drop = FALSE]
      sel <- slice[slice$iso3_code == iso, , drop = FALSE]
      peer_income <- if (nrow(sel) && "income_group" %in% names(sel)) {
        sel$income_group[1]
      } else {
        NA_character_
      }
      if (!is.na(peer_income) && nzchar(peer_income)) {
        slice <- slice[!is.na(slice$income_group) & slice$income_group == peer_income, , drop = FALSE]
      }
      if (length(input$continents %||% character())) slice <- slice[slice$continent %in% input$continents, , drop = FALSE]
      if (!nrow(slice)) return(reactable::reactable(data.frame()))
      slice <- slice[order(-slice[[var]]), , drop = FALSE]
      slice$rank <- seq_len(nrow(slice))
      tab <- slice[, intersect(c("rank", "country_name", "iso3_code", "continent", "income_group", var), names(slice)), drop = FALSE]
      names(tab)[names(tab) == var] <- mod_mapstudio_metric_label(var)
      reactable::reactable(tab,
        searchable = TRUE, highlight = TRUE, compact = TRUE,
        defaultPageSize = 8,
        defaultColDef = reactable::colDef(maxWidth = 180),
        rowStyle = function(index) {
          if (!is.na(tab$iso3_code[index]) && identical(tab$iso3_code[index], iso)) {
            list(background = "#fff3df", fontWeight = 700)
          } else {
            NULL
          }
        })
    })
  })
}
