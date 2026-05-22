mod_home_ui <- function(id) {
  ns <- shiny::NS(id)

  route_card <- function(target, kicker, title, text, icon, tone = "primary") {
    tone_color <- switch(tone,
      primary = "#254f5c", secondary = "#6b6077",
      good = "#587669", warn = "#8f743d", bad = "#8b544e",
      "#5d6965")
    htmltools::tags$button(
      class = "home-route-card",
      type = "button",
      style = sprintf("--tone:%s", tone_color),
      onclick = sprintf(
        "Shiny.setInputValue('%s', '%s', {priority:'event'});",
        ns("nav_to"), target
      ),
      htmltools::span(class = "home-route-icon", shiny::icon(icon)),
      htmltools::span(class = "home-route-kicker", kicker),
      htmltools::strong(class = "home-route-title", title),
      htmltools::span(class = "home-route-text", text)
    )
  }

  bslib::nav_panel(
    title = "\u9996\u9875",
    value = "home",
    mod_v3_page_body(
      wide = TRUE,
      class = "home-page",
      htmltools::tags$section(
        class = "home-hero",
        htmltools::div(
          class = "home-hero-copy",
          htmltools::span(class = "home-kicker", "GLOBAL HEALTH SPENDING DASHBOARD"),
          htmltools::h1("全球卫生支出 2000-2023"),
          htmltools::p(
            class = "home-lead",
            "以 WHO GHED 与 WDI 为数据底座，把卫生筹资、家庭自付、公共财政、外部援助与健康产出放在同一套交互叙事中。首页先给出结论框架，后续页面再展开国家、区域、模型和组件。"
          ),
          htmltools::div(
            class = "home-actions",
            htmltools::tags$button(
              class = "home-action home-action-primary",
              type = "button",
              onclick = sprintf(
                "Shiny.setInputValue('%s','overview',{priority:'event'});",
                ns("nav_to")
              ),
              shiny::icon("chart-simple"),
              htmltools::span("\u8fdb\u5165\u603b\u89c8")
            ),
            htmltools::tags$button(
              class = "home-action",
              type = "button",
              onclick = sprintf(
                "Shiny.setInputValue('%s','widgets',{priority:'event'});",
                ns("nav_to")
              ),
              shiny::icon("sliders"),
              htmltools::span("\u4ea4\u4e92\u7ec4\u4ef6")
            ),
            htmltools::tags$a(
              class = "home-action",
              href = "https://2711944586.github.io/R/",
              target = "_blank",
              rel = "noreferrer",
              shiny::icon("file-lines"),
              htmltools::span("\u9759\u6001\u62a5\u544a")
            )
          )
        ),
        htmltools::div(
          class = "home-hero-panel",
          htmltools::div(class = "home-panel-label", "\u7814\u7a76\u6846\u67b6"),
          htmltools::div(
            class = "home-panel-flow",
            htmltools::span("资金来源"),
            htmltools::span("制度结构"),
            htmltools::span("家庭负担"),
            htmltools::span("健康产出")
          ),
          shiny::uiOutput(ns("home_kpis")),
          htmltools::div(
            class = "home-motion",
            htmltools::span(),
            htmltools::span(),
            htmltools::span(),
            htmltools::span(),
            htmltools::span()
          )
        )
      ),
      htmltools::tags$section(
        class = "home-section",
        htmltools::tags$header(
          class = "home-section-head",
          htmltools::span("READING ORDER"),
          htmltools::h2("\u4ece\u95ee\u9898\u5230\u7ed3\u8bba\u7684\u8def\u5f84")
        ),
        htmltools::div(
          class = "home-route-grid",
          route_card("country", "\u56fd\u5bb6", "\u5148\u770b\u5355\u56fd\u753b\u50cf",
                     "\u4ece\u4e2d\u56fd\u3001\u7f8e\u56fd\u3001\u5370\u5ea6\u7b49\u56fd\u5bb6\u5165\u624b\uff0c\u8bfb\u51fa\u652f\u51fa\u89c4\u6a21\u3001OOPS \u8d1f\u62c5\u548c\u7b79\u8d44\u7ed3\u6784\u7684\u4e2a\u4f53\u8f68\u8ff9\u3002",
                     "location-dot", "primary"),
          route_card("financing", "\u7b79\u8d44", "\u518d\u62c6\u8d44\u91d1\u6765\u6e90",
                     "\u628a\u653f\u5e9c\u3001\u79c1\u4eba\u3001\u5bb6\u5ead\u81ea\u4ed8\u548c\u5916\u63f4\u5206\u5f00\u770b\uff0c\u907f\u514d\u53ea\u7528\u536b\u751f\u603b\u652f\u51fa\u5224\u65ad\u5236\u5ea6\u80fd\u529b\u3002",
                     "sitemap", "secondary"),
          route_card("equity", "\u516c\u5e73", "\u5b9a\u4f4d\u5bb6\u5ead\u98ce\u9669",
                     "OOPS \u662f\u8d22\u52a1\u4fdd\u62a4\u7684\u5165\u53e3\u6307\u6807\uff1b\u5176\u5c3e\u90e8\u5206\u5e03\u5e38\u6bd4\u5e73\u5747\u6570\u66f4\u80fd\u8bf4\u660e\u95ee\u9898\u3002",
                     "scale-balanced", "bad"),
          route_card("outcomes", "\u4ea7\u51fa", "\u5bf9\u7167\u5065\u5eb7\u7ed3\u679c",
                     "\u4eba\u5747 CHE \u8f83\u9ad8\u4e0d\u5fc5\u7136\u5e26\u6765\u66f4\u597d\u4ea7\u51fa\uff0c\u9700\u8981\u540c\u65f6\u8bfb\u5bff\u547d\u3001U5MR \u548c\u9884\u9632\u652f\u51fa\u3002",
                     "heart-pulse", "good"),
          route_card("mapstudio", "\u5de5\u5177", "\u7528\u5730\u56fe\u8fdb\u884c\u590d\u6838",
                     "\u5728 Map Studio \u4e2d\u5207\u6362\u5e74\u4efd\u3001\u6307\u6807\u3001\u6536\u5165\u7ec4\u548c\u5730\u56fe\u5c3a\u5ea6\uff0c\u68c0\u67e5\u7ed3\u8bba\u662f\u5426\u7a33\u5b9a\u3002",
                     "map", "primary"),
          route_card("policy", "\u7ed3\u8bba", "\u6700\u540e\u56de\u5230\u653f\u7b56",
                     "\u653f\u7b56\u9875\u628a\u53d1\u73b0\u8f6c\u6210\u53ef\u64cd\u4f5c\u7684\u5206\u7c7b\u5efa\u8bae\uff0c\u5f3a\u8c03\u8d44\u91d1\u66ff\u4ee3\u3001\u98ce\u9669\u5171\u62c5\u548c\u6570\u636e\u8d28\u91cf\u3002",
                     "clipboard-check", "warn")
        )
      ),
      htmltools::tags$section(
        class = "home-section home-conclusion-section",
        htmltools::tags$header(
          class = "home-section-head",
          htmltools::span("SUMMARY"),
          htmltools::h2("\u9996\u9875\u7ed3\u8bba")
        ),
        htmltools::div(
          class = "home-conclusion-grid",
          mod_v3_insight(
            kicker = "\u7ed3\u8bba 01",
            title = "\u536b\u751f\u603b\u652f\u51fa\u6301\u7eed\u589e\u957f\uff0c\u4f46\u5206\u914d\u683c\u5c40\u5e76\u4e0d\u5747\u8861",
            text = "\u9ad8\u6536\u5165\u56fd\u5bb6\u7ee7\u7eed\u62ac\u9ad8\u5168\u7403\u4eba\u5747\u652f\u51fa\u4e2d\u5fc3\uff0c\u4f4e\u6536\u5165\u548c\u90e8\u5206\u4e2d\u7b49\u6536\u5165\u56fd\u5bb6\u5219\u66f4\u591a\u9762\u5bf9\u670d\u52a1\u8986\u76d6\u548c\u8d22\u52a1\u4fdd\u62a4\u7684\u53cc\u91cd\u7ea6\u675f\u3002",
            tone = "primary"
          ),
          mod_v3_insight(
            kicker = "\u7ed3\u8bba 02",
            title = "OOPS \u662f\u8bc6\u522b\u5bb6\u5ead\u98ce\u9669\u7684\u5173\u952e\u4fe1\u53f7",
            text = "\u5f53 OOPS \u9ad8\u4f4d\u4e0e\u516c\u5171\u7b79\u8d44\u504f\u4f4e\u540c\u65f6\u51fa\u73b0\u65f6\uff0c\u8bf4\u660e\u5bb6\u5ead\u4ecd\u627f\u62c5\u5927\u91cf\u73b0\u91d1\u652f\u4ed8\u98ce\u9669\uff0c\u8fd9\u7c7b\u56fd\u5bb6\u4e0d\u5e94\u53ea\u7528 CHE \u603b\u91cf\u8bc4\u4ef7\u8fdb\u5c55\u3002",
            tone = "bad"
          ),
          mod_v3_insight(
            kicker = "\u7ed3\u8bba 03",
            title = "\u516c\u5171\u7b79\u8d44\u662f\u6297\u51b2\u51fb\u7684\u7a33\u5b9a\u5668",
            text = "COVID-19 \u524d\u540e\u7684\u6570\u636e\u8868\u660e\uff0c\u653f\u5e9c\u548c\u5f3a\u5236\u6027\u9884\u4ed8\u673a\u5236\u80fd\u66f4\u597d\u5730\u5e73\u6ed1\u51b2\u51fb\uff0c\u5916\u63f4\u4f9d\u8d56\u56fd\u5bb6\u5219\u9700\u8981\u63d0\u524d\u8bbe\u8ba1\u56fd\u5185\u66ff\u4ee3\u8d44\u91d1\u8def\u5f84\u3002",
            tone = "good"
          ),
          mod_v3_insight(
            kicker = "\u7ed3\u8bba 04",
            title = "\u4ea7\u51fa\u6548\u7387\u9700\u8981\u8de8\u6a21\u5757\u5224\u65ad",
            text = "\u5bff\u547d\u548c U5MR \u4e0d\u53ea\u53d7\u536b\u751f\u652f\u51fa\u89c4\u6a21\u5f71\u54cd\uff0c\u8fd8\u53d7\u4eba\u53e3\u7ed3\u6784\u3001\u9884\u9632\u6295\u5165\u3001\u8d22\u653f\u7a7a\u95f4\u548c\u536b\u751f\u7cfb\u7edf\u6cbb\u7406\u5f71\u54cd\u3002",
            tone = "secondary"
          )
        )
      )
    )
  )
}

mod_home_server <- function(id, master_r, parent_session = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    output$home_kpis <- shiny::renderUI({
      m <- master_r()
      years <- range(m$year, na.rm = TRUE)
      latest <- max(m$year, na.rm = TRUE)
      d_latest <- m[m$year == latest, , drop = FALSE]
      htmltools::div(
        class = "home-kpi-grid",
        mod_v3_kpi("\u56fd\u5bb6/\u5730\u533a", fmt_v3_num(dplyr::n_distinct(m$iso3_code), 0),
                   hint = sprintf("%s-%s \u957f\u9762\u677f", years[1], years[2]), tone = "primary"),
        mod_v3_kpi("\u6700\u65b0\u5e74", as.character(latest),
                   hint = "Shiny \u4e0e HTML \u5171\u7528\u540c\u4e00\u6570\u636e\u53e3\u5f84", tone = "secondary"),
        mod_v3_kpi("OOPS \u4e2d\u4f4d\u6570",
                   paste0(fmt_v3_num(stats::median(d_latest$hf3_che, na.rm = TRUE), 1), "%"),
                   hint = "\u53cd\u6620\u5bb6\u5ead\u73b0\u91d1\u652f\u4ed8\u538b\u529b", tone = "bad"),
        mod_v3_kpi("GGHED \u4e2d\u4f4d\u6570",
                   paste0(fmt_v3_num(stats::median(d_latest$gghed_che, na.rm = TRUE), 1), "%"),
                   hint = "\u653f\u5e9c\u4e0e\u5f3a\u5236\u7b79\u8d44\u5360 CHE", tone = "good")
      )
    })

    shiny::observeEvent(input$nav_to, {
      target <- input$nav_to
      if (!is.null(parent_session) && length(target) && nzchar(target)) {
        bslib::nav_select(id = "main_nav", selected = target, session = parent_session)
      }
    }, ignoreInit = TRUE)
  })
}
