mod_home_ui <- function(id) {
  ns <- shiny::NS(id)

  nav_button <- function(target, label, icon, primary = FALSE) {
    htmltools::tags$button(
      class = paste("home-cover-action", if (primary) "home-cover-action-primary" else ""),
      type = "button",
      onclick = sprintf(
        "Shiny.setInputValue('%s','%s',{priority:'event'});",
        ns("nav_to"), target
      ),
      shiny::icon(icon),
      htmltools::span(label)
    )
  }

  bslib::nav_panel(
    title = "\u9996\u9875",
    value = "home",
    mod_v3_page_body(
      wide = TRUE,
      class = "home-page home-cover-page",
      htmltools::tags$section(
        class = "home-cover",
        htmltools::div(
          class = "home-cover-copy",
          htmltools::span(class = "home-cover-kicker", "GLOBAL HEALTH SPENDING DASHBOARD"),
          htmltools::h1(
            htmltools::span("\u5168\u7403\u536b\u751f\u652f\u51fa"),
            htmltools::span("2000-2023")
          ),
          htmltools::p(
            class = "home-cover-lead",
            "WHO GHED \u4e0e WDI \u6570\u636e\u7684\u4ea4\u4e92\u5f0f\u4eea\u8868\u76d8\uff0c\u7528\u4e8e\u67e5\u770b\u536b\u751f\u7b79\u8d44\u3001\u5bb6\u5ead\u81ea\u4ed8\u3001\u516c\u5171\u6295\u5165\u4e0e\u5065\u5eb7\u4ea7\u51fa\u7684\u957f\u671f\u53d8\u5316\u3002"
          ),
          htmltools::div(
            class = "home-cover-meta",
            htmltools::span("\u5e84\u9882"),
            htmltools::span("20241334"),
            htmltools::span("WHO GHED + WDI")
          ),
          htmltools::div(
            class = "home-cover-actions",
            nav_button("overview", "\u8fdb\u5165\u4eea\u8868\u76d8", "arrow-right", TRUE),
            nav_button("mapstudio", "\u6253\u5f00\u5730\u56fe", "map"),
            htmltools::tags$a(
              class = "home-cover-action",
              href = "https://2711944586.github.io/R/",
              target = "_blank",
              rel = "noreferrer",
              shiny::icon("file-lines"),
              htmltools::span("\u9759\u6001\u62a5\u544a")
            )
          )
        ),
        htmltools::div(
          class = "home-cover-visual",
          htmltools::div(
            class = "home-cover-years",
            htmltools::span("2000"),
            htmltools::span("2023")
          ),
          htmltools::div(
            class = "home-cover-bars",
            htmltools::span(),
            htmltools::span(),
            htmltools::span(),
            htmltools::span(),
            htmltools::span(),
            htmltools::span()
          ),
          htmltools::div(class = "home-cover-caption", "\u536b\u751f\u7b79\u8d44\u00b7\u8d22\u52a1\u4fdd\u62a4\u00b7\u5065\u5eb7\u4ea7\u51fa")
        )
      )
    )
  )
}

mod_home_server <- function(id, master_r, parent_session = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input$nav_to, {
      target <- input$nav_to
      if (!is.null(parent_session) && length(target) && nzchar(target)) {
        bslib::nav_select(id = "main_nav", selected = target, session = parent_session)
      }
    }, ignoreInit = TRUE)
  })
}
