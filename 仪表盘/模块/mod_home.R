mod_home_ui <- function(id) {
  bslib::nav_panel(
    title = "\u9996\u9875",
    value = "home",
    mod_v3_page_body(
      wide = TRUE,
      class = "home-page home-title-page",
      htmltools::tags$section(
        class = "home-title-cover",
        htmltools::div(
          class = "home-title-motion",
          `aria-hidden` = "true",
          htmltools::span(),
          htmltools::span(),
          htmltools::span(),
          htmltools::span(),
          htmltools::span(),
          htmltools::span()
        ),
        htmltools::h1(
          class = "home-title",
          htmltools::span("\u5168\u7403\u536b\u751f\u652f\u51fa"),
          htmltools::span("2000-2023")
        )
      )
    )
  )
}

mod_home_server <- function(id, master_r, parent_session = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    invisible(NULL)
  })
}
