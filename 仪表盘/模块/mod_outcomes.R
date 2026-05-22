
mod_outcomes_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = "\u4ea7\u51fa Outcomes",
    value = "outcomes",
    mod_v3_hero(
      kicker = "\u5065\u5eb7\u4ea7\u51fa",
      title = "\u8d44\u91d1 \u2192 \u5bff\u547d\uff1a\u6536\u5165\u7ec4\u95f4\u7684\u8fb9\u9645\u4ea7\u51fa",
      lead = paste(
        "\u5c06\u6700\u65b0\u5e74\u7684\u4eba\u5747 CHE \u4e0e\u9884\u671f\u5bff\u547d\u8054\u7cfb\u8d77\u6765\uff0c",
        "\u5e76\u6309 World Bank \u6536\u5165\u7ec4\u5206\u6bb5\u4f30\u8ba1 life_exp ~ log(CHE per capita) \u659c\u7387\u3002",
        "\u9875\u9762\u7528\u4e8e\u89c2\u5bdf\u4e0d\u540c\u53d1\u5c55\u9636\u6bb5\u4e0b\u652f\u51fa\u589e\u52a0\u4e0e\u5bff\u547d\u6539\u5584\u7684\u5173\u8054\u5f3a\u5ea6\u3002"
      ),
      meta = list("Latest-year cross section", "log CHE per capita", "Income-group OLS", "95% CI")
    ),
    mod_v3_page_body(
      wide = TRUE,
      mod_v3_badge_row(
        "\u6536\u5165\u7ec4\u5206\u6bb5",
        "\u5bf9\u6570 CHE",
        "\u5bff\u547d\u5f39\u6027",
        "\u63cf\u8ff0\u6027\u5173\u8054",
        tone = "good"
      ),
      mod_v3_story_grid(
        columns = 3,
        mod_v3_insight(
          kicker = "Gradient",
          title = "\u6563\u70b9\u56fe\u663e\u793a\u536b\u751f\u6295\u5165\u68af\u5ea6",
          text = "\u4eba\u5747 CHE \u91c7\u7528 log \u5c3a\u5ea6\uff0c\u53ef\u540c\u65f6\u5bb9\u7eb3\u4f4e\u6536\u5165\u548c\u9ad8\u6536\u5165\u56fd\u5bb6\u7684\u5de8\u5927\u91cf\u7ea7\u5dee\u5f02\u3002",
          tone = "primary"
        ),
        mod_v3_insight(
          kicker = "Elasticity",
          title = "\u659c\u7387\u56de\u7b54\u201c\u591a\u6295\u5165\u4e00\u4e9b\u8fd8\u80fd\u589e\u52a0\u591a\u5c11\u201d",
          text = "\u5206\u7ec4 OLS \u659c\u7387\u662f\u5bff\u547d\u5bf9 log CHE \u7684\u8fb9\u9645\u5173\u8054\uff0c\u5e38\u89c1\u6a21\u5f0f\u662f\u4f4e\u6536\u5165\u7ec4\u659c\u7387\u66f4\u5927\u3002",
          tone = "secondary"
        ),
        mod_v3_insight(
          kicker = "Caution",
          title = "\u5173\u8054\u4e0d\u7b49\u4e8e\u652f\u51fa\u7684\u51c0\u6548\u679c",
          text = "\u6559\u80b2\u3001\u57fa\u7840\u8bbe\u65bd\u3001\u75be\u75c5\u8d1f\u62c5\u548c\u4eba\u53e3\u7ed3\u6784\u90fd\u4f1a\u5f71\u54cd\u5bff\u547d\uff0c\u56e0\u6b64\u672c\u9875\u53ea\u4f5c\u63cf\u8ff0\u6027\u4fe1\u53f7\u3002",
          tone = "warn"
        )
      ),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 292,
          shiny::checkboxGroupInput(ns("groups"), "\u663e\u793a\u6536\u5165\u7ec4",
            choices = c("Low income", "Lower middle income",
                         "Upper middle income", "High income"),
            selected = c("Low income", "Lower middle income",
                          "Upper middle income", "High income")),
          mod_v3_sidebar_note(
            "\u5f39\u6027\u5b9a\u4e49",
            "\u5f39\u6027\u8868\u683c\u4e2d\u7684 \u03b2 \u6765\u81ea\u6bcf\u4e2a\u6536\u5165\u7ec4\u5185\u90e8\u7684 life_exp ~ log(CHE/cap) \u56de\u5f52\u3002",
            bullets = c("\u4ec5\u7528\u6700\u65b0\u5e74\u622a\u9762", "\u6bcf\u7ec4\u81f3\u5c11 5 \u4e2a\u89c2\u6d4b\u624d\u4f30\u8ba1", "\u4fe1\u8d56\u533a\u95f4\u8d8a\u5bbd\u4ee3\u8868\u4e0d\u786e\u5b9a\u6027\u8d8a\u9ad8")
          )
        ),
        shiny::uiOutput(ns("kpi_strip")),
        mod_v3_rail(list(
          list(title = "\u9009\u7ec4", text = "\u53ef\u5355\u72ec\u67e5\u770b\u67d0\u4e00\u6536\u5165\u7ec4\u6216\u4fdd\u7559\u5168\u90e8\u7ec4\u522b\u3002"),
          list(title = "\u770b\u659c\u7387", text = "\u7ebf\u8d8a\u9661\uff0c\u8868\u793a log CHE \u589e\u52a0\u4e0e\u5bff\u547d\u63d0\u5347\u7684\u5173\u8054\u8d8a\u5f3a\u3002"),
          list(title = "\u67e5\u533a\u95f4", text = "\u8868\u683c\u7ed9\u51fa \u03b2 \u548c 95% CI\uff0c\u7528\u4e8e\u5224\u65ad\u4f30\u8ba1\u7a33\u5b9a\u6027\u3002"),
          list(title = "\u907f\u514d\u8fc7\u5ea6\u89e3\u8bfb", text = "\u8fd9\u662f\u622a\u9762\u5173\u8054\uff0c\u4e0d\u662f\u968f\u673a\u5b9e\u9a8c\u6216\u51c6\u5b9e\u9a8c\u3002")
        )),
        mod_card(
          kicker = "\u4ea7\u51fa\u68af\u5ea6",
          title = "log CHE \u00d7 \u5bff\u547d \u00d7 \u6536\u5165\u7ec4",
          mod_v3_chart_guide(
            "\u8bfb\u56fe\u65b9\u6cd5",
            "\u6bcf\u4e2a\u70b9\u662f\u4e00\u4e2a\u56fd\u5bb6\uff0c\u989c\u8272\u4e3a\u6536\u5165\u7ec4\uff1b\u7ec4\u5185\u56de\u5f52\u7ebf\u7528\u4e8e\u6bd4\u8f83\u4e0d\u540c\u53d1\u5c55\u9636\u6bb5\u7684\u6295\u5165-\u4ea7\u51fa\u5173\u8054\u3002",
            bullets = c("\u6a2a\u8f74\u662f log(CHE/cap)", "\u56fe\u4f8b\u53ef\u7528\u4e8e\u9690\u85cf\u6536\u5165\u7ec4", "\u4e0d\u540c\u7ec4\u522b\u7684\u6837\u672c\u6570\u5f71\u54cd\u4f30\u8ba1\u7a33\u5b9a\u6027")
          ),
          mod_spinner(plotly::plotlyOutput(ns("elasticity_plot"), height = 540))
        ),
        mod_card(
          kicker = "\u56de\u5f52\u53c2\u6570",
          title = "\u5f39\u6027\u7cfb\u6570\uff08\u6309\u6536\u5165\u7ec4\u5206\u6bb5 OLS\uff09",
          mod_v3_chart_guide(
            "\u8868\u683c\u7528\u9014",
            "\u8868\u683c\u4e2d \u03b2 \u8868\u793a log CHE \u589e\u52a0 1 \u4e2a\u5355\u4f4d\u4e0e\u9884\u671f\u5bff\u547d\u7684\u5e74\u6570\u5173\u8054\uff0cCI \u7528\u4e8e\u5224\u65ad\u4e0d\u786e\u5b9a\u6027\u3002",
            tone = "good"
          ),
          mod_spinner(reactable::reactableOutput(ns("elasticity_table"))),
          footer = "\u5f39\u6027\u662f\u6700\u65b0\u5e74\u622a\u9762\u63cf\u8ff0\u6027\u4f30\u8ba1\uff0c\u672a\u63a7\u5236\u6240\u6709\u6f5c\u5728\u6df7\u6742\u56e0\u7d20\u3002"
        )
      )
    )
  )
}

mod_outcomes_server <- function(id, master_r, year_max) {
  shiny::moduleServer(id, function(input, output, session) {
    out_data <- shiny::reactive({
      m <- master_r()
      d <- m[m$year == year_max &
              is.finite(m$che_pc_usd2023) &
              is.finite(m$life_exp) &
              !is.na(m$income_group), , drop = FALSE]
      d$log_che <- log(d$che_pc_usd2023)
      d <- d[d$income_group %in% input$groups, , drop = FALSE]
      d$income_group <- factor(d$income_group,
        levels = c("Low income", "Lower middle income",
                    "Upper middle income", "High income"))
      d
    })

    output$kpi_strip <- shiny::renderUI({
      d <- out_data()
      shiny::req(nrow(d) > 0)
      mod_v3_kpi_grid(
        mod_v3_kpi("\u5f53\u524d\u6837\u672c", fmt_v3_num(nrow(d)),
                   hint = "\u6700\u65b0\u5e74\u53ef\u7528\u56fd\u5bb6\u6570",
                   tone = "primary"),
        mod_v3_kpi("\u6536\u5165\u7ec4", fmt_v3_num(length(unique(stats::na.omit(d$income_group)))),
                   hint = "\u5f53\u524d\u52fe\u9009\u4e14\u6709\u6570\u636e\u7684\u7ec4\u522b",
                   tone = "secondary"),
        mod_v3_kpi("\u5bff\u547d\u4e2d\u4f4d\u6570", fmt_v3_num(stats::median(d$life_exp, na.rm = TRUE), 1, "\u5e74"),
                   hint = "\u9009\u4e2d\u6837\u672c\u7684\u5bff\u547d\u4e2d\u4f4d\u6570",
                   tone = "good"),
        mod_v3_kpi("\u4eba\u5747 CHE \u4e2d\u4f4d\u6570", fmt_v3_usd(stats::median(d$che_pc_usd2023, na.rm = TRUE)),
                   hint = "2023 \u4e0d\u53d8\u4ef7 USD",
                   tone = "warn")
      )
    })

    output$elasticity_plot <- plotly::renderPlotly({
      d <- out_data()
      shiny::req(nrow(d) > 0)
      pal <- c("Low income" = "#A03B27", "Lower middle income" = "#D89B5B",
               "Upper middle income" = "#4F8FBF", "High income" = "#0B3D5C")
      p <- ggplot2::ggplot(d, ggplot2::aes(log_che, life_exp,
                                            colour = income_group,
                                            text = country_name)) +
        ggplot2::geom_point(size = 2.4, alpha = 0.85) +
        ggplot2::geom_smooth(method = "lm", se = TRUE, linewidth = 1,
                              ggplot2::aes(group = income_group)) +
        ggplot2::scale_colour_manual(values = pal, name = "\u6536\u5165\u7ec4") +
        ggplot2::labs(x = "log \u4eba\u5747 CHE\uff08USD 2023\uff09",
                      y = "\u51fa\u751f\u65f6\u9884\u671f\u5bff\u547d\uff08\u5e74\uff09") +
        ggplot2::theme_minimal(base_size = 12)
      plotly::ggplotly(p, tooltip = "text") |>
        plotly::config(displaylogo = FALSE)
    })

    output$elasticity_table <- reactable::renderReactable({
      d <- out_data()
      shiny::req(nrow(d) > 0)
      out_tab <- d |>
        split(d$income_group) |>
        Filter(f = function(x) nrow(x) >= 5) |>
        lapply(function(x) {
          fit <- stats::lm(life_exp ~ log_che, data = x)
          ci <- tryCatch(stats::confint(fit)[2, ],
                          error = function(e) c(NA, NA))
          data.frame(
            n = nrow(x),
            beta = round(stats::coef(fit)[2], 3),
            ci_low = round(ci[1], 3),
            ci_high = round(ci[2], 3),
            r2 = round(summary(fit)$r.squared, 3)
          )
        }) |>
        do.call(rbind, args = _)
      out_tab$income_group <- rownames(out_tab)
      out_tab <- out_tab[, c("income_group", "n", "beta", "ci_low", "ci_high", "r2")]
      names(out_tab) <- c("收入组", "n", "弹性 \u03b2",
                          "CI 下", "CI 上", "R\u00b2")
      reactable::reactable(out_tab, defaultPageSize = 4,
        pagination = FALSE, highlight = TRUE,
        defaultColDef = reactable::colDef(headerStyle = list(background = "#f1f3f7")))
    })
  })
}
