
mod_efficiency_ui <- function(id, year_min, year_max) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = "\u6548\u7387 Efficiency",
    value = "efficiency",
    mod_v3_hero(
      kicker = "\u6548\u7387\u524d\u6cbf",
      title = "CHE \u2192 \u5065\u5eb7\u7ed3\u679c\uff1a\u6548\u7387\u524d\u6cbf\u4e0e\u5f02\u5e38\u8868\u73b0",
      lead = paste(
        "\u5c06\u4eba\u5747 CHE \u4e0e\u9884\u671f\u5bff\u547d\u653e\u5230\u540c\u4e00\u5f20\u5bf9\u6570\u6563\u70b9\u56fe\u4e0a\uff0c",
        "\u7528\u5e73\u6ed1\u524d\u6cbf\u548c log-log \u6b8b\u5dee\u8bc6\u522b\u201c\u540c\u7b49\u6295\u5165\u4e0b\u7ed3\u679c\u66f4\u597d\u201d\u7684\u56fd\u5bb6\u3002",
        "\u9875\u9762\u4f18\u5148\u5448\u73b0\u6548\u7387\u4fe1\u53f7\uff0c\u4f46\u4e0d\u5c06\u5176\u89e3\u91ca\u4e3a\u56e0\u679c\u8d21\u732e\u3002"
      ),
      meta = list("log \u4eba\u5747 CHE", "\u5bff\u547d\u524d\u6cbf", "\u6b8b\u5dee\u6548\u7387\u5206\u6570")
    ),
    mod_v3_page_body(
      wide = TRUE,
      mod_v3_badge_row(
        "\u622a\u9762\u524d\u6cbf",
        "\u5bf9\u6570\u5c3a\u5ea6",
        "\u6b8b\u5dee\u6392\u540d",
        "\u8d22\u52a1\u6295\u5165-\u7ed3\u679c\u8f6c\u5316",
        tone = "good"
      ),
      mod_v3_story_grid(
        columns = 3,
        mod_v3_insight(
          kicker = "\u524d\u6cbf",
          title = "\u524d\u6cbf\u662f\u540c\u6295\u5165\u6c34\u5e73\u7684\u53c2\u7167\u7ebf",
          text = "\u56fd\u5bb6\u70b9\u8d8a\u9760\u8fd1\u5e73\u6ed1\u4e0a\u6cbf\uff0c\u8d8a\u8bf4\u660e\u5176\u536b\u751f\u652f\u51fa\u66f4\u6709\u6548\u5730\u8f6c\u5316\u4e3a\u5bff\u547d\u7ed3\u679c\u3002",
          tone = "good"
        ),
        mod_v3_insight(
          kicker = "\u6b8b\u5dee",
          title = "\u6b8b\u5dee\u5206\u6570\u9002\u5408\u627e\u6b63\u5411\u5f02\u5e38",
          text = "\u6b8b\u5dee\u4e3a\u6b63\u7684\u56fd\u5bb6\u5728\u540c\u7b49 CHE \u4e0b\u5bff\u547d\u9ad8\u4e8e\u6a21\u578b\u9884\u671f\uff0c\u53ef\u4f5c\u4e3a\u6df1\u5165\u4e2a\u6848\u7814\u7a76\u5019\u9009\u3002",
          tone = "primary"
        ),
        mod_v3_insight(
          kicker = "\u8fb9\u754c",
          title = "\u6548\u7387\u4e0d\u4ee3\u66ff\u5236\u5ea6\u548c\u75be\u75c5\u8d1f\u62c5\u5206\u6790",
          text = "\u5bff\u547d\u540c\u65f6\u53d7\u6559\u80b2\u3001\u6536\u5165\u3001\u4eba\u53e3\u7ed3\u6784\u548c\u75be\u75c5\u8c31\u5f71\u54cd\uff0c\u56e0\u6b64\u672c\u9875\u5b9a\u4f4d\u4e3a\u7b5b\u67e5\u4fe1\u53f7\u3002",
          tone = "warn"
        )
      ),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 292,
          shiny::sliderInput(ns("year"), "\u89c2\u5bdf\u5e74\u4efd",
                              min = year_min, max = year_max, value = year_max,
                              step = 1, sep = ""),
          mod_v3_sidebar_note(
            "\u9605\u8bfb\u987a\u5e8f",
            "\u5148\u770b\u6563\u70b9\u7684\u603b\u4f53\u5f62\u72b6\uff0c\u518d\u770b\u5404\u5927\u6d32\u989c\u8272\u5206\u5e03\uff0c\u6700\u540e\u7528\u6392\u540d\u8868\u627e\u6b63\u5411\u548c\u8d1f\u5411\u6b8b\u5dee\u56fd\u5bb6\u3002",
            bullets = c("\u6a2a\u8f74\u4e3a log \u5c3a\u5ea6", "\u5206\u6570\u4e3a log-log \u56de\u5f52\u6b8b\u5dee", "\u7ed3\u8bba\u9700\u914d\u5408\u7ed3\u6784\u6027\u80cc\u666f\u89e3\u8bfb")
          )
        ),
        shiny::uiOutput(ns("kpi_strip")),
        mod_v3_rail(list(
          list(title = "\u9009\u5e74\u4efd", text = "\u5c06\u5404\u56fd\u653e\u5728\u540c\u4e00\u5e74\u7684\u622a\u9762\u4e2d\u6bd4\u8f83\u3002"),
          list(title = "\u770b\u524d\u6cbf", text = "\u5e73\u6ed1\u7ebf\u663e\u793a\u652f\u51fa\u4e0e\u5bff\u547d\u7684\u5e73\u5747\u8f6c\u5316\u8def\u5f84\u3002"),
          list(title = "\u8bfb\u6b8b\u5dee", text = "\u6b63\u6b8b\u5dee\u8868\u793a\u540c\u6295\u5165\u4e0b\u9ad8\u4e8e\u9884\u671f\u7684\u5bff\u547d\u7ed3\u679c\u3002"),
          list(title = "\u627e\u4e2a\u6848", text = "\u6392\u540d\u8868\u7528\u4e8e\u9009\u62e9\u9700\u8fdb\u4e00\u6b65\u8ffd\u8e2a\u7684\u56fd\u5bb6\u3002")
        )),
        mod_card(
          kicker = "\u524d\u6cbf\u6563\u70b9",
          title = "CHE \u00d7 \u5bff\u547d\u6563\u70b9 + \u6548\u7387\u524d\u6cbf",
          mod_v3_chart_guide(
            "\u8bfb\u56fe\u65b9\u6cd5",
            "\u6bcf\u4e2a\u70b9\u4ee3\u8868\u4e00\u4e2a\u56fd\u5bb6\uff0c\u989c\u8272\u4ee3\u8868\u5927\u6d32\uff1b\u8d8a\u9ad8\u4e8e\u5e73\u6ed1\u7ebf\uff0c\u8bf4\u660e\u5176\u5bff\u547d\u7ed3\u679c\u76f8\u5bf9\u6295\u5165\u66f4\u7a81\u51fa\u3002",
            bullets = c("\u70b9\u51fb\u56fe\u4f8b\u53ef\u9690\u85cf\u5927\u6d32", "\u60ac\u505c\u67e5\u770b\u56fd\u5bb6\u548c\u6b8b\u5dee\u5206\u6570", "\u6a2a\u8f74\u5bf9\u6570\u5316\u540e\u66f4\u9002\u5408\u8de8\u6570\u91cf\u7ea7\u6bd4\u8f83")
          ),
          mod_spinner(plotly::plotlyOutput(ns("dea_plot"), height = 560))
        ),
        bslib::layout_columns(
          col_widths = c(7, 5),
          mod_card(
            kicker = "\u6548\u7387\u6392\u540d",
            title = "\u6548\u7387\u6392\u540d\uff08\u6b8b\u5dee\u8d8a\u6b63\u8d8a\u4f18\uff09",
            mod_v3_chart_guide(
              "\u8868\u683c\u7528\u9014",
              "\u6392\u540d\u8868\u5c06 CHE\u3001\u5bff\u547d\u548c\u6b8b\u5dee\u5206\u6570\u653e\u5728\u540c\u4e00\u884c\uff0c\u4fbf\u4e8e\u533a\u5206\u4f4e\u6295\u5165\u9ad8\u8868\u73b0\u548c\u9ad8\u6295\u5165\u4f4e\u8868\u73b0\u3002",
              tone = "good"
            ),
            mod_spinner(reactable::reactableOutput(ns("eff_rank"))),
            footer = "\u5206\u6570\u57fa\u4e8e\u5f53\u5e74 log(\u9884\u671f\u5bff\u547d) ~ log(\u4eba\u5747 CHE) \u6b8b\u5dee\uff0c\u4ec5\u7528\u4e8e\u63cf\u8ff0\u6027\u6bd4\u8f83\u3002"
          ),
          mod_card(
            kicker = "\u6a21\u578b\u8bca\u65ad",
            title = "\u524d\u6cbf\u56de\u5f52\u6458\u8981",
            mod_v3_chart_guide(
              "\u8bca\u65ad\u542b\u4e49",
              "\u8fd9\u5f20\u8868\u5e2e\u52a9\u5224\u65ad\u5f53\u5e74\u622a\u9762\u7684\u62df\u5408\u5f3a\u5ea6\u548c\u6b8b\u5dee\u79bb\u6563\u8303\u56f4\uff0c\u9632\u6b62\u628a\u5355\u4e2a\u56fd\u5bb6\u7684\u6b8b\u5dee\u8fc7\u5ea6\u89e3\u8bfb\u3002",
              tone = "warn"
            ),
            mod_spinner(reactable::reactableOutput(ns("model_table"))),
            footer = "\u6a21\u578b\u4ec5\u63a7\u5236\u4eba\u5747 CHE\uff0c\u672a\u63a7\u5236\u75be\u75c5\u8d1f\u62c5\u3001\u6559\u80b2\u3001\u5e74\u9f84\u7ed3\u6784\u6216\u533b\u7597\u4ef7\u683c\u5dee\u5f02\u3002"
          )
        )
      )
    )
  )
}

mod_efficiency_server <- function(id, master_r) {
  shiny::moduleServer(id, function(input, output, session) {
    eff_data <- shiny::reactive({
      m <- master_r()
      d <- m[m$year == input$year &
              is.finite(m$che_pc_usd2023) &
              is.finite(m$life_exp), , drop = FALSE]
      shiny::req(nrow(d) >= 30)

      d$log_che  <- log(d$che_pc_usd2023)
      d$log_life <- log(d$life_exp)
      lm_fit <- stats::lm(log_life ~ log_che, data = d)
      d$pred  <- stats::fitted(lm_fit)
      d$resid <- stats::resid(lm_fit)
      d$score <- round(d$resid * 100, 2)
      d
    })

    output$kpi_strip <- shiny::renderUI({
      d <- eff_data()
      shiny::req(nrow(d) > 0)
      top <- d[which.max(d$score), , drop = FALSE]
      mod_v3_kpi_grid(
        mod_v3_kpi("\u89c2\u5bdf\u5e74\u4efd", as.character(input$year),
                   hint = "\u5f53\u524d\u622a\u9762\u5e74\u4efd",
                   tone = "neutral"),
        mod_v3_kpi("\u53ef\u6bd4\u56fd\u5bb6", fmt_v3_num(nrow(d)),
                   hint = "\u540c\u65f6\u5177\u6709 CHE \u548c\u9884\u671f\u5bff\u547d\u7684\u89c2\u6d4b",
                   tone = "primary"),
        mod_v3_kpi("\u5bff\u547d\u4e2d\u4f4d\u6570", fmt_v3_num(stats::median(d$life_exp, na.rm = TRUE), 1, "\u5e74"),
                   hint = "\u5f53\u5e74\u6837\u672c\u4e2d\u4f4d\u6570",
                   tone = "good"),
        mod_v3_kpi("\u6700\u9ad8\u6b8b\u5dee", top$iso3_code[1],
                   hint = sprintf("\u6548\u7387\u5206\u6570 %+0.2f", top$score[1]),
                   tone = "secondary")
      )
    })

    output$dea_plot <- plotly::renderPlotly({
      d <- eff_data()
      pal <- c(Africa = "#C0504D", Americas = "#1B5E88", Asia = "#E8833C",
               Europe = "#2A9D8F", Oceania = "#7B4B94", Antarctica = "#9C9C9C")
      p <- ggplot2::ggplot(d, ggplot2::aes(che_pc_usd2023, life_exp,
                                            colour = continent,
                                            text = paste0(country_name,
                                                          "<br>\u6548\u7387\u5206\u6570: ", score))) +
        ggplot2::geom_point(size = 2.4, alpha = 0.85) +
        ggplot2::geom_smooth(ggplot2::aes(group = 1), method = "loess",
                              se = TRUE, colour = "#1A1A1F", linewidth = 0.6,
                              fill = "#1A1A1F18") +
        ggplot2::scale_x_log10(labels = scales::label_dollar()) +
        ggplot2::scale_colour_manual(values = pal, name = NULL) +
        ggplot2::labs(x = "\u4eba\u5747 CHE\uff08USD 2023\uff0clog\uff09",
                      y = "\u51fa\u751f\u65f6\u9884\u671f\u5bff\u547d") +
        ggplot2::theme_minimal(base_size = 12)
      plotly::ggplotly(p, tooltip = "text") |>
        ghs_plotly_layout() |>
        plotly::config(displaylogo = FALSE)
    })

    output$eff_rank <- reactable::renderReactable({
      d <- eff_data()
      d <- d[order(-d$score), , drop = FALSE]
      tab <- data.frame(
        "\u6392\u540d" = seq_len(nrow(d)),
        "\u56fd\u5bb6" = d$country_name,
        "\u5927\u6d32" = d$continent,
        "\u4eba\u5747 CHE (USD)" = round(d$che_pc_usd2023, 0),
        "\u9884\u671f\u5bff\u547d" = round(d$life_exp, 1),
        "\u6548\u7387\u5206\u6570" = d$score,
        check.names = FALSE
      )
      reactable::reactable(tab, searchable = TRUE, defaultPageSize = 15,
        pagination = TRUE, highlight = TRUE,
        defaultColDef = reactable::colDef(headerStyle = list(background = "#f1f3f7")),
        columns = stats::setNames(list(
          reactable::colDef(
            style = function(value) {
              col <- if (value > 0) "#3F8F4A" else "#C0504D"
              list(color = col, fontWeight = "bold")
            })
        ), "\u6548\u7387\u5206\u6570"))
    })

    output$model_table <- reactable::renderReactable({
      d <- eff_data()
      fit <- stats::lm(log_life ~ log_che, data = d)
      tab <- data.frame(
        "\u8bca\u65ad\u9879" = c(
          "\u53ef\u6bd4\u56fd\u5bb6",
          "log(CHE) \u659c\u7387",
          "R\u00b2",
          "\u6b8b\u5dee\u5206\u6570\u6700\u5c0f",
          "\u6b8b\u5dee\u5206\u6570\u6700\u5927",
          "\u6b8b\u5dee\u5206\u6570 IQR"
        ),
        "\u503c" = c(
          fmt_v3_num(nrow(d)),
          fmt_v3_num(stats::coef(fit)[["log_che"]], 4),
          fmt_v3_num(summary(fit)$r.squared, 3),
          fmt_v3_num(min(d$score, na.rm = TRUE), 2),
          fmt_v3_num(max(d$score, na.rm = TRUE), 2),
          fmt_v3_num(stats::IQR(d$score, na.rm = TRUE), 2)
        ),
        check.names = FALSE
      )
      reactable::reactable(tab, pagination = FALSE, highlight = TRUE,
        defaultColDef = reactable::colDef(headerStyle = list(background = "#f1f3f7")),
        columns = list(
          "\u8bca\u65ad\u9879" = reactable::colDef(minWidth = 150),
          "\u503c" = reactable::colDef(align = "right")
        ))
    })
  })
}
