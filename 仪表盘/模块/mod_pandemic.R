
mod_pandemic_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = "\u97e7\u6027 Pandemic",
    value = "pandemic",
    mod_v3_hero(
      kicker = "\u51b2\u51fb\u4e0e\u97e7\u6027",
      title = "COVID \u51b2\u51fb\u4e0b\u7684\u8d22\u52a1\u97e7\u6027\u8bc4\u4ef7",
      lead = paste(
        "\u5bf9\u6bd4 2019 \u4e0e 2022 \u7684\u7b79\u8d44\u7ed3\u6784\u53d8\u5316\uff0c\u7528 \u0394GGHE-D \u548c \u0394OOPS \u5224\u65ad\u516c\u5171\u8d22\u653f\u662f\u5426\u5728\u51b2\u51fb\u671f\u95f4\u627f\u62c5\u4e86\u66f4\u591a\u4fdd\u62a4\u529f\u80fd\u3002",
        "\u6563\u70b9\u56fe\u5448\u73b0\u56db\u8c61\u9650\uff0cDumbbell \u653e\u5927\u5bb6\u5ead\u81ea\u4ed8\u53d8\u5316\u6700\u5927\u7684\u56fd\u5bb6\uff0c\u8868\u683c\u5219\u628a\u97e7\u6027\u5206\u6570\u53d8\u6210\u53ef\u6392\u5e8f\u7684\u6e05\u5355\u3002"
      ),
      meta = list("2019 vs 2022", "\u0394GGHE-D", "\u0394OOPS", "Resilience score")
    ),
    mod_v3_page_body(
      wide = TRUE,
      mod_v3_badge_row(
        "\u51b2\u51fb\u524d\u540e\u5bf9\u7167",
        "\u516c\u5171\u8d22\u653f\u6258\u5e95",
        "\u5bb6\u5ead\u81ea\u4ed8\u538b\u529b",
        "\u97e7\u6027\u6392\u540d",
        tone = "bad"
      ),
      mod_v3_story_grid(
        columns = 3,
        mod_v3_insight(
          kicker = "Public buffer",
          title = "\u0394GGHE-D \u4e0a\u5347\u8868\u793a\u653f\u5e9c\u4efd\u989d\u6269\u5927",
          text = "\u82e5\u516c\u5171\u7b79\u8d44\u5360 CHE \u6bd4\u63d0\u5347\uff0c\u901a\u5e38\u610f\u5473\u7740\u51b2\u51fb\u671f\u95f4\u8d22\u653f\u6216\u793e\u4fdd\u673a\u5236\u6709\u66f4\u5f3a\u627f\u63a5\u3002",
          tone = "primary"
        ),
        mod_v3_insight(
          kicker = "Household stress",
          title = "\u0394OOPS \u4e0a\u5347\u4ee3\u8868\u5bb6\u5ead\u538b\u529b\u52a0\u91cd",
          text = "\u5c45\u6c11\u73b0\u91d1\u81ea\u4ed8\u5360\u6bd4\u4e0a\u884c\u65f6\uff0c\u9700\u5173\u6ce8\u7a81\u53d1\u536b\u751f\u9700\u6c42\u662f\u5426\u8f6c\u5316\u4e3a\u8d22\u52a1\u98ce\u9669\u3002",
          tone = "bad"
        ),
        mod_v3_insight(
          kicker = "Score",
          title = "\u97e7\u6027\u5206\u6570\u805a\u7126\u201c\u6258\u5e95\u51c0\u6548\u5e94\u201d",
          text = "\u5206\u6570 = \u0394GGHE-D - \u0394OOPS\uff1b\u6570\u503c\u8d8a\u9ad8\uff0c\u8bf4\u660e\u516c\u5171\u7b79\u8d44\u63d0\u5347\u66f4\u80fd\u62b5\u6d88\u5bb6\u5ead\u81ea\u4ed8\u4e0a\u5347\u3002",
          tone = "good"
        )
      ),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 292,
          shiny::numericInput(ns("top_n"), "Dumbbell \u663e\u793a\u6570\u91cf Top N", value = 25,
                               min = 10, max = 60, step = 5),
          mod_v3_sidebar_note(
            "\u989c\u8272\u903b\u8f91",
            "Dumbbell \u6bd4\u8f83 2019 \u4e0e 2022 \u7684 OOPS \u5360\u6bd4\u53d8\u5316\uff1a\u6a59\u7ea2\u8868\u793a\u5bb6\u5ead\u538b\u529b\u4e0a\u5347\uff0c\u7eff\u8272\u8868\u793a\u81ea\u4ed8\u5360\u6bd4\u4e0b\u964d\u3002",
            bullets = c("\u4ec5\u5c55\u793a\u53d8\u5316\u7edd\u5bf9\u503c\u6700\u5927\u7684 Top N", "\u6563\u70b9\u56fe\u5efa\u8bae\u5148\u770b\u8c61\u9650", "\u8868\u683c\u53ef\u641c\u7d22\u56fd\u5bb6\u6216\u6536\u5165\u7ec4")
          )
        ),
        shiny::uiOutput(ns("kpi_strip")),
        mod_v3_rail(list(
          list(title = "\u5b9a\u57fa\u7ebf", text = "2019 \u4ee3\u8868 COVID \u524d\u7684\u7ed3\u6784\u72b6\u6001\u3002"),
          list(title = "\u770b\u53d8\u5316", text = "2022 \u7528\u4e8e\u8868\u793a\u51b2\u51fb\u540e\u4e2d\u671f\u53cd\u5e94\u3002"),
          list(title = "\u5206\u8c61\u9650", text = "\u0394GGHE-D \u4e0e \u0394OOPS \u7684\u7ec4\u5408\u5224\u65ad\u6258\u5e95\u7c7b\u578b\u3002"),
          list(title = "\u6392\u5206\u6570", text = "\u97e7\u6027\u8868\u683c\u5c06\u53d8\u5316\u538b\u7f29\u6210\u53ef\u6bd4\u6392\u540d\u3002")
        )),
        bslib::layout_columns(
          col_widths = c(6, 6),
          mod_card(
            kicker = "\u516c\u5171\u7b79\u8d44 vs \u5bb6\u5ead\u538b\u529b",
            title = "COVID \u524d\u540e GGHE-D \u548c OOPS \u53d8\u5316",
            mod_v3_chart_guide(
              "\u8c61\u9650\u89e3\u8bfb",
              "\u6a2a\u8f74\u662f\u653f\u5e9c\u7b79\u8d44\u4efd\u989d\u53d8\u5316\uff0c\u7eb5\u8f74\u662f\u5bb6\u5ead\u81ea\u4ed8\u4efd\u989d\u53d8\u5316\u3002\u53f3\u4e0b\u8c61\u9650\u901a\u5e38\u662f\u66f4\u7406\u60f3\u7684\u6258\u5e95\u7ec4\u5408\u3002",
              bullets = c("\u53f3\u4e0b\uff1a\u653f\u5e9c\u4efd\u989d\u4e0a\u5347\u3001OOPS \u4e0b\u964d", "\u53f3\u4e0a\uff1a\u516c\u5171\u4efd\u989d\u548c\u81ea\u4ed8\u540c\u65f6\u4e0a\u884c", "\u5de6\u4e0a\uff1a\u5bb6\u5ead\u538b\u529b\u4e0a\u5347\u4e14\u516c\u5171\u4efd\u989d\u4e0b\u964d")
            ),
            mod_spinner(plotly::plotlyOutput(ns("covid_scatter"), height = 500))
          ),
          mod_card(
            kicker = "\u81ea\u4ed8\u53d8\u5316",
            title = "OOPS \u53d8\u5316 dumbbell",
            mod_v3_chart_guide(
              "\u7aef\u70b9\u5bf9\u7167",
              "\u7070\u70b9\u4e3a 2019\uff0c\u6a59\u70b9\u4e3a 2022\uff1b\u7ebf\u6bb5\u8d8a\u957f\u8868\u793a OOPS \u5360\u6bd4\u53d8\u5316\u8d8a\u5927\u3002",
              tone = "warn"
            ),
            mod_spinner(plotly::plotlyOutput(ns("dumbbell"), height = 500))
          )
        ),
        mod_card(
          kicker = "\u97e7\u6027\u6392\u540d",
          title = "\u97e7\u6027\u8bc4\u5206\uff08\u0394GGHE-D \u4e0a\u5347 - \u0394OOPS \u4e0a\u5347\uff09",
          mod_v3_chart_guide(
            "\u8868\u683c\u7528\u9014",
            "\u6392\u540d\u8868\u5c06\u51b2\u51fb\u671f\u95f4\u7684\u516c\u5171\u7b79\u8d44\u53d8\u5316\u548c\u5bb6\u5ead\u81ea\u4ed8\u53d8\u5316\u653e\u5728\u540c\u4e00\u884c\uff0c\u4fbf\u4e8e\u627e\u9ad8\u97e7\u6027\u548c\u9ad8\u98ce\u9669\u56fd\u5bb6\u3002",
            tone = "good"
          ),
          mod_spinner(reactable::reactableOutput(ns("resilience_table"))),
          footer = "\u51b2\u51fb\u9875\u4f7f\u7528 2019 \u4e0e 2022 \u4e24\u4e2a\u622a\u9762\uff1b\u5206\u6570\u662f\u63cf\u8ff0\u6027\u6307\u6807\uff0c\u4e0d\u76f4\u63a5\u8868\u793a\u653f\u7b56\u56e0\u679c\u3002"
        )
      )
    )
  )
}

mod_pandemic_server <- function(id, master_r) {
  shiny::moduleServer(id, function(input, output, session) {
    covid_panel <- shiny::reactive({
      m <- master_r()
      d <- tryCatch(covid_shock(m), error = function(e) NULL)
      if (is.null(d) || !nrow(d)) return(d)
      meta <- m |>
        dplyr::filter(.data$year == 2019) |>
        dplyr::select(.data$iso3_code, .data$continent, .data$income_group) |>
        dplyr::distinct(.data$iso3_code, .keep_all = TRUE)
      dplyr::left_join(d, meta, by = "iso3_code")
    })

    output$kpi_strip <- shiny::renderUI({
      m <- master_r()
      d19 <- m[m$year == 2019, c("iso3_code", "country_name", "continent",
                                  "income_group", "gghed_che", "hf3_che")]
      d22 <- m[m$year == 2022, c("iso3_code", "gghed_che", "hf3_che")]
      names(d19)[5:6] <- c("gghed19", "oops19")
      names(d22)[2:3] <- c("gghed22", "oops22")
      d <- merge(d19, d22, by = "iso3_code")
      d <- d[is.finite(d$gghed19) & is.finite(d$gghed22) &
               is.finite(d$oops19) & is.finite(d$oops22), , drop = FALSE]
      shiny::req(nrow(d) > 0)
      d$delta_gghe <- d$gghed22 - d$gghed19
      d$delta_oops <- d$oops22 - d$oops19
      d$resilience <- d$delta_gghe - d$delta_oops
      top <- d[which.max(d$resilience), , drop = FALSE]
      mod_v3_kpi_grid(
        mod_v3_kpi("\u53ef\u6bd4\u56fd\u5bb6", fmt_v3_num(nrow(d)),
                   hint = "\u540c\u65f6\u5177\u6709 2019 \u548c 2022 \u7b79\u8d44\u6570\u636e",
                   tone = "primary"),
        mod_v3_kpi("OOPS \u4e0a\u5347\u56fd\u5bb6", fmt_v3_num(sum(d$delta_oops > 0, na.rm = TRUE)),
                   hint = "\u5bb6\u5ead\u81ea\u4ed8\u5360\u6bd4\u9ad8\u4e8e 2019",
                   tone = "bad"),
        mod_v3_kpi("\u0394GGHE-D \u4e2d\u4f4d\u6570", fmt_v3_num(stats::median(d$delta_gghe, na.rm = TRUE), 1, " pp"),
                   hint = "\u653f\u5e9c\u7b79\u8d44\u4efd\u989d\u53d8\u5316",
                   tone = "secondary"),
        mod_v3_kpi("\u6700\u9ad8\u97e7\u6027", top$iso3_code[1],
                   hint = sprintf("\u5206\u6570 %+0.1f pp", top$resilience[1]),
                   tone = "good")
      )
    })

    output$covid_scatter <- plotly::renderPlotly({
      d <- covid_panel()
      shiny::req(d, nrow(d) > 0)
      pal <- c(Africa = "#C0504D", Americas = "#1B5E88", Asia = "#E8833C",
               Europe = "#2A9D8F", Oceania = "#7B4B94", Antarctica = "#9C9C9C")
      x_col <- if ("gghed_delta_pp" %in% names(d)) "gghed_delta_pp"
                else if ("delta_gghe_che" %in% names(d)) "delta_gghe_che"
                else if ("delta_gghed_che" %in% names(d)) "delta_gghed_che"
                else names(d)[grepl("delta", names(d), ignore.case = TRUE)][1]
      y_col <- if ("oops_delta_pp" %in% names(d)) "oops_delta_pp"
                else if ("delta_oop" %in% names(d)) "delta_oop"
                else if ("delta_hf3_che" %in% names(d)) "delta_hf3_che"
                else names(d)[grepl("oop", names(d), ignore.case = TRUE)][1]
      shiny::req(!is.null(x_col), !is.null(y_col))
      d2 <- d[is.finite(d[[x_col]]) & is.finite(d[[y_col]]), , drop = FALSE]
      p <- ggplot2::ggplot(d2, ggplot2::aes(.data[[x_col]], .data[[y_col]],
                                              colour = continent,
                                              text = country_name)) +
        ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                             colour = "#1A1A1F40") +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                             colour = "#1A1A1F40") +
        ggplot2::geom_point(size = 2.6, alpha = 0.85) +
        ggplot2::scale_colour_manual(values = pal, name = NULL) +
        ggplot2::labs(x = "\u0394 GGHE-D %", y = "\u0394 OOPS %") +
        ggplot2::theme_minimal(base_size = 12)
      plotly::ggplotly(p, tooltip = "text") |>
        ghs_plotly_layout() |>
        plotly::config(displaylogo = FALSE)
    })

    output$dumbbell <- plotly::renderPlotly({
      m <- master_r()
      shiny::req(input$top_n)
      d19 <- m[m$year == 2019 &
                is.finite(m$hf3_che),
                c("iso3_code", "country_name", "continent", "hf3_che")]
      d22 <- m[m$year == 2022 &
                is.finite(m$hf3_che),
                c("iso3_code", "hf3_che")]
      names(d19)[4] <- "y2019"; names(d22)[2] <- "y2022"
      d <- merge(d19, d22, by = "iso3_code")
      d$delta <- d$y2022 - d$y2019
      d <- d[order(-abs(d$delta)), ][seq_len(min(input$top_n, nrow(d))), ]
      d$country_name <- factor(d$country_name,
        levels = rev(d$country_name[order(-abs(d$delta))]))
      d$dir <- ifelse(d$delta > 0, "up", "down")
      p <- ggplot2::ggplot(d) +
        ggplot2::geom_segment(ggplot2::aes(x = y2019, xend = y2022,
                                            y = country_name, yend = country_name,
                                            colour = dir), linewidth = 1) +
        ggplot2::geom_point(ggplot2::aes(y2019, country_name),
                             size = 2.6, colour = "#5A5A65") +
        ggplot2::geom_point(ggplot2::aes(y2022, country_name),
                             size = 2.6, colour = "#C46B27") +
        ggplot2::scale_colour_manual(
          values = c(up = "#C46B27", down = "#6B8E5A"),
          labels = c(up = "OOPS \u4e0a\u5347", down = "OOPS \u4e0b\u964d"),
          name = NULL) +
        ggplot2::labs(x = "OOPS / CHE  (%)", y = NULL) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(legend.position = "top")
      plotly::ggplotly(p) |>
        ghs_plotly_layout() |>
        plotly::config(displaylogo = FALSE)
    })

    output$resilience_table <- reactable::renderReactable({
      m <- master_r()
      d19 <- m[m$year == 2019, c("iso3_code", "country_name", "continent",
                                  "income_group", "gghed_che", "hf3_che")]
      d22 <- m[m$year == 2022, c("iso3_code", "gghed_che", "hf3_che")]
      names(d19)[5:6] <- c("gghed19", "oops19")
      names(d22)[2:3] <- c("gghed22", "oops22")
      d <- merge(d19, d22, by = "iso3_code")
      d$delta_gghe <- d$gghed22 - d$gghed19
      d$delta_oops <- d$oops22  - d$oops19
      d$resilience <- round(d$delta_gghe - d$delta_oops, 2)
      d <- d[order(-d$resilience), ]
      tab <- data.frame(
        rank        = seq_len(nrow(d)),
        country     = d$country_name,
        continent   = d$continent,
        income_group = d$income_group,
        delta_GGHE  = round(d$delta_gghe, 1),
        delta_OOPS  = round(d$delta_oops, 1),
        resilience  = d$resilience
      )
      names(tab) <- c("\u6392\u540d", "\u56fd\u5bb6", "\u5927\u6d32", "\u6536\u5165\u7ec4",
                       "\u0394 GGHE-D %", "\u0394 OOPS %", "\u97e7\u6027\u5206\u6570")
      reactable::reactable(tab, searchable = TRUE, defaultPageSize = 15,
        pagination = TRUE, highlight = TRUE,
        defaultColDef = reactable::colDef(headerStyle = list(background = "#f1f3f7")),
        columns = stats::setNames(list(
          reactable::colDef(style = function(value) {
            col <- if (value > 0) "#3F8F4A" else "#C0504D"
            list(color = col, fontWeight = "bold")
          })
        ), "\u97e7\u6027\u5206\u6570"))
    })
  })
}
