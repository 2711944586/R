
mod_country_ui <- function(id, country_choices_named, year_min, year_max) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = "\u56fd\u5bb6 Country",
    value = "country",
    mod_v3_hero(
      kicker = "\u56fd\u5bb6\u753b\u50cf",
      title = "\u56fd\u5bb6\u753b\u50cf\uff1a\u4ece\u8d44\u91d1\u603b\u91cf\u5230\u5bb6\u5ead\u8d1f\u62c5",
      lead = paste(
        "\u5355\u56fd\u9875\u628a 24 \u5e74\u5e74\u5ea6\u9762\u677f\u62c6\u6210\u56db\u6761\u7ebf\u7d22\uff1a\u603b CHE \u89c4\u6a21\u3001",
        "\u4eba\u5747\u6295\u5165\u3001\u653f\u5e9c/\u79c1\u4eba/\u5916\u63f4\u7ed3\u6784\u548c\u9884\u9632-\u6cbb\u7597\u529f\u80fd\u5360\u6bd4\u3002",
        "\u9009\u4e2d\u56fd\u5bb6\u540e\uff0c\u4e0a\u65b9 KPI \u4ee5\u533a\u95f4\u672b\u5e74\u4f5c\u4e3a\u5f53\u524d\u72b6\u6001\u3002"
      ),
      meta = list("Country-year panel", "CHE / OOPS / GGHED / EXT", "2000\u20132023")
    ),
    mod_v3_page_body(
      wide = TRUE,
      mod_v3_badge_row(
        "\u5355\u56fd\u5e74\u5ea6\u8f68\u8ff9",
        "\u56db\u7c7b\u7ed3\u6784\u56fe",
        "\u533a\u95f4\u672b\u5e74 KPI",
        "\u652f\u51fa\u7528\u9014\u53ef\u8ffd\u8e2a",
        tone = "primary"
      ),
      mod_v3_story_grid(
        columns = 3,
        mod_v3_insight(
          kicker = "What to read",
          title = "\u5148\u770b\u603b\u91cf\u548c\u4eba\u5747",
          text = "\u603b CHE \u53cd\u6620\u56fd\u5bb6\u5e02\u573a\u4e0e\u8d22\u653f\u89c4\u6a21\uff0c\u4eba\u5747 CHE \u66f4\u9002\u5408\u6a2a\u5411\u7406\u89e3\u4fdd\u969c\u5f3a\u5ea6\u3002",
          tone = "primary"
        ),
        mod_v3_insight(
          kicker = "Structure",
          title = "\u518d\u770b\u8c01\u5728\u4ed8\u8d39",
          text = "GGHE-D\u3001PVT-D\u3001EXT \u7684\u9762\u79ef\u53d8\u5316\u7528\u6765\u8bc6\u522b\u516c\u5171\u8d22\u653f\u6269\u5f20\u3001\u79c1\u4eba\u652f\u4ed8\u4e0a\u5347\u6216\u5916\u63f4\u4f9d\u8d56\u3002",
          tone = "secondary"
        ),
        mod_v3_insight(
          kicker = "\u4fdd\u62a4",
          title = "\u6700\u540e\u843d\u5230\u5bb6\u5ead\u538b\u529b",
          text = "OOPS \u5360\u6bd4\u9ad8\u65f6\uff0c\u5373\u4f7f\u4eba\u5747 CHE \u4e0a\u5347\uff0c\u4e5f\u9700\u8b66\u60d5\u8d22\u52a1\u4fdd\u62a4\u4e0d\u8db3\u3002",
          tone = "warn"
        )
      ),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 292,
          shinyWidgets::pickerInput(
            ns("country"), "\u9009\u62e9\u56fd\u5bb6",
            choices = country_choices_named, selected = "CHN",
            options = list(`live-search` = TRUE)),
          shiny::sliderInput(
            ns("year_range"), "年份区间",
            min = year_min, max = year_max,
            value = c(year_min, year_max), step = 1, sep = ""),
          mod_v3_sidebar_note(
            "\u4ea4\u4e92\u8bf4\u660e",
            "\u62d6\u52a8\u5e74\u4efd\u533a\u95f4\u53ef\u628a KPI \u548c\u56db\u7ec4\u56fe\u540c\u6b65\u6536\u7a84\u5230\u67d0\u4e00\u653f\u7b56\u5468\u671f\u3002",
            bullets = c("\u9002\u5408\u56fd\u522b\u6c47\u62a5", "\u53ef\u8ddf\u8e2a COVID \u524d\u540e", "\u53ef\u8bc6\u522b\u7b79\u8d44\u7ed3\u6784\u8f6c\u578b")
          )
        ),
        shiny::uiOutput(ns("kpi_strip")),
        mod_v3_rail(list(
          list(title = "\u9009\u56fd\u5bb6", text = "\u7528 ISO3 \u4e0e\u56fd\u540d\u641c\u7d22\u5b9a\u4f4d\u3002"),
          list(title = "\u5b9a\u533a\u95f4", text = "\u53ef\u770b\u5168\u5468\u671f\u6216\u7279\u5b9a\u653f\u7b56\u7a97\u53e3\u3002"),
          list(title = "\u8bfb\u7ed3\u6784", text = "\u5c06\u6765\u6e90\u548c\u7b79\u8d44\u65b9\u6848\u5206\u5f00\u89e3\u91ca\u3002"),
          list(title = "\u770b\u4fdd\u62a4", text = "\u628a OOPS \u548c\u9884\u9632\u652f\u51fa\u4e00\u8d77\u68c0\u67e5\u3002")
        )),
        mod_card(
          kicker = "\u56fd\u5bb6\u5de5\u4f5c\u53f0",
          title = "\u5355\u56fd\u8d44\u91d1\u548c\u529f\u80fd\u9762\u677f",
          bslib::navset_card_tab(
            bslib::nav_panel("总量与人均",
              mod_v3_chart_guide(
                "\u8bfb\u56fe\u987a\u5e8f",
                "\u4e0a\u56fe\u770b\u56fd\u5bb6\u536b\u751f\u603b\u652f\u51fa\u89c4\u6a21\uff0c\u4e0b\u56fe\u770b\u4eba\u5747\u6295\u5165\u6c34\u5e73\uff1b\u4e24\u8005\u5206\u79bb\u80fd\u533a\u5206\u4eba\u53e3\u89c4\u6a21\u548c\u4fdd\u969c\u5f3a\u5ea6\u3002"
              ),
              mod_spinner(plotly::plotlyOutput(ns("total_che"), height = 320)),
              mod_spinner(plotly::plotlyOutput(ns("pc_che"), height = 320))),
            bslib::nav_panel("来源结构",
              mod_v3_chart_guide(
                "\u8d44\u91d1\u6765\u81ea\u54ea\u91cc",
                "\u9762\u79ef\u56fe\u628a\u653f\u5e9c\u3001\u79c1\u4eba\u548c\u5916\u63f4\u6309 CHE \u5360\u6bd4\u5c55\u5f00\uff0c\u5efa\u8bae\u91cd\u70b9\u770b\u957f\u671f\u66ff\u4ee3\u800c\u975e\u5355\u5e74\u6ce2\u52a8\u3002"
              ),
              mod_spinner(plotly::plotlyOutput(ns("sources"), height = 380))),
            bslib::nav_panel("筹资方案",
              mod_v3_chart_guide(
                "\u652f\u4ed8\u673a\u5236",
                "HF \u7cfb\u5217\u53ef\u4ee5\u628a\u201c\u79c1\u4eba\u652f\u51fa\u201d\u7ee7\u7eed\u62c6\u6210\u81ea\u4ed8\u3001\u81ea\u613f\u4fdd\u9669\u548c\u5176\u4ed6\u5b89\u6392\uff0c\u7528\u4e8e\u8bc6\u522b\u8d22\u52a1\u4fdd\u62a4\u8584\u5f31\u70b9\u3002"
              ),
              mod_spinner(plotly::plotlyOutput(ns("schemes"), height = 380))),
            bslib::nav_panel("预防 vs 治疗",
              mod_v3_chart_guide(
                "\u529f\u80fd\u4fa7\u91cd",
                "HC1 \u662f\u6cbb\u7597\u6027\u670d\u52a1\uff0cHC6 \u662f\u9884\u9632\u6027\u670d\u52a1\uff1b\u6ce8\u610f HC \u7cfb\u5217\u5728\u65e9\u671f\u5e74\u4efd\u53ef\u80fd\u8986\u76d6\u4e0d\u5168\u3002",
                tone = "warn"
              ),
              mod_spinner(plotly::plotlyOutput(ns("purpose"), height = 380)))
          ),
          footer = "\u6240\u6709\u91d1\u989d\u6307\u6807\u5747\u4ee5 2023 \u4e0d\u53d8\u4ef7 USD \u8868\u793a\uff1b\u5360\u6bd4\u6307\u6807\u4ee5 CHE \u4e3a\u5206\u6bcd\u3002"
        ),
        mod_card(
          kicker = "\u5e74\u5ea6\u660e\u7ec6",
          title = "\u56fd\u5bb6\u5e74\u5ea6\u6838\u5fc3\u6307\u6807\u8868",
          mod_v3_chart_guide(
            "\u8868\u683c\u7528\u9014",
            "\u5c06\u56fe\u4e2d\u7684\u603b\u989d\u3001\u4eba\u5747\u3001\u516c\u5171\u7b79\u8d44\u3001\u81ea\u4ed8\u3001\u5916\u63f4\u548c\u5bff\u547d\u6309\u5e74\u5c55\u5f00\uff0c\u4fbf\u4e8e\u5199\u56fd\u522b\u6458\u8981\u65f6\u5f15\u7528\u5177\u4f53\u6570\u503c\u3002",
            tone = "good"
          ),
          mod_spinner(reactable::reactableOutput(ns("country_table"))),
          footer = "\u8868\u683c\u53d7\u5de6\u4fa7\u5e74\u4efd\u533a\u95f4\u540c\u6b65\u63a7\u5236\uff1b\u91d1\u989d\u5747\u4e3a 2023 \u4e0d\u53d8\u4ef7 USD\u3002"
        )
      )
    )
  )
}

mod_country_server <- function(id, master_r) {
  shiny::moduleServer(id, function(input, output, session) {
    cp_data <- shiny::reactive({
      shiny::req(input$country)
      m <- master_r()
      m[m$iso3_code == input$country &
        m$year >= input$year_range[1] &
        m$year <= input$year_range[2], , drop = FALSE]
    })

    output$kpi_strip <- shiny::renderUI({
      d <- cp_data()
      shiny::req(nrow(d) > 0)
      latest <- d[d$year == max(d$year, na.rm = TRUE), , drop = FALSE]
      yr <- latest$year[1]
      mod_v3_kpi_grid(
        mod_v3_kpi(sprintf("%d CHE 总额", yr),
                   fmt_usd(latest$che_usd2023[1]),
                   hint = "\u56fd\u5bb6\u5c42\u9762\u536b\u751f\u603b\u652f\u51fa",
                   tone = "primary"),
        mod_v3_kpi("OOPS 占比", fmt_pct(latest$hf3_che[1]),
                   hint = "\u5c45\u6c11\u73b0\u91d1\u81ea\u4ed8\u5360 CHE",
                   tone = "bad"),
        mod_v3_kpi("GGHE-D 占比", fmt_pct(latest$gghed_che[1]),
                   hint = "\u56fd\u5185\u653f\u5e9c\u536b\u751f\u652f\u51fa",
                   tone = "good"),
        mod_v3_kpi("EXT 占比", fmt_pct(latest$ext_che[1]),
                   hint = "\u5916\u90e8\u63f4\u52a9\u536b\u751f\u652f\u51fa",
                   tone = "warn")
      )
    })

    output$total_che <- plotly::renderPlotly({
      d <- cp_data(); shiny::req(nrow(d) > 0)
      plotly::plot_ly(d, x = ~year, y = ~che_usd2023 / 1e9,
                       type = "scatter", mode = "lines+markers",
                       line = list(color = "#1B5E88", width = 3),
                       marker = list(color = "#1B5E88", size = 6),
                       hovertemplate = "%{x}: $%{y:.2f}B<extra></extra>") |>
        ghs_plotly_layout() |>
        plotly::layout(title = "总卫生支出 CHE (十亿 USD 2023)",
                       xaxis = list(title = ""),
                       yaxis = list(title = "\u5341\u4ebf USD")) |>
        plotly::config(displaylogo = FALSE)
    })

    output$pc_che <- plotly::renderPlotly({
      d <- cp_data(); shiny::req(nrow(d) > 0)
      plotly::plot_ly(d, x = ~year, y = ~che_pc_usd2023,
                       type = "scatter", mode = "lines+markers",
                       line = list(color = "#C46B27", width = 3),
                       marker = list(color = "#C46B27", size = 6),
                       hovertemplate = "%{x}: $%{y:,.0f}<extra></extra>") |>
        ghs_plotly_layout() |>
        plotly::layout(title = "人均 CHE (USD 2023)",
                       xaxis = list(title = ""),
                       yaxis = list(title = "\u4eba\u5747 USD")) |>
        plotly::config(displaylogo = FALSE)
    })

    output$sources <- plotly::renderPlotly({
      d <- cp_data(); shiny::req(nrow(d) > 0)
      d2 <- d[, c("year", "gghed_che", "pvtd_che", "ext_che")]
      d2 <- tidyr::pivot_longer(d2, c("gghed_che", "pvtd_che", "ext_che"),
                                 names_to = "src", values_to = "pct")
      d2$src <- factor(d2$src,
                       levels = c("gghed_che", "pvtd_che", "ext_che"),
                       labels = c("政府 GGHE-D", "私人 PVT-D", "外援 EXT"))
      p <- ggplot2::ggplot(d2, ggplot2::aes(year, pct, fill = src)) +
        ggplot2::geom_area(alpha = 0.85) +
        ggplot2::scale_fill_manual(values = c("政府 GGHE-D" = "#1B5E88",
                                               "私人 PVT-D" = "#C46B27",
                                               "外援 EXT"  = "#6B8E5A"),
                                    name = NULL) +
        ggplot2::scale_y_continuous(labels = scales::label_percent(scale = 1)) +
        ggplot2::labs(title = "来源结构", x = NULL, y = NULL) +
        ggplot2::theme_minimal(base_size = 11)
      plotly::ggplotly(p) |> plotly::config(displaylogo = FALSE)
    })

    output$schemes <- plotly::renderPlotly({
      d <- cp_data(); shiny::req(nrow(d) > 0)
      cols <- intersect(c("hf1_che","hf2_che","hf3_che","hf4_che","hfnec_che"),
                        names(d))
      d2 <- d[, c("year", cols)]
      d2 <- tidyr::pivot_longer(d2, dplyr::all_of(cols),
                                 names_to = "sch", values_to = "pct")
      p <- ggplot2::ggplot(d2, ggplot2::aes(year, pct, fill = sch)) +
        ggplot2::geom_area(alpha = 0.85) +
        ggplot2::scale_y_continuous(labels = scales::label_percent(scale = 1)) +
        ggplot2::labs(title = "筹资方案 HF", x = NULL, y = NULL) +
        ggplot2::theme_minimal(base_size = 11)
      plotly::ggplotly(p) |> plotly::config(displaylogo = FALSE)
    })

    output$purpose <- plotly::renderPlotly({
      d <- cp_data(); shiny::req(nrow(d) > 0)
      if (!all(c("hc1_che", "hc6_che") %in% names(d))) {
        return(plotly::plotly_empty() |>
                  plotly::layout(title = "缺少 hc 系列字段"))
      }
      d2 <- d[, c("year", "hc1_che", "hc6_che")]
      d2 <- tidyr::drop_na(d2)
      d2 <- tidyr::pivot_longer(d2, c("hc1_che", "hc6_che"),
                                 names_to = "purpose", values_to = "pct")
      d2$purpose <- factor(d2$purpose,
                           levels = c("hc1_che", "hc6_che"),
                           labels = c("hc1 治疗", "hc6 预防"))
      if (nrow(d2) < 2) {
        return(plotly::plotly_empty() |>
                  plotly::layout(title = "数据不足 (hc 系列仅 2016+)"))
      }
      plotly::plot_ly(d2, x = ~year, y = ~pct, color = ~purpose,
                       type = "scatter", mode = "lines+markers",
                       hovertemplate = "%{x}: %{y:.1f}%<extra></extra>") |>
        ghs_plotly_layout() |>
        plotly::layout(title = "预防 hc6 vs 治疗 hc1",
                       yaxis = list(title = "\u5360 CHE \u6bd4\u4f8b (%)"),
                       xaxis = list(title = "")) |>
        plotly::config(displaylogo = FALSE)
    })

    output$country_table <- reactable::renderReactable({
      d <- cp_data()
      shiny::req(nrow(d) > 0)
      cols <- c("year", "che_usd2023", "che_pc_usd2023", "gghed_che",
                "pvtd_che", "hf3_che", "ext_che", "life_exp")
      cols <- intersect(cols, names(d))
      tab <- d[order(d$year, decreasing = TRUE), cols, drop = FALSE]
      out <- data.frame(
        "\u5e74\u4efd" = tab$year,
        "\u603b CHE\uff08\u5341\u4ebf USD\uff09" =
          if ("che_usd2023" %in% names(tab)) round(tab$che_usd2023 / 1e9, 2) else NA_real_,
        "\u4eba\u5747 CHE\uff08USD\uff09" =
          if ("che_pc_usd2023" %in% names(tab)) round(tab$che_pc_usd2023, 0) else NA_real_,
        "GGHE-D%" =
          if ("gghed_che" %in% names(tab)) round(tab$gghed_che, 1) else NA_real_,
        "PVT-D%" =
          if ("pvtd_che" %in% names(tab)) round(tab$pvtd_che, 1) else NA_real_,
        "OOPS%" =
          if ("hf3_che" %in% names(tab)) round(tab$hf3_che, 1) else NA_real_,
        "EXT%" =
          if ("ext_che" %in% names(tab)) round(tab$ext_che, 1) else NA_real_,
        "\u9884\u671f\u5bff\u547d" =
          if ("life_exp" %in% names(tab)) round(tab$life_exp, 1) else NA_real_,
        check.names = FALSE
      )
      reactable::reactable(out, searchable = TRUE, defaultPageSize = 12,
        pagination = TRUE, highlight = TRUE,
        defaultColDef = reactable::colDef(headerStyle = list(background = "#f1f3f7")),
        columns = list(
          "\u5e74\u4efd" = reactable::colDef(width = 80),
          "OOPS%" = reactable::colDef(style = function(value) {
            if (is.na(value)) return(NULL)
            col <- if (value > 40) "#a23b3b"
                   else if (value > 25) "#c89a3b"
                   else "#2a857a"
            list(color = col, fontWeight = 700)
          })
        ))
    })
  })
}
