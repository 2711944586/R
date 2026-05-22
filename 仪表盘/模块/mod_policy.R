# =============================================================================
# 仪表盘/模块/mod_policy.R
# 政策建议生成器：根据国家风险画像给出量化的政策建议
# =============================================================================

mod_policy_ui <- function(id, country_choices_named) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = "\u653f\u7b56 Policy",
    value = "policy",
    mod_v3_hero(
      kicker = "\u653f\u7b56\u987e\u95ee",
      title = "\u653f\u7b56\u5efa\u8bae\u751f\u6210\u5668",
      lead = paste(
        "\u9009\u62e9\u4e00\u4e2a\u56fd\u5bb6\uff0c\u83b7\u5f97\u57fa\u4e8e\u6570\u636e\u7684\u91cf\u5316\u653f\u7b56\u5efa\u8bae\u3002",
        "\u4ece\u8d22\u52a1\u4fdd\u62a4\u3001\u5916\u63f4\u4f9d\u8d56\u3001\u9886\u57df\u504f\u91cd\u3001\u5bff\u547d\u4ea7\u51fa\u56db\u4e2a\u7ef4\u5ea6\u8bca\u65ad\u3002"
      ),
      meta = list("\u8bca\u65ad\u8868\u91cf", "\u540c\u4f34\u56fd\u5bf9\u6807", "\u70b9\u8bc4\u5206\u6cd5")
    ),
    mod_v3_page_body(
      wide = TRUE,
      mod_v3_topic_brief("policy"),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 300,
          shinyWidgets::pickerInput(ns("country"), "\u9009\u62e9\u56fd\u5bb6",
            choices = country_choices_named, selected = "CHN",
            options = list(`live-search` = TRUE)),
          shiny::tags$hr(),
          shiny::checkboxGroupInput(ns("priorities"), "\u653f\u7b56\u4f18\u5148\u9886\u57df",
            choices = c(
              "\u8d22\u52a1\u4fdd\u62a4 (OOPS<25%)" = "fp",
              "\u51cf\u5c11\u5916\u63f4\u4f9d\u8d56" = "ext",
              "\u9884\u9632\u4e3b\u5bfc" = "prevent",
              "\u63d0\u9ad8\u5bff\u547d" = "life"
            ),
            selected = c("fp", "ext", "prevent", "life")),
          mod_v3_sidebar_note(
            "\u751f\u6210\u903b\u8f91",
            "\u6a21\u5757\u5c06\u56fd\u5bb6\u6700\u65b0\u89c2\u6d4b\u503c\u8f6c\u6210\u5206\u4f4d\u8bc4\u5206\uff0c\u518d\u7ed3\u5408\u540c\u6536\u5165\u7ec4\u4e2d\u4f4d\u6570\u751f\u6210\u53ef\u6267\u884c\u7684\u653f\u7b56\u63d0\u793a\u3002",
            bullets = c(
              "\u5206\u6570\u9ad8\u4f4e\u8868\u793a\u76f8\u5bf9\u4f4d\u7f6e\uff0c\u4e0d\u662f\u653f\u7b56\u7ee9\u6548\u7edd\u5bf9\u8bc4\u7ea7\u3002",
              "\u540c\u4f34\u56fd\u7528\u4e8e\u627e\u53ef\u6bd4\u53c2\u7167\uff0c\u4e0d\u4ee3\u8868\u76f4\u63a5\u7167\u642c\u7684\u653f\u7b56\u6a21\u677f\u3002",
              "\u5efa\u8bae\u9700\u8981\u548c\u8d8b\u52bf\u56fe\u4e00\u8d77\u9605\u8bfb\u3002"
            )
          )
        ),
        shiny::uiOutput(ns("country_header")),
        shiny::uiOutput(ns("score_strip")),
        bslib::layout_columns(
          col_widths = c(6, 6),
          mod_card(
            kicker = "F1 \u00b7 Diagnostic radar",
            title = "\u56db\u7ef4\u96f7\u8fbe\u8bca\u65ad",
            htmltools::p(class = "card-note",
              "\u56db\u4e2a\u7ef4\u5ea6\u4e0e\u540c\u6536\u5165\u7ec4\u540c\u4f34\u56fd\u5bf9\u6bd4\u3002"),
            mod_v3_chart_guide(
              "\u9605\u8bfb\u8f6e\u5ed3\u800c\u4e0d\u53ea\u8bfb\u5355\u70b9",
              "\u56fd\u5bb6\u8f6e\u5ed3\u8d85\u51fa\u540c\u4f34\u4e2d\u4f4d\u6570\u7684\u7ef4\u5ea6\u4ee3\u8868\u76f8\u5bf9\u4f18\u52bf\uff1b\u660e\u663e\u6536\u7f29\u7684\u7ef4\u5ea6\u5219\u662f\u4f18\u5148\u653f\u7b56\u5165\u53e3\u3002"
            ),
            mod_spinner(plotly::plotlyOutput(ns("radar"), height = 430))
          ),
          mod_card(
            kicker = "F2 \u00b7 Peer reference",
            title = "\u540c\u4f34\u56fd\u5bf9\u6807",
            htmltools::p(class = "card-note",
              "\u6309\u6536\u5165\u7ec4\u9009\u51fa 5 \u4e2a\u53c2\u8003\u56fd\u5bb6\uff0c\u7528\u4e8e\u5feb\u901f\u5b9a\u4f4d\u53ef\u6bd4\u6837\u672c\u3002"),
            mod_v3_chart_guide(
              "\u5bf9\u6807\u9700\u8981\u540c\u65f6\u770b\u652f\u51fa\u3001OOPS \u548c\u5bff\u547d",
              "\u9ad8 CHE/cap \u4e0d\u4e00\u5b9a\u4ee3\u8868\u66f4\u597d\u4ea7\u51fa\uff0c\u53ea\u6709\u5bb6\u5ead\u8d1f\u62c5\u548c\u5bff\u547d\u4e00\u8d77\u6539\u5584\u65f6\uff0c\u624d\u66f4\u50cf\u6709\u6548\u7684\u5236\u5ea6\u5bf9\u6807\u3002",
              tone = "good"
            ),
            mod_spinner(reactable::reactableOutput(ns("peer_table")))
          )
        ),
        mod_card(
          kicker = "F3 \u00b7 Action notes",
          title = "\u91cf\u5316\u653f\u7b56\u5efa\u8bae",
          htmltools::p(class = "card-note",
            "\u6839\u636e\u5f53\u524d\u8bca\u65ad\u3001\u9608\u503c\u4e0e\u540c\u4f34\u56fd\u5bf9\u6807\u751f\u6210\u7684\u5efa\u8bae\u3002"),
          shiny::uiOutput(ns("recommendations")),
          footer = "\u5efa\u8bae\u662f\u6570\u636e\u89c4\u5219\u751f\u6210\u7684\u4f18\u5148\u7ea7\u63d0\u793a\uff0c\u5e94\u7ed3\u5408\u5236\u5ea6\u80cc\u666f\u3001\u75be\u75c5\u8d1f\u62c5\u548c\u8d22\u653f\u7a7a\u95f4\u89e3\u8bfb\u3002"
        ),
        mod_card(
          kicker = "F4 \u00b7 Time check",
          title = "\u91cd\u70b9\u6307\u6807\u8d8b\u52bf",
          mod_v3_chart_guide(
            "\u8d8b\u52bf\u51b3\u5b9a\u5efa\u8bae\u7d27\u6025\u5ea6",
            "\u5982\u679c OOPS \u8fde\u7eed\u4e0a\u5347\u6216 EXT \u957f\u671f\u504f\u9ad8\uff0c\u5c31\u9700\u8981\u6bd4\u5355\u5e74\u8bc4\u5206\u66f4\u9ad8\u7684\u653f\u7b56\u4f18\u5148\u7ea7\u3002",
            tone = "warn"
          ),
          mod_spinner(plotly::plotlyOutput(ns("trend_panel"), height = 380))
        )
      )
    )
  )
}

mod_policy_server <- function(id, master_r) {
  shiny::moduleServer(id, function(input, output, session) {

    country_data <- shiny::reactive({
      m <- master_r()
      shiny::req(input$country)
      yr <- max(m$year, na.rm = TRUE)
      latest <- m[m$iso3_code == input$country & m$year == yr, , drop = FALSE]
      if (!nrow(latest)) {
        latest <- m[m$iso3_code == input$country, , drop = FALSE]
        latest <- latest[order(-latest$year), , drop = FALSE][1, , drop = FALSE]
      }
      latest
    })

    output$country_header <- shiny::renderUI({
      d <- country_data()
      htmltools::div(
        style = "padding: 18px 24px; background: #f1e8da; border-radius: 12px; margin-bottom: 16px;",
        htmltools::h3(style = "margin: 0; font-family: 'Source Serif 4', serif;",
                     sprintf("%s (%s)", d$country_name[1], d$iso3_code[1])),
        htmltools::p(style = "margin: 4px 0 0; color: #5d667a;",
                    sprintf("\u5927\u6d32: %s \u00b7 \u6536\u5165\u7ec4: %s \u00b7 \u6700\u65b0\u5e74: %d",
                            d$continent[1] %||% "—",
                            d$income_group[1] %||% "—",
                            d$year[1]))
      )
    })

    output$score_strip <- shiny::renderUI({
      d <- country_data(); m <- master_r()
      yr <- d$year[1]
      ref <- m[m$year == yr, ]

      score <- function(val, ref_vals, lower_better = FALSE) {
        if (!is.finite(val)) return(NA_real_)
        finite_ref <- ref_vals[is.finite(ref_vals)]
        if (length(finite_ref) < 5) return(NA_real_)
        rank_pct <- mean(val >= finite_ref) * 100
        if (lower_better) 100 - rank_pct else rank_pct
      }

      fp_score <- score(d$hf3_che[1], ref$hf3_che, lower_better = TRUE)
      ext_score <- score(d$ext_che[1], ref$ext_che, lower_better = TRUE)
      prevent_score <- score(if ("hc6_che" %in% names(d)) d$hc6_che[1] else NA, 
                              if ("hc6_che" %in% names(ref)) ref$hc6_che else NA)
      life_score <- score(d$life_exp[1], ref$life_exp)

      mk_kpi <- function(s, label, hint) {
        if (is.na(s)) return(mod_v3_kpi("—", label, hint = hint, tone = "neutral"))
        tone <- if (s >= 70) "good" else if (s >= 40) "warn" else "bad"
        mod_v3_kpi(sprintf("%.0f", s), label, hint = hint, tone = tone)
      }

      mod_v3_kpi_grid(
        mk_kpi(fp_score, "\u8d22\u52a1\u4fdd\u62a4", "OOPS \u8d8a\u4f4e\u8d8a\u597d"),
        mk_kpi(ext_score, "\u81ea\u4e3b\u8d22\u52a1", "EXT \u8d8a\u4f4e\u8d8a\u597d"),
        mk_kpi(prevent_score, "\u9884\u9632\u504f\u91cd", "HC6 \u8d8a\u9ad8\u8d8a\u597d"),
        mk_kpi(life_score, "\u5bff\u547d\u4ea7\u51fa", "\u6309\u5f53\u524d\u5e74\u540c\u6837\u672c\u6392\u540d")
      )
    })

    output$radar <- plotly::renderPlotly({
      d <- country_data(); m <- master_r()
      yr <- d$year[1]
      ref <- m[m$year == yr & m$income_group == d$income_group[1], ]
      vars <- c("che_pc_usd2023", "gghed_che", "life_exp")
      vars <- vars[vars %in% names(d)]
      country_vals <- as.numeric(d[1, vars])
      peer_vals <- sapply(vars, function(v) median(ref[[v]], na.rm = TRUE))
      max_vals <- sapply(vars, function(v) max(ref[[v]], na.rm = TRUE))
      country_norm <- pmin(1, country_vals / max_vals)
      peer_norm <- pmin(1, peer_vals / max_vals)
      labels <- c("\u4eba\u5747 CHE", "GGHE-D %", "\u9884\u671f\u5bff\u547d")
      labels <- labels[seq_along(vars)]
      safe_plotly({
        plotly::plot_ly(
          type = "scatterpolar", mode = "lines+markers", fill = "toself"
        ) |>
          plotly::add_trace(
            r = c(country_norm, country_norm[1]),
            theta = c(labels, labels[1]),
            name = d$country_name[1],
            line = list(color = "#c46327", width = 2),
            fillcolor = "rgba(196,99,39,0.25)"
          ) |>
          plotly::add_trace(
            r = c(peer_norm, peer_norm[1]),
            theta = c(labels, labels[1]),
            name = "\u540c\u6536\u5165\u7ec4\u4e2d\u4f4d\u6570",
            line = list(color = "#1d3f5f", width = 2, dash = "dash"),
            fillcolor = "rgba(29,63,95,0.15)"
          ) |>
          plotly::layout(polar = list(radialaxis = list(visible = TRUE, range = c(0, 1))))
      })
    })

    output$peer_table <- reactable::renderReactable({
      d <- country_data(); m <- master_r()
      yr <- d$year[1]
      peers <- m[m$year == yr & m$income_group == d$income_group[1] &
                 m$iso3_code != d$iso3_code[1], ]
      peers <- peers[is.finite(peers$che_pc_usd2023), ]
      peers <- utils::head(peers[order(-peers$che_pc_usd2023), ], 5)
      tab <- data.frame(
        country = peers$country_name,
        che_pc = round(peers$che_pc_usd2023, 0),
        oops_pct = round(peers$hf3_che, 1),
        life_exp = round(peers$life_exp, 1),
        check.names = FALSE
      )
      reactable::reactable(tab, pagination = FALSE, highlight = TRUE,
        defaultColDef = reactable::colDef(headerStyle = list(background = "#f1f3f7")))
    })

    output$recommendations <- shiny::renderUI({
      d <- country_data(); m <- master_r()
      yr <- d$year[1]
      ref <- m[m$year == yr & m$income_group == d$income_group[1], ]

      recs <- htmltools::tagList()

      if ("fp" %in% input$priorities && is.finite(d$hf3_che[1])) {
        target <- 25
        if (d$hf3_che[1] > target) {
          gap <- d$hf3_che[1] - target
          recs <- htmltools::tagAppendChild(recs, mod_v3_callout(
            sprintf(htmltools::HTML("\u5f53\u524d OOPS = %.1f%%\uff0c\u8d85\u8fc7 WHO \u63a8\u8350\u9608\u503c (25%%)\u3002\u5efa\u8bae\u901a\u8fc7\u6269\u5927\u516c\u5171\u62a5\u9500\u8303\u56f4\u3001\u63d0\u9ad8\u9884\u4ed8\u6bd4\u4f8b\u6216\u5bf9\u8106\u5f31\u4eba\u7fa4\u7ed9\u4e88\u5b9a\u5411\u8865\u52a9\uff0c\u4f18\u5148\u7f29\u5c0f %.1f \u4e2a\u767e\u5206\u70b9\u7684\u5bb6\u5ead\u73b0\u91d1\u652f\u4ed8\u7f3a\u53e3\u3002", d$hf3_che[1], gap)),
            tone = "warn", title = "\u8d22\u52a1\u4fdd\u62a4"))
        } else {
          recs <- htmltools::tagAppendChild(recs, mod_v3_callout(
            sprintf("\u5f53\u524d OOPS = %.1f%%\uff0c\u4f4e\u4e8e WHO \u9608\u503c\u3002\u5efa\u8bae\u4fdd\u6301\u73b0\u6709\u8d22\u52a1\u4fdd\u62a4\u673a\u5236\u3002", d$hf3_che[1]),
            tone = "good", title = "\u8d22\u52a1\u4fdd\u62a4"))
        }
      }

      if ("ext" %in% input$priorities && is.finite(d$ext_che[1])) {
        if (d$ext_che[1] > 20) {
          recs <- htmltools::tagAppendChild(recs, mod_v3_callout(
            sprintf("\u5916\u63f4\u4f9d\u8d56 = %.1f%%\uff0c\u504f\u9ad8\u3002\u5efa\u8bae\u589e\u52a0\u672c\u56fd\u8d22\u653f\u8d44\u52a9\u4ee5\u63d0\u9ad8\u53ef\u6301\u7eed\u6027\u3002", d$ext_che[1]),
            tone = "warn", title = "\u8d22\u653f\u81ea\u4e3b"))
        }
      }

      if ("prevent" %in% input$priorities && "hc6_che" %in% names(d)) {
        hc6 <- d$hc6_che[1]
        ref_hc6 <- median(ref$hc6_che, na.rm = TRUE)
        if (is.finite(hc6) && is.finite(ref_hc6) && hc6 < ref_hc6 * 0.7) {
          recs <- htmltools::tagAppendChild(recs, mod_v3_callout(
            sprintf("\u9884\u9632\u6027\u62a4\u7406\u5360\u6bd4 = %.1f%%\uff0c\u4f4e\u4e8e\u540c\u6536\u5165\u7ec4\u4e2d\u4f4d\u6570 (%.1f%%)\u3002\u5efa\u8bae\u52a0\u5927\u9884\u9632\u6027\u62a4\u7406\u6295\u5165\u3002", hc6, ref_hc6),
            tone = "warn", title = "\u9884\u9632\u504f\u91cd"))
        }
      }

      if ("life" %in% input$priorities && is.finite(d$life_exp[1])) {
        ref_life <- median(ref$life_exp, na.rm = TRUE)
        if (d$life_exp[1] < ref_life - 3) {
          recs <- htmltools::tagAppendChild(recs, mod_v3_callout(
            sprintf("\u5bff\u547d = %.1f \u5c81\uff0c\u4f4e\u4e8e\u540c\u6536\u5165\u7ec4\u4e2d\u4f4d\u6570 (%.1f \u5c81) %0.1f \u5c81\u3002\u5efa\u8bae\u4f18\u5148\u6392\u67e5\u53ef\u9884\u9632\u6b7b\u4ea1\u3001\u6162\u75c5\u7ba1\u7406\u548c\u57fa\u5c42\u670d\u52a1\u53ef\u53ca\u6027\uff0c\u5c06\u8d44\u91d1\u589e\u91cf\u8f6c\u5316\u4e3a\u66f4\u53ef\u89c1\u7684\u5065\u5eb7\u4ea7\u51fa\u3002", d$life_exp[1], ref_life, ref_life - d$life_exp[1]),
            tone = "bad", title = "\u5bff\u547d\u4ea7\u51fa"))
        }
      }

      if (length(recs) == 0) {
        recs <- mod_v3_callout(
          "\u5f53\u524d\u9009\u4e2d\u7684\u4f18\u5148\u9886\u57df\u4e0b\u672a\u8bca\u65ad\u51fa\u660e\u663e\u95ee\u9898\u3002\u5efa\u8bae\u7ee7\u7eed\u4fdd\u6301\u5e76\u5173\u6ce8\u8de8\u5e74\u8d8b\u52bf\u3002",
          tone = "good", title = "\u8bca\u65ad\u7ed3\u679c")
      }

      recs
    })

    output$trend_panel <- plotly::renderPlotly({
      m <- master_r()
      d <- m[m$iso3_code == input$country, ]
      shiny::req(nrow(d) > 5)
      safe_plotly({
        plotly::plot_ly() |>
          plotly::add_lines(data = d, x = ~year, y = ~hf3_che, name = "OOPS%",
                             line = list(color = "#a23b3b", width = 2)) |>
          plotly::add_lines(data = d, x = ~year, y = ~gghed_che, name = "GGHED%",
                             line = list(color = "#1d3f5f", width = 2)) |>
          plotly::add_lines(data = d, x = ~year, y = ~ext_che, name = "EXT%",
                             line = list(color = "#2a857a", width = 2)) |>
          ghs_plotly_layout() |>
          plotly::layout(xaxis = list(title = ""),
                         yaxis = list(title = "\u5360 CHE \u6bd4\u4f8b (%)"),
                         legend = list(orientation = "h", y = -0.15))
      })
    })
  })
}

`%||%` <- function(a, b) if (is.null(a) || is.na(a)) b else a
