
mod_robustness_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = "\u7a33\u5065\u6027 Robustness",
    value = "robustness",
    mod_v3_hero(
      kicker = "\u7a33\u5065\u6027\u4e0e\u654f\u611f\u5ea6",
      title = "\u7a33\u5065\u6027\u4e0e\u654f\u611f\u5ea6\u5206\u6790",
      lead = paste(
        "\u8c03\u8282\u6837\u672c\u8303\u56f4\u4e0e\u9608\u503c\uff0c\u89c2\u5bdf\u5173\u952e\u7ed3\u8bba\u662f\u5426\u7a33\u5b9a\u3002",
        "\u9875\u9762\u540c\u65f6\u63d0\u4f9b\u6837\u672c\u88c1\u526a\u3001\u5e74\u4efd\u7a97\u53e3\u3001\u5f02\u5e38\u503c\u9608\u503c\u548c Bootstrap \u91cd\u91c7\u6837\uff0c\u7528\u6765\u68c0\u67e5\u7ed3\u8bba\u662f\u5426\u53d7\u5c11\u6570\u89c2\u6d4b\u652f\u914d\u3002"
      ),
      meta = list("\u4ea4\u4e92\u5f0f\u654f\u611f\u5ea6", "Bootstrap \u7f6e\u4fe1\u533a\u95f4", "Cook \u8ddd\u79bb\u8bca\u65ad")
    ),
    mod_v3_page_body(
      wide = TRUE,
      mod_v3_badge_row(
        "\u5e74\u4efd\u7a97\u53e3",
        "\u6700\u5c0f\u56fd\u5bb6\u89c2\u6d4b",
        "\u5927\u6d32\u6392\u9664",
        "Bootstrap \u91cd\u91c7\u6837",
        tone = "warn"
      ),
      mod_v3_story_grid(
        columns = 3,
        mod_v3_insight(
          kicker = "\u654f\u611f\u5ea6",
          title = "\u4e0d\u628a\u5355\u4e00\u6837\u672c\u5f53\u6210\u552f\u4e00\u7b54\u6848",
          text = "\u5e74\u4efd\u8303\u56f4\u3001\u5927\u6d32\u7ec4\u5408\u548c\u6700\u5c0f\u89c2\u6d4b\u6570\u90fd\u4f1a\u6539\u53d8\u56de\u5f52\u6837\u672c\uff0c\u8fd9\u91cc\u7528\u4ea4\u4e92\u63a7\u4ef6\u628a\u654f\u611f\u5ea6\u653e\u5230\u53f0\u524d\u3002",
          tone = "warn",
          icon = "S"
        ),
        mod_v3_insight(
          kicker = "\u91cd\u91c7\u6837",
          title = "\u7528\u91cd\u91c7\u6837\u770b\u4f30\u8ba1\u6ce2\u52a8",
          text = "Bootstrap \u5206\u5e03\u76f4\u89c2\u5448\u73b0 GDP-CHE \u5f39\u6027\u5728\u968f\u673a\u62bd\u6837\u4e0b\u7684\u4e0d\u786e\u5b9a\u6027\uff0c\u800c\u4e0d\u53ea\u662f\u62a5\u544a\u4e00\u4e2a\u70b9\u4f30\u8ba1\u3002",
          tone = "secondary",
          icon = "B"
        ),
        mod_v3_insight(
          kicker = "\u5f02\u5e38\u503c",
          title = "\u628a\u6781\u7aef OOPS \u5355\u72ec\u5217\u51fa",
          text = "\u5f02\u5e38\u503c\u8868\u4fdd\u7559\u56fd\u5bb6\u3001\u5e74\u4efd\u3001OOPS\u3001CHE \u548c GDP\uff0c\u4fbf\u4e8e\u56de\u5230\u56fd\u5bb6\u753b\u50cf\u6216\u539f\u59cb\u8bb0\u5f55\u8fdb\u4e00\u6b65\u6838\u5bf9\u3002",
          tone = "bad",
          icon = "O"
        )
      ),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 300,
          shiny::sliderInput(ns("year_range"), "\u5e74\u4efd\u8303\u56f4",
            min = 2000, max = 2023, value = c(2000, 2023), step = 1, sep = ""),
          shiny::sliderInput(ns("min_obs"), "\u6700\u5c11\u89c2\u6d4b\u6570\uff08\u6309\u56fd\u5bb6\uff09",
            min = 5, max = 24, value = 10),
          shiny::sliderInput(ns("oop_threshold"), "OOPS \u5f02\u5e38\u9608\u503c (%)",
            min = 60, max = 90, value = 80),
          shiny::checkboxGroupInput(ns("exclude_continents"), "\u6392\u9664\u5927\u6d32",
            choices = c("\u975e\u6d32" = "Africa", "\u7f8e\u6d32" = "Americas",
                        "\u4e9a\u6d32" = "Asia", "\u6b27\u6d32" = "Europe",
                        "\u5927\u6d0b\u6d32" = "Oceania"),
            selected = character(0)),
          shiny::numericInput(ns("boot_B"), "Bootstrap \u91cd\u590d\u6b21\u6570",
            value = 200, min = 50, max = 1000, step = 50),
          shiny::actionButton(ns("rerun"), "\u91cd\u65b0\u8ba1\u7b97 Bootstrap",
            class = "btn-primary"),
          mod_v3_sidebar_note(
            title = "\u53c2\u6570\u8bf4\u660e",
            text = "\u8c03\u6574\u4efb\u4f55\u6837\u672c\u6761\u4ef6\u540e\uff0c\u53f3\u4fa7\u7edf\u8ba1\u548c\u56fe\u8868\u4f1a\u81ea\u52a8\u66f4\u65b0\u3002",
            bullets = c(
              "\u6700\u5c11\u89c2\u6d4b\u6570\u8fc7\u9ad8\u4f1a\u5254\u9664\u65ad\u6863\u56fd\u5bb6\u3002",
              "\u6392\u9664\u5927\u6d32\u53ef\u7528\u4e8e\u68c0\u67e5\u5730\u533a\u9a71\u52a8\u6548\u5e94\u3002",
              "Bootstrap \u6b21\u6570\u8d8a\u9ad8\uff0c\u8ba1\u7b97\u8017\u65f6\u8d8a\u957f\u3002"
            )
          )
        ),
        shiny::uiOutput(ns("kpi_strip")),
        mod_v3_rail(list(
          list(title = "\u8bbe\u5b9a\u6837\u672c", text = "\u901a\u8fc7\u5e74\u4efd\u3001\u5927\u6d32\u548c\u6700\u5c0f\u89c2\u6d4b\u6570\u5b9a\u4e49\u5206\u6790\u7a97\u53e3\u3002"),
          list(title = "\u770b\u5f39\u6027", text = "\u6bd4\u8f83\u4e0d\u540c\u5b50\u6837\u672c\u7684 GDP-CHE \u5f39\u6027\u70b9\u4f30\u8ba1\u548c\u7f6e\u4fe1\u533a\u95f4\u3002"),
          list(title = "\u91cd\u91c7\u6837", text = "\u70b9\u51fb\u6309\u94ae\u91cd\u65b0\u751f\u6210 Bootstrap \u5206\u5e03\uff0c\u68c0\u67e5\u70b9\u4f30\u8ba1\u662f\u5426\u7a33\u5b9a\u3002"),
          list(title = "\u67e5\u5f02\u5e38", text = "\u5c06\u8d85\u9608\u503c OOPS \u8bb0\u5f55\u5355\u72ec\u5217\u51fa\uff0c\u5c06\u7edf\u8ba1\u654f\u611f\u70b9\u8fd8\u539f\u5230\u56fd\u5bb6-\u5e74\u4efd\u3002")
        )),
        bslib::layout_columns(
          col_widths = c(6, 6),
          mod_card(
            kicker = "\u5f39\u6027\u4f30\u8ba1",
            title = "\u5f39\u6027\u4f30\u8ba1\u4e0e\u7f6e\u4fe1\u533a\u95f4\uff08\u4e0d\u540c\u6837\u672c\u5b50\u96c6\uff09",
            mod_v3_chart_guide(
              title = "\u8bfb\u56fe\u65b9\u5f0f",
              text = "log(CHE_pc) ~ log(GDP_pc) \u7684\u4f30\u8ba1\u503c\u5982\u679c\u5728\u4e0d\u540c\u5b50\u6837\u672c\u95f4\u63a5\u8fd1\uff0c\u8bf4\u660e\u5f39\u6027\u7ed3\u8bba\u66f4\u7a33\u5b9a\u3002",
              bullets = c(
                "\u70b9\u4f4d\u7f6e\u8868\u793a\u4f30\u8ba1\u5f39\u6027\u3002",
                "\u8bef\u5dee\u7ebf\u8868\u793a 95% \u7f6e\u4fe1\u533a\u95f4\u3002",
                "\u533a\u95f4\u8fc7\u5bbd\u901a\u5e38\u610f\u5473\u7740\u6837\u672c\u4e0d\u8db3\u6216\u5185\u90e8\u5dee\u5f02\u5927\u3002"
              )
            ),
            mod_spinner(plotly::plotlyOutput(ns("elasticity_box"), height = 380))
          ),
          mod_card(
            kicker = "\u91cd\u91c7\u6837",
            title = "Bootstrap \u5f39\u6027\u5206\u5e03",
            mod_v3_chart_guide(
              title = "\u4e0d\u786e\u5b9a\u6027",
              text = "\u76f4\u65b9\u56fe\u5c55\u793a\u91cd\u91c7\u6837\u540e\u7684\u5f39\u6027\u4f30\u8ba1\u5206\u5e03\uff1b\u865a\u7ebf\u662f\u5e73\u5747\u503c\u3002",
              tone = "good"
            ),
            mod_spinner(plotly::plotlyOutput(ns("boot_dist"), height = 380))
          )
        ),
        bslib::layout_columns(
          col_widths = c(5, 7),
          mod_card(
            kicker = "\u6837\u672c\u6784\u6210",
            title = "\u5f53\u524d\u7a97\u53e3\u6837\u672c\u68c0\u67e5",
            mod_v3_chart_guide(
              title = "\u5148\u770b\u6837\u672c",
              text = "\u6240\u6709\u7a33\u5065\u6027\u5224\u65ad\u90fd\u4f9d\u8d56\u5f53\u524d\u7b5b\u9009\u540e\u7684\u56fd\u5bb6\u548c\u89c2\u6d4b\u6784\u6210\uff0c\u8868\u683c\u7528\u6765\u68c0\u67e5\u662f\u5426\u88ab\u5355\u4e00\u6536\u5165\u7ec4\u6216\u5927\u6d32\u652f\u914d\u3002",
              tone = "info"
            ),
            mod_spinner(reactable::reactableOutput(ns("sample_table")))
          ),
          mod_card(
            kicker = "\u5b50\u6837\u672c\u8868",
            title = "\u4e0d\u540c\u6837\u672c\u5b50\u96c6\u4e0b\u7684\u7a33\u5065\u6027\u8868",
            mod_spinner(reactable::reactableOutput(ns("robust_table"))),
            footer = "\u82e5\u4f4e\u6536\u5165\u7ec4\u6216\u5355\u4e00\u5927\u6d32\u6837\u672c\u7ed3\u679c\u504f\u79bb\u660e\u663e\uff0c\u7ed3\u8bba\u5e94\u5206\u7ec4\u8868\u8ff0\u3002"
          )
        ),
        mod_card(
          kicker = "\u5f02\u5e38\u8bca\u65ad",
          title = "\u5f02\u5e38\u503c\u8bca\u65ad",
          mod_v3_chart_guide(
            title = "\u8bca\u65ad\u53e3\u5f84",
            text = "\u5217\u51fa OOPS \u5360 CHE \u8d85\u8fc7\u9608\u503c\u7684\u56fd\u5bb6-\u5e74\u4efd\u8bb0\u5f55\uff0c\u8fd9\u4e9b\u70b9\u53ef\u80fd\u5f71\u54cd\u56de\u5f52\u4f30\u8ba1\u6216\u653f\u7b56\u89e3\u8bfb\u3002",
            tone = "warn"
          ),
          mod_spinner(reactable::reactableOutput(ns("outlier_table")))
        )
      )
    )
  )
}

mod_robustness_server <- function(id, master_r) {
  shiny::moduleServer(id, function(input, output, session) {

    continent_labels <- c(
      Africa = "\u975e\u6d32",
      Americas = "\u7f8e\u6d32",
      Asia = "\u4e9a\u6d32",
      Europe = "\u6b27\u6d32",
      Oceania = "\u5927\u6d0b\u6d32",
      All = "\u5168\u90e8\u6837\u672c"
    )
    income_labels <- c(
      "High income" = "\u9ad8\u6536\u5165",
      "Upper middle income" = "\u4e2d\u9ad8\u6536\u5165",
      "Lower middle income" = "\u4e2d\u4f4e\u6536\u5165",
      "Low income" = "\u4f4e\u6536\u5165",
      All = "\u5168\u90e8\u6837\u672c"
    )
    label_lookup <- function(x, dict) {
      if (!length(x) || is.na(x)) return("\u672a\u5206\u7ec4")
      out <- unname(dict[as.character(x)])
      if (is.null(out) || is.na(out)) x else unname(out)
    }

    filtered <- shiny::reactive({
      m <- master_r()
      d <- m[m$year >= input$year_range[1] & m$year <= input$year_range[2], , drop = FALSE]
      if (length(input$exclude_continents) > 0) {
        d <- d[!d$continent %in% input$exclude_continents, , drop = FALSE]
      }
      iso_counts <- table(d$iso3_code)
      keep_isos <- names(iso_counts[iso_counts >= input$min_obs])
      d[d$iso3_code %in% keep_isos, , drop = FALSE]
    })

    output$kpi_strip <- shiny::renderUI({
      d <- filtered()
      mod_v3_kpi_grid(
        mod_v3_kpi(format(length(unique(d$iso3_code)), big.mark = ","),
                   "\u8986\u76d6\u56fd\u5bb6", tone = "primary"),
        mod_v3_kpi(format(nrow(d), big.mark = ","), "\u89c2\u6d4b\u6570", tone = "secondary"),
        mod_v3_kpi(sprintf("%d\u2013%d", input$year_range[1], input$year_range[2]),
                   "\u5e74\u4efd\u8303\u56f4", tone = "neutral"),
        mod_v3_kpi(as.character(input$boot_B), "Bootstrap B", tone = "warn")
      )
    })

    output$elasticity_box <- plotly::renderPlotly({
      d <- filtered()
      d <- d[is.finite(d$che_pc_usd2023) & is.finite(d$gdp_pc_usd) &
             d$che_pc_usd2023 > 0 & d$gdp_pc_usd > 0, ]
      shiny::req(nrow(d) > 50)
      results <- list()
      for (sub in c("Africa", "Asia", "Europe", "Americas", "All")) {
        sub_d <- if (sub == "All") d else d[d$continent == sub, ]
        if (nrow(sub_d) > 30) {
          fit <- stats::lm(log(che_pc_usd2023) ~ log(gdp_pc_usd), data = sub_d)
          ci <- stats::confint(fit)["log(gdp_pc_usd)", ]
          results[[sub]] <- data.frame(
            sample = label_lookup(sub, continent_labels),
            est = stats::coef(fit)["log(gdp_pc_usd)"],
            lo = ci[1], hi = ci[2]
          )
        }
      }
      df <- do.call(rbind, results)
      safe_plotly({
        plotly::plot_ly(df, x = ~sample, y = ~est,
                        error_y = list(type = "data",
                                       symmetric = FALSE,
                                       array = ~(hi - est),
                                       arrayminus = ~(est - lo)),
                        type = "scatter", mode = "markers",
                        marker = list(size = 12, color = "#1d3f5f")) |>
          ghs_plotly_layout() |>
          plotly::layout(
            xaxis = list(title = "\u6837\u672c\u5b50\u96c6"),
            yaxis = list(title = "GDP \u5f39\u6027 \u00b1 95% CI")
          )
      })
    })

    boot_results <- shiny::eventReactive(input$rerun, {
      d <- filtered()
      d <- d[is.finite(d$che_pc_usd2023) & is.finite(d$gdp_pc_usd) &
             d$che_pc_usd2023 > 0 & d$gdp_pc_usd > 0, ]
      shiny::req(nrow(d) > 50)
      set.seed(1)
      replicate(input$boot_B, {
        idx <- sample(seq_len(nrow(d)), replace = TRUE)
        fit <- stats::lm(log(che_pc_usd2023) ~ log(gdp_pc_usd), data = d[idx, ])
        stats::coef(fit)["log(gdp_pc_usd)"]
      })
    }, ignoreNULL = FALSE)

    output$boot_dist <- plotly::renderPlotly({
      bs <- boot_results()
      shiny::req(length(bs) > 0)
      safe_plotly({
        plotly::plot_ly(x = bs, type = "histogram",
                        marker = list(color = "#c46327", opacity = 0.7),
                        nbinsx = 30) |>
          ghs_plotly_layout() |>
          plotly::layout(
            xaxis = list(title = "GDP \u5f39\u6027\u4f30\u8ba1\u503c"),
            yaxis = list(title = "\u9891\u6b21"),
            shapes = list(
              list(type = "line", x0 = mean(bs), x1 = mean(bs),
                   y0 = 0, y1 = 1, yref = "paper",
                   line = list(color = "#1d3f5f", width = 2, dash = "dash"))
            )
          )
      })
    })

    output$sample_table <- reactable::renderReactable({
      d <- filtered()
      shiny::req(nrow(d) > 0)
      d$continent_label <- vapply(d$continent, label_lookup, character(1),
                                  dict = continent_labels)
      d$income_label <- vapply(d$income_group, label_lookup, character(1),
                               dict = income_labels)
      groups <- split(d, interaction(d$continent_label, d$income_label, drop = TRUE))
      df <- do.call(rbind, lapply(groups, function(ch) {
        data.frame(
          continent = ch$continent_label[1],
          income = ch$income_label[1],
          countries = length(unique(ch$iso3_code)),
          records = nrow(ch),
          pct = round(nrow(ch) / nrow(d) * 100, 1),
          che_median = round(stats::median(ch$che_pc_usd2023, na.rm = TRUE), 0),
          check.names = FALSE
        )
      }))
      df <- df[order(-df$records), , drop = FALSE]
      names(df) <- c("\u5927\u6d32", "\u6536\u5165\u7ec4", "\u56fd\u5bb6\u6570",
                     "\u89c2\u6d4b\u6570", "\u6837\u672c\u5360\u6bd4(%)",
                     "\u4eba\u5747 CHE \u4e2d\u4f4d\u6570")
      reactable::reactable(df, defaultPageSize = 10, highlight = TRUE,
        defaultColDef = reactable::colDef(headerStyle = list(background = "#f1f3f7")),
        columns = list(
          "\u56fd\u5bb6\u6570" = reactable::colDef(align = "right"),
          "\u89c2\u6d4b\u6570" = reactable::colDef(align = "right",
            format = reactable::colFormat(separators = TRUE)),
          "\u6837\u672c\u5360\u6bd4(%)" = reactable::colDef(align = "right"),
          "\u4eba\u5747 CHE \u4e2d\u4f4d\u6570" = reactable::colDef(align = "right",
            format = reactable::colFormat(separators = TRUE))
        ))
    })

    output$robust_table <- reactable::renderReactable({
      d <- filtered()
      d <- d[is.finite(d$che_pc_usd2023) & is.finite(d$gdp_pc_usd) &
             d$che_pc_usd2023 > 0 & d$gdp_pc_usd > 0, ]
      results <- data.frame()
      for (sub in c("All", "High income", "Upper middle income",
                    "Lower middle income", "Low income")) {
        sub_d <- if (sub == "All") d else d[d$income_group == sub, ]
        if (nrow(sub_d) > 30) {
          fit <- stats::lm(log(che_pc_usd2023) ~ log(gdp_pc_usd), data = sub_d)
          se <- summary(fit)$coefficients["log(gdp_pc_usd)", "Std. Error"]
          results <- rbind(results, data.frame(
            sample_group = label_lookup(sub, income_labels),
            n = nrow(sub_d),
            elasticity = round(stats::coef(fit)["log(gdp_pc_usd)"], 3),
            se = round(se, 3),
            R2 = round(summary(fit)$r.squared, 3),
            check.names = FALSE
          ))
        }
      }
      if (!nrow(results)) {
        results <- data.frame(
          "\u6837\u672c\u5b50\u96c6" = character(),
          "n" = integer(),
          "\u5f39\u6027" = numeric(),
          "\u6807\u51c6\u8bef" = numeric(),
          "R\u00b2" = numeric(),
          check.names = FALSE
        )
      } else {
        names(results) <- c("\u6837\u672c\u5b50\u96c6", "n", "\u5f39\u6027", "\u6807\u51c6\u8bef", "R\u00b2")
      }
      reactable::reactable(results, pagination = FALSE, highlight = TRUE,
        defaultColDef = reactable::colDef(headerStyle = list(background = "#f1f3f7")))
    })

    output$outlier_table <- reactable::renderReactable({
      d <- filtered()
      d <- d[is.finite(d$hf3_che) & d$hf3_che > input$oop_threshold, ]
      d <- d[order(-d$hf3_che), c("country_name", "year", "iso3_code",
                                    "hf3_che", "che_pc_usd2023", "gdp_pc_usd")]
      d$hf3_che <- round(d$hf3_che, 1)
      d$che_pc_usd2023 <- round(d$che_pc_usd2023, 0)
      d$gdp_pc_usd <- round(d$gdp_pc_usd, 0)
      names(d) <- c("\u56fd\u5bb6", "\u5e74\u4efd", "ISO", "OOPS%", "\u4eba\u5747 CHE", "\u4eba\u5747 GDP")
      reactable::reactable(d, defaultPageSize = 12, searchable = TRUE,
        defaultColDef = reactable::colDef(headerStyle = list(background = "#f1f3f7")))
    })
  })
}
