# =============================================================================
# 仪表盘/模块/mod_robustness.R
# 稳健性分析：用户可拖动样本切片，实时观察结论敏感性
# =============================================================================

mod_robustness_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = htmltools::HTML("&#129534; \u7a33\u5065\u6027 Robustness"),
    mod_v3_hero(
      kicker = "ROBUSTNESS & SENSITIVITY",
      title = "\u7a33\u5065\u6027\u4e0e\u654f\u611f\u5ea6\u5206\u6790",
      lead = paste(
        "\u8c03\u8282\u6837\u672c\u8303\u56f4\u4e0e\u9608\u503c\uff0c\u89c2\u5bdf\u5173\u952e\u7ed3\u8bba\u662f\u5426\u7a33\u5b9a\u3002",
        "\u5305\u542b\uff1a\u6837\u672c\u88c1\u526a\u3001year_col\u9650\u5236\u3001\u5f02\u5e38\u503c\u9608\u503c\u3001\u5f15\u8d77\u8bef\u5deeBootstrap \u3002"
      ),
      meta = list("\u4ea4\u4e92\u5f0f\u654f\u611f\u5ea6", "Bootstrap CI", "Cook's distance")
    ),
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 280,
        shiny::sliderInput(ns("year_range"), "year_col\u8303\u56f4",
          min = 2000, max = 2023, value = c(2000, 2023), step = 1, sep = ""),
        shiny::sliderInput(ns("min_obs"), "\u6700\u5c11\u89c2\u6d4b\u6570 (\u6309\u56fd)",
          min = 5, max = 24, value = 10),
        shiny::sliderInput(ns("oop_threshold"), "OOPS \u5f02\u5e38\u9608\u503c (%)",
          min = 60, max = 90, value = 80),
        shiny::checkboxGroupInput(ns("exclude_continents"), "\u6392\u9664\u5927\u6d32",
          choices = c("Africa", "Americas", "Asia", "Europe", "Oceania"),
          selected = character(0)),
        shiny::tags$hr(),
        shiny::numericInput(ns("boot_B"), "Bootstrap \u91cd\u590d\u6b21\u6570",
          value = 200, min = 50, max = 1000, step = 50),
        shiny::actionButton(ns("rerun"), "\u91cd\u65b0\u8ba1\u7b97 Bootstrap",
          class = "btn-primary"),
        shiny::tags$hr(),
        shiny::helpText(
          "\u8c03\u6574\u5de6\u4fa7\u53c2\u6570\u540e\uff0c\u53f3\u4fa7\u7ed3\u679c\u81ea\u52a8\u66f4\u65b0\u3002"
        )
      ),
      shiny::uiOutput(ns("kpi_strip")),
      bslib::layout_columns(
        col_widths = c(6, 6),
        mod_card(
          title = "elasticity\u5305\u62ec\u533a\u95f4\uff08\u4e0d\u540c\u6837\u672c\u5b50\u96c6\uff09",
          htmltools::p(class = "card-note",
            "log(CHE_pc) ~ log(GDP_pc) \u7684\u4f30\u8ba1\u503c\u968f\u6837\u672c\u53d8\u5316\u3002"),
          mod_spinner(plotly::plotlyOutput(ns("elasticity_box"), height = 380))
        ),
        mod_card(
          title = "Bootstrap \u5206\u5e03",
          htmltools::p(class = "card-note",
            "Bootstrap \u4f30\u8ba1\u7684 1000 \u4e2a\u91cd\u91c7\u6837\u3002"),
          mod_spinner(plotly::plotlyOutput(ns("boot_dist"), height = 380))
        )
      ),
      mod_card(
        title = "\u4e0d\u540c\u6837\u672c\u5b50\u96c6\u4e0b\u7684\u5f3a\u5067\u6027\u8868",
        mod_spinner(reactable::reactableOutput(ns("robust_table")))
      ),
      mod_card(
        title = "\u5f02\u5e38\u503c\u8bca\u65ad",
        htmltools::p(class = "card-note",
          "\u8d85\u8fc7\u9608\u503c\u7684country\u00b7year_col\u8bb0\u5f55\u3002"),
        mod_spinner(reactable::reactableOutput(ns("outlier_table")))
      )
    )
  )
}

mod_robustness_server <- function(id, master_r) {
  shiny::moduleServer(id, function(input, output, session) {

    filtered <- shiny::reactive({
      m <- master_r()
      d <- m[m$year >= input$year_range[1] & m$year <= input$year_range[2], , drop = FALSE]
      if (length(input$exclude_continents) > 0) {
        d <- d[!d$continent %in% input$exclude_continents, , drop = FALSE]
      }
      # Apply minimum observations per country
      iso_counts <- table(d$iso3_code)
      keep_isos <- names(iso_counts[iso_counts >= input$min_obs])
      d[d$iso3_code %in% keep_isos, , drop = FALSE]
    })

    output$kpi_strip <- shiny::renderUI({
      d <- filtered()
      mod_v3_kpi_grid(
        mod_v3_kpi(format(length(unique(d$iso3_code)), big.mark = ","),
                   "country\u6570", tone = "primary"),
        mod_v3_kpi(format(nrow(d), big.mark = ","), "\u89c2\u6d4b\u6570", tone = "secondary"),
        mod_v3_kpi(sprintf("%d\u2013%d", input$year_range[1], input$year_range[2]),
                   "year_col\u8303\u56f4", tone = "neutral"),
        mod_v3_kpi(as.character(input$boot_B), "Bootstrap B", tone = "warn")
      )
    })

    output$elasticity_box <- plotly::renderPlotly({
      d <- filtered()
      d <- d[is.finite(d$che_pc_usd2023) & is.finite(d$gdp_pc_usd) &
             d$che_pc_usd2023 > 0 & d$gdp_pc_usd > 0, ]
      shiny::req(nrow(d) > 50)
      # Different sub-samples
      results <- list()
      for (sub in c("Africa", "Asia", "Europe", "Americas", "All")) {
        sub_d <- if (sub == "All") d else d[d$continent == sub, ]
        if (nrow(sub_d) > 30) {
          fit <- stats::lm(log(che_pc_usd2023) ~ log(gdp_pc_usd), data = sub_d)
          ci <- stats::confint(fit)["log(gdp_pc_usd)", ]
          results[[sub]] <- data.frame(
            sample = sub,
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
            xaxis = list(title = "subsample"),
            yaxis = list(title = "GDP elasticity \u00b1 95% CI")
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
            xaxis = list(title = "GDP elasticity\u4f30\u8ba1\u503c"),
            yaxis = list(title = "\u9891\u6b21"),
            shapes = list(
              list(type = "line", x0 = mean(bs), x1 = mean(bs),
                   y0 = 0, y1 = 1, yref = "paper",
                   line = list(color = "#1d3f5f", width = 2, dash = "dash"))
            )
          )
      })
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
            `subsample` = sub,
            n = nrow(sub_d),
            `elasticity` = round(stats::coef(fit)["log(gdp_pc_usd)"], 3),
            `se` = round(se, 3),
            R2 = round(summary(fit)$r.squared, 3),
            check.names = FALSE
          ))
        }
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
      names(d) <- c("country", "year_col", "ISO", "OOPS%", "che_pc", "gdp_pc")
      reactable::reactable(d, defaultPageSize = 12, searchable = TRUE,
        defaultColDef = reactable::colDef(headerStyle = list(background = "#f1f3f7")))
    })
  })
}
