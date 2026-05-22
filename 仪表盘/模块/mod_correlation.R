# =============================================================================
# 仪表盘/模块/mod_correlation.R
# 变量关联：核心指标间的相关矩阵、散点矩阵与条件相关
# =============================================================================

mod_correlation_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = "\u5173\u8054 Correlation",
    value = "correlation",
    mod_v3_hero(
      kicker = "\u53d8\u91cf\u5173\u8054",
      title = "\u6838\u5fc3\u6307\u6807\u95f4\u7684\u5173\u8054\u7ed3\u6784",
      lead = paste(
        "\u536b\u751f\u652f\u51fa\u3001\u7ecf\u6d4e\u6c34\u5e73\u3001\u5065\u5eb7\u4ea7\u51fa\u4e09\u8005\u4e4b\u95f4\u5b58\u5728\u590d\u6742\u7684\u975e\u7ebf\u6027\u5173\u8054\u3002",
        "\u672c\u6a21\u5757\u63d0\u4f9b\u76f8\u5173\u77e9\u9635\u3001\u6563\u70b9\u77e9\u9635\u3001\u6761\u4ef6\u76f8\u5173\u5206\u6790\u3002"
      ),
      meta = list("Pearson / Spearman", "195 \u56fd\u5bb6", "2000\u20132023")
    ),
    mod_v3_page_body(
      wide = TRUE,
      mod_v3_topic_brief("correlation"),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 280,
          shiny::sliderInput(ns("year"), "\u5e74\u4efd",
            min = 2000, max = 2023, value = 2023, step = 1, sep = ""),
          shiny::selectInput(ns("x_var"), "X \u8f74\u6307\u6807",
            choices = c(
              "\u4eba\u5747 CHE (USD)" = "che_pc_usd2023",
              "OOPS (%)" = "hf3_che",
              "GGHE-D (%)" = "gghed_che",
              "GDP/cap" = "gdp_pc_usd",
              "\u9884\u671f\u5bff\u547d" = "life_exp"
            ), selected = "che_pc_usd2023"),
          shiny::selectInput(ns("y_var"), "Y \u8f74\u6307\u6807",
            choices = c(
              "\u9884\u671f\u5bff\u547d" = "life_exp",
              "\u4eba\u5747 CHE (USD)" = "che_pc_usd2023",
              "OOPS (%)" = "hf3_che",
              "GGHE-D (%)" = "gghed_che",
              "GDP/cap" = "gdp_pc_usd"
            ), selected = "life_exp"),
          shiny::selectInput(ns("color_by"), "\u7740\u8272",
            choices = c("\u5927\u6d32" = "continent",
                        "\u6536\u5165\u7ec4" = "income_group"),
            selected = "continent"),
          shiny::checkboxInput(ns("log_x"), "X \u8f74\u53d6\u5bf9\u6570", value = TRUE),
          mod_v3_sidebar_note(
            "\u76f8\u5173\u9605\u8bfb",
            "\u76f8\u5173\u7cfb\u6570\u4e3a Spearman \u79e9\u76f8\u5173\uff0c\u5bf9\u975e\u7ebf\u6027\u5173\u7cfb\u548c\u6781\u7aef\u503c\u66f4\u7a33\u5065\uff0c\u4f46\u4ecd\u7136\u4e0d\u4ee3\u8868\u56e0\u679c\u3002",
            bullets = c(
              "\u6563\u70b9\u56fe\u8bfb\u5f62\u72b6\uff0c\u70ed\u529b\u56fe\u8bfb\u603b\u4f53\u7ed3\u6784\u3002",
              "\u5206\u7ec4\u76f8\u5173\u5dee\u5f02\u53ef\u80fd\u63d0\u793a\u6536\u5165\u9636\u6bb5\u6216\u533a\u57df\u5236\u5ea6\u5dee\u522b\u3002",
              "\u6b8b\u5dee\u56fe\u9002\u5408\u627e\u540c\u7b49 X \u6c34\u5e73\u4e0b\u504f\u79bb Y \u9884\u671f\u7684\u56fd\u5bb6\u3002"
            )
          )
        ),
        shiny::uiOutput(ns("kpi_strip")),
        bslib::layout_columns(
          col_widths = c(7, 5),
          mod_card(
            kicker = "F1 · Pairwise shape",
            title = "\u53cc\u53d8\u91cf\u6563\u70b9\u56fe",
            htmltools::p(class = "card-note",
              "\u53ef\u5207\u6362 X/Y \u8f74\u6307\u6807\uff0c\u89c2\u5bdf\u4efb\u610f\u4e24\u4e2a\u53d8\u91cf\u7684\u5173\u8054\u5f62\u6001\u3002"),
            mod_v3_chart_guide(
              "\u76f8\u5173\u5f3a\u5ea6\u8981\u548c\u4e91\u56e2\u5f62\u72b6\u4e00\u8d77\u8bfb",
              "\u5355\u8c03\u4f46\u975e\u7ebf\u6027\u7684\u5173\u7cfb\u53ef\u80fd\u4f7f Pearson \u5931\u771f\uff0c\u6240\u4ee5\u9875\u9762\u7528 Spearman \u4f5c\u4e3a\u4e3b\u6307\u6807\u3002\u79bb\u7fa4\u70b9\u53ef\u80fd\u4ee3\u8868\u771f\u5b9e\u5236\u5ea6\u5dee\u5f02\uff0c\u4e5f\u53ef\u80fd\u662f\u6570\u636e\u53e3\u5f84\u95ee\u9898\u3002",
              bullets = c("X \u53d6\u5bf9\u6570\u9002\u5408 CHE/cap \u548c GDP/cap\u3002", "\u989c\u8272\u5206\u7ec4\u53ef\u4ee5\u5224\u65ad\u76f8\u5173\u662f\u7ec4\u95f4\u9a71\u52a8\u8fd8\u662f\u7ec4\u5185\u4ecd\u7136\u5b58\u5728\u3002")
            ),
            mod_spinner(plotly::plotlyOutput(ns("main_scatter"), height = 440))
          ),
          mod_card(
            kicker = "F2 · Correlation map",
            title = "\u76f8\u5173\u77e9\u9635\uff08\u5f53\u524d\u5e74\uff09",
            htmltools::p(class = "card-note",
              "6 \u4e2a\u6838\u5fc3\u6307\u6807\u7684 Spearman \u79e9\u76f8\u5173\u7cfb\u6570\u3002"),
            mod_v3_chart_guide(
              "\u77e9\u9635\u7528\u4e8e\u627e\u6307\u6807\u7ec4\u56e2",
              "\u6df1\u84dd\u8868\u793a\u6b63\u76f8\u5173\uff0c\u7ea2\u8272\u8868\u793a\u8d1f\u76f8\u5173\uff1b\u6210\u7247\u7684\u9ad8\u76f8\u5173\u533a\u5757\u8bf4\u660e\u8fd9\u4e9b\u6307\u6807\u53ef\u80fd\u5171\u540c\u53cd\u6620\u540c\u4e00\u7c7b\u7ed3\u6784\u3002",
              tone = "good"
            ),
            mod_spinner(plotly::plotlyOutput(ns("corr_heatmap"), height = 440)),
            footer = "\u77e9\u9635\u4f7f\u7528\u5f53\u524d\u9009\u5b9a\u5e74\u4efd\uff0c\u4e0e\u9876\u90e8 KPI \u4fdd\u6301\u540c\u4e00\u6a2a\u622a\u9762\u3002"
          )
        ),
        bslib::layout_columns(
          col_widths = c(6, 6),
          mod_card(
            kicker = "F3 · Stratified rho",
            title = "\u5206\u7ec4\u76f8\u5173\u7cfb\u6570\u5bf9\u6bd4",
            htmltools::p(class = "card-note",
              "\u540c\u4e00\u5bf9\u53d8\u91cf\u5728\u4e0d\u540c\u7ec4\u522b\u4e2d\u7684\u76f8\u5173\u5f3a\u5ea6\u5dee\u5f02\u3002"),
            mod_v3_chart_guide(
              "\u5206\u7ec4\u540e\u76f8\u5173\u53ef\u80fd\u53d8\u5f31",
              "\u5982\u679c\u5168\u6837\u672c\u76f8\u5173\u5f88\u5f3a\uff0c\u4f46\u5206\u7ec4\u540e\u660e\u663e\u53d8\u5f31\uff0c\u8bf4\u660e\u5b83\u53ef\u80fd\u4e3b\u8981\u7531\u6536\u5165\u6216\u533a\u57df\u5c42\u7ea7\u5dee\u5f02\u9a71\u52a8\u3002",
              tone = "warn"
            ),
            mod_spinner(plotly::plotlyOutput(ns("corr_by_group"), height = 380))
          ),
          mod_card(
            kicker = "F4 · Temporal stability",
            title = "\u76f8\u5173\u7cfb\u6570\u65f6\u5e8f\u6f14\u5316",
            htmltools::p(class = "card-note",
              "\u9009\u5b9a\u53d8\u91cf\u5bf9\u7684\u76f8\u5173\u7cfb\u6570\u968f\u65f6\u95f4\u53d8\u5316\u3002"),
            mod_v3_chart_guide(
              "\u7a33\u5b9a\u76f8\u5173\u6bd4\u5355\u5e74\u76f8\u5173\u66f4\u53ef\u4fe1",
              "\u957f\u671f\u4fdd\u6301\u540c\u5411\u7684\u03c1 \u8bf4\u660e\u5173\u8054\u7ed3\u6784\u7a33\u5b9a\uff1b\u5982\u679c\u67d0\u4e9b\u5e74\u4efd\u7a81\u7136\u7ffb\u8f6c\uff0c\u5e94\u56de\u5230\u6563\u70b9\u56fe\u68c0\u67e5\u6837\u672c\u548c\u6781\u7aef\u70b9\u3002"
            ),
            mod_spinner(plotly::plotlyOutput(ns("corr_trend"), height = 380))
          )
        ),
        bslib::layout_columns(
          col_widths = c(6, 6),
          mod_card(
            kicker = "F5 · Residual signal",
            title = "\u6b8b\u5dee\u5206\u5e03",
            htmltools::p(class = "card-note",
              "\u7ebf\u6027\u62df\u5408\u540e\u7684\u6b8b\u5dee\uff0c\u8bc6\u522b\u504f\u79bb\u9884\u671f\u7684\u56fd\u5bb6\u3002"),
            mod_v3_chart_guide(
              "\u6b8b\u5dee\u662f\u56fd\u5bb6\u4e2a\u6848\u5165\u53e3",
              "\u6b8b\u5dee\u660e\u663e\u4e3a\u6b63\u6216\u4e3a\u8d1f\u7684\u56fd\u5bb6\uff0c\u8868\u793a\u5728\u7ed9\u5b9a X \u6c34\u5e73\u4e0b Y \u9ad8\u4e8e\u6216\u4f4e\u4e8e\u7b80\u5355\u62df\u5408\u9884\u671f\uff0c\u53ef\u7ee7\u7eed\u8fdb\u5165\u56fd\u5bb6\u753b\u50cf\u590d\u6838\u3002",
              tone = "good"
            ),
            mod_spinner(plotly::plotlyOutput(ns("residual_plot"), height = 380))
          ),
          mod_card(
            kicker = "F6 · Coefficient table",
            title = "\u76f8\u5173\u7cfb\u6570\u8868",
            mod_v3_chart_guide(
              "\u8868\u683c\u4fdd\u7559\u53ef\u590d\u6838\u7684\u7cfb\u6570",
              "\u7528\u4e8e\u5feb\u901f\u6bd4\u8f83\u591a\u5bf9\u6307\u6807\u7684\u65b9\u5411\u548c\u5f3a\u5ea6\uff0c\u4e5f\u4fbf\u4e8e\u4e0e\u56fe\u4e2d\u7684\u4e3b\u8981\u53d1\u73b0\u5bf9\u7167\u3002"
            ),
            mod_spinner(reactable::reactableOutput(ns("corr_table"))),
            footer = "Spearman \u03c1 \u63cf\u8ff0\u5355\u8c03\u5173\u8054\uff1b\u975e\u7ebf\u6027\u3001\u6ede\u540e\u6548\u5e94\u548c\u6df7\u6742\u56e0\u7d20\u9700\u8981\u5355\u72ec\u5efa\u6a21\u3002"
          )
        )
      )
    )
  )
}

mod_correlation_server <- function(id, master_r) {
  shiny::moduleServer(id, function(input, output, session) {

    vars_for_corr <- c("che_pc_usd2023", "hf3_che", "gghed_che",
                       "ext_che", "life_exp", "gdp_pc_usd")
    var_labels <- c("CHE/cap", "OOPS%", "GGHED%", "EXT%", "Life exp", "GDP/cap")

    output$kpi_strip <- shiny::renderUI({
      m <- master_r(); yr <- input$year
      d <- m[m$year == yr & is.finite(m[[input$x_var]]) &
             is.finite(m[[input$y_var]]), ]
      rho <- tryCatch(
        stats::cor(d[[input$x_var]], d[[input$y_var]],
                   method = "spearman", use = "complete.obs"),
        error = function(e) NA_real_
      )
      mod_v3_kpi_grid(
        mod_v3_kpi(format(nrow(d), big.mark = ","),
                   "\u56fd\u5bb6\u6570", tone = "primary"),
        mod_v3_kpi(sprintf("%.3f", rho %||% 0),
                   "Spearman \u03c1", tone = if (!is.na(rho) && abs(rho) > 0.5) "good" else "neutral"),
        mod_v3_kpi(as.character(input$year), "\u5e74\u4efd", tone = "neutral"),
        mod_v3_kpi(sprintf("%.3f", rho^2 %||% 0),
                   "\u03c1\u00b2", tone = "secondary")
      )
    })

    output$main_scatter <- plotly::renderPlotly({
      m <- master_r(); yr <- input$year
      d <- m[m$year == yr & is.finite(m[[input$x_var]]) &
             is.finite(m[[input$y_var]]) & !is.na(m[[input$color_by]]), ]
      shiny::req(nrow(d) > 5)
      pal <- if (input$color_by == "continent") unname(brand_palette$continent)
             else unname(brand_palette$income)
      safe_plotly({
        plotly::plot_ly(d, x = stats::as.formula(paste0("~", input$x_var)),
                        y = stats::as.formula(paste0("~", input$y_var)),
                        color = stats::as.formula(paste0("~", input$color_by)),
                        text = ~country_name, type = "scatter", mode = "markers",
                        colors = pal,
                        marker = list(size = 9, opacity = 0.7)) |>
          ghs_plotly_layout() |>
          plotly::layout(
            xaxis = list(title = input$x_var,
                         type = if (input$log_x) "log" else "linear"),
            yaxis = list(title = input$y_var),
            legend = list(orientation = "h", y = -0.15)
          )
      })
    })

    output$corr_heatmap <- plotly::renderPlotly({
      m <- master_r(); yr <- input$year
      d <- m[m$year == yr, vars_for_corr, drop = FALSE]
      d <- d[stats::complete.cases(d), ]
      shiny::req(nrow(d) > 10)
      cor_mat <- stats::cor(d, method = "spearman", use = "complete.obs")
      safe_plotly({
        plotly::plot_ly(
          x = var_labels, y = var_labels,
          z = round(cor_mat, 2), type = "heatmap",
          colorscale = list(c(0, "#a23b3b"), c(0.5, "#ffffff"), c(1, "#1d3f5f")),
          zmin = -1, zmax = 1,
          text = round(cor_mat, 2), texttemplate = "%{text}",
          showscale = TRUE
        ) |>
          ghs_plotly_layout() |>
          plotly::layout(
            xaxis = list(title = ""),
            yaxis = list(title = "", autorange = "reversed")
          )
      })
    })

    output$corr_by_group <- plotly::renderPlotly({
      m <- master_r(); yr <- input$year
      grp <- input$color_by
      d <- m[m$year == yr & is.finite(m[[input$x_var]]) &
             is.finite(m[[input$y_var]]) & !is.na(m[[grp]]), ]
      groups <- unique(d[[grp]])
      results <- lapply(groups, function(g) {
        sub <- d[d[[grp]] == g, ]
        if (nrow(sub) < 5) return(NULL)
        rho <- stats::cor(sub[[input$x_var]], sub[[input$y_var]],
                          method = "spearman", use = "complete.obs")
        data.frame(group = g, rho = rho, n = nrow(sub))
      })
      df <- do.call(rbind, results)
      shiny::req(nrow(df) > 1)
      df <- df[order(-df$rho), ]
      safe_plotly({
        plotly::plot_ly(df, x = ~rho, y = ~reorder(group, rho),
                        type = "bar", orientation = "h",
                        marker = list(color = ifelse(df$rho > 0, "#1d3f5f", "#a23b3b")),
                        text = ~sprintf("\u03c1=%.3f (n=%d)", rho, n),
                        textposition = "outside") |>
          ghs_plotly_layout() |>
          plotly::layout(
            xaxis = list(title = "Spearman \u03c1", range = c(-1, 1)),
            yaxis = list(title = "")
          )
      })
    })

    output$corr_trend <- plotly::renderPlotly({
      m <- master_r()
      years <- sort(unique(m$year))
      rhos <- vapply(years, function(yr) {
        d <- m[m$year == yr & is.finite(m[[input$x_var]]) &
               is.finite(m[[input$y_var]]), ]
        if (nrow(d) < 10) return(NA_real_)
        stats::cor(d[[input$x_var]], d[[input$y_var]],
                   method = "spearman", use = "complete.obs")
      }, numeric(1))
      df <- data.frame(year = years, rho = rhos)
      df <- df[is.finite(df$rho), ]
      shiny::req(nrow(df) > 3)
      safe_plotly({
        plotly::plot_ly(df, x = ~year, y = ~rho,
                        type = "scatter", mode = "lines+markers",
                        line = list(color = "#1d3f5f", width = 3),
                        marker = list(color = "#1d3f5f", size = 6)) |>
          ghs_plotly_layout() |>
          plotly::layout(
            xaxis = list(title = ""),
            yaxis = list(title = "Spearman \u03c1", range = c(-1, 1)),
            shapes = list(
              list(type = "line", x0 = min(df$year), x1 = max(df$year),
                   y0 = 0, y1 = 0,
                   line = list(color = "#5d667a", width = 1, dash = "dash"))
            )
          )
      })
    })

    output$residual_plot <- plotly::renderPlotly({
      m <- master_r(); yr <- input$year
      d <- m[m$year == yr & is.finite(m[[input$x_var]]) &
             is.finite(m[[input$y_var]]) & !is.na(m$country_name), ]
      shiny::req(nrow(d) > 10)
      fit <- stats::lm(
        stats::as.formula(paste(input$y_var, "~", input$x_var)),
        data = d
      )
      d$resid <- stats::residuals(fit)
      d <- d[order(-abs(d$resid)), ]
      top_resid <- utils::head(d, 15)
      top_resid$country_name <- factor(top_resid$country_name,
                                        levels = rev(top_resid$country_name))
      safe_plotly({
        plotly::plot_ly(top_resid, x = ~resid, y = ~country_name,
                        type = "bar", orientation = "h",
                        marker = list(
                          color = ifelse(top_resid$resid > 0, "#2a857a", "#a23b3b")
                        )) |>
          ghs_plotly_layout() |>
          plotly::layout(
            xaxis = list(title = "\u6b8b\u5dee"),
            yaxis = list(title = ""),
            margin = list(l = 120)
          )
      })
    })

    output$corr_table <- reactable::renderReactable({
      m <- master_r(); yr <- input$year
      d <- m[m$year == yr, vars_for_corr, drop = FALSE]
      d <- d[stats::complete.cases(d), ]
      cor_mat <- stats::cor(d, method = "spearman", use = "complete.obs")
      tab <- as.data.frame(round(cor_mat, 3))
      tab <- cbind(data.frame("\u6307\u6807" = var_labels, check.names = FALSE), tab)
      names(tab)[-1] <- var_labels
      reactable::reactable(tab, pagination = FALSE, highlight = TRUE,
        defaultColDef = reactable::colDef(
          headerStyle = list(background = "#f1f3f7", fontWeight = 700),
          style = function(value) {
            if (!is.numeric(value)) return(list())
            color <- if (value > 0.5) "#1d3f5f"
                     else if (value < -0.5) "#a23b3b"
                     else "#5d667a"
            list(color = color, fontWeight = if (abs(value) > 0.7) 700 else 400)
          }
        ))
    })
  })
}
