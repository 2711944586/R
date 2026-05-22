
mod_cluster_ui <- function(id, year_min, year_max) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = "\u805a\u7c7b Cluster",
    value = "cluster",
    mod_v3_hero(
      kicker = "\u7c7b\u578b\u53d1\u73b0",
      title = "PCA + k-means\uff1a\u56fd\u5bb6\u7b79\u8d44\u8c61\u9650\u548c\u5236\u5ea6\u539f\u578b",
      lead = paste(
        "\u4ee5 GGHE-D\u3001PVT-D\u3001OOPS \u548c EXT \u56db\u4e2a\u7b79\u8d44\u7ef4\u5ea6\u6784\u5efa\u622a\u9762\u7279\u5f81\uff0c",
        "\u5148\u7528 PCA \u538b\u7f29\u6210\u4e3b\u6210\u5206\u7a7a\u95f4\uff0c\u518d\u5728\u4e3b\u6210\u5206\u4e0a\u8fd0\u884c k-means\u3002",
        "\u9875\u9762\u9002\u5408\u89c2\u5bdf\u5404\u5e74\u4efd\u56fd\u5bb6\u7b79\u8d44\u7ed3\u6784\u662f\u5426\u5448\u73b0\u516c\u5171\u4e3b\u5bfc\u3001\u79c1\u4eba\u4ed8\u8d39\u6216\u5916\u63f4\u4f9d\u8d56\u7b49\u539f\u578b\u3002"
      ),
      meta = list("GGHE-D / PVT-D / OOPS / EXT", "PCA scores", "k-means clusters", "Yearly snapshots")
    ),
    mod_v3_page_body(
      wide = TRUE,
      mod_v3_badge_row(
        "\u56db\u7ef4\u7b79\u8d44\u7279\u5f81",
        "\u4e3b\u6210\u5206\u964d\u7ef4",
        "K \u503c\u53ef\u8c03",
        "\u805a\u7c7b\u4e2d\u5fc3\u53ef\u89e3\u91ca",
        tone = "secondary"
      ),
      mod_v3_story_grid(
        columns = 3,
        mod_v3_insight(
          kicker = "Feature space",
          title = "\u7279\u5f81\u53cd\u6620\u8c01\u5728\u4ed8\u8d39",
          text = "GGHE-D \u4ee3\u8868\u516c\u5171\u7b79\u8d44\uff0cPVT-D \u548c OOPS \u53cd\u6620\u79c1\u4eba\u548c\u73b0\u91d1\u538b\u529b\uff0cEXT \u7528\u4e8e\u8bc6\u522b\u5916\u63f4\u4f9d\u8d56\u3002",
          tone = "primary"
        ),
        mod_v3_insight(
          kicker = "PCA",
          title = "\u4e3b\u6210\u5206\u628a\u591a\u7ef4\u7ed3\u6784\u6295\u5230\u5e73\u9762",
          text = "\u524d\u4e24\u4e2a PC \u901a\u5e38\u6355\u6349\u6700\u4e3b\u8981\u7684\u7b79\u8d44\u5dee\u5f02\uff0c\u6563\u70b9\u56fe\u7528\u4e8e\u67e5\u770b\u56fd\u5bb6\u5728\u7ed3\u6784\u7a7a\u95f4\u4e2d\u7684\u4f4d\u7f6e\u3002",
          tone = "secondary"
        ),
        mod_v3_insight(
          kicker = "Cluster",
          title = "K-means \u5e2e\u52a9\u627e\u5236\u5ea6\u539f\u578b",
          text = "\u8c03\u6574 K \u503c\u53ef\u89c2\u5bdf\u7ec4\u522b\u662f\u5426\u7a33\u5b9a\uff0c\u4e2d\u5fc3\u67f1\u56fe\u7528\u4e8e\u7406\u89e3\u6bcf\u7c7b\u7684 PC \u7279\u5f81\u3002",
          tone = "good"
        )
      ),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 292,
          shiny::sliderInput(ns("year"), "\u5e74\u4efd",
                              min = year_min, max = year_max, value = year_max,
                              step = 1, sep = "", animate = TRUE),
          shiny::sliderInput(ns("k"), "\u805a\u7c7b\u6570 K",
                              min = 2, max = 8, value = 4, step = 1),
          mod_v3_sidebar_note(
            "\u53c2\u6570\u68c0\u67e5",
            "\u805a\u7c7b\u662f\u63a2\u7d22\u6027\u5de5\u5177\uff1aK \u503c\u8fc7\u5c0f\u4f1a\u5408\u5e76\u4e0d\u540c\u5236\u5ea6\u7c7b\u578b\uff0cK \u503c\u8fc7\u5927\u4f1a\u4ea7\u751f\u96be\u4ee5\u89e3\u91ca\u7684\u5c0f\u7c7b\u3002",
            bullets = c("\u5efa\u8bae\u5728 K=3-5 \u95f4\u5bf9\u6bd4", "\u52a8\u753b\u5e74\u4efd\u53ef\u89c2\u5bdf\u7ed3\u6784\u6f14\u5316", "\u805a\u7c7b\u4e0d\u662f\u8d28\u91cf\u6392\u540d")
          )
        ),
        shiny::uiOutput(ns("kpi_strip")),
        mod_v3_rail(list(
          list(title = "\u53d6\u622a\u9762", text = "\u4ec5\u4f7f\u7528\u6240\u9009\u5e74\u4efd\u7684\u56fd\u5bb6\u89c2\u6d4b\u3002"),
          list(title = "\u6807\u51c6\u5316", text = "PCA \u5185\u90e8\u5bf9\u56db\u4e2a\u7279\u5f81\u505a\u4e2d\u5fc3\u5316\u548c\u6807\u51c6\u5316\u3002"),
          list(title = "\u6295\u5f71", text = "\u524d\u4e24\u4e2a PC \u4f5c\u4e3a\u6563\u70b9\u5750\u6807\u3002"),
          list(title = "\u805a\u7c7b", text = "k-means \u5728\u4e3b\u6210\u5206\u7a7a\u95f4\u4e2d\u5212\u5206\u56fd\u5bb6\u539f\u578b\u3002")
        )),
        mod_card(
          kicker = "\u4e3b\u6210\u5206\u7a7a\u95f4",
          title = "PCA + K-means \u6563\u70b9",
          mod_v3_chart_guide(
            "\u8bfb\u56fe\u65b9\u6cd5",
            "\u70b9\u7684\u4f4d\u7f6e\u8868\u793a\u56fd\u5bb6\u5728\u7b79\u8d44\u7ed3\u6784\u4e3b\u6210\u5206\u7a7a\u95f4\u4e2d\u7684\u5750\u6807\uff0c\u989c\u8272\u8868\u793a k-means \u805a\u7c7b\u3002\u8ddd\u79bb\u8fd1\u7684\u56fd\u5bb6\u5728\u56db\u4e2a\u7b79\u8d44\u6307\u6807\u4e0a\u66f4\u76f8\u4f3c\u3002",
            bullets = c("\u60ac\u505c\u53ef\u67e5\u770b ISO3", "\u6587\u672c\u6807\u7b7e\u4fbf\u4e8e\u5b9a\u4f4d\u5f02\u5e38\u70b9", "\u989c\u8272\u7ec4\u4e0d\u5e26\u6709\u4f18\u52a3\u542b\u4e49")
          ),
          mod_spinner(plotly::plotlyOutput(ns("pca_scatter"), height = 540))
        ),
        bslib::layout_columns(
          col_widths = c(6, 6),
          mod_card(
            kicker = "\u805a\u7c7b\u4e2d\u5fc3",
            title = "\u5404\u805a\u7c7b\u4e2d\u5fc3 PC \u8d1f\u8377",
            mod_v3_chart_guide(
              "\u4e2d\u5fc3\u542b\u4e49",
              "\u67f1\u56fe\u5c55\u793a\u6bcf\u4e2a\u805a\u7c7b\u4e2d\u5fc3\u5728\u4e3b\u6210\u5206\u4e0a\u7684\u5750\u6807\uff0c\u7528\u4e8e\u5224\u65ad\u7ec4\u95f4\u4e3b\u8981\u5dee\u5f02\u6765\u81ea\u54ea\u4e2a\u65b9\u5411\u3002",
              tone = "good"
            ),
            mod_spinner(plotly::plotlyOutput(ns("centroid_bar"), height = 360)),
            footer = "\u7279\u5f81\u4e3a GGHE-D\u3001PVT-D\u3001EXT\u3001OOPS \u5360 CHE \u6bd4\u4f8b\uff1b\u805a\u7c7b\u662f\u63a2\u7d22\u6027\u5206\u7ec4\uff0c\u4e0d\u5e94\u5355\u72ec\u4f5c\u4e3a\u653f\u7b56\u5224\u65ad\u3002"
          ),
          mod_card(
            kicker = "\u4e3b\u6210\u5206\u8bca\u65ad",
            title = "PCA \u8f7d\u8377\u4e0e\u89e3\u91ca\u65b9\u5dee",
            mod_v3_chart_guide(
              "\u5982\u4f55\u89e3\u91ca PC",
              "\u8f7d\u8377\u7edd\u5bf9\u503c\u8d8a\u5927\uff0c\u8bf4\u660e\u8be5\u7b79\u8d44\u7279\u5f81\u5bf9\u5bf9\u5e94\u4e3b\u6210\u5206\u8d21\u732e\u8d8a\u5927\uff1b\u89e3\u91ca\u65b9\u5dee\u7528\u4e8e\u5224\u65ad\u524d\u4e24\u4e2a PC \u662f\u5426\u8db3\u4ee5\u5448\u73b0\u4e3b\u8981\u7ed3\u6784\u3002",
              tone = "warn"
            ),
            mod_spinner(reactable::reactableOutput(ns("loading_table"))),
            footer = "\u8f7d\u8377\u4ec5\u7528\u4e8e\u7406\u89e3\u4e3b\u6210\u5206\u65b9\u5411\uff0c\u6b63\u8d1f\u53f7\u53d6\u51b3\u4e8e PCA \u7b26\u53f7\u7ea6\u5b9a\u3002"
          )
        ),
        mod_card(
          kicker = "\u805a\u7c7b\u6e05\u5355",
          title = "\u805a\u7c7b\u89c4\u6a21\u4e0e\u4ee3\u8868\u56fd\u5bb6",
          mod_v3_chart_guide(
            "\u8868\u683c\u7528\u9014",
            "\u6bcf\u884c\u8868\u793a\u4e00\u4e2a\u805a\u7c7b\uff0c\u540c\u65f6\u7ed9\u51fa\u89c4\u6a21\u3001\u4efd\u989d\u548c\u524d\u51e0\u4e2a\u4ee3\u8868\u56fd\u5bb6\uff0c\u4fbf\u4e8e\u628a\u989c\u8272\u7ec4\u8fd8\u539f\u6210\u53ef\u8bfb\u7684\u5236\u5ea6\u539f\u578b\u3002"
          ),
          mod_spinner(reactable::reactableOutput(ns("cluster_table")))
        )
      )
    )
  )
}

mod_cluster_server <- function(id, master_r) {
  shiny::moduleServer(id, function(input, output, session) {
    cluster_obj <- shiny::reactive({
      shiny::req(input$year, input$k)
      m <- master_r()
      snap <- m[m$year == input$year, , drop = FALSE]
      need <- c("gghed_che", "pvtd_che", "ext_che", "hf3_che")
      snap <- snap[stats::complete.cases(snap[, need, drop = FALSE]), , drop = FALSE]
      shiny::req(nrow(snap) > input$k)
      pca <- fit_pca(snap, vars = need)
      cl <- fit_cluster(pca, k = input$k)
      if (!is.null(cl)) {
        cl$loadings <- pca$loadings
        cl$var_explained <- pca$var_explained
      }
      cl
    })

    output$kpi_strip <- shiny::renderUI({
      cl <- cluster_obj()
      shiny::req(cl)
      d <- cl$scores
      cluster_sizes <- sort(table(d$cluster), decreasing = TRUE)
      largest <- if (length(cluster_sizes)) as.integer(cluster_sizes[[1]]) else NA_integer_
      pc_cols <- grep("^PC", names(d), value = TRUE)
      mod_v3_kpi_grid(
        mod_v3_kpi("\u5e74\u4efd", as.character(input$year),
                   hint = "\u5f53\u524d\u805a\u7c7b\u622a\u9762",
                   tone = "neutral"),
        mod_v3_kpi("\u53ef\u6bd4\u56fd\u5bb6", fmt_v3_num(nrow(d)),
                   hint = "\u56db\u4e2a\u7b79\u8d44\u7279\u5f81\u5b8c\u6574\u7684\u56fd\u5bb6\u6570",
                   tone = "primary"),
        mod_v3_kpi("\u805a\u7c7b\u6570 K", as.character(input$k),
                   hint = "\u5f53\u524d k-means \u4e2d\u5fc3\u6570",
                   tone = "secondary"),
        mod_v3_kpi("\u6700\u5927\u7c07\u89c4\u6a21", fmt_v3_num(largest),
                   hint = sprintf("\u4e3b\u6210\u5206\u6570\uff1a%d", length(pc_cols)),
                   tone = "good")
      )
    })

    output$pca_scatter <- plotly::renderPlotly({
      cl <- cluster_obj(); shiny::req(cl)
      d <- cl$scores
      plotly::plot_ly(d, x = ~PC1, y = ~PC2,
                       color = ~cluster, text = ~iso3_code,
                       hovertemplate = "<b>%{text}</b><br>PC1=%{x:.2f}<br>PC2=%{y:.2f}<extra></extra>",
                       type = "scatter", mode = "markers+text",
                       textposition = "top center",
                       marker = list(size = 9, opacity = 0.85)) |>
        ghs_plotly_layout() |>
        plotly::layout(title = sprintf("PCA + K-means\uff08K=%d\uff0c%d \u5e74\uff09",
                                        input$k, input$year),
                       xaxis = list(title = "PC1"),
                       yaxis = list(title = "PC2"),
                       legend = list(orientation = "h", y = -0.15)) |>
        plotly::config(displaylogo = FALSE)
    })

    output$centroid_bar <- plotly::renderPlotly({
      cl <- cluster_obj(); shiny::req(cl)
      centroids <- as.data.frame(cl$km$centers)
      centroids$cluster <- rownames(centroids)
      centroids <- tidyr::pivot_longer(centroids, -"cluster",
                                        names_to = "PC", values_to = "value")
      plotly::plot_ly(centroids, x = ~PC, y = ~value, color = ~cluster,
                       type = "bar") |>
        ghs_plotly_layout() |>
        plotly::layout(title = "\u5404\u805a\u7c7b\u4e2d\u5fc3\u4e3b\u6210\u5206\u5750\u6807",
                       barmode = "group",
                       xaxis = list(title = ""),
                       yaxis = list(title = "\u4e2d\u5fc3\u5750\u6807"),
                       legend = list(orientation = "h", y = -0.15)) |>
        plotly::config(displaylogo = FALSE)
    })

    output$loading_table <- reactable::renderReactable({
      cl <- cluster_obj(); shiny::req(cl)
      loadings <- cl$loadings
      pc_cols <- intersect(c("PC1", "PC2", "PC3", "PC4"), names(loadings))
      tab <- loadings[, c("variable", pc_cols), drop = FALSE]
      names(tab)[1] <- "\u539f\u59cb\u7279\u5f81"
      for (pc in pc_cols) tab[[pc]] <- round(tab[[pc]], 3)
      ve <- cl$var_explained[pc_cols]
      ve_row <- as.data.frame(as.list(round(as.numeric(ve) * 100, 1)))
      names(ve_row) <- pc_cols
      ve_row <- data.frame(
        "\u539f\u59cb\u7279\u5f81" = "\u89e3\u91ca\u65b9\u5dee(%)",
        ve_row,
        check.names = FALSE
      )
      tab <- rbind(tab, ve_row)
      reactable::reactable(tab, pagination = FALSE, highlight = TRUE,
        defaultColDef = reactable::colDef(headerStyle = list(background = "#f1f3f7")),
        columns = list(
          "\u539f\u59cb\u7279\u5f81" = reactable::colDef(minWidth = 140,
            style = list(fontFamily = "'JetBrains Mono', monospace", fontWeight = 700))
        ))
    })

    output$cluster_table <- reactable::renderReactable({
      cl <- cluster_obj(); shiny::req(cl)
      d <- cl$scores
      parts <- split(d, d$cluster)
      tab <- do.call(rbind, lapply(parts, function(ch) {
        reps <- paste(utils::head(ch$iso3_code[order(ch$PC1)], 6), collapse = " / ")
        data.frame(
          "\u805a\u7c7b" = as.character(ch$cluster[1]),
          "\u56fd\u5bb6\u6570" = nrow(ch),
          "\u6837\u672c\u5360\u6bd4" = sprintf("%.1f%%", nrow(ch) / nrow(d) * 100),
          "PC1 \u4e2d\u4f4d\u6570" = round(stats::median(ch$PC1, na.rm = TRUE), 2),
          "PC2 \u4e2d\u4f4d\u6570" = round(stats::median(ch$PC2, na.rm = TRUE), 2),
          "\u4ee3\u8868 ISO3" = reps,
          check.names = FALSE
        )
      }))
      reactable::reactable(tab, pagination = FALSE, highlight = TRUE,
        defaultColDef = reactable::colDef(headerStyle = list(background = "#f1f3f7")),
        columns = list(
          "\u56fd\u5bb6\u6570" = reactable::colDef(align = "right"),
          "PC1 \u4e2d\u4f4d\u6570" = reactable::colDef(align = "right"),
          "PC2 \u4e2d\u4f4d\u6570" = reactable::colDef(align = "right"),
          "\u4ee3\u8868 ISO3" = reactable::colDef(minWidth = 240,
            style = list(fontFamily = "'JetBrains Mono', monospace"))
        ))
    })
  })
}
