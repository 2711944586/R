# =============================================================================
# 仪表盘/模块/mod_cluster.R
# Tab 8 · 聚类：PCA + k-means + 聚类中心 bar
# =============================================================================

mod_cluster_ui <- function(id, year_min, year_max) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = htmltools::HTML("&#128202; \u805a\u7c7b Cluster"),
    htmltools::div(
      class = "panel-hero",
      htmltools::h2("PCA + k-means \u00b7 \u56fd\u5bb6\u8c61\u9650"),
      htmltools::p(class = "text-muted",
                   paste("\u4ee5 financing 4 \u7ef4\uff08GGHE-D / PVT-D / OOPS / EXT\uff09\u505a\u4e3b\u6210\u5206\u5206\u89e3\uff0c",
                         "\u7136\u540e\u5728\u524d 2 \u4e2a\u4e3b\u6210\u5206\u4e0a\u8dd1 k-means\u3002",
                         "\u53ef\u8c03 k\u3001\u5e74\u4efd\u4e0e\u662f\u5426\u6807\u51c6\u5316\uff0c\u89c2\u5bdf\u8c61\u9650\u6f02\u79fb\u3002"))
    ),
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 280,
        shiny::sliderInput(ns("year"), "年份",
                            min = year_min, max = year_max, value = year_max,
                            step = 1, sep = "", animate = TRUE),
        shiny::sliderInput(ns("k"), "聚类数 K",
                            min = 2, max = 8, value = 4, step = 1),
        shiny::helpText("不同年份与 K 值揭示卫生筹资模式的演化。")
      ),
      mod_card(
        title = "PCA + K-means 散点",
        mod_spinner(plotly::plotlyOutput(ns("pca_scatter"), height = 520))
      ),
      mod_card(
        title = "各聚类中心 PC 负荷",
        mod_spinner(plotly::plotlyOutput(ns("centroid_bar"), height = 280))
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
      fit_cluster(pca, k = input$k)
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
        plotly::layout(title = sprintf("PCA + K-means (k=%d, year=%d)",
                                        input$k, input$year),
                       xaxis = list(title = "PC1"),
                       yaxis = list(title = "PC2")) |>
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
        plotly::layout(title = "各聚类中心主成分负荷", barmode = "group") |>
        plotly::config(displaylogo = FALSE)
    })
  })
}
