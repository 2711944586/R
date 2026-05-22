
cat("[build_v2] start\n")
t0 <- Sys.time()

for (f in list.files("程序", pattern = "\\.R$", full.names = TRUE)) {
  suppressWarnings(suppressMessages(source(f, encoding = "UTF-8")))
}
cat("[build_v2] all 程序/ sourced\n")

suppressWarnings(suppressMessages({
  register_brand_fonts()
  ggplot2::theme_set(theme_ghs2())
}))
cat("[build] theme set + fonts registered\n")

cache <- file.path("派生数据", "处理结果", "master_enriched.rds")
if (file.exists(cache)) {
  master <- readRDS(cache)
  cat("[build_v2] master loaded:", nrow(master), "rows\n")
} else {
  ghs <- load_ghs()
  master <- enrich_master(build_master_wide(ghs), with_wdi = TRUE)
  dir.create(dirname(cache), recursive = TRUE, showWarnings = FALSE)
  saveRDS(master, cache)
  cat("[build_v2] master built:", nrow(master), "rows\n")
}

world_sf <- tryCatch(load_world_sf("medium", 0.08), error = function(e) NULL)
cat("[build_v2] world_sf:",
    if (is.null(world_sf)) "NULL" else paste(nrow(world_sf), "polygons"), "\n")

cat("\n========== thematic figures ==========\n")
n_thematic <- tryCatch(ghs_export_v2_thematic(master),
                       error = function(e) { message(e); 0L })

cat("\n========== dataviz figures ==========\n")
n_dataviz <- tryCatch(ghs_export_v2_dataviz(master, world_sf = world_sf),
                      error = function(e) { message(e); 0L })

cat("\n========== plotly widgets ==========\n")
n_plotly <- tryCatch(ghs_export_v2_widgets_plotly(master),
                     error = function(e) { message(e); 0L })

cat("\n========== other widgets ==========\n")
n_other <- tryCatch(ghs_export_v2_widgets_other(master, world_sf = world_sf),
                    error = function(e) { message(e); 0L })

cat("\n========== data quality report ==========\n")
data_quality_export(master)

elapsed <- difftime(Sys.time(), t0, units = "secs")
cat(sprintf("\n[build_v2] DONE in %.1fs\n", elapsed))
cat(sprintf("  thematic figures: %d / 8\n", n_thematic))
cat(sprintf("  dataviz figures:  %d / 6\n", n_dataviz))
cat(sprintf("  plotly widgets:   %d / 8\n", n_plotly))
cat(sprintf("  other widgets:    %d / 5\n", n_other))

fig_n <- length(list.files("分析输出/图表", pattern = "\\.png$"))
svg_n <- length(list.files("分析输出/图表", pattern = "\\.svg$"))
wid_n <- length(list.files("分析输出/交互组件", pattern = "\\.html$",
                           recursive = TRUE))
tab_n <- length(list.files("分析输出/模型表", pattern = "\\.csv$"))

cat(sprintf("\n[build_v2] 分析输出/ status:\n"))
cat(sprintf("  figures PNG: %d\n", fig_n))
cat(sprintf("  figures SVG: %d\n", svg_n))
cat(sprintf("  widgets HTML: %d\n", wid_n))
cat(sprintf("  tables CSV: %d\n", tab_n))
