options(warn = -1)
for (f in list.files("程序", pattern = "[.]R$", full.names = TRUE)) {
  suppressMessages(suppressWarnings(source(f, encoding = "UTF-8")))
}
m <- readRDS("派生数据/处理结果/master_enriched.rds")
register_brand_fonts()
ggplot2::theme_set(theme_ghs3())

td <- file.path(tempdir(), "adv_smoke")
dir.create(td, showWarnings = FALSE, recursive = TRUE)

samples <- list(
  adv_ridge_oops       = plot_adv_ridge_oops_by_income(m),
  adv_ridge_che_pc     = plot_adv_ridge_che_pc_evolution(m),
  adv_beeswarm_che     = plot_adv_beeswarm_che_pc(m),
  adv_beeswarm_oops    = plot_adv_beeswarm_oops(m),
  adv_stream_global    = plot_adv_stream_global_sources(m),
  adv_stream_continent = plot_adv_stream_continent(m),
  adv_bump_top25       = plot_adv_bump_top25(m, n = 20),
  adv_slope_panel      = plot_adv_slope_smallmultiples(m, top_n = 8),
  adv_lollipop         = plot_adv_lollipop_che_change(m, n = 12),
  adv_treemap          = plot_adv_treemap_continent_che(m),
  adv_parallel         = plot_adv_parallel_finance(m),
  adv_marimekko        = plot_adv_marimekko_finance(m),
  adv_radial_usa       = plot_adv_radial_hc_purpose(m, "USA"),
  adv_radial_chn       = plot_adv_radial_hc_purpose(m, "CHN"),
  adv_calendar         = plot_adv_calendar_growth(m, top_n = 30),
  adv_density2d        = plot_adv_density_2d_finance(m),
  adv_che_pc_continent = plot_adv_che_pc_by_continent(m),
  adv_sources_income   = plot_adv_sources_by_income(m),
  adv_topbot_che       = plot_adv_topbot_dotplot(m, "che_pc_usd2023", n = 12),
  adv_topbot_life      = plot_adv_topbot_dotplot(m, "life_exp", n = 12)
)

ok_n <- 0
fail_n <- 0
for (n in names(samples)) {
  p <- samples[[n]]
  if (is.null(p)) {
    cat(sprintf("  %-30s NULL\n", n)); fail_n <- fail_n + 1
    next
  }
  res <- tryCatch({
    ggplot2::ggsave(file.path(td, paste0(n, ".png")), p,
                    width = 10, height = 6, dpi = 130,
                    bg = brand_palette$paper)
    "OK"
  }, error = function(e) conditionMessage(e))
  if (res == "OK") ok_n <- ok_n + 1 else fail_n <- fail_n + 1
  cat(sprintf("  %-30s %s\n", n, res))
}
cat(sprintf("\n=> %d ok / %d fail\n", ok_n, fail_n))
cat("dir:", td, "\n")
files <- list.files(td, full.names = TRUE)
total_kb <- sum(file.info(files)$size) / 1024
cat(sprintf("=> %d files, %.1f KB total\n", length(files), total_kb))
