options(warn = -1)
for (f in list.files("程序", pattern = "[.]R$", full.names = TRUE)) {
  suppressMessages(suppressWarnings(source(f, encoding = "UTF-8")))
}
m  <- readRDS("派生数据/处理结果/master_enriched.rds")
register_brand_fonts()
ggplot2::theme_set(theme_ghs3())

td <- file.path(tempdir(), "shocks_smoke")
dir.create(td, showWarnings = FALSE, recursive = TRUE)

samples <- list(
  shk_covid_chepc          = plot_shock_covid_chepc(m),
  shk_covid_oop            = plot_shock_covid_oop(m),
  shk_covid_dot_income     = plot_shock_covid_dot_income(m),
  shk_covid_lifeexp        = plot_shock_covid_lifeexp(m),
  shk_covid_track          = plot_shock_covid_track(m),
  shk_gfc_chepc            = plot_shock_gfc_chepc(m),
  shk_gfc_oop              = plot_shock_gfc_oop(m),
  shk_gfc_trend_gap        = plot_shock_gfc_trend_gap(m),
  shk_gfc_recovery_years   = plot_shock_gfc_recovery_years(m),
  shk_gfc_sensitivity      = plot_shock_gfc_sensitivity(m),
  shk_seg_usa_2008         = plot_shock_segmented_chepc(m, "USA", 2008),
  shk_seg_chn_2019         = plot_shock_segmented_chepc(m, "CHN", 2019),
  shk_seg_multi            = plot_shock_segmented_multi(m),
  shk_seg_dot              = plot_shock_segmented_dot(m),
  shk_pre_post_slope       = plot_shock_pre_post_slope(m),
  shk_segment_fit_chn      = plot_shock_segment_fit(m, "CHN"),
  shk_global_trend         = plot_shock_global_trend(m),
  shk_growth_rate_box      = plot_shock_growth_rate_box(m),
  shk_volatility_env       = plot_shock_volatility_envelope(m),
  shk_anomaly_heat         = plot_shock_anomaly_heat(m),
  shk_freq_rank            = plot_shock_freq_rank(m),
  shk_recovery_2022        = plot_shock_recovery_2022(m),
  shk_recovery_scatter     = plot_shock_recovery_scatter(m),
  shk_catchup              = plot_shock_catchup(m),
  shk_residual_2022        = plot_shock_residual_2022(m),
  shk_recovery_dashboard   = plot_shock_recovery_dashboard(m)
)

ok_n <- 0; fail_n <- 0
for (n in names(samples)) {
  p <- samples[[n]]
  if (is.null(p)) { cat(sprintf("  %-26s NULL\n", n)); fail_n <- fail_n + 1; next }
  res <- tryCatch({
    ggplot2::ggsave(file.path(td, paste0(n, ".png")), p,
                    width = 10, height = 6, dpi = 130, bg = brand_palette$paper)
    "OK"
  }, error = function(e) conditionMessage(e))
  if (res == "OK") ok_n <- ok_n + 1 else fail_n <- fail_n + 1
  cat(sprintf("  %-26s %s\n", n, res))
}
cat(sprintf("\n=> %d ok / %d fail\n", ok_n, fail_n))
cat("dir:", td, "\n")
total_kb <- sum(file.info(list.files(td, full.names = TRUE))$size, na.rm = TRUE) / 1024
cat(sprintf("=> %.1f KB total\n", total_kb))
