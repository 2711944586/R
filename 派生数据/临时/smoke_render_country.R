options(warn = -1)
for (f in list.files("程序", pattern = "[.]R$", full.names = TRUE)) {
  suppressMessages(suppressWarnings(source(f, encoding = "UTF-8")))
}
m  <- readRDS("派生数据/处理结果/master_enriched.rds")
register_brand_fonts()
ggplot2::theme_set(theme_ghs3())

td <- file.path(tempdir(), "country_smoke")
dir.create(td, showWarnings = FALSE, recursive = TRUE)

samples <- list(
  cty_trend_chepc_chn    = plot_country_trend_chepc(m, "CHN"),
  cty_trend_chepc_usa    = plot_country_trend_chepc(m, "USA"),
  cty_hf_stack_chn       = plot_country_hf_stack(m, "CHN"),
  cty_hf_stack_usa       = plot_country_hf_stack(m, "USA"),
  cty_hc_bars_chn        = plot_country_hc_bars(m, "CHN"),
  cty_outcome_dual_chn   = plot_country_outcome_dual(m, "CHN"),
  cty_dashboard_chn      = plot_country_dashboard(m, "CHN"),
  cty_smallmult_brics    = plot_country_smallmult_chepc(m),
  cty_compare_g7         = plot_country_compare_chepc(m),
  cty_compare_heat_brics = plot_country_compare_heat(m),
  cty_pair_usa_chn       = plot_country_pair_hc(m),
  cty_cagr_brics         = plot_country_cagr_compare(m),
  cty_zscore_chn         = plot_country_zscore_income(m),
  cty_oop_gap_chn        = plot_country_oop_gap(m),
  cty_lifeexp_gap_chn    = plot_country_lifeexp_gap(m),
  cty_multi_vs_global    = plot_country_multi_vs_global(m),
  cty_rank_trend_chn     = plot_country_rank_trend(m),
  cty_topbot_chepc       = plot_country_topbot_chepc(m),
  cty_topbot_lifeexp     = plot_country_topbot_lifeexp(m),
  cty_start_progress     = plot_country_start_progress(m),
  cty_growth_dist        = plot_country_growth_dist(m),
  cty_topn_grid          = plot_country_topn_grid(m),
  cty_brics_dual         = plot_country_brics_dual(m),
  cty_g7_oop             = plot_country_g7_oop(m),
  cty_eu_vs_asean        = plot_country_eu_vs_asean(m),
  cty_nordic_combo       = plot_country_nordic_combo(m),
  cty_lowle_highle       = plot_country_lowle_highle(m)
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
