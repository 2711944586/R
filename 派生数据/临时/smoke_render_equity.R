options(warn = -1)
for (f in list.files("程序", pattern = "[.]R$", full.names = TRUE)) {
  suppressMessages(suppressWarnings(source(f, encoding = "UTF-8")))
}
m  <- readRDS("派生数据/处理结果/master_enriched.rds")
register_brand_fonts()
ggplot2::theme_set(theme_ghs3())

td <- file.path(tempdir(), "equity_smoke")
dir.create(td, showWarnings = FALSE, recursive = TRUE)

samples <- list(
  eq_oop_share_global       = plot_equity_oop_share_global(m),
  eq_oop_share_income       = plot_equity_oop_share_income(m),
  eq_oop_share_continent    = plot_equity_oop_share_continent(m),
  eq_oop_box_income         = plot_equity_oop_box_income(m),
  eq_catastrophic_proxy     = plot_equity_catastrophic_proxy(m),
  eq_lorenz_che             = plot_equity_lorenz_che(m),
  eq_gini_trend             = plot_equity_gini_trend(m),
  eq_theil_decomp           = plot_equity_theil_between_within(m),
  eq_concentration_lifeexp  = plot_equity_concentration_lifeexp(m),
  eq_index_dot              = plot_equity_index_dot(m),
  eq_che_pc_iqr             = plot_equity_che_pc_iqr(m),
  eq_top_bottom_ratio       = plot_equity_top_bottom_ratio(m),
  eq_dispersion_income      = plot_equity_dispersion_income(m),
  eq_logvar_trend           = plot_equity_logvar_trend(m),
  eq_che_fan                = plot_equity_che_fan(m),
  eq_continent_oop_box      = plot_equity_continent_oop_box(m),
  eq_region_heat            = plot_equity_region_heat(m),
  eq_continent_cv           = plot_equity_continent_cv(m),
  eq_oop_vs_che_facet       = plot_equity_oop_vs_che_facet(m),
  eq_continent_rank         = plot_equity_continent_rank(m),
  eq_high_oop_top           = plot_equity_high_oop_top(m),
  eq_oop_change             = plot_equity_oop_change(m),
  eq_oop_vs_gghed           = plot_equity_oop_vs_gghed(m),
  eq_threshold_share        = plot_equity_threshold_share(m),
  eq_oop_velocity           = plot_equity_oop_velocity(m)
)

ok_n <- 0; fail_n <- 0
for (n in names(samples)) {
  p <- samples[[n]]
  if (is.null(p)) { cat(sprintf("  %-28s NULL\n", n)); fail_n <- fail_n + 1; next }
  res <- tryCatch({
    ggplot2::ggsave(file.path(td, paste0(n, ".png")), p,
                    width = 10, height = 6, dpi = 130, bg = brand_palette$paper)
    "OK"
  }, error = function(e) conditionMessage(e))
  if (res == "OK") ok_n <- ok_n + 1 else fail_n <- fail_n + 1
  cat(sprintf("  %-28s %s\n", n, res))
}
cat(sprintf("\n=> %d ok / %d fail\n", ok_n, fail_n))
cat("dir:", td, "\n")
total_kb <- sum(file.info(list.files(td, full.names = TRUE))$size, na.rm = TRUE) / 1024
cat(sprintf("=> %.1f KB total\n", total_kb))
