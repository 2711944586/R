options(warn = -1)
for (f in list.files("程序", pattern = "[.]R$", full.names = TRUE)) {
  suppressMessages(suppressWarnings(source(f, encoding = "UTF-8")))
}
m  <- readRDS("派生数据/处理结果/master_enriched.rds")
sf_path <- "派生数据/处理结果/world_sf_medium.rds"
sf <- if (file.exists(sf_path)) readRDS(sf_path) else NULL
register_brand_fonts()
ggplot2::theme_set(theme_ghs3())

td <- file.path(tempdir(), "outcomes_smoke")
dir.create(td, showWarnings = FALSE, recursive = TRUE)

samples <- list(
  outcome_lexis_lifeexp           = plot_outcome_lexis_lifeexp(m),
  outcome_lexis_u5mr              = plot_outcome_lexis_u5mr(m),
  outcome_lexis_dual              = plot_outcome_lexis_dual(m),
  outcome_lifeexp_trend_global    = plot_outcome_lifeexp_trend_global(m),
  outcome_lifeexp_trend_income    = plot_outcome_lifeexp_trend_income(m),
  outcome_lifeexp_trend_continent = plot_outcome_lifeexp_trend_continent(m),
  outcome_u5mr_trend_global       = plot_outcome_u5mr_trend_global(m),
  outcome_u5mr_trend_income       = plot_outcome_u5mr_trend_income(m),
  outcome_u5mr_trend_continent    = plot_outcome_u5mr_trend_continent(m),
  outcome_lifeexp_gap_frontier    = plot_outcome_lifeexp_gap_to_frontier(m),
  outcome_u5mr_gap_frontier       = plot_outcome_u5mr_gap_to_frontier(m),
  outcome_lifeexp_gain            = plot_outcome_lifeexp_gain(m),
  outcome_u5mr_reduction          = plot_outcome_u5mr_reduction(m),
  outcome_frontier_lifeexp        = plot_outcome_frontier_lifeexp(m),
  outcome_frontier_u5mr           = plot_outcome_frontier_u5mr(m),
  outcome_frontier_composite      = plot_outcome_frontier_composite(m),
  outcome_efficiency_score        = plot_outcome_efficiency_score(m),
  outcome_frontier_panel_income   = plot_outcome_frontier_panel_income(m),
  outcome_elasticity_lifeexp      = plot_outcome_elasticity_lifeexp(m),
  outcome_elasticity_u5mr         = plot_outcome_elasticity_u5mr(m),
  outcome_residual_lifeexp        = plot_outcome_residual_lifeexp(m),
  outcome_residual_u5mr           = plot_outcome_residual_u5mr(m),
  outcome_decoupling_track        = plot_outcome_decoupling_track(m),
  outcome_u5mr_velocity           = plot_outcome_u5mr_velocity(m)
)
if (!is.null(sf)) {
  samples$outcome_residual_map_lifeexp <- plot_outcome_residual_map_lifeexp(m, sf)
}

ok_n <- 0; fail_n <- 0
for (n in names(samples)) {
  p <- samples[[n]]
  if (is.null(p)) { cat(sprintf("  %-32s NULL\n", n)); fail_n <- fail_n + 1; next }
  res <- tryCatch({
    ggplot2::ggsave(file.path(td, paste0(n, ".png")), p,
                    width = 10, height = 6, dpi = 130, bg = brand_palette$paper)
    "OK"
  }, error = function(e) conditionMessage(e))
  if (res == "OK") ok_n <- ok_n + 1 else fail_n <- fail_n + 1
  cat(sprintf("  %-32s %s\n", n, res))
}
cat(sprintf("\n=> %d ok / %d fail\n", ok_n, fail_n))
cat("dir:", td, "\n")
total_kb <- sum(file.info(list.files(td, full.names = TRUE))$size, na.rm = TRUE) / 1024
cat(sprintf("=> %.1f KB total\n", total_kb))
