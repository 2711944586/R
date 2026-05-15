options(warn = -1)
for (f in list.files("程序", pattern = "[.]R$", full.names = TRUE)) {
  suppressMessages(suppressWarnings(source(f, encoding = "UTF-8")))
}
m  <- readRDS("派生数据/处理结果/master_enriched.rds")
sf <- readRDS("派生数据/处理结果/world_sf_medium.rds")
register_brand_fonts()
ggplot2::theme_set(theme_ghs3())

td <- file.path(tempdir(), "maps_smoke")
dir.create(td, showWarnings = FALSE, recursive = TRUE)

samples <- list(
  map_world_che_pc       = plot_map_che_pc(m, sf),
  map_world_gghed        = plot_map_gghed(m, sf),
  map_world_oops         = plot_map_oops(m, sf),
  map_world_lifeexp      = plot_map_lifeexp(m, sf),
  map_world_u5mr         = plot_map_u5mr(m, sf),
  map_africa_che_pc      = plot_map_continent(m, sf, "Africa", "che_pc_usd2023", trans = "log10"),
  map_asia_che_pc        = plot_map_continent(m, sf, "Asia", "che_pc_usd2023", trans = "log10"),
  map_europe_che_pc      = plot_map_continent(m, sf, "Europe", "che_pc_usd2023", trans = "log10"),
  map_bivariate_che_life = plot_map_bivariate_che_life(m, sf),
  map_change_che_pc      = plot_map_che_pc_change(m, sf),
  map_change_oops        = plot_map_oops_change(m, sf),
  map_quintile_gghed     = plot_map_quintile(m, sf, "gghed_che", palette = "ocean"),
  map_bubble_che_total   = plot_map_bubble(m, sf, "che_usd2023"),
  map_sm_che_pc          = plot_map_smallmultiples(m, sf, "che_pc_usd2023",
                                                     years = c(2000, 2010, 2018, 2022),
                                                     trans = "log10")
)
ok_n <- 0; fail_n <- 0
for (n in names(samples)) {
  p <- samples[[n]]
  if (is.null(p)) { cat(sprintf("  %-25s NULL\n", n)); fail_n <- fail_n + 1; next }
  res <- tryCatch({
    ggplot2::ggsave(file.path(td, paste0(n, ".png")), p,
                    width = 11, height = 6.5, dpi = 130, bg = brand_palette$paper)
    "OK"
  }, error = function(e) conditionMessage(e))
  if (res == "OK") ok_n <- ok_n + 1 else fail_n <- fail_n + 1
  cat(sprintf("  %-25s %s\n", n, res))
}
cat(sprintf("\n=> %d ok / %d fail\n", ok_n, fail_n))
cat("dir:", td, "\n")
total_kb <- sum(file.info(list.files(td, full.names = TRUE))$size, na.rm = TRUE) / 1024
cat(sprintf("=> %.1f KB total\n", total_kb))
