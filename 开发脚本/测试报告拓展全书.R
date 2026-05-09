# 开发脚本/测试报告拓展全书.R
# 验证 报告书/05-12 v2 拓展节中的所有函数都能跑通
source("报告书/_setup.R", encoding = "UTF-8")
cat("[setup] master rows:", nrow(master_enriched), "\n")
yr_max <- max(master_enriched$year)

run_one <- function(label, expr) {
  res <- tryCatch({
    obj <- force(expr)
    cat(sprintf("  %-32s class: %s\n", label, class(obj)[1]))
    TRUE
  }, error = function(e) {
    cat(sprintf("  %-32s ERR: %s\n", label, conditionMessage(e)))
    FALSE
  })
  res
}

results <- list()

cat("\n========== ch05 ==========\n")
results$ch05 <- all(c(
  run_one("plot_v2_equity_lorenz",
           plot_v2_equity_lorenz(master_enriched, years = c(2000, yr_max))),
  run_one("widget_v2_income_violin",
           widget_v2_income_violin(master_enriched))
))

cat("\n========== ch06 ==========\n")
results$ch06 <- all(c(
  run_one("plot_v2_fiscal_ghe_rank",
           plot_v2_fiscal_ghe_rank(master_enriched, year = yr_max)),
  run_one("widget_v2_highlight_lines",
           widget_v2_highlight_lines(master_enriched,
             countries = c("USA","CHN","DEU","BRA","ZAF","IND")))
))

cat("\n========== ch07 ==========\n")
results$ch07 <- all(c(
  run_one("plot_v2_efficiency_dea",
           plot_v2_efficiency_dea(master_enriched, year = yr_max)),
  run_one("widget_v2_splom",
           widget_v2_splom(master_enriched, year = yr_max))
))

cat("\n========== ch08 ==========\n")
results$ch08 <- all(c(
  run_one("plot_v2_combined_ridges",
           plot_v2_combined_ridges(master_enriched)),
  run_one("widget_v2_ternary",
           widget_v2_ternary(master_enriched, year = yr_max))
))

cat("\n========== ch09 ==========\n")
results$ch09 <- all(c(
  run_one("plot_v2_aid_dependency",
           plot_v2_aid_dependency(master_enriched, year = yr_max)),
  run_one("widget_v2_country_network",
           widget_v2_country_network(master_enriched, year = yr_max, k = 4))
))

cat("\n========== ch10 ==========\n")
results$ch10 <- all(c(
  run_one("plot_v2_bivariate_oops_gov",
           plot_v2_bivariate_oops_gov(master_enriched, year = yr_max)),
  run_one("widget_v2_reactable_rank",
           widget_v2_reactable_rank(master_enriched, year = yr_max))
))

cat("\n========== ch11 ==========\n")
results$ch11 <- all(c(
  run_one("widget_v2_scenarios",
           widget_v2_scenarios(master_enriched, country_iso = "CHN")),
  run_one("widget_v2_mc_fan",
           widget_v2_mc_fan(master_enriched, country_iso = "USA"))
))

cat("\n========== ch12 ==========\n")
results$ch12 <- all(c(
  run_one("plot_v2_continent_stream",
           plot_v2_continent_stream(master_enriched)),
  run_one("widget_v2_dt_atlas",
           widget_v2_dt_atlas(master_enriched))
))

cat("\n[summary] ", paste(sprintf("%s=%s", names(results), unlist(results)),
                           collapse = " · "), "\n")
stopifnot(all(unlist(results)))
cat("\nAll ch05-12 v2 chunks pass\n")
