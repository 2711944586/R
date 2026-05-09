# 开发脚本/测试报告拓展前四章.R
# 验证 报告书/01-04 v2 拓展节的所有函数都能跑通
source("报告书/_setup.R", encoding = "UTF-8")
hl <- ghs_headline_numbers(master_enriched)
cat("[setup] master rows:", nrow(master_enriched), "\n")

cat("\n========== ch01 v2 chunks ==========\n")
res1 <- tryCatch({
  k1 <- narrative_kpi_grid(
    list(value = "$9T", label = "Total CHE"),
    list(value = "5.4%", label = "growth"),
    list(value = "125x", label = "gap"),
    list(value = "60/35/5", label = "ratio"))
  cat("[ch01] kpi_grid: OK\n")
  p1 <- plot_v2_oops_small_multiples(master_enriched)
  cat("[ch01] oops_small_multiples class:", class(p1)[1], "\n")
  w1 <- widget_v2_gapminder_bubble(master_enriched)
  cat("[ch01] gapminder_bubble class:", class(w1)[1], "\n")
  TRUE
}, error = function(e) { cat("ERR:", conditionMessage(e), "\n"); FALSE })

cat("\n========== ch02 v2 chunks ==========\n")
res2 <- tryCatch({
  p2a <- plot_v2_fiscal_ghe_share(master_enriched, year = max(master_enriched$year))
  cat("[ch02] fiscal_ghe_share class:", class(p2a)[1], "\n")
  p2b <- plot_v2_aid_dependency(master_enriched, year = max(master_enriched$year))
  cat("[ch02] aid_dependency class:", class(p2b)[1], "\n")
  w2 <- widget_v2_sankey_flows(master_enriched, year = max(master_enriched$year))
  cat("[ch02] sankey_flows class:", class(w2)[1], "\n")
  TRUE
}, error = function(e) { cat("ERR:", conditionMessage(e), "\n"); FALSE })

cat("\n========== ch03 v2 chunks ==========\n")
res3 <- tryCatch({
  p3a <- plot_v2_outcomes_elasticity(master_enriched)
  cat("[ch03] outcomes_elasticity class:", class(p3a)[1], "\n")
  p3b <- plot_v2_country_compare_hc(master_enriched)
  cat("[ch03] country_compare_hc class:", class(p3b)[1], "\n")
  w3 <- widget_v2_oops_heatmap(master_enriched)
  cat("[ch03] oops_heatmap class:", class(w3)[1], "\n")
  TRUE
}, error = function(e) { cat("ERR:", conditionMessage(e), "\n"); FALSE })

cat("\n========== ch04 v2 chunks ==========\n")
res4 <- tryCatch({
  p4a <- plot_v2_covid_dumbbell(master_enriched, n_top = 25)
  cat("[ch04] covid_dumbbell class:", class(p4a)[1], "\n")
  p4b <- plot_v2_continent_stream(master_enriched)
  cat("[ch04] continent_stream class:", class(p4b)[1], "\n")
  w4 <- widget_v2_mc_fan(master_enriched, country_iso = "CHN")
  cat("[ch04] mc_fan class:", class(w4)[1], "\n")
  TRUE
}, error = function(e) { cat("ERR:", conditionMessage(e), "\n"); FALSE })

cat(sprintf("\n[summary] ch01=%s ch02=%s ch03=%s ch04=%s\n",
             res1, res2, res3, res4))
stopifnot(all(c(res1, res2, res3, res4)))
cat("\nAll v2 book chunks pass\n")
