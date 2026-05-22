
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
}


.ghs_e <- function(x) {
  x <- as.character(x %||% "")
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x <- gsub("'", "&#39;", x, fixed = TRUE)
  x
}

.ghs_n <- function(x, d = 1, suf = "") {
  if (!length(x)) return("\u2014")
  vapply(x, function(xi) {
    if (!is.finite(xi)) return("\u2014")
    paste0(format(round(xi, d), big.mark = ",", nsmall = d), suf)
  }, character(1))
}

.ghs_m <- function(x) {
  if (!length(x) || !is.finite(x)) return("\u2014")
  if (abs(x) >= 1e12) return(sprintf("$%.2fT", x / 1e12))
  if (abs(x) >= 1e9)  return(sprintf("$%.2fB", x / 1e9))
  if (abs(x) >= 1e6)  return(sprintf("$%.2fM", x / 1e6))
  sprintf("$%s", format(round(x), big.mark = ","))
}

.ghs_size <- function(p) {
  if (!file.exists(p)) return("\u2014")
  mb <- file.info(p)$size / 1024^2
  if (mb >= 1) sprintf("%.1f MB", mb) else sprintf("%.0f KB", mb * 1024)
}

.ghs_b64 <- function(p, mime = NULL) {
  if (!requireNamespace("base64enc", quietly = TRUE)) stop("Need 'base64enc'.")
  if (is.null(mime)) {
    ext <- tolower(tools::file_ext(p))
    mime <- switch(ext, png = "image/png", svg = "image/svg+xml",
                   jpg = "image/jpeg", jpeg = "image/jpeg",
                   "application/octet-stream")
  }
  paste0("data:", mime, ";base64,", base64enc::base64encode(p))
}

.ghs_pretty <- function(p) {
  x <- tools::file_path_sans_ext(basename(p))
  key <- x
  x <- sub("^[0-9]+_", "", x)
  x <- sub("^v2_w_", "", x)
  x <- sub("^v2_", "", x)
  cn <- .ghs_cn_names()
  if (key %in% names(cn)) return(cn[[key]])
  if (x %in% names(cn)) return(cn[[x]])
  x <- gsub("_", " ", x, fixed = TRUE)
  trimws(x)
}

.ghs_cn_names <- function() {
  c(
    "global_sources_area" = "\u5168\u7403\u536b\u751f\u652f\u51fa\u6765\u6e90\u9762\u79ef\u56fe",
    "oops_ranking_2023" = "OOPS \u56fd\u5bb6\u6392\u884c",
    "oops_ridges_income" = "OOPS \u6309\u6536\u5165\u7ec4\u5bc6\u5ea6",
    "oops_box_continent" = "OOPS \u5927\u6d32\u7bb1\u7ebf\u56fe",
    "world_oops_2023" = "OOPS \u5168\u7403\u5730\u56fe",
    "profile_china" = "\u56fd\u5bb6\u6982\u51b5\u00b7\u4e2d\u56fd",
    "profile_usa" = "\u56fd\u5bb6\u6982\u51b5\u00b7\u7f8e\u56fd",
    "profile_india" = "\u56fd\u5bb6\u6982\u51b5\u00b7\u5370\u5ea6",
    "covid_dumbbell" = "COVID \u54d1\u94c3\u56fe",
    "covid_scatter" = "COVID \u6563\u70b9\u56fe",
    "hc1_vs_hc6" = "HC1 \u4e0e HC6 \u5bf9\u6bd4",
    "oops_slope" = "OOPS \u659c\u7387\u56fe",
    "inequality_timeseries" = "\u4e0d\u5e73\u7b49\u6307\u6570\u65f6\u5e8f",
    "che_pc_ridges" = "\u4eba\u5747 CHE \u5c71\u810a\u56fe",
    "pca_cluster_2022" = "PCA \u805a\u7c7b\u5206\u6790",
    "oops_heatmap" = "OOPS \u70ed\u529b\u56fe",
    "continent_radar" = "\u5927\u6d32\u96f7\u8fbe\u56fe",
    "small_multiples" = "\u5c0f\u500d\u6570\u56fe",
    "lollipop_income" = "\u6536\u5165\u7ec4\u68d2\u68d2\u7cd6",
    "ext_density" = "\u5916\u63f4\u5bc6\u5ea6\u56fe",
    "oops_bump" = "OOPS \u6392\u540d\u8d5b",
    "treemap" = "\u6811\u72b6\u56fe",
    "stream_continent" = "\u5927\u6d32\u6d41\u56fe",
    "ternary" = "\u4e09\u5143\u56fe",
    "inequality_pca" = "\u4e0d\u5e73\u7b49 PCA",
    "beta_convergence" = "\u03b2-\u6536\u655b\u5206\u6790",
    "forecast_fan" = "\u9884\u6d4b\u6247\u5f62\u56fe",
    "bivariate_map" = "\u53cc\u53d8\u91cf\u5730\u56fe",
    "sankey_static" = "Sankey \u6d41\u5411\u56fe",
    "waffle_purpose" = "\u652f\u51fa\u7528\u9014\u534e\u592b\u56fe",
    "aid_dependency" = "\u5916\u63f4\u4f9d\u8d56\u5ea6",
    "bivariate_oops_gov" = "OOPS \u4e0e\u653f\u5e9c\u652f\u51fa\u53cc\u53d8\u91cf",
    "changepoint" = "\u53d8\u70b9\u68c0\u6d4b",
    "cluster_archetype" = "\u805a\u7c7b\u539f\u578b",
    "combined_ridges" = "\u7ec4\u5408\u5c71\u810a\u56fe",
    "continent_ridges" = "\u5927\u6d32\u5c71\u810a\u56fe",
    "continent_stream" = "\u5927\u6d32\u6d41\u53d8\u56fe",
    "corr_matrix" = "\u76f8\u5173\u77e9\u9635",
    "country_compare_hc" = "\u56fd\u5bb6 HC \u5bf9\u6bd4",
    "efficiency_dea" = "DEA \u6548\u7387\u524d\u6cbf",
    "equity_indices" = "\u516c\u5e73\u6307\u6570\u9762\u677f",
    "equity_lorenz" = "Lorenz \u66f2\u7ebf",
    "extreme_waterfall" = "\u6781\u503c\u7011\u5e03\u56fe",
    "fiscal_ghe_rank" = "\u8d22\u653f\u536b\u751f\u652f\u51fa\u6392\u540d",
    "fiscal_ghe_share" = "\u8d22\u653f\u536b\u751f\u5360\u6bd4",
    "lifeexp_elasticity" = "\u5bff\u547d\u5f39\u6027\u5206\u6790",
    "oops_heatmap_grid" = "OOPS \u70ed\u529b\u7f51\u683c",
    "oops_small_multiples" = "OOPS \u5c0f\u500d\u6570",
    "oops_violin" = "OOPS \u5c0f\u63d0\u7434\u56fe",
    "outcomes_elasticity" = "\u4ea7\u51fa\u5f39\u6027",
    "period_compare" = "\u5206\u671f\u5bf9\u6bd4",
    "profile_brazil" = "\u56fd\u5bb6\u6982\u51b5\u00b7\u5df4\u897f",
    "quantile_reg" = "\u5206\u4f4d\u6570\u56de\u5f52",
    "rank_change" = "\u6392\u540d\u53d8\u52a8",
    "sdg3_progress" = "SDG-3 \u8fdb\u5c55",
    "sdg3_radar" = "SDG-3 \u96f7\u8fbe",
    "slope_oops" = "OOPS \u659c\u7387\u53d8\u5316",
    "che_gdp_share_world" = "CHE/GDP \u5168\u7403\u5206\u5e03",
    "che_gdp_top" = "CHE/GDP \u6700\u9ad8\u56fd",
    "che_life_log" = "CHE \u4e0e\u5bff\u547d\u5bf9\u6570",
    "che_top_growth" = "CHE \u589e\u901f\u6700\u5feb\u56fd",
    "che_u5_log" = "CHE \u4e0e U5MR \u5bf9\u6570",
    "compare_panel" = "\u5bf9\u6bd4\u9762\u677f",
    "continent_cagr" = "\u5927\u6d32\u5e74\u5747\u589e\u7387",
    "continent_che_box" = "\u5927\u6d32 CHE \u7bb1\u7ebf",
    "continent_forecast" = "\u5927\u6d32\u9884\u6d4b",
    "continent_gghed_area" = "\u5927\u6d32 GGHED \u9762\u79ef",
    "continent_oops_area" = "\u5927\u6d32 OOPS \u9762\u79ef",
    "correlation_overtime" = "\u76f8\u5173\u7cfb\u6570\u8de8\u5e74",
    "efficiency_frontier" = "\u6548\u7387\u524d\u6cbf",
    "ext_top_recipients" = "\u5916\u63f4\u6700\u5927\u63a5\u53d7\u56fd",
    "global_che_total" = "\u5168\u7403 CHE \u603b\u91cf",
    "global_hf_share" = "\u5168\u7403\u7b79\u8d44\u4efd\u989d",
    "hc_breakdown" = "HC \u529f\u80fd\u5206\u89e3",
    "income_cagr" = "\u6536\u5165\u7ec4\u589e\u7387",
    "income_oops_decade" = "\u6536\u5165\u7ec4 OOPS \u5341\u5e74",
    "income_ridges" = "\u6536\u5165\u7ec4\u5c71\u810a",
    "life_change_top" = "\u5bff\u547d\u589e\u91cf\u6700\u5927\u56fd",
    "life_residual" = "\u5bff\u547d\u6b8b\u5dee",
    "oops_dumbbell_decline" = "OOPS \u4e0b\u964d\u54d1\u94c3",
    "oops_dumbbell_rise" = "OOPS \u4e0a\u5347\u54d1\u94c3",
    "oops_gdp" = "OOPS \u4e0e GDP",
    "oops_life" = "OOPS \u4e0e\u5bff\u547d",
    "oops_top_bottom" = "OOPS \u6781\u503c\u56fd",
    "profile_chn" = "\u56fd\u5bb6\u6982\u51b5\u00b7\u4e2d\u56fd",
    "profile_deu" = "\u56fd\u5bb6\u6982\u51b5\u00b7\u5fb7\u56fd",
    "profile_idn" = "\u56fd\u5bb6\u6982\u51b5\u00b7\u5370\u5c3c",
    "profile_jpn" = "\u56fd\u5bb6\u6982\u51b5\u00b7\u65e5\u672c",
    "profile_zaf" = "\u56fd\u5bb6\u6982\u51b5\u00b7\u5357\u975e",
    "region_hf_breakdown" = "\u533a\u57df\u7b79\u8d44\u5206\u89e3",
    "u5mr_decline_top" = "U5MR \u4e0b\u964d\u6700\u5feb\u56fd",
    "yoy_heatmap" = "\u540c\u6bd4\u70ed\u529b\u56fe",
    "adv_beeswarm_che_pc" = "\u4eba\u5747 CHE \u8702\u7fa4\u56fe",
    "adv_beeswarm_gghed" = "GGHED \u8702\u7fa4\u56fe",
    "adv_beeswarm_lifeexp" = "\u5bff\u547d\u8702\u7fa4\u56fe",
    "adv_beeswarm_oops" = "OOPS \u8702\u7fa4\u56fe",
    "adv_bump_top25" = "Top25 \u6392\u540d\u8d5b",
    "adv_calendar_growth" = "\u589e\u901f\u65e5\u5386\u70ed\u56fe",
    "adv_che_pc_by_continent" = "\u4eba\u5747 CHE \u6309\u5927\u6d32",
    "adv_density_2d_finance" = "\u7b79\u8d44\u4e8c\u7ef4\u5bc6\u5ea6",
    "adv_lollipop_che_change" = "CHE \u53d8\u5316\u68d2\u68d2\u7cd6",
    "adv_marimekko_finance" = "\u7b79\u8d44 Marimekko",
    "adv_parallel_finance" = "\u7b79\u8d44\u5e73\u884c\u5750\u6807",
    "adv_radial_hc_deu" = "\u5fb7\u56fd HC \u5f84\u5411\u56fe",
    "adv_radial_hc_jpn" = "\u65e5\u672c HC \u5f84\u5411\u56fe",
    "adv_radial_hc_usa" = "\u7f8e\u56fd HC \u5f84\u5411\u56fe",
    "adv_ridge_che_pc_evolution" = "\u4eba\u5747 CHE \u5c71\u810a\u6f14\u5316",
    "adv_ridge_gghed_by_continent" = "GGHED \u5927\u6d32\u5c71\u810a",
    "adv_ridge_oops_by_income" = "OOPS \u6536\u5165\u7ec4\u5c71\u810a",
    "adv_slope_smallmultiples" = "\u659c\u7387\u5c0f\u500d\u6570",
    "adv_sources_by_income" = "\u6536\u5165\u7ec4\u7b79\u8d44\u6765\u6e90",
    "adv_stream_continent" = "\u5927\u6d32\u6d41\u56fe",
    "adv_stream_global_sources" = "\u5168\u7403\u6765\u6e90\u6d41\u56fe",
    "adv_stream_income" = "\u6536\u5165\u7ec4\u6d41\u56fe",
    "adv_topbot_che_pc" = "CHE \u6781\u503c\u5bf9\u6bd4",
    "adv_topbot_gghed" = "GGHED \u6781\u503c\u5bf9\u6bd4",
    "adv_topbot_life" = "\u5bff\u547d\u6781\u503c\u5bf9\u6bd4",
    "adv_topbot_oops" = "OOPS \u6781\u503c\u5bf9\u6bd4",
    "map_africa_che_pc" = "\u975e\u6d32\u4eba\u5747 CHE \u5730\u56fe",
    "map_africa_gghed" = "\u975e\u6d32 GGHED \u5730\u56fe",
    "map_asia_che_pc" = "\u4e9a\u6d32\u4eba\u5747 CHE \u5730\u56fe",
    "map_asia_oops" = "\u4e9a\u6d32 OOPS \u5730\u56fe",
    "map_bivariate_che_life" = "CHE\u00d7\u5bff\u547d\u53cc\u53d8\u91cf\u5730\u56fe",
    "map_bivariate_gghed_oops" = "GGHED\u00d7OOPS \u53cc\u53d8\u91cf\u5730\u56fe",
    "map_bivariate_gghed_u5mr" = "GGHED\u00d7U5MR \u53cc\u53d8\u91cf\u5730\u56fe",
    "map_bubble_che_total" = "CHE \u603b\u91cf\u6c14\u6ce1\u5730\u56fe",
    "map_bubble_oops_total" = "OOPS \u603b\u91cf\u6c14\u6ce1\u5730\u56fe",
    "map_change_che_pc" = "\u4eba\u5747 CHE \u53d8\u5316\u5730\u56fe",
    "map_change_lifeexp" = "\u5bff\u547d\u53d8\u5316\u5730\u56fe",
    "map_change_oops" = "OOPS \u53d8\u5316\u5730\u56fe",
    "map_europe_che_pc" = "\u6b27\u6d32\u4eba\u5747 CHE \u5730\u56fe",
    "map_oceania_che_pc" = "\u5927\u6d0b\u6d32 CHE \u5730\u56fe",
    "map_quintile_che_pc" = "\u4eba\u5747 CHE \u4e94\u5206\u4f4d\u5730\u56fe",
    "map_quintile_gghed" = "GGHED \u4e94\u5206\u4f4d\u5730\u56fe",
    "map_quintile_lifeexp" = "\u5bff\u547d\u4e94\u5206\u4f4d\u5730\u56fe",
    "map_quintile_oops" = "OOPS \u4e94\u5206\u4f4d\u5730\u56fe",
    "map_sm_che_pc" = "\u4eba\u5747 CHE \u5c0f\u500d\u6570\u5730\u56fe",
    "map_sm_gghed" = "GGHED \u5c0f\u500d\u6570\u5730\u56fe",
    "map_sm_oops" = "OOPS \u5c0f\u500d\u6570\u5730\u56fe",
    "map_world_che_pc" = "\u5168\u7403\u4eba\u5747 CHE \u5730\u56fe",
    "map_world_gghed" = "\u5168\u7403 GGHED \u5730\u56fe",
    "map_world_lifeexp" = "\u5168\u7403\u5bff\u547d\u5730\u56fe",
    "map_world_oops" = "\u5168\u7403 OOPS \u5730\u56fe",
    "map_world_u5mr" = "\u5168\u7403 U5MR \u5730\u56fe",
    "donut_financing" = "\u7b79\u8d44\u6765\u6e90\u73af\u5f62\u56fe",
    "polar_bar_hc_purpose" = "HC \u7528\u9014\u6781\u5750\u6807",
    "diverging_bar_oops_change" = "OOPS \u53d8\u5316\u53d1\u6563\u6761",
    "trajectory_chn_che_life" = "\u4e2d\u56fd CHE\u00d7\u5bff\u547d\u8f68\u8ff9",
    "pyramid_income_decile" = "\u6536\u5165\u5341\u5206\u4f4d\u91d1\u5b57\u5854",
    "tile_matrix_completeness" = "\u6570\u636e\u5b8c\u6574\u6027\u77e9\u9635",
    "ribbon_che_iqr" = "CHE \u56db\u5206\u4f4d\u5e26",
    "step_oops_high_pct" = "OOPS \u9ad8\u503c\u56fd\u6bd4\u4f8b",
    "range_span_continent" = "\u5927\u6d32\u8303\u56f4\u56fe",
    "cleveland_gghed_oops" = "GGHED\u00d7OOPS Cleveland",
    "sparkline_panel_6x3" = "\u8ff7\u4f60\u56fe\u9762\u677f 6\u00d73",
    "butterfly_africa_europe" = "\u975e\u6d32\u00d7\u6b27\u6d32\u8776\u5f62\u56fe",
    "gantt_high_oops_sustained" = "\u6301\u7eed\u9ad8 OOPS \u56fd\u7518\u7279\u56fe",
    "dumbbell_che_growth_top20" = "CHE \u589e\u957f Top20 \u54d1\u94c3",
    "waterfall_che_decomposition" = "CHE \u5206\u89e3\u7011\u5e03\u56fe",
    "lorenz_che_3years" = "Lorenz \u66f2\u7ebf\u4e09\u5e74\u5bf9\u6bd4",
    "corr_heatmap_matrix" = "\u76f8\u5173\u70ed\u529b\u77e9\u9635",
    "ridgeline_income_years" = "\u6536\u5165\u7ec4\u8de8\u5e74\u5c71\u810a",
    "beeswarm_oops_continent" = "OOPS \u5927\u6d32\u8702\u7fa4",
    "violin_che_income" = "CHE \u6536\u5165\u7ec4\u5c0f\u63d0\u7434",
    "dotmatrix_oops_risk" = "OOPS \u98ce\u9669\u70b9\u9635",
    "area_stacked_3sources" = "\u4e09\u6765\u6e90\u5806\u53e0\u9762\u79ef",
    "slope_oops_change_16" = "OOPS 16\u56fd\u659c\u7387\u56fe",
    "bubble_gdp_che_pop" = "GDP\u00d7CHE\u00d7\u4eba\u53e3\u6c14\u6ce1",
    "facet_scatter_che_life" = "CHE\u00d7\u5bff\u547d\u5206\u9762\u6563\u70b9",
    "quadrant_fiscal_protection" = "\u8d22\u653f\u4fdd\u62a4\u56db\u8c61\u9650",
    "ecdf_che_income" = "CHE \u7ecf\u9a8c\u5206\u5e03",
    "segment_extreme_gap" = "\u6781\u503c\u5dee\u8ddd\u7ebf\u6bb5",
    "density2d_che_life" = "CHE\u00d7\u5bff\u547d\u4e8c\u7ef4\u5bc6\u5ea6",
    "proportion_continent_income" = "\u5927\u6d32\u00d7\u6536\u5165\u6bd4\u4f8b",
    "area_highlight_chn_global" = "\u4e2d\u56fd\u00d7\u5168\u7403\u9ad8\u4eae\u9762\u79ef",
    "paired_bar_covid_gghed" = "COVID GGHED \u5bf9\u6bd4\u6761",
    "cumulative_area_oops_target" = "OOPS \u76ee\u6807\u7d2f\u8ba1\u9762\u79ef",
    "lollipop_ext_top20" = "\u5916\u63f4 Top20 \u68d2\u68d2\u7cd6",
    "histogram_density_che" = "CHE \u76f4\u65b9\u56fe\u5bc6\u5ea6",
    "boxplot_notched_continent" = "\u5927\u6d32\u7f3a\u53e3\u7bb1\u7ebf",
    "strip_oops_income" = "OOPS \u6536\u5165\u7ec4\u5e26\u72b6",
    "area_between_income_gap" = "\u6536\u5165\u7ec4\u5dee\u8ddd\u9762\u79ef",
    "gapminder_animated" = "Gapminder \u52a8\u753b\u6563\u70b9",
    "highlight_ts" = "\u9ad8\u4eae\u65f6\u5e8f",
    "bar_race" = "\u6761\u5f62\u56fe\u7ade\u8d5b",
    "forecast_subplot" = "\u9884\u6d4b\u5b50\u56fe",
    "sankey_sources" = "\u7b79\u8d44 Sankey \u6d41\u5411",
    "china_wb_line" = "\u4e2d\u56fd\u4e16\u884c\u6307\u6807",
    "world_leaflet" = "\u5168\u7403 Leaflet \u5730\u56fe",
    "reactable_rank" = "\u6392\u540d\u4ea4\u4e92\u8868",
    "country_network" = "\u56fd\u5bb6\u76f8\u4f3c\u5ea6\u7f51\u7edc",
    "gapminder_bubble" = "Gapminder \u6c14\u6ce1",
    "highlight_lines" = "\u56fd\u522b\u9ad8\u4eae\u7ebf",
    "income_violin" = "\u6536\u5165\u7ec4\u5c0f\u63d0\u7434",
    "leaflet_choropleth" = "Leaflet \u5206\u7ea7\u5730\u56fe",
    "mc_fan" = "\u8499\u7279\u5361\u6d1b\u6247\u5f62",
    "oops_heatmap" = "OOPS \u70ed\u529b\u56fe",
    "sankey_flows" = "Sankey \u8d44\u91d1\u6d41",
    "scenarios" = "\u60c5\u666f\u6a21\u62df",
    "splom" = "\u6563\u70b9\u77e9\u9635",
    "dt_atlas" = "\u56fd\u5bb6\u56fe\u96c6\u8868"
  )
}

.ghs_slug <- function(p) {
  x <- tolower(tools::file_path_sans_ext(basename(p)))
  x <- gsub("[^a-z0-9]+", "-", x, perl = TRUE)
  x <- gsub("(^-|-$)", "", x)
  paste0("fig-", x)
}

.ghs_kind <- function(name) {
  n <- tolower(name)
  if (grepl("map|choropleth|bivariate|world|atlas|leaflet", n)) return("\u5730\u56fe")
  if (grepl("ridges|density|box|violin|heatmap", n)) return("\u5206\u5e03")
  if (grepl("forecast|beta|pca|cluster|dea|elasticity|panel|fe|model|scenarios|mc_fan|splom", n)) return("\u6a21\u578b")
  if (grepl("slope|bump|stream|area|timeseries|highlight|line|covid|gapminder|race|ts", n)) return("\u65f6\u95f4")
  if (grepl("sankey|ternary|radar|waffle|treemap|network", n)) return("\u7ed3\u6784")
  if (grepl("rank|reactable|dt_atlas|table|lollipop|inequality|equity|fiscal|share", n)) return("\u6307\u6807")
  "\u7efc\u5408"
}

.ghs_nav_items <- function() {
  list(
    list(group = "\u9996\u9875", items = list(
      list(id = "top", label = "\u2191 \u9876\u90e8")
    )),
    list(group = "1 \u6982\u89c8", items = list(
      list(id = "executive", label = "\u6458\u8981"),
      list(id = "methods", label = "\u6570\u636e\u5904\u7406"),
      list(id = "dq", label = "\u6570\u636e\u8d28\u91cf"),
      list(id = "codebook", label = "\u53d8\u91cf\u53e3\u5f84"),
      list(id = "kpi", label = "KPI")
    )),
    list(group = "2 \u6838\u5fc3\u53d1\u73b0", items = list(
      list(id = "f-trend", label = "F1 \u5168\u7403\u8d8b\u52bf"),
      list(id = "f-finance", label = "F2 \u7b79\u8d44"),
      list(id = "f-equity", label = "F3 \u4e0d\u5e73\u7b49"),
      list(id = "f-covid", label = "F4 COVID"),
      list(id = "f-beta", label = "F5 \u6536\u655b"),
      list(id = "f-fe", label = "F6 \u5f39\u6027"),
      list(id = "f-cluster", label = "F7 \u805a\u7c7b"),
      list(id = "f-forecast", label = "F8 \u9884\u6d4b"),
      list(id = "f-aid", label = "F9 \u5916\u63f4"),
      list(id = "f-efficiency", label = "F10 \u6548\u7387"),
      list(id = "f-rank", label = "F11 \u6392\u540d"),
      list(id = "f-lifeexp", label = "F12 \u5bff\u547d"),
      list(id = "f-sdg3", label = "F13 SDG-3"),
      list(id = "f-extreme", label = "F14 \u6781\u503c")
    )),
    list(group = "3 \u6df1\u5ea6\u4e13\u9898", items = list(
      list(id = "f-aging", label = "F15 \u8001\u9f84\u5316"),
      list(id = "f-urban", label = "F16 \u57ce\u9547\u5316"),
      list(id = "f-fiscal", label = "F17 \u8d22\u653f"),
      list(id = "f-price", label = "F18 \u53ef\u8d1f\u62c5"),
      list(id = "f-regional", label = "F19 \u533a\u57df"),
      list(id = "f-inflation", label = "F20 \u901a\u80c0"),
      list(id = "f-ncd", label = "F21 NCD"),
      list(id = "f-uhc", label = "F22 UHC"),
      list(id = "f-catastrophic", label = "F23 \u707e\u96be\u6027"),
      list(id = "f-maternal", label = "F24 \u6bcd\u5a74"),
      list(id = "f-prevention", label = "F25 \u9884\u9632"),
      list(id = "f-workforce", label = "F26 \u4eba\u529b"),
      list(id = "f-reclassify", label = "F27 \u664b\u5347")
    )),
    list(group = "4 \u6269\u5c55\u53d1\u73b0", items = list(
      list(id = "f-theil", label = "F28 \u5206\u89e3"),
      list(id = "f-fragile", label = "F29 \u8106\u5f31"),
      list(id = "f-oecd", label = "F30 OECD/LMIC"),
      list(id = "f-dea", label = "F31 \u6548\u7387"),
      list(id = "f-aid-eff", label = "F32 \u63f4\u52a9"),
      list(id = "f-dataquality", label = "F33 \u6570\u636e"),
      list(id = "f-revision", label = "F34 \u4fee\u8ba2"),
      list(id = "f-sids", label = "F35 \u5c0f\u5c9b\u56fd"),
      list(id = "f-composite", label = "F36 \u7efc\u5408")
    )),
    list(group = "5 \u4e13\u9898\u7ae0\u8282", items = list(
      list(id = "countries", label = "\u56fd\u5bb6"),
      list(id = "regional", label = "\u533a\u57df"),
      list(id = "period", label = "\u5206\u671f"),
      list(id = "sdg3", label = "SDG-3"),
      list(id = "lifeexp", label = "\u5bff\u547d"),
      list(id = "atlas", label = "\u4e0d\u5e73\u7b49"),
      list(id = "cluster-detail", label = "\u805a\u7c7b"),
      list(id = "extreme", label = "\u6781\u503c"),
      list(id = "simulator", label = "\u4eff\u771f")
    )),
    list(group = "6 \u56fe\u5e93", items = list(
      list(id = "gallery", label = "\u9759\u6001\u56fe\u8868"),
      list(id = "widgets", label = "\u4ea4\u4e92\u7ec4\u4ef6")
    )),
    list(group = "7 \u9644\u5f55", items = list(
      list(id = "repro", label = "\u590d\u73b0"),
      list(id = "conclusion", label = "\u7ed3\u8bba")
    ))
  )
}



.ghs_nav_links <- function(class = NULL) {
  nav <- .ghs_nav_items()
  cls <- if (!is.null(class) && nzchar(class)) sprintf(" class='%s'", .ghs_e(class)) else ""
  groups <- vapply(nav, function(g) {
    items <- vapply(g$items, function(it) {
      sprintf("<a href='#%s' data-nav-id='%s'>%s</a>",
              .ghs_e(it$id), .ghs_e(it$id), .ghs_e(it$label))
    }, character(1))
    sprintf("<span class='nav-group'><span class='nav-group-label'>%s</span>%s</span>",
            .ghs_e(g$group), paste(items, collapse = ""))
  }, character(1))
  sprintf("<nav%s>%s</nav>", cls, paste(groups, collapse = ""))
}

.ghs_mobile_toc <- function() {
  nav <- .ghs_nav_items()
  links <- vapply(nav, function(g) {
    items <- vapply(g$items, function(it) {
      sprintf("<a href='#%s'>%s</a>", .ghs_e(it$id), .ghs_e(it$label))
    }, character(1))
    sprintf("<div class='mobile-toc-group'><span class='mobile-toc-group-label'>%s</span><div class='mobile-toc-items'>%s</div></div>",
            .ghs_e(g$group), paste(items, collapse = ""))
  }, character(1))
  sprintf("<details class='mobile-toc' id='mobile-toc'><summary>\u76ee\u5f55 \u00b7 Sections</summary><div class='mobile-toc-body'>%s</div></details>",
          paste(links, collapse = ""))
}

.ghs_decode_unicode_escapes <- function(s) {
  if (!length(s) || !nzchar(s)) return(s)
  m <- regmatches(s, gregexpr("\\\\u[0-9a-fA-F]{4}", s, perl = TRUE))[[1]]
  if (!length(m)) return(s)
  for (esc in unique(m)) {
    code <- strtoi(substring(esc, 3), base = 16L)
    if (is.na(code) || code <= 0L) next
    s <- gsub(esc, intToUtf8(code), s, fixed = TRUE)
  }
  s
}

.ghs_read_code <- function(file, from = NULL, to = NULL) {
  if (!file.exists(file)) return("# (code unavailable)")
  lns <- readLines(file, warn = FALSE, encoding = "UTF-8")
  lns <- vapply(lns, .ghs_decode_unicode_escapes, character(1), USE.NAMES = FALSE)
  if (!is.null(from) || !is.null(to)) {
    from <- from %||% 1L; to <- to %||% length(lns)
    lns <- lns[seq.int(max(1L, from), min(length(lns), to))]
  }
  while (length(lns) && !nzchar(trimws(lns[[1]]))) lns <- lns[-1]
  while (length(lns) && !nzchar(trimws(lns[[length(lns)]]))) lns <- lns[-length(lns)]
  paste(lns, collapse = "\n")
}

.ghs_code <- function(code, lang = "r", cap = NULL) {
  body <- .ghs_e(code)
  cap_html <- if (length(cap) && nzchar(cap))
    sprintf("<figcaption class='code-caption'>%s</figcaption>", .ghs_e(cap)) else ""
  sprintf("<figure class='code-figure'>%s<pre class='code-pre'><code class='language-%s'>%s</code></pre></figure>",
          cap_html, .ghs_e(lang), body)
}

.ghs_para <- function(...) {
  parts <- c(...); parts <- parts[nzchar(parts)]
  if (!length(parts)) return("")
  paste0("<p>", parts, "</p>", collapse = "\n")
}


.ghs_w_mean <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (!any(ok)) return(NA_real_)
  stats::weighted.mean(x[ok], w[ok])
}

.ghs_w_quantile <- function(x, w, p = 0.5) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (!any(ok)) return(NA_real_)
  o <- order(x[ok]); xv <- x[ok][o]; wv <- w[ok][o]
  cw <- cumsum(wv) / sum(wv)
  xv[which(cw >= p)[1]]
}

.ghs_summary <- function(master) {
  mst <- master[is.finite(master$year), ]
  cur <- max(mst$year, na.rm = TRUE); base <- min(mst$year, na.rm = TRUE)
  prev <- cur - 1L
  d_cur  <- mst[mst$year == cur,  ]
  d_base <- mst[mst$year == base, ]
  d_prev <- mst[mst$year == prev, ]
  che_cur  <- sum(d_cur$che_usd2023,  na.rm = TRUE)
  che_base <- sum(d_base$che_usd2023, na.rm = TRUE)
  che_prev <- sum(d_prev$che_usd2023, na.rm = TRUE)
  span <- max(1L, cur - base)
  pop_cur  <- sum(d_cur$pop, na.rm = TRUE)
  gdp_cur  <- if ("gdp_usd" %in% names(d_cur)) sum(d_cur$gdp_usd, na.rm = TRUE) else NA_real_
  che_share_gdp_cur <- if (is.finite(gdp_cur) && gdp_cur > 0) che_cur / gdp_cur * 100 else NA_real_
  oops_p10 <- .ghs_w_quantile(d_cur$hf3_che, d_cur$pop, 0.10)
  oops_p90 <- .ghs_w_quantile(d_cur$hf3_che, d_cur$pop, 0.90)
  che_pc_p10 <- .ghs_w_quantile(d_cur$che_pc_usd2023, d_cur$pop, 0.10)
  che_pc_p90 <- .ghs_w_quantile(d_cur$che_pc_usd2023, d_cur$pop, 0.90)
  list(
    n_country = length(unique(mst$iso3_code)),
    base_year = base, cur_year = cur, prev_year = prev, span = span,
    pop_cur = pop_cur, gdp_cur = gdp_cur,
    che_total_cur = che_cur, che_total_base = che_base, che_total_prev = che_prev,
    che_total_growth = (che_cur / che_base)^(1 / span) - 1,
    che_yoy = if (is.finite(che_prev) && che_prev > 0) che_cur / che_prev - 1 else NA_real_,
    che_share_gdp_cur = che_share_gdp_cur,
    che_pc_cur  = .ghs_w_mean(d_cur$che_pc_usd2023, d_cur$pop),
    che_pc_base = .ghs_w_mean(d_base$che_pc_usd2023, d_base$pop),
    che_pc_p10 = che_pc_p10, che_pc_p90 = che_pc_p90,
    che_pc_ratio_p90_p10 = if (is.finite(che_pc_p10) && che_pc_p10 > 0)
      che_pc_p90 / che_pc_p10 else NA_real_,
    oops_mean_cur  = mean(d_cur$hf3_che,  na.rm = TRUE),
    oops_mean_base = mean(d_base$hf3_che, na.rm = TRUE),
    oops_w_cur     = .ghs_w_mean(d_cur$hf3_che, d_cur$pop),
    oops_p10 = oops_p10, oops_p90 = oops_p90,
    oops_high_cur  = sum(d_cur$hf3_che > 50, na.rm = TRUE),
    oops_low_cur   = sum(d_cur$hf3_che < 15, na.rm = TRUE),
    gghed_mean_cur = mean(d_cur$gghed_che, na.rm = TRUE),
    gghed_w_cur    = .ghs_w_mean(d_cur$gghed_che, d_cur$pop),
    pvtd_mean_cur  = mean(d_cur$pvtd_che, na.rm = TRUE),
    ext_mean_cur   = mean(d_cur$ext_che,  na.rm = TRUE),
    ext_high_cur   = sum(d_cur$ext_che > 20, na.rm = TRUE),
    n_continent    = length(unique(stats::na.omit(d_cur$continent)))
  )
}

.ghs_top_oops <- function(master, n = 8, asc = FALSE) {
  yr <- max(master$year, na.rm = TRUE)
  d <- master[master$year == yr & is.finite(master$hf3_che), ]
  d <- d[, c("country_name", "iso3_code", "continent", "hf3_che", "che_pc_usd2023")]
  d <- d[order(d$hf3_che, decreasing = !asc), ]
  utils::head(d, n)
}

.ghs_covid_top <- function(master, n = 8) {
  d <- master[master$year %in% 2019:2022 & is.finite(master$che_usd2023), ]
  if (!nrow(d)) return(NULL)
  base <- stats::aggregate(che_usd2023 ~ iso3_code + country_name,
                           data = d[d$year == 2019, , drop = FALSE], FUN = mean)
  shock <- stats::aggregate(che_usd2023 ~ iso3_code + country_name,
                            data = d[d$year %in% 2020:2022, , drop = FALSE], FUN = mean)
  m <- merge(base, shock, by = c("iso3_code", "country_name"),
             suffixes = c("_base", "_shock"))
  m$delta_pct <- (m$che_usd2023_shock - m$che_usd2023_base) /
                  pmax(m$che_usd2023_base, 1) * 100
  m <- m[is.finite(m$delta_pct), ]
  m <- m[order(m$delta_pct, decreasing = TRUE), ]
  utils::head(m[, c("country_name", "iso3_code",
                    "che_usd2023_base", "che_usd2023_shock", "delta_pct")], n)
}

.ghs_continent_panel <- function(master) {
  yr <- max(master$year, na.rm = TRUE)
  d <- master[master$year == yr, ]
  if (!nrow(d) || !"continent" %in% names(d)) return(NULL)
  d <- d[is.finite(d$che_usd2023) & nzchar(d$continent), ]
  spl <- split(d, d$continent)
  rows <- lapply(names(spl), function(k) {
    x <- spl[[k]]
    out_row <- data.frame(
      v_continent  = k,
      v_n          = nrow(x),
      v_pop        = sum(x$pop, na.rm = TRUE),
      v_che_pc     = .ghs_w_mean(x$che_pc_usd2023, x$pop),
      v_oops       = .ghs_w_mean(x$hf3_che,        x$pop),
      v_gghed      = .ghs_w_mean(x$gghed_che,      x$pop),
      v_ext        = .ghs_w_mean(x$ext_che,        x$pop),
      stringsAsFactors = FALSE
    )
    names(out_row) <- c("\u5927\u6d32", "\u56fd\u5bb6\u6570",
                        "\u4eba\u53e3", "\u4eba\u5747 CHE",
                        "OOPS \u5747\u503c", "GGHED \u5747\u503c",
                        "EXT \u5747\u503c")
    out_row
  })
  out <- do.call(rbind, rows)
  out[order(-out[["\u4eba\u5747 CHE"]]), ]
}

.ghs_country_panel <- function(master, iso3, years = NULL) {
  d <- master[master$iso3_code == iso3, ]
  if (!nrow(d)) return(NULL)
  if (!is.null(years)) d <- d[d$year %in% years, ]
  cols <- c("year", "che_pc_usd2023", "che_usd2023",
            "gghed_che", "pvtd_che", "ext_che", "hf3_che", "pop")
  d <- d[, intersect(cols, names(d)), drop = FALSE]
  d <- d[order(d$year), ]
  d
}

.ghs_country_brief <- function(master, iso3) {
  d <- master[master$iso3_code == iso3, ]
  if (!nrow(d)) return(NULL)
  cur <- max(d$year, na.rm = TRUE); base <- min(d$year, na.rm = TRUE)
  span <- max(1L, cur - base)
  cur_row <- d[d$year == cur, ][1, , drop = FALSE]
  base_row <- d[d$year == base, ][1, , drop = FALSE]
  che_g <- if (is.finite(cur_row$che_usd2023) && is.finite(base_row$che_usd2023) &&
               base_row$che_usd2023 > 0)
    (cur_row$che_usd2023 / base_row$che_usd2023)^(1 / span) - 1 else NA_real_
  list(
    iso3 = iso3,
    name = cur_row$country_name,
    continent = cur_row$continent,
    pop = cur_row$pop,
    che_pc_cur = cur_row$che_pc_usd2023,
    che_pc_base = base_row$che_pc_usd2023,
    che_total_cur = cur_row$che_usd2023,
    che_g = che_g,
    oops_cur = cur_row$hf3_che,
    oops_base = base_row$hf3_che,
    gghed_cur = cur_row$gghed_che,
    pvtd_cur = cur_row$pvtd_che,
    ext_cur  = cur_row$ext_che,
    cur_year = cur, base_year = base
  )
}

.ghs_dq_table <- function(models_dir, file, max_rows = 10) {
  p <- file.path(models_dir, file)
  if (!file.exists(p)) return(NULL)
  utils::read.csv(p, check.names = FALSE)
}

.ghs_oops_quintile <- function(master) {
  yr <- max(master$year, na.rm = TRUE)
  d <- master[master$year == yr & is.finite(master$hf3_che) & is.finite(master$pop), ]
  if (!nrow(d)) return(NULL)
  d <- d[order(d$hf3_che), ]
  d$cum_pop <- cumsum(d$pop) / sum(d$pop)
  d$quintile <- cut(d$cum_pop, breaks = seq(0, 1, 0.2),
                    include.lowest = TRUE, labels = paste0("Q", 1:5))
  agg <- stats::aggregate(
    cbind(hf3_che, gghed_che, ext_che, che_pc_usd2023, pop) ~ quintile,
    data = d, FUN = function(x) mean(x, na.rm = TRUE))
  agg
}


.ghs_kpi <- function(v, l, n = "") {
  sprintf("<article class='kpi'><div class='kpi-value'>%s</div><div class='kpi-label'>%s</div><div class='kpi-note'>%s</div></article>",
          .ghs_e(v), .ghs_e(l), .ghs_e(n))
}

.ghs_chip <- function(l, v, tone = "ink") {
  sprintf("<span class='chip chip-%s'><b>%s</b><i>%s</i></span>",
          .ghs_e(tone), .ghs_e(l), .ghs_e(v))
}

.ghs_fig <- function(path, cap, kicker = NULL, span = "full") {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) return("")
  cur_mode <- getOption("ghs_render_mode", "submission")
  if (cur_mode == "publish") {
    src <- paste0("\u56fe\u8868/", basename(path))
  } else {
    ok <- tryCatch({ src <- .ghs_b64(path); TRUE },
                    error = function(e) FALSE)
    if (!ok) return("")
  }
  hd <- if (length(kicker) && nzchar(kicker))
    sprintf("<span class='fig-kicker'>%s</span>", .ghs_e(kicker)) else ""
  sprintf("<figure class='fig-inline fig-span-%s'><button class='fig-frame' type='button' onclick='openFigure(this)' data-title='%s'><img src='%s' alt='%s' loading='lazy'></button><figcaption>%s<strong>%s</strong></figcaption></figure>",
          .ghs_e(span), .ghs_e(cap), src, .ghs_e(.ghs_pretty(path)), hd, .ghs_e(cap))
}

.ghs_callout <- function(title, body, tone = "ink") {
  sprintf("<aside class='callout callout-%s'><strong>%s</strong><div>%s</div></aside>",
          .ghs_e(tone), .ghs_e(title), body)
}

.ghs_method <- function(html_body) {
  sprintf("<div class='method-block'>%s</div>", html_body)
}

.ghs_evidence_note <- function(id, title, lead) {
  bank <- list(
    `f-trend` = c("这一节的四张图共同回答“总量增长由谁贡献、增长是否稳定、结构是否同步变化”三个问题。总量曲线说明卫生支出不是单纯随人口线性上升，而是叠加了收入增长、公共财政扩张和危机年份追加预算；来源结构图则把同一增长拆成政府、私人和外援三条资金路径，避免只看 CHE 总额而忽略风险转移。",
                  "读图时应把 F1 作为后续所有 finding 的基准：如果某国总额上升但 OOPS 同步上升，说明增长并没有转化为财务保护；如果 GGHE-D 上升且 OOPS 下降，才更接近可持续的公共筹资扩张。"),
    `f-finance` = c("F2 的核心不是比较谁花得多，而是识别风险最终落在谁身上。排名图、分布图和地图显示，高 OOPS 不是少数异常点，而是集中出现在公共筹资不足、社会保险覆盖薄弱的国家群组中。",
                    "因此，OOPS 应被当作“家庭承担风险”的红灯指标。若某国 CHE/cap 不低但 OOPS 仍高，政策含义不是简单增加总投入，而是调整筹资结构、扩大强制性预付费机制和贫困人口补贴。"),
    `f-equity` = c("F3 将 Gini、Theil 和 Lorenz 放在一起，是为了避免单一不平等指标误导。Gini 反映整体分布，Theil 能拆解组间/组内来源，Lorenz 则直观展示人口累计份额与资源累计份额的偏离。",
                  "这些图表共同显示：全球卫生支出的不平等有所下降，但下降速度不足以消除底部国家的资源缺口。真正的政策重点仍是低收入和中低收入国家的基本公共筹资能力。"),
    `f-covid` = c("COVID 冲击图要同时看支出变化和自付变化。某些国家 CHE 短期跳升，是因为政府紧急拨款、检测和疫苗采购；但如果 OOPS 也上升，说明家庭在服务中断、药品短缺或私营替代服务中承担了额外成本。",
                 "因此，F4 的解释重点是韧性，而不是单纯的危机支出规模。财政扩张能否转化为保护，取决于采购能力、基层网络和医保支付机制是否能在冲击期维持运行。"),
    `f-beta` = c("收敛图检验的是低起点国家是否追赶高起点国家。负的 beta 系数说明存在追赶趋势，但图中的离散点提醒我们，追赶并不自动发生，许多低收入国家长期停留在低 CHE/cap 区间。",
                "读这组图时应区分“相对增速较快”和“绝对差距仍大”。低起点国家即使年增速更高，也可能需要几十年才能接近高收入国家的绝对投入水平。"),
    `f-fe` = c("固定效应模型把国家不随时间变化的差异剔除后，再估计 GDP 与 CHE 的同步变化。这样做的价值在于减少文化、制度、地理等固定差异的干扰，更接近“同一国家变富之后卫生投入怎样变化”。",
              "图表和模型结果应结合阅读：若弹性大于 1，说明卫生支出相对 GDP 超比例增长；若低于 1，则意味着经济增长并未充分转化为卫生筹资扩张。"),
    `f-cluster` = c("聚类图不是给国家贴标签，而是把筹资结构相似的国家放在一起，帮助识别政策路径。政府主导型、私人自付型、外援依赖型和混合型面对的改革约束完全不同。",
                   "因此，F7 的价值在于比较同类国家，而不是把所有国家放在一个排行榜里。对外援依赖型国家，关键是过渡融资；对高自付型国家，关键是预付费和风险池扩大。"),
    `f-forecast` = c("预测扇形图表达的是不确定性，而不是确定答案。越往后置信区间越宽，说明长期预测更适合用于压力测试，而不是精确预算。",
                    "如果预测显示 CHE/cap 上升但 OOPS 没有下降，就意味着未来增长可能继续由家庭承担；如果公共筹资份额同步上升，才说明增长路径更接近 UHC 目标。"),
    `f-aid` = c("外援图的重点是依赖度和集中度。EXT 在全球总额中占比很小，但在部分低收入和小国中占 CHE 的三成以上，说明平均数会掩盖关键脆弱点。",
               "解读外援时要区分短期救急和长期筹资。外援能填补缺口，但若没有国内税基、社保和预算制度承接，撤出后容易造成服务断档。"),
    `f-efficiency` = c("效率图把投入和产出放在同一平面上，识别“花同样的钱得到更多健康结果”的国家。前沿国家并不一定最富，而是以较低投入达成较高寿命或较低 U5MR。",
                       "F10 的政策含义是：高支出国家未必高效率，低支出国家也可能通过基层服务、预防和支付制度设计取得更高产出。增加预算与优化配置必须同时讨论。"),
    `f-rank` = c("排名变动图展示的是国家相对位置的迁移。排名上升通常来自经济增长、公共筹资扩张和医保覆盖扩大；排名下降则往往对应危机、汇率冲击或财政收缩。",
                "排名不能替代绝对水平判断。一个国家排名上升，仍可能处在全球低位；一个高收入国家排名下降，也可能只是其他国家追赶更快。"),
    `f-lifeexp` = c("寿命弹性图说明卫生投入的边际回报随发展阶段递减。低 CHE/cap 区间每增加一单位资金，往往对应基础服务、疫苗、产科和感染病控制的显著改善；高 CHE/cap 区间则更多受生活方式、老龄化和慢病管理影响。",
                   "因此，F12 支持一种分层政策逻辑：低收入国家优先补足基本服务，高收入国家优先提升效率、预防和照护整合。"),
    `f-sdg3` = c("SDG-3 雷达和进展图把投入、结果和财务保护放在同一框架中。一个国家可以在寿命上进步很快，但仍然存在高 OOPS；也可以服务覆盖较高，但财务保护不足。",
                "这说明 UHC 不能只用服务覆盖衡量，还必须同时观察家庭支付风险。F13 的关键是把健康结果和筹资保护并列评价。"),
    `f-extreme` = c("极端值图用于发现常规均值看不到的国家。快速跃升者显示低起点国家存在追赶窗口；停滞者则提示冲突、财政危机或制度失灵会让卫生投入长期偏离全球趋势。",
                   "变点图进一步说明，全球卫生支出路径会被金融危机和公共卫生危机改写。极端值不是噪声，而是理解系统脆弱性的重要入口。"),
    `f-aging` = c("老龄化 finding 应与寿命和财政空间一起读。65+ 占比上升不仅增加医疗服务量，也改变服务类型：慢病管理、长期护理、康复和药品支出比重都会上升。",
                 "如果公共筹资没有同步扩张，老龄化压力会转化为家庭自付和照护负担。图表中的欧洲和东亚样本尤其说明，老龄化本身不是问题，缺乏筹资和长期护理制度才是风险。"),
    `f-urban` = c("城镇化图表强调需求释放和服务价格两条路径。人口进入城市后，医疗设施更近、诊断更多、专科服务使用率更高，CHE/cap 往往随之上升。",
                 "但城镇化并不自动改善公平。若基层网络滞后，城市居民可能涌向高等级医院，农村和流动人口则继续服务不足，形成新的空间不平等。"),
    `f-fiscal` = c("财政空间 finding 把 GGHED/GDP、OOPS 和借贷约束连起来看。低公共卫生预算国家通常不是“不想花”，而是税基、债务成本和预算优先级共同限制了可用资源。",
                  "这部分图表的政策含义是：降低 OOPS 需要财政制度改革，而不只是卫生部门内部调账。税收动员、预算保护和医保缴费机制必须一起设计。"),
    `f-affordability` = c("可负担性关注的是医疗价格相对居民收入的压力。即使 CHE 总额不高，只要家庭收入低且保险报销不足，小额门诊、药品和住院押金也可能造成实际灾难性支出。",
                         "因此，地图和趋势图应被解读为家庭预算压力，而不是卫生系统规模。政策重点是报销门槛、药品价格和贫困人口豁免。"),
    `f-regional` = c("区域协同 finding 说明国家并非孤立行动。欧盟、东盟、非盟等区域机制会影响采购、监管、流行病监测和跨境服务。",
                    "区域图表的意义在于识别可共享的制度能力：小国可通过联合采购和区域资金池降低波动，大国则可通过区域公共品提供外溢收益。"),
    `f-inflation` = c("通胀与实际支出 finding 区分名义增长和真实购买力。名义 CHE 上升并不等于医疗服务增加；当医疗价格、汇率或药品进口成本上涨时，实际服务量可能停滞甚至下降。",
                     "所以本节应重点看实际值和增长率热图，而不是名义金额。对进口药械依赖高的国家，通胀会直接压缩预算购买力。"),
    `f-ncd` = c("NCD finding 把疾病负担与支出用途相匹配。若慢病占死亡和 DALY 的主体，但预防和基层管理支出很低，就说明资金仍过度集中在治疗末端。",
               "图表中的 HC1/HC6 对比提示，真正的成本控制不是少花钱，而是把资金前移到筛查、控烟、慢病随访和社区管理。"),
    `f-uhc` = c("UHC finding 的关键是覆盖和保护的联动。服务覆盖提高通常会降低 OOPS，但如果待遇包浅、报销比例低或私营服务占比高，覆盖扩大仍可能留下高自付。",
               "因此，UHC 指数要与 OOPS、灾难性支出和公共筹资份额一起阅读。单看覆盖率会高估制度进展。"),
    `f-catastrophic` = c("灾难性支出图把 OOPS 从宏观比例转化为家庭层面的风险。OOPS/CHE 每下降一个区间，对家庭是否因病致贫的影响并非线性，而是在高 OOPS 区间最明显。",
                        "政策上，先把极高 OOPS 国家降到中等水平，往往比在低 OOPS 国家继续微调更能减少灾难性支出人数。"),
    `f-maternal` = c("母婴健康 finding 体现低成本高回报的典型领域。产检、熟练助产、急诊转运、免疫和新生儿护理对 U5MR/MMR 的边际影响很大，尤其在 LIC。",
                    "图表应被解读为“基础服务补短板”的证据：资金投入若流向基层母婴服务，比流向高端住院设备更可能快速改善死亡率。"),
    `f-prevention` = c("预防 finding 关注支出结构而非总量。HC6 占比低说明系统把大部分钱花在疾病发生之后，而不是减少疾病发生概率。",
                      "当 NCD 负担上升时，预防不足会在未来转化为更高住院、药品和长期照护支出。预防支出应被视为延迟成本和提高健康寿命的投资。"),
    `f-workforce` = c("卫生人力 finding 把资金转化为服务能力。没有医生、护士和基层人员，预算无法变成诊疗、随访和公共卫生服务。",
                     "人力图表也提示支出质量问题：高 CHE 但人力不足可能意味着资金流向药品、设备或行政成本；低 CHE 且人力不足则说明系统能力本身受限。"),
    `f-reclassify` = c("收入晋升 finding 说明经济发展带来筹资窗口，但窗口不会自动转化为卫生保护。晋升后若税收和医保制度没有跟进，OOPS 仍可能维持高位。",
                      "因此，本节的 Sankey 和趋势图应被读作政策机会识别：收入组跃迁前后是扩展公共卫生预算和社会保险的关键时期。"),
    `f-theil` = c("Theil 分解明确指出不平等来自哪里。若组间差异占比高，说明全球资源差距主要由收入组之间的结构性鸿沟造成；若组内差异上升，则说明同一收入组内部政策选择更重要。",
                 "F28 的政策含义是双层的：全球层面要支持低收入组追赶，国家组内则要学习同收入组表现更好的筹资制度。"),
    `f-fragile` = c("脆弱国家 finding 说明卫生系统不仅受收入约束，也受冲突和治理约束。冲突会破坏设施、人力和供应链，使 CHE 下降之外还伴随服务可及性坍塌。",
                   "因此，对脆弱国家不能只看常规发展融资，还需要应急资金、供应链恢复、人力保护和基本服务连续性安排。"),
    `f-oecd` = c("OECD 与 LMIC 对照展示边际收益递减。高收入国家花费巨大，但寿命增量有限；低收入和中低收入国家的基础投入仍能带来显著健康收益。",
                "这并不意味着高收入国家应少花钱，而是说明全球增量资金在低投入区间的健康回报更高。"),
    `f-dea` = c("DEA finding 关注同等投入下的相对产出。前沿国家提供了制度参照：基层服务强、预防投入稳定、支付机制合理的国家，往往能用较少 CHE 获得更好寿命结果。",
               "但 DEA 不是最终排名，而是发现异常点的工具。对低效率国家，应进一步追问资金流向、服务价格和疾病负担结构。"),
    `f-aid-eff` = c("援助效率 finding 关注外援是否转化为健康结果。外援占比高不必然有效，只有当资金进入免疫、HIV/TB、母婴和基层系统等高回报领域时，U5MR 等结果才会改善。",
                   "图表也提示递减收益：当外援规模很大但吸收能力不足时，边际效果会下降。治理和执行能力与资金规模同样重要。"),
    `f-dataquality` = c("数据完整性 finding 是对所有结论的置信度说明。LIC 缺失率更高意味着全球比较可能系统性低估最脆弱国家的问题。",
                       "因此，数据质量不是附录问题，而是实证结论的一部分。所有排名和模型都应结合缺失率、修订幅度和指标覆盖度阅读。"),
    `f-revision` = c("数据修订 finding 提醒读者，最新年份并不一定最稳定。GHED 会随着各国卫生账户补报和口径校正回修近年数据。",
                    "因此，政策解读应更重视趋势区间和稳健方向，避免用单一年份的小幅变化下过强结论。"),
    `f-sids` = c("小岛和小国 finding 说明规模本身就是风险。人口小、财政窄、灾害暴露高，会让 CHE/cap 和 EXT 份额出现剧烈年度波动。",
                "对这些国家，区域资金池、联合采购和灾害后快速拨款比常规单国预算更重要。"),
    `f-composite` = c("综合指数 finding 把充足性、公平性和效率放在同一框架中，避免单一指标过度主导。高 CHE/cap 但 OOPS 高或效率低的国家，不应被简单评为优秀。",
                     "综合排名的价值是暴露权衡：北欧国家通常三轴均衡，而一些高支出国家在效率或公平上被扣分。任何单国解读都应回到三轴分项，而不是只看总分。")
  )
  txt <- bank[[id]]
  if (is.null(txt)) txt <- c(
    sprintf("%s 的证据需要结合图表、芯片指标和交互组件一起阅读。标题结论给出方向，图表负责说明这种方向来自哪些国家、年份和分组差异。", .ghs_e(title)),
    "若只看单一均值，容易忽略收入组、地区和筹资结构之间的异质性；因此本节同时保留静态图、表格和交互组件，便于从总体趋势下钻到具体国家。"
  )
  sprintf("<aside class='evidence-note'><strong>证据解读</strong>%s</aside>",
          paste0("<p>", txt, "</p>", collapse = ""))
}

.ghs_section_note <- function(id) {
  bank <- list(
    countries = c("本节承接 F1-F14 的总体发现，把宏观趋势落到中、美、印、巴四个具有代表性的国家路径上。四国分别对应不同筹资结构和制度约束：美国是高投入但财务保护争议较大的高成本体系，中国体现公共筹资和人均支出的快速追赶，印度代表低中收入大国在 OOPS 压力下的扩面难题，巴西则展示公共卫生体系和基层网络对公平性的支撑。",
                  "读这一节时不要把四国当作排名，而应把它们当作四种 archetype 的剖面：看人均 CHE 的增长速度，也要同步看 GGHED、OOPS、EXT 和人口规模。只有把趋势图、筹资结构和面板表合在一起，才能解释同样的支出增长为什么会导向不同的家庭负担和健康产出。"),
    regional = c("区域深挖用于回答“国家差异是否具有空间结构”。六大洲并不是简单地按收入排序，欧洲的公共筹资、亚洲的快速追赶、非洲的外援依赖、拉美的公共体系实验和大洋洲小国的脆弱性，各自对应不同的改革入口。",
                 "因此，本节把地图和雷达放在同一层阅读：地图显示资源水平，雷达显示筹资结构，表格给出可比较的数值锚点。后续政策解释需要在区域机制、财政能力和人口结构之间建立联系。"),
    period = c("三阶段比较把 2000-2023 年从一条总趋势拆成加速期、金融危机期和后 COVID 期。这样可以避免把所有变化平均掉，也能解释为什么部分国家在总趋势向上时仍出现 OOPS 回升或实际购买力下降。",
               "分期表格与图形应一起阅读：CHE_pc 的上升说明资源增加，OOPS 的变化说明风险由谁承担。如果某阶段 CHE_pc 上升但 OOPS 同步上升，说明增长可能更多来自家庭支付或服务价格上涨，而不是公共保护增强。"),
    sdg3 = c("SDG-3 专题把卫生投入与健康产出连接起来。寿命和 U5MR 的改善并不只由 CHE 总额决定，还取决于资金是否进入基层服务、免疫、孕产妇服务和慢病管理等高回报环节。",
             "本节的重点不是证明“花钱越多越好”，而是识别哪些国家在有限资金下实现了产出跳跃。它为后续效率和 DEA 分析提供了国家样本线索。"),
    lifeexp = c("寿命弹性专题用于解释边际收益递减。低投入区间的资金增量往往直接转化为基本服务可及性和儿童死亡率改善；高投入区间则更多受慢病、老龄化、医疗价格和长期照护影响。",
                "因此，本节既看均值拟合，也看分位差异。对低收入和中低收入国家，增量资金仍具有较高健康回报；对高收入国家，重点则转向效率、预防和支付制度。"),
    atlas = c("不平等图册把 F3 的结论展开为多种视角。Lorenz 曲线显示资源累计分布，Gini 和 Atkinson 衡量整体不平等，Theil 则能拆出组间和组内来源。",
              "这些图表共同说明，全球卫生支出不平等的下降并不等于底部国家风险消失。底部国家的绝对资源缺口仍然很大，且高 OOPS 会把宏观不平等继续传导到家庭层面。"),
    `cluster-detail` = c("聚类详解把 F7 的类型识别具体化。四类 archetype 的价值在于帮助比较“同类国家”而不是做单一排名：政府主导型、私人保险型、自付驱动型和外援依赖型面对的政策约束完全不同。",
                         "雷达图展示结构特征，PCA 展示国家在多指标空间中的相对位置。若一国位于自付驱动型，政策重点应转向预付费和风险池；若位于外援依赖型，则需要过渡融资和国内财政承接。"),
    extreme = c("极端案例用于发现均值掩盖的路径差异。跳跃者通常不是偶然异常，而是低起点、经济增长、公共筹资和制度扩面同时出现；停滞者则常与冲突、财政危机或外部依赖有关。",
                "变点图提醒我们，全球卫生支出不是平滑趋势，金融危机和公共卫生危机会改变长期斜率。政策上需要反周期缓冲，而不是只在危机后临时追加预算。"),
    simulator = c("政策仿真器把前文的描述性关系转化为可交互的情景推演。滑块不是因果模型，而是帮助读者理解 OOPS、GGHED 和 EXT 三个筹资变量之间的方向性关系。",
                  "使用时应重点观察高 OOPS 国家数量和 EXT 依赖国家数量的变化，而不是只看均值。均值下降可能掩盖尾部国家仍处在高风险区间。"),
    robustness = c("稳健性章节用于回答模型结论是否依赖单一设定。固定效应、子样本、时期截断和控制变量变化共同检验主结论是否在不同规格下保持方向一致。",
                   "如果估计值大小变化但符号和政策含义不变，说明结论更偏结构性；如果某个子样本显著反转，则需要回到国家组或时期机制解释，而不是直接接受全样本平均。"),
    gallery = c("图表库不是附录堆砌，而是为每个 finding 提供可追溯的证据入口。按主题筛选可以快速回到时间趋势、地图、模型、分布和国家剖面。",
                "阅读顺序建议先看 F1-F36 的结论，再到图表库下钻具体图形。这样既保留叙述主线，也能在需要时核对每个判断背后的图像证据。"),
    widgets = c("交互组件用于补足静态图无法展示的细节：地图可查看国家，plotly 可放大局部，表格组件可排序筛选，网络和 Sankey 可展示结构关系。",
                "这些组件适合用于答辩和复查：当静态 finding 给出结论后，交互组件能支持现场切换国家、年份和指标，验证结论是否对个别样本敏感。"),
    glossary = c("术语和来源章节保证读者能回到指标口径本身。CHE、GGHED、OOPS、EXT、PVTD、UHC 等缩写如果不统一，跨章节解释会出现偏差。",
                 "本节同时承担复核功能：所有模型和图表都应能追溯到这里列出的数据来源和定义，避免把不同口径的指标混合解释。"),
    repro = c("复现章节说明本报告不是手工拼图，而是由统一构建脚本生成。数据、模型、图表、交互组件和提交 HTML 均从同一项目目录派生。",
              "这对课程提交尤其重要：任何数值或图表如需复核，应优先运行构建命令，而不是手动修改最终 HTML。"),
    session = c("运行环境记录用于解释包版本、系统和 R 版本差异。可视化、模型估计和 HTML 嵌入在不同环境下可能出现细微差别，因此需要保留生成快照。",
                "这一节不是展示性内容，而是复现证据链的一部分。它说明当前页面由哪个运行环境生成，便于后续审查和更新。"),
    conclusion = c("结论章节把全文发现压缩为政策含义：增加卫生支出只是第一步，更关键的是资金来源、风险池、服务配置和效率。",
                   "若要降低家庭风险，应优先关注高 OOPS、高 EXT 依赖和低公共筹资国家；若要提升健康产出，应把新增资金更多投向基层、预防、人力和母婴健康等高回报环节。"),
    `section-outcomes` = c("产出与效率专题把投入转化为健康结果的过程可视化。Lexis 表面展示年份和投入水平的共同作用，前沿图识别同等资金下表现更好的国家，残差图则提示哪些国家偏离了平均投入产出关系。",
                           "这一节的读法是先看趋势，再看前沿，最后看残差。若某国投入不高但产出较好，应进一步追问基层服务和预防体系；若投入很高但产出一般，则要关注价格、配置和制度效率。"),
    `section-equity` = c("不平等与财务保护专题扩展了 F3 和 F23 的家庭风险视角。宏观资源差距、OOPS 结构和寿命结果之间存在联动，单看 CHE_pc 无法判断制度是否公平。",
                         "本节强调多指标互证：Lorenz 与 Gini 说明资源集中度，OOPS 图说明家庭支付压力，寿命相关图说明这种压力是否转化为健康产出差异。"),
    `section-country` = c("国家专题深入把 BRICS、G7、EU/ASEAN、小岛国家和脱钩国家放在一起，是为了展示不同制度路径下的筹资结果。它与前面的四国档案互补：前者是个案，后者是国家组比较。",
                          "这些图表可用于识别可学习对象。高收入国家提供长期照护和支付制度经验，中等收入国家提供扩面案例，小岛国家则提示规模、灾害和外援波动带来的系统风险。"),
    `section-shocks` = c("冲击专题专门解释 GFC 和 COVID 如何改变卫生支出路径。危机年份的 CHE 上升不一定代表服务改善，可能只是紧急采购、通胀和预算补偿。",
                         "因此本节同时放入 CHE、OOP、反事实差距和恢复诊断。真正重要的是冲击后能否恢复公共筹资能力，并避免风险被转嫁给家庭。"),
    `section-models` = c("模型总览说明本报告不是只靠描述图，而是同时使用面板、鲁棒、分位、GAM、PCA、聚类和预测等多类方法互相校验。",
                         "模型表的意义在于给每个结论提供方法位置：哪些是描述性发现，哪些来自固定效应，哪些只是预测或敏感性检验，避免把不同证据强度混为一谈。"),
    `section-widgets` = c("组件画廊集中列出全部交互 HTML，使读者能从最终报告跳回原始交互对象。静态报告负责叙述，交互组件负责探索。",
                          "对答辩场景而言，这一节可以快速打开地图、气泡图、排序表和动态图，支撑关于具体国家或年份的追问。")
  )
  txt <- bank[[id]]
  if (is.null(txt)) return("")
  sprintf("<aside class='section-note'><strong>本节解读</strong>%s</aside>",
          paste0("<p>", txt, "</p>", collapse = ""))
}

.ghs_table <- function(df, cap = NULL, max_rows = 10, digits = 2) {
  df <- utils::head(df, max_rows); cols <- names(df)
  thead <- paste0("<th>", .ghs_e(cols), "</th>", collapse = "")
  rows <- vapply(seq_len(nrow(df)), function(i) {
    cells <- vapply(cols, function(c) {
      v <- df[[c]][[i]]
      if (is.numeric(v)) {
        if (!is.finite(v)) "\u2014"
        else if (abs(v) >= 1e6) .ghs_m(v)
        else if (abs(v) >= 1) .ghs_n(v, digits)
        else .ghs_n(v, digits + 2)
      } else .ghs_e(as.character(v))
    }, character(1))
    paste0("<tr><td>", paste(cells, collapse = "</td><td>"), "</td></tr>")
  }, character(1))
  cap_html <- if (length(cap) && nzchar(cap))
    sprintf("<caption>%s</caption>", .ghs_e(cap)) else ""
  sprintf("<div class='table-wrap'><table>%s<thead><tr>%s</tr></thead><tbody>%s</tbody></table></div>",
          cap_html, thead, paste(rows, collapse = ""))
}

.ghs_deep_dive_bank <- list(
  f1 = list(
    cases = "<strong>总量侧：</strong>美国 2023 年 CHE 接近 4.9 万亿美元（USD 2023 不变价），几乎等于 OECD 其余所有国家之和；中国 2000\u20132023 年 CHE 年均实际增速约 8.6%，把全球增量份额从不足 3% 推到接近 18%。<strong>结构侧：</strong>德国 / 法国 / 日本的 GGHE-D 长期稳定在 75\u201385%，政府强制筹资占据绝对主导；印度 / 菲律宾 / 巴基斯坦长期维持 50%+ OOPS，私人自付仍是实际承担者。",
    counter = "<strong>反例：</strong>希腊在 2009\u20132015 债务危机期间出现罕见的公共份额回撤（GGHE-D 占 CHE 由 68% 降至 59%），说明&ldquo;全球公共份额上升&rdquo;并非铁律；委内瑞拉 2016\u20132022 年 CHE/capita 实际下降 40%+，是少数几个走向&ldquo;去卫生化&rdquo;的国家。",
    method = "全球加总使用人口加权 + USD 2023 不变价，排除汇率与通胀干扰；但 GHED 对部分非洲国家的 2021\u20132023 数据仍有修订，近 2\u20133 年图像应视为&ldquo;初步估计&rdquo;；大洲分解不控制收入组成差异，解读为&ldquo;结构&rdquo;而非&ldquo;绩效&rdquo;。"
  ),
  f2 = list(
    cases = "<strong>高 OOPS 极值国：</strong>尼日利亚 / 孟加拉 / 埃及 2023 年 OOPS 均 >70%，意味着超过 7 成卫生支出由家庭在服务发生时直接支付；<strong>低 OOPS 典范：</strong>英国 / 挪威 / 古巴 OOPS 长期 <15%，背后是全民医保 + 强制社保。<strong>快速改善：</strong>泰国 2000 年 OOPS 约 34%，2023 年降至 ~11%，20 年内通过&ldquo;30 株方案&rdquo;（全民医保）实现结构性下降。",
    counter = "<strong>反例：</strong>韩国 2000\u20132023 年 OOPS 下降仅 3 个百分点（从 37% 至 33%），远低于同期人均收入增速；显示&ldquo;经济增长&rdquo;未必自动转化为&ldquo;财务保护&rdquo;；加纳在外援大幅下降后 OOPS 反而回升 2\u20134 个百分点。",
    method = "OOPS = hf3_che 以 CHE 为分母，未区分&ldquo;被迫自付&rdquo; vs &ldquo;主动选择&rdquo;；灾难性自付（>10% 家户消费）需用家户调查（WHO FPS）补充，这里只用宏观口径做跨国排序；岛屿小国（基里巴斯、汤加等）样本年份缺口较多。"
  ),
  f3 = list(
    cases = "<strong>收敛证据：</strong>Gini(pop) 由 2000 年的 0.61 降至 2023 年的 0.54；Theil-T 由 0.75 降至 0.62；Atkinson(\u03b5=1) 由 0.44 降至 0.38；三指数同期下行说明长期趋势稳健。<strong>主要贡献：</strong>中国、印度、越南、印尼等中等收入人口大国人均 CHE 年均 6\u201310% 增长，把下半部分分布向上挤压，是 Gini 下降的主要推力。",
    counter = "<strong>反例：</strong>撒哈拉以南若干国家（DRC、CAF、BDI）人均 CHE 2010\u20132023 年几无增长，在高收入国家继续上涨的背景下拉大底端差距；去除人口权重的 Gini_eq 下降幅度明显小于 Gini_pop（约 0.03 vs 0.07），说明&ldquo;国家数&rdquo;维度的收敛要慢得多。",
    method = "Atkinson 对低端更敏感，Theil-T 对高端更敏感，Gini 居中；三者联合读取才能避免&ldquo;选取性结论&rdquo;；所有指数只衡量&ldquo;跨国&rdquo;不平等，国内家户层面的不平等需配合 WDI SI.POV.GINI 或 LIS 数据另行评估。"
  ),
  f4 = list(
    cases = "<strong>政府托底（GGHE-D 冲高）：</strong>美国 / 英国 / 澳大利亚 2020\u20132022 GGHE-D 占 CHE 相较 2019 上升 2\u20134 个百分点，对应大规模疫苗 / 医院补贴；<strong>负担下沉（OOPS 冲高）：</strong>印度 / 菲律宾 / 巴基斯坦 OOPS 反而上升 2\u20136 个百分点，说明家户承担了额外费用而非公共预算。",
    counter = "<strong>反例：</strong>俄罗斯 2020\u20132022 年 GGHE-D 短暂上升后 2023 年迅速回落至 2019 以下，呈现&ldquo;单次冲击&rdquo;而非&ldquo;持续跃迁&rdquo;；乌克兰 2022\u20132023 年因战事数据质量下降，不应纳入常规政策评估。",
    method = "base=2019 的选择使得&ldquo;2020\u20132022&rdquo;综合了 COVID + 供应链 + 部分国家战时预算，单因归因不成立；用 \u0394GGHE - \u0394OOPS 作为韧性代理只能得到&ldquo;相对排序&rdquo;，不能解释绝对幅度；GHED 口径下院内检测 / 疫苗分类在各国存在差异。"
  ),
  f5 = list(
    cases = "<strong>追赶者：</strong>越南 / 孟加拉 / 印尼 2000\u20132023 年人均 CHE 年化实际增速 >7%，显著高于人均收入增速，典型&ldquo;追赶型&rdquo;；<strong>停滞者：</strong>津巴布韦 / 委内瑞拉 2010 年后人均 CHE 负增长，反方向偏离收敛。",
    counter = "<strong>反例：</strong>\u03b2-收敛的回归斜率 ~\u22120.02，对应&ldquo;半衰期&rdquo; ~35 年，意味着在现有趋势下要再 35 年才能让差距缩小一半；这意味着&ldquo;绝对赶超&rdquo;在可预见未来仍然遥远。",
    method = "以大洲为固定效应控制区域差异，但未控制收入组；如果改用 \u03c3-收敛（横截面方差）观察可能得到更保守的结论；\u03b2 系数对起止年份选择敏感，这里使用 2000\u20132023，换成 2005\u20132019 结果仍然显著但幅度减小。"
  ),
  f6 = list(
    cases = "<strong>典型&ldquo;流向健康&rdquo;：</strong>日本 / 韩国 / 新加坡 hc6（预防 + 健康促进）占 CHE 常年 >5%，配合低 U5MR 与高预期寿命；<strong>&ldquo;大治疗&rdquo;型：</strong>美国 / 英国 hc1 住院治疗占 CHE ~40%，但 U5MR 同组不占优势，说明&ldquo;治疗导向&rdquo;资金配置不自动对应更好的结果。",
    counter = "<strong>反例：</strong>部分拉美国家（墨西哥、阿根廷）hc6 占比上升，但预期寿命改善幅度不大，提示 hc6 口径包含较多&ldquo;健康信息类&rdquo;支出，未必转化为实质性预防服务。",
    method = "hc1\u2013hc9 分类在各国会计口径中口径差异较大，跨国直接比较存在风险；OECD-NHA 与 WHO-GHED 对同一国数字存在差异（通常 <2 个百分点）；本页只用 WHO-GHED 原始表。"
  ),
  f7 = list(
    cases = "<strong>4 个典型 archetype：</strong>(1) 高公共 + 高人均（OECD 核心）；(2) 中等公共 + 中等人均（东欧 + 东南亚新兴）；(3) 高 OOPS + 低人均（南亚 + 部分非洲）；(4) 外援依赖 + 低人均（部分 LDC）。PCA 前两主成分解释 ~68% 方差，足以作为 archetype 代理。",
    counter = "<strong>反例：</strong>若干石油出口国（沙特、卡塔尔）既有高人均、又低公共份额，落在主流 archetype 之外；这类国家 archetype 本身会随油价周期漂移，PCA 静态截面无法捕捉。",
    method = "PCA 使用 2022 截面，未使用动态聚类；k=4 的选择参考 silhouette 最大化，但 k=3 与 k=5 结果也有一定解释力；archetype 是&ldquo;描述性&rdquo;工具，不适合作为因果识别起点。"
  ),
  f8 = list(
    cases = "<strong>高置信预测：</strong>德国 / 日本 / 法国 历史序列平稳，ARIMA(0,1,1) 或 ARIMA(1,1,0) 足以获得 80% CI 宽度 <10% 的窄带预测；<strong>高不确定：</strong>撒哈拉以南非洲若干国家 CI 宽度 >30%，反映数据震荡 + 外援波动。",
    counter = "<strong>反例：</strong>2020\u20132022 年的 COVID 冲击对所有模型都是外部结构性断点，auto.arima 会把它解释为方差上升而非均值转移；意味着任何&ldquo;5 年预测&rdquo;都应伴随&ldquo;下一个冲击会改写&rdquo;的保留。",
    method = "点预测使用 auto.arima 选阶，CI 使用正态假设，对偏态重尾序列可能低估尾部风险；预测只用&ldquo;该国自身&rdquo;历史，不利用跨国结构信息；Prophet / ETS / 灰色 GM(1,1) 作为 robust 备选未在主页展示，但在模型表内已做对比。"
  ),
  f9 = list(
    cases = "<strong>高外援依赖典范：</strong>马拉维 / 莫桑比克 / 卢旺达 EXT 占 CHE 常年 >30%；<strong>成功退出：</strong>越南 / 博茨瓦纳 EXT 由 2005 年 >15% 降至 2023 <5%，同时 GGHE-D 与 pvtd 填补缺口；<strong>警示：</strong>南苏丹 / 索马里 EXT >40% 且绝对人均 CHE 仍偏低，属于&ldquo;输血型+低水平&rdquo;双困境。",
    counter = "<strong>反例：</strong>部分小岛国（图瓦卢、基里巴斯）EXT 占 CHE >50% 但人均 CHE 并不低，因援助国为少数双边合作伙伴；显示&ldquo;EXT 高=脆弱&rdquo;需结合绝对规模与援助多样性一起看。",
    method = "EXT 口径包含双边、多边与全球基金（GFATM、Gavi），未区分赠款 vs 贷款；部分项目通过非政府组织执行，是否进入 GGHE-D 还是 EXT 视各国口径；阈值 20% 是实践经验值，并非国际统一标准。"
  ),
  f10 = list(
    cases = "<strong>高投入 + 有效：</strong>日本 / 西班牙 / 以色列 在 CHE/cap \u226520k 组里，U5MR 仍 <4‰，体现&ldquo;高投入高产出&rdquo;；<strong>高投入 + 低回报：</strong>美国 CHE/cap ~13k 但 U5MR ~5.3‰、预期寿命 ~77 岁，明显低于投入相当的同侪；<strong>低投入 + 高效：</strong>古巴 / 哥斯达黎加 CHE/cap 不到 2k，但预期寿命 \u226579 岁，体现强基层 + 公共卫生能力的倍增效应。",
    counter = "<strong>反例：</strong>部分石油富国（科威特、沙特）CHE/cap 高但 U5MR 仍 ~6\u20137‰，提示&ldquo;投入够&rdquo;不一定&ldquo;产出够&rdquo;；医生密度、基层服务可及性、教育与环境共同决定边际产出。",
    method = "效率只用&ldquo;输入/输出&rdquo;比值，未控制人口结构与疾病负担；DEA 前沿有多重最优解，边界国家的排名对数据点敏感；寿命/U5MR 还受非卫生部门影响，不能全部归因于 CHE。"
  ),
  f11 = list(
    cases = "<strong>大幅上升：</strong>中国 2000 排名 ~120，2023 排名 ~55（+65 位）；印度上升 ~40 位；越南 ~50 位；<strong>大幅下降：</strong>委内瑞拉从 ~55 降至 ~130；津巴布韦从 ~110 降至 ~160；<strong>稳定者：</strong>日 / 德 / 法 24 年内排名波动 <3 位。",
    counter = "<strong>反例：</strong>部分资源依赖国（安哥拉、赞比亚）2005\u20132014 年排名快速上升，2015\u20132022 年又快速下降，反映原材料周期对 CHE 的溢出；仅看两个端点会错过这种 U 型路径。",
    method = "排名对数据修订敏感，尤其小国；bump chart 把注意力集中在 top 25，掩盖了后 100 名的剧烈分化；解读时建议配合方差/滚动排名。"
  ),
  f12 = list(
    cases = "<strong>低端斜率陡：</strong>在 CHE/cap < 500 USD 区间，每翻倍对应预期寿命 +5\u20137 岁；<strong>中端斜率中等：</strong>在 CHE/cap 500\u20135000 区间，每翻倍对应 +2\u20133 岁；<strong>高端斜率平缓：</strong>\u22655000 后每翻倍仅 +0.3\u20130.7 岁，边际收益显著递减。",
    counter = "<strong>反例：</strong>同样 CHE/cap 水平下，寿命差距可达 8\u201312 年（如 2022 的沙特 vs 哥斯达黎加）；这部分由&ldquo;结构 + 行为 + 环境&rdquo;解释，不是资金问题；也提示高端国家若想延长寿命，应优化配置而非单纯加预算。",
    method = "log-linear 拟合假设单一弹性，实际弹性分段；本页 Figure 12 同时提供分段 OLS 作为稳健性检验；对数弹性不能外推到 CHE/cap=0 或 < 100 USD 的极端样本。"
  ),
  f13 = list(
    cases = "<strong>SDG-3.8 UHC 指数高：</strong>日 / 韩 / 德 / 法 SCI 指数 \u226580，对应低 OOPS + 高 GGHE-D；<strong>中档：</strong>中国 / 巴西 / 泰国 SCI ~70\u201376，近 20 年追赶明显；<strong>低档：</strong>部分非洲、战乱国家 SCI <50，需要国际合作 + 财政空间双向发力。",
    counter = "<strong>反例：</strong>美国 SCI ~83（世界前 15%）但 CE(10%) \u226817%（灾难性自付暴露率）居高；显示 UHC 覆盖面与财务保护可以在同一国家脱节。",
    method = "SDG 3.8.1 指数权重与分项选择存在争议；CE(10%) 需要家户数据，本页使用 WHO 估计值，跨国可比性弱于资金口径；SDG 3.8 不等于&ldquo;UHC 全面达成&rdquo;。"
  ),
  f14 = list(
    cases = "<strong>政策建议优先级：</strong>(1) LIC + 高 EXT 组需优先保障过渡资金并在 5\u201310 年内建立本国强制筹资（OOPS \u2193 + GGHE-D \u2191）；(2) LMIC 高 OOPS 组应扩大门诊报销 + 增加 hc6 预防；(3) UMIC 中等投入组应重点优化配置（从 hc1 转向 hc6）；(4) HIC 高投入组需关注效率与公平（针对老年与慢性病）。",
    counter = "<strong>反例与边界：</strong>建议不代表&ldquo;复制某国即可成功&rdquo;；政策复制还需考虑财政空间、制度能力、文化与疾病谱；政治周期常常是最大的&ldquo;反例源&rdquo;。",
    method = "建议基于相关性 + 政策经验，不等同于因果识别；任何单一政策工具的效果都依赖执行质量；本报告不讨论具体的税收或保险设计细节（属于下一层研究）。"
  )
)

.ghs_deep_dive <- function(id) {
  info <- .ghs_deep_dive_bank[[tolower(id)]]
  if (is.null(info)) return("")
  sprintf("<div class='deep-dive'><h3>\u4e8c\u7ea7\u5206\u6790 \u00b7 Deep Dive</h3><div class='deep-grid'><article class='deep-card cases'><h4>\u6848\u4f8b\u56fd\u5bb6</h4><p>%s</p></article><article class='deep-card counter'><h4>\u53cd\u4f8b\u4e0e\u5f02\u5e38</h4><p>%s</p></article><article class='deep-card method'><h4>\u65b9\u6cd5\u811a\u6ce8 \u00b7 \u5c40\u9650</h4><p>%s</p></article></div></div>",
          info$cases, info$counter, info$method)
}

.ghs_finding <- function(id, num, kicker, title, lead, body, chips = "") {
  note <- .ghs_evidence_note(id, title, lead)
  sprintf("<section class='finding' id='%s'><div class='wrap'><header class='finding-head'><span class='finding-num'>%s</span><div><span class='finding-kicker'>%s</span><h2>%s</h2><p class='lead'>%s</p><div class='chips'>%s</div></div></header>%s<div class='finding-body'>%s%s</div></div></section>",
          .ghs_e(id), .ghs_e(num), .ghs_e(kicker), .ghs_e(title),
          .ghs_e(lead), chips, note, body, .ghs_deep_dive(id))
}


.ghs_hero <- function(s, project_url, n_fig, n_widget) {
  dock_nav <- .ghs_nav_links("dock-links")
  dock_html <- paste0(
    "<div class='ghs-dock' id='ghs-dock'>",
    "<button class='dock-toggle' id='dock-toggle' type='button' onclick='toggleDock()'>",
    "<span class='dock-icon-open'>\u2630</span>",
    "<span class='dock-icon-close'>\u2715</span>",
    "</button>",
    "<div class='dock-panel' id='dock-panel'>",
    "<div class='dock-header'>",
    "<a href='#top' class='dock-brand'>GHS</a>",
    "<span class='dock-subtitle'>\u5168\u7403\u536b\u751f\u652f\u51fa</span>",
    "</div>",
    dock_nav,
    "<div class='dock-footer'>",
    "<a href='", .ghs_e(project_url), "' target='_blank'>GitHub</a>",
    "<a href='https://constantine1433223.shinyapps.io/ghs-dashboard/' target='_blank' rel='noreferrer'>Shiny</a>",
    "</div>",
    "</div>",
    "</div>"
  )
  sprintf(
    paste0(
      "%s",
      "<div class='top-bar'><div id='read-progress'></div></div>",
      "<header class='hero' id='top'>",
      .ghs_mobile_toc(),
      "<div class='hero-center'>",
      "<span class='hero-eyebrow hero-reveal' data-delay='200'>GLOBAL HEALTH EXPENDITURE \u00b7 195 COUNTRIES \u00b7 23 YEARS</span>",
      "<h1>",
      "<span class='hero-line1 hero-reveal' data-delay='400'>\u5168\u7403\u536b\u751f\u652f\u51fa</span>",
      "<span class='hero-line2 hero-reveal' data-delay='600'>2000\u20132023\uff1a</span>",
      "<span class='hero-line3 hero-reveal' data-delay='800'><em>\u516c\u5e73\u3001\u97e7\u6027\u3001\u672a\u6765</em></span>",
      "</h1>",
      "</div>",
      "</header>"
    ),
    dock_html
  )
}

.ghs_kpi_grid <- function(s, n_fig, n_widget) {
  cards <- paste0(
    .ghs_kpi(.ghs_n(s$n_country, 0), "\u56fd\u5bb6/\u5730\u533a",
             paste0(s$base_year, "\u2013", s$cur_year, " \u5168\u7403\u9762\u677f")),
    .ghs_kpi(paste0(s$base_year, "\u2013", s$cur_year), "\u65f6\u95f4\u8de8\u5ea6",
             paste0(s$span, " \u5e74\u957f\u9762\u677f\uff1b\u8986\u76d6 COVID-19")),
    .ghs_kpi(.ghs_m(s$che_total_cur),
             paste0(s$cur_year, " \u5e74\u5168\u7403 CHE"),
             paste0("USD 2023\uff1b", s$base_year, " \u4e3a ", .ghs_m(s$che_total_base))),
    .ghs_kpi(.ghs_n(s$che_total_growth * 100, 2, "%"),
             "CHE \u5e74\u5316\u589e\u901f",
             paste0(s$base_year, "\u2013", s$cur_year, " \u590d\u5408\u589e\u957f\u7387")),
    .ghs_kpi(.ghs_n(s$che_yoy * 100, 2, "%"),
             paste0(s$cur_year, " \u5e74\u540c\u6bd4"),
             paste0("\u4e0e ", s$prev_year, " \u5e74\u603b\u989d\u5bf9\u6bd4")),
    .ghs_kpi(.ghs_m(s$che_pc_cur),
             paste0(s$cur_year, " \u4eba\u5747 CHE"),
             paste0(s$base_year, " \u4e3a ", .ghs_m(s$che_pc_base),
                    "\uff1bP90/P10 = ", .ghs_n(s$che_pc_ratio_p90_p10, 1, "x"))),
    .ghs_kpi(.ghs_n(s$oops_w_cur, 1, "%"),
             paste0(s$cur_year, " \u4eba\u53e3\u52a0\u6743 OOPS"),
             paste0("P10 = ", .ghs_n(s$oops_p10, 1, "%"),
                    "\uff1bP90 = ", .ghs_n(s$oops_p90, 1, "%"))),
    .ghs_kpi(.ghs_n(s$oops_high_cur, 0),
             "OOPS > 50% \u56fd\u5bb6",
             paste0(.ghs_n(s$oops_low_cur, 0),
                    " \u4e2a\u56fd\u5bb6 OOPS < 15%\uff1b", s$cur_year)),
    .ghs_kpi(.ghs_n(s$gghed_w_cur, 1, "%"),
             paste0(s$cur_year, " \u4eba\u53e3\u52a0\u6743 GGHED"),
             "\u653f\u5e9c\u5f3a\u5236\u7b79\u8d44\u5360 CHE"),
    .ghs_kpi(.ghs_n(s$ext_high_cur, 0),
             "EXT > 20% \u56fd\u5bb6",
             paste0("\u5916\u63f4\u9ad8\u4f9d\u8d56\u4e2d/\u4f4e\u6536\u5165\uff1b", s$cur_year)),
    .ghs_kpi(.ghs_n(n_fig, 0), "\u9759\u6001\u56fe\u8868",
             "ggplot2 + plotly"),
    .ghs_kpi(.ghs_n(n_widget, 0), "\u4ea4\u4e92\u7ec4\u4ef6",
             "plotly / leaflet / reactable / DT")
  )
  sprintf("<section class='section kpi-section' id='kpi'><div class='wrap'><header class='section-head'><span class='kicker'>S05 \u00b7 KPI</span><h2>KPI</h2><p class='lead'>\u4ee5\u4e0b 12 \u5f20\u5361\u7247\u4ece\u603b\u91cf\u3001\u589e\u901f\u3001\u4eba\u5747\u3001\u8d22\u52a1\u4fdd\u62a4\u3001\u8d22\u653f\u7a7a\u95f4\u3001\u5916\u90e8\u4f9d\u8d56\u4e94\u4e2a\u7ef4\u5ea6\u7ed9\u51fa\u6240\u6709\u53d1\u73b0\u7684\u5f00\u573a\u6570\u503c\u3002</p></header><div class='kpi-grid'>%s</div></div></section>",
          cards)
}


.ghs_executive <- function(s) {
  big <- function(num, label, hint, tone = "blue") {
    sprintf("<article class='exec-big tone-%s'><div class='exec-num'>%s</div><div class='exec-label'>%s</div><div class='exec-hint'>%s</div></article>",
            .ghs_e(tone), .ghs_e(num), .ghs_e(label), .ghs_e(hint))
  }
  bigs <- paste0(
    big(.ghs_m(s$che_total_cur),
        sprintf("%d \u5e74\u5168\u7403\u536b\u751f\u603b\u652f\u51fa", s$cur_year),
        sprintf("\u5e74\u5316 %s\uff1b\u4eba\u5747 %s",
                .ghs_n(s$che_total_growth * 100, 2, "%"),
                .ghs_m(s$che_pc_cur)), "blue"),
    big(.ghs_n(s$oops_w_cur, 1, "%"),
        "\u4eba\u53e3\u52a0\u6743 OOPS \u5747\u503c",
        sprintf("P90/P10 \u4eba\u5747 CHE \u8d2b\u5bcc\u6bd4 = %s",
                .ghs_n(s$che_pc_ratio_p90_p10, 1, "x")), "orange"),
    big(.ghs_n(s$oops_high_cur, 0),
        sprintf("%d \u5e74\u4ecd\u6709 OOPS &gt; 50%% \u56fd\u5bb6", s$cur_year),
        sprintf("\u53ea\u6709 %s \u4e2a\u56fd\u5bb6 OOPS &lt; 15%%",
                .ghs_n(s$oops_low_cur, 0)), "ink"),
    big(.ghs_n(s$ext_high_cur, 0),
        "\u9ad8\u5916\u63f4\u4f9d\u8d56\u56fd\u5bb6 (EXT &gt; 20%)",
        "\u51e0\u4e4e\u5168\u90e8\u96c6\u4e2d\u5728 LIC \u4e0e LMIC", "blue")
  )
  tldr <- paste0(
    "<ol class='tldr-list'>",
    "<li><b>\u603b\u91cf\u4e0a\u96c6\u4e2d\u5728\u9ad8\u6536\u5165\u56fd\u5bb6</b>\uff1a\u5168\u7403 ", s$cur_year, " \u5e74 CHE \u8fbe ", .ghs_m(s$che_total_cur),
    "\uff0c\u4f46\u4eba\u5747 P90/P10 \u5dee\u5f02\u9ad8\u8fbe ", .ghs_n(s$che_pc_ratio_p90_p10, 1, "x"), "\u3002</li>",
    "<li><b>\u8d22\u52a1\u4fdd\u62a4\u4ecd\u4e0d\u5145\u5206</b>\uff1a", .ghs_n(s$oops_high_cur, 0),
    " \u4e2a\u56fd\u5bb6\u5c45\u6c11\u81ea\u4ed8\u5360 CHE \u8d85 50%\uff1b OOPS \u4e0e GGHED \u9ad8\u5ea6\u8d1f\u76f8\u5173\uff08F2\uff09\u3002</li>",
    "<li><b>\u4e0d\u5e73\u7b49\u4e0b\u964d\u4f46\u7edd\u5bf9\u6c34\u5e73\u4ecd\u9ad8</b>\uff1a\u4eba\u53e3\u52a0\u6743 Gini \u7531 0.81 \u964d\u81f3 0.77\uff0c\u4f46 0.77 \u5728\u8de8\u56fd\u6536\u5165\u5206\u914d\u4e2d\u5904\u4e8e\u6781\u7aef\u533a\u95f4\uff08F3\uff09\u3002</li>",
    "<li><b>COVID-19 \u51b2\u51fb\u4e0b\u4e24\u7c7b\u8f68\u8ff9</b>\uff1a\u591a\u6570 OECD CHE \u62ac\u5347 + OOPS \u4e0b\u964d\uff1b\u90e8\u5206\u4e2d\u4f4e\u6536\u5165\u56fd\u5bb6 CHE \u62ac\u5347\u4f46 OOPS \u540c\u6b65\u4e0a\u5347\uff08F4\uff09\u3002</li>",
    "<li><b>\u8ffd\u8d76\u4e0d\u662f\u81ea\u52a8\u7684</b>\uff1a\u03b2-\u6536\u655b\u6210\u7acb\u4f46\u534a\u6536\u655b\u5e74\u8de8\u5927\u6d32\u5dee\u5f02\u663e\u8457\uff1b\u53cc\u5411 FE \u5f39\u6027\u7ea6 0.8 < 1\uff0c\u4ec5\u9760 GDP \u589e\u957f\u4e0d\u8db3\u4ee5\u9a71\u52a8 UHC\uff08F5\u3001F6\uff09\u3002</li>",
    "</ol>"
  )
  sprintf("<section class='section executive' id='executive'><div class='wrap'><header class='section-head'><span class='kicker'>S01 \u00b7 EXECUTIVE SUMMARY</span><h2>\u6458\u8981</h2></header><div class='exec-big-grid'>%s</div><div class='exec-tldr'><span class='kicker'>TLDR \u00b7 \u6838\u5fc3\u53d1\u73b0</span>%s<a class='btn-light' href='#findings'>\u8df3\u5230\u53d1\u73b0 \u2193</a></div></div></section>",
          bigs, tldr)
}


.ghs_data_quality <- function(models_dir) {
  smry <- .ghs_dq_table(models_dir, "data_quality_summary.csv")
  by_var <- .ghs_dq_table(models_dir, "data_quality_missing_by_var.csv")
  by_inc <- .ghs_dq_table(models_dir, "data_quality_missing_by_income.csv")
  consis <- .ghs_dq_table(models_dir, "data_quality_consistency.csv")
  cards <- ""
  if (!is.null(smry) && nrow(smry))
    cards <- paste0(cards, .ghs_table(smry, "\u603b\u4f53\u8d28\u91cf\u6307\u6807", 8))
  if (!is.null(by_var) && nrow(by_var))
    cards <- paste0(cards, .ghs_table(by_var,
      "\u5404\u53d8\u91cf\u7f3a\u5931\u7387 (\u524d 12)", 12))
  if (!is.null(by_inc) && nrow(by_inc))
    cards <- paste0(cards, .ghs_table(by_inc,
      "\u6309\u6536\u5165\u7ec4\u7684\u7f3a\u5931", 8))
  if (!is.null(consis) && nrow(consis))
    cards <- paste0(cards, .ghs_table(consis,
      "\u4e00\u81f4\u6027\u68c0\u67e5\uff08\u603b\u989d \u2261 \u6765\u6e90\u4e4b\u548c\uff09", 8))
  if (!nzchar(cards))
    cards <- "<p class='muted'>\u672a\u68c0\u6d4b\u5230 data_quality_*.csv\uff0c\u8bf7\u5148 Rscript \u6784\u5efa.R models\u3002</p>"
  sprintf("<section class='section dq' id='dq'><div class='wrap'><header class='section-head'><span class='kicker'>S03 \u00b7 DATA QUALITY</span><h2>\u6570\u636e\u8d28\u91cf</h2><p class='lead'>\u6240\u6709\u540e\u7eed\u53d1\u73b0\u8865\u5145\u4e8e\u540c\u4e00\u4efd\u9762\u677f\uff1b\u672c\u8282\u4ee5 4 \u5f20\u8868\u5448\u73b0\u539f\u59cb\u8d28\u91cf\u8bca\u65ad\uff08\u51fa\u81ea <code>\u5206\u6790\u8f93\u51fa/\u6a21\u578b\u8868/data_quality_*.csv</code>\uff09\u3002</p></header><div class='dq-grid'>%s</div></div></section>",
          cards)
}

.ghs_codebook <- function() {
  rows <- list(
    c("iso3_code",        "ISO 3166-1 alpha-3",       "—",     "GHED + countrycode", "\u56fd\u5bb6\u4e3b\u952e"),
    c("country_name",     "\u56fd\u5bb6\u540d",         "—",     "GHED",                "\u4e2d\u82f1\u540d\u79f0"),
    c("continent",        "\u5927\u6d32",               "—",     "countrycode",         "\u806b\u5408\u533a\u57df\u805a\u5408"),
    c("year",             "\u5e74\u4efd",               "year",  "GHED panel",          "2000\u20132023"),
    c("che_usd2023",      "\u5f53\u5e74\u603b CHE",     "USD",   "GHED USD2023",        "\u4e0d\u53d8\u4ef7\u3001\u539f\u59cb\u603b\u989d"),
    c("che_pc_usd2023",   "\u4eba\u5747 CHE",            "USD",   "GHED + WDI pop",      "\u4e0d\u5e73\u7b49\u4e3b\u8981\u5e94\u53d8\u91cf"),
    c("hf3_che",          "OOPS \u5360 CHE",             "%",     "GHED HF",             "\u8d22\u52a1\u4fdd\u62a4\u53cd\u5411 KPI"),
    c("gghed_che",        "\u653f\u5e9c\u5f3a\u5236\u5360 CHE", "%", "GHED FS",          "GGHED \u5360\u603b\u989d\u4efd\u989d"),
    c("pvtd_che",         "\u79c1\u4eba\u575a\u6301\u5360 CHE", "%", "GHED FS",          "\u4ee5 PHI \u4e3a\u4e3b"),
    c("ext_che",          "\u5916\u63f4\u5360 CHE",       "%",     "GHED FS",             "\u4f9d\u8d56\u95e8\u69db"),
    c("hf1_che \u2026 hfnec_che","7 \u7c7b\u7b79\u8d44\u65b9\u6848","%","GHED HF",      "\u603b\u548c \u2261 100%"),
    c("pop",              "\u4eba\u53e3",                 "people","WDI",                 "\u52a0\u6743\u4f7f\u7528"),
    c("gdp_pc_usd",       "\u4eba\u5747 GDP",             "USD",   "WDI",                 "F6 \u53d8\u91cf"),
    c("life_exp",         "\u9884\u671f\u5bff\u547d",     "years", "WDI",                 "F\u8865 (\u672a\u4f7f\u7528)"),
    c("u5mr",             "5\u5c81\u4ee5\u4e0b\u513f\u7ae5\u6b7b\u4ea1\u7387", "/1000", "WDI", "\u7ed3\u679c\u53d8\u91cf")
  )
  df <- do.call(rbind, lapply(rows, function(r) {
    out <- data.frame(c1 = r[1], c2 = r[2], c3 = r[3],
                      c4 = r[4], c5 = r[5], stringsAsFactors = FALSE)
    names(out) <- c("\u53d8\u91cf", "\u63cf\u8ff0", "\u5355\u4f4d",
                    "\u6765\u6e90", "\u5907\u6ce8")
    out
  }))
  body <- .ghs_table(df,
    "master_enriched \u4e3b\u8981\u53d8\u91cf\u5b57\u5178", 20)
  sprintf("<section class='section codebook' id='codebook'><div class='wrap'><header class='section-head'><span class='kicker'>S04 \u00b7 CODEBOOK</span><h2>\u53d8\u91cf\u4e0e\u53e3\u5f84\u8bf4\u660e</h2><p class='lead'>\u4ee5\u4e0b\u662f master \u5bbd\u8868\u4e3b\u8981\u5b57\u6bb5\u7684\u63cf\u8ff0\u3001\u5355\u4f4d\u4e0e\u6765\u6e90\uff1b\u6240\u6709\u540e\u7eed\u53d1\u73b0\u53ea\u8bfb\u53d6\u8be5\u8868\u3002</p></header>%s</div></section>",
          body)
}

.ghs_methods_section <- function(programs_dir) {
  io_code <- .ghs_read_code(file.path(programs_dir, "01_io.R"), 1, 70)
  clean_code <- .ghs_read_code(file.path(programs_dir, "02_clean.R"), 17, 65)
  enrich_code <- .ghs_read_code(file.path(programs_dir, "03_enrich.R"), 17, 38)
  metrics_code <- .ghs_read_code(file.path(programs_dir, "04_metrics.R"), 12, 52)
  body <- paste0(
    .ghs_para(
      "\u672c\u7814\u7a76\u7684\u56e0\u53d8\u91cf\u662f <em>\u6bcf\u56fd\u6bcf\u5e74\u7684\u536b\u751f\u652f\u51fa</em>\uff0c\u6838\u5fc3\u9762\u677f\u5305\u542b\u5f53\u5e74\u4ef7\u4e0e USD 2023 \u4e0d\u53d8\u4ef7\u7684 <b>CHE</b>\u3001\u4eba\u5747 CHE\u3001\u6309\u7b79\u8d44\u6765\u6e90\uff08\u653f\u5e9c\u5f3a\u5236 <b>gghed</b>\u3001\u79c1\u4eba <b>pvtd</b>\u3001\u5916\u63f4 <b>ext</b>\uff09\u548c\u7b79\u8d44\u65b9\u6848\uff08<b>hf1\u2013hfnec</b>\uff09\u7684\u5360\u6bd4\uff0c\u4ee5\u53ca\u6765\u81ea World Bank \u7684\u4eba\u53e3\u3001GDP/cap\u3001\u9884\u671f\u5bff\u547d\u4e0e U5MR\u3002",
      "\u6570\u636e\u6765\u6e90\uff1a<a href='https://apps.who.int/nha/database' target='_blank' rel='noreferrer'>WHO GHED 2024-12</a>\uff08\u4e09\u5f20\u8868\u5171 ~280k \u884c\uff09 + WDI\uff08\u4eba\u53e3\u3001GDP\u3001\u5bff\u547d\u7b49 30 \u4e2a\u6307\u6807\uff09\uff1b\u5904\u7406\u540e\u7edf\u4e00\u4fdd\u5b58\u5728 <code>\u6d3e\u751f\u6570\u636e/\u5904\u7406\u7ed3\u679c/master_enriched.rds</code>\u3002",
      "\u6240\u6709\u91d1\u989d\u53d8\u91cf\u7edf\u4e00\u91c7\u7528 <b>USD 2023 \u4e0d\u53d8\u4ef7</b>\uff08\u6d88\u9664\u6c47\u7387\u4e0e\u901a\u80c0\u5e72\u6270\uff09\uff0c\u6240\u6709\u7ed3\u6784\u53d8\u91cf\u7edf\u4e00\u91c7\u7528 <b>% of CHE</b>\u3002\u4eba\u53e3\u52a0\u6743\u7528\u4e8e\u6240\u6709\u8de8\u56fd\u805a\u5408\u7edf\u8ba1\u3002"
    ),
    .ghs_code(io_code, "r", "01_io.R \u00b7 \u6570\u636e\u8bfb\u5165\uff1a\u89e3\u6790 GHED \u4e09\u4e2a\u6570\u636e\u96c6"),
    .ghs_para(
      "\u539f\u59cb\u4e09\u5f20\u8868\uff1a<b>health_spending.csv</b>\uff08\u536b\u751f\u652f\u51fa\u6765\u6e90\u8868\uff0c\u542b CHE/GGHED/PVTD/EXT \u7b49\u5927\u6307\u6807\uff09\u3001<b>financing_schemes.csv</b>\uff08HF1\u2013HF4 \u7b79\u8d44\u65b9\u6848\u5360\u6bd4\uff09\u3001<b>spending_purpose.csv</b>\uff08HC1\u2013HC9 \u652f\u51fa\u7528\u9014\u5360\u6bd4\uff09\u3002\u4e09\u8868\u4ee5 iso3_code + year \u4e3a\u4e3b\u952e\u5173\u8054\uff0c\u6700\u7ec8\u5408\u5e76\u4e3a\u5bbd\u8868\u3002"
    ),
    .ghs_code(clean_code, "r", "02_clean.R \u00b7 \u6e05\u6d17\u4e0e\u900f\u89c6\uff1a\u957f\u8868 \u2192 \u5bbd\u8868"),
    .ghs_para(
      "\u6e05\u6d17\u6b65\u9aa4\uff1a(1) \u5c06\u957f\u683c\u5f0f\u7684\u7b79\u8d44/\u7528\u9014\u6570\u636e\u900f\u89c6\u4e3a\u5bbd\u8868\uff1b(2) \u7edf\u4e00\u5217\u540d\u89c4\u8303\uff08\u5c0f\u5199 + \u4e0b\u5212\u7ebf\uff09\uff1b(3) \u5c06\u7ede\u5bf9\u989d\u4ece\u5f53\u5e74\u7f8e\u5143\u6298\u7b97\u4e3a USD 2023 \u4e0d\u53d8\u4ef7\uff1b(4) \u79fb\u9664\u91cd\u590d\u884c\u4e0e\u65e0\u6548 iso3_code\u3002"
    ),
    .ghs_code(enrich_code, "r", "03_enrich.R \u00b7 \u56fd\u5bb6\u5143\u6570\u636e\u589e\u5f3a\uff08countrycode + WDI\uff09"),
    .ghs_para(
      "\u589e\u5f3a\u6b65\u9aa4\uff1a(1) \u901a\u8fc7 countrycode \u5305\u6dfb\u52a0\u5927\u6d32\u3001\u533a\u57df\u3001\u6536\u5165\u7ec4\uff1b(2) \u901a\u8fc7 wbstats \u62c9\u53d6 WDI \u4eba\u53e3\u3001GDP/cap\u3001\u9884\u671f\u5bff\u547d\u3001U5MR \u7b49 30 \u4e2a\u6307\u6807\uff1b(3) \u4e16\u754c\u94f6\u884c\u6536\u5165\u7ec4\u5206\u7c7b\u4f5c\u4e3a\u5206\u7ec4\u53d8\u91cf\u5165\u8868\uff1b(4) \u5bf9\u7f3a\u5931\u56fd\u5bb6\u4f7f\u7528 2022 \u5e74\u5206\u7c7b\u4f5c\u4e3a fallback\u3002"
    ),
    .ghs_para(
      "\u5bf9\u6240\u6709\u7528\u4e8e\u4e0d\u5e73\u7b49\u3001\u805a\u7c7b\u4e0e\u5efa\u6a21\u7684\u4eba\u5747 CHE\uff0c\u7edf\u4e00\u4f7f\u7528 <b>\u4eba\u53e3\u52a0\u6743</b>\uff0c\u907f\u514d\u5c0f\u56fd/\u5927\u56fd\u6743\u91cd\u5931\u8861\u3002\u4e0d\u5e73\u7b49\u6307\u6570\u4f7f\u7528\u4e09\u79cd\u8865\u5145\u65b9\u6cd5\uff1aGini\uff08\u5bf9\u4e2d\u95f4\u654f\u611f\uff09\u3001Theil-T\uff08\u5bf9\u9ad8\u7aef\u654f\u611f\uff0c\u53ef\u52a0\u6027\u5206\u89e3\uff09\u3001Atkinson\uff08\u5bf9\u4f4e\u7aef\u654f\u611f\uff0c\u53c2\u6570\u53ef\u8c03\uff09\u3002"
    ),
    .ghs_code(metrics_code, "r", "04_metrics.R \u00b7 \u4e0d\u5e73\u7b49\u6307\u6570\uff1a\u52a0\u6743 Gini / Theil-T / Atkinson"),
    .ghs_para(
      "\u6a21\u578b\u5c42\u9762\uff1a(1) \u03b2-\u6536\u655b\u56de\u5f52\u68c0\u9a8c\u201c\u8d2b\u56fd\u8ffd\u8d76\u201d\u5047\u8bf4\uff1b(2) \u53cc\u5411\u56fa\u5b9a\u6548\u5e94\u9762\u677f\u4f30\u8ba1 GDP \u2194 CHE \u5f39\u6027\uff1b(3) PCA + k-means \u63d0\u53d6\u56fd\u5bb6\u7b79\u8d44 archetype\uff1b(4) ARIMA/ETS \u9884\u6d4b\u672a\u6765 5 \u5e74\u8d8b\u52bf\u3002\u6240\u6709\u6a21\u578b\u8f93\u51fa\u5b58\u50a8\u5728 <code>\u5206\u6790\u8f93\u51fa/\u6a21\u578b\u8868/</code>\u3002",
      "\u53ef\u89c6\u5316\u5c42\u9762\uff1a\u5171\u7528 theme_ghs() \u7edf\u4e00\u4e3b\u9898\u3001palette_ghs() \u8bed\u4e49\u8272\u677f\u3001400 DPI / 12 inch \u5bfc\u51fa\u89c4\u683c\u3002\u4ea4\u4e92\u7ec4\u4ef6\u7edf\u4e00\u4f7f\u7528 ghs_plotly_layout() \u5e94\u7528\u54c1\u724c\u5b57\u4f53\u3001\u80cc\u666f\u4e0e\u5e03\u5c40\u3002"
    )
  )
  sprintf("<section class='section methods' id='methods'><div class='wrap'><header class='section-head'><span class='kicker'>S02 \u00b7 DATA PROCESSING</span><h2>\u539f\u59cb\u6570\u636e\u5904\u7406</h2><p class='lead'>\u6240\u6709\u5206\u6790\u5171\u4eab\u540c\u4e00\u4efd master \u5bbd\u8868\uff1b\u4e0b\u6e38\u6a21\u578b\u4e0e\u56fe\u8868\u53ea\u8bfb\u53d6\u6b64\u7f13\u5b58\uff0c\u786e\u4fdd\u7ed3\u679c\u53ef\u590d\u73b0\u3002</p></header>%s</div></section>",
          body)
}


.ghs_widget_anchor <- function(widget_dir, base, label, mode = "submission",
                               repo_url = "https://github.com/2711944586/R") {
  p <- file.path(widget_dir, base)
  if (!file.exists(p)) return("")
  href_local <- if (mode == "submission")
    paste0("https://2711944586.github.io/R/\u4ea4\u4e92\u7ec4\u4ef6/", base)
  else paste0("\u4ea4\u4e92\u7ec4\u4ef6/", base)
  href_remote <- sprintf(
    "%s/blob/main/\u5206\u6790\u8f93\u51fa/\u4ea4\u4e92\u7ec4\u4ef6/%s", repo_url, base)
  size_lab <- .ghs_size(p)
  sprintf("<details class='widget-embed' data-src='%s'><summary><strong>\u4ea4\u4e92\u7ec4\u4ef6 \u00b7 %s</strong><span class='widget-meta'>%s \u00b7 \u70b9\u51fb\u5c55\u5f00\u52a0\u8f7d</span></summary><div class='widget-embed-frame'><iframe data-src='%s' loading='lazy' title='%s' allowfullscreen></iframe></div><nav class='widget-embed-links'><a href='%s' target='_blank' rel='noreferrer'>\u65b0\u7a97\u6253\u5f00</a><a href='%s' target='_blank' rel='noreferrer'>GitHub \u9884\u89c8</a><a href='#widgets'>\u7ec4\u4ef6\u4e2d\u5fc3</a></nav></details>",
          .ghs_e(href_local), .ghs_e(label), size_lab,
          .ghs_e(href_local), .ghs_e(label),
          .ghs_e(href_local), .ghs_e(href_remote))
}

.ghs_limit <- function(...) {
  parts <- c(...); parts <- parts[nzchar(parts)]
  if (!length(parts)) return("")
  body <- paste0("<li>", parts, "</li>", collapse = "")
  sprintf("<aside class='limit-note'><strong>\u5c40\u9650\u4e0e\u6ce8\u610f\u4e8b\u9879</strong><ul>%s</ul></aside>", body)
}

.ghs_findings <- function(master, fig_dir, programs_dir, models_dir,
                          widget_dir = NULL, mode = "submission",
                          repo_url = "https://github.com/2711944586/R") {
  s <- .ghs_summary(master)
  if (is.null(widget_dir)) widget_dir <- file.path(dirname(fig_dir), "\u4ea4\u4e92\u7ec4\u4ef6")

  f1_code <- .ghs_read_code(file.path(programs_dir, "07_plot_static.R"), 1, 60)
  f1_body <- paste0(
    .ghs_method(.ghs_para(
      "<b>\u7814\u7a76\u95ee\u9898\uff1a</b>2000\u20132023 \u5168\u7403\u536b\u751f\u652f\u51fa\u5728\u603b\u91cf\u4e0e\u7ed3\u6784\u4e0a\u53d1\u751f\u4e86\u600e\u6837\u7684\u6f14\u53d8\uff1f",
      sprintf("<b>\u65b9\u6cd5\uff1a</b>\u5bf9 master \u5bbd\u8868\u6309\u5e74\u4efd\u6c47\u603b\u4e09\u7c7b\u7b79\u8d44\u6765\u6e90\uff08GGHED \u00b7 PVTD \u00b7 EXT\uff09\u7684\u5168\u7403\u52a0\u603b\uff0c\u8fed\u52a0\u9762\u79ef\u56fe\u91cf\u5316\u957f\u671f\u4efd\u989d\u53d8\u5316\uff1b\u5168\u7403 %d\u2013%d \u5e74\u5316\u589e\u901f = (CHE<sub>%d</sub>/CHE<sub>%d</sub>)<sup>1/%d</sup>\u22121 = %s\u3002",
              s$base_year, s$cur_year, s$cur_year, s$base_year, s$span,
              .ghs_n(s$che_total_growth * 100, 2, "%"))
    )),
    .ghs_code(f1_code, "r", "07_plot_static.R \u00b7 \u6784\u5efa\u5168\u7403-\u5e74\u9762\u677f\u4e0e\u5806\u53e0\u9762\u79ef\u56fe"),
    .ghs_fig(file.path(fig_dir, "001_global_sources_area.png"),
             "\u5168\u7403\u536b\u751f\u652f\u51fa\u6765\u6e90\u7ed3\u6784 2000\u20132023\uff08USD 2023 \u4e0d\u53d8\u4ef7\uff09",
             "Figure 1A \u00b7 \u5168\u7403\u603b\u989d\u4e0e\u6765\u6e90"),
    .ghs_fig(file.path(fig_dir, "023_stream_continent.png"),
             "\u6309\u5927\u6d32\u5206\u89e3\u7684 CHE \u6d41\u53d8\u56fe",
             "Figure 1B \u00b7 \u5927\u6d32\u5206\u89e3"),
    .ghs_fig(file.path(fig_dir, "073_global_che_total.png"),
             "\u5168\u7403 CHE \u603b\u91cf\u65f6\u5e8f\u53d8\u5316",
             "Figure 1C \u00b7 \u603b\u91cf\u8d8b\u52bf"),
    .ghs_fig(file.path(fig_dir, "074_global_hf_share.png"),
             "\u5168\u7403\u4e09\u6e90\u7b79\u8d44\u4efd\u989d\u6f14\u5316",
             "Figure 1D \u00b7 \u4efd\u989d\u53d8\u5316"),
    .ghs_callout("\u5206\u6790\u89e3\u8bfb",
      .ghs_para(
        sprintf("<b>\u603b\u91cf\u5c42\u9762\uff1a</b>\u5168\u7403\u536b\u751f\u652f\u51fa\u603b\u91cf\u7531 %d \u5e74\u7684 %s \u589e\u957f\u81f3 %d \u5e74\u7684 %s\uff0c\u5e74\u5316\u590d\u5408\u589e\u7387 %s\u3002\u8fd9\u610f\u5473\u7740 23 \u5e74\u95f4\u5168\u7403\u536b\u751f\u603b\u6295\u5165\u7d2f\u8ba1\u7ffb\u4e86\u4e00\u500d\u591a\u3002\u4eba\u5747 CHE \u7531 %s \u5347\u81f3 %s\uff0c\u4f46\u8fd9\u4e00\u5e73\u5747\u6570\u63a9\u76d6\u4e86\u5de8\u5927\u7684\u8de8\u56fd\u5dee\u5f02\u2014\u2014\u9ad8\u6536\u5165\u56fd\u5bb6\u4eba\u5747\u8d85 $5,000\uff0c\u4f4e\u6536\u5165\u56fd\u5bb6\u4ec5 $30\u201350\u3002",
                s$base_year, .ghs_m(s$che_total_base),
                s$cur_year, .ghs_m(s$che_total_cur),
                .ghs_n(s$che_total_growth * 100, 2, "%"),
                .ghs_m(s$che_pc_base), .ghs_m(s$che_pc_cur)),
        "<b>\u7ed3\u6784\u5c42\u9762\uff1a</b>\u653f\u5e9c\u5f3a\u5236\u7b79\u8d44\uff08GGHE-D\uff09\u4efd\u989d\u5728\u591a\u6570\u9ad8\u6536\u5165\u56fd\u5bb6\u4fdd\u6301\u4e0a\u5347\uff0c\u5168\u7403\u52a0\u6743\u5747\u503c\u7ea6 60%\uff1b\u79c1\u4eba\u7b79\u8d44\uff08PVT-D\uff09\u5728\u4e2d\u7b49\u6536\u5165\u56fd\u5bb6\u4ecd\u5360 35\u201340%\uff1b\u5916\u63f4\uff08EXT\uff09\u4efd\u989d\u6301\u7eed\u4e0b\u964d\uff0c\u4f46\u5411\u6700\u8d2b\u56f0\u56fd\u5bb6\u96c6\u4e2d\uff0c\u90e8\u5206\u56fd\u5bb6 EXT \u5360 CHE \u8d85\u8fc7 30%\u3002",
        "<b>\u52a8\u6001\u5c42\u9762\uff1a</b>2008\u20132010 \u5168\u7403\u91d1\u878d\u5371\u673a\u671f\u95f4\uff0c\u591a\u6570\u56fd\u5bb6\u536b\u751f\u652f\u51fa\u589e\u901f\u653e\u7f13\u4f46\u7edd\u5bf9\u503c\u672a\u4e0b\u964d\uff08\u201c\u536b\u751f\u652f\u51fa\u521a\u6027\u201d\uff09\uff1b2020\u20132022 COVID-19 \u671f\u95f4\uff0c\u5404\u56fd\u7d27\u6025\u8ffd\u52a0\u8d22\u653f\u62e8\u6b3e\uff0c\u603b\u91cf\u66f2\u7ebf\u51fa\u73b0\u660e\u663e\u51f8\u8d77\u3002\u4e24\u6b21\u51b2\u51fb\u7684\u5f71\u54cd\u5728 F4 \u4e2d\u5355\u72ec\u62c6\u89e3\u3002",
        "<b>\u6536\u5165\u7ec4\u5dee\u5f02\uff1a</b>\u9ad8\u6536\u5165\u56fd\u5bb6 23 \u5e74\u95f4\u4eba\u5747 CHE \u589e\u901f\u7ea6 2.5%/\u5e74\uff0c\u4e2d\u7b49\u6536\u5165\u56fd\u5bb6\u7ea6 5\u20138%/\u5e74\uff08\u4e2d\u56fd\u3001\u5370\u5ea6\u3001\u8d8a\u5357\u8d21\u732e\u5927\u90e8\u5206\u589e\u91cf\uff09\uff0c\u4f4e\u6536\u5165\u56fd\u5bb6\u4ec5 1\u20132%/\u5e74\u3002\u8fd9\u79cd\u5206\u5316\u662f\u5426\u8db3\u4ee5\u5b9e\u73b0\u201c\u6536\u655b\u201d\uff0c\u5728 F5 \u4e2d\u4ee5 \u03b2-\u6536\u655b\u6a21\u578b\u6b63\u5f0f\u68c0\u9a8c\u3002"
      ), tone = "blue"),
    .ghs_widget_anchor(widget_dir, "02_highlight_ts.html",
      "\u8de8\u56fd\u4eba\u5747 CHE \u9ad8\u4eae\u65f6\u5e8f\u7ebf", mode, repo_url),
    .ghs_widget_anchor(widget_dir, "01_gapminder_animated.html",
      "\u52a8\u6001\u6c14\u6ce1\u56fe\uff1aCHE \u00d7 GDP \u00d7 \u4eba\u53e3", mode, repo_url),
    .ghs_limit(
      "全球加总受 USD2023 不变价调整，与各国本币发布口径存在领域差异。",
      "GGHED / PVTD / EXT 占比年代间不严格合计 100%（PVD 与 PHI 定义调整）。",
      "该趋势不识别因果，仅描述总量与结构的联动变化。"
    )
  )
  f1 <- .ghs_finding("f-trend", "F1", "MACRO",
    "\u5168\u7403\u957f\u671f\u8d8b\u52bf",
    "\u4e8c\u5341\u591a\u5e74\u91cc\u5168\u7403\u536b\u751f\u652f\u51fa\u603b\u91cf\u7ffb\u500d\uff0c\u4f46\u589e\u957f\u52a8\u529b\u5728\u4e0d\u540c\u6536\u5165\u7ec4\u4e4b\u95f4\u9ad8\u5ea6\u4e0d\u5747\u8861\u3002",
    f1_body, chips = paste0(
      .ghs_chip("\u5e74\u5316\u589e\u901f", .ghs_n(s$che_total_growth * 100, 2, "%"), "blue"),
      .ghs_chip(sprintf("%d \u5e74\u4eba\u5747", s$cur_year), .ghs_m(s$che_pc_cur), "ink"),
      .ghs_chip(sprintf("%d \u5e74\u603b\u989d", s$cur_year), .ghs_m(s$che_total_cur), "orange")
    ))

  top_oops <- .ghs_top_oops(master, n = 8, asc = FALSE)
  bot_oops <- .ghs_top_oops(master, n = 8, asc = TRUE)
  bot_oops <- bot_oops[order(bot_oops$hf3_che), ]
  top_show <- top_oops; bot_show <- bot_oops
  names(top_show) <- c("\u56fd\u5bb6/\u5730\u533a", "ISO3", "\u5927\u6d32",
                       "OOPS \u5360 CHE (%)", "\u4eba\u5747 CHE")
  names(bot_show) <- names(top_show)
  f2_code <- "library(dplyr)\nrank_oops <- master |>\n  dplyr::filter(year == max(year, na.rm = TRUE)) |>\n  dplyr::select(country_name, continent, hf3_che, che_pc_usd2023) |>\n  dplyr::arrange(dplyr::desc(hf3_che))\nutils::head(rank_oops, 10)"
  f2_body <- paste0(
    .ghs_method(.ghs_para(
      "<b>\u7814\u7a76\u95ee\u9898\uff1a</b>\u8c01\u5728\u4e3a\u536b\u751f\u652f\u51fa\u4e70\u5355\uff1f\u653f\u5e9c\u3001\u5c45\u6c11\u81ea\u4ed8\u3001\u79c1\u4eba\u4fdd\u9669\u4e0e\u56fd\u9645\u5916\u63f4\uff0c\u5404\u81ea\u627f\u62c5\u591a\u5c11\u98ce\u9669\uff1f",
      "<b>\u65b9\u6cd5\uff1a</b>\u4f7f\u7528 financing scheme \u7ef4\u5ea6\uff08hf1\u2013hfnec\uff09\u7684\u5360 CHE \u6bd4\u4f8b\u3002OOPS = hf3_che \u662f\u8861\u91cf\u8d22\u52a1\u4fdd\u62a4\u7684\u6838\u5fc3\u4fe1\u53f7\u3002"
    )),
    .ghs_code(f2_code, "r", "OOPS \u56fd\u5bb6\u6392\u884c\uff08Top10 / Bottom10\uff09"),
    .ghs_fig(file.path(fig_dir, "002_oops_ranking_2023.png"),
             sprintf("%d \u5e74\u5c45\u6c11\u81ea\u4ed8\u5360 CHE \u56fd\u5bb6\u6392\u884c", s$cur_year),
             "Figure 2A \u00b7 OOPS \u6392\u884c"),
    .ghs_fig(file.path(fig_dir, "003_oops_ridges_income.png"),
             "OOPS \u5206\u5e03\u968f\u6536\u5165\u7ec4\u522b\u7684\u5bc6\u5ea6\u8c31",
             "Figure 2B \u00b7 OOPS \u5c71\u810a\u56fe"),
    .ghs_fig(file.path(fig_dir, "004_oops_box_continent.png"),
             "\u5404\u5927\u6d32 OOPS \u7bb1\u7ebf\u56fe\u5bf9\u6bd4",
             "Figure 2C \u00b7 OOPS \u5927\u6d32\u7bb1\u7ebf"),
    .ghs_fig(file.path(fig_dir, "005_world_oops_2023.png"),
             sprintf("%d \u5e74 OOPS \u5168\u7403\u5730\u56fe", s$cur_year),
             "Figure 2D \u00b7 OOPS \u4e16\u754c\u5730\u56fe"),
    sprintf("<div class='two-col'>%s%s</div>",
            .ghs_table(top_show, sprintf("OOPS \u6700\u9ad8 8 \u56fd\uff08%d\uff09", s$cur_year), 8),
            .ghs_table(bot_show, sprintf("OOPS \u6700\u4f4e 8 \u56fd\uff08%d\uff09", s$cur_year), 8)),
    .ghs_callout("\u89e3\u8bfb \u00b7 OOPS \u4e0e\u7b79\u8d44\u7ed3\u6784\u5206\u6790",
      .ghs_para(
        sprintf("<b>OOPS \u6982\u5ff5\uff1a</b>OOPS\uff08Out-of-pocket spending as share of CHE\uff09\u5373\u5c45\u6c11\u81ea\u4ed8\u5360\u536b\u751f\u603b\u8d39\u7528\u7684\u6bd4\u4f8b\uff0c\u662f WHO/\u4e16\u754c\u94f6\u884c\u63a8\u8350\u7684\u7b79\u8d44\u4fdd\u62a4\u6838\u5fc3\u6307\u6807\u3002\u5f53 OOPS \u8d85\u8fc7 15\u201320%% \u65f6\uff0c\u5bb6\u5ead\u53d1\u751f\u201c\u707e\u96be\u6027\u536b\u751f\u652f\u51fa\u201d\u7684\u98ce\u9669\u663e\u8457\u4e0a\u5347\uff0c\u5373\u5c45\u6c11\u56e0\u770b\u75c5\u800c\u81f4\u8d2b\u7684\u53ef\u80fd\u6027\u5927\u5927\u589e\u52a0\u3002%d \u5e74\u5168\u7403\u5e73\u5747 OOPS \u7ea6\u4e3a %s%%\uff0c\u4f46\u56fd\u5bb6\u95f4\u5dee\u5f02\u5de8\u5927\uff0c\u4ece\u4e0d\u8db3 5%% \u5230\u8d85\u8fc7 80%%\u3002",
                s$cur_year, .ghs_n(s$oops_mean_cur, 1)),
        sprintf("<b>\u5730\u7406\u6a21\u5f0f\uff1a</b>\u4ece\u5730\u56fe\u4e0e\u5927\u6d32\u7bb1\u7ebf\u56fe\u53ef\u89c1\uff0cOOPS \u5448\u73b0\u663e\u8457\u7684\u5730\u7406\u805a\u96c6\uff1a\u5357\u4e9a\uff08\u5370\u5ea6\u3001\u5b5f\u52a0\u62c9\u3001\u5c3c\u6cca\u5c14\uff09\u4e0e\u4e2d\u4e9c\u5e7f\u6cdb\u8d85\u8fc7 60%%\uff0c\u6492\u54c8\u62c9\u4ee5\u5357\u975e\u6d32\u591a\u56fd\u5728 30\u201350%% \u95f4\uff0c\u800c\u897f\u6b27\u3001\u5317\u6b27\u3001\u5927\u6d0b\u6d32\u666e\u904d\u4f4e\u4e8e 15%%\u3002\u8fd9\u4e0e\u5168\u6c11\u5065\u5eb7\u8986\u76d6\uff08UHC\uff09\u7684\u5b9e\u73b0\u7a0b\u5ea6\u9ad8\u5ea6\u4e00\u81f4\u2014\u2014\u51e0\u4e4e\u6240\u6709 OOPS > 50%% \u7684\u56fd\u5bb6\u5176 UHC \u6709\u6548\u8986\u76d6\u6307\u6570\u5747\u4f4e\u4e8e 50\u3002"),
        sprintf("<b>\u6536\u5165\u7ec4\u522b\u5dee\u5f02\uff1a</b>\u5c71\u810a\u56fe\u6e05\u6670\u5c55\u793a\u4e86\u4e0d\u540c\u6536\u5165\u7ec4\u522b OOPS \u5206\u5e03\u7684\u5f62\u6001\u5dee\u5f02\u3002\u9ad8\u6536\u5165\u56fd\u5bb6\uff08HIC\uff09\u5206\u5e03\u5de6\u5076\u3001\u6781\u4e3a\u96c6\u4e2d\uff0c\u4e2d\u4f4d\u6570\u7ea6 12%%\uff1b\u4f4e\u6536\u5165\u56fd\u5bb6\uff08LIC\uff09\u5219\u53f3\u5076\u5e76\u5c55\u5f00\uff0c\u4e2d\u4f4d\u6570\u8d85\u8fc7 40%%\uff0c\u4e14\u5c3e\u90e8\u5ef6\u4f38\u81f3 80%% \u4ee5\u4e0a\u3002%d \u5e74\u4ecd\u6709 %d \u4e2a\u56fd\u5bb6 OOPS \u9ad8\u4e8e 50%%\uff0c\u8fd9\u4e9b\u56fd\u5bb6\u51e0\u4e4e\u5168\u90e8\u5c5e\u4e8e LIC \u6216 LMIC\uff0c\u5f62\u6210\u660e\u663e\u7684\u201c\u7b79\u8d44\u9677\u9631\u201d\u3002",
                s$cur_year, s$oops_high_cur),
        "<b>\u653f\u7b56\u542b\u4e49\uff1a</b>\u964d\u4f4e OOPS \u7684\u5173\u952e\u5728\u4e8e\u63d0\u5347\u5f3a\u5236\u6027\u7b79\u8d44\uff08GGHED + SHI\uff09\u5360\u6bd4\u3002\u7ecf\u9a8c\u8868\u660e\uff0c\u5f53 GGHED \u5360 CHE \u8d85\u8fc7 40%% \u65f6\uff0cOOPS \u901a\u5e38\u53ef\u63a7\u5236\u5728 30%% \u4ee5\u4e0b\u3002\u5916\u63f4\uff08EXT\uff09\u53ef\u63d0\u4f9b\u77ed\u671f\u8865\u5145\uff0c\u4f46\u4e0d\u53ef\u6301\u7eed\uff1bSDG 3.8 \u8981\u6c42\u5404\u56fd\u6784\u5efa\u56fd\u5185\u7b79\u8d44\u673a\u5236\u4ee5\u5b9e\u73b0\u8d22\u52a1\u4fdd\u62a4\u3002\u5bf9\u4e8e OOPS > 50%% \u7684\u56fd\u5bb6\uff0c\u4f18\u5148\u63a8\u8350\u6269\u5927\u793e\u4f1a\u533b\u7597\u4fdd\u9669\u8986\u76d6\u3001\u52a0\u5f3a\u8d22\u653f\u7a7a\u95f4\u5f00\u53d1\uff0c\u4ee5\u53ca\u9488\u5bf9\u8d2b\u56f0\u4eba\u53e3\u7684\u5b9a\u5411\u8865\u8d34\u653f\u7b56\u3002"
      ), tone = "orange"),
    .ghs_widget_anchor(widget_dir, "08_heatmap_oops.html",
      "OOPS \u7b79\u8d44\u7ed3\u6784\u70ed\u529b\u56fe", mode, repo_url),
    .ghs_limit(
      "OOPS 口径依赖住户卫生支出与成本分摊调查，部分 LIC 可能低估。",
      "外援 EXT 在人道紧急期会出现年度剧烈跳动。",
      "财务保护还受财政货币、医保覆盖面等未入面板变量影响。"
    )
  )
  f2 <- .ghs_finding("f-finance", "F2", "FINANCE",
    "\u7b79\u8d44\u6765\u6e90",
    "OOPS \u5360\u6bd4\u662f\u8861\u91cf\u8d22\u52a1\u98ce\u9669\u7684\u6838\u5fc3\u4fe1\u53f7\uff1b\u8d8a\u9ad8\u7684\u56fd\u5bb6\uff0c\u5c45\u6c11\u56e0\u75c5\u81f4\u8d2b\u7684\u6982\u7387\u8d8a\u5927\u3002",
    f2_body, chips = paste0(
      .ghs_chip(sprintf("%d \u5e74 OOPS \u5747\u503c", s$cur_year),
                .ghs_n(s$oops_mean_cur, 1, "%"), "orange"),
      .ghs_chip(paste0("OOPS > 50% \u56fd\u5bb6 (", s$cur_year, ")"),
                .ghs_n(s$oops_high_cur, 0), "ink"),
      .ghs_chip("GGHED \u5747\u503c", .ghs_n(s$gghed_mean_cur, 1, "%"), "blue")
    ))

  ineq_path <- file.path(models_dir, "ineq_panel.csv")
  ineq <- if (file.exists(ineq_path)) utils::read.csv(ineq_path) else NULL
  ineq_chips <- ""
  if (!is.null(ineq) && nrow(ineq)) {
    fy <- ineq[ineq$year == min(ineq$year), ]
    ly <- ineq[ineq$year == max(ineq$year), ]
    ineq_chips <- paste0(
      .ghs_chip("Gini 2000", .ghs_n(fy$gini_pop, 3), "ink"),
      .ghs_chip("Gini 2023", .ghs_n(ly$gini_pop, 3), "blue"),
      .ghs_chip("Theil 2023", .ghs_n(ly$theil_pop, 3), "orange")
    )
  }
  f3_code <- .ghs_read_code(file.path(programs_dir, "04_metrics.R"), 13, 82)
  f3_table <- if (!is.null(ineq) && nrow(ineq)) {
    iq <- ineq[, c("year", "gini_pop", "theil_pop", "atk05", "atk1", "mean_val", "median_val")]
    names(iq) <- c("\u5e74\u4efd", "Gini(pop)", "Theil-T",
                   "Atk 0.5", "Atk 1.0", "\u4eba\u5747\u5747\u503c", "\u4eba\u5747\u4e2d\u4f4d")
    .ghs_table(iq[order(-iq[[1]]), ], "\u8fd1 8 \u5e74\u4e0d\u5e73\u7b49\u6307\u6570", 8, 3)
  } else ""
  f3_body <- paste0(
    .ghs_method(.ghs_para(
      "<b>\u7814\u7a76\u95ee\u9898\uff1a</b>\u8de8\u56fd\u4eba\u5747\u536b\u751f\u652f\u51fa\u7684\u4e0d\u5e73\u7b49\u5982\u4f55\u6f14\u5316\uff1f",
      "<b>\u65b9\u6cd5\uff1a</b>\u5bf9\u6bcf\u5e74\u7684\u4eba\u5747 CHE\uff08USD 2023\uff09\u5e8f\u5217\u8ba1\u7b97\u4e09\u4e2a\u4e92\u8865\u7684\u4e0d\u5e73\u7b49\u6307\u6807\uff1aGini\u3001Theil-T\u3001Atkinson(\u03b5)\uff0c\u5168\u90e8\u4f7f\u7528\u4eba\u53e3\u52a0\u6743\u3002"
    )),
    .ghs_code(f3_code, "r", "04_metrics.R \u00b7 \u4e09\u4e2a\u4e0d\u5e73\u7b49\u6307\u6570\u7684\u5b9e\u73b0"),
    .ghs_fig(file.path(fig_dir, "042_equity_indices.png"),
             "\u4e0d\u5e73\u7b49\u6307\u6570\u9762\u677f",
             "Figure 3A \u00b7 \u591a\u6307\u6807\u5bf9\u6bd4"),
    .ghs_fig(file.path(fig_dir, "043_equity_lorenz.png"),
             "\u6d1b\u4f26\u5179\u66f2\u7ebf\uff1a2000 vs 2023",
             "Figure 3B \u00b7 \u6d1b\u4f26\u5179\u66f2\u7ebf"),
    .ghs_fig(file.path(fig_dir, "160_eq_gini_trend.png"),
             "Gini \u7cfb\u6570\u65f6\u95f4\u8d8b\u52bf",
             "Figure 3C \u00b7 Gini \u8d8b\u52bf"),
    .ghs_fig(file.path(fig_dir, "174_eq_theil_decomp.png"),
             "Theil \u6307\u6570\u5206\u89e3",
             "Figure 3D \u00b7 Theil \u5206\u89e3"),
    f3_table,
    .ghs_callout("\u89e3\u8bfb \u00b7 \u5e73\u5747\u4e0a\u5347 / \u4e0d\u5e73\u7b49\u4e0b\u964d\u7684\u60b6\u8bba",
      .ghs_para(
        "<b>\u7ed3\u6784\u6027\u4e0b\u964d\uff1a</b>\u4eba\u53e3\u52a0\u6743 Gini \u7531 2000 \u5e74\u7684\u7ea6 0.811 \u964d\u81f3 2023 \u5e74\u7684\u7ea6 0.772\uff1b\u5e76\u975e\u6765\u81ea\u5bcc\u56fd\u505c\u6ede\uff0c\u800c\u662f\u4e2d\u3001\u5370\u3001\u5370\u5c3c\u7b49\u5927\u56fd\u4eba\u5747 CHE \u5feb\u901f\u4e0a\u5347\u3002\u8fd9\u79cd\u4e0b\u964d\u4e3b\u8981\u7531\u53d1\u5c55\u4e2d\u56fd\u5bb6\u5411\u4e0a\u653b\u51fb\u9a71\u52a8\uff0c\u800c\u975e\u9ad8\u6536\u5165\u56fd\u5bb6\u7684\u4e0b\u6ed1\u3002",
        "<b>\u5269\u4f59\u4e0d\u5e73\u7b49\u4ecd\u6781\u9ad8\uff1a</b>0.77 \u7684 Gini \u4ecd\u5904\u4e8e\u5168\u7403\u6536\u5165\u5206\u914d\u7684\u6781\u7aef\u533a\u95f4\u3002\u8de8\u56fd\u4eba\u5747 CHE \u7684 P90/P10 \u6bd4\u503c\u4ecd\u53ef\u8fbe 50 \u500d\u4ee5\u4e0a\uff0c\u8868\u660e\u4e0d\u540c\u53d1\u5c55\u6c34\u5e73\u7684\u56fd\u5bb6\u5728\u536b\u751f\u8d44\u6e90\u53ef\u53ca\u6027\u4e0a\u5b58\u5728\u5de8\u5927\u9e3f\u6c9f\u3002",
        "<b>\u4e09\u6307\u6807\u4e92\u8865\uff1a</b>Atkinson(\u03b5=1) \u957f\u671f\u9ad8\u4e8e 0.69\uff0c\u63d0\u793a\u5e95\u5c42\u654f\u611f\u7684\u4e0d\u5e73\u7b49\u4e0b\u964d\u5e45\u5ea6\u66f4\u5c0f\u3002Theil \u5bf9\u9ad8\u7aef\u654f\u611f\uff0c\u5176\u4e0b\u964d\u5e45\u5ea6\u66f4\u5927\uff0c\u8bf4\u660e\u9ad8\u6536\u5165\u56fd\u5bb6\u4e4b\u95f4\u7684\u96c6\u4e2d\u5ea6\u6709\u6240\u7f13\u89e3\u3002",
        "<b>\u653f\u7b56\u542b\u4e49\uff1a</b>\u5c3d\u7ba1\u8de8\u56fd\u4e0d\u5e73\u7b49\u6574\u4f53\u5728\u6539\u5584\uff0c\u4f46\u6539\u5584\u901f\u5ea6\u7f13\u6162\uff0c\u82e5\u6309\u5f53\u524d\u8d8b\u52bf\u9700 30+ \u5e74\u624d\u80fd\u663e\u8457\u7f29\u5c0f\u5dee\u8ddd\u3002\u5bf9\u4e8e\u5e95\u90e8\u56fd\u5bb6\uff0c\u4ec5\u4f9d\u9760\u7ecf\u6d4e\u589e\u957f\u4e0d\u8db3\u4ee5\u5f25\u5408\u5dee\u8ddd\uff0c\u9700\u8981\u56fd\u9645\u793e\u4f1a\u5b9a\u5411\u7684\u536b\u751f\u7b79\u8d44\u652f\u6301\u548c\u5236\u5ea6\u80fd\u529b\u5efa\u8bbe\u3002"
      ), tone = "blue"),
    .ghs_widget_anchor(widget_dir, "07_inequality.html",
      "不平等三指标交互趋势", mode, repo_url),
    .ghs_limit(
      "Gini 与 Theil 在极端重尾分布下估计偏差受样本大小影响。",
      "Atkinson 取 ε = 0.5 与 ε = 1.0 反映不同偏好，需同时报呈。",
      "跨国不平等 ≠ 国内不平等；后者需 LSMS / DHS 微观样本。"
    )
  )
  f3 <- .ghs_finding("f-equity", "F3", "EQUITY",
    "\u8de8\u56fd\u4e0d\u5e73\u7b49",
    "\u4eba\u53e3\u52a0\u6743\u540e\u8de8\u56fd\u4eba\u5747 CHE \u7684\u4e0d\u5e73\u7b49\u957f\u671f\u4e0b\u964d\uff0c\u4f46\u7edd\u5bf9\u6c34\u5e73\u4f9d\u7136\u5904\u4e8e\u6781\u7aef\u533a\u95f4\u3002",
    f3_body, chips = ineq_chips)

  covid <- .ghs_covid_top(master, n = 8)
  covid_code <- .ghs_read_code(file.path(programs_dir, "04_metrics.R"), 116, 139)
  covid_chips <- ""; covid_table_html <- ""
  if (!is.null(covid)) {
    covid_chips <- paste0(
      .ghs_chip("\u6700\u5927\u6b63\u51b2\u51fb",
                paste0("+", .ghs_n(max(covid$delta_pct), 1, "%")), "blue"),
      .ghs_chip("\u6700\u5927\u8d1f\u51b2\u51fb",
                paste0(.ghs_n(min(covid$delta_pct), 1, "%")), "orange"),
      .ghs_chip("\u89c2\u6d4b\u56fd\u5bb6", .ghs_n(nrow(covid), 0), "ink")
    )
    cs <- covid
    cs$delta_pct <- paste0(.ghs_n(cs$delta_pct, 2), "%")
    names(cs) <- c("\u56fd\u5bb6/\u5730\u533a", "ISO3",
                   "2019 \u57fa\u7ebf CHE", "2020\u20132022 \u5e73\u5747 CHE",
                   "\u76f8\u5bf9\u53d8\u5316")
    covid_table_html <- .ghs_table(cs, "CHE \u76f8\u5bf9\u53d8\u5316\u6700\u5927\u7684 8 \u4e2a\u56fd\u5bb6", 8)
  }
  f4_body <- paste0(
    .ghs_method(.ghs_para(
      "<b>\u7814\u7a76\u95ee\u9898\uff1a</b>\u65b0\u51a0\u5927\u6d41\u884c\u671f\u95f4\uff0c\u54ea\u4e9b\u56fd\u5bb6\u663e\u8457\u589e\u52a0\u4e86\u536b\u751f\u652f\u51fa\uff1f\u54ea\u4e9b\u53cd\u800c\u840e\u7f29\uff1f",
      "<b>\u65b9\u6cd5\uff1a</b>\u5b9a\u4e49 base = 2019\u3001shock = 2020\u20132022 \u5e73\u5747\uff1b\u5bf9\u6bcf\u4e2a\u56fd\u5bb6\u8ba1\u7b97 CHE \u76f8\u5bf9\u53d8\u5316\u4e0e OOPS \u767e\u5206\u70b9\u53d8\u5316\u3002"
    )),
    .ghs_code(covid_code, "r", "04_metrics.R \u00b7 COVID \u51b2\u51fb\u5ea6\u91cf"),
    .ghs_fig(file.path(fig_dir, "009_covid_dumbbell.png"),
             "OOPS \u5728 2019 vs 2020\u20132022 \u7684\u56fd\u5bb6\u7ea7\u53d8\u5316\uff08\u54d1\u94c3\u56fe\uff09",
             "Figure 4A \u00b7 OOPS \u54d1\u94c3"),
    .ghs_fig(file.path(fig_dir, "010_covid_scatter.png"),
             "CHE \u589e\u91cf vs OOPS \u589e\u91cf\u6563\u70b9",
             "Figure 4B \u00b7 CHE \u4e0e OOPS \u8054\u52a8"),
    .ghs_fig(file.path(fig_dir, "231_shk_covid_chepc.png"),
             "COVID \u671f\u95f4\u4eba\u5747 CHE \u53d8\u5316\u5206\u5e03",
             "Figure 4C \u00b7 CHE \u51b2\u51fb"),
    .ghs_fig(file.path(fig_dir, "234_shk_covid_oop.png"),
             "COVID \u671f\u95f4 OOPS \u53d8\u5316\u5206\u5e03",
             "Figure 4D \u00b7 OOPS \u51b2\u51fb"),
    covid_table_html,
    .ghs_callout("\u89e3\u8bfb \u00b7 \u5371\u673a\u4e2d\u7684\u4e24\u7c7b\u8f68\u8ff9",
      .ghs_para(
        "<b>\u653f\u5e9c\u6258\u5e95\u578b\uff1a</b>\u591a\u6570 OECD \u56fd\u5bb6 CHE \u62ac\u5347\u3001GGHED \u4e0a\u5347\uff0cOOPS \u53cd\u800c\u4e0b\u964d\u3002\u8fd9\u4e9b\u56fd\u5bb6\u901a\u8fc7\u5927\u89c4\u6a21\u8d22\u653f\u8f6c\u79fb\u652f\u4ed8\u548c\u7d27\u6025\u536b\u751f\u62e8\u6b3e\u5438\u6536\u4e86\u5371\u673a\u6210\u672c\uff0c\u4f53\u73b0\u4e86\u5236\u5ea6\u97e7\u6027\u3002",
        "<b>\u8d22\u653f\u7d27\u7f29\u578b\uff1a</b>\u90e8\u5206\u4e2d\u4f4e\u6536\u5165\u56fd\u5bb6 CHE \u540d\u4e49\u62ac\u5347\u4f46 OOPS \u540c\u6b65\u4e0a\u5347\uff0c\u610f\u5473\u7740\u62ac\u5347\u4e3b\u8981\u6765\u81ea\u5c45\u6c11\u81ea\u4ed8\u3002\u8fd9\u7c7b\u56fd\u5bb6\u7f3a\u4e4f\u8db3\u591f\u7684\u8d22\u653f\u7f13\u51b2\u7a7a\u95f4\uff0c\u5371\u673a\u6210\u672c\u76f4\u63a5\u4e0b\u6c89\u5230\u5c45\u6c11\u5c42\u9762\u3002",
        "<b>\u8de8\u56fd\u5dee\u5f02\uff1a</b>\u7f8e\u56fd\u3001\u82f1\u56fd\u3001\u6fb3\u5927\u5229\u4e9a\u5728 2020\u20132022 \u5e74 GGHED \u5360 CHE \u4e0a\u5347 2\u20134 \u4e2a\u767e\u5206\u70b9\uff0c\u800c\u5370\u5ea6\u3001\u83f2\u5f8b\u5bbe\u7b49\u56fd\u5bb6 OOPS \u53cd\u800c\u4e0a\u5347 2\u20136 \u4e2a\u767e\u5206\u70b9\uff0c\u5f62\u6210\u9c9c\u660e\u5bf9\u6bd4\u3002",
        "<b>\u653f\u7b56\u542b\u4e49\uff1a</b>\u5728\u5e38\u6001\u9884\u7b97\u5916\u9884\u8bbe\u53cd\u5468\u671f\u536b\u751f\u7f13\u51b2\u662f\u5173\u952e\uff1b\u5927\u6d41\u884c\u51c6\u5907\u57fa\u91d1\u3001\u5f39\u6027\u7b79\u8d44\u673a\u5236\u548c\u5feb\u901f\u8c03\u914d\u7f51\u7edc\u662f\u63d0\u5347\u536b\u751f\u4f53\u7cfb\u97e7\u6027\u7684\u4e09\u5927\u652f\u67f1\u3002"
      ), tone = "orange"),
    .ghs_widget_anchor(widget_dir, "13_oops_heatmap.html",
      "OOPS 2019 vs 2020-2022 \u70ed\u529b\u56fe", mode, repo_url),
    .ghs_limit(
      "2020–2022 均值与 2019 基线对比仅捕捉短期冲击。",
      "部分国家 2022–2023 数据仍为初估，存在修正风险。",
      "未及 COVID 主动付费后续调整（如报销、补贴、交叉补偿）。"
    )
  )
  f4 <- .ghs_finding("f-covid", "F4", "RESILIENCE",
    "\u5371\u673a\u51b2\u51fb",
    "COVID-19 \u66b4\u9732\u4e86\u536b\u751f\u4f53\u7cfb\u7684\u8106\u5f31\u6027\uff0c\u4f46\u4e0d\u540c\u56fd\u5bb6\u627f\u62c5\u5371\u673a\u6210\u672c\u7684\u65b9\u5f0f\u622a\u7136\u4e0d\u540c\u3002",
    f4_body, chips = covid_chips)

  beta_path <- file.path(models_dir, "beta_panel.csv")
  beta_panel <- if (file.exists(beta_path)) utils::read.csv(beta_path) else NULL
  beta_chips <- ""; beta_text <- ""
  if (!is.null(beta_panel) && nrow(beta_panel)) {
    fit <- tryCatch(stats::lm(growth ~ log_start + continent, data = beta_panel),
                    error = function(e) NULL)
    if (!is.null(fit)) {
      co <- stats::coef(fit); beta_val <- co[["log_start"]]
      half <- if (is.finite(beta_val) && beta_val < 0) -log(2) / beta_val else NA
      r2 <- summary(fit)$r.squared
      beta_chips <- paste0(
        .ghs_chip("\u03b2 (log y\u2080)", .ghs_n(beta_val, 4), "blue"),
        .ghs_chip("\u534a\u6536\u655b\u5e74", .ghs_n(half, 1), "orange"),
        .ghs_chip("R\u00b2", .ghs_n(r2, 3), "ink")
      )
      beta_text <- sprintf("\u62df\u5408\u7ed3\u679c\uff1a\u03b2 = %s\uff0c\u534a\u6536\u655b\u5e74 \u2248 %s \u5e74\uff1b\u03b2 < 0 \u5373\u652f\u6301\u6536\u655b\u5047\u8bbe\u3002",
                           .ghs_n(beta_val, 4), .ghs_n(half, 1))
    }
  }
  f5_code <- .ghs_read_code(file.path(programs_dir, "05_models.R"), 80, 114)
  f5_body <- paste0(
    .ghs_method(.ghs_para(
      "<b>\u7814\u7a76\u95ee\u9898\uff1a</b>\u4f4e\u6c34\u5e73\u56fd\u5bb6\u662f\u5426\u5728\u8ffd\u8d76\u9ad8\u6c34\u5e73\u56fd\u5bb6\uff1f",
      "<b>\u65b9\u6cd5\uff1a</b>\u6784\u9020\u622a\u9762\uff1a\u6bcf\u56fd 2000 \u4e0e 2023 \u7684\u4eba\u5747 CHE\uff1b\u8ba1\u7b97\u5e74\u5316\u5bf9\u6570\u589e\u901f\uff1b\u4ee5 continent \u4e3a\u56fa\u5b9a\u6548\u5e94\uff0c\u56de\u5f52 growth ~ log y\u2080\u3002",
      beta_text
    )),
    .ghs_code(f5_code, "r", "05_models.R \u00b7 \u03b2-\u6536\u655b\u56de\u5f52"),
    .ghs_fig(file.path(fig_dir, "026_beta_convergence.png"),
             "\u03b2-\u6536\u655b\u6563\u70b9 + \u56de\u5f52\u7ebf",
             "Figure 5 \u00b7 \u03b2-\u6536\u655b"),
    .ghs_callout("\u89e3\u8bfb \u00b7 \u6536\u655b\u4e0e\u5f02\u8d28\u6027",
      .ghs_para(
        "<b>\u6574\u4f53\u6536\u655b\uff1a</b>\u03b2 \u663e\u8457\u4e3a\u8d1f\uff0c\u8d77\u70b9\u8d8a\u4f4e\u8fc7\u53bb 23 \u5e74\u4eba\u5747 CHE \u589e\u901f\u8d8a\u5feb\u3002\u8fd9\u4e00\u7ed3\u679c\u652f\u6301\u201c\u8d2b\u56fd\u8ffd\u8d76\u201d\u5047\u8bbe\uff0c\u4f46\u534a\u6536\u655b\u5e74\u7ea6 35 \u5e74\uff0c\u610f\u5473\u7740\u5dee\u8ddd\u7f29\u5c0f\u4e00\u534a\u4ecd\u9700\u4e00\u4ee3\u4eba\u65f6\u95f4\u3002",
        "<b>\u5927\u6d32\u5dee\u5f02\uff1a</b>\u975e\u6d32\u56fd\u5bb6\u5e73\u5747\u589e\u901f\u6700\u9ad8\uff0c\u4f46\u8d77\u70b9\u6700\u4f4e\uff0c\u534a\u6536\u655b\u5e74\u6700\u957f\u3002\u4e1c\u4e9a\u548c\u4e1c\u6b27\u7684\u6536\u655b\u6548\u679c\u6700\u663e\u8457\uff0c\u5f97\u76ca\u4e8e\u5236\u5ea6\u8f6c\u578b\u548c\u653f\u5e9c\u536b\u751f\u6295\u5165\u5feb\u901f\u6269\u5f20\u3002",
        "<b>\u505c\u6ede\u8005\u4e0e\u53cd\u4f8b\uff1a</b>\u6d25\u5df4\u5e03\u97e6\u3001\u59d4\u5185\u745e\u62c9\u7b49\u56fd\u5bb6 2010 \u5e74\u540e\u4eba\u5747 CHE \u8d1f\u589e\u957f\uff0c\u504f\u79bb\u6536\u655b\u8d8b\u52bf\uff0c\u8bf4\u660e\u653f\u6cbb\u4e0d\u7a33\u5b9a\u548c\u7ecf\u6d4e\u5371\u673a\u53ef\u4ee5\u6253\u65ad\u8ffd\u8d76\u8fdb\u7a0b\u3002",
        "<b>\u542b\u4e49\uff1a</b>\u8ffd\u8d76\u4e0d\u662f\u81ea\u52a8\u7684\uff1b\u9700\u8981\u6301\u7eed\u7684 GGHED \u6295\u5165\u4e0e\u5916\u90e8\u6280\u672f\u63f4\u52a9\u3002\u5bcc\u88d5\u56fd\u5bb6\u7684\u7ecf\u9a8c\u8868\u660e\uff0c\u653f\u5e9c\u5360\u4e3b\u5bfc\u7684\u7b79\u8d44\u6a21\u5f0f\u662f\u5b9e\u73b0\u5feb\u901f\u6536\u655b\u7684\u5173\u952e\u5236\u5ea6\u6761\u4ef6\u3002"
      ), tone = "blue"),
    .ghs_widget_anchor(widget_dir, "13_gapminder_bubble.html",
      "GDP \u00d7 CHE_pc \u00d7 \u4eba\u53e3\u6c14\u6ce1 (gapminder)", mode, repo_url),
    .ghs_limit(
      "经典 β-收敛只描述平均趋势，心中与低位间距仍有开。",
      "continent 固定效应只控制领域面，未控制个体 / 制度面变量。",
      "起点 2000 年部分 LIC 存在数据缺失，需以 enrich 补齐。"
    )
  )
  f5 <- .ghs_finding("f-beta", "F5", "CONVERGENCE",
    "\u6536\u655b\u5206\u6790",
    "\u628a 23 \u5e74\u4eba\u5747 CHE \u589e\u901f\u62df\u5408\u5230\u8d77\u70b9\u6c34\u5e73\uff0c\u7ed3\u679c\u652f\u6301 \u03b2-\u6536\u655b\u4f46\u5206\u5927\u6d32\u5b58\u5728\u663e\u8457\u5f02\u8d28\u6027\u3002",
    f5_body, chips = beta_chips)

  fe_path <- file.path(models_dir, "panel_fe_tidy.csv")
  fe <- if (file.exists(fe_path)) utils::read.csv(fe_path) else NULL
  fe_chips <- ""; fe_table_html <- ""
  if (!is.null(fe) && nrow(fe)) {
    row <- fe[fe$term == "log_gdp_pc", , drop = FALSE]
    if (nrow(row)) {
      fe_chips <- paste0(
        .ghs_chip("\u5f39\u6027 \u03b2\u0302", .ghs_n(row$estimate, 3), "blue"),
        .ghs_chip("\u6807\u51c6\u8bef", .ghs_n(row$std.error, 3), "ink"),
        .ghs_chip("t-stat", .ghs_n(row$statistic, 2), "orange")
      )
    }
    fed <- fe[, c("term", "estimate", "std.error", "statistic", "p.value")]
    names(fed) <- c("\u53d8\u91cf", "\u4f30\u8ba1\u503c", "\u6807\u51c6\u8bef",
                    "t \u503c", "p \u503c")
    fe_table_html <- .ghs_table(fed, "\u56de\u5f52\u7ed3\u679c\uff08\u6309 iso3 \u805a\u7c7b\u6807\u51c6\u8bef\uff09", 5, 4)
  }
  f6_code <- .ghs_read_code(file.path(programs_dir, "05_models.R"), 167, 187)
  f6_body <- paste0(
    .ghs_method(.ghs_para(
      "<b>\u7814\u7a76\u95ee\u9898\uff1a</b>\u5f53\u4e00\u56fd GDP \u4e0a\u5347 1%\uff0c\u5176\u4eba\u5747\u536b\u751f\u652f\u51fa\u5927\u7ea6\u4e0a\u5347\u591a\u5c11\uff1f",
      "<b>\u65b9\u6cd5\uff1a</b>\u7528 fixest::feols \u62df\u5408\u53cc\u5411\u56fa\u5b9a\u6548\u5e94\uff1alog(che_pc_usd2023) ~ log(gdp_pc_usd) | iso3_code + year\uff0c\u6309\u56fd\u5bb6\u805a\u7c7b\u6807\u51c6\u8bef\u3002"
    )),
    .ghs_code(f6_code, "r", "05_models.R \u00b7 \u9762\u677f\u53cc\u5411\u56fa\u5b9a\u6548\u5e94"),
    fe_table_html,
    .ghs_callout("\u89e3\u8bfb \u00b7 \u5f39\u6027\u63a5\u8fd1 0.8",
      .ghs_para(
        "<b>\u4e3b\u7ed3\u8bba\uff1a</b>GDP \u6bcf\u4e0a\u5347 1%\uff0c\u4eba\u5747 CHE \u4e0a\u5347\u7ea6 0.8%\uff0c\u5f39\u6027\u663e\u8457\u5c0f\u4e8e 1\u3002\u8fd9\u8868\u660e\u536b\u751f\u652f\u51fa\u5728\u5168\u7403\u8303\u56f4\u5185\u5c1a\u4e0d\u662f\u201c\u5962\u4f88\u54c1\u201d\uff0c\u800c\u662f\u4e00\u79cd\u201c\u5fc5\u9700\u54c1\u201d\u5c5e\u6027\u7684\u652f\u51fa\u3002",
        "<b>\u8de8\u56fd\u5f02\u8d28\u6027\uff1a</b>\u9ad8\u6536\u5165\u56fd\u5bb6\u5f39\u6027\u8d8b\u8fd1 1.0\uff08\u5962\u4f88\u54c1\u7279\u5f81\uff09\uff0c\u4f4e\u6536\u5165\u56fd\u5bb6\u5f39\u6027\u663e\u8457\u4f4e\u4e8e 0.7\uff0c\u53cd\u6620\u4e86\u8d22\u653f\u7a7a\u95f4\u7ea6\u675f\u3002\u4e2d\u7b49\u6536\u5165\u56fd\u5bb6\u5219\u5904\u4e8e\u8fc7\u6e21\u5e26\u3002",
        "<b>\u56e0\u679c\u8b66\u544a\uff1a</b>FE \u4ec5\u63a7\u5236\u4e0d\u53ef\u89c2\u6d4b\u4e14\u4e0d\u968f\u65f6\u95f4\u53d8\u5316\u7684\u9879\uff0c\u4e0d\u4ee3\u8868\u56e0\u679c\u3002GDP \u589e\u957f\u53ef\u80fd\u540c\u65f6\u5e26\u52a8\u536b\u751f\u652f\u51fa\u548c\u5176\u4ed6\u793e\u4f1a\u652f\u51fa\uff0c\u5b58\u5728\u53cc\u5411\u56e0\u679c\u95ee\u9898\u3002",
        "<b>\u542b\u4e49\uff1a</b>\u7eaf\u7c8b\u4f9d\u9760\u7ecf\u6d4e\u589e\u957f\u6765\u6269\u5927\u536b\u751f\u652f\u51fa\u662f\u4e0d\u591f\u7684\uff1b\u8d22\u653f\u7a7a\u95f4\u9700\u8981\u4e3b\u52a8\u5236\u5ea6\u5b89\u6392\u3002\u5c24\u5176\u5bf9\u4e8e\u4f4e\u6536\u5165\u56fd\u5bb6\uff0c\u4ec5\u7b49\u5f85 GDP \u589e\u957f\u5c06\u5bfc\u81f4\u536b\u751f\u7b79\u8d44\u6c38\u8fdc\u4e0d\u8db3\u3002"
      ), tone = "blue"),
    .ghs_widget_anchor(widget_dir, "13_splom.html",
      "\u591a\u53d8\u91cf\u6563\u70b9\u77e9\u9635 (SPLOM)", mode, repo_url),
    .ghs_limit(
      "FE 仅控面不可观测且不随时间变化的项，不代表因果。",
      "未加入人口老龄化、默克尔指数等可能控变量。",
      "聚类标准误与年代限制为推断设计选择，可能偏低。"
    )
  )
  f6 <- .ghs_finding("f-fe", "F6", "ELASTICITY",
    "GDP\u5f39\u6027",
    "\u5728\u63a7\u5236\u56fd\u5bb6\u4e0e\u5e74\u4efd\u56fa\u5b9a\u6548\u5e94\u540e\uff0c\u4eba\u5747\u536b\u751f\u652f\u51fa\u5bf9 GDP \u7684\u5f39\u6027\u63a5\u8fd1 0.8\u3002",
    f6_body, chips = fe_chips)

  f7_code <- .ghs_read_code(file.path(programs_dir, "05_models.R"), 17, 53)
  f7_body <- paste0(
    .ghs_method(.ghs_para(
      "<b>\u7814\u7a76\u95ee\u9898\uff1a</b>\u628a\u56fd\u5bb6\u653e\u5728\u7b79\u8d44\u7ed3\u6784\u7a7a\u95f4\uff08GGHED \u00b7 PVTD \u00b7 EXT \u00b7 OOPS\uff09\uff0c\u5b83\u4eec\u80fd\u805a\u6210\u51e0\u4e2a archetype\uff1f",
      "<b>\u65b9\u6cd5\uff1a</b>\u5bf9\u6700\u8fd1\u4e00\u5e74\u7684\u622a\u9762\u505a PCA\uff08\u4e2d\u5fc3\u5316 + \u6807\u51c6\u5316\uff09\uff0c\u4fdd\u7559\u524d\u4e24\u4e3b\u6210\u5206\uff0c\u518d\u7528 k-means(k=4, nstart=25) \u805a\u7c7b\u3002"
    )),
    .ghs_code(f7_code, "r", "05_models.R \u00b7 PCA + k-means"),
    .ghs_fig(file.path(fig_dir, "015_pca_cluster_2022.png"),
             sprintf("PCA + k-means \u622a\u9762\uff08%d \u5e74\uff09", s$cur_year),
             "Figure 7A \u00b7 \u56fd\u5bb6\u7ec4\u5408"),
    .ghs_fig(file.path(fig_dir, "025_inequality_pca.png"),
             "PCA \u4e3b\u6210\u5206\u8f7d\u8377\uff08financing \u7ef4\u5ea6\uff09",
             "Figure 7B \u00b7 \u4e3b\u6210\u5206\u8f7d\u8377"),
    .ghs_callout("\u89e3\u8bfb \u00b7 4 \u7c7b\u5178\u578b archetype",
      .ghs_para(
        "<b>A \u00b7 \u653f\u5e9c\u5f3a\u4e3b\u5bfc\uff1a</b>\u6b27\u6d32 + \u90e8\u5206\u4e1c\u4e9a\u9ad8\u6536\u5165\u56fd\u5bb6\uff0cGGHED &gt; 70%\u3002\u8fd9\u7c7b\u56fd\u5bb6\u901a\u8fc7\u5f3a\u5236\u6027\u793e\u4f1a\u4fdd\u9669\u6216\u7a0e\u6536\u7b79\u8d44\u5b9e\u73b0\u4e86\u5e7f\u6cdb\u7684\u8d22\u52a1\u4fdd\u62a4\uff0cOOPS \u957f\u671f\u7ef4\u6301\u5728 15% \u4ee5\u4e0b\u3002",
        "<b>B \u00b7 \u79c1\u4eba\u4fdd\u9669\u578b\uff1a</b>\u7f8e\u56fd\u4e3a\u5178\u578b\uff1bPVTD \u4e0e OOPS \u5747\u9ad8\u3002\u5c3d\u7ba1\u4eba\u5747 CHE \u5168\u7403\u6700\u9ad8\uff0c\u4f46\u7531\u4e8e\u53e4\u5168\u6027\u8f83\u4f4e\uff0c\u536b\u751f\u7ed3\u679c\uff08\u9884\u671f\u5bff\u547d\u3001U5MR\uff09\u5e76\u672a\u6bd4\u80a9 A \u7c7b\u56fd\u5bb6\u3002",
        "<b>C \u00b7 \u81ea\u4ed8\u9a71\u52a8\u578b\uff1a</b>\u5357\u4e9a\u3001\u4e2d\u4e9a\u56fd\u5bb6\uff1bOOPS &gt; 40%\uff0c\u8d22\u52a1\u4fdd\u62a4\u8584\u5f31\u3002\u5c45\u6c11\u5728\u5c31\u533b\u65f6\u627f\u53d7\u5de8\u5927\u8d22\u52a1\u538b\u529b\uff0c\u706d\u8d2b\u6548\u679c\u660e\u663e\u6291\u5236\u4e86\u536b\u751f\u670d\u52a1\u5229\u7528\u7387\u3002",
        "<b>D \u00b7 \u5916\u63f4\u4f9d\u8d56\u578b\uff1a</b>\u6492\u54c8\u4ee5\u5357\u975e\u6d32\u4f4e\u6536\u5165\u56fd\u5bb6\uff1bEXT \u5360\u6bd4\u663e\u8457\u3002\u8fd9\u7c7b\u56fd\u5bb6\u9762\u4e34\u201c\u63f4\u52a9\u9000\u51fa\u540e\u7a7a\u767d\u201d\u7684\u98ce\u9669\uff0c\u9700\u8981\u5728\u63f4\u52a9\u7a97\u53e3\u671f\u5185\u5efa\u7acb\u672c\u571f\u7b79\u8d44\u80fd\u529b\u3002"
      ), tone = "ink"),
    .ghs_widget_anchor(widget_dir, "13_country_network.html",
      "PCA \u9644\u8fd1\u8054\u7f51\u00b7\u56fd\u5bb6 archetype", mode, repo_url),
    .ghs_limit(
      "PCA 主成分载荷依赖变量选择；加入 / 去除 EXT 与 PVTD 会改变双众 plot。",
      "k = 4 为使用 elbow + silhouette 联合选取；不代表唯一能选择。",
      "聚类仅描述截面状态，动态转移路径需 longitudinal cluster。"
    )
  )
  f7 <- .ghs_finding("f-cluster", "F7", "TYPOLOGY",
    "\u56fd\u5bb6\u5206\u7c7b",
    "\u5728 financing \u7ed3\u6784\u7a7a\u95f4\u91cc\uff0c\u5168\u7403\u56fd\u5bb6\u81ea\u7136\u5206\u6210 4 \u7c7b\uff0c\u6bcf\u7c7b\u6709\u4e0d\u540c\u7684\u653f\u7b56\u91cd\u70b9\u3002",
    f7_body)

  fc_path <- file.path(models_dir, "forecast_5y.csv")
  fc <- if (file.exists(fc_path)) utils::read.csv(fc_path) else NULL
  fc_chips <- ""; fc_table_html <- ""
  if (!is.null(fc) && nrow(fc)) {
    fc_chips <- paste0(
      .ghs_chip("\u65b9\u6cd5", "ARIMA(auto)", "blue"),
      .ghs_chip("\u9884\u6d4b\u5e74\u9650", "5 \u5e74", "ink"),
      .ghs_chip("\u8986\u76d6\u56fd\u5bb6", "CHN \u00b7 USA \u00b7 IND \u00b7 BRA", "orange")
    )
    fc_show <- fc
    fc_show$band <- sprintf("[%s, %s]",
                             .ghs_n(fc_show$lo_80, 1), .ghs_n(fc_show$hi_80, 1))
    fc_show <- fc_show[, c("country_name", "year", "point", "band", "method")]
    names(fc_show) <- c("\u56fd\u5bb6", "\u5e74\u4efd",
                       "\u70b9\u4f30\u8ba1", "80% \u533a\u95f4", "\u65b9\u6cd5")
    fc_table_html <- .ghs_table(fc_show, "5 \u5e74\u9884\u6d4b\u660e\u7ec6", 20)
  }
  f8_code <- .ghs_read_code(file.path(programs_dir, "05_models.R"), 117, 165)
  f8_body <- paste0(
    .ghs_method(.ghs_para(
      "<b>\u7814\u7a76\u95ee\u9898\uff1a</b>2024\u20132028 \u4e2d\u3001\u7f8e\u3001\u5370\u3001\u5df4\u7684\u4eba\u5747\u536b\u751f\u652f\u51fa\u4f1a\u671d\u54ea\u4e2a\u65b9\u5411\u8d70\uff1f",
      "<b>\u65b9\u6cd5\uff1a</b>\u5bf9\u6bcf\u56fd 2000\u20132023 \u7684 che_pc_usd2023 \u5e8f\u5217\u62df\u5408 auto.arima\uff0c\u8f93\u51fa 5 \u5e74\u70b9\u9884\u6d4b\u4e0e 80%/95% \u533a\u95f4\u3002"
    )),
    .ghs_code(f8_code, "r", "05_models.R \u00b7 ARIMA \u9884\u6d4b"),
    .ghs_fig(file.path(fig_dir, "027_forecast_fan.png"),
             "5 \u5e74 ARIMA \u9884\u6d4b\u6247\u5f62\u56fe",
             "Figure 8 \u00b7 \u9884\u6d4b\u6247\u5f62"),
    fc_table_html,
    .ghs_callout("\u89e3\u8bfb \u00b7 \u9884\u6d4b\u7684\u8fb9\u754c",
      .ghs_para(
        "<b>\u8d8b\u52bf\u5ef6\u7eed\uff1a</b>\u56db\u56fd\u5747\u7ef4\u6301\u4e0a\u5347\u8d8b\u52bf\uff0c\u4e2d\u3001\u5370\u76f8\u5bf9\u589e\u901f\u6700\u5927\u3002\u4e2d\u56fd\u9884\u6d4b 2028 \u5e74\u4eba\u5747 CHE \u5c06\u63a5\u8fd1 1500 USD\uff0c\u5370\u5ea6\u5219\u53ef\u80fd\u7a81\u7834 200 USD \u95e8\u69db\u3002",
        "<b>\u4e0d\u786e\u5b9a\u6027\u5e26\uff1a</b>\u5916\u63a8 5 \u5e74\u540e\u533a\u95f4\u5feb\u901f\u53d1\u6563\uff0c\u5176\u4e2d\u975e\u6d32\u3001\u5357\u4e9a\u56fd\u5bb6\u7684\u7f6e\u4fe1\u533a\u95f4\u5bbd\u5ea6\u53ef\u8fbe 30% \u4ee5\u4e0a\uff0c\u53cd\u6620\u5386\u53f2\u5e8f\u5217\u7684\u9ad8\u6ce2\u52a8\u6027\u3002",
        "<b>\u7ed3\u6784\u6027\u65ad\u70b9\uff1a</b>COVID-19 \u5bf9\u6240\u6709\u6a21\u578b\u90fd\u662f\u5916\u90e8\u7ed3\u6784\u6027\u65ad\u70b9\uff0cauto.arima \u4f1a\u628a\u5b83\u89e3\u91ca\u4e3a\u65b9\u5dee\u4e0a\u5347\u800c\u975e\u5747\u503c\u8f6c\u79fb\uff0c\u8fd9\u610f\u5473\u7740\u4efb\u4f55\u9884\u6d4b\u90fd\u5e94\u9644\u5e26\u201c\u4e0b\u4e00\u4e2a\u51b2\u51fb\u4f1a\u6539\u5199\u201d\u7684\u4fdd\u7559\u3002",
        "<b>\u5efa\u8bae\uff1a</b>\u7ed3\u5408 Shiny \u4eea\u8868\u76d8 Scenarios \u6a21\u5757\u505a\u60c5\u666f\u63a8\u6f14\uff0c\u5c06 ARIMA \u4e0e\u60c5\u666f\u5047\u8bbe\uff08\u5982 GDP \u51b2\u51fb\u3001\u8001\u9f84\u5316\u52a0\u901f\uff09\u7ed3\u5408\u4f7f\u7528\uff0c\u907f\u514d\u5bf9\u5355\u4e00\u70b9\u9884\u6d4b\u8fc7\u5ea6\u4f9d\u8d56\u3002"
      ), tone = "blue"),
    .ghs_widget_anchor(widget_dir, "06_forecast_subplot.html",
      "4 国 ARIMA 交互子图", mode, repo_url),
    .ghs_limit(
      "ARIMA 是单变量时序，不控 GDP · 财政 · 老龄化等驱动项。",
      "5 年以上外推使 80% 区间快速发散，需谨慎引用。",
      "未插入重大冲击（另一大流行、金融危机）的场景损动。"
    )
  )
  f8 <- .ghs_finding("f-forecast", "F8", "FORECAST",
    "\u8d8b\u52bf\u9884\u6d4b",
    "auto.arima \u5bf9\u4e2d\u3001\u7f8e\u3001\u5370\u3001\u5df4\u7684\u4eba\u5747 CHE \u7ed9\u51fa\u77ed\u671f\u5ef6\u7eed\u4e0a\u5347\u7684\u5224\u65ad\u3002",
    f8_body, chips = fc_chips)

  yr <- s$cur_year
  d_aid <- master[master$year == yr & is.finite(master$ext_che), ]
  ext_top <- if (nrow(d_aid)) {
    o <- d_aid[order(-d_aid$ext_che), ][1:8, c("country_name", "iso3_code",
                                                "continent", "ext_che",
                                                "che_pc_usd2023", "gghed_che")]
    names(o) <- c("\u56fd\u5bb6/\u5730\u533a", "ISO3", "\u5927\u6d32",
                  "EXT \u5360 CHE (%)", "\u4eba\u5747 CHE",
                  "GGHED \u5360 CHE (%)")
    o
  } else NULL
  ext_chips <- paste0(
    .ghs_chip("EXT > 20% \u56fd\u5bb6", .ghs_n(s$ext_high_cur, 0), "orange"),
    .ghs_chip("EXT \u5747\u503c", .ghs_n(s$ext_mean_cur, 1, "%"), "blue"),
    .ghs_chip("\u5927\u6d32\u8986\u76d6", .ghs_n(s$n_continent, 0), "ink")
  )
  f9_code <- paste0(
    "library(dplyr)\n",
    "aid_rank <- master |>\n",
    "  dplyr::filter(year == max(year, na.rm = TRUE)) |>\n",
    "  dplyr::select(country_name, continent, ext_che, gghed_che, hf3_che, che_pc_usd2023) |>\n",
    "  dplyr::arrange(dplyr::desc(ext_che))\n",
    "head(aid_rank, 10)"
  )
  f9_body <- paste0(
    .ghs_method(.ghs_para(
      "<b>\u7814\u7a76\u95ee\u9898\uff1a</b>\u54ea\u4e9b\u56fd\u5bb6\u7684\u536b\u751f\u8d22\u52a1\u96be\u4ee5\u8131\u79bb\u5916\u63f4\uff1f\u5916\u63f4\u5360\u6bd4\u4e0e\u8d22\u52a1\u4fdd\u62a4\uff08OOPS\uff09\u662f\u5982\u4f55\u8054\u52a8\u7684\uff1f",
      "<b>\u65b9\u6cd5\uff1a</b>\u53d6 GHED financing source \u4e2d\u7684 EXT (External transfer schemes) \u5360 CHE \u4efd\u989d\uff0c\u8de8\u8054 OOPS / GGHED \u4e0e\u4eba\u5747 CHE\uff0c\u91cd\u70b9\u8bc6\u522b\u54ea\u4e9b LIC \u4ecd\u5904\u4e8e\u8f93\u8840\u578b\u9636\u6bb5\u3002"
    )),
    .ghs_code(f9_code, "r", "EXT > 20% \u9ad8\u4f9d\u8d56\u56fd\u5bb6\u6392\u884c"),
    .ghs_fig(file.path(fig_dir, "031_aid_dependency.png"),
             sprintf("\u5916\u63f4\u4f9d\u8d56\u5ea6\u9762\u677f\uff08%d\uff09", yr),
             "Figure 9A \u00b7 EXT \u4f9d\u8d56\u5ea6"),
    .ghs_fig(file.path(fig_dir, "020_ext_density.png"),
             "EXT \u5360\u6bd4\u8de8\u56fd\u5206\u5e03\u5bc6\u5ea6",
             "Figure 9B \u00b7 EXT \u5206\u5e03"),
    if (!is.null(ext_top)) .ghs_table(ext_top,
      sprintf("EXT \u4f9d\u8d56\u5ea6\u6700\u9ad8\u7684 8 \u4e2a\u56fd\u5bb6 (%d)", yr), 8) else "",
    .ghs_callout("\u89e3\u8bfb \u00b7 \u5916\u63f4\u4ec5\u80fd\u6258\u5e95",
      .ghs_para(
        sprintf("<b>\u96c6\u4e2d\u4e8e\u4f4e\u6536\u5165\uff1a</b>%d \u5e74\u4ecd\u6709 %d \u4e2a\u56fd\u5bb6 EXT > 20%%\uff0c\u5168\u90e8\u4e3a LIC/LMIC\u3002\u8fd9\u4e9b\u56fd\u5bb6\u7684\u536b\u751f\u7cfb\u7edf\u5728\u7ed3\u6784\u4e0a\u4f9d\u8d56\u5916\u90e8\u8d44\u91d1\uff0c\u4e00\u65e6\u63f4\u52a9\u9000\u51fa\u5c06\u9762\u4e34\u4f53\u7cfb\u5d29\u6e83\u98ce\u9669\u3002",
                yr, s$ext_high_cur),
        "<b>\u4e0e OOPS \u5e76\u5b58\uff1a</b>\u9ad8 EXT \u56fd\u5bb6\u591a\u6570 OOPS \u540c\u6837\u504f\u9ad8\uff0c\u610f\u5473\u7740\u5916\u90e8\u8f93\u8840\u672a\u80fd\u6709\u6548\u51cf\u8f7b\u5c45\u6c11\u8d22\u52a1\u8d1f\u62c5\u3002\u63f4\u52a9\u8d44\u91d1\u591a\u6d41\u5411\u533b\u9662\u57fa\u5efa\u548c\u7eb5\u5411\u9879\u76ee\uff0c\u800c\u975e\u62ac\u5347\u5c45\u6c11\u8d22\u52a1\u4fdd\u62a4\u6c34\u5e73\u3002",
        "<b>\u6210\u529f\u9000\u51fa\u6848\u4f8b\uff1a</b>\u8d8a\u5357\u3001\u535a\u8328\u74e6\u7eb3 EXT \u7531 2005 \u5e74 >15% \u964d\u81f3 2023 \u5e74 <5%\uff0c\u540c\u65f6 GGHED \u4e0e PVTD \u586b\u8865\u4e86\u7f3a\u53e3\uff0c\u5b9e\u73b0\u4e86\u201c\u63f4\u52a9\u6bd5\u4e1a\u201d\u3002",
        "<b>\u8def\u5f84\uff1a</b>\u9700\u540c\u6b65\u63a8\u52a8\u672c\u571f\u8d22\u653f\u5f3a\u5236\u6c47\u96c6\u3001\u793e\u4f1a\u533b\u7597\u4fdd\u9669\u6269\u9762\u3001PHC \u521d\u8bca\u8865\u507f\u3002\u63f4\u52a9\u65b9\u5e94\u5c06\u201c\u672c\u571f\u7b79\u8d44\u80fd\u529b\u201d\u4f5c\u4e3a\u63f4\u52a9\u9000\u51fa\u7684\u5173\u952e\u6307\u6807\u7eb3\u5165\u76d1\u6d4b\u3002"
      ), tone = "orange"),
    .ghs_widget_anchor(widget_dir, "13_sankey_flows.html",
      "GGHED / PVTD / EXT / OOPS \u8d44\u91d1\u6d41 sankey", mode, repo_url),
    .ghs_limit(
      "EXT \u8c03\u67e5\u5b58\u5728\u8df3\u53d8\uff1b2020\u20132022 \u4eba\u9053\u54cd\u5e94\u62ac\u5347\u4f7f\u90e8\u5206\u56fd\u5bb6\u5916\u63f4\u4efd\u989d\u4e34\u65f6\u9ad8\u4f30\u3002",
      "GHED \u672a\u80fd\u5b8c\u5168\u8986\u76d6\u53cc\u8fb9\u9879\u76ee\u3001\u5168\u7403\u57fa\u91d1\u548c\u4e0d\u540c\u62a5\u9500\u53e3\u5f84\u4e4b\u95f4\u7684\u5dee\u5f02\u3002",
      "EXT \u5360\u6bd4\u4ec5\u53cd\u6620\u8d44\u91d1\u6765\u6e90\uff0c\u4e0d\u4ee3\u8868\u8d44\u91d1\u6548\u7387\u3002"
    )
  )
  f9 <- .ghs_finding("f-aid", "F9", "AID",
    "\u5916\u63f4\u4f9d\u8d56",
    "EXT \u5360 CHE \u8d85\u8fc7 20% \u610f\u5473\u7740\u672c\u571f\u8d22\u653f\u96be\u4ee5\u72ec\u7acb\u652f\u6491\u7cfb\u7edf\uff1b\u8be5\u4fe1\u53f7\u4e0e OOPS \u5e76\u5b58\u63d0\u793a\u5c45\u6c11\u4ecd\u9700\u6309\u6b21\u4ed8\u8d39\u3002",
    f9_body, chips = ext_chips)

  d_eff <- master[master$year == yr &
                    is.finite(master$che_pc_usd2023) &
                    is.finite(master$life_exp), ]
  eff_chips <- ""
  eff_text <- ""
  eff_table_html <- ""
  if (nrow(d_eff)) {
    d_eff <- d_eff[, c("country_name", "iso3_code", "continent",
                       "che_pc_usd2023", "life_exp")]
    fit_eff <- tryCatch(stats::lm(life_exp ~ log(che_pc_usd2023), data = d_eff),
                        error = function(e) NULL)
    if (!is.null(fit_eff)) {
      co_eff <- stats::coef(fit_eff)
      r2_eff <- summary(fit_eff)$r.squared
      eff_chips <- paste0(
        .ghs_chip("\u62df\u5408\u659c\u7387",
                  .ghs_n(co_eff[2], 2), "blue"),
        .ghs_chip("R\u00b2", .ghs_n(r2_eff, 3), "ink"),
        .ghs_chip("\u53ef\u7528\u56fd\u5bb6", .ghs_n(nrow(d_eff), 0), "orange"))
      eff_text <- sprintf(
        "\u62df\u5408\u9884\u671f\u5bff\u547d ~ log(\u4eba\u5747 CHE)\uff1a\u4eba\u5747\u8d44\u91d1\u6bcf\u53d8\u4e3a e \u500d (\u22482.72 \u500d) \u8054\u52a8\u9884\u671f\u5bff\u547d\u589e\u52a0 %s \u5e74\uff0cR\u00b2 = %s\u3002",
        .ghs_n(co_eff[2], 2), .ghs_n(r2_eff, 3))
    }
    d_eff$resid <- if (!is.null(fit_eff)) stats::residuals(fit_eff) else NA_real_
    d_eff_show <- utils::head(d_eff[order(-d_eff$resid), ], 8)
    names(d_eff_show) <- c("\u56fd\u5bb6", "ISO3", "\u5927\u6d32",
                           "\u4eba\u5747 CHE", "\u9884\u671f\u5bff\u547d", "\u6b8b\u5dee")
    eff_table_html <- .ghs_table(d_eff_show,
      "\u9884\u671f\u5bff\u547d\u9ad8\u4e8e\u4eba\u5747 CHE \u9884\u6d4b\u6700\u591a\u7684 8 \u4e2a\u56fd\u5bb6\uff08\u9ad8\u6548\u7387\u8c61\u9650\uff09", 8, 3)
  }
  f10_code <- paste0(
    "library(dplyr)\n",
    "fit <- master |>\n",
    "  dplyr::filter(year == max(year, na.rm = TRUE),\n",
    "                is.finite(che_pc_usd2023), is.finite(life_exp)) |>\n",
    "  lm(life_exp ~ log(che_pc_usd2023), data = _)\n",
    "summary(fit)"
  )
  f10_body <- paste0(
    .ghs_method(.ghs_para(
      "<b>\u7814\u7a76\u95ee\u9898\uff1a</b>\u540c\u6837\u4eba\u5747 CHE \u4e0b\uff0c\u54ea\u4e9b\u56fd\u5bb6\u201c\u82b1\u5f97\u66f4\u52a0\u5212\u7b97\u201d\uff08\u9884\u671f\u5bff\u547d\u9ad8\u4e8e\u9884\u6d4b\u503c\uff09\uff1f",
      paste0("<b>\u65b9\u6cd5\uff1a</b>\u62df\u5408 \u9884\u671f\u5bff\u547d ~ log(\u4eba\u5747 CHE)\uff0c\u53d6\u6b63\u6b8b\u5dee\u4f5c\u4e3a\u201c\u8d77\u52a8\u6548\u7387\u9886\u5148\u8005\u201d\u4ee3\u7406\uff1b\u4e0e DEA \u8f93\u51fa\u4e0d\u540c\u4f46\u601d\u8def\u4e92\u8865\u3002 ",
             eff_text)
    )),
    .ghs_code(f10_code, "r", "\u201c\u82b1\u5f97\u66f4\u591a\uff0c\u4e0d\u4e00\u5b9a\u7b49\u4e8e\u66f4\u957f\u5bff\u201d \u00b7 \u7b80\u6790\u62df\u5408"),
    .ghs_fig(file.path(fig_dir, "041_efficiency_dea.png"),
             "DEA \u6548\u7387\u524d\u6cbf\uff1a\u4eba\u5747 CHE \u00b7 U5MR / \u9884\u671f\u5bff\u547d",
             "Figure 10A \u00b7 \u6548\u7387\u524d\u6cbf"),
    .ghs_fig(file.path(fig_dir, "051_outcomes_elasticity.png"),
             "\u4eba\u5747 CHE \u4e0e \u9884\u671f\u5bff\u547d / U5MR \u7684\u5f39\u6027",
             "Figure 10B \u00b7 \u8f93\u5165 \u00b7 \u8f93\u51fa"),
    eff_table_html,
    .ghs_callout("\u89e3\u8bfb \u00b7 \u8d44\u91d1\u4e0d\u7b49\u4e8e\u6210\u6548",
      .ghs_para(
        "<b>\u4f30\u8ba1\u5173\u7cfb\uff1a</b>\u4eba\u5747 CHE \u7ffb\u500d \u2248 \u589e\u52a0 4-5 \u5e74\u9884\u671f\u5bff\u547d\uff0c\u4f46\u8de8\u8d44\u91d1\u533a\u95f4\u5448\u73b0\u8fb9\u9645\u9012\u51cf\u3002\u5728 CHE/cap < 500 USD \u533a\u95f4\uff0c\u8fb9\u9645\u4ea7\u51fa\u6700\u9ad8\uff1b\u8d85\u8fc7 5000 USD \u540e\u6bcf\u7ffb\u500d\u4ec5\u5e26\u6765\u4e0d\u5230 1 \u5e74\u5bff\u547d\u63d0\u5347\u3002",
        "<b>\u9ad8\u6548\u7387\u9886\u5148\u8005\uff1a</b>\u53e4\u5df4\u3001\u8d8a\u5357\u3001\u6cf0\u56fd\u3001\u65af\u91cc\u5170\u5361\u548c\u90e8\u5206\u52a0\u52d2\u6bd4\u5c0f\u56fd\u5448\u73b0\u201c\u4f4e\u6295\u5165\u9ad8\u4ea7\u51fa\u201d\u7279\u5f81\u3002\u8fd9\u4e9b\u56fd\u5bb6\u7684\u5171\u540c\u7279\u5f81\u662f\u5f3a\u5927\u7684\u57fa\u5c42\u536b\u751f\u670d\u52a1\u4f53\u7cfb\u548c\u8f83\u9ad8\u7684\u793e\u4f1a\u533b\u4fdd\u8986\u76d6\u7387\u3002",
        "<b>\u4f4e\u6548\u7387\u5f02\u5e38\u70b9\uff1a</b>\u90e8\u5206\u4e2d\u4e1c\u3001\u4e2d\u4e9a\u548c\u62c9\u4e01\u7f8e\u6d32\u56fd\u5bb6\u4eba\u5747 CHE \u8f83\u9ad8\uff0c\u4f46\u9884\u671f\u5bff\u547d\u4f4e\u4e8e\u6a21\u578b\u9884\u6d4b\u503c\u3002\u7f8e\u56fd\u662f\u6700\u663e\u8457\u7684\u5f02\u5e38\u70b9\uff1aCHE/cap \u5168\u7403\u6700\u9ad8\u4f46\u9884\u671f\u5bff\u547d\u4ec5\u7ea6 77 \u5c81\u3002",
        "<b>\u542b\u4e49\uff1a</b>\u8d44\u91d1\u4e0d\u662f\u5bff\u547d\u7684\u540c\u4e49\u8bcd\uff0c\u4f53\u7cfb\u8d28\u91cf\u3001\u793e\u4fdd\u548c\u516c\u536b\u80fd\u529b\u4ecd\u4e3b\u5bfc\u6548\u7387\u3002\u5bf9\u4e8e\u9ad8\u6295\u5165\u4f4e\u4ea7\u51fa\u56fd\u5bb6\uff0c\u4f18\u5316\u8d44\u6e90\u914d\u7f6e\uff08\u4ece hc1 \u8f6c\u5411 hc6\uff09\u6bd4\u589e\u52a0\u603b\u91cf\u66f4\u4e3a\u91cd\u8981\u3002"
      ), tone = "blue"),
    .ghs_widget_anchor(widget_dir, "13_scenarios.html",
      "DEA \u00b7 ARIMA \u00b7 Lorenz \u8054\u52a8\u573a\u666f", mode, repo_url),
    .ghs_limit(
      "\u9884\u671f\u5bff\u547d\u662f\u591a\u56e0\u7d20\u7ed3\u679c\uff0c\u4e0d\u4ec5\u53d6\u51b3\u4e8e\u536b\u751f\u652f\u51fa\u3002",
      "\u8de8\u56fd\u6bd4\u8f83\u53d7 PPP\u3001\u7269\u4ef7\u6c34\u5e73\u3001\u4eba\u53e3\u7ed3\u6784\u548c\u5236\u5ea6\u5dee\u5f02\u5f71\u54cd\u3002",
      "\u672c\u56fe\u672a\u4f7f\u7528\u591a\u8f93\u5165 DEA\uff08\u4ec5 1 \u8f93\u5165 \u00b7 1 \u8f93\u51fa\uff09\u3002"
    )
  )
  f10 <- .ghs_finding("f-efficiency", "F10", "EFFICIENCY",
    "\u8d44\u91d1\u6548\u7387",
    "\u5728\u7ed9\u5b9a\u4eba\u5747\u8d44\u91d1\u4e0b\uff0c\u5404\u56fd\u9884\u671f\u5bff\u547d / U5MR \u7684\u5dee\u5f02\u53ef\u80fd\u53cd\u6620\u4f53\u7cfb\u8d28\u91cf\u3001PHC \u4e0e\u793e\u4f1a\u533b\u4fdd\u8986\u76d6\u9762\u7b49\u975e\u8d44\u91d1\u56e0\u7d20\u3002",
    f10_body, chips = eff_chips)

  f11_code <- paste0(
    "library(dplyr)\n",
    "rk <- master |>\n",
    "  dplyr::filter(year %in% c(2000, 2010, 2023),\n",
    "                is.finite(che_pc_usd2023)) |>\n",
    "  dplyr::group_by(year) |>\n",
    "  dplyr::mutate(rank = rank(-che_pc_usd2023)) |>\n",
    "  dplyr::ungroup() |>\n",
    "  dplyr::filter(rank <= 25)"
  )
  rank_d <- master[master$year %in% c(2000, 2023) &
                     is.finite(master$che_pc_usd2023), ]
  rank_top <- NULL
  if (nrow(rank_d)) {
    yrs <- c(2000, max(rank_d$year))
    w <- stats::reshape(
      rank_d[, c("iso3_code", "country_name", "year",
                  "che_pc_usd2023")],
      idvar = c("iso3_code", "country_name"),
      timevar = "year",
      direction = "wide"
    )
    names(w) <- gsub("che_pc_usd2023.", "y_", names(w), fixed = TRUE)
    if (all(c("y_2000", paste0("y_", yrs[2])) %in% names(w))) {
      w$rank0 <- rank(-w$y_2000, na.last = "keep")
      w$rank1 <- rank(-w[[paste0("y_", yrs[2])]], na.last = "keep")
      w$delta <- w$rank0 - w$rank1
      w <- w[is.finite(w$delta), ]
      o <- w[order(-abs(w$delta)), c("country_name", "rank0", "rank1",
                                       "delta", "y_2000",
                                       paste0("y_", yrs[2]))]
      o <- utils::head(o, 12)
      o[[5]] <- round(o[[5]], 0); o[[6]] <- round(o[[6]], 0)
      names(o) <- c("\u56fd\u5bb6", "2000 \u6392\u540d", "2023 \u6392\u540d",
                     "\u53d8\u52a8(+\u4e0a\u5347)",
                     "2000 CHE_pc", "2023 CHE_pc")
      rank_top <- o
    }
  }
  f11_table <- if (!is.null(rank_top)) .ghs_table(rank_top,
    "F11 \u00b7 \u4eba\u5747 CHE \u6392\u540d\u53d8\u52a8\u6700\u5927\u7684 12 \u4e2a\u56fd\u5bb6", 12, 3) else ""
  f11_chips <- paste0(
    .ghs_chip("\u8d77\u70b9\u5e74", "2000", "ink"),
    .ghs_chip("\u7ec8\u70b9\u5e74", as.character(s$cur_year), "ink"),
    .ghs_chip("\u6837\u672c", "Top 25 \u00b7 \u4eba\u5747 CHE", "blue")
  )
  f11_body <- paste0(
    .ghs_method(.ghs_para(
      "<b>\u95ee\u9898\uff1a</b>\u8fc7\u53bb 23 \u5e74\uff0c\u54ea\u4e9b\u56fd\u5bb6\u4eba\u5747\u536b\u751f\u652f\u51fa\u6392\u540d\u63d0\u5347\u6700\u5feb\uff0c\u54ea\u4e9b\u8df1\u843d\uff1f",
      "<b>\u65b9\u6cd5\uff1a</b>\u53d6 2000 / 2010 / 2023 \u4e09\u5e74\u4eba\u5747 CHE \u6392\u540d\uff0c\u8ba1\u7b97 \u0394rank\uff1b\u8dc3\u8fc1\u8d8a\u5927\u8868\u793a\u8d22\u653f\u00b7\u589e\u957f\u52a8\u5458\u8d8a\u5f3a\u3002"
    )),
    .ghs_code(f11_code, "r", "\u4eba\u5747 CHE Top 25 \u6392\u540d\u53d8\u8fc1"),
    .ghs_fig(file.path(fig_dir, "055_rank_change.png"),
             "Top 25 \u4eba\u5747 CHE \u8de8\u671f\u6392\u540d\u8de8\u8d8a\u3002",
             "Figure 11A \u00b7 \u6392\u540d bump"),
    f11_table,
    .ghs_callout("\u89e3\u8bfb \u00b7 \u540e\u53d1\u8d76\u8d85",
      .ghs_para(
        "<b>\u9886\u5148\u52a0\u901f\uff1a</b>\u4e2d\u56fd\u3001\u97e9\u56fd\u548c\u4e1c\u6b27\u90e8\u5206\u56fd\u5bb6\u9760 GGHED \u6269\u5f20\u5feb\u901f\u4e0a\u4f4d\u3002\u4e2d\u56fd 2000 \u5e74\u6392\u540d\u7ea6 120 \u4f4d\uff0c2023 \u5e74\u5df2\u5347\u81f3\u7ea6 55 \u4f4d\uff0c\u8df3\u8dc3\u5e45\u5ea6\u5168\u7403\u6700\u5927\u3002",
        "<b>\u76f8\u5bf9\u4e0b\u6ed1\uff1a</b>\u90e8\u5206\u8d44\u6e90\u578b\u3001\u793e\u4f1a\u4e0d\u7a33\u548c\u51b2\u7a81\u578b\u56fd\u5bb6\u6392\u540d\u4e0b\u964d\u3002\u59d4\u5185\u745e\u62c9\u3001\u6d25\u5df4\u5e03\u97e6\u7b49\u56fd\u5bb6\u4e0b\u6ed1\u5e45\u5ea6\u8d85\u8fc7 60 \u4f4d\uff0c\u4e0e\u7ecf\u6d4e\u5371\u673a\u548c\u653f\u6cbb\u52a8\u8361\u76f4\u63a5\u76f8\u5173\u3002",
        "<b>\u5468\u671f\u6027\u6ce2\u52a8\uff1a</b>\u90e8\u5206\u8d44\u6e90\u4f9d\u8d56\u56fd\u5bb6\uff08\u5b89\u54e5\u62c9\u3001\u8d5e\u6bd4\u4e9a\uff092005\u20132014 \u5e74\u5feb\u901f\u4e0a\u5347\uff0c2015\u20132022 \u5e74\u53c8\u5feb\u901f\u4e0b\u964d\uff0c\u53cd\u6620\u539f\u6750\u6599\u5468\u671f\u5bf9 CHE \u7684\u6ea2\u51fa\u6548\u5e94\u3002",
        "<b>\u65b9\u6cd5\u8b66\u793a\uff1a</b>\u6392\u540d\u5bf9\u6570\u636e\u4fee\u8ba2\u654f\u611f\uff0c\u5c24\u5176\u5c0f\u56fd\uff1b\u89e3\u8bfb\u65f6\u5efa\u8bae\u914d\u5408\u65b9\u5dee\u3001\u6eda\u52a8\u6392\u540d\u7b49\u7a33\u5065\u6027\u5de5\u5177\u4e00\u540c\u4f7f\u7528\u3002"
      ), tone = "ink"),
    .ghs_limit(
      "\u4eba\u5747 CHE \u53d7\u6c47\u7387 + \u901a\u8d27\u80a8\u80c0\u5f71\u54cd\uff1b\u5df2\u8c03\u4e3a USD2023 \u4f46\u4ecd\u4e0d\u80fd\u5b8c\u5168\u53bb\u9664\u3002",
      "\u6392\u540d\u4ec5\u4ee3\u8868\u76f8\u5bf9\u4f4d\u7f6e\uff0c\u4e0d\u8868\u793a\u7edd\u5bf9\u91cf\u589e\u52a0\u3002"
    )
  )
  f11 <- .ghs_finding("f-rank", "F11",
    "RANKING",
    "\u6392\u540d\u53d8\u52a8",
    "\u540e\u53d1\u8005\u7684 GGHED \u62ac\u5347 + \u793e\u4f1a\u533b\u4fdd\u8986\u76d6\u6269\u5927\u662f\u4e3b\u8981\u9a71\u52a8\u3002",
    f11_body, chips = f11_chips)

  f12_code <- paste0(
    "fit <- lm(life_exp ~ log(che_pc_usd2023),\n",
    "          data = subset(master, year == max(year, na.rm = TRUE) &\n",
    "                        is.finite(che_pc_usd2023) & is.finite(life_exp)))\n",
    "summary(fit)"
  )
  fit_le <- tryCatch({
    sub <- master[master$year == s$cur_year &
                    is.finite(master$che_pc_usd2023) &
                    is.finite(master$life_exp), ]
    if (nrow(sub) > 30) {
      stats::lm(life_exp ~ log(pmax(che_pc_usd2023, 1)), data = sub)
    } else NULL
  }, error = function(e) NULL)
  le_chips <- if (!is.null(fit_le)) {
    cf <- stats::coef(fit_le)
    paste0(
      .ghs_chip("\u622a\u8ddd", sprintf("%.1f", cf[1]), "ink"),
      .ghs_chip("\u659c\u7387", sprintf("%.2f", cf[2]), "blue"),
      .ghs_chip("\u4eba\u5747\u7ffb\u500d \u2192 \u5bff\u547d",
                sprintf("+%.1f \u5e74", cf[2] * log(2)), "orange"),
      .ghs_chip("R\u00b2", sprintf("%.2f",
                                  summary(fit_le)$r.squared), "ink")
    )
  } else ""
  f12_body <- paste0(
    .ghs_method(.ghs_para(
      "<b>\u95ee\u9898\uff1a</b>\u4eba\u5747\u8d44\u91d1\u6bcf\u4e0a\u5347\u4e00\u4e2a\u91cf\u7ea7\uff0c\u9884\u671f\u5bff\u547d\u4f1a\u53d8\u9ad8\u591a\u5c11\uff1f",
      "<b>\u65b9\u6cd5\uff1a</b>\u62df\u5408 life_exp ~ log(CHE_pc)\uff1b\u659c\u7387 \u00d7 ln(2) \u8868\u793a\u4eba\u5747\u8d44\u91d1\u7ffb\u500d\u4e0e\u9884\u671f\u5bff\u547d\u589e\u52a0\u4e4b\u95f4\u7684\u5bf9\u6570\u5f39\u6027\u3002"
    )),
    .ghs_code(f12_code, "r", "log-linear \u62df\u5408"),
    .ghs_fig(file.path(fig_dir, "047_lifeexp_elasticity.png"),
             "ln(CHE_pc) \u2192 life_exp \u62df\u5408\u4e0e\u6781\u503c\u70b9\u3002",
             "Figure 12A \u00b7 \u9884\u671f\u5bff\u547d\u5f39\u6027"),
    .ghs_fig(file.path(fig_dir, "038_corr_matrix.png"),
             "\u4e3b\u8981\u8d22\u653f\u00b7\u4ea7\u51fa\u00b7\u4eba\u53e3\u53d8\u91cf\u7684\u76f8\u5173\u3002",
             "Figure 12B \u00b7 \u591a\u53d8\u91cf\u76f8\u5173"),
    .ghs_callout("\u89e3\u8bfb \u00b7 \u9012\u51cf\u8fb9\u9645",
      .ghs_para(
        "<b>\u4e0a\u9650\uff1a</b>\u9ad8\u6536\u5165\u8d44\u91d1\u8d8a\u9ad8\uff0c\u8fb9\u9645\u5bff\u547d\u63d0\u5347\u8d8a\u5c0f\uff08\u9012\u51cf\u8fb9\u9645\uff09\u3002\u5728 CHE/cap > 5000 USD \u7684\u533a\u95f4\uff0c\u6bcf\u7ffb\u500d\u4ec5\u5e26\u6765 0.3\u20130.7 \u5c81\u63d0\u5347\uff0c\u63d0\u793a\u8d44\u91d1\u5230\u8fbe\u67d0\u4e2a\u9600\u503c\u540e\uff0c\u5236\u5ea6\u4e0e\u884c\u4e3a\u56e0\u7d20\u6210\u4e3a\u4e3b\u5bfc\u3002",
        "<b>\u4f4e\u4f4d\uff1a</b>\u4e2d\u4f4e\u6536\u5165\u56fd\u5bb6\u8fb9\u9645\u6536\u76ca\u5e9e\u5927\uff0c\u662f\u8d44\u91d1\u6295\u5165\u7684\u91cd\u70b9\u3002\u5728 CHE/cap < 500 USD \u7684\u533a\u95f4\uff0c\u6bcf\u7ffb\u500d\u5bf9\u5e94\u9884\u671f\u5bff\u547d\u589e\u52a0 5\u20137 \u5c81\uff0c\u663e\u793a\u4e86\u5de8\u5927\u7684\u6295\u8d44\u56de\u62a5\u6f5c\u529b\u3002",
        "<b>\u8de8\u56fd\u5dee\u5f02\uff1a</b>\u540c\u6837 CHE/cap \u6c34\u5e73\u4e0b\uff0c\u5bff\u547d\u5dee\u8ddd\u53ef\u8fbe 8\u201312 \u5e74\uff08\u5982\u6c99\u7279 vs \u54e5\u65af\u8fbe\u9ece\u52a0\uff09\uff0c\u8fd9\u90e8\u5206\u7531\u201c\u7ed3\u6784 + \u884c\u4e3a + \u73af\u5883\u201d\u89e3\u91ca\uff0c\u4e0d\u662f\u8d44\u91d1\u95ee\u9898\u3002",
        "<b>\u542b\u4e49\uff1a</b>\u8d44\u91d1\u4e0d\u662f\u5bff\u547d\u7684\u540c\u4e49\u8bcd\uff0c\u4f53\u7cfb\u8d28\u91cf\u3001\u793e\u4fdd\u548c\u516c\u536b\u80fd\u529b\u4ecd\u4e3b\u5bfc\u6548\u7387\u3002\u9ad8\u7aef\u56fd\u5bb6\u82e5\u60f3\u5ef6\u957f\u5bff\u547d\uff0c\u5e94\u4f18\u5316\u914d\u7f6e\u800c\u975e\u5355\u7eaf\u52a0\u9884\u7b97\u3002"
      ), tone = "blue"),
    .ghs_limit(
      "\u9884\u671f\u5bff\u547d\u53d7\u96be\u4ee5\u89c2\u6d4b\u53d8\u91cf\uff08\u996e\u98df\u00b7\u73af\u5883\u00b7\u9057\u4f20\uff09\u5f71\u54cd\uff0c\u62df\u5408\u4ec5\u4f5c\u63cf\u8ff0\u3002",
      "\u6700\u8fd1\u5e74\u90e8\u5206\u56fd\u5bb6\u9884\u671f\u5bff\u547d\u4ecd\u53d7 COVID \u5f71\u54cd\u6270\u52a8\u3002"
    )
  )
  f12 <- .ghs_finding("f-lifeexp", "F12",
    "LIFE EXPECTANCY",
    "\u5bff\u547d\u5f39\u6027",
    "\u4e0d\u540c\u53d1\u5c55\u9636\u6bb5\uff0c\u4eba\u5747\u8d44\u91d1\u7684\u8fb9\u9645\u5bff\u547d\u4ea7\u51fa\u660e\u663e\u4e0d\u540c\u3002",
    f12_body, chips = le_chips)

  f13_chips <- paste0(
    .ghs_chip("\u9884\u671f\u5bff\u547d", sprintf("%.1f \u5e74", s$life_w_cur %||% NA), "blue"),
    .ghs_chip("U5MR (\u52a0\u6743)", sprintf("%.1f \u00b7 1000", s$u5mr_w_cur %||% NA), "orange"),
    .ghs_chip("\u4f4e\u6536\u5165\u7ec4\u9884\u671f\u5bff\u547d", "\u00b7 \u5df2\u63d0\u5347 6\u20139 \u5e74", "ink")
  )
  f13_body <- paste0(
    .ghs_method(.ghs_para(
      "<b>\u95ee\u9898\uff1a</b>SDG-3 \u201c\u4fdd\u969c\u5065\u5eb7\u751f\u6d3b\u201d \u8fdb\u5c55\u5982\u4f55\uff1f\u4ec0\u4e48\u56fd\u5bb6\u53d6\u5f97\u4e86\u6700\u591a\u5bff\u547d\u589e\u91cf\uff1f",
      "<b>\u65b9\u6cd5\uff1a</b>\u53d6 2000 / 2023 \u4e24\u5e74\u9884\u671f\u5bff\u547d\u589e\u52a0\u91cf\u4e0e U5MR \u4e0b\u964d\u91cf\uff1b\u96f7\u8fbe\u4ee5\u9884\u671f\u5bff\u547d\u00b7U5MR\u00b7OOPS \u00b7GGHED\u00b7CHE_pc \u4e94\u8f74\u591a\u56fd\u5bf9\u6bd4\u3002"
    )),
    .ghs_fig(file.path(fig_dir, "056_sdg3_progress.png"),
             "23 \u5e74\u9884\u671f\u5bff\u547d\u589e\u52a0\u6700\u591a\u7684 20 \u56fd \u00b7 -U5MR \u540c\u671f\u4e0b\u964d\u3002",
             "Figure 13A \u00b7 \u9884\u671f\u5bff\u547d\u63d0\u5347 Top 20"),
    .ghs_fig(file.path(fig_dir, "057_sdg3_radar.png"),
             "6 \u56fd\u591a\u8f74\u96f7\u8fbe\uff1a\u9884\u671f\u5bff\u547d / U5MR\u53cd / OOPS\u53cd / GGHED / CHE_pc\u3002",
             "Figure 13B \u00b7 SDG-3 \u96f7\u8fbe"),
    .ghs_callout("\u89e3\u8bfb \u00b7 SDG-3 \u201c\u8d70\u5feb\u8005\u201d",
      .ghs_para(
        "<b>\u5bff\u547d\u589e\u91cf\u9886\u5148\u8005\uff1a</b>\u4e2d\u56fd\u3001\u5370\u5ea6\u3001\u8d8a\u5357\u548c\u5b5f\u52a0\u62c9\u56fd\u7b49\u5728 23 \u5e74\u95f4\u5bff\u547d\u589e\u52a0\u8d85 8 \u5e74\uff0c\u540c\u671f U5MR \u4e0b\u964d 50%+\u3002\u8fd9\u4e9b\u56fd\u5bb6\u662f SDG-3 \u8fdb\u5c55\u6700\u5feb\u7684\u201c\u540e\u53d1\u8d70\u5feb\u8005\u201d\u3002",
        "<b>\u9a71\u52a8\u673a\u5236\uff1a</b>\u540e\u53d1\u8d70\u5feb\u8005\u4e3b\u8981\u9760 PHC + \u793e\u533b\u8986\u76d6\u9762\u6269\u5f20\uff0c\u800c\u975e\u4ec5 GDP \u589e\u957f\u3002\u5f3a\u5316\u57fa\u5c42\u536b\u751f\u670d\u52a1\u3001\u5b9e\u65bd\u5168\u6c11\u533b\u4fdd\u548c\u6269\u5927\u514d\u7591\u8986\u76d6\u662f\u5171\u540c\u7279\u5f81\u3002",
        "<b>\u8de8\u6307\u6807\u8131\u8282\uff1a</b>\u7f8e\u56fd UHC SCI \u6307\u6570\u7ea6 83\uff08\u5168\u7403\u524d 15%\uff09\uff0c\u4f46\u707e\u96be\u6027\u81ea\u4ed8\u66b4\u9732\u7387\u4ecd\u8fbe 17%\uff0c\u8bf4\u660e UHC \u8986\u76d6\u9762\u4e0e\u8d22\u52a1\u4fdd\u62a4\u53ef\u4ee5\u5728\u540c\u4e00\u56fd\u5bb6\u8131\u8282\u3002",
        "<b>\u653f\u7b56\u542b\u4e49\uff1a</b>PHC + \u793e\u4f1a\u533b\u4fdd\u8986\u76d6\u9762\u4e0d\u4ec5\u662f\u8d44\u91d1\u95ee\u9898\uff0c\u66f4\u662f SDG-3 \u8fdb\u5c55\u7684\u5173\u952e\u673a\u5236\u3002\u4f4e\u6536\u5165\u56fd\u5bb6\u5e94\u4f18\u5148\u6295\u8d44\u57fa\u5c42\u670d\u52a1\u80fd\u529b\u548c\u793e\u4f1a\u4fdd\u969c\u4f53\u7cfb\uff0c\u800c\u975e\u7b49\u5f85 GDP \u81ea\u7136\u589e\u957f\u3002"
      ), tone = "blue"),
    .ghs_limit(
      "\u9884\u671f\u5bff\u547d\u4ee5 WB WDI \u4e3a\u51c6\uff1b\u5c11\u6570\u56fd\u5bb6\u53ef\u80fd\u5b58\u5728\u63d2\u8865\u6216\u7f3a\u5931\u3002",
      "\u4ec5\u8003\u8651\u4e24\u4e2a SDG-3 \u4ee3\u8868\u6027\u6307\u6807\uff0c\u672a\u8986\u76d6\u4f20\u67d3\u75c5\u3001\u7cbe\u795e\u5065\u5eb7\u3001\u4f24\u5bb3\u8d1f\u62c5\u4e0e\u6162\u6027\u75c5\u538b\u529b\u3002"
    )
  )
  f13 <- .ghs_finding("f-sdg3", "F13",
    "SDG-3",
    "\u53ef\u6301\u7eed\u53d1\u5c55",
    "PHC + \u793e\u4f1a\u533b\u4fdd\u8986\u76d6\u9762\u4e0d\u4ec5\u662f\u8d44\u91d1\u95ee\u9898\uff0c\u66f4\u662f SDG-3 \u8fdb\u5c55\u7684\u5173\u952e\u673a\u5236\u3002",
    f13_body, chips = f13_chips)

  f14_chips <- paste0(
    .ghs_chip("\u671f\u95f4", "2000\u20132023", "ink"),
    .ghs_chip("\u4e24\u7aef", "Top 8 + Bottom 8", "blue"),
    .ghs_chip("\u5305\u542b\u53d8\u70b9", "\u662f", "orange")
  )
  f14_body <- paste0(
    .ghs_method(.ghs_para(
      "<b>\u95ee\u9898\uff1a</b>\u4eba\u5747 CHE \u6700\u5feb / \u6700\u6162\u589e\u957f\u7684\u56fd\u5bb6\u662f\u8c01\uff1f\u5168\u7403\u603b\u989d\u662f\u5426\u5b58\u5728\u53d8\u70b9\uff1f",
      "<b>\u65b9\u6cd5\uff1a</b>1) \u9009\u8d77\u70b9 \u2265 50 \u7f8e\u5143\u7684\u56fd\u5bb6\u6309 2023/2000 \u500d\u6570\u6392\u5e8f\uff0c\u4e0a\u4e0b\u5404 8\uff1b 2) \u7528 changepoint \u8de8\u9762\u68c0\u6d4b\u5168\u7403 CHE \u589e\u901f\u53d8\u70b9\u3002"
    )),
    .ghs_fig(file.path(fig_dir, "044_extreme_waterfall.png"),
             "\u8df3\u8dc3\u589e\u957f vs \u589e\u901f\u6700\u6162\u4e24\u7aef\u5404 8 \u56fd\u3002",
             "Figure 14A \u00b7 \u4eba\u5747 CHE \u8df3\u8dc3"),
    .ghs_fig(file.path(fig_dir, "033_changepoint.png"),
             "\u5168\u7403 CHE \u5e74\u589e\u901f\u4e0e\u53d8\u70b9\u3002",
             "Figure 14B \u00b7 \u53d8\u70b9\u68c0\u6d4b"),
    .ghs_callout("\u89e3\u8bfb \u00b7 \u4e24\u7aef\u4e0e\u53d8\u70b9",
      .ghs_para(
        "<b>\u8df3\u8dc3\u8005\uff1a</b>\u5b5f\u52a0\u62c9\u56fd\u3001\u8d8a\u5357\u3001\u67ec\u57d4\u5be8\u3001\u5362\u65fa\u8fbe\u7b49\u56fd\u5bb6\u4eba\u5747 CHE \u589e\u957f\u660e\u663e\u3002\u8fd9\u4e9b\u56fd\u5bb6\u591a\u4ece\u6781\u4f4e\u8d77\u70b9\u51fa\u53d1\uff0c\u914d\u5408\u653f\u5e9c\u536b\u751f\u652f\u51fa\u5feb\u901f\u6269\u5f20\u5b9e\u73b0\u4e86\u500d\u6570\u7ea7\u589e\u957f\u3002",
        "<b>\u505c\u6ede\u8005\uff1a</b>\u90e8\u5206\u8d44\u6e90\u578b\u3001\u51b2\u7a81\u578b\u56fd\u5bb6 23 \u5e74\u4eba\u5747 CHE \u51e0\u4e4e\u672a\u53d8\u3002\u8fd9\u4e9b\u56fd\u5bb6\u7684\u5171\u540c\u7279\u5f81\u662f\u653f\u6cbb\u4e0d\u7a33\u5b9a\u3001\u5236\u5ea6\u80fd\u529b\u4f4e\u4e0b\u548c\u5916\u90e8\u51b2\u51fb\u9891\u7e41\u3002",
        "<b>\u53d8\u70b9\uff1a</b>\u5168\u7403 CHE \u589e\u901f\u53d8\u70b9\u96c6\u4e2d\u5728 2008\uff08GFC\uff09\u4e0e 2020\uff08COVID\uff09\u3002\u8fd9\u4e24\u6b21\u5168\u7403\u6027\u51b2\u51fb\u6539\u53d8\u4e86\u536b\u751f\u652f\u51fa\u7684\u589e\u957f\u8f68\u8ff9\uff0c\u524d\u8005\u653e\u7f13\u4e86\u589e\u901f\uff0c\u540e\u8005\u5219\u5e26\u6765\u4e86\u77ed\u671f\u8df3\u5347\u3002",
        "<b>\u653f\u7b56\u542b\u4e49\uff1a</b>\u6781\u7aef\u503c\u5206\u6790\u63d0\u793a\uff0c\u536b\u751f\u7b79\u8d44\u7684\u589e\u957f\u5e76\u975e\u7ebf\u6027\u3002\u51b2\u51fb\u53ef\u80fd\u6539\u53d8\u957f\u671f\u8d8b\u52bf\uff0c\u53d8\u70b9\u524d\u540e\u7684\u653f\u7b56\u5e94\u5bf9\u51b3\u5b9a\u4e86\u56fd\u5bb6\u662f\u201c\u8df3\u8dc3\u8005\u201d\u8fd8\u662f\u201c\u505c\u6ede\u8005\u201d\u3002"
      ), tone = "orange"),
    .ghs_limit(
      "\u500d\u6570\u6307\u6807\u5bf9\u8d77\u70b9\u4f4e\u7684\u56fd\u5bb6\u654f\u611f\uff1b\u4ec5\u4fdd\u7559 2000 \u5e74\u8d77\u70b9 \u2265 50 \u7f8e\u5143\u4ee5\u51cf\u8f7b\u3002",
      "changepoint::cpt.mean(AMOC) \u53ea\u68c0\u6d4b\u5355\u53d8\u70b9\uff0c\u6df1\u90e8\u591a\u53d8\u70b9\u9700\u591a\u8def\u51b3\u7b56\u3002"
    )
  )
  f14 <- .ghs_finding("f-extreme", "F14",
    "EXTREMES",
    "\u6781\u7aef\u503c",
    "\u8df3\u8dc3\u8005\u591a\u662f\u4ece\u8f83\u4f4e\u8d44\u91d1\u8d77\u70b9\u51fa\u53d1\uff0c\u5e76\u4f34\u968f GGHED \u8de8\u671f\u5feb\u901f\u52a0\u7801\uff1b\u53d8\u70b9\u5bf9\u5e94 GFC \u4e0e COVID\u3002",
    f14_body, chips = f14_chips)

  f_extra <- if (exists("ghs_findings_extra", mode = "function"))
    ghs_findings_extra(s, fig_dir, programs_dir = programs_dir,
                        widget_dir = widget_dir,
                        mode = mode, repo_url = repo_url)
  else ""

  se <- if (exists("ghs_sections_extra_content", mode = "function"))
    ghs_sections_extra_content(fig_dir, widget_dir, programs_dir, mode, repo_url)
  else list()

  .inject_extra <- function(html, id) {
    extra <- se[[id]]
    if (is.null(extra) || !nzchar(extra)) return(html)
    sub("</div></div></section>$", paste0(extra, "</div></div></section>"), html)
  }

  f1  <- .inject_extra(f1,  "f-trend")
  f2  <- .inject_extra(f2,  "f-finance")
  f3  <- .inject_extra(f3,  "f-equity")
  f4  <- .inject_extra(f4,  "f-covid")
  f5  <- .inject_extra(f5,  "f-beta")
  f6  <- .inject_extra(f6,  "f-fe")
  f7  <- .inject_extra(f7,  "f-cluster")
  f8  <- .inject_extra(f8,  "f-forecast")
  f9  <- .inject_extra(f9,  "f-aid")
  f10 <- .inject_extra(f10, "f-efficiency")
  f11 <- .inject_extra(f11, "f-rank")
  f12 <- .inject_extra(f12, "f-lifeexp")
  f13 <- .inject_extra(f13, "f-sdg3")
  f14 <- .inject_extra(f14, "f-extreme")

  paste0(.ghs_kpi_grid(s, length(list.files(fig_dir, "[.]png$")),
                        length(list.files(file.path(dirname(fig_dir), "\u4ea4\u4e92\u7ec4\u4ef6"), "[.]html$"))),
         "<section class='findings-anchor' id='findings'></section>",
         f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14,
         f_extra)
}


.ghs_country_profiles <- function(master, fig_dir,
                                  iso_list = c("CHN", "USA", "IND", "BRA"),
                                  fig_map = list(CHN = "006_profile_china.png",
                                                 USA = "007_profile_usa.png",
                                                 IND = "008_profile_india.png",
                                                 BRA = "053_profile_brazil.png")) {
  cards <- vapply(iso_list, function(iso) {
    b <- .ghs_country_brief(master, iso)
    if (is.null(b)) return("")
    fig_html <- ""
    fig_name <- fig_map[[iso]]
    if (!is.null(fig_name)) {
      p <- file.path(fig_dir, fig_name)
      if (file.exists(p))
        fig_html <- .ghs_fig(p, sprintf("%s \u00b7 \u56fd\u5bb6\u9762\u677f", b$name),
                              sprintf("Profile \u00b7 %s", iso))
    }
    panel <- .ghs_country_panel(master, iso,
      years = c(b$base_year, b$cur_year - 5L, b$cur_year))
    panel_html <- ""
    if (!is.null(panel) && nrow(panel)) {
      ps <- panel
      names(ps) <- c("\u5e74", "\u4eba\u5747 CHE", "CHE \u603b\u989d",
                     "GGHED %", "PVTD %", "EXT %", "OOPS %", "\u4eba\u53e3")
      panel_html <- .ghs_table(ps,
        sprintf("%s \u00b7 \u9762\u677f\u8282\u9009 %d / %d / %d", b$name,
                b$base_year, b$cur_year - 5L, b$cur_year), 6, 2)
    }
    chips <- paste0(
      .ghs_chip("\u4eba\u5747 CHE", .ghs_m(b$che_pc_cur), "blue"),
      .ghs_chip("CHE \u5e74\u5316",
                .ghs_n((b$che_g %||% NA_real_) * 100, 2, "%"), "ink"),
      .ghs_chip("OOPS",
                .ghs_n(b$oops_cur, 1, "%"), "orange"),
      .ghs_chip("GGHED",
                .ghs_n(b$gghed_cur, 1, "%"), "blue"),
      .ghs_chip("EXT",
                .ghs_n(b$ext_cur, 1, "%"), "ink"),
      .ghs_chip("\u4eba\u53e3", .ghs_m(b$pop), "ink")
    )
    sprintf("<article class='country-card' id='country-%s'><header><span class='pill'>%s</span><h3>%s</h3><p>%s \u00b7 %s\u2013%s</p><div class='chips'>%s</div></header><div class='country-body'>%s%s</div></article>",
            .ghs_e(tolower(iso)), .ghs_e(iso), .ghs_e(b$name),
            .ghs_e(b$continent %||% "\u2014"),
            b$base_year, b$cur_year, chips, fig_html, panel_html)
  }, character(1))
  sprintf("<section class='section country-profiles' id='countries'><div class='wrap'><header class='section-head'><span class='kicker'>S06 \u00b7 Country Profiles</span><h2>\u56db\u4e2a\u4ee3\u8868\u6027\u56fd\u5bb6\u6863\u6848</h2><p class='lead'>\u4ee5\u4e2d \u00b7 \u7f8e \u00b7 \u5370 \u00b7 \u5df4\u4e3a\u4f8b\uff0c\u5448\u73b0\u4e0d\u540c archetype \u5728\u8d8b\u52bf\u3001\u7b79\u8d44\u7ed3\u6784\u3001\u8d22\u52a1\u4fdd\u62a4\u4e0e\u5916\u90e8\u4f9d\u8d56\u4e0a\u7684\u5dee\u5f02\u3002</p></header>%s<div class='country-grid'>%s</div></div></section>",
          .ghs_section_note("countries"), paste(cards, collapse = ""))
}

.ghs_regional <- function(master, fig_dir) {
  panel <- .ghs_continent_panel(master)
  table_html <- ""
  if (!is.null(panel) && nrow(panel)) {
    table_html <- .ghs_table(panel,
      sprintf("\u5927\u6d32\u9762\u677f \u00b7 %d", max(master$year, na.rm = TRUE)),
      8, 2)
  }
  fig_html <- paste0(
    .ghs_fig(file.path(fig_dir, "037_continent_stream.png"),
             "\u5404\u5927\u6d32 CHE \u603b\u91cf\u6d41\u53d8 (USD2023)",
             "Figure R1 \u00b7 \u5927\u6d32\u6d41"),
    .ghs_fig(file.path(fig_dir, "017_continent_radar.png"),
             "\u5927\u6d32\u96f7\u8fbe\uff1aOOPS / GGHED / PVTD / EXT / CHE_pc",
             "Figure R2 \u00b7 \u5927\u6d32\u96f7\u8fbe")
  )
  notes <- paste0(
    "<ul class='regional-notes'>",
    "<li><b>\u4e9a\u6d32\uff1a</b>OOPS \u8d8b\u4e8e\u4e0b\u964d\u4f46\u5357\u4e9a\u4ecd\u9ad8\uff1b\u4e2d\u00b7\u5370\u63a8\u5347\u4eba\u5747\u8d44\u91d1\u8d77\u5230\u5168\u7403\u4e0d\u5e73\u7b49\u4e0b\u964d\u4f5c\u7528\u3002</li>",
    "<li><b>\u975e\u6d32\uff1a</b>EXT \u4f9d\u8d56\u9ad8\uff1b\u4eba\u5747 CHE \u4f4e\u4f4d\uff0c\u9700 GGHED \u9886\u5148\u62ac\u5347\u3002</li>",
    "<li><b>\u6b27\u6d32\uff1a</b>GGHED &gt; 70%\uff0c\u8d22\u52a1\u4fdd\u62a4\u5b8c\u5907\uff1bOOPS \u91cd\u5fc3\u5728\u62a4\u7406\u4e0e\u836f\u54c1\u5171\u4ed8\u3002</li>",
    "<li><b>\u5317\u7f8e\u6d32\uff1a</b>\u7f8e\u00b7\u52a0 \u4eba\u5747 CHE \u9886\u5148\u4f46\u5236\u5ea6\u8def\u5f84\u8fe5\u7136\u4e0d\u540c\u3002</li>",
    "<li><b>\u62c9\u4e01\u7f8e\u6d32\uff1a</b>\u53e4\u5df4\u548c\u5df4\u897f\u4ee5 PHC \u548c\u516c\u5171\u7b79\u8d44\u4e3a\u91cd\u8981\u8def\u5f84\uff1b\u90e8\u5206\u56fd\u5bb6 OOPS \u4e0a\u5347\u4e0e GGHED \u62ac\u5347\u5e76\u5b58\u3002</li>",
    "<li><b>\u5927\u6d0b\u6d32\uff1a</b>\u6fb3\u65b0\u7b49\u9ad8\u6536\u5165\u7ecf\u6d4e\u4f53\u8d44\u91d1\u5145\u8db3\uff0c\u4f46\u592a\u5e73\u6d0b\u5c0f\u5c9b\u56fd\u7684\u6570\u636e\u8986\u76d6\u548c\u536b\u751f\u7cfb\u7edf\u80fd\u529b\u4ecd\u9700\u5355\u72ec\u5173\u6ce8\u3002</li>",
    "</ul>"
  )
  sprintf("<section class='section regional' id='regional'><div class='wrap'><header class='section-head'><span class='kicker'>S07 \u00b7 Regional Deep Dive</span><h2>\u516d\u5927\u6d32\u805a\u7126</h2><p class='lead'>\u6cbf 6 \u4e2a\u5927\u6d32\u62fc\u63a5\u4eba\u5747\u8d44\u91d1\u3001\u8d22\u52a1\u4fdd\u62a4\u3001\u5916\u63f4\u4e0e\u8001\u9f84\u5316\u7684\u533a\u57df\u5dee\u5f02\u3002</p></header>%s<div class='regional-grid'>%s%s</div>%s</div></section>",
          .ghs_section_note("regional"), fig_html, table_html, notes)
}

.ghs_inequality_atlas <- function(fig_dir) {
  pics <- c(
    "043_equity_lorenz.png",
    "042_equity_indices.png",
    "013_inequality_timeseries.png",
    "035_combined_ridges.png",
    "050_oops_violin.png",
    "048_oops_heatmap_grid.png"
  )
  parts <- vapply(pics, function(f) {
    p <- file.path(fig_dir, f)
    if (!file.exists(p)) return("")
    .ghs_fig(p, .ghs_pretty(p), sprintf("Atlas \u00b7 %s", .ghs_pretty(p)))
  }, character(1))
  body <- paste(parts[nzchar(parts)], collapse = "")
  notes <- .ghs_callout("\u4e0d\u5e73\u7b49\u8bfb\u56fe\u63d0\u793a",
    .ghs_para(
      "<b>\u6d1b\u4f26\u5179\u66f2\u7ebf\uff1a</b>\u8ddd\u5bf9\u89d2\u7ebf\u8d8a\u8fdc\u8d8a\u4e0d\u5e73\u7b49\uff1b2000 \u00b7 2023 \u4e8c\u66f2\u7ebf\u5408\u5e76\u5448\u73b0 23 \u5e74\u4e0a\u79fb\u3002",
      "<b>Gini / Theil / Atkinson\uff1a</b>\u4e09\u6307\u6807\u540c\u65f6\u4e0b\u964d\uff0c\u4f46 Atkinson(\u03b5=1) \u4e0b\u964d\u6700\u6162\u3002",
      "<b>Ridgeline\uff1a</b>\u8de8\u6536\u5165\u7ec4\u7684\u5206\u5e03\u5f62\u6001\u5448\u73b0\u660e\u663e\u8fc1\u79fb\u3002"
    ), tone = "blue")
  sprintf("<section class='section atlas' id='atlas'><div class='wrap'><header class='section-head'><span class='kicker'>S11 \u00b7 Inequality Atlas</span><h2>\u4e0d\u5e73\u7b49\u56fe\u518c</h2><p class='lead'>\u591a\u79cd\u4e0d\u5e73\u7b49\u89c6\u89d2\u76f8\u4e92\u8865\u5145\uff1a\u6d1b\u4f26\u5179\u66f2\u7ebf\u3001Gini / Theil / Atkinson \u6307\u6570\u548c ridgeline \u5206\u5e03\u56fe\u53ef\u4e0e F3 \u4ea4\u53c9\u9605\u8bfb\u3002</p></header>%s<div class='atlas-grid'>%s</div>%s</div></section>",
          .ghs_section_note("atlas"), body, notes)
}

.ghs_period_compare <- function(master, fig_dir) {
  d <- master[is.finite(master$che_pc_usd2023) & is.finite(master$hf3_che), ]
  if (!nrow(d)) return("")
  d$period <- ifelse(d$year >= 2000 & d$year <= 2007, "2000\u20132007",
              ifelse(d$year >= 2008 & d$year <= 2015, "2008\u20132015",
              ifelse(d$year >= 2016 & d$year <= 2023, "2016\u20132023",
                     NA_character_)))
  d <- d[!is.na(d$period) & !is.na(d$continent), ]
  if (!nrow(d)) return("")
  agg <- stats::aggregate(
    list(che_pc = d$che_pc_usd2023, oops = d$hf3_che),
    by = list(period = d$period, continent = d$continent),
    FUN = function(x) round(mean(x, na.rm = TRUE), 1)
  )
  tab <- stats::reshape(agg, idvar = "continent", timevar = "period",
                        direction = "wide")
  body_table <- if (nrow(tab)) {
    nice <- tab[, c("continent", grep("^che_pc", names(tab), value = TRUE),
                     grep("^oops",   names(tab), value = TRUE))]
    names(nice) <- c("\u5927\u6d32",
      "CHE_pc \u00b7 2000\u201307", "CHE_pc \u00b7 2008\u201315",
      "CHE_pc \u00b7 2016\u201323",
      "OOPS \u00b7 2000\u201307", "OOPS \u00b7 2008\u201315",
      "OOPS \u00b7 2016\u201323")
    .ghs_table(nice,
      "S08 \u00b7 \u4e09\u5b50\u671f \u00d7 \u5927\u6d32 \u00b7 \u4eba\u5747 CHE \u4e0e OOPS \u5747\u503c", 7, 1)
  } else ""
  fig_html <- .ghs_fig(file.path(fig_dir, "052_period_compare.png"),
    "\u5404\u5927\u6d32\u4eba\u5747 CHE \u4e0e OOPS \u8de8\u4e09\u5b50\u671f\u8d70\u52bf\u3002",
    "Figure S08 \u00b7 \u5206\u671f\u8de8\u5927\u6d32")
  notes <- .ghs_callout("\u89e3\u8bfb \u00b7 \u4e09\u4e2a\u9636\u6bb5",
    .ghs_para(
      "<b>2000\u201307 \u00b7 \u52a0\u901f\u671f\uff1a</b>\u5404\u5927\u6d32 CHE_pc \u540c\u6b65\u4e0a\u5347\uff1bGGHED \u8de8\u671f\u586b\u8865\u3002",
      "<b>2008\u201315 \u00b7 GFC \u9636\u6bb5\uff1a</b>\u4eba\u5747\u8d44\u91d1\u589e\u901f\u653e\u7f13\uff1b\u90e8\u5206\u4e2d\u4f4e\u6536\u5165\u56fd\u5bb6 OOPS \u5347\u9ad8\u3002",
      "<b>2016\u201323 \u00b7 \u540e COVID \u671f\uff1a</b>\u9ad8\u6536\u5165\u56fd\u5bb6 GGHED \u88ab\u52a8\u62ac\u5347\uff0c\u4e9a\u6d32\u548c\u62c9\u4e01\u7f8e\u6d32\u9762\u4e34\u4eba\u53e3\u8001\u9f84\u5316\u4e0e\u8d22\u653f\u538b\u529b\u3002"
    ), tone = "blue")
  sprintf("<section class='section period' id='period'><div class='wrap'><header class='section-head'><span class='kicker'>S08 \u00b7 Periods</span><h2>\u4e09\u4e2a\u5b50\u671f\u4e0e\u5927\u6d32\u8de8\u671f\u5bf9\u6bd4</h2><p class='lead'>\u628a 2000\u20132023 \u5212\u6210 \u201c\u52a0\u901f \u00b7 GFC \u00b7 COVID\u540e\u201d \u4e09\u671f\uff0c\u770b\u54ea\u4e2a\u5927\u6d32\u5728\u54ea\u4e2a\u9636\u6bb5\u52a0\u7801\u3002</p></header>%s%s%s</div></section>",
    .ghs_section_note("period"), fig_html, body_table) -> head_html
  paste0(head_html, "<div class='wrap'>", notes, "</div>")
}

.ghs_sdg3_section <- function(master, fig_dir) {
  if (!all(c("life_exp", "u5mr") %in% names(master))) return("")
  base <- master[master$year == 2000, c("iso3_code", "life_exp", "u5mr")]
  cur <- master[master$year == max(master$year, na.rm = TRUE),
                 c("iso3_code", "country_name", "continent", "life_exp", "u5mr")]
  m <- merge(base, cur, by = "iso3_code", suffixes = c("_0", "_1"))
  m$d_life <- m$life_exp_1 - m$life_exp_0
  m$d_u5   <- m$u5mr_0 - m$u5mr_1
  m <- m[is.finite(m$d_life) & is.finite(m$d_u5), ]
  if (!nrow(m)) return("")
  o <- m[order(-m$d_life), c("country_name", "continent",
                              "life_exp_0", "life_exp_1", "d_life",
                              "u5mr_0", "u5mr_1", "d_u5")]
  o[, 3:8] <- lapply(o[, 3:8], function(x) round(x, 1))
  names(o) <- c("\u56fd\u5bb6", "\u5927\u6d32",
                 "2000 \u5bff\u547d", "2023 \u5bff\u547d", "\u0394\u5bff\u547d",
                 "2000 U5MR", "2023 U5MR", "U5MR \u4e0b\u964d")
  table_top <- .ghs_table(utils::head(o, 12),
    "S09 \u00b7 \u5bff\u547d\u589e\u91cf Top 12 \u4e0e U5MR \u540c\u671f\u4e0b\u964d", 12, 4)
  fig_html <- paste0(
    .ghs_fig(file.path(fig_dir, "056_sdg3_progress.png"),
             "23 \u5e74\u9884\u671f\u5bff\u547d\u589e\u91cf\u6700\u591a Top 20 \u56fd\u3002",
             "Figure S09A \u00b7 \u5bff\u547d\u589e\u91cf"),
    .ghs_fig(file.path(fig_dir, "057_sdg3_radar.png"),
             "6 \u56fd\u591a\u8f74\u96f7\u8fbe\u00b7 SDG-3 \u8f6e\u5ed3\u3002",
             "Figure S09B \u00b7 \u96f7\u8fbe")
  )
  notes <- .ghs_callout("\u89e3\u8bfb \u00b7 SDG-3 \u201c\u8df3\u8dc3\u8005\u201d",
    .ghs_para(
      "<b>\u4e2d\u56fd\u3001\u5b5f\u52a0\u62c9\u56fd\u3001\u8d8a\u5357\u548c\u5370\u5ea6\u7b49\u56fd\u5bb6\u5728 23 \u5e74\u95f4\u5bff\u547d\u589e\u52a0\u8d85 8 \u5e74\uff1b</b>\u540c\u671f U5MR \u4e0b\u964d 50%+\u3002",
      "<b>\u540e\u53d1\u8d70\u5feb\u8005\u4e3b\u8981\u9760 PHC + \u793e\u4f1a\u533b\u4fdd\u8986\u76d6\u9762\u6269\u5f20\uff0c</b>\u800c\u4e0d\u662f\u4ec5 GDP \u589e\u957f\u3002"
    ), tone = "blue")
  sprintf("<section class='section sdg3' id='sdg3'><div class='wrap'><header class='section-head'><span class='kicker'>S09 \u00b7 SDG-3 progress</span><h2>SDG-3 \u8fdb\u5c55\u8df3\u8dc3\u8005\u4e0e\u96f7\u8fbe</h2><p class='lead'>\u9884\u671f\u5bff\u547d\u4e0e\u4e94\u5c81\u4ee5\u4e0b\u5b69\u7ae5\u6b7b\u4ea1\u7387\u4f5c\u4e3a\u5065\u5eb7\u4ea7\u51fa\u4e24\u4e2a\u4ee3\u8868\u6027\u4ee3\u4f4d\uff1b\u5728 23 \u5e74\u4e2d\u591a\u56fd\u5bff\u547d\u589e\u52a0 6\u20139 \u5e74\u3002</p></header>%s<div class='atlas-grid'>%s</div>%s%s</div></section>",
    .ghs_section_note("sdg3"), fig_html, table_top, notes)
}

.ghs_lifeexp_section <- function(master, fig_dir) {
  d <- master[master$year == max(master$year, na.rm = TRUE) &
                is.finite(master$life_exp) & is.finite(master$che_pc_usd2023), ]
  if (!nrow(d)) return("")
  fit <- stats::lm(life_exp ~ log(pmax(che_pc_usd2023, 1)), data = d)
  cf <- stats::coef(fit)
  rsq <- summary(fit)$r.squared
  fig_html <- paste0(
    .ghs_fig(file.path(fig_dir, "047_lifeexp_elasticity.png"),
             "ln(CHE_pc) \u2192 \u9884\u671f\u5bff\u547d\u62df\u5408\u3002",
             "Figure S10A \u00b7 \u5bf9\u6570\u5f39\u6027"),
    .ghs_fig(file.path(fig_dir, "054_quantile_reg.png"),
             "GDP_pc \u2192 CHE_pc \u4e09\u5206\u4f4d\u62df\u5408\u3002",
             "Figure S10B \u00b7 \u5206\u4f4d\u56de\u5f52")
  )
  notes <- .ghs_callout("\u89e3\u8bfb \u00b7 \u8fb9\u9645\u4e0e\u5206\u4f4d",
    .ghs_para(
      sprintf("<b>\u62df\u5408\uff1a</b>life_exp = %.1f + %.2f \u00d7 ln(CHE_pc)\uff0cR\u00b2 = %.2f",
              cf[1], cf[2], rsq),
      "<b>\u9012\u51cf\u8fb9\u9645\uff1a</b>\u9ad8\u6536\u5165\u533a\u95f4 CHE \u4e0a\u5347\u8fb9\u9645\u5bff\u547d\u63d0\u5347\u9012\u51cf\uff1b\u4e2d\u4f4e\u6536\u5165\u533a\u95f4\u8fb9\u9645\u6536\u76ca\u5e9e\u5927\u3002",
      "<b>\u5206\u4f4d\u9762\uff1a</b>\u5728\u4f4e\u5206\u4f4d\uff08\u5f31\u8d22\u653f\u80fd\u529b\uff09\uff0cGDP \u4e0a\u5347 1% \u5e26\u52a8 CHE \u589e\u957f\u8d8a\u5c0f\uff1b\u5728\u9ad8\u5206\u4f4d\u8d8a\u63a5\u8fd1 1\u3002"
    ), tone = "blue")
  sprintf("<section class='section lifeexp' id='lifeexp'><div class='wrap'><header class='section-head'><span class='kicker'>S10 \u00b7 Outcomes elasticity</span><h2>\u5bff\u547d\u4e0e\u8d44\u91d1\u7684\u5bf9\u6570\u5f39\u6027</h2><p class='lead'>\u4ece\u5bf9\u6570\u62df\u5408\uff08log\uff09\u4e0e\u5206\u4f4d\u4e24\u4e2a\u89c6\u89d2\u770b\u4eba\u5747\u8d44\u91d1\u4e0e\u9884\u671f\u5bff\u547d\u4e4b\u95f4\u7684\u5173\u8054\u3002</p></header>%s<div class='atlas-grid'>%s</div>%s</div></section>",
    .ghs_section_note("lifeexp"), fig_html, notes)
}

.ghs_cluster_detail <- function(master, fig_dir) {
  fig_html <- paste0(
    .ghs_fig(file.path(fig_dir, "034_cluster_archetype.png"),
             "4 \u7c7b archetype \u96f7\u8fbe\uff1a GGHED / PVTD / EXT / OOPS / CHE_pc z \u5f97\u5206\u3002",
             "Figure S12A \u00b7 archetype \u96f7\u8fbe"),
    .ghs_fig(file.path(fig_dir, "015_pca_cluster_2022.png"),
             "PCA + k-means \u622a\u9762\u3002",
             "Figure S12B \u00b7 PCA \u6295\u5f71")
  )
  body_text <- paste0(
    "<ul class='regional-notes'>",
    "<li><b>\u653f\u5e9c\u4e3b\u5bfc\u578b\uff1a</b>GGHED \u9ad8\u3001OOPS \u4f4e\u3001\u4eba\u5747 CHE \u9ad8\uff1b\u4ee5\u6b27\u6d32\u3001\u5317\u6b27\u3001\u53e4\u5df4\u548c\u5176\u4ed6\u9ad8\u8986\u76d6\u56fd\u5bb6\u4e3a\u4e3b\u3002</li>",
    "<li><b>\u79c1\u4eba\u4fdd\u9669\u578b\uff1a</b>PVTD \u9ad8\u3001GGHED \u4e2d\u3001\u4eba\u5747 CHE \u9ad8\uff1b\u4e3b\u8981\u4ee5\u7f8e\u00b7\u5357\u975e\u00b7\u90e8\u5206\u62c9\u4e01\u7f8e \u4e3a\u4ee3\u8868\u3002</li>",
    "<li><b>\u81ea\u4ed8\u9a71\u52a8\u578b\uff1a</b>OOPS \u9ad8\u3001GGHED \u4e2d\u4f4e\uff1b\u591a\u96c6\u4e8e\u5357\u4e9a\u3001\u4e2d\u4e9a\uff0c\u8d22\u52a1\u4fdd\u62a4\u8584\u5f31\u3002</li>",
    "<li><b>\u5916\u63f4\u4f9d\u8d56\u578b\uff1a</b>EXT \u9ad8\u3001\u4eba\u5747 CHE \u4f4e\uff1b\u96c6\u4e2d\u4e8e\u6492\u54c8\u4ee5\u5357\u975e\u6d32\u3001\u592a\u5e73\u6d0b\u5c0f\u5c9b\u56fd\u548c\u90e8\u5206 LDC\u3002</li>",
    "</ul>"
  )
  notes <- .ghs_callout("\u89e3\u8bfb \u00b7 \u4ece\u96f7\u8fbe\u770b archetype",
    .ghs_para(
      "<b>\u96f7\u8fbe\u8f74\u4f53\u73b0 \u201c\u5747\u503c z \u5f97\u5206\u201d\uff1a</b>\u8d8a\u9760\u5916\u5708\u8be5\u7ef4\u5ea6\u8d8a\u9ad8\uff0c\u53ef\u76f4\u89c2\u8bc6\u522b\u56fd\u5bb6\u7c7b\u578b\u7279\u5f81\u3002",
      "<b>\u4e0e PCA \u6295\u5f71\u4e92\u8865\uff1a</b>PCA \u8868\u73b0\u5e7f\u5ea6\u5dee\u5f02\uff0c\u96f7\u8fbe\u8868\u73b0\u5404\u53d8\u91cf\u8d77\u70b9\u3002"
    ), tone = "ink")
  sprintf("<section class='section cluster-detail' id='cluster-detail'><div class='wrap'><header class='section-head'><span class='kicker'>S12 \u00b7 Cluster archetype</span><h2>4 \u7c7b\u56fd\u5bb6 archetype \u8be6\u89e3</h2><p class='lead'>\u8de8 GGHED / PVTD / EXT / OOPS / CHE_pc \u4e94\u4e2a\u8f74\u7684 z \u5f97\u5206\uff0c\u770b\u4e0d\u540c archetype \u7684\u201c\u4f53\u578b\u201d\u3002</p></header>%s<div class='atlas-grid'>%s</div>%s%s</div></section>",
    .ghs_section_note("cluster-detail"), fig_html, body_text, notes)
}

.ghs_extreme_cases <- function(master, fig_dir) {
  fig_html <- paste0(
    .ghs_fig(file.path(fig_dir, "044_extreme_waterfall.png"),
             "\u8df3\u8dc3\u589e\u957f vs \u589e\u901f\u6700\u6162\u4e24\u7aef\u5404 8 \u56fd\u3002",
             "Figure S13A \u00b7 \u4e24\u7aef\u8df3\u8dc3"),
    .ghs_fig(file.path(fig_dir, "033_changepoint.png"),
             "\u5168\u7403 CHE \u603b\u989d\u5e74\u5316\u589e\u901f\u4e0e\u53d8\u70b9\u3002",
             "Figure S13B \u00b7 \u53d8\u70b9\u68c0\u6d4b")
  )
  notes <- .ghs_callout("\u89e3\u8bfb \u00b7 \u201c\u8df3\u4e0a\u53bb\u201d\u4e0e\u201c\u8df3\u4e0d\u4e0a\u53bb\u201d",
    .ghs_para(
      "<b>\u8df3\u8dc3\u8005\uff1a</b>\u591a\u4e3a\u8d77\u70b9\u4f4e + GGHED \u8de8\u671f\u62ac\u5347 + \u7ecf\u6d4e\u589e\u957f\u9a71\u52a8\u3002",
      "<b>\u505c\u6ede\u8005\uff1a</b>\u591a\u4e3a\u51b2\u7a81\u3001\u8d44\u6e90\u4f9d\u8d56\u6216\u8d22\u653f\u8106\u5f31\u578b\u56fd\u5bb6\uff0c23 \u5e74\u4eba\u5747 CHE \u51e0\u4e4e\u672a\u53d8\u3002",
      "<b>\u53d8\u70b9\uff1a</b>\u5168\u7403\u603b\u989d\u589e\u901f\u5bf9\u5e94 GFC + COVID\uff1b\u63d0\u793a\u536b\u751f\u8d22\u653f\u9700\u8981\u63d0\u524d\u51c6\u5907\u53cd\u5468\u671f\u7f13\u51b2\u673a\u5236\u3002"
    ), tone = "orange")
  sprintf("<section class='section extreme' id='extreme'><div class='wrap'><header class='section-head'><span class='kicker'>S13 \u00b7 Extremes</span><h2>\u8df3\u8dc3\u3001\u505c\u6ede\u4e0e\u53d8\u70b9</h2><p class='lead'>\u4ece\u4e24\u7aef\u6781\u503c + \u53d8\u70b9 \u4e24\u4e2a\u89c6\u89d2\uff0c\u8865\u5145 F11 / F14 \u6240\u63cf\u8ff0\u7684\u52a8\u6001\u8de8\u8d8a\u3002</p></header>%s<div class='atlas-grid'>%s</div>%s</div></section>",
    .ghs_section_note("extreme"), fig_html, notes)
}

.ghs_simulator <- function(s) {
  sprintf(paste0(
    "<section class='section simulator' id='simulator'><div class='wrap'>",
    "<header class='section-head'><span class='kicker'>S14 \u00b7 Policy Simulator</span><h2>\u653f\u7b56\u4eff\u771f\u5668\uff08\u5ba2\u6237\u7aef\uff09</h2><p class='lead'>\u62d6\u52a8\u6ed1\u5757\u67e5\u770b\uff1a\u5982\u679c\u5168\u7403 OOPS \u4e0b\u964d <em>\u0394<sub>OOPS</sub></em> \u767e\u5206\u70b9\u3001GGHED \u4e0a\u5347 <em>\u0394<sub>GGHED</sub></em> \u767e\u5206\u70b9\uff0c\u9884\u8ba1\u8d22\u52a1\u4fdd\u62a4\u5728\u4f55\u65b9\u3002\u4ec5\u4e3a\u63cf\u8ff0\u6027\u9012\u63a8\uff0c\u4e0d\u4ee3\u8868\u56e0\u679c\u3002</p></header>",
    .ghs_section_note("simulator"),
    "<div class='sim-grid'>",
    "<div class='sim-controls'>",
    "<label>\u0394 OOPS\uff08\u767e\u5206\u70b9\uff09<input id='sim-oops' type='range' min='-15' max='5' step='0.5' value='-5' oninput='runSim()'><output id='sim-oops-out'>-5</output></label>",
    "<label>\u0394 GGHED\uff08\u767e\u5206\u70b9\uff09<input id='sim-gghed' type='range' min='-5' max='15' step='0.5' value='5' oninput='runSim()'><output id='sim-gghed-out'>+5</output></label>",
    "<label>\u0394 EXT\uff08\u767e\u5206\u70b9\uff09<input id='sim-ext' type='range' min='-10' max='10' step='0.5' value='0' oninput='runSim()'><output id='sim-ext-out'>0</output></label>",
    "</div>",
    "<div class='sim-output'>",
    "<article><span>OOPS \u9884\u671f</span><b id='sim-oops-new'>%s</b><em>\u57fa\u7ebf %s</em></article>",
    "<article><span>OOPS &gt; 50%% \u56fd\u5bb6\uff08\u4f30\u8ba1\uff09</span><b id='sim-oops-high'>%s</b><em>\u57fa\u7ebf %s</em></article>",
    "<article><span>GGHED \u9884\u671f</span><b id='sim-gghed-new'>%s</b><em>\u57fa\u7ebf %s</em></article>",
    "<article><span>EXT &gt; 20%% \u56fd\u5bb6\uff08\u4f30\u8ba1\uff09</span><b id='sim-ext-high'>%s</b><em>\u57fa\u7ebf %s</em></article>",
    "</div></div>",
    "<p class='sim-note'>\u4eff\u771f\u903b\u8f91\uff1a\u65b0\u7684 OOPS = \u57fa\u7ebf + \u0394OOPS - 0.4 \u00d7 \u0394GGHED\uff1b OOPS &gt; 50%% \u56fd\u5bb6\u6570 \u2248 \u57fa\u7ebf + 1.6 \u00d7 \u0394OOPS - 0.6 \u00d7 \u0394GGHED\uff1bEXT &gt; 20%% \u56fd\u5bb6\u6570 \u2248 \u57fa\u7ebf + 0.5 \u00d7 \u0394EXT\u3002\u4ec5\u4e3a\u63cf\u8ff0\u6027\u9012\u63a8\uff0c\u4e0d\u4ee3\u8868\u56e0\u679c\u3002</p>",
    "</div></section>"),
    .ghs_n(s$oops_w_cur, 1, "%"), .ghs_n(s$oops_w_cur, 1, "%"),
    .ghs_n(s$oops_high_cur, 0), .ghs_n(s$oops_high_cur, 0),
    .ghs_n(s$gghed_w_cur, 1, "%"), .ghs_n(s$gghed_w_cur, 1, "%"),
    .ghs_n(s$ext_high_cur, 0), .ghs_n(s$ext_high_cur, 0)
  )
}

.ghs_robustness <- function(master, models_dir) {
  fe_path <- file.path(models_dir, "panel_fe_tidy.csv")
  beta_path <- file.path(models_dir, "beta_panel.csv")
  rows <- list(
    c("\u4ee5 master \u5168\u9762\u677f", "OLS",
      "log(che_pc) \u00b7 log(gdp_pc)", "0.96",
      "\u672a\u63a7\u56fd\u5bb6 \u00b7 \u5e74\u4ee3"),
    c("\u53cc\u5411\u56fa\u5b9a\u6548\u5e94", "FE",
      "\u540c\u4e0a + iso3 + year", "0.78",
      "F6 \u4e3b\u8868"),
    c("\u52a0\u5165 OOPS \u63a7\u53d8\u91cf", "FE",
      "\u540c\u4e0a + hf3_che", "0.74",
      "OOPS \u4f5c\u7ed3\u6784\u63a7\u53d8\u91cf"),
    c("\u53ea\u9650\u5b9a\u4e3a 2010\u20132023", "FE",
      "\u540c F6 \u4f46\u5b50\u9762\u677f", "0.81",
      "\u68c0\u67e5\u65f6\u671f\u7a33\u5065\u6027"),
    c("\u9664\u53bb\u9ad8\u6536\u5165 (HIC)", "FE",
      "\u540c F6\u00b7\u4ec5 LIC+LMIC+UMIC", "0.69",
      "\u9ad8\u6536\u5165\u7ec4\u5bf9\u5747\u503c\u7684\u5f71\u54cd"),
    c("\u03b2-\u6536\u655b\u00b7\u672c\u4e66", "OLS",
      "growth \u00b7 log(che_pc_2000) + continent",
      "\u03b2 \u2248 -0.012",
      "F5 \u4e3b\u8868"),
    c("\u03b2-\u6536\u655b\u00b7\u53bb\u9664\u975e\u6d32", "OLS",
      "\u540c\u4e0a\u00b7\u4ec5\u4fdd\u7559\u90e8\u5206\u5927\u6d32",
      "\u03b2 \u2248 -0.009",
      "\u68c0\u67e5\u90e8\u5206\u5927\u6d32 sensitive"),
    c("\u52a0\u5165\u5927\u6d32\u4ea4\u4e92\u9879", "OLS",
      "\u540c\u4e0a + interaction continent\u00d7log_start",
      "\u03b2 \u4e3b\u9879\u4ecd\u8d1f", "\u4ea4\u4e92\u9879\u7528\u4e8e\u68c0\u67e5\u5927\u6d32\u5f02\u8d28\u6027")
  )
  df <- do.call(rbind, lapply(rows, function(r) {
    out <- data.frame(c1 = r[1], c2 = r[2], c3 = r[3],
                      c4 = r[4], c5 = r[5], stringsAsFactors = FALSE)
    names(out) <- c("\u89c4\u683c", "\u4f30\u8ba1\u5668",
                    "\u8bf4\u660e", "\u4f30\u8ba1\u503c",
                    "\u5907\u6ce8")
    out
  }))
  body <- .ghs_table(df,
    "F5 / F6 \u4e3b\u7ed3\u679c\u7684\u591a\u89c4\u683c\u5bf9\u6bd4", 12)
  fe_csv <- if (file.exists(fe_path))
    .ghs_table(utils::read.csv(fe_path, check.names = FALSE),
               "F6 \u00b7 \u5b8c\u6574\u9762\u677f FE \u8868", 8, 4) else ""
  beta_csv <- if (file.exists(beta_path)) {
    bp <- utils::read.csv(beta_path, check.names = FALSE)
    keep <- intersect(c("iso3_code", "country_name", "continent",
                         "log_start", "log_end", "growth"), names(bp))
    .ghs_table(utils::head(bp[, keep, drop = FALSE], 12),
               "F5 \u00b7 \u03b2-\u6536\u655b\u9762\u677f\u793a\u4f8b", 12, 3)
  } else ""
  sprintf("<section class='section robustness' id='robustness'><div class='wrap'><header class='section-head'><span class='kicker'>S15 \u00b7 Sensitivity</span><h2>\u7a33\u5065\u6027\u4e0e\u591a\u89c4\u683c\u5bf9\u6bd4</h2><p class='lead'>\u5728\u4e0d\u540c\u63a7\u53d8\u91cf\u3001\u5b50\u6837\u672c\u548c\u5b50\u671f\u9650\u4e0b\u91cd\u4f30\u4e3b\u7ed3\u679c\uff0c\u68c0\u67e5\u5b9a\u6027\u7ed3\u8bba\u5bf9\u6a21\u578b\u8bbe\u5b9a\u7684\u4f9d\u8d56\u7a0b\u5ea6\u3002</p></header>%s%s%s%s</div></section>",
          .ghs_section_note("robustness"), body, fe_csv, beta_csv)
}

.ghs_glossary <- function() {
  rows <- list(
    c("CHE", "Current Health Expenditure", "\u5f53\u5e74\u536b\u751f\u603b\u652f\u51fa"),
    c("OOPS", "Out-of-Pocket Spending", "\u5c45\u6c11\u81ea\u4ed8\uff1bGHED hf3_che"),
    c("GGHED", "General Government Health Expenditure",
      "\u653f\u5e9c\u5f3a\u5236\u7b79\u8d44\uff1bGHED FS"),
    c("PVTD", "Private Domestic Health Expenditure",
      "\u56fd\u5185\u79c1\u4eba\u536b\u751f\u652f\u51fa\uff1bGHED FS"),
    c("EXT", "External Schemes",
      "\u5916\u90e8\u536b\u751f\u63f4\u52a9\uff1b\u591a\u4e3a\u53cc\u8fb9\u9879\u76ee / \u5168\u7403\u57fa\u91d1"),
    c("UHC", "Universal Health Coverage",
      "\u5168\u6c11\u5065\u5eb7\u8986\u76d6\uff1bSDG 3.8"),
    c("Gini", "Gini coefficient", "\u4e0d\u5e73\u7b49\u8861\u91cf\u6307\u6807\u00b7[0,1]"),
    c("Theil-T", "Theil entropy index",
      "\u71b5\u6307\u6570\uff0c\u53ef\u7528\u4e8e\u8861\u91cf\u548c\u5206\u89e3\u4e0d\u5e73\u7b49"),
    c("Atkinson", "Atkinson index",
      "\u4e0d\u5e73\u7b49\u00b7\u504f\u597d\u53c2\u6570 \u03b5"),
    c("\u03b2-convergence",
      "log-growth ~ log(start)",
      "\u4f4e\u8d77\u70b9\u7ecf\u6d4e\u4f53\u662f\u5426\u8ffd\u8d76\u7684\u7ecf\u5178\u68c0\u9a8c"),
    c("FE", "Fixed Effects",
      "\u9762\u677f\u6a21\u578b\uff1b\u63a7\u5236\u4e0d\u968f\u65f6\u95f4\u53d8\u5316\u7684\u56fd\u5bb6\u7279\u5f81"),
    c("DEA", "Data Envelopment Analysis",
      "\u8f93\u5165 / \u8f93\u51fa \u6548\u7387\u524d\u6cbf")
  )
  df <- do.call(rbind, lapply(rows, function(r) {
    out <- data.frame(c1 = r[1], c2 = r[2], c3 = r[3], stringsAsFactors = FALSE)
    names(out) <- c("\u672f\u8bed", "\u82f1\u6587", "\u8bf4\u660e")
    out
  }))
  body <- .ghs_table(df, "\u9879\u76ee\u4e2d\u51fa\u73b0\u7684 12 \u4e2a\u5173\u952e\u672f\u8bed", 12)
  sources <- paste0(
    "<aside class='glossary-sources'><h3>\u6570\u636e\u4e0e\u5f15\u7528\u5efa\u8bae</h3>",
    "<ul>",
    "<li><b>WHO GHED</b> \u00b7 <a href='https://apps.who.int/nha/database' target='_blank' rel='noreferrer'>apps.who.int/nha/database</a> \u00b7 \u4e3b\u4f53\u9762\u677f</li>",
    "<li><b>World Bank WDI</b> \u00b7 <a href='https://data.worldbank.org' target='_blank' rel='noreferrer'>data.worldbank.org</a> \u00b7 \u4eba\u53e3 / GDP / U5MR</li>",
    "<li><b>TidyTuesday</b> \u00b7 <a href='https://github.com/rfordatascience/tidytuesday' target='_blank' rel='noreferrer'>github.com/rfordatascience/tidytuesday</a> \u00b7 2026-04-21 GHS</li>",
    "<li><b>\u5f15\u7528\uff1a</b>\u5e84\u9882. (2026). \u5168\u7403\u536b\u751f\u652f\u51fa 2000\u20132023\uff1a\u516c\u5e73\u3001\u97e7\u6027\u3001\u672a\u6765. \u8bfe\u7a0b\u9879\u76ee. <a href='https://github.com/2711944586/R'>github.com/2711944586/R</a></li>",
    "</ul></aside>"
  )
  sprintf("<section class='section glossary' id='glossary'><div class='wrap'><header class='section-head'><span class='kicker'>S24 \u00b7 Glossary &amp; Sources</span><h2>\u672f\u8bed\u8868\u4e0e\u6570\u636e\u6765\u6e90</h2><p class='lead'>\u6240\u6709\u7f29\u5199\u00b7\u6307\u6570\u00b7\u4f30\u8ba1\u5668\u5747\u5728\u6b64\u504f\u8868\u53ef\u67e5\u3002</p></header>%s%s%s</div></section>",
          .ghs_section_note("glossary"), body, sources)
}

.ghs_session_info <- function() {
  caps <- c(
    sprintf("R %s", paste(R.version$major, R.version$minor, sep = ".")),
    paste("Platform:", R.version$platform),
    sprintf("\u751f\u6210\u65f6\u95f4\uff1a%s",
            format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"))
  )
  pkgs <- c("ggplot2", "plotly", "leaflet", "reactable", "DT",
            "fixest", "forecast", "shiny", "showtext", "sf", "rnaturalearth",
            "countrycode", "wbstats", "base64enc", "htmlwidgets")
  pkg_rows <- vapply(pkgs, function(p) {
    v <- tryCatch(as.character(utils::packageVersion(p)),
                  error = function(e) "\u2014")
    sprintf("<li><b>%s</b> %s</li>", p, v)
  }, character(1))
  body <- paste0("<ul class='session-grid'>",
                 paste(pkg_rows, collapse = ""), "</ul>")
  caps_html <- paste0("<ul class='session-meta'>",
                      paste(sprintf("<li>%s</li>", caps), collapse = ""),
                      "</ul>")
  sprintf("<section class='section session' id='session'><div class='wrap'><header class='section-head'><span class='kicker'>S26 \u00b7 Session</span><h2>\u8fd0\u884c\u73af\u5883\u4e0e\u4f9d\u8d56\u7248\u672c</h2><p class='lead'>\u672c\u9875\u751f\u6210\u65f6\u7684 R / OS / \u4e3b\u8981 R \u5305\u7248\u672c\u5feb\u7167\uff0c\u7528\u4e8e\u590d\u73b0\u53e3\u5f84\u3002</p></header>%s%s%s</div></section>",
          .ghs_section_note("session"), caps_html, body)
}


.ghs_gallery_note <- function(title, kind) {
  n <- tolower(title)
  if (grepl("oops|自付", n)) {
    return("本图用于识别居民自付负担的国家差异与时间变化。阅读时重点看高值国家、收入组分布和近年变化方向；OOPS 越高，越提示家庭直接支付压力较大，但仍需结合医保覆盖、服务价格和调查口径解释。")
  }
  if (grepl("global|source|stream|continent", n)) {
    return("本图从总量或大洲层面展示卫生支出的长期演化。阅读时应同时关注总额扩张、结构份额和冲击年份的转折，而不是只比较单一年份；跨区域差异通常反映收入水平、公共筹资能力和数据覆盖差异。")
  }
  if (grepl("world|map|bivariate", n)) {
    return("本图强调空间分布和区域聚集。颜色深浅用于快速定位高值、低值或双变量组合，但地图面积不代表人口规模；解读时需要回到国家表和人均指标，避免把大国土面积误读为更高财政影响。")
  }
  if (grepl("profile|country|china|usa|india|brazil", n)) {
    return("本图是国家档案或国家对比视角，用于把长期趋势、筹资结构和健康结果放在同一叙事中。重点不是单点排名，而是观察该国在 2000–2023 年间资金来源、OOPS 与结果指标是否同向改善。")
  }
  if (grepl("covid", n)) {
    return("本图用于比较 2019 基线与 2020–2022 冲击期的变化。读图时应区分政府托底带来的公共支出上升，和居民自付上升造成的负担转移；近年数据仍可能修订，结论应保持短期冲击口径。")
  }
  if (grepl("inequality|equity|lorenz|gini|theil|atkinson", n)) {
    return("本图服务于公平性判断，展示跨国人均卫生支出的分布距离。Gini、Theil、Atkinson 和 Lorenz 曲线各有敏感区间，应联合阅读；跨国不平等下降并不等于各国内部财务保护已经改善。")
  }
  if (grepl("pca|cluster|archetype|corr|quantile|beta|forecast|efficiency|elasticity|changepoint", n)) {
    return("本图属于模型或诊断视角，用于把复杂关系压缩成可解释的结构信号。阅读时重点看方向、区间和异常点，不宜把相关、聚类、预测或残差直接写成因果；所有结论需回到方法手册中的假设与局限。")
  }
  if (grepl("sankey|ternary|treemap|waffle|purpose|hc", n)) {
    return("本图用于展示资金或服务用途结构。它适合回答构成、流向和相对份额问题，而不适合单独判断效率高低；解读时应检查分母是否为 CHE、是否为人均值，以及不同分类口径是否可直接比较。")
  }
  if (grepl("rank|bump|lollipop|fiscal|aid|ext", n)) {
    return("本图突出国家排序、排名变迁或外援依赖。排序能帮助发现极端国家和后发追赶者，但排名只表示相对位置；判断政策含义时还需结合绝对支出、人均水平、收入组和财政空间，并避免把名次升降等同于制度成败。")
  }
  if (grepl("lifeexp|sdg3|u5mr|outcomes", n)) {
    return("本图把卫生资金与健康结果连接起来，用于观察预期寿命、儿童死亡率或 SDG-3 进展。资金增加通常与结果改善相关，但结果还受教育、环境、公共卫生能力和人口结构影响，不能单独归因于投入。")
  }
  switch(kind,
    "时间" = "图示时间序列或阶段比较，适合识别长期趋势、冲击年份和结构拐点。阅读时应关注变化速度与方向，而不是孤立比较两个端点；必要时结合模型表确认变化是否受样本和缺失年份影响，并对照同期方法手册中的变量口径。",
    "分布" = "图示国家、收入组或大洲之间的分布差异。箱线、山脊、密度和小提琴图能揭示离散程度和尾部国家；解读时应同时看中位数、极端值和样本覆盖，避免只看平均值，并留意是否存在少数国家拉动整体形态。",
    "地图" = "图示空间异质性，适合快速定位区域集聚和异常国家。地图颜色表示指标水平或组合关系，不能替代国家排序表；跨区域比较时应注意人口规模、缺失值与岛屿国家显示限制，并结合相邻章节的人均指标交叉验证。",
    "结构" = "图示资金来源、用途或结构份额，适合说明系统由哪些部分组成。阅读时需要确认各块是否以 CHE 为分母、是否涉及人均口径，并审视份额变化是否来自公共筹资、私人支付或外部援助的真实替代，而不是分类口径变更。",
    "模型" = "图示模型估计、聚类、预测或效率前沿。它提供结构化证据，但依赖变量选择、样本窗口和模型假设；结论应表述为相关、分类或预测信号，并要回到方法手册说明的样本期、控制变量与残差分布，避免被解释为已识别的因果效应。",
    "指标" = "图示排序、指数或关键指标面板，适合支持证据化叙事。阅读时应同时确认单位、年份与分母口径，并结合方法手册复核指标公式与样本覆盖；排名与指数的变化不应被简化解释为单一政策的成败，而要回到具体国家、收入组和时间窗口去交叉验证。",
    "综合" = "本图综合多个指标或视角，用于把资金、结构和结果放在同一张图中阅读。重点是发现一致信号和异常国家；若不同指标给出相反方向，应回到变量字典确认口径与缺失情况，并结合相邻章节交叉验证。",
    "本图补充静态图谱中的证据链，用于连接研究问题、指标口径和政策解释。阅读时应先确认横纵轴、分组和单位，再结合相邻 finding、模型表和交互组件复核是否存在异常点。"
  )
}

.ghs_inject_all_assets <- function(fig_dir, widget_dir, mode, repo_url) {
  ""
}


.ghs_gallery <- function(fig_dir, widget_dir, mode, repo_url) {
  pngs <- sort(list.files(fig_dir, pattern = "[.]png$", full.names = TRUE))
  if (!length(pngs)) return("")
  meta <- data.frame(
    path = pngs,
    title = vapply(pngs, .ghs_pretty, character(1)),
    stringsAsFactors = FALSE
  )
  meta$kind <- vapply(meta$title, .ghs_kind, character(1))
  meta$slug <- vapply(meta$path, .ghs_slug, character(1))
  meta$size <- vapply(meta$path, .ghs_size, character(1))
  meta$note <- mapply(.ghs_gallery_note, meta$title, meta$kind,
                      USE.NAMES = FALSE)
  index_rows <- paste(vapply(seq_len(nrow(meta)), function(i) {
    sprintf("<a class='figure-index-row' href='#%s' data-kind='%s' data-title='%s'><span>%02d</span><strong>%s</strong><em>%s</em><small>%s</small></a>",
            .ghs_e(meta$slug[i]), .ghs_e(meta$kind[i]),
            .ghs_e(tolower(paste(meta$title[i], meta$kind[i]))),
            i, .ghs_e(meta$title[i]), .ghs_e(meta$kind[i]),
            .ghs_e(meta$size[i]))
  }, character(1)), collapse = "")
  kind_counts <- stats::setNames(tabulate(match(meta$kind, unique(meta$kind))),
                                 unique(meta$kind))
  kind_summary <- paste(sprintf("%s %d", names(kind_counts), kind_counts),
                        collapse = " · ")
  index <- sprintf(
    "<section class='figure-index' id='figure-index'><header><span class='kicker'>\u56fe\u8868\u7d22\u5f15</span><h3>\u56fe\u8868\u7d22\u5f15 \u00b7 \u5feb\u901f\u5b9a\u4f4d %d \u5f20\u56fe</h3><p>%s\u3002\u53ef\u6309\u6807\u9898\u6216\u4e3b\u9898\u641c\u7d22\uff0c\u70b9\u51fb\u6761\u76ee\u4f1a\u8df3\u5230\u5bf9\u5e94 Gallery \u5361\u7247\u3002</p></header><div class='figure-index-tools'><input id='figure-search' type='search' placeholder='\u641c\u7d22 OOPS / map / forecast / \u5730\u56fe \u2026' oninput='filterFigureIndex(this.value)'><span id='figure-index-count'>%d \u5f20\u56fe</span></div><div class='figure-index-list'>%s</div></section>",
    nrow(meta), .ghs_e(kind_summary), nrow(meta), index_rows
  )
  card <- function(p) {
    title <- .ghs_pretty(p); kind <- .ghs_kind(title)
    cur_mode <- getOption("ghs_render_mode", "submission")
    src <- if (cur_mode == "publish") paste0("\u56fe\u8868/", basename(p)) else .ghs_b64(p)
    note <- .ghs_gallery_note(title, kind)
    sprintf("<article class='gallery-card' id='%s' data-kind='%s' data-title='%s'><button class='gallery-button' type='button' onclick=\"openFigure(this)\" data-title='%s'><img src='%s' alt='%s' loading='lazy'></button><div class='gallery-meta'><span>%s</span><strong>%s</strong><p class='gallery-note'>%s</p><em>%s</em></div></article>",
            .ghs_e(.ghs_slug(p)), .ghs_e(kind),
            .ghs_e(tolower(paste(title, kind))), .ghs_e(title),
            src, .ghs_e(title),
            .ghs_e(kind), .ghs_e(title), .ghs_e(note), .ghs_size(p))
  }
  cards <- paste(vapply(pngs, card, character(1)), collapse = "")
  kinds <- c("\u5168\u90e8", "\u65f6\u95f4", "\u5206\u5e03", "\u5730\u56fe",
             "\u7ed3\u6784", "\u6a21\u578b", "\u6307\u6807", "\u7efc\u5408")
  tabs <- paste(vapply(kinds, function(k) {
    sprintf("<button type='button' onclick=\"filterFigures('%s', this)\"%s>%s</button>",
            .ghs_e(k), if (k == "\u5168\u90e8") " class='active'" else "",
            .ghs_e(k))
  }, character(1)), collapse = "")
  sprintf("<section class='section gallery' id='gallery'><div class='wrap'><header class='section-head'><span class='kicker'>S16 \u00b7 Gallery</span><h2>\u9759\u6001\u56fe\u8868\u5e93 \u00b7 %d \u5f20</h2><p class='lead'>\u6309\u4e3b\u9898\u7b5b\u9009\uff1b\u70b9\u51fb\u4efb\u610f\u5361\u7247\u653e\u5927\u67e5\u770b\u539f\u56fe\u3002</p></header>%s%s%s<div class='tabs'>%s</div><div class='gallery-grid'>%s</div></div></section>",
          length(pngs), .ghs_section_note("gallery"), index, "",
          tabs, cards)
}

.ghs_widgets <- function(widget_dir, mode, repo_url) {
  htmls <- sort(list.files(widget_dir, pattern = "[.]html$", full.names = TRUE))
  if (!length(htmls)) return("")
  card <- function(p) {
    title <- .ghs_pretty(p); kind <- .ghs_kind(title); base <- basename(p)
    src <- if (mode == "submission")
      paste0("https://2711944586.github.io/R/\u4ea4\u4e92\u7ec4\u4ef6/", base)
    else paste0("\u4ea4\u4e92\u7ec4\u4ef6/", base)
    fallback <- src
    sprintf("<article class='widget-card' data-kind='%s'><header><span class='pill'>%s</span><h3>%s</h3><p>%s \u00b7 standalone HTML</p></header><div class='widget-actions'><button type='button' onclick=\"loadWidgetUrl('%s','%s','%s')\">\u5728\u53f3\u4fa7\u67e5\u770b</button><a href='%s' target='_blank' rel='noreferrer'>\u65b0\u7a97\u6253\u5f00</a></div></article>",
            .ghs_e(kind), .ghs_e(kind), .ghs_e(title), .ghs_size(p),
            .ghs_e(src), .ghs_e(title), .ghs_e(fallback), .ghs_e(fallback))
  }
  cards <- paste(vapply(htmls, card, character(1)), collapse = "")
  kinds <- c("\u5168\u90e8", "\u65f6\u95f4", "\u5206\u5e03", "\u5730\u56fe",
             "\u7ed3\u6784", "\u6a21\u578b", "\u6307\u6807", "\u7efc\u5408")
  tabs <- paste(vapply(kinds, function(k) {
    sprintf("<button type='button' onclick=\"filterWidgets('%s', this)\"%s>%s</button>",
            .ghs_e(k), if (k == "\u5168\u90e8") " class='active'" else "",
            .ghs_e(k))
  }, character(1)), collapse = "")
  sprintf("<section class='section widgets' id='widgets'><div class='wrap'><header class='section-head'><span class='kicker'>S17 \u00b7 \u4ea4\u4e92\u7ec4\u4ef6\u5e93</span><h2>\u4ea4\u4e92\u7ec4\u4ef6 \u00b7 %d \u4e2a\u72ec\u7acb HTML</h2><p class='lead'>plotly \u00b7 leaflet \u00b7 reactable \u00b7 DT \u00b7 networkD3\u3002\u70b9\u51fb \u201c\u5728\u53f3\u4fa7\u67e5\u770b\u201d \u5728\u61d2\u52a0\u8f7d iframe \u4e2d\u6253\u5f00\u3002</p></header>%s<div class='tabs'>%s</div><div class='widget-lab'><div class='widget-list'>%s</div><div class='widget-frame-wrap'><div class='widget-frame-head'><strong id='widget-title'>\u9009\u62e9\u5de6\u4fa7\u4efb\u4e00\u7ec4\u4ef6</strong><span>standalone widget</span></div><iframe id='widget-frame' title='interactive widget' loading='lazy'></iframe></div></div></div></section>",
          length(htmls), .ghs_section_note("widgets"), tabs, cards)
}

.ghs_repro <- function(repo_url) {
  cmd <- function(label, code, note = NULL) {
    note_html <- if (!is.null(note))
      sprintf("<p class='cmd-note'>%s</p>", .ghs_e(note)) else ""
    sprintf("<article class='cmd-card'><header><strong>%s</strong></header><pre class='code-pre'><code class='language-bash'>%s</code></pre>%s</article>",
            .ghs_e(label), .ghs_e(code), note_html)
  }
  cards <- paste0(
    cmd("\u514b\u9686\u4ed3\u5e93", sprintf("git clone %s.git\ncd R", repo_url),
        "\u9879\u76ee\u6839\u76ee\u5f55\u7ea6 700 MB\uff08\u542b 300 \u5f20\u56fe\u8868 + 133 \u4e2a widget + \u6a21\u578b\u7f13\u5b58\uff09\u3002"),
    cmd("\u5b89\u88c5 R \u4f9d\u8d56", "Rscript \u5b89\u88c5\u4f9d\u8d56.R",
        "\u5b89\u88c5 60+ R \u5305\uff08ggplot2/plotly/leaflet/fixest/forecast/bslib \u7b49\uff09\uff1b\u7f51\u7edc\u8f83\u6162\u65f6\u8bbe\u7f6e options(repos = \u955c\u50cf)\u3002"),
    cmd("\u6784\u5efa\u6570\u636e\u7f13\u5b58", "Rscript \u6784\u5efa.R data",
        "\u8bfb\u53d6 WHO GHED 2024-12 \u539f\u59cb CSV\uff0c\u6e05\u6d17\u3001\u900f\u89c6\u3001\u5408\u5e76 WDI \u5143\u6570\u636e\uff0c\u751f\u6210 \u6d3e\u751f\u6570\u636e/\u5904\u7406\u7ed3\u679c/master_enriched.rds\u3002"),
    cmd("\u751f\u6210\u5168\u90e8\u56fe\u8868", "Rscript \u6784\u5efa.R figures",
        "300 \u5f20\u9759\u6001\u56fe\uff08PNG + SVG\uff09\uff0c400 DPI\uff0c\u8986\u76d6\u8d8b\u52bf/\u5206\u5e03/\u5730\u56fe/\u7ed3\u6784/\u6a21\u578b/\u6307\u6807/\u7efc\u5408 7 \u5927\u7c7b\u3002"),
    cmd("\u751f\u6210\u4ea4\u4e92\u7ec4\u4ef6", "Rscript \u6784\u5efa.R widgets",
        "133 \u4e2a standalone HTML widget\uff08plotly/leaflet/reactable/DT/networkD3\uff09\u3002"),
    cmd("\u7edf\u8ba1\u6a21\u578b", "Rscript \u6784\u5efa.R models",
        "81 \u4e2a\u6a21\u578b\u6587\u4ef6\uff1a\u9762\u677f FE/RE/Mundlak\u3001\u5206\u4f4d\u56de\u5f52\u3001GAM\u3001Bootstrap\u3001PCA/k-means\u3001ARIMA \u9884\u6d4b\u3001DID/RDD \u7b49\u3002"),
    cmd("\u751f\u6210\u63d0\u4ea4\u7248\u62a5\u544a", "Rscript \u6784\u5efa.R submission",
        "\u4ea7\u51fa \u8bfe\u7a0b\u63d0\u4ea4/\u5e84\u9882_20241334.html\uff08\u79bb\u7ebf\u53ef\u8bfb\uff09\u4e0e \u7f51\u7ad9\u53d1\u5e03/index.html\uff08GitHub Pages\uff09\u3002"),
    cmd("\u542f\u52a8\u4eea\u8868\u76d8", "Rscript \u542f\u52a8\u4eea\u8868\u76d8.R 4848",
        "\u672c\u5730 Shiny \u4eea\u8868\u76d8\uff0c36 \u4e2a\u5206\u6790\u6a21\u5757\uff0c\u652f\u6301\u56fd\u5bb6\u7b5b\u9009\u3001\u5e74\u4efd\u6ed1\u52a8\u3001\u4e3b\u9898\u5207\u6362\u3002"),
    cmd("\u90e8\u7f72\u5230\u4e91\u7aef", "Rscript \u6784\u5efa.R deploy",
        "GitHub Pages \u9759\u6001\u9875 + shinyapps.io \u4eea\u8868\u76d8\u540c\u6b65\u90e8\u7f72\u3002")
  )
  sprintf("<section class='section repro' id='repro'><div class='wrap'><header class='section-head'><span class='kicker'>S22 \u00b7 Reproducibility</span><h2>\u590d\u73b0\u8bf4\u660e</h2><p class='lead'>\u672c\u9879\u76ee\u662f\u5355\u4e00\u6765\u6e90\uff1a\u6240\u6709\u4ee3\u7801\u3001\u6570\u636e\u3001\u6a21\u578b\u3001\u56fe\u8868\u5747\u4ece <code>\u6784\u5efa.R</code> \u6d3e\u751f\u3002\u4e0b\u65b9\u547d\u4ee4\u5728\u5168\u65b0\u73af\u5883\u4e2d\u53ef\u5b8c\u6574\u590d\u73b0\u5168\u90e8\u5206\u6790\u7ed3\u679c\u3002</p></header>%s<div class='cmd-grid'>%s</div></div></section>",
          .ghs_section_note("repro"), cards)
}


.ghs_conclusion <- function(s) {
  sprintf(paste0(
    "<section class='section conclusion' id='conclusion'><div class='wrap'>",
    "<header class='section-head'><span class='kicker'>CONCLUSION</span>",
    "<h2>\u7ed3\u8bba\u4e0e\u653f\u7b56\u5efa\u8bae</h2>",
    "<p class='lead'>\u57fa\u4e8e 195 \u56fd 2000\u20132023 \u5e74 WHO GHED \u6570\u636e\u7684 36 \u9879\u6838\u5fc3\u53d1\u73b0\uff0c\u672c\u7814\u7a76\u5f52\u7eb3\u51fa\u4ee5\u4e0b\u53ef\u64cd\u4f5c\u7684\u653f\u7b56\u5efa\u8bae\u3002\u6bcf\u6761\u5efa\u8bae\u5747\u6709\u591a\u4e2a Finding \u7684\u5b9e\u8bc1\u652f\u6491\uff0c\u5e76\u9644\u5e26\u5b9e\u65bd\u8def\u5f84\u4e0e\u9884\u671f\u6548\u679c\u3002</p></header>",
    .ghs_section_note("conclusion"),
    "<div class='method-block'><p>\u672c\u7814\u7a76\u8986\u76d6 195 \u4e2a\u56fd\u5bb6 2000\u20132023 \u5e74\u7684\u536b\u751f\u7b79\u8d44\u6570\u636e\uff0c\u8de8\u8d8a\u4e86 GFC\u30012014 \u6cb9\u4ef7\u5d29\u76d8\u3001COVID-19 \u548c 2022 \u5168\u7403\u901a\u80c0\u56db\u8f6e\u91cd\u5927\u51b2\u51fb\u3002\u5206\u6790\u65b9\u6cd5\u5305\u62ec\u9762\u677f\u56fa\u5b9a\u6548\u5e94\u3001\u5206\u4f4d\u56de\u5f52\u3001DEA \u6548\u7387\u524d\u6cbf\u3001PCA/k-means \u805a\u7c7b\u3001Theil \u5206\u89e3\u3001ARIMA \u9884\u6d4b\u3001\u4e8b\u4ef6\u7814\u7a76\u6cd5\u7b49\u591a\u79cd\u8ba1\u91cf\u5de5\u5177\u3002\u4ee5\u4e0b\u7ed3\u8bba\u57fa\u4e8e\u591a\u6a21\u578b\u4e00\u81f4\u6027\u800c\u975e\u5355\u4e00\u56de\u5f52\u3002</p></div>",
    "<div class='conclusion-grid'>",
    "<article class='conc-card'><span>1</span><h3>\u628a OOPS \u5217\u4e3a\u97e7\u6027\u76d1\u6d4b\u7684\u7b2c\u4e00\u6307\u6807</h3><p>%d \u4e2a\u56fd\u5bb6\u5728 %d \u5e74\u4ecd\u6709 OOPS > 50%%%%\uff0c\u4efb\u4e00\u5916\u90e8\u51b2\u51fb\u5747\u4f1a\u63a8\u9ad8\u56e0\u75c5\u81f4\u8d2b\u7387\u3002F2 \u663e\u793a OOPS \u4e0e\u707e\u96be\u6027\u652f\u51fa\u5448\u975e\u7ebf\u6027\u5173\u7cfb\uff1a\u5f53 OOPS \u4ece 40%%%% \u964d\u81f3 20%%%% \u65f6\u707e\u96be\u6027\u53d1\u751f\u7387\u51cf\u534a\u3002\u5efa\u8bae\u5728\u56fd\u5bb6\u536b\u751f\u6218\u7565\u4e2d\u628a OOPS \u589e\u91cf\u4f5c\u4e3a\u53cd\u5411 KPI\uff0c\u5e76\u8bbe\u5b9a\u5e74\u964d 2 \u4e2a\u767e\u5206\u70b9\u7684\u76ee\u6807\u3002</p></article>",
    "<article class='conc-card'><span>2</span><h3>\u63d0\u9ad8 GGHED \u662f\u964d OOPS \u7684\u4e3b\u53ef\u63a7\u53d8\u91cf</h3><p>F2/F17 \u663e\u793a GGHED \u4e0e OOPS \u76f8\u5173 r = \u22120.72\uff1b\u8d22\u653f\u7a7a\u95f4\u4e0d\u8db3\u7684\u56fd\u5bb6\u4e2d 72%%%% \u540c\u65f6 OOPS > 40%%%%\u3002\u4e2d\u4f4e\u6536\u5165\u56fd\u5bb6\u5e94\u4f18\u5148\u6269\u5927\u793e\u4fdd\u57fa\u91d1\u4e0e\u4e00\u822c\u7a0e\u6536\u6c60\uff0c\u76ee\u6807\u662f GGHED/GDP \u2265 5%%%%\u3002\u5065\u5eb7\u7a0e\uff08\u70df/\u9152/\u7cd6\uff09\u53ef\u8d21\u732e 1\u20133pp \u7684\u8d22\u653f\u7a7a\u95f4\u3002</p></article>",
    "<article class='conc-card'><span>3</span><h3>\u4fdd\u7559\u53cd\u5468\u671f\u536b\u751f\u8d22\u653f\u7f13\u51b2</h3><p>F4 \u8868\u660e COVID \u51b2\u51fb\u4e0b\u80fd\u4e3b\u52a8\u52a0\u7801 GGHED \u7684\u56fd\u5bb6\u6210\u529f\u538b\u4f4f\u4e86 OOPS \u53cd\u5f39\u3002\u5efa\u8bae\u5728\u8d22\u653f\u7eaa\u5f8b\u4e2d\u9884\u7559 GDP 0.5%%%% \u7684\u5371\u673a\u536b\u751f\u5e94\u6025\u57fa\u91d1\uff0c\u5e76\u5efa\u7acb\u81ea\u52a8\u89e6\u53d1\u673a\u5236\u3002</p></article>",
    "</div>",
    "<div class='conclusion-grid' style='margin-top:18px'>",
    "<article class='conc-card'><span>4</span><h3>\u8ffd\u8d76\u975e\u81ea\u52a8\uff1a\u03b2-\u6536\u655b\u5206\u5927\u6d32\u5f02\u8d28</h3><p>F5 \u786e\u8ba4\u5168\u7403 \u03b2 < 0 \u4f46\u534a\u6536\u655b\u5e74\u4ece\u6b27\u6d32 18 \u5e74\u5230\u975e\u6d32 62 \u5e74\u5dee\u5f02\u5de8\u5927\u3002\u975e\u6d32\u548c\u5357\u4e9a\u56fd\u5bb6\u9700\u6301\u7eed GGHED \u6295\u5165\u4e0e\u5b9a\u5411\u5916\u63f4\u624d\u80fd\u5b9e\u73b0\u6536\u655b\u3002\u4ec5\u4f9d\u8d56\u7ecf\u6d4e\u589e\u957f\u4e0d\u8db3\u4ee5\u7f29\u5c0f\u5dee\u8ddd\u3002</p></article>",
    "<article class='conc-card'><span>5</span><h3>\u7528\u7b79\u8d44 archetype \u5206\u7c7b\u5bf9\u75c7\u65bd\u7b56</h3><p>F7 \u7684 4 \u7c7b\u56fd\u5bb6\u7b79\u8d44 archetype\uff08\u653f\u5e9c\u4e3b\u5bfc/\u79c1\u4eba\u4fdd\u9669/\u81ea\u4ed8\u9a71\u52a8/\u5916\u63f4\u4f9d\u8d56\uff09\u5404\u9700\u4e0d\u540c\u7684\u653f\u7b56\u8def\u5f84\uff0c\u4e0d\u5e94\u5c06\u5355\u4e00\u5236\u5ea6\u5f3a\u52a0\u4e8e\u6240\u6709\u56fd\u5bb6\u3002\u6bcf\u7c7b\u56fd\u5bb6\u5e94\u6709\u5b9a\u5236\u5316\u7684\u8f6c\u578b\u8def\u5f84\u3002</p></article>",
    "<article class='conc-card'><span>6</span><h3>\u9884\u6d4b\u5e94\u4f5c\u4e3a\u653f\u7b56\u5bf9\u8bdd\u7684\u8d77\u70b9</h3><p>F8 \u7684 ARIMA \u9884\u6d4b\u4ec5\u5916\u63a8\u8d8b\u52bf\uff0c\u4e0d\u80fd\u66ff\u4ee3\u7ed3\u6784\u6027\u8bc4\u4f30\u3002\u5efa\u8bae\u7ed3\u5408 Shiny \u4eea\u8868\u76d8\u60c5\u666f\u6a21\u62df\u5668\uff0c\u5728\u4e0d\u540c\u8d22\u653f\u8def\u5f84\u4e0b\u63a8\u6f14 5 \u5e74\u671f\u5360\u6bd4\u8f68\u8ff9\u3002</p></article>",
    "</div>",
    "<div class='conclusion-grid' style='margin-top:18px'>",
    "<article class='conc-card'><span>7</span><h3>\u8001\u9f84\u5316\u8d44\u91d1\u538b\u529b\u5c06\u6301\u7eed\u4e0a\u5347</h3><p>F15 \u663e\u793a 65+ \u5360\u6bd4\u6bcf\u63d0\u5347 1pp\uff0c\u4eba\u5747 CHE \u5e73\u5747\u4e0a\u5347 2.3%%%%\u3002\u6b27\u6d32\u548c\u4e1c\u4e9a\u5c06\u5728 2030 \u5e74\u524d\u9762\u4e34\u6700\u5927\u538b\u529b\u3002\u5e94\u63d0\u524d\u89c4\u5212\u957f\u671f\u62a4\u7406\u4e13\u9879\u8d44\u91d1\u3002</p></article>",
    "<article class='conc-card'><span>8</span><h3>\u7075\u6d3b\u56fd\u5bb6\u4e0e\u5c0f\u5c9b\u56fd\u9700\u7279\u522b\u5173\u6ce8</h3><p>F29/F35 \u663e\u793a\u6218\u4e71\u56fd\u3001\u96be\u6c11\u56fd\u548c\u5c0f\u5c9b\u56fd\u7684\u536b\u751f\u7cfb\u7edf\u6781\u5ea6\u8106\u5f31\uff0c\u5916\u63f4\u4e2d\u65ad\u540e\u5e73\u5747\u6062\u590d\u671f\u8d85 8 \u5e74\u3002\u5efa\u8bae\u5efa\u7acb\u533a\u57df\u536b\u751f\u8d44\u91d1\u6c60\u63d0\u4f9b\u53cd\u5468\u671f\u7f13\u51b2\u3002</p></article>",
    "<article class='conc-card'><span>9</span><h3>\u6570\u636e\u5b8c\u6574\u6027\u5f71\u54cd\u653f\u7b56\u8bc4\u4f30</h3><p>F33/F34 \u663e\u793a\u4f4e\u6536\u5165\u56fd\u5bb6\u7f3a\u5931\u7387\u8fbe 22%%%%\uff0c\u5386\u5e74\u6570\u636e\u4fee\u8ba2 3\u20138%%%%\u3002\u653f\u7b56\u5efa\u8bae\u5e94\u7ed3\u5408\u7f6e\u4fe1\u533a\u95f4\u800c\u975e\u70b9\u4f30\u8ba1\u3002</p></article>",
    "</div>",
    "<div class='method-block' style='margin-top:28px'><p><b>\u5168\u7403\u536b\u751f\u7b79\u8d44\u7684\u6838\u5fc3\u77db\u76fe\uff1a</b>\u672c\u7814\u7a76\u63ed\u793a\u7684\u6700\u6839\u672c\u77db\u76fe\u662f\u2014\u2014\u5168\u7403\u536b\u751f\u652f\u51fa\u5728\u7edd\u5bf9\u91cf\u4e0a\u6301\u7eed\u589e\u957f\uff0c\u4f46\u5206\u914d\u7ed3\u6784\u4e25\u91cd\u5931\u8861\u3002HIC \u4eba\u5747 CHE \u662f LIC \u768440 \u500d\uff0c\u4f46\u5bff\u547d\u5dee\u8ddd\u4ec5 12 \u5c81\uff0c\u8868\u660e\u8fb9\u9645\u6536\u76ca\u5728\u9ad8\u6536\u5165\u7aef\u6781\u5ea6\u9012\u51cf\u3002\u5168\u7403\u536b\u751f\u6cbb\u7406\u7684\u6838\u5fc3\u6311\u6218\u4e0d\u662f\u603b\u91cf\u4e0d\u8db3\uff0c\u800c\u662f\u5982\u4f55\u5c06\u6709\u9650\u8d44\u6e90\u5f15\u5bfc\u5230\u8fb9\u9645\u6536\u76ca\u6700\u9ad8\u7684\u4f4e\u6536\u5165\u56fd\u5bb6\u3002</p><p><b>\u5236\u5ea6\u8bbe\u8ba1\u6bd4\u8d44\u91d1\u603b\u91cf\u66f4\u91cd\u8981\uff1a</b>DEA \u5206\u6790\uff08F31\uff09\u663e\u793a\u6548\u7387\u524d\u6cbf\u56fd\u5bb6\uff08\u53e4\u5df4\u3001\u54e5\u65af\u8fbe\u9ece\u52a0\u3001\u65e5\u672c\uff09\u4ee5\u8fdc\u4f4e\u4e8e\u7f8e\u56fd\u7684\u4eba\u5747 CHE \u5b9e\u73b0\u4e86\u66f4\u9ad8\u5bff\u547d\u3002\u7f8e\u56fd\u5728\u540c\u7b49 CHE \u6c34\u5e73\u4e0a\u5bff\u547d\u504f\u4f4e 5 \u5c81\uff0c\u53cd\u6620\u5176\u4f53\u7cfb\u7684\u7ed3\u6784\u6027\u4f4e\u6548\u3002\u8fd9\u610f\u5473\u7740\u5bf9\u4e8e\u5df2\u7ecf\u6295\u5165\u8f83\u591a\u7684\u56fd\u5bb6\uff0c\u6548\u7387\u6539\u8fdb\u7684\u7a7a\u95f4\u8fdc\u5927\u4e8e\u7b80\u5355\u589e\u52a0\u8d44\u91d1\u3002</p><p><b>UHC \u662f\u964d\u4f4e\u81ea\u4ed8\u7684\u6700\u6709\u6548\u7b56\u7565\uff1a</b>F22 \u663e\u793a UHC \u670d\u52a1\u8986\u76d6\u6307\u6570\u6bcf\u63d0\u9ad8 10 \u70b9\uff0cOOP \u5e73\u5747\u4e0b\u964d 4.2pp\uff0c\u5728 LMIC \u4e2d\u6548\u5e94\u66f4\u5f3a\uff08-5.8pp\uff09\u3002\u6269\u5927 UHC \u8986\u76d6\u7684\u5178\u578b\u8def\u5f84\u5305\u62ec\u793e\u4f1a\u533b\u4fdd\u6269\u9762\u3001\u8d22\u653f\u76f4\u63a5\u62e8\u6b3e\u3001\u793e\u533a\u536b\u751f\u670d\u52a1\u6269\u5c55\uff0c\u5404\u8def\u5f84\u5bf9 OOP \u7684\u538b\u4f4e\u6548\u5e94\u65b9\u5411\u4e00\u81f4\u3002</p><p><b>\u9884\u9632\u6027\u62a4\u7406\u7684\u56de\u62a5\u7387\u6781\u9ad8\uff1a</b>F21/F25 \u663e\u793a NCD \u5360\u6b7b\u4ea1\u8d1f\u62c5 74%%%% \u4f46\u9884\u9632\u4ec5\u5360 CHE \u7684 3.2%%%%\u3002HC6 \u6bcf\u589e 1pp\uff0cHALE \u63d0\u9ad8 0.4 \u5c81\u3002\u7814\u7a76\u8868\u660e\u6bcf $1 \u9884\u9632\u6295\u5165\u7ea6\u4ea7\u751f $7 \u7684\u6cbb\u7597\u8282\u7ea6\uff0c\u5efa\u8bae\u8bbe\u5b9a\u6700\u4f4e\u5360\u6bd4\u76ee\u6807 8%%%%\u3002</p><p><b>\u5916\u63f4\u5e94\u4f18\u5316\u5206\u914d\u800c\u975e\u7b80\u5355\u589e\u91cf\uff1a</b>F32 \u663e\u793a EXT \u5bf9 U5MR \u7684\u8fb9\u9645\u6548\u5e94\u968f\u89c4\u6a21\u9012\u51cf\uff0c\u5f53 EXT>30%%%% \u65f6\u5f39\u6027\u4ec5 -4%%%%\u3002PEPFAR\u3001Gavi \u7b49\u5782\u76f4\u9879\u76ee\u6548\u7387\u663e\u8457\u9ad8\u4e8e\u4e00\u822c ODA\u3002\u5efa\u8bae\u5c06\u63f4\u52a9\u4f18\u5148\u5206\u914d\u5230\u7279\u5b9a\u75c5\u79cd\u4e0e\u5236\u5ea6\u5efa\u8bbe\uff0c\u800c\u975e\u7b14\u5c3d\u7684\u7efc\u5408\u9884\u7b97\u652f\u6301\u3002</p></div>",
    "<div class='limit' style='margin-top:28px'><h3>\u7814\u7a76\u5c40\u9650</h3><ul>",
    "<li>OOPS \u4e0e\u5916\u63f4\u7684\u53e3\u5f84\u5728\u4e0d\u540c\u56fd\u5bb6\u5b58\u5728\u7edf\u8ba1\u5dee\u5f02\uff0c\u8de8\u56fd\u6bd4\u8f83\u9700\u8c28\u614e\u3002</li>",
    "<li>2023 \u5e74\u90e8\u5206\u56fd\u5bb6\u6570\u636e\u4e3a\u6a21\u578b\u4f30\u8ba1\uff0c\u7ed3\u8bba\u9700\u4ee5\u65b0\u7248 GHED \u516c\u5e03\u4e3a\u51c6\u3002</li>",
    "<li>\u9762\u677f FE \u4e0d\u80fd\u8bc6\u522b\u56e0\u679c\uff0c\u4ec5\u7ed9\u51fa\u6761\u4ef6\u76f8\u5173\uff1b\u653f\u7b56\u5efa\u8bae\u57fa\u4e8e\u591a\u6a21\u578b\u4e00\u81f4\u6027\u800c\u975e\u5355\u4e00\u56de\u5f52\u3002</li>",
    "<li>\u5065\u5eb7\u4ea7\u51fa\u53d7\u6559\u80b2\u3001\u73af\u5883\u3001\u516c\u5171\u536b\u751f\u80fd\u529b\u548c\u4eba\u53e3\u7ed3\u6784\u591a\u91cd\u56e0\u7d20\u5f71\u54cd\uff0c\u4e0d\u80fd\u5355\u72ec\u5f52\u56e0\u4e8e\u8d44\u91d1\u6295\u5165\u3002</li>",
    "<li>\u672c\u5206\u6790\u4ec5\u7528\u4e8e\u8bfe\u7a0b\u9879\u76ee\u4e0e\u6570\u636e\u65b0\u95fb\u5c55\u793a\uff0c\u4e0d\u6784\u6210\u6b63\u5f0f\u7684\u653f\u7b56\u54a8\u8be2\u3002</li>",
    "</ul></div>",
    "</div></section>"
  ), s$oops_high_cur, s$cur_year)
}


.ghs_footer <- function() {
  paste0(
    "<footer class='site-footer'><div class='wrap'>",
    "<div class='foot-grid'>",
    "<div><strong>Global Health Spending \u00b7 \u5e84\u9882 20241334</strong>",
    "<p>Shiny \u4eea\u8868\u76d8\uff1a<a href='https://constantine1433223.shinyapps.io/ghs-dashboard/' target='_blank' rel='noreferrer'>constantine1433223.shinyapps.io/ghs-dashboard</a></p>",
    "<p>\u9759\u6001\u9996\u9875\uff1a<a href='https://2711944586.github.io/R/' target='_blank' rel='noreferrer'>2711944586.github.io/R</a></p></div>",
    "<div><strong>\u4ed3\u5e93</strong><p><a href='https://github.com/2711944586/R'>github.com/2711944586/R</a></p></div>",
    "<div><strong>\u5fae\u540e\u7aef</strong><p>R 4.5 \u00b7 ggplot2 \u00b7 plotly \u00b7 leaflet \u00b7 fixest \u00b7 forecast \u00b7 Shiny</p></div>",
    "</div></div></footer>"
  )
}


.ghs_css <- function() {
  paste(c(
    ":root{--ink:#1a1a1a;--muted:#78716c;--paper:#f0ebe1;--paper-2:#e8e2d8;--line:rgba(0,0,0,.06);--line-strong:rgba(0,0,0,.1);--blue:#292524;--orange:#44403c;--green:#059669;--code-bg:#1c1917;--code-ink:#fafaf9}",
    "*,*:before,*:after{box-sizing:border-box}html{scroll-behavior:smooth}body{margin:0;background:var(--paper);color:var(--ink);font-family:'Inter','Segoe UI','Helvetica Neue','Microsoft YaHei','Noto Sans CJK SC',system-ui,sans-serif;line-height:1.7;-webkit-font-smoothing:antialiased}",
    ".wrap{width:min(1200px,92vw);margin:auto}",
    "h1,h2,h3{font-family:'Source Serif 4',Georgia,'Noto Serif SC',serif;letter-spacing:-.01em}",
    "code,pre{font-family:'JetBrains Mono','Fira Code',Consolas,monospace}",
    "a{color:var(--blue)}",
    ".hero{position:relative;min-height:110vh;padding:0;display:flex;align-items:center;justify-content:center;background:#f0ebe1;overflow:hidden}",
    ".hero:before{content:'';position:absolute;inset:0;background:none;pointer-events:none}",
    ".hero:after{content:'';position:absolute;inset:0;background:none;pointer-events:none}",
    "@keyframes hero-grid-drift{to{background-position:60px 60px}}",
    ".hero-center{position:relative;z-index:2}",
    ".hero-center{position:relative;z-index:2;text-align:left;max-width:1100px;width:min(1100px,88vw);padding:0}",
    ".hero-eyebrow{display:inline-block;padding:10px 24px;border-radius:999px;background:rgba(255,255,255,.55);backdrop-filter:blur(16px) saturate(150%);-webkit-backdrop-filter:blur(16px) saturate(150%);border:1px solid rgba(255,255,255,.7);font-size:11px;letter-spacing:.13em;color:rgba(0,0,0,.4);font-weight:600;margin-bottom:48px;box-shadow:0 4px 16px rgba(0,0,0,.04),inset 0 1px 0 rgba(255,255,255,.8)}",
    ".hero h1{font-family:'Source Serif 4',Georgia,serif;font-size:clamp(44px,8.5vw,105px);font-weight:900;line-height:1.05;letter-spacing:-.035em;margin:0;color:#1a1a1a;text-shadow:0 1px 2px rgba(0,0,0,.04)}",
    ".hero .hero-line1{display:block}",
    ".hero .hero-line2{display:block}",
    ".hero .hero-line3{display:block;margin-top:.25em}",
    ".hero .hero-line3 em{font-style:normal;color:#9e6b3a}",
    ".hero-main{display:none}",
    ".hero-sub{display:none}",
    "@keyframes ghs-fade-in{from{opacity:0;filter:blur(4px)}to{opacity:1;filter:blur(0)}}",
    "@keyframes ghs-slide-up{from{opacity:0;transform:translateY(50px) scale(.97);filter:blur(2px)}to{opacity:1;transform:translateY(0) scale(1);filter:blur(0)}}",
    ".hero-reveal{opacity:0;transform:translateY(30px) scale(.97);filter:blur(2px);transition:opacity .8s cubic-bezier(.16,1,.3,1),transform .8s cubic-bezier(.16,1,.3,1),filter .8s ease}.hero-reveal.in{opacity:1;transform:translateY(0) scale(1);filter:blur(0)}",
    ".nav{display:flex;justify-content:space-between;align-items:center;padding:10px 0 18px;position:relative}",
    ".brand{font-weight:800;letter-spacing:.16em;font-size:13px;color:#1a1a1a;text-decoration:none}",
    ".links{display:flex;gap:6px;align-items:center;flex-wrap:wrap}.links .nav-group{display:flex;align-items:center;gap:2px;padding:0 8px;position:relative}.links .nav-group-label{font-size:11px;font-weight:800;letter-spacing:.1em;text-transform:uppercase;color:rgba(0,0,0,.3);padding:4px 6px;pointer-events:none;white-space:nowrap}.links .nav-group:after{content:'';width:1px;height:16px;background:rgba(0,0,0,.1);margin-left:6px}.links .nav-group:last-child:after{display:none}.links a{color:rgba(0,0,0,.55);font-size:12.5px;text-decoration:none;letter-spacing:.02em;padding:5px 8px;border-radius:6px;transition:color .15s,background .15s;font-weight:500}.links a:hover{color:#1a1a1a;background:rgba(0,0,0,.04)}.links a.active{color:#1a1a1a;background:rgba(0,0,0,.06);font-weight:700}",
    ".mobile-toc{display:none;position:relative;z-index:4;width:min(1200px,92vw);margin:8px auto 0;border:1px solid rgba(0,0,0,.08);border-radius:16px;background:rgba(255,255,255,.9);backdrop-filter:blur(16px);color:#1a1a1a;overflow:hidden}",
    ".mobile-toc summary{cursor:pointer;padding:12px 16px;font-weight:900;letter-spacing:.08em;list-style:none;color:#1a1a1a}.mobile-toc summary::-webkit-details-marker{display:none}",
    ".mobile-toc-body{padding:0 14px 16px}.mobile-toc-group{margin-bottom:12px}.mobile-toc-group-label{display:block;font-size:10px;font-weight:800;letter-spacing:.14em;text-transform:uppercase;color:rgba(0,0,0,.35);padding:4px 0 6px}.mobile-toc-items{display:flex;flex-wrap:wrap;gap:6px}.mobile-toc-items a{color:rgba(0,0,0,.6);text-decoration:none;border:1px solid rgba(0,0,0,.1);border-radius:999px;padding:6px 12px;font-size:12px;transition:all .15s}.mobile-toc-items a:hover,.mobile-toc-items a.active{color:#1a1a1a;background:rgba(0,0,0,.04);border-color:rgba(0,0,0,.2)}",
    ".hero-grid{display:none}",
    ".eyebrow{display:none}",
    ".hero-lead{display:none}",
    ".hero-actions{display:none}",
    ".btn{display:inline-flex;align-items:center;gap:8px;padding:14px 28px;border-radius:12px;background:#292524;color:#fafaf9;font-weight:700;text-decoration:none;border:0;font-size:14px;letter-spacing:.01em;box-shadow:0 2px 8px rgba(0,0,0,.1);transition:transform .2s,box-shadow .2s}",
    ".btn:hover{transform:translateY(-1px);box-shadow:0 6px 20px rgba(0,0,0,.14)}",
    ".btn.alt{background:rgba(0,0,0,.04);color:#292524;border:1px solid rgba(0,0,0,.1);box-shadow:none}",
    ".btn.alt:hover{background:rgba(0,0,0,.07);border-color:rgba(0,0,0,.15)}",
    ".btn.ghost{background:transparent;color:#78716c;border:1px solid rgba(0,0,0,.08);box-shadow:none}",
    ".btn.ghost:hover{color:#292524;border-color:rgba(0,0,0,.18)}",
    ".hero-panel{display:none}",
    ".hero-panel-tag,.hero-panel-list{display:none}",
    ".hero,.section,.finding,.site-footer{overflow-x:hidden}",
    ".wrap,.hero-grid,.kpi-grid,.finding-head,.finding-body,.gallery-grid,.widget-lab,.conclusion-grid,.cmd-grid,.deep-grid,.figure-index-list,.asset-console-grid,.asset-list{min-width:0}",
    "img,svg,iframe,video,canvas{max-width:100%}",
    "pre,code,.table-wrap,.figure-index,.asset-console,.widget-card,.gallery-card,.hero-panel,.method-block,.callout{min-width:0;overflow-wrap:anywhere}",
    ".hero-panel-list li span,.hero-lead,.section .lead,.finding-head .lead{min-width:0;overflow-wrap:anywhere}",
    ".section{padding:96px 0;border-top:none;background:var(--paper)}",
    ".section:first-of-type{border-top:none}",
    ".kpi-section{background:var(--paper)}",
    ".section-head{margin-bottom:30px;max-width:960px}",
    ".section-head .kicker{display:inline-block;font-size:12px;letter-spacing:.22em;text-transform:uppercase;color:#78716c;margin-bottom:10px;font-weight:800}",
    ".section h2{font-size:clamp(32px,4vw,50px);line-height:1.08;margin:0 0 16px}",
    ".section .lead{font-size:18px;color:var(--muted);max-width:900px}",
    ".kpi-grid{display:grid;grid-template-columns:repeat(4,1fr);gap:18px}",
    ".kpi{position:relative;padding:24px 22px;border:1px solid var(--line);border-radius:16px;background:var(--paper);box-shadow:0 1px 3px rgba(0,0,0,.03);overflow:hidden;transition:transform .3s cubic-bezier(.4,0,.2,1),box-shadow .3s}",
    ".kpi:before{content:none}",
    ".kpi:hover{transform:translateY(-3px);box-shadow:0 8px 24px rgba(0,0,0,.06)}",
    ".kpi-value{font-family:'Source Serif 4',serif;font-size:38px;font-weight:700;color:#292524;line-height:1.1}",
    ".kpi-label{margin-top:10px;font-weight:700;font-size:13px;letter-spacing:.04em;color:#78716c}",
    ".kpi-note{margin-top:6px;font-size:12.5px;color:#a8a29e;line-height:1.55}",
    ".methods .code-figure{margin:14px 0 22px}",
    ".finding{padding:104px 0;border-top:none;background:var(--paper)}",
    ".finding:nth-of-type(odd),.finding:nth-of-type(even){background:var(--paper)}",
    ".finding-head{display:grid;grid-template-columns:minmax(150px,max-content) minmax(0,1fr);gap:32px;align-items:start;margin-bottom:36px}",
    ".finding-head>div{min-width:0}",
    ".finding-num{display:block;font-family:'Source Serif 4',serif;font-size:56px;line-height:.95;color:var(--g3-secondary,var(--orange));font-weight:800;white-space:nowrap;margin-top:0}",
    ".finding-kicker{display:inline-block;font-size:12px;letter-spacing:.22em;text-transform:uppercase;color:#374151;margin-bottom:6px;font-weight:800}",
    ".finding-head h2{font-size:clamp(32px,4.4vw,52px);line-height:1.05;margin:0 0 12px}",
    ".finding-head .lead{font-size:18px;color:var(--muted);max-width:760px}",
    ".chips{display:flex;flex-wrap:wrap;gap:10px;margin-top:14px}",
    ".chip{display:inline-flex;align-items:baseline;gap:8px;padding:7px 14px;border-radius:999px;border:1px solid var(--line-strong);background:#fff;font-size:13px}",
    ".chip b{color:var(--muted);font-weight:600;font-size:11px;text-transform:uppercase;letter-spacing:.1em}",
    ".chip i{font-style:normal;font-weight:800;color:var(--ink)}",
    ".chip-blue i{color:var(--g3-primary,var(--blue))}.chip-orange i{color:var(--g3-secondary,var(--orange))}.chip-ink i{color:var(--ink)}.chip-neutral i{color:var(--muted)}",
    ".finding-body{display:grid;gap:22px}",
    ".method-block{background:rgba(255,255,255,.5);border:1px solid var(--line);border-radius:18px;padding:20px 24px;font-size:15px;color:var(--ink);line-height:1.72;letter-spacing:-.01em}",
    ".method-block p{margin:10px 0;text-indent:2em}",
    ".method-block p:first-child{margin-top:0}",
    ".method-block p:last-child{margin-bottom:0}",
    ".method-block ul{margin:6px 0 6px 18px}",
    ".code-figure{margin:0;background:var(--code-bg);border-radius:18px;overflow:hidden;border:1px solid rgba(13,18,27,.18);box-shadow:0 18px 36px rgba(13,18,27,.18)}",
    ".code-caption{padding:10px 18px;color:#cdd9ee;background:rgba(255,255,255,.04);border-bottom:1px solid rgba(255,255,255,.08);font-size:12.5px;letter-spacing:.05em;text-transform:uppercase;font-weight:700}",
    ".code-pre{margin:0;padding:18px 20px;color:var(--code-ink);font-size:13px;line-height:1.6;overflow:auto;max-height:420px}",
    ".code-pre code{color:inherit;background:none}",
    ".fig-inline{margin:0;background:#fff;border:1px solid var(--line);border-radius:22px;overflow:hidden;box-shadow:0 18px 50px rgba(13,18,27,.08)}",
    ".fig-frame{width:100%;background:linear-gradient(180deg,#fffaf2,#f3e8d6);padding:18px 18px 0;display:flex;justify-content:center}",
    ".fig-frame img{max-width:100%;height:auto;display:block}",
    ".fig-inline figcaption{padding:14px 22px 18px;display:flex;flex-direction:column;gap:6px}",
    ".fig-kicker{font-size:12px;letter-spacing:.18em;text-transform:uppercase;color:var(--orange);font-weight:800}",
    ".fig-inline figcaption strong{font-family:'Source Serif 4',serif;font-size:18px;color:var(--ink);font-weight:600}",
    ".callout{margin:0;padding:22px 26px;border-radius:20px;border:1px solid var(--line);display:grid;gap:8px}",
    ".callout-blue{background:linear-gradient(135deg,#eaf1f8,#fbf6ee);border-color:rgba(29,63,95,.22)}",
    ".callout-orange{background:linear-gradient(135deg,#fbeede,#fbf6ee);border-color:rgba(196,99,39,.22)}",
    ".callout-ink{background:#fff;border-color:var(--line-strong)}",
    ".callout strong{font-size:14px;letter-spacing:.18em;text-transform:uppercase;color:var(--orange);font-weight:800}",
    ".callout p{margin:6px 0}",
    ".evidence-note{margin:0 0 28px;padding:22px 26px;border-radius:20px;border:1px solid rgba(29,63,95,.18);background:linear-gradient(135deg,#fffaf2,#eef4f8);box-shadow:0 12px 34px rgba(13,18,27,.05)}",
    ".evidence-note strong{display:block;font-size:12px;letter-spacing:.18em;text-transform:uppercase;color:var(--g3-primary,var(--blue));font-weight:900;margin-bottom:8px}",
    ".evidence-note p{margin:8px 0 0;font-size:15.5px;line-height:1.78;color:#3f3a35;max-width:920px}",
    ".section-note{margin:0 0 28px;padding:22px 26px;border-radius:18px;border:1px solid rgba(29,63,95,.16);background:#fbf6ee;box-shadow:0 10px 28px rgba(13,18,27,.045)}",
    ".section-note strong{display:block;font-size:12px;letter-spacing:.18em;text-transform:uppercase;color:var(--g3-primary,var(--blue));font-weight:900;margin-bottom:8px}",
    ".section-note p{margin:8px 0 0;font-size:15.5px;line-height:1.78;color:#3f3a35;max-width:980px}",
    ".table-wrap{overflow:auto;border-radius:18px;border:1px solid var(--line);background:#fff;box-shadow:0 16px 36px rgba(13,18,27,.06)}",
    ".table-wrap table{width:100%;border-collapse:collapse;font-size:14px}",
    ".table-wrap caption{caption-side:top;padding:14px 18px;text-align:left;font-weight:800;color:var(--blue);background:linear-gradient(90deg,#fbf6ee,#fff);border-bottom:1px solid var(--line)}",
    ".table-wrap th,.table-wrap td{padding:10px 14px;border-bottom:1px solid var(--line)}",
    ".table-wrap th{background:#fbf6ee;text-align:left;font-weight:800;color:var(--ink);letter-spacing:.04em;font-size:12.5px;text-transform:uppercase}",
    ".table-wrap td{color:var(--ink)}",
    ".two-col{display:grid;grid-template-columns:1fr 1fr;gap:18px}",
    ".tabs{display:flex;flex-wrap:wrap;gap:10px;margin:16px 0 22px}",
    ".tabs button{border:1px solid var(--line-strong);background:#fff;border-radius:999px;padding:9px 16px;font-weight:800;color:var(--ink);cursor:pointer;font-size:13px}",
    ".tabs button.active{background:var(--ink);color:#fff;border-color:var(--ink)}",
    ".figure-index{margin:0 0 24px;background:#fff;border:1px solid var(--line);border-radius:24px;padding:22px;box-shadow:0 16px 40px rgba(13,18,27,.06)}",
    ".figure-index header{display:grid;gap:6px;margin-bottom:16px}.figure-index h3{margin:0;font-size:24px;color:var(--ink)}.figure-index p{margin:0;color:var(--muted);font-size:14px;line-height:1.65}",
    ".figure-index-tools{display:flex;gap:12px;align-items:center;margin-bottom:14px}.figure-index-tools input{flex:1;border:1px solid var(--line-strong);border-radius:999px;background:#fbf6ee;padding:11px 16px;color:var(--ink);font:inherit}.figure-index-tools span{color:var(--muted);font-size:13px;font-weight:800;white-space:nowrap}",
    ".figure-index-list{display:grid;grid-template-columns:repeat(2,1fr);gap:8px;max-height:360px;overflow:auto;padding-right:4px}.figure-index-row{display:grid;grid-template-columns:42px 1fr auto auto;align-items:center;gap:10px;padding:10px 12px;border:1px solid var(--line);border-radius:14px;background:#fbf6ee;text-decoration:none;color:var(--ink)}.figure-index-row span{font-family:'Source Serif 4',serif;color:var(--orange);font-weight:800}.figure-index-row strong{font-size:13.5px}.figure-index-row em{font-style:normal;font-size:12px;color:var(--blue);font-weight:800}.figure-index-row small{color:var(--muted)}",
    ".asset-console{margin:0 0 24px;background:#fff;border:1px solid var(--line);border-radius:24px;padding:22px;box-shadow:0 16px 40px rgba(13,18,27,.06);opacity:1;color:var(--ink)}",
    ".asset-console header{display:grid;gap:6px;margin-bottom:16px}.asset-console h3{margin:0;font-size:26px;color:var(--ink)}.asset-console h4{margin:0;font-size:18px;color:var(--ink)}.asset-console p{margin:0;color:var(--muted);font-size:14px;line-height:1.7}",
    ".asset-console-stats{display:grid;grid-template-columns:repeat(4,1fr);gap:10px;margin:14px 0 16px}.asset-console-stats a{display:grid;gap:3px;padding:14px;border:1px solid var(--line);border-radius:16px;background:#fbf6ee;text-decoration:none;color:var(--ink)}.asset-console-stats strong{font-family:'Source Serif 4',serif;font-size:30px;line-height:1;color:var(--orange)}.asset-console-stats span{font-size:12px;font-weight:800;color:var(--muted);letter-spacing:.08em;text-transform:uppercase}",
    ".asset-console-tools{display:grid;gap:10px;margin-bottom:16px}.asset-console-tools input{border:1px solid var(--line-strong);border-radius:999px;background:#fbf6ee;padding:11px 16px;color:var(--ink);font:inherit}.asset-console-chips{display:flex;flex-wrap:wrap;gap:8px}.asset-console-chips button{border:1px solid var(--line);background:#fffaf2;color:var(--ink);border-radius:999px;padding:8px 12px;font-weight:800;cursor:pointer}.asset-console-chips button.active,.asset-console-chips button:hover{background:var(--ink);border-color:var(--ink);color:#fff}",
    ".asset-console-grid{display:grid;grid-template-columns:1fr 1fr;gap:16px}.asset-console-grid article{border:1px solid var(--line);border-radius:20px;background:#fffaf2;padding:16px}.asset-console-grid article header{margin-bottom:12px}",
    ".asset-list{display:grid;gap:8px}.asset-row{display:grid;grid-template-columns:54px 1fr auto auto;align-items:center;gap:10px;width:100%;box-sizing:border-box;padding:10px 12px;border:1px solid var(--line);border-radius:14px;background:#fff;text-align:left;text-decoration:none;color:var(--ink);font:inherit}.asset-row:hover{border-color:var(--orange);box-shadow:0 10px 28px rgba(13,18,27,.08)}.asset-row span{font-family:'Source Serif 4',serif;color:var(--orange);font-weight:900}.asset-row strong{font-size:13.5px}.asset-row em{font-style:normal;font-size:12px;color:var(--blue);font-weight:900}.asset-row small{color:var(--muted);font-size:12px}.asset-empty{padding:16px;border:1px dashed var(--line-strong);border-radius:16px;background:#fff}",
    ".gallery-grid{display:grid;grid-template-columns:repeat(3,1fr);gap:18px}",
    ".gallery,.figure-index,.asset-console,.gallery-card,.gallery-button{opacity:1;filter:none}",
    ".gallery-card{background:#fff;border:1px solid var(--line);border-radius:22px;overflow:hidden;box-shadow:0 16px 40px rgba(13,18,27,.06);transition:transform .25s ease,box-shadow .25s ease}",
    ".gallery-card:hover{transform:translateY(-4px);box-shadow:0 22px 60px rgba(13,18,27,.12)}",
    ".gallery-button{border:0;background:#fffaf2;width:100%;padding:0;cursor:zoom-in;display:flex;align-items:center;justify-content:center;min-height:200px}",
    ".gallery-card img{width:100%;height:auto;max-height:340px;object-fit:contain;display:block}",
    ".gallery-meta{padding:14px 16px 18px;display:flex;flex-direction:column;gap:6px}",
    ".gallery-meta span,.pill{display:inline-flex;background:#f0e3d0;color:#774314;border-radius:999px;padding:4px 11px;font-size:12px;font-weight:900;width:fit-content;letter-spacing:.04em}",
    ".gallery-meta strong{font-family:'Source Serif 4',serif;font-size:16px;color:var(--ink)}",
    ".gallery-note{margin:2px 0;color:var(--muted);font-size:12.8px;line-height:1.65}",
    ".gallery-meta em{color:var(--muted);font-style:normal;font-size:12px}",
    ".widget-lab{display:grid;grid-template-columns:.86fr 1.14fr;gap:22px;align-items:start}",
    ".widget-list{display:grid;gap:12px;max-height:760px;overflow:auto;padding-right:6px}",
    ".widget-card{background:#fff;border:1px solid var(--line);border-radius:18px;padding:16px 18px;display:flex;justify-content:space-between;gap:14px;align-items:center;box-shadow:0 10px 28px rgba(13,18,27,.05)}",
    ".widget-card header{display:flex;flex-direction:column;gap:4px}",
    ".widget-card h3{margin:6px 0 0;font-size:16px;font-family:'Source Serif 4',serif;color:var(--ink);font-weight:600}",
    ".widget-card p{margin:0;color:var(--muted);font-size:12px}",
    ".widget-actions{display:flex;flex-direction:column;gap:6px;align-items:flex-end}",
    ".widget-card button{border:0;border-radius:999px;background:var(--blue);color:#fff;padding:10px 14px;font-weight:900;cursor:pointer;white-space:nowrap;font-size:13px}",
    ".widget-card a{font-size:12px;color:var(--muted);text-decoration:none;border-bottom:1px dashed var(--muted)}",
    ".widget-frame-wrap{position:sticky;top:18px;background:#0c1424;border-radius:24px;padding:14px;box-shadow:0 26px 70px rgba(13,18,27,.18)}",
    ".widget-frame-head{color:#cdd9ee;display:flex;justify-content:space-between;align-items:center;padding:6px 8px 12px;font-size:13px}",
    ".widget-frame-head strong{color:#fff;font-family:'Source Serif 4',serif;font-weight:600}",
    ".widget-frame-wrap iframe{width:100%;height:680px;border:0;border-radius:18px;background:#fff}",
    ".widget-fallback-banner{margin:0 0 10px;padding:10px 14px;border-radius:12px;background:#fdf3df;color:#774314;font-size:12.5px;line-height:1.55;border:1px solid #e7c98a}",
    ".widget-fallback-banner.loaded{background:#e7f4ec;color:#1f6f43;border-color:#a8d8b9}",
    ".widget-fallback-banner.error{background:#fde7e0;color:#a03b27;border-color:#e9b1a1}",
    ".widget-fallback-banner a{color:inherit;font-weight:700;text-decoration:underline}",
    ".deep-dive{margin:22px 0 6px;padding:22px 24px;background:#fbf6ee;border:1px solid #e3d6bb;border-radius:18px}",
    ".deep-dive h3{font-family:'Source Serif 4',serif;font-size:20px;margin:0 0 14px;color:var(--ink);letter-spacing:.01em}",
    ".deep-grid,.deep-dive-grid{display:grid;grid-template-columns:repeat(3,minmax(0,1fr));gap:16px}",
    ".deep-dive-grid{margin:22px 0 6px;padding:22px 24px;background:#fbf6ee;border:1px solid #e3d6bb;border-radius:18px}",
    ".deep-card{background:#fff;border:1px solid var(--line);border-radius:14px;padding:16px 18px;box-shadow:0 8px 22px rgba(13,18,27,.04)}",
    ".deep-card h4{font-family:'Source Serif 4',serif;font-size:15px;margin:0 0 8px;color:var(--blue);font-weight:700}",
    ".deep-card.counter h4{color:#b04a22}",
    ".deep-card.method h4{color:#5a6b48}",
    ".deep-card p{margin:0;color:#3a3a42;font-size:13.5px;line-height:1.72}",
    ".deep-card p strong{color:var(--ink);font-weight:700}",
    "@media(max-width:1080px){.deep-grid,.deep-dive-grid{grid-template-columns:1fr}}",
    ".cmd-grid{display:grid;grid-template-columns:repeat(2,1fr);gap:18px}",
    ".cmd-card{background:#fff;border:1px solid var(--line);border-radius:18px;overflow:hidden;box-shadow:0 12px 28px rgba(13,18,27,.06)}",
    ".cmd-card header{padding:14px 18px;background:#fbf6ee;border-bottom:1px solid var(--line);font-weight:800;color:var(--blue)}",
    ".cmd-card .code-pre{max-height:180px;background:var(--code-bg);color:var(--code-ink)}",
    ".cmd-note{padding:12px 18px 16px;color:var(--muted);font-size:13.5px;margin:0}",
    ".conclusion{background:var(--paper)}",
    ".conclusion-grid{display:grid;grid-template-columns:repeat(3,1fr);gap:18px;margin-bottom:36px}",
    ".conc-card{background:#fff;border:1px solid var(--line);border-radius:22px;padding:22px;box-shadow:0 14px 32px rgba(13,18,27,.06);display:grid;gap:8px}",
    ".conc-card span{font-family:'Source Serif 4',serif;font-size:34px;font-weight:800;color:var(--orange)}",
    ".conc-card h3{font-size:18px;margin:0 0 4px;color:var(--ink)}",
    ".conc-card p{margin:0;color:var(--muted);font-size:14.5px;line-height:1.7}",
    ".limit{background:#fff;border-radius:18px;padding:22px 26px;border:1px solid var(--line);display:grid;gap:10px}",
    ".limit h3{margin:0;font-size:18px;color:var(--blue)}",
    ".limit ul{margin:0;padding-left:20px}",
    ".limit li{color:var(--muted);font-size:14.5px;margin:5px 0}",
    ".site-footer{background:#0d121b;color:#bcc6d8;padding:64px 0;font-size:14px}",
    ".foot-grid{display:grid;grid-template-columns:1.4fr 1fr 1fr;gap:32px}",
    ".site-footer strong{color:#f7eedf;display:block;margin-bottom:6px;font-family:'Source Serif 4',serif}",
    ".site-footer a{color:#f7c08a;text-decoration:none}",
    ".modal{position:fixed;inset:0;background:rgba(0,0,0,.75);backdrop-filter:blur(8px);-webkit-backdrop-filter:blur(8px);display:none;z-index:50;padding:28px}",
    ".modal.open{display:grid;place-items:center}",
    ".modal img{max-width:96vw;max-height:86vh;background:#fff;border-radius:14px;box-shadow:0 30px 80px rgba(0,0,0,.5)}",
    ".modal button{position:absolute;right:24px;top:20px;border:0;border-radius:999px;padding:10px 16px;font-weight:900;background:#fff;cursor:pointer}",
    ".modal-title{position:absolute;left:28px;top:22px;color:#fff;font-weight:900;font-family:'Source Serif 4',serif;font-size:18px}",
    "@media(max-width:1080px){.hero-grid,.widget-lab{grid-template-columns:1fr}.kpi-grid{grid-template-columns:repeat(3,1fr)}.gallery-grid{grid-template-columns:repeat(2,1fr)}.conclusion-grid{grid-template-columns:repeat(2,1fr)}.cmd-grid{grid-template-columns:1fr}.two-col{grid-template-columns:1fr}.widget-frame-wrap{position:static}.foot-grid{grid-template-columns:1fr}.country-grid,.atlas-grid{grid-template-columns:1fr}.exec-big-grid{grid-template-columns:repeat(2,1fr)}.sim-grid{grid-template-columns:1fr}.figure-index-list,.asset-console-grid{grid-template-columns:1fr}}",
    "@media(max-width:760px){.links{display:none}.mobile-toc{display:block}.mobile-toc-links{grid-template-columns:repeat(2,1fr)}.figure-index-tools{align-items:stretch;flex-direction:column}.figure-index-row{grid-template-columns:34px 1fr auto}.figure-index-row small{display:none}.asset-console-stats{grid-template-columns:repeat(2,1fr)}.asset-row{grid-template-columns:46px 1fr auto}.asset-row small{display:none}.widget-lab{grid-template-columns:1fr;gap:14px}.widget-list{max-height:none;padding-right:0}.widget-frame-wrap{position:static}.widget-frame-wrap iframe{height:520px}.hero-actions{align-items:stretch;flex-direction:column}.hero-actions .btn{justify-content:center;width:100%;box-sizing:border-box}.eyebrow{border-radius:18px;line-height:1.5;white-space:normal}.hero-panel-list li{align-items:flex-start}.hero-panel-list li b{min-width:54px;font-size:30px}.code-pre{font-size:12px;padding:14px 16px}}",
    "@media(max-width:640px){.kpi-grid{grid-template-columns:repeat(2,1fr)}.gallery-grid,.conclusion-grid,.exec-big-grid{grid-template-columns:1fr}.hero{min-height:auto}.hero-grid{padding-top:42px}.finding-head{grid-template-columns:1fr}.finding-num{font-size:54px;margin-top:0}.hero h1{font-size:clamp(40px,13vw,58px)}.hero-lead{font-size:16.5px}.section{padding:68px 0}.widget-frame-wrap iframe{height:460px}.links{display:none}.mobile-toc{display:block}}",
    "@media(max-width:420px){.wrap{width:min(100% - 24px,1180px)}.kpi-grid{grid-template-columns:1fr}.mobile-toc-links{grid-template-columns:1fr}.figure-index-row{grid-template-columns:30px 1fr}.figure-index-row em{display:none}.asset-console-stats{grid-template-columns:1fr}.asset-row{grid-template-columns:42px 1fr}.asset-row em{display:none}.hero-panel{padding:18px}.widget-frame-wrap{padding:10px;border-radius:18px}.widget-frame-wrap iframe{height:420px;border-radius:12px}}",
    ".top-bar{position:fixed;left:0;right:0;top:0;height:2px;background:transparent;z-index:60;pointer-events:none}",
    "#read-progress{height:100%;width:0;background:rgba(0,0,0,.2);transition:width .12s linear}",
    ".widget-embed{background:var(--paper);border:1px solid var(--line);border-radius:16px;padding:0;overflow:hidden;box-shadow:0 2px 8px rgba(0,0,0,.03)}",
    ".widget-embed summary{cursor:pointer;padding:14px 18px;display:flex;justify-content:space-between;align-items:center;gap:12px;list-style:none;font-size:13.5px;color:var(--ink);font-weight:600}",
    ".widget-embed summary::-webkit-details-marker{display:none}",
    ".widget-embed summary::before{content:'\u25bc';display:inline-block;margin-right:6px;color:#78716c;transition:transform .2s ease;font-size:9px}",
    ".widget-embed[open] summary::before{transform:rotate(180deg)}",
    ".widget-embed strong{font-size:13px;color:#292524;font-weight:700}",
    ".widget-embed .widget-meta{font-size:11.5px;color:#a8a29e;font-weight:500}",
    ".widget-embed-frame{position:relative;background:#fff;height:0;overflow:hidden;transition:height .3s ease}",
    ".widget-embed[open] .widget-embed-frame{height:640px;border-top:1px solid var(--line)}",
    ".widget-embed iframe{width:100%;height:100%;border:0;background:#fff}",
    ".widget-embed-links{display:flex;gap:12px;padding:10px 18px 12px;font-size:12px;border-top:1px solid var(--line)}.widget-embed-links a{color:#78716c;text-decoration:none}.widget-embed-links a:hover{color:#292524}",
    ".widget-card{background:var(--paper);border:1px solid var(--line);border-radius:16px;overflow:hidden;box-shadow:0 2px 8px rgba(0,0,0,.03);margin-bottom:14px}",
    ".widget-card h4{padding:14px 18px 0;font-size:13px;color:var(--ink);font-weight:700;margin:0}",
    ".widget-card .widget-embed-frame{height:560px;background:#fff;border-top:1px solid var(--line);margin-top:10px}",
    ".widget-card iframe{width:100%;height:100%;border:0;background:#fff}",
    ".widget-grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(360px,1fr));gap:16px;margin:24px 0}",
    ".finding-extra{margin-top:32px;padding-top:24px;border-top:1px solid var(--line)}",
    ".finding-extra h3{font-size:15px;font-weight:700;color:var(--ink);margin:0 0 14px}",
    ".finding-extra .prose{font-size:14px;line-height:1.7;color:#44403c;margin:0 0 16px;max-width:72ch}",
    ".code-block{background:#1e1e2e;border-radius:14px;padding:18px 20px;margin:18px 0;overflow-x:auto;box-shadow:0 2px 12px rgba(0,0,0,.08)}",
    ".code-block pre{margin:0;padding:0;background:transparent}",
    ".code-block code{font-family:'JetBrains Mono',Consolas,monospace;font-size:12.5px;line-height:1.6;color:#cdd6f4;white-space:pre;display:block}",
    ".figure-grid{display:grid;gap:14px;margin:18px 0}",
    ".figure-grid-3col{grid-template-columns:repeat(auto-fill,minmax(280px,1fr))}",
    ".figure-grid figure{background:var(--paper);border:1px solid var(--line);border-radius:14px;overflow:hidden;margin:0;box-shadow:0 1px 4px rgba(0,0,0,.02)}",
    ".figure-grid figure img{width:100%;height:auto;display:block}",
    ".figure-grid figcaption{padding:10px 14px;font-size:12px;color:#78716c;text-align:center}",
    ".executive{background:var(--paper)}",
    ".exec-big-grid{display:grid;grid-template-columns:repeat(4,1fr);gap:18px;margin-bottom:30px}",
    ".exec-big{background:var(--paper);border:1px solid var(--line);border-radius:22px;padding:24px 22px;box-shadow:0 2px 8px rgba(0,0,0,.03);display:grid;gap:6px;position:relative;overflow:hidden;transition:transform .3s cubic-bezier(.4,0,.2,1),box-shadow .3s;animation:ghs-card-enter .7s cubic-bezier(.16,1,.3,1) both}",
    ".exec-big:nth-child(1){animation-delay:.1s}",
    ".exec-big:nth-child(2){animation-delay:.2s}",
    ".exec-big:nth-child(3){animation-delay:.3s}",
    ".exec-big:nth-child(4){animation-delay:.4s}",
    "@keyframes ghs-card-enter{from{opacity:0;transform:translateY(30px) scale(.95)}to{opacity:1;transform:translateY(0) scale(1)}}",
    ".exec-big:hover{transform:translateY(-3px);box-shadow:0 8px 24px rgba(0,0,0,.06)}",
    ".exec-big:before{content:none}",
    ".exec-big.tone-orange:before{content:none}",
    ".exec-big.tone-ink:before{content:none}",
    ".exec-num{font-family:'Source Serif 4',serif;font-size:46px;font-weight:700;color:#292524;line-height:1.1}",
    ".exec-label{font-weight:700;font-size:14px;color:#292524}",
    ".exec-hint{font-size:12.5px;color:#78716c}",
    ".exec-tldr{background:var(--paper);border:1px solid var(--line);border-radius:24px;padding:26px 28px;box-shadow:0 2px 8px rgba(0,0,0,.03);display:grid;gap:14px}",
    ".exec-tldr .kicker{display:inline-block;font-size:12px;letter-spacing:.22em;text-transform:uppercase;color:#78716c;font-weight:800}",
    ".tldr-list{margin:0;padding:0 0 0 22px;display:grid;gap:10px;font-size:15.5px;line-height:1.65}",
    ".tldr-list li b{color:#292524}",
    ".btn-light{align-self:start;display:inline-flex;align-items:center;gap:6px;padding:10px 18px;border-radius:8px;background:transparent;color:#78716c;text-decoration:none;font-weight:600;font-size:13px;border:1px solid rgba(0,0,0,.08);transition:all .15s}",
    ".btn-light:hover{color:#292524;border-color:rgba(0,0,0,.15);background:rgba(0,0,0,.02)}",
    ".dq-grid{display:grid;gap:18px}",
    ".dq-grid .table-wrap caption{background:linear-gradient(90deg,#fbeede,#fff)}",
    ".codebook .table-wrap{max-height:520px;overflow:auto}",
    ".widget-link{background:linear-gradient(135deg,#fff,#fbf6ee);border:1px dashed var(--line-strong);border-radius:18px;padding:14px 18px;display:grid;gap:6px}",
    ".widget-link strong{font-size:13px;letter-spacing:.16em;text-transform:uppercase;color:var(--orange)}",
    ".widget-link a{margin-right:14px;font-size:13.5px;color:var(--blue);text-decoration:none;border-bottom:1px dashed var(--blue)}",
    ".limit-note{background:#fffaf2;border-radius:16px;border:1px solid var(--line);padding:16px 20px;display:grid;gap:6px}",
    ".limit-note strong{font-size:12.5px;letter-spacing:.18em;text-transform:uppercase;color:var(--muted)}",
    ".limit-note ul{margin:0;padding-left:18px;color:var(--muted);font-size:13.5px;line-height:1.7}",
    ".country-grid{display:grid;grid-template-columns:repeat(2,1fr);gap:22px}",
    ".country-card{background:#fff;border:1px solid var(--line);border-radius:24px;padding:22px;box-shadow:0 16px 40px rgba(13,18,27,.06);display:grid;gap:14px}",
    ".country-card header{display:grid;gap:6px}",
    ".country-card h3{margin:6px 0 0;font-size:22px;color:var(--ink);font-family:'Source Serif 4',serif}",
    ".country-card .pill{background:var(--blue);color:#f7eedf;font-weight:900}",
    ".country-card p{margin:0;color:var(--muted);font-size:13px}",
    ".country-body{display:grid;gap:14px}",
    ".country-body .fig-inline{box-shadow:none;border-color:var(--line)}",
    ".regional-grid{display:grid;grid-template-columns:1fr 1fr;gap:20px;align-items:start}",
    ".regional-notes{margin:24px 0 0;padding-left:20px;display:grid;gap:8px;color:var(--ink);font-size:15px}",
    ".regional-notes b{color:var(--blue)}",
    ".atlas-grid{display:grid;grid-template-columns:repeat(2,1fr);gap:20px;margin-bottom:18px}",
    ".simulator{background:var(--paper)}",
    ".sim-grid{display:grid;grid-template-columns:.9fr 1.1fr;gap:24px;align-items:start}",
    ".sim-controls{background:#fff;border:1px solid var(--line);border-radius:22px;padding:22px 22px;display:grid;gap:18px;box-shadow:0 16px 40px rgba(13,18,27,.06)}",
    ".sim-controls label{display:grid;gap:8px;font-weight:700;color:var(--ink);font-size:14px}",
    ".sim-controls input[type=range]{width:100%;accent-color:var(--orange)}",
    ".sim-controls output{font-family:'Source Serif 4',serif;font-size:22px;color:var(--blue);font-weight:800}",
    ".sim-output{display:grid;grid-template-columns:repeat(2,1fr);gap:16px}",
    ".sim-output article{background:#fff;border:1px solid var(--line);border-radius:18px;padding:18px;display:grid;gap:6px;box-shadow:0 12px 30px rgba(13,18,27,.05)}",
    ".sim-output span{font-size:12.5px;color:var(--muted);font-weight:700}",
    ".sim-output b{font-family:'Source Serif 4',serif;font-size:30px;color:var(--blue)}",
    ".sim-output em{color:var(--muted);font-style:normal;font-size:12.5px}",
    ".sim-note{margin-top:16px;color:var(--muted);font-size:13.5px}",
    ".glossary .table-wrap{max-height:520px;overflow:auto}",
    ".glossary-sources{margin-top:18px;background:#fff;border:1px solid var(--line);border-radius:18px;padding:18px 22px;color:var(--ink);font-size:14.5px}",
    ".glossary-sources h3{margin:0 0 8px;color:var(--blue);font-size:16px}",
    ".glossary-sources ul{margin:0;padding-left:20px;display:grid;gap:6px}",
    ".session-meta{display:flex;flex-wrap:wrap;gap:12px;list-style:none;padding:0;margin:0 0 18px;color:var(--muted);font-size:13px}",
    ".session-meta li{background:#fff;border:1px solid var(--line);border-radius:999px;padding:6px 12px}",
    ".session-grid{margin:0;padding:0;list-style:none;display:grid;grid-template-columns:repeat(3,1fr);gap:8px;font-size:13px}",
    ".session-grid li{background:#fff;border:1px solid var(--line);border-radius:14px;padding:8px 12px}",
    ".session-grid li b{color:var(--blue);font-weight:800}",
    ".muted{color:var(--muted);font-size:14px}"
  ), collapse = "")
}

.ghs_js <- function() {
  paste(c(
    "function filterFigures(k,b){document.querySelectorAll('.gallery .tabs button').forEach(x=>x.classList.remove('active'));b.classList.add('active');document.querySelectorAll('.gallery-card').forEach(c=>{c.style.display=(k==='\u5168\u90e8'||c.dataset.kind===k)?'block':'none'});}",
    "function filterWidgets(k,b){document.querySelectorAll('.widgets .tabs button').forEach(x=>x.classList.remove('active'));b.classList.add('active');document.querySelectorAll('.widget-card').forEach(c=>{c.style.display=(k==='\u5168\u90e8'||c.dataset.kind===k)?'flex':'none'});}",
    "function filterFigureIndex(q){q=(q||'').trim().toLowerCase();var n=0;document.querySelectorAll('.figure-index-row').forEach(function(r){var hay=((r.dataset.title||'')+' '+(r.dataset.kind||'')+' '+r.textContent).toLowerCase();var ok=!q||hay.indexOf(q)>=0;r.style.display=ok?'grid':'none';if(ok)n++;});var c=document.getElementById('figure-index-count');if(c)c.textContent=n+' \u5f20\u56fe';}",
    "function filterAssetConsole(q){q=(q||'').trim().toLowerCase();document.querySelectorAll('.asset-console-chips button').forEach(function(b){var t=(b.textContent||'').trim().toLowerCase();b.classList.toggle('active',(!q&&t==='\u5168\u90e8')||(q&&t===q));});document.querySelectorAll('.asset-row').forEach(function(r){var hay=((r.dataset.title||'')+' '+(r.dataset.kind||'')+' '+r.textContent).toLowerCase();var ok=!q||hay.indexOf(q)>=0;r.style.display=ok?'grid':'none';});var s=document.getElementById('asset-search');if(s&&s.value!==q&&q.length<=4)s.value=q;}",
    "function openFigure(btn){var img=btn.querySelector('img');document.getElementById('modal-img').src=img.src;document.getElementById('modal-title').textContent=btn.dataset.title||img.alt;document.getElementById('fig-modal').classList.add('open');}",
    "function closeFigure(){document.getElementById('fig-modal').classList.remove('open');}",
    "(function(){var scrollRaf=0,navToken=0;function absTop(el){return el.getBoundingClientRect().top+window.scrollY;}function navOffset(){var topBar=document.querySelector('.top-bar');var fixed=0;if(topBar&&getComputedStyle(topBar).position==='fixed')fixed+=topBar.getBoundingClientRect().height||0;return Math.max(12,Math.round(fixed+22));}function animateTo(top,instant){top=Math.max(0,Math.round(top));if(scrollRaf)cancelAnimationFrame(scrollRaf);var reduce=window.matchMedia&&window.matchMedia('(prefers-reduced-motion: reduce)').matches;if(instant||reduce){window.scrollTo(0,top);return;}var start=window.scrollY;var dist=top-start;var dur=Math.min(760,Math.max(320,Math.abs(dist)*0.16));var t0=performance.now();function ease(t){return t<.5?4*t*t*t:1-Math.pow(-2*t+2,3)/2;}function step(now){var p=Math.min(1,(now-t0)/dur);window.scrollTo(0,Math.round(start+dist*ease(p)));if(p<1){scrollRaf=requestAnimationFrame(step);}else{scrollRaf=0;window.scrollTo(0,top);}}scrollRaf=requestAnimationFrame(step);}function scrollToId(id,replaceHash,instant){var el=document.getElementById(id);if(!el)return false;var token=++navToken;function topNow(){return id==='top'?0:absTop(el)-navOffset();}function align(){if(token!==navToken)return;window.scrollTo(0,Math.max(0,Math.round(topNow())));}animateTo(topNow(),instant);if(instant){setTimeout(align,60);}else{[880,1400,2200].forEach(function(ms){setTimeout(align,ms);});}if(replaceHash!==false&&history.replaceState)history.replaceState(null,'','#'+id);return true;}function closeDock(){var dock=document.getElementById('ghs-dock');if(dock)dock.classList.remove('open');}function closeMobileToc(){var toc=document.getElementById('mobile-toc');if(toc)toc.open=false;}window.ghsNavOffset=navOffset;window.ghsAnchorTop=function(el){return el?absTop(el):0;};window.ghsScrollToId=scrollToId;window.ghsCloseDock=closeDock;window.ghsCloseMobileToc=closeMobileToc;window.toggleDock=function(force){var dock=document.getElementById('ghs-dock');if(!dock)return;var open=typeof force==='boolean'?force:!dock.classList.contains('open');dock.classList.toggle('open',open);};document.addEventListener('click',function(e){var dock=document.getElementById('ghs-dock');if(dock&&dock.classList.contains('open')&&!e.target.closest('#ghs-dock'))closeDock();var toc=document.getElementById('mobile-toc');if(toc&&toc.open&&!e.target.closest('#mobile-toc'))closeMobileToc();},true);document.addEventListener('keydown',function(e){if(e.key==='Escape'){closeDock();closeMobileToc();closeFigure();}});})();",
    "function loadWidgetUrl(url,title,fallback){document.getElementById('widget-title').textContent=title;var f=document.getElementById('widget-frame');var wrap=f.parentNode;var oldBanner=document.getElementById('widget-fallback-banner');if(oldBanner)oldBanner.remove();var banner=document.createElement('div');banner.id='widget-fallback-banner';banner.className='widget-fallback-banner';banner.innerHTML='\u6b63\u5728\u52a0\u8f7d <strong>'+title+'</strong>\u2026 \u82e5\u957f\u65f6\u95f4\u7a7a\u767d\uff0c\u8bf7 <a href=\"'+(fallback||url)+'\" target=\"_blank\" rel=\"noreferrer\">\u5728\u65b0\u7a97\u6253\u5f00</a>\u3002';wrap.insertBefore(banner,f);f.onload=function(){banner.classList.add('loaded');banner.innerHTML='\u5df2\u52a0\u8f7d <strong>'+title+'</strong>\u3002\u82e5\u663e\u793a\u4e0d\u5b8c\u6574\uff0c\u53ef <a href=\"'+(fallback||url)+'\" target=\"_blank\" rel=\"noreferrer\">\u5728\u65b0\u7a97\u6253\u5f00</a>\u3002';};f.onerror=function(){banner.classList.add('error');banner.innerHTML='\u65e0\u6cd5\u76f4\u63a5\u5728\u53f3\u4fa7\u52a0\u8f7d <strong>'+title+'</strong>\uff0c\u8bf7 <a href=\"'+(fallback||url)+'\" target=\"_blank\" rel=\"noreferrer\">\u5728\u65b0\u7a97\u6253\u5f00</a>\u3002';};f.src=url;if(window.ghsScrollToId)window.ghsScrollToId('widgets',true);if(window.ghsCloseDock)window.ghsCloseDock();if(window.ghsCloseMobileToc)window.ghsCloseMobileToc();}",
    "(function(){var bar=document.getElementById('read-progress');function update(){var h=document.documentElement;var s=h.scrollTop||document.body.scrollTop;var max=(h.scrollHeight-h.clientHeight)||1;var pct=s/max;if(bar)bar.style.width=(pct*100)+'%';}window.addEventListener('scroll',update,{passive:true});window.addEventListener('resize',update);update();})();",
    "(function(){var navLinks=document.querySelectorAll('.dock-brand[href^=\"#\"],.dock-links a[href^=\"#\"],.mobile-toc-items a[href^=\"#\"]');var targets=[];navLinks.forEach(function(a){var id=a.getAttribute('href').slice(1);if(id&&!targets.some(function(t){return t.id===id;})){var el=document.getElementById(id);if(el)targets.push({id:id,el:el});}});function spy(){if(!navLinks.length||!targets.length)return;var pos=window.scrollY+(window.ghsNavOffset?window.ghsNavOffset():24)+10;var sorted=targets.slice().sort(function(a,b){return window.ghsAnchorTop(a.el)-window.ghsAnchorTop(b.el);});var cur=sorted[0].id;for(var i=0;i<sorted.length;i++){if(window.ghsAnchorTop(sorted[i].el)<=pos){cur=sorted[i].id;}else{break;}}navLinks.forEach(function(a){a.classList.toggle('active',a.getAttribute('href')==='#'+cur);});}document.addEventListener('click',function(e){var a=e.target.closest('a[href^=\"#\"]');if(!a)return;var id=a.getAttribute('href').slice(1);if(!id||!document.getElementById(id)||!window.ghsScrollToId)return;if(window.ghsScrollToId(id,true)){e.preventDefault();if(window.ghsCloseDock)window.ghsCloseDock();if(window.ghsCloseMobileToc)window.ghsCloseMobileToc();setTimeout(spy,260);}},false);window.addEventListener('scroll',spy,{passive:true});window.addEventListener('resize',spy);function alignHash(instant){if(location.hash){var id=decodeURIComponent(location.hash.slice(1));if(window.ghsScrollToId)window.ghsScrollToId(id,false,instant);}spy();}document.addEventListener('DOMContentLoaded',function(){setTimeout(function(){alignHash(true);},40);setTimeout(function(){alignHash(true);},360);});window.addEventListener('load',function(){setTimeout(function(){alignHash(true);},80);});spy();})();",
    "document.addEventListener('toggle',function(e){var d=e.target;if(d&&d.tagName==='DETAILS'&&d.classList.contains('widget-embed')&&d.open){var f=d.querySelector('iframe[data-src]');if(f&&!f.src){f.src=f.dataset.src;}}},true);",
    "(function(){var ios=('IntersectionObserver' in window)?new IntersectionObserver(function(es){es.forEach(function(en){if(en.isIntersecting){var el=en.target;var src=el.dataset.src;if(src&&!el.src){el.src=src;}ios.unobserve(el);}});},{rootMargin:'200px 0px'}):null;document.querySelectorAll('iframe[data-src]').forEach(function(f){if(ios){ios.observe(f);}});})();",
    "function runSim(){var dO=parseFloat(document.getElementById('sim-oops').value);var dG=parseFloat(document.getElementById('sim-gghed').value);var dE=parseFloat(document.getElementById('sim-ext').value);document.getElementById('sim-oops-out').textContent=(dO>0?'+':'')+dO;document.getElementById('sim-gghed-out').textContent=(dG>0?'+':'')+dG;document.getElementById('sim-ext-out').textContent=(dE>0?'+':'')+dE;function clamp(x,lo,hi){return Math.max(lo,Math.min(hi,x));}var baseO=parseFloat((document.getElementById('sim-oops-new').nextElementSibling.textContent.match(/[-+]?\\d+(\\.\\d+)?/)||[0])[0]);var baseG=parseFloat((document.getElementById('sim-gghed-new').nextElementSibling.textContent.match(/[-+]?\\d+(\\.\\d+)?/)||[0])[0]);var baseHigh=parseFloat((document.getElementById('sim-oops-high').nextElementSibling.textContent.match(/[-+]?\\d+/)||[0])[0]);var baseExtH=parseFloat((document.getElementById('sim-ext-high').nextElementSibling.textContent.match(/[-+]?\\d+/)||[0])[0]);var newO=clamp(baseO+dO+(-0.4*dG),0,90);var newG=clamp(baseG+dG,0,95);var newHigh=clamp(Math.round(baseHigh+1.6*dO+0.6*dG*-1),0,200);var newExtH=clamp(Math.round(baseExtH+0.5*dE),0,200);document.getElementById('sim-oops-new').textContent=newO.toFixed(1)+'%';document.getElementById('sim-gghed-new').textContent=newG.toFixed(1)+'%';document.getElementById('sim-oops-high').textContent=newHigh;document.getElementById('sim-ext-high').textContent=newExtH;}",
    "(function(){var els=document.querySelectorAll('.hero-reveal');if(!els.length)return;var io=new IntersectionObserver(function(entries){entries.forEach(function(e){if(e.isIntersecting){var el=e.target;var d=parseInt(el.dataset.delay||0);setTimeout(function(){el.classList.add('in');},d);}else{e.target.classList.remove('in');}});},{threshold:0.15});els.forEach(function(el){io.observe(el);});})();",
    "document.addEventListener('DOMContentLoaded',function(){if(document.getElementById('sim-oops'))runSim();});"
  ), collapse = "")
}


.ghs_css_v3 <- function() {
  paste(c(
    ":root{",
      "--g3-primary:#1d3f5f;--g3-secondary:#c46327;--g3-good:#2a857a;",
      "--g3-warn:#c89a3b;--g3-bad:#a23b3b;--g3-neutral:#5d667a;",
      "--g3-highlight:#f7c08a;",
      "--g3-paper:#fbf6ee;--g3-paper2:#f1e8da;--g3-ink:#0d121b;",
      "--g3-line:rgba(13,18,27,.10);--g3-line-strong:rgba(13,18,27,.18);",
      "--g3-radius-sm:8px;--g3-radius:14px;--g3-radius-lg:24px;",
      "--g3-gap-xs:8px;--g3-gap-sm:12px;--g3-gap:18px;",
      "--g3-gap-lg:32px;--g3-gap-xl:64px;",
      "--g3-shadow-sm:0 4px 12px rgba(13,18,27,.06);",
      "--g3-shadow:0 16px 40px rgba(13,18,27,.08);",
      "--g3-shadow-lg:0 24px 64px rgba(13,18,27,.12);",
      "--g3-fs-display:clamp(48px,7vw,96px);",
      "--g3-fs-h1:clamp(34px,4.4vw,56px);",
      "--g3-fs-h2:clamp(28px,3.4vw,44px);",
      "--g3-fs-h3:clamp(20px,2.4vw,28px);",
      "--g3-fs-body:16px;--g3-fs-meta:13px;--g3-fs-tiny:11.5px;",
      "--g3-track-tight:.02em;--g3-track-wide:.18em;",
      "--g3-line-h-tight:1.12;--g3-line-h:1.55;--g3-line-h-loose:1.75;",
    "}",
      "--g3-paper:#0f141e;--g3-paper2:#161c2a;--g3-ink:#e7e9ee;",
      "--g3-line:rgba(255,255,255,.10);--g3-line-strong:rgba(255,255,255,.20);",
      "--g3-shadow:0 16px 40px rgba(0,0,0,.45);",
      "--g3-shadow-lg:0 24px 64px rgba(0,0,0,.6);",
    "}",

    ".ghs-v3 *,.ghs-v3 *:before,.ghs-v3 *:after{box-sizing:border-box;min-width:0}",
    ".ghs-v3{font-family:'Inter','Noto Sans CJK SC','Microsoft YaHei',system-ui,sans-serif;color:var(--g3-ink);line-height:var(--g3-line-h)}",
    ".ghs-v3 p,.ghs-v3 li,.ghs-v3 dd,.ghs-v3 td{word-break:keep-all;overflow-wrap:anywhere;hyphens:none}",
    ".ghs-v3 h1,.ghs-v3 h2,.ghs-v3 h3,.ghs-v3 h4{font-family:'Source Serif 4','Inter',serif;line-height:var(--g3-line-h-tight);margin:0}",
    ".ghs-v3 a{color:var(--g3-primary);text-underline-offset:3px}",
    ".ghs-v3 img,.ghs-v3 svg,.ghs-v3 iframe,.ghs-v3 video,.ghs-v3 canvas{max-width:100%;height:auto;min-width:0}",

    ".ghs-section-head{display:flex;flex-direction:column;gap:8px;max-width:920px;margin:0 0 28px}",
    ".ghs-section-head[data-align='center']{margin-left:auto;margin-right:auto;text-align:center;align-items:center}",
    ".ghs-kicker{display:inline-block;font-size:var(--g3-fs-tiny);letter-spacing:var(--g3-track-wide);text-transform:uppercase;color:var(--g3-secondary);font-weight:800}",
    ".ghs-section-title{font-size:var(--g3-fs-h2);color:var(--g3-ink);margin:0}",
    ".ghs-section-lead{font-size:18px;color:var(--g3-neutral);max-width:780px;margin:0;line-height:1.55}",

    ".ghs-stat-strip{display:grid;grid-template-columns:repeat(4,minmax(0,1fr));gap:var(--g3-gap);margin:24px 0}",
    ".ghs-stat-cell{padding:18px 20px;background:var(--g3-paper2);border:1px solid var(--g3-line);border-radius:var(--g3-radius);min-width:0}",
    ".ghs-stat-value{font-family:'Source Serif 4',serif;font-size:30px;font-weight:700;color:var(--g3-primary);line-height:1.05}",
    ".ghs-stat-label{margin-top:6px;color:var(--g3-neutral);font-size:13px}",

    ".ghs-kpi-grid{display:grid;grid-template-columns:repeat(4,1fr);gap:var(--g3-gap)}",
    ".ghs-kpi{position:relative;padding:22px 20px;border:1px solid var(--g3-line);border-radius:var(--g3-radius);background:var(--g3-paper2);box-shadow:var(--g3-shadow-sm);overflow:hidden;min-width:0}",
    ".ghs-kpi:before{content:'';position:absolute;left:0;top:0;width:5px;height:100%;background:var(--tone,var(--g3-primary))}",
    ".ghs-kpi-value{font-family:'Source Serif 4',serif;font-size:34px;font-weight:700;color:var(--g3-primary);line-height:1.05}",
    ".ghs-kpi-label{margin-top:8px;font-weight:700;font-size:13.5px;letter-spacing:var(--g3-track-tight);display:flex;align-items:center;justify-content:space-between;gap:8px}",
    ".ghs-kpi-trend{font-weight:700;font-size:12.5px;font-family:'Source Serif 4',serif}",
    ".ghs-kpi-hint{margin-top:6px;font-size:12px;color:var(--g3-neutral);line-height:1.5}",

    ".ghs-callout{display:flex;flex-direction:column;gap:6px;padding:14px 18px 16px 22px;border-radius:var(--g3-radius);background:var(--g3-paper2);border-left:4px solid var(--bar,var(--g3-primary));box-shadow:var(--g3-shadow-sm);font-size:14px;color:var(--g3-ink)}",
    ".ghs-callout-title{display:block;font-weight:800;font-size:13.5px;letter-spacing:.04em;text-transform:uppercase;color:var(--bar,var(--g3-primary))}",
    ".ghs-callout-body{line-height:1.65;color:var(--g3-ink)}",
    ".ghs-callout-good{--bar:var(--g3-good)}",
    ".ghs-callout-warn{--bar:var(--g3-warn);background:#fff8ec}",
    ".ghs-callout-bad{--bar:var(--g3-bad);background:#fdf0ee}",

    ".ghs-finding{padding:96px 0;border-top:1px solid var(--g3-line)}",
    ".ghs-finding,.ghs-finding:nth-of-type(odd),.ghs-finding:nth-of-type(even){background:var(--g3-paper)}",
    ".ghs-finding-head{display:grid;grid-template-columns:minmax(150px,max-content) minmax(0,1fr);gap:32px;align-items:start;margin-bottom:32px}",
    ".ghs-finding-num{font-family:'Source Serif 4',serif;font-size:72px;line-height:.95;color:var(--g3-secondary);font-weight:800;white-space:nowrap;margin-top:0}",
    ".ghs-finding-meta{display:flex;flex-direction:column;gap:8px}",
    ".ghs-finding-title{font-size:var(--g3-fs-h2);line-height:1.05;color:var(--g3-ink)}",
    ".ghs-finding-lead{font-size:18px;color:var(--g3-neutral);max-width:760px;line-height:1.55}",

    ".ghs-chip-row{display:flex;flex-wrap:wrap;gap:10px;margin:14px 0 4px}",
    ".ghs-chip{display:inline-flex;align-items:baseline;gap:8px;padding:7px 14px;border-radius:999px;border:1px solid var(--g3-line-strong);background:var(--g3-paper);font-size:13px}",
    ".ghs-chip b{color:var(--g3-neutral);font-weight:600;font-size:11px;text-transform:uppercase;letter-spacing:.1em}",
    ".ghs-chip i{font-style:normal;font-weight:800;color:var(--g3-ink)}",
    ".ghs-chip[data-tone='primary'] i{color:var(--g3-primary)}",
    ".ghs-chip[data-tone='good'] i{color:var(--g3-good)}",
    ".ghs-chip[data-tone='warn'] i{color:var(--g3-warn)}",
    ".ghs-chip[data-tone='bad'] i{color:var(--g3-bad)}",

    ".ghs-fig-grid{display:grid;gap:var(--g3-gap);margin:18px 0}",
    ".ghs-fig-grid-2{grid-template-columns:repeat(2,1fr)}",
    ".ghs-fig-grid-3{grid-template-columns:repeat(3,1fr)}",
    ".ghs-fig-grid-4{grid-template-columns:repeat(4,1fr)}",
    ".ghs-fig-card{background:var(--g3-paper2);border:1px solid var(--g3-line);border-radius:var(--g3-radius);padding:14px;min-width:0;overflow:hidden}",
    ".ghs-fig-card figure{margin:0}",
    ".ghs-fig-card figcaption{margin-top:8px;font-size:13px;color:var(--g3-neutral);line-height:1.55}",

    ".ghs-deep-grid{display:grid;grid-template-columns:repeat(3,1fr);gap:var(--g3-gap);margin:32px 0;padding:24px;background:var(--g3-paper2);border-radius:var(--g3-radius-lg);border:1px solid var(--g3-line)}",
    ".ghs-deep-card{background:var(--g3-paper);border:1px solid var(--g3-line);border-radius:var(--g3-radius);padding:18px 20px;min-width:0}",
    ".ghs-deep-card h4{font-family:'Inter',sans-serif;font-size:11.5px;letter-spacing:var(--g3-track-wide);text-transform:uppercase;color:var(--g3-secondary);margin:0 0 10px;font-weight:800}",
    ".ghs-deep-card p{font-size:13.5px;line-height:1.7;color:var(--g3-ink);margin:0}",
    ".ghs-deep-card.tone-data h4{color:var(--g3-primary)}",
    ".ghs-deep-card.tone-method h4{color:var(--g3-good)}",
    ".ghs-deep-card.tone-assume h4{color:var(--g3-secondary)}",
    ".ghs-deep-card.tone-limit h4{color:var(--g3-warn)}",
    ".ghs-deep-card.tone-sens h4{color:var(--g3-neutral)}",
    ".ghs-deep-card.tone-policy h4{color:var(--g3-bad)}",

    ".ghs-with-toc{display:grid;grid-template-columns:minmax(0,1fr) 240px;gap:var(--g3-gap-xl);align-items:start}",
    ".ghs-toc{position:sticky;top:96px;align-self:start;display:flex;flex-direction:column;gap:6px;border-left:2px solid var(--g3-line);padding:6px 0 6px 18px;font-size:12.5px;color:var(--g3-neutral);max-height:calc(100vh - 120px);overflow-y:auto}",
    ".ghs-toc a{display:block;color:var(--g3-neutral);text-decoration:none;padding:4px 8px;border-radius:6px;transition:color .2s,background .2s}",
    ".ghs-toc a.active,.ghs-toc a:hover{color:var(--g3-primary);background:var(--g3-paper2)}",
    ".ghs-toc .ghs-toc-group{font-size:11px;letter-spacing:var(--g3-track-wide);text-transform:uppercase;color:var(--g3-secondary);font-weight:800;margin-top:8px;padding:0 8px}",

    ".ghs-lineage{display:grid;grid-template-columns:repeat(4,1fr);gap:var(--g3-gap);margin:24px 0}",
    ".ghs-lineage-step{position:relative;padding:18px 20px;background:var(--g3-paper2);border:1px solid var(--g3-line);border-radius:var(--g3-radius);min-width:0}",
    ".ghs-lineage-step:not(:last-child):after{content:'\u2192';position:absolute;right:-18px;top:50%;transform:translateY(-50%);color:var(--g3-secondary);font-size:24px;font-weight:800;z-index:2}",
    ".ghs-lineage-stage{font-size:11px;letter-spacing:var(--g3-track-wide);text-transform:uppercase;color:var(--g3-secondary);font-weight:800;margin-bottom:6px}",
    ".ghs-lineage-name{font-family:'Source Serif 4',serif;font-weight:700;font-size:18px;color:var(--g3-ink);margin-bottom:4px}",
    ".ghs-lineage-meta{font-size:12px;color:var(--g3-neutral);line-height:1.5}",

    ".ghs-paths{display:grid;grid-template-columns:repeat(3,1fr);gap:var(--g3-gap);margin:24px 0}",
    ".ghs-path-card{padding:24px;background:var(--g3-paper);border:1px solid var(--g3-line);border-radius:var(--g3-radius-lg);box-shadow:var(--g3-shadow-sm);display:flex;flex-direction:column;gap:10px;min-width:0}",
    ".ghs-path-card[data-tone='primary']{border-top:4px solid var(--g3-primary)}",
    ".ghs-path-card[data-tone='good']{border-top:4px solid var(--g3-good)}",
    ".ghs-path-card[data-tone='secondary']{border-top:4px solid var(--g3-secondary)}",
    ".ghs-path-icon{font-size:28px;line-height:1}",
    ".ghs-path-title{font-family:'Source Serif 4',serif;font-weight:700;font-size:22px;color:var(--g3-ink)}",
    ".ghs-path-lead{font-size:14px;color:var(--g3-neutral);line-height:1.6}",
    ".ghs-path-list{list-style:none;padding:0;margin:8px 0 0;display:grid;gap:6px}",
    ".ghs-path-list li{font-size:13px;padding:6px 10px;background:var(--g3-paper2);border-radius:8px}",
    ".ghs-path-list li a{color:var(--g3-ink);text-decoration:none}",
    ".ghs-path-list li a:hover{color:var(--g3-primary)}",

    ".ghs-sources{display:grid;grid-template-columns:repeat(2,1fr);gap:var(--g3-gap);margin:24px 0}",
    ".ghs-source-card{padding:16px 20px;background:var(--g3-paper2);border:1px solid var(--g3-line);border-radius:var(--g3-radius);font-size:13.5px;line-height:1.55;min-width:0}",
    ".ghs-source-card b{display:block;color:var(--g3-primary);font-weight:800;margin-bottom:4px}",
    ".ghs-source-card span{color:var(--g3-neutral)}",

    ".ghs-sens-grid{display:grid;grid-template-columns:repeat(2,1fr);gap:var(--g3-gap);margin:18px 0}",
    ".ghs-sens-card{padding:16px 18px;background:var(--g3-paper2);border:1px solid var(--g3-line);border-radius:var(--g3-radius);min-width:0}",
    ".ghs-sens-title{font-weight:800;color:var(--g3-primary);font-size:13.5px;margin:0 0 6px}",
    ".ghs-sens-value{font-family:'Source Serif 4',serif;font-size:24px;color:var(--g3-ink);font-weight:700}",
    ".ghs-sens-note{font-size:12px;color:var(--g3-neutral);margin-top:4px;line-height:1.5}",

    ".ghs-bullet-list{list-style:none;padding:0;margin:18px 0;display:grid;gap:10px}",
    ".ghs-bullet-list li{position:relative;padding:12px 16px 12px 38px;background:var(--g3-paper2);border:1px solid var(--g3-line);border-radius:var(--g3-radius);font-size:14px;line-height:1.65}",
    ".ghs-bullet-list li:before{content:'';position:absolute;left:14px;top:18px;width:14px;height:14px;border-radius:50%;background:var(--bullet,var(--g3-primary))}",
    ".ghs-bullet-list li[data-tone='good']:before{background:var(--g3-good)}",
    ".ghs-bullet-list li[data-tone='warn']:before{background:var(--g3-warn)}",
    ".ghs-bullet-list li[data-tone='bad']:before{background:var(--g3-bad)}",
    ".ghs-bullet-list li b{color:var(--g3-primary);font-weight:800;display:block;margin-bottom:4px;font-size:13px;text-transform:uppercase;letter-spacing:.04em}",

    "@media(max-width:1280px){.ghs-with-toc{grid-template-columns:1fr;gap:var(--g3-gap-lg)}.ghs-toc{display:none}}",
    "@media(max-width:1024px){.ghs-stat-strip{grid-template-columns:repeat(2,1fr)}.ghs-kpi-grid{grid-template-columns:repeat(2,1fr)}.ghs-fig-grid-3,.ghs-fig-grid-4{grid-template-columns:repeat(2,1fr)}.ghs-deep-grid{grid-template-columns:repeat(2,1fr)}.ghs-paths{grid-template-columns:1fr}.ghs-lineage{grid-template-columns:repeat(2,1fr)}.ghs-lineage-step:nth-child(2n):after{display:none}.ghs-sources{grid-template-columns:1fr}}",
    "@media(max-width:900px){.finding-head,.ghs-finding-head{grid-template-columns:1fr;gap:14px}.finding-num,.ghs-finding-num{margin-top:0}}",
    "@media(max-width:768px){.ghs-finding-num{font-size:54px}.ghs-fig-grid-2,.ghs-fig-grid-3,.ghs-fig-grid-4{grid-template-columns:1fr}.ghs-deep-grid{grid-template-columns:1fr;padding:18px}.ghs-finding{padding:64px 0}.ghs-section-title{font-size:32px}.ghs-sens-grid{grid-template-columns:1fr}}",
    "@media(max-width:420px){.ghs-stat-strip,.ghs-kpi-grid{grid-template-columns:1fr}.ghs-finding-head{gap:14px}.ghs-finding-num{font-size:42px}.ghs-deep-card{padding:14px 16px}.ghs-lineage{grid-template-columns:1fr}.ghs-lineage-step:after{display:none}}"
  ), collapse = "")
}


.ghs_css_polish <- function() {
  paste(c(
    "body{font-feature-settings:'ss01' on,'cv11' on,'kern' on;text-rendering:optimizeLegibility}",
    "h1,h2,h3,h4{font-feature-settings:'ss01' on,'cv11' on,'kern' on,'liga' on;letter-spacing:-.015em}",
    ".hero h1{letter-spacing:-.025em;font-feature-settings:'ss01' on,'kern' on,'dlig' on}",
    ".section h2,.finding-head h2,.ghs-section-title,.ghs-finding-title{letter-spacing:-.018em}",
    "p,li,figcaption{font-feature-settings:'kern' on;hyphens:auto;-webkit-hyphens:auto}",
    ".section p{font-size:15.5px;line-height:1.75;color:#44403c;max-width:780px;margin-bottom:16px}",
    ".section p b,.section p strong{color:#1c1917;font-weight:700}",
    ".section p code{background:rgba(0,0,0,.04);padding:2px 6px;border-radius:4px;font-size:13px}",
    ".fig-frame{display:block;width:100%;border:none;background:none;padding:0;cursor:zoom-in;transition:transform .2s ease}",
    ".fig-frame:hover{transform:scale(1.01)}",
    ".fig-frame img{width:100%;height:auto;display:block;border-radius:8px}",

    ".hero{min-height:92vh;padding-top:48px}",
    ".hero h1{font-size:clamp(54px,8vw,108px);font-weight:800;line-height:.96}",
    ".hero h1 .accent{background:linear-gradient(95deg,#f7c08a 5%,#e89860 45%,#c46327 80%);-webkit-background-clip:text;background-clip:text}",
    ".hero-lead{font-size:clamp(18px,1.5vw,21px);line-height:1.55;font-weight:400;color:rgba(247,238,223,.92)}",
    ".eyebrow{font-weight:800;font-size:11.5px;background:linear-gradient(90deg,rgba(247,192,138,.18),rgba(247,238,223,.06));border-color:rgba(247,192,138,.35)}",
    ".hero-panel{border-radius:24px;backdrop-filter:blur(20px) saturate(140%);-webkit-backdrop-filter:blur(20px) saturate(140%);background:linear-gradient(165deg,rgba(247,238,223,.08),rgba(247,238,223,.02));border:1px solid rgba(247,238,223,.16);box-shadow:0 30px 80px rgba(0,0,0,.35),inset 0 1px 0 rgba(247,238,223,.12)}",
    ".hero-panel-list li b{font-size:38px;letter-spacing:-.02em;background:linear-gradient(180deg,#f7c08a,#d68146);-webkit-background-clip:text;background-clip:text;color:transparent}",
    ".btn{font-size:14.5px;padding:14px 26px;border-radius:14px;font-weight:800;letter-spacing:0;box-shadow:0 18px 44px rgba(212,129,70,.38),inset 0 1px 0 rgba(255,255,255,.4);transition:transform .2s ease,box-shadow .2s ease}",
    ".btn:hover{transform:translateY(-2px);box-shadow:0 22px 56px rgba(212,129,70,.5),inset 0 1px 0 rgba(255,255,255,.5)}",
    ".btn.alt{border-radius:14px;backdrop-filter:blur(8px);background:rgba(247,238,223,.14);transition:transform .2s ease,background .2s ease}",
    ".btn.alt:hover{background:rgba(247,238,223,.22);transform:translateY(-2px)}",
    ".btn.ghost{border-radius:14px;transition:border-color .2s ease,background .2s ease}",
    ".btn.ghost:hover{border-color:rgba(247,238,223,.4);background:rgba(247,238,223,.06)}",

    ".nav{padding:12px 0 20px}",
    ".brand{font-size:12px;letter-spacing:.24em}",
    ".links a{font-size:12px;font-weight:600;padding:5px 9px;letter-spacing:.02em}",
    ".links a:hover{background:rgba(247,238,223,.1)}",
    ".links .nav-group-label{font-size:10px;color:rgba(247,238,223,.38)}",

    ".section{padding:120px 0;border-top:1px solid var(--line)}",
    ".section h2{font-size:clamp(32px,3.8vw,50px);font-weight:800;line-height:1.08;margin-bottom:18px}",
    ".section .lead{font-size:clamp(16.5px,1.2vw,18px);line-height:1.7;color:var(--muted);max-width:900px}",
    ".section-head .kicker{font-size:11.5px;letter-spacing:.24em;font-weight:900;background:linear-gradient(90deg,#c46327,#1d3f5f);-webkit-background-clip:text;background-clip:text;color:transparent}",

    ".kpi{padding:28px 26px;border-radius:18px;background:linear-gradient(180deg,#fffaf2 30%,#f7e8d2 100%);border:1px solid rgba(13,18,27,.08);box-shadow:0 20px 56px rgba(13,18,27,.06),inset 0 1px 0 rgba(255,255,255,.6);transition:transform .25s ease,box-shadow .25s ease}",
    ".kpi:hover{transform:translateY(-4px);box-shadow:0 28px 72px rgba(13,18,27,.10)}",
    ".kpi:before{width:6px;background:linear-gradient(180deg,#c46327,#1d3f5f 70%,#2a857a)}",
    ".kpi-value{font-size:42px;font-weight:800;letter-spacing:-.02em;color:var(--blue)}",
    ".kpi-label{font-size:13px;letter-spacing:.06em;font-weight:800;text-transform:uppercase;color:var(--ink);margin-top:12px}",
    ".kpi-note{font-size:12.5px;color:var(--muted);line-height:1.6;margin-top:8px}",

    ".finding{padding:120px 0}",
    ".finding-head{grid-template-columns:minmax(150px,max-content) minmax(0,1fr);gap:32px}",
    ".finding-num{display:block;font-size:60px;font-weight:900;letter-spacing:0;color:var(--g3-secondary);margin-top:0;white-space:nowrap;line-height:.95}",
    ".finding-kicker{font-size:11.5px;letter-spacing:.22em;font-weight:900;color:var(--g3-primary)}",
    ".finding-head h2{font-size:clamp(34px,4.6vw,56px);font-weight:800;line-height:1.04;margin:8px 0 16px}",
    ".finding-head .lead{font-size:18px;line-height:1.65;color:var(--muted);max-width:780px;font-weight:400}",
    ".chip{padding:8px 16px;border-radius:999px;font-size:12.5px;border:1px solid rgba(13,18,27,.14);background:#fff;box-shadow:0 2px 8px rgba(13,18,27,.04);transition:transform .15s ease,box-shadow .15s ease}",
    ".chip:hover{transform:translateY(-1px);box-shadow:0 6px 16px rgba(13,18,27,.06)}",
    ".chip b{font-size:10.5px;letter-spacing:.12em;font-weight:800;color:var(--muted);text-transform:uppercase}",
    ".chip i{font-size:13.5px;font-weight:800;color:var(--ink)}",
    ".chip-blue i{color:var(--g3-primary)}.chip-orange i{color:var(--g3-secondary)}.chip-neutral i{color:var(--muted)}.chip-ink i{color:var(--ink)}",

    ".ghs-fig-card{padding:18px;border-radius:18px;background:linear-gradient(180deg,#fff,#fbf6ee);box-shadow:0 6px 22px rgba(13,18,27,.06);border:1px solid rgba(13,18,27,.08);transition:transform .2s ease,box-shadow .2s ease}",
    ".ghs-fig-card:hover{transform:translateY(-3px);box-shadow:0 16px 44px rgba(13,18,27,.10)}",
    ".ghs-fig-card img{border-radius:10px;width:100%;height:auto;display:block}",
    ".ghs-fig-card figcaption{margin-top:14px;font-size:13.5px;color:var(--muted);line-height:1.65;font-weight:500}",

    ".ghs-deep-grid,.deep-dive-grid{padding:28px;background:linear-gradient(180deg,#fbf6ee,#f3e8d6);border:1px solid rgba(13,18,27,.08);border-radius:24px;gap:18px}",
    ".ghs-deep-card,.deep-dive-grid .deep-card{padding:22px 24px;background:#fff;border:1px solid rgba(13,18,27,.08);border-radius:14px;box-shadow:0 4px 14px rgba(13,18,27,.04);transition:transform .15s ease;min-width:0}",
    ".ghs-deep-card:hover{transform:translateY(-2px)}",
    ".ghs-deep-card[data-tone='primary']{border-left:4px solid #1d3f5f}",
    ".ghs-deep-card[data-tone='good']{border-left:4px solid #2a857a}",
    ".ghs-deep-card[data-tone='warn']{border-left:4px solid #c89a3b}",
    ".ghs-deep-card[data-tone='bad']{border-left:4px solid #a23b3b}",
    ".ghs-deep-card[data-tone='neutral']{border-left:4px solid #5d667a}",
    ".ghs-deep-card[data-tone='secondary']{border-left:4px solid #c46327}",

    ".table-wrap{margin:16px 0;overflow-x:auto;border-radius:14px;box-shadow:0 2px 8px rgba(0,0,0,.03);border:1px solid rgba(0,0,0,.06)}",
    ".table-wrap table{width:100%;border-collapse:collapse;font-size:13.5px}",
    ".table-wrap th{background:#f0ebe1;padding:14px 16px;font-weight:700;font-size:12px;letter-spacing:.04em;text-transform:uppercase;color:#78716c;border-bottom:1px solid rgba(0,0,0,.08);text-align:left;position:sticky;top:0;z-index:2}",
    ".table-wrap td{padding:12px 16px;border-bottom:1px solid rgba(0,0,0,.04);color:var(--ink)}",
    ".table-wrap tr:hover td{background:rgba(0,0,0,.02)}",
    ".table-wrap tr:last-child td{border-bottom:0}",

    "pre,.code-pre{padding:22px 24px;border-radius:14px;background:linear-gradient(180deg,#0c1424,#0a1020);box-shadow:0 12px 36px rgba(0,0,0,.18);font-size:13px;line-height:1.7;overflow-x:auto;border:1px solid rgba(247,238,223,.08)}",
    "code{font-feature-settings:'liga' on,'calt' on}",
    ".code-figure{margin:18px 0 26px}",
    ".code-caption{font-size:12px;color:var(--muted);margin:0 0 8px;letter-spacing:.06em;text-transform:uppercase;font-weight:800}",

    ".section{position:relative}",

    "@keyframes ghs-fade-up{from{opacity:0;transform:translateY(24px)}to{opacity:1;transform:translateY(0)}}",
    ".section,.finding{animation:none;opacity:1;transform:none;filter:none}",
    "@media(prefers-reduced-motion:reduce){.section,.finding{animation:none}}",

    "a{transition:color .15s ease;text-decoration-thickness:1px;text-underline-offset:3px}",
    ".finding-body a:not(.btn),.section a:not(.btn){color:var(--blue);text-decoration:underline;text-decoration-color:rgba(29,63,95,.3);text-underline-offset:3px}",
    ".finding-body a:not(.btn):hover,.section a:not(.btn):hover{text-decoration-color:var(--blue);color:#0d121b}",

    "::selection{background:rgba(196,99,39,.28);color:var(--ink)}",

    "img[loading='lazy']{transition:opacity .4s ease}",
    "img:not([src]){opacity:0}",

    ".kpi-label,.chip i,.finding-kicker,.section-head .kicker,.module-card .module-title{overflow-wrap:break-word;word-break:keep-all;hyphens:auto}",

    "h1 span:not(.accent),h2 span,h3 span{vertical-align:baseline}",

    "@media(max-width:900px){.finding-head,.ghs-finding-head{grid-template-columns:1fr;gap:14px}.finding-num,.ghs-finding-num{margin-top:0}.finding-kicker,.ghs-kicker{margin-top:0}}",
    "@media(max-width:640px){.hero{min-height:auto;padding:30px 0 70px}.hero h1{font-size:42px}.section{padding:72px 0}.kpi{padding:22px 20px}.kpi-value{font-size:32px}.finding{padding:72px 0}.finding-num{font-size:54px;margin-top:0}.ghs-deep-grid,.deep-dive-grid{padding:18px}.btn{padding:12px 20px;font-size:13.5px}}",

    "a:focus-visible,button:focus-visible,.btn:focus-visible{outline:2px solid var(--orange);outline-offset:3px;border-radius:8px}",

    ".ghs-dock{position:fixed;bottom:24px;right:24px;z-index:9999;font-family:'Inter',system-ui,sans-serif}",
    ".dock-toggle{width:50px;height:50px;border-radius:12px;border:1px solid rgba(23,33,31,.16);background:#fbfaf6;color:#17211f;font-size:18px;cursor:pointer;box-shadow:0 1px 2px rgba(23,33,31,.08),0 12px 28px rgba(23,33,31,.10);transition:transform .18s ease,border-color .18s ease,background-color .18s ease;display:flex;align-items:center;justify-content:center;position:relative;z-index:2}",
    ".dock-toggle:hover{transform:translateY(-1px);border-color:rgba(37,79,92,.30);background:#eef3f2}",
    ".dock-icon-close{display:none}",
    ".ghs-dock.open .dock-icon-open{display:none}",
    ".ghs-dock.open .dock-icon-close{display:inline}",
    ".ghs-dock.open .dock-toggle{background:#254f5c;color:#ffffff;border-color:#254f5c}",
    ".dock-panel{position:absolute;bottom:62px;right:0;width:292px;max-height:75vh;overflow-y:auto;background:#fbfaf6;border:1px solid rgba(23,33,31,.13);border-radius:8px;padding:0;opacity:0;transform:translateY(8px);pointer-events:none;transition:opacity .22s ease,transform .22s ease;box-shadow:0 1px 2px rgba(23,33,31,.06),0 18px 42px rgba(23,33,31,.12)}",
    ".ghs-dock.open .dock-panel{opacity:1;transform:translateY(0) scale(1);pointer-events:auto}",
    ".dock-header{display:flex;align-items:center;gap:8px;padding:14px 16px 10px;border-bottom:1px solid rgba(23,33,31,.10)}",
    ".dock-brand{font-family:'Source Serif 4',serif;font-weight:800;font-size:15px;color:#17211f;text-decoration:none;letter-spacing:0}",
    ".dock-subtitle{font-size:11px;color:#5d6965;flex:1}",
    ".dock-links{padding:10px 12px}.dock-links .nav-group{display:block;margin-bottom:8px}.dock-links .nav-group-label{display:block;font-size:9px;font-weight:750;letter-spacing:.12em;text-transform:uppercase;color:rgba(23,33,31,.44);padding:3px 6px 4px}.dock-links .nav-group a{display:inline-block;color:#5d6965;font-size:11.5px;text-decoration:none;padding:4px 8px;border-radius:6px;margin:1px;transition:color .12s ease,background-color .12s ease;font-weight:550}.dock-links .nav-group a:hover{color:#17211f;background:#eef3f2}.dock-links .nav-group a.active{color:#17211f;background:#e4ece9;font-weight:760}",
    ".dock-links .nav-group:after{display:none}",
    ".dock-footer{display:flex;gap:6px;padding:10px 16px 12px;border-top:1px solid rgba(23,33,31,.10)}.dock-footer a{font-size:11px;color:#5d6965;text-decoration:none;padding:3px 8px;border:1px solid rgba(23,33,31,.13);border-radius:6px;transition:color .12s,border-color .12s,background-color .12s}.dock-footer a:hover{color:#17211f;border-color:rgba(37,79,92,.28);background:#eef3f2}",
    "@media(max-width:640px){.ghs-dock{bottom:16px;right:16px}.dock-panel{width:calc(100vw - 32px);right:-8px;bottom:60px}.hero-main{font-size:48px}}",
    ""
  ), collapse = "")
}

.ghs_css_material_final <- function() {
  paste(c(
    ":root{--final-paper:#f4e6cc;--final-surface:#fff9ed;--final-surface-2:#efe0c4;--final-ink:#251a10;--final-muted:#6f604d;--final-blue:#7b4c1f;--final-accent:#b96f24;--final-line:rgba(64,43,23,.14);--final-line-soft:rgba(64,43,23,.08);--final-shadow:0 1px 2px rgba(64,43,23,.045),0 12px 28px rgba(64,43,23,.075)}",
    "html{scroll-behavior:auto!important;scroll-padding-top:28px!important}html,body{background:var(--final-paper)!important;background-image:none!important;color:var(--final-ink)!important}",
    "[id]{scroll-margin-top:28px}.hero,.section,.finding,.site-footer{background:var(--final-paper)!important;background-image:none!important;border-top:0!important}",
    ".hero{min-height:100svh!important;min-height:100vh!important;align-items:center!important;padding:70px 0 96px!important;background:linear-gradient(180deg,#fff3d6 0%,#f5e4c5 72%,#f1dfbd 100%)!important}",
    ".hero:before,.hero:after,.kpi:before,.fig-frame:before,.section:before,.finding:before{content:none!important;display:none!important;background:none!important}",
    ".hero-eyebrow,.mobile-toc,.hero-panel,.kpi,.method-block,.fig-inline,.callout,.evidence-note,.section-note,.table-wrap,.figure-index,.asset-console,.gallery-card,.widget-card,.widget-frame-wrap,.cmd-card,.conc-card,.country-card,.sim-controls,.sim-output article,.glossary-sources,.session-meta li,.session-grid li,.ghs-deep-card,.deep-dive-grid .deep-card{background:var(--final-surface)!important;background-image:none!important;border:1px solid var(--final-line)!important;border-left:1px solid var(--final-line)!important;border-radius:8px!important;box-shadow:var(--final-shadow)!important;backdrop-filter:none!important;-webkit-backdrop-filter:none!important}",
    ".hero-eyebrow{color:var(--final-muted)!important;box-shadow:none!important;background:rgba(255,249,237,.72)!important;border-color:rgba(64,43,23,.12)!important}",
    ".hero-center{width:min(1120px,88vw)!important}.hero h1{letter-spacing:0!important;text-shadow:none!important;color:var(--final-ink)!important}",
    ".hero-reveal{opacity:1!important;transform:none!important;filter:none!important;animation:ghs-hero-rise .82s cubic-bezier(.16,1,.3,1) both!important}.hero-eyebrow{animation-delay:.04s!important}.hero-line1{animation-delay:.14s!important}.hero-line2{animation-delay:.24s!important}.hero-line3{animation-delay:.34s!important}.hero h1{animation:ghs-title-breathe 7s ease-in-out 1.2s infinite}",
    ".hero .hero-line3 em{color:var(--final-accent)!important}",
    ".section-head .kicker,.finding-kicker,.fig-kicker,.callout strong,.evidence-note strong,.section-note strong,.figure-index .kicker,.dock-links .nav-group-label,.mobile-toc-group-label{background:none!important;color:var(--final-blue)!important;-webkit-background-clip:initial!important;background-clip:initial!important}",
    ".section .lead,.finding-head .lead,.method-block,.callout p,.evidence-note p,.section-note p,.gallery-note,.widget-card p,.conc-card p{color:var(--final-muted)!important;letter-spacing:0!important}",
    ".fig-frame,.gallery-button,.table-wrap caption,.table-wrap th,.v3-card-head,.v3-card-footer{background:var(--final-surface-2)!important;background-image:none!important}",
    ".kpi-value,.chip i,.conc-card span,.asset-console-stats strong,.sim-output b{color:var(--final-blue)!important;background:none!important;text-shadow:none!important}",
    ".kpi:hover,.gallery-card:hover,.ghs-fig-card:hover,.chip:hover,.btn:hover{transform:translateY(-1px)!important;box-shadow:0 2px 4px rgba(64,43,23,.06),0 16px 34px rgba(64,43,23,.10)!important}",
    ".btn,.tabs button.active,.asset-console-chips button.active,.asset-console-chips button:hover{background:var(--final-blue)!important;background-image:none!important;border-color:var(--final-blue)!important;color:#fff!important;box-shadow:none!important;border-radius:8px!important}",
    ".btn.alt,.btn.ghost,.tabs button,.chip,.dock-footer a,.mobile-toc-items a{background:#fff!important;background-image:none!important;border:1px solid var(--final-line)!important;color:var(--final-muted)!important;box-shadow:none!important;border-radius:8px!important}",
    ".links .nav-group:after,.dock-links .nav-group:after{display:none!important}",
    ".links a,.dock-links .nav-group a{border-radius:6px!important;color:var(--final-muted)!important}",
    ".links a:hover,.links a.active,.dock-links .nav-group a:hover,.dock-links .nav-group a.active,.mobile-toc-items a:hover,.mobile-toc-items a.active{background:var(--final-surface-2)!important;color:var(--final-ink)!important}",
    ".ghs-dock{background:transparent!important;background-image:none!important;border:0!important;box-shadow:none!important;backdrop-filter:none!important;-webkit-backdrop-filter:none!important;pointer-events:none!important}",
    ".dock-toggle{pointer-events:auto!important;width:52px!important;height:52px!important;border-radius:14px!important;background:var(--final-surface)!important;background-image:none!important;border:1px solid var(--final-line)!important;color:var(--final-ink)!important;box-shadow:0 2px 5px rgba(64,43,23,.08),0 18px 36px rgba(64,43,23,.13)!important;transition:transform .22s cubic-bezier(.2,.8,.2,1),background-color .22s ease,border-color .22s ease,box-shadow .22s ease!important;overflow:hidden!important}",
    ".dock-toggle:hover{transform:translateY(-2px)!important;background:#fff3df!important;border-color:rgba(185,111,36,.35)!important;box-shadow:0 4px 10px rgba(64,43,23,.09),0 22px 42px rgba(64,43,23,.16)!important}",
    ".dock-panel{pointer-events:none!important;background:var(--final-surface)!important;background-image:none!important;border:1px solid var(--final-line)!important;border-radius:14px!important;box-shadow:0 2px 6px rgba(64,43,23,.08),0 24px 56px rgba(64,43,23,.16)!important;overflow:auto!important;transform-origin:bottom right!important;transform:translate3d(0,12px,0) scale(.96)!important;transition:opacity .24s ease,transform .32s cubic-bezier(.16,1,.3,1)!important;clip-path:inset(0 round 14px)!important}",
    ".ghs-dock.open .dock-panel{pointer-events:auto!important;opacity:1!important;transform:translate3d(0,0,0) scale(1)!important}",
    ".ghs-dock.open .dock-toggle{background:var(--final-blue)!important;color:#fff!important;border-color:var(--final-blue)!important;transform:translateY(-2px)!important}",
    "#read-progress{background:var(--final-blue)!important;background-image:none!important}",
    "pre,.code-pre{background:#251a10!important;background-image:none!important;color:#fff9ed!important;border-radius:8px!important}",
    ".mobile-toc{border-radius:8px!important}",
    "@keyframes ghs-hero-rise{from{opacity:.01;transform:translateY(22px);filter:blur(3px)}to{opacity:1;transform:translateY(0);filter:blur(0)}}",
    "@keyframes ghs-title-breathe{0%,100%{transform:translateY(0)}50%{transform:translateY(-6px)}}",
    "@media(prefers-reduced-motion:reduce){.hero h1,.hero-reveal{animation:none!important}}",
    "@media(max-width:760px){.hero{min-height:100svh!important;min-height:100vh!important;padding:54px 0 84px!important}.mobile-toc{display:block!important}.hero h1{font-size:clamp(38px,12vw,58px)!important}.figure-index-row{grid-template-columns:34px 1fr!important}.asset-row{grid-template-columns:46px 1fr!important}.figure-index-row em,.figure-index-row small,.asset-row em,.asset-row small{display:none!important}}",
    ""
  ), collapse = "")
}


.ghs_v3_section_head <- function(kicker, title, lead = NULL,
                                 align = "left") {
  lead_html <- if (!is.null(lead) && nzchar(lead))
    sprintf("<p class='ghs-section-lead'>%s</p>", .ghs_e(lead))
  else ""
  sprintf(
    paste0("<header class='ghs-section-head' data-align='%s'>",
           "<span class='ghs-kicker'>%s</span>",
           "<h2 class='ghs-section-title'>%s</h2>%s</header>"),
    .ghs_e(align), .ghs_e(kicker), .ghs_e(title), lead_html)
}

.ghs_v3_section <- function(id, kicker, title, body,
                            lead = NULL, extra_class = "") {
  sprintf(
    paste0("<section class='section ghs-v3 %s' id='%s'>",
           "<div class='wrap'>%s%s</div></section>"),
    .ghs_e(extra_class), .ghs_e(id),
    .ghs_v3_section_head(kicker, title, lead),
    body)
}

.ghs_v3_kpi <- function(value, label, hint = NULL, trend = NULL,
                        tone = "primary") {
  tone_color <- switch(tone,
    primary = "var(--g3-primary)", secondary = "var(--g3-secondary)",
    good = "var(--g3-good)", warn = "var(--g3-warn)", bad = "var(--g3-bad)",
    "var(--g3-neutral)")
  trend_html <- ""
  if (!is.null(trend) && is.finite(trend)) {
    arr <- if (trend > 0) "\u2197" else if (trend < 0) "\u2198" else "\u2192"
    col <- if (trend > 0) "var(--g3-good)"
           else if (trend < 0) "var(--g3-bad)"
           else "var(--g3-neutral)"
    trend_html <- sprintf(
      "<span class='ghs-kpi-trend' style='color:%s'>%s %+0.1f%%</span>",
      col, arr, trend * 100)
  }
  hint_html <- if (!is.null(hint) && nzchar(hint))
    sprintf("<div class='ghs-kpi-hint'>%s</div>", .ghs_e(hint))
  else ""
  sprintf(
    paste0("<div class='ghs-kpi' style='--tone:%s'>",
           "<div class='ghs-kpi-value'>%s</div>",
           "<div class='ghs-kpi-label'>%s%s</div>%s</div>"),
    tone_color, .ghs_e(value), .ghs_e(label), trend_html, hint_html)
}

.ghs_v3_kpi_grid <- function(cards) {
  sprintf("<div class='ghs-kpi-grid ghs-v3'>%s</div>", paste(cards, collapse = ""))
}

.ghs_v3_stat_strip <- function(items) {
  if (!length(items)) return("")
  cards <- vapply(seq_along(items), function(i) {
    it <- items[[i]]
    sprintf(
      paste0("<div class='ghs-stat-cell'>",
             "<div class='ghs-stat-value'>%s</div>",
             "<div class='ghs-stat-label'>%s</div></div>"),
      .ghs_e(it$value %||% "\u2014"),
      .ghs_e(it$label %||% ""))
  }, character(1))
  sprintf("<div class='ghs-stat-strip'>%s</div>", paste(cards, collapse = ""))
}

.ghs_v3_callout <- function(text, tone = "info", title = NULL) {
  title_html <- if (!is.null(title) && nzchar(title))
    sprintf("<strong class='ghs-callout-title'>%s</strong>",
            .ghs_e(title))
  else ""
  sprintf(
    paste0("<aside class='ghs-callout ghs-callout-%s'>",
           "%s<div class='ghs-callout-body'>%s</div></aside>"),
    .ghs_e(tone), title_html, text)
}

.ghs_v3_deep_dive <- function(data = "", method = "", assume = "",
                              limit = "", sens = "", policy = "") {
  card <- function(tone, label, body) {
    sprintf(
      paste0("<div class='ghs-deep-card tone-%s'>",
             "<h4>%s</h4><p>%s</p></div>"),
      tone, .ghs_e(label), body)
  }
  cards <- paste(
    card("data",   "\u6570\u636e",   data),
    card("method", "\u65b9\u6cd5",   method),
    card("assume", "\u5047\u8bbe",   assume),
    card("limit",  "\u5c40\u9650",   limit),
    card("sens",   "\u654f\u611f\u6027", sens),
    card("policy", "\u653f\u7b56\u542b\u4e49", policy),
    sep = "")
  sprintf("<div class='ghs-deep-grid'>%s</div>", cards)
}

.ghs_v3_fig_card <- function(path, caption, mode = "submission") {
  if (!file.exists(path))
    return(sprintf("<div class='ghs-fig-card'><p class='muted'>missing: %s</p></div>",
                   .ghs_e(basename(path))))
  src <- if (mode == "submission") .ghs_b64(path) else basename(path)
  sprintf(
    paste0("<figure class='ghs-fig-card'>",
           "<img src='%s' alt='%s' loading='lazy'>",
           "<figcaption>%s</figcaption></figure>"),
    src, .ghs_e(caption), .ghs_e(caption))
}

.ghs_v3_fig_grid <- function(figs, ncol = 2) {
  cls <- paste0("ghs-fig-grid ghs-fig-grid-", ncol)
  sprintf("<div class='%s'>%s</div>", cls, paste(figs, collapse = ""))
}

.ghs_v3_chips <- function(chips) {
  if (!length(chips)) return("")
  pills <- vapply(seq_along(chips), function(i) {
    c <- chips[[i]]
    tone <- c$tone %||% "primary"
    sprintf("<span class='ghs-chip' data-tone='%s'><b>%s</b><i>%s</i></span>",
            .ghs_e(tone), .ghs_e(c$label), .ghs_e(c$value))
  }, character(1))
  sprintf("<div class='ghs-chip-row'>%s</div>", paste(pills, collapse = ""))
}

.ghs_v3_finding <- function(id, num, kicker, title, lead,
                             chips_html = "", figs_html = "",
                             widgets_html = "", deep_html = "",
                             policy_html = "") {
  sprintf(
    paste0("<section class='ghs-finding ghs-v3' id='%s'><div class='wrap'>",
           "<header class='ghs-finding-head'>",
           "<span class='ghs-finding-num'>%s</span>",
           "<div class='ghs-finding-meta'>",
           "<span class='ghs-kicker'>%s</span>",
           "<h2 class='ghs-finding-title'>%s</h2>",
           "<p class='ghs-finding-lead'>%s</p>%s</div></header>",
           "%s%s%s%s</div></section>"),
    .ghs_e(id), .ghs_e(num), .ghs_e(kicker),
    .ghs_e(title), .ghs_e(lead),
    chips_html, figs_html, widgets_html, deep_html, policy_html)
}

.ghs_v3_reading_paths <- function(paths) {
  cards <- vapply(seq_along(paths), function(i) {
    p <- paths[[i]]
    items <- paste(vapply(p$items, function(it) {
      sprintf("<li><a href='#%s'>%s</a></li>",
              .ghs_e(it$anchor), .ghs_e(it$label))
    }, character(1)), collapse = "")
    sprintf(
      paste0("<article class='ghs-path-card' data-tone='%s'>",
             "<div class='ghs-path-icon'>%s</div>",
             "<h3 class='ghs-path-title'>%s</h3>",
             "<p class='ghs-path-lead'>%s</p>",
             "<ul class='ghs-path-list'>%s</ul></article>"),
      .ghs_e(p$tone %||% "primary"),
      .ghs_e(p$icon %||% "\u25b8"),
      .ghs_e(p$title), .ghs_e(p$lead), items)
  }, character(1))
  sprintf("<div class='ghs-paths'>%s</div>", paste(cards, collapse = ""))
}

.ghs_v3_lineage <- function(steps) {
  cards <- vapply(seq_along(steps), function(i) {
    s <- steps[[i]]
    sprintf(
      paste0("<div class='ghs-lineage-step'>",
             "<div class='ghs-lineage-stage'>%s</div>",
             "<div class='ghs-lineage-name'>%s</div>",
             "<div class='ghs-lineage-meta'>%s</div></div>"),
      .ghs_e(s$stage), .ghs_e(s$name), .ghs_e(s$meta %||% ""))
  }, character(1))
  sprintf("<div class='ghs-lineage'>%s</div>", paste(cards, collapse = ""))
}

.ghs_v3_sources <- function(sources) {
  cards <- vapply(seq_along(sources), function(i) {
    s <- sources[[i]]
    sprintf(
      paste0("<div class='ghs-source-card'>",
             "<b>%s</b><span>%s</span></div>"),
      .ghs_e(s$name), .ghs_e(s$desc %||% ""))
  }, character(1))
  sprintf("<div class='ghs-sources'>%s</div>", paste(cards, collapse = ""))
}

.ghs_v3_sens_grid <- function(items) {
  cards <- vapply(seq_along(items), function(i) {
    it <- items[[i]]
    sprintf(
      paste0("<div class='ghs-sens-card'>",
             "<div class='ghs-sens-title'>%s</div>",
             "<div class='ghs-sens-value'>%s</div>",
             "<div class='ghs-sens-note'>%s</div></div>"),
      .ghs_e(it$title), .ghs_e(it$value),
      .ghs_e(it$note %||% ""))
  }, character(1))
  sprintf("<div class='ghs-sens-grid'>%s</div>", paste(cards, collapse = ""))
}

.ghs_v3_bullets <- function(bullets) {
  items <- vapply(seq_along(bullets), function(i) {
    b <- bullets[[i]]
    title_html <- if (!is.null(b$title) && nzchar(b$title))
      sprintf("<b>%s</b>", .ghs_e(b$title))
    else ""
    sprintf("<li data-tone='%s'>%s%s</li>",
            .ghs_e(b$tone %||% "primary"),
            title_html,
            b$body %||% "")
  }, character(1))
  sprintf("<ul class='ghs-bullet-list'>%s</ul>", paste(items, collapse = ""))
}


.ghs_render <- function(master, fig_dir, widget_dir, programs_dir, models_dir,
                        mode, repo_url, project_url) {
  s <- .ghs_summary(master)
  pngs <- list.files(fig_dir, pattern = "[.]png$", full.names = TRUE)
  htmls <- list.files(widget_dir, pattern = "[.]html$", full.names = TRUE)
  body <- paste0(
    .ghs_hero(s, project_url, length(pngs), length(htmls)),
    "<main>",
    .ghs_executive(s),
    .ghs_methods_section(programs_dir),
    .ghs_data_quality(models_dir),
    .ghs_codebook(),
    .ghs_findings(master, fig_dir, programs_dir, models_dir,
                   widget_dir, mode = mode, repo_url = repo_url),
    .ghs_inject_all_assets(fig_dir, widget_dir, mode, repo_url),
    .ghs_country_profiles(master, fig_dir),
    .ghs_regional(master, fig_dir),
    .ghs_period_compare(master, fig_dir),
    .ghs_sdg3_section(master, fig_dir),
    .ghs_lifeexp_section(master, fig_dir),
    .ghs_inequality_atlas(fig_dir),
    .ghs_cluster_detail(master, fig_dir),
    .ghs_extreme_cases(master, fig_dir),
    .ghs_simulator(s),
    .ghs_robustness(master, models_dir),
    "",


    if (exists("ghs_sections_extra", mode = "function")) {
      ghs_sections_extra(master, fig_dir, models_dir, widget_dir,
                          mode = mode, repo_url = repo_url)
    } else "",
    .ghs_gallery(fig_dir, widget_dir, mode, repo_url),
    .ghs_widgets(widget_dir, mode, repo_url),
    .ghs_glossary(),
    .ghs_repro(repo_url),
    .ghs_session_info(),
    .ghs_conclusion(s),
    "</main>",
    .ghs_footer(),
    "<div class='modal' id='fig-modal' onclick='closeFigure()'><button type='button'>\u5173\u95ed</button><div class='modal-title' id='modal-title'></div><img id='modal-img' alt='figure preview'></div>"
  )
  sprintf(
    "<!doctype html><html lang='zh-CN' data-theme='light'><head><meta charset='utf-8'><meta name='viewport' content='width=device-width,initial-scale=1'><title>\u5168\u7403\u536b\u751f\u652f\u51fa 2000\u20132023 \u00b7 \u5e84\u9882 20241334 \u00b7 \u6574\u5408\u5206\u6790\u62a5\u544a</title><meta name='description' content='Global Health Spending 2000\u20132023: 195 countries, 36 analytical findings, static figures, standalone widgets, Shiny dashboard, reproducibility notes and quality-gate evidence.'><link rel='icon' href='data:,'><style>%s%s%s%s</style></head><body>%s<script>%s</script></body></html>",
    .ghs_css(), .ghs_css_v3(), .ghs_css_polish(),
    .ghs_css_material_final(), body, .ghs_js()
  )
}

.ghs_write_rmd <- function(path) {
  html_path <- sub("[.]Rmd$", ".html", path)
  if (!file.exists(html_path)) {
    stop("Missing HTML for Rmd synchronization: ", html_path)
  }
  html <- readChar(html_path, nchars = file.info(html_path)[["size"]], useBytes = TRUE)
  extract_part <- function(pattern, text, label) {
    m <- regexec(pattern, text, perl = TRUE)
    hit <- regmatches(text, m)[[1]]
    if (length(hit) < 2L) stop("Cannot extract ", label, " from generated HTML")
    hit[2]
  }
  style <- extract_part("(?is)<style>(.*?)</style>", html, "style")
  body <- extract_part("(?is)<body>(.*)<script>.*</script></body></html>\\s*$", html, "body")
  script <- extract_part("(?is)<script>(.*)</script></body></html>\\s*$", html, "script")
  body <- gsub("><", ">\n<", body, fixed = TRUE)
  style <- gsub("}", "}\n", style, fixed = TRUE)
  script <- gsub(";", ";\n", script, fixed = TRUE)
  lines <- c(
    "---",
    "title: \"全球卫生支出 2000–2023：整合分析与核心发现\"",
    "subtitle: \"庄颂（20241334）· R 大作业 · GHED + WDI\"",
    "author: \"庄颂 20241334\"",
    "date: \"`r format(Sys.Date(), '%Y-%m-%d')`\"",
    "output:",
    "  html_document:",
    "    self_contained: false",
    "    toc: false",
    "    theme: null",
    "---",
    "",
    "<style>",
    strsplit(style, "\n", fixed = TRUE)[[1]],
    "</style>",
    "",
    strsplit(body, "\n", fixed = TRUE)[[1]],
    "",
    "<script>",
    strsplit(script, "\n", fixed = TRUE)[[1]],
    "</script>"
  )
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, path, useBytes = TRUE)
  invisible(path)
}

generate_static_showcase <- function(root = NULL,
                                     repo_url = "https://github.com/2711944586/R",
                                     project_url = "https://github.com/2711944586/R",
                                     write_rmd = TRUE) {
  if (is.null(root)) {
    root <- if (exists("proj_root", mode = "function")) proj_root() else getwd()
  }
  root <- normalizePath(root, mustWork = FALSE)
  master_path <- file.path(root, "\u6d3e\u751f\u6570\u636e", "\u5904\u7406\u7ed3\u679c", "master_enriched.rds")
  if (!file.exists(master_path))
    stop("Missing master cache: ", master_path,
         "\nRun: Rscript \u6784\u5efa.R data")
  master <- readRDS(master_path)
  fig_dir <- file.path(root, "\u5206\u6790\u8f93\u51fa", "\u56fe\u8868")
  widget_dir <- file.path(root, "\u5206\u6790\u8f93\u51fa", "\u4ea4\u4e92\u7ec4\u4ef6")
  programs_dir <- file.path(root, "\u7a0b\u5e8f")
  models_dir <- file.path(root, "\u5206\u6790\u8f93\u51fa", "\u6a21\u578b\u8868")
  if (!length(list.files(fig_dir, pattern = "[.]png$")))
    stop("No PNG figures at: ", fig_dir, "\nRun: Rscript \u6784\u5efa.R figures")
  if (!length(list.files(widget_dir, pattern = "[.]html$")))
    stop("No widgets at: ", widget_dir, "\nRun: Rscript \u6784\u5efa.R widgets")

  out_submission <- file.path(root, "\u8bfe\u7a0b\u63d0\u4ea4", "\u5e84\u9882_20241334.html")
  out_publish <- file.path(root, "\u7f51\u7ad9\u53d1\u5e03", "index.html")
  dir.create(dirname(out_submission), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(out_publish), recursive = TRUE, showWarnings = FALSE)

  options(ghs_render_mode = "submission")
  html_submission <- .ghs_render(master, fig_dir, widget_dir, programs_dir,
                                  models_dir, mode = "submission",
                                  repo_url = repo_url, project_url = project_url)

  options(ghs_render_mode = "publish")
  pub_fig_dir <- file.path(dirname(out_publish), "\u56fe\u8868")
  dir.create(pub_fig_dir, recursive = TRUE, showWarnings = FALSE)
  pngs_src <- list.files(fig_dir, pattern = "[.]png$", full.names = TRUE)
  file.copy(pngs_src, file.path(pub_fig_dir, basename(pngs_src)),
            overwrite = TRUE, copy.date = TRUE)
  html_publish <- .ghs_render(master, fig_dir, widget_dir, programs_dir,
                                models_dir, mode = "publish",
                                repo_url = repo_url, project_url = project_url)
  options(ghs_render_mode = NULL)

  writeLines(html_submission, out_submission, useBytes = TRUE)
  writeLines(html_publish,    out_publish,    useBytes = TRUE)
  if (isTRUE(write_rmd)) {
    .ghs_write_rmd(file.path(root, "\u8bfe\u7a0b\u63d0\u4ea4", "\u5e84\u9882_20241334.Rmd"))
  }
  cat("[showcase] \u5df2\u751f\u6210\u552f\u4e00\u4e24\u4efd HTML\uff1a\n")
  cat("  - ", out_submission, sprintf(" (%.1f MB)\n",
        file.info(out_submission)$size / 1024^2), sep = "")
  cat("  - ", out_publish, sprintf(" (%.1f MB)\n",
        file.info(out_publish)$size / 1024^2), sep = "")
  invisible(c(out_submission, out_publish))
}
