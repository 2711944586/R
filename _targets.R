# =============================================================================
# _targets.R  ---  可复现数据流水线（{targets} 包驱动）
# -----------------------------------------------------------------------------
# 用法:
#   library(targets)
#   tar_make()            # 全量构建
#   tar_make(p_oops_box)  # 增量构建到指定目标
#   tar_visnetwork()      # 可视化依赖图
#   tar_load(master_enriched)  # 加载缓存的目标
# =============================================================================

if (!requireNamespace("targets", quietly = TRUE)) {
  stop("Run install.packages('targets') first.")
}

library(targets)

# 加载所有 程序/ 函数库
for (f in list.files("程序", pattern = "\\.R$", full.names = TRUE)) {
  source(f, encoding = "UTF-8")
}

# 流水线选项
tar_option_set(
  packages = c(
    "dplyr", "tidyr", "tibble", "readr", "stringr", "forcats", "purrr",
    "ggplot2", "scales", "rlang"
  ),
  format    = "rds",
  memory    = "transient",
  garbage_collection = TRUE
)

# 流水线声明
list(
  # ---------- 1. 原始数据加载 ----------
  tar_target(name = ghs_raw,
             command = load_ghs(assign_globals = FALSE)),

  tar_target(name = data_summary,
             command = summarize_ghs(ghs_raw)),

  tar_target(name = scheme_check,
             command = check_scheme_sum(ghs_raw$financing_schemes)),

  # ---------- 2. 宽表 + 增强 ----------
  tar_target(name = master_wide,
             command = build_master_wide(ghs_raw)),

  tar_target(name = master_enriched,
             command = enrich_master(master_wide, with_wdi = TRUE)),

  # ---------- 3. 衍生指标 ----------
  tar_target(name = inequality_panel,
             command = inequality_by_year(master_enriched,
                                           value_col = "che_pc_usd2023",
                                           weight_col = "pop")),

  tar_target(name = covid_shock_tbl,
             command = covid_shock(master_enriched,
                                    base_year = 2019,
                                    shock_years = 2020:2022)),

  # ---------- 4. 模型 ----------
  tar_target(name = pca_2022,
             command = fit_pca(
               master_enriched |> dplyr::filter(year == 2022) |>
                 tidyr::drop_na(gghed_che, pvtd_che, ext_che, hf3_che),
               vars = c("gghed_che", "pvtd_che", "ext_che", "hf3_che")
             )),

  tar_target(name = clusters_2022,
             command = fit_cluster(pca_2022, k = 4)),

  tar_target(name = beta_conv,
             command = fit_beta_convergence(master_enriched,
                                             start_year = 2005,
                                             end_year   = 2022)),

  tar_target(name = panel_fe,
             command = fit_panel_fe(master_enriched)),

  # ---------- 5. 静态图 ----------
  tar_target(name = fig_sources,
             command = save_fig(plot_source_area(master_enriched),
                                "01_global_sources_area",
                                width = 10, height = 6),
             format = "file"),

  tar_target(name = fig_oops_rank,
             command = save_fig(
               plot_oops_ranking(master_enriched, year_focus = 2023, top_n = 15),
               "02_oops_ranking_2023",
               width = 12, height = 7
             ),
             format = "file"),

  tar_target(name = fig_oops_box,
             command = save_fig(
               plot_oops_box_continent(master_enriched, year_focus = 2023),
               "04_oops_box_continent",
               width = 10, height = 6
             ),
             format = "file"),

  tar_target(name = fig_profile_china,
             command = save_fig(
               plot_country_profile(master_enriched, iso = "CHN"),
               "06_profile_china", width = 13, height = 8
             ),
             format = "file"),

  # ---------- 6. 输出快照 CSV ----------
  tar_target(name = master_csv,
             command = {
               p <- file.path("派生数据", "处理结果", "master_enriched.csv")
               dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
               readr::write_csv(master_enriched, p)
               p
             },
             format = "file")
)
