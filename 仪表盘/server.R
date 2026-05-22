# Shiny 服务端调度。

function(input, output, session) {

  master_r <- shiny::reactive({
    master_enriched
  })

  mod_home_server("home", master_r, parent_session = session)
  mod_overview_server("overview", master_r, world_sf_obj, year_max,
                      parent_session = session)
  mod_about_server("about")
  mod_methods_server("methods")

  # 国家与区域
  mod_country_server("country", master_r)
  mod_regional_server("regional", master_r)
  mod_ranking_server("ranking", master_r)
  mod_benchmark_server("benchmark", master_r)

  # 筹资结构
  mod_financing_server("financing", master_r)
  mod_spending_server("spending", master_r)
  mod_purpose_server("purpose", master_r)
  mod_aid_server("aid", master_r)
  mod_fiscal_server("fiscal", master_r)

  # 公平与效率
  mod_equity_server("equity", master_r, year_max)
  mod_inequality_server("inequality", master_r)
  mod_efficiency_server("efficiency", master_r)
  mod_convergence_server("convergence", master_r)
  mod_decomposition_server("decomposition", master_r)

  # 健康产出
  mod_outcomes_server("outcomes", master_r, year_max)
  mod_sdg_server("sdg", master_r)
  mod_prevention_server("prevention", master_r)
  mod_aging_server("aging", master_r)

  # 冲击与变化
  mod_pandemic_server("pandemic", master_r)
  mod_growth_server("growth", master_r)
  mod_transition_server("transition", master_r)
  mod_timeline_server("timeline", master_r)
  mod_extremes_server("extremes", master_r)

  # 分析工具
  mod_compare_server("compare", master_r, year_max)
  mod_cluster_server("cluster", master_r)
  mod_forecast_server("forecast", master_r)
  mod_scenarios_server("scenarios", master_r)
  mod_mapstudio_server("mapstudio", master_r, world_sf_obj, year_min, year_max)
  mod_widgets_server("widgets", master_r, world_sf_obj)
  mod_correlation_server("correlation", master_r)
  mod_distribution_server("distribution", master_r)

  # 质量与稳健
  mod_robustness_server("robustness", master_r)
  mod_dataquality_server("dataquality", master_r)
  mod_policy_server("policy", master_r)
  mod_atlas_server("atlas", master_r)
}
