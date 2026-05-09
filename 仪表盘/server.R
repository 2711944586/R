# =============================================================================
# 仪表盘/server.R  ---  Shiny v2 仪表盘服务端 · 12 模块 callModule 链
# =============================================================================

function(input, output, session) {

  # ---- 共享 reactive：master_r 包装为函数（便于 module 共享） -----------
  master_r <- shiny::reactive({
    master_enriched
  })

  # ---- 12 模块 server -----------------------------------------------------
  mod_overview_server("overview", master_r, world_sf_obj, year_max,
                      parent_session = session)
  mod_country_server("country", master_r)
  mod_equity_server("equity", master_r, year_max)
  mod_efficiency_server("efficiency", master_r)
  mod_pandemic_server("pandemic", master_r)
  mod_outcomes_server("outcomes", master_r, year_max)
  mod_compare_server("compare", master_r, year_max)
  mod_cluster_server("cluster", master_r)
  mod_forecast_server("forecast", master_r)
  mod_scenarios_server("scenarios", master_r)
  mod_atlas_server("atlas", master_r)
  mod_about_server("about")
}
