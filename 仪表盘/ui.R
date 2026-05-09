# =============================================================================
# 仪表盘/ui.R  ---  Shiny v2 仪表盘 UI · bslib::page_navbar + 12 模块化 nav_panel
# =============================================================================

# v2 视觉补丁（在 bslib 主题之上的最终覆盖）
v2_css <- htmltools::tags$style(htmltools::HTML("
  /* 整体节奏 */
  body { background: #FAF7F2; color: #1A1A1F;
         font-family: 'Inter', system-ui, -apple-system, sans-serif; }
  h1, h2, h3, h4 { font-family: 'Source Serif 4', Georgia, serif;
                    color: #1A1A1F; }
  /* 卡片 */
  .panel-content {
    background: #FFFFFF;
    padding: 18px 20px;
    border-radius: 10px;
    border: 1px solid #1A1A1F12;
    margin-bottom: 18px;
  }
  .panel-content h4 {
    margin: 0 0 12px 0;
    font-size: 1.05rem;
    font-weight: 600;
    color: #1A1A1F;
  }
  .panel-hero {
    margin-bottom: 18px;
  }
  .panel-hero h2 {
    font-size: 1.7rem; font-weight: 700; margin: 0 0 6px 0;
  }
  /* KPI 卡片（6 色变体） */
  .kpi-card {
    background: #FFFFFF;
    padding: 16px 18px;
    border-radius: 10px;
    border: 1px solid #1A1A1F12;
    margin-bottom: 12px;
    transition: transform .15s ease, box-shadow .15s ease;
  }
  .kpi-card:hover {
    transform: translateY(-2px);
    box-shadow: 0 4px 12px rgba(26,26,31,0.07);
  }
  .kpi-label { color: #5A5A65; font-size: 12px; letter-spacing: 0.05em;
                text-transform: uppercase; font-weight: 600; }
  .kpi-value { font-size: 24px; font-weight: 700; color: #1B5E88;
                margin-top: 4px; font-family: 'Source Serif 4', serif; }
  .kpi-sublabel { color: #5A5A65; font-size: 12px; margin-top: 2px; }
  .kpi-primary .kpi-value { color: #1B5E88; }
  .kpi-success .kpi-value { color: #6B8E5A; }
  .kpi-danger  .kpi-value { color: #C46B27; }
  .kpi-warning .kpi-value { color: #E07B00; }
  .kpi-muted   .kpi-value { color: #5A5A65; }
  /* navbar */
  .navbar { border-bottom: 1px solid #1A1A1F12;
            background: #FAF7F2 !important; }
  .navbar-brand { font-family: 'Source Serif 4', serif !important;
                   font-weight: 700 !important; color: #1A1A1F !important; }
  .nav-link.active { font-weight: 600; color: #1B5E88 !important; }
  /* 滚动条优化 */
  ::-webkit-scrollbar { width: 8px; height: 8px; }
  ::-webkit-scrollbar-thumb { background: #1A1A1F33; border-radius: 4px; }
  ::-webkit-scrollbar-thumb:hover { background: #1A1A1F66; }
"))

bslib::page_navbar(
  title = htmltools::tagList(
    htmltools::HTML("&#127973; "),
    "Global Health Spending · v2"
  ),
  id          = "main_nav",
  theme       = ghs_theme,
  window_title = "GHED v2 \u00b7 Global Health Spending Dashboard",
  fillable    = FALSE,
  navbar_options = bslib::navbar_options(bg = "#FAF7F2", theme = "light"),
  header      = htmltools::tags$head(v2_css),

  # 12 模块化 tab
  mod_overview_ui("overview"),
  mod_country_ui("country", country_choices_named, year_min, year_max),
  mod_equity_ui("equity", year_min, year_max),
  mod_efficiency_ui("efficiency", year_min, year_max),
  mod_pandemic_ui("pandemic"),
  mod_outcomes_ui("outcomes"),
  mod_compare_ui("compare", country_choices_named, indicator_choices),
  mod_cluster_ui("cluster", year_min, year_max),
  mod_forecast_ui("forecast", country_choices_named, indicator_choices),
  mod_scenarios_ui("scenarios", country_choices_named),
  mod_atlas_ui("atlas"),
  mod_about_ui("about"),

  # 页脚
  footer = htmltools::div(
    style = "color: #5A5A65; font-size: 12px; padding: 12px 16px;
              border-top: 1px solid #1A1A1F12; margin-top: 24px;",
    htmltools::HTML(paste0(
      "&copy; 2026 \u00b7 \u5e84\u9882 (20241334) \u00b7 ",
      "\u6570\u636e: WHO GHED 2024-12 \u00b7 ",
      "\u6e90\u7801: ",
      "<a href='https://github.com/2711944586/R' target='_blank'>",
      "github.com/2711944586/R</a>"))
  )
)
