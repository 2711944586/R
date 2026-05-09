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

  /* v2 hero（总览页大标题） */
  .v2-hero {
    background: linear-gradient(135deg, #1B5E88 0%, #224a6f 50%, #2c3e50 100%);
    color: #fff;
    border-radius: 16px;
    padding: 40px 44px;
    margin: -8px 0 22px;
    box-shadow: 0 18px 44px rgba(13,18,27,0.16);
    position: relative;
    overflow: hidden;
  }
  .v2-hero::after {
    content: '';
    position: absolute;
    inset: 0;
    background:
      radial-gradient(circle at 78% 18%, rgba(224,123,0,0.18), transparent 38%),
      radial-gradient(circle at 14% 82%, rgba(255,255,255,0.06), transparent 42%);
    pointer-events: none;
  }
  .v2-hero-inner { position: relative; z-index: 1; max-width: 980px; }
  .v2-hero-kicker {
    display: inline-block; padding: 5px 14px; border-radius: 999px;
    background: rgba(255,255,255,0.14); color: #ffd9a8;
    font-size: 11px; font-weight: 800; letter-spacing: 0.16em;
    text-transform: uppercase;
  }
  .v2-hero-title {
    color: #fff !important;
    font-size: 34px;
    line-height: 1.2;
    margin: 16px 0 10px;
    font-family: 'Source Serif 4', Georgia, serif;
    font-weight: 700;
  }
  .v2-hero-lead {
    color: rgba(255,255,255,0.86);
    font-size: 16px;
    line-height: 1.7;
    margin: 0 0 18px;
    max-width: 780px;
  }
  .v2-hero-meta {
    display: flex; flex-wrap: wrap; gap: 8px 14px;
    color: rgba(255,255,255,0.72); font-size: 13px;
  }
  .v2-hero-meta a { color: #ffd9a8; text-decoration: underline; }
  .v2-hero-meta a:hover { color: #fff; }

  /* v2 KPI grid */
  .v2-kpi-grid {
    display: grid; grid-template-columns: repeat(6, 1fr); gap: 12px;
    margin-bottom: 20px;
  }
  @media (max-width: 1100px) {
    .v2-kpi-grid { grid-template-columns: repeat(3, 1fr); }
  }
  @media (max-width: 600px) {
    .v2-kpi-grid { grid-template-columns: repeat(2, 1fr); }
  }

  /* card-note：每个图卡上方的引导句 */
  .card-note {
    color: #5A5A65; font-size: 13px; line-height: 1.6;
    margin: -4px 0 12px; padding: 0;
  }

  /* v2 section head */
  .v2-section-head {
    margin: 28px 0 14px;
  }
  .v2-section-head h3 {
    font-family: 'Source Serif 4', serif;
    font-size: 22px; margin: 0 0 6px; color: #1A1A1F;
  }
  .v2-section-lead {
    color: #5A5A65; font-size: 14px; line-height: 1.7; margin: 0;
    max-width: 760px;
  }

  /* 模块卡片导航 */
  .module-grid {
    display: grid; grid-template-columns: repeat(4, 1fr); gap: 14px;
    margin-bottom: 20px;
  }
  @media (max-width: 1100px) {
    .module-grid { grid-template-columns: repeat(3, 1fr); }
  }
  @media (max-width: 720px) {
    .module-grid { grid-template-columns: repeat(2, 1fr); }
  }
  .module-card {
    text-align: left;
    background: #FFFFFF;
    border: 1px solid #1A1A1F12;
    border-radius: 12px;
    padding: 16px 18px;
    cursor: pointer;
    transition: transform .15s ease, box-shadow .15s ease, border-color .15s ease;
    display: flex; flex-direction: column; gap: 6px;
    min-height: 118px;
  }
  .module-card:hover {
    transform: translateY(-3px);
    box-shadow: 0 12px 30px rgba(13,18,27,0.10);
    border-color: #1B5E88;
  }
  .module-card .module-kicker {
    color: #E07B00; font-size: 11px; font-weight: 800;
    letter-spacing: 0.12em; text-transform: uppercase;
  }
  .module-card .module-title {
    color: #1A1A1F; font-family: 'Source Serif 4', serif;
    font-size: 17px; font-weight: 600;
  }
  .module-card .module-desc {
    color: #5A5A65; font-size: 12.5px; line-height: 1.55;
  }
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
