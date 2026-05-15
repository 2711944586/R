# =============================================================================
# 仪表盘/ui.R
# Shiny 仪表盘 UI · bslib::page_navbar + 36 模块化 nav_panel
# =============================================================================

# 前端样式系统
v2_css <- htmltools::tags$style(htmltools::HTML("
  /* ---- 基础排版 ---- */
  body {
    background: #fbf6ee; color: #0d121b;
    font-family: 'Inter', system-ui, -apple-system, sans-serif;
    -webkit-font-smoothing: antialiased;
    line-height: 1.55;
  }
  h1, h2, h3, h4 {
    font-family: 'Source Serif 4', Georgia, serif;
    color: #0d121b; font-weight: 700;
  }

  /* ---- 卡片系统 ---- */
  .panel-content {
    background: #fff;
    padding: 20px 22px;
    border-radius: 14px;
    border: 1px solid rgba(13,18,27,.08);
    margin-bottom: 16px;
    box-shadow: 0 2px 8px rgba(13,18,27,.04);
    transition: box-shadow .15s ease;
  }
  .panel-content:hover {
    box-shadow: 0 6px 20px rgba(13,18,27,.07);
  }
  .panel-content h4 {
    margin: 0 0 10px 0;
    font-size: 1.05rem;
    font-weight: 700;
    color: #0d121b;
    letter-spacing: -0.01em;
  }

  /* ---- KPI 卡片 ---- */
  .kpi-card {
    background: #fff;
    padding: 16px 18px;
    border-radius: 14px;
    border: 1px solid rgba(13,18,27,.08);
    margin-bottom: 12px;
    transition: transform .15s ease, box-shadow .15s ease;
    position: relative;
    overflow: hidden;
  }
  .kpi-card::before {
    content: '';
    position: absolute; left: 0; top: 0;
    width: 4px; height: 100%;
    background: var(--kpi-accent, #1d3f5f);
  }
  .kpi-card:hover {
    transform: translateY(-2px);
    box-shadow: 0 8px 24px rgba(13,18,27,.08);
  }
  .kpi-label {
    color: #5d667a; font-size: 11.5px; letter-spacing: 0.06em;
    text-transform: uppercase; font-weight: 700;
  }
  .kpi-value {
    font-size: 26px; font-weight: 700; color: #1d3f5f;
    margin-top: 4px; font-family: 'Source Serif 4', serif;
    line-height: 1.1;
  }
  .kpi-sublabel { color: #5d667a; font-size: 11.5px; margin-top: 4px; }
  .kpi-primary { --kpi-accent: #1d3f5f; }
  .kpi-primary .kpi-value { color: #1d3f5f; }
  .kpi-success { --kpi-accent: #2a857a; }
  .kpi-success .kpi-value { color: #2a857a; }
  .kpi-danger { --kpi-accent: #a23b3b; }
  .kpi-danger .kpi-value { color: #a23b3b; }
  .kpi-warning { --kpi-accent: #c89a3b; }
  .kpi-warning .kpi-value { color: #c89a3b; }
  .kpi-muted { --kpi-accent: #5d667a; }
  .kpi-muted .kpi-value { color: #5d667a; }

  /* ---- 导航栏 ---- */
  .navbar {
    border-bottom: none !important;
    box-shadow: 0 1px 3px rgba(13,18,27,.06);
    backdrop-filter: blur(12px);
    -webkit-backdrop-filter: blur(12px);
  }
  .navbar-brand {
    font-family: 'Source Serif 4', serif !important;
    font-weight: 700 !important;
    letter-spacing: 0.02em;
  }
  .nav-link {
    font-size: 13.5px !important;
    font-weight: 500 !important;
    padding: 8px 14px !important;
    border-radius: 8px !important;
    transition: background .12s ease, color .12s ease;
  }
  .nav-link:hover {
    background: rgba(255,255,255,.12) !important;
  }
  .nav-link.active {
    font-weight: 700 !important;
    background: rgba(255,255,255,.15) !important;
  }
  .dropdown-menu {
    border-radius: 12px !important;
    border: 1px solid rgba(13,18,27,.08) !important;
    box-shadow: 0 12px 40px rgba(13,18,27,.12) !important;
    padding: 8px !important;
    backdrop-filter: blur(16px);
  }
  .dropdown-item {
    border-radius: 8px !important;
    padding: 8px 14px !important;
    font-size: 13.5px !important;
    transition: background .1s ease;
  }
  .dropdown-item:hover {
    background: rgba(29,63,95,.06) !important;
  }

  /* ---- 滚动条 ---- */
  ::-webkit-scrollbar { width: 7px; height: 7px; }
  ::-webkit-scrollbar-track { background: transparent; }
  ::-webkit-scrollbar-thumb { background: rgba(13,18,27,.18); border-radius: 4px; }
  ::-webkit-scrollbar-thumb:hover { background: rgba(13,18,27,.35); }

  /* ---- Hero ---- */
  .ghs-hero {
    background: linear-gradient(135deg, #0d121b 0%, #1d3f5f 60%, #2a857a 100%);
    color: #f7eedf;
    border-radius: 0 0 20px 20px;
    padding: 44px 48px;
    margin: -16px -12px 24px;
    position: relative;
    overflow: hidden;
  }
  .ghs-hero::before {
    content: '';
    position: absolute; right: -60px; top: -60px;
    width: 300px; height: 300px; border-radius: 50%;
    background: radial-gradient(circle, rgba(247,192,138,.15) 0%, transparent 70%);
    pointer-events: none;
  }
  .ghs-hero::after {
    content: '';
    position: absolute; left: 10%; bottom: -40px;
    width: 200px; height: 200px; border-radius: 50%;
    background: radial-gradient(circle, rgba(42,133,122,.12) 0%, transparent 70%);
    pointer-events: none;
  }
  .ghs-hero-inner { position: relative; z-index: 2; max-width: 1000px; }
  .ghs-hero-kicker {
    display: inline-block; padding: 5px 14px; border-radius: 999px;
    background: rgba(255,255,255,.1); color: rgba(247,192,138,.9);
    font-size: 11px; font-weight: 800; letter-spacing: 0.16em;
    text-transform: uppercase;
  }
  .ghs-hero-title {
    color: #f7eedf !important;
    font-size: clamp(28px, 3.5vw, 38px);
    line-height: 1.15;
    margin: 14px 0 10px;
    font-family: 'Source Serif 4', Georgia, serif;
    font-weight: 700;
  }
  .ghs-hero-lead {
    color: rgba(247,238,223,.78);
    font-size: 15.5px;
    line-height: 1.65;
    margin: 0 0 16px;
    max-width: 760px;
  }
  .ghs-hero-meta {
    display: flex; flex-wrap: wrap; gap: 6px 12px;
    color: rgba(247,238,223,.55); font-size: 12.5px;
  }
  .ghs-hero-meta a {
    color: rgba(247,192,138,.85); text-decoration: none;
    border-bottom: 1px dotted rgba(247,192,138,.4);
  }
  .ghs-hero-meta a:hover { color: #fff; border-color: #fff; }

  /* ---- KPI grid (overview) ---- */
  .ghs-kpi-grid-top {
    display: grid; grid-template-columns: repeat(6, 1fr); gap: 12px;
    margin-bottom: 20px;
  }

  /* ---- card-note ---- */
  .card-note {
    color: #5d667a; font-size: 13px; line-height: 1.6;
    margin: -2px 0 12px; padding: 0;
    border-left: 3px solid rgba(29,63,95,.15);
    padding-left: 10px;
  }

  /* ---- section head ---- */
  .ghs-section-head-nav { margin: 32px 0 16px; }
  .ghs-section-head-nav h3 {
    font-family: 'Source Serif 4', serif;
    font-size: 22px; margin: 0 0 6px; color: #0d121b;
  }
  .ghs-section-lead {
    color: #5d667a; font-size: 14px; line-height: 1.65; margin: 0;
    max-width: 760px;
  }

  /* ---- 模块卡片导航 ---- */
  .module-grid {
    display: grid; grid-template-columns: repeat(4, 1fr); gap: 14px;
    margin-bottom: 24px;
  }
  .module-card {
    text-align: left;
    background: #fff;
    border: 1px solid rgba(13,18,27,.08);
    border-radius: 14px;
    padding: 18px 20px;
    cursor: pointer;
    transition: transform .15s ease, box-shadow .15s ease, border-color .15s ease;
    display: flex; flex-direction: column; gap: 6px;
    min-height: 120px;
    position: relative;
    overflow: hidden;
  }
  .module-card::before {
    content: '';
    position: absolute; left: 0; top: 0;
    width: 4px; height: 100%;
    background: #c46327;
    opacity: 0;
    transition: opacity .15s ease;
  }
  .module-card:hover {
    transform: translateY(-3px);
    box-shadow: 0 12px 32px rgba(13,18,27,.09);
    border-color: rgba(29,63,95,.2);
  }
  .module-card:hover::before { opacity: 1; }
  .module-card .module-kicker {
    color: #c46327; font-size: 10.5px; font-weight: 800;
    letter-spacing: 0.14em; text-transform: uppercase;
  }
  .module-card .module-title {
    color: #0d121b; font-family: 'Source Serif 4', serif;
    font-size: 16px; font-weight: 700;
  }
  .module-card .module-desc {
    color: #5d667a; font-size: 12px; line-height: 1.55;
  }

  /* ---- 响应式断点 ---- */
  @media (max-width: 1280px) {
    .module-grid { grid-template-columns: repeat(3, 1fr); }
    .ghs-kpi-grid-top { grid-template-columns: repeat(3, 1fr); }
  }
  @media (max-width: 1024px) {
    .module-grid { grid-template-columns: repeat(2, 1fr); }
    .ghs-hero { padding: 32px 28px; }
  }
  @media (max-width: 768px) {
    .ghs-kpi-grid-top { grid-template-columns: repeat(2, 1fr); }
    .ghs-hero-title { font-size: 24px; }
  }
  @media (max-width: 480px) {
    .module-grid { grid-template-columns: 1fr; }
    .ghs-kpi-grid-top { grid-template-columns: 1fr; }
    .ghs-hero { padding: 24px 18px; margin: -16px -8px 16px; }
  }

  /* ---- Sidebar 优化 ---- */
  .bslib-sidebar-layout > .sidebar {
    border-right: 1px solid rgba(13,18,27,.06) !important;
    background: #f8f4ed !important;
  }
  .bslib-sidebar-layout > .main {
    padding: 16px 20px !important;
  }

  /* ---- 表格优化 ---- */
  .rt-table { font-size: 13px; }
  .rt-th { font-weight: 700 !important; }
"))

bslib::page_navbar(
  title = htmltools::tagList(
    htmltools::HTML("&#127973; "),
    htmltools::span("GHS", style = "font-weight:800;letter-spacing:.04em;"),
    htmltools::span(" \u00b7 Global Health Spending",
                    style = "font-weight:400;opacity:.8;font-size:14px;margin-left:4px;")
  ),
  id          = "main_nav",
  theme       = ghs_theme,
  window_title = "Global Health Spending \u00b7 195 Countries \u00b7 2000\u20132023",
  fillable    = FALSE,
  navbar_options = bslib::navbar_options(bg = "#0d121b", theme = "dark"),
  header      = htmltools::tags$head(v2_css, ghs_v3_shiny_css),

  # ---- Navigation: 36 modules grouped by theme ----
  bslib::nav_menu(
    title = "\u603b\u89c8",
    mod_overview_ui("overview"),
    mod_about_ui("about"),
    mod_methods_ui("methods")
  ),
  bslib::nav_menu(
    title = "\u56fd\u5bb6\u4e0e\u533a\u57df",
    mod_country_ui("country", country_choices_named, year_min, year_max),
    mod_regional_ui("regional"),
    mod_ranking_ui("ranking"),
    mod_benchmark_ui("benchmark")
  ),
  bslib::nav_menu(
    title = "\u7b79\u8d44\u7ed3\u6784",
    mod_financing_ui("financing"),
    mod_spending_ui("spending"),
    mod_purpose_ui("purpose"),
    mod_aid_ui("aid"),
    mod_fiscal_ui("fiscal")
  ),
  bslib::nav_menu(
    title = "\u516c\u5e73\u4e0e\u6548\u7387",
    mod_equity_ui("equity", year_min, year_max),
    mod_inequality_ui("inequality"),
    mod_efficiency_ui("efficiency", year_min, year_max),
    mod_convergence_ui("convergence"),
    mod_decomposition_ui("decomposition")
  ),
  bslib::nav_menu(
    title = "\u5065\u5eb7\u4ea7\u51fa",
    mod_outcomes_ui("outcomes"),
    mod_sdg_ui("sdg"),
    mod_prevention_ui("prevention"),
    mod_aging_ui("aging")
  ),
  bslib::nav_menu(
    title = "\u51b2\u51fb\u4e0e\u53d8\u5316",
    mod_pandemic_ui("pandemic"),
    mod_growth_ui("growth"),
    mod_transition_ui("transition"),
    mod_timeline_ui("timeline"),
    mod_extremes_ui("extremes")
  ),
  bslib::nav_menu(
    title = "\u5206\u6790\u5de5\u5177",
    mod_compare_ui("compare", country_choices_named, indicator_choices),
    mod_cluster_ui("cluster", year_min, year_max),
    mod_forecast_ui("forecast", country_choices_named, indicator_choices),
    mod_scenarios_ui("scenarios", country_choices_named),
    mod_correlation_ui("correlation"),
    mod_distribution_ui("distribution")
  ),
  bslib::nav_menu(
    title = "\u8d28\u91cf\u4e0e\u7a33\u5065",
    mod_robustness_ui("robustness"),
    mod_dataquality_ui("dataquality"),
    mod_policy_ui("policy", country_choices_named),
    mod_atlas_ui("atlas")
  ),

  # 页脚
  footer = htmltools::div(
    style = paste0(
      "color: #5d667a; font-size: 12px; padding: 16px 24px;",
      "border-top: 1px solid rgba(13,18,27,.06); margin-top: 32px;",
      "display: flex; justify-content: space-between; align-items: center;",
      "flex-wrap: wrap; gap: 8px;"
    ),
    htmltools::span(
      htmltools::HTML(paste0(
        "&copy; 2026 \u00b7 \u5e84\u9882 (20241334) \u00b7 ",
        "\u6570\u636e: WHO GHED 2024-12 + WDI"))
    ),
    htmltools::span(
      htmltools::tags$a(
        href = "https://github.com/2711944586/R", target = "_blank",
        style = "color:#1d3f5f;text-decoration:none;font-weight:600;",
        "GitHub"),
      htmltools::HTML(" \u00b7 "),
      htmltools::tags$a(
        href = "https://2711944586.github.io/R/", target = "_blank",
        style = "color:#1d3f5f;text-decoration:none;font-weight:600;",
        "\u9759\u6001\u62a5\u544a")
    )
  )
)
