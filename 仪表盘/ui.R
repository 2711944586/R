# =============================================================================
# 仪表盘/ui.R
# Shiny 仪表盘 UI · bslib::page_navbar + 36 模块化 nav_panel
# =============================================================================

# 前端样式系统
v2_css <- htmltools::tags$style(htmltools::HTML("
  body{background:#f8f5f0;color:#0d121b;font-family:'Inter',-apple-system,BlinkMacSystemFont,sans-serif;-webkit-font-smoothing:antialiased;line-height:1.6;font-feature-settings:'ss01' on,'kern' on;text-rendering:optimizeLegibility}
  h1,h2,h3,h4,h5{font-family:'Source Serif 4','Noto Serif SC',Georgia,serif;color:#0d121b;font-weight:700;letter-spacing:-.015em;line-height:1.15}
  .panel-content{background:#fff;padding:24px 26px;border-radius:16px;border:1px solid rgba(13,18,27,.06);margin-bottom:18px;box-shadow:0 1px 2px rgba(13,18,27,.04),0 4px 12px rgba(13,18,27,.03);transition:transform .2s cubic-bezier(.4,0,.2,1),box-shadow .2s cubic-bezier(.4,0,.2,1)}
  .panel-content:hover{transform:translateY(-2px);box-shadow:0 4px 8px rgba(13,18,27,.06),0 16px 40px rgba(13,18,27,.06)}
  .panel-content h4{margin:0 0 14px;font-size:1.1rem;font-weight:700;color:#0d121b;letter-spacing:-.01em}
  .kpi-card{background:linear-gradient(180deg,#fff,#faf7f2);padding:20px 22px;border-radius:16px;border:1px solid rgba(13,18,27,.06);margin-bottom:14px;transition:transform .2s cubic-bezier(.4,0,.2,1),box-shadow .2s cubic-bezier(.4,0,.2,1);position:relative;overflow:hidden}
  .kpi-card::before{content:'';position:absolute;left:0;top:0;width:5px;height:100%;background:linear-gradient(180deg,var(--kpi-accent,#1d3f5f),rgba(13,18,27,.2));border-radius:0 4px 4px 0}
  .kpi-card:hover{transform:translateY(-3px);box-shadow:0 8px 16px rgba(13,18,27,.06),0 24px 48px rgba(13,18,27,.06)}
  .kpi-label{color:#5d667a;font-size:11px;letter-spacing:.08em;text-transform:uppercase;font-weight:800;margin-bottom:2px}
  .kpi-value{font-size:30px;font-weight:800;color:#1d3f5f;margin-top:6px;font-family:'Source Serif 4',serif;line-height:1.05;letter-spacing:-.02em}
  .kpi-sublabel{color:#5d667a;font-size:11.5px;margin-top:6px;line-height:1.4}
  .kpi-primary{--kpi-accent:#1d3f5f}.kpi-primary .kpi-value{color:#1d3f5f}
  .kpi-success{--kpi-accent:#2a857a}.kpi-success .kpi-value{color:#2a857a}
  .kpi-danger{--kpi-accent:#a23b3b}.kpi-danger .kpi-value{color:#a23b3b}
  .kpi-warning{--kpi-accent:#c89a3b}.kpi-warning .kpi-value{color:#c89a3b}
  .kpi-muted{--kpi-accent:#5d667a}.kpi-muted .kpi-value{color:#5d667a}
  .navbar{border-bottom:none !important;box-shadow:0 2px 12px rgba(0,0,0,.15) !important;backdrop-filter:blur(16px) saturate(140%);-webkit-backdrop-filter:blur(16px) saturate(140%)}
  .navbar-brand{font-family:'Source Serif 4',serif !important;font-weight:800 !important;letter-spacing:.03em;font-size:15px !important}
  .nav-link{font-size:13px !important;font-weight:600 !important;padding:7px 14px !important;border-radius:8px !important;transition:all .15s ease;letter-spacing:.01em}
  .nav-link:hover{background:rgba(255,255,255,.12) !important;color:#fff !important}
  .nav-link.active,.dropdown-toggle.active{font-weight:800 !important;background:rgba(255,255,255,.16) !important;color:#fff !important}
  .dropdown-menu{border-radius:14px !important;border:1px solid rgba(13,18,27,.06) !important;box-shadow:0 4px 6px rgba(0,0,0,.04),0 16px 48px rgba(0,0,0,.12) !important;padding:6px !important;background:#fff !important;min-width:200px !important}
  .dropdown-item{border-radius:8px !important;padding:10px 16px !important;font-size:13.5px !important;font-weight:500 !important;color:#0d121b !important;transition:all .12s ease}
  .dropdown-item:hover,.dropdown-item:focus{background:rgba(29,63,95,.06) !important;color:#1d3f5f !important}
  .dropdown-item.active,.dropdown-item:active{background:#1d3f5f !important;color:#fff !important;font-weight:700 !important}
  ::-webkit-scrollbar{width:6px;height:6px}::-webkit-scrollbar-track{background:transparent}::-webkit-scrollbar-thumb{background:rgba(13,18,27,.15);border-radius:99px}::-webkit-scrollbar-thumb:hover{background:rgba(13,18,27,.3)}
  .ghs-hero{background:linear-gradient(145deg,#0a0f1a 0%,#1d3f5f 55%,#1a5c4e 100%);color:#f7eedf;border-radius:0 0 24px 24px;padding:52px 56px;margin:-16px -12px 28px;position:relative;overflow:hidden;box-shadow:0 20px 60px rgba(0,0,0,.2)}
  .ghs-hero::before{content:'';position:absolute;right:-80px;top:-80px;width:400px;height:400px;border-radius:50%;background:radial-gradient(circle,rgba(247,192,138,.12) 0%,transparent 65%);pointer-events:none}
  .ghs-hero::after{content:'';position:absolute;left:5%;bottom:-60px;width:300px;height:300px;border-radius:50%;background:radial-gradient(circle,rgba(42,133,122,.1) 0%,transparent 60%);pointer-events:none}
  .ghs-hero-inner{position:relative;z-index:2;max-width:1040px}
  .ghs-hero-kicker{display:inline-block;padding:6px 16px;border-radius:999px;background:linear-gradient(90deg,rgba(247,192,138,.15),rgba(255,255,255,.06));border:1px solid rgba(247,192,138,.25);color:rgba(247,192,138,.95);font-size:10.5px;font-weight:800;letter-spacing:.2em;text-transform:uppercase}
  .ghs-hero-title{color:#f7eedf !important;font-size:clamp(30px,4vw,44px);line-height:1.1;margin:18px 0 14px;font-family:'Source Serif 4',Georgia,serif;font-weight:800;letter-spacing:-.02em}
  .ghs-hero-lead{color:rgba(247,238,223,.82);font-size:16px;line-height:1.7;margin:0 0 20px;max-width:720px;font-weight:400}
  .ghs-hero-meta{display:flex;flex-wrap:wrap;gap:6px 14px;color:rgba(247,238,223,.5);font-size:12px;font-weight:500}
  .ghs-hero-meta a{color:rgba(247,192,138,.8);text-decoration:none;border-bottom:1px dotted rgba(247,192,138,.35);transition:all .15s ease}
  .ghs-hero-meta a:hover{color:#fff;border-color:#fff}
  .ghs-kpi-grid-top{display:grid;grid-template-columns:repeat(6,1fr);gap:14px;margin-bottom:24px}
  .card-note{color:#5d667a;font-size:13px;line-height:1.65;margin:-2px 0 14px;padding:0 0 0 12px;border-left:3px solid rgba(196,99,39,.25);font-style:italic}
  .ghs-section-head-nav{margin:36px 0 18px}
  .ghs-section-head-nav h3{font-family:'Source Serif 4',serif;font-size:24px;margin:0 0 8px;color:#0d121b;font-weight:800}
  .ghs-section-lead{color:#5d667a;font-size:14.5px;line-height:1.7;margin:0;max-width:720px}
  .module-grid{display:grid;grid-template-columns:repeat(4,1fr);gap:16px;margin-bottom:28px}
  .module-card{text-align:left;background:#fff;border:1px solid rgba(13,18,27,.06);border-radius:16px;padding:22px 24px;cursor:pointer;transition:all .2s cubic-bezier(.4,0,.2,1);display:flex;flex-direction:column;gap:8px;min-height:130px;position:relative;overflow:hidden;box-shadow:0 1px 3px rgba(13,18,27,.03)}
  .module-card::before{content:'';position:absolute;left:0;top:0;width:4px;height:100%;background:linear-gradient(180deg,#c46327,#1d3f5f);opacity:0;transition:opacity .2s ease}
  .module-card:hover{transform:translateY(-4px);box-shadow:0 8px 16px rgba(13,18,27,.06),0 24px 48px rgba(13,18,27,.08);border-color:rgba(29,63,95,.15)}
  .module-card:hover::before{opacity:1}
  .module-card .module-kicker{color:#c46327;font-size:10px;font-weight:800;letter-spacing:.16em;text-transform:uppercase}
  .module-card .module-title{color:#0d121b;font-family:'Source Serif 4',serif;font-size:17px;font-weight:700;letter-spacing:-.01em}
  .module-card .module-desc{color:#5d667a;font-size:12.5px;line-height:1.6}
  @media(max-width:1280px){.module-grid{grid-template-columns:repeat(3,1fr)}.ghs-kpi-grid-top{grid-template-columns:repeat(3,1fr)}}
  @media(max-width:1024px){.module-grid{grid-template-columns:repeat(2,1fr)}.ghs-hero{padding:36px 32px}}
  @media(max-width:768px){.ghs-kpi-grid-top{grid-template-columns:repeat(2,1fr)}.ghs-hero-title{font-size:26px}}
  @media(max-width:480px){.module-grid,.ghs-kpi-grid-top{grid-template-columns:1fr}.ghs-hero{padding:28px 20px;margin:-16px -8px 18px}}
  .bslib-sidebar-layout>.sidebar{border-right:1px solid rgba(13,18,27,.05) !important;background:linear-gradient(180deg,#f8f5f0,#f3ede4) !important}
  .bslib-sidebar-layout>.main{padding:18px 24px !important}
  .rt-table{font-size:13px}.rt-th{font-weight:800 !important;font-size:11px !important;letter-spacing:.04em;text-transform:uppercase;color:#5d667a !important}
  .rt-td{padding:10px 12px !important}.rt-tr:hover .rt-td{background:rgba(29,63,95,.03) !important}
  ::selection{background:rgba(196,99,39,.2)}
  a:focus-visible,button:focus-visible{outline:2px solid #c46327;outline-offset:3px;border-radius:6px}
  .plotly,.leaflet{border-radius:12px;overflow:hidden}
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
