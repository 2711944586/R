
ghs_app_css <- htmltools::tags$style(htmltools::HTML("
:root {
  --ghs-primary: #254f5c;
  --ghs-secondary: #6b6077;
  --ghs-good: #587669;
  --ghs-warn: #8f743d;
  --ghs-bad: #8b544e;
  --ghs-neutral: #5d6965;
  --ghs-paper: #f4f5f0;
  --ghs-ink: #17211f;
  --ghs-line: rgba(23,33,31,.13);
  --ghs-line-strong: rgba(23,33,31,.20);
  --ghs-radius: 8px;
  --ghs-shadow-sm: 0 1px 2px rgba(23,33,31,.05), 0 10px 26px rgba(23,33,31,.075);
  --ghs-shadow: 0 2px 4px rgba(23,33,31,.06), 0 16px 34px rgba(23,33,31,.10);
  --ghs-font-sans: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
  --ghs-font-serif: 'Source Serif 4', 'Noto Serif SC', Georgia, serif;
  --ghs-font-mono: 'JetBrains Mono', 'Fira Code', Consolas, monospace;
}

body {
  background: var(--ghs-paper);
  color: var(--ghs-ink);
  font-family: var(--ghs-font-sans);
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
  text-rendering: optimizeLegibility;
  font-feature-settings: 'ss01' on, 'kern' on, 'liga' on;
  line-height: 1.6;
  font-size: 15px;
}

h1, h2, h3, h4, h5, h6 {
  font-family: var(--ghs-font-serif);
  color: var(--ghs-ink);
  font-weight: 750;
  letter-spacing: -.02em;
  line-height: 1.15;
}

.navbar.navbar-default,
.navbar,
.navbar.bg-dark,
nav.navbar {
  background: linear-gradient(180deg, rgba(255,253,248,.98), rgba(248,241,231,.96)) !important;
  border: none !important;
  border-bottom: 1px solid rgba(13,18,27,.10) !important;
  box-shadow: 0 1px 0 rgba(255,255,255,.75) inset, 0 8px 24px rgba(13,18,27,.055) !important;
  backdrop-filter: blur(18px) saturate(135%);
  -webkit-backdrop-filter: blur(18px) saturate(135%);
  min-height: 56px !important;
  padding: 0 20px !important;
  position: sticky !important;
  top: 0;
  z-index: 1050;
}

.navbar .navbar-brand,
.navbar-brand {
  font-family: var(--ghs-font-serif) !important;
  font-weight: 800 !important;
  font-size: 16px !important;
  letter-spacing: .02em !important;
  color: var(--ghs-ink) !important;
  padding: 12px 0 !important;
  display: flex !important;
  align-items: center !important;
  gap: 2px !important;
}

.navbar .nav-link,
.navbar .navbar-nav .nav-link {
  font-family: var(--ghs-font-sans) !important;
  font-size: 13px !important;
  font-weight: 600 !important;
  color: rgba(13,18,27,.68) !important;
  padding: 8px 14px !important;
  border-radius: 8px !important;
  transition: all .15s ease !important;
  letter-spacing: 0 !important;
  white-space: nowrap !important;
}

.navbar .nav-link:hover,
.navbar .navbar-nav .nav-link:hover {
  color: var(--ghs-primary) !important;
  background: rgba(29,63,95,.07) !important;
}

.navbar .nav-link.active,
.navbar .navbar-nav .nav-link.active,
.navbar .nav-link.show {
  color: #fff !important;
  background: var(--ghs-primary) !important;
  font-weight: 700 !important;
}

.navbar .dropdown-toggle {
  color: rgba(13,18,27,.70) !important;
  font-size: 13px !important;
  font-weight: 600 !important;
  padding: 8px 14px !important;
  border-radius: 8px !important;
  transition: all .15s ease !important;
}
.navbar .dropdown-toggle:hover,
.navbar .dropdown-toggle:focus {
  color: var(--ghs-primary) !important;
  background: rgba(29,63,95,.07) !important;
}
.navbar .dropdown-toggle.show,
.navbar .show > .dropdown-toggle {
  color: #fff !important;
  background: var(--ghs-primary) !important;
}

.navbar .dropdown-menu,
.dropdown-menu {
  background: #fffdf8 !important;
  border: 1px solid rgba(13,18,27,.10) !important;
  border-radius: 12px !important;
  box-shadow: 0 4px 12px rgba(13,18,27,.06), 0 18px 48px rgba(13,18,27,.11) !important;
  padding: 6px !important;
  margin-top: 6px !important;
  min-width: 220px !important;
  animation: ghs-dropdown-in .15s ease;
}

@keyframes ghs-dropdown-in {
  from { opacity: 0; transform: translateY(-6px); }
  to { opacity: 1; transform: translateY(0); }
}

.dropdown-menu .dropdown-item,
.dropdown-item {
  font-family: var(--ghs-font-sans) !important;
  font-size: 13.5px !important;
  font-weight: 500 !important;
  color: var(--ghs-ink) !important;
  padding: 9px 14px !important;
  border-radius: 8px !important;
  transition: all .12s ease !important;
  line-height: 1.4 !important;
}

.dropdown-item:hover,
.dropdown-item:focus {
  background: rgba(29,63,95,.06) !important;
  color: var(--ghs-primary) !important;
}

.dropdown-item.active,
.dropdown-item:active {
  background: var(--ghs-primary) !important;
  color: #fff !important;
  font-weight: 700 !important;
}

.navbar-toggler {
  border: 1px solid rgba(13,18,27,.16) !important;
  border-radius: 8px !important;
  padding: 6px 10px !important;
}
.navbar-toggler-icon {
  filter: none !important;
  background-image: url('data:image/svg+xml,%3csvg xmlns=%27http://www.w3.org/2000/svg%27 viewBox=%270 0 30 30%27%3e%3cpath stroke=%27rgba%2813,18,27,.74%29%27 stroke-linecap=%27round%27 stroke-miterlimit=%2710%27 stroke-width=%272%27 d=%27M4 7h22M4 15h22M4 23h22%27/%3e%3c/svg%3e') !important;
}

@media (max-width: 991px) {
  .navbar-collapse {
    background: #fffdf8 !important;
    border-radius: 0 0 16px 16px !important;
    padding: 12px 16px !important;
    margin: 0 -20px !important;
    border-top: 1px solid rgba(13,18,27,.10) !important;
    box-shadow: 0 12px 36px rgba(13,18,27,.12) !important;
    max-height: 80vh;
    overflow-y: auto;
  }
  .navbar .dropdown-menu {
    background: #fbf6ee !important;
    border: 1px solid rgba(13,18,27,.10) !important;
    box-shadow: none !important;
  }
  .navbar .dropdown-item {
    color: rgba(13,18,27,.72) !important;
  }
  .navbar .dropdown-item:hover {
    color: var(--ghs-primary) !important;
    background: rgba(29,63,95,.07) !important;
  }
  .navbar .dropdown-item.active {
    background: var(--ghs-primary) !important;
    color: #fff !important;
  }
}

.panel-content {
  background: #fff;
  padding: 22px 24px;
  border-radius: var(--ghs-radius);
  border: 1px solid var(--ghs-line);
  margin-bottom: 16px;
  box-shadow: var(--ghs-shadow-sm);
  transition: transform .18s ease, box-shadow .18s ease;
}
.panel-content:hover {
  transform: translateY(-1px);
  box-shadow: var(--ghs-shadow);
}
.panel-content h4 {
  margin: 0 0 12px;
  font-size: 1.05rem;
  font-weight: 750;
  color: var(--ghs-ink);
}

.kpi-card {
  background: #fff;
  padding: 18px 20px;
  border-radius: var(--ghs-radius);
  border: 1px solid var(--ghs-line);
  margin-bottom: 14px;
  transition: transform .18s ease, box-shadow .18s ease;
  position: relative;
  overflow: hidden;
  box-shadow: var(--ghs-shadow-sm);
}
.kpi-card::before {
  content: '';
  position: absolute;
  left: 0; top: 0;
  width: 4px; height: 100%;
  background: var(--kpi-accent, var(--ghs-primary));
  border-radius: 0 3px 3px 0;
}
.kpi-card:hover {
  transform: translateY(-2px);
  box-shadow: var(--ghs-shadow);
}
.kpi-label {
  color: var(--ghs-neutral);
  font-size: 11px;
  letter-spacing: .08em;
  text-transform: uppercase;
  font-weight: 800;
  margin-bottom: 4px;
}
.kpi-value {
  font-size: 28px;
  font-weight: 800;
  color: var(--ghs-primary);
  font-family: var(--ghs-font-serif);
  line-height: 1.05;
  letter-spacing: -.02em;
}
.kpi-sublabel {
  color: var(--ghs-neutral);
  font-size: 11.5px;
  margin-top: 6px;
  line-height: 1.4;
}
.kpi-primary { --kpi-accent: var(--ghs-primary); }
.kpi-primary .kpi-value { color: var(--ghs-primary); }
.kpi-success { --kpi-accent: var(--ghs-good); }
.kpi-success .kpi-value { color: var(--ghs-good); }
.kpi-danger { --kpi-accent: var(--ghs-bad); }
.kpi-danger .kpi-value { color: var(--ghs-bad); }
.kpi-warning { --kpi-accent: var(--ghs-warn); }
.kpi-warning .kpi-value { color: var(--ghs-warn); }
.kpi-muted { --kpi-accent: var(--ghs-neutral); }
.kpi-muted .kpi-value { color: var(--ghs-neutral); }

.ghs-hero {
  background: #eef3f2;
  color: #f7eedf;
  border-radius: 0 0 24px 24px;
  padding: 48px 52px;
  margin: -16px -12px 26px;
  position: relative;
  overflow: hidden;
  box-shadow: 0 16px 48px rgba(0,0,0,.2);
}
.ghs-hero::before {
  content: '';
  position: absolute;
  right: -60px; top: -60px;
  width: 360px; height: 360px;
  border-radius: 50%;
  background: radial-gradient(circle, rgba(247,192,138,.12) 0%, transparent 65%);
  pointer-events: none;
}
.ghs-hero::after {
  content: '';
  position: absolute;
  left: 5%; bottom: -50px;
  width: 280px; height: 280px;
  border-radius: 50%;
  background: radial-gradient(circle, rgba(42,133,122,.08) 0%, transparent 60%);
  pointer-events: none;
}
.ghs-hero-inner { position: relative; z-index: 2; max-width: 1040px; }
.ghs-hero-kicker {
  display: inline-block;
  padding: 5px 14px;
  border-radius: 999px;
  background: linear-gradient(90deg, rgba(247,192,138,.12), rgba(255,255,255,.04));
  border: 1px solid rgba(247,192,138,.2);
  color: rgba(247,192,138,.9);
  font-size: 10.5px;
  font-weight: 800;
  letter-spacing: .18em;
  text-transform: uppercase;
}
.ghs-hero-title {
  color: #f7eedf !important;
  font-size: clamp(28px, 3.5vw, 42px);
  line-height: 1.1;
  margin: 16px 0 12px;
  font-family: var(--ghs-font-serif);
  font-weight: 800;
  letter-spacing: -.02em;
}
.ghs-hero-lead {
  color: rgba(247,238,223,.78);
  font-size: 15.5px;
  line-height: 1.7;
  margin: 0 0 18px;
  max-width: 700px;
  font-weight: 400;
}
.ghs-hero-meta {
  display: flex;
  flex-wrap: wrap;
  gap: 6px 12px;
  color: rgba(247,238,223,.5);
  font-size: 12px;
  font-weight: 500;
}
.ghs-hero-meta a {
  color: rgba(247,192,138,.8);
  text-decoration: none;
  border-bottom: 1px dotted rgba(247,192,138,.35);
  transition: all .15s ease;
}
.ghs-hero-meta a:hover { color: #fff; border-color: #fff; }

.ghs-kpi-grid-top {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(160px, 1fr));
  gap: 14px;
  margin-bottom: 22px;
}

.card-note {
  color: var(--ghs-neutral);
  font-size: 13px;
  line-height: 1.65;
  margin: -2px 0 12px;
  padding: 0 0 0 12px;
  border-left: 3px solid rgba(196,99,39,.25);
  font-style: italic;
}

.ghs-section-head-nav { margin: 32px 0 16px; }
.ghs-section-head-nav h3 {
  font-family: var(--ghs-font-serif);
  font-size: 22px;
  margin: 0 0 6px;
  font-weight: 800;
}
.ghs-section-lead {
  color: var(--ghs-neutral);
  font-size: 14px;
  line-height: 1.65;
  max-width: 700px;
}

.module-grid {
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(250px, 1fr));
  gap: 14px;
  margin-bottom: 24px;
}
.module-card {
  text-align: left;
  background: #fff;
  border: 1px solid var(--ghs-line);
  border-radius: var(--ghs-radius);
  padding: 20px 22px;
  cursor: pointer;
  transition: all .18s ease;
  display: flex;
  flex-direction: column;
  gap: 6px;
  min-height: 120px;
  position: relative;
  overflow: hidden;
  box-shadow: var(--ghs-shadow-sm);
}
.module-card::before {
  content: '';
  position: absolute;
  left: 0; top: 0;
  width: 4px; height: 100%;
  background: linear-gradient(180deg, var(--ghs-secondary), var(--ghs-primary));
  opacity: 0;
  transition: opacity .18s ease;
}
.module-card:hover {
  transform: translateY(-3px);
  box-shadow: var(--ghs-shadow);
  border-color: rgba(29,63,95,.12);
}
.module-card:hover::before { opacity: 1; }
.module-card .module-kicker {
  color: var(--ghs-secondary);
  font-size: 10px;
  font-weight: 800;
  letter-spacing: .14em;
  text-transform: uppercase;
}
.module-card .module-title {
  color: var(--ghs-ink);
  font-family: var(--ghs-font-serif);
  font-size: 16px;
  font-weight: 700;
}
.module-card .module-desc {
  color: var(--ghs-neutral);
  font-size: 12.5px;
  line-height: 1.55;
}

.bslib-sidebar-layout > .sidebar {
  border-right: 1px solid var(--ghs-line) !important;
  background: linear-gradient(180deg, #f8f4ee, #f2ebe0) !important;
}
.bslib-sidebar-layout > .main {
  padding: 18px 22px !important;
}

.rt-table { font-size: 13px; border-radius: 12px; overflow: hidden; }
.rt-th {
  font-weight: 800 !important;
  font-size: 11px !important;
  letter-spacing: .04em;
  text-transform: uppercase;
  color: var(--ghs-neutral) !important;
  background: #f5efe6 !important;
}
.rt-td { padding: 9px 12px !important; }
.rt-tr:hover .rt-td { background: rgba(29,63,95,.02) !important; }

.plotly, .leaflet, .html-widget { border-radius: 12px; overflow: hidden; }
.leaflet-container { background: #eef2ed; }

::-webkit-scrollbar { width: 6px; height: 6px; }
::-webkit-scrollbar-track { background: transparent; }
::-webkit-scrollbar-thumb { background: rgba(13,18,27,.12); border-radius: 99px; }
::-webkit-scrollbar-thumb:hover { background: rgba(13,18,27,.25); }

::selection { background: rgba(196,99,39,.15); }
a:focus-visible, button:focus-visible {
  outline: 2px solid var(--ghs-secondary);
  outline-offset: 3px;
  border-radius: 6px;
}

.form-control, .form-select, .selectize-input {
  border-radius: 10px !important;
  border: 1px solid var(--ghs-line-strong) !important;
  font-size: 13px !important;
}
.bslib-sidebar-layout .form-label,
.bslib-sidebar-layout label.control-label {
  font-size: 11px;
  font-weight: 800;
  letter-spacing: .08em;
  text-transform: uppercase;
  color: var(--ghs-primary);
  margin-bottom: 5px;
}

.btn { border-radius: 10px !important; font-weight: 700 !important; }
.btn-primary {
  background: var(--ghs-primary) !important;
  border-color: var(--ghs-primary) !important;
}

.card, .bslib-card {
  border-color: var(--ghs-line) !important;
  border-radius: var(--ghs-radius) !important;
  box-shadow: var(--ghs-shadow-sm) !important;
}

.nav-tabs .nav-link {
  color: var(--ghs-neutral) !important;
  border-radius: 10px 10px 0 0 !important;
  font-weight: 600 !important;
  font-size: 13px !important;
}
.nav-tabs .nav-link.active {
  color: var(--ghs-ink) !important;
  background: #fff !important;
  font-weight: 700 !important;
}

@media (max-width: 1280px) {
  .ghs-kpi-grid-top { grid-template-columns: repeat(3, 1fr); }
}
@media (max-width: 1024px) {
  .ghs-hero { padding: 36px 32px; }
}
@media (max-width: 768px) {
  .ghs-kpi-grid-top { grid-template-columns: repeat(2, 1fr); }
  .ghs-hero-title { font-size: 24px !important; }
  .ghs-hero { padding: 28px 22px; margin: -16px -8px 18px; }
}
@media (max-width: 480px) {
  .ghs-kpi-grid-top { grid-template-columns: 1fr; }
  .ghs-hero { padding: 22px 16px; }
}
"))

bslib::page_navbar(
  title = htmltools::tagList(
    htmltools::span("GHS", style = "font-weight:800;letter-spacing:.03em;"),
    htmltools::span(" \u00b7 Global Health Spending",
                    style = "font-weight:400;opacity:.8;font-size:13px;margin-left:3px;")
  ),
  id          = "main_nav",
  theme       = ghs_theme,
  window_title = "GHS \u00b7 Global Health Spending \u00b7 195 Countries \u00b7 2000\u20132023",
  fillable    = FALSE,
  navbar_options = bslib::navbar_options(
    bg = "#fffdf8",
    theme = "light",
    collapsible = TRUE
  ),
  header = htmltools::tags$head(
    ghs_app_css,
    ghs_v3_shiny_css,
    htmltools::tags$link(
      rel = "preconnect", href = "https://fonts.googleapis.com"
    ),
    htmltools::tags$link(
      rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = NA
    ),
    htmltools::tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800;900&family=Noto+Sans+SC:wght@400;500;700;900&family=Noto+Serif+SC:wght@500;700;900&family=Source+Serif+4:wght@400;600;700;800&family=JetBrains+Mono:wght@400;700&display=swap"
    ),
    htmltools::tags$link(
      rel = "icon",
      type = "image/svg+xml",
      href = paste0(
        "data:image/svg+xml,",
        utils::URLencode(
          "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 64 64'><rect width='64' height='64' rx='12' fill='#254f5c'/><path d='M14 43V21h6v17h9v5H14Zm19 0V21h17v5H39v5h10v5H39v7h-6Z' fill='#f8faf6'/></svg>",
          reserved = TRUE
        )
      )
    ),
    htmltools::tags$link(rel = "stylesheet", href = "ghs-shiny.css"),
    htmltools::tags$script(src = "ghs-shiny.js", defer = NA)
  ),

  mod_home_ui("home"),
  bslib::nav_menu(
    title = "\u603b\u89c8",
    icon = NULL,
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
  mod_widgets_ui("widgets", country_choices_named, year_min, year_max),
  bslib::nav_menu(
    title = "\u5206\u6790\u5de5\u5177",
    mod_compare_ui("compare", country_choices_named, indicator_choices),
    mod_cluster_ui("cluster", year_min, year_max),
    mod_forecast_ui("forecast", country_choices_named, indicator_choices),
    mod_scenarios_ui("scenarios", country_choices_named),
    mod_mapstudio_ui("mapstudio", country_choices_named, indicator_choices,
                     year_min, year_max),
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

  footer = htmltools::div(
    style = paste0(
      "color: var(--ghs-neutral); font-size: 12px; padding: 18px 24px;",
      "border-top: 1px solid var(--ghs-line); margin-top: 36px;",
      "display: flex; justify-content: space-between; align-items: center;",
      "flex-wrap: wrap; gap: 8px; max-width: 1400px; margin-left: auto;",
      "margin-right: auto;"
    ),
    htmltools::span(
      htmltools::HTML(paste0(
        "&copy; 2026 \u00b7 \u5e84\u9882 (20241334) \u00b7 ",
        "\u6570\u636e: WHO GHED 2024-12 + WDI \u00b7 195 \u56fd 2000\u20132023"))
    ),
    htmltools::span(
      htmltools::tags$a(
        href = "https://github.com/2711944586/R", target = "_blank",
        style = "color:var(--ghs-primary);text-decoration:none;font-weight:600;",
        "GitHub"),
      htmltools::HTML(" \u00b7 "),
      htmltools::tags$a(
        href = "https://2711944586.github.io/R/", target = "_blank",
        style = "color:var(--ghs-primary);text-decoration:none;font-weight:600;",
        "\u9759\u6001\u62a5\u544a")
    )
  )
)
