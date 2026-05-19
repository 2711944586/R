# =============================================================================
# 程序/44_sections_extra.R   —— F 阶段：4 个新章节
# -----------------------------------------------------------------------------
# 函数集合：.ghs_section_outcomes / .ghs_section_equity / .ghs_section_country /
#           .ghs_section_shocks
# 入口：ghs_sections_extra(master, fig_dir, models_dir, widget_dir, mode)
# 返回拼接 HTML，由 21_static_showcase.R::.ghs_render 在末尾追加。
# =============================================================================

.ghsx_fig_or_empty <- function(fig_dir, fname, caption, label) {
  path <- file.path(fig_dir, fname)
  if (!file.exists(path)) {
    files <- list.files(fig_dir, pattern = "\\.png$", ignore.case = TRUE)
    suffix_hit <- files[endsWith(files, paste0("_", fname))]
    if (length(suffix_hit)) {
      path <- file.path(fig_dir, suffix_hit[[1]])
    }
  }
  if (file.exists(path) && exists(".ghs_fig", mode = "function")) {
    return(.ghs_fig(path, caption, label))
  }
  ""
}

.ghsx_section_wrap <- function(id, kicker, title, lead, body) {
  sprintf(paste0(
    "<section class='section section-extra' id='%s'>",
    "<div class='wrap'>",
    "<header class='section-head'>",
    "<span class='kicker'>%s</span>",
    "<h2>%s</h2>",
    "<p class='lead'>%s</p>",
    "</header>",
    if (exists(".ghs_section_note", mode = "function")) .ghs_section_note(id) else "",
    "%s",
    "</div>",
    "</section>"),
    id, kicker, title, lead, body)
}

#' 章节 1·产出与效率 · outcomes
.ghs_section_outcomes <- function(master, fig_dir) {
  body <- paste0(
    "<div class='atlas-grid'>",
    .ghsx_fig_or_empty(fig_dir, "outcome_lexis_lifeexp.png",
                        "Lexis \u8868\u9762\u00b7\u5e74\u00d7log CHE \u00b7 \u5bff\u547d",
                        "A"),
    .ghsx_fig_or_empty(fig_dir, "outcome_frontier_lifeexp.png",
                        "DEA \u751f\u4ea7\u524d\u6cbf\u00b7\u9ad8\u4f4e\u6548",
                        "B"),
    .ghsx_fig_or_empty(fig_dir, "outcome_lifeexp_trend_global.png",
                        "SDG-3 \u9884\u671f\u5bff\u547d \u8de8\u671f\u8f68\u8ff9",
                        "C"),
    .ghsx_fig_or_empty(fig_dir, "outcome_u5mr_trend_global.png",
                        "SDG-3 U5MR \u8de8\u671f\u8f68\u8ff9",
                        "D"),
    .ghsx_fig_or_empty(fig_dir, "outcome_elasticity_lifeexp.png",
                        "ln-ln \u5f39\u6027\u00b7\u5bff\u547d \u00b7 CHE",
                        "E"),
    .ghsx_fig_or_empty(fig_dir, "outcome_residual_lifeexp.png",
                        "\u540c\u8d44\u91d1\u4e0b \u5bff\u547d\u6b8b\u5dee",
                        "F"),
    "</div>")
  .ghsx_section_wrap("section-outcomes",
    "S18 \u00b7 Outcomes & Efficiency",
    "\u4ea7\u51fa\u4e0e\u6548\u7387\u4e13\u9898",
    "\u5728\u8d44\u91d1\u4e0d\u53d8\u7684\u524d\u63d0\u4e0b\u00b7\u4ea7\u51fa\u5982\u4f55\u8de8\u8d8a\uff1fLexis \u8868\u9762\u00b7DEA \u524d\u6cbf\u00b7SDG-3 \u8f68\u8ff9\u00b7\u5f39\u6027\u4e0e\u6b8b\u5dee \u56db\u4e2a\u4e92\u8865\u89c6\u89d2\u3002",
    body)
}

#' 章节 2·不平等与财务保护 · equity
.ghs_section_equity <- function(master, fig_dir) {
  body <- paste0(
    "<div class='atlas-grid'>",
    .ghsx_fig_or_empty(fig_dir, "eq_lorenz_che.png",
                        "Lorenz \u00b7 \u4eba\u5747 CHE \u8de8\u5e74", "A"),
    .ghsx_fig_or_empty(fig_dir, "eq_gini_trend.png",
                        "Gini \u00b7 \u8de8\u5e74\u8d8b\u52bf", "B"),
    .ghsx_fig_or_empty(fig_dir, "eq_theil_decomp.png",
                        "Theil \u5206\u89e3 \u00b7 \u5927\u6d32\u5185\u5916", "C"),
    .ghsx_fig_or_empty(fig_dir, "eq_concentration_lifeexp.png",
                        "Atkinson \u00b7 \u53c2\u6570 \u03b5", "D"),
    .ghsx_fig_or_empty(fig_dir, "eq_continent_cv.png",
                        "Top X \u56fd\u5360\u5168\u7403 90%", "E"),
    .ghsx_fig_or_empty(fig_dir, "eq_che_pc_iqr.png",
                        "\u4eba\u5747 CHE \u5206\u4f4d", "F"),
    .ghsx_fig_or_empty(fig_dir, "eq_oop_share_global.png",
                        "OOP \u00d7 \u5bff\u547d", "G"),
    .ghsx_fig_or_empty(fig_dir, "eq_oop_change.png",
                        "OOP \u5e74\u540c\u5757", "H"),
    "</div>")
  .ghsx_section_wrap("section-equity",
    "S19 \u00b7 Equity & Financial Protection",
    "\u4e0d\u5e73\u7b49\u4e0e\u8d22\u52a1\u4fdd\u62a4",
    "\u4ee5 Gini/Theil/Atkinson \u4e09\u6307\u6807\u8de8\u671f\u8de8\u5c42\u68b3\u7406\u5168\u7403\u536b\u751f\u652f\u51fa\u7684\u5206\u914d\u516c\u5e73\u6027\uff0c\u540c\u65f6\u5c06 OOP \u4e0e\u5065\u5eb7\u4ea7\u51fa\u8054\u63a5\uff0c\u8bc4\u4f30\u8d22\u52a1\u4fdd\u62a4\u4e0e\u5065\u5eb7\u7ed3\u679c\u7684\u534f\u540c\u5173\u7cfb\u3002",
    body)
}

#' 章节 3·国家专题 · country
.ghs_section_country <- function(master, fig_dir) {
  body <- paste0(
    "<div class='atlas-grid'>",
    .ghsx_fig_or_empty(fig_dir, "cty_brics_dual.png",
                        "BRICS \u00b7 \u4eba\u5747 CHE", "A"),
    .ghsx_fig_or_empty(fig_dir, "cty_g7_oop.png",
                        "G7 \u00b7 \u4eba\u5747 CHE", "B"),
    .ghsx_fig_or_empty(fig_dir, "cty_eu_vs_asean.png",
                        "EU \u00b7 OOP %", "C"),
    .ghsx_fig_or_empty(fig_dir, "cty_nordic_combo.png",
                        "\u592a\u5e73\u6d0b\u5c0f\u56fd\u00b7\u8d44\u91d1\u4f9d\u8d56", "D"),
    .ghsx_fig_or_empty(fig_dir, "cty_lowle_highle.png",
                        "\u4f4e\u5bff\u547d\u9ad8\u8d44\u91d1\u00b7\u8131\u94a9\u56fd\u5bb6", "E"),
    .ghsx_fig_or_empty(fig_dir, "cty_rank_trend_chn.png",
                        "\u8de8\u671f\u6392\u540d\u5927\u52a8 Top 25", "F"),
    "</div>")
  .ghsx_section_wrap("section-country",
    "S20 \u00b7 Country Spotlight",
    "\u56fd\u5bb6\u4e13\u9898\u6df1\u5165",
    "\u56db\u4e2a\u56fd\u5bb6\u96c6\u00b7BRICS / G7 / EU / Pacific SIDS \u4e0e\u8131\u94a9\u578b\u00b7\u63d0\u4f9b\u5907\u4ee3\u578b\u53c2\u8003\u3002",
    body)
}

#' 章节 4·冲击与变点 · shocks
.ghs_section_shocks <- function(master, fig_dir) {
  body <- paste0(
    "<div class='atlas-grid'>",
    .ghsx_fig_or_empty(fig_dir, "shk_covid_chepc.png",
                        "COVID \u00b7 \u4eba\u5747 CHE", "A"),
    .ghsx_fig_or_empty(fig_dir, "shk_covid_oop.png",
                        "COVID \u00b7 OOP", "B"),
    .ghsx_fig_or_empty(fig_dir, "shk_gfc_chepc.png",
                        "GFC \u00b7 \u4eba\u5747 CHE", "C"),
    .ghsx_fig_or_empty(fig_dir, "shk_gfc_trend_gap.png",
                        "GFC \u00b7 \u53cd\u4e8b\u5b9e\u5dee", "D"),
    .ghsx_fig_or_empty(fig_dir, "shk_seg_multi.png",
                        "\u53d8\u70b9 \u00b7 \u591a\u56fd\u62df\u5408", "E"),
    .ghsx_fig_or_empty(fig_dir, "shk_recovery_dashboard.png",
                        "\u6062\u590d\u8bca\u65ad\u9762\u677f", "F"),
    "</div>")
  .ghsx_section_wrap("section-shocks",
    "S21 \u00b7 Shocks & Change-points",
    "\u51b2\u51fb\u54cd\u5e94\u4e0e\u53d8\u70b9",
    "GFC + COVID \u4e24\u8f6e\u5168\u7403\u51b2\u51fb \u00b7 \u8d44\u91d1\u4e0a\u884c\u5feb\u3001\u4e0b\u884c\u6162 \u00b7 \u591a\u56fd\u53d8\u70b9\u62df\u5408 \u00b7 \u6062\u590d\u8bca\u65ad\u3002",
    body)
}

#' 入口：返回 4 个新章节 HTML
ghs_sections_extra <- function(master, fig_dir, models_dir,
                                widget_dir = NULL,
                                mode = "publish", repo_url = "") {
  if (is.null(widget_dir))
    widget_dir <- file.path(dirname(fig_dir), "\u4ea4\u4e92\u7ec4\u4ef6")
  paste0(
    .ghs_section_outcomes(master, fig_dir),
    .ghs_section_equity(master, fig_dir),
    .ghs_section_country(master, fig_dir),
    .ghs_section_shocks(master, fig_dir))
}
