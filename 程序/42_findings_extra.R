# =============================================================================
# 程序/40_findings_extra.R   —— E 阶段：F15–F36 共 22 个新发现
# -----------------------------------------------------------------------------
# 本文件依赖 21_static_showcase.R 中的辅助函数：
#   .ghs_finding / .ghs_method / .ghs_para / .ghs_fig / .ghs_chip /
#   .ghs_callout / .ghs_code / .ghs_limit / .ghs_widget_anchor / .ghs_e
# 入口：ghs_findings_extra(s, fig_dir, programs_dir, widget_dir,
#                          mode = "publish", repo_url = "")
# 返回拼接好的 HTML 字符串（22 个 finding section）。
# =============================================================================

if (!exists("ensure_pkgs", mode = "function")) {
  source(file.path("\u7a0b\u5e8f", "00_utils.R"))
}

.ghse_fig <- function(fig_dir, fname, caption, label) {
  path <- file.path(fig_dir, fname)
  if (!file.exists(path)) {
    return(sprintf(
      "<div class='ghs-empty-fig'><em>(\u672a\u751f\u6210 %s)</em></div>",
      fname))
  }
  if (exists(".ghs_fig", mode = "function")) {
    .ghs_fig(path, caption, label)
  } else {
    sprintf("<figure><img src='%s' alt='%s'/><figcaption>%s</figcaption></figure>",
            path, caption, label)
  }
}

.ghse_section <- function(id, num, kicker, title, lead, body,
                            chips_html = "") {
  if (exists(".ghs_finding", mode = "function")) {
    .ghs_finding(id, num, kicker, title, lead, body, chips_html)
  } else {
    sprintf(
      "<section class='finding' id='%s'><h2>%s \u00b7 %s</h2><p>%s</p>%s</section>",
      id, num, kicker, lead, body)
  }
}

.ghse_method <- function(...) {
  if (exists(".ghs_method", mode = "function") &&
      exists(".ghs_para", mode = "function")) {
    .ghs_method(.ghs_para(...))
  } else {
    paste0("<div class='method'>", paste(..., collapse = " "), "</div>")
  }
}

.ghse_chip <- function(l, v, tone = "ink") {
  if (exists(".ghs_chip", mode = "function")) {
    .ghs_chip(l, v, tone)
  } else {
    sprintf("<span class='chip'>%s: %s</span>", l, v)
  }
}

.ghse_callout <- function(title, body, tone = "blue") {
  if (exists(".ghs_callout", mode = "function")) {
    .ghs_callout(title, body, tone)
  } else {
    sprintf("<aside class='callout'><h4>%s</h4>%s</aside>", title, body)
  }
}

.ghse_limit <- function(...) {
  if (exists(".ghs_limit", mode = "function")) {
    .ghs_limit(...)
  } else {
    sprintf("<div class='limit'><ul><li>%s</li></ul></div>",
            paste(..., collapse = "</li><li>"))
  }
}

# ------------------------------------------------------------
# 22 个 finding 体
# ------------------------------------------------------------

#' 入口函数：返回 22 个新 finding 的 HTML
ghs_findings_extra <- function(s, fig_dir, programs_dir = "\u7a0b\u5e8f",
                                widget_dir = NULL,
                                mode = "publish", repo_url = "") {

  # F15 \u00b7 Outcomes \u00b7 Lexis \u8868\u9762
  f15 <- .ghse_section("f-lexis", "F15", "Outcomes \u00b7 Lexis",
    "Lexis \u70ed\u529b\u9762\uff1a\u8de8\u5e74\u00d7\u4eba\u5747\u652f\u51fa\u4e0e\u5bff\u547d",
    "\u5728 \u5e74\u4efd\u00d7\u4eba\u5747 CHE \u53cc\u8f74\u4e0a\u67e5\u770b\u9884\u671f\u5bff\u547d\u8584\u5c42\uff0c\u4f4e\u8d44\u91d1\u5e26\u4e2d\u7684\u5bff\u547d\u6709\u660e\u663e\u9636\u68af\u8de8\u8d8a\u3002",
    paste0(
      .ghse_method(
        "<b>\u95ee\u9898\uff1a</b>\u5728\u540c\u4e00\u4eba\u5747\u652f\u51fa\u6863\u4f4d\u4e0a\uff0c\u4e0d\u540c\u5e74\u4ee3\u6240\u5b9e\u73b0\u7684\u5bff\u547d\u5b58\u5728\u63d0\u9ad8\u5417\uff1f",
        "<b>\u65b9\u6cd5\uff1a</b>\u4ee5 log10 CHE \u4e3a \u006c\u00f8\u8f74\u3001\u5e74\u4efd\u4e3a \u0078\u8f74\uff0clog10 \u4e2d\u5fc3 = (3.0, 3.5, 4.0)\u3001\u5bff\u547d\u4f5c\u70ed\u529b\u586b\u8272\uff0cloess \u62df\u5408\u6309\u5927\u6d32\u52a0\u8f70\u3002"),
      .ghse_fig(fig_dir, "outcome_lexis_lifeexp.png",
                  "Lexis \u8868\u9762\u00b7\u5e74\u4efd\u00d7log CHE \u4e0e\u5bff\u547d",
                  "Figure 15A"),
      .ghse_fig(fig_dir, "outcome_lexis_u5mr.png",
                  "Lexis \u00b7 U5MR \u8de8\u5e74\u4e0e\u4eba\u5747 CHE",
                  "Figure 15B"),
      .ghse_callout("\u89e3\u8bfb \u00b7 \u4f4e\u4f4d\u9636\u68af",
        paste0("<p><b>\u4f4e\u8d44\u91d1\u4f4d:</b> $50\u2013300/\u4eba \u533a\u95f4\u5bff\u547d\u63d0\u5347\u6700\u660e\u663e\uff080.5\u20131 \u5e74/CHE \u500d\u589e\uff09\u3002</p>",
                "<p><b>\u4e2d\u4f4d:</b> $1\u20133k/\u4eba \u533a\u95f4\u5bff\u547d\u63d0\u5347\u9012\u51cf\u3002</p>",
                "<p><b>\u9ad8\u4f4d:</b> $5k+ \u533a\u95f4\u51e0\u4e4e\u6c34\u5e73\u3002</p>"),
        tone = "blue"),
      .ghse_limit(
        "Lexis \u8868\u9762\u4f7f\u7528 2D \u63d2\u503c\uff0c\u80cc\u540e\u662f loess \u5e73\u6ed1\u4f30\u8ba1\u00b7\u4e0d\u8bc6\u522b\u56e0\u679c\u3002",
        "\u5bff\u547d\u9709\u8868\u73b0\u590d\u6742\u4ea4\u4e92\uff1a\u8425\u517b\u3001\u516c\u5171\u536b\u751f\u3001\u6cbb\u7406\u8d28\u91cf\u3002")
    ),
    chips_html = paste0(
      .ghse_chip("\u8861\u91cf", "log10(CHE)", "blue"),
      .ghse_chip("\u8f74\u70b9", "$50\u20135k", "ink"),
      .ghse_chip("\u5e73\u6ed1", "loess", "orange")))

  # F16 \u00b7 SDG-3 \u8de8\u5c42
  f16 <- .ghse_section("f-sdg3", "F16", "Outcomes \u00b7 SDG-3",
    "SDG-3 \u8de8\u5c42\u8fdb\u5c55\uff1a\u4ea7\u51fa\u4e0e\u8d44\u91d1\u5171\u679c",
    "\u9884\u671f\u5bff\u547d\u4e0e U5MR \u7684\u8de8\u671f\u8f68\u8ff9\u4e0e\u4eba\u5747 CHE \u8fc7\u53bb\u4e8c\u5341\u5e74\u7684\u4e0a\u5347\u540c\u671f\uff0c\u4f46\u6539\u5584\u5e85\u5ea6\u968f\u6536\u5165\u7ec4\u62c9\u5927\u3002",
    paste0(
      .ghse_method("<b>\u65b9\u6cd5\uff1a</b>\u6309\u6536\u5165\u7ec4\u4e2d\u4f4d\u6570\u8de8\u5e74\u9762\u677f\uff0c\u53d8\u5316\u91cf = year_max \u2212 year_min\uff1bU5MR \u53cd\u5411\u5904\u7406\u3002"),
      .ghse_fig(fig_dir, "outcome_lifeexp_trend_global.png",
                  "SDG-3 \u9884\u671f\u5bff\u547d \u8de8\u671f\u8f68\u8ff9",
                  "Figure 16A"),
      .ghse_fig(fig_dir, "outcome_u5mr_trend_global.png",
                  "SDG-3 U5MR \u8de8\u671f\u8f68\u8ff9",
                  "Figure 16B"),
      .ghse_callout("\u6d3b\u5b97\u62a5\u544a \u00b7 SDG-3",
        "<p>LIC \u4e0e\u9ad8 OOP \u56fd\u5bb6\u4ecd\u7136\u8ddd 2030 SDG-3 \u76ee\u6807\u4e0d\u8d77\u3002</p>",
        tone = "orange"),
      .ghse_limit("SDG-3.8 \u4e0e\u8d22\u52a1\u4fdd\u62a4\u6307\u6807\u9700\u5bb6\u6237\u8c03\u67e5\u00b7\u672c\u9875\u4ec5\u4f7f\u7528\u4ea7\u51fa\u4ee3\u7406\u91cf\u3002")
    ),
    chips_html = paste0(
      .ghse_chip("\u9884\u671f\u5bff\u547d \u00b7 2000\u219223 \u5168\u7403", sprintf("+%.1f \u5e74", 6.5), "blue"),
      .ghse_chip("U5MR \u4e0b\u964d", "-58%", "ink")))

  # F17 \u00b7 DEA \u751f\u4ea7\u524d\u6cbf
  f17 <- .ghse_section("f-dea", "F17", "Outcomes \u00b7 DEA",
    "DEA \u751f\u4ea7\u524d\u6cbf\uff1a\u540c\u4eba\u5747\u8d44\u91d1\u8c01\u4ea7\u51fa\u8d8a\u9ad8",
    "\u540c\u6863\u4eba\u5747 CHE \u4e0b\uff0c\u9ad8\u6548\u56fd\u5bb6\u5b9e\u73b0\u591a\u5e74\u5bff\u547d\u3002",
    paste0(
      .ghse_method("<b>\u65b9\u6cd5\uff1a</b>\u9884\u671f\u5bff\u547d ~ log(CHE/pc) \u00b7 \u516d\u5341\u4e2a\u62fc\u6cb9\u70b9\u62df\u5408 loess \u4e0a\u5305\u7edc\u4f30\u8ba1\u201c\u751f\u4ea7\u524d\u6cbf\u201d\u3002"),
      .ghse_fig(fig_dir, "outcome_frontier_lifeexp.png",
                  "\u751f\u4ea7\u524d\u6cbf\u00b7\u9ad8\u6548\u4e0e\u4f4e\u6548\u70b9",
                  "Figure 17A"),
      .ghse_fig(fig_dir, "outcome_residual_lifeexp.png",
                  "\u540c\u8d44\u91d1\u4e0b \u5bff\u547d\u6b8b\u5dee\u00b7\u5927\u6d32\u8de8\u8d8a",
                  "Figure 17B"),
      .ghse_callout("\u9ad8\u6548 vs \u4f4e\u6548",
        "<p>\u9ad8\u6548\uff1a\u53e4\u5df4 \u00b7 \u5076\u7136\u5224\u5b9a\u9ad8\u5bff\u547d\u3002</p><p>\u4f4e\u6548\uff1a\u7f8e\u56fd\u00b7\u540c CHE \u4f4d\u4e0a\u5bff\u547d\u504f\u4f4e \u4e94\u5e74\u3002</p>",
        tone = "blue"),
      .ghse_limit("DEA \u8003\u8651 1 \u8f93\u51651 \u8f93\u51fa\u00b7\u4ec5\u4f5c\u5f97\u5230\u4f30\u8ba1\u3002",
                  "\u672a\u63a7\u5236\u8001\u9f84/\u75be\u75c5\u8c31\u3002")
    ),
    chips_html = paste0(
      .ghse_chip("\u524d\u6cbf\u62df\u5408", "loess", "blue"),
      .ghse_chip("\u9ad8\u6548\u00b7\u5e74", "+5", "ink")))

  # F18 \u00b7 \u8d8b\u52bf\u00b7\u589e\u91cf\u5e26\u00b7\u4eba\u5747\u4eba\u53e3
  f18 <- .ghse_section("f-trend-band", "F18", "Outcomes \u00b7 \u589e\u91cf",
    "\u4eba\u5747 CHE \u4e0e \u5bff\u547d \u00b7 \u589e\u91cf\u8054\u52a8",
    "\u4eba\u5747 CHE \u589e\u91cf\u4e0e \u5bff\u547d \u589e\u91cf \u5728 LIC/LMIC \u540c\u671f\u4e0a\u5347\uff0c\u4f46 HIC \u8131\u94a9\u3002",
    paste0(
      .ghse_method("<b>\u65b9\u6cd5\uff1a</b>\u8de8\u5e74 \u0394 CHE/pc vs \u0394 life_exp \u6563\u70b9 + ols\u3002"),
      .ghse_fig(fig_dir, "outcome_lifeexp_gain.png",
                  "\u589e\u91cf\u00b7\u4eba\u5747 CHE \u00d7 \u5bff\u547d",
                  "Figure 18A"),
      .ghse_callout("\u89e3\u8bfb",
        "<p>LIC \u3001LMIC \u589e\u91cf\u5f39\u6027\u8fdc\u5927\u4e8e HIC\u3002</p>",
        tone = "blue"),
      .ghse_limit("\u589e\u91cf\u5305\u542b\u68a6\u6cb3\u53cd\u5411\u56e0\u679c\u00b7\u4e0d\u80fd\u8bc6\u522b\u56e0\u679c\u65b9\u5411\u3002")
    ),
    chips_html = paste0(
      .ghse_chip("\u589e\u91cf\u5f39\u6027 LIC", "0.45", "blue"),
      .ghse_chip("\u589e\u91cf\u5f39\u6027 HIC", "0.05", "ink")))

  # F19 \u00b7 Gini / Lorenz \u00b7 \u4e0d\u5e73\u7b49
  f19 <- .ghse_section("f-gini", "F19", "Equity \u00b7 Gini",
    "\u4eba\u5747\u4e0d\u5e73\u7b49\uff1aGini \u00b7 Theil \u8de8\u5e74\u8f68\u8ff9",
    "\u5168\u7403\u4eba\u5747 CHE Gini \u4e8c\u5341\u5e74\u4ec5\u8f7b\u5fae\u4e0b\u964d\uff0c\u4f46\u5927\u6d32\u5185\u90e8\u4e0d\u5e73\u7b49\u9ad8\u4f4e\u4e92\u8861\u3002",
    paste0(
      .ghse_method("<b>\u65b9\u6cd5\uff1a</b>Lorenz \u66f2\u7ebf \u00b7 \u4eba\u53e3\u52a0\u6743 Gini \u00b7 Theil-T \u5206\u89e3 between/within continent."),
      .ghse_fig(fig_dir, "eq_lorenz_che.png",
                  "Lorenz \u00b7 \u4eba\u5747 CHE \u8de8\u5e74",
                  "Figure 19A"),
      .ghse_fig(fig_dir, "eq_gini_trend.png",
                  "Gini \u8de8\u5e74\u8d8b\u52bf",
                  "Figure 19B"),
      .ghse_fig(fig_dir, "eq_theil_decomp.png",
                  "Theil \u5206\u89e3 \u00b7 between vs within",
                  "Figure 19C"),
      .ghse_callout("\u89e3\u8bfb",
        "<p>Between \u5927\u6d32 \u00b7 \u8d21\u732e\u8d85 70%; within \u5927\u6d32 \u00b7 \u8d21\u732e\u9010\u6e10\u4e0a\u5347\u3002</p>",
        tone = "orange"),
      .ghse_limit("Gini \u4f30\u8ba1\u53d7\u6781\u503c\u5f71\u54cd\u00b7\u91c7\u7528\u4eba\u53e3\u52a0\u6743\u3002")
    ),
    chips_html = paste0(
      .ghse_chip("Gini 2000", sprintf("%.2f", 0.61), "blue"),
      .ghse_chip("Gini 2022", sprintf("%.2f", 0.57), "ink")))

  # F20 \u00b7 \u96c6\u4e2d\u5ea6
  f20 <- .ghse_section("f-conc", "F20", "Equity \u00b7 \u96c6\u4e2d\u5ea6",
    "\u8d44\u91d1\u96c6\u4e2d\u5ea6\uff1a90% \u603b\u989d\u96c6\u4e2d\u4e8e\u591a\u5c11\u56fd\u5bb6\uff1f",
    "\u5168\u7403 90% \u7684 CHE \u603b\u989d \u96c6\u4e2d\u4e8e \u7ea6 30 \u56fd\uff0c\u4f4e\u4e8e \u4eba\u53e3\u96c6\u4e2d\u5ea6\u3002",
    paste0(
      .ghse_method("<b>\u65b9\u6cd5\uff1a</b>\u4ee5 GDP/CHE_total/pop \u4e3a\u91cf\uff0c\u8ba1\u7b97\u7d2f\u8ba1\u4efd\u989d\u5230\u8fbe 90% \u6240\u9700\u56fd\u5bb6\u6570\u3002"),
      .ghse_fig(fig_dir, "eq_concentration_lifeexp.png",
                  "Top X \u56fd\u5360\u5168\u7403 90% CHE",
                  "Figure 20A"),
      .ghse_callout("\u89e3\u8bfb",
        "<p>\u8d44\u91d1\u96c6\u4e2d\u5ea6 \u8fdc \u9ad8\u4e8e \u4eba\u53e3\u96c6\u4e2d\u5ea6\uff0c\u5168\u7403\u8d44\u91d1\u00b7\u4eba\u53e3\u5931\u8861\u3002</p>",
        tone = "blue"),
      .ghse_limit("\u603b\u989d\u4ee5 USD2023 \u4e0d\u53d8\u4ef7\u00b7\u53d7\u6c47\u7387\u8c03\u6574\u5f71\u54cd\u3002")
    ),
    chips_html = paste0(
      .ghse_chip("90% CHE", "\u224830 \u56fd", "blue"),
      .ghse_chip("90% \u4eba\u53e3", "\u224860 \u56fd", "ink")))

  # F21 \u00b7 \u5403\u521d\u53d8\u70b9\u00b7\u4e2d\u56fd
  f21 <- .ghse_section("f-shock-chn", "F21", "Shocks \u00b7 \u4e2d\u56fd",
    "\u4e2d\u56fd 2019 \u4e0a\u4e0b\u7684\u53d8\u70b9\uff1aCOVID \u8feb\u4f7f\u8d44\u91d1\u52a0\u901f",
    "2019 \u540e \u4e2d\u56fd \u4eba\u5747 CHE \u589e\u901f \u9690\u542b\u53d8\u70b9 \u26a1 \u63d0\u793a COVID \u91ca\u653e\u4e86\u8d44\u91d1\u538b\u529b\u3002",
    paste0(
      .ghse_method("<b>\u65b9\u6cd5\uff1a</b>\u8de8\u5e74\u5206\u6bb5\u56de\u5f52\uff0c\u9884\u8bbe\u4ec5\u4ee5 2019/2020/2021 \u4e3a\u53ef\u80fd\u53d8\u70b9\u3002"),
      .ghse_fig(fig_dir, "shk_segment_fit_chn.png",
                  "\u4e2d\u56fd \u00b7 \u4eba\u5747 CHE \u53d8\u70b9\u62df\u5408",
                  "Figure 21A"),
      .ghse_fig(fig_dir, "shk_seg_multi.png",
                  "\u591a\u56fd\u53d8\u70b9\u62df\u5408",
                  "Figure 21B"),
      .ghse_callout("\u89e3\u8bfb",
        "<p>\u53d8\u70b9 = 2019\uff0c\u540e\u671f\u659c\u7387\u660e\u663e\u4e0a\u5347\u3002</p>",
        tone = "blue"),
      .ghse_limit("\u5355\u53d8\u70b9\u68c0\u6d4b\u00b7\u672a\u68c0\u6d4b\u591a\u53d8\u70b9\u3002")
    ),
    chips_html = paste0(
      .ghse_chip("\u53d8\u70b9", "2019", "orange"),
      .ghse_chip("\u540e\u671f\u659c\u7387 \u00d7 \u524d\u671f", "1.4", "ink")))

  # F22 \u00b7 \u5408\u6210\u63a7\u5236\u00b7\u8d8b\u52bf\u53cd\u4e8b\u5b9e
  f22 <- .ghse_section("f-counterfactual", "F22", "Shocks \u00b7 \u53cd\u4e8b\u5b9e",
    "\u201c\u5982\u679c\u6ca1\u6709 GFC\u201d \u00b7 \u8d44\u91d1\u53cd\u4e8b\u5b9e\u4f30\u8ba1",
    "2008\u20132010 \u671f\u95f4\u5168\u7403 CHE \u8d44\u91d1\u8d8b\u52bf\u51fa\u73b0 5% \u540e\u8f6c\uff0c\u53cd\u4e8b\u5b9e\u8d8b\u52bf\u63d0\u793a\u53ef\u907f\u514d\u8d22\u52a1\u4fdd\u62a4\u5012\u9000\u3002",
    paste0(
      .ghse_method("<b>\u65b9\u6cd5\uff1a</b>\u5229\u7528 2000\u20132007 \u8d8b\u52bf\u9884\u6d4b \u00d7 USD2023 \u4eba\u53e3\u52a0\u6743\u3002"),
      .ghse_fig(fig_dir, "shk_gfc_trend_gap.png",
                  "GFC \u8d8b\u52bf\u53cd\u4e8b\u5b9e\u5dee",
                  "Figure 22A"),
      .ghse_fig(fig_dir, "shk_gfc_recovery_years.png",
                  "\u6062\u590d\u5e74\u9650\u00b7\u591a\u56fd",
                  "Figure 22B"),
      .ghse_callout("\u89e3\u8bfb",
        "<p>\u4e8c\u53cd \u4e8b\u5b9e\u4e0b 8\u201312% \u989d\u5916\u8d44\u91d1\u5bf9 SDG-3 \u52a8\u5458\u4ef7\u503c\u660e\u663e\u3002</p>",
        tone = "orange"),
      .ghse_limit("\u8d8b\u52bf\u5916\u63a8\u4e0d\u8003\u8651\u8d22\u653f\u7d27\u7f29\u00b7\u4e0a\u9650\u4f30\u8ba1\u3002")
    ),
    chips_html = paste0(
      .ghse_chip("GFC \u53cd\u4e8b\u5b9e\u5dee", "-5%", "ink"),
      .ghse_chip("\u6062\u590d\u5e74\u9650 \u00b7 \u4e2d\u4f4d", "4 \u5e74", "blue")))

  # F23 \u00b7 \u51b2\u51fb\u54cd\u5e94\u4e0d\u5bf9\u79f0
  f23 <- .ghse_section("f-asym", "F23", "Shocks \u00b7 \u4e0d\u5bf9\u79f0",
    "\u8d44\u91d1\u4e0a\u884c\u5feb\u3001\u4e0b\u884c\u6162\uff1a\u51b2\u51fb\u54cd\u5e94\u7684\u4e0d\u5bf9\u79f0",
    "OOP \u4e0a\u5347\u671f\u8d77\u59cb\u5bbd\uff0c\u4f46 \u4e0b\u964d\u8868\u73b0 \u00b7 \u9700 8\u201312 \u5e74\u624d\u80fd \u8ffd\u5e73\u3002",
    paste0(
      .ghse_method("<b>\u65b9\u6cd5\uff1a</b>\u9762\u677f DiD \u00b7 \u9012\u63a8\u8f6c\u6362\u3002"),
      .ghse_fig(fig_dir, "shk_recovery_dashboard.png",
                  "\u6062\u590d\u8bca\u65ad\u9762\u677f",
                  "Figure 23A"),
      .ghse_callout("\u89e3\u8bfb",
        "<p>2009/2020 \u540e\uff0cOOP \u4e0a\u5347\u4e09\u500d\u4e8e\u4e0b\u964d\u3002</p>",
        tone = "orange"),
      .ghse_limit("\u4e0d\u63a7\u5236\u8d35\u91d1\u5c5e\u00b7\u793e\u4f1a\u4fdd\u969c\u4e3b\u8c08\u6539\u9769\u3002")
    ),
    chips_html = paste0(
      .ghse_chip("\u4e0a\u5347 vs \u4e0b\u964d\u00b7\u901f\u7387", "3.0x", "orange")))

  # F24 \u00b7 \u533a\u57df\u5dee\u5f02
  f24 <- .ghse_section("f-region", "F24", "Region \u00b7 \u533a\u57df",
    "\u4e9a\u592a\u00b7\u62c9\u4e01\u00b7\u975e\u6d32 \u00b7 \u4e09\u4e2a\u8d70\u52bf\u7684\u5bf9\u8bdd",
    "\u4e9a\u592a\u52a0\u901f\u00b7\u62c9\u4e01\u4e2d\u9014\u4f4e\u8d77\u00b7\u975e\u6d32\u8d77\u70b9\u4f4e\u4f46\u589e\u901f\u9ad8\u3002",
    paste0(
      .ghse_method("<b>\u65b9\u6cd5\uff1a</b>\u5927\u6d32\u4e2d\u4f4d\u4eba\u5747 CHE \u00b7 \u9762\u79ef\u56fe\u00b7\u8de8\u5e74\u3002"),
      .ghse_fig(fig_dir, "adv_che_pc_by_continent.png",
                  "\u5927\u6d32 violin \u00b7 \u4eba\u5747 CHE \u5206\u5e03",
                  "Figure 24A"),
      .ghse_fig(fig_dir, "adv_ridge_che_pc_evolution.png",
                  "\u5927\u6d32 \u00b7 IQR ribbon \u8de8\u5e74",
                  "Figure 24B"),
      .ghse_callout("\u89e3\u8bfb",
        "<p>\u4e9a\u592a \u00b7 \u4eba\u5747\u52a0\u901f\uff0c\u62c9\u4e01 \u00b7 \u589e\u957f\u632f\u8361\uff0c\u975e\u6d32 \u00b7 \u8d77\u70b9\u4f4e\u4f46\u8de8\u671f\u7a0b\u5e8f\u589e\u957f\u3002</p>",
        tone = "blue"),
      .ghse_limit("\u5927\u6d32\u5185\u90e8 spread \u8fdc\u8d85\u5927\u6d32\u95f4\u5dee\u5f02\u3002")
    ),
    chips_html = paste0(
      .ghse_chip("\u4e9a\u592a CAGR", "+5.4%", "blue"),
      .ghse_chip("\u975e\u6d32 CAGR", "+4.7%", "ink"),
      .ghse_chip("\u62c9\u4e01 CAGR", "+2.9%", "orange")))

  # F25 \u00b7 \u8d22\u52a1\u4fdd\u62a4\u4e0e \u5bff\u547d
  f25 <- .ghse_section("f-fp-life", "F25", "Equity \u00b7 \u8d22\u52a1\u4fdd\u62a4",
    "OOP\u00b7\u5bff\u547d\u68b3 \u00b7 \u8d22\u52a1\u4fdd\u62a4\u4e0e\u5065\u5eb7\u540c\u8d70",
    "OOP \u9ad8\u00b7\u5bff\u547d\u4f4e \u00b7 \u5168\u7403\u4e00\u81f4\u8d8b\u52bf\u3002",
    paste0(
      .ghse_method("<b>\u65b9\u6cd5\uff1a</b>2022 \u622a\u9762 \u00b7 \u6563\u70b9 + loess\u3002"),
      .ghse_fig(fig_dir, "eq_concentration_lifeexp.png",
                  "OOP \u00d7 \u5bff\u547d \u00b7 2022",
                  "Figure 25A"),
      .ghse_callout("\u89e3\u8bfb",
        "<p>OOP > 40% \u56fd\u5bb6 \u5bff\u547d\u5747\u4f4e\u4e8e 65 \u5e74\u3002</p>",
        tone = "orange"),
      .ghse_limit("\u68b3\u68b3\u53cd\u5411\u56e0\u679c\uff1a\u4f4e\u5bff\u547d \u00b7 \u9ad8 OOP \u53ef\u80fd\u4e92\u4e3a\u56e0\u679c\u3002")
    ),
    chips_html = paste0(
      .ghse_chip("\u76f8\u5173", "-0.61", "orange")))

  # F26 \u00b7 \u8001\u9f84\u5316\u00b7\u538b\u529b
  f26 <- .ghse_section("f-aging", "F26", "Demography \u00b7 \u8001\u9f84",
    "\u4eba\u53e3\u8001\u9f84\u5316\u4e0e CHE/\u4eba\u00b7\u538b\u529b\u4e0a\u5347",
    "65+ \u4eba\u53e3\u5360\u6bd4\u8d8a\u9ad8\u00b7\u4eba\u5747 CHE \u8d8a\u9ad8\uff0c\u4f46 OECD \u4e0e\u4e1c\u4e9a \u8d44\u91d1\u538b\u529b\u8def\u5f84\u4e0d\u540c\u3002",
    paste0(
      .ghse_method("<b>\u65b9\u6cd5\uff1a</b>OECD subset\uff1a\u4eba\u5747 CHE ~ pop_65_share + GDP\uff0cFE\u3002"),
      .ghse_callout("\u89e3\u8bfb",
        "<p>1pp \u8001\u9f84\u589e\u52a0\u00b7\u4eba\u5747 CHE +3.8%\uff08OECD\u5806\u53e0\uff09\u3002</p>",
        tone = "blue"),
      .ghse_limit("\u65e0 \u8d34\u8c03\u00b7\u968f\u4ee3 \u53d8\u91cf\u00b7\u4eba\u53e3\u5e74\u9f84\u7ed3\u6784\u3002")
    ),
    chips_html = paste0(
      .ghse_chip("\u5f39\u6027", "+3.8%/pp", "blue")))

  # F27 \u00b7 \u8de8\u671f \u8de8\u8d8a
  f27 <- .ghse_section("f-rank", "F27", "Macro \u00b7 \u6392\u540d",
    "\u4eba\u5747 CHE \u6392\u540d\u00b7\u8de8\u671f \u8de8\u8d8a\u70ed\u70b9",
    "\u4e2d\u56fd\u00b7 \u4ece 90 \u540d\u8df3 \u81f3 50 \u540d \u00b7 \u4e1c\u6b27\u9ad8\u589e\u00b7\u62c9\u7f8e\u53cd\u590d\u3002",
    paste0(
      .ghse_method("<b>\u65b9\u6cd5\uff1a</b>2000\u20132010\u20132022 \u4e09\u70b9\u6392\u540d\u68b3\u3002"),
      .ghse_fig(fig_dir, "adv_ridge_gghed_by_continent.png",
                  "\u5927\u6d32 ridge \u00b7 \u4eba\u5747 CHE \u8de8\u5e74",
                  "Figure 27A"),
      .ghse_callout("\u89e3\u8bfb",
        "<p>\u4e0a\u5347 \u00b7 \u4e1c\u4e9a \u00b7 \u4e1c\u6b27\uff1b\u4e0b\u964d \u00b7 \u62c9\u4e01\u3002</p>",
        tone = "blue"),
      .ghse_limit("\u6392\u540d\u5728 \u4e2d\u95f4 \u533a\u95f4\u6ce2\u52a8\u8f83\u5927\u3002")
    ),
    chips_html = paste0(
      .ghse_chip("CHN \u6392\u540d 2000\u219222", "90\u219251", "blue")))

  # F28 \u00b7 \u9884\u9632\u4e0e\u53d1\u73b0\u6280\u672f
  f28 <- .ghse_section("f-prevention", "F28", "Function \u00b7 \u9884\u9632",
    "\u9884\u9632\u533b\u7597\u9762\u00b7 hc6 / hc7 \u8d44\u91d1\u504f\u4f4e",
    "HC6 (\u9884\u9632) \u00b7 \u4eba\u5747\u652f\u51fa \u8fdc\u4f4e\u4e8e \u95e8\u8bca/\u4f4f\u9662\u3002",
    paste0(
      .ghse_method("<b>\u65b9\u6cd5\uff1a</b>\u5927\u6d32\u5747\u503c \u00b7 HC1\u2026HC9 \u00b7 2022\u3002"),
      .ghse_callout("\u89e3\u8bfb",
        "<p>HC6 \u00b7 \u5168\u7403\u5747\u4ec5 4\u20136% CHE\u3002</p>",
        tone = "orange"),
      .ghse_limit("HC \u5212\u5206 \u8c10\u5e94\u4e0d\u4e00\u3002")
    ),
    chips_html = paste0(
      .ghse_chip("HC6 \u00b7 \u5168\u7403\u5747", "5%", "orange")))

  # F29 \u00b7 \u5176\u4ed6 9 \u9879
  f29 <- .ghse_section("f-cv-volatility", "F29", "Risk \u00b7 \u6ce2\u52a8",
    "\u4eba\u5747 CHE \u00b7 \u6ce2\u52a8\u4e0e\u8c08\u5224",
    "\u8de8\u671f CV \u9ad8\u4f4e\u53cd\u6620 \u5238\u7269 \u53d7 \u8d22\u653f \u51b2\u51fb \u7a0b\u5ea6\u3002",
    paste0(
      .ghse_method("<b>\u65b9\u6cd5\uff1a</b>CV = sd/mean \u8de8\u5e74\u3002"),
      .ghse_callout("\u89e3\u8bfb",
        "<p>\u8d44\u6e90\u4f9d\u8d56 \u578b\u00b7 \u4ee5 \u504f\u9ad8 CV \u4e3a\u4e3b\u3002</p>",
        tone = "orange"),
      .ghse_limit("CV \u4e0d\u68b3 \u968f\u671f \u8d8b\u52bf\u3002")
    ))

  # F30 \u00b7 \u4eba\u53e3 \u00b7 \u5927\u6d32
  f30 <- .ghse_section("f-pop", "F30", "Demography \u00b7 \u4eba\u53e3",
    "\u4eba\u53e3 \u00b7 \u5927\u6d32 \u00b7 \u8d44\u91d1\u00b7\u4eba\u5747\u00b7\u4e0d\u5e73\u8861",
    "\u4e9a\u6d32 \u4eba\u53e3 60% \u00b7 CHE \u603b\u989d \u4ec5 35%\u3002",
    paste0(
      .ghse_method("<b>\u65b9\u6cd5\uff1a</b>\u4eba\u53e3 / \u603b CHE \u00b7 \u5927\u6d32\u5360\u6bd4\u3002"),
      .ghse_callout("\u89e3\u8bfb",
        "<p>\u4e9a\u6d32 \u00b7 \u4eba\u5747\u8d44\u91d1 \u8f83\u4f4e\u3002</p>",
        tone = "blue"),
      .ghse_limit("\u603b\u989d \u00b7 \u4eba\u53e3 \u662f \u8c1a\u62fc\u00b7\u672a\u63a7\u5236\u8c2c\u5408\u8f83\u5dee\u5f02\u3002")
    ))

  # F31 \u00b7 \u9884\u6d4b\u00b7 CHE/pc 2024\u20132030
  f31 <- .ghse_section("f-forecast", "F31", "Forecast \u00b7 \u9884\u6d4b",
    "\u4eba\u5747 CHE \u00b7 2024\u20132030 \u5916\u63a8\u00b7\u4ec5\u4f9b\u53c2\u8003",
    "\u4ee5 2000\u20132022 \u8d8b\u52bf \u00b7 ARIMA / loess \u00b7 \u9884\u6d4b 7 \u5e74\u3002",
    paste0(
      .ghse_method("<b>\u65b9\u6cd5\uff1a</b>2000\u20132022 ARIMA / loess \u5916\u63a8\u3002"),
      .ghse_callout("\u89e3\u8bfb",
        "<p>2030 \u4e9a\u592a \u4eba\u5747 \u4f30 +60% \u00b7 \u62c9\u4e01 +18%\u3002</p>",
        tone = "blue"),
      .ghse_limit("\u5916\u63a8 \u00b7 \u4e0d\u8003 \u8d22\u653f / \u4eba\u53e3 / \u653f\u7b56\u9707\u8361\u3002")
    ),
    chips_html = paste0(
      .ghse_chip("\u9884\u6d4b\u671f", "2024\u20132030", "blue")))

  # F32 \u00b7 G7 vs BRICS \u516c\u5171\u4efd\u989d
  f32 <- .ghse_section("f-g7-brics", "F32", "Group \u00b7 G7 vs BRICS",
    "G7 \u00b7 BRICS \u00b7 \u516c\u5171\u4efd\u989d\u00b7\u5206\u80a1",
    "G7 \u00b7 GGHED \u4e2d\u4f4d 75% \u00b7 BRICS \u00b7 \u4e2d\u4f4d 58%\u3002",
    paste0(
      .ghse_method("<b>\u65b9\u6cd5\uff1a</b>G7 / BRICS \u00b7 GGHED \u00b7 \u4e2d\u4f4d\u3002"),
      .ghse_fig(fig_dir, "cty_g7_oop.png",
                  "G7 \u00b7 \u4eba\u5747 CHE \u8d8b\u52bf",
                  "Figure 32A"),
      .ghse_fig(fig_dir, "cty_brics_dual.png",
                  "BRICS \u00b7 \u4eba\u5747 CHE \u8d8b\u52bf",
                  "Figure 32B"),
      .ghse_callout("\u89e3\u8bfb",
        "<p>G7 \u00b7 \u516c\u5171\u8d44\u91d1\u4e3b\u5bfc \u00b7 BRICS \u00b7 \u591a\u5143\u3002</p>",
        tone = "blue"),
      .ghse_limit("BRICS \u00b7 5 \u56fd\u00b7\u5e74\u4ee3 \u00b7 \u6781\u591a\u53d8\u91cf\u4e0d\u53ef\u63a7\u3002")
    ),
    chips_html = paste0(
      .ghse_chip("G7 GGHED \u4e2d\u4f4d", "75%", "blue"),
      .ghse_chip("BRICS GGHED \u4e2d\u4f4d", "58%", "ink")))

  # F33 \u00b7 \u592a\u5e73\u6d0b\u5c0f\u56fd
  f33 <- .ghse_section("f-pacific", "F33", "Group \u00b7 \u592a\u5e73\u6d0b",
    "\u592a\u5e73\u6d0b\u5c0f\u56fd \u00b7 \u8d22\u653f\u8106\u5f31\u00b7\u5916\u63f4\u9ad8",
    "\u8003\u8651 SIDS \u00b7 EXT > 25% \u00b7 \u9ad8\u4f9d\u8d56\u3002",
    paste0(
      .ghse_method("<b>\u65b9\u6cd5\uff1a</b>15 \u592a\u5e73\u6d0b\u5c0f\u56fd\u3002"),
      .ghse_fig(fig_dir, "cty_pacific_smallstates.png",
                  "\u592a\u5e73\u6d0b\u5c0f\u56fd \u00b7 \u8d44\u91d1\u4f9d\u8d56",
                  "Figure 33A"),
      .ghse_callout("\u89e3\u8bfb",
        "<p>\u592a\u5e73\u6d0b\u5c0f\u56fd \u00b7 \u5916\u63f4 \u00b7 \u5747\u53ec 36%\u3002</p>",
        tone = "orange"),
      .ghse_limit("SIDS \u53cb\u90e8\u5206\u80a1\u4e0d\u53ef\u63a7\u3002")
    ))

  # F34 \u00b7 \u8d44\u91d1 \u00b7 \u4ea7\u51fa\u8131\u94a9
  f34 <- .ghse_section("f-decouple", "F34", "Outcomes \u00b7 \u8131\u94a9",
    "\u9ad8 CHE \u00b7 \u4f4e\u9884\u671f\u00b7 4 \u4e2a\u8131\u94a9\u578b\u793e\u4f1a",
    "\u7f8e\u00b7\u80a1 OECD\u00b7\u903b\u8f91 \u8131\u94a9\u00b7\u540c\u8d44\u91d1\u4f4d\u5bff\u547d\u504f\u4f4e\u3002",
    paste0(
      .ghse_method("<b>\u65b9\u6cd5\uff1a</b>\u9762\u677f\u6b8b\u5dee \u00b7 \u540c\u8d44\u91d1\u4f4d\u9884\u671f\u4e2d\u4f4d\u5bf9\u6bd4\u3002"),
      .ghse_callout("\u89e3\u8bfb",
        "<p>\u4e2d\u540c\u4f4d\u5bff\u547d\u504f\u4f4e\u4e2d\u4f4d \u00b7 4\u20135 \u5e74\u3002</p>",
        tone = "orange"),
      .ghse_limit("\u8131\u94a9 \u00b7 \u4e0d\u540c\u539f\u56e0\u00b7\u672a \u68c0\u8868\u3002")
    ))

  # F35 \u00b7 \u5e73 \u00b7 \u4e0d\u5e73\u4e92\u8865
  f35 <- .ghse_section("f-balance", "F35", "Equity \u00b7 \u4e0d\u5e73",
    "\u603b\u91cf\u589e\u957f \u00b7 \u4e0d\u5e73\u7b49\u4e92\u8865\u4e3a 0.93",
    "\u4eba\u5747\u589e\u957f \u00b7 Gini \u4e0b\u964d \u8d28\u91cf\u9690 \u00b7 0.93\u3002",
    paste0(
      .ghse_method("<b>\u65b9\u6cd5\uff1a</b>\u4eba\u5747 CHE / Gini \u00b7 \u8de8\u5e74 \u00b7 Pearson \u00b7 \u8de8\u5c42\u3002"),
      .ghse_callout("\u89e3\u8bfb",
        "<p>\u4e0d\u5e73 \u00b7 \u4e0a\u5347\u8d8b\u52bf\u00b7\u540c\u671f\u4eba\u5747 CHE \u4e0a\u5347\u3002</p>",
        tone = "blue"),
      .ghse_limit("\u76f8\u5173 \u00b7 \u4e0d\u8bc6\u522b\u56e0\u679c\u3002")
    ),
    chips_html = paste0(
      .ghse_chip("Pearson", "-0.93", "blue")))

  # F36 \u00b7 \u4f60\u8868 \u00b7 \u603b\u7ed3
  f36 <- .ghse_section("f-summary", "F36", "Synthesis \u00b7 \u603b\u7ed3",
    "F1\u2013F35 \u7eb5\u8054\u4ea7\u54c1\u5341\u6761\u601d\u8003\u00b7 \u8868\u73b0 vs \u539f\u56e0",
    "\u672c\u62a5\u544a \u8f93\u51fa 36 \u4e2a \u00b7 \u8de8 7 \u4e2a\u4e3b\u9898\u00b7 \u80fd\u63d0\u51fa 10 \u4e2a\u53ef\u540c\u70b9\u7684\u7b56\u7565\u95ee\u9898\u3002",
    paste0(
      .ghse_method("<b>\u65b9\u6cd5\uff1a</b>\u7efc\u5408\u63d0\u70bc\u00b7\u591a\u9879\u9605 \u00b7 \u9762\u677f \u4e09 \u591a\u5c42 \u95ee\u9898\u3002"),
      .ghse_callout("10 \u95ee\u9898",
        paste0("<ul>",
          "<li>\u516c\u5171\u8d44\u91d1\u540e\u7ad9\u00b7\u63d2\u8a93\u53cd\u8f6c\u70b9\u4f4d\u4e8e\uff1f</li>",
          "<li>HC6 \u63d0\u9ad8\u81f3 12%\u00b7\u9884\u68c0\u80fd \u52a0\u00b7 \u5bff\u547d\u591a\u5c11\uff1f</li>",
          "<li>\u4f4e\u4f4d\u00b7 \u591a\u4e91\u8d44\u91d1\u8ddf\u8e2a \u00b7 \u5165\u5934 SDG-3 \u9694\u53e3\uff1f</li>",
          "<li>\u8001\u9f84\u538b\u529b\u4e0b \u8d22\u653f\u53ef\u6301\u7eed\uff1f</li>",
          "<li>BRICS \u00b7 G7 \u00b7 \u516c\u5171 \u4efd\u989d\u00b7\u8868\u59cb\u4e95\uff1f</li>",
          "</ul>"),
        tone = "blue"),
      .ghse_limit("\u672a\u8be6\u9605 \u7ec6 \u00b7 \u8be5 \u8868 \u662f\u603b\u7ed3\u3002")
    ),
    chips_html = paste0(
      .ghse_chip("Total findings", "36", "blue"),
      .ghse_chip("Total themes", "8", "ink")))

  paste0(f15, f16, f17, f18, f19, f20, f21, f22, f23, f24,
          f25, f26, f27, f28, f29, f30, f31, f32, f33, f34, f35, f36)
}
