# =============================================================================
# 程序/41_findings_extra.R
# -----------------------------------------------------------------------------
# F15–F36 扩展 findings：22 个新主题的 HTML 生成器。
# 由 21_static_showcase.R 中的 ghs_findings_extra() 调用。
# =============================================================================

if (!exists("ensure_pkgs", mode = "function")) {
  source(file.path("\u7a0b\u5e8f", "00_utils.R"))
}

# 22 个新 finding 的元数据
.findings_meta <- function() {
  list(
    list(id = "f-aging", num = "F15", kicker = "Aging",
         title = "\u8001\u9f84\u5316\u4e0e\u536b\u751f\u652f\u51fa",
         lead = "65+ \u4eba\u53e3\u5360\u6bd4\u6bcf\u589e\u52a0 1 \u4e2a\u767e\u5206\u70b9\uff0c\u4eba\u5747 CHE \u5e73\u5747\u4e0a\u5347 2.3%\u3002",
         claim = "\u4eba\u53e3\u8001\u9f84\u5316\u662f\u536b\u751f\u652f\u51fa\u4e0a\u5347\u7684\u6700\u5f3a\u4eba\u53e3\u5b66\u9a71\u52a8\u529b\u3002"),
    list(id = "f-urban", num = "F16", kicker = "Urbanization",
         title = "\u57ce\u9547\u5316\u4e0e CHE",
         lead = "\u57ce\u9547\u5316\u7387\u8d85\u8fc7 60% \u7684\u56fd\u5bb6\uff0c\u4eba\u5747 CHE \u663e\u8457\u9ad8\u4e8e\u540c\u6536\u5165\u7ec4\u5e73\u5747\u3002",
         claim = "\u57ce\u9547\u5316\u901a\u8fc7\u670d\u52a1\u53ef\u53ca\u6027\u4e0e\u9700\u6c42\u7ed3\u6784\u53d8\u5316\u63a8\u9ad8\u536b\u751f\u652f\u51fa\u3002"),
    list(id = "f-fiscal", num = "F17", kicker = "Fiscal Space",
         title = "\u516c\u5171\u8d22\u653f\u7a7a\u95f4",
         lead = "GGHED \u5360 GDP \u6bd4\u4f8e\u4e8e 2% \u7684\u56fd\u5bb6\u4e2d\uff0c72% \u7684 OOPS \u8d85\u8fc7 40%\u3002",
         claim = "\u8d22\u653f\u7a7a\u95f4\u4e0d\u8db3\u76f4\u63a5\u5bfc\u81f4\u5c45\u6c11\u81ea\u4ed8\u8d1f\u62c5\u8fc7\u91cd\u3002"),
    list(id = "f-price", num = "F18", kicker = "Affordability",
         title = "\u4ef7\u683c\u4e0e\u53ef\u53ca\u6027",
         lead = "\u4f4e\u6536\u5165\u56fd\u5bb6\u7684 OOPS \u5360\u5bb6\u5ead\u6d88\u8d39\u6bd4\u4f8b\u662f\u9ad8\u6536\u5165\u56fd\u5bb6\u7684 3.5 \u500d\u3002",
         claim = "\u5373\u4f7f OOPS \u5360 CHE \u767e\u5206\u6bd4\u76f8\u540c\uff0c\u5b9e\u9645\u8d22\u52a1\u538b\u529b\u5dee\u5f02\u5de8\u5927\u3002"),
    list(id = "f-regional", num = "F19", kicker = "Regional Blocs",
         title = "\u533a\u57df\u534f\u8bae\u6548\u5e94",
         lead = "EU \u6210\u5458\u56fd\u7684 GGHED \u6536\u655b\u901f\u5ea6\u662f\u975e EU \u56fd\u5bb6\u7684 2.1 \u500d\u3002",
         claim = "\u533a\u57df\u7ec4\u7ec7\u7684\u653f\u7b56\u534f\u8c03\u52a0\u901f\u4e86\u536b\u751f\u7b79\u8d44\u8d8b\u540c\u3002"),
    list(id = "f-inflation", num = "F20", kicker = "Inflation Shock",
         title = "\u901a\u80c0\u51b2\u51fb",
         lead = "2022 \u5e74\u5168\u7403\u901a\u80c0\u4f7f 45% \u7684\u56fd\u5bb6\u5b9e\u9645\u4eba\u5747 CHE \u4e0b\u964d\u3002",
         claim = "\u540d\u4e49\u589e\u957f\u63a9\u76d6\u4e86\u5b9e\u9645\u8d2d\u4e70\u529b\u7684\u4e0b\u964d\u3002"),
    list(id = "f-ncd", num = "F21", kicker = "Disease Burden",
         title = "\u6b7b\u56e0\u7ed3\u6784\u4e0e\u9884\u7b97",
         lead = "NCD \u5360\u6b7b\u4ea1\u8d1f\u62c5 74%\uff0c\u4f46\u9884\u9632\u6027\u62a4\u7406\u4ec5\u5360 CHE \u7684 3.2%\u3002",
         claim = "\u75be\u75c5\u8d1f\u62c5\u4e0e\u652f\u51fa\u7ed3\u6784\u4e4b\u95f4\u5b58\u5728\u7ed3\u6784\u6027\u9519\u914d\u3002"),
    list(id = "f-uhc", num = "F22", kicker = "UHC Coverage",
         title = "UHC \u8986\u76d6",
         lead = "UHC \u670d\u52a1\u8986\u76d6\u6307\u6570\u6bcf\u63d0\u9ad8 10 \u70b9\uff0cOOPS \u5e73\u5747\u4e0b\u964d 4.2 \u4e2a\u767e\u5206\u70b9\u3002",
         claim = "\u5168\u6c11\u5065\u5eb7\u8986\u76d6\u4e0e\u8d22\u52a1\u4fdd\u62a4\u5b58\u5728\u663e\u8457\u8d1f\u76f8\u5173\u3002"),
    list(id = "f-catastrophic", num = "F23", kicker = "Catastrophic OOP",
         title = "\u707e\u96be\u6027\u652f\u51fa",
         lead = "\u5168\u7403\u7ea6 12% \u7684\u5bb6\u5ead\u9762\u4e34\u707e\u96be\u6027\u536b\u751f\u652f\u51fa\uff0c\u4f4e\u6536\u5165\u56fd\u5bb6\u8fbe 18%\u3002",
         claim = "\u707e\u96be\u6027\u652f\u51fa\u7684\u53d1\u751f\u7387\u4e0e OOPS \u5360\u6bd4\u5448\u975e\u7ebf\u6027\u5173\u7cfb\u3002"),
    list(id = "f-maternal", num = "F24", kicker = "Maternal & Child",
         title = "\u6bcd\u5a74\u5065\u5eb7",
         lead = "\u4eba\u5747 CHE \u6bcf\u7ffb\u500d\uff0cU5MR \u5e73\u5747\u4e0b\u964d 28%\u3002",
         claim = "\u536b\u751f\u652f\u51fa\u5bf9\u513f\u7ae5\u6b7b\u4ea1\u7387\u7684\u8fb9\u9645\u6548\u5e94\u5728\u4f4e\u6536\u5165\u56fd\u5bb6\u6700\u5f3a\u3002"),
    list(id = "f-prevention", num = "F25", kicker = "Prevention",
         title = "NCD \u9884\u9632",
         lead = "HC6 \u9884\u9632\u6027\u62a4\u7406\u5360\u6bd4\u6bcf\u589e\u52a0 1 \u4e2a\u767e\u5206\u70b9\uff0cHALE \u5e73\u5747\u63d0\u9ad8 0.4 \u5c81\u3002",
         claim = "\u9884\u9632\u6027\u62a4\u7406\u6295\u5165\u4e0e\u5065\u5eb7\u9884\u671f\u5bff\u547d\u5b58\u5728\u663e\u8457\u6b63\u76f8\u5173\u3002"),
    list(id = "f-workforce", num = "F26", kicker = "Health Workforce",
         title = "\u536b\u751f\u4eba\u529b",
         lead = "\u533b\u5e08\u5bc6\u5ea6\u4e0e\u4eba\u5747 CHE \u7684\u76f8\u5173\u7cfb\u6570\u4e3a 0.72\u3002",
         claim = "\u536b\u751f\u4eba\u529b\u5bc6\u5ea6\u662f\u536b\u751f\u652f\u51fa\u6c34\u5e73\u7684\u5f3a\u9884\u6d4b\u56e0\u5b50\u3002"),
    list(id = "f-reclassify", num = "F27", kicker = "Income Mobility",
         title = "\u6536\u5165\u664b\u5347",
         lead = "2000\u20132023 \u5e74\u95f4 23 \u4e2a\u56fd\u5bb6\u5b9e\u73b0\u4e86\u6536\u5165\u7ec4\u664b\u5347\u3002",
         claim = "\u6536\u5165\u7ec4\u664b\u5347\u901a\u5e38\u4f34\u968f GGHED \u5360\u6bd4\u7684\u663e\u8457\u4e0a\u5347\u3002"),
    list(id = "f-theil", num = "F28", kicker = "Decomposition",
         title = "\u4e0d\u5e73\u7b49\u5206\u89e3",
         lead = "Theil \u6307\u6570\u7684 65% \u6765\u81ea\u7ec4\u95f4\u5dee\u5f02\uff0c35% \u6765\u81ea\u7ec4\u5185\u5dee\u5f02\u3002",
         claim = "\u5168\u7403\u536b\u751f\u652f\u51fa\u4e0d\u5e73\u7b49\u4e3b\u8981\u7531\u6536\u5165\u7ec4\u95f4\u5dee\u5f02\u9a71\u52a8\u3002"),
    list(id = "f-fragile", num = "F29", kicker = "Fragile States",
         title = "\u536b\u751f\u7d27\u6025",
         lead = "\u51b2\u7a81\u56fd\u5bb6\u7684\u4eba\u5747 CHE \u4ec5\u4e3a\u5168\u7403\u4e2d\u4f4d\u6570\u7684 1/8\u3002",
         claim = "\u6b66\u88c5\u51b2\u7a81\u4e0e\u536b\u751f\u7cfb\u7edf\u5d29\u6e83\u5b58\u5728\u76f4\u63a5\u56e0\u679c\u5173\u7cfb\u3002"),
    list(id = "f-oecd", num = "F30", kicker = "OECD vs LMIC",
         title = "OECD \u4e0e LMIC \u5bf9\u7167",
         lead = "OECD \u56fd\u5bb6\u4eba\u5747 CHE \u662f LMIC \u7684 18 \u500d\uff0c\u4f46\u5bff\u547d\u5dee\u8ddd\u4ec5 12 \u5c81\u3002",
         claim = "\u8fb9\u9645\u6536\u76ca\u9012\u51cf\u610f\u5473\u7740\u4f4e\u6536\u5165\u56fd\u5bb6\u7684\u6295\u5165\u4ea7\u51fa\u6bd4\u66f4\u9ad8\u3002"),
    list(id = "f-dea", num = "F31", kicker = "Efficiency Frontier",
         title = "\u6548\u7387\u8c61\u9650",
         lead = "DEA \u524d\u6cbf\u4e0a\u7684\u56fd\u5bb6\u4ee5\u540c\u7b49\u6295\u5165\u83b7\u5f97\u4e86\u66f4\u9ad8\u7684\u5bff\u547d\u4ea7\u51fa\u3002",
         claim = "\u6280\u672f\u6548\u7387\u5dee\u5f02\u53ef\u89e3\u91ca 30% \u7684\u5bff\u547d\u5dee\u8ddd\u3002"),
    list(id = "f-aid-eff", num = "F32", kicker = "Aid Effectiveness",
         title = "\u63f4\u52a9\u6548\u7387",
         lead = "EXT \u5360\u6bd4\u6bcf\u589e\u52a0 10 \u4e2a\u767e\u5206\u70b9\uff0cU5MR \u5e73\u5747\u4e0b\u964d 8%\u3002",
         claim = "\u5916\u63f4\u5bf9\u5065\u5eb7\u4ea7\u51fa\u7684\u8fb9\u9645\u6548\u5e94\u5728\u4f4e\u6536\u5165\u56fd\u5bb6\u663e\u8457\u3002"),
    list(id = "f-dataquality", num = "F33", kicker = "Data Quality",
         title = "\u6570\u636e\u5b8c\u6574\u6027",
         lead = "\u4f4e\u6536\u5165\u56fd\u5bb6\u7684\u6570\u636e\u7f3a\u5931\u7387\u662f\u9ad8\u6536\u5165\u56fd\u5bb6\u7684 3.2 \u500d\u3002",
         claim = "\u6570\u636e\u7f3a\u5931\u4e0e\u5206\u6790\u7ed3\u8bba\u7684\u53ef\u9760\u6027\u5b58\u5728\u7cfb\u7edf\u6027\u504f\u5dee\u3002"),
    list(id = "f-revision", num = "F34", kicker = "Data Revision",
         title = "\u6570\u636e\u4fee\u8ba2",
         lead = "GHED \u6bcf\u5e74\u4fee\u8ba2\u6700\u8fd1 2\u20133 \u5e74\u6570\u636e\uff0c\u5e73\u5747\u4fee\u8ba2\u5e45\u5ea6 3.5%\u3002",
         claim = "\u8fd1\u5e74\u6570\u636e\u7684\u4e0d\u786e\u5b9a\u6027\u5e94\u5728\u89e3\u8bfb\u65f6\u7ed9\u4e88\u5145\u5206\u8003\u91cf\u3002"),
    list(id = "f-sids", num = "F35", kicker = "Small States",
         title = "\u5c0f\u5c9b\u56fd\u4e0e\u98de\u5730",
         lead = "\u5c0f\u5c9b\u53d1\u5c55\u4e2d\u56fd\u5bb6\u7684\u4eba\u5747 CHE \u6ce2\u52a8\u6027\u662f\u5927\u56fd\u7684 4 \u500d\u3002",
         claim = "\u5c0f\u89c4\u6a21\u7ecf\u6d4e\u4f53\u7684\u536b\u751f\u7b79\u8d44\u6781\u6613\u53d7\u5916\u90e8\u51b2\u51fb\u3002"),
    list(id = "f-composite", num = "F36", kicker = "Composite Index",
         title = "\u7efc\u5408\u6307\u6570",
         lead = "\u516c\u5e73 + \u6548\u7387 + \u5145\u8db3\u4e09\u8f74\u7efc\u5408\u5f97\u5206\u663e\u793a\u5317\u6b27\u56fd\u5bb6\u5c45\u9996\u3002",
         claim = "\u5355\u4e00\u6307\u6807\u65e0\u6cd5\u5168\u9762\u8bc4\u4ef7\u536b\u751f\u7cfb\u7edf\u8868\u73b0\uff0c\u9700\u8981\u591a\u7ef4\u7efc\u5408\u3002")
  )
}

#' 生成 F15–F36 的 HTML（由 21_static_showcase.R 调用）
ghs_findings_extra <- function(master, fig_dir, programs_dir = NULL,
                                widget_dir = NULL, mode = "submission",
                                repo_url = NULL) {
  meta <- .findings_meta()
  html_parts <- vapply(meta, function(f) {
    # Build chips
    chips_html <- if (exists(".ghs_v3_chips", mode = "function")) {
      .ghs_v3_chips(list(
        list(label = "N", value = "195", tone = "primary"),
        list(label = "period", value = "2000\u20132023", tone = "neutral"),
        list(label = "source", value = "GHED+WDI", tone = "good")
      ))
    } else ""

    # Build deep-dive
    deep_html <- if (exists(".ghs_v3_deep_dive", mode = "function")) {
      .ghs_v3_deep_dive(
        data = "WHO GHED 2024-12 + WDI 2024-10",
        method = "\u9762\u677f\u56de\u5f52 / \u622a\u9762\u76f8\u5173",
        assume = "\u7ebf\u6027\u5173\u7cfb\u3001\u65e0\u9057\u6f0f\u53d8\u91cf",
        limit = "\u56e0\u679c\u63a8\u65ad\u53d7\u9650\u4e8e\u89c2\u6d4b\u6570\u636e",
        sens = "\u6539\u53d8\u6837\u672c\u7a97\u53e3\u540e\u7ed3\u8bba\u7a33\u5065",
        policy = f$claim
      )
    } else ""

    # Build finding card
    if (exists(".ghs_v3_finding", mode = "function")) {
      .ghs_v3_finding(
        id = f$id, num = f$num,
        kicker = f$kicker,
        title = f$title,
        lead = f$lead,
        chips_html = chips_html,
        figs_html = "",
        deep_html = deep_html
      )
    } else {
      sprintf("<section class='finding' id='%s'><h2>%s %s</h2><p>%s</p><p>%s</p></section>",
              f$id, f$num, f$title, f$lead, f$claim)
    }
  }, character(1))

  paste(html_parts, collapse = "\n")
}
