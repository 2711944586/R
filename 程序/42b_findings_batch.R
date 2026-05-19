
# =============================================================================
# 程序/42b_findings_batch.R
# -----------------------------------------------------------------------------
# 自动把所有未在 F1-F36 中引用的图片和交互组件嵌入到对应 Finding 正文中。
# 入口：ghs_findings_supplement(fig_dir, widget_dir, mode, repo_url, used_figs, used_widgets)
# =============================================================================

# 图片 → Finding 映射规则（基于文件名前缀/关键字）
.fb_assign_fig <- function(fname) {
  fn <- tolower(fname)
  # 地图类
  if (grepl("^map_|bivariate|cartogram|choropleth", fn)) {
    if (grepl("oops|oop", fn)) return("F2")
    if (grepl("gghed|gdp", fn)) return("F17")
    if (grepl("che_pc|chepc", fn)) return("F1")
    if (grepl("europe", fn)) return("F15")
    if (grepl("lifeexp|life_exp", fn)) return("F12")
    return("F1")
  }
  # 老龄化
  if (grepl("aging|age_65|old", fn)) return("F15")
  # 城镇化
  if (grepl("urban", fn)) return("F16")
  # 财政
  if (grepl("fiscal|gghed.*gdp|borrowing", fn)) return("F17")
  # 可负担性
  if (grepl("afford|price", fn)) return("F18")
  # 区域
  if (grepl("eu_|asean|au_|regional|bloc", fn)) return("F19")
  # 通胀
  if (grepl("inflat|nominal|real_growth", fn)) return("F20")
  # 疾病
  if (grepl("disease|ncd|burden|cause", fn)) return("F21")
  # UHC
  if (grepl("uhc|coverage|universal", fn)) return("F22")
  # 灾难性支出
  if (grepl("catastroph|threshold", fn)) return("F23")
  # 母婴
  if (grepl("maternal|child|u5mr|mmr|vaccin", fn)) return("F24")
  # 预防
  if (grepl("prevent|hc6|hale|daly", fn)) return("F25")
  # 卫生人力
  if (grepl("workforce|physician|nurse|doctor", fn)) return("F26")
  # 收入晋升
  if (grepl("reclassif|mobility|income_group.*change", fn)) return("F27")
  # Theil 分解
  if (grepl("theil|decompos|within.*between", fn)) return("F28")
  # 脆弱国家
  if (grepl("fragile|conflict|refugee", fn)) return("F29")
  # OECD vs LMIC
  if (grepl("oecd.*lmic|lmic.*oecd|policy_compare", fn)) return("F30")
  # 效率
  if (grepl("efficien|dea|frontier", fn)) return("F31")
  # 援助
  if (grepl("aid|ext_|external|donor", fn)) return("F32")
  # 数据质量
  if (grepl("missing|complete|quality|data_qual", fn)) return("F33")
  # 数据修订
  if (grepl("revision|revis", fn)) return("F34")
  # 小岛国
  if (grepl("small.*state|island|sids", fn)) return("F35")
  # 综合指数
  if (grepl("composite|index.*combin|score", fn)) return("F36")
  # COVID / 冲击
  if (grepl("covid|pandemic|shock|gfc|recover", fn)) return("F4")
  # 收敛
  if (grepl("converg|beta_conv|sigma", fn)) return("F5")
  # 弹性
  if (grepl("elast", fn)) return("F6")
  # 聚类
  if (grepl("cluster|pca|archetype|kmeans", fn)) return("F7")
  # 预测
  if (grepl("forecast|arima|fan|predict", fn)) return("F8")
  # 援助
  if (grepl("aid_|ext_dep|depend", fn)) return("F9")
  # 效率
  if (grepl("effic", fn)) return("F10")
  # 排名
  if (grepl("rank|bump", fn)) return("F11")
  # 寿命
  if (grepl("lifeexp|life_exp|sdg3|sdg_3", fn)) return("F13")
  # 极值
  if (grepl("extreme|jump|changepoint|outlier", fn)) return("F14")
  # 不平等
  if (grepl("inequal|gini|lorenz|equity|atkinson", fn)) return("F3")
  # OOPS
  if (grepl("oops|oop_|oop$|自付", fn)) return("F2")
  # 国家
  if (grepl("profile|country|cty_|chn|usa|ind|bra|nga|deu|jpn|zaf", fn)) return("F11")
  # 全球趋势/来源
  if (grepl("global|source|stream|continent|hf_share|area_stack", fn)) return("F1")
  # 分布类
  if (grepl("ridge|violin|beeswarm|density|distribut|box", fn)) return("F3")
  # 结构类
  if (grepl("sankey|treemap|waffle|ternary|purpose|sunburst|donut|polar", fn)) return("F1")
  # 模型类
  if (grepl("model|panel|robust|boot|quantile|gam", fn)) return("F6")
  # 时间类
  if (grepl("trend|timeline|slope|time_series", fn)) return("F1")
  # 其他
  return("F1")
}

.fb_assign_widget <- function(fname) {
  fn <- tolower(fname)
  if (grepl("aging|age", fn)) return("F15")
  if (grepl("urban", fn)) return("F16")
  if (grepl("fiscal|gghed.*gdp", fn)) return("F17")
  if (grepl("oops|oop|dumbbell", fn)) return("F2")
  if (grepl("lifeexp|life|u5mr|sdg", fn)) return("F13")
  if (grepl("che_gdp|world|global|hf", fn)) return("F1")
  if (grepl("forecast|fan|scenario", fn)) return("F8")
  if (grepl("inequal|gini|lorenz", fn)) return("F3")
  if (grepl("cluster|pca|archetype", fn)) return("F7")
  if (grepl("ext_|aid|donor", fn)) return("F9")
  if (grepl("efficien|dea", fn)) return("F10")
  if (grepl("covid|shock|recover", fn)) return("F4")
  if (grepl("rank|compare", fn)) return("F11")
  if (grepl("corr|matrix", fn)) return("F6")
  if (grepl("continent|region", fn)) return("F19")
  if (grepl("income|violin", fn)) return("F3")
  if (grepl("country|chn|china", fn)) return("F11")
  if (grepl("sankey|treemap|sunburst|chord|network|force", fn)) return("F1")
  if (grepl("heatmap|calendar", fn)) return("F14")
  if (grepl("dt_|reactable|rt_", fn)) return("F11")
  if (grepl("bar_race|animated|gapminder", fn)) return("F1")
  if (grepl("area|ribbon|stacked", fn)) return("F1")
  if (grepl("density|hist|box|violin", fn)) return("F3")
  if (grepl("bubble|scatter", fn)) return("F6")
  if (grepl("map|leaflet|choropleth", fn)) return("F1")
  return("F1")
}

# 生成补充图组 HTML（按 Finding 分组展示未引用的图）
ghs_findings_supplement <- function(fig_dir, widget_dir, mode, repo_url,
                                     used_figs = character(0),
                                     used_widgets = character(0)) {
  all_figs <- list.files(fig_dir, pattern = "[.]png$", full.names = FALSE)
  all_widgets <- list.files(widget_dir, pattern = "[.]html$", full.names = FALSE)

  # 排除已使用的
  unused_figs <- setdiff(all_figs, basename(used_figs))
  unused_widgets <- setdiff(all_widgets, basename(used_widgets))

  if (!length(unused_figs) && !length(unused_widgets)) return("")

  # 分配
  fig_map <- split(unused_figs, vapply(unused_figs, .fb_assign_fig, character(1)))
  widget_map <- split(unused_widgets, vapply(unused_widgets, .fb_assign_widget, character(1)))

  # 为每个 Finding 生成补充区块
  all_findings <- paste0("F", 1:36)
  parts <- vapply(all_findings, function(fid) {
    figs <- fig_map[[fid]]
    widgets <- widget_map[[fid]]
    if (!length(figs) && !length(widgets)) return("")

    fig_html <- ""
    if (length(figs)) {
      fig_cards <- vapply(figs, function(f) {
        path <- file.path(fig_dir, f)
        if (file.exists(path) && exists(".ghs_fig", mode = "function")) {
          .ghs_fig(path, .ghs_pretty(f), .ghs_pretty(f))
        } else ""
      }, character(1))
      fig_html <- paste(fig_cards[nzchar(fig_cards)], collapse = "")
    }

    widget_html <- ""
    if (length(widgets)) {
      wgt_cards <- vapply(widgets, function(w) {
        if (exists(".ghs_widget_anchor", mode = "function")) {
          .ghs_widget_anchor(widget_dir, w, .ghs_pretty(w), mode, repo_url)
        } else ""
      }, character(1))
      widget_html <- paste(wgt_cards[nzchar(wgt_cards)], collapse = "")
    }

    paste0(fig_html, widget_html)
  }, character(1))

  names(parts) <- all_findings
  # 返回命名列表供 findings 函数在对应位置插入
  parts
}

