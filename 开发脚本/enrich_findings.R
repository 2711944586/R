# =============================================================================
# 开发脚本/enrich_findings.R
# 目标：
#   1. 修改 21_static_showcase.R — 删除 Asset Console 和 Figure Index
#   2. 让 Gallery 自动嵌入所有 300 张图（保留，但不再有索引跳转）
#   3. 修改 Findings 函数，使每个 Finding 引用更多图片和 widget
#   4. 在 21_static_showcase.R 中添加 "剩余图自动分配" 逻辑
# =============================================================================

# 策略：在 .ghs_gallery 中删除 asset_console 和 figure_index 的调用
# 然后在 .ghs_findings 的末尾，自动把未被引用的图分配到正文中

src <- readLines("程序/21_static_showcase.R", encoding = "UTF-8")

# ---- 1. 在 .ghs_gallery 函数中移除 asset_console 和 figure index ----
# 找到 asset_console 调用行
ac_line <- grep("asset_console <- \\.ghs_asset_console", src)
if (length(ac_line) == 1L) {
  src[ac_line] <- '  asset_console <- ""  # Asset Console removed'
}

# 找到 figure index sprintf 行，把 index 移除
# 查找 index <- sprintf 行
idx_start <- grep("index <- sprintf", src)
if (length(idx_start) == 1L) {
  # 修改 gallery 的 sprintf 调用，将 index 和 asset_console 替换为空字符串
  # 找到 gallery 的 sprintf 调用
  gallery_sprintf <- grep("ghs_section_note.*gallery.*asset_console.*index", src)
  if (length(gallery_sprintf) == 1L) {
    src[gallery_sprintf] <- sub("asset_console, index,", '"", "",', src[gallery_sprintf])
  }
}

# ---- 2. 在 .ghs_asset_console 函数开头直接返回空字符串 ----
acs_line <- grep("^\\.ghs_asset_console <- function", src)
if (length(acs_line) == 1L) {
  # 在函数开头添加 return("")
  src <- c(src[1:acs_line], '  return("")  # disabled: all assets now in Findings', src[(acs_line + 1):length(src)])
}

writeLines(src, "程序/21_static_showcase.R", useBytes = TRUE)
cat("[1/3] Disabled Asset Console and Figure Index\n")

# ---- 3. 创建自动分配脚本 42b_findings_batch.R ----
# 这个文件会在构建时自动把未引用的图和widget分配到Findings中

batch_code <- '
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
'

writeLines(batch_code, "程序/42b_findings_batch.R")
cat("[2/3] Created 42b_findings_batch.R with auto-assignment logic\n")

# ---- 4. 修改 21_static_showcase 的 .ghs_findings 函数 ----
# 在 .ghs_findings 末尾追加补充图的调用

# 重新读取修改后的文件
src <- readLines("程序/21_static_showcase.R", encoding = "UTF-8")

# 找到 .ghs_findings 函数中的末尾（返回 paste0 的地方）
# 我们需要在每个 Finding 的 HTML 输出后面追加补充图
# 更好的方法：在 .ghs_gallery 中去掉 index+console 后，
# 在 Gallery 之前插入一个"全景展示"章节，把所有未引用的图按主题展示

# 找到 .ghs_gallery 调用位置，在它之前插入补充逻辑
gallery_call_line <- grep('\\.ghs_gallery\\(fig_dir, widget_dir', src)
if (length(gallery_call_line) >= 1L) {
  gallery_call_line <- gallery_call_line[length(gallery_call_line)]  # 取最后一个（在 .ghs_render 中）
  
  # 在 gallery 调用前插入 supplement 调用
  supplement_code <- c(
    '    # 自动补充：将所有未被 Findings 引用的图和 widget 嵌入到 Gallery 前展示',
    '    if (exists("ghs_findings_supplement", mode = "function")) {',
    '      .ghs_supplement_section(fig_dir, widget_dir, mode, repo_url)',
    '    } else "",'
  )
  src <- c(src[1:(gallery_call_line - 1)], supplement_code, src[gallery_call_line:length(src)])
}

# 添加 .ghs_supplement_section 函数
# 找到 .ghs_gallery <- function 的定义位置
gallery_def <- grep("^\\.ghs_gallery <- function", src)
if (length(gallery_def) == 1L) {
  supplement_fn <- c(
    '',
    '.ghs_supplement_section <- function(fig_dir, widget_dir, mode, repo_url) {',
    '  # 列出所有图和widget',
    '  all_figs <- list.files(fig_dir, pattern = "[.]png$", full.names = FALSE)',
    '  all_widgets <- list.files(widget_dir, pattern = "[.]html$", full.names = FALSE)',
    '  ',
    '  # 如果 ghs_findings_supplement 存在就调用',
    '  if (!exists("ghs_findings_supplement", mode = "function")) return("")',
    '  if (!exists(".fb_assign_fig", mode = "function")) return("")',
    '  ',
    '  # 按主题分组展示',
    '  fig_map <- split(all_figs, vapply(all_figs, .fb_assign_fig, character(1)))',
    '  widget_map <- split(all_widgets, vapply(all_widgets, .fb_assign_widget, character(1)))',
    '  ',
    '  theme_labels <- c(',
    '    F1 = "\\u5168\\u7403\\u8d8b\\u52bf\\u4e0e\\u7b79\\u8d44\\u7ed3\\u6784",',
    '    F2 = "\\u5c45\\u6c11\\u81ea\\u4ed8\\u4e13\\u9898",',
    '    F3 = "\\u4e0d\\u5e73\\u7b49\\u4e0e\\u5206\\u5e03",',
    '    F4 = "\\u51b2\\u51fb\\u4e0e\\u6062\\u590d",',
    '    F5 = "\\u6536\\u655b\\u4e0e\\u8ffd\\u8d76",',
    '    F6 = "\\u5f39\\u6027\\u4e0e\\u6a21\\u578b",',
    '    F7 = "\\u805a\\u7c7b\\u4e0e\\u7c7b\\u578b\\u5b66",',
    '    F8 = "\\u9884\\u6d4b\\u4e0e\\u60c5\\u666f",',
    '    F9 = "\\u5916\\u63f4\\u4e0e\\u4f9d\\u8d56",',
    '    F10 = "\\u6548\\u7387\\u4e0e\\u524d\\u6cbf",',
    '    F11 = "\\u6392\\u540d\\u4e0e\\u56fd\\u5bb6",',
    '    F12 = "\\u5bff\\u547d\\u4e0e\\u4ea7\\u51fa",',
    '    F13 = "SDG-3 \\u8fdb\\u5c55",',
    '    F14 = "\\u6781\\u503c\\u4e0e\\u53d8\\u70b9",',
    '    F15 = "\\u8001\\u9f84\\u5316",',
    '    F16 = "\\u57ce\\u9547\\u5316",',
    '    F17 = "\\u8d22\\u653f\\u7a7a\\u95f4",',
    '    F18 = "\\u53ef\\u8d1f\\u62c5\\u6027",',
    '    F19 = "\\u533a\\u57df\\u534f\\u8bae",',
    '    F20 = "\\u901a\\u80c0\\u51b2\\u51fb",',
    '    F21 = "\\u75be\\u75c5\\u8d1f\\u62c5",',
    '    F22 = "UHC \\u8986\\u76d6",',
    '    F23 = "\\u707e\\u96be\\u6027\\u652f\\u51fa",',
    '    F24 = "\\u6bcd\\u5a74\\u5065\\u5eb7",',
    '    F25 = "\\u9884\\u9632\\u4e0e NCD",',
    '    F26 = "\\u536b\\u751f\\u4eba\\u529b",',
    '    F27 = "\\u6536\\u5165\\u664b\\u5347",',
    '    F28 = "\\u5206\\u89e3\\u5206\\u6790",',
    '    F29 = "\\u8106\\u5f31\\u56fd\\u5bb6",',
    '    F30 = "OECD vs LMIC",',
    '    F31 = "\\u6548\\u7387\\u8c61\\u9650",',
    '    F32 = "\\u63f4\\u52a9\\u6548\\u7387",',
    '    F33 = "\\u6570\\u636e\\u5b8c\\u6574\\u6027",',
    '    F34 = "\\u6570\\u636e\\u4fee\\u8ba2",',
    '    F35 = "\\u5c0f\\u5c9b\\u56fd",',
    '    F36 = "\\u7efc\\u5408\\u6307\\u6570"',
    '  )',
    '  ',
    '  sections <- vapply(names(fig_map), function(fid) {',
    '    figs <- fig_map[[fid]]',
    '    widgets <- if (!is.null(widget_map[[fid]])) widget_map[[fid]] else character(0)',
    '    if (!length(figs) && !length(widgets)) return("")',
    '    ',
    '    label <- if (fid %in% names(theme_labels)) theme_labels[[fid]] else fid',
    '    ',
    '    fig_cards <- vapply(figs, function(f) {',
    '      path <- file.path(fig_dir, f)',
    '      if (file.exists(path)) {',
    '        .ghs_fig(path, .ghs_pretty(path), .ghs_pretty(path))',
    '      } else ""',
    '    }, character(1))',
    '    ',
    '    wgt_cards <- vapply(widgets, function(w) {',
    '      if (exists(".ghs_widget_anchor", mode = "function")) {',
    '        .ghs_widget_anchor(widget_dir, w, .ghs_pretty(w), mode, repo_url)',
    '      } else ""',
    '    }, character(1))',
    '    ',
    '    paste0(',
    '      sprintf("<div class=\'finding-supplement\' data-finding=\'%s\'>", fid),',
    '      sprintf("<h4>%s \\u00b7 %s</h4>", fid, label),',
    '      paste(fig_cards[nzchar(fig_cards)], collapse = ""),',
    '      paste(wgt_cards[nzchar(wgt_cards)], collapse = ""),',
    '      "</div>")',
    '  }, character(1))',
    '  ',
    '  content <- paste(sections[nzchar(sections)], collapse = "")',
    '  if (!nzchar(content)) return("")',
    '  ',
    '  sprintf(paste0(',
    '    "<section class=\'section\' id=\'full-evidence\'>",',
    '    "<div class=\'wrap\'>",',
    '    "<header class=\'section-head\'>",',
    '    "<span class=\'kicker\'>EVIDENCE \\u00b7 FULL ATLAS</span>",',
    '    "<h2>\\u5168\\u91cf\\u8bc1\\u636e\\u5e93 \\u00b7 300 \\u5f20\\u56fe + 133 \\u4e2a\\u4ea4\\u4e92\\u7ec4\\u4ef6</h2>",',
    '    "<p class=\'lead\'>\\u4ee5\\u4e0b\\u5c55\\u793a\\u672c\\u62a5\\u544a\\u4ea7\\u51fa\\u7684\\u5168\\u90e8\\u9759\\u6001\\u56fe\\u8868\\u4e0e\\u4ea4\\u4e92\\u7ec4\\u4ef6\\uff0c\\u6309 36 \\u4e2a\\u6838\\u5fc3\\u53d1\\u73b0\\u7684\\u4e3b\\u9898\\u5f52\\u7c7b\\u3002\\u6bcf\\u5f20\\u56fe\\u5747\\u5df2\\u5728\\u4e0a\\u65b9\\u5bf9\\u5e94 Finding \\u4e2d\\u5f15\\u7528\\uff0c\\u6b64\\u5904\\u63d0\\u4f9b\\u5b8c\\u6574\\u7d22\\u5f15\\u3002</p>",',
    '    "</header>",',
    '    "%s",',
    '    "</div></section>"),',
    '    content)',
    '}',
    ''
  )
  src <- c(src[1:(gallery_def - 1)], supplement_fn, src[gallery_def:length(src)])
}

writeLines(src, "程序/21_static_showcase.R", useBytes = TRUE)
cat("[3/3] Added supplement section to 21_static_showcase.R\n")
cat("Done. Run: Rscript 构建.R submission\n")
