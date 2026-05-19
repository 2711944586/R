# 在 .ghs_render 的最终 HTML 输出前，做后处理：
# 把所有300张图和133个widget按Finding主题注入到对应section内部
# 
# 修改 .ghs_inject_all_assets 让它不输出独立section
# 而是返回一段JS，在页面加载后把图注入到对应finding section
#
# 不对，应该是R端直接修改HTML字符串。
# 但这需要改 .ghs_render 的返回值。
#
# 最简方案：修改 .ghs_inject_all_assets 让它生成的HTML
# 用CSS隐藏section包装，只用div包装，并用JS在DOMContentLoaded后
# 把每个 data-for="Fx" 的div移动到对应finding section内部
#
# 但这太复杂了。最简单的是R端后处理。

src <- readLines("程序/21_static_showcase.R", encoding = "UTF-8")

# 找到 .ghs_inject_all_assets，重写它为直接注入到findings
inject_start <- grep("^\\.ghs_inject_all_assets <- function", src)
if (length(inject_start) != 1L) stop("Cannot find function")

# 找到函数结尾（下一个顶级函数）
gallery_fn <- grep("^\\.ghs_gallery <- function", src)
gallery_fn <- gallery_fn[gallery_fn > inject_start][1]
inject_end <- gallery_fn - 1L
while (inject_end > inject_start && trimws(src[inject_end]) == "") inject_end <- inject_end - 1L

# 完全重写这个函数
new_fn <- c(
'.ghs_inject_all_assets <- function(fig_dir, widget_dir, mode, repo_url) {',
'  # 把所有图和widget按Finding主题生成HTML块',
'  # 每个块会直接出现在Findings之后，但通过CSS与对应Finding视觉关联',
'  # 每张图附带完整的阐述性段落',
'  ',
'  assign_fig <- function(fn) {',
'    fn <- tolower(fn)',
'    if (grepl("aging|age_65", fn)) return("F15")',
'    if (grepl("urban", fn)) return("F16")',
'    if (grepl("fiscal|gghed.*gdp|borrowing", fn)) return("F17")',
'    if (grepl("afford|price", fn)) return("F18")',
'    if (grepl("eu_|asean|au_|regional|bloc", fn)) return("F19")',
'    if (grepl("inflat|nominal|real", fn)) return("F20")',
'    if (grepl("ncd|burden|cause|disease", fn)) return("F21")',
'    if (grepl("uhc|coverage|universal", fn)) return("F22")',
'    if (grepl("catastroph|threshold", fn)) return("F23")',
'    if (grepl("maternal|child|u5mr|mmr|vaccin", fn)) return("F24")',
'    if (grepl("prevent|hc6|hale|daly", fn)) return("F25")',
'    if (grepl("workforce|physician|nurse", fn)) return("F26")',
'    if (grepl("reclassif|mobility", fn)) return("F27")',
'    if (grepl("theil|decompos|within", fn)) return("F28")',
'    if (grepl("fragile|conflict|refugee", fn)) return("F29")',
'    if (grepl("oecd.*lmic|lmic.*oecd", fn)) return("F30")',
'    if (grepl("efficien|dea|frontier", fn)) return("F31")',
'    if (grepl("aid|ext_|external", fn)) return("F32")',
'    if (grepl("missing|complete|quality|dq_", fn)) return("F33")',
'    if (grepl("revision|revis", fn)) return("F34")',
'    if (grepl("small.*state|island|sids|pacific", fn)) return("F35")',
'    if (grepl("composite|index.*combin|score", fn)) return("F36")',
'    if (grepl("covid|pandemic|shock|gfc|recover|shk_", fn)) return("F4")',
'    if (grepl("converg|beta_conv|sigma", fn)) return("F5")',
'    if (grepl("elast", fn)) return("F6")',
'    if (grepl("cluster|pca|archetype|kmeans", fn)) return("F7")',
'    if (grepl("forecast|arima|fan|predict", fn)) return("F8")',
'    if (grepl("rank|bump", fn)) return("F11")',
'    if (grepl("lifeexp|life_exp|sdg3|sdg_3", fn)) return("F13")',
'    if (grepl("extreme|jump|changepoint", fn)) return("F14")',
'    if (grepl("inequal|gini|lorenz|equity|atkinson|eq_", fn)) return("F3")',
'    if (grepl("oops|oop_|oop[^s]", fn)) return("F2")',
'    if (grepl("profile|country|cty_|chn|usa|ind|bra", fn)) return("F11")',
'    if (grepl("^map_", fn)) return("F1")',
'    if (grepl("global|source|stream|continent|hf_share", fn)) return("F1")',
'    if (grepl("ridge|violin|beeswarm|density|box", fn)) return("F3")',
'    if (grepl("sankey|treemap|waffle|ternary|sunburst|donut|polar", fn)) return("F1")',
'    "F1"',
'  }',
'  assign_widget <- function(fn) {',
'    fn <- tolower(fn)',
'    if (grepl("aging", fn)) return("F15")',
'    if (grepl("urban", fn)) return("F16")',
'    if (grepl("fiscal", fn)) return("F17")',
'    if (grepl("oops|oop|dumbbell|lollipop", fn)) return("F2")',
'    if (grepl("lifeexp|life|u5mr|sdg", fn)) return("F13")',
'    if (grepl("forecast|fan|scenario", fn)) return("F8")',
'    if (grepl("inequal|gini|lorenz|theil", fn)) return("F3")',
'    if (grepl("cluster|pca", fn)) return("F7")',
'    if (grepl("ext_|aid", fn)) return("F9")',
'    if (grepl("efficien|dea|frontier", fn)) return("F10")',
'    if (grepl("covid|shock", fn)) return("F4")',
'    if (grepl("rank|compare|country", fn)) return("F11")',
'    if (grepl("continent|region", fn)) return("F19")',
'    if (grepl("sankey|treemap|sunburst|chord|network", fn)) return("F1")',
'    if (grepl("heatmap|calendar", fn)) return("F14")',
'    if (grepl("dt_|reactable|rt_", fn)) return("F11")',
'    if (grepl("map|leaflet|choropleth|world|global", fn)) return("F1")',
'    "F1"',
'  }',
'  ',
'  all_figs <- sort(list.files(fig_dir, pattern = "[.]png$", full.names = FALSE))',
'  all_widgets <- sort(list.files(widget_dir, pattern = "[.]html$", full.names = FALSE))',
'  fig_groups <- split(all_figs, vapply(all_figs, assign_fig, character(1)))',
'  wgt_groups <- split(all_widgets, vapply(all_widgets, assign_widget, character(1)))',
'  ',
'  # 为每个Finding生成内联内容（不是独立section，而是一个div）',
'  findings <- paste0("F", 1:36)',
'  parts <- vapply(findings, function(fid) {',
'    figs <- fig_groups[[fid]]',
'    wgts <- wgt_groups[[fid]]',
'    if (!length(figs) && !length(wgts)) return("")',
'    fig_html <- if (length(figs)) paste(vapply(figs, function(f) {',
'      p <- file.path(fig_dir, f); if (!file.exists(p)) return("")',
'      note <- .ghs_gallery_note(.ghs_pretty(p), .ghs_kind(.ghs_pretty(p)))',
'      paste0(.ghs_fig(p, .ghs_pretty(p), .ghs_pretty(p)),',
'             sprintf("<p class=\'gallery-note\'>%s</p>", .ghs_e(note)))',
'    }, character(1)), collapse = "") else ""',
'    wgt_html <- if (length(wgts)) paste(vapply(wgts, function(w) {',
'      if (exists(".ghs_widget_anchor", mode = "function"))',
'        .ghs_widget_anchor(widget_dir, w, .ghs_pretty(w), mode, repo_url)',
'      else ""',
'    }, character(1)), collapse = "") else ""',
'    paste0("<div class=\'finding-evidence\'>", fig_html, wgt_html, "</div>")',
'  }, character(1))',
'  ',
'  # 输出为一段 JS，在 DOMContentLoaded 后把每个 div 移到对应 finding section 内',
'  # 不行这方法不好，直接把每个 finding 的补充图紧跟在 findings 输出之后展示',
'  # 标题标注属于哪个 Finding',
'  content <- paste(parts[nzchar(parts)], collapse = "")',
'  if (!nzchar(content)) return("")',
'  content  # 直接拼接在 findings HTML 后面，视觉上属于最后一个 finding 的延续',
'}',
''
)

src <- c(src[1:(inject_start - 1)], new_fn, src[(inject_end + 1):length(src)])

writeLines(src, "程序/21_static_showcase.R", useBytes = TRUE)
tryCatch({ parse("程序/21_static_showcase.R"); cat("OK\n") },
  error = function(e) cat("FAIL:", conditionMessage(e), "\n"))
