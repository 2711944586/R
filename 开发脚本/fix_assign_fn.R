# 把 .fb_assign_fig 和 .fb_assign_widget 放回 21_static_showcase 中
# 放在 .ghs_inject_all_assets 函数内部作为局部函数

src <- readLines("程序/21_static_showcase.R", encoding = "UTF-8")

# 找到 .ghs_inject_all_assets 函数中的 "if (!exists" 检查行
check_ln <- grep('if \\(!exists\\("\\.fb_assign_fig"', src)
if (length(check_ln) == 1L) {
  # 替换这个检查为内联定义
  replacement <- c(
  '  # 内联定义分配规则',
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
  '    if (grepl("income|violin", fn)) return("F3")',
  '    if (grepl("sankey|treemap|sunburst|chord|network", fn)) return("F1")',
  '    if (grepl("heatmap|calendar", fn)) return("F14")',
  '    if (grepl("dt_|reactable|rt_", fn)) return("F11")',
  '    if (grepl("bar_race|animated|gapminder", fn)) return("F1")',
  '    if (grepl("map|leaflet|choropleth|world|global", fn)) return("F1")',
  '    "F1"',
  '  }'
  )
  # 替换原来的 exists 检查行
  src[check_ln] <- paste(replacement, collapse = "\n")
  
  # 同时需要把下面用到 .fb_assign_fig / .fb_assign_widget 的地方改为 assign_fig / assign_widget
  for (i in seq_along(src)) {
    src[i] <- gsub("\\.fb_assign_fig", "assign_fig", src[i])
    src[i] <- gsub("\\.fb_assign_widget", "assign_widget", src[i])
  }
}

writeLines(src, "程序/21_static_showcase.R", useBytes = TRUE)
tryCatch({ parse("程序/21_static_showcase.R"); cat("OK\n") },
  error = function(e) cat("FAIL:", conditionMessage(e), "\n"))
