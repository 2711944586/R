# 批量升级：
# 1. 进度条：关闭面板显示，打开时隐藏
# 2. S18-S21 移到 gallery/widgets 前面
# 3. 删除 footer 数据来源段
# 4. dock 导航细化

src <- readLines("程序/21_static_showcase.R", encoding = "UTF-8")

# ---- 1. 进度条逻辑反转 ----
# 当前：.ghs-dock:not(.open) .dock-progress{display:none}
# 改为：.ghs-dock.open .dock-progress{display:none}
dp_closed <- grep("ghs-dock:not\\(\\.open\\) \\.dock-progress\\{display:none\\}", src)
if (length(dp_closed) == 1L) {
  src[dp_closed] <- '    ".ghs-dock.open .dock-progress{display:none}",'
}
# 同时要让关闭时显示：找到原始 dock-progress 定义并确保没有 display:none
# 原定义: .dock-progress{...transform:scaleX(0)...} 已经有了，关闭时scaleX就是进度

# ---- 2. 章节顺序：S18-S21 移到 gallery/widgets 前面 ----
# 在 .ghs_render 中找到 ghs_sections_extra 调用和 .ghs_gallery 调用
# 当前顺序: ... .ghs_gallery ... .ghs_widgets ... ghs_sections_extra ...
# 改为: ... ghs_sections_extra ... .ghs_gallery ... .ghs_widgets ...
sections_extra_line <- grep("ghs_sections_extra\\(master, fig_dir", src)
gallery_line <- grep("\\.ghs_gallery\\(fig_dir, widget_dir", src)

if (length(sections_extra_line) >= 1L && length(gallery_line) >= 1L) {
  se_line <- sections_extra_line[length(sections_extra_line)]
  gl_line <- gallery_line[length(gallery_line)]
  
  # 如果 sections_extra 在 gallery 后面，需要把它移到前面
  if (se_line > gl_line) {
    # 提取 sections_extra 块（通常是 if(...){ ... } else ""）
    # 查找这个 if 块的范围
    se_start <- se_line
    # 往前找 if 开头
    while (se_start > 1 && !grepl("if \\(exists\\(\"ghs_sections_extra\"", src[se_start])) {
      se_start <- se_start - 1
    }
    se_end <- se_line
    # 往后找 } else ""
    while (se_end < length(src) && !grepl('} else ""', src[se_end])) {
      se_end <- se_end + 1
    }
    
    # 提取这个块
    se_block <- src[se_start:se_end]
    # 删除原位置
    src <- src[-(se_start:se_end)]
    
    # 重新找 gallery 行（因为行号变了）
    gl_line2 <- grep("\\.ghs_gallery\\(fig_dir, widget_dir", src)
    gl_line2 <- gl_line2[length(gl_line2)]
    
    # 在 gallery 前面插入
    src <- c(src[1:(gl_line2 - 1)], se_block, src[gl_line2:length(src)])
    cat("  Moved sections_extra before gallery\n")
  }
}

# ---- 3. 删除 footer 数据来源段 ----
footer_datasrc <- grep("\u6570\u636e\u6765\u6e90\uff1aWHO Global Health", src)
if (length(footer_datasrc) == 1L) {
  # 这一行包含了数据来源描述，替换为空
  src[footer_datasrc] <- sub(
    "<p>.*\u6570\u636e\u6765\u6e90.*\u4e0d\u6784\u6210\u56e0\u679c\u63a8\u65ad.*</p>",
    "",
    src[footer_datasrc])
}

# ---- 4. dock 导航细化 ----
# 重写 .ghs_nav_items 加入更多条目
nav_start <- grep("^\\.ghs_nav_items <- function", src)
nav_end_fn <- grep("^\\.ghs_nav_links <- function", src)
if (length(nav_start) == 1L && length(nav_end_fn) == 1L) {
  nav_end <- nav_end_fn - 1L
  while (nav_end > nav_start && trimws(src[nav_end]) == "") nav_end <- nav_end - 1L
  
  new_nav <- c(
'.ghs_nav_items <- function() {',
'  list(',
'    list(group = "\\u9996\\u9875", items = list(',
'      list(id = "top", label = "\\u2191 \\u9876\\u90e8")',
'    )),',
'    list(group = "1 \\u6982\\u89c8", items = list(',
'      list(id = "executive", label = "\\u6458\\u8981"),',
'      list(id = "methods", label = "\\u6570\\u636e\\u5904\\u7406"),',
'      list(id = "dq", label = "\\u6570\\u636e\\u8d28\\u91cf"),',
'      list(id = "codebook", label = "\\u53d8\\u91cf\\u53e3\\u5f84"),',
'      list(id = "kpi", label = "KPI")',
'    )),',
'    list(group = "2 \\u6838\\u5fc3\\u53d1\\u73b0", items = list(',
'      list(id = "f-macro", label = "F1 \\u5168\\u7403\\u8d8b\\u52bf"),',
'      list(id = "f-oops", label = "F2 \\u81ea\\u4ed8\\u8d1f\\u62c5"),',
'      list(id = "f-equity", label = "F3 \\u4e0d\\u5e73\\u7b49"),',
'      list(id = "f-covid", label = "F4 COVID"),',
'      list(id = "f-convergence", label = "F5 \\u6536\\u655b"),',
'      list(id = "f-elasticity", label = "F6 \\u5f39\\u6027"),',
'      list(id = "f-cluster", label = "F7 \\u805a\\u7c7b"),',
'      list(id = "f-forecast", label = "F8 \\u9884\\u6d4b"),',
'      list(id = "f-aid", label = "F9 \\u5916\\u63f4"),',
'      list(id = "f-efficiency", label = "F10 \\u6548\\u7387"),',
'      list(id = "f-ranking", label = "F11 \\u6392\\u540d"),',
'      list(id = "f-lifeexp", label = "F12 \\u5bff\\u547d"),',
'      list(id = "f-sdg3", label = "F13 SDG-3"),',
'      list(id = "f-extremes", label = "F14 \\u6781\\u503c")',
'    )),',
'    list(group = "3 \\u6df1\\u5ea6\\u4e13\\u9898", items = list(',
'      list(id = "f-aging", label = "F15 \\u8001\\u9f84\\u5316"),',
'      list(id = "f-urban", label = "F16 \\u57ce\\u9547\\u5316"),',
'      list(id = "f-fiscal", label = "F17 \\u8d22\\u653f"),',
'      list(id = "f-afford", label = "F18 \\u53ef\\u8d1f\\u62c5"),',
'      list(id = "f-bloc", label = "F19 \\u533a\\u57df"),',
'      list(id = "f-inflation", label = "F20 \\u901a\\u80c0"),',
'      list(id = "f-ncd", label = "F21 NCD"),',
'      list(id = "f-uhc", label = "F22 UHC"),',
'      list(id = "f-catastrophic", label = "F23 \\u707e\\u96be\\u6027"),',
'      list(id = "f-maternal", label = "F24 \\u6bcd\\u5a74"),',
'      list(id = "f-prevention", label = "F25 \\u9884\\u9632"),',
'      list(id = "f-workforce", label = "F26 \\u4eba\\u529b"),',
'      list(id = "f-reclassify", label = "F27 \\u664b\\u5347")',
'    )),',
'    list(group = "4 \\u6269\\u5c55\\u53d1\\u73b0", items = list(',
'      list(id = "f-theil", label = "F28 \\u5206\\u89e3"),',
'      list(id = "f-fragile", label = "F29 \\u8106\\u5f31"),',
'      list(id = "f-oecd", label = "F30 OECD/LMIC"),',
'      list(id = "f-dea", label = "F31 \\u6548\\u7387"),',
'      list(id = "f-aid-eff", label = "F32 \\u63f4\\u52a9"),',
'      list(id = "f-dataquality", label = "F33 \\u6570\\u636e"),',
'      list(id = "f-revision", label = "F34 \\u4fee\\u8ba2"),',
'      list(id = "f-sids", label = "F35 \\u5c0f\\u5c9b\\u56fd"),',
'      list(id = "f-composite", label = "F36 \\u7efc\\u5408")',
'    )),',
'    list(group = "5 \\u4e13\\u9898\\u7ae0\\u8282", items = list(',
'      list(id = "countries", label = "\\u56fd\\u5bb6"),',
'      list(id = "regional", label = "\\u533a\\u57df"),',
'      list(id = "period", label = "\\u5206\\u671f"),',
'      list(id = "sdg3", label = "SDG-3"),',
'      list(id = "lifeexp", label = "\\u5bff\\u547d"),',
'      list(id = "atlas", label = "\\u4e0d\\u5e73\\u7b49"),',
'      list(id = "cluster-detail", label = "\\u805a\\u7c7b"),',
'      list(id = "extreme", label = "\\u6781\\u503c"),',
'      list(id = "simulator", label = "\\u4eff\\u771f")',
'    )),',
'    list(group = "6 \\u56fe\\u5e93", items = list(',
'      list(id = "gallery", label = "\\u9759\\u6001\\u56fe\\u8868"),',
'      list(id = "widgets", label = "\\u4ea4\\u4e92\\u7ec4\\u4ef6")',
'    )),',
'    list(group = "7 \\u9644\\u5f55", items = list(',
'      list(id = "repro", label = "\\u590d\\u73b0"),',
'      list(id = "conclusion", label = "\\u7ed3\\u8bba")',
'    ))',
'  )',
'}',
''
  )
  src <- c(src[1:(nav_start - 1)], new_nav, src[(nav_end + 1):length(src)])
  cat("  Updated nav items\n")
}

writeLines(src, "程序/21_static_showcase.R", useBytes = TRUE)

tryCatch({
  parse("程序/21_static_showcase.R")
  cat("parse OK\n")
}, error = function(e) cat("PARSE FAILED:", conditionMessage(e), "\n"))
