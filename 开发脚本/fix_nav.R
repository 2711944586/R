# 修复导航面板：
# 1. 添加首页标题
# 2. 删除已不存在的 4.2 素材中控 / 4.3 图表索引
# 3. 细分 "2.1 核心发现" 为 F1-F14 子条目
# 4. 添加 F15-F36 条目
# 5. 改善动画（CSS transition）和跳转精度（scroll offset）

src <- readLines("程序/21_static_showcase.R", encoding = "UTF-8")

# 找到 .ghs_nav_items 函数
nav_start <- grep("^\\.ghs_nav_items <- function", src)
if (length(nav_start) != 1L) stop("Cannot find .ghs_nav_items")

# 找到函数结尾（下一个顶级函数定义）
nav_end <- grep("^\\.ghs_nav_links <- function", src)
if (length(nav_end) != 1L) stop("Cannot find .ghs_nav_links")
nav_end <- nav_end - 1L
while (nav_end > nav_start && trimws(src[nav_end]) == "") nav_end <- nav_end - 1L

# 替换导航条目
new_nav <- c(
'.ghs_nav_items <- function() {',
'  list(',
'    list(group = "\\u9996\\u9875", items = list(',
'      list(id = "top", label = "\\u2191 \\u56de\\u5230\\u9876\\u90e8")',
'    )),',
'    list(group = "1 \\u6982\\u89c8", items = list(',
'      list(id = "executive", label = "1.1 \\u6458\\u8981"),',
'      list(id = "methods", label = "1.2 \\u6570\\u636e\\u5904\\u7406"),',
'      list(id = "dq", label = "1.3 \\u6570\\u636e\\u8d28\\u91cf"),',
'      list(id = "codebook", label = "1.4 \\u53d8\\u91cf\\u53e3\\u5f84"),',
'      list(id = "kpi", label = "1.5 KPI")',
'    )),',
'    list(group = "2 \\u53d1\\u73b0", items = list(',
'      list(id = "findings", label = "2.1 F1\\u2013F14"),',
'      list(id = "f-aging", label = "2.2 F15\\u2013F20"),',
'      list(id = "f-ncd", label = "2.3 F21\\u2013F27"),',
'      list(id = "f-theil", label = "2.4 F28\\u2013F36"),',
'      list(id = "countries", label = "2.5 \\u56fd\\u5bb6"),',
'      list(id = "regional", label = "2.6 \\u533a\\u57df"),',
'      list(id = "period", label = "2.7 \\u5206\\u671f")',
'    )),',
'    list(group = "3 \\u4e13\\u9898", items = list(',
'      list(id = "sdg3", label = "3.1 SDG-3"),',
'      list(id = "lifeexp", label = "3.2 \\u5bff\\u547d"),',
'      list(id = "atlas", label = "3.3 \\u4e0d\\u5e73\\u7b49"),',
'      list(id = "cluster-detail", label = "3.4 \\u805a\\u7c7b"),',
'      list(id = "extreme", label = "3.5 \\u6781\\u503c")',
'    )),',
'    list(group = "4 \\u5de5\\u5177", items = list(',
'      list(id = "simulator", label = "4.1 \\u4eff\\u771f"),',
'      list(id = "gallery", label = "4.2 \\u56fe\\u5e93"),',
'      list(id = "widgets", label = "4.3 \\u4ea4\\u4e92\\u7ec4\\u4ef6")',
'    )),',
'    list(group = "5 \\u9644\\u5f55", items = list(',
'      list(id = "repro", label = "5.1 \\u590d\\u73b0"),',
'      list(id = "conclusion", label = "5.2 \\u7ed3\\u8bba")',
'    ))',
'  )',
'}',
''
)

src <- c(src[1:(nav_start - 1)], new_nav, src[(nav_end + 1):length(src)])

# 改善动画：在 dock-panel CSS 中加入更平滑的 transition
# 找到当前的 dock-panel transition
dp_line <- grep("dock-panel\\{position:absolute", src)
if (length(dp_line) == 1L) {
  src[dp_line] <- sub(
    "transition:opacity .22s cubic-bezier\\(.4,0,.2,1\\),transform .22s cubic-bezier\\(.4,0,.2,1\\)",
    "transition:opacity .3s cubic-bezier(.16,1,.3,1),transform .3s cubic-bezier(.16,1,.3,1)",
    src[dp_line])
}

# 改善跳转精度：在 JS 中修改 scroll offset 从 80 到 40
js_line <- grep("var top=absTop\\(el\\)-80", src)
if (length(js_line) >= 1L) {
  for (ln in js_line) {
    src[ln] <- sub("absTop\\(el\\)-80", "absTop(el)-40", src[ln])
  }
}

# 在 hero section 添加 id="top" 锚点
hero_line <- grep("<section class=.hero", src)
if (length(hero_line) >= 1L) {
  for (ln in hero_line) {
    if (!grepl("id=.top", src[ln])) {
      src[ln] <- sub("<section class='hero'", "<section class='hero' id='top'", src[ln], fixed = FALSE)
      src[ln] <- sub("class='hero'", "class='hero' id='top'", src[ln], fixed = TRUE)
    }
  }
}

writeLines(src, "程序/21_static_showcase.R", useBytes = TRUE)

# 验证
tryCatch({
  parse("程序/21_static_showcase.R")
  cat("parse OK\n")
}, error = function(e) cat("PARSE FAILED:", conditionMessage(e), "\n"))
