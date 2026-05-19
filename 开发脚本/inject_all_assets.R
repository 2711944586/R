# 在 21_static_showcase.R 的 .ghs_findings 函数末尾追加逻辑：
# 遍历所有 300 张图 / 133 个 widget，按命名规则分配到 F1-F36
# 对于已被引用的跳过，对于未被引用的自动嵌入到页面中
# 方法：修改 .ghs_gallery 函数，让它不仅展示缩略图
# 而是直接把每张图以完整尺寸嵌入并附带简要说明

# 更好的方法：写一个新函数 .ghs_full_evidence()
# 在 .ghs_render 中调用，替代 gallery 的"仅缩略图"展示

src <- readLines("程序/21_static_showcase.R", encoding = "UTF-8")

# 找到 .ghs_gallery 定义
gallery_def <- grep("^\\.ghs_gallery <- function", src)
if (length(gallery_def) != 1L) stop("Cannot find .ghs_gallery")

# 在 gallery 函数前插入一个新函数 .ghs_inject_unused_to_findings
# 这个函数在 .ghs_render 中被调用，把所有图按分配规则嵌入到一个
# "完整证据附录"区块中，每个 Finding 一组
# 
# 但更简洁的方案是：让 gallery 本身展示所有图的完整版本，
# 每张图附带一句自动生成的 note（基于 .ghs_gallery_note）
# Gallery 已经做了这件事！它展示所有 300 张图+note。
#
# 真正的问题是：F1-F14 的代码每个只引用了 2-4 张图。
# 解决：在 .ghs_render 中，Findings 输出之后、gallery 之前，
# 插入一个 pass 把所有图按 finding 主题分组再次展示（带阐述）

# 实际上最简单的方案：利用已有的 .fb_assign_fig 映射函数
# 在 render 时自动把所有未被 finding 引用的图追加到对应 finding section

# 让我在 .ghs_render 中的 findings HTML 后面做后处理：
# 对每个 finding id，把对应的未引用图注入到该 section 末尾

# 找到 .ghs_render 中组装 findings 的位置
findings_call <- grep("\\.ghs_findings\\(master", src)
if (length(findings_call) >= 1L) {
  fc <- findings_call[length(findings_call)]
  
  # 在 findings 调用后面添加自动注入逻辑
  inject_code <- c(
    '    # 自动注入：把所有图和widget按Finding主题分配嵌入',
    '    .ghs_inject_all_assets(fig_dir, widget_dir, mode, repo_url),'
  )
  src <- c(src[1:fc], inject_code, src[(fc+1):length(src)])
}

# 在 .ghs_gallery 前定义 .ghs_inject_all_assets 函数
gallery_def2 <- grep("^\\.ghs_gallery <- function", src)
inject_fn <- c(
'',
'.ghs_inject_all_assets <- function(fig_dir, widget_dir, mode, repo_url) {',
'  # 加载分配函数',
'  if (!exists(".fb_assign_fig", mode = "function")) return("")',
'  ',
'  all_figs <- sort(list.files(fig_dir, pattern = "[.]png$", full.names = FALSE))',
'  all_widgets <- sort(list.files(widget_dir, pattern = "[.]html$", full.names = FALSE))',
'  ',
'  # 按 Finding 分组',
'  fig_groups <- split(all_figs, vapply(all_figs, .fb_assign_fig, character(1)))',
'  wgt_groups <- split(all_widgets, vapply(all_widgets, .fb_assign_widget, character(1)))',
'  ',
'  # 为每个 Finding 生成补充内容',
'  findings <- paste0("F", 1:36)',
'  parts <- vapply(findings, function(fid) {',
'    figs <- fig_groups[[fid]]',
'    wgts <- wgt_groups[[fid]]',
'    if (!length(figs) && !length(wgts)) return("")',
'    ',
'    # 生成图卡片',
'    fig_html <- if (length(figs)) {',
'      cards <- vapply(figs, function(f) {',
'        p <- file.path(fig_dir, f)',
'        if (!file.exists(p)) return("")',
'        note <- .ghs_gallery_note(.ghs_pretty(p), .ghs_kind(.ghs_pretty(p)))',
'        paste0(',
'          .ghs_fig(p, .ghs_pretty(p), .ghs_pretty(p)),',
'          sprintf("<p class=\'gallery-note\'>%s</p>", .ghs_e(note)))',
'      }, character(1))',
'      paste(cards[nzchar(cards)], collapse = "")',
'    } else ""',
'    ',
'    # 生成widget锚点',
'    wgt_html <- if (length(wgts)) {',
'      cards <- vapply(wgts, function(w) {',
'        if (exists(".ghs_widget_anchor", mode = "function"))',
'          .ghs_widget_anchor(widget_dir, w, .ghs_pretty(w), mode, repo_url)',
'        else ""',
'      }, character(1))',
'      paste(cards[nzchar(cards)], collapse = "")',
'    } else ""',
'    ',
'    # 包装在一个补充区域中',
'    fid_lower <- tolower(sub("F", "f-", fid))',
'    paste0(',
'      sprintf("<div class=\'finding-supplement\' data-for=\'%s\'>", fid),',
'      fig_html, wgt_html,',
'      "</div>")',
'  }, character(1))',
'  ',
'  content <- paste(parts[nzchar(parts)], collapse = "")',
'  if (!nzchar(content)) return("")',
'  ',
'  # 包装为一个完整的 section',
'  sprintf(paste0(',
'    "<section class=\'section\' id=\'full-assets\'>",',
'    "<div class=\'wrap\'>",',
'    "<header class=\'section-head\'>",',
'    "<span class=\'kicker\'>EVIDENCE ATLAS</span>",',
'    "<h2>\\u5b8c\\u6574\\u8bc1\\u636e\\u5e93 \\u00b7 \\u6309\\u4e3b\\u9898\\u5f52\\u7c7b</h2>",',
'    "<p class=\'lead\'>\\u4ee5\\u4e0b\\u5c55\\u793a\\u672c\\u62a5\\u544a\\u4ea7\\u51fa\\u7684\\u5168\\u90e8 %d \\u5f20\\u9759\\u6001\\u56fe\\u4e0e %d \\u4e2a\\u4ea4\\u4e92\\u7ec4\\u4ef6\\uff0c\\u6309 36 \\u4e2a\\u6838\\u5fc3\\u53d1\\u73b0\\u7684\\u4e3b\\u9898\\u5f52\\u7c7b\\u3002\\u6bcf\\u5f20\\u56fe\\u5747\\u9644\\u5e26\\u9605\\u8bfb\\u6307\\u5f15\\uff0c\\u7528\\u4e8e\\u8fde\\u63a5\\u7814\\u7a76\\u95ee\\u9898\\u3001\\u6307\\u6807\\u53e3\\u5f84\\u548c\\u653f\\u7b56\\u89e3\\u91ca\\u3002</p>",',
'    "</header>%s</div></section>"),',
'    length(all_figs), length(all_widgets), content)',
'}',
''
)

src <- c(src[1:(gallery_def2 - 1)], inject_fn, src[gallery_def2:length(src)])

writeLines(src, "程序/21_static_showcase.R", useBytes = TRUE)

tryCatch({
  parse("程序/21_static_showcase.R")
  cat("parse OK\n")
}, error = function(e) cat("FAIL:", conditionMessage(e), "\n"))
