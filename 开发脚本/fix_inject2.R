src <- readLines("程序/21_static_showcase.R", encoding = "UTF-8")

# 删除错误插入的两行 (3314-3315)
comment_ln <- grep("# 自动注入：把所有图和widget按Finding主题分配嵌入", src)
inject_ln <- grep("    \\.ghs_inject_all_assets\\(fig_dir, widget_dir, mode, repo_url\\),", src)

if (length(comment_ln) == 1L && length(inject_ln) == 1L) {
  # 删除这两行
  src <- src[-c(comment_ln, inject_ln)]
  cat("Removed misplaced lines\n")
}

# 现在找到 .ghs_findings 调用完成后的位置
# 它应该是: .ghs_findings(master, fig_dir, programs_dir, models_dir,
#                widget_dir, mode = mode, repo_url = repo_url),
findings_end <- grep("widget_dir, mode = mode, repo_url = repo_url\\)", src)
# 取在 .ghs_render 中的那一个（靠后的）
findings_end <- findings_end[length(findings_end)]

if (length(findings_end) == 1L) {
  # 在这一行之后插入
  new_lines <- c(
    "    .ghs_inject_all_assets(fig_dir, widget_dir, mode, repo_url),"
  )
  src <- c(src[1:findings_end], new_lines, src[(findings_end + 1):length(src)])
  cat("Inserted inject call after .ghs_findings\n")
}

writeLines(src, "程序/21_static_showcase.R", useBytes = TRUE)
tryCatch({ parse("程序/21_static_showcase.R"); cat("parse OK\n") },
  error = function(e) cat("FAIL:", conditionMessage(e), "\n"))
