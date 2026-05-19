# 修复注入位置 - 需要作为 paste0 中的独立参数
src <- readLines("程序/21_static_showcase.R", encoding = "UTF-8")

# 找到注入行（在paste0内部）
inject_lines <- grep("ghs_inject_all_assets\\(fig_dir", src)
# 第二个是在 .ghs_render 中的调用
if (length(inject_lines) >= 2L) {
  call_ln <- inject_lines[2]
  # 检查前一行是否是注释
  comment_ln <- call_ln - 1
  if (grepl("自动注入", src[comment_ln])) {
    # 当前这两行（注释 + 调用）被插在了错误位置
    # 它们在 .ghs_findings(...) 后面，但可能缺少逗号分隔
    # 查看上下文
    # 正确做法：确保它是 paste0 的一个独立参数
    # 检查它前面那行是否以逗号结尾
    prev_line <- src[comment_ln - 1]
    if (!grepl(",$", trimws(prev_line))) {
      # 前面缺少逗号，说明它被嵌入到了函数调用参数中
      # 需要确保前一行以逗号结尾
      src[comment_ln - 1] <- paste0(trimws(src[comment_ln - 1]), ",")
    }
  }
}

writeLines(src, "程序/21_static_showcase.R", useBytes = TRUE)
tryCatch({ parse("程序/21_static_showcase.R"); cat("OK\n") },
  error = function(e) cat("FAIL:", conditionMessage(e), "\n"))
