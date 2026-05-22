# 修复：删除 .ghs_supplement_section 和其调用

src <- readLines("程序/21_static_showcase.R", encoding = "UTF-8")

# 1. 删除 .ghs_supplement_section 调用（在 .ghs_render 中）
supp_call <- grep("ghs_findings_supplement", src)
if (length(supp_call)) {
  # 找到这段 if...else "" 的范围（通常3-4行）
  for (ln in supp_call) {
    if (grepl("if \\(exists", src[ln])) {
      # 这是一个 if(...) { } else "" 块，占3行
      src[ln] <- '    "",'
      if (ln + 1 <= length(src) && grepl("ghs_supplement_section", src[ln + 1])) {
        src[ln + 1] <- ''
      }
      if (ln + 2 <= length(src) && grepl('} else ""', src[ln + 2])) {
        src[ln + 2] <- ''
      }
    }
  }
}

# 2. 删除 .ghs_supplement_section 函数定义
supp_def <- grep("^\\.ghs_supplement_section <- function", src)
if (length(supp_def) == 1L) {
  # 定位 .ghs_gallery 前的插入点
  gallery_def <- grep("^\\.ghs_gallery <- function", src)
  if (length(gallery_def) >= 1L) {
    next_fn <- gallery_def[gallery_def > supp_def][1]
    if (!is.na(next_fn)) {
      # 删除从 supp_def 前的空行到 next_fn - 1
      start_del <- supp_def
      # 查找前面的空行
      while (start_del > 1 && trimws(src[start_del - 1]) == "") {
        start_del <- start_del - 1
      }
      end_del <- next_fn - 1
      while (end_del > start_del && trimws(src[end_del]) == "") {
        end_del <- end_del - 1
      }
      src <- src[-(start_del:end_del)]
      cat(sprintf("  Removed .ghs_supplement_section: lines %d-%d\n", start_del, end_del))
    }
  }
}

# 3. 同时删除 42b_findings_batch.R 中不需要的 ghs_findings_supplement 函数
# 因为我们不再需要独立的 supplement 函数

writeLines(src, "程序/21_static_showcase.R", useBytes = TRUE)
cat("[1/2] Removed .ghs_supplement_section\n")

# 4. 验证 parse
tryCatch({
  parse("程序/21_static_showcase.R")
  cat("[2/2] parse OK\n")
}, error = function(e) {
  cat("[2/2] PARSE FAILED:", conditionMessage(e), "\n")
})
