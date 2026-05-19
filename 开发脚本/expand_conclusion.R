# 扩充结论到原来的五倍

src <- readLines("程序/21_static_showcase.R", encoding = "UTF-8")

# 找到 .ghs_conclusion 函数
conc_start <- grep("^\\.ghs_conclusion <- function", src)
conc_end <- grep("^\\.ghs_footer <- function", src)
if (length(conc_start) == 1L && length(conc_end) == 1L) {
  conc_end <- conc_end - 1L
  while (conc_end > conc_start && trimws(src[conc_end]) == "") conc_end <- conc_end - 1L
  conc_end <- conc_end + 1L
  
  # 完全重写结论函数
  new_conc <- readLines("开发脚本/conclusion_expanded.R", encoding = "UTF-8")
  src <- c(src[1:(conc_start - 1)], new_conc, src[(conc_end):length(src)])
}

writeLines(src, "程序/21_static_showcase.R", useBytes = TRUE)
tryCatch({ parse("程序/21_static_showcase.R"); cat("OK\n") },
  error = function(e) cat("FAIL:", conditionMessage(e), "\n"))
