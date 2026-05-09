# 开发脚本/修复裸中文参数.R
# 修复 .qmd chunk 中 `\uXXXX\uXXXX = value` 裸参数名（R 解析失败）
# 改为：`实际中文 = value`（反引号 + 直接 UTF-8 字符）

decode_u <- function(s) {
  # 把 \uXXXX 序列解码为 UTF-8 字符
  # parse 一个 R string literal 安全完成
  out <- s
  m <- gregexpr("\\\\u[0-9a-fA-F]{4}", out)[[1]]
  if (m[1] == -1) return(out)
  # 用 eval(parse) 安全解码：把 \uXXXX 包到字符串里
  result <- tryCatch(
    eval(parse(text = paste0('"', s, '"'))),
    error = function(e) s
  )
  result
}

chapters <- list.files("报告书", pattern = "^[0-9]+.*\\.qmd$", full.names = TRUE)
total_fixes <- 0

for (f in chapters) {
  txt <- readLines(f, encoding = "UTF-8", warn = FALSE)
  changed <- FALSE
  # 模式：行首空白 + 一串 \uXXXX 序列 + 空白 + =
  # 例：    \u6392\u540d   = dplyr::row_number(),
  pat <- "^(\\s*)((?:\\\\u[0-9a-fA-F]{4})+)(\\s*)(=.*)$"
  for (i in seq_along(txt)) {
    line <- txt[i]
    if (grepl(pat, line, perl = TRUE)) {
      m <- regmatches(line, regexec(pat, line, perl = TRUE))[[1]]
      if (length(m) >= 5) {
        leading <- m[2]; uesc <- m[3]; rest <- m[5]
        decoded <- decode_u(uesc)
        new_line <- paste0(leading, "`", decoded, "` ", rest)
        txt[i] <- new_line
        changed <- TRUE
        total_fixes <- total_fixes + 1
        cat(sprintf("  %s:%d  %s -> `%s`\n",
                     basename(f), i, uesc, decoded))
      }
    }
  }
  if (changed) {
    writeLines(txt, f, useBytes = TRUE)
    cat("[ok] saved", basename(f), "\n")
  }
}

cat(sprintf("\nTotal fixes: %d\n", total_fixes))
