# 开发脚本/修复反引号中文.R
# 修复 .qmd chunk 中所有 `<...\uXXXX...>` 反引号内的 unicode 转义
# R 不支持 backtick 内的 \uXXXX，必须改为真实 UTF-8 字符

decode_str <- function(s) {
  tryCatch(eval(parse(text = paste0('"', s, '"'))),
            error = function(e) s)
}

chapters <- list.files("报告书",
                        pattern = "^[0-9A-Z].*\\.qmd$", full.names = TRUE)
total_fixes <- 0

for (f in chapters) {
  txt <- readLines(f, encoding = "UTF-8", warn = FALSE)
  joined <- paste(txt, collapse = "\n")
  # 找所有 backtick 包裹的字符串：`...`
  m <- gregexpr("`[^`\n]*\\\\u[0-9a-fA-F]{4}[^`\n]*`", joined, perl = TRUE)[[1]]
  if (m[1] == -1) next
  matches <- regmatches(joined, list(m))[[1]]
  if (!length(matches)) next
  matches_unique <- unique(matches)
  changed <- FALSE
  for (mt in matches_unique) {
    # 提取反引号内的内容并解码
    inner <- substr(mt, 2, nchar(mt) - 1)
    decoded <- decode_str(inner)
    if (!identical(inner, decoded)) {
      replacement <- paste0("`", decoded, "`")
      joined <- gsub(mt, replacement, joined, fixed = TRUE)
      cat(sprintf("  %s: %s -> %s\n",
                   basename(f), mt, replacement))
      total_fixes <- total_fixes + 1
      changed <- TRUE
    }
  }
  if (changed) {
    new_lines <- strsplit(joined, "\n", fixed = TRUE)[[1]]
    writeLines(new_lines, f, useBytes = TRUE)
    cat("[ok] saved", basename(f), "\n")
  }
}

cat(sprintf("\nTotal fixes: %d\n", total_fixes))
