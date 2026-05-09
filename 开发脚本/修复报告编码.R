# 开发脚本/修复报告编码.R
# 修复 12 章 setup chunk: source() 加 encoding="UTF-8" 以处理中文路径
chapters <- list.files("报告书", pattern = "^[0-9]+.*\\.qmd$", full.names = TRUE)
cat("found", length(chapters), "chapters\n")
old_str <- 'source(here::here("报告书", "_setup.R"))'
new_str <- 'source(here::here("报告书", "_setup.R"), encoding = "UTF-8")'
patched <- 0
for (f in chapters) {
  txt <- readLines(f, encoding = "UTF-8", warn = FALSE)
  if (any(grepl(old_str, txt, fixed = TRUE))) {
    txt <- gsub(old_str, new_str, txt, fixed = TRUE)
    writeLines(txt, f, useBytes = TRUE)
    cat("[ok] patched", basename(f), "\n")
    patched <- patched + 1
  } else if (any(grepl(new_str, txt, fixed = TRUE))) {
    cat("[skip]", basename(f), "already patched\n")
  } else {
    cat("[skip]", basename(f), "no setup chunk\n")
  }
}
cat("\nTotal patched:", patched, "\n")
