# 开发脚本/修复报告项目根.R
# 修复 here::here() 在 报告书/ 被 _quarto.yml 误导：改用 rprojroot 锚定 DESCRIPTION
chapters <- list.files("报告书", pattern = "^[0-9]+.*\\.qmd$", full.names = TRUE)
chapters <- c(chapters, "报告书/index.qmd", "报告书/A0-methodology.qmd")
chapters <- chapters[file.exists(chapters)]
cat("found", length(chapters), "files\n")

old_pat1 <- 'source(here::here("报告书", "_setup.R"), encoding = "UTF-8")'
old_pat2 <- 'source(here::here("报告书", "_setup.R"))'
new_str  <- '.proj_root <- rprojroot::find_root(rprojroot::has_file("DESCRIPTION"))\nsource(file.path(.proj_root, "报告书", "_setup.R"), encoding = "UTF-8")'

patched <- 0
for (f in chapters) {
  txt <- readLines(f, encoding = "UTF-8", warn = FALSE)
  joined <- paste(txt, collapse = "\n")
  if (grepl(old_pat1, joined, fixed = TRUE)) {
    joined <- gsub(old_pat1, new_str, joined, fixed = TRUE)
    writeLines(strsplit(joined, "\n", fixed = TRUE)[[1]], f, useBytes = TRUE)
    cat("[ok] patched (variant 1)", basename(f), "\n"); patched <- patched + 1
  } else if (grepl(old_pat2, joined, fixed = TRUE)) {
    joined <- gsub(old_pat2, new_str, joined, fixed = TRUE)
    writeLines(strsplit(joined, "\n", fixed = TRUE)[[1]], f, useBytes = TRUE)
    cat("[ok] patched (variant 2)", basename(f), "\n"); patched <- patched + 1
  } else if (grepl("rprojroot::find_root", joined, fixed = TRUE)) {
    cat("[skip]", basename(f), "already rprojroot\n")
  } else {
    cat("[skip]", basename(f), "no setup\n")
  }
}
cat("\nTotal:", patched, "\n")
