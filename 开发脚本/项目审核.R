# 项目整体审核
root <- "d:/文件/大作业类/R/大作业"

cat("=== 1. 顶层结构 ===\n")
top <- list.files(root)
cat(paste(top, collapse = " | "), "\n\n")

cat("=== 2. R 程序模块 ===\n")
prog <- list.files(file.path(root, "程序"), pattern = "\\.R$")
total_lines <- sum(sapply(prog, function(f) {
  length(readLines(file.path(root, "程序", f)))
}))
cat("count:", length(prog), "\n")
cat("total lines:", format(total_lines, big.mark = ","), "\n\n")

cat("=== 3. Shiny 模块 ===\n")
mods <- list.files(file.path(root, "仪表盘/模块"), pattern = "^mod_.+\\.R$")
mod_lines <- sum(sapply(mods, function(f) {
  length(readLines(file.path(root, "仪表盘/模块", f)))
}))
cat("count:", length(mods), "\n")
cat("module lines:", format(mod_lines, big.mark = ","), "\n\n")

cat("=== 4. 静态图 ===\n")
pngs <- list.files(file.path(root, "分析输出/图表"), pattern = "\\.png$")
svgs <- list.files(file.path(root, "分析输出/图表"), pattern = "\\.svg$")
cat("PNG:", length(pngs), "| SVG:", length(svgs), "\n\n")

cat("=== 5. 交互组件 ===\n")
htmls <- list.files(file.path(root, "分析输出/交互组件"), pattern = "\\.html$")
cat("HTML widgets:", length(htmls), "\n\n")

cat("=== 6. 模型/指标表 ===\n")
csvs <- list.files(file.path(root, "分析输出/模型表"), pattern = "\\.csv$")
rds <- list.files(file.path(root, "分析输出/模型表"), pattern = "\\.rds$")
cat("CSV:", length(csvs), "| RDS:", length(rds), "\n\n")

cat("=== 7. 测试 ===\n")
tests <- list.files(file.path(root, "自动测试/testthat"), pattern = "^test-.+\\.R$")
cat("test files:", length(tests), "\n\n")

cat("=== 8. 课程提交 ===\n")
sub_files <- list.files(file.path(root, "课程提交"))
for (f in sub_files) {
  fp <- file.path(root, "课程提交", f)
  sz <- file.info(fp)$size
  cat(" ", f, ":", round(sz / 1024 / 1024, 1), "MB\n")
}

cat("\n=== 9. 网站发布 ===\n")
cat("index.html:", round(file.info(file.path(root, "网站发布/index.html"))$size / 1024 / 1024, 1), "MB\n")
ws_widgets <- list.files(file.path(root, "网站发布/交互组件"), pattern = "\\.html$")
cat("widgets in 网站发布:", length(ws_widgets), "\n\n")

cat("=== 10. 模块/findings 内容密度 ===\n")
f15_36 <- length(readLines(file.path(root, "程序/42_findings_extra.R"))) +
          length(readLines(file.path(root, "程序/42b_findings_batch.R")))
cat("F15-F36 总行数:", f15_36, "\n")
cat("F1-F14 (in 21_static_showcase.R):", length(readLines(file.path(root, "程序/21_static_showcase.R"))), "lines\n\n")

cat("=== 11. parse 检查 ===\n")
all_r <- list.files(file.path(root, "程序"), pattern = "\\.R$", full.names = TRUE)
ok <- 0L; fail <- 0L
for (f in all_r) {
  tryCatch({parse(file = f); ok <- ok + 1L},
           error = function(e) fail <<- fail + 1L)
}
cat("程序/:", ok, "OK,", fail, "FAIL\n")

mod_r <- list.files(file.path(root, "仪表盘/模块"), pattern = "\\.R$", full.names = TRUE)
ok <- 0L; fail <- 0L
for (f in mod_r) {
  tryCatch({parse(file = f); ok <- ok + 1L},
           error = function(e) fail <<- fail + 1L)
}
cat("仪表盘/模块:", ok, "OK,", fail, "FAIL\n")
