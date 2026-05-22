if (!dir.exists("程序") && basename(getwd()) == "开发脚本") setwd("..")

section <- function(title) cat(sprintf("\n=== %s ===\n", title))
`%||%` <- function(a, b) if (is.null(a)) b else a
ok <- function(file_path, label = NULL) {
  exists <- file.exists(file_path)
  size_kb <- if (exists) file.info(file_path)[["size"]] / 1024 else NA_real_
  mark <- if (exists) "OK " else "MIS"
  cat(sprintf("  %s %-50s %s\n", mark,
              label %||% file_path,
              if (exists) sprintf("(%.1f KB)", size_kb) else ""))
  invisible(exists)
}

section("Repo skeleton")
for (f in c(".Rprofile", ".gitignore", "DESCRIPTION", "LICENSE",
             "构建.R", "安装依赖.R", "启动仪表盘.R", "_targets.R", "README.md",
             file.path("项目文档", "方法手册.md"),
             file.path("项目文档", "部署总览.md"),
             file.path("部署配置", "Dockerfile"),
             file.path("部署配置", "docker-compose.yml"),
             ".dockerignore", ".lintr")) ok(f)

section("Submission")
for (f in list.files("课程提交", recursive = TRUE, full.names = TRUE)) ok(f)

section("Data engineering modules")
for (f in list.files("程序", pattern = "[.]R$", full.names = TRUE)) ok(f)

section("Cached data")
for (f in list.files("派生数据/处理结果", full.names = TRUE)) ok(f)

section("Models / metrics tables")
for (f in list.files("分析输出/模型表", full.names = TRUE)) ok(f)

section("Static figures")
ff <- list.files("分析输出/图表", pattern = "[.]png$", full.names = TRUE)
cat(sprintf("  TOTAL PNG: %d\n", length(ff)))
ff <- list.files("分析输出/图表", pattern = "[.]svg$", full.names = TRUE)
cat(sprintf("  TOTAL SVG: %d\n", length(ff)))

section("Interactive widgets")
ff <- list.files("分析输出/交互组件", pattern = "[.]html$", full.names = TRUE)
cat(sprintf("  TOTAL HTML widgets: %d\n", length(ff)))

section("Shiny app structure")
for (f in list.files("仪表盘", recursive = TRUE, full.names = TRUE)) ok(f)

section("CI/CD workflows")
for (f in list.files(".github/workflows", full.names = TRUE)) ok(f)

section("Tests")
for (f in list.files("自动测试", recursive = TRUE, full.names = TRUE)) ok(f)

section("Dev scripts")
for (f in list.files("开发脚本", full.names = TRUE)) ok(f)

cat("\n=== AUDIT DONE ===\n")
