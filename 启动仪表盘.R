# run_app.R  ---  本地启动 Shiny 仪表盘
# 用法: Rscript 启动仪表盘.R [port]
args <- commandArgs(trailingOnly = TRUE)
port <- if (length(args) >= 1) as.integer(args[1]) else 4848L
shiny::runApp(
  appDir = "仪表盘",
  host = "0.0.0.0",
  port = port,
  launch.browser = interactive()
)
