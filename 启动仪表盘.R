args <- commandArgs(trailingOnly = TRUE)
port <- if (length(args) >= 1) as.integer(args[1]) else 4848L
shiny::runApp(
  appDir = "仪表盘",
  host = "0.0.0.0",
  port = port,
  launch.browser = interactive()
)
