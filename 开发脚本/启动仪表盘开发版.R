# 开发脚本/启动仪表盘开发版.R
# 用 4848 端口跑 Shiny v2 仪表盘（后台进程）
shiny::runApp("仪表盘", port = 4848, host = "127.0.0.1",
              launch.browser = FALSE)
