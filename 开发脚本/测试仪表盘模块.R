# 开发脚本/测试仪表盘模块.R
# 验证 Shiny 模块化能 source/parse 通过且 UI 对象可生成

cat("[shiny test] starting\n")
t0 <- Sys.time()

# 1. source global.R
cat("\n[1/4] source 仪表盘/global.R ...\n")
res <- tryCatch({
  source("仪表盘/global.R", encoding = "UTF-8")
  TRUE
}, error = function(e) {
  cat("ERR:", conditionMessage(e), "\n"); FALSE
})
stopifnot(res)
cat("    master_enriched rows:", nrow(master_enriched), "\n")
cat("    world_sf:", if (is.null(world_sf_obj)) "missing"
                     else paste(nrow(world_sf_obj), "polys"), "\n")

# 2. 验证 12 个 mod_*_ui / mod_*_server 都已加载
cat("\n[2/4] checking 12 modules ...\n")
mod_names <- c("overview", "country", "equity", "efficiency",
                "pandemic", "outcomes", "compare", "cluster",
                "forecast", "scenarios", "atlas", "about")
for (nm in mod_names) {
  ui_fn  <- paste0("mod_", nm, "_ui")
  srv_fn <- paste0("mod_", nm, "_server")
  ok_ui  <- exists(ui_fn,  mode = "function")
  ok_srv <- exists(srv_fn, mode = "function")
  cat(sprintf("    %-12s UI: %s · Server: %s\n", nm,
              if (ok_ui) "OK" else "FAIL",
              if (ok_srv) "OK" else "FAIL"))
  stopifnot(ok_ui && ok_srv)
}

# 3. 构建 UI 对象（不真的 runApp）
cat("\n[3/4] build ui object via 仪表盘/ui.R ...\n")
ui_obj <- tryCatch(
  source("仪表盘/ui.R", encoding = "UTF-8")$value,
  error = function(e) {
    cat("ERR:", conditionMessage(e), "\n"); NULL
  }
)
stopifnot(!is.null(ui_obj))
cat("    ui class:", paste(class(ui_obj), collapse = ", "), "\n")

# 4. 验证 server 函数
cat("\n[4/4] build server function via 仪表盘/server.R ...\n")
server_obj <- tryCatch(
  source("仪表盘/server.R", encoding = "UTF-8")$value,
  error = function(e) {
    cat("ERR:", conditionMessage(e), "\n"); NULL
  }
)
stopifnot(is.function(server_obj))
cat("    server is function with args:",
    paste(names(formals(server_obj)), collapse = ", "), "\n")

cat(sprintf("\n[shiny test] DONE in %.1fs\n",
             as.numeric(difftime(Sys.time(), t0, units = "secs"))))
cat("\nAll 12 modules loaded · UI built · Server function ready\n")
