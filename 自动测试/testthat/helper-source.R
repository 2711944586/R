# 自动测试/testthat/helper-source.R
# testthat 自动加载 helper-*.R 文件
# 作用：source 所有 程序/*.R 模块到测试环境，让任何 test_* 文件都能直接调用项目函数
# 这样 testthat::test_dir("自动测试/testthat") 也能跑通（v1 只支持 testthat.R 入口）

.proj_root <- (function() {
  cwd <- getwd()
  for (up in 0:5) {
    candidate <- do.call(file.path, c(list(cwd), as.list(rep("..", up))))
    candidate <- normalizePath(candidate, mustWork = FALSE)
    if (file.exists(file.path(candidate, "DESCRIPTION")) ||
        file.exists(file.path(candidate, ".Rprofile"))) {
      return(candidate)
    }
  }
  cwd
})()

for (.f in list.files(file.path(.proj_root, "程序"),
                      pattern = "\\.R$", full.names = TRUE)) {
  suppressWarnings(suppressMessages(source(.f, encoding = "UTF-8")))
}
rm(.f)
