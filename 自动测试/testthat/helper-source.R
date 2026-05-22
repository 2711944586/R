




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
