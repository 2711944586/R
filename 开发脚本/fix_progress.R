src <- readLines("程序/21_static_showcase.R", encoding = "UTF-8")
# 隐藏关闭状态下的进度条（那个难看的黑条）
ln <- grep("ghs-dock:not\\(\\.open\\) \\.dock-progress", src)
if (length(ln) == 1L) {
  src[ln] <- '    ".ghs-dock:not(.open) .dock-progress{display:none}",'
}
writeLines(src, "程序/21_static_showcase.R", useBytes = TRUE)
cat("done\n")
