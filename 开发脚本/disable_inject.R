src <- readLines("程序/21_static_showcase.R", encoding = "UTF-8")
ln <- grep("^\\.ghs_inject_all_assets <- function", src)
if (length(ln) == 1L) src[ln + 1] <- '  return("")'
writeLines(src, "程序/21_static_showcase.R", useBytes = TRUE)
cat("done\n")
