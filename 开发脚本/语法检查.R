files <- list.files("程序", pattern = "\\.R$", full.names = TRUE)
ok_count <- 0L
for (f in files) {
  res <- tryCatch({
    parse(file = f)
    "OK"
  }, error = function(e) paste0("ERR: ", conditionMessage(e)))
  cat(sprintf("[%s] %s\n", res, f))
  if (identical(res, "OK")) ok_count <- ok_count + 1L
}
cat(sprintf("\n==> %d/%d files parse OK\n", ok_count, length(files)))
