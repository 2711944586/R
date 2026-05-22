suppressMessages({
  options(testthat.progress.max_fails = 5)
  helper <- "自动测试/testthat/helper-source.R"
  if (file.exists(helper)) source(helper, encoding = "UTF-8")
  files <- list.files("自动测试/testthat", pattern = "^test-.*\\.R$",
                      full.names = TRUE)
  total <- list(passed = 0L, failed = 0L, skipped = 0L, warning = 0L)
  for (f in files) {
    res <- testthat::test_file(f, reporter = "summary",
                                stop_on_failure = FALSE)
    df <- as.data.frame(res)
    total$passed  <- total$passed  + sum(df$passed,  na.rm = TRUE)
    total$failed  <- total$failed  + sum(df$failed,  na.rm = TRUE)
    total$skipped <- total$skipped + sum(df$skipped, na.rm = TRUE)
    total$warning <- total$warning + sum(df$warning, na.rm = TRUE)
  }
})
cat(sprintf("\nTOTAL passed: %d\n", total$passed))
cat(sprintf("TOTAL failed: %d\n", total$failed))
cat(sprintf("TOTAL skipped: %d\n", total$skipped))
cat(sprintf("TOTAL warnings: %d\n", total$warning))
if (total$failed > 0L) quit(status = 1L)
