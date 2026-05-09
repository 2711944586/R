# 开发脚本/运行自动测试.R
# Run testthat suite and report pass/fail count
suppressMessages({
  options(testthat.progress.max_fails = 5)
  res <- testthat::test_dir("自动测试/testthat",
                             reporter = "summary",
                             stop_on_failure = FALSE)
})
df <- as.data.frame(res)
cat(sprintf("\nTOTAL passed: %d\n", sum(df$passed)))
cat(sprintf("TOTAL failed: %d\n", sum(df$failed)))
cat(sprintf("TOTAL skipped: %d\n", sum(df$skipped)))
cat(sprintf("TOTAL warnings: %d\n", sum(df$warning)))
