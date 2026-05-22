
if (!requireNamespace("testthat", quietly = TRUE)) {
  message("testthat not installed; skip tests.")
  quit(status = 0)
}
library(testthat)


proj_root_local <- (function() {
  if (basename(getwd()) == "testthat") return(file.path("..", ".."))
  if (basename(getwd()) == "tests")    return("..")
  "."
})()

for (f in list.files(file.path(proj_root_local, "程序"),
                     pattern = "\\.R$", full.names = TRUE)) {
  source(f, encoding = "UTF-8")
}

test_dir(file.path(proj_root_local, "tests", "testthat"),
         reporter = "summary",
         stop_on_failure = FALSE)
