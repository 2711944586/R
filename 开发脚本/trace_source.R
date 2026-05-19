setwd("d:/文件/大作业类/R/大作业")
fns <- list.files("程序", pattern = "\\.R$", full.names = TRUE)
for (f in fns) {
  source(f, encoding = "UTF-8")
  cat(sprintf("%-45s -> ghs_findings_extra=%s, fe_f15=%s, fe_batch_spec=%s\n",
              basename(f),
              exists("ghs_findings_extra", mode = "function"),
              exists(".fe_f15", mode = "function"),
              exists(".fe_batch_spec", mode = "function")))
}
