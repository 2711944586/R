con <- file("d:/文件/大作业类/R/大作业/网站发布/index.html", "r", encoding = "UTF-8")
html <- readLines(con, warn = FALSE)
close(con)
cat("class='finding' id='f-:", length(grep("class='finding' id='f-", html)), "\n")
cat("class='finding':", length(grep("class='finding'", html)), "\n")
cat("id='f-:", length(grep("id='f-", html)), "\n")
cat("finding-num:", length(grep("finding-num", html)), "\n\n")

ids <- c("f-aging", "f-urban", "f-fiscal", "f-price", "f-regional",
         "f-inflation", "f-ncd", "f-uhc", "f-catastrophic", "f-maternal",
         "f-prevention", "f-workforce", "f-reclassify", "f-theil",
         "f-fragile", "f-oecd", "f-dea", "f-aid-eff", "f-dataquality",
         "f-revision", "f-sids", "f-composite")
for (id in ids) {
  found <- any(grepl(paste0("id='", id, "'"), html, fixed = TRUE))
  cat(sprintf("  %-20s : %s\n", id, if (found) "OK" else "MISSING"))
}
