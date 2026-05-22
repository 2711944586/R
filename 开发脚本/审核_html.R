html_path <- "d:/文件/大作业类/R/大作业/网站发布/index.html"
con <- file(html_path, "r", encoding = "UTF-8")
html <- readLines(con, warn = FALSE)
close(con)

cat("=== HTML 结构审核 ===\n")
cat("total lines:", format(length(html), big.mark = ","), "\n")

finds <- length(grep("class='finding' id='f-", html))
cat("finding sections (F-cards):", finds, "\n")

top_sections <- length(grep("<section class='section[^']*'", html))
cat("top-level sections:", top_sections, "\n")

chips <- length(grep("<span class='chip", html))
cat("chip elements:", chips, "\n")

imgs <- length(grep("<img ", html))
cat("img tags:", imgs, "\n")

iframes <- length(grep("<iframe", html))
cat("iframe (widgets):", iframes, "\n")

methods <- length(grep("class='method-block'", html))
cat("method blocks:", methods, "\n")

callouts <- length(grep("class='callout", html))
cat("callouts:", callouts, "\n")

limits <- length(grep("class='limit-note'", html))
cat("limit notes:", limits, "\n")

deep <- length(grep("class='deep-dive-grid'", html))
cat("deep-dive grids:", deep, "\n")

codes <- length(grep("class='code-figure'", html))
cat("code figures:", codes, "\n")

cat("\n=== TOC IDs vs section anchors ===\n")
toc_ids <- c("executive", "methods", "dq", "codebook", "kpi", "findings",
             "countries", "regional", "period", "sdg3", "lifeexp", "atlas",
             "cluster-detail", "extreme", "simulator", "figure-index", "widgets",
             "repro", "conclusion")
for (id in toc_ids) {
  pat <- sprintf("id='%s'", id)
  found <- any(grepl(pat, html, fixed = TRUE))
  cat(sprintf("  %-20s : %s\n", id, if (found) "OK" else "MISSING"))
}

cat("\n=== 检查 image src 是否为 base64 / 相对路径 ===\n")
img_lines <- grep("<img ", html, value = TRUE)
b64 <- sum(grepl("data:image", img_lines))
rel <- sum(!grepl("data:image", img_lines))
cat("img with base64:", b64, "\n")
cat("img with src path:", rel, "\n")
