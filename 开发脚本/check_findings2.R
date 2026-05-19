con <- file("d:/文件/大作业类/R/大作业/网站发布/index.html", "r", encoding = "UTF-8")
html <- readLines(con, warn = FALSE)
close(con)

# Each <section class='finding' id='...'> on its own
all_fids <- regmatches(html, gregexpr("id='f-[a-z0-9-]+'", html))
all_fids <- unique(unlist(all_fids))
cat("unique f- IDs in HTML:\n")
for (id in all_fids) cat("  ", id, "\n")
cat("\nTotal unique:", length(all_fids), "\n")

# Now the actual count of <section class='finding' ...> tags
sec_starts <- length(grep("<section class='finding'", html, fixed = TRUE))
cat("section starts:", sec_starts, "\n")

# F1-F14 numbers
f_nums <- regmatches(html, gregexpr("'finding-num'>F[0-9]+", html))
f_nums <- sort(unique(unlist(f_nums)))
cat("\nF-numbers found:\n")
for (n in f_nums) cat("  ", n, "\n")
