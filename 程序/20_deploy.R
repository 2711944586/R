
deploy_copy_widgets <- function(src = file.path("分析输出", "交互组件"),
                                  dst = file.path("网站发布", "交互组件"),
                                  pattern = "\\.html$",
                                  verbose = TRUE) {
  if (!dir.exists(src)) {
    if (verbose) message("[deploy] no widgets to copy at: ", src); return(0L)
  }
  dir.create(dst, recursive = TRUE, showWarnings = FALSE)
  files <- list.files(src, pattern = pattern, full.names = TRUE,
                       recursive = TRUE)
  if (!length(files)) return(0L)
  copied <- file.copy(files, dst, overwrite = TRUE, recursive = FALSE)
  if (verbose) cat("[deploy] copied", sum(copied), "widgets to", dst, "\n")
  invisible(sum(copied))
}

deploy_copy_figures <- function(src = file.path("分析输出", "图表"),
                                  dst = file.path("网站发布", "图表"),
                                  pattern = "\\.(png|svg)$",
                                  verbose = TRUE) {
  if (!dir.exists(src)) {
    if (verbose) message("[deploy] no figures at: ", src); return(0L)
  }
  dir.create(dst, recursive = TRUE, showWarnings = FALSE)
  files <- list.files(src, pattern = pattern, full.names = TRUE,
                       recursive = TRUE)
  if (!length(files)) return(0L)
  copied <- file.copy(files, dst, overwrite = TRUE, recursive = FALSE)
  if (verbose) cat("[deploy] copied", sum(copied), "figures to", dst, "\n")
  invisible(sum(copied))
}

deploy_sitemap <- function(网站发布_dir = "网站发布",
                            base_url = "https://2711944586.github.io/R/") {
  if (!dir.exists(网站发布_dir)) {
    message("[deploy] 网站发布/ not found"); return(invisible(NULL))
  }
  htmls <- list.files(网站发布_dir, pattern = "\\.html$",
                       recursive = TRUE, full.names = FALSE)
  if (!length(htmls)) return(invisible(NULL))
  urls <- vapply(htmls, function(p) {
    p <- gsub("\\\\", "/", p)
    sprintf("  <url><loc>%s%s</loc><changefreq>weekly</changefreq></url>",
             base_url, p)
  }, character(1))
  xml <- c(
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
    "<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">",
    urls,
    "</urlset>"
  )
  writeLines(xml, file.path(网站发布_dir, "sitemap.xml"), useBytes = TRUE)
  cat("[deploy] sitemap.xml written with",
      length(urls), "URLs\n")
  invisible(file.path(网站发布_dir, "sitemap.xml"))
}

deploy_size_report <- function(网站发布_dir = "网站发布") {
  if (!dir.exists(网站发布_dir)) return(invisible(NULL))
  subs <- list.dirs(网站发布_dir, recursive = FALSE, full.names = TRUE)
  out <- data.frame(
    path = c(网站发布_dir, subs),
    n_files = NA_integer_,
    size_mb = NA_real_,
    stringsAsFactors = FALSE
  )
  for (i in seq_len(nrow(out))) {
    fl <- list.files(out$path[i], recursive = TRUE, full.names = TRUE)
    out$n_files[i] <- length(fl)
    out$size_mb[i] <- sum(file.info(fl)$size, na.rm = TRUE) / 1024^2
  }
  out$size_mb <- round(out$size_mb, 2)
  out
}

deploy_warm_cache <- function() {
  cat("[deploy] warming cache...\n")
  if (exists("load_ghs", mode = "function") &&
      exists("enrich_master", mode = "function")) {
    cache <- file.path("派生数据", "处理结果", "master_enriched.rds")
    if (!file.exists(cache)) {
      ghs <- load_ghs()
      master <- enrich_master(build_master_wide(ghs), with_wdi = TRUE)
      dir.create(dirname(cache), recursive = TRUE, showWarnings = FALSE)
      saveRDS(master, cache)
    }
  }
  if (exists("load_world_sf", mode = "function")) {
    tryCatch(load_world_sf("medium", 0.08), error = function(e) NULL)
  }
  if (exists("load_external_all", mode = "function")) {
    tryCatch(load_external_all(), error = function(e) NULL)
  }
  cat("[deploy] cache warm done\n")
  invisible(TRUE)
}

deploy_finalize <- function(网站发布_dir = "网站发布",
                              base_url = "https://2711944586.github.io/R/",
                              verbose = TRUE) {
  deploy_copy_widgets(verbose = verbose)
  deploy_copy_figures(verbose = verbose)
  deploy_sitemap(网站发布_dir = 网站发布_dir, base_url = base_url)
  rep <- deploy_size_report(网站发布_dir = 网站发布_dir)
  if (verbose && !is.null(rep)) {
    cat("\n[deploy] size report:\n")
    print(rep)
  }
  invisible(rep)
}
