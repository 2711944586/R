
ensure_pkgs <- function(pkg) {
  miss <- pkg[!vapply(pkg, requireNamespace, logical(1), quietly = TRUE)]
  if (length(miss)) {
    warning(
      "Missing packages: ", paste(miss, collapse = ", "),
      "\n  Run: install.packages(c('", paste(miss, collapse = "','"), "'))",
      call. = FALSE
    )
  }
  invisible(miss)
}

proj_root <- function() {
  root_here <- tryCatch(here::here(), error = function(e) NULL, warning = function(w) NULL)
  if (!is.null(root_here) && dir.exists(root_here)) return(root_here)
  wd <- getwd()
  for (up in 0:4) {
    candidate <- do.call(file.path, c(list(wd), as.list(rep("..", up))))
    candidate <- normalizePath(candidate, mustWork = FALSE)
    if (any(file.exists(file.path(candidate, c(".Rprofile", "DESCRIPTION", "项目文档/方法手册.md"))))) {
      return(candidate)
    }
  }
  wd
}

path_ensure <- function(...) {
  p <- file.path(...)
  dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
  p
}

.log_stamp <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")
logi <- function(...) message("[", .log_stamp(), "] [INFO] ", ...)
logw <- function(...) message("[", .log_stamp(), "] [WARN] ", ...)
loge <- function(...) message("[", .log_stamp(), "] [ERR ] ", ...)

cache_or_run <- function(key, expr,
                         dir = file.path(proj_root(), "派生数据", "原始缓存"),
                         format = c("rds", "parquet"),
                         force = FALSE) {
  format <- match.arg(format)
  ext <- switch(format, rds = ".rds", parquet = ".parquet")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  cache_path <- file.path(dir, paste0(key, ext))
  if (!force && file.exists(cache_path)) {
    logi("cache hit: ", cache_path)
    return(switch(
      format,
      rds     = readRDS(cache_path),
      parquet = arrow::read_parquet(cache_path)
    ))
  }
  logi("cache miss, computing: ", key)
  val <- force(expr)
  switch(
    format,
    rds     = saveRDS(val, cache_path),
    parquet = arrow::write_parquet(val, cache_path)
  )
  logi("cache stored: ", cache_path)
  val
}

fmt_pct <- function(x, digits = 1) {
  if (is.null(x) || length(x) == 0) return("—")
  v <- suppressWarnings(as.numeric(x))
  if (length(v) == 1 && !is.finite(v)) return("—")
  out <- sprintf(paste0("%.", digits, "f%%"), v)
  out[!is.finite(v)] <- "—"
  out
}

fmt_usd <- function(x, big = TRUE) {
  if (is.null(x) || length(x) == 0) return("—")
  v <- suppressWarnings(as.numeric(x))
  if (length(v) == 1 && !is.finite(v)) return("—")
  if (!big) return(sprintf("$%.0f", v))
  units <- c("", "K", "M", "B", "T")
  fmt_one <- function(u) {
    if (!is.finite(u)) return("—")
    k <- min(floor(log10(abs(u) + 1e-9) / 3), length(units) - 1)
    sprintf("$%.2f%s", u / 10^(3 * k), units[k + 1])
  }
  vapply(v, fmt_one, character(1))
}

register_fonts <- function() {
  if (!requireNamespace("showtext", quietly = TRUE)) return(invisible(FALSE))
  showtext::showtext_auto()
  sysfonts <- requireNamespace("sysfonts", quietly = TRUE)
  if (!sysfonts) return(invisible(FALSE))
  candidates <- list(
    list(family = "Noto Sans SC", regular = "NotoSansSC-Regular.otf"),
    list(family = "Source Han Sans SC", regular = "SourceHanSansSC-Regular.otf"),
    list(family = "Microsoft YaHei", regular = "msyh.ttc"),
    list(family = "SimHei", regular = "simhei.ttf")
  )
  for (cand in candidates) {
    ok <- tryCatch({
      sysfonts::font_add(family = cand$family, regular = cand$regular)
      TRUE
    }, error = function(e) FALSE)
    if (ok) {
      logi("registered font: ", cand$family)
      return(invisible(cand$family))
    }
  }
  invisible(FALSE)
}

save_fig <- function(plot, name,
                     width = 12, height = 7, dpi = 400,
                     formats = c("png", "svg"),
                     dir = file.path(proj_root(), "分析输出", "图表")) {
  ensure_pkgs("ggplot2")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  has_ragg <- requireNamespace("ragg", quietly = TRUE)
  has_svglite <- requireNamespace("svglite", quietly = TRUE)
  for (fmt in formats) {
    path <- file.path(dir, paste0(name, ".", fmt))
    if (identical(fmt, "png") && has_ragg) {
      ggplot2::ggsave(filename = path, plot = plot,
                      width = width, height = height, dpi = dpi,
                      device = ragg::agg_png, bg = "white")
    } else if (identical(fmt, "svg") && has_svglite) {
      ggplot2::ggsave(filename = path, plot = plot,
                      width = width, height = height,
                      device = svglite::svglite, bg = "white")
    } else if (identical(fmt, "pdf")) {
      ggplot2::ggsave(filename = path, plot = plot,
                      width = width, height = height,
                      device = grDevices::cairo_pdf, bg = "white")
    } else {
      ggplot2::ggsave(filename = path, plot = plot,
                      width = width, height = height, dpi = dpi,
                      device = fmt, bg = "white")
    }
  }
  logi("saved: ", name, " (", paste(formats, collapse = ","), ")")
  invisible(file.path(dir, paste0(name, ".", formats)))
}

ghs_caption_zh <- function() {
  "数据来源: WHO Global Health Expenditure Database | 分析: 庄颂 (20241334)"
}
ghs_caption_en <- function() {
  "Source: WHO GHED | Analysis: Zhuang Song"
}
ghs_caption_bi <- function() {
  "\u6570\u636e\u6e90 Source: WHO Global Health Expenditure Database (GHED)  \u00b7  \u5206\u6790 Analysis: \u5e84\u9882 (20241334)"
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

is_nonempty <- function(x) !is.null(x) && length(x) > 0 && any(!is.na(x))

ghs_palette <- list(
  funding = c(
    gghed = "#1B5E88",
    pvtd  = "#E8833C",
    ext   = "#7A9C7A",
    oops  = "#C0504D",
    other = "#8D8D8D"
  ),
  scheme = c(
    hf1    = "#1B5E88",
    hf2    = "#4B9FD6",
    hf3    = "#C0504D",
    hf4    = "#7A9C7A",
    hfnec  = "#8D8D8D"
  ),
  purpose = c(
    hc1 = "#1B5E88", hc2 = "#4B9FD6", hc3 = "#6FA8A8",
    hc4 = "#C9B75D", hc5 = "#E8833C", hc6 = "#2E8B57",
    hc7 = "#7B4B94", hc9 = "#8D8D8D"
  ),
  continent = c(
    Africa   = "#E63946",
    Americas = "#457B9D",
    Asia     = "#F4A261",
    Europe   = "#2A9D8F",
    Oceania  = "#8E7CC3"
  ),
  income = c(
    `High income`         = "#1B5E88",
    `Upper middle income` = "#6FA8A8",
    `Lower middle income` = "#E8833C",
    `Low income`          = "#C0504D"
  )
)
