# =============================================================================
# 程序/00_utils.R
# -----------------------------------------------------------------------------
# 通用小工具：路径、日志、缓存、数值格式化、字体、颜色、异常处理。
# =============================================================================

# ---- 0. package-safe imports -------------------------------------------------
#' 确保命名空间可用，必要时加载
#'
#' @param pkg 包名字符向量
#' @return 不可用的包名（长度 0 代表全部就位）
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

# ---- 1. 路径 ----------------------------------------------------------------
#' 项目根路径（兼容 RStudio / VSCode / Shiny）
#' 中文路径下 `here` 包会因 normalizePath 多字节字符串错误而 onLoad 失败，
#' 这里用 tryCatch 优雅降级到 getwd() / 向上找 .Rprofile 的父目录。
proj_root <- function() {
  root_here <- tryCatch(here::here(), error = function(e) NULL, warning = function(w) NULL)
  if (!is.null(root_here) && dir.exists(root_here)) return(root_here)
  wd <- getwd()
  for (up in 0:4) {
    candidate <- do.call(file.path, c(list(wd), as.list(rep("..", up))))
    candidate <- normalizePath(candidate, mustWork = FALSE)
    if (any(file.exists(file.path(candidate, c(".Rprofile", "DESCRIPTION", "项目文档/项目方案.md"))))) {
      return(candidate)
    }
  }
  wd
}

#' 拼路径并确保父目录存在
path_ensure <- function(...) {
  p <- file.path(...)
  dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
  p
}

# ---- 2. 日志 ----------------------------------------------------------------
.log_stamp <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")
logi <- function(...) message("[", .log_stamp(), "] [INFO] ", ...)
logw <- function(...) message("[", .log_stamp(), "] [WARN] ", ...)
loge <- function(...) message("[", .log_stamp(), "] [ERR ] ", ...)

# ---- 3. 缓存 ----------------------------------------------------------------
#' 文件缓存: 如果 cache 已存在则读, 否则执行 expr 并写缓存
#'
#' @param key 缓存标识（无扩展名）
#' @param expr 需要求值的表达式
#' @param dir 缓存目录
#' @param format 'rds' 或 'parquet'（parquet 依赖 arrow）
#' @param force 强制重算
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

# ---- 4. 数值格式化 ----------------------------------------------------------
#' 把 USD 数值格式化为 $1.2B / $345M / $67K
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

# ---- 5. 字体 ----------------------------------------------------------------
#' 注册中文字体（思源黑体/微软雅黑 fallback）到 showtext
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

# ---- 6. 保存图 --------------------------------------------------------------
#' 统一保存 ggplot 到 figures 目录（同时导出 png/svg）
#'
#' 优先使用 ragg::agg_png / svglite::svglite，它们原生支持系统字体（含中文），
#' 避免 showtext 与默认 Cairo 设备冲突导致的 "豆腐块" 问题。
save_fig <- function(plot, name,
                     width = 10, height = 6, dpi = 300,
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

# ---- 7. 标准信息/注释文本 ---------------------------------------------------
ghs_caption_zh <- function() {
  "数据来源: WHO Global Health Expenditure Database | 分析: 庄颂 (20241334)"
}
ghs_caption_en <- function() {
  "Source: WHO GHED | Analysis: Zhuang Song"
}
ghs_caption_bi <- function() {
  "\u6570\u636e\u6e90 Source: WHO Global Health Expenditure Database (GHED)  \u00b7  \u5206\u6790 Analysis: \u5e84\u9882 (20241334)"
}

# ---- 8. 管道友好的空检 ------------------------------------------------------
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

#' 判断向量是否非空
is_nonempty <- function(x) !is.null(x) && length(x) > 0 && any(!is.na(x))

# ---- 9. 颜色 ----------------------------------------------------------------
#' 项目标准配色
ghs_palette <- list(
  funding = c(
    gghed = "#1B5E88",   # 政府（深蓝）
    pvtd  = "#E8833C",   # 私人（琥珀）
    ext   = "#7A9C7A",   # 外援（橄榄绿）
    oops  = "#C0504D",   # 自付（红）
    other = "#8D8D8D"    # 灰
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
