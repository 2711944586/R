# =============================================================================
# 程序/16_data_quality.R  ·  数据质量诊断（v2）
# -----------------------------------------------------------------------------
# - 缺失模式（按列、按年份、按收入组）
# - 加总一致性（hf1+...+hfnec ≈ 100；gghed+pvtd+ext ≈ 100）
# - 异常值（IQR + 3σ 双标准）
# - 数据完备度报告（适合附录方法论引用）
# =============================================================================

#' 缺失值矩阵：返回 list(by_var, by_year, by_income, total_pct_missing)
#' @export
data_quality_missing <- function(master) {
  if (!is.data.frame(master)) return(list())
  cols_num <- names(master)[vapply(master, is.numeric, logical(1))]
  cols_num <- setdiff(cols_num, "year")

  by_var <- data.frame(
    column = cols_num,
    n_total = nrow(master),
    n_missing = vapply(master[cols_num], function(v) sum(is.na(v)), integer(1)),
    stringsAsFactors = FALSE
  )
  by_var$pct_missing <- by_var$n_missing / by_var$n_total * 100
  by_var <- by_var[order(-by_var$pct_missing), ]

  by_year <- master |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      n_rows = dplyr::n(),
      n_cells_missing = sum(is.na(dplyr::pick(dplyr::all_of(cols_num)))),
      pct_missing = n_cells_missing / (n_rows * length(cols_num)) * 100,
      .groups = "drop"
    )

  by_income <- if ("income_group" %in% names(master)) {
    master |>
      dplyr::group_by(income_group) |>
      dplyr::summarise(
        n_rows = dplyr::n(),
        n_cells_missing = sum(is.na(dplyr::pick(dplyr::all_of(cols_num)))),
        pct_missing = n_cells_missing / (n_rows * length(cols_num)) * 100,
        .groups = "drop"
      )
  } else NULL

  total_pct <- mean(is.na(as.matrix(master[cols_num]))) * 100

  list(
    by_var = by_var,
    by_year = by_year,
    by_income = by_income,
    total_pct_missing = total_pct
  )
}

#' 加总一致性：hf1+hf2+hf3+hf4+hfnec ≈ 100；gghed+pvtd+ext ≈ 100
#' @export
data_quality_consistency <- function(master, tol = 5) {
  if (!is.data.frame(master)) return(list())

  res <- list()

  if (all(c("hf1_che", "hf2_che", "hf3_che", "hf4_che", "hfnec_che")
          %in% names(master))) {
    s <- with(master,
      hf1_che + hf2_che + hf3_che + hf4_che + ifelse(is.na(hfnec_che), 0, hfnec_che))
    res$hf <- data.frame(
      n_rows = sum(!is.na(s)),
      mean = mean(s, na.rm = TRUE),
      median = stats::median(s, na.rm = TRUE),
      pct_within_tol = mean(abs(s - 100) < tol, na.rm = TRUE) * 100
    )
  }

  if (all(c("gghed_che", "pvtd_che", "ext_che") %in% names(master))) {
    s <- with(master, gghed_che + pvtd_che + ext_che)
    res$source <- data.frame(
      n_rows = sum(!is.na(s)),
      mean = mean(s, na.rm = TRUE),
      median = stats::median(s, na.rm = TRUE),
      pct_within_tol = mean(abs(s - 100) < tol, na.rm = TRUE) * 100
    )
  }

  res
}

#' 异常值：IQR + 3σ 双标准，返回标记
#' @export
data_quality_outliers <- function(master, cols = c("che_pc_usd2023", "hf3_che")) {
  cols <- intersect(cols, names(master))
  if (length(cols) == 0) return(data.frame())

  out <- lapply(cols, function(c) {
    x <- master[[c]]
    if (!is.numeric(x)) return(NULL)
    qs <- stats::quantile(x, c(0.25, 0.75), na.rm = TRUE)
    iqr <- qs[2] - qs[1]
    lo_iqr <- qs[1] - 1.5 * iqr
    hi_iqr <- qs[2] + 1.5 * iqr
    mu <- mean(x, na.rm = TRUE)
    sd <- stats::sd(x, na.rm = TRUE)
    lo_3sd <- mu - 3 * sd
    hi_3sd <- mu + 3 * sd
    data.frame(
      column = c,
      n = sum(!is.na(x)),
      n_iqr_outliers = sum(x < lo_iqr | x > hi_iqr, na.rm = TRUE),
      n_3sd_outliers = sum(x < lo_3sd | x > hi_3sd, na.rm = TRUE),
      iqr_lo = lo_iqr, iqr_hi = hi_iqr,
      sd3_lo = lo_3sd, sd3_hi = hi_3sd,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, out)
}

#' 一键导出质量报告（CSV → 分析输出/模型表/data_quality_*.csv）
#' @export
data_quality_export <- function(master,
                                 out_dir = file.path("分析输出", "模型表")) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  miss <- data_quality_missing(master)
  if (length(miss)) {
    if (!is.null(miss$by_var))    utils::write.csv(miss$by_var,
        file.path(out_dir, "data_quality_missing_by_var.csv"), row.names = FALSE)
    if (!is.null(miss$by_year))   utils::write.csv(miss$by_year,
        file.path(out_dir, "data_quality_missing_by_year.csv"), row.names = FALSE)
    if (!is.null(miss$by_income)) utils::write.csv(miss$by_income,
        file.path(out_dir, "data_quality_missing_by_income.csv"), row.names = FALSE)
  }

  cons <- data_quality_consistency(master)
  if (length(cons)) {
    df <- do.call(rbind, lapply(names(cons), function(k) {
      d <- cons[[k]]; d$check <- k; d
    }))
    utils::write.csv(df,
      file.path(out_dir, "data_quality_consistency.csv"),
      row.names = FALSE)
  }

  ol <- data_quality_outliers(master)
  if (nrow(ol)) utils::write.csv(ol,
    file.path(out_dir, "data_quality_outliers.csv"),
    row.names = FALSE)

  summary <- data.frame(
    metric = c("rows", "cols",
               "missing_pct_total",
               "hf_consistency_pct_within_5pp",
               "source_consistency_pct_within_5pp"),
    value = c(
      nrow(master), ncol(master),
      round(if (!is.null(miss$total_pct_missing))
              miss$total_pct_missing else NA_real_, 3),
      round(if (!is.null(cons$hf))
              cons$hf$pct_within_tol else NA_real_, 3),
      round(if (!is.null(cons$source))
              cons$source$pct_within_tol else NA_real_, 3)
    )
  )
  utils::write.csv(summary,
    file.path(out_dir, "data_quality_summary.csv"),
    row.names = FALSE)

  invisible(summary)
}
