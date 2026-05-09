# =============================================================================
# 程序/04_metrics.R
# -----------------------------------------------------------------------------
# 指标计算：不平等、结构、离散度等。
# =============================================================================

if (!exists("proj_root", mode = "function")) {
  source(file.path("程序", "00_utils.R"))
}

# ---- 1. 不平等指数 ----------------------------------------------------------
#' 加权基尼系数（Dagum 2000 式）
gini_weighted <- function(x, w = NULL) {
  if (is.null(w)) w <- rep(1, length(x))
  ok <- is.finite(x) & is.finite(w) & x >= 0 & w > 0
  x <- x[ok]; w <- w[ok]
  if (length(x) < 2) return(NA_real_)
  ord <- order(x)
  x <- x[ord]; w <- w[ord]
  cum_w <- cumsum(w)
  cum_xw <- cumsum(x * w)
  total_w <- sum(w); total_xw <- sum(x * w)
  if (total_xw == 0) return(0)
  g <- 1 - 2 * sum((cum_xw - x * w / 2) * w) / (total_w * total_xw)
  g
}

#' Theil-T 指数（人口加权）
theil_t <- function(x, w = NULL) {
  if (is.null(w)) w <- rep(1, length(x))
  ok <- is.finite(x) & is.finite(w) & x > 0 & w > 0
  x <- x[ok]; w <- w[ok]
  if (length(x) < 2) return(NA_real_)
  mean_x <- stats::weighted.mean(x, w)
  if (mean_x == 0) return(0)
  sum(w * x / sum(w * x) * log(x / mean_x))
}

#' Atkinson 指数 (ε)
atkinson <- function(x, w = NULL, eps = 0.5) {
  if (is.null(w)) w <- rep(1, length(x))
  ok <- is.finite(x) & is.finite(w) & x > 0 & w > 0
  x <- x[ok]; w <- w[ok]
  if (length(x) < 2) return(NA_real_)
  mean_x <- stats::weighted.mean(x, w)
  if (eps == 1) {
    ede <- exp(stats::weighted.mean(log(x), w))
  } else {
    ede <- (stats::weighted.mean(x^(1 - eps), w))^(1 / (1 - eps))
  }
  1 - ede / mean_x
}

# ---- 2. 时间序列的不平等 ---------------------------------------------------
#' 按 year 计算全球人均 CHE 的不平等指数面板
inequality_by_year <- function(master, value_col = "che_pc_usd2023",
                               weight_col = "pop") {
  ensure_pkgs(c("dplyr", "tidyr", "tibble"))
  val <- rlang::sym(value_col)
  wgt <- rlang::sym(weight_col)
  master |>
    dplyr::filter(
      is.finite(!!val), !!val > 0,
      is.finite(!!wgt) | TRUE
    ) |>
    dplyr::group_by(.data$year) |>
    dplyr::summarise(
      n            = dplyr::n(),
      gini_eq      = gini_weighted(!!val, NULL),
      gini_pop     = if (weight_col %in% names(master)) {
        gini_weighted(!!val, !!wgt)
      } else NA_real_,
      theil_pop    = if (weight_col %in% names(master)) {
        theil_t(!!val, !!wgt)
      } else NA_real_,
      atk05        = atkinson(!!val, NULL, eps = 0.5),
      atk1         = atkinson(!!val, NULL, eps = 1),
      mean_val     = mean(!!val, na.rm = TRUE),
      median_val   = median(!!val, na.rm = TRUE),
      .groups      = "drop"
    )
}

# ---- 3. 集中率 --------------------------------------------------------------
#' Top-k 国家占全球总支出的份额
top_k_share <- function(df, value_col, k = 20) {
  ensure_pkgs("dplyr")
  val <- rlang::sym(value_col)
  df |>
    dplyr::filter(is.finite(!!val)) |>
    dplyr::arrange(dplyr::desc(!!val)) |>
    dplyr::mutate(rank = dplyr::row_number()) |>
    dplyr::summarise(
      total     = sum(!!val, na.rm = TRUE),
      top_k_sum = sum((!!val)[.data$rank <= k], na.rm = TRUE),
      share     = .data$top_k_sum / .data$total,
      n         = dplyr::n()
    )
}

# ---- 4. 结构变化指数 --------------------------------------------------------
#' 结构变化距离：两年间份额向量的 cosine distance
struct_change <- function(shares_a, shares_b) {
  a <- as.numeric(shares_a); b <- as.numeric(shares_b)
  stopifnot(length(a) == length(b))
  if (all(!is.finite(a)) || all(!is.finite(b))) return(NA_real_)
  dot <- sum(a * b, na.rm = TRUE)
  na_norm <- sqrt(sum(a^2, na.rm = TRUE))
  nb_norm <- sqrt(sum(b^2, na.rm = TRUE))
  if (na_norm == 0 || nb_norm == 0) return(NA_real_)
  1 - dot / (na_norm * nb_norm)
}

# ---- 5. COVID 冲击度 --------------------------------------------------------
#' 对每国计算 2020-2022 相对 2019 的相对变化（% of CHE 口径与 USD 口径各一）
covid_shock <- function(master, base_year = 2019,
                        shock_years = 2020:2022) {
  ensure_pkgs("dplyr")
  master |>
    dplyr::filter(.data$year %in% c(base_year, shock_years)) |>
    dplyr::mutate(
      phase = dplyr::if_else(.data$year == base_year, "base", "shock")
    ) |>
    dplyr::group_by(.data$iso3_code, .data$country_name) |>
    dplyr::summarise(
      base_che    = mean(.data$che_usd2023[.data$phase == "base"], na.rm = TRUE),
      shock_che   = mean(.data$che_usd2023[.data$phase == "shock"], na.rm = TRUE),
      base_oops   = mean(.data$hf3_che[.data$phase == "base"], na.rm = TRUE),
      shock_oops  = mean(.data$hf3_che[.data$phase == "shock"], na.rm = TRUE),
      base_gghed  = mean(.data$gghed_che[.data$phase == "base"], na.rm = TRUE),
      shock_gghed = mean(.data$gghed_che[.data$phase == "shock"], na.rm = TRUE),
      .groups     = "drop"
    ) |>
    dplyr::mutate(
      che_delta_pct   = (.data$shock_che  - .data$base_che)  / .data$base_che  * 100,
      oops_delta_pp   =  .data$shock_oops - .data$base_oops,
      gghed_delta_pp  =  .data$shock_gghed - .data$base_gghed
    )
}
