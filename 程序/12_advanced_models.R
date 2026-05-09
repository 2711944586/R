# =============================================================================
# 程序/12_advanced_models.R  ---  v2 高级统计模型
# -----------------------------------------------------------------------------
#  M1 · catastrophic_oop_share()    — 灾难性医疗支出占比（>10/25%）
#  M2 · fit_elasticity_segments()   — 分段弹性（log-log + breakpoint）
#  M3 · fit_dea_efficiency()        — 简化 DEA 效率前沿（CRS / VRS）
#  M4 · mc_scenarios()              — 蒙特卡罗 1000 次情景模拟
#  M5 · fit_lorenz()                — Lorenz 曲线 + 基尼收敛分解
#  M6 · concentration_index()       — Kakwani / 累进性
#  M7 · fit_panel_iv()              — 工具变量替代式
#  M8 · changepoint_panel()         — 国家级变点检测面板
#  M9 · fit_growth_decomposition()  — 增长分解（Δlog CHE = β·Δlog GDP + …）
# =============================================================================

if (!exists("proj_root", mode = "function")) {
  source(file.path("程序", "00_utils.R"))
}

# ---- M1 · 灾难性医疗支出 ---------------------------------------------------

#' 灾难性 OOP 支出占人口比（已有指标用 WHO 数据，未有则用近似）
#'
#' @param master_panel 含 hf3_che 与 health_oop_pct 的数据
#' @param threshold 阈值（10 或 25，对应 WHO/WB 标准）
catastrophic_oop_share <- function(master_panel, threshold = c(10, 25)) {
  threshold <- match.arg(as.character(threshold), c("10", "25"))
  ensure_pkgs("dplyr")
  col <- if (threshold == "10") "UHC_FIN" else "UHC_FIN25"
  if (!col %in% names(master_panel)) {
    return(tibble::tibble(iso3_code = character(),
                          year = integer(),
                          catastrophic_pct = numeric()))
  }
  master_panel |>
    dplyr::filter(is.finite(.data[[col]])) |>
    dplyr::transmute(
      iso3_code = .data$iso3_code,
      year      = .data$year,
      catastrophic_pct = .data[[col]]
    )
}

# ---- M2 · 分段弹性 ----------------------------------------------------------

#' log(CHE/cap) 对 log(GDP/cap) 的弹性，分段：低/中/高收入区
#'
#' @param master_panel master 宽表
#' @param breaks log GDP/cap 断点（默认 7.5, 9.5 ≈ ~$1800, $13400）
fit_elasticity_segments <- function(master_panel, breaks = c(7.5, 9.5)) {
  ensure_pkgs("dplyr")
  d <- master_panel |>
    dplyr::filter(is.finite(.data$che_pc_usd2023), .data$che_pc_usd2023 > 0,
                  is.finite(.data$gdp_pc_usd),     .data$gdp_pc_usd > 0) |>
    dplyr::transmute(
      iso3_code = .data$iso3_code,
      year      = .data$year,
      log_che   = log(.data$che_pc_usd2023),
      log_gdp   = log(.data$gdp_pc_usd),
      seg = cut(log(.data$gdp_pc_usd), c(-Inf, breaks, Inf),
                labels = c("Low", "Mid", "High"))
    )
  if (nrow(d) < 30) return(NULL)
  fits <- d |>
    dplyr::group_by(.data$seg) |>
    dplyr::summarise(
      n        = dplyr::n(),
      beta     = stats::coef(stats::lm(log_che ~ log_gdp, data = dplyr::cur_data()))[["log_gdp"]],
      se       = summary(stats::lm(log_che ~ log_gdp, data = dplyr::cur_data()))$coefficients["log_gdp", "Std. Error"],
      r2       = summary(stats::lm(log_che ~ log_gdp, data = dplyr::cur_data()))$r.squared,
      .groups  = "drop"
    ) |>
    dplyr::mutate(
      ci_lo = .data$beta - 1.96 * .data$se,
      ci_hi = .data$beta + 1.96 * .data$se
    )
  list(panel = d, fits = fits)
}

# ---- M3 · 简化 DEA 效率前沿 -----------------------------------------------

#' 单输入（CHE/cap）单输出（HALE）DEA — CRS 假设
#'
#' @param master_panel 含 che_pc_usd2023 + HALE
#' @param year_focus 关注年（HALE 数据 2000/2010/2019/2021）
fit_dea_efficiency <- function(master_panel, year_focus = 2019) {
  ensure_pkgs("dplyr")
  # 优雅处理缺 HALE 列（外部 GHO/GBD 数据未拉取时返回 NULL）
  if (!all(c("HALE", "che_pc_usd2023") %in% names(master_panel))) {
    return(NULL)
  }
  d <- master_panel |>
    dplyr::filter(.data$year == year_focus,
                  is.finite(.data$che_pc_usd2023), .data$che_pc_usd2023 > 0,
                  is.finite(.data$HALE), .data$HALE > 0)
  if (nrow(d) < 10) return(NULL)
  # 简化 CRS DEA：效率 = (HALE / CHE_pc) / max(HALE / CHE_pc)
  d <- d |>
    dplyr::mutate(
      ratio = .data$HALE / .data$che_pc_usd2023,
      eff_crs = .data$ratio / max(.data$ratio, na.rm = TRUE)
    )
  # VRS 近似：分收入组归一
  if ("income_group" %in% names(d)) {
    d <- d |>
      dplyr::group_by(.data$income_group) |>
      dplyr::mutate(eff_vrs = .data$ratio / max(.data$ratio, na.rm = TRUE)) |>
      dplyr::ungroup()
  } else {
    d$eff_vrs <- d$eff_crs
  }
  d |>
    dplyr::select(dplyr::any_of(c("iso3_code", "country_name", "year",
                                   "continent", "income_group",
                                   "che_pc_usd2023", "HALE",
                                   "ratio", "eff_crs", "eff_vrs"))) |>
    dplyr::arrange(dplyr::desc(.data$eff_crs))
}

# ---- M4 · 蒙特卡罗情景 ----------------------------------------------------

#' 1000 次模拟 OECD 国家"维持 7% GDP 投入"对寿命的增益
#'
#' @param master_panel 主面板
#' @param target_pct 目标 CHE/GDP 比例
#' @param n_sims 模拟次数
#' @param horizon 模拟年数
mc_scenarios <- function(master_panel, target_pct = 0.07,
                         n_sims = 1000, horizon = 7,
                         seed = 42) {
  ensure_pkgs("dplyr")
  set.seed(seed)
  baseline <- master_panel |>
    dplyr::filter(.data$year == max(.data$year, na.rm = TRUE),
                  is.finite(.data$che_pc_usd2023),
                  is.finite(.data$gdp_pc_usd),
                  is.finite(.data$life_exp))
  if (!nrow(baseline)) return(NULL)
  # 弹性来自历史回归：每 +1% CHE/cap → 寿命 +η（年）
  elastic <- tryCatch({
    fit <- stats::lm(life_exp ~ log(che_pc_usd2023), data = baseline)
    as.numeric(stats::coef(fit)["log(che_pc_usd2023)"])
  }, error = function(e) 1.5)
  # 模拟
  sim <- expand.grid(iso3_code = baseline$iso3_code, sim = seq_len(n_sims),
                     year_offset = seq_len(horizon)) |>
    tibble::as_tibble() |>
    dplyr::left_join(baseline, by = "iso3_code") |>
    dplyr::mutate(
      noise   = stats::rnorm(dplyr::n(), 0, 0.05),
      che_target = .data$gdp_pc_usd * target_pct,
      che_path = .data$che_pc_usd2023 +
                 (.data$che_target - .data$che_pc_usd2023) *
                 (.data$year_offset / horizon) * (1 + .data$noise),
      gain_years = elastic * (log(.data$che_path) - log(.data$che_pc_usd2023))
    ) |>
    dplyr::filter(.data$year_offset == horizon) |>
    dplyr::group_by(.data$iso3_code, .data$country_name) |>
    dplyr::summarise(
      mean_gain = mean(.data$gain_years, na.rm = TRUE),
      lo80      = stats::quantile(.data$gain_years, 0.10, na.rm = TRUE),
      hi80      = stats::quantile(.data$gain_years, 0.90, na.rm = TRUE),
      lo95      = stats::quantile(.data$gain_years, 0.025, na.rm = TRUE),
      hi95      = stats::quantile(.data$gain_years, 0.975, na.rm = TRUE),
      .groups   = "drop"
    )
  list(elasticity = elastic, summary = sim, n_sims = n_sims, horizon = horizon)
}

# ---- M5 · Lorenz / 基尼分解 ------------------------------------------------

#' Lorenz 曲线坐标（人均 CHE 加权）
#'
#' @param values 数值向量（如人均 CHE）
#' @param weights 权重（如人口）
fit_lorenz <- function(values, weights = NULL) {
  ok <- is.finite(values) & values >= 0
  values <- values[ok]
  if (!length(values)) return(NULL)
  if (is.null(weights)) weights <- rep(1, length(values))
  weights <- weights[ok]
  ord <- order(values)
  v <- values[ord]; w <- weights[ord]
  cum_w <- cumsum(w) / sum(w)
  cum_v <- cumsum(v * w) / sum(v * w)
  tibble::tibble(p_pop = c(0, cum_w), p_value = c(0, cum_v))
}

# ---- M6 · 集中指数（Kakwani） ----------------------------------------------

#' Kakwani 累进性指数：C - G（C=支出集中指数, G=收入基尼）
#'
#' @param values 卫生支出额（每人）
#' @param income GDP/cap 等收入
concentration_index <- function(values, income) {
  if (!requireNamespace("ineq", quietly = TRUE)) return(NULL)
  ok <- is.finite(values) & is.finite(income) & income > 0
  values <- values[ok]; income <- income[ok]
  if (length(values) < 5) return(NULL)
  ord <- order(income)
  v <- values[ord]
  rk <- seq_along(v) / length(v)
  c_i <- 2 * stats::cov(v, rk) / mean(v)
  g_i <- ineq::Gini(income)
  list(C = c_i, G = g_i, kakwani = c_i - g_i)
}

# ---- M7 · 工具变量替代式 ---------------------------------------------------

#' 用 GDP 滞后期作为 IV，估计真实弹性
fit_panel_iv <- function(master_panel) {
  if (!requireNamespace("fixest", quietly = TRUE)) return(NULL)
  ensure_pkgs("dplyr")
  d <- master_panel |>
    dplyr::filter(is.finite(.data$che_pc_usd2023), .data$che_pc_usd2023 > 0,
                  is.finite(.data$gdp_pc_usd),     .data$gdp_pc_usd > 0) |>
    dplyr::arrange(.data$iso3_code, .data$year) |>
    dplyr::group_by(.data$iso3_code) |>
    dplyr::mutate(
      log_che = log(.data$che_pc_usd2023),
      log_gdp = log(.data$gdp_pc_usd),
      log_gdp_lag1 = dplyr::lag(.data$log_gdp, 1),
      log_gdp_lag2 = dplyr::lag(.data$log_gdp, 2)
    ) |>
    dplyr::ungroup() |>
    tidyr::drop_na(dplyr::any_of(c("log_che", "log_gdp",
                                     "log_gdp_lag1", "log_gdp_lag2")))
  if (nrow(d) < 100) return(NULL)
  tryCatch(
    fixest::feols(log_che ~ 1 | iso3_code + year |
                  log_gdp ~ log_gdp_lag1 + log_gdp_lag2,
                  data = d, cluster = ~iso3_code),
    error = function(e) NULL
  )
}

# ---- M8 · 国家变点检测面板 ------------------------------------------------

#' 对每国 OOPS 时序做 PELT 变点检测，输出全国汇总表
changepoint_panel <- function(master_panel,
                                indicator_col = "hf3_che",
                                min_n = 10) {
  if (!requireNamespace("changepoint", quietly = TRUE)) return(NULL)
  ensure_pkgs("dplyr")
  panel <- master_panel |>
    dplyr::filter(is.finite(.data[[indicator_col]])) |>
    dplyr::group_by(.data$iso3_code) |>
    dplyr::filter(dplyr::n() >= min_n) |>
    dplyr::ungroup()
  if (!nrow(panel)) return(NULL)
  isos <- unique(panel$iso3_code)
  rows <- lapply(isos, function(iso) {
    sub <- dplyr::filter(panel, .data$iso3_code == iso) |>
      dplyr::arrange(.data$year)
    res <- tryCatch(
      changepoint::cpt.mean(sub[[indicator_col]], method = "PELT", penalty = "BIC"),
      error = function(e) NULL
    )
    if (is.null(res)) return(NULL)
    cp_idx <- changepoint::cpts(res)
    if (!length(cp_idx)) return(NULL)
    tibble::tibble(
      iso3_code = iso,
      country_name = sub$country_name[1],
      changepoint_year = sub$year[cp_idx],
      n_obs = nrow(sub)
    )
  })
  dplyr::bind_rows(Filter(Negate(is.null), rows))
}

# ---- M9 · 增长分解 -------------------------------------------------------

#' Δ log(CHE) ≈ β·Δ log(GDP) + γ·Δ pop_65 + ε（OECD 子集）
fit_growth_decomposition <- function(master_panel) {
  if (!requireNamespace("fixest", quietly = TRUE)) return(NULL)
  ensure_pkgs("dplyr")
  d <- master_panel |>
    dplyr::filter(is.finite(.data$che_pc_usd2023), .data$che_pc_usd2023 > 0,
                  is.finite(.data$gdp_pc_usd),     .data$gdp_pc_usd > 0,
                  is.finite(.data$pop_65)) |>
    dplyr::arrange(.data$iso3_code, .data$year) |>
    dplyr::group_by(.data$iso3_code) |>
    dplyr::mutate(
      d_log_che = log(.data$che_pc_usd2023) - dplyr::lag(log(.data$che_pc_usd2023)),
      d_log_gdp = log(.data$gdp_pc_usd) - dplyr::lag(log(.data$gdp_pc_usd)),
      d_pop_65  = .data$pop_65 - dplyr::lag(.data$pop_65)
    ) |>
    dplyr::ungroup() |>
    tidyr::drop_na(dplyr::any_of(c("d_log_che", "d_log_gdp", "d_pop_65")))
  if (nrow(d) < 100) return(NULL)
  tryCatch(
    fixest::feols(d_log_che ~ d_log_gdp + d_pop_65 | iso3_code + year,
                  data = d, cluster = ~iso3_code),
    error = function(e) NULL
  )
}
