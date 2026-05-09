# =============================================================================
# 程序/02_clean.R
# -----------------------------------------------------------------------------
# 清洗、透视、校验。核心产出：
#   - long_tidy(): 长表带单位标签，便于 ggplot facet
#   - to_wide():   每国 - 年一行，列为 <prefix>_<unit>，便于建模
#   - check_scheme_sum(): hf1..hfnec 之和应 ≈ 100%，输出偏差日志
# =============================================================================

if (!exists("proj_root", mode = "function")) {
  source(file.path("程序", "00_utils.R"))
}
if (!exists("load_ghs", mode = "function")) {
  source(file.path("程序", "01_io.R"))
}

# ---- 1. long tidy -----------------------------------------------------------
#' 长表：统一列名、加 prefix / unit_type 辅助列
long_tidy <- function(ghs = load_ghs()) {
  ensure_pkgs(c("dplyr"))
  bind_one <- function(df, dataset_name, label_col) {
    df |>
      dplyr::mutate(
        dataset      = dataset_name,
        prefix       = ghs_indicator_prefix(.data$indicator_code),
        unit_type    = ghs_indicator_unit(.data$indicator_code),
        label        = .data[[label_col]]
      )
  }
  out <- dplyr::bind_rows(
    bind_one(ghs$financing_schemes, "financing_schemes", "financing_scheme"),
    bind_one(ghs$health_spending,   "health_spending",   "expenditure_type"),
    bind_one(ghs$spending_purpose,  "spending_purpose",  "spending_purpose")
  )
  out |>
    dplyr::mutate(
      value = dplyr::if_else(.data$value < 0, 0, .data$value)
    ) |>
    dplyr::select(
      dplyr::any_of(c(
        "dataset", "country_name", "iso3_code", "year",
        "indicator_code", "prefix", "unit_type", "label", "value", "unit"
      ))
    )
}

# ---- 2. 宽表透视 ------------------------------------------------------------
#' 把一个数据集透视为宽表：每国-每年一行，列名 = indicator_code
#' 这样 hf1_che / gghed_usd2023 / hc6_che 等都直接对应原始编码，
#' 下游所有函数（plots / models）使用一致的命名。
#'
#' @param df long_tidy() 的某一 dataset 子集，或原始 CSV df
#' @return 宽 tibble
to_wide <- function(df) {
  ensure_pkgs(c("dplyr", "tidyr"))
  df |>
    dplyr::select(dplyr::any_of(c(
      "country_name", "iso3_code", "year", "indicator_code", "value"
    ))) |>
    tidyr::pivot_wider(
      names_from = "indicator_code",
      values_from = "value",
      values_fn  = ~ mean(.x, na.rm = TRUE)
    )
}

# ---- 3. 校验：hf1..hfnec 占比之和 ≈ 100 ------------------------------------
check_scheme_sum <- function(financing_schemes, tol = 5) {
  ensure_pkgs(c("dplyr"))
  financing_schemes |>
    dplyr::filter(grepl("_che$", .data$indicator_code)) |>
    dplyr::mutate(prefix = ghs_indicator_prefix(.data$indicator_code)) |>
    dplyr::filter(.data$prefix %in% c("hf1", "hf2", "hf3", "hf4", "hfnec")) |>
    dplyr::group_by(.data$country_name, .data$iso3_code, .data$year) |>
    dplyr::summarise(
      pct_sum = sum(.data$value, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      deviation   = .data$pct_sum - 100,
      within_tol  = abs(.data$deviation) <= tol
    )
}

#' 同样的逻辑给 health_spending 的 gghed/pvtd/ext
check_source_sum <- function(health_spending, tol = 5) {
  ensure_pkgs(c("dplyr"))
  health_spending |>
    dplyr::filter(grepl("_che$", .data$indicator_code)) |>
    dplyr::mutate(prefix = ghs_indicator_prefix(.data$indicator_code)) |>
    dplyr::filter(.data$prefix %in% c("gghed", "pvtd", "ext")) |>
    dplyr::group_by(.data$country_name, .data$iso3_code, .data$year) |>
    dplyr::summarise(
      pct_sum = sum(.data$value, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      deviation  = .data$pct_sum - 100,
      within_tol = abs(.data$deviation) <= tol
    )
}

# ---- 4. 便捷宽表：带收入组、大洲等 -------------------------------------------
#' 三合一宽表：按 country-year 一行，列包含所有指标 × 单位
build_master_wide <- function(ghs = load_ghs()) {
  ensure_pkgs(c("dplyr"))
  w_fs <- to_wide(ghs$financing_schemes)
  w_hs <- to_wide(ghs$health_spending)
  w_sp <- to_wide(ghs$spending_purpose)
  w_hs |>
    dplyr::full_join(w_fs, by = c("country_name", "iso3_code", "year")) |>
    dplyr::full_join(w_sp, by = c("country_name", "iso3_code", "year")) |>
    dplyr::arrange(.data$country_name, .data$year)
}
