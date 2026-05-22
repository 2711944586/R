# =============================================================================
# 程序/01_io.R
# -----------------------------------------------------------------------------
# 原始数据读入：保留作业给定的三个对象名 financing_schemes / health_spending /
# spending_purpose。提供标准化的读取接口、类型修正和基础清洗。
# =============================================================================

if (!exists("proj_root", mode = "function")) {
  source(file.path("程序", "00_utils.R"))
}

# ---- 0. 数据集路径 ---------------------------------------------------------
ghs_data_dir <- function() {
  file.path(proj_root(), "原始数据")
}

ghs_csv_paths <- function() {
  d <- ghs_data_dir()
  list(
    financing_schemes = file.path(d, "financing_schemes.csv"),
    health_spending   = file.path(d, "health_spending.csv"),
    spending_purpose  = file.path(d, "spending_purpose.csv")
  )
}

# ---- 1. 通用读取 ------------------------------------------------------------
#' 读取单个 CSV，统一类型
#'
#' @param path 文件路径
#' @return tibble
read_ghs_csv <- function(path) {
  ensure_pkgs(c("readr"))
  readr::read_csv(
    path,
    col_types = readr::cols(
      country_name   = readr::col_character(),
      iso3_code      = readr::col_character(),
      year           = readr::col_integer(),
      indicator_code = readr::col_character(),
      value          = readr::col_double(),
      unit           = readr::col_character(),
      .default       = readr::col_character()
    ),
    progress = FALSE,
    show_col_types = FALSE
  )
}

# ---- 2. 三数据集入口 --------------------------------------------------------
#' 读取三张原始表；保留作业给定对象名
#'
#' @param assign_globals 若为 TRUE，在全局环境也赋值，供作业脚本直接使用
#' @return named list(financing_schemes, health_spending, spending_purpose)
load_ghs <- function(assign_globals = FALSE) {
  paths <- ghs_csv_paths()
  missing_files <- vapply(paths, function(p) !file.exists(p), logical(1))
  if (any(missing_files)) {
    stop(
      "Missing raw CSV(s): \n  ",
      paste(paths[missing_files], collapse = "\n  "),
      call. = FALSE
    )
  }
  logi("reading 3 raw CSVs from ", ghs_data_dir())
  out <- list(
    financing_schemes = read_ghs_csv(paths$financing_schemes),
    health_spending   = read_ghs_csv(paths$health_spending),
    spending_purpose  = read_ghs_csv(paths$spending_purpose)
  )
  if (isTRUE(assign_globals)) {
    assign("financing_schemes", out$financing_schemes, envir = globalenv())
    assign("health_spending",   out$health_spending,   envir = globalenv())
    assign("spending_purpose",  out$spending_purpose,  envir = globalenv())
  }
  out
}

# ---- 3. 数据摘要 ------------------------------------------------------------
#' 对三数据集给一个一页概览（行数、年份范围、国家数、指标数）
summarize_ghs <- function(ghs = load_ghs()) {
  ensure_pkgs(c("dplyr", "tibble"))
  mk_row <- function(df, name) {
    tibble::tibble(
      dataset       = name,
      rows          = nrow(df),
      countries     = dplyr::n_distinct(df$iso3_code),
      years_min     = suppressWarnings(min(df$year, na.rm = TRUE)),
      years_max     = suppressWarnings(max(df$year, na.rm = TRUE)),
      indicators    = dplyr::n_distinct(df$indicator_code),
      units         = paste(sort(unique(df$unit)), collapse = "; "),
      missing_value = sum(is.na(df$value))
    )
  }
  dplyr::bind_rows(
    mk_row(ghs$financing_schemes, "financing_schemes"),
    mk_row(ghs$health_spending,   "health_spending"),
    mk_row(ghs$spending_purpose,  "spending_purpose")
  )
}

# ---- 4. 指标字典 ------------------------------------------------------------
#' 指标编码 -> 中文解释 字典
ghs_indicator_dict <- function() {
  tibble::tribble(
    ~prefix,  ~dataset,            ~group,   ~label_en,                                                        ~label_zh,
    "che",    "health_spending",   "total",  "Current Health Expenditure",                                     "\u5f53\u524d\u536b\u751f\u603b\u652f\u51fa",
    "gghed",  "health_spending",   "source", "Domestic General Government Health Expenditure",                 "\u56fd\u5185\u653f\u5e9c\u536b\u751f\u652f\u51fa",
    "pvtd",   "health_spending",   "source", "Domestic Private Health Expenditure",                            "\u56fd\u5185\u79c1\u4eba\u536b\u751f\u652f\u51fa",
    "ext",    "health_spending",   "source", "External Health Expenditure",                                    "\u5916\u63f4\u536b\u751f\u652f\u51fa",
    "hf1",    "financing_schemes", "scheme", "Government & Compulsory Contributory Schemes",                   "\u653f\u5e9c\u4e0e\u5f3a\u5236\u6027\u7b79\u8d44",
    "hf2",    "financing_schemes", "scheme", "Voluntary Health Care Payment Schemes",                          "\u81ea\u613f\u6027\u4ed8\u8d39",
    "hf3",    "financing_schemes", "scheme", "Household Out-of-Pocket Payments (OOPS)",                        "\u5c45\u6c11\u81ea\u4ed8 (OOPS)",
    "hf4",    "financing_schemes", "scheme", "Rest of the World Financing Schemes",                            "\u5883\u5916\u7b79\u8d44",
    "hfnec",  "financing_schemes", "scheme", "Unspecified Financing Schemes",                                  "\u672a\u5206\u7c7b\u7b79\u8d44",
    "hc1",    "spending_purpose",  "purpose","Curative Care",                                                  "\u6cbb\u7597\u6027\u62a4\u7406",
    "hc2",    "spending_purpose",  "purpose","Rehabilitative Care",                                            "\u5eb7\u590d\u6027\u62a4\u7406",
    "hc3",    "spending_purpose",  "purpose","Long-term Care (Health)",                                        "\u957f\u671f\u62a4\u7406",
    "hc4",    "spending_purpose",  "purpose","Ancillary Services",                                             "\u8f85\u52a9\u670d\u52a1",
    "hc5",    "spending_purpose",  "purpose","Medical Goods",                                                  "\u533b\u7597\u7528\u54c1",
    "hc6",    "spending_purpose",  "purpose","Preventive Care",                                                "\u9884\u9632\u6027\u62a4\u7406",
    "hc7",    "spending_purpose",  "purpose","Governance & Health System Administration",                     "\u6cbb\u7406\u4e0e\u7ba1\u7406",
    "hc9",    "spending_purpose",  "purpose","Other Health Care Services (n.e.c.)",                            "\u5176\u4ed6"
  )
}

#' 取 indicator_code 的前缀（去掉 _che / _usd2023 后缀）
ghs_indicator_prefix <- function(code) {
  sub("_(che|usd2023)$", "", code)
}

ghs_indicator_unit <- function(code) {
  dplyr::case_when(
    grepl("_che$", code)     ~ "%_che",
    grepl("_usd2023$", code) ~ "usd2023",
    TRUE                     ~ NA_character_
  )
}
