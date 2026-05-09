if (!exists("proj_root", mode = "function")) {
  source(file.path("程序", "00_utils.R"), encoding = "UTF-8")
}

.feature_pick <- function(df, names, default = NA) {
  hit <- names[names %in% colnames(df)]
  if (!length(hit)) return(rep(default, nrow(df)))
  df[[hit[[1]]]]
}

.feature_add_missing <- function(df, cols, default = NA_real_) {
  for (col in cols) {
    if (!col %in% colnames(df)) df[[col]] <- default
  }
  df
}

.feature_num <- function(x) {
  suppressWarnings(as.numeric(x))
}

.feature_div <- function(num, den) {
  num <- .feature_num(num)
  den <- .feature_num(den)
  out <- rep(NA_real_, length(num))
  ok <- is.finite(num) & is.finite(den) & den != 0
  out[ok] <- num[ok] / den[ok]
  out
}

.feature_log <- function(x) {
  x <- .feature_num(x)
  out <- rep(NA_real_, length(x))
  ok <- is.finite(x) & x > 0
  out[ok] <- log(x[ok])
  out
}

.feature_baseline <- function(x, year, base_year = 2000L) {
  x <- .feature_num(x)
  year <- suppressWarnings(as.integer(year))
  at_base <- which(year == base_year & is.finite(x))
  if (length(at_base)) return(x[at_base[[1]]])
  finite <- which(is.finite(x))
  if (length(finite)) return(x[finite[[1]]])
  NA_real_
}

.feature_load_cached_external <- function() {
  if (!exists("read_external_cache", mode = "function")) {
    src <- file.path("程序", "11_external_data.R")
    if (file.exists(src)) source(src, encoding = "UTF-8")
  }
  nms <- c("wdi", "gho", "imf", "oecd", "gbd")
  out <- stats::setNames(vector("list", length(nms)), nms)
  for (nm in nms) {
    out[[nm]] <- tryCatch({
      if (exists("read_external_cache", mode = "function")) read_external_cache(nm) else NULL
    }, error = function(e) NULL)
  }
  out
}

.feature_dictionary_seed <- function() {
  variables <- c(
    "iso3_code", "country_name", "year", "continent", "region23", "income_group",
    "che_usd2023", "che_pc_usd2023", "log_che_pc", "gdp_pc_usd", "log_gdp_pc",
    "che_gdp_pct", "pop", "life_exp", "u5mr", "life_exp_gain_since_2000",
    "u5mr_decline_since_2000", "gghed_che", "pvtd_che", "ext_che", "hf3_che",
    "public_to_oop_ratio", "oops_high_flag", "ext_high_flag", "che_pc_yoy_pct",
    "oops_yoy_pp", "gghed_yoy_pp", "covid_phase", "period", "income_rank",
    "pop_65", "urban_pct", "gdp_growth", "physicians", "hosp_beds",
    "HALE", "UHC_SCI", "UHC_FIN", "gov_debt_gdp", "gov_balance_gdp"
  )
  labels <- c(
    "ISO3 国家代码", "国家名称", "年份", "大洲", "23 区域", "世界银行收入组",
    "当年卫生总支出", "人均卫生支出", "人均卫生支出对数", "人均 GDP", "人均 GDP 对数",
    "卫生支出占 GDP 估计比例", "人口", "预期寿命", "五岁以下死亡率", "相对基准年的寿命增量",
    "相对基准年的 U5MR 下降", "政府卫生支出占 CHE", "国内私人卫生支出占 CHE", "外部援助占 CHE", "居民自付占 CHE",
    "政府支出与自付比例", "OOPS 高风险标记", "外援高依赖标记", "人均卫生支出同比增速",
    "OOPS 同比百分点变化", "GGHED 同比百分点变化", "COVID 时段", "分析分期", "收入组排序",
    "65 岁及以上人口占比", "城镇化率", "GDP 增速", "医生密度", "床位密度",
    "健康预期寿命", "UHC 服务覆盖指数", "灾难性卫生支出人口占比", "政府债务占 GDP", "政府财政余额占 GDP"
  )
  stopifnot(length(variables) == length(labels))
  out <- data.frame(
    variable = variables,
    label_zh = labels,
    source = "项目派生",
    unit = "按变量定义",
    formula = "原始或缓存字段",
    role = "covariate",
    stringsAsFactors = FALSE
  )
  set_prop <- function(vars, source = NULL, unit = NULL, formula = NULL, role = NULL) {
    idx <- match(vars, out$variable)
    idx <- idx[!is.na(idx)]
    if (!length(idx)) return(invisible(NULL))
    if (!is.null(source)) out[idx, "source"] <<- source
    if (!is.null(unit)) out[idx, "unit"] <<- unit
    if (!is.null(formula)) out[idx, "formula"] <<- formula
    if (!is.null(role)) out[idx, "role"] <<- role
    invisible(NULL)
  }
  set_prop(c("iso3_code", "country_name", "year"), "GHED / countrycode", "标识", "原始或映射字段", "key")
  set_prop(c("continent", "region23", "income_group"), "countrycode / World Bank", "分类", "国家元数据映射", "strata")
  set_prop(c("che_usd2023", "che_pc_usd2023"), "GHED", "2023 USD", "原始或缓存字段", "outcome / exposure")
  set_prop("log_che_pc", "GHED 派生", "log", "log(che_pc_usd2023)", "outcome / exposure")
  set_prop("gdp_pc_usd", "WDI", "constant USD", "原始或缓存字段", "exposure")
  set_prop("log_gdp_pc", "WDI 派生", "log", "log(gdp_pc_usd)", "exposure")
  set_prop("che_gdp_pct", "GHED + WDI", "%", "che_pc_usd2023 / gdp_pc_usd * 100", "exposure")
  set_prop("pop", "WDI", "person", "原始或缓存字段", "weight")
  set_prop(c("life_exp", "life_exp_gain_since_2000"), "WDI / WDI 派生", "year", "life_exp - baseline life_exp", "outcome")
  set_prop(c("u5mr", "u5mr_decline_since_2000"), "WDI / WDI 派生", "per 1,000 live births", "baseline u5mr - u5mr", "outcome")
  set_prop(c("gghed_che", "pvtd_che", "ext_che", "hf3_che"), "GHED", "% of CHE", "原始或缓存字段", "financing structure")
  set_prop("public_to_oop_ratio", "GHED 派生", "ratio", "gghed_che / hf3_che", "financing structure")
  set_prop(c("oops_high_flag", "ext_high_flag"), "GHED 派生", "0/1", "阈值标记", "risk flag")
  set_prop(c("che_pc_yoy_pct", "oops_yoy_pp", "gghed_yoy_pp"), "GHED 派生", "变化量", "本年值 - 滞后值", "time dynamics")
  set_prop(c("covid_phase", "period"), "项目分期", "class", "year 分类", "time dynamics")
  set_prop("income_rank", "World Bank", "ordinal", "income_group 映射", "strata")
  set_prop(c("pop_65", "urban_pct", "gdp_growth", "physicians", "hosp_beds"), "WDI", "WDI unit", "原始或缓存字段", "covariate")
  set_prop(c("HALE", "UHC_SCI", "UHC_FIN"), "WHO GHO", "GHO unit", "原始或缓存字段", "covariate / outcome")
  set_prop(c("gov_debt_gdp", "gov_balance_gdp"), "IMF", "% GDP", "原始或缓存字段", "fiscal covariate")
  out
}

build_feature_mart <- function(master_enriched = NULL, external = NULL) {
  ensure_pkgs(c("dplyr", "tibble"))
  if (is.null(master_enriched)) {
    cache <- file.path(proj_root(), "派生数据", "处理结果", "master_enriched.rds")
    if (!file.exists(cache)) stop("Missing master_enriched.rds. Run: Rscript 构建.R data", call. = FALSE)
    master_enriched <- readRDS(cache)
  }
  if (is.null(external)) external <- .feature_load_cached_external()
  panel <- if (exists("build_master_panel", mode = "function")) {
    build_master_panel(master_enriched, external = external)
  } else {
    master_enriched
  }
  if (!"country_name" %in% colnames(panel)) {
    panel$country_name <- .feature_pick(panel, c("country_name_iso", "country"), NA_character_)
  }
  for (nm in c("pop", "gdp_pc_usd", "life_exp", "u5mr")) {
    wdi_nm <- paste0(nm, "_wdi")
    if (wdi_nm %in% colnames(panel)) {
      base <- if (nm %in% colnames(panel)) panel[[nm]] else rep(NA_real_, nrow(panel))
      fill <- panel[[wdi_nm]]
      base[is.na(base) & !is.na(fill)] <- fill[is.na(base) & !is.na(fill)]
      panel[[nm]] <- base
    }
  }
  numeric_cols <- c(
    "che_usd2023", "che_pc_usd2023", "gdp_pc_usd", "pop", "life_exp", "u5mr",
    "gghed_che", "pvtd_che", "ext_che", "hf3_che", "pop_65", "urban_pct",
    "gdp_growth", "physicians", "hosp_beds", "HALE", "UHC_SCI", "UHC_FIN",
    "gov_debt_gdp", "gov_balance_gdp"
  )
  panel <- .feature_add_missing(panel, numeric_cols)
  panel <- .feature_add_missing(panel, c("continent", "region23", "income_group", "country_name"), NA_character_)
  panel |>
    dplyr::mutate(
      year = as.integer(.data$year),
      che_usd2023 = .feature_num(.data$che_usd2023),
      che_pc_usd2023 = .feature_num(.data$che_pc_usd2023),
      gdp_pc_usd = .feature_num(.data$gdp_pc_usd),
      pop = .feature_num(.data$pop),
      life_exp = .feature_num(.data$life_exp),
      u5mr = .feature_num(.data$u5mr),
      gghed_che = .feature_num(.data$gghed_che),
      pvtd_che = .feature_num(.data$pvtd_che),
      ext_che = .feature_num(.data$ext_che),
      hf3_che = .feature_num(.data$hf3_che),
      log_che_pc = .feature_log(.data$che_pc_usd2023),
      log_gdp_pc = .feature_log(.data$gdp_pc_usd),
      che_gdp_pct = .feature_div(.data$che_pc_usd2023, .data$gdp_pc_usd) * 100,
      public_to_oop_ratio = .feature_div(.data$gghed_che, .data$hf3_che),
      oops_high_flag = as.integer(is.finite(.data$hf3_che) & .data$hf3_che > 50),
      ext_high_flag = as.integer(is.finite(.data$ext_che) & .data$ext_che > 20),
      covid_phase = dplyr::case_when(
        .data$year <= 2019 ~ "pre_covid",
        .data$year %in% 2020:2022 ~ "covid_shock",
        .data$year >= 2023 ~ "post_covid",
        TRUE ~ NA_character_
      ),
      period = dplyr::case_when(
        .data$year >= 2000 & .data$year <= 2007 ~ "2000-2007",
        .data$year >= 2008 & .data$year <= 2015 ~ "2008-2015",
        .data$year >= 2016 & .data$year <= 2023 ~ "2016-2023",
        TRUE ~ NA_character_
      ),
      income_rank = dplyr::case_when(
        .data$income_group == "Low income" ~ 1L,
        .data$income_group == "Lower middle income" ~ 2L,
        .data$income_group == "Upper middle income" ~ 3L,
        .data$income_group == "High income" ~ 4L,
        TRUE ~ NA_integer_
      )
    ) |>
    dplyr::arrange(.data$iso3_code, .data$year) |>
    dplyr::group_by(.data$iso3_code) |>
    dplyr::mutate(
      che_pc_yoy_pct = (.feature_div(.data$che_pc_usd2023, dplyr::lag(.data$che_pc_usd2023)) - 1) * 100,
      oops_yoy_pp = .data$hf3_che - dplyr::lag(.data$hf3_che),
      gghed_yoy_pp = .data$gghed_che - dplyr::lag(.data$gghed_che),
      life_exp_gain_since_2000 = .data$life_exp - .feature_baseline(.data$life_exp, .data$year),
      u5mr_decline_since_2000 = .feature_baseline(.data$u5mr, .data$year) - .data$u5mr
    ) |>
    dplyr::ungroup() |>
    dplyr::select(dplyr::any_of(c(
      "iso3_code", "country_name", "year", "continent", "region23", "income_group",
      "che_usd2023", "che_pc_usd2023", "log_che_pc", "gdp_pc_usd", "log_gdp_pc",
      "che_gdp_pct", "pop", "life_exp", "u5mr", "life_exp_gain_since_2000",
      "u5mr_decline_since_2000", "gghed_che", "pvtd_che", "ext_che", "hf3_che",
      "public_to_oop_ratio", "oops_high_flag", "ext_high_flag", "che_pc_yoy_pct",
      "oops_yoy_pp", "gghed_yoy_pp", "covid_phase", "period", "income_rank",
      "pop_65", "urban_pct", "gdp_growth", "physicians", "hosp_beds",
      "HALE", "UHC_SCI", "UHC_FIN", "gov_debt_gdp", "gov_balance_gdp"
    )))
}

build_feature_dictionary <- function(feature_mart) {
  seed <- .feature_dictionary_seed()
  present <- seed$variable %in% colnames(feature_mart)
  dict <- seed[present, , drop = FALSE]
  dict$valid_n <- vapply(dict$variable, function(v) {
    x <- feature_mart[[v]]
    if (is.numeric(x)) sum(is.finite(x)) else sum(!is.na(x) & nzchar(as.character(x)))
  }, integer(1))
  dict$missing_rate <- vapply(dict$variable, function(v) {
    x <- feature_mart[[v]]
    miss <- if (is.numeric(x)) !is.finite(x) else is.na(x) | !nzchar(as.character(x))
    round(mean(miss), 4)
  }, numeric(1))
  dict
}

ghs_export_feature_mart <- function(master_enriched = NULL,
                                    external = NULL,
                                    out_dir = file.path(proj_root(), "派生数据", "处理结果")) {
  mart <- build_feature_mart(master_enriched = master_enriched, external = external)
  dict <- build_feature_dictionary(mart)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  mart_csv <- file.path(out_dir, "feature_mart_country_year.csv")
  mart_rds <- file.path(out_dir, "feature_mart_country_year.rds")
  dict_csv <- file.path(out_dir, "feature_dictionary.csv")
  utils::write.csv(mart, mart_csv, row.names = FALSE, fileEncoding = "UTF-8")
  saveRDS(mart, mart_rds)
  utils::write.csv(dict, dict_csv, row.names = FALSE, fileEncoding = "UTF-8")
  cat("[features] feature mart rows =", nrow(mart), "cols =", ncol(mart), "\n")
  cat("[features] wrote ", mart_csv, "\n", sep = "")
  cat("[features] wrote ", dict_csv, "\n", sep = "")
  invisible(list(mart = mart, dictionary = dict, files = c(mart_csv, mart_rds, dict_csv)))
}
