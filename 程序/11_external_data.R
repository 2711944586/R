# =============================================================================
# 程序/11_external_data.R  ---  多源外部数据
# -----------------------------------------------------------------------------
# 拉取 + 缓存 5 个外部数据源，全部带 offline fallback：
#   D2  WB WDI 扩展（30 指标：人口/GDP/老龄化/寿命/U5MR/CHE-GDP/教育...）
#   D3  WHO GHO（HALE / UHC SCI / 疫苗 / 烟草 / NCD / 灾难性 OOP）
#   D4  IMF GFS / WEO（财政平衡 / 政府债务 / GDP-PPP）
#   D5  OECD Health Statistics（OECD 子集：床位/医生密度/药品支出）
#   D6  IHME GBD lite（DALY / 死因前 10 / U5MR 分死因）
#
# 每个 fetcher 函数：
#   1. 优先读 派生数据/外部数据/<src>.parquet（commit 进 git）
#   2. 失败则尝试在线 API
#   3. 在线失败则返回 NULL（章节 chunk 容错）
#
# load_external_all() 一次性返回 list(wdi, gho, imf, oecd, gbd)
# =============================================================================

if (!exists("proj_root", mode = "function")) {
  source(file.path("程序", "00_utils.R"))
}

# ---- 0. 通用 cache ---------------------------------------------------------

.external_dir <- function() {
  d <- file.path(proj_root(), "派生数据", "外部数据")
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

#' 通用：读 parquet（fallback rds），写 parquet（fallback rds）
read_external_cache <- function(name) {
  base <- file.path(.external_dir(), name)
  parq <- paste0(base, ".parquet")
  rds  <- paste0(base, ".rds")
  if (file.exists(parq) && requireNamespace("arrow", quietly = TRUE)) {
    return(arrow::read_parquet(parq))
  }
  if (file.exists(rds)) return(readRDS(rds))
  NULL
}
write_external_cache <- function(df, name) {
  if (is.null(df) || !nrow(df)) return(invisible(NULL))
  base <- file.path(.external_dir(), name)
  if (requireNamespace("arrow", quietly = TRUE)) {
    try(arrow::write_parquet(df, paste0(base, ".parquet")), silent = TRUE)
  }
  try(saveRDS(df, paste0(base, ".rds")), silent = TRUE)
  invisible(df)
}

# ---- 1. D2 · World Bank WDI 扩展 -------------------------------------------

#' 30 个 WB WDI 指标的 R 名 → WB 编码
.wdi_indicators <- function() {
  c(
    pop          = "SP.POP.TOTL",
    pop_65       = "SP.POP.65UP.TO.ZS",
    pop_growth   = "SP.POP.GROW",
    urban_pct    = "SP.URB.TOTL.IN.ZS",
    gdp_pc_usd   = "NY.GDP.PCAP.KD",
    gdp_pc_ppp   = "NY.GDP.PCAP.PP.KD",
    gdp_growth   = "NY.GDP.MKTP.KD.ZG",
    gni_pc       = "NY.GNP.PCAP.KD",
    life_exp     = "SP.DYN.LE00.IN",
    life_exp_f   = "SP.DYN.LE00.FE.IN",
    life_exp_m   = "SP.DYN.LE00.MA.IN",
    u5mr         = "SH.DYN.MORT",
    imr          = "SP.DYN.IMRT.IN",
    mmr          = "SH.STA.MMRT",
    fertility    = "SP.DYN.TFRT.IN",
    smoking      = "SH.PRV.SMOK",
    obesity      = "SH.STA.OWAD.ZS",
    hosp_beds    = "SH.MED.BEDS.ZS",
    physicians   = "SH.MED.PHYS.ZS",
    nurses       = "SH.MED.NUMW.P3",
    health_pct_gdp  = "SH.XPD.CHEX.GD.ZS",
    health_pc_usd   = "SH.XPD.CHEX.PC.CD",
    health_oop_pct  = "SH.XPD.OOPC.CH.ZS",
    health_priv_pct = "SH.XPD.PVTD.CH.ZS",
    health_pub_pct  = "SH.XPD.GHED.CH.ZS",
    school_yrs   = "BAR.SCHL.15UP",
    edu_exp_gdp  = "SE.XPD.TOTL.GD.ZS",
    poverty_215  = "SI.POV.DDAY",
    co2_pc       = "EN.ATM.CO2E.PC",
    inflation    = "FP.CPI.TOTL.ZG"
  )
}

#' 拉取 WB WDI 扩展面板（2000-2023）
#'
#' @param force_online TRUE 时强制重新下载，否则优先缓存
wdi_fetch_extended <- function(start = 2000, end = 2023, force_online = FALSE) {
  if (!force_online) {
    cached <- read_external_cache("wdi")
    if (!is.null(cached)) return(cached)
  }
  inds <- .wdi_indicators()
  panel <- tryCatch({
    ensure_pkgs("wbstats")
    raw <- wbstats::wb_data(inds, start_date = start, end_date = end,
                            country = "countries_only")
    raw |>
      dplyr::transmute(
        iso3_code = .data$iso3c,
        country   = .data$country,
        year      = as.integer(.data$date),
        dplyr::across(dplyr::any_of(names(inds)), as.numeric)
      )
  }, error = function(e) {
    logw("wbstats extended fetch failed: ", conditionMessage(e))
    NULL
  })
  if (!is.null(panel)) write_external_cache(panel, "wdi")
  panel
}

# ---- 2. D3 · WHO GHO ------------------------------------------------------

#' WHO GHO 关键指标（通过 OData API 或 cached snapshot）
.gho_indicators <- function() {
  list(
    HALE      = list(code = "WHOSIS_000002", desc = "Healthy life expectancy at birth"),
    UHC_SCI   = list(code = "UHC_INDEX_REPORTED", desc = "UHC service coverage index"),
    UHC_FIN   = list(code = "FINPROTECTION_CATA_TOT_10_POP", desc = "Population with catastrophic OOP > 10%"),
    UHC_FIN25 = list(code = "FINPROTECTION_CATA_TOT_25_POP", desc = "Population with catastrophic OOP > 25%"),
    DTP3      = list(code = "WHS4_100", desc = "DTP3 immunization coverage"),
    MCV1      = list(code = "WHS4_543", desc = "Measles 1st dose coverage"),
    HIB       = list(code = "WHS4_544", desc = "Hib3 coverage"),
    PCV3      = list(code = "WHS8_110", desc = "PCV3 coverage"),
    SMOKING   = list(code = "M_Est_smk_curr_std", desc = "Adult smoking prevalence (std)"),
    NCD_MORT  = list(code = "NCDMORT3070", desc = "NCD mortality 30-70 (probability)"),
    AIR_PM25  = list(code = "SDGPM25", desc = "Annual mean PM2.5 concentration"),
    HW_DENS   = list(code = "HWF_0001", desc = "Medical doctors per 10,000")
  )
}

#' 拉取 WHO GHO（多指标合并）
gho_fetch <- function(force_online = FALSE) {
  if (!force_online) {
    cached <- read_external_cache("gho")
    if (!is.null(cached)) return(cached)
  }
  inds <- .gho_indicators()
  out <- tryCatch({
    if (!requireNamespace("httr", quietly = TRUE) ||
        !requireNamespace("jsonlite", quietly = TRUE)) {
      stop("httr / jsonlite not available")
    }
    fetch_one <- function(code, name) {
      url <- sprintf(
        "https://ghoapi.azureedge.net/api/%s",
        utils::URLencode(code)
      )
      r <- tryCatch(httr::GET(url, httr::timeout(15)),
                    error = function(e) NULL)
      if (is.null(r) || httr::status_code(r) != 200) return(NULL)
      js <- httr::content(r, as = "text", encoding = "UTF-8")
      d  <- jsonlite::fromJSON(js, simplifyVector = TRUE)$value
      if (is.null(d) || !nrow(d)) return(NULL)
      tibble::tibble(
        iso3_code  = d$SpatialDim,
        year       = suppressWarnings(as.integer(d$TimeDim)),
        sex        = ifelse("Dim1" %in% names(d), d$Dim1, NA_character_),
        indicator  = name,
        value      = suppressWarnings(as.numeric(d$NumericValue))
      )
    }
    pieces <- purrr::map2(
      vapply(inds, `[[`, character(1), "code"),
      names(inds),
      fetch_one
    )
    res <- dplyr::bind_rows(Filter(Negate(is.null), pieces))
    if (!nrow(res)) return(NULL)
    # 透视为宽
    res |>
      dplyr::filter(is.na(.data$sex) | .data$sex == "BTSX") |>
      dplyr::group_by(.data$iso3_code, .data$year, .data$indicator) |>
      dplyr::summarise(value = mean(.data$value, na.rm = TRUE),
                       .groups = "drop") |>
      tidyr::pivot_wider(names_from = "indicator", values_from = "value")
  }, error = function(e) {
    logw("GHO API fetch failed: ", conditionMessage(e))
    NULL
  })
  if (!is.null(out)) write_external_cache(out, "gho")
  out
}

# ---- 3. D4 · IMF GFS / WEO ------------------------------------------------

#' IMF（财政指标） — 用 IMF Data Mapper API
imf_fetch <- function(force_online = FALSE) {
  if (!force_online) {
    cached <- read_external_cache("imf")
    if (!is.null(cached)) return(cached)
  }
  inds <- list(
    gov_balance_gdp = "GGXCNL_NGDP",   # General gov net lending/borrowing % GDP
    gov_debt_gdp    = "GGXWDG_NGDP",   # Gross gov debt % GDP
    gov_exp_gdp     = "GGX_NGDP",       # Gen gov total expenditure % GDP
    gov_revenue_gdp = "GGR_NGDP",       # Gen gov revenue % GDP
    gdp_ppp_pc      = "PPPPC",          # GDP per capita PPP
    gdp_growth      = "NGDP_RPCH",      # Real GDP growth
    inflation_avg   = "PCPIPCH",
    cur_acct_gdp    = "BCA_NGDPD"
  )
  out <- tryCatch({
    if (!requireNamespace("httr", quietly = TRUE) ||
        !requireNamespace("jsonlite", quietly = TRUE)) {
      stop("httr / jsonlite not available")
    }
    pieces <- lapply(seq_along(inds), function(i) {
      code <- inds[[i]]; nm <- names(inds)[i]
      url <- sprintf("https://www.imf.org/external/datamapper/api/v1/%s", code)
      r <- tryCatch(httr::GET(url, httr::timeout(15)),
                    error = function(e) NULL)
      if (is.null(r) || httr::status_code(r) != 200) return(NULL)
      js <- jsonlite::fromJSON(httr::content(r, as = "text", encoding = "UTF-8"))
      block <- js$values[[code]]
      if (is.null(block)) return(NULL)
      rows <- lapply(names(block), function(iso) {
        years <- block[[iso]]
        if (is.null(years) || !length(years)) return(NULL)
        tibble::tibble(
          iso3_code = iso,
          year      = suppressWarnings(as.integer(names(years))),
          indicator = nm,
          value     = suppressWarnings(as.numeric(unlist(years)))
        )
      })
      dplyr::bind_rows(Filter(Negate(is.null), rows))
    })
    dplyr::bind_rows(Filter(Negate(is.null), pieces)) |>
      tidyr::pivot_wider(names_from = "indicator", values_from = "value")
  }, error = function(e) {
    logw("IMF fetch failed: ", conditionMessage(e))
    NULL
  })
  if (!is.null(out)) write_external_cache(out, "imf")
  out
}

# ---- 4. D5 · OECD Health Statistics ---------------------------------------

#' OECD 子集，使用 OECD.Stat REST（部分 OECD 国家会有数据）
oecd_fetch <- function(force_online = FALSE) {
  if (!force_online) {
    cached <- read_external_cache("oecd")
    if (!is.null(cached)) return(cached)
  }
  out <- tryCatch({
    ensure_pkgs("OECD")
    if (!requireNamespace("OECD", quietly = TRUE)) stop("OECD pkg unavailable")
    df <- OECD::get_dataset(
      dataset = "HEALTH_STAT",
      filter = list(NULL, NULL, NULL),
      start_time = 2000, end_time = 2023,
      pre_formatted = TRUE
    )
    df
  }, error = function(e) {
    logw("OECD fetch failed: ", conditionMessage(e))
    NULL
  })
  if (!is.null(out)) write_external_cache(out, "oecd")
  out
}

# ---- 5. D6 · IHME GBD lite ------------------------------------------------

#' IHME GBD（无公开 API；用项目快照或留 NULL）
gbd_fetch <- function() {
  read_external_cache("gbd")  # 无 fallback：未提交快照则 NULL
}

# ---- 6. 一次性加载所有外部数据 -------------------------------------------

#' 一次拉齐所有外部源（带容错）
#'
#' @return list(wdi, gho, imf, oecd, gbd) — 每项可能为 NULL
load_external_all <- function(force_online = FALSE) {
  list(
    wdi  = tryCatch(wdi_fetch_extended(force_online = force_online),
                    error = function(e) NULL),
    gho  = tryCatch(gho_fetch(force_online = force_online),
                    error = function(e) NULL),
    imf  = tryCatch(imf_fetch(force_online = force_online),
                    error = function(e) NULL),
    oecd = tryCatch(oecd_fetch(force_online = force_online),
                    error = function(e) NULL),
    gbd  = tryCatch(gbd_fetch(),
                    error = function(e) NULL)
  )
}

# ---- 7. master_panel: 合并 GHED + 外部 ------------------------------------

#' 把 master_enriched 与外部数据合并为 master_panel（主面板）
#'
#' @param master_enriched build 自 v1 的 enrich_master() 结果
#' @param external load_external_all() 结果
build_master_panel <- function(master_enriched, external = NULL) {
  ensure_pkgs("dplyr")
  if (is.null(external)) external <- load_external_all()
  out <- master_enriched

  # 合并 WDI 扩展（优先于 v1 简版）
  if (!is.null(external$wdi)) {
    out <- out |>
      dplyr::left_join(
        external$wdi |> dplyr::select(-dplyr::any_of("country")),
        by = c("iso3_code", "year"),
        suffix = c("", "_wdi")
      )
  }

  # 合并 WHO GHO（HALE / UHC / 疫苗 / NCD）
  if (!is.null(external$gho)) {
    gho_wide <- external$gho
    out <- out |>
      dplyr::left_join(gho_wide, by = c("iso3_code", "year"))
  }

  # 合并 IMF（财政）
  if (!is.null(external$imf)) {
    out <- out |>
      dplyr::left_join(external$imf, by = c("iso3_code", "year"))
  }

  out
}
