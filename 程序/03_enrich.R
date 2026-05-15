# =============================================================================
# 程序/03_enrich.R
# -----------------------------------------------------------------------------
# 外部数据增强：
#   - 大洲 / 子区域 / 收入组（countrycode）
#   - World Bank WDI：人口、GDP/cap、预期寿命、U5MR（可选）
#   - 地图几何（Natural Earth，按需加载）
# 所有网络调用带本地缓存；离线也能跑通。
# =============================================================================

if (!exists("proj_root", mode = "function")) {
  source(file.path("程序", "00_utils.R"))
}

# ---- 1. 国家元数据 ----------------------------------------------------------
#' 用 countrycode 给一组 iso3_code 附加 continent / region / income_group
enrich_country_meta <- function(iso_vec) {
  ensure_pkgs(c("countrycode", "dplyr", "tibble"))
  iso_vec <- unique(iso_vec)
  tibble::tibble(
    iso3_code = iso_vec,
    country_name_iso = suppressWarnings(
      countrycode::countrycode(iso_vec, "iso3c", "country.name")
    ),
    continent = suppressWarnings(
      countrycode::countrycode(iso_vec, "iso3c", "continent")
    ),
    region23 = suppressWarnings(
      countrycode::countrycode(iso_vec, "iso3c", "region23")
    ),
    un_region = suppressWarnings(
      countrycode::countrycode(iso_vec, "iso3c", "un.region.name")
    ),
    un_subregion = suppressWarnings(
      countrycode::countrycode(iso_vec, "iso3c", "un.regionsub.name")
    )
  )
}

# ---- 2. 收入组（简易内置，避免 WDI 失败） -----------------------------------
#' 内置的 World Bank 4 类收入组（2022 分类快照），作为 fallback
#' 完整列表较长，这里提供常见国家 + 回退默认 NA
income_group_fallback <- function() {
  tibble::tribble(
    ~iso3_code, ~income_group,
    "USA", "High income",       "CAN", "High income",       "JPN", "High income",
    "DEU", "High income",       "FRA", "High income",       "GBR", "High income",
    "ITA", "High income",       "ESP", "High income",       "KOR", "High income",
    "AUS", "High income",       "NZL", "High income",       "SGP", "High income",
    "SWE", "High income",       "NOR", "High income",       "DNK", "High income",
    "FIN", "High income",       "NLD", "High income",       "BEL", "High income",
    "AUT", "High income",       "CHE", "High income",       "ISR", "High income",
    "ARE", "High income",       "SAU", "High income",       "QAT", "High income",
    "CHN", "Upper middle income", "BRA", "Upper middle income", "MEX", "Upper middle income",
    "RUS", "Upper middle income", "ARG", "Upper middle income", "TUR", "Upper middle income",
    "ZAF", "Upper middle income", "MYS", "Upper middle income", "THA", "Upper middle income",
    "COL", "Upper middle income", "PER", "Upper middle income", "ROU", "Upper middle income",
    "BGR", "Upper middle income", "KAZ", "Upper middle income",
    "IND", "Lower middle income", "IDN", "Lower middle income", "PHL", "Lower middle income",
    "VNM", "Lower middle income", "EGY", "Lower middle income", "NGA", "Lower middle income",
    "KEN", "Lower middle income", "UKR", "Lower middle income", "BGD", "Lower middle income",
    "PAK", "Lower middle income", "MAR", "Lower middle income", "TUN", "Lower middle income",
    "GHA", "Lower middle income", "CIV", "Lower middle income", "UZB", "Lower middle income",
    "ETH", "Low income",         "AFG", "Low income",         "YEM", "Low income",
    "SYR", "Low income",         "MDG", "Low income",         "RWA", "Low income",
    "UGA", "Low income",         "BFA", "Low income",         "NER", "Low income",
    "MLI", "Low income",         "TCD", "Low income",         "SOM", "Low income",
    "COD", "Low income",         "MOZ", "Low income",         "SDN", "Low income",
    "SSD", "Low income",         "ERI", "Low income",         "CAF", "Low income",
    "TGO", "Low income",         "LBR", "Low income",         "GNB", "Low income",
    "SLE", "Low income",         "GMB", "Low income",         "MWI", "Low income"
  )
}

#' 获取收入组映射：优先 WDI，失败则用 fallback
fetch_income_group <- function(iso_vec = NULL, use_cache = TRUE) {
  ensure_pkgs(c("dplyr"))
  cache_path <- file.path(proj_root(), "派生数据", "原始缓存", "wb_income_group.rds")
  if (isTRUE(use_cache) && file.exists(cache_path)) {
    logi("income group cache hit")
    ig <- readRDS(cache_path)
  } else {
    ig <- tryCatch({
      ensure_pkgs("wbstats")
      df <- wbstats::wb_countries()
      df |>
        dplyr::transmute(
          iso3_code    = .data$iso3c,
          income_group = .data$income_level
        )
    }, error = function(e) {
      logw("wbstats failed, using fallback income_group: ", conditionMessage(e))
      income_group_fallback()
    })
    dir.create(dirname(cache_path), showWarnings = FALSE, recursive = TRUE)
    try(saveRDS(ig, cache_path), silent = TRUE)
  }
  if (is.null(iso_vec)) return(ig)
  dplyr::filter(ig, .data$iso3_code %in% iso_vec)
}

# ---- 3. WDI 宏观变量（可选） -------------------------------------------------
#' 拉取 World Bank WDI 指标：人口、人均 GDP、预期寿命、U5MR
fetch_wdi_panel <- function(start = 2000, end = 2023, use_cache = TRUE) {
  ensure_pkgs(c("dplyr"))
  cache_path <- file.path(proj_root(), "派生数据", "原始缓存", "wdi_panel.rds")
  if (isTRUE(use_cache) && file.exists(cache_path)) {
    logi("wdi cache hit")
    return(readRDS(cache_path))
  }
  panel <- tryCatch({
    ensure_pkgs("wbstats")
    inds <- c(
      pop        = "SP.POP.TOTL",
      gdp_pc_usd = "NY.GDP.PCAP.KD",
      life_exp   = "SP.DYN.LE00.IN",
      u5mr       = "SH.DYN.MORT"
    )
    raw <- wbstats::wb_data(inds, start_date = start, end_date = end)
    raw |>
      dplyr::transmute(
        iso3_code  = .data$iso3c,
        year       = as.integer(.data$date),
        pop        = .data$pop,
        gdp_pc_usd = .data$gdp_pc_usd,
        life_exp   = .data$life_exp,
        u5mr       = .data$u5mr
      )
  }, error = function(e) {
    logw("wbstats failed, returning NULL WDI panel: ", conditionMessage(e))
    NULL
  })
  if (!is.null(panel)) {
    dir.create(dirname(cache_path), showWarnings = FALSE, recursive = TRUE)
    try(saveRDS(panel, cache_path), silent = TRUE)
  }
  panel
}

# ---- 4. 合并入口 ------------------------------------------------------------
#' 将宽表加上大洲/区域/收入组/WDI
enrich_master <- function(master_wide, with_wdi = TRUE) {
  ensure_pkgs(c("dplyr"))
  meta <- enrich_country_meta(master_wide$iso3_code)
  inc  <- fetch_income_group(master_wide$iso3_code)
  out  <- master_wide |>
    dplyr::left_join(meta, by = "iso3_code") |>
    dplyr::left_join(inc,  by = "iso3_code")
  if (isTRUE(with_wdi)) {
    wdi <- fetch_wdi_panel()
    if (!is.null(wdi)) {
      out <- out |>
        dplyr::left_join(wdi, by = c("iso3_code", "year")) |>
        dplyr::mutate(
          che_pc_usd2023 = dplyr::if_else(
            is.finite(.data$pop) & .data$pop > 0 & !is.na(.data$che_usd2023),
            .data$che_usd2023 / .data$pop,
            NA_real_
          )
        )
    }
  }
  out
}

# ---- 5. 地图几何 ------------------------------------------------------------
#' 读取 Natural Earth 世界多边形（50m 或 110m）
load_world_sf <- function(scale = c("medium", "small", "large"),
                          simplify_keep = 0.08) {
  scale <- match.arg(scale)
  ensure_pkgs(c("sf", "rnaturalearth", "rnaturalearthdata", "dplyr"))
  cache_path <- file.path(proj_root(), "派生数据", "原始缓存",
                          paste0("world_sf_", scale, ".rds"))
  if (file.exists(cache_path)) return(readRDS(cache_path))
  world <- rnaturalearth::ne_countries(scale = scale, returnclass = "sf")
  world <- dplyr::transmute(
    world,
    iso3_code = .data$iso_a3,
    country_name_sf = .data$name,
    continent_sf    = .data$continent,
    region_sf       = .data$region_un,
    geometry        = .data$geometry
  )
  if (requireNamespace("rmapshaper", quietly = TRUE) && !is.null(simplify_keep)) {
    world <- rmapshaper::ms_simplify(world, keep = simplify_keep, keep_shapes = TRUE)
  }
  dir.create(dirname(cache_path), showWarnings = FALSE, recursive = TRUE)
  try(saveRDS(world, cache_path), silent = TRUE)
  world
}
