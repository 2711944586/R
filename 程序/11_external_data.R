
if (!exists("proj_root", mode = "function")) {
  source(file.path("程序", "00_utils.R"))
}


.external_dir <- function() {
  d <- file.path(proj_root(), "派生数据", "外部数据")
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

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


imf_fetch <- function(force_online = FALSE) {
  if (!force_online) {
    cached <- read_external_cache("imf")
    if (!is.null(cached)) return(cached)
  }
  inds <- list(
    gov_balance_gdp = "GGXCNL_NGDP",
    gov_debt_gdp    = "GGXWDG_NGDP",
    gov_exp_gdp     = "GGX_NGDP",
    gov_revenue_gdp = "GGR_NGDP",
    gdp_ppp_pc      = "PPPPC",
    gdp_growth      = "NGDP_RPCH",
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


gbd_fetch <- function() {
  read_external_cache("gbd")
}


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


build_master_panel <- function(master_enriched, external = NULL) {
  ensure_pkgs("dplyr")
  if (is.null(external)) external <- load_external_all()
  out <- master_enriched

  if (!is.null(external$wdi)) {
    out <- out |>
      dplyr::left_join(
        external$wdi |> dplyr::select(-dplyr::any_of("country")),
        by = c("iso3_code", "year"),
        suffix = c("", "_wdi")
      )
  }

  if (!is.null(external$gho)) {
    gho_wide <- external$gho
    out <- out |>
      dplyr::left_join(gho_wide, by = c("iso3_code", "year"))
  }

  if (!is.null(external$imf)) {
    out <- out |>
      dplyr::left_join(external$imf, by = c("iso3_code", "year"))
  }

  out
}
