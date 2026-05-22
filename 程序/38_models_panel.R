
if (!exists("ensure_pkgs", mode = "function")) {
  source(file.path("\u7a0b\u5e8f", "00_utils.R"))
}

.mp_has <- function(pkg) requireNamespace(pkg, quietly = TRUE)

.mp_skip <- function(reason) list(status = "skipped", reason = reason)

mp_panel_twoway_fe <- function(master) {
  if (!.mp_has("fixest")) return(.mp_skip("fixest"))
  d <- master[is.finite(master$che_pc_usd2023) &
              is.finite(master$gdp_pc_usd) &
              master$che_pc_usd2023 > 0 &
              master$gdp_pc_usd > 0, ]
  if (nrow(d) < 100) return(NULL)
  d$lche <- log(d$che_pc_usd2023)
  d$lgdp <- log(d$gdp_pc_usd)
  fixest::feols(lche ~ lgdp | iso3_code + year, data = d,
                 cluster = ~iso3_code)
}

mp_panel_oop_fe <- function(master) {
  if (!.mp_has("fixest")) return(.mp_skip("fixest"))
  d <- master[is.finite(master$hf3_che) &
              is.finite(master$gdp_pc_usd) &
              master$gdp_pc_usd > 0, ]
  if (nrow(d) < 100) return(NULL)
  d$lgdp <- log(d$gdp_pc_usd)
  fixest::feols(hf3_che ~ lgdp | iso3_code + year, data = d,
                 cluster = ~iso3_code)
}

mp_panel_dynamic_ar1 <- function(master) {
  if (!.mp_has("fixest") || !.mp_has("dplyr")) return(.mp_skip("fixest/dplyr"))
  d <- master[is.finite(master$che_pc_usd2023) &
              master$che_pc_usd2023 > 0, ]
  d <- d[order(d$iso3_code, d$year), ]
  d$lche <- log(d$che_pc_usd2023)
  d$lche_lag <- stats::ave(d$lche, d$iso3_code,
                            FUN = function(x) c(NA, utils::head(x, -1)))
  fixest::feols(lche ~ lche_lag | iso3_code + year, data = d,
                 cluster = ~iso3_code)
}

mp_panel_inc_interact <- function(master) {
  if (!.mp_has("fixest")) return(.mp_skip("fixest"))
  d <- master[is.finite(master$che_pc_usd2023) &
              is.finite(master$gdp_pc_usd) &
              master$gdp_pc_usd > 0 &
              master$che_pc_usd2023 > 0 &
              !is.na(master$income_group), ]
  if (nrow(d) < 100) return(NULL)
  d$lche <- log(d$che_pc_usd2023)
  d$lgdp <- log(d$gdp_pc_usd)
  fixest::feols(lche ~ lgdp:income_group | iso3_code + year,
                 data = d, cluster = ~iso3_code)
}

mp_panel_continent_split <- function(master) {
  if (!.mp_has("fixest") || !.mp_has("dplyr")) return(.mp_skip("fixest/dplyr"))
  d <- master[is.finite(master$che_pc_usd2023) &
              is.finite(master$gdp_pc_usd) &
              master$che_pc_usd2023 > 0 & master$gdp_pc_usd > 0 &
              !is.na(master$continent), ]
  d$lche <- log(d$che_pc_usd2023); d$lgdp <- log(d$gdp_pc_usd)
  conts <- unique(d$continent)
  res <- lapply(conts, function(c) {
    sub <- d[d$continent == c, ]
    if (nrow(sub) < 50) return(NULL)
    list(continent = c,
         fit = fixest::feols(lche ~ lgdp | iso3_code + year,
                              data = sub, cluster = ~iso3_code))
  })
  names(res) <- conts
  res
}

mp_panel_lifeexp <- function(master) {
  if (!.mp_has("fixest")) return(.mp_skip("fixest"))
  d <- master[is.finite(master$life_exp) &
              is.finite(master$che_pc_usd2023) &
              master$che_pc_usd2023 > 0 &
              is.finite(master$gdp_pc_usd) &
              master$gdp_pc_usd > 0, ]
  if (nrow(d) < 100) return(NULL)
  d$lche <- log(d$che_pc_usd2023); d$lgdp <- log(d$gdp_pc_usd)
  fixest::feols(life_exp ~ lche + lgdp | iso3_code + year,
                 data = d, cluster = ~iso3_code)
}

mp_panel_u5mr <- function(master) {
  if (!.mp_has("fixest")) return(.mp_skip("fixest"))
  d <- master[is.finite(master$u5mr) &
              is.finite(master$che_pc_usd2023) &
              master$che_pc_usd2023 > 0, ]
  if (nrow(d) < 100) return(NULL)
  d$lche <- log(d$che_pc_usd2023)
  fixest::feols(log(u5mr) ~ lche | iso3_code + year,
                 data = d, cluster = ~iso3_code)
}

mp_panel_growth_init <- function(master) {
  if (!.mp_has("dplyr")) return(.mp_skip("dplyr"))
  yrs <- range(master$year, na.rm = TRUE)
  d1 <- master[master$year == yrs[1] &
              is.finite(master$che_pc_usd2023), ]
  d2 <- master[master$year == yrs[2] &
              is.finite(master$che_pc_usd2023), ]
  m <- merge(d1[, c("iso3_code", "country_name", "continent",
                       "che_pc_usd2023")],
              d2[, c("iso3_code", "che_pc_usd2023")],
              by = "iso3_code", suffixes = c(".y1", ".y2"))
  span <- yrs[2] - yrs[1]
  m$cagr <- (m$che_pc_usd2023.y2 / m$che_pc_usd2023.y1) ^
              (1 / span) - 1
  m$linit <- log(pmax(m$che_pc_usd2023.y1, 1))
  m <- m[is.finite(m$cagr) & is.finite(m$linit), ]
  if (!nrow(m)) return(NULL)
  stats::lm(cagr ~ linit + continent, data = m)
}

mp_panel_high_income <- function(master) {
  if (!.mp_has("fixest")) return(.mp_skip("fixest"))
  d <- master[master$income_group == "High income" &
              is.finite(master$che_pc_usd2023) &
              is.finite(master$gdp_pc_usd) &
              master$che_pc_usd2023 > 0 & master$gdp_pc_usd > 0, ]
  if (nrow(d) < 50) return(NULL)
  d$lche <- log(d$che_pc_usd2023); d$lgdp <- log(d$gdp_pc_usd)
  fixest::feols(lche ~ lgdp | iso3_code + year, data = d,
                 cluster = ~iso3_code)
}

mp_panel_lmic <- function(master) {
  if (!.mp_has("fixest")) return(.mp_skip("fixest"))
  inc_levels <- c("Low income", "Lower middle income",
                   "Upper middle income")
  d <- master[master$income_group %in% inc_levels &
              is.finite(master$che_pc_usd2023) &
              is.finite(master$gdp_pc_usd) &
              master$che_pc_usd2023 > 0 & master$gdp_pc_usd > 0, ]
  if (nrow(d) < 50) return(NULL)
  d$lche <- log(d$che_pc_usd2023); d$lgdp <- log(d$gdp_pc_usd)
  fixest::feols(lche ~ lgdp | iso3_code + year, data = d,
                 cluster = ~iso3_code)
}

mp_panel_iv_lag <- function(master) {
  if (!.mp_has("fixest") || !.mp_has("dplyr")) return(.mp_skip("fixest/dplyr"))
  d <- master[is.finite(master$che_pc_usd2023) &
              is.finite(master$gdp_pc_usd) &
              master$che_pc_usd2023 > 0 & master$gdp_pc_usd > 0, ]
  d <- d[order(d$iso3_code, d$year), ]
  d$lgdp <- log(d$gdp_pc_usd)
  d$lche <- log(d$che_pc_usd2023)
  d$lgdp_lag <- stats::ave(d$lgdp, d$iso3_code,
                            FUN = function(x) c(NA, utils::head(x, -1)))
  d <- d[!is.na(d$lgdp_lag), ]
  if (!nrow(d)) return(NULL)
  fixest::feols(lche ~ 1 | iso3_code + year |
                 lgdp ~ lgdp_lag, data = d, cluster = ~iso3_code)
}

mp_panel_inc_year <- function(master) {
  if (!.mp_has("fixest")) return(.mp_skip("fixest"))
  d <- master[is.finite(master$hf3_che) &
              !is.na(master$income_group), ]
  if (nrow(d) < 100) return(NULL)
  d$decade <- factor(floor(d$year / 10) * 10)
  fixest::feols(hf3_che ~ income_group:decade | iso3_code,
                 data = d, cluster = ~iso3_code)
}

mp_panel_re <- function(master) {
  if (!.mp_has("lme4")) return(.mp_skip("lme4"))
  d <- master[is.finite(master$che_pc_usd2023) &
              is.finite(master$gdp_pc_usd) &
              master$che_pc_usd2023 > 0 & master$gdp_pc_usd > 0, ]
  if (nrow(d) < 100) return(NULL)
  d$lche <- log(d$che_pc_usd2023); d$lgdp <- log(d$gdp_pc_usd)
  lme4::lmer(lche ~ lgdp + (1 | iso3_code), data = d)
}

mp_panel_random_slope <- function(master) {
  if (!.mp_has("lme4")) return(.mp_skip("lme4"))
  d <- master[is.finite(master$che_pc_usd2023) &
              is.finite(master$gdp_pc_usd) &
              master$che_pc_usd2023 > 0 & master$gdp_pc_usd > 0, ]
  if (nrow(d) < 100) return(NULL)
  d$lche <- log(d$che_pc_usd2023); d$lgdp <- log(d$gdp_pc_usd)
  lme4::lmer(lche ~ lgdp + (lgdp | iso3_code), data = d,
              control = lme4::lmerControl(
                check.conv.singular = "ignore"))
}

mp_panel_first_diff <- function(master) {
  if (!.mp_has("dplyr")) return(.mp_skip("dplyr"))
  d <- master[is.finite(master$che_pc_usd2023) &
              is.finite(master$gdp_pc_usd) &
              master$che_pc_usd2023 > 0 & master$gdp_pc_usd > 0, ]
  d <- d[order(d$iso3_code, d$year), ]
  d$lche <- log(d$che_pc_usd2023); d$lgdp <- log(d$gdp_pc_usd)
  d$dche <- stats::ave(d$lche, d$iso3_code,
                        FUN = function(x) c(NA, diff(x)))
  d$dgdp <- stats::ave(d$lgdp, d$iso3_code,
                        FUN = function(x) c(NA, diff(x)))
  fit <- stats::lm(dche ~ dgdp, data = d[!is.na(d$dche), ])
  fit
}

mp_panel_pop_weighted <- function(master) {
  d <- master[is.finite(master$che_pc_usd2023) &
              is.finite(master$gdp_pc_usd) &
              is.finite(master$pop) &
              master$che_pc_usd2023 > 0 & master$gdp_pc_usd > 0, ]
  if (nrow(d) < 100) return(NULL)
  d$lche <- log(d$che_pc_usd2023); d$lgdp <- log(d$gdp_pc_usd)
  stats::lm(lche ~ lgdp + factor(year), data = d, weights = d$pop)
}

mp_panel_aging <- function(master) {
  if (!.mp_has("fixest")) return(.mp_skip("fixest"))
  if (!"pop_65_share" %in% names(master)) return(NULL)
  d <- master[is.finite(master$life_exp) &
              is.finite(master$pop_65_share) &
              is.finite(master$che_pc_usd2023) &
              master$che_pc_usd2023 > 0, ]
  if (nrow(d) < 100) return(NULL)
  d$lche <- log(d$che_pc_usd2023)
  fixest::feols(life_exp ~ lche * pop_65_share | iso3_code + year,
                 data = d, cluster = ~iso3_code)
}

mp_panel_ssa <- function(master) {
  if (!.mp_has("fixest")) return(.mp_skip("fixest"))
  if (!"region" %in% names(master)) {
    d <- master[master$continent == "Africa", ]
  } else {
    d <- master[grepl("Sub-Saharan", master$region), ]
  }
  d <- d[is.finite(d$che_pc_usd2023) &
          is.finite(d$gdp_pc_usd) &
          d$che_pc_usd2023 > 0 & d$gdp_pc_usd > 0, ]
  if (nrow(d) < 30) return(NULL)
  d$lche <- log(d$che_pc_usd2023); d$lgdp <- log(d$gdp_pc_usd)
  fixest::feols(lche ~ lgdp | iso3_code + year, data = d,
                 cluster = ~iso3_code)
}

mp_panel_pc1_trend <- function(master) {
  if (!.mp_has("fixest")) return(.mp_skip("fixest"))
  cols <- intersect(c("hf1_che", "hf2_che", "hf3_che",
                       "gghed_che", "ext_che"), names(master))
  if (length(cols) < 3) return(NULL)
  d <- master[stats::complete.cases(master[, cols]), ]
  if (nrow(d) < 100) return(NULL)
  pc <- stats::prcomp(d[, cols], scale. = TRUE)
  d$pc1 <- pc$x[, 1]
  fixest::feols(pc1 ~ year | iso3_code, data = d,
                 cluster = ~iso3_code)
}

mp_panel_mundlak <- function(master) {
  if (!.mp_has("fixest") || !.mp_has("dplyr")) return(.mp_skip("fixest/dplyr"))
  d <- master[is.finite(master$che_pc_usd2023) &
              is.finite(master$gdp_pc_usd) &
              master$che_pc_usd2023 > 0 & master$gdp_pc_usd > 0, ]
  d$lche <- log(d$che_pc_usd2023); d$lgdp <- log(d$gdp_pc_usd)
  d$mean_lgdp <- stats::ave(d$lgdp, d$iso3_code, FUN = mean)
  if (!nrow(d)) return(NULL)
  fixest::feols(lche ~ lgdp + mean_lgdp | year,
                 data = d, cluster = ~iso3_code)
}


ghs_validate_models_panel <- function(master = NULL) {
  if (is.null(master)) {
    cache <- file.path(proj_root(), "\u6d3e\u751f\u6570\u636e",
                        "\u5904\u7406\u7ed3\u679c",
                        "master_enriched.rds")
    if (!file.exists(cache)) stop("Missing master_enriched.rds")
    master <- readRDS(cache)
  }
  fn_names <- ls(envir = .GlobalEnv, pattern = "^mp_[a-z]")
  results <- list()
  for (nm in fn_names) {
    f <- get(nm, envir = .GlobalEnv)
    res <- tryCatch({
      obj <- f(master)
      list(ok = !is.null(obj), class = class(obj)[1],
           is_skip = is.list(obj) && identical(obj$status, "skipped"))
    }, error = function(e) list(ok = FALSE,
                                  class = conditionMessage(e),
                                  is_skip = FALSE))
    results[[nm]] <- res
  }
  results
}
