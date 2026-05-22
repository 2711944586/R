
if (!exists("ensure_pkgs", mode = "function")) {
  source(file.path("\u7a0b\u5e8f", "00_utils.R"))
}

.mr_has  <- function(pkg) requireNamespace(pkg, quietly = TRUE)
.mr_skip <- function(reason) list(status = "skipped", reason = reason)

mr_quantile_reg <- function(master) {
  if (!.mr_has("quantreg")) return(.mr_skip("quantreg"))
  d <- master[is.finite(master$che_pc_usd2023) &
              is.finite(master$gdp_pc_usd) &
              master$che_pc_usd2023 > 0 & master$gdp_pc_usd > 0, ]
  if (nrow(d) < 100) return(NULL)
  d$lche <- log(d$che_pc_usd2023); d$lgdp <- log(d$gdp_pc_usd)
  quantreg::rq(lche ~ lgdp, tau = c(0.1, 0.5, 0.9), data = d)
}

mr_rlm <- function(master) {
  if (!.mr_has("MASS")) return(.mr_skip("MASS"))
  d <- master[is.finite(master$che_pc_usd2023) &
              is.finite(master$gdp_pc_usd) &
              master$che_pc_usd2023 > 0 & master$gdp_pc_usd > 0, ]
  if (nrow(d) < 100) return(NULL)
  d$lche <- log(d$che_pc_usd2023); d$lgdp <- log(d$gdp_pc_usd)
  MASS::rlm(lche ~ lgdp, data = d)
}

mr_theil_sen <- function(master) {
  if (!.mr_has("mblm")) return(.mr_skip("mblm"))
  d <- master[is.finite(master$che_pc_usd2023) &
              is.finite(master$gdp_pc_usd) &
              master$che_pc_usd2023 > 0 & master$gdp_pc_usd > 0, ]
  if (nrow(d) < 50) return(NULL)
  d$lche <- log(d$che_pc_usd2023); d$lgdp <- log(d$gdp_pc_usd)
  d <- utils::head(d, 1500)
  mblm::mblm(lche ~ lgdp, dataframe = d, repeated = FALSE)
}

mr_gam_lifeexp <- function(master) {
  if (!.mr_has("mgcv")) return(.mr_skip("mgcv"))
  d <- master[is.finite(master$life_exp) &
              is.finite(master$che_pc_usd2023) &
              master$che_pc_usd2023 > 0, ]
  if (nrow(d) < 100) return(NULL)
  d$lche <- log(d$che_pc_usd2023)
  mgcv::gam(life_exp ~ s(lche, k = 5) + s(year, k = 5),
             data = d, method = "REML")
}

mr_gam_oop <- function(master) {
  if (!.mr_has("mgcv")) return(.mr_skip("mgcv"))
  d <- master[is.finite(master$hf3_che) &
              is.finite(master$gdp_pc_usd) &
              master$gdp_pc_usd > 0, ]
  if (nrow(d) < 100) return(NULL)
  d$lgdp <- log(d$gdp_pc_usd)
  mgcv::gam(hf3_che ~ s(lgdp, k = 5), data = d, method = "REML")
}

mr_poly_lifeexp <- function(master) {
  d <- master[is.finite(master$life_exp) &
              is.finite(master$che_pc_usd2023) &
              master$che_pc_usd2023 > 0, ]
  if (nrow(d) < 100) return(NULL)
  d$lche <- log(d$che_pc_usd2023)
  stats::lm(life_exp ~ stats::poly(lche, 3), data = d)
}

mr_bspline_che <- function(master) {
  if (!.mr_has("splines")) return(.mr_skip("splines"))
  d <- master[is.finite(master$che_pc_usd2023) &
              is.finite(master$year), ]
  if (nrow(d) < 50) return(NULL)
  d$lche <- log(pmax(d$che_pc_usd2023, 1))
  stats::lm(lche ~ splines::bs(year, df = 5), data = d)
}

mr_boot_elasticity <- function(master, B = 200) {
  d <- master[is.finite(master$che_pc_usd2023) &
              is.finite(master$gdp_pc_usd) &
              master$che_pc_usd2023 > 0 & master$gdp_pc_usd > 0, ]
  if (nrow(d) < 100) return(NULL)
  d$lche <- log(d$che_pc_usd2023); d$lgdp <- log(d$gdp_pc_usd)
  set.seed(1)
  betas <- replicate(B, {
    idx <- sample.int(nrow(d), replace = TRUE)
    stats::coef(stats::lm(lche ~ lgdp, data = d[idx, ]))["lgdp"]
  })
  list(beta = mean(betas, na.rm = TRUE),
       se   = stats::sd(betas, na.rm = TRUE),
       ci   = stats::quantile(betas, c(0.025, 0.975), na.rm = TRUE),
       B    = B)
}

mr_jackknife <- function(master) {
  d <- master[is.finite(master$che_pc_usd2023) &
              is.finite(master$gdp_pc_usd) &
              master$che_pc_usd2023 > 0 & master$gdp_pc_usd > 0, ]
  if (nrow(d) < 50) return(NULL)
  d$lche <- log(d$che_pc_usd2023); d$lgdp <- log(d$gdp_pc_usd)
  isos <- unique(d$iso3_code)
  betas <- vapply(isos, function(iso) {
    sub <- d[d$iso3_code != iso, ]
    unname(stats::coef(stats::lm(lche ~ lgdp, data = sub))["lgdp"])
  }, numeric(1))
  list(mean = mean(betas, na.rm = TRUE),
       sd   = stats::sd(betas, na.rm = TRUE),
       range = range(betas, na.rm = TRUE),
       n = length(betas))
}

mr_permutation_continent <- function(master, year = NULL, var = "hf3_che",
                                       B = 500) {
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year &
              is.finite(master[[var]]) &
              !is.na(master$continent), ]
  if (nrow(d) < 30) return(NULL)
  obs_f <- summary(stats::aov(d[[var]] ~ d$continent))[[1]][1, "F value"]
  set.seed(1)
  perm_f <- replicate(B, {
    summary(stats::aov(d[[var]] ~ sample(d$continent)))[[1]][1, "F value"]
  })
  list(obs_F = obs_f, p_perm = mean(perm_f >= obs_f, na.rm = TRUE),
       B = B)
}

mr_did_covid <- function(master) {
  if (!.mr_has("fixest")) return(.mr_skip("fixest"))
  d <- master[is.finite(master$che_pc_usd2023) &
              master$year %in% 2015:2022, ]
  if (nrow(d) < 50) return(NULL)
  d$lche <- log(pmax(d$che_pc_usd2023, 1))
  d$post <- as.integer(d$year >= 2020)
  d$treated <- as.integer(d$continent %in% c("Europe", "Americas"))
  d$did <- d$post * d$treated
  fixest::feols(lche ~ did | iso3_code + year, data = d,
                 cluster = ~iso3_code)
}

mr_segmented_che <- function(master) {
  if (!.mr_has("segmented")) return(.mr_skip("segmented"))
  d <- master[is.finite(master$che_pc_usd2023) &
              master$iso3_code == "USA", ]
  d <- d[order(d$year), ]
  if (nrow(d) < 10) return(NULL)
  d$lche <- log(d$che_pc_usd2023)
  base <- stats::lm(lche ~ year, data = d)
  segmented::segmented(base, seg.Z = ~year, npsi = 1)
}

mr_tukey_oop <- function(master, year = NULL) {
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year &
              is.finite(master$hf3_che) &
              !is.na(master$income_group), ]
  if (nrow(d) < 50) return(NULL)
  fit <- stats::aov(hf3_che ~ income_group, data = d)
  stats::TukeyHSD(fit)
}

mr_robust_se <- function(master) {
  if (!.mr_has("sandwich") || !.mr_has("lmtest")) {
    return(.mr_skip("sandwich/lmtest"))
  }
  d <- master[is.finite(master$che_pc_usd2023) &
              is.finite(master$gdp_pc_usd) &
              master$che_pc_usd2023 > 0 & master$gdp_pc_usd > 0, ]
  d$lche <- log(d$che_pc_usd2023); d$lgdp <- log(d$gdp_pc_usd)
  fit <- stats::lm(lche ~ lgdp, data = d)
  vc  <- sandwich::vcovHC(fit, type = "HC3")
  lmtest::coeftest(fit, vcov. = vc)
}

mr_cluster_se <- function(master) {
  if (!.mr_has("sandwich") || !.mr_has("lmtest")) {
    return(.mr_skip("sandwich/lmtest"))
  }
  d <- master[is.finite(master$che_pc_usd2023) &
              is.finite(master$gdp_pc_usd) &
              master$che_pc_usd2023 > 0 & master$gdp_pc_usd > 0, ]
  d$lche <- log(d$che_pc_usd2023); d$lgdp <- log(d$gdp_pc_usd)
  fit <- stats::lm(lche ~ lgdp, data = d)
  vc <- sandwich::vcovCL(fit, cluster = d$iso3_code)
  lmtest::coeftest(fit, vcov. = vc)
}

mr_decline_model <- function(master) {
  yrs <- range(master$year, na.rm = TRUE)
  d1 <- master[master$year == yrs[1] &
                is.finite(master$hf3_che),
                c("iso3_code", "country_name", "hf3_che")]
  d2 <- master[master$year == yrs[2] &
                is.finite(master$hf3_che),
                c("iso3_code", "hf3_che")]
  m <- merge(d1, d2, by = "iso3_code", suffixes = c(".y1", ".y2"))
  m$improved <- as.integer(m$hf3_che.y2 < m$hf3_che.y1)
  if (!nrow(m)) return(NULL)
  stats::glm(improved ~ hf3_che.y1, data = m, family = "binomial")
}

mr_lifeexp_hazard <- function(master) {
  if (!.mr_has("survival")) return(.mr_skip("survival"))
  d <- master[is.finite(master$life_exp) &
              is.finite(master$che_pc_usd2023) &
              master$che_pc_usd2023 > 0, ]
  if (nrow(d) < 100) return(NULL)
  d$above <- as.integer(d$life_exp >= 75)
  d$lche <- log(d$che_pc_usd2023)
  survival::coxph(survival::Surv(year, above) ~ lche,
                   data = d)
}

mr_maha_outlier <- function(master, year = NULL) {
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  cols <- intersect(c("hf3_che", "gghed_che", "che_pc_usd2023",
                       "life_exp", "u5mr"), names(master))
  d <- master[master$year == year, c("iso3_code", "country_name",
                                        cols)]
  d <- d[stats::complete.cases(d), ]
  if (nrow(d) < 30) return(NULL)
  X <- as.matrix(d[, cols])
  X <- scale(X)
  cov_inv <- tryCatch(solve(stats::cov(X)),
                       error = function(e) MASS::ginv(stats::cov(X)))
  mu <- colMeans(X)
  d$maha <- mahalanobis(X, mu, cov_inv, inverted = TRUE)
  d <- d[order(-d$maha), ]
  utils::head(d, 20)
}

mr_rdd_income <- function(master, year = NULL) {
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year &
              !is.na(master$income_group), ]
  if (nrow(d) < 30) return(NULL)
  d$high <- as.integer(d$income_group == "High income")
  d$lgdp <- log(pmax(d$gdp_pc_usd, 1))
  stats::lm(hf3_che ~ high + lgdp + high:lgdp, data = d)
}

mr_cv_log_che <- function(master, k = 10) {
  d <- master[is.finite(master$che_pc_usd2023) &
              is.finite(master$gdp_pc_usd) &
              master$che_pc_usd2023 > 0 & master$gdp_pc_usd > 0, ]
  if (nrow(d) < 100) return(NULL)
  d$lche <- log(d$che_pc_usd2023); d$lgdp <- log(d$gdp_pc_usd)
  set.seed(1)
  fold <- sample(rep(seq_len(k), length.out = nrow(d)))
  rmse <- numeric(k)
  for (i in seq_len(k)) {
    tr <- d[fold != i, ]; te <- d[fold == i, ]
    fit <- stats::lm(lche ~ lgdp, data = tr)
    pr <- stats::predict(fit, newdata = te)
    rmse[i] <- sqrt(mean((te$lche - pr) ^ 2, na.rm = TRUE))
  }
  list(rmse_mean = mean(rmse), rmse_sd = stats::sd(rmse),
       fold_rmse = rmse)
}


ghs_validate_models_robust <- function(master = NULL) {
  if (is.null(master)) {
    cache <- file.path(proj_root(), "\u6d3e\u751f\u6570\u636e",
                        "\u5904\u7406\u7ed3\u679c",
                        "master_enriched.rds")
    if (!file.exists(cache)) stop("Missing master_enriched.rds")
    master <- readRDS(cache)
  }
  fn_names <- ls(envir = .GlobalEnv, pattern = "^mr_[a-z]")
  results <- list()
  for (nm in fn_names) {
    f <- get(nm, envir = .GlobalEnv)
    res <- tryCatch({
      obj <- f(master)
      list(ok = !is.null(obj), class = class(obj)[1],
           is_skip = is.list(obj) && identical(obj[["status"]],
                                                  "skipped"))
    }, error = function(e) list(ok = FALSE,
                                  class = conditionMessage(e),
                                  is_skip = FALSE))
    results[[nm]] <- res
  }
  results
}
