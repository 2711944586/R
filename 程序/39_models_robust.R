# =============================================================================
# 程序/39_models_robust.R   —— D2 阶段：稳健/因果/非线性扩展（20）
# -----------------------------------------------------------------------------
# 命名前缀：mr_   （model_robust）
# =============================================================================

if (!exists("ensure_pkgs", mode = "function")) {
  source(file.path("\u7a0b\u5e8f", "00_utils.R"))
}

.mr_has  <- function(pkg) requireNamespace(pkg, quietly = TRUE)
.mr_skip <- function(reason) list(status = "skipped", reason = reason)

#' mr1 \u5206\u4f4d\u56de\u5f52 \u00b7 ln(CHE) ~ ln(GDP) on \u03c4=0.1/0.5/0.9
mr_quantile_reg <- function(master) {
  if (!.mr_has("quantreg")) return(.mr_skip("quantreg"))
  d <- master[is.finite(master$che_pc_usd2023) &
              is.finite(master$gdp_pc_usd) &
              master$che_pc_usd2023 > 0 & master$gdp_pc_usd > 0, ]
  if (nrow(d) < 100) return(NULL)
  d$lche <- log(d$che_pc_usd2023); d$lgdp <- log(d$gdp_pc_usd)
  quantreg::rq(lche ~ lgdp, tau = c(0.1, 0.5, 0.9), data = d)
}

#' mr2 \u9c81\u68d2 OLS \u00b7 rlm
mr_rlm <- function(master) {
  if (!.mr_has("MASS")) return(.mr_skip("MASS"))
  d <- master[is.finite(master$che_pc_usd2023) &
              is.finite(master$gdp_pc_usd) &
              master$che_pc_usd2023 > 0 & master$gdp_pc_usd > 0, ]
  if (nrow(d) < 100) return(NULL)
  d$lche <- log(d$che_pc_usd2023); d$lgdp <- log(d$gdp_pc_usd)
  MASS::rlm(lche ~ lgdp, data = d)
}

#' mr3 Theil-Sen \u4f30\u8ba1
mr_theil_sen <- function(master) {
  if (!.mr_has("mblm")) return(.mr_skip("mblm"))
  d <- master[is.finite(master$che_pc_usd2023) &
              is.finite(master$gdp_pc_usd) &
              master$che_pc_usd2023 > 0 & master$gdp_pc_usd > 0, ]
  if (nrow(d) < 50) return(NULL)
  d$lche <- log(d$che_pc_usd2023); d$lgdp <- log(d$gdp_pc_usd)
  d <- utils::head(d, 1500)  # mblm is slow
  mblm::mblm(lche ~ lgdp, dataframe = d, repeated = FALSE)
}

#' mr4 \u5e7f\u4e49\u53ef\u52a0\u6a21\u578b GAM \u00b7 \u5bff\u547d ~ s(log CHE)
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

#' mr5 GAM \u00b7 OOP ~ s(log GDP)
mr_gam_oop <- function(master) {
  if (!.mr_has("mgcv")) return(.mr_skip("mgcv"))
  d <- master[is.finite(master$hf3_che) &
              is.finite(master$gdp_pc_usd) &
              master$gdp_pc_usd > 0, ]
  if (nrow(d) < 100) return(NULL)
  d$lgdp <- log(d$gdp_pc_usd)
  mgcv::gam(hf3_che ~ s(lgdp, k = 5), data = d, method = "REML")
}

#' mr6 \u591a\u9879\u5f0f\u56de\u5f52 \u00b7 \u5bff\u547d\u4e0e log CHE \u4e09\u9636
mr_poly_lifeexp <- function(master) {
  d <- master[is.finite(master$life_exp) &
              is.finite(master$che_pc_usd2023) &
              master$che_pc_usd2023 > 0, ]
  if (nrow(d) < 100) return(NULL)
  d$lche <- log(d$che_pc_usd2023)
  stats::lm(life_exp ~ stats::poly(lche, 3), data = d)
}

#' mr7 \u6837\u6761\u62df\u5408 \u00b7 splines::bs
mr_bspline_che <- function(master) {
  if (!.mr_has("splines")) return(.mr_skip("splines"))
  d <- master[is.finite(master$che_pc_usd2023) &
              is.finite(master$year), ]
  if (nrow(d) < 50) return(NULL)
  d$lche <- log(pmax(d$che_pc_usd2023, 1))
  stats::lm(lche ~ splines::bs(year, df = 5), data = d)
}

#' mr8 Bootstrap 95% CI \u00b7 log-log \u5f39\u6027
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

#' mr9 Jackknife \u5f39\u6027
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

#' mr10 Permutation test \u00b7 \u5927\u6d32\u95f4\u5dee\u5f02
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

#' mr11 \u5dee\u5206\u4e2d\u7684\u5dee\u5206 DID \u00b7 COVID
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

#' mr12 \u5206\u6bb5\u56de\u5f52 \u00b7 changepoint
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

#' mr13 Tukey HSD \u00b7 \u6536\u5165\u7ec4 OOP
mr_tukey_oop <- function(master, year = NULL) {
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year &
              is.finite(master$hf3_che) &
              !is.na(master$income_group), ]
  if (nrow(d) < 50) return(NULL)
  fit <- stats::aov(hf3_che ~ income_group, data = d)
  stats::TukeyHSD(fit)
}

#' mr14 \u9c81\u68d2\u6807\u51c6\u8bef HC3
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

#' mr15 \u7c7b\u7fa4\u9c81\u68d2\u6807\u51c6\u8bef
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

#' mr16 \u4f8b\u5916\u5012\u9000 \u5012\u5411\u7269\u7406\u6a21\u578b
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

#' mr17 \u751f\u5b58\u5206\u6790 \u00b7 \u9884\u671f\u5bff\u547d\u8d85 75 \u00b7 hazard
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

#' mr18 \u8ddd\u79bb \u00b7 PCA-Maha
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

#' mr19 RDD \u4f3c\u6837\u672c \u00b7 \u4e2d\u9ad8\u5dee\u8ddd
mr_rdd_income <- function(master, year = NULL) {
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year &
              !is.na(master$income_group), ]
  if (nrow(d) < 30) return(NULL)
  # \u7eaf\u5408\u6210\u00b7\u4ee5\u6536\u5165\u7ec4 dummy \u4f30\u8ba1\u5dee\u5f02
  d$high <- as.integer(d$income_group == "High income")
  d$lgdp <- log(pmax(d$gdp_pc_usd, 1))
  stats::lm(hf3_che ~ high + lgdp + high:lgdp, data = d)
}

#' mr20 K\u6298\u4ea4\u53c9\u9a8c\u8bc1 RMSE
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

# =============================================================================
# 批量验证
# =============================================================================

#' 验证 mr_* 模型
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
