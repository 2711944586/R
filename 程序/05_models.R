
if (!exists("proj_root", mode = "function")) {
  source(file.path("程序", "00_utils.R"))
}

fit_pca <- function(df, vars, scale = TRUE) {
  ensure_pkgs(c("dplyr"))
  keep <- df |>
    dplyr::select(dplyr::all_of(c("iso3_code", "country_name", vars))) |>
    tidyr::drop_na()
  if (nrow(keep) < 5) {
    warning("PCA: fewer than 5 complete observations; returning NULL")
    return(NULL)
  }
  X <- as.matrix(keep[, vars, drop = FALSE])
  rownames(X) <- keep$iso3_code
  pca <- stats::prcomp(X, center = TRUE, scale. = scale)
  list(
    pca    = pca,
    scores = as.data.frame(pca$x) |>
      tibble::rownames_to_column("iso3_code") |>
      dplyr::left_join(keep[, c("iso3_code", "country_name")], by = "iso3_code"),
    loadings = as.data.frame(pca$rotation) |> tibble::rownames_to_column("variable"),
    var_explained = summary(pca)$importance["Proportion of Variance", ]
  )
}

fit_cluster <- function(pca_obj, k = 4, seed = 1L) {
  if (is.null(pca_obj)) return(NULL)
  set.seed(seed)
  X <- pca_obj$scores[, grep("^PC", names(pca_obj$scores))]
  if (ncol(X) < 2) {
    warning("cluster: need at least 2 PCs")
    return(NULL)
  }
  km <- stats::kmeans(X[, 1:min(ncol(X), 5)], centers = k, nstart = 25)
  scores <- pca_obj$scores
  scores$cluster <- factor(km$cluster)
  list(km = km, scores = scores, k = k)
}

detect_changepoints <- function(ts_vec, years, method = "PELT", min_seg = 4) {
  if (!requireNamespace("changepoint", quietly = TRUE)) {
    warning("changepoint package not installed; returning NULL")
    return(NULL)
  }
  ts_vec <- as.numeric(ts_vec)
  ok <- is.finite(ts_vec)
  if (sum(ok) < min_seg * 2) return(NULL)
  cpt <- tryCatch(
    changepoint::cpt.mean(ts_vec[ok], method = method, minseglen = min_seg),
    error = function(e) NULL
  )
  if (is.null(cpt)) return(NULL)
  pts <- cpt@cpts
  pts <- pts[pts < length(ts_vec[ok])]
  tibble::tibble(
    year_index  = pts,
    year        = years[ok][pts],
    seg_means   = if (length(cpt@param.est$mean)) cpt@param.est$mean[seq_along(pts)] else NA_real_
  )
}

fit_beta_convergence <- function(master, start_year = 2000, end_year = 2023) {
  ensure_pkgs(c("dplyr"))
  if (!requireNamespace("fixest", quietly = TRUE)) {
    warning("fixest not installed; beta-convergence skipped")
    return(NULL)
  }
  panel <- master |>
    dplyr::filter(.data$year %in% c(start_year, end_year),
                  is.finite(.data$che_pc_usd2023),
                  .data$che_pc_usd2023 > 0) |>
    dplyr::select("iso3_code", "continent", "year", "che_pc_usd2023") |>
    tidyr::pivot_wider(names_from = "year", values_from = "che_pc_usd2023",
                       names_prefix = "y_") |>
    tidyr::drop_na() |>
    dplyr::mutate(
      log_start  = log(!!rlang::sym(paste0("y_", start_year))),
      log_end    = log(!!rlang::sym(paste0("y_", end_year))),
      growth     = (.data$log_end - .data$log_start) / (end_year - start_year)
    )
  if (nrow(panel) < 10) return(NULL)
  mod <- tryCatch(
    fixest::feols(growth ~ log_start | continent, data = panel),
    error = function(e) lm(growth ~ log_start + continent, data = panel)
  )
  list(
    panel = panel,
    model = mod,
    beta  = stats::coef(mod)[["log_start"]],
    half_life = if ("log_start" %in% names(stats::coef(mod))) {
      -log(2) / stats::coef(mod)[["log_start"]]
    } else NA_real_
  )
}

fit_forecast <- function(ts_vec, years, h = 5, method = c("auto", "prophet")) {
  method <- match.arg(method)
  ts_vec <- as.numeric(ts_vec)
  ok <- is.finite(ts_vec)
  if (sum(ok) < 5) return(NULL)
  ts_vec <- ts_vec[ok]; years <- years[ok]
  if (method == "auto" && requireNamespace("forecast", quietly = TRUE)) {
    ts_obj <- stats::ts(ts_vec, start = min(years), frequency = 1)
    fit <- tryCatch(forecast::auto.arima(ts_obj), error = function(e) NULL)
    if (is.null(fit)) return(NULL)
    fc <- forecast::forecast(fit, h = h)
    return(tibble::tibble(
      year  = (max(years) + 1):(max(years) + h),
      point = as.numeric(fc$mean),
      lo_80 = as.numeric(fc$lower[, 1]),
      hi_80 = as.numeric(fc$upper[, 1]),
      lo_95 = as.numeric(fc$lower[, 2]),
      hi_95 = as.numeric(fc$upper[, 2]),
      method = "ARIMA"
    ))
  }
  if (method == "prophet" && requireNamespace("prophet", quietly = TRUE)) {
    dfp <- data.frame(
      ds = as.Date(paste0(years, "-01-01")),
      y  = ts_vec
    )
    m <- tryCatch(
      prophet::prophet(dfp, yearly.seasonality = FALSE, weekly.seasonality = FALSE,
                       daily.seasonality = FALSE, verbose = FALSE),
      error = function(e) NULL
    )
    if (is.null(m)) return(NULL)
    future <- prophet::make_future_dataframe(m, periods = h, freq = "year")
    pred <- stats::predict(m, future)
    tail_n <- utils::tail(pred, h)
    return(tibble::tibble(
      year  = as.integer(format(tail_n$ds, "%Y")),
      point = tail_n$yhat,
      lo_80 = tail_n$yhat_lower,
      hi_80 = tail_n$yhat_upper,
      lo_95 = tail_n$yhat_lower,
      hi_95 = tail_n$yhat_upper,
      method = "Prophet"
    ))
  }
  NULL
}

fit_panel_fe <- function(master) {
  ensure_pkgs("dplyr")
  if (!requireNamespace("fixest", quietly = TRUE)) {
    warning("fixest not installed; returning NULL")
    return(NULL)
  }
  dat <- master |>
    dplyr::filter(is.finite(.data$che_pc_usd2023), .data$che_pc_usd2023 > 0,
                  is.finite(.data$gdp_pc_usd),      .data$gdp_pc_usd > 0) |>
    dplyr::mutate(
      log_che_pc = log(.data$che_pc_usd2023),
      log_gdp_pc = log(.data$gdp_pc_usd)
    )
  if (nrow(dat) < 100) return(NULL)
  tryCatch(
    fixest::feols(log_che_pc ~ log_gdp_pc | iso3_code + year, data = dat,
                  cluster = ~iso3_code),
    error = function(e) NULL
  )
}
