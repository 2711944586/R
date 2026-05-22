
if (!exists("proj_root", mode = "function")) {
  source(file.path("程序", "00_utils.R"))
}


catastrophic_oop_share <- function(master_panel, threshold = c(10, 25)) {
  threshold <- match.arg(as.character(threshold), c("10", "25"))
  ensure_pkgs("dplyr")
  col <- if (threshold == "10") "UHC_FIN" else "UHC_FIN25"
  if (!col %in% names(master_panel)) {
    return(tibble::tibble(iso3_code = character(),
                          year = integer(),
                          catastrophic_pct = numeric()))
  }
  master_panel |>
    dplyr::filter(is.finite(.data[[col]])) |>
    dplyr::transmute(
      iso3_code = .data$iso3_code,
      year      = .data$year,
      catastrophic_pct = .data[[col]]
    )
}


fit_elasticity_segments <- function(master_panel, breaks = c(7.5, 9.5)) {
  ensure_pkgs("dplyr")
  d <- master_panel |>
    dplyr::filter(is.finite(.data$che_pc_usd2023), .data$che_pc_usd2023 > 0,
                  is.finite(.data$gdp_pc_usd),     .data$gdp_pc_usd > 0) |>
    dplyr::transmute(
      iso3_code = .data$iso3_code,
      year      = .data$year,
      log_che   = log(.data$che_pc_usd2023),
      log_gdp   = log(.data$gdp_pc_usd),
      seg = cut(log(.data$gdp_pc_usd), c(-Inf, breaks, Inf),
                labels = c("Low", "Mid", "High"))
    )
  if (nrow(d) < 30) return(NULL)
  fits <- d |>
    dplyr::group_by(.data$seg) |>
    dplyr::summarise(
      n        = dplyr::n(),
      beta     = stats::coef(stats::lm(log_che ~ log_gdp, data = dplyr::cur_data()))[["log_gdp"]],
      se       = summary(stats::lm(log_che ~ log_gdp, data = dplyr::cur_data()))$coefficients["log_gdp", "Std. Error"],
      r2       = summary(stats::lm(log_che ~ log_gdp, data = dplyr::cur_data()))$r.squared,
      .groups  = "drop"
    ) |>
    dplyr::mutate(
      ci_lo = .data$beta - 1.96 * .data$se,
      ci_hi = .data$beta + 1.96 * .data$se
    )
  list(panel = d, fits = fits)
}


fit_dea_efficiency <- function(master_panel, year_focus = 2019) {
  ensure_pkgs("dplyr")
  if (!all(c("HALE", "che_pc_usd2023") %in% names(master_panel))) {
    return(NULL)
  }
  d <- master_panel |>
    dplyr::filter(.data$year == year_focus,
                  is.finite(.data$che_pc_usd2023), .data$che_pc_usd2023 > 0,
                  is.finite(.data$HALE), .data$HALE > 0)
  if (nrow(d) < 10) return(NULL)
  d <- d |>
    dplyr::mutate(
      ratio = .data$HALE / .data$che_pc_usd2023,
      eff_crs = .data$ratio / max(.data$ratio, na.rm = TRUE)
    )
  if ("income_group" %in% names(d)) {
    d <- d |>
      dplyr::group_by(.data$income_group) |>
      dplyr::mutate(eff_vrs = .data$ratio / max(.data$ratio, na.rm = TRUE)) |>
      dplyr::ungroup()
  } else {
    d$eff_vrs <- d$eff_crs
  }
  d |>
    dplyr::select(dplyr::any_of(c("iso3_code", "country_name", "year",
                                   "continent", "income_group",
                                   "che_pc_usd2023", "HALE",
                                   "ratio", "eff_crs", "eff_vrs"))) |>
    dplyr::arrange(dplyr::desc(.data$eff_crs))
}


mc_scenarios <- function(master_panel, target_pct = 0.07,
                         n_sims = 1000, horizon = 7,
                         seed = 42) {
  ensure_pkgs("dplyr")
  set.seed(seed)
  baseline <- master_panel |>
    dplyr::filter(.data$year == max(.data$year, na.rm = TRUE),
                  is.finite(.data$che_pc_usd2023),
                  is.finite(.data$gdp_pc_usd),
                  is.finite(.data$life_exp))
  if (!nrow(baseline)) return(NULL)
  elastic <- tryCatch({
    fit <- stats::lm(life_exp ~ log(che_pc_usd2023), data = baseline)
    as.numeric(stats::coef(fit)["log(che_pc_usd2023)"])
  }, error = function(e) 1.5)
  sim <- expand.grid(iso3_code = baseline$iso3_code, sim = seq_len(n_sims),
                     year_offset = seq_len(horizon)) |>
    tibble::as_tibble() |>
    dplyr::left_join(baseline, by = "iso3_code") |>
    dplyr::mutate(
      noise   = stats::rnorm(dplyr::n(), 0, 0.05),
      che_target = .data$gdp_pc_usd * target_pct,
      che_path = .data$che_pc_usd2023 +
                 (.data$che_target - .data$che_pc_usd2023) *
                 (.data$year_offset / horizon) * (1 + .data$noise),
      gain_years = elastic * (log(.data$che_path) - log(.data$che_pc_usd2023))
    ) |>
    dplyr::filter(.data$year_offset == horizon) |>
    dplyr::group_by(.data$iso3_code, .data$country_name) |>
    dplyr::summarise(
      mean_gain = mean(.data$gain_years, na.rm = TRUE),
      lo80      = stats::quantile(.data$gain_years, 0.10, na.rm = TRUE),
      hi80      = stats::quantile(.data$gain_years, 0.90, na.rm = TRUE),
      lo95      = stats::quantile(.data$gain_years, 0.025, na.rm = TRUE),
      hi95      = stats::quantile(.data$gain_years, 0.975, na.rm = TRUE),
      .groups   = "drop"
    )
  list(elasticity = elastic, summary = sim, n_sims = n_sims, horizon = horizon)
}


fit_lorenz <- function(values, weights = NULL) {
  ok <- is.finite(values) & values >= 0
  values <- values[ok]
  if (!length(values)) return(NULL)
  if (is.null(weights)) weights <- rep(1, length(values))
  weights <- weights[ok]
  ord <- order(values)
  v <- values[ord]; w <- weights[ord]
  cum_w <- cumsum(w) / sum(w)
  cum_v <- cumsum(v * w) / sum(v * w)
  tibble::tibble(p_pop = c(0, cum_w), p_value = c(0, cum_v))
}


concentration_index <- function(values, income) {
  if (!requireNamespace("ineq", quietly = TRUE)) return(NULL)
  ok <- is.finite(values) & is.finite(income) & income > 0
  values <- values[ok]; income <- income[ok]
  if (length(values) < 5) return(NULL)
  ord <- order(income)
  v <- values[ord]
  rk <- seq_along(v) / length(v)
  c_i <- 2 * stats::cov(v, rk) / mean(v)
  g_i <- ineq::Gini(income)
  list(C = c_i, G = g_i, kakwani = c_i - g_i)
}


fit_panel_iv <- function(master_panel) {
  if (!requireNamespace("fixest", quietly = TRUE)) return(NULL)
  ensure_pkgs("dplyr")
  d <- master_panel |>
    dplyr::filter(is.finite(.data$che_pc_usd2023), .data$che_pc_usd2023 > 0,
                  is.finite(.data$gdp_pc_usd),     .data$gdp_pc_usd > 0) |>
    dplyr::arrange(.data$iso3_code, .data$year) |>
    dplyr::group_by(.data$iso3_code) |>
    dplyr::mutate(
      log_che = log(.data$che_pc_usd2023),
      log_gdp = log(.data$gdp_pc_usd),
      log_gdp_lag1 = dplyr::lag(.data$log_gdp, 1),
      log_gdp_lag2 = dplyr::lag(.data$log_gdp, 2)
    ) |>
    dplyr::ungroup() |>
    tidyr::drop_na(dplyr::any_of(c("log_che", "log_gdp",
                                     "log_gdp_lag1", "log_gdp_lag2")))
  if (nrow(d) < 100) return(NULL)
  tryCatch(
    fixest::feols(log_che ~ 1 | iso3_code + year |
                  log_gdp ~ log_gdp_lag1 + log_gdp_lag2,
                  data = d, cluster = ~iso3_code),
    error = function(e) NULL
  )
}


changepoint_panel <- function(master_panel,
                                indicator_col = "hf3_che",
                                min_n = 10) {
  if (!requireNamespace("changepoint", quietly = TRUE)) return(NULL)
  ensure_pkgs("dplyr")
  panel <- master_panel |>
    dplyr::filter(is.finite(.data[[indicator_col]])) |>
    dplyr::group_by(.data$iso3_code) |>
    dplyr::filter(dplyr::n() >= min_n) |>
    dplyr::ungroup()
  if (!nrow(panel)) return(NULL)
  isos <- unique(panel$iso3_code)
  rows <- lapply(isos, function(iso) {
    sub <- dplyr::filter(panel, .data$iso3_code == iso) |>
      dplyr::arrange(.data$year)
    res <- tryCatch(
      changepoint::cpt.mean(sub[[indicator_col]], method = "PELT", penalty = "BIC"),
      error = function(e) NULL
    )
    if (is.null(res)) return(NULL)
    cp_idx <- changepoint::cpts(res)
    if (!length(cp_idx)) return(NULL)
    tibble::tibble(
      iso3_code = iso,
      country_name = sub$country_name[1],
      changepoint_year = sub$year[cp_idx],
      n_obs = nrow(sub)
    )
  })
  dplyr::bind_rows(Filter(Negate(is.null), rows))
}


fit_growth_decomposition <- function(master_panel) {
  if (!requireNamespace("fixest", quietly = TRUE)) return(NULL)
  ensure_pkgs("dplyr")
  d <- master_panel |>
    dplyr::filter(is.finite(.data$che_pc_usd2023), .data$che_pc_usd2023 > 0,
                  is.finite(.data$gdp_pc_usd),     .data$gdp_pc_usd > 0,
                  is.finite(.data$pop_65)) |>
    dplyr::arrange(.data$iso3_code, .data$year) |>
    dplyr::group_by(.data$iso3_code) |>
    dplyr::mutate(
      d_log_che = log(.data$che_pc_usd2023) - dplyr::lag(log(.data$che_pc_usd2023)),
      d_log_gdp = log(.data$gdp_pc_usd) - dplyr::lag(log(.data$gdp_pc_usd)),
      d_pop_65  = .data$pop_65 - dplyr::lag(.data$pop_65)
    ) |>
    dplyr::ungroup() |>
    tidyr::drop_na(dplyr::any_of(c("d_log_che", "d_log_gdp", "d_pop_65")))
  if (nrow(d) < 100) return(NULL)
  tryCatch(
    fixest::feols(d_log_che ~ d_log_gdp + d_pop_65 | iso3_code + year,
                  data = d, cluster = ~iso3_code),
    error = function(e) NULL
  )
}
