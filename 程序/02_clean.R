
if (!exists("proj_root", mode = "function")) {
  source(file.path("程序", "00_utils.R"))
}
if (!exists("load_ghs", mode = "function")) {
  source(file.path("程序", "01_io.R"))
}

long_tidy <- function(ghs = load_ghs()) {
  ensure_pkgs(c("dplyr"))
  bind_one <- function(df, dataset_name, label_col) {
    df |>
      dplyr::mutate(
        dataset      = dataset_name,
        prefix       = ghs_indicator_prefix(.data$indicator_code),
        unit_type    = ghs_indicator_unit(.data$indicator_code),
        label        = .data[[label_col]]
      )
  }
  out <- dplyr::bind_rows(
    bind_one(ghs$financing_schemes, "financing_schemes", "financing_scheme"),
    bind_one(ghs$health_spending,   "health_spending",   "expenditure_type"),
    bind_one(ghs$spending_purpose,  "spending_purpose",  "spending_purpose")
  )
  out |>
    dplyr::mutate(
      value = dplyr::if_else(.data$value < 0, 0, .data$value)
    ) |>
    dplyr::select(
      dplyr::any_of(c(
        "dataset", "country_name", "iso3_code", "year",
        "indicator_code", "prefix", "unit_type", "label", "value", "unit"
      ))
    )
}

to_wide <- function(df) {
  ensure_pkgs(c("dplyr", "tidyr"))
  df |>
    dplyr::select(dplyr::any_of(c(
      "country_name", "iso3_code", "year", "indicator_code", "value"
    ))) |>
    tidyr::pivot_wider(
      names_from = "indicator_code",
      values_from = "value",
      values_fn  = ~ mean(.x, na.rm = TRUE)
    )
}

check_scheme_sum <- function(financing_schemes, tol = 5) {
  ensure_pkgs(c("dplyr"))
  financing_schemes |>
    dplyr::filter(grepl("_che$", .data$indicator_code)) |>
    dplyr::mutate(prefix = ghs_indicator_prefix(.data$indicator_code)) |>
    dplyr::filter(.data$prefix %in% c("hf1", "hf2", "hf3", "hf4", "hfnec")) |>
    dplyr::group_by(.data$country_name, .data$iso3_code, .data$year) |>
    dplyr::summarise(
      pct_sum = sum(.data$value, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      deviation   = .data$pct_sum - 100,
      within_tol  = abs(.data$deviation) <= tol
    )
}

check_source_sum <- function(health_spending, tol = 5) {
  ensure_pkgs(c("dplyr"))
  health_spending |>
    dplyr::filter(grepl("_che$", .data$indicator_code)) |>
    dplyr::mutate(prefix = ghs_indicator_prefix(.data$indicator_code)) |>
    dplyr::filter(.data$prefix %in% c("gghed", "pvtd", "ext")) |>
    dplyr::group_by(.data$country_name, .data$iso3_code, .data$year) |>
    dplyr::summarise(
      pct_sum = sum(.data$value, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      deviation  = .data$pct_sum - 100,
      within_tol = abs(.data$deviation) <= tol
    )
}

build_master_wide <- function(ghs = load_ghs()) {
  ensure_pkgs(c("dplyr"))
  w_fs <- to_wide(ghs$financing_schemes)
  w_hs <- to_wide(ghs$health_spending)
  w_sp <- to_wide(ghs$spending_purpose)
  w_hs |>
    dplyr::full_join(w_fs, by = c("country_name", "iso3_code", "year")) |>
    dplyr::full_join(w_sp, by = c("country_name", "iso3_code", "year")) |>
    dplyr::arrange(.data$country_name, .data$year)
}
