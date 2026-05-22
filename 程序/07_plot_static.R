
if (!exists("proj_root", mode = "function")) {
  source(file.path("程序", "00_utils.R"))
  source(file.path("程序", "06_plot_theme.R"))
}


plot_source_area <- function(master,
                             title = "\u5168\u7403\u536b\u751f\u652f\u51fa\u6765\u6e90\u7ed3\u6784\u6f14\u5316 Global Health Financing Sources",
                             subtitle = NULL) {
  ensure_pkgs(c("dplyr", "tidyr", "ggplot2", "scales"))
  has_pop <- "pop" %in% names(master) && any(is.finite(master$pop))
  safe_wmean <- function(x, w) {
    ok <- is.finite(x) & is.finite(w) & w > 0
    if (!any(ok)) return(mean(x, na.rm = TRUE))
    stats::weighted.mean(x[ok], w[ok], na.rm = TRUE)
  }
  if (is.null(subtitle)) {
    subtitle <- if (has_pop) {
      "2000\u20132023 \u00b7 \u6309\u4eba\u53e3\u52a0\u6743\u5747\u503c\uff0c\u5360\u603b\u5f00\u652f % of CHE"
    } else {
      "2000\u20132023 \u00b7 \u672a\u52a0\u6743\u5747\u503c\uff0c\u5360\u603b\u5f00\u652f % of CHE"
    }
  }
  d <- master |>
    dplyr::group_by(.data$year) |>
    dplyr::summarise(
      gghed = if (has_pop) {
        safe_wmean(.data$gghed_che, .data$pop)
      } else mean(.data$gghed_che, na.rm = TRUE),
      pvtd  = if (has_pop) {
        safe_wmean(.data$pvtd_che, .data$pop)
      } else mean(.data$pvtd_che, na.rm = TRUE),
      ext   = if (has_pop) {
        safe_wmean(.data$ext_che, .data$pop)
      } else mean(.data$ext_che, na.rm = TRUE),
      .groups = "drop"
    ) |>
    tidyr::pivot_longer(c("gghed", "pvtd", "ext"),
                        names_to = "source", values_to = "pct") |>
    dplyr::mutate(source = factor(.data$source, levels = c("gghed", "pvtd", "ext")))
  ggplot2::ggplot(d, ggplot2::aes(.data$year, .data$pct, fill = .data$source)) +
    ggplot2::geom_area(alpha = 0.9, position = "stack") +
    scale_fill_ghs_source(
      labels = c(gghed = "\u653f\u5e9c GGHE-D", pvtd = "\u79c1\u4eba PVT-D", ext = "\u5916\u63f4 EXT")
    ) +
    ggplot2::scale_y_continuous(labels = scales::label_percent(scale = 1)) +
    labs_ghs(
      title = title, subtitle = subtitle,
      x = "\u5e74\u4efd Year", y = "\u5360\u6bd4 Share (%)"
    ) +
    theme_ghs()
}

plot_oops_ranking <- function(master, year_focus = 2023, top_n = 15,
                              title = "\u81ea\u4ed8\u4f9d\u8d56 OOPS Ranking",
                              subtitle = NULL) {
  ensure_pkgs(c("dplyr", "ggplot2", "forcats", "scales"))
  d <- master |>
    dplyr::filter(.data$year == year_focus, !is.na(.data$hf3_che)) |>
    dplyr::arrange(dplyr::desc(.data$hf3_che))
  top <- utils::head(d, top_n) |> dplyr::mutate(group = "\u6700\u9ad8 Highest")
  bot <- utils::tail(d, top_n) |> dplyr::mutate(group = "\u6700\u4f4e Lowest")
  plot_d <- dplyr::bind_rows(top, bot) |>
    dplyr::mutate(
      country_name = forcats::fct_reorder(.data$country_name, .data$hf3_che)
    )
  subtitle <- subtitle %||% sprintf(
    "%d \u5e74 OOPS \u5360 CHE \u767e\u5206\u6bd4  \u00b7  Top/Bottom %d", year_focus, top_n
  )
  ggplot2::ggplot(plot_d,
                  ggplot2::aes(.data$hf3_che, .data$country_name, colour = .data$group)) +
    ggplot2::geom_segment(ggplot2::aes(x = 0, xend = .data$hf3_che,
                                        yend = .data$country_name),
                          linewidth = 1) +
    ggplot2::geom_point(size = 3) +
    ggplot2::scale_x_continuous(labels = scales::label_percent(scale = 1),
                                 limits = c(0, max(plot_d$hf3_che) * 1.05)) +
    ggplot2::scale_colour_manual(values = c("\u6700\u9ad8 Highest" = "#C0504D",
                                            "\u6700\u4f4e Lowest"  = "#1B5E88")) +
    ggplot2::facet_wrap(~ .data$group, scales = "free_y") +
    labs_ghs(title = title, subtitle = subtitle,
             x = "OOPS \u5360\u6bd4 (% of CHE)", y = NULL) +
    theme_ghs(grid = "x") +
    ggplot2::theme(legend.position = "none")
}


plot_oops_box_continent <- function(master, year_focus = 2023,
                                    title = "\u5404\u5927\u6d32 OOPS \u5360\u6bd4\u5206\u5e03  By Continent",
                                    subtitle = NULL) {
  ensure_pkgs(c("dplyr", "ggplot2", "scales"))
  d <- master |>
    dplyr::filter(.data$year == year_focus,
                  !is.na(.data$hf3_che),
                  !is.na(.data$continent))
  subtitle <- subtitle %||% sprintf("%d \u5e74  \u00b7  \u9ed1\u70b9 = \u5404\u56fd", year_focus)
  ggplot2::ggplot(d, ggplot2::aes(.data$continent, .data$hf3_che,
                                   fill = .data$continent)) +
    ggplot2::geom_boxplot(alpha = 0.5, outlier.shape = NA, width = 0.5) +
    ggplot2::geom_jitter(width = 0.15, alpha = 0.6, size = 1.5, colour = "grey20") +
    scale_fill_ghs_continent() +
    ggplot2::scale_y_continuous(labels = scales::label_percent(scale = 1)) +
    labs_ghs(title = title, subtitle = subtitle,
             x = NULL, y = "OOPS \u5360\u6bd4 (% of CHE)") +
    theme_ghs() +
    ggplot2::theme(legend.position = "none")
}

plot_oops_ridges_income <- function(master, year_focus = 2023,
                                    title = "\u4e0d\u540c\u6536\u5165\u7ec4 OOPS \u5206\u5e03  By Income Group",
                                    subtitle = NULL) {
  ensure_pkgs(c("dplyr", "ggplot2", "ggridges", "scales"))
  d <- master |>
    dplyr::filter(.data$year == year_focus,
                  !is.na(.data$hf3_che),
                  !is.na(.data$income_group))
  subtitle <- subtitle %||% sprintf("%d \u5e74  \u00b7  \u5bc6\u5ea6\u5c71\u810a\u56fe", year_focus)
  ggplot2::ggplot(d,
                  ggplot2::aes(x = .data$hf3_che,
                               y = forcats::fct_relevel(
                                 .data$income_group,
                                 c("Low income", "Lower middle income",
                                   "Upper middle income", "High income")
                               ),
                               fill = .data$income_group)) +
    ggridges::geom_density_ridges(alpha = 0.8, colour = "white", scale = 1.1) +
    scale_fill_ghs_income() +
    ggplot2::scale_x_continuous(labels = scales::label_percent(scale = 1)) +
    labs_ghs(title = title, subtitle = subtitle,
             x = "OOPS \u5360\u6bd4 (% of CHE)", y = NULL) +
    theme_ghs(grid = "x") +
    ggplot2::theme(legend.position = "none")
}


plot_world_choropleth <- function(master, world_sf,
                                  indicator_col = "hf3_che",
                                  year_focus = 2023,
                                  title = "OOPS \u4e16\u754c\u5730\u56fe  World Choropleth",
                                  subtitle = NULL,
                                  palette = "magma",
                                  label_fmt = scales::label_percent(scale = 1)) {
  ensure_pkgs(c("dplyr", "ggplot2", "sf"))
  d <- master |>
    dplyr::filter(.data$year == year_focus) |>
    dplyr::select("iso3_code", val = !!rlang::sym(indicator_col))
  map_df <- world_sf |>
    dplyr::left_join(d, by = "iso3_code")
  subtitle <- subtitle %||% sprintf("%d \u5e74  \u00b7  \u7070\u8272\u4e3a\u6570\u636e\u7f3a\u5931", year_focus)
  ggplot2::ggplot(map_df) +
    ggplot2::geom_sf(ggplot2::aes(fill = .data$val), colour = "white",
                     linewidth = 0.1) +
    ggplot2::scale_fill_viridis_c(option = palette, na.value = "grey85",
                                   labels = label_fmt) +
    ggplot2::coord_sf(crs = "+proj=robin") +
    labs_ghs(title = title, subtitle = subtitle,
             x = NULL, y = NULL) +
    theme_ghs(grid = FALSE) +
    ggplot2::theme(axis.text = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank(),
                   legend.key.width = ggplot2::unit(1.2, "cm"))
}


plot_country_profile <- function(master, iso = "CHN",
                                 title = NULL) {
  ensure_pkgs(c("dplyr", "tidyr", "ggplot2", "patchwork", "scales"))
  d <- master |> dplyr::filter(.data$iso3_code == iso)
  if (nrow(d) == 0) stop("No data for iso3_code: ", iso)

  cn <- unique(d$country_name)[1]
  title <- title %||% sprintf("%s \u536b\u751f\u652f\u51fa\u753b\u50cf  Health Spending Profile", cn)

  p1 <- ggplot2::ggplot(d, ggplot2::aes(.data$year, .data$che_usd2023 / 1e9)) +
    ggplot2::geom_area(fill = "#1B5E88", alpha = 0.85) +
    ggplot2::labs(subtitle = "\u5f53\u524d\u536b\u751f\u603b\u652f\u51fa CHE (\u5341\u4ebf\u7f8e\u5143)",
                  x = NULL, y = NULL) +
    theme_ghs(base_size = 10)

  d2 <- d |>
    dplyr::select("year", gghed = "gghed_che", pvtd = "pvtd_che", ext = "ext_che") |>
    tidyr::pivot_longer(c("gghed", "pvtd", "ext"), names_to = "src", values_to = "pct")
  p2 <- ggplot2::ggplot(d2, ggplot2::aes(.data$year, .data$pct, fill = .data$src)) +
    ggplot2::geom_area(alpha = 0.85) +
    scale_fill_ghs_source(
      labels = c(gghed = "\u653f\u5e9c", pvtd = "\u79c1\u4eba", ext = "\u5916\u63f4")
    ) +
    ggplot2::scale_y_continuous(labels = scales::label_percent(scale = 1)) +
    ggplot2::labs(subtitle = "\u6765\u6e90\u7ed3\u6784 (%)",
                  x = NULL, y = NULL, fill = NULL) +
    theme_ghs(base_size = 10)

  d3 <- d |>
    dplyr::select("year", hf1 = "hf1_che", hf2 = "hf2_che",
                  hf3 = "hf3_che", hf4 = "hf4_che", hfnec = "hfnec_che") |>
    tidyr::pivot_longer(c("hf1", "hf2", "hf3", "hf4", "hfnec"),
                        names_to = "sch", values_to = "pct")
  p3 <- ggplot2::ggplot(d3, ggplot2::aes(.data$year, .data$pct, fill = .data$sch)) +
    ggplot2::geom_area(alpha = 0.85) +
    scale_fill_ghs_scheme() +
    ggplot2::scale_y_continuous(labels = scales::label_percent(scale = 1)) +
    ggplot2::labs(subtitle = "\u7b79\u8d44\u65b9\u6848 (%)",
                  x = NULL, y = NULL, fill = NULL) +
    theme_ghs(base_size = 10)

  p4 <- if ("che_pc_usd2023" %in% names(d)) {
    ggplot2::ggplot(d, ggplot2::aes(.data$year, .data$che_pc_usd2023)) +
      ggplot2::geom_line(colour = "#C0504D", linewidth = 1.2) +
      ggplot2::geom_point(colour = "#C0504D", size = 1.2) +
      ggplot2::labs(subtitle = "\u4eba\u5747 CHE (USD 2023)",
                    x = NULL, y = NULL) +
      theme_ghs(base_size = 10)
  } else {
    patchwork::plot_spacer()
  }

  d5 <- d |>
    dplyr::select("year", hc1 = "hc1_che", hc6 = "hc6_che") |>
    tidyr::drop_na() |>
    tidyr::pivot_longer(c("hc1", "hc6"), names_to = "purpose", values_to = "pct")
  p5 <- if (nrow(d5) >= 2) {
    ggplot2::ggplot(d5, ggplot2::aes(.data$year, .data$pct,
                                      colour = .data$purpose)) +
      ggplot2::geom_line(linewidth = 1.2) + ggplot2::geom_point(size = 1.4) +
      ggplot2::scale_colour_manual(
        values = c(hc1 = "#1B5E88", hc6 = "#2E8B57"),
        labels = c(hc1 = "\u6cbb\u7597 Curative", hc6 = "\u9884\u9632 Preventive")
      ) +
      ggplot2::scale_y_continuous(labels = scales::label_percent(scale = 1)) +
      ggplot2::labs(subtitle = "\u6cbb\u7597 vs \u9884\u9632", x = NULL, y = NULL,
                    colour = NULL) +
      theme_ghs(base_size = 10)
  } else {
    patchwork::plot_spacer()
  }

  p6 <- {
    last_row <- d |> dplyr::filter(.data$year == max(.data$year, na.rm = TRUE))
    kpi <- data.frame(
      metric = c("CHE (USD 2023)", "OOPS %", "\u653f\u5e9c %", "\u5916\u63f4 %"),
      value  = c(
        fmt_usd(last_row$che_usd2023),
        fmt_pct(last_row$hf3_che),
        fmt_pct(last_row$gghed_che),
        fmt_pct(last_row$ext_che)
      )
    )
    ggplot2::ggplot(kpi, ggplot2::aes(x = 1, y = seq_along(.data$metric))) +
      ggplot2::geom_text(ggplot2::aes(label = paste0(.data$metric, "\n", .data$value)),
                         size = 4, lineheight = 1) +
      ggplot2::scale_y_reverse() +
      ggplot2::labs(subtitle = sprintf("%d \u5e74\u5feb\u7167 Snapshot",
                                        max(d$year, na.rm = TRUE))) +
      theme_ghs(base_size = 10, grid = FALSE) +
      ggplot2::theme(axis.text = ggplot2::element_blank(),
                     axis.title = ggplot2::element_blank(),
                     panel.grid = ggplot2::element_blank())
  }

  (p1 | p4 | p6) / (p2 | p3 | p5) +
    patchwork::plot_annotation(
      title = title,
      caption = ghs_caption_bi(),
      theme = ggplot2::theme(plot.title = ggplot2::element_text(
        face = "bold", size = 16, margin = ggplot2::margin(b = 4)
      ))
    )
}


plot_slope_chart <- function(master,
                              value_col = "hf3_che",
                              year_a = 2000, year_b = 2023,
                              top_n = 20,
                              title = NULL,
                              subtitle = NULL) {
  ensure_pkgs(c("dplyr", "tidyr", "ggrepel", "ggplot2", "scales"))
  val <- rlang::sym(value_col)
  d <- master |>
    dplyr::filter(.data$year %in% c(year_a, year_b),
                  is.finite(!!val)) |>
    dplyr::select("iso3_code", "country_name", "continent", "year", value = !!val) |>
    tidyr::pivot_wider(names_from = "year", values_from = "value",
                       names_prefix = "y") |>
    tidyr::drop_na() |>
    dplyr::mutate(delta = abs(!!rlang::sym(paste0("y", year_b)) -
                              !!rlang::sym(paste0("y", year_a)))) |>
    dplyr::slice_max(.data$delta, n = top_n)
  d_long <- d |>
    tidyr::pivot_longer(c(!!rlang::sym(paste0("y", year_a)),
                          !!rlang::sym(paste0("y", year_b))),
                        names_to = "year", values_to = "value") |>
    dplyr::mutate(year = as.integer(sub("y", "", .data$year)))
  title <- title %||% sprintf(
    "%d \u2192 %d: %s \u53d8\u5316\u6700\u5927\u7684 %d \u4e2a\u56fd\u5bb6",
    year_a, year_b, value_col, top_n)
  subtitle <- subtitle %||% sprintf("\u6309\u7edd\u5bf9\u53d8\u5316\u91cf\u6392\u5e8f")
  ggplot2::ggplot(d_long,
                  ggplot2::aes(.data$year, .data$value,
                                group = .data$iso3_code,
                                colour = .data$continent)) +
    ggplot2::geom_line(linewidth = 0.8, alpha = 0.85) +
    ggplot2::geom_point(size = 2.5) +
    ggrepel::geom_text_repel(
      data = d_long |> dplyr::filter(.data$year == year_b),
      ggplot2::aes(label = .data$country_name),
      hjust = 0, nudge_x = 0.35, size = 3,
      direction = "y", segment.size = 0.2
    ) +
    ggplot2::scale_x_continuous(breaks = c(year_a, year_b),
                                 limits = c(year_a - 1, year_b + 6)) +
    scale_colour_ghs_continent() +
    labs_ghs(title = title, subtitle = subtitle,
             x = NULL, y = value_col) +
    theme_ghs(grid = "y")
}

plot_bump_chart <- function(master, value_col = "hf3_che",
                            top_n = 15,
                            year_min = 2000, year_max = 2023,
                            title = NULL) {
  ensure_pkgs(c("dplyr", "ggplot2", "ggrepel"))
  val <- rlang::sym(value_col)
  pool <- master |>
    dplyr::filter(.data$year == year_max, is.finite(!!val)) |>
    dplyr::slice_max(!!val, n = top_n) |>
    dplyr::pull(.data$iso3_code)
  d <- master |>
    dplyr::filter(.data$iso3_code %in% pool,
                  .data$year >= year_min, .data$year <= year_max,
                  is.finite(!!val)) |>
    dplyr::group_by(.data$year) |>
    dplyr::mutate(rk = rank(-!!val, ties.method = "first")) |>
    dplyr::ungroup()
  title <- title %||% sprintf(
    "OOPS \u5360\u6bd4 %d-%d \u6392\u540d\u53d8\u5316 (Top %d) ",
    year_min, year_max, top_n)
  ggplot2::ggplot(d, ggplot2::aes(.data$year, .data$rk,
                                   colour = .data$country_name,
                                   group = .data$iso3_code)) +
    ggplot2::geom_line(linewidth = 1.1, alpha = 0.8) +
    ggplot2::geom_point(size = 2) +
    ggrepel::geom_text_repel(
      data = d |> dplyr::filter(.data$year == year_max),
      ggplot2::aes(label = .data$country_name),
      direction = "y", hjust = 0, nudge_x = 0.6,
      size = 3, segment.size = 0.2
    ) +
    ggplot2::scale_y_reverse(breaks = seq_len(top_n)) +
    ggplot2::scale_x_continuous(limits = c(year_min, year_max + 4),
                                 breaks = scales::pretty_breaks()) +
    labs_ghs(title = title,
             subtitle = "1 = \u6700\u9ad8 OOPS \u5360\u6bd4",
             x = NULL, y = "Rank") +
    theme_ghs(grid = "y") +
    ggplot2::theme(legend.position = "none")
}

plot_covid_dumbbell <- function(master,
                                 value_col = "hf3_che",
                                 base_year = 2019, shock_year = 2022,
                                 top_n = 25, by = "abs",
                                 title = NULL) {
  ensure_pkgs(c("dplyr", "tidyr", "ggplot2"))
  val <- rlang::sym(value_col)
  d <- master |>
    dplyr::filter(.data$year %in% c(base_year, shock_year),
                  is.finite(!!val)) |>
    dplyr::select("iso3_code", "country_name", "continent", "year",
                  value = !!val) |>
    tidyr::pivot_wider(names_from = "year", values_from = "value",
                       names_prefix = "y") |>
    tidyr::drop_na() |>
    dplyr::mutate(delta = !!rlang::sym(paste0("y", shock_year)) -
                          !!rlang::sym(paste0("y", base_year)),
                  abs_delta = abs(.data$delta))
  d <- if (by == "abs") d |> dplyr::slice_max(.data$abs_delta, n = top_n)
       else d |> dplyr::slice_max(.data$delta, n = top_n)
  d <- d |>
    dplyr::mutate(country_name = forcats::fct_reorder(.data$country_name,
                                                       .data$delta))
  title <- title %||% sprintf(
    "%d vs %d: %s \u53d8\u5316\u524d %d \u540d",
    base_year, shock_year, value_col, top_n)
  ggplot2::ggplot(d,
                  ggplot2::aes(y = .data$country_name)) +
    ggplot2::geom_segment(ggplot2::aes(
      x = !!rlang::sym(paste0("y", base_year)),
      xend = !!rlang::sym(paste0("y", shock_year)),
      yend = .data$country_name,
      colour = .data$delta > 0
    ), linewidth = 1) +
    ggplot2::geom_point(ggplot2::aes(x = !!rlang::sym(paste0("y", base_year))),
                        colour = "#1B5E88", size = 2.5) +
    ggplot2::geom_point(ggplot2::aes(x = !!rlang::sym(paste0("y", shock_year))),
                        colour = "#C0504D", size = 2.5) +
    ggplot2::scale_colour_manual(values = c(`TRUE` = "#C0504D", `FALSE` = "#2E8B57"),
                                  labels = c(`TRUE` = "\u4e0a\u5347",
                                             `FALSE` = "\u4e0b\u964d")) +
    ggplot2::scale_x_continuous(labels = scales::label_percent(scale = 1)) +
    labs_ghs(title = title,
             subtitle = sprintf("\u84dd: %d \u00b7 \u7ea2: %d",
                                 base_year, shock_year),
             x = value_col, y = NULL,
             caption = ghs_caption_bi()) +
    theme_ghs(grid = "x") +
    ggplot2::guides(colour = ggplot2::guide_legend(title = NULL))
}

plot_stream_continent <- function(master,
                                   value_col = "che_pc_usd2023",
                                   title = NULL) {
  ensure_pkgs(c("dplyr", "ggplot2"))
  has_stream <- requireNamespace("ggstream", quietly = TRUE)
  val <- rlang::sym(value_col)
  d <- master |>
    dplyr::filter(!is.na(.data$continent), is.finite(!!val)) |>
    dplyr::group_by(.data$year, .data$continent) |>
    dplyr::summarise(value = stats::weighted.mean(!!val,
                                                    if ("pop" %in% names(master))
                                                      .data$pop else NULL,
                                                    na.rm = TRUE),
                     .groups = "drop")
  base <- ggplot2::ggplot(d, ggplot2::aes(.data$year, .data$value,
                                            fill = .data$continent))
  geom <- if (has_stream) {
    ggstream::geom_stream(alpha = 0.85, type = "ridge")
  } else {
    ggplot2::geom_area(alpha = 0.85, position = "stack")
  }
  base + geom +
    scale_fill_ghs_continent() +
    ggplot2::scale_y_continuous(labels = scales::label_dollar()) +
    labs_ghs(
      title = title %||% "\u5404\u5927\u6d32\u4eba\u5747 CHE \u6f14\u5316 (Stream)",
      subtitle = "\u5bbd\u5ea6 = \u4eba\u53e3\u52a0\u6743\u4eba\u5747\u503c",
      x = NULL, y = "Per-capita CHE (USD 2023)"
    ) +
    theme_ghs()
}


plot_ternary_schemes <- function(master, year_focus = 2022,
                                  title = NULL) {
  d <- master |>
    dplyr::filter(.data$year == year_focus,
                  is.finite(.data$hf1_che),
                  is.finite(.data$hf2_che),
                  is.finite(.data$hf3_che),
                  !is.na(.data$continent))
  if (!nrow(d)) return(NULL)
  if (!requireNamespace("ggtern", quietly = TRUE)) {
    total <- d$hf1_che + d$hf2_che + d$hf3_che
    d <- d[is.finite(total) & total > 0, , drop = FALSE]
    total <- d$hf1_che + d$hf2_che + d$hf3_che
    d$a <- d$hf1_che / total
    d$b <- d$hf2_che / total
    d$c <- d$hf3_che / total
    d$tx <- d$b + 0.5 * d$c
    d$ty <- d$c * sqrt(3) / 2
    tri <- data.frame(x = c(0, 1, 0.5, 0),
                      y = c(0, 0, sqrt(3) / 2, 0))
    label_data <- d |>
      dplyr::slice_max(.data$hf3_che, n = 10)
    label_layer <- if (requireNamespace("ggrepel", quietly = TRUE)) {
      ggrepel::geom_text_repel(
        data = label_data,
        ggplot2::aes(label = .data$iso3_code),
        size = 3, colour = "#0d121b", max.overlaps = 12
      )
    } else {
      ggplot2::geom_text(
        data = label_data,
        ggplot2::aes(label = .data$iso3_code),
        size = 3, colour = "#0d121b", vjust = -0.7
      )
    }
    ggplot2::ggplot(d, ggplot2::aes(.data$tx, .data$ty,
                                     colour = .data$continent)) +
      ggplot2::geom_path(data = tri, ggplot2::aes(.data$x, .data$y),
                         inherit.aes = FALSE, linewidth = 0.8,
                         colour = "#1d3f5f") +
      ggplot2::geom_point(size = 2.4, alpha = 0.82) +
      label_layer +
      ggplot2::annotate("text", x = -0.03, y = -0.03,
                        label = "HF1 政府", hjust = 0, size = 3.8,
                        colour = "#1d3f5f", fontface = "bold") +
      ggplot2::annotate("text", x = 1.03, y = -0.03,
                        label = "HF2 社保", hjust = 1, size = 3.8,
                        colour = "#1d3f5f", fontface = "bold") +
      ggplot2::annotate("text", x = 0.5, y = sqrt(3) / 2 + 0.04,
                        label = "HF3 OOPS", hjust = 0.5, size = 3.8,
                        colour = "#c46327", fontface = "bold") +
      scale_colour_ghs_continent() +
      ggplot2::coord_equal(xlim = c(-0.07, 1.07),
                           ylim = c(-0.07, sqrt(3) / 2 + 0.08),
                           clip = "off") +
      labs_ghs(
        title = title %||% sprintf("%d HF1 · HF2 · HF3 三角坐标投影", year_focus),
        subtitle = "无需 ggtern 的 barycentric fallback；点越靠上，OOPS 占比越高。",
        x = NULL, y = NULL
      ) +
      theme_ghs(grid = FALSE) +
      ggplot2::theme(axis.text = ggplot2::element_blank(),
                     axis.ticks = ggplot2::element_blank())
  } else {
  ggtern::ggtern(d, ggtern::aes(x = .data$hf1_che,
                                  y = .data$hf2_che,
                                  z = .data$hf3_che,
                                  colour = .data$continent)) +
    ggplot2::geom_point(size = 2.4, alpha = 0.8) +
    scale_colour_ghs_continent() +
    ggplot2::labs(
      title = title %||% sprintf("%d HF1 \u00b7 HF2 \u00b7 HF3 \u4e09\u5143\u5206\u5e03",
                                  year_focus),
      x = "HF1 \u653f\u5e9c\u8ba1\u5212",
      y = "HF2 \u793e\u4fdd",
      z = "HF3 OOPS",
      caption = ghs_caption_bi()
    )
  }
}

plot_oops_heatmap <- function(master, top_n = 60, title = NULL) {
  ensure_pkgs(c("dplyr", "ggplot2"))
  pool <- master |>
    dplyr::filter(.data$year == max(.data$year, na.rm = TRUE),
                  is.finite(.data$hf3_che)) |>
    dplyr::slice_max(.data$hf3_che, n = top_n) |>
    dplyr::pull(.data$iso3_code)
  d <- master |>
    dplyr::filter(.data$iso3_code %in% pool,
                  is.finite(.data$hf3_che)) |>
    dplyr::mutate(country_name = forcats::fct_reorder(.data$country_name,
                                                       .data$hf3_che,
                                                       .fun = stats::median,
                                                       na.rm = TRUE))
  ggplot2::ggplot(d, ggplot2::aes(.data$year, .data$country_name,
                                   fill = .data$hf3_che)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.1) +
    ggplot2::scale_fill_viridis_c(option = "magma", direction = -1,
                                   labels = scales::label_percent(scale = 1)) +
    labs_ghs(
      title = title %||% sprintf(
        "%d Top-%d \u9ad8 OOPS \u56fd\u5bb6 \u00b7 \u5e74\u5ea6\u70ed\u529b\u56fe",
        max(d$year, na.rm = TRUE), top_n),
      x = NULL, y = NULL
    ) +
    theme_ghs(grid = FALSE) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 8))
}

plot_pc_ridges_income <- function(master, year_focus = 2022, title = NULL) {
  ensure_pkgs(c("dplyr", "ggplot2", "ggridges", "scales"))
  d <- master |>
    dplyr::filter(.data$year == year_focus,
                  is.finite(.data$che_pc_usd2023),
                  !is.na(.data$income_group))
  ggplot2::ggplot(d,
                  ggplot2::aes(x = .data$che_pc_usd2023,
                                y = forcats::fct_relevel(
                                  .data$income_group,
                                  c("Low income", "Lower middle income",
                                    "Upper middle income", "High income")
                                ),
                                fill = .data$income_group)) +
    ggridges::geom_density_ridges(alpha = 0.78, colour = "white",
                                    scale = 1.1) +
    ggplot2::scale_x_log10(labels = scales::label_dollar()) +
    scale_fill_ghs_income() +
    labs_ghs(title = title %||%
               sprintf("%d \u4eba\u5747 CHE \u00b7 \u4e0d\u540c\u6536\u5165\u7ec4\u5206\u5e03 (\u5bf9\u6570\u8f74)",
                        year_focus),
             subtitle = "\u5bf9\u6570 x \u8f74 \u00b7 \u9876\u5cf0\u4f4d\u7f6e\u4e2d\u4f4d\u6570",
             x = "\u4eba\u5747 CHE (USD 2023, log)", y = NULL) +
    theme_ghs(grid = "x") +
    ggplot2::theme(legend.position = "none")
}

plot_che_treemap <- function(master, year_focus = 2022, title = NULL) {
  d <- master |>
    dplyr::filter(.data$year == year_focus,
                  is.finite(.data$che_usd2023),
                  !is.na(.data$continent))
  if (!nrow(d)) return(NULL)
  if (!requireNamespace("treemapify", quietly = TRUE)) {
    d <- d |>
      dplyr::slice_max(.data$che_usd2023, n = 30) |>
      dplyr::mutate(country_name = forcats::fct_reorder(.data$country_name,
                                                        .data$che_usd2023))
    return(
      ggplot2::ggplot(d, ggplot2::aes(.data$che_usd2023, .data$country_name,
                                      fill = .data$continent)) +
        ggplot2::geom_col(width = 0.72, alpha = 0.92) +
        ggplot2::geom_text(ggplot2::aes(label = scales::dollar(.data$che_usd2023,
                                                               scale = 1e-9,
                                                               suffix = "B")),
                           hjust = -0.08, size = 3.2, colour = "#0d121b") +
        ggplot2::scale_x_continuous(labels = scales::label_dollar(scale = 1e-9,
                                                                    suffix = "B"),
                                    expand = ggplot2::expansion(mult = c(0, 0.14))) +
        scale_fill_ghs_continent() +
        labs_ghs(
          title = title %||% sprintf("%d 各国 CHE 绝对规模 Top 30", year_focus),
          subtitle = "treemapify 不可用时自动降级为横向排名条形图；长度 ∝ 总 CHE (USD 2023)。",
          x = "总 CHE (十亿美元，USD2023)", y = NULL
        ) +
        theme_ghs(grid = "x")
    )
  }
  ggplot2::ggplot(d,
                  ggplot2::aes(area = .data$che_usd2023,
                                fill = .data$continent,
                                label = .data$country_name,
                                subgroup = .data$continent)) +
    treemapify::geom_treemap() +
    treemapify::geom_treemap_subgroup_border(colour = "white", linewidth = 1.5) +
    treemapify::geom_treemap_text(colour = "white", place = "centre",
                                    grow = FALSE, reflow = TRUE, size = 8) +
    scale_fill_ghs_continent() +
    labs_ghs(
      title = title %||%
        sprintf("%d \u5404\u56fd CHE \u7edd\u5bf9\u89c4\u6a21 \u00b7 Treemap", year_focus),
      subtitle = "\u9762\u79ef \u221d \u603b CHE (USD 2023)",
      x = NULL, y = NULL
    )
}

plot_income_oops_lollipop <- function(master, year_focus = 2022, title = NULL) {
  ensure_pkgs(c("dplyr", "ggplot2"))
  d <- master |>
    dplyr::filter(.data$year == year_focus,
                  is.finite(.data$hf3_che),
                  !is.na(.data$income_group)) |>
    dplyr::group_by(.data$income_group) |>
    dplyr::summarise(median_oops = stats::median(.data$hf3_che, na.rm = TRUE),
                     q1 = stats::quantile(.data$hf3_che, 0.25, na.rm = TRUE),
                     q3 = stats::quantile(.data$hf3_che, 0.75, na.rm = TRUE),
                     .groups = "drop") |>
    dplyr::mutate(income_group = forcats::fct_relevel(
      .data$income_group,
      c("Low income", "Lower middle income",
        "Upper middle income", "High income")
    ))
  ggplot2::ggplot(d, ggplot2::aes(.data$median_oops, .data$income_group,
                                   colour = .data$income_group)) +
    ggplot2::geom_segment(ggplot2::aes(x = .data$q1, xend = .data$q3,
                                         yend = .data$income_group),
                          linewidth = 4, alpha = 0.45) +
    ggplot2::geom_point(size = 5) +
    scale_colour_ghs_income() +
    ggplot2::scale_x_continuous(labels = scales::label_percent(scale = 1)) +
    labs_ghs(
      title = title %||% sprintf(
        "%d \u4e0d\u540c\u6536\u5165\u7ec4 OOPS \u4e2d\u4f4d + IQR",
        year_focus),
      x = "OOPS \u5360\u6bd4 (% of CHE)", y = NULL
    ) +
    theme_ghs(grid = "x") +
    ggplot2::theme(legend.position = "none")
}

plot_ext_density <- function(master, year_focus = 2022, title = NULL) {
  ensure_pkgs(c("dplyr", "ggplot2"))
  d <- master |>
    dplyr::filter(.data$year == year_focus, is.finite(.data$ext_che),
                  is.finite(.data$hf3_che)) |>
    dplyr::mutate(group = dplyr::case_when(
      .data$ext_che > 20 ~ "\u9ad8\u5916\u63f4 (>20%)",
      .data$ext_che < 1  ~ "\u4f4e\u5916\u63f4 (<1%)",
      TRUE              ~ "\u4e2d\u95f4 (1-20%)"
    ))
  ggplot2::ggplot(d, ggplot2::aes(.data$hf3_che, fill = .data$group)) +
    ggplot2::geom_density(alpha = 0.45, colour = NA) +
    ggplot2::scale_fill_manual(values = c(
      "\u9ad8\u5916\u63f4 (>20%)" = "#C0504D",
      "\u4f4e\u5916\u63f4 (<1%)" = "#1B5E88",
      "\u4e2d\u95f4 (1-20%)" = "#7A9C7A"
    )) +
    ggplot2::scale_x_continuous(labels = scales::label_percent(scale = 1)) +
    labs_ghs(
      title = title %||% sprintf(
        "%d \u9ad8/\u4e2d/\u4f4e\u5916\u63f4\u4f9d\u8d56\u56fd OOPS \u5206\u5e03",
        year_focus),
      x = "OOPS \u5360\u6bd4 (% of CHE)", y = "\u5bc6\u5ea6"
    ) +
    theme_ghs()
}


plot_continent_radar <- function(master, year_focus = 2022, title = NULL) {
  ensure_pkgs(c("dplyr", "tidyr", "ggplot2"))
  d <- master |>
    dplyr::filter(.data$year == year_focus, !is.na(.data$continent)) |>
    dplyr::group_by(.data$continent) |>
    dplyr::summarise(
      gghed = mean(.data$gghed_che, na.rm = TRUE),
      pvtd  = mean(.data$pvtd_che,  na.rm = TRUE),
      hf3   = mean(.data$hf3_che,   na.rm = TRUE),
      ext   = mean(.data$ext_che,   na.rm = TRUE),
      hc6   = mean(.data$hc6_che,   na.rm = TRUE),
      .groups = "drop"
    ) |>
    tidyr::pivot_longer(c("gghed", "pvtd", "hf3", "ext", "hc6"),
                        names_to = "metric", values_to = "value")
  ggplot2::ggplot(d, ggplot2::aes(.data$metric, .data$value,
                                   fill = .data$continent)) +
    ggplot2::geom_col(width = 0.85, position = ggplot2::position_dodge2(width = 0.9)) +
    ggplot2::coord_polar(theta = "x") +
    scale_fill_ghs_continent() +
    ggplot2::scale_y_continuous(labels = scales::label_percent(scale = 1)) +
    labs_ghs(
      title = title %||% sprintf("%d \u5404\u5927\u6d32\u5747\u503c\u96f7\u8fbe\u5bf9\u6bd4", year_focus),
      x = NULL, y = "%"
    ) +
    theme_ghs(grid = "y")
}

plot_small_multiples <- function(master,
                                  isos = c("USA", "CHN", "DEU", "JPN", "GBR",
                                           "IND", "BRA", "ZAF", "NGA", "RUS"),
                                  title = NULL) {
  ensure_pkgs(c("dplyr", "tidyr", "ggplot2"))
  d <- master |>
    dplyr::filter(.data$iso3_code %in% isos) |>
    dplyr::select("country_name", "year",
                  gghed = "gghed_che", pvtd = "pvtd_che", ext = "ext_che") |>
    tidyr::pivot_longer(c("gghed", "pvtd", "ext"),
                        names_to = "src", values_to = "pct") |>
    dplyr::mutate(src = factor(.data$src, levels = c("gghed", "pvtd", "ext")))
  ggplot2::ggplot(d, ggplot2::aes(.data$year, .data$pct, fill = .data$src)) +
    ggplot2::geom_area(alpha = 0.85, position = "stack") +
    ggplot2::facet_wrap(~ .data$country_name, ncol = 5) +
    scale_fill_ghs_source(
      labels = c(gghed = "\u653f\u5e9c", pvtd = "\u79c1\u4eba", ext = "\u5916\u63f4")
    ) +
    ggplot2::scale_y_continuous(labels = scales::label_percent(scale = 1)) +
    labs_ghs(
      title = title %||% "10 \u56fd\u4e09\u6e90\u5360\u6bd4\u5c0f\u591a\u9762",
      subtitle = "\u8c03\u67e5\u4e0d\u540c\u56fd\u5bb6\u7684\u7ed3\u6784\u8f68\u8ff9",
      x = NULL, y = "% of CHE"
    ) +
    theme_ghs(grid = "y")
}


plot_inequality_timeseries <- function(master, title = NULL) {
  ensure_pkgs(c("dplyr", "tidyr", "ggplot2"))
  ineq <- inequality_by_year(master)
  d <- ineq |>
    tidyr::pivot_longer(c("gini_eq", "gini_pop", "atk05", "atk1", "theil_pop"),
                        names_to = "metric", values_to = "value") |>
    dplyr::filter(is.finite(.data$value))
  ggplot2::ggplot(d, ggplot2::aes(.data$year, .data$value,
                                   colour = .data$metric)) +
    ggplot2::geom_line(linewidth = 1.2) +
    ggplot2::scale_colour_viridis_d(option = "D", end = 0.85) +
    labs_ghs(
      title = title %||% "\u5168\u7403\u4eba\u5747 CHE \u4e0d\u5e73\u7b49\u6307\u6570",
      subtitle = "Gini / Atkinson(0.5,1) / Theil-T \u591a\u6307\u6807",
      x = NULL, y = "Index"
    ) +
    theme_ghs()
}

plot_beta_convergence_scatter <- function(beta_obj, title = NULL) {
  if (is.null(beta_obj)) return(NULL)
  ensure_pkgs(c("ggplot2", "ggrepel"))
  d <- beta_obj$panel
  ggplot2::ggplot(d, ggplot2::aes(.data$log_start, .data$growth,
                                   colour = .data$continent)) +
    ggplot2::geom_smooth(method = "lm", se = TRUE,
                         colour = "#1B5E88", linewidth = 0.6,
                         alpha = 0.18, fill = "#1B5E88") +
    ggplot2::geom_point(size = 2.5, alpha = 0.85) +
    ggrepel::geom_text_repel(
      data = d |> dplyr::slice_max(abs(.data$growth), n = 8),
      ggplot2::aes(label = .data$iso3_code),
      size = 3, segment.size = 0.2
    ) +
    scale_colour_ghs_continent() +
    labs_ghs(
      title = title %||% sprintf("\u03b2-\u6536\u655b\u6563\u70b9 (\u03b2 = %.4f)",
                                   beta_obj$beta %||% NA_real_),
      subtitle = "\u8d1f\u659c\u7387 = \u7a77\u56fd\u8feb\u8ff9 \u00b7 \u62df\u5408\u9634\u5f71 = 95% CI",
      x = "log(\u8d77\u59cb\u4eba\u5747 CHE)", y = "\u5e74\u5747\u589e\u901f"
    ) +
    theme_ghs()
}

plot_pca_biplot <- function(pca_obj, cluster_obj = NULL, title = NULL) {
  if (is.null(pca_obj)) return(NULL)
  ensure_pkgs(c("ggplot2", "ggrepel"))
  scores <- pca_obj$scores
  if (!is.null(cluster_obj)) {
    scores$cluster <- cluster_obj$scores$cluster
  } else {
    scores$cluster <- factor("all")
  }
  loadings <- pca_obj$loadings
  loadings$x_end <- loadings$PC1 * max(abs(scores$PC1)) * 0.85
  loadings$y_end <- loadings$PC2 * max(abs(scores$PC2)) * 0.85
  ggplot2::ggplot(scores, ggplot2::aes(.data$PC1, .data$PC2)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey80", linewidth = 0.3) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey80", linewidth = 0.3) +
    ggplot2::geom_point(ggplot2::aes(colour = .data$cluster), size = 2.5,
                        alpha = 0.8) +
    ggplot2::geom_segment(
      data = loadings,
      ggplot2::aes(x = 0, y = 0, xend = .data$x_end, yend = .data$y_end),
      arrow = ggplot2::arrow(length = ggplot2::unit(0.18, "cm")),
      colour = "#C0504D"
    ) +
    ggrepel::geom_text_repel(
      data = loadings,
      ggplot2::aes(x = .data$x_end, y = .data$y_end, label = .data$variable),
      colour = "#C0504D", fontface = "bold", size = 3.5
    ) +
    ggrepel::geom_text_repel(
      data = scores |>
        dplyr::group_by(.data$cluster) |>
        dplyr::slice_max(abs(.data$PC1), n = 3) |>
        dplyr::ungroup(),
      ggplot2::aes(label = .data$iso3_code), size = 3, alpha = 0.85
    ) +
    ggplot2::scale_colour_viridis_d(option = "D", end = 0.85) +
    labs_ghs(
      title = title %||% "PCA \u53cc\u6a99\u56fe + K-means \u805a\u7c7b",
      subtitle = sprintf("\u8d21\u732e\u7387: PC1 = %.1f%% \u00b7 PC2 = %.1f%%",
                         pca_obj$var_explained[1] * 100,
                         pca_obj$var_explained[2] * 100),
      x = "PC1", y = "PC2"
    ) +
    theme_ghs()
}

plot_forecast_fan <- function(master, isos = c("CHN", "USA", "IND", "BRA"),
                               value_col = "che_pc_usd2023",
                               h = 5, title = NULL) {
  ensure_pkgs(c("dplyr", "tidyr", "ggplot2"))
  d_list <- lapply(isos, function(iso) {
    di <- master |>
      dplyr::filter(.data$iso3_code == iso) |>
      dplyr::arrange(.data$year)
    fc <- fit_forecast(di[[value_col]], di$year, h = h)
    if (is.null(fc)) return(NULL)
    hist <- tibble::tibble(
      year = di$year, point = di[[value_col]],
      lo_80 = NA_real_, hi_80 = NA_real_,
      lo_95 = NA_real_, hi_95 = NA_real_,
      type = "history",
      iso3_code = iso, country_name = di$country_name[1]
    )
    fc$type <- "forecast"
    fc$iso3_code <- iso
    fc$country_name <- di$country_name[1]
    dplyr::bind_rows(hist, fc[, c("year", "point", "lo_80", "hi_80",
                                    "lo_95", "hi_95", "type",
                                    "iso3_code", "country_name")])
  })
  d <- dplyr::bind_rows(d_list)
  if (nrow(d) == 0) return(NULL)
  ggplot2::ggplot(d, ggplot2::aes(.data$year, .data$point)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lo_95, ymax = .data$hi_95),
                          fill = "#C0504D", alpha = 0.18,
                          data = d |> dplyr::filter(.data$type == "forecast")) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lo_80, ymax = .data$hi_80),
                          fill = "#C0504D", alpha = 0.30,
                          data = d |> dplyr::filter(.data$type == "forecast")) +
    ggplot2::geom_line(ggplot2::aes(linetype = .data$type), linewidth = 1.1,
                        colour = "#1B5E88") +
    ggplot2::scale_linetype_manual(values = c(history = "solid",
                                                forecast = "dashed")) +
    ggplot2::facet_wrap(~ .data$country_name, scales = "free_y") +
    labs_ghs(
      title = title %||% sprintf("ARIMA \u9884\u6d4b\u6247\u5f62\u56fe (h=%d)", h),
      subtitle = "\u5b9e\u7ebf: \u5386\u53f2 \u00b7 \u865a\u7ebf: \u9884\u6d4b \u00b7 \u9634\u5f71: 80% / 95% CI",
      x = NULL, y = value_col
    ) +
    theme_ghs()
}

plot_sankey_static <- function(master, year_focus = 2022, title = NULL) {
  if (!requireNamespace("ggalluvial", quietly = TRUE)) {
    warning("ggalluvial not installed; returning placeholder")
    return(ggplot2::ggplot() +
             ggplot2::annotate("text", x = 1, y = 1,
                               label = "ggalluvial not installed") +
             theme_ghs())
  }
  ensure_pkgs(c("dplyr", "tidyr"))
  src_lbl_gov <- "\u653f\u5e9c GGHE-D"
  src_lbl_pvt <- "\u79c1\u4eba PVT-D"
  src_lbl_ext <- "\u5916\u63f4 EXT"
  hf1_lbl <- "HF1 \u653f\u5e9c\u8ba1\u5212"
  hf2_lbl <- "HF2 \u793e\u4fdd"
  hf3_lbl <- "HF3 OOPS"
  hf4_lbl <- "HF4 \u81ea\u613f"

  src <- master |>
    dplyr::filter(.data$year == year_focus,
                  !is.na(.data$continent)) |>
    dplyr::summarise(
      gov = mean(.data$gghed_che, na.rm = TRUE),
      pvt = mean(.data$pvtd_che, na.rm = TRUE),
      ext = mean(.data$ext_che,  na.rm = TRUE)
    ) |>
    tidyr::pivot_longer(everything(),
                         names_to = "src_id", values_to = "share") |>
    dplyr::mutate(source = dplyr::case_when(
      .data$src_id == "gov" ~ src_lbl_gov,
      .data$src_id == "pvt" ~ src_lbl_pvt,
      .data$src_id == "ext" ~ src_lbl_ext
    ))

  flows <- tibble::tribble(
    ~source,     ~scheme,  ~weight,
    src_lbl_gov, hf1_lbl,  0.75,
    src_lbl_gov, hf2_lbl,  0.25,
    src_lbl_pvt, hf2_lbl,  0.20,
    src_lbl_pvt, hf3_lbl,  0.65,
    src_lbl_pvt, hf4_lbl,  0.15,
    src_lbl_ext, hf1_lbl,  0.40,
    src_lbl_ext, hf4_lbl,  0.60
  )
  d <- dplyr::left_join(flows, src, by = "source") |>
    dplyr::mutate(value = .data$share * .data$weight)

  ggplot2::ggplot(d, ggplot2::aes(axis1 = .data$source,
                                    axis2 = .data$scheme,
                                    y = .data$value)) +
    ggalluvial::geom_alluvium(ggplot2::aes(fill = .data$source),
                               alpha = 0.75, curve_type = "sigmoid") +
    ggalluvial::geom_stratum(width = 0.18, fill = "white", colour = "grey60") +
    ggplot2::geom_text(stat = ggalluvial::StatStratum,
                       ggplot2::aes(label = ggplot2::after_stat(.data$stratum)),
                       size = 3.4, fontface = "bold") +
    ggplot2::scale_x_discrete(limits = c("\u6765\u6e90", "\u7b79\u8d44\u65b9\u6848"),
                               expand = c(0.05, 0.05)) +
    scale_fill_ghs_source() +
    labs_ghs(title = title %||% sprintf(
        "%d \u7b79\u8d44\u6d41\u5411\u793a\u610f \u00b7 Sankey", year_focus),
              subtitle = "\u4ee5\u5404\u6d32\u5747\u503c\u4e3a\u57fa  \u00b7  \u542f\u53d1\u6027\u6620\u5c04\uff08\u975e\u7cbe\u786e\uff09",
              x = NULL, y = "% of CHE") +
    theme_ghs(grid = FALSE) +
    ggplot2::theme(legend.position = "none",
                    axis.text.y = ggplot2::element_blank(),
                    axis.ticks.y = ggplot2::element_blank())
}

plot_waffle_purpose <- function(master, year_focus = 2022,
                                  isos = c("USA", "DEU", "CHN", "BRA",
                                            "IND", "ZAF"),
                                  title = NULL) {
  ensure_pkgs(c("dplyr"))
  d <- master |>
    dplyr::filter(.data$iso3_code %in% isos,
                  .data$year == year_focus,
                  is.finite(.data$hc1_che),
                  is.finite(.data$hc6_che))
  if (nrow(d) == 0) return(NULL)
  d <- d |>
    dplyr::mutate(
      curative   = round(.data$hc1_che),
      preventive = round(.data$hc6_che),
      other      = pmax(0L, 100L - .data$curative - .data$preventive)
    ) |>
    dplyr::select("country_name", "curative", "preventive", "other") |>
    tidyr::pivot_longer(c("curative", "preventive", "other"),
                         names_to = "purpose",
                         values_to = "value")
  if (!requireNamespace("waffle", quietly = TRUE)) {
    wide <- tidyr::pivot_wider(d, names_from = "purpose", values_from = "value")
    tiles <- do.call(rbind, lapply(seq_len(nrow(wide)), function(i) {
      row <- wide[i, , drop = FALSE]
      purpose <- c(rep("curative", row$curative),
                   rep("preventive", row$preventive),
                   rep("other", row$other))
      purpose <- purpose[seq_len(min(100, length(purpose)))]
      if (length(purpose) < 100) purpose <- c(purpose, rep("other", 100 - length(purpose)))
      data.frame(country_name = row$country_name,
                 tile = seq_len(100),
                 x = ((seq_len(100) - 1) %% 10) + 1,
                 y = 10 - ((seq_len(100) - 1) %/% 10),
                 purpose = purpose)
    }))
    return(
      ggplot2::ggplot(tiles, ggplot2::aes(.data$x, .data$y,
                                          fill = .data$purpose)) +
        ggplot2::geom_tile(width = 0.88, height = 0.88, colour = "white",
                           linewidth = 0.2) +
        ggplot2::facet_wrap(~ .data$country_name, ncol = 3) +
        ggplot2::scale_fill_manual(
          values = c(curative = "#1B5E88", preventive = "#2E8B57",
                     other = "#d7d1c6"),
          labels = c(curative = "治疗 hc1",
                     preventive = "预防 hc6",
                     other = "其他")
        ) +
        ggplot2::coord_equal() +
        labs_ghs(title = title %||% sprintf(
          "%d 治疗 vs 预防 · 100 格矩阵", year_focus),
          subtitle = "每格 = 1% of CHE；无需 waffle 包的稳定 fallback。",
          x = NULL, y = NULL) +
        theme_ghs(grid = FALSE) +
        ggplot2::theme(axis.text = ggplot2::element_blank(),
                       axis.ticks = ggplot2::element_blank())
    )
  }
  ggplot2::ggplot(d,
                  ggplot2::aes(fill = .data$purpose, values = .data$value)) +
    waffle::geom_waffle(n_rows = 5, size = 0.4,
                          colour = "white", flip = FALSE) +
    ggplot2::facet_wrap(~ .data$country_name) +
    ggplot2::scale_fill_manual(
      values = c(curative = "#1B5E88", preventive = "#2E8B57",
                  other    = "#cccccc"),
      labels = c(curative   = "\u6cbb\u7597 hc1",
                  preventive = "\u9884\u9632 hc6",
                  other      = "\u5176\u4ed6")
    ) +
    ggplot2::coord_equal() +
    labs_ghs(title = title %||% sprintf(
              "%d \u6cbb\u7597 vs \u9884\u9632 \u00b7 Waffle", year_focus),
              subtitle = "\u6bcf\u4e00\u683c = 1% of CHE",
              x = NULL, y = NULL) +
    theme_ghs(grid = FALSE) +
    ggplot2::theme(axis.text = ggplot2::element_blank(),
                    axis.ticks = ggplot2::element_blank())
}

animate_oops_map <- function(master, world_sf,
                                years = NULL,
                                fps = 4, width = 1100, height = 600,
                                out_path = file.path(
                                  proj_root(), "分析输出", "图表",
                                  "26_anim_oops_map.gif")) {
  if (!requireNamespace("gganimate", quietly = TRUE)) {
    warning("gganimate not installed; skipping")
    return(invisible(NULL))
  }
  ensure_pkgs(c("dplyr", "ggplot2", "sf"))
  if (is.null(years)) years <- seq(2000, max(master$year, na.rm = TRUE), by = 1)
  d <- master |>
    dplyr::filter(.data$year %in% years) |>
    dplyr::select("iso3_code", "year", val = "hf3_che")
  map_df <- world_sf |>
    dplyr::left_join(d, by = "iso3_code")
  p <- ggplot2::ggplot(map_df) +
    ggplot2::geom_sf(ggplot2::aes(fill = .data$val), colour = "white",
                     linewidth = 0.05) +
    ggplot2::scale_fill_viridis_c(option = "magma", direction = -1,
                                    na.value = "grey85",
                                    labels = scales::label_percent(scale = 1)) +
    ggplot2::coord_sf(crs = "+proj=robin") +
    labs_ghs(title = "OOPS \u4e16\u754c\u5730\u56fe \u00b7 {round(frame_time)}",
              subtitle = "\u989c\u8272\u8d8a\u6df1 \u00b7 \u5c45\u6c11\u81ea\u4ed8\u8d1f\u62c5\u8d8a\u91cd",
              x = NULL, y = NULL) +
    theme_ghs(grid = FALSE) +
    ggplot2::theme(axis.text = ggplot2::element_blank(),
                    panel.grid = ggplot2::element_blank()) +
    gganimate::transition_time(.data$year)
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  gganimate::anim_save(out_path, animation = p,
                       fps = fps, width = width, height = height,
                       renderer = if (requireNamespace("gifski", quietly = TRUE))
                                     gganimate::gifski_renderer()
                                  else gganimate::magick_renderer())
  logi("animation saved: ", out_path)
  invisible(out_path)
}

plot_bivariate_map <- function(master, world_sf,
                                year_focus = 2022,
                                title = NULL) {
  ensure_pkgs(c("dplyr", "ggplot2", "sf"))
  d <- master |>
    dplyr::filter(.data$year == year_focus,
                  is.finite(.data$hf3_che),
                  is.finite(.data$gghed_che)) |>
    dplyr::mutate(
      x_bin = cut(.data$hf3_che, breaks = stats::quantile(.data$hf3_che,
                                                            c(0, 1/3, 2/3, 1),
                                                            na.rm = TRUE),
                  labels = 1:3, include.lowest = TRUE),
      y_bin = cut(.data$gghed_che, breaks = stats::quantile(.data$gghed_che,
                                                              c(0, 1/3, 2/3, 1),
                                                              na.rm = TRUE),
                  labels = 1:3, include.lowest = TRUE),
      bivar = paste0(.data$x_bin, "-", .data$y_bin)
    ) |>
    dplyr::select("iso3_code", "bivar")
  bivar_palette <- c(
    "1-1" = "#e8e8e8", "2-1" = "#e4acac", "3-1" = "#c85a5a",
    "1-2" = "#b0d5df", "2-2" = "#ad9ea5", "3-2" = "#985356",
    "1-3" = "#64acbe", "2-3" = "#627f8c", "3-3" = "#574249"
  )
  map_df <- world_sf |>
    dplyr::left_join(d, by = "iso3_code")
  ggplot2::ggplot(map_df) +
    ggplot2::geom_sf(ggplot2::aes(fill = .data$bivar),
                     colour = "white", linewidth = 0.1) +
    ggplot2::scale_fill_manual(values = bivar_palette,
                                na.value = "grey85") +
    ggplot2::coord_sf(crs = "+proj=robin") +
    labs_ghs(
      title = title %||% sprintf("%d \u53cc\u53d8\u91cf\u5730\u56fe (OOPS \u00d7 GGHE-D)",
                                  year_focus),
      subtitle = "\u989c\u8272\u8d8a\u6df1 \u00b7 OOPS \u4e0e\u653f\u5e9c\u5360\u6bd4\u90fd\u8d8a\u9ad8",
      x = NULL, y = NULL
    ) +
    theme_ghs(grid = FALSE) +
    ggplot2::theme(axis.text = ggplot2::element_blank(),
                    panel.grid = ggplot2::element_blank(),
                    legend.position = "right")
}
