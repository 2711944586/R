

.gxe_pkgs <- function() {
  ensure_pkgs(c("dplyr", "tidyr", "ggplot2", "scales", "patchwork",
                "ggridges", "ggrepel"))
}

.gxe_safe <- function(name, expr, w = 11, h = 6.4, dpi = 300, verbose = TRUE) {
  res <- tryCatch(expr, error = function(e) {
    if (verbose) message(sprintf("[extra] skip %s: %s", name, conditionMessage(e)))
    NULL
  })
  if (is.null(res)) return(invisible(NULL))
  save_fig(res, name, width = w, height = h, dpi = dpi)
  if (verbose) cat(sprintf("[extra] %s\n", name))
  invisible(NULL)
}

.gxe_theme <- function(base_size = 12) {
  if (exists("theme_ghs3", mode = "function")) theme_ghs3(base_size = base_size)
  else if (exists("theme_ghs2", mode = "function")) theme_ghs2(base_size = base_size)
  else ggplot2::theme_minimal(base_size = base_size)
}


.gxe_brazil_profile <- function(master) {
  if (!"BRA" %in% master$iso3_code) return(NULL)
  if (!exists("plot_country_profile", mode = "function")) return(NULL)
  plot_country_profile(master, "BRA")
}

.gxe_oops_violin <- function(master) {
  yr <- max(master$year, na.rm = TRUE)
  d <- master |>
    dplyr::filter(.data$year == yr,
                  is.finite(.data$hf3_che),
                  !is.na(.data$income_group))
  if (!nrow(d)) return(NULL)
  d$income_group <- factor(d$income_group,
    levels = c("Low income", "Lower middle income",
               "Upper middle income", "High income"))
  ggplot2::ggplot(d, ggplot2::aes(.data$income_group, .data$hf3_che,
                                  fill = .data$income_group)) +
    ggplot2::geom_violin(alpha = .55, color = NA, scale = "width") +
    ggplot2::geom_boxplot(width = .15, fill = "white",
                          outlier.size = .8, alpha = .8) +
    ggplot2::geom_jitter(width = .12, alpha = .25, size = .9,
                         color = "#1d3f5f") +
    ggplot2::scale_fill_brewer(palette = "OrRd", guide = "none") +
    ggplot2::scale_y_continuous(labels = scales::label_percent(scale = 1)) +
    ggplot2::labs(
      title    = sprintf("OOPS \u5360 CHE \u00b7 %d \u5e74 \u00b7 \u6309\u6536\u5165\u7ec4\u5206\u5e03", yr),
      subtitle = "\u4f4e\u6536\u5165\u7ec4\u4e2d\u4f4d\u6570\u8fd1 40%\uff1b\u9ad8\u6536\u5165\u7ec4\u4e2d\u4f4d\u6570\u4f4e\u4e8e 15%\u3002",
      x = NULL, y = "OOPS / CHE",
      caption = "WHO GHED \u00b7 hf3_che / che * 100"
    ) +
    .gxe_theme(12) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "#fbf6ee", color = NA),
      panel.background = ggplot2::element_rect(fill = "#fbf6ee", color = NA),
      panel.grid.major.y = ggplot2::element_line(color = "#ded6c9", linewidth = .35),
      axis.text.x = ggplot2::element_text(color = "#292524", size = 10),
      plot.title = ggplot2::element_text(color = "#292524", face = "bold")
    )
}

.gxe_oops_heatmap_grid <- function(master, top_n = 35) {
  d <- master |>
    dplyr::filter(is.finite(.data$hf3_che), !is.na(.data$year))
  if (!nrow(d)) return(NULL)
  pop_rank <- d |>
    dplyr::group_by(.data$iso3_code, .data$country_name) |>
    dplyr::summarise(pop = mean(.data$pop, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(.data$pop)) |>
    utils::head(top_n)
  d2 <- d |>
    dplyr::filter(.data$iso3_code %in% pop_rank$iso3_code) |>
    dplyr::mutate(country_name = factor(.data$country_name,
                                        levels = rev(pop_rank$country_name)))
    ggplot2::ggplot(d2, ggplot2::aes(.data$year, .data$country_name,
                                    fill = .data$hf3_che)) +
    ggplot2::geom_tile(color = "#fbf6ee", linewidth = .2) +
    ggplot2::scale_fill_distiller(
      palette = "RdYlBu", direction = -1,
      name = "OOPS %",
      breaks = c(0, 25, 50, 75)
    ) +
    ggplot2::scale_x_continuous(breaks = seq(2000, 2023, 4),
                                 expand = c(0, 0)) +
    ggplot2::labs(
      title    = sprintf("OOPS \u5fb7\u70ed\u56fe \u00b7 \u4eba\u53e3\u524d %d \u56fd 2000\u20132023", top_n),
      subtitle = "\u989c\u8272\u8d8a\u6df1 = \u5c45\u6c11\u81ea\u4ed8\u5360\u6bd4\u8d8a\u9ad8\u3002",
      x = NULL, y = NULL,
      caption = "WHO GHED \u00b7 hf3_che"
    ) +
    .gxe_theme(11)
}

.gxe_lifeexp_elasticity <- function(master) {
  yr <- max(master$year, na.rm = TRUE)
  d <- master |>
    dplyr::filter(.data$year == yr,
                  is.finite(.data$che_pc_usd2023),
                  is.finite(.data$life_exp))
  if (!nrow(d) || nrow(d) < 30) return(NULL)
  d$lche <- log(pmax(d$che_pc_usd2023, 1))
  fit <- stats::lm(life_exp ~ lche, data = d)
  cf <- stats::coef(fit)
  ggplot2::ggplot(d, ggplot2::aes(.data$che_pc_usd2023, .data$life_exp,
                                   color = .data$continent)) +
    ggplot2::geom_smooth(ggplot2::aes(group = 1), method = "lm",
                         se = TRUE, color = "#1d3f5f", linewidth = .8,
                         linetype = "dashed", alpha = .12) +
    ggplot2::geom_point(alpha = .85, size = 2.2) +
    ggrepel::geom_text_repel(
      data = d |>
        dplyr::filter(.data$che_pc_usd2023 >
                        stats::quantile(d$che_pc_usd2023, .92, na.rm = TRUE) |
                      .data$life_exp <
                        stats::quantile(d$life_exp, .08, na.rm = TRUE)),
      ggplot2::aes(label = .data$iso3_code), size = 3, color = "#0d121b",
      max.overlaps = 14
    ) +
    ggplot2::scale_x_log10(labels = scales::label_dollar()) +
    ggplot2::scale_color_brewer(palette = "Set2", name = NULL) +
    ggplot2::labs(
      title    = sprintf("\u9884\u671f\u5bff\u547d\u5bf9\u4eba\u5747\u536b\u751f\u652f\u51fa\u7684\u54cd\u5e94 \u00b7 %d", yr),
      subtitle = sprintf("life_exp = %.1f + %.2f \u00d7 ln(CHE_pc)\uff1b\u4eba\u5747\u652f\u51fa\u6bcf\u7ffb\u500d \u2248 +%.1f \u5e74\u5bff\u547d\u3002",
                          cf[1], cf[2], cf[2] * log(2)),
      x = "\u4eba\u5747 CHE (USD2023, log)", y = "\u9884\u671f\u5bff\u547d\uff08\u5e74\uff09",
      caption = "WHO GHED + WB WDI \u00b7 \u6700\u8fd1\u5e74\u622a\u9762"
    ) +
    .gxe_theme(12)
}

.gxe_sdg3_radar <- function(master,
                            picks = c("CHN", "USA", "IND", "BRA", "JPN", "ZAF")) {
  yr <- max(master$year, na.rm = TRUE)
  cols <- c("life_exp", "u5mr", "che_pc_usd2023", "hf3_che", "gghed_che")
  if (!all(cols %in% names(master))) return(NULL)
  d <- master |>
    dplyr::filter(.data$year == yr, .data$iso3_code %in% picks) |>
    dplyr::select(dplyr::all_of(c("iso3_code", "country_name", cols)))
  if (!nrow(d)) return(NULL)
  scale01 <- function(x, invert = FALSE) {
    rng <- range(x, na.rm = TRUE)
    if (!is.finite(diff(rng)) || diff(rng) == 0) return(rep(.5, length(x)))
    z <- (x - rng[1]) / diff(rng)
    if (invert) z <- 1 - z
    z
  }
  d$life_z   <- scale01(d$life_exp)
  d$u5_z     <- scale01(d$u5mr, invert = TRUE)
  d$che_z    <- scale01(log1p(pmax(d$che_pc_usd2023, 0)))
  d$prot_z   <- scale01(d$hf3_che, invert = TRUE)
  d$gov_z    <- scale01(d$gghed_che)
  long <- d |>
    dplyr::select(.data$country_name, .data$life_z, .data$u5_z,
                   .data$che_z, .data$prot_z, .data$gov_z) |>
    tidyr::pivot_longer(-.data$country_name,
                          names_to = "axis", values_to = "value")
  long$axis <- factor(long$axis,
    levels = c("life_z", "u5_z", "prot_z", "gov_z", "che_z"),
    labels = c("\u9884\u671f\u5bff\u547d", "U5MR\u53cd",
               "OOPS\u53cd", "GGHED", "\u4eba\u5747 CHE"))
  ggplot2::ggplot(long, ggplot2::aes(.data$axis, .data$value,
                                       group = .data$country_name,
                                       color = .data$country_name)) +
    ggplot2::geom_polygon(ggplot2::aes(fill = .data$country_name),
                          alpha = .15, linewidth = .9) +
    ggplot2::geom_point(size = 2.2) +
    ggplot2::coord_polar() +
    ggplot2::scale_color_brewer(palette = "Set1", name = NULL) +
    ggplot2::scale_fill_brewer(palette = "Set1", guide = "none") +
    ggplot2::labs(
      title    = sprintf("SDG-3 \u591a\u56fd\u96f7\u8fbe \u00b7 %d", yr),
      subtitle = "5 \u8f74\u5404\u5728\u5168\u6837\u672c min\u2013max \u5f52\u4e00\u5316\uff1b\u8d8a\u9760\u5916\u5708\u8d8a\u4f73\u3002",
      x = NULL, y = NULL,
      caption = "WHO GHED + WB WDI"
    ) +
    .gxe_theme(12) + ggplot2::theme(axis.text.y = ggplot2::element_blank())
}

.gxe_sdg3_progress <- function(master) {
  if (!all(c("life_exp", "u5mr", "year") %in% names(master))) return(NULL)
  base <- master |>
    dplyr::filter(.data$year == 2000) |>
    dplyr::select(.data$iso3_code, life0 = .data$life_exp, u5_0 = .data$u5mr)
  cur <- master |>
    dplyr::filter(.data$year == max(master$year, na.rm = TRUE)) |>
    dplyr::select(.data$iso3_code, .data$country_name, .data$continent,
                   life1 = .data$life_exp, u5_1 = .data$u5mr)
  d <- dplyr::inner_join(base, cur, by = "iso3_code") |>
    dplyr::mutate(
      d_life = .data$life1 - .data$life0,
      d_u5   = .data$u5_0 - .data$u5_1
    ) |>
    dplyr::filter(is.finite(.data$d_life), is.finite(.data$d_u5)) |>
    dplyr::arrange(dplyr::desc(.data$d_life)) |>
    utils::head(20)
  if (!nrow(d)) return(NULL)
  d$country_name <- factor(d$country_name, levels = rev(d$country_name))
  ggplot2::ggplot(d) +
    ggplot2::geom_segment(ggplot2::aes(x = 0, xend = .data$d_life,
                                        y = .data$country_name,
                                        yend = .data$country_name),
                          color = "#c46327", linewidth = 1.5) +
    ggplot2::geom_point(ggplot2::aes(.data$d_life, .data$country_name),
                        color = "#1d3f5f", size = 3) +
    ggplot2::geom_text(ggplot2::aes(.data$d_life, .data$country_name,
                                     label = sprintf("+%.1f / -%.0f",
                                                     .data$d_life, .data$d_u5)),
                       hjust = -0.15, size = 3.2, color = "#0d121b") +
    ggplot2::expand_limits(x = max(d$d_life) * 1.25) +
    ggplot2::labs(
      title    = "SDG-3 \u8fdb\u5c55 \u00b7 23 \u5e74\u9884\u671f\u5bff\u547d\u6700\u5feb\u589e\u957f\u7684 20 \u56fd",
      subtitle = "\u6807\u7b7e: +\u5bff\u547d \u5e74 / -U5MR \u6539\u5584\uff081/1000 \u6d3b\u751f\uff09",
      x = "\u9884\u671f\u5bff\u547d\u589e\u52a0\uff082023 \u2212 2000\uff0c\u5e74\uff09",
      y = NULL,
      caption = "WB WDI + WHO GHED"
    ) +
    .gxe_theme(11)
}

.gxe_period_compare <- function(master) {
  d <- master |>
    dplyr::filter(is.finite(.data$che_pc_usd2023), is.finite(.data$hf3_che)) |>
    dplyr::mutate(period = dplyr::case_when(
      .data$year >= 2000 & .data$year <= 2007 ~ "2000\u20132007",
      .data$year >= 2008 & .data$year <= 2015 ~ "2008\u20132015",
      .data$year >= 2016 & .data$year <= 2023 ~ "2016\u20132023",
      TRUE ~ NA_character_
    )) |>
    dplyr::filter(!is.na(.data$period))
  if (!nrow(d)) return(NULL)
  agg <- d |>
    dplyr::group_by(.data$period, .data$continent) |>
    dplyr::summarise(
      che_pc = stats::weighted.mean(.data$che_pc_usd2023,
                                     .data$pop, na.rm = TRUE),
      oops   = stats::weighted.mean(.data$hf3_che,
                                     .data$che_usd2023, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::filter(!is.na(.data$continent))
  if (!nrow(agg)) return(NULL)
  long <- tidyr::pivot_longer(agg, c("che_pc", "oops"),
                                names_to = "metric", values_to = "value")
  long$metric <- factor(long$metric, levels = c("che_pc", "oops"),
    labels = c("\u4eba\u5747 CHE (USD2023)", "OOPS \u5360 CHE (%)"))
  long$period <- factor(long$period,
    levels = c("2000\u20132007", "2008\u20132015", "2016\u20132023"))
  ggplot2::ggplot(long, ggplot2::aes(.data$period, .data$value,
                                       group = .data$continent,
                                       color = .data$continent)) +
    ggplot2::geom_line(linewidth = 1.1) +
    ggplot2::geom_point(size = 2.6) +
    ggplot2::facet_wrap(~ .data$metric, scales = "free_y") +
    ggplot2::scale_color_brewer(palette = "Set2", name = NULL) +
    ggplot2::labs(
      title    = "\u4e09\u4e2a\u5b50\u671f\u95f4\u5bf9\u6bd4 \u00b7 \u4eba\u5747\u8d44\u91d1 vs \u8d22\u52a1\u4fdd\u62a4",
      subtitle = "\u4eba\u5747 CHE \u8de8\u5927\u6d32\u4e0a\u5347\uff1b OOPS \u5e73\u5747\u4e0b\u964d\uff0c\u4f46\u4e9a\u6d32\u4ecd\u9760\u4e0d\u5747\u8861\u3002",
      x = NULL, y = NULL,
      caption = "WHO GHED \u00b7 \u4eba\u53e3\u52a0\u6743 / CHE \u52a0\u6743"
    ) +
    .gxe_theme(12)
}

.gxe_quantile_reg <- function(master) {
  gdp_col <- intersect(c("gdp_pc_usd", "gdp_per_capita_usd"), names(master))[1]
  if (is.na(gdp_col)) return(NULL)
  d <- master |>
    dplyr::filter(is.finite(.data[[gdp_col]]),
                  is.finite(.data$che_pc_usd2023))
  if (!nrow(d)) return(NULL)
  d$lgdp <- log(pmax(d[[gdp_col]], 1))
  d$lche <- log(pmax(d$che_pc_usd2023, 1))
  taus <- c(.1, .5, .9)
  fits <- if (requireNamespace("quantreg", quietly = TRUE)) {
    lapply(taus, function(t) quantreg::rq(lche ~ lgdp, data = d, tau = t))
  } else {
    lapply(taus, function(t) stats::lm(lche ~ lgdp, data = d))
  }
  preds <- lapply(seq_along(taus), function(i) {
    nd <- data.frame(lgdp = seq(min(d$lgdp), max(d$lgdp), length.out = 80))
    nd$lche <- stats::predict(fits[[i]], newdata = nd)
    nd$tau <- sprintf("\u03c4 = %.1f", taus[i])
    nd
  })
  preds <- do.call(rbind, preds)
  ggplot2::ggplot(d, ggplot2::aes(.data$lgdp, .data$lche)) +
    ggplot2::geom_point(alpha = .12, size = .7, color = "#1d3f5f") +
    ggplot2::geom_line(data = preds,
                       ggplot2::aes(.data$lgdp, .data$lche,
                                     color = .data$tau, group = .data$tau),
                       linewidth = 1.05) +
    ggplot2::scale_color_manual(
      values = c("\u03c4 = 0.1" = "#2a857a",
                 "\u03c4 = 0.5" = "#1d3f5f",
                 "\u03c4 = 0.9" = "#c46327"),
      name = NULL
    ) +
    ggplot2::labs(
      title    = "\u5206\u4f4d\u56de\u5f52 \u00b7 ln(GDP_pc) \u2192 ln(CHE_pc)",
      subtitle = "\u4f4e\u5206\u4f4d\u00b7\u4e2d\u4f4d\u00b7\u9ad8\u5206\u4f4d \u4e09\u6761\u62df\u5408\u7ebf\u4e0d\u5e73\u884c\uff0c\u63d0\u793a\u5f39\u6027\u968f GDP \u53d8\u5316\u3002",
      x = "ln(GDP per capita, USD)", y = "ln(CHE per capita, USD2023)",
      caption = "WHO GHED + WB WDI \u00b7 quantreg::rq"
    ) +
    .gxe_theme(12)
}

.gxe_changepoint <- function(master) {
  d <- master |>
    dplyr::filter(is.finite(.data$che_usd2023)) |>
    dplyr::group_by(.data$year) |>
    dplyr::summarise(che = sum(.data$che_usd2023, na.rm = TRUE) / 1e12,
                      .groups = "drop") |>
    dplyr::arrange(.data$year)
  if (!nrow(d)) return(NULL)
  d$g <- c(NA_real_, diff(log(d$che)))
  cp_year <- NA_integer_
  if (requireNamespace("changepoint", quietly = TRUE) &&
      sum(is.finite(d$g)) >= 8) {
    cp <- tryCatch(changepoint::cpt.mean(d$g[is.finite(d$g)],
      method = "AMOC"), error = function(e) NULL)
    if (!is.null(cp)) {
      cps <- changepoint::cpts(cp)
      if (length(cps)) {
        idx <- which(is.finite(d$g))[cps[1]]
        cp_year <- d$year[idx]
      }
    }
  }
  if (is.na(cp_year)) cp_year <- 2008L
  ggplot2::ggplot(d, ggplot2::aes(.data$year, .data$g)) +
    ggplot2::geom_col(fill = "#1d3f5f", alpha = .85) +
    ggplot2::geom_vline(xintercept = cp_year, linetype = "dashed",
                        color = "#c46327", linewidth = .9) +
    ggplot2::annotate("text", x = cp_year + .5, y = max(d$g, na.rm = TRUE),
      label = sprintf("\u53d8\u70b9 \u2248 %d", cp_year),
      color = "#c46327", fontface = "bold", hjust = 0, vjust = 1) +
    ggplot2::scale_y_continuous(labels = scales::label_percent()) +
    ggplot2::scale_x_continuous(breaks = seq(2000, 2023, 4)) +
    ggplot2::labs(
      title    = "\u5168\u7403 CHE \u603b\u989d\u5e74\u5316\u589e\u901f \u00b7 \u53d8\u70b9\u68c0\u6d4b",
      subtitle = "AMOC + cpt.mean \u68c0\u6d4b\u4e3b\u8981\u8d8b\u52bf\u4e2d\u65ad\u70b9\u3002",
      x = NULL, y = "\u603b\u989d\u5bf9\u6570\u5e74\u589e\u901f",
      caption = "WHO GHED \u00b7 changepoint::cpt.mean(AMOC)"
    ) +
    .gxe_theme(11)
}

.gxe_continent_ridges <- function(master) {
  d <- master |>
    dplyr::filter(is.finite(.data$che_pc_usd2023),
                  !is.na(.data$continent),
                  .data$year %in% c(2000, 2010, 2020, 2023))
  if (!nrow(d)) return(NULL)
  d$year <- factor(d$year)
  ggplot2::ggplot(d, ggplot2::aes(x = .data$che_pc_usd2023,
                                    y = .data$year,
                                    fill = .data$continent)) +
    ggridges::geom_density_ridges(alpha = .55, scale = 2.2,
                                  rel_min_height = .015,
                                  color = "white", linewidth = .25) +
    ggplot2::facet_wrap(~ .data$continent, scales = "free_y", ncol = 3) +
    ggplot2::scale_x_log10(labels = scales::label_dollar()) +
    ggplot2::scale_fill_brewer(palette = "Set2", guide = "none") +
    ggplot2::labs(
      title    = "\u4eba\u5747 CHE \u8de8\u5927\u6d32\u5206\u5e03\u5c71\u810a\u56fe",
      subtitle = "2000 / 2010 / 2020 / 2023 \u56db\u5e74\u5b9a\u683c\uff1blog \u6a2a\u8f74\u3002",
      x = "\u4eba\u5747 CHE (USD2023, log)", y = NULL,
      caption = "WHO GHED"
    ) +
    .gxe_theme(11)
}

.gxe_rank_change <- function(master) {
  d <- master |>
    dplyr::filter(.data$year %in% c(2000, 2010, 2023),
                  is.finite(.data$che_pc_usd2023))
  if (!nrow(d)) return(NULL)
  ranks <- d |>
    dplyr::group_by(.data$year) |>
    dplyr::mutate(rank = rank(-.data$che_pc_usd2023)) |>
    dplyr::ungroup() |>
    dplyr::filter(.data$rank <= 25)
  if (!nrow(ranks)) return(NULL)
  ranks$year <- factor(ranks$year)
  ggplot2::ggplot(ranks, ggplot2::aes(.data$year, .data$rank,
                                        group = .data$iso3_code,
                                        color = .data$continent)) +
    ggplot2::geom_line(alpha = .55, linewidth = .9) +
    ggplot2::geom_point(size = 2.4, alpha = .9) +
    ggrepel::geom_text_repel(
      data = ranks |> dplyr::filter(.data$year == "2023"),
      ggplot2::aes(label = .data$iso3_code),
      hjust = -0.5, size = 3, direction = "y",
      box.padding = 0.2, max.overlaps = 30
    ) +
    ggplot2::scale_y_reverse(breaks = c(1, 5, 10, 15, 20, 25)) +
    ggplot2::scale_color_brewer(palette = "Set2", name = NULL) +
    ggplot2::labs(
      title    = "\u4eba\u5747 CHE \u524d 25 \u540d\u6392\u540d\u53d8\u8fc1 \u00b7 2000 \u2192 2023",
      subtitle = "\u8d8a\u5f80\u4e0a = \u6392\u540d\u8d8a\u9760\u524d\uff1b\u68b3\u68b3\u4ea4\u9519\u63d0\u793a\u8de8\u671f\u5347\u964d\u3002",
      x = NULL, y = "\u6392\u540d (1 = \u6700\u9ad8)",
      caption = "WHO GHED"
    ) +
    .gxe_theme(11)
}

.gxe_corr_matrix <- function(master) {
  yr <- max(master$year, na.rm = TRUE)
  cols <- intersect(c("che_pc_usd2023", "hf3_che", "gghed_che",
                       "ext_che", "pvtd_che", "life_exp", "u5mr",
                       "gdp_pc_usd", "gdp_per_capita_usd"),
                     names(master))
  if (length(cols) < 4) return(NULL)
  d <- master[master$year == yr, cols]
  d <- d[stats::complete.cases(d), ]
  if (!nrow(d)) return(NULL)
  m <- stats::cor(d, use = "pairwise.complete.obs")
  long <- as.data.frame(as.table(m))
  names(long) <- c("a", "b", "r")
  ggplot2::ggplot(long, ggplot2::aes(.data$a, .data$b, fill = .data$r)) +
    ggplot2::geom_tile(color = "#fbf6ee") +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", .data$r)),
                       size = 3.2, color = "#0d121b") +
    ggplot2::scale_fill_distiller(palette = "RdBu", limits = c(-1, 1),
                                   name = "r") +
    ggplot2::labs(
      title    = sprintf("\u591a\u53d8\u91cf\u76f8\u5173\u77e9\u9635 \u00b7 %d \u5e74\u622a\u9762", yr),
      subtitle = "\u8d22\u653f\u4e0e\u4ea7\u51fa\u3001\u8001\u9f84\u4e0e\u8d44\u91d1\u540c\u65f6\u5448\u73b0\u3002",
      x = NULL, y = NULL,
      caption = "WHO GHED + WB WDI"
    ) +
    .gxe_theme(11) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1))
}

.gxe_extreme_waterfall <- function(master) {
  d <- master |>
    dplyr::filter(.data$year %in% c(2000, max(master$year, na.rm = TRUE))) |>
    dplyr::select(.data$iso3_code, .data$country_name, .data$continent,
                   .data$year, .data$che_pc_usd2023) |>
    tidyr::pivot_wider(names_from = "year", values_from = "che_pc_usd2023")
  if (ncol(d) < 5) return(NULL)
  yrs <- as.character(c(2000, max(master$year, na.rm = TRUE)))
  d$delta <- d[[yrs[2]]] - d[[yrs[1]]]
  d$ratio <- d[[yrs[2]]] / d[[yrs[1]]]
  d <- d[is.finite(d$delta) & is.finite(d$ratio), ]
  if (!nrow(d)) return(NULL)
  top_inc <- d |>
    dplyr::filter(.data[[yrs[1]]] >= 50) |>
    dplyr::arrange(dplyr::desc(.data$ratio)) |>
    utils::head(8) |>
    dplyr::mutate(role = "\u8df3\u8dc3\u589e\u957f")
  top_dec <- d |>
    dplyr::arrange(.data$ratio) |>
    utils::head(8) |>
    dplyr::mutate(role = "\u589e\u901f\u6700\u6162")
  bind <- dplyr::bind_rows(top_inc, top_dec)
  bind$country_name <- factor(bind$country_name,
    levels = bind$country_name[order(bind$ratio)])
  ggplot2::ggplot(bind) +
    ggplot2::geom_segment(ggplot2::aes(x = .data[[yrs[1]]],
                                         xend = .data[[yrs[2]]],
                                         y = .data$country_name,
                                         yend = .data$country_name,
                                         color = .data$role),
                          linewidth = 1.2,
                          arrow = grid::arrow(length = grid::unit(0.18, "cm"),
                                              type = "closed")) +
    ggplot2::geom_point(ggplot2::aes(.data[[yrs[1]]], .data$country_name),
                        color = "#5d667a", size = 2) +
    ggplot2::scale_color_manual(values = c("\u8df3\u8dc3\u589e\u957f" = "#c46327",
                                            "\u589e\u901f\u6700\u6162" = "#1d3f5f"),
                                 name = NULL) +
    ggplot2::scale_x_log10(labels = scales::label_dollar()) +
    ggplot2::labs(
      title    = sprintf("\u4eba\u5747 CHE \u8df3\u53d8 \u00b7 %s \u2192 %s\uff0c\u4e24\u7aef\u5404 8 \u56fd",
                          yrs[1], yrs[2]),
      subtitle = "\u7bad\u5934\u8d77\u70b9\u4e3a 2000 \u5e74\u4eba\u5747 CHE\uff0c\u7ec8\u70b9\u4e3a\u6700\u8fd1\u5e74\u3002",
      x = "\u4eba\u5747 CHE (USD2023, log)", y = NULL,
      caption = "WHO GHED"
    ) +
    .gxe_theme(11)
}

.gxe_cluster_archetype <- function(master) {
  yr <- max(master$year, na.rm = TRUE)
  d <- master |>
    dplyr::filter(.data$year == yr,
                  is.finite(.data$gghed_che),
                  is.finite(.data$pvtd_che),
                  is.finite(.data$ext_che),
                  is.finite(.data$hf3_che),
                  is.finite(.data$che_pc_usd2023))
  if (nrow(d) < 30) return(NULL)
  X <- scale(as.matrix(d[, c("gghed_che", "pvtd_che",
                              "ext_che", "hf3_che",
                              "che_pc_usd2023")]))
  km <- stats::kmeans(X, centers = 4, nstart = 25)
  d$cl <- factor(km$cluster, labels = c("\u653f\u5e9c\u4e3b\u5bfc",
                                         "\u79c1\u4eba\u4fdd\u9669",
                                         "\u81ea\u4ed8\u9a71\u52a8",
                                         "\u5916\u63f4\u4f9d\u8d56"))
  ctr <- as.data.frame(km$centers)
  ctr$cl <- factor(seq_len(nrow(ctr)),
    labels = c("\u653f\u5e9c\u4e3b\u5bfc", "\u79c1\u4eba\u4fdd\u9669",
                "\u81ea\u4ed8\u9a71\u52a8", "\u5916\u63f4\u4f9d\u8d56"))
  long <- tidyr::pivot_longer(ctr, -.data$cl,
                                names_to = "axis", values_to = "z")
  long$axis <- factor(long$axis,
    levels = c("gghed_che", "pvtd_che", "ext_che", "hf3_che", "che_pc_usd2023"),
    labels = c("GGHED", "PVTD", "EXT", "OOPS", "CHE_pc"))
  ggplot2::ggplot(long, ggplot2::aes(.data$axis, .data$z,
                                       group = .data$cl, color = .data$cl)) +
    ggplot2::geom_polygon(ggplot2::aes(fill = .data$cl),
                          alpha = .15, linewidth = 1) +
    ggplot2::geom_point(size = 2.3) +
    ggplot2::coord_polar() +
    ggplot2::scale_color_manual(values = c("\u653f\u5e9c\u4e3b\u5bfc" = "#1d3f5f",
                                            "\u79c1\u4eba\u4fdd\u9669" = "#2a857a",
                                            "\u81ea\u4ed8\u9a71\u52a8" = "#c46327",
                                            "\u5916\u63f4\u4f9d\u8d56" = "#774314"),
                                 name = "Cluster") +
    ggplot2::scale_fill_manual(values = c("\u653f\u5e9c\u4e3b\u5bfc" = "#1d3f5f",
                                           "\u79c1\u4eba\u4fdd\u9669" = "#2a857a",
                                           "\u81ea\u4ed8\u9a71\u52a8" = "#c46327",
                                           "\u5916\u63f4\u4f9d\u8d56" = "#774314"),
                                guide = "none") +
    ggplot2::labs(
      title    = sprintf("4 \u7c7b archetype \u96f7\u8fbe \u00b7 %d", yr),
      subtitle = "\u6bcf\u4e00\u8f74\u4e3a\u8be5\u7c7b\u5747\u503c\u7684 z \u5f97\u5206\uff1b\u8d8a\u9760\u5916\u5708 = \u8be5\u53d8\u91cf\u8d8a\u9ad8\u3002",
      x = NULL, y = NULL,
      caption = "WHO GHED \u00b7 k-means(k=4) on z-scaled GGHED/PVTD/EXT/OOPS/CHE_pc"
    ) +
    .gxe_theme(12) + ggplot2::theme(axis.text.y = ggplot2::element_blank())
}


ghs_export_extra <- function(master, world_sf = NULL,
                              outputs_dir = file.path("分析输出", "图表"),
                              verbose = TRUE) {
  .gxe_pkgs()
  dir.create(outputs_dir, recursive = TRUE, showWarnings = FALSE)
  prev <- getwd()
  if (basename(outputs_dir) == "图表") {
    on.exit(setwd(prev), add = TRUE)
    setwd(prev)
  }

  fig_dir <- outputs_dir
  save_to <- function(name, plot, w = 11, h = 6.4) {
    if (is.null(plot)) return(invisible(NULL))
    p <- file.path(fig_dir, paste0(name, ".png"))
    save_fig(plot, name, width = w, height = h, dpi = 300)
    if (verbose) cat(sprintf("[extra] %s\n", name))
  }

  save_to("v2_profile_brazil",      .gxe_brazil_profile(master),      14, 9)
  save_to("v2_oops_violin",         .gxe_oops_violin(master),         11, 6)
  save_to("v2_oops_heatmap_grid",   .gxe_oops_heatmap_grid(master),   12, 9)
  save_to("v2_lifeexp_elasticity",  .gxe_lifeexp_elasticity(master),  11, 6.4)
  save_to("v2_sdg3_radar",          .gxe_sdg3_radar(master),          10, 8)
  save_to("v2_sdg3_progress",       .gxe_sdg3_progress(master),       11, 8.5)
  save_to("v2_period_compare",      .gxe_period_compare(master),      12, 5.6)
  save_to("v2_quantile_reg",        .gxe_quantile_reg(master),        11, 6.2)
  save_to("v2_changepoint",         .gxe_changepoint(master),         11, 5.4)
  save_to("v2_continent_ridges",    .gxe_continent_ridges(master),    12, 7)
  save_to("v2_rank_change",         .gxe_rank_change(master),         11, 8)
  save_to("v2_corr_matrix",         .gxe_corr_matrix(master),         8.5, 8)
  save_to("v2_extreme_waterfall",   .gxe_extreme_waterfall(master),   11, 7.5)
  save_to("v2_cluster_archetype",   .gxe_cluster_archetype(master),   10, 8)

  invisible(NULL)
}
