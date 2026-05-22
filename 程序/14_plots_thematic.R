
plot_v2_equity_indices <- function(master) {
  if (!exists("inequality_by_year", mode = "function"))
    stop("程序/04_metrics.R::inequality_by_year() not loaded")

  ineq <- tryCatch(inequality_by_year(master, value_col = "che_pc_usd2023"),
                    error = function(e) NULL)
  if (is.null(ineq) || nrow(ineq) == 0) return(ggplot2::ggplot())

  ix_cols <- intersect(c("gini_pop", "theil_pop", "atk1"), names(ineq))
  if (!length(ix_cols)) return(ggplot2::ggplot())
  long <- tidyr::pivot_longer(ineq,
    cols = dplyr::all_of(ix_cols),
    names_to = "index", values_to = "value")
  long$index <- factor(long$index,
    levels = c("gini_pop", "theil_pop", "atk1"),
    labels = c("Gini", "Theil-T", "Atkinson(\u03b5=1)"))

  ggplot2::ggplot(long, ggplot2::aes(year, value, colour = index)) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_point(size = 1.6) +
    ggplot2::scale_colour_manual(
      values = stats::setNames(
        c(brand_palette$gghed, brand_palette$pvtd, brand_palette$ext),
        c("Gini", "Theil-T", "Atkinson(\u03b5=1)")),
      name = NULL) +
    ggplot2::scale_x_continuous(breaks = scales::breaks_pretty(7)) +
    labs_news(
      title    = "\u4e0d\u5e73\u7b49\u4e09\u8054\u5f39\u00b7\u5168\u7403\u4eba\u5747 CHE",
      subtitle = "Gini / Theil / Atkinson \u540c\u671f\u4e0b\u884c\uff0c\u6536\u655b\u4f46\u8fdc\u672a\u5e73\u62bc",
      x = NULL, y = "Inequality index (0\u20131)"
    ) +
    ggplot2::theme(legend.position = "top")
}

plot_v2_equity_lorenz <- function(master, years = c(2000, 2010, 2023)) {
  if (!exists("fit_lorenz", mode = "function"))
    stop("程序/12_advanced_models.R::fit_lorenz() not loaded")

  bind_one <- function(yr) {
    d <- master[master$year == yr & is.finite(master$che_pc_usd2023), ]
    if (nrow(d) < 5) return(NULL)
    l <- fit_lorenz(d$che_pc_usd2023, weights = d$pop)
    l$year <- yr
    l
  }
  data <- do.call(rbind, lapply(years, bind_one))
  if (is.null(data) || nrow(data) == 0) return(ggplot2::ggplot())
  data$year <- factor(data$year)

  ggplot2::ggplot(data, ggplot2::aes(p_pop, p_value, colour = year)) +
    ggplot2::geom_abline(slope = 1, intercept = 0,
                         linetype = "dashed",
                         colour = brand_palette$rule_dark) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::scale_colour_manual(
      values = c(brand_palette$gghed, brand_palette$pvtd, brand_palette$ext),
      name = NULL) +
    ggplot2::scale_x_continuous(labels = scales::percent_format(1)) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(1)) +
    ggplot2::coord_equal() +
    labs_news(
      title    = "Lorenz \u66f2\u7ebf\u00b7\u4eba\u5747 CHE \u5728\u4eba\u53e3\u4e0a\u7684\u96c6\u4e2d",
      subtitle = "\u66f2\u7ebf\u8d8a\u8fd1\u5bf9\u89d2\u7ebf\u00b7\u8d44\u6e90\u8d8a\u5e73\u5747",
      x = "\u4eba\u53e3\u7d2f\u8ba1\u6bd4\u4f8b", y = "\u652f\u51fa\u7d2f\u8ba1\u6bd4\u4f8b"
    ) +
    ggplot2::theme(legend.position = "top")
}

plot_v2_fiscal_ghe_share <- function(master, year = 2022) {
  if (!"gghed_usd2023" %in% names(master)) return(ggplot2::ggplot())
  d <- master[master$year == year &
              is.finite(master$gghed_usd2023) &
              is.finite(master$gdp_pc_usd) &
              !is.na(master$income_group), ]
  if (nrow(d) < 10) return(ggplot2::ggplot())
  d$ghe_per_cap <- d$gghed_usd2023 / d$pop

  ggplot2::ggplot(d, ggplot2::aes(gdp_pc_usd, ghe_per_cap,
                                   colour = income_group, size = pop)) +
    ggplot2::geom_point(alpha = 0.7) +
    ggrepel::geom_text_repel(
      data = utils::head(d[order(-d$ghe_per_cap), ], 8),
      ggplot2::aes(label = country_name),
      family = "sans", size = 3, max.overlaps = 12) +
    ggplot2::scale_x_log10(labels = scales::label_dollar()) +
    ggplot2::scale_y_log10(labels = scales::label_dollar()) +
    scale_colour_brand_income(name = "\u6536\u5165\u7ec4") +
    ggplot2::scale_size_area(max_size = 8, guide = "none") +
    labs_news(
      title    = "\u8d22\u653f\u7a7a\u95f4\u00b7GHE/cap vs GDP/cap (log-log)",
      subtitle = sprintf("%d\u5e74\u00b7\u9ad8\u6536\u5165\u56fd\u8054\u5747\u8d85 $4000/cap", year),
      x = "GDP per capita (USD, log)", y = "Government health spending per capita (USD, log)"
    )
}

plot_v2_fiscal_ghe_rank <- function(master, year = 2022, n_top = 25) {
  if (!"hf1_che" %in% names(master)) return(ggplot2::ggplot())
  d <- master[master$year == year &
              is.finite(master$hf1_che) &
              !is.na(master$income_group), ]
  d <- utils::head(d[order(-d$hf1_che), ], n_top)
  d$country_name <- factor(d$country_name,
                           levels = rev(unique(d$country_name)))

  ggplot2::ggplot(d, ggplot2::aes(hf1_che, country_name,
                                   fill = income_group)) +
    ggplot2::geom_col(width = 0.75) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.0f%%", hf1_che)),
                       hjust = -0.15, size = 3, family = "sans") +
    scale_fill_brand_income(name = "\u6536\u5165\u7ec4") +
    ggplot2::expand_limits(x = max(d$hf1_che, na.rm = TRUE) * 1.1) +
    labs_news(
      title    = "\u653f\u5e9c\u4e3b\u5bfc\u578b\u00b7HF1 \u5360 CHE \u6700\u9ad8\u7684 25 \u56fd",
      subtitle = sprintf("%d\u5e74\u00b7HF1 \u5305\u542b\u793e\u4fdd + \u8d22\u653f\u62e8\u6b3e", year),
      x = "HF1 / CHE  (%)", y = NULL
    ) +
    ggplot2::theme(legend.position = "top")
}

plot_v2_efficiency_dea <- function(master, year = 2021) {
  if (!"life_exp" %in% names(master)) return(ggplot2::ggplot())
  d <- master[master$year == year &
              is.finite(master$che_pc_usd2023) &
              is.finite(master$life_exp), ]
  if (nrow(d) < 20) return(ggplot2::ggplot())

  d$bin <- cut(log10(d$che_pc_usd2023),
               breaks = pretty(log10(d$che_pc_usd2023), 12))
  frontier <- d |>
    dplyr::group_by(bin) |>
    dplyr::slice_max(life_exp, n = 1, with_ties = FALSE) |>
    dplyr::ungroup()

  ggplot2::ggplot(d, ggplot2::aes(che_pc_usd2023, life_exp)) +
    ggplot2::geom_point(ggplot2::aes(colour = continent),
                        alpha = 0.55, size = 2.2) +
    ggplot2::geom_smooth(method = "loess", se = TRUE,
                         colour = brand_palette$ink,
                         fill = brand_palette$rule, linewidth = 0.6) +
    ggplot2::geom_line(data = frontier[order(frontier$che_pc_usd2023), ],
                       ggplot2::aes(che_pc_usd2023, life_exp),
                       linewidth = 1, linetype = "longdash",
                       colour = brand_palette$pvtd) +
    ggplot2::scale_x_log10(labels = scales::label_dollar()) +
    scale_colour_brand_continent(name = NULL) +
    labs_news(
      title    = "\u6548\u7387\u524d\u6cbf\u00b7\u540c\u4e00\u91d1\u989d\u4e0b\u8c01\u4e70\u5230\u66f4\u591a\u5bff\u547d",
      subtitle = sprintf("%d\u5e74\u00b7\u6a59\u8679\u865a\u7ebf=\u5404 log(CHE) \u533a\u95f4\u4e0a\u5305\u7edc", year),
      x = "CHE per capita (USD, log)", y = "Life expectancy (years)"
    ) +
    ggplot2::theme(legend.position = "top")
}

plot_v2_outcomes_elasticity <- function(master) {
  if (!"life_exp" %in% names(master)) return(ggplot2::ggplot())
  d <- master[is.finite(master$che_pc_usd2023) &
              is.finite(master$life_exp), ]
  if (nrow(d) < 50) return(ggplot2::ggplot())
  d$log_che <- log(d$che_pc_usd2023)

  q <- stats::quantile(d$log_che, c(0.33, 0.67), na.rm = TRUE)
  d$tier <- cut(d$log_che, breaks = c(-Inf, q[1], q[2], Inf),
                labels = c("Low", "Mid", "High"))

  ggplot2::ggplot(d, ggplot2::aes(log_che, life_exp, colour = tier)) +
    ggplot2::geom_point(alpha = 0.35, size = 1.6) +
    ggplot2::geom_smooth(method = "lm", se = TRUE, linewidth = 1,
                         ggplot2::aes(group = tier)) +
    ggplot2::scale_colour_manual(
      values = c(Low = brand_palette$africa,
                 Mid = brand_palette$asia,
                 High = brand_palette$europe),
      name = "log CHE/cap \u5206\u6bb5"
    ) +
    labs_news(
      title    = "\u5f39\u6027\u66f2\u7ebf\u00b7\u8c01\u592a\u8d35\u4e86\u5c31\u4e70\u4e0d\u5230\u5bff\u547d\u4e86\uff1f",
      subtitle = "\u7ebf\u6027\u62df\u5408\u00b7Low/Mid/High \u4e09\u6bb5\u00b7\u9ad8\u6298\u8861\u70b9\u660e\u663e",
      x = "log(CHE per capita, USD)", y = "Life expectancy"
    ) +
    ggplot2::theme(legend.position = "top")
}

plot_v2_aid_dependency <- function(master, year = 2021, n_top = 25) {
  if (!"ext_che" %in% names(master)) return(ggplot2::ggplot())
  d <- master[master$year == year & is.finite(master$ext_che), ]
  d <- utils::head(d[order(-d$ext_che), ], n_top)
  d$country_name <- factor(d$country_name,
                           levels = rev(unique(d$country_name)))

  ggplot2::ggplot(d, ggplot2::aes(ext_che, country_name,
                                   colour = continent)) +
    ggplot2::geom_segment(ggplot2::aes(x = 0, xend = ext_che,
                                        yend = country_name),
                          linewidth = 0.6) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.0f%%", ext_che)),
                       hjust = -0.4, size = 3, colour = brand_palette$ink,
                       family = "sans") +
    scale_colour_brand_continent(name = NULL) +
    ggplot2::expand_limits(x = max(d$ext_che, na.rm = TRUE) * 1.15) +
    labs_news(
      title    = "\u5916\u63f4\u4f9d\u8d56\u00b7Top 25 \u00b7 ext_che",
      subtitle = sprintf("%d\u5e74\u00b7\u591a\u4e3a\u4e1c\u975e/\u592a\u5e73\u6d0b\u5c0f\u56fd", year),
      x = "External funds / CHE  (%)", y = NULL
    ) +
    ggplot2::theme(legend.position = "top")
}

plot_v2_combined_ridges <- function(master, years = c(2000, 2010, 2023)) {
  if (!requireNamespace("ggridges", quietly = TRUE)) {
    return(ggplot2::ggplot() + ggplot2::ggtitle("ggridges not installed"))
  }
  d <- master[master$year %in% years &
              is.finite(master$che_pc_usd2023) &
              !is.na(master$income_group), ]
  if (nrow(d) == 0) return(ggplot2::ggplot())
  d$year <- factor(d$year)

  ggplot2::ggplot(d, ggplot2::aes(x = che_pc_usd2023,
                                   y = income_group,
                                   fill = year)) +
    ggridges::geom_density_ridges(alpha = 0.6, scale = 1.2,
                                  rel_min_height = 0.005) +
    ggplot2::scale_x_log10(labels = scales::label_dollar()) +
    ggplot2::scale_fill_manual(
      values = c(brand_palette$ext, brand_palette$pvtd, brand_palette$gghed),
      name = NULL) +
    labs_news(
      title    = "\u6536\u5165\u7ec4\u00d7\u5e74\u4efd\u00b7\u4eba\u5747 CHE \u5206\u5e03\u00b7\u5c71\u810a\u56fe",
      subtitle = "23 \u5e74\u95f4\u9ad8\u6536\u5165\u56fd\u4e0a\u9762\u00b7\u4f4e\u6536\u5165\u56fd\u539f\u5730\u8e0f\u6b65",
      x = "CHE per capita (USD, log)", y = NULL
    ) +
    ggplot2::theme(legend.position = "top")
}


ghs_export_v2_thematic <- function(master,
                                    out_dir = file.path("分析输出", "图表"),
                                    verbose = TRUE) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  jobs <- list(
    list(id = "v2_equity_indices",      fn = function() plot_v2_equity_indices(master),       w = 9, h = 5),
    list(id = "v2_equity_lorenz",       fn = function() plot_v2_equity_lorenz(master),        w = 7, h = 7),
    list(id = "v2_fiscal_ghe_share",    fn = function() plot_v2_fiscal_ghe_share(master),     w = 9, h = 6),
    list(id = "v2_fiscal_ghe_rank",     fn = function() plot_v2_fiscal_ghe_rank(master),      w = 8, h = 8),
    list(id = "v2_efficiency_dea",      fn = function() plot_v2_efficiency_dea(master),       w = 9, h = 6),
    list(id = "v2_outcomes_elasticity", fn = function() plot_v2_outcomes_elasticity(master),  w = 9, h = 6),
    list(id = "v2_aid_dependency",      fn = function() plot_v2_aid_dependency(master),       w = 8, h = 8),
    list(id = "v2_combined_ridges",     fn = function() plot_v2_combined_ridges(master),      w = 9, h = 6)
  )
  ok <- 0
  for (j in jobs) {
    p <- tryCatch(j$fn(), error = function(e) {
      if (verbose) message("[", j$id, "] error: ", conditionMessage(e))
      NULL
    })
    if (is.null(p)) next
    fn_png <- file.path(out_dir, paste0(j$id, ".png"))
    fn_svg <- file.path(out_dir, paste0(j$id, ".svg"))
    saved_png <- tryCatch({
      ggplot2::ggsave(fn_png, p, width = j$w, height = j$h, dpi = 150,
                      bg = brand_palette$paper)
      TRUE
    }, error = function(e) {
      if (verbose) message("[", j$id, "] PNG save failed: ", conditionMessage(e))
      FALSE
    })
    tryCatch(ggplot2::ggsave(fn_svg, p, width = j$w, height = j$h,
                             bg = brand_palette$paper),
             error = function(e) NULL)
    if (saved_png) {
      if (verbose) cat("[v2_thematic]", j$id, "saved\n")
      ok <- ok + 1
    }
  }
  invisible(ok)
}
