# =============================================================================
# 程序/24_plots_more.R
# -----------------------------------------------------------------------------
# 第二批静态图扩展（30 张），统一通过 ghs_export_more() 入口调用，
# 与 22_plots_extra.R 互补。所有函数仅依赖 master_enriched 已有列。
# =============================================================================

.gxm_pkgs <- function() {
  ensure_pkgs(c("dplyr", "tidyr", "ggplot2", "scales", "ggrepel", "ggridges"))
}

.gxm_theme <- function() {
  # 优先使用扩展主题，回退到 minimal
  if (exists("theme_ghs3", mode = "function")) theme_ghs3()
  else if (exists("theme_ghs2", mode = "function")) theme_ghs2()
  else ggplot2::theme_minimal(base_size = 12)
}

.gxm_latest_year <- function(master) max(master$year, na.rm = TRUE)

# ---- 1. global CHE total trend (USD trillions) ----
.gxm_global_che_total <- function(master) {
  d <- master |>
    dplyr::filter(is.finite(.data$che_usd2023)) |>
    dplyr::group_by(.data$year) |>
    dplyr::summarise(che = sum(.data$che_usd2023, na.rm = TRUE) / 1e12,
                     .groups = "drop")
  if (!nrow(d)) return(NULL)
  ggplot2::ggplot(d, ggplot2::aes(.data$year, .data$che)) +
    ggplot2::geom_area(fill = "#1d3f5f", alpha = .25) +
    ggplot2::geom_line(color = "#1d3f5f", linewidth = 1.1) +
    ggplot2::geom_point(color = "#1d3f5f", size = 2) +
    ggplot2::scale_x_continuous(breaks = seq(2000, 2023, 4)) +
    ggplot2::scale_y_continuous(labels = scales::label_dollar(suffix = "T")) +
    ggplot2::labs(
      title = "\u5168\u7403\u536b\u751f\u603b\u652f\u51fa CHE \u00b7 2000\u20132023",
      subtitle = "\u5355\u4f4d\uff1aUSD 2023 \u4e07\u4ebf\u3002",
      x = NULL, y = "Total CHE",
      caption = "WHO GHED"
    ) + .gxm_theme()
}

# ---- 2. CHE per capita CAGR by continent ----
.gxm_continent_cagr <- function(master) {
  d <- master |>
    dplyr::filter(.data$year %in% c(2000, .gxm_latest_year(master)),
                  is.finite(.data$che_pc_usd2023),
                  !is.na(.data$continent))
  if (!nrow(d)) return(NULL)
  agg <- d |>
    dplyr::group_by(.data$continent, .data$year) |>
    dplyr::summarise(che = stats::weighted.mean(.data$che_pc_usd2023,
                                                .data$pop, na.rm = TRUE),
                     .groups = "drop") |>
    tidyr::pivot_wider(names_from = "year", values_from = "che")
  yrs <- as.character(c(2000, .gxm_latest_year(master)))
  if (!all(yrs %in% names(agg))) return(NULL)
  agg$cagr <- (agg[[yrs[2]]] / agg[[yrs[1]]]) ^ (1 / (as.integer(yrs[2]) -
                                                       as.integer(yrs[1]))) - 1
  agg <- agg[is.finite(agg$cagr), ]
  agg$continent <- factor(agg$continent, levels = agg$continent[order(agg$cagr)])
  ggplot2::ggplot(agg, ggplot2::aes(.data$cagr, .data$continent)) +
    ggplot2::geom_col(fill = "#2a857a") +
    ggplot2::geom_text(ggplot2::aes(label = scales::percent(.data$cagr,
                                                              accuracy = .1)),
                       hjust = -0.15, color = "#0d121b", size = 3.4) +
    ggplot2::expand_limits(x = max(agg$cagr) * 1.18) +
    ggplot2::scale_x_continuous(labels = scales::label_percent()) +
    ggplot2::labs(
      title = "\u4eba\u5747 CHE 23 \u5e74\u590d\u5408\u589e\u901f \u00b7 \u6309\u5927\u6d32",
      subtitle = "\u4eba\u53e3\u52a0\u6743\uff1bCAGR = (CHE_2023 / CHE_2000)^(1/23) \u2212 1\u3002",
      x = "CAGR", y = NULL,
      caption = "WHO GHED"
    ) + .gxm_theme()
}

# ---- 3. CHE per capita CAGR by income group ----
.gxm_income_cagr <- function(master) {
  d <- master |>
    dplyr::filter(.data$year %in% c(2000, .gxm_latest_year(master)),
                  is.finite(.data$che_pc_usd2023),
                  !is.na(.data$income_group))
  if (!nrow(d)) return(NULL)
  agg <- d |>
    dplyr::group_by(.data$income_group, .data$year) |>
    dplyr::summarise(che = stats::weighted.mean(.data$che_pc_usd2023,
                                                .data$pop, na.rm = TRUE),
                     .groups = "drop") |>
    tidyr::pivot_wider(names_from = "year", values_from = "che")
  yrs <- as.character(c(2000, .gxm_latest_year(master)))
  if (!all(yrs %in% names(agg))) return(NULL)
  agg$cagr <- (agg[[yrs[2]]] / agg[[yrs[1]]]) ^ (1 / 23) - 1
  agg <- agg[is.finite(agg$cagr), ]
  agg$income_group <- factor(agg$income_group,
    levels = c("Low income", "Lower middle income",
               "Upper middle income", "High income"))
  ggplot2::ggplot(agg, ggplot2::aes(.data$income_group, .data$cagr,
                                    fill = .data$income_group)) +
    ggplot2::geom_col(width = .65) +
    ggplot2::geom_text(ggplot2::aes(label = scales::percent(.data$cagr,
                                                              accuracy = .1)),
                       vjust = -0.6, size = 3.4, color = "#0d121b") +
    ggplot2::scale_fill_brewer(palette = "OrRd", guide = "none") +
    ggplot2::scale_y_continuous(labels = scales::label_percent(),
                                expand = ggplot2::expansion(mult = c(0, .15))) +
    ggplot2::labs(
      title = "\u4eba\u5747 CHE 23 \u5e74\u590d\u5408\u589e\u901f \u00b7 \u6309\u6536\u5165\u7ec4",
      subtitle = "\u4eba\u53e3\u52a0\u6743\uff1b\u4f4e\u6536\u5165\u7ec4\u589e\u901f\u6700\u9ad8\u3002",
      x = NULL, y = "CAGR",
      caption = "WHO GHED + WB \u6536\u5165\u7ec4\u5206\u7c7b"
    ) + .gxm_theme()
}

# ---- 4. OOPS share dumbbell 2000 vs latest, top 25 declines ----
.gxm_oops_dumbbell_decline <- function(master) {
  yrs <- c(2000, .gxm_latest_year(master))
  d <- master |>
    dplyr::filter(.data$year %in% yrs, is.finite(.data$hf3_che)) |>
    dplyr::select(.data$iso3_code, .data$country_name, .data$continent,
                   .data$year, .data$hf3_che) |>
    tidyr::pivot_wider(names_from = "year", values_from = "hf3_che",
                        names_prefix = "y")
  cols <- paste0("y", yrs)
  if (!all(cols %in% names(d))) return(NULL)
  d$delta <- d[[cols[2]]] - d[[cols[1]]]
  d <- d[is.finite(d$delta), ]
  d <- d[order(d$delta), ]
  top <- utils::head(d, 25)
  top$country_name <- factor(top$country_name,
    levels = rev(top$country_name))
  long <- tidyr::pivot_longer(top, cols = dplyr::all_of(cols),
                                names_to = "year", values_to = "oops")
  long$year <- factor(long$year, levels = cols, labels = as.character(yrs))
  ggplot2::ggplot(top) +
    ggplot2::geom_segment(ggplot2::aes(x = .data[[cols[1]]],
                                         xend = .data[[cols[2]]],
                                         y = .data$country_name,
                                         yend = .data$country_name),
                          color = "#5d667a", linewidth = .9) +
    ggplot2::geom_point(data = long,
                         ggplot2::aes(x = .data$oops,
                                       y = .data$country_name,
                                       color = .data$year),
                         size = 2.6) +
    ggplot2::scale_color_manual(values = stats::setNames(
      c("#5d667a", "#1d3f5f"), as.character(yrs)), name = NULL) +
    ggplot2::scale_x_continuous(labels = scales::label_percent(scale = 1)) +
    ggplot2::labs(
      title = sprintf("OOPS \u5360\u6bd4\u4e0b\u964d\u6700\u5feb\u7684 25 \u56fd \u00b7 %s \u2192 %s",
                       yrs[1], yrs[2]),
      subtitle = "\u70b9\u8d8a\u5de6 = OOPS \u8d8a\u4f4e\uff1b\u5de6 vs \u53f3\u8d8a\u8fdc\u8868\u793a\u51cf\u5e45\u8d8a\u5927\u3002",
      x = "OOPS \u5360 CHE", y = NULL,
      caption = "WHO GHED \u00b7 hf3_che"
    ) + .gxm_theme()
}

# ---- 5. OOPS share dumbbell 2000 vs latest, top 25 increases ----
.gxm_oops_dumbbell_rise <- function(master) {
  yrs <- c(2000, .gxm_latest_year(master))
  d <- master |>
    dplyr::filter(.data$year %in% yrs, is.finite(.data$hf3_che)) |>
    dplyr::select(.data$iso3_code, .data$country_name, .data$continent,
                   .data$year, .data$hf3_che) |>
    tidyr::pivot_wider(names_from = "year", values_from = "hf3_che",
                        names_prefix = "y")
  cols <- paste0("y", yrs)
  if (!all(cols %in% names(d))) return(NULL)
  d$delta <- d[[cols[2]]] - d[[cols[1]]]
  d <- d[is.finite(d$delta), ]
  d <- d[order(-d$delta), ]
  top <- utils::head(d, 25)
  top$country_name <- factor(top$country_name,
    levels = rev(top$country_name))
  long <- tidyr::pivot_longer(top, cols = dplyr::all_of(cols),
                                names_to = "year", values_to = "oops")
  long$year <- factor(long$year, levels = cols, labels = as.character(yrs))
  ggplot2::ggplot(top) +
    ggplot2::geom_segment(ggplot2::aes(x = .data[[cols[1]]],
                                         xend = .data[[cols[2]]],
                                         y = .data$country_name,
                                         yend = .data$country_name),
                          color = "#c46327", linewidth = .9) +
    ggplot2::geom_point(data = long,
                         ggplot2::aes(x = .data$oops,
                                       y = .data$country_name,
                                       color = .data$year),
                         size = 2.6) +
    ggplot2::scale_color_manual(values = stats::setNames(
      c("#5d667a", "#c46327"), as.character(yrs)), name = NULL) +
    ggplot2::scale_x_continuous(labels = scales::label_percent(scale = 1)) +
    ggplot2::labs(
      title = sprintf("OOPS \u5360\u6bd4\u4e0a\u5347\u6700\u5feb\u7684 25 \u56fd \u00b7 %s \u2192 %s",
                       yrs[1], yrs[2]),
      subtitle = "\u70b9\u8d8a\u53f3 = OOPS \u8d8a\u9ad8\uff1b\u8d22\u52a1\u4fdd\u62a4\u5012\u9000\u9884\u8b66\u3002",
      x = "OOPS \u5360 CHE", y = NULL,
      caption = "WHO GHED \u00b7 hf3_che"
    ) + .gxm_theme()
}

# ---- 6. CHE/cap top growth countries ----
.gxm_che_top_growth <- function(master) {
  yrs <- c(2000, .gxm_latest_year(master))
  d <- master |>
    dplyr::filter(.data$year %in% yrs,
                  is.finite(.data$che_pc_usd2023)) |>
    dplyr::select(.data$iso3_code, .data$country_name, .data$continent,
                   .data$year, .data$che_pc_usd2023) |>
    tidyr::pivot_wider(names_from = "year",
                        values_from = "che_pc_usd2023",
                        names_prefix = "y")
  cols <- paste0("y", yrs)
  if (!all(cols %in% names(d))) return(NULL)
  d <- d[is.finite(d[[cols[1]]]) & d[[cols[1]]] > 30 &
           is.finite(d[[cols[2]]]), ]
  d$ratio <- d[[cols[2]]] / d[[cols[1]]]
  d$cagr <- d$ratio ^ (1 / 23) - 1
  d <- d[is.finite(d$cagr), ]
  d <- d[order(-d$cagr), ]
  top <- utils::head(d, 30)
  top$country_name <- factor(top$country_name,
    levels = rev(top$country_name))
  ggplot2::ggplot(top, ggplot2::aes(.data$cagr, .data$country_name,
                                     fill = .data$continent)) +
    ggplot2::geom_col(width = .7) +
    ggplot2::geom_text(ggplot2::aes(label = scales::percent(.data$cagr,
                                                              accuracy = .1)),
                       hjust = -0.1, size = 3, color = "#0d121b") +
    ggplot2::expand_limits(x = max(top$cagr) * 1.2) +
    ggplot2::scale_fill_brewer(palette = "Set2", name = NULL) +
    ggplot2::scale_x_continuous(labels = scales::label_percent()) +
    ggplot2::labs(
      title = "\u4eba\u5747 CHE 23 \u5e74 CAGR \u524d 30 \u56fd",
      subtitle = "\u8d77\u70b9\u5316\u5904\u7406\u540e\u8ba1\u7b97\u590d\u5408\u589e\u901f\u3002",
      x = "CAGR", y = NULL,
      caption = "WHO GHED"
    ) + .gxm_theme()
}

# ---- 7. life expectancy change top ----
.gxm_life_change_top <- function(master) {
  yrs <- c(2000, .gxm_latest_year(master))
  d <- master |>
    dplyr::filter(.data$year %in% yrs, is.finite(.data$life_exp)) |>
    dplyr::select(.data$iso3_code, .data$country_name, .data$continent,
                   .data$year, .data$life_exp) |>
    tidyr::pivot_wider(names_from = "year", values_from = "life_exp",
                        names_prefix = "y")
  cols <- paste0("y", yrs)
  if (!all(cols %in% names(d))) return(NULL)
  d$delta <- d[[cols[2]]] - d[[cols[1]]]
  d <- d[is.finite(d$delta), ]
  d <- d[order(-d$delta), ]
  top <- utils::head(d, 25)
  top$country_name <- factor(top$country_name,
    levels = rev(top$country_name))
  ggplot2::ggplot(top, ggplot2::aes(.data$delta, .data$country_name,
                                     fill = .data$continent)) +
    ggplot2::geom_col(width = .68) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("+%.1f", .data$delta)),
                       hjust = -0.1, size = 3.1, color = "#0d121b") +
    ggplot2::expand_limits(x = max(top$delta) * 1.15) +
    ggplot2::scale_fill_brewer(palette = "Set2", name = NULL) +
    ggplot2::labs(
      title = sprintf("\u9884\u671f\u5bff\u547d\u589e\u957f\u6700\u5feb\u7684 25 \u56fd \u00b7 %s \u2192 %s",
                       yrs[1], yrs[2]),
      subtitle = "\u5355\u4f4d\uff1a\u5e74\u3002",
      x = "\u0394 \u5bff\u547d", y = NULL,
      caption = "WB WDI \u00b7 life_exp"
    ) + .gxm_theme()
}

# ---- 8. U5MR decline top ----
.gxm_u5mr_decline_top <- function(master) {
  yrs <- c(2000, .gxm_latest_year(master))
  d <- master |>
    dplyr::filter(.data$year %in% yrs, is.finite(.data$u5mr)) |>
    dplyr::select(.data$iso3_code, .data$country_name, .data$continent,
                   .data$year, .data$u5mr) |>
    tidyr::pivot_wider(names_from = "year", values_from = "u5mr",
                        names_prefix = "y")
  cols <- paste0("y", yrs)
  if (!all(cols %in% names(d))) return(NULL)
  d$delta <- d[[cols[1]]] - d[[cols[2]]]
  d <- d[is.finite(d$delta), ]
  d <- d[order(-d$delta), ]
  top <- utils::head(d, 25)
  top$country_name <- factor(top$country_name,
    levels = rev(top$country_name))
  ggplot2::ggplot(top, ggplot2::aes(.data$delta, .data$country_name,
                                     fill = .data$continent)) +
    ggplot2::geom_col(width = .68) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("-%.0f", .data$delta)),
                       hjust = -0.1, size = 3.1, color = "#0d121b") +
    ggplot2::expand_limits(x = max(top$delta) * 1.15) +
    ggplot2::scale_fill_brewer(palette = "Set2", name = NULL) +
    ggplot2::labs(
      title = sprintf("U5MR \u4e0b\u964d\u6700\u591a\u7684 25 \u56fd \u00b7 %s \u2192 %s",
                       yrs[1], yrs[2]),
      subtitle = "\u5355\u4f4d\uff1a/1000 \u6d3b\u4ea7\u3002",
      x = "U5MR \u4e0b\u964d\u70b9", y = NULL,
      caption = "WB WDI \u00b7 u5mr"
    ) + .gxm_theme()
}

# ---- 9. external aid top recipients ----
.gxm_ext_top_recipients <- function(master) {
  yr <- .gxm_latest_year(master)
  d <- master |>
    dplyr::filter(.data$year == yr,
                  is.finite(.data$ext_che),
                  is.finite(.data$che_usd2023)) |>
    dplyr::arrange(dplyr::desc(.data$ext_che)) |>
    utils::head(25)
  if (!nrow(d)) return(NULL)
  d$country_name <- factor(d$country_name,
    levels = rev(d$country_name))
  ggplot2::ggplot(d, ggplot2::aes(.data$ext_che, .data$country_name,
                                   fill = .data$continent)) +
    ggplot2::geom_col(width = .68) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.0f%%", .data$ext_che)),
                       hjust = -0.1, size = 3.1, color = "#0d121b") +
    ggplot2::expand_limits(x = max(d$ext_che) * 1.15) +
    ggplot2::scale_fill_brewer(palette = "Set2", name = NULL) +
    ggplot2::labs(
      title = sprintf("\u5916\u63f4\u5360 CHE \u6700\u9ad8\u7684 25 \u56fd \u00b7 %d", yr),
      subtitle = "EXT > 30% \u4e3a\u9ad8\u4f9d\u8d56\u9608\u503c\uff1b\u9ad8\u4f9d\u8d56\u96c6\u4e2d\u4e8e\u6492\u54c8\u4ee5\u5357\u3002",
      x = "EXT \u5360 CHE", y = NULL,
      caption = "WHO GHED \u00b7 ext_che"
    ) + .gxm_theme()
}

# ---- 10. HC purpose breakdown latest year ----
.gxm_hc_breakdown <- function(master) {
  yr <- .gxm_latest_year(master)
  cols <- intersect(c("hc1_che", "hc2_che", "hc3_che", "hc4_che",
                       "hc5_che", "hc6_che", "hc7_che", "hc9_che"),
                     names(master))
  if (length(cols) < 4) return(NULL)
  d <- master |>
    dplyr::filter(.data$year == yr) |>
    dplyr::group_by(.data$continent) |>
    dplyr::summarise(dplyr::across(dplyr::all_of(cols),
                                    \(x) mean(x, na.rm = TRUE)),
                     .groups = "drop") |>
    dplyr::filter(!is.na(.data$continent))
  if (!nrow(d)) return(NULL)
  long <- tidyr::pivot_longer(d, -.data$continent,
                                names_to = "hc", values_to = "share")
  long$hc <- factor(long$hc, levels = cols,
    labels = c("HC1 \u6cbb\u7597", "HC2 \u5eb7\u590d", "HC3 \u62a4\u7406",
               "HC4 \u8f85\u52a9", "HC5 \u836f\u54c1", "HC6 \u9884\u9632",
               "HC7 \u7ba1\u7406", "HC9 \u5176\u4ed6")[seq_along(cols)])
  ggplot2::ggplot(long, ggplot2::aes(.data$continent, .data$share,
                                      fill = .data$hc)) +
    ggplot2::geom_col(position = "fill", width = .7) +
    ggplot2::scale_y_continuous(labels = scales::label_percent()) +
    ggplot2::scale_fill_brewer(palette = "Spectral", name = NULL) +
    ggplot2::labs(
      title = sprintf("HC \u7528\u9014\u5206\u7c7b\u7ed3\u6784 \u00b7 %d", yr),
      subtitle = "\u5404\u5927\u6d32\u5747\u503c\uff1b\u9884\u9632\u4f9d\u7136\u504f\u4f4e\u3002",
      x = NULL, y = NULL,
      caption = "WHO GHED \u00b7 hc1_che..hc9_che"
    ) + .gxm_theme() +
    ggplot2::theme(legend.position = "bottom")
}

# ---- 11. OOPS vs GDP per capita scatter ----
.gxm_oops_gdp <- function(master) {
  yr <- .gxm_latest_year(master)
  d <- master |>
    dplyr::filter(.data$year == yr,
                  is.finite(.data$hf3_che),
                  is.finite(.data$gdp_pc_usd))
  if (nrow(d) < 30) return(NULL)
  ggplot2::ggplot(d, ggplot2::aes(.data$gdp_pc_usd, .data$hf3_che,
                                    color = .data$continent)) +
    ggplot2::geom_point(alpha = .8, size = 2.2) +
    ggplot2::geom_smooth(ggplot2::aes(group = 1), method = "loess",
                         color = "#1d3f5f", linewidth = .8,
                         linetype = "dashed", se = FALSE) +
    ggrepel::geom_text_repel(
      data = d |>
        dplyr::filter(.data$hf3_che > 60 |
                       .data$gdp_pc_usd > 50000),
      ggplot2::aes(label = .data$iso3_code), size = 3,
      color = "#0d121b", max.overlaps = 18) +
    ggplot2::scale_x_log10(labels = scales::label_dollar()) +
    ggplot2::scale_y_continuous(labels = scales::label_percent(scale = 1)) +
    ggplot2::scale_color_brewer(palette = "Set2", name = NULL) +
    ggplot2::labs(
      title = sprintf("OOPS \u5360\u6bd4 \u00d7 GDP/cap \u00b7 %d", yr),
      subtitle = "GDP/cap \u4e0a\u5347\uff0cOOPS \u5360\u6bd4\u9012\u51cf\uff0c\u6536\u655b\u8d8b\u52bf\u660e\u663e\u3002",
      x = "GDP per capita (USD, log)", y = "OOPS / CHE",
      caption = "WHO GHED + WB WDI"
    ) + .gxm_theme()
}

# ---- 12. OOPS vs life expectancy scatter ----
.gxm_oops_life <- function(master) {
  yr <- .gxm_latest_year(master)
  d <- master |>
    dplyr::filter(.data$year == yr,
                  is.finite(.data$hf3_che),
                  is.finite(.data$life_exp))
  if (nrow(d) < 30) return(NULL)
  ggplot2::ggplot(d, ggplot2::aes(.data$hf3_che, .data$life_exp,
                                    color = .data$continent)) +
    ggplot2::geom_point(alpha = .8, size = 2.1) +
    ggplot2::geom_smooth(ggplot2::aes(group = 1), method = "loess",
                         color = "#c46327", linewidth = .8,
                         linetype = "dashed", se = FALSE) +
    ggrepel::geom_text_repel(
      data = d |>
        dplyr::filter(.data$hf3_che > 65 |
                       .data$life_exp < 60),
      ggplot2::aes(label = .data$iso3_code), size = 3,
      color = "#0d121b", max.overlaps = 18) +
    ggplot2::scale_x_continuous(labels = scales::label_percent(scale = 1)) +
    ggplot2::scale_color_brewer(palette = "Set2", name = NULL) +
    ggplot2::labs(
      title = sprintf("OOPS \u5360\u6bd4 \u00d7 \u9884\u671f\u5bff\u547d \u00b7 %d", yr),
      subtitle = "OOPS \u8d8a\u9ad8\u5bff\u547d\u8d8a\u4f4e\uff1b\u8d22\u52a1\u4fdd\u62a4\u4e0e\u5065\u5eb7\u4ea7\u51fa\u5171\u8d70\u3002",
      x = "OOPS / CHE", y = "Life expectancy (years)",
      caption = "WHO GHED + WB WDI"
    ) + .gxm_theme()
}

# ---- 13. CHE per capita vs life expectancy log ----
.gxm_che_life_log <- function(master) {
  yr <- .gxm_latest_year(master)
  d <- master |>
    dplyr::filter(.data$year == yr,
                  is.finite(.data$che_pc_usd2023),
                  is.finite(.data$life_exp))
  if (nrow(d) < 30) return(NULL)
  ggplot2::ggplot(d, ggplot2::aes(.data$che_pc_usd2023, .data$life_exp,
                                    color = .data$income_group)) +
    ggplot2::geom_point(alpha = .8, size = 2.2) +
    ggplot2::geom_smooth(ggplot2::aes(group = 1), method = "lm",
                         color = "#1d3f5f", linetype = "dashed",
                         se = TRUE, alpha = .12) +
    ggrepel::geom_text_repel(
      data = d |>
        dplyr::filter(.data$che_pc_usd2023 >
                       stats::quantile(d$che_pc_usd2023, .9) |
                       .data$life_exp <
                       stats::quantile(d$life_exp, .1)),
      ggplot2::aes(label = .data$iso3_code), size = 3,
      color = "#0d121b", max.overlaps = 16) +
    ggplot2::scale_x_log10(labels = scales::label_dollar()) +
    ggplot2::scale_color_brewer(palette = "OrRd", name = NULL) +
    ggplot2::labs(
      title = sprintf("\u4eba\u5747 CHE \u00d7 \u9884\u671f\u5bff\u547d \u00b7 %d", yr),
      subtitle = "log \u6a2a\u8f74\u5448\u73b0\u9012\u51cf\u8fb9\u9645\u6536\u76ca\u3002",
      x = "CHE per capita (USD2023, log)", y = "Life expectancy (years)",
      caption = "WHO GHED + WB WDI"
    ) + .gxm_theme()
}

# ---- 14. CHE per capita vs U5MR log ----
.gxm_che_u5_log <- function(master) {
  yr <- .gxm_latest_year(master)
  d <- master |>
    dplyr::filter(.data$year == yr,
                  is.finite(.data$che_pc_usd2023),
                  is.finite(.data$u5mr))
  if (nrow(d) < 30) return(NULL)
  ggplot2::ggplot(d, ggplot2::aes(.data$che_pc_usd2023, .data$u5mr,
                                    color = .data$continent)) +
    ggplot2::geom_point(alpha = .8, size = 2.1) +
    ggplot2::geom_smooth(ggplot2::aes(group = 1), method = "lm",
                         color = "#c46327", linetype = "dashed",
                         se = TRUE, alpha = .12) +
    ggrepel::geom_text_repel(
      data = d |>
        dplyr::filter(.data$u5mr >
                       stats::quantile(d$u5mr, .9, na.rm = TRUE)),
      ggplot2::aes(label = .data$iso3_code), size = 3,
      color = "#0d121b", max.overlaps = 14) +
    ggplot2::scale_x_log10(labels = scales::label_dollar()) +
    ggplot2::scale_y_log10() +
    ggplot2::scale_color_brewer(palette = "Set2", name = NULL) +
    ggplot2::labs(
      title = sprintf("\u4eba\u5747 CHE \u00d7 5 \u5c81\u4ee5\u4e0b\u6b7b\u4ea1\u7387 \u00b7 %d", yr),
      subtitle = "\u53cc\u5bf9\u6570\u8f74\u5448\u73b0\u8d1f\u5e42\u5f8b\u5173\u7cfb\u3002",
      x = "CHE per capita (USD2023, log)", y = "U5MR (per 1000 live births, log)",
      caption = "WHO GHED + WB WDI"
    ) + .gxm_theme()
}

# ---- 15. continent CHE per capita boxplot latest year ----
.gxm_continent_che_box <- function(master) {
  yr <- .gxm_latest_year(master)
  d <- master |>
    dplyr::filter(.data$year == yr,
                  is.finite(.data$che_pc_usd2023),
                  !is.na(.data$continent))
  if (!nrow(d)) return(NULL)
  ggplot2::ggplot(d, ggplot2::aes(.data$continent, .data$che_pc_usd2023,
                                    fill = .data$continent)) +
    ggplot2::geom_boxplot(outlier.size = 0, alpha = .35) +
    ggplot2::geom_jitter(width = .12, alpha = .35, size = 1.1,
                         color = "#1d3f5f") +
    ggplot2::scale_y_log10(labels = scales::label_dollar()) +
    ggplot2::scale_fill_brewer(palette = "Set2", guide = "none") +
    ggplot2::labs(
      title = sprintf("\u4eba\u5747 CHE \u8de8\u5927\u6d32\u5206\u5e03 \u00b7 %d", yr),
      subtitle = "\u5217\u5185\u70b9 = \u56fd\u5bb6\uff1blog \u7eb5\u8f74\u4f7f\u9ad8\u4f4e\u540c\u5728\u4e00\u56fe\u3002",
      x = NULL, y = "CHE per capita (USD2023, log)",
      caption = "WHO GHED"
    ) + .gxm_theme()
}

# ---- 16. CHE/cap year-over-year YoY heatmap (top 30 by CHE) ----
.gxm_yoy_heatmap <- function(master) {
  ranking <- master |>
    dplyr::filter(.data$year == .gxm_latest_year(master),
                  is.finite(.data$che_pc_usd2023)) |>
    dplyr::arrange(dplyr::desc(.data$che_pc_usd2023)) |>
    utils::head(30) |>
    dplyr::pull(.data$iso3_code)
  d <- master |>
    dplyr::filter(.data$iso3_code %in% ranking,
                  is.finite(.data$che_pc_usd2023)) |>
    dplyr::arrange(.data$iso3_code, .data$year) |>
    dplyr::group_by(.data$iso3_code) |>
    dplyr::mutate(yoy = c(NA_real_, diff(log(.data$che_pc_usd2023)))) |>
    dplyr::ungroup() |>
    dplyr::filter(is.finite(.data$yoy))
  if (!nrow(d)) return(NULL)
  d$country_name <- factor(d$country_name,
    levels = rev(unique(d$country_name[order(d$iso3_code)])))
  ggplot2::ggplot(d, ggplot2::aes(.data$year, .data$country_name,
                                    fill = .data$yoy)) +
    ggplot2::geom_tile(color = "#fbf6ee", linewidth = .15) +
    ggplot2::scale_fill_distiller(palette = "RdBu", direction = 1,
                                    name = "YoY",
                                    labels = scales::label_percent(),
                                    limits = c(-0.2, 0.2),
                                    oob = scales::squish) +
    ggplot2::scale_x_continuous(breaks = seq(2002, 2023, 3), expand = c(0, 0)) +
    ggplot2::labs(
      title = "\u4eba\u5747 CHE \u540c\u6bd4\u589e\u901f \u00b7 \u603b\u989d\u524d 30 \u56fd",
      subtitle = "\u8d1f\u503c = \u8d2c\u503c\u540e\u4e0b\u964d\uff1b 2008\u30012020 \u4e24\u8f6e\u51b2\u51fb\u6e05\u6670\u53ef\u89c1\u3002",
      x = NULL, y = NULL,
      caption = "WHO GHED \u00b7 \u5e74\u5bf9\u6570\u5dee\u5206"
    ) + .gxm_theme()
}

# ---- 17. efficiency frontier facet by continent ----
.gxm_efficiency_frontier <- function(master) {
  yr <- .gxm_latest_year(master)
  d <- master |>
    dplyr::filter(.data$year == yr,
                  is.finite(.data$che_pc_usd2023),
                  is.finite(.data$life_exp),
                  !is.na(.data$continent))
  if (!nrow(d)) return(NULL)
  ggplot2::ggplot(d, ggplot2::aes(.data$che_pc_usd2023, .data$life_exp)) +
    ggplot2::geom_point(alpha = .8, size = 1.8, color = "#1d3f5f") +
    ggplot2::geom_smooth(method = "loess", se = TRUE, color = "#c46327",
                         linewidth = .9, alpha = .12) +
    ggplot2::facet_wrap(~ .data$continent, scales = "free", ncol = 3) +
    ggplot2::scale_x_log10(labels = scales::label_dollar()) +
    ggplot2::labs(
      title = sprintf("\u6548\u7387\u524d\u6cbf \u00b7 \u6309\u5927\u6d32\u00b7%d", yr),
      subtitle = "\u540c\u4e00\u4eba\u5747 CHE \u4e0b\uff0c\u4e0d\u540c\u5927\u6d32\u4ea7\u51fa\u5b58\u5728\u7cfb\u7edf\u5dee\u5f02\u3002",
      x = "CHE per capita (USD2023, log)", y = "Life expectancy",
      caption = "WHO GHED + WB WDI \u00b7 loess"
    ) + .gxm_theme()
}

# ---- 18. life expectancy residual after controlling GDP ----
.gxm_life_residual <- function(master) {
  yr <- .gxm_latest_year(master)
  d <- master |>
    dplyr::filter(.data$year == yr,
                  is.finite(.data$life_exp),
                  is.finite(.data$gdp_pc_usd))
  if (nrow(d) < 30) return(NULL)
  d$lgdp <- log(pmax(d$gdp_pc_usd, 1))
  fit <- stats::lm(life_exp ~ lgdp, data = d)
  d$resid <- stats::residuals(fit)
  d <- d[order(d$resid), ]
  bottom <- utils::head(d, 12)
  top <- utils::tail(d, 12)
  use <- dplyr::bind_rows(bottom, top)
  use$role <- ifelse(use$resid > 0,
                     "\u9ad8\u4e8e\u9884\u671f", "\u4f4e\u4e8e\u9884\u671f")
  use$country_name <- factor(use$country_name,
    levels = use$country_name[order(use$resid)])
  ggplot2::ggplot(use, ggplot2::aes(.data$resid, .data$country_name,
                                      fill = .data$role)) +
    ggplot2::geom_col(width = .68) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%+.1f", .data$resid)),
                       hjust = ifelse(use$resid > 0, -0.1, 1.1),
                       size = 3, color = "#0d121b") +
    ggplot2::scale_fill_manual(values = c("\u9ad8\u4e8e\u9884\u671f" = "#2a857a",
                                            "\u4f4e\u4e8e\u9884\u671f" = "#c46327"),
                                 name = NULL) +
    ggplot2::labs(
      title = sprintf("\u63a7\u5236 GDP \u540e\u7684\u9884\u671f\u5bff\u547d\u6b8b\u5dee \u00b7 %d", yr),
      subtitle = "\u9ad8\u4f4e\u5404 12 \u56fd\uff1b\u6b8b\u5dee = \u5b9e\u9645\u5bff\u547d \u2212 \u4ec5\u7531 GDP \u9884\u6d4b\u3002",
      x = "\u5bff\u547d\u6b8b\u5dee\uff08\u5e74\uff09", y = NULL,
      caption = "OLS\uff1alife_exp ~ log(gdp_pc_usd)"
    ) + .gxm_theme()
}

# ---- 19. income group OOPS box by decade ----
.gxm_income_oops_decade <- function(master) {
  d <- master |>
    dplyr::filter(is.finite(.data$hf3_che),
                  !is.na(.data$income_group)) |>
    dplyr::mutate(decade = dplyr::case_when(
      .data$year >= 2000 & .data$year <= 2009 ~ "2000s",
      .data$year >= 2010 & .data$year <= 2019 ~ "2010s",
      .data$year >= 2020 ~ "2020s",
      TRUE ~ NA_character_
    )) |>
    dplyr::filter(!is.na(.data$decade))
  if (!nrow(d)) return(NULL)
  d$income_group <- factor(d$income_group,
    levels = c("Low income", "Lower middle income",
               "Upper middle income", "High income"))
  ggplot2::ggplot(d, ggplot2::aes(.data$decade, .data$hf3_che,
                                    fill = .data$decade)) +
    ggplot2::geom_boxplot(alpha = .65, outlier.size = .8) +
    ggplot2::facet_wrap(~ .data$income_group, ncol = 4) +
    ggplot2::scale_fill_brewer(palette = "PuRd", guide = "none") +
    ggplot2::scale_y_continuous(labels = scales::label_percent(scale = 1)) +
    ggplot2::labs(
      title = "OOPS \u5360 CHE \u00b7 \u6536\u5165\u7ec4 \u00d7 \u5341\u5e74\u671f",
      subtitle = "\u8de8\u5341\u5e74\u671f\u8de8\u6536\u5165\u7ec4\u5bf9\u6bd4\uff1b\u4f4e\u6536\u5165\u7ec4 OOPS \u4ecd\u504f\u9ad8\u3002",
      x = NULL, y = "OOPS / CHE",
      caption = "WHO GHED \u00b7 hf3_che"
    ) + .gxm_theme()
}

# ---- 20. continent OOPS weighted average over time stacked area ----
.gxm_continent_oops_area <- function(master) {
  d <- master |>
    dplyr::filter(is.finite(.data$hf3_che),
                  is.finite(.data$che_usd2023),
                  !is.na(.data$continent)) |>
    dplyr::group_by(.data$continent, .data$year) |>
    dplyr::summarise(oops = stats::weighted.mean(.data$hf3_che,
                                                  .data$che_usd2023,
                                                  na.rm = TRUE),
                     .groups = "drop")
  if (!nrow(d)) return(NULL)
  ggplot2::ggplot(d, ggplot2::aes(.data$year, .data$oops,
                                    color = .data$continent)) +
    ggplot2::geom_line(linewidth = 1.1) +
    ggplot2::geom_point(size = 1.6) +
    ggplot2::scale_x_continuous(breaks = seq(2000, 2023, 4)) +
    ggplot2::scale_y_continuous(labels = scales::label_percent(scale = 1)) +
    ggplot2::scale_color_brewer(palette = "Set2", name = NULL) +
    ggplot2::labs(
      title = "\u8de8\u5927\u6d32 OOPS \u5360\u6bd4\u8d8b\u52bf \u00b7 2000\u20132023",
      subtitle = "\u4ee5 CHE \u4e3a\u6743\u91cd\u52a0\u6743\u5e73\u5747\u3002",
      x = NULL, y = "OOPS / CHE",
      caption = "WHO GHED \u00b7 hf3_che \u00b7 CHE \u52a0\u6743"
    ) + .gxm_theme()
}

# ---- 21. continent GGHED weighted average over time ----
.gxm_continent_gghed_area <- function(master) {
  d <- master |>
    dplyr::filter(is.finite(.data$gghed_che),
                  is.finite(.data$che_usd2023),
                  !is.na(.data$continent)) |>
    dplyr::group_by(.data$continent, .data$year) |>
    dplyr::summarise(gghed = stats::weighted.mean(.data$gghed_che,
                                                   .data$che_usd2023,
                                                   na.rm = TRUE),
                     .groups = "drop")
  if (!nrow(d)) return(NULL)
  ggplot2::ggplot(d, ggplot2::aes(.data$year, .data$gghed,
                                    color = .data$continent)) +
    ggplot2::geom_line(linewidth = 1.1) +
    ggplot2::geom_point(size = 1.6) +
    ggplot2::scale_x_continuous(breaks = seq(2000, 2023, 4)) +
    ggplot2::scale_y_continuous(labels = scales::label_percent(scale = 1)) +
    ggplot2::scale_color_brewer(palette = "Set2", name = NULL) +
    ggplot2::labs(
      title = "\u8de8\u5927\u6d32\u653f\u5e9c\u7b79\u8d44\u5360\u6bd4\u8d8b\u52bf \u00b7 2000\u20132023",
      subtitle = "GGHED / CHE\uff1bCHE \u52a0\u6743\u5e73\u5747\u3002",
      x = NULL, y = "GGHED / CHE",
      caption = "WHO GHED \u00b7 gghed_che"
    ) + .gxm_theme()
}

# ---- 22. global HF1/HF2/HF3 stacked area share over time ----
.gxm_global_hf_share <- function(master) {
  d <- master |>
    dplyr::filter(is.finite(.data$hf1_usd2023),
                  is.finite(.data$hf2_usd2023),
                  is.finite(.data$hf3_usd2023)) |>
    dplyr::group_by(.data$year) |>
    dplyr::summarise(HF1 = sum(.data$hf1_usd2023, na.rm = TRUE),
                     HF2 = sum(.data$hf2_usd2023, na.rm = TRUE),
                     HF3 = sum(.data$hf3_usd2023, na.rm = TRUE),
                     .groups = "drop")
  if (!nrow(d)) return(NULL)
  long <- tidyr::pivot_longer(d, -.data$year,
                                names_to = "scheme", values_to = "v")
  long$scheme <- factor(long$scheme, levels = c("HF1", "HF2", "HF3"),
    labels = c("HF1 \u653f\u5e9c\u8ba1\u5212",
               "HF2 \u4fdd\u9669\u8ba1\u5212",
               "HF3 \u81ea\u4ed8 OOPS"))
  ggplot2::ggplot(long, ggplot2::aes(.data$year, .data$v, fill = .data$scheme)) +
    ggplot2::geom_area(position = "fill", alpha = .85) +
    ggplot2::scale_fill_manual(values = c("HF1 \u653f\u5e9c\u8ba1\u5212" = "#1d3f5f",
                                            "HF2 \u4fdd\u9669\u8ba1\u5212" = "#2a857a",
                                            "HF3 \u81ea\u4ed8 OOPS" = "#c46327"),
                                 name = NULL) +
    ggplot2::scale_y_continuous(labels = scales::label_percent()) +
    ggplot2::scale_x_continuous(breaks = seq(2000, 2023, 4)) +
    ggplot2::labs(
      title = "\u5168\u7403\u4e09\u6e90\u5360\u6bd4\u52a8\u6001 \u00b7 2000\u20132023",
      subtitle = "\u603b\u989d\u52a0\u603b\u540e\u53d6\u5360\u6bd4\uff1bHF1/HF2 \u4e0a\u5347\uff0cHF3 \u4e0b\u964d\u3002",
      x = NULL, y = NULL,
      caption = "WHO GHED \u00b7 hf1/2/3_usd2023"
    ) + .gxm_theme() + ggplot2::theme(legend.position = "bottom")
}

# ---- 23. CHE per capita ridges by income group latest year ----
.gxm_income_ridges <- function(master) {
  yr <- .gxm_latest_year(master)
  d <- master |>
    dplyr::filter(.data$year == yr,
                  is.finite(.data$che_pc_usd2023),
                  !is.na(.data$income_group))
  if (!nrow(d)) return(NULL)
  d$income_group <- factor(d$income_group,
    levels = c("High income", "Upper middle income",
               "Lower middle income", "Low income"))
  ggplot2::ggplot(d, ggplot2::aes(.data$che_pc_usd2023,
                                    .data$income_group,
                                    fill = .data$income_group)) +
    ggridges::geom_density_ridges(alpha = .55, scale = 2.4,
                                  rel_min_height = .015,
                                  color = "white", linewidth = .25) +
    ggplot2::scale_x_log10(labels = scales::label_dollar()) +
    ggplot2::scale_fill_brewer(palette = "OrRd", guide = "none") +
    ggplot2::labs(
      title = sprintf("\u4eba\u5747 CHE \u00b7 %d \u5e74\u00b7\u6309\u6536\u5165\u7ec4\u5c71\u810a\u56fe", yr),
      subtitle = "\u9ad8\u4f4e\u6536\u5165\u7ec4\u8de8\u5ea6\u4e0d\u91cd\u53e0\uff1b\u4eba\u5747\u8d44\u91d1\u5dee\u5f02\u8d8a\u662f\u4e00\u4e2a\u6570\u91cf\u7ea7\u3002",
      x = "CHE per capita (USD2023, log)", y = NULL,
      caption = "WHO GHED"
    ) + .gxm_theme()
}

# ---- 24. OOPS top10 vs bottom10 (latest year) ----
.gxm_oops_top_bottom <- function(master) {
  yr <- .gxm_latest_year(master)
  d <- master |>
    dplyr::filter(.data$year == yr, is.finite(.data$hf3_che)) |>
    dplyr::arrange(dplyr::desc(.data$hf3_che))
  if (nrow(d) < 20) return(NULL)
  top <- utils::head(d, 10)
  bot <- utils::tail(d, 10)
  top$role <- "Top 10 \u9ad8 OOPS"
  bot$role <- "Bottom 10 \u4f4e OOPS"
  bind <- dplyr::bind_rows(top, bot)
  bind$country_name <- factor(bind$country_name,
    levels = rev(bind$country_name[order(bind$hf3_che)]))
  ggplot2::ggplot(bind, ggplot2::aes(.data$hf3_che, .data$country_name,
                                       fill = .data$role)) +
    ggplot2::geom_col(width = .7) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.0f%%",
                                                     .data$hf3_che)),
                       hjust = -0.1, size = 3.1, color = "#0d121b") +
    ggplot2::scale_fill_manual(values = c("Top 10 \u9ad8 OOPS" = "#c46327",
                                            "Bottom 10 \u4f4e OOPS" = "#2a857a"),
                                 name = NULL) +
    ggplot2::scale_x_continuous(labels = scales::label_percent(scale = 1)) +
    ggplot2::expand_limits(x = max(bind$hf3_che) * 1.15) +
    ggplot2::labs(
      title = sprintf("OOPS Top 10 vs Bottom 10 \u00b7 %d", yr),
      subtitle = "\u8d22\u52a1\u4fdd\u62a4\u4e24\u6781\u56fd\u5bb6\u5bf9\u7167\u3002",
      x = "OOPS / CHE", y = NULL,
      caption = "WHO GHED \u00b7 hf3_che"
    ) + .gxm_theme()
}

# ---- 25. country compare panel: CHE per capita across CHN/IND/USA/BRA/NGA ----
.gxm_compare_panel <- function(master) {
  picks <- c("CHN", "IND", "USA", "BRA", "NGA")
  d <- master |>
    dplyr::filter(.data$iso3_code %in% picks,
                  is.finite(.data$che_pc_usd2023))
  if (!nrow(d)) return(NULL)
  ggplot2::ggplot(d, ggplot2::aes(.data$year, .data$che_pc_usd2023,
                                    color = .data$country_name)) +
    ggplot2::geom_line(linewidth = 1.1) +
    ggplot2::geom_point(size = 1.4) +
    ggplot2::scale_x_continuous(breaks = seq(2000, 2023, 4)) +
    ggplot2::scale_y_log10(labels = scales::label_dollar()) +
    ggplot2::scale_color_brewer(palette = "Set1", name = NULL) +
    ggplot2::labs(
      title = "5 \u56fd\u4eba\u5747 CHE \u52a8\u6001 \u00b7 CHN / IND / USA / BRA / NGA",
      subtitle = "log \u7eb5\u8f74\uff1b\u8de8\u4e09\u4e2a\u6570\u91cf\u7ea7\u53ef\u540c\u56fe\u5bf9\u6bd4\u3002",
      x = NULL, y = "CHE per capita (USD2023, log)",
      caption = "WHO GHED"
    ) + .gxm_theme()
}

# ---- 26. global CHE/GDP share by year ----
.gxm_che_gdp_share_world <- function(master) {
  d <- master |>
    dplyr::filter(is.finite(.data$che_usd2023),
                  is.finite(.data$gdp_pc_usd),
                  is.finite(.data$pop)) |>
    dplyr::mutate(gdp = .data$gdp_pc_usd * .data$pop) |>
    dplyr::group_by(.data$year) |>
    dplyr::summarise(che_total = sum(.data$che_usd2023, na.rm = TRUE),
                     gdp_total = sum(.data$gdp, na.rm = TRUE),
                     .groups = "drop") |>
    dplyr::mutate(share = .data$che_total / .data$gdp_total)
  if (!nrow(d)) return(NULL)
  ggplot2::ggplot(d, ggplot2::aes(.data$year, .data$share)) +
    ggplot2::geom_area(fill = "#2a857a", alpha = .25) +
    ggplot2::geom_line(color = "#2a857a", linewidth = 1.1) +
    ggplot2::geom_point(color = "#2a857a", size = 2) +
    ggplot2::scale_x_continuous(breaks = seq(2000, 2023, 4)) +
    ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 0.1)) +
    ggplot2::labs(
      title = "\u5168\u7403\u536b\u751f\u652f\u51fa\u5360 GDP \u6bd4\u91cd \u00b7 2000\u20132023",
      subtitle = "\u603b CHE / \u603b GDP\uff1b\u9879\u542b\u5916\u63f4\u4e0e\u8de8\u8d27\u5e01\u8c03\u6574\u3002",
      x = NULL, y = "CHE / GDP",
      caption = "WHO GHED + WB WDI"
    ) + .gxm_theme()
}

# ---- 27. forecast CAGR fan: continent average historical + linear extrapolation 2024-2030 ----
.gxm_continent_forecast <- function(master) {
  d <- master |>
    dplyr::filter(is.finite(.data$che_pc_usd2023),
                  !is.na(.data$continent)) |>
    dplyr::group_by(.data$continent, .data$year) |>
    dplyr::summarise(che = stats::weighted.mean(.data$che_pc_usd2023,
                                                .data$pop, na.rm = TRUE),
                     .groups = "drop")
  if (!nrow(d)) return(NULL)
  pieces <- split(d, d$continent)
  out <- lapply(pieces, function(g) {
    if (nrow(g) < 10) return(NULL)
    fit <- stats::lm(log(che) ~ year, data = g)
    yrs_f <- seq(max(g$year) + 1, max(g$year) + 7)
    pred <- exp(stats::predict(fit, newdata = data.frame(year = yrs_f)))
    rbind(
      data.frame(continent = g$continent[1], year = g$year, che = g$che,
                 type = "history", stringsAsFactors = FALSE),
      data.frame(continent = g$continent[1], year = yrs_f, che = pred,
                 type = "forecast", stringsAsFactors = FALSE)
    )
  })
  out <- do.call(rbind, Filter(Negate(is.null), out))
  if (is.null(out) || !nrow(out)) return(NULL)
  ggplot2::ggplot(out, ggplot2::aes(.data$year, .data$che,
                                     color = .data$continent,
                                     linetype = .data$type)) +
    ggplot2::geom_line(linewidth = 1.05) +
    ggplot2::geom_vline(xintercept = max(out$year[out$type == "history"]),
                        color = "#5d667a", linetype = "dotted") +
    ggplot2::scale_y_log10(labels = scales::label_dollar()) +
    ggplot2::scale_x_continuous(breaks = seq(2000, 2030, 5)) +
    ggplot2::scale_linetype_manual(values = c(history = "solid",
                                              forecast = "dashed"),
                                   guide = "none") +
    ggplot2::scale_color_brewer(palette = "Set2", name = NULL) +
    ggplot2::labs(
      title = "\u5404\u5927\u6d32\u4eba\u5747 CHE \u00b7 \u5386\u53f2\u4e0e 7 \u5e74\u7ebf\u6027\u6307\u6570\u5916\u63a8",
      subtitle = "\u865a\u7ebf = \u5916\u63a8\uff1b\u4ec5\u4f9b\u53c2\u8003\uff0c\u4e0d\u4f5c\u4e3a\u9884\u8a00\u3002",
      x = NULL, y = "CHE per capita (USD2023, log)",
      caption = "WHO GHED \u00b7 lm(log(che) ~ year)"
    ) + .gxm_theme()
}

# ---- 28. CHE / GDP rank top 25 latest year ----
.gxm_che_gdp_top <- function(master) {
  yr <- .gxm_latest_year(master)
  d <- master |>
    dplyr::filter(.data$year == yr,
                  is.finite(.data$che_pc_usd2023),
                  is.finite(.data$gdp_pc_usd))
  if (!nrow(d)) return(NULL)
  d$share <- d$che_pc_usd2023 / d$gdp_pc_usd
  d <- d[is.finite(d$share), ]
  d <- d[order(-d$share), ]
  top <- utils::head(d, 25)
  top$country_name <- factor(top$country_name,
    levels = rev(top$country_name))
  ggplot2::ggplot(top, ggplot2::aes(.data$share, .data$country_name,
                                      fill = .data$continent)) +
    ggplot2::geom_col(width = .7) +
    ggplot2::geom_text(ggplot2::aes(label = scales::percent(.data$share,
                                                              accuracy = 0.1)),
                       hjust = -0.1, size = 3, color = "#0d121b") +
    ggplot2::expand_limits(x = max(top$share) * 1.18) +
    ggplot2::scale_fill_brewer(palette = "Set2", name = NULL) +
    ggplot2::scale_x_continuous(labels = scales::label_percent(accuracy = 0.1)) +
    ggplot2::labs(
      title = sprintf("CHE / GDP \u6700\u9ad8\u7684 25 \u56fd \u00b7 %d", yr),
      subtitle = "\u8868\u793a\u8be5\u56fd\u636e\u70b9\u4e0a\u5e74\u4eba\u5747\u8d44\u91d1\u5360\u4eba\u5747\u4ea7\u51fa\u7684\u6bd4\u91cd\u3002",
      x = "CHE / GDP", y = NULL,
      caption = "WHO GHED + WB WDI"
    ) + .gxm_theme()
}

# ---- 29. global region share by GHED 2023 hf1/hf2/hf3 stacked bars ----
.gxm_region_hf_breakdown <- function(master) {
  yr <- .gxm_latest_year(master)
  d <- master |>
    dplyr::filter(.data$year == yr,
                  !is.na(.data$continent),
                  is.finite(.data$hf1_che),
                  is.finite(.data$hf2_che),
                  is.finite(.data$hf3_che)) |>
    dplyr::group_by(.data$continent) |>
    dplyr::summarise(HF1 = stats::weighted.mean(.data$hf1_che,
                                                  .data$che_usd2023,
                                                  na.rm = TRUE),
                     HF2 = stats::weighted.mean(.data$hf2_che,
                                                  .data$che_usd2023,
                                                  na.rm = TRUE),
                     HF3 = stats::weighted.mean(.data$hf3_che,
                                                  .data$che_usd2023,
                                                  na.rm = TRUE),
                     .groups = "drop")
  if (!nrow(d)) return(NULL)
  long <- tidyr::pivot_longer(d, -.data$continent,
                                names_to = "scheme", values_to = "share")
  long$scheme <- factor(long$scheme, levels = c("HF1", "HF2", "HF3"),
    labels = c("HF1 \u653f\u5e9c", "HF2 \u4fdd\u9669", "HF3 OOPS"))
  ggplot2::ggplot(long, ggplot2::aes(.data$continent, .data$share,
                                       fill = .data$scheme)) +
    ggplot2::geom_col(position = "fill", width = .7) +
    ggplot2::scale_y_continuous(labels = scales::label_percent()) +
    ggplot2::scale_fill_manual(values = c("HF1 \u653f\u5e9c" = "#1d3f5f",
                                            "HF2 \u4fdd\u9669" = "#2a857a",
                                            "HF3 OOPS" = "#c46327"),
                                 name = NULL) +
    ggplot2::labs(
      title = sprintf("\u5404\u5927\u6d32\u4e09\u6e90\u5360\u6bd4 \u00b7 %d", yr),
      subtitle = "CHE \u52a0\u6743\u5e73\u5747\uff1b\u533a\u8c03\u6574\u4ec5\u53d8\u51b7\u70ed\u8c03\u3002",
      x = NULL, y = NULL,
      caption = "WHO GHED \u00b7 hf1/hf2/hf3_che"
    ) + .gxm_theme() + ggplot2::theme(legend.position = "bottom")
}

# ---- 30. correlation between CHE_pc and life_exp / U5MR / OOPS over time ----
.gxm_correlation_overtime <- function(master) {
  d <- master |>
    dplyr::filter(is.finite(.data$che_pc_usd2023),
                  is.finite(.data$life_exp),
                  is.finite(.data$u5mr),
                  is.finite(.data$hf3_che)) |>
    dplyr::group_by(.data$year) |>
    dplyr::summarise(
      r_life = stats::cor(log(.data$che_pc_usd2023), .data$life_exp),
      r_u5   = stats::cor(log(.data$che_pc_usd2023), log(.data$u5mr)),
      r_oops = stats::cor(log(.data$che_pc_usd2023), .data$hf3_che),
      .groups = "drop"
    )
  if (!nrow(d)) return(NULL)
  long <- tidyr::pivot_longer(d, -.data$year,
                                names_to = "metric", values_to = "r")
  long$metric <- factor(long$metric,
    levels = c("r_life", "r_u5", "r_oops"),
    labels = c("ln CHE_pc \u00d7 \u5bff\u547d (+)",
               "ln CHE_pc \u00d7 ln U5MR (\u2212)",
               "ln CHE_pc \u00d7 OOPS (\u2212)"))
  ggplot2::ggplot(long, ggplot2::aes(.data$year, .data$r,
                                       color = .data$metric)) +
    ggplot2::geom_hline(yintercept = 0, color = "#5d667a",
                        linetype = "dashed") +
    ggplot2::geom_line(linewidth = 1.1) +
    ggplot2::geom_point(size = 1.6) +
    ggplot2::scale_x_continuous(breaks = seq(2000, 2023, 4)) +
    ggplot2::scale_color_manual(
      values = c("ln CHE_pc \u00d7 \u5bff\u547d (+)" = "#1d3f5f",
                 "ln CHE_pc \u00d7 ln U5MR (\u2212)" = "#c46327",
                 "ln CHE_pc \u00d7 OOPS (\u2212)" = "#2a857a"),
      name = NULL
    ) +
    ggplot2::labs(
      title = "CHE \u4e0e\u5065\u5eb7\u4ea7\u51fa\u7684\u6210\u4ef6\u76f8\u5173\u4fc2\u6570 \u00b7 2000\u20132023",
      subtitle = "\u8de8\u56fd\u622a\u9762\u76f8\u5173\u968f\u65f6\u95f4\u8be5\u53d8\u3002",
      x = NULL, y = "Pearson r",
      caption = "WHO GHED + WB WDI"
    ) + .gxm_theme()
}

# ---- 入口 ------------------------------------------------------------------

#' 第二批 30 张静态图扩展
#' @export
ghs_export_more <- function(master,
                            outputs_dir = file.path("分析输出", "图表"),
                            verbose = TRUE) {
  .gxm_pkgs()
  dir.create(outputs_dir, recursive = TRUE, showWarnings = FALSE)
  fig_dir <- outputs_dir
  save_to <- function(name, plot, w = 11, h = 6.4) {
    if (is.null(plot)) {
      if (verbose) cat(sprintf("[more] skip %s (no plot)\n", name))
      return(invisible(NULL))
    }
    save_fig(plot, name, width = w, height = h, dpi = 300, dir = fig_dir)
    if (verbose) cat(sprintf("[more] %s\n", name))
  }
  save_to("v2m_global_che_total",      .gxm_global_che_total(master),      11, 5.6)
  save_to("v2m_continent_cagr",        .gxm_continent_cagr(master),        10, 5.6)
  save_to("v2m_income_cagr",           .gxm_income_cagr(master),            9, 5.4)
  save_to("v2m_oops_dumbbell_decline", .gxm_oops_dumbbell_decline(master), 11, 8)
  save_to("v2m_oops_dumbbell_rise",    .gxm_oops_dumbbell_rise(master),    11, 8)
  save_to("v2m_che_top_growth",        .gxm_che_top_growth(master),        11, 9)
  save_to("v2m_life_change_top",       .gxm_life_change_top(master),       11, 8.5)
  save_to("v2m_u5mr_decline_top",      .gxm_u5mr_decline_top(master),      11, 8.5)
  save_to("v2m_ext_top_recipients",    .gxm_ext_top_recipients(master),    11, 8.5)
  save_to("v2m_hc_breakdown",          .gxm_hc_breakdown(master),          11, 6.5)
  save_to("v2m_oops_gdp",              .gxm_oops_gdp(master),              11, 6.4)
  save_to("v2m_oops_life",             .gxm_oops_life(master),             11, 6.4)
  save_to("v2m_che_life_log",          .gxm_che_life_log(master),          11, 6.4)
  save_to("v2m_che_u5_log",            .gxm_che_u5_log(master),            11, 6.4)
  save_to("v2m_continent_che_box",     .gxm_continent_che_box(master),     10, 6)
  save_to("v2m_yoy_heatmap",           .gxm_yoy_heatmap(master),           12, 9)
  save_to("v2m_efficiency_frontier",   .gxm_efficiency_frontier(master),   12, 8)
  save_to("v2m_life_residual",         .gxm_life_residual(master),         11, 7.5)
  save_to("v2m_income_oops_decade",    .gxm_income_oops_decade(master),    12, 6)
  save_to("v2m_continent_oops_area",   .gxm_continent_oops_area(master),   11, 6)
  save_to("v2m_continent_gghed_area",  .gxm_continent_gghed_area(master),  11, 6)
  save_to("v2m_global_hf_share",       .gxm_global_hf_share(master),       11, 5.6)
  save_to("v2m_income_ridges",         .gxm_income_ridges(master),         11, 6)
  save_to("v2m_oops_top_bottom",       .gxm_oops_top_bottom(master),       11, 7)
  save_to("v2m_compare_panel",         .gxm_compare_panel(master),         11, 6)
  save_to("v2m_che_gdp_share_world",   .gxm_che_gdp_share_world(master),   11, 5.6)
  save_to("v2m_continent_forecast",    .gxm_continent_forecast(master),    11, 6)
  save_to("v2m_che_gdp_top",           .gxm_che_gdp_top(master),           11, 8.5)
  save_to("v2m_region_hf_breakdown",   .gxm_region_hf_breakdown(master),   11, 5.6)
  save_to("v2m_correlation_overtime",  .gxm_correlation_overtime(master),  11, 5.6)
  if (exists("plot_country_profile", mode = "function")) {
    profile_fn <- get("plot_country_profile", mode = "function")
    for (iso in c("USA", "CHN", "JPN", "DEU", "ZAF", "IDN")) {
      if (!iso %in% master$iso3_code) next
      key <- sprintf("v2m_profile_%s", tolower(iso))
      pl <- tryCatch(profile_fn(master, iso), error = function(e) {
        if (verbose) message("[more] profile ", iso, " fail: ",
                             conditionMessage(e))
        NULL
      })
      save_to(key, pl, 14, 9)
    }
  }
  invisible(NULL)
}
