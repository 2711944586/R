# =============================================================================
# 程序/35_plots_shocks.R   —— 冲击与变点图集（B6 阶段）
# -----------------------------------------------------------------------------
# 5 个组：
#   A. COVID-19 冲击 (2019→2021/2022)              × 5
#   B. 2008 金融危机 (2007→2009)                   × 5
#   C. 国家级变点检测 / segmented                  × 5
#   D. 全球冲击事件标注 / 波动包络                 × 5
#   E. 恢复诊断                                    × 5
# 总：25 个 plot_shock_* + ghs_export_shocks() 导出器
# =============================================================================

if (!exists("ensure_pkgs", mode = "function")) {
  source(file.path("\u7a0b\u5e8f", "00_utils.R"))
}

.cap_shocks <- function(extra = NULL) {
  base <- "\u6570\u636e \u00b7 WHO GHED 2024 + WDI"
  if (is.null(extra) || !nzchar(extra)) base
  else paste0(base, " \u00b7 ", extra)
}

ghs_shock_events <- list(
  gfc      = list(label = "GFC 2008-09", x = 2008.5,
                  xmin = 2008, xmax = 2009),
  covid    = list(label = "COVID-19 2020-21", x = 2020.5,
                  xmin = 2020, xmax = 2021),
  recovery = list(label = "Recovery 2022-23", x = 2022.5,
                  xmin = 2022, xmax = 2023)
)

# 帮助：单变量 country-year 变化对比 ---------------------------------------
.shock_diff_panel <- function(master, var, y1, y2, group_col = "continent") {
  d1 <- master[master$year == y1, c("iso3_code", "country_name",
                                       group_col, var)]
  d2 <- master[master$year == y2, c("iso3_code", var)]
  m <- merge(d1, d2, by = "iso3_code", suffixes = c(".y1", ".y2"))
  m[[paste0(var, ".y1")]] <- as.numeric(m[[paste0(var, ".y1")]])
  m[[paste0(var, ".y2")]] <- as.numeric(m[[paste0(var, ".y2")]])
  m
}

# 帮助：分段线性
.segmented_slopes <- function(years, values, breakpoint) {
  ok <- is.finite(years) & is.finite(values)
  years <- years[ok]; values <- values[ok]
  if (length(years) < 4) return(NULL)
  d <- data.frame(year = years, val = values,
                  post = as.numeric(years > breakpoint))
  fit <- stats::lm(val ~ year + post + year:post, data = d)
  list(fit = fit,
       pred = data.frame(year = years,
                          pred = stats::predict(fit, newdata = d)))
}

# =============================================================================
# A. COVID-19 冲击
# =============================================================================

#' 2019→2021 \u4eba\u5747 CHE \u53d8\u5316 \u00b7 \u6563\u70b9
plot_shock_covid_chepc <- function(master, y1 = 2019, y2 = 2021) {
  ensure_pkgs(c("ggplot2"))
  m <- .shock_diff_panel(master, "che_pc_usd2023", y1, y2)
  m$delta <- log(m$che_pc_usd2023.y2 / m$che_pc_usd2023.y1)
  m <- m[is.finite(m$delta) & is.finite(m$che_pc_usd2023.y1) &
         m$che_pc_usd2023.y1 > 0, ]
  if (!nrow(m)) return(ggplot2::ggplot() + ggplot2::theme_void())
  ggplot2::ggplot(m, ggplot2::aes(x = .data$che_pc_usd2023.y1,
                                      y = .data$delta,
                                      colour = .data$continent)) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2,
      colour = palette_ghs3("ink"), alpha = 0.5) +
    ggplot2::geom_point(alpha = 0.82, size = 2.4) +
    ggplot2::geom_smooth(method = "loess", se = FALSE, formula = y ~ x,
      colour = palette_ghs3("ink"), linewidth = 0.9) +
    ggplot2::scale_x_log10(labels = scales::dollar_format(accuracy = 1)) +
    ggplot2::scale_y_continuous(labels = scales::number_format(
      accuracy = 0.1, suffix = " (log\u00d7)")) +
    scale_colour_brand_continent(name = NULL) +
    theme_ghs3() +
    ggplot2::theme(legend.position = "top") +
    labs_news(
      title = sprintf("COVID \u00b7 \u4eba\u5747 CHE \u53d8\u5316 %d\u2192%d",
                       y1, y2),
      subtitle = "log\u500d\u6570\uff1b\u5219\u6b27\u7f8e\u9ad8\u53d8\u52a8\uff0c\u8f83\u591a\u56fd\u5bb6\u51fa\u73b0\u8868\u73b0\u9000\u6b65",
      x = sprintf("\u4eba\u5747 CHE %d (log)", y1),
      y = "log(\u500d\u6570)",
      caption = .cap_shocks())
}

#' 2019→2021 OOP / CHE \u53d8\u5316 \u00b7 \u6563\u70b9
plot_shock_covid_oop <- function(master, y1 = 2019, y2 = 2021) {
  ensure_pkgs(c("ggplot2"))
  m <- .shock_diff_panel(master, "hf3_che", y1, y2)
  m$delta <- m$hf3_che.y2 - m$hf3_che.y1
  m <- m[is.finite(m$delta) & is.finite(m$hf3_che.y1), ]
  if (!nrow(m)) return(ggplot2::ggplot() + ggplot2::theme_void())
  ggplot2::ggplot(m, ggplot2::aes(x = .data$hf3_che.y1,
                                       y = .data$delta,
                                       colour = .data$continent)) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2,
      colour = palette_ghs3("ink"), alpha = 0.5) +
    ggplot2::geom_point(alpha = 0.82, size = 2.4) +
    ggplot2::scale_x_continuous(
        labels = scales::percent_format(accuracy = 1, scale = 1)) +
    ggplot2::scale_y_continuous(
        labels = scales::percent_format(accuracy = 1, scale = 1)) +
    scale_colour_brand_continent(name = NULL) +
    theme_ghs3() +
    ggplot2::theme(legend.position = "top") +
    labs_news(
      title = sprintf("COVID \u00b7 OOP / CHE \u53d8\u5316 %d\u2192%d",
                       y1, y2),
      subtitle = "\u8d1f\u503c = OOP \u4e0b\u964d\uff08\u63d0\u5347\uff09\uff1b\u9759\u671f\u504f\u8c03\u5f31",
      x = sprintf("OOP / CHE %d", y1),
      y = "\u53d8\u5316 (\u767e\u5206\u70b9)",
      caption = .cap_shocks())
}

#' COVID 期 CHE_pc 变化 dotplot \u00b7 收入组
plot_shock_covid_dot_income <- function(master, y1 = 2019, y2 = 2021) {
  ensure_pkgs(c("ggplot2"))
  m <- .shock_diff_panel(master, "che_pc_usd2023", y1, y2,
                          group_col = "income_group")
  m$delta <- log(m$che_pc_usd2023.y2 / m$che_pc_usd2023.y1)
  m <- m[is.finite(m$delta) & !is.na(m$income_group), ]
  if (!nrow(m)) return(ggplot2::ggplot() + ggplot2::theme_void())
  m$income_group <- factor(m$income_group,
    levels = c("Low income", "Lower middle income",
               "Upper middle income", "High income"))
  ggplot2::ggplot(m, ggplot2::aes(x = .data$income_group, y = .data$delta,
                                       fill = .data$income_group)) +
    ggplot2::geom_violin(alpha = 0.5, scale = "width") +
    ggplot2::geom_boxplot(width = 0.18, outlier.shape = NA,
      colour = "#1a1f28", fill = "white", alpha = 0.85) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2,
      colour = "#1a1f28", alpha = 0.55) +
    ggplot2::scale_y_continuous(labels = scales::number_format(
      accuracy = 0.1, suffix = " (log\u00d7)")) +
    scale_fill_brand_income(guide = "none") +
    theme_ghs3() +
    labs_news(
      title = sprintf("COVID \u00b7 \u4eba\u5747 CHE \u53d8\u5316\u5206\u5e03 %d\u2192%d",
                       y1, y2),
      subtitle = "\u4ee5log\u500d\u6570\u8861\u91cf\uff1b\u4f4e\u6536\u5165\u56fd\u504f\u53f3\u5219\u53d7\u51b2\u51fb\u8f83\u5c11",
      x = NULL, y = "log(\u500d\u6570)",
      caption = .cap_shocks())
}

#' 寿命下降：2019→2021 直接差
plot_shock_covid_lifeexp <- function(master, y1 = 2019, y2 = 2021) {
  ensure_pkgs(c("ggplot2"))
  m <- .shock_diff_panel(master, "life_exp", y1, y2)
  m$delta <- m$life_exp.y2 - m$life_exp.y1
  m <- m[is.finite(m$delta), ]
  if (!nrow(m)) return(ggplot2::ggplot() + ggplot2::theme_void())
  m <- m[order(m$delta), ]
  picked <- rbind(utils::head(m, 12), utils::tail(m, 12))
  picked$grp <- c(rep("\u6700\u5927\u4e0b\u964d", 12),
                   rep("\u6700\u5927\u63d0\u5347", 12))
  picked$country_name <- factor(picked$country_name,
    levels = picked$country_name[order(picked$delta)])
  ggplot2::ggplot(picked, ggplot2::aes(x = .data$delta, y = .data$country_name,
                                            fill = .data$grp)) +
    ggplot2::geom_col(alpha = 0.88) +
    ggplot2::scale_fill_manual(
      values = c("\u6700\u5927\u4e0b\u964d" = palette_ghs3("bad"),
                 "\u6700\u5927\u63d0\u5347" = palette_ghs3("good")),
      name = NULL) +
    theme_ghs3(grid = "x") +
    ggplot2::theme(legend.position = "top") +
    labs_news(
      title = sprintf("COVID \u00b7 \u5bff\u547d\u53d8\u5316 %d\u2192%d Top 12/12",
                       y1, y2),
      subtitle = "\u591a\u56fd\u4e0b\u964d 1-3 \u5e74\uff1b\u90e8\u5206\u4f4e\u4f9d\u8d56\u62a5\u544a\u56fd\u9006\u52bf\u4e0a\u5347",
      x = "\u5bff\u547d\u53d8\u5316\uff08\u5e74\uff09", y = NULL,
      caption = .cap_shocks("WDI life_exp"))
}

#' \u8f68\u8ff9：选定 4 国 2018→2022 路径
plot_shock_covid_track <- function(master,
                                    isos = c("USA", "GBR", "DEU", "BRA",
                                                "CHN", "IND")) {
  ensure_pkgs(c("ggplot2"))
  d <- master[master$iso3_code %in% isos & master$year >= 2018 &
              master$year <= 2022 & is.finite(master$che_pc_usd2023), ]
  if (!nrow(d)) return(ggplot2::ggplot() + ggplot2::theme_void())
  ggplot2::ggplot(d, ggplot2::aes(x = .data$year, y = .data$che_pc_usd2023,
                                       colour = .data$country_name)) +
    ggplot2::annotate("rect", xmin = 2020, xmax = 2021, ymin = -Inf,
                     ymax = Inf, fill = palette_ghs3("bad"), alpha = 0.10) +
    ggplot2::geom_line(linewidth = 1.3) +
    ggplot2::geom_point(size = 2.4) +
    ggplot2::scale_y_log10(labels = scales::dollar_format(accuracy = 1)) +
    ggplot2::scale_x_continuous(breaks = 2018:2022) +
    scale_colour_ghs3(name = NULL) +
    theme_ghs3() +
    ggplot2::theme(legend.position = "top") +
    labs_news(
      title = "COVID \u671f \u4eba\u5747 CHE \u8f68\u8ff9 \u00b7 6 \u56fd",
      subtitle = "\u9634\u5f71\u533a = 2020-21\uff1b\u4e2d\u9ad8\u6536\u5165\u56fd\u591a\u72ec\u7acb\u62b9\u5e73",
      x = NULL, y = "USD\u4eba\u5747 (log)",
      caption = .cap_shocks())
}

# =============================================================================
# B. 2008 金融危机
# =============================================================================

#' 2007→2009 \u4eba\u5747 CHE \u53d8\u5316
plot_shock_gfc_chepc <- function(master, y1 = 2007, y2 = 2009) {
  plot_shock_covid_chepc(master, y1, y2) +
    labs_news(
      title = sprintf("GFC \u00b7 \u4eba\u5747 CHE \u53d8\u5316 %d\u2192%d",
                       y1, y2),
      subtitle = "2008 \u91d1\u878d\u5371\u673a\u51b2\u51fb\u8de8\u56fd\u5dee\u5f02\u660e\u663e",
      x = sprintf("\u4eba\u5747 CHE %d (log)", y1),
      y = "log(\u500d\u6570)",
      caption = .cap_shocks())
}

#' 2007→2009 OOP \u53d8\u5316
plot_shock_gfc_oop <- function(master, y1 = 2007, y2 = 2009) {
  plot_shock_covid_oop(master, y1, y2) +
    labs_news(
      title = sprintf("GFC \u00b7 OOP \u53d8\u5316 %d\u2192%d", y1, y2),
      subtitle = "\u90e8\u5206\u56fd\u5bb6\u8d22\u52a1\u4fdd\u62a4\u51fa\u73b0\u660e\u663e\u9000\u6b65\u4fe1\u53f7",
      x = sprintf("OOP / CHE %d", y1),
      y = "\u53d8\u5316 (\u767e\u5206\u70b9)",
      caption = .cap_shocks())
}

#' GFC vs trend gap：2009 实际 vs 趋势线外推
plot_shock_gfc_trend_gap <- function(master) {
  ensure_pkgs(c("ggplot2"))
  # 每国 2000-2007 的对数线性拟合，外推到 2009，比 actual
  d <- master[is.finite(master$che_pc_usd2023) &
              master$che_pc_usd2023 > 0 &
              master$year >= 2000 & master$year <= 2009, ]
  if (!nrow(d)) return(ggplot2::ggplot() + ggplot2::theme_void())
  isos <- unique(d$iso3_code)
  rows <- list()
  for (iso in isos) {
    sub <- d[d$iso3_code == iso, ]
    pre <- sub[sub$year <= 2007, ]
    if (nrow(pre) < 4) next
    fit <- stats::lm(log(che_pc_usd2023) ~ year, data = pre)
    pred09 <- stats::predict(fit, newdata = data.frame(year = 2009))
    actual09 <- sub$che_pc_usd2023[sub$year == 2009]
    if (!length(actual09)) next
    rows[[iso]] <- data.frame(
      iso = iso,
      country = sub$country_name[1],
      continent = sub$continent[1],
      gap = log(actual09) - pred09)
  }
  if (!length(rows)) return(ggplot2::ggplot() + ggplot2::theme_void())
  df <- do.call(rbind, rows)
  df <- df[order(df$gap), ]
  picked <- rbind(utils::head(df, 12), utils::tail(df, 12))
  picked$country <- factor(picked$country, levels = picked$country)
  ggplot2::ggplot(picked, ggplot2::aes(x = .data$gap, y = .data$country,
                                            fill = .data$gap > 0)) +
    ggplot2::geom_col(alpha = 0.88) +
    ggplot2::scale_x_continuous(labels = scales::number_format(
      accuracy = 0.1, suffix = " (log\u00d7)")) +
    ggplot2::scale_fill_manual(
      values = c(`TRUE` = palette_ghs3("good"),
                 `FALSE` = palette_ghs3("bad")), guide = "none") +
    theme_ghs3(grid = "x") +
    labs_news(
      title = "GFC \u00b7 2009 \u5b9e\u9645 vs 2000-07 \u8d8b\u52bf\u9884\u671f",
      subtitle = "\u8d1f\u504f\u79bb = \u4f4e\u4e8e\u8d8b\u52bf\uff08\u51b2\u51fb\u660e\u663e\uff09\uff1b\u6b63\u504f\u79bb = \u52a0\u901f",
      x = "log(\u5b9e\u9645/\u9884\u671f)", y = NULL,
      caption = .cap_shocks())
}

#' 恢复年数：到达 2007 水平所需年数
plot_shock_gfc_recovery_years <- function(master) {
  ensure_pkgs(c("ggplot2"))
  d <- master[is.finite(master$che_pc_usd2023) &
              master$che_pc_usd2023 > 0 &
              master$year >= 2007, ]
  isos <- unique(d$iso3_code)
  rows <- list()
  for (iso in isos) {
    sub <- d[d$iso3_code == iso, ]
    base <- sub$che_pc_usd2023[sub$year == 2007]
    if (!length(base) || !is.finite(base)) next
    sub_post <- sub[sub$year >= 2008, ]
    if (!nrow(sub_post)) next
    over <- sub_post[sub_post$che_pc_usd2023 >= base, ]
    if (!nrow(over)) next
    recover_yr <- min(over$year)
    rows[[iso]] <- data.frame(
      iso = iso,
      country = sub$country_name[1],
      continent = sub$continent[1],
      years_to_recover = recover_yr - 2007)
  }
  if (!length(rows)) return(ggplot2::ggplot() + ggplot2::theme_void())
  df <- do.call(rbind, rows)
  ggplot2::ggplot(df, ggplot2::aes(x = .data$years_to_recover,
                                        fill = .data$continent)) +
    ggplot2::geom_histogram(binwidth = 1, colour = "white", alpha = 0.85,
      position = "stack") +
    scale_fill_brand_continent(name = NULL) +
    theme_ghs3() +
    ggplot2::theme(legend.position = "top") +
    labs_news(
      title = "GFC \u00b7 \u6062\u590d\u5230 2007 \u6c34\u5e73\u6240\u9700\u5e74\u6570",
      subtitle = "\u591a\u6570\u56fd\u5bb6 1-3 \u5e74\u6062\u590d\uff1b\u90e8\u5206\u8d4d\u5904\u5728 5+ \u5e74",
      x = "\u6062\u590d\u5e74\u6570 (post-2007)", y = "\u56fd\u5bb6\u6570",
      caption = .cap_shocks())
}

#' GFC 敏感度：收入组对比 boxplot
plot_shock_gfc_sensitivity <- function(master, y1 = 2007, y2 = 2009) {
  ensure_pkgs(c("ggplot2"))
  m <- .shock_diff_panel(master, "che_pc_usd2023", y1, y2,
                          group_col = "income_group")
  m$delta <- log(m$che_pc_usd2023.y2 / m$che_pc_usd2023.y1)
  m <- m[is.finite(m$delta) & !is.na(m$income_group), ]
  if (!nrow(m)) return(ggplot2::ggplot() + ggplot2::theme_void())
  m$income_group <- factor(m$income_group,
    levels = c("Low income", "Lower middle income",
               "Upper middle income", "High income"))
  ggplot2::ggplot(m, ggplot2::aes(x = .data$income_group, y = .data$delta,
                                       fill = .data$income_group)) +
    ggplot2::geom_violin(alpha = 0.5, scale = "width") +
    ggplot2::geom_boxplot(width = 0.18, outlier.shape = NA,
      colour = "#1a1f28", fill = "white", alpha = 0.85) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2,
      colour = "#1a1f28", alpha = 0.55) +
    ggplot2::scale_y_continuous(labels = scales::number_format(
      accuracy = 0.1, suffix = " (log\u00d7)")) +
    scale_fill_brand_income(guide = "none") +
    theme_ghs3() +
    labs_news(
      title = sprintf("GFC \u654f\u611f\u5ea6 \u00b7 \u6309\u6536\u5165\u7ec4 %d\u2192%d",
                       y1, y2),
      subtitle = "\u9ad8\u6536\u5165\u56fd\u53d7\u5ea6\u660e\u663e\uff1b\u4f4e\u6536\u5165\u56fd\u4f9d\u8d56\u5916\u90e8\u63f4\u52a9",
      x = NULL, y = "log(\u500d\u6570)",
      caption = .cap_shocks())
}

# =============================================================================
# C. 国家级变点 / segmented
# =============================================================================

#' 单国 segmented：以 2008 / 2020 为变点示意
plot_shock_segmented_chepc <- function(master, iso = "USA",
                                        breakpoint = 2008) {
  ensure_pkgs(c("ggplot2"))
  d <- master[master$iso3_code == iso &
              is.finite(master$che_pc_usd2023) &
              master$che_pc_usd2023 > 0, ]
  if (!nrow(d)) return(ggplot2::ggplot() + ggplot2::theme_void())
  seg <- .segmented_slopes(d$year, log(d$che_pc_usd2023), breakpoint)
  if (is.null(seg)) return(ggplot2::ggplot() + ggplot2::theme_void())
  pred <- seg$pred
  name <- unique(d$country_name)[1]
  ggplot2::ggplot(d, ggplot2::aes(x = .data$year)) +
    ggplot2::geom_vline(xintercept = breakpoint, linetype = 2,
      colour = palette_ghs3("bad"), linewidth = 0.7) +
    ggplot2::geom_point(ggplot2::aes(y = log(.data$che_pc_usd2023)),
      colour = palette_ghs3("primary"), size = 2) +
    ggplot2::geom_line(data = pred,
      ggplot2::aes(x = .data$year, y = .data$pred),
      colour = palette_ghs3("primary"), linewidth = 1.2) +
    theme_ghs3() +
    labs_news(
      title = sprintf("%s \u00b7 log\u4eba\u5747 CHE segmented \u00b7 \u53d8\u70b9 %d",
                       name, breakpoint),
      subtitle = "\u4e24\u6bb5\u5206\u522b\u62df\u5408\uff1b\u659c\u7387\u53d8\u5316\u53cd\u6620\u589e\u957f\u52a8\u80fd\u53d8\u8c03",
      x = NULL, y = "log(\u4eba\u5747 CHE)",
      caption = .cap_shocks("piecewise OLS"))
}

#' 多国 segmented small multiples（COVID 变点）
plot_shock_segmented_multi <- function(master,
                                       isos = c("USA", "CHN", "IND", "BRA",
                                                  "DEU", "JPN")) {
  ensure_pkgs(c("ggplot2"))
  rows <- list()
  preds <- list()
  for (iso in isos) {
    d <- master[master$iso3_code == iso &
                is.finite(master$che_pc_usd2023) &
                master$che_pc_usd2023 > 0 & master$year >= 2010, ]
    if (nrow(d) < 6) next
    seg <- .segmented_slopes(d$year, log(d$che_pc_usd2023), 2019)
    if (is.null(seg)) next
    rows[[iso]] <- data.frame(year = d$year, lc = log(d$che_pc_usd2023),
                                country = d$country_name[1])
    preds[[iso]] <- data.frame(year = d$year,
                                 lc = seg$pred$pred,
                                 country = d$country_name[1])
  }
  if (!length(rows)) return(ggplot2::ggplot() + ggplot2::theme_void())
  rdf <- do.call(rbind, rows)
  pdf <- do.call(rbind, preds)
  ggplot2::ggplot(rdf, ggplot2::aes(x = .data$year, y = .data$lc)) +
    ggplot2::geom_vline(xintercept = 2019, linetype = 2,
      colour = palette_ghs3("bad"), linewidth = 0.6) +
    ggplot2::geom_point(colour = palette_ghs3("primary"), size = 1.7) +
    ggplot2::geom_line(data = pdf, ggplot2::aes(x = .data$year, y = .data$lc),
      colour = palette_ghs3("primary"), linewidth = 1) +
    ggplot2::facet_wrap(~ .data$country, ncol = 3, scales = "free_y") +
    theme_ghs3() +
    labs_news(
      title = "Segmented \u00b7 \u53d8\u70b9 2019",
      subtitle = "\u5404\u56fd\u5728 COVID \u524d\u540e \u659c\u7387\u53d8\u5316 (\u8868\u793a\u8d8b\u52bf\u91cd\u8c03)",
      x = NULL, y = "log(\u4eba\u5747 CHE)",
      caption = .cap_shocks("piecewise OLS"))
}

#' 变点幅度排序 dotplot
plot_shock_segmented_dot <- function(master, breakpoint = 2019) {
  ensure_pkgs(c("ggplot2"))
  isos <- unique(master$iso3_code)
  rows <- list()
  for (iso in isos) {
    d <- master[master$iso3_code == iso &
                is.finite(master$che_pc_usd2023) &
                master$che_pc_usd2023 > 0 & master$year >= 2012 &
                master$year <= 2022, ]
    if (nrow(d) < 6) next
    seg <- .segmented_slopes(d$year, log(d$che_pc_usd2023), breakpoint)
    if (is.null(seg)) next
    co <- stats::coef(seg$fit)
    delta <- unname(co["year:post"])
    if (!is.finite(delta)) next
    rows[[iso]] <- data.frame(
      iso = iso,
      country = d$country_name[1],
      delta_slope = delta)
  }
  if (!length(rows)) return(ggplot2::ggplot() + ggplot2::theme_void())
  df <- do.call(rbind, rows)
  df <- df[order(df$delta_slope), ]
  picked <- rbind(utils::head(df, 10), utils::tail(df, 10))
  picked$country <- factor(picked$country, levels = picked$country)
  ggplot2::ggplot(picked, ggplot2::aes(x = .data$delta_slope,
                                            y = .data$country,
                                            fill = .data$delta_slope > 0)) +
    ggplot2::geom_col(alpha = 0.88) +
    ggplot2::scale_fill_manual(
      values = c(`TRUE` = palette_ghs3("good"),
                 `FALSE` = palette_ghs3("bad")), guide = "none") +
    theme_ghs3(grid = "x") +
    labs_news(
      title = sprintf("Segmented \u00b7 %d \u540e\u659c\u7387\u53d8\u5316 Top/Bot 10",
                       breakpoint),
      subtitle = "\u6b63\u503c = \u53d8\u70b9\u540e\u589e\u901f\u52a0\u5feb\uff1b\u8d1f\u503c = \u653e\u7f13",
      x = "\u659c\u7387\u53d8\u5316 (log/yr)", y = NULL,
      caption = .cap_shocks("year:post \u4ea4\u4e92\u9879"))
}

#' Pre-2019 vs post-2019 增速 散点
plot_shock_pre_post_slope <- function(master, breakpoint = 2019) {
  ensure_pkgs(c("ggplot2"))
  isos <- unique(master$iso3_code)
  rows <- list()
  for (iso in isos) {
    d <- master[master$iso3_code == iso &
                is.finite(master$che_pc_usd2023) &
                master$che_pc_usd2023 > 0, ]
    pre <- d[d$year >= 2012 & d$year <= breakpoint, ]
    post <- d[d$year > breakpoint, ]
    if (nrow(pre) < 4 || nrow(post) < 2) next
    s1 <- tryCatch(unname(stats::coef(stats::lm(log(che_pc_usd2023) ~ year,
                                                   data = pre))[2]),
                   error = function(e) NA_real_)
    s2 <- tryCatch(unname(stats::coef(stats::lm(log(che_pc_usd2023) ~ year,
                                                   data = post))[2]),
                   error = function(e) NA_real_)
    rows[[iso]] <- data.frame(iso = iso, country = d$country_name[1],
                                continent = d$continent[1],
                                pre = s1, post = s2)
  }
  if (!length(rows)) return(ggplot2::ggplot() + ggplot2::theme_void())
  df <- do.call(rbind, rows)
  df <- df[is.finite(df$pre) & is.finite(df$post), ]
  ggplot2::ggplot(df, ggplot2::aes(x = .data$pre, y = .data$post,
                                        colour = .data$continent)) +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = 2,
      colour = palette_ghs3("ink"), alpha = 0.5) +
    ggplot2::geom_hline(yintercept = 0, linetype = 3,
      colour = palette_ghs3("ink"), alpha = 0.35) +
    ggplot2::geom_vline(xintercept = 0, linetype = 3,
      colour = palette_ghs3("ink"), alpha = 0.35) +
    ggplot2::geom_point(alpha = 0.82, size = 2.4) +
    scale_colour_brand_continent(name = NULL) +
    theme_ghs3() +
    ggplot2::theme(legend.position = "top") +
    labs_news(
      title = sprintf("Pre-%d vs Post-%d \u589e\u901f \u00b7 \u8de8\u56fd\u6563\u70b9",
                       breakpoint, breakpoint),
      subtitle = "\u5bf9\u89d2\u7ebf = \u4fdd\u6301; \u53f3\u4e0a\u65b9 = \u52a0\u901f\uff1b\u5de6\u4e0b\u65b9 = \u9000\u6b65",
      x = "Pre slope (log/yr)", y = "Post slope (log/yr)",
      caption = .cap_shocks("OLS"))
}

#' \u533a\u95f4\u62df\u5408\u53e0\u52a0\u70b9：单国全期
plot_shock_segment_fit <- function(master, iso = "CHN") {
  ensure_pkgs(c("ggplot2"))
  d <- master[master$iso3_code == iso &
              is.finite(master$che_pc_usd2023) &
              master$che_pc_usd2023 > 0, ]
  if (!nrow(d)) return(ggplot2::ggplot() + ggplot2::theme_void())
  bp <- c(2008, 2019)
  d$seg <- cut(d$year,
    breaks = c(-Inf, bp, Inf),
    labels = c("Phase1\uff082000-08\uff09",
               "Phase2\uff082009-19\uff09",
               "Phase3\uff082020+\uff09"))
  name <- unique(d$country_name)[1]
  ggplot2::ggplot(d, ggplot2::aes(x = .data$year, y = .data$che_pc_usd2023)) +
    ggplot2::geom_point(ggplot2::aes(colour = .data$seg),
      size = 2.5, alpha = 0.85) +
    ggplot2::geom_smooth(ggplot2::aes(group = .data$seg, colour = .data$seg),
      method = "lm", se = FALSE, formula = y ~ x, linewidth = 1.2) +
    ggplot2::scale_y_log10(labels = scales::dollar_format(accuracy = 1)) +
    scale_colour_ghs3(name = NULL) +
    theme_ghs3() +
    ggplot2::theme(legend.position = "top") +
    labs_news(
      title = sprintf("%s \u00b7 3 \u6bb5\u62df\u5408 (\u53d8\u70b9 2008/2019)",
                       name),
      subtitle = "log \u8f74\uff1b\u4e09\u4e2a\u6bb5\u7684\u659c\u7387 = \u4e09\u79cd\u5236\u5ea6\u8282\u594f",
      x = NULL, y = "USD\u4eba\u5747 (log)",
      caption = .cap_shocks())
}

# =============================================================================
# D. 全球冲击事件标注 / 波动包络
# =============================================================================

#' 全球 CHE_pc 中位 + 冲击区间标注
plot_shock_global_trend <- function(master) {
  ensure_pkgs(c("ggplot2"))
  d <- master[is.finite(master$che_pc_usd2023) &
              master$che_pc_usd2023 > 0, ]
  agg <- stats::aggregate(che_pc_usd2023 ~ year, data = d,
                          FUN = function(v) stats::median(v, na.rm = TRUE))
  ggplot2::ggplot(agg, ggplot2::aes(x = .data$year, y = .data$che_pc_usd2023)) +
    ggplot2::annotate("rect", xmin = 2008, xmax = 2009, ymin = -Inf,
                     ymax = Inf, fill = palette_ghs3("warn"), alpha = 0.16) +
    ggplot2::annotate("rect", xmin = 2020, xmax = 2021, ymin = -Inf,
                     ymax = Inf, fill = palette_ghs3("bad"), alpha = 0.16) +
    ggplot2::annotate("text", x = 2008.5, y = max(agg$che_pc_usd2023,
                                                       na.rm = TRUE) * 1.05,
                     label = "GFC 2008-09", colour = palette_ghs3("warn"),
                     size = 3.2, hjust = 0.5) +
    ggplot2::annotate("text", x = 2020.5, y = max(agg$che_pc_usd2023,
                                                       na.rm = TRUE) * 1.05,
                     label = "COVID 2020-21", colour = palette_ghs3("bad"),
                     size = 3.2, hjust = 0.5) +
    ggplot2::geom_line(colour = palette_ghs3("primary"), linewidth = 1.4) +
    ggplot2::geom_point(colour = palette_ghs3("primary"), size = 2.2) +
    ggplot2::scale_y_log10(labels = scales::dollar_format(accuracy = 1)) +
    theme_ghs3() +
    labs_news(
      title = "\u5168\u7403\u4eba\u5747 CHE \u4e2d\u4f4d \u00b7 \u51b2\u51fb\u533a\u95f4",
      subtitle = "GFC \u00b7 COVID \u4e24\u6b21\u51b2\u51fb\u5bf9\u4e2d\u4f4d\u8d8b\u52bf\u7684\u54cd\u54cd",
      x = NULL, y = "USD\u4eba\u5747 (log)",
      caption = .cap_shocks())
}

#' 增长率分布每年 boxplot + 冲击带
plot_shock_growth_rate_box <- function(master) {
  ensure_pkgs(c("ggplot2"))
  d <- master[is.finite(master$che_pc_usd2023) &
              master$che_pc_usd2023 > 0, ]
  d <- d[order(d$iso3_code, d$year), ]
  d$lag <- ave(d$che_pc_usd2023, d$iso3_code,
               FUN = function(v) c(NA_real_, v[-length(v)]))
  d$gr <- log(d$che_pc_usd2023 / d$lag)
  d <- d[is.finite(d$gr) & abs(d$gr) < 1, ]
  if (!nrow(d)) return(ggplot2::ggplot() + ggplot2::theme_void())
  d$year <- as.integer(d$year)
  ggplot2::ggplot(d, ggplot2::aes(x = .data$year, y = .data$gr,
                                       group = .data$year)) +
    ggplot2::annotate("rect", xmin = 2008, xmax = 2009, ymin = -Inf,
                     ymax = Inf, fill = palette_ghs3("warn"), alpha = 0.16) +
    ggplot2::annotate("rect", xmin = 2020, xmax = 2021, ymin = -Inf,
                     ymax = Inf, fill = palette_ghs3("bad"), alpha = 0.16) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2,
      colour = palette_ghs3("ink"), alpha = 0.5) +
    ggplot2::geom_boxplot(fill = palette_ghs3("primary"), alpha = 0.45,
      colour = "#1a1f28", outlier.size = 0.5) +
    ggplot2::scale_y_continuous(labels = scales::number_format(
      accuracy = 0.1, suffix = " (log\u00d7)")) +
    theme_ghs3() +
    labs_news(
      title = "\u5168\u7403\u4eba\u5747 CHE \u5e74\u589e\u957f\u7387\u5206\u5e03",
      subtitle = "\u9634\u5f71 = GFC / COVID\uff1b\u51b2\u51fb\u671f\u5206\u5e03\u660e\u663e\u53d8\u5bbd",
      x = NULL, y = "Δlog (\u500d\u6570)",
      caption = .cap_shocks("country-year diff(log)"))
}

#' 波动包络：每年 IQR 宽度
plot_shock_volatility_envelope <- function(master) {
  ensure_pkgs(c("ggplot2"))
  d <- master[is.finite(master$che_pc_usd2023) &
              master$che_pc_usd2023 > 0, ]
  d <- d[order(d$iso3_code, d$year), ]
  d$lag <- ave(d$che_pc_usd2023, d$iso3_code,
               FUN = function(v) c(NA_real_, v[-length(v)]))
  d$gr <- log(d$che_pc_usd2023 / d$lag)
  d <- d[is.finite(d$gr) & abs(d$gr) < 1, ]
  agg <- stats::aggregate(gr ~ year, data = d,
    FUN = function(v) stats::quantile(v, c(0.25, 0.5, 0.75), na.rm = TRUE))
  mat <- as.data.frame(agg$gr)
  out <- data.frame(year = agg$year,
                    p25 = mat[, 1], p50 = mat[, 2], p75 = mat[, 3])
  ggplot2::ggplot(out, ggplot2::aes(x = .data$year)) +
    ggplot2::annotate("rect", xmin = 2020, xmax = 2021, ymin = -Inf,
                     ymax = Inf, fill = palette_ghs3("bad"), alpha = 0.16) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2,
      colour = palette_ghs3("ink"), alpha = 0.4) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$p25, ymax = .data$p75),
      fill = palette_ghs3("primary"), alpha = 0.3) +
    ggplot2::geom_line(ggplot2::aes(y = .data$p50),
      colour = palette_ghs3("primary"), linewidth = 1.3) +
    ggplot2::scale_y_continuous(labels = scales::number_format(
      accuracy = 0.01, suffix = " (log\u00d7)")) +
    theme_ghs3() +
    labs_news(
      title = "\u5168\u7403\u589e\u957f\u7387\u4e2d\u4f4d + IQR",
      subtitle = "COVID \u671f IQR \u5feb\u901f\u62c9\u5bbd\uff0c\u8868\u793a\u8de8\u56fd\u504f\u79bb\u589e\u5927",
      x = NULL, y = "\u5e74\u589e\u957f\u7387",
      caption = .cap_shocks())
}

#' 国家\u00d7年 冲击热力图（z>2 标红）
plot_shock_anomaly_heat <- function(master,
                                     min_year = 2000,
                                     max_year = NULL) {
  ensure_pkgs(c("ggplot2"))
  if (is.null(max_year)) max_year <- max(master$year, na.rm = TRUE)
  d <- master[is.finite(master$che_pc_usd2023) &
              master$che_pc_usd2023 > 0 &
              master$year >= min_year &
              master$year <= max_year, ]
  d <- d[order(d$iso3_code, d$year), ]
  d$lag <- ave(d$che_pc_usd2023, d$iso3_code,
               FUN = function(v) c(NA_real_, v[-length(v)]))
  d$gr <- log(d$che_pc_usd2023 / d$lag)
  # 全样本 z-score
  mu <- mean(d$gr, na.rm = TRUE)
  sg <- stats::sd(d$gr, na.rm = TRUE)
  d$z <- (d$gr - mu) / sg
  d <- d[is.finite(d$z) & abs(d$z) > 2, ]
  if (!nrow(d)) return(ggplot2::ggplot() + ggplot2::theme_void())
  # 按异常次数取 25 个国家
  tab <- as.data.frame(table(iso = d$iso3_code))
  tab <- tab[order(-tab$Freq), ]
  pick <- utils::head(tab$iso, 25)
  d <- d[d$iso3_code %in% pick, ]
  ggplot2::ggplot(d, ggplot2::aes(x = .data$year,
                                       y = stats::reorder(.data$iso3_code,
                                                            .data$z),
                                       fill = .data$z)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.4) +
    scale_fill_ghs3_div(midpoint = 0,
                        name = "z (log\u589e\u957f)") +
    theme_ghs3(grid = FALSE) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(size = 8)) +
    labs_news(
      title = "\u5f02\u5e38\u589e\u957f\u4e8b\u4ef6 z > 2 \u00b7 \u9ad8\u9891\u56fd\u5bb6",
      subtitle = "\u6b63\u7ea2 = \u8d44\u91d1\u6fc0\u589e\uff1b\u8d1f\u84dd = \u6025\u8d8c\u4e0b\u964d",
      x = NULL, y = "ISO3",
      caption = .cap_shocks("country-year z > 2"))
}

#' Shock 频率：每国异常年数 (|z|>2) 排序
plot_shock_freq_rank <- function(master) {
  ensure_pkgs(c("ggplot2"))
  d <- master[is.finite(master$che_pc_usd2023) &
              master$che_pc_usd2023 > 0, ]
  d <- d[order(d$iso3_code, d$year), ]
  d$lag <- ave(d$che_pc_usd2023, d$iso3_code,
               FUN = function(v) c(NA_real_, v[-length(v)]))
  d$gr <- log(d$che_pc_usd2023 / d$lag)
  mu <- mean(d$gr, na.rm = TRUE)
  sg <- stats::sd(d$gr, na.rm = TRUE)
  d$z <- (d$gr - mu) / sg
  d$abnormal <- is.finite(d$z) & abs(d$z) > 2
  agg <- stats::aggregate(abnormal ~ iso3_code + country_name, data = d,
                           FUN = sum)
  agg <- agg[order(-agg$abnormal), ]
  picked <- utils::head(agg, 20)
  picked$country_name <- factor(picked$country_name,
    levels = picked$country_name[order(picked$abnormal)])
  ggplot2::ggplot(picked, ggplot2::aes(x = .data$abnormal,
                                            y = .data$country_name)) +
    ggplot2::geom_col(fill = palette_ghs3("bad"), alpha = 0.88) +
    theme_ghs3(grid = "x") +
    labs_news(
      title = "Shock \u9891\u6b21 Top 20\uff08|z| > 2 \u5e74\u6570\uff09",
      subtitle = "\u9ad8\u9891\u56fd\u591a\u4e3a\u8d22\u52a1\u4e0d\u7a33\u5b9a\u6216\u5916\u90e8\u63f4\u52a9\u4f9d\u8d56\u578b",
      x = "\u5f02\u5e38\u5e74\u6570", y = NULL,
      caption = .cap_shocks())
}

# =============================================================================
# E. 恢复诊断
# =============================================================================

#' 2022 vs 2019 恢复 dotplot
plot_shock_recovery_2022 <- function(master, y1 = 2019, y2 = 2022) {
  ensure_pkgs(c("ggplot2"))
  m <- .shock_diff_panel(master, "che_pc_usd2023", y1, y2,
                          group_col = "income_group")
  m$delta <- log(m$che_pc_usd2023.y2 / m$che_pc_usd2023.y1)
  m <- m[is.finite(m$delta) & !is.na(m$income_group), ]
  if (!nrow(m)) return(ggplot2::ggplot() + ggplot2::theme_void())
  m$income_group <- factor(m$income_group,
    levels = c("Low income", "Lower middle income",
               "Upper middle income", "High income"))
  ggplot2::ggplot(m, ggplot2::aes(x = .data$income_group, y = .data$delta,
                                       fill = .data$income_group)) +
    ggplot2::geom_violin(alpha = 0.5, scale = "width") +
    ggplot2::geom_boxplot(width = 0.18, outlier.shape = NA,
      colour = "#1a1f28", fill = "white", alpha = 0.85) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2,
      colour = "#1a1f28", alpha = 0.55) +
    ggplot2::scale_y_continuous(labels = scales::number_format(
      accuracy = 0.1, suffix = " (log\u00d7)")) +
    scale_fill_brand_income(guide = "none") +
    theme_ghs3() +
    labs_news(
      title = sprintf("\u6062\u590d \u00b7 %d\u2192%d \u4eba\u5747 CHE\u53d8\u5316",
                       y1, y2),
      subtitle = "\u5468\u671f\u4e2d\u8de8\u8d8a 2020-21 \u4f4e\u8c37\uff1b\u9ad8\u6536\u5165\u56fd\u62b5\u8fbe\u6700\u5feb",
      x = NULL, y = "log(\u500d\u6570)",
      caption = .cap_shocks())
}

#' 恢复速度散点：2019 水平 vs 2022 相对位置
plot_shock_recovery_scatter <- function(master) {
  ensure_pkgs(c("ggplot2"))
  d <- master[master$year %in% c(2019, 2022) &
              is.finite(master$che_pc_usd2023) &
              master$che_pc_usd2023 > 0, ]
  m <- merge(d[d$year == 2019,
                c("iso3_code", "country_name", "continent",
                   "che_pc_usd2023")],
              d[d$year == 2022, c("iso3_code", "che_pc_usd2023")],
              by = "iso3_code", suffixes = c(".y1", ".y2"))
  m$rel <- m$che_pc_usd2023.y2 / m$che_pc_usd2023.y1
  m <- m[is.finite(m$rel), ]
  if (!nrow(m)) return(ggplot2::ggplot() + ggplot2::theme_void())
  ggplot2::ggplot(m, ggplot2::aes(x = .data$che_pc_usd2023.y1,
                                       y = .data$rel,
                                       colour = .data$continent)) +
    ggplot2::geom_hline(yintercept = 1, linetype = 2,
      colour = palette_ghs3("ink"), alpha = 0.5) +
    ggplot2::geom_point(alpha = 0.82, size = 2.4) +
    ggplot2::scale_x_log10(labels = scales::dollar_format(accuracy = 1)) +
    ggplot2::scale_y_continuous(labels = scales::number_format(
      accuracy = 0.1)) +
    scale_colour_brand_continent(name = NULL) +
    theme_ghs3() +
    ggplot2::theme(legend.position = "top") +
    labs_news(
      title = "\u6062\u590d\u6307\u6570 \u00b7 2022 / 2019",
      subtitle = ">1 = \u8d85\u8d8a 2019; <1 = \u4ecd\u5904 sub-COVID",
      x = "\u4eba\u5747 CHE 2019 (log)", y = "2022 / 2019",
      caption = .cap_shocks())
}

#' Catch-up vs starting level
plot_shock_catchup <- function(master) {
  ensure_pkgs(c("ggplot2"))
  d <- master[master$year %in% c(2000, 2010, 2022) &
              is.finite(master$che_pc_usd2023) &
              master$che_pc_usd2023 > 0, ]
  d_w <- list()
  for (y in c(2000, 2010, 2022)) {
    sub <- d[d$year == y, c("iso3_code", "che_pc_usd2023", "continent")]
    names(sub)[2] <- paste0("y", y)
    d_w[[as.character(y)]] <- sub
  }
  if (length(d_w) < 3) return(ggplot2::ggplot() + ggplot2::theme_void())
  mall <- Reduce(function(x, y) merge(x, y, by = c("iso3_code", "continent")),
                  d_w)
  mall$gr2010 <- log(mall$y2010 / mall$y2000) / 10
  mall$gr2022 <- log(mall$y2022 / mall$y2010) / 12
  mall <- mall[is.finite(mall$gr2010) & is.finite(mall$gr2022), ]
  if (!nrow(mall)) return(ggplot2::ggplot() + ggplot2::theme_void())
  ggplot2::ggplot(mall, ggplot2::aes(x = .data$y2000, y = .data$gr2022,
                                          colour = .data$continent)) +
    ggplot2::geom_point(alpha = 0.82, size = 2.4) +
    ggplot2::geom_smooth(method = "lm", se = FALSE, formula = y ~ x,
      colour = palette_ghs3("ink"), linewidth = 0.9) +
    ggplot2::scale_x_log10(labels = scales::dollar_format(accuracy = 1)) +
    ggplot2::scale_y_continuous(labels = scales::number_format(
      accuracy = 0.01, suffix = " (log/yr)")) +
    scale_colour_brand_continent(name = NULL) +
    theme_ghs3() +
    ggplot2::theme(legend.position = "top") +
    labs_news(
      title = "Catch-up \u00b7 2000 \u8d77\u70b9 vs 2010-22 \u589e\u901f",
      subtitle = "\u8d1f\u659c\u7387 = \u8ffd\u8d76\uff1b\u4e0d\u5e73\u6536\u655b\u4f9d\u6d32\u4e0d\u540c",
      x = "\u4eba\u5747 CHE 2000 (log)", y = "log/yr (2010-22)",
      caption = .cap_shocks())
}

#' 2022 残差：实际 - pre-COVID 趋势外推
plot_shock_residual_2022 <- function(master) {
  ensure_pkgs(c("ggplot2"))
  d <- master[is.finite(master$che_pc_usd2023) &
              master$che_pc_usd2023 > 0 &
              master$year >= 2010 & master$year <= 2022, ]
  isos <- unique(d$iso3_code)
  rows <- list()
  for (iso in isos) {
    sub <- d[d$iso3_code == iso, ]
    pre <- sub[sub$year <= 2019, ]
    if (nrow(pre) < 5) next
    fit <- tryCatch(stats::lm(log(che_pc_usd2023) ~ year, data = pre),
                     error = function(e) NULL)
    if (is.null(fit)) next
    pred22 <- stats::predict(fit, newdata = data.frame(year = 2022))
    actual22 <- sub$che_pc_usd2023[sub$year == 2022]
    if (!length(actual22) || !is.finite(actual22)) next
    rows[[iso]] <- data.frame(
      iso = iso, country = sub$country_name[1],
      continent = sub$continent[1],
      res = log(actual22) - pred22)
  }
  if (!length(rows)) return(ggplot2::ggplot() + ggplot2::theme_void())
  df <- do.call(rbind, rows)
  df <- df[is.finite(df$res), ]
  df <- df[order(df$res), ]
  picked <- rbind(utils::head(df, 12), utils::tail(df, 12))
  picked$country <- factor(picked$country, levels = picked$country)
  ggplot2::ggplot(picked, ggplot2::aes(x = .data$res, y = .data$country,
                                            fill = .data$res > 0)) +
    ggplot2::geom_col(alpha = 0.88) +
    ggplot2::scale_x_continuous(labels = scales::number_format(
      accuracy = 0.1, suffix = " (log\u00d7)")) +
    ggplot2::scale_fill_manual(
      values = c(`TRUE` = palette_ghs3("good"),
                 `FALSE` = palette_ghs3("bad")), guide = "none") +
    theme_ghs3(grid = "x") +
    labs_news(
      title = "2022 \u5b9e\u9645 vs Pre-COVID \u8d8b\u52bf\u9884\u671f",
      subtitle = "\u8d85\u8d8b\u52bf = \u53cd\u5468\u671f\u62a5\u544a\uff1b\u4e0b\u504f\u79bb = \u672a\u80fd\u6062\u590d",
      x = "log(\u5b9e\u9645/\u9884\u671f)", y = NULL,
      caption = .cap_shocks("OLS 2010-19 extrapolated"))
}

#' 恢复仪表板：5 指标 dotplot
plot_shock_recovery_dashboard <- function(master) {
  ensure_pkgs(c("ggplot2"))
  vars <- list(
    che_pc_usd2023 = "\u4eba\u5747 CHE",
    gghed_che      = "GGHED / CHE",
    hf3_che        = "OOP / CHE",
    life_exp       = "\u9884\u671f\u5bff\u547d",
    u5mr           = "U5MR /1000")
  rows <- list()
  for (k in names(vars)) {
    d <- master[master$year %in% c(2019, 2022) &
                is.finite(master[[k]]), ]
    if (!nrow(d)) next
    a <- stats::aggregate(d[[k]], by = list(year = d$year), median,
                          na.rm = TRUE)
    if (nrow(a) < 2) next
    rows[[k]] <- data.frame(
      metric = vars[[k]],
      y2019 = a$x[a$year == 2019],
      y2022 = a$x[a$year == 2022])
  }
  if (!length(rows)) return(ggplot2::ggplot() + ggplot2::theme_void())
  df <- do.call(rbind, rows)
  df$rel <- df$y2022 / df$y2019
  df$metric <- factor(df$metric, levels = df$metric[order(df$rel)])
  ggplot2::ggplot(df, ggplot2::aes(x = .data$rel, y = .data$metric)) +
    ggplot2::geom_vline(xintercept = 1, linetype = 2,
      colour = palette_ghs3("ink"), alpha = 0.5) +
    ggplot2::geom_segment(ggplot2::aes(x = 1, xend = .data$rel,
        y = .data$metric, yend = .data$metric),
      colour = palette_ghs3("primary"), linewidth = 1) +
    ggplot2::geom_point(colour = palette_ghs3("primary"), size = 4.5) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f\u00d7",
                                                       .data$rel)),
      hjust = -0.3, size = 3, colour = palette_ghs3("ink")) +
    ggplot2::scale_x_continuous(labels = scales::number_format(accuracy = 0.1),
      expand = ggplot2::expansion(mult = c(0.05, 0.18))) +
    theme_ghs3(grid = "x") +
    labs_news(
      title = "\u6062\u590d\u4eea\u8868\u677f \u00b7 2022/2019",
      subtitle = "\u5168\u7403\u4e2d\u4f4d\u6bd4\u4f8b\uff1b\u4eba\u5747 CHE \u4ecd\u552f\u662f\u4e3b\u5bfc\u52a8\u80fd",
      x = "rel (2022 / 2019)", y = NULL,
      caption = .cap_shocks())
}

# =============================================================================
# 导出器
# =============================================================================

#' 批量导出 B6 冲击与变点图集
ghs_export_shocks <- function(master = NULL, fig_dir = NULL) {
  if (is.null(master)) {
    cache <- file.path(proj_root(), "\u6d3e\u751f\u6570\u636e",
                        "\u5904\u7406\u7ed3\u679c", "master_enriched.rds")
    if (!file.exists(cache)) stop("Missing master_enriched.rds")
    master <- readRDS(cache)
  }
  if (is.null(fig_dir)) {
    fig_dir <- file.path(proj_root(), "\u5206\u6790\u8f93\u51fa",
                          "\u56fe\u8868")
  }
  dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

  plots <- list(
    shk_covid_chepc          = plot_shock_covid_chepc(master),
    shk_covid_oop            = plot_shock_covid_oop(master),
    shk_covid_dot_income     = plot_shock_covid_dot_income(master),
    shk_covid_lifeexp        = plot_shock_covid_lifeexp(master),
    shk_covid_track          = plot_shock_covid_track(master),
    shk_gfc_chepc            = plot_shock_gfc_chepc(master),
    shk_gfc_oop              = plot_shock_gfc_oop(master),
    shk_gfc_trend_gap        = plot_shock_gfc_trend_gap(master),
    shk_gfc_recovery_years   = plot_shock_gfc_recovery_years(master),
    shk_gfc_sensitivity      = plot_shock_gfc_sensitivity(master),
    shk_seg_usa_2008         = plot_shock_segmented_chepc(master, "USA", 2008),
    shk_seg_chn_2019         = plot_shock_segmented_chepc(master, "CHN", 2019),
    shk_seg_multi            = plot_shock_segmented_multi(master),
    shk_seg_dot              = plot_shock_segmented_dot(master),
    shk_pre_post_slope       = plot_shock_pre_post_slope(master),
    shk_segment_fit_chn      = plot_shock_segment_fit(master, "CHN"),
    shk_global_trend         = plot_shock_global_trend(master),
    shk_growth_rate_box      = plot_shock_growth_rate_box(master),
    shk_volatility_env       = plot_shock_volatility_envelope(master),
    shk_anomaly_heat         = plot_shock_anomaly_heat(master),
    shk_freq_rank            = plot_shock_freq_rank(master),
    shk_recovery_2022        = plot_shock_recovery_2022(master),
    shk_recovery_scatter     = plot_shock_recovery_scatter(master),
    shk_catchup              = plot_shock_catchup(master),
    shk_residual_2022        = plot_shock_residual_2022(master),
    shk_recovery_dashboard   = plot_shock_recovery_dashboard(master)
  )

  out <- character(0)
  for (nm in names(plots)) {
    p <- plots[[nm]]
    if (is.null(p)) next
    res <- tryCatch({
      png_path <- file.path(fig_dir, paste0(nm, ".png"))
      svg_path <- file.path(fig_dir, paste0(nm, ".svg"))
      ggplot2::ggsave(png_path, p, width = 10, height = 6, dpi = 160,
                       bg = brand_palette$paper)
      ggplot2::ggsave(svg_path, p, width = 10, height = 6,
                       bg = brand_palette$paper)
      out <<- c(out, png_path, svg_path)
      TRUE
    }, error = function(e) {
      message("[shocks export] ", nm, " failed: ", conditionMessage(e))
      FALSE
    })
  }
  invisible(out)
}
