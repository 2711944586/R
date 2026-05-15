# =============================================================================
# 程序/34_plots_country.R   —— 国家专题图集（B5 阶段）
# -----------------------------------------------------------------------------
# 5 个组：
#   A. 单国时间序列                                       × 5
#   B. 国家对比 / Small multiples                         × 5
#   C. 同侪 / 区域内对比                                  × 5
#   D. 排名 / Top / Bottom                               × 5
#   E. 专题国家集（BRICS / G7 / EU / 太平洋小国 / Lo-LE） × 5
# 总：25 个 plot_country_* + ghs_export_country() 导出器
# =============================================================================

if (!exists("ensure_pkgs", mode = "function")) {
  source(file.path("\u7a0b\u5e8f", "00_utils.R"))
}

.cap_country <- function(extra = NULL) {
  base <- "\u6570\u636e \u00b7 WHO GHED 2024 + WDI"
  if (is.null(extra) || !nzchar(extra)) base
  else paste0(base, " \u00b7 ", extra)
}

# 预设国家集 ---------------------------------------------------------------
ghs_country_sets <- list(
  brics   = c("BRA", "RUS", "IND", "CHN", "ZAF"),
  g7      = c("USA", "GBR", "FRA", "DEU", "ITA", "JPN", "CAN"),
  asean5  = c("IDN", "THA", "VNM", "PHL", "MYS"),
  eu5     = c("DEU", "FRA", "ITA", "ESP", "NLD"),
  nordic  = c("SWE", "NOR", "DNK", "FIN", "ISL"),
  pacific = c("FJI", "PNG", "SLB", "VUT", "WSM"),
  lowle   = c("CAF", "TCD", "NGA", "SLE", "SOM"),
  highle  = c("JPN", "CHE", "ESP", "AUS", "ITA")
)

.country_label <- function(master, iso) {
  out <- unique(master$country_name[master$iso3_code %in% iso])
  if (length(out)) out[1] else iso
}

.country_subset <- function(master, isos) {
  d <- master[master$iso3_code %in% isos, ]
  d$iso3_code <- factor(d$iso3_code, levels = isos)
  d <- d[order(d$iso3_code, d$year), ]
  d
}

# =============================================================================
# A. 单国时间序列
# =============================================================================

#' 单国 CHE_pc 轨迹 + 全球中位参考线
plot_country_trend_chepc <- function(master, iso = "CHN") {
  ensure_pkgs(c("ggplot2"))
  ref <- stats::aggregate(che_pc_usd2023 ~ year, data = master,
                          FUN = function(v) stats::median(v, na.rm = TRUE))
  d <- master[master$iso3_code == iso & is.finite(master$che_pc_usd2023), ]
  if (!nrow(d)) return(ggplot2::ggplot() + ggplot2::theme_void())
  name <- .country_label(master, iso)
  ggplot2::ggplot() +
    ggplot2::geom_line(data = ref, ggplot2::aes(x = .data$year,
                                                  y = .data$che_pc_usd2023),
      colour = palette_ghs3("neutral"), linewidth = 0.9, linetype = 2) +
    ggplot2::geom_line(data = d, ggplot2::aes(x = .data$year,
                                                y = .data$che_pc_usd2023),
      colour = palette_ghs3("primary"), linewidth = 1.5) +
    ggplot2::geom_point(data = d, ggplot2::aes(x = .data$year,
                                                  y = .data$che_pc_usd2023),
      colour = palette_ghs3("primary"), size = 2) +
    ggplot2::scale_y_log10(labels = scales::dollar_format(accuracy = 1)) +
    theme_ghs3() +
    labs_news(
      title = sprintf("\u4eba\u5747 CHE \u00b7 %s vs \u5168\u7403\u4e2d\u4f4d", name),
      subtitle = "\u865a\u7ebf = \u5168\u7403\u4e2d\u4f4d\uff1b\u5b9e\u7ebf = \u8be5\u56fd",
      x = NULL, y = "USD\u4eba\u5747 (2023, log)",
      caption = .cap_country())
}

#' 单国 HF1/HF2/HF3 三源叠加
plot_country_hf_stack <- function(master, iso = "CHN") {
  ensure_pkgs(c("ggplot2"))
  d <- master[master$iso3_code == iso, ]
  if (!nrow(d)) return(ggplot2::ggplot() + ggplot2::theme_void())
  d$hf1 <- d$hf1_che; d$hf2 <- d$hf2_che; d$hf3 <- d$hf3_che
  long <- rbind(
    data.frame(year = d$year, src = "HF1 \u653f\u5e9c", val = d$hf1),
    data.frame(year = d$year, src = "HF2 \u793e\u4fdd / \u79c1\u4eba\u4fdd\u9669",
               val = d$hf2),
    data.frame(year = d$year, src = "HF3 OOP", val = d$hf3)
  )
  long <- long[is.finite(long$val), ]
  long$src <- factor(long$src,
    levels = c("HF1 \u653f\u5e9c", "HF2 \u793e\u4fdd / \u79c1\u4eba\u4fdd\u9669",
               "HF3 OOP"))
  name <- .country_label(master, iso)
  ggplot2::ggplot(long, ggplot2::aes(x = .data$year, y = .data$val,
                                          fill = .data$src)) +
    ggplot2::geom_area(alpha = 0.9, position = "stack") +
    ggplot2::scale_y_continuous(
        labels = scales::percent_format(accuracy = 1, scale = 1)) +
    ggplot2::scale_fill_manual(values = c(
      "HF1 \u653f\u5e9c" = palette_ghs3("primary"),
      "HF2 \u793e\u4fdd / \u79c1\u4eba\u4fdd\u9669" = palette_ghs3("good"),
      "HF3 OOP" = palette_ghs3("bad")),
      name = NULL) +
    theme_ghs3() +
    ggplot2::theme(legend.position = "top") +
    labs_news(
      title = sprintf("%s \u00b7 \u8d44\u91d1\u6e90 HF1/HF2/HF3 \u53e0\u52a0",
                       name),
      subtitle = "HF1 \u00b7 HF2 \u00b7 HF3 \u4e09\u8005\u7406\u8bba\u4e0a\u7d2f\u8ba1 100%",
      x = NULL, y = "\u5360 CHE \u6bd4",
      caption = .cap_country("WHO GHED HF schema"))
}

#' 单国 HC 功能分布（最近 1 年 + 历史中位）
plot_country_hc_bars <- function(master, iso = "CHN", year = NULL) {
  ensure_pkgs(c("ggplot2"))
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$iso3_code == iso & master$year == year, ]
  if (!nrow(d)) return(ggplot2::ggplot() + ggplot2::theme_void())
  cats <- c("hc1_che" = "HC1 \u4f4f\u9662",
            "hc3_che" = "HC3 \u95e8\u8bca",
            "hc4_che" = "HC4 \u8f85\u52a9",
            "hc5_che" = "HC5 \u836f\u54c1",
            "hc6_che" = "HC6 \u9884\u9632",
            "hc7_che" = "HC7 \u7ba1\u7406",
            "hc9_che" = "HC9 \u5176\u4ed6",
            "hc2_che" = "HC2 \u65e5\u95f4")
  vals <- vapply(names(cats), function(k) {
    v <- d[[k]]
    if (length(v) == 0 || !is.finite(v[1])) NA_real_ else v[1]
  }, numeric(1))
  df <- data.frame(
    cat = cats[!is.na(vals)],
    val = vals[!is.na(vals)])
  df <- df[order(df$val), ]
  df$cat <- factor(df$cat, levels = df$cat)
  name <- .country_label(master, iso)
  ggplot2::ggplot(df, ggplot2::aes(x = .data$val, y = .data$cat)) +
    ggplot2::geom_col(fill = palette_ghs3("primary"), alpha = 0.88) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f%%", .data$val)),
      hjust = -0.05, colour = palette_ghs3("ink"), size = 3) +
    ggplot2::scale_x_continuous(
        labels = scales::percent_format(accuracy = 1, scale = 1),
        expand = ggplot2::expansion(mult = c(0, 0.18))) +
    theme_ghs3(grid = "x") +
    labs_news(
      title = sprintf("%s \u00b7 HC \u529f\u80fd\u5206\u5e03 \u00b7 %d",
                       name, year),
      subtitle = "\u4f4f\u9662 / \u95e8\u8bca / \u836f\u54c1 \u4e3a\u4e09\u5927\u9886\u57df",
      x = "\u5360 CHE \u6bd4", y = NULL,
      caption = .cap_country("WHO GHED HC \u5206\u7c7b"))
}

#' 单国寿命 + U5MR 双轴
plot_country_outcome_dual <- function(master, iso = "CHN") {
  ensure_pkgs(c("ggplot2"))
  d <- master[master$iso3_code == iso, ]
  d <- d[is.finite(d$life_exp) | (is.finite(d$u5mr) & d$u5mr > 0), ]
  if (!nrow(d)) return(ggplot2::ggplot() + ggplot2::theme_void())
  # 双轴：以一次缩放将 u5mr 映射进 life_exp 量级
  rng_le <- range(d$life_exp, na.rm = TRUE)
  rng_u5 <- range(d$u5mr, na.rm = TRUE)
  if (any(!is.finite(rng_le)) || any(!is.finite(rng_u5))) {
    return(ggplot2::ggplot() + ggplot2::theme_void())
  }
  k <- (rng_le[2] - rng_le[1]) / (rng_u5[2] - rng_u5[1])
  b <- rng_le[1] - k * rng_u5[1]
  d$u5_scaled <- d$u5mr * k + b
  name <- .country_label(master, iso)
  ggplot2::ggplot(d, ggplot2::aes(x = .data$year)) +
    ggplot2::geom_line(ggplot2::aes(y = .data$life_exp),
      colour = palette_ghs3("primary"), linewidth = 1.4) +
    ggplot2::geom_line(ggplot2::aes(y = .data$u5_scaled),
      colour = palette_ghs3("bad"), linewidth = 1.4, linetype = 5) +
    ggplot2::scale_y_continuous(
      name = "\u9884\u671f\u5bff\u547d\uff08\u5e74\uff09",
      sec.axis = ggplot2::sec_axis(~ (. - b) / k,
        name = "U5MR /1000",
        labels = scales::number_format(accuracy = 1))) +
    theme_ghs3() +
    labs_news(
      title = sprintf("%s \u00b7 \u9884\u671f\u5bff\u547d (\u5b9e\u7ebf) \u4e0e U5MR (\u865a\u7ebf)",
                       name),
      subtitle = "\u53cc\u8f74\u7f29\u653e\uff1b\u4e24\u6307\u6807\u5468\u671f\u4e0d\u5fc5\u5b8c\u5168\u540c\u8c03",
      x = NULL, y = NULL,
      caption = .cap_country("WDI life_exp & U5MR"))
}

#' 单国 5 指标 dashboard 小型面板
plot_country_dashboard <- function(master, iso = "CHN") {
  ensure_pkgs(c("ggplot2"))
  d <- master[master$iso3_code == iso, ]
  if (!nrow(d)) return(ggplot2::ggplot() + ggplot2::theme_void())
  long <- rbind(
    data.frame(year = d$year, kind = "1 \u4eba\u5747 CHE (log)",
               val = log10(pmax(d$che_pc_usd2023, 1e-2))),
    data.frame(year = d$year, kind = "2 OOP / CHE", val = d$hf3_che),
    data.frame(year = d$year, kind = "3 GGHED / CHE", val = d$gghed_che),
    data.frame(year = d$year, kind = "4 \u9884\u671f\u5bff\u547d",
               val = d$life_exp),
    data.frame(year = d$year, kind = "5 U5MR /1000", val = d$u5mr)
  )
  long <- long[is.finite(long$val), ]
  long$kind <- factor(long$kind, levels = sort(unique(long$kind)))
  name <- .country_label(master, iso)
  ggplot2::ggplot(long, ggplot2::aes(x = .data$year, y = .data$val)) +
    ggplot2::geom_line(colour = palette_ghs3("primary"), linewidth = 1.2) +
    ggplot2::geom_point(colour = palette_ghs3("primary"), size = 1.6) +
    ggplot2::facet_wrap(~ .data$kind, scales = "free_y", ncol = 3) +
    theme_ghs3() +
    labs_news(
      title = sprintf("%s \u00b7 5 \u6307\u6807\u603b\u89c8", name),
      subtitle = "log CHE\u4eba\u5747 \u00b7 OOP \u00b7 GGHED \u00b7 \u5bff\u547d \u00b7 U5MR",
      x = NULL, y = NULL,
      caption = .cap_country())
}

# =============================================================================
# B. 国家对比
# =============================================================================

#' Small multiples：N 国 CHE_pc 时序
plot_country_smallmult_chepc <- function(master,
                                          isos = ghs_country_sets$brics) {
  ensure_pkgs(c("ggplot2"))
  d <- .country_subset(master, isos)
  d <- d[is.finite(d$che_pc_usd2023), ]
  if (!nrow(d)) return(ggplot2::ggplot() + ggplot2::theme_void())
  ggplot2::ggplot(d, ggplot2::aes(x = .data$year, y = .data$che_pc_usd2023)) +
    ggplot2::geom_line(colour = palette_ghs3("primary"), linewidth = 1.3) +
    ggplot2::scale_y_log10(labels = scales::dollar_format(accuracy = 1)) +
    ggplot2::facet_wrap(~ .data$country_name, scales = "free_y", ncol = 3) +
    theme_ghs3() +
    labs_news(
      title = "Small multiples \u00b7 \u4eba\u5747 CHE",
      subtitle = sprintf("\u9009\u53d6 %d \u56fd\uff1b\u72ec\u7acb log \u8f74",
                          length(isos)),
      x = NULL, y = "USD\u4eba\u5747 (log)",
      caption = .cap_country())
}

#' N 国对比折线（同 log 轴）
plot_country_compare_chepc <- function(master,
                                        isos = ghs_country_sets$g7) {
  ensure_pkgs(c("ggplot2"))
  d <- .country_subset(master, isos)
  d <- d[is.finite(d$che_pc_usd2023), ]
  if (!nrow(d)) return(ggplot2::ggplot() + ggplot2::theme_void())
  ggplot2::ggplot(d, ggplot2::aes(x = .data$year, y = .data$che_pc_usd2023,
                                       colour = .data$country_name)) +
    ggplot2::geom_line(linewidth = 1.3) +
    ggplot2::scale_y_log10(labels = scales::dollar_format(accuracy = 1)) +
    scale_colour_ghs3(name = NULL) +
    theme_ghs3() +
    ggplot2::theme(legend.position = "top") +
    labs_news(
      title = "G7 \u4eba\u5747 CHE \u8de8\u56fd\u5bf9\u6bd4",
      subtitle = "\u540c log \u8f74\uff1bUSA \u5728\u8d44\u91d1\u5c42\u9762\u660e\u663e\u9886\u5148\u4f46\u589e\u901f\u8d8b\u7f13",
      x = NULL, y = "USD\u4eba\u5747 (log)",
      caption = .cap_country())
}

#' N 国 5 指标热力（最近年）
plot_country_compare_heat <- function(master,
                                       isos = ghs_country_sets$brics,
                                       year = NULL) {
  ensure_pkgs(c("ggplot2"))
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$iso3_code %in% isos & master$year == year, ]
  if (!nrow(d)) return(ggplot2::ggplot() + ggplot2::theme_void())
  cols <- c("che_pc_usd2023", "hf3_che", "gghed_che", "life_exp", "u5mr")
  long <- list()
  for (k in cols) {
    if (is.null(d[[k]])) next
    long[[k]] <- data.frame(iso = d$country_name, var = k, val = d[[k]])
  }
  if (!length(long)) return(ggplot2::ggplot() + ggplot2::theme_void())
  lg <- do.call(rbind, long)
  # 列内 z-score
  lg$z <- ave(lg$val, lg$var, FUN = function(v) {
    if (sum(is.finite(v)) < 2) return(rep(NA_real_, length(v)))
    (v - mean(v, na.rm = TRUE)) / stats::sd(v, na.rm = TRUE)
  })
  lg$var <- factor(lg$var, levels = cols, labels = c(
    "\u4eba\u5747 CHE", "OOP / CHE", "GGHED / CHE",
    "\u5bff\u547d", "U5MR"))
  ggplot2::ggplot(lg, ggplot2::aes(x = .data$var, y = .data$iso,
                                          fill = .data$z)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.4) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%+.1f", .data$z)),
      colour = palette_ghs3("ink"), size = 3) +
    scale_fill_ghs3_div(midpoint = 0,
      name = "z\uff08\u540c\u96c6\u5185\uff09") +
    theme_ghs3(grid = FALSE) +
    ggplot2::theme(panel.grid = ggplot2::element_blank()) +
    labs_news(
      title = sprintf("\u591a\u56fd z-score \u70ed\u56fe \u00b7 %d", year),
      subtitle = "\u6b63 = \u9ad8\u4e8e\u540c\u96c6\u5747\u503c\uff1b\u8d1f = \u4f4e\u4e8e",
      x = NULL, y = NULL,
      caption = .cap_country())
}

#' 两国并列：选定 2 国，HC 功能并列条形（最近年）
plot_country_pair_hc <- function(master, iso_a = "USA", iso_b = "CHN",
                                  year = NULL) {
  ensure_pkgs(c("ggplot2"))
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  cats <- c("hc1_che" = "HC1 \u4f4f\u9662",
            "hc3_che" = "HC3 \u95e8\u8bca",
            "hc4_che" = "HC4 \u8f85\u52a9",
            "hc5_che" = "HC5 \u836f\u54c1",
            "hc6_che" = "HC6 \u9884\u9632",
            "hc7_che" = "HC7 \u7ba1\u7406")
  rows <- list()
  for (iso in c(iso_a, iso_b)) {
    d <- master[master$iso3_code == iso & master$year == year, ]
    if (!nrow(d)) next
    for (k in names(cats)) {
      v <- d[[k]]
      if (is.null(v) || !is.finite(v[1])) next
      rows[[paste(iso, k)]] <- data.frame(
        country = .country_label(master, iso),
        cat = cats[[k]], val = v[1])
    }
  }
  if (!length(rows)) return(ggplot2::ggplot() + ggplot2::theme_void())
  df <- do.call(rbind, rows)
  df$country <- factor(df$country,
    levels = unique(df$country))
  ggplot2::ggplot(df, ggplot2::aes(x = .data$val, y = .data$cat,
                                       fill = .data$country)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.7),
                     width = 0.65, alpha = 0.92) +
    ggplot2::scale_x_continuous(
        labels = scales::percent_format(accuracy = 1, scale = 1)) +
    ggplot2::scale_fill_manual(
      values = c(palette_ghs3("primary"), palette_ghs3("secondary")),
      name = NULL) +
    theme_ghs3(grid = "x") +
    ggplot2::theme(legend.position = "top") +
    labs_news(
      title = sprintf("HC \u529f\u80fd\u5e76\u5217\uff1a%s vs %s \u00b7 %d",
                       .country_label(master, iso_a),
                       .country_label(master, iso_b), year),
      subtitle = "\u4f4f\u9662 / \u95e8\u8bca / \u836f\u54c1 \u662f\u4e3b\u8981\u504f\u79bb\u70b9",
      x = "\u5360 CHE \u6bd4", y = NULL,
      caption = .cap_country())
}

#' 同期增长率对比 (CAGR)
plot_country_cagr_compare <- function(master,
                                      isos = ghs_country_sets$brics,
                                      y1 = 2000, y2 = NULL) {
  ensure_pkgs(c("ggplot2"))
  if (is.null(y2)) y2 <- max(master$year, na.rm = TRUE)
  rows <- list()
  for (iso in isos) {
    d <- master[master$iso3_code == iso &
                master$year %in% c(y1, y2) &
                is.finite(master$che_pc_usd2023) &
                master$che_pc_usd2023 > 0, ]
    if (nrow(d) < 2) next
    v1 <- d$che_pc_usd2023[d$year == y1]
    v2 <- d$che_pc_usd2023[d$year == y2]
    if (!length(v1) || !length(v2)) next
    cagr <- (v2 / v1) ^ (1 / (y2 - y1)) - 1
    rows[[iso]] <- data.frame(country = .country_label(master, iso),
                               cagr = cagr)
  }
  if (!length(rows)) return(ggplot2::ggplot() + ggplot2::theme_void())
  df <- do.call(rbind, rows)
  df <- df[order(df$cagr), ]
  df$country <- factor(df$country, levels = df$country)
  ggplot2::ggplot(df, ggplot2::aes(x = .data$cagr, y = .data$country,
                                       fill = .data$cagr > 0)) +
    ggplot2::geom_col(alpha = 0.88) +
    ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    ggplot2::scale_fill_manual(
      values = c(`TRUE` = palette_ghs3("good"),
                 `FALSE` = palette_ghs3("bad")), guide = "none") +
    theme_ghs3(grid = "x") +
    labs_news(
      title = sprintf("\u4eba\u5747 CHE CAGR \u00b7 %d\u2192%d", y1, y2),
      subtitle = "\u590d\u5408\u5e74\u589e\u957f\u7387\u5bf9\u6bd4",
      x = "CAGR", y = NULL,
      caption = .cap_country())
}

# =============================================================================
# C. 同侪 / 区域内对比
# =============================================================================

#' 单国 vs 收入组 z-score 时序
plot_country_zscore_income <- function(master, iso = "CHN") {
  ensure_pkgs(c("ggplot2"))
  d <- master[is.finite(master$che_pc_usd2023) &
              master$che_pc_usd2023 > 0 &
              !is.na(master$income_group), ]
  zs <- ave(d$che_pc_usd2023, d$income_group, d$year,
            FUN = function(v) {
              if (sum(is.finite(v)) < 3) return(rep(NA_real_, length(v)))
              (v - mean(v, na.rm = TRUE)) / stats::sd(v, na.rm = TRUE)
            })
  pick <- d[d$iso3_code == iso, ]
  pick$z <- zs[d$iso3_code == iso]
  pick <- pick[is.finite(pick$z), ]
  if (!nrow(pick)) return(ggplot2::ggplot() + ggplot2::theme_void())
  name <- .country_label(master, iso)
  ggplot2::ggplot(pick, ggplot2::aes(x = .data$year, y = .data$z)) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2,
      colour = palette_ghs3("ink"), alpha = 0.5) +
    ggplot2::geom_line(colour = palette_ghs3("primary"), linewidth = 1.4) +
    ggplot2::geom_point(colour = palette_ghs3("primary"), size = 2.2) +
    theme_ghs3() +
    labs_news(
      title = sprintf("%s \u00b7 \u540c\u6536\u5165\u7ec4 z-score \u8f68\u8ff9",
                       name),
      subtitle = "\u5168 0 \u4ee3\u8868\u4e0e\u540c\u7ec4\u5747\u503c\u4e00\u81f4",
      x = NULL, y = "z\uff08\u540c\u96c6\u5185\u4eba\u5747 CHE\uff09",
      caption = .cap_country())
}

#' 单国 vs 大洲 OOP gap 时序
plot_country_oop_gap <- function(master, iso = "CHN") {
  ensure_pkgs(c("ggplot2"))
  d <- master[is.finite(master$hf3_che), ]
  med_cont <- stats::aggregate(hf3_che ~ continent + year, data = d,
                                FUN = function(v) stats::median(v, na.rm = TRUE))
  pick <- d[d$iso3_code == iso, c("year", "continent", "hf3_che")]
  m <- merge(pick, med_cont, by = c("continent", "year"),
              suffixes = c(".c", ".m"))
  m$gap <- m$hf3_che.c - m$hf3_che.m
  m <- m[is.finite(m$gap), ]
  if (!nrow(m)) return(ggplot2::ggplot() + ggplot2::theme_void())
  name <- .country_label(master, iso)
  ggplot2::ggplot(m, ggplot2::aes(x = .data$year, y = .data$gap)) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2,
      colour = palette_ghs3("ink"), alpha = 0.5) +
    ggplot2::geom_col(fill = palette_ghs3("warn"), alpha = 0.85) +
    ggplot2::scale_y_continuous(
        labels = scales::number_format(accuracy = 0.1,
                                          suffix = " pp")) +
    theme_ghs3() +
    labs_news(
      title = sprintf("%s OOP \u4e0e\u5927\u6d32\u4e2d\u4f4d gap", name),
      subtitle = "\u6b63\u503c = \u9ad8\u4e8e\u5927\u6d32\u4e2d\u4f4d\uff08\u4fdd\u62a4\u8f83\u5dee\uff09",
      x = NULL, y = "gap (\u767e\u5206\u70b9)",
      caption = .cap_country())
}

#' 单国 vs 全球：寿命差距
plot_country_lifeexp_gap <- function(master, iso = "CHN") {
  ensure_pkgs(c("ggplot2"))
  med <- stats::aggregate(life_exp ~ year, data = master,
                          FUN = function(v) stats::median(v, na.rm = TRUE))
  pick <- master[master$iso3_code == iso, c("year", "life_exp")]
  m <- merge(pick, med, by = "year", suffixes = c(".c", ".g"))
  m$gap <- m$life_exp.c - m$life_exp.g
  m <- m[is.finite(m$gap), ]
  if (!nrow(m)) return(ggplot2::ggplot() + ggplot2::theme_void())
  name <- .country_label(master, iso)
  ggplot2::ggplot(m, ggplot2::aes(x = .data$year, y = .data$gap)) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2,
      colour = palette_ghs3("ink"), alpha = 0.5) +
    ggplot2::geom_area(fill = palette_ghs3("primary"), alpha = 0.35) +
    ggplot2::geom_line(colour = palette_ghs3("primary"), linewidth = 1.4) +
    theme_ghs3() +
    labs_news(
      title = sprintf("%s \u9884\u671f\u5bff\u547d \u4e0e \u5168\u7403\u4e2d\u4f4d gap",
                       name),
      subtitle = "\u9762\u79ef\u8d8a\u9ad8 \u2192 \u8be5\u56fd\u8d85\u51fa\u4e2d\u4f4d\u8d8a\u591a",
      x = NULL, y = "gap (\u5e74)",
      caption = .cap_country())
}

#' 多国 vs 全球 趋势对照
plot_country_multi_vs_global <- function(master,
                                          isos = ghs_country_sets$brics) {
  ensure_pkgs(c("ggplot2"))
  med <- stats::aggregate(che_pc_usd2023 ~ year, data = master,
                          FUN = function(v) stats::median(v, na.rm = TRUE))
  med$country_name <- "\u5168\u7403\u4e2d\u4f4d"
  d <- master[master$iso3_code %in% isos &
              is.finite(master$che_pc_usd2023), c("country_name", "year",
                                                     "che_pc_usd2023")]
  d <- rbind(d, med)
  d$country_name <- factor(d$country_name,
    levels = c("\u5168\u7403\u4e2d\u4f4d",
               setdiff(unique(d$country_name), "\u5168\u7403\u4e2d\u4f4d")))
  ggplot2::ggplot(d, ggplot2::aes(x = .data$year, y = .data$che_pc_usd2023,
                                       colour = .data$country_name)) +
    ggplot2::geom_line(linewidth = 1.2) +
    ggplot2::scale_y_log10(labels = scales::dollar_format(accuracy = 1)) +
    scale_colour_ghs3(name = NULL) +
    theme_ghs3() +
    ggplot2::theme(legend.position = "top") +
    labs_news(
      title = "\u591a\u56fd vs \u5168\u7403\u4e2d\u4f4d \u00b7 \u4eba\u5747 CHE",
      subtitle = "\u540c log \u8f74\u4e0a\u53e0\u52a0\u5168\u7403\u4e2d\u4f4d\u53c2\u8003",
      x = NULL, y = "USD\u4eba\u5747 (log)",
      caption = .cap_country())
}

#' 单国排名变化（CHE_pc 全球排名）
plot_country_rank_trend <- function(master, iso = "CHN") {
  ensure_pkgs(c("ggplot2"))
  d <- master[is.finite(master$che_pc_usd2023), ]
  d$rank <- ave(-d$che_pc_usd2023, d$year,
                FUN = function(v) rank(v, ties.method = "min"))
  pick <- d[d$iso3_code == iso, c("year", "rank")]
  pick <- pick[is.finite(pick$rank), ]
  if (!nrow(pick)) return(ggplot2::ggplot() + ggplot2::theme_void())
  name <- .country_label(master, iso)
  ggplot2::ggplot(pick, ggplot2::aes(x = .data$year, y = .data$rank)) +
    ggplot2::geom_line(colour = palette_ghs3("primary"), linewidth = 1.4) +
    ggplot2::geom_point(colour = palette_ghs3("primary"), size = 2.2) +
    ggplot2::scale_y_reverse() +
    theme_ghs3() +
    labs_news(
      title = sprintf("%s \u00b7 \u5168\u7403 CHE\u4eba\u5747 \u6392\u540d\u8f68\u8ff9",
                       name),
      subtitle = "1 \u4e3a\u9886\u5148\uff1b\u6392\u540d\u4e0a\u5347 = \u7ebf\u4e0a\u63d0",
      x = NULL, y = "\u6392\u540d",
      caption = .cap_country())
}

# =============================================================================
# D. 排名 / Top / Bottom
# =============================================================================

#' Top10 / Bottom10 \u4eba\u5747 CHE （最近年）
plot_country_topbot_chepc <- function(master, year = NULL, n = 10) {
  ensure_pkgs(c("ggplot2"))
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year & is.finite(master$che_pc_usd2023), ]
  d <- d[order(-d$che_pc_usd2023), ]
  top <- utils::head(d, n); bot <- utils::tail(d, n)
  top$grp <- sprintf("Top %d", n)
  bot$grp <- sprintf("Bottom %d", n)
  m <- rbind(top, bot)
  m$country_name <- factor(m$country_name,
    levels = m$country_name[order(m$che_pc_usd2023)])
  ggplot2::ggplot(m, ggplot2::aes(x = .data$che_pc_usd2023,
                                       y = .data$country_name,
                                       fill = .data$grp)) +
    ggplot2::geom_col(alpha = 0.88) +
    ggplot2::scale_x_log10(labels = scales::dollar_format(accuracy = 1)) +
    ggplot2::scale_fill_manual(values = c(palette_ghs3("primary"),
                                            palette_ghs3("warn")),
                                name = NULL) +
    theme_ghs3(grid = "x") +
    ggplot2::theme(legend.position = "top") +
    labs_news(
      title = sprintf("\u4eba\u5747 CHE Top/Bot %d \u00b7 %d", n, year),
      subtitle = "log \u8f74\uff1b\u9ad8\u4f4e\u4e24\u6781\u5dee\u8de5 100 \u500d",
      x = "USD\u4eba\u5747 (log)", y = NULL,
      caption = .cap_country())
}

#' 寿命 Top10 / Bottom10
plot_country_topbot_lifeexp <- function(master, year = NULL, n = 10) {
  ensure_pkgs(c("ggplot2"))
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year & is.finite(master$life_exp), ]
  d <- d[order(-d$life_exp), ]
  top <- utils::head(d, n); bot <- utils::tail(d, n)
  top$grp <- sprintf("Top %d", n)
  bot$grp <- sprintf("Bottom %d", n)
  m <- rbind(top, bot)
  m$country_name <- factor(m$country_name,
    levels = m$country_name[order(m$life_exp)])
  ggplot2::ggplot(m, ggplot2::aes(x = .data$life_exp, y = .data$country_name,
                                       fill = .data$grp)) +
    ggplot2::geom_col(alpha = 0.88) +
    ggplot2::scale_fill_manual(values = c(palette_ghs3("good"),
                                            palette_ghs3("bad")),
                                name = NULL) +
    theme_ghs3(grid = "x") +
    ggplot2::theme(legend.position = "top") +
    labs_news(
      title = sprintf("\u9884\u671f\u5bff\u547d Top/Bot %d \u00b7 %d", n, year),
      subtitle = "\u4e24\u6781\u5dee\u8de5\u8d85 30 \u5e74",
      x = "\u9884\u671f\u5bff\u547d\uff08\u5e74\uff09", y = NULL,
      caption = .cap_country("WDI life expectancy"))
}

#' \u8d77\u70b9 vs \u8fdb\u6b65：散点（增长 vs 起点）
plot_country_start_progress <- function(master, y1 = 2000, y2 = NULL) {
  ensure_pkgs(c("ggplot2"))
  if (is.null(y2)) y2 <- max(master$year, na.rm = TRUE)
  d1 <- master[master$year == y1, c("iso3_code", "country_name",
                                       "continent", "che_pc_usd2023")]
  d2 <- master[master$year == y2, c("iso3_code", "che_pc_usd2023")]
  m <- merge(d1, d2, by = "iso3_code", suffixes = c(".y1", ".y2"))
  m <- m[is.finite(m$che_pc_usd2023.y1) &
         is.finite(m$che_pc_usd2023.y2) &
         m$che_pc_usd2023.y1 > 0 & m$che_pc_usd2023.y2 > 0, ]
  m$delta <- log(m$che_pc_usd2023.y2 / m$che_pc_usd2023.y1)
  if (!nrow(m)) return(ggplot2::ggplot() + ggplot2::theme_void())
  ggplot2::ggplot(m, ggplot2::aes(x = .data$che_pc_usd2023.y1,
                                       y = .data$delta,
                                       colour = .data$continent)) +
    ggplot2::geom_point(alpha = 0.85, size = 2.3) +
    ggplot2::geom_smooth(method = "loess", se = FALSE, formula = y ~ x,
      colour = palette_ghs3("ink"), linewidth = 0.9) +
    ggplot2::scale_x_log10(labels = scales::dollar_format(accuracy = 1)) +
    ggplot2::scale_y_continuous(labels = scales::number_format(accuracy = 0.1,
      suffix = " (log\u00d7)")) +
    scale_colour_brand_continent(name = NULL) +
    theme_ghs3() +
    ggplot2::theme(legend.position = "top") +
    labs_news(
      title = sprintf("\u8d77\u70b9 vs \u8fdb\u6b65\uff1a%d\u2192%d", y1, y2),
      subtitle = "log\u500d\u6570\uff1b\u53f3\u4e0b = \u9ad8\u8d77\u70b9\u4f4e\u589e\u957f",
      x = sprintf("\u4eba\u5747 CHE %d (log)", y1),
      y = "log(\u500d\u6570)",
      caption = .cap_country())
}

#' 年化增长率分布：分位 + 标注极值
plot_country_growth_dist <- function(master, y1 = 2000, y2 = NULL) {
  ensure_pkgs(c("ggplot2"))
  if (is.null(y2)) y2 <- max(master$year, na.rm = TRUE)
  d1 <- master[master$year == y1, c("iso3_code", "country_name",
                                       "che_pc_usd2023")]
  d2 <- master[master$year == y2, c("iso3_code", "che_pc_usd2023")]
  m <- merge(d1, d2, by = "iso3_code", suffixes = c(".y1", ".y2"))
  m$cagr <- (m$che_pc_usd2023.y2 / m$che_pc_usd2023.y1) ^
              (1 / (y2 - y1)) - 1
  m <- m[is.finite(m$cagr), ]
  if (!nrow(m)) return(ggplot2::ggplot() + ggplot2::theme_void())
  m <- m[order(m$cagr), ]
  labels_low <- utils::head(m, 5)
  labels_high <- utils::tail(m, 5)
  ggplot2::ggplot(m, ggplot2::aes(x = .data$cagr)) +
    ggplot2::geom_histogram(bins = 30,
      fill = palette_ghs3("primary"), alpha = 0.55,
      colour = "white") +
    ggplot2::geom_vline(xintercept = stats::median(m$cagr, na.rm = TRUE),
      linetype = 2, colour = palette_ghs3("ink"), linewidth = 0.7) +
    ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    theme_ghs3() +
    labs_news(
      title = sprintf("\u4eba\u5747 CHE CAGR \u5206\u5e03 \u00b7 %d\u2192%d",
                       y1, y2),
      subtitle = sprintf("\u4e2d\u4f4d %.1f%%\uff1b\u6700\u4f4e %s -> %s",
                          100 * stats::median(m$cagr, na.rm = TRUE),
                          labels_low$country_name[1],
                          labels_high$country_name[nrow(labels_high)]),
      x = "CAGR", y = "\u56fd\u5bb6\u6570",
      caption = .cap_country())
}

#' 多指标 Top 10 \u00b7 5 列汇总
plot_country_topn_grid <- function(master, year = NULL, n = 10) {
  ensure_pkgs(c("ggplot2"))
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year, ]
  metrics <- list(
    list(col = "che_pc_usd2023", lbl = "\u4eba\u5747 CHE",
         dir = -1),
    list(col = "gghed_che", lbl = "GGHED / CHE",   dir = -1),
    list(col = "hf3_che",   lbl = "OOP / CHE",     dir =  1),
    list(col = "life_exp",  lbl = "\u9884\u671f\u5bff\u547d", dir = -1),
    list(col = "u5mr",      lbl = "U5MR /1000",   dir =  1))
  rows <- list()
  for (m in metrics) {
    sub <- d[is.finite(d[[m$col]]), c("country_name", m$col)]
    sub <- sub[order(m$dir * sub[[m$col]]), ]
    sub <- utils::head(sub, n)
    sub$rank <- seq_len(nrow(sub))
    sub$metric <- m$lbl
    sub$value <- sub[[m$col]]
    rows[[m$col]] <- sub[, c("country_name", "rank", "metric", "value")]
  }
  df <- do.call(rbind, rows)
  df$metric <- factor(df$metric, levels = vapply(metrics, function(x) x$lbl,
                                                    character(1)))
  ggplot2::ggplot(df, ggplot2::aes(x = .data$rank, y = .data$metric,
                                          fill = .data$metric)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.4) +
    ggplot2::geom_text(ggplot2::aes(label = .data$country_name),
      colour = palette_ghs3("ink"), size = 2.7) +
    ggplot2::scale_x_continuous(breaks = 1:n) +
    scale_fill_ghs3(guide = "none") +
    theme_ghs3(grid = FALSE) +
    ggplot2::theme(panel.grid = ggplot2::element_blank()) +
    labs_news(
      title = sprintf("\u591a\u6307\u6807 Top %d \u00b7 %d", n, year),
      subtitle = "\u540c\u4e00\u56fd\u5bb6\u53ef\u591a\u6307\u6807\u4e0a\u699c\uff1b\u53cd\u6620\u7efc\u5408\u8d44\u6e90 / \u8d23\u4efb",
      x = "rank", y = NULL,
      caption = .cap_country())
}

# =============================================================================
# E. 专题国家集
# =============================================================================

#' BRICS \u4eba\u5747 CHE 与寿命双轴小倍数
plot_country_brics_dual <- function(master) {
  isos <- ghs_country_sets$brics
  ensure_pkgs(c("ggplot2"))
  d <- .country_subset(master, isos)
  d <- d[is.finite(d$che_pc_usd2023) & is.finite(d$life_exp), ]
  if (!nrow(d)) return(ggplot2::ggplot() + ggplot2::theme_void())
  rng_le <- range(d$life_exp, na.rm = TRUE)
  rng_ch <- range(log10(d$che_pc_usd2023), na.rm = TRUE)
  k <- (rng_le[2] - rng_le[1]) / (rng_ch[2] - rng_ch[1])
  b <- rng_le[1] - k * rng_ch[1]
  d$le2 <- d$life_exp
  d$ch2 <- log10(d$che_pc_usd2023) * k + b
  long <- rbind(
    data.frame(country = d$country_name, year = d$year,
               metric = "\u9884\u671f\u5bff\u547d", val = d$le2),
    data.frame(country = d$country_name, year = d$year,
               metric = "log CHE\u4eba\u5747 (\u7f29\u653e)",
               val = d$ch2)
  )
  long$country <- factor(long$country, levels = unique(d$country_name))
  ggplot2::ggplot(long, ggplot2::aes(x = .data$year, y = .data$val,
                                          colour = .data$metric)) +
    ggplot2::geom_line(linewidth = 1.2) +
    ggplot2::facet_wrap(~ .data$country, ncol = 3, scales = "free_y") +
    ggplot2::scale_colour_manual(values = c(palette_ghs3("primary"),
                                              palette_ghs3("good")),
                                  name = NULL) +
    theme_ghs3() +
    ggplot2::theme(legend.position = "top") +
    labs_news(
      title = "BRICS \u00b7 \u5bff\u547d vs log\u4eba\u5747 CHE (\u540c\u8f74\u7f29\u653e)",
      subtitle = "\u4e24\u6307\u6807\u540c\u4f4d\u540c\u8d8b\u52bf = \u8d44\u91d1 -> \u4ea7\u51fa\u8f6c\u5316\u9ad8",
      x = NULL, y = NULL,
      caption = .cap_country())
}

#' G7 \u8de8\u56fd OOP 趋势对比
plot_country_g7_oop <- function(master) {
  isos <- ghs_country_sets$g7
  ensure_pkgs(c("ggplot2"))
  d <- .country_subset(master, isos)
  d <- d[is.finite(d$hf3_che), ]
  if (!nrow(d)) return(ggplot2::ggplot() + ggplot2::theme_void())
  ggplot2::ggplot(d, ggplot2::aes(x = .data$year, y = .data$hf3_che,
                                       colour = .data$country_name)) +
    ggplot2::geom_line(linewidth = 1.3) +
    ggplot2::scale_y_continuous(
        labels = scales::percent_format(accuracy = 1, scale = 1)) +
    scale_colour_ghs3(name = NULL) +
    theme_ghs3() +
    ggplot2::theme(legend.position = "top") +
    labs_news(
      title = "G7 \u00b7 OOP / CHE \u8de8\u56fd\u8d8b\u52bf",
      subtitle = "\u7f8e\u56fd \u00b7 \u610f\u5927\u5229 OOP \u504f\u9ad8\uff1b\u82f1 / \u6cd5 / \u52a0 \u504f\u4f4e",
      x = NULL, y = "OOP / CHE",
      caption = .cap_country())
}

#' EU5 vs ASEAN5 \u5e73\u5747 CHE\u4eba\u5747 对比
plot_country_eu_vs_asean <- function(master) {
  ensure_pkgs(c("ggplot2"))
  eu <- ghs_country_sets$eu5
  asean <- ghs_country_sets$asean5
  d <- master[master$iso3_code %in% c(eu, asean) &
              is.finite(master$che_pc_usd2023), ]
  d$grp <- ifelse(d$iso3_code %in% eu, "EU5", "ASEAN5")
  agg <- stats::aggregate(che_pc_usd2023 ~ grp + year, data = d,
                          FUN = function(v) stats::median(v, na.rm = TRUE))
  ggplot2::ggplot(agg, ggplot2::aes(x = .data$year, y = .data$che_pc_usd2023,
                                         colour = .data$grp)) +
    ggplot2::geom_line(linewidth = 1.4) +
    ggplot2::geom_point(size = 2) +
    ggplot2::scale_y_log10(labels = scales::dollar_format(accuracy = 1)) +
    ggplot2::scale_colour_manual(values = c("EU5" = palette_ghs3("primary"),
                                              "ASEAN5" = palette_ghs3("secondary")),
                                  name = NULL) +
    theme_ghs3() +
    ggplot2::theme(legend.position = "top") +
    labs_news(
      title = "EU5 vs ASEAN5 \u00b7 \u4eba\u5747 CHE \u4e2d\u4f4d",
      subtitle = "EU5 = DEU \u00b7 FRA \u00b7 ITA \u00b7 ESP \u00b7 NLD\uff1bASEAN5 = IDN \u00b7 THA \u00b7 VNM \u00b7 PHL \u00b7 MYS",
      x = NULL, y = "USD\u4eba\u5747 (log)",
      caption = .cap_country())
}

#' Nordic 国家 OOP & 寿命联动
plot_country_nordic_combo <- function(master) {
  ensure_pkgs(c("ggplot2"))
  isos <- ghs_country_sets$nordic
  d <- .country_subset(master, isos)
  d <- d[is.finite(d$hf3_che) & is.finite(d$life_exp), ]
  if (!nrow(d)) return(ggplot2::ggplot() + ggplot2::theme_void())
  ggplot2::ggplot(d, ggplot2::aes(x = .data$hf3_che, y = .data$life_exp,
                                       colour = .data$country_name)) +
    ggplot2::geom_path(linewidth = 1.1, alpha = 0.85) +
    ggplot2::geom_point(ggplot2::aes(size = .data$year), alpha = 0.85) +
    ggplot2::scale_size_continuous(range = c(1.5, 4.2), guide = "none") +
    ggplot2::scale_x_continuous(
        labels = scales::percent_format(accuracy = 1, scale = 1)) +
    scale_colour_ghs3(name = NULL) +
    theme_ghs3() +
    ggplot2::theme(legend.position = "top") +
    labs_news(
      title = "Nordic 5 \u56fd \u00b7 OOP vs \u5bff\u547d \u8def\u5f84",
      subtitle = "\u8d8a\u9760\u53f3\u4e0a = OOP \u4f4e \u00b7 \u5bff\u547d\u9ad8\uff1b\u70b9\u5927\u5c0f = \u5e74\u4efd",
      x = "OOP / CHE", y = "\u9884\u671f\u5bff\u547d",
      caption = .cap_country())
}

#' Low-LE 与 High-LE 国家 stark 对比
plot_country_lowle_highle <- function(master) {
  ensure_pkgs(c("ggplot2"))
  isos <- c(ghs_country_sets$lowle, ghs_country_sets$highle)
  d <- master[master$iso3_code %in% isos & is.finite(master$life_exp), ]
  if (!nrow(d)) return(ggplot2::ggplot() + ggplot2::theme_void())
  d$grp <- ifelse(d$iso3_code %in% ghs_country_sets$lowle,
                   "Low-LE 5", "High-LE 5")
  ggplot2::ggplot(d, ggplot2::aes(x = .data$year, y = .data$life_exp,
                                       group = .data$country_name,
                                       colour = .data$grp)) +
    ggplot2::geom_line(linewidth = 1.1, alpha = 0.85) +
    ggplot2::scale_colour_manual(values = c("Low-LE 5" = palette_ghs3("bad"),
                                              "High-LE 5" = palette_ghs3("good")),
                                  name = NULL) +
    theme_ghs3() +
    ggplot2::theme(legend.position = "top") +
    labs_news(
      title = "Low-LE 5 vs High-LE 5 \u00b7 \u9884\u671f\u5bff\u547d",
      subtitle = "\u4e24\u7ec4\u95f4\u504f\u79bb >25 \u5e74\uff1b\u793a\u610f\u4ea7\u51fa\u9886\u5148\u4e0e\u843d\u540e",
      x = NULL, y = "\u9884\u671f\u5bff\u547d",
      caption = .cap_country("WDI life_exp"))
}

# =============================================================================
# 导出器
# =============================================================================

#' 批量导出 B5 国家专题图集
ghs_export_country <- function(master = NULL, fig_dir = NULL) {
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
    cty_trend_chepc_chn    = plot_country_trend_chepc(master, "CHN"),
    cty_trend_chepc_usa    = plot_country_trend_chepc(master, "USA"),
    cty_hf_stack_chn       = plot_country_hf_stack(master, "CHN"),
    cty_hf_stack_usa       = plot_country_hf_stack(master, "USA"),
    cty_hc_bars_chn        = plot_country_hc_bars(master, "CHN"),
    cty_outcome_dual_chn   = plot_country_outcome_dual(master, "CHN"),
    cty_dashboard_chn      = plot_country_dashboard(master, "CHN"),
    cty_smallmult_brics    = plot_country_smallmult_chepc(master),
    cty_compare_g7         = plot_country_compare_chepc(master),
    cty_compare_heat_brics = plot_country_compare_heat(master),
    cty_pair_usa_chn       = plot_country_pair_hc(master),
    cty_cagr_brics         = plot_country_cagr_compare(master),
    cty_zscore_chn         = plot_country_zscore_income(master),
    cty_oop_gap_chn        = plot_country_oop_gap(master),
    cty_lifeexp_gap_chn    = plot_country_lifeexp_gap(master),
    cty_multi_vs_global    = plot_country_multi_vs_global(master),
    cty_rank_trend_chn     = plot_country_rank_trend(master),
    cty_topbot_chepc       = plot_country_topbot_chepc(master),
    cty_topbot_lifeexp     = plot_country_topbot_lifeexp(master),
    cty_start_progress     = plot_country_start_progress(master),
    cty_growth_dist        = plot_country_growth_dist(master),
    cty_topn_grid          = plot_country_topn_grid(master),
    cty_brics_dual         = plot_country_brics_dual(master),
    cty_g7_oop             = plot_country_g7_oop(master),
    cty_eu_vs_asean        = plot_country_eu_vs_asean(master),
    cty_nordic_combo       = plot_country_nordic_combo(master),
    cty_lowle_highle       = plot_country_lowle_highle(master)
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
      message("[country export] ", nm, " failed: ", conditionMessage(e))
      FALSE
    })
  }
  invisible(out)
}
