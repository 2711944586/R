
if (!exists("ensure_pkgs", mode = "function")) {
  source(file.path("\u7a0b\u5e8f", "00_utils.R"))
}

.cap_equity <- function(extra = NULL) {
  base <- "\u6570\u636e \u00b7 WHO GHED 2024\uff1bOOP=HF3"
  if (is.null(extra) || !nzchar(extra)) base
  else paste0(base, " \u00b7 ", extra)
}

.weighted_gini <- function(x, w = NULL) {
  ok <- is.finite(x) & x >= 0
  if (!is.null(w)) ok <- ok & is.finite(w) & w > 0
  x <- x[ok]; w <- if (is.null(w)) rep(1, length(x)) else w[ok]
  if (length(x) < 2) return(NA_real_)
  ord <- order(x)
  x <- x[ord]; w <- w[ord]
  cw <- cumsum(w) / sum(w)
  cx <- cumsum(x * w) / sum(x * w)
  L <- sum((cw[-1] - cw[-length(cw)]) * (cx[-1] + cx[-length(cx)]) / 2)
  1 - 2 * L
}

.weighted_lorenz <- function(x, w = NULL) {
  ok <- is.finite(x) & x >= 0
  if (!is.null(w)) ok <- ok & is.finite(w) & w > 0
  x <- x[ok]; w <- if (is.null(w)) rep(1, length(x)) else w[ok]
  if (!length(x)) return(data.frame(p = numeric(0), L = numeric(0)))
  ord <- order(x)
  x <- x[ord]; w <- w[ord]
  cw <- c(0, cumsum(w) / sum(w))
  cx <- c(0, cumsum(x * w) / sum(x * w))
  data.frame(p = cw, L = cx)
}

.weighted_theil_t <- function(x, w = NULL) {
  ok <- is.finite(x) & x > 0
  if (!is.null(w)) ok <- ok & is.finite(w) & w > 0
  x <- x[ok]; w <- if (is.null(w)) rep(1, length(x)) else w[ok]
  if (length(x) < 2) return(NA_real_)
  W <- sum(w); xb <- sum(x * w) / W
  sum((w / W) * (x / xb) * log(x / xb))
}

.theil_between_within <- function(x, group, w = NULL) {
  ok <- is.finite(x) & x > 0 & !is.na(group)
  if (!is.null(w)) ok <- ok & is.finite(w) & w > 0
  x <- x[ok]; g <- group[ok]
  w <- if (is.null(w)) rep(1, length(x)) else w[ok]
  if (!length(x)) return(c(between = NA_real_, within = NA_real_))
  W <- sum(w); xb <- sum(x * w) / W
  T_b <- 0; T_w <- 0
  for (lvl in unique(g)) {
    sel <- g == lvl
    Wk <- sum(w[sel]); xk <- sum(x[sel] * w[sel]) / Wk
    T_b <- T_b + (Wk / W) * (xk / xb) * log(xk / xb)
    Tk <- .weighted_theil_t(x[sel], w[sel])
    if (is.finite(Tk)) T_w <- T_w + (Wk / W) * (xk / xb) * Tk
  }
  c(between = T_b, within = T_w)
}


plot_equity_oop_share_global <- function(master) {
  ensure_pkgs(c("ggplot2"))
  d <- master[is.finite(master$hf3_che), ]
  if (!nrow(d)) return(ggplot2::ggplot() + ggplot2::theme_void())
  q <- stats::aggregate(hf3_che ~ year, data = d,
    FUN = function(v) stats::quantile(v, c(0.25, 0.5, 0.75), na.rm = TRUE))
  mat <- as.data.frame(q$hf3_che)
  agg <- data.frame(year = q$year, p25 = mat[, 1],
                    p50 = mat[, 2], p75 = mat[, 3])
  ggplot2::ggplot(agg, ggplot2::aes(x = .data$year)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$p25, ymax = .data$p75),
      fill = palette_ghs3("warn"), alpha = 0.28) +
    ggplot2::geom_line(ggplot2::aes(y = .data$p50),
      colour = palette_ghs3("warn"), linewidth = 1.4) +
    ggplot2::geom_point(ggplot2::aes(y = .data$p50),
      colour = palette_ghs3("warn"), size = 2) +
    ggplot2::scale_y_continuous(
        labels = scales::percent_format(accuracy = 1, scale = 1)) +
    theme_ghs3() +
    labs_news(
      title = "OOP \u5360 CHE \u6bd4\u4f8b \u00b7 \u5168\u7403\u4e2d\u4f4d\u8f68\u8ff9",
      subtitle = "\u9634\u5f71 = IQR\uff1b\u4e2d\u4f4d\u4ece 2000 \u7ea6 38% \u964d\u81f3 2022 \u7ea6 28%",
      x = NULL, y = "OOP / CHE",
      caption = .cap_equity())
}

plot_equity_oop_share_income <- function(master) {
  ensure_pkgs(c("ggplot2"))
  d <- master[is.finite(master$hf3_che) &
              !is.na(master$income_group), ]
  d$income_group <- factor(d$income_group,
    levels = c("Low income", "Lower middle income",
               "Upper middle income", "High income"))
  ggplot2::ggplot(d, ggplot2::aes(
      x = .data$year, y = .data$hf3_che,
      colour = .data$income_group)) +
    ggplot2::stat_summary(geom = "line", fun = stats::median, linewidth = 1.2) +
    ggplot2::scale_y_continuous(
        labels = scales::percent_format(accuracy = 1, scale = 1)) +
    scale_colour_brand_income(name = NULL) +
    theme_ghs3() +
    ggplot2::theme(legend.position = "top") +
    labs_news(
      title = "OOP / CHE \u00b7 \u6309\u6536\u5165\u7ec4\u4e2d\u4f4d",
      subtitle = "\u4f4e\u6536\u5165\u56fd\u4ecd\u9760\u8fd1 40-50%\uff1b\u9ad8\u6536\u5165\u56fd\u7a33\u5b9a\u7ea6 13-20%",
      x = NULL, y = "OOP / CHE",
      caption = .cap_equity("\u533b\u9886\u519c\u9020\u8865\u52a9\u4e3b\u8981\u9006\u8f6c\u4e2d\u9ad8\u6536\u5165\u56fd"))
}

plot_equity_oop_share_continent <- function(master) {
  ensure_pkgs(c("ggplot2"))
  d <- master[is.finite(master$hf3_che) & !is.na(master$continent), ]
  ggplot2::ggplot(d, ggplot2::aes(
      x = .data$year, y = .data$hf3_che,
      colour = .data$continent)) +
    ggplot2::stat_summary(geom = "line", fun = stats::median, linewidth = 1.1) +
    ggplot2::scale_y_continuous(
        labels = scales::percent_format(accuracy = 1, scale = 1)) +
    scale_colour_brand_continent(name = NULL) +
    theme_ghs3() +
    ggplot2::theme(legend.position = "top") +
    labs_news(
      title = "OOP / CHE \u00b7 \u5927\u6d32\u4e2d\u4f4d\u8f68\u8ff9",
      subtitle = "\u4e9a\u6d32 / \u975e\u6d32 \u4f4d\u4e8e\u9ad8\u4f4d\uff1b\u6b27\u6d32 \u00b7 \u5317\u7f8e \u9760\u4f4e",
      x = NULL, y = "OOP / CHE",
      caption = .cap_equity())
}

plot_equity_oop_box_income <- function(master, year = NULL) {
  ensure_pkgs(c("ggplot2"))
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year & is.finite(master$hf3_che) &
              !is.na(master$income_group), ]
  d$income_group <- factor(d$income_group,
    levels = c("Low income", "Lower middle income",
               "Upper middle income", "High income"))
  ggplot2::ggplot(d, ggplot2::aes(
      x = .data$income_group, y = .data$hf3_che,
      fill = .data$income_group)) +
    ggplot2::geom_violin(alpha = 0.4, colour = NA, scale = "width") +
    ggplot2::geom_boxplot(width = 0.22, outlier.shape = NA,
      fill = "white", colour = "#1a1f28") +
    ggplot2::geom_jitter(width = 0.08, alpha = 0.55, size = 1.3,
      ggplot2::aes(colour = .data$income_group)) +
    ggplot2::scale_y_continuous(
        labels = scales::percent_format(accuracy = 1, scale = 1)) +
    scale_fill_brand_income(guide = "none") +
    scale_colour_brand_income(guide = "none") +
    theme_ghs3() +
    labs_news(
      title = sprintf("OOP / CHE \u5206\u5e03 \u00b7 \u6309\u6536\u5165\u7ec4 \u00b7 %d", year),
      subtitle = "\u70b9 = \u56fd\u5bb6\uff1b\u4f4e\u6536\u5165\u56fd\u504f\u53f3\uff08\u9ad8\u8d1f\u62c5\uff09",
      x = NULL, y = "OOP / CHE",
      caption = .cap_equity())
}

plot_equity_catastrophic_proxy <- function(master, year = NULL) {
  ensure_pkgs(c("ggplot2"))
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year &
              is.finite(master$hf3_usd2023) & is.finite(master$pop) &
              is.finite(master$gdp_pc_usd) & master$gdp_pc_usd > 0 &
              master$pop > 0, ]
  d$oop_pc <- d$hf3_usd2023 / d$pop
  d$share <- d$oop_pc / d$gdp_pc_usd
  d <- d[is.finite(d$share) & d$share > 0 & d$share < 0.5, ]
  if (!nrow(d)) return(ggplot2::ggplot() + ggplot2::theme_void())
  ggplot2::ggplot(d, ggplot2::aes(
      x = .data$gdp_pc_usd, y = .data$share,
      colour = .data$continent)) +
    ggplot2::geom_point(alpha = 0.85, size = 2.4) +
    ggplot2::geom_hline(yintercept = 0.10, linetype = 2,
      colour = palette_ghs3("bad"), linewidth = 0.7) +
    ggplot2::geom_hline(yintercept = 0.25, linetype = 3,
      colour = palette_ghs3("bad"), linewidth = 0.7) +
    ggplot2::scale_x_log10(labels = scales::dollar_format(scale = 1e-3,
      accuracy = 1, suffix = "K")) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_colour_brand_continent(name = NULL) +
    theme_ghs3() +
    ggplot2::theme(legend.position = "top") +
    labs_news(
      title = sprintf("\u707e\u96be\u6027\u652f\u51fa\u4ee3\u7406 \u00b7 %d", year),
      subtitle = "OOP \u4eba\u5747 / GDP \u4eba\u5747\uff1b\u8651\u7ebf 10% / 25%",
      x = "GDP \u4eba\u5747 (log)", y = "OOP\u4eba\u5747 / GDP\u4eba\u5747",
      caption = .cap_equity("\u5b97\u6307\u6807 SDG 3.8.2 \u8fd1\u4f3c"))
}


plot_equity_lorenz_che <- function(master,
                                    years = c(2000, 2010, 2018, 2022)) {
  ensure_pkgs(c("ggplot2"))
  rows <- list()
  for (y in years) {
    d <- master[master$year == y &
                is.finite(master$che_pc_usd2023) &
                is.finite(master$pop) & master$pop > 0, ]
    if (!nrow(d)) next
    L <- .weighted_lorenz(d$che_pc_usd2023, d$pop)
    L$year <- y
    rows[[as.character(y)]] <- L
  }
  if (!length(rows)) return(ggplot2::ggplot() + ggplot2::theme_void())
  ld <- do.call(rbind, rows)
  ld$year <- factor(ld$year, levels = sort(unique(ld$year)))
  ggplot2::ggplot(ld, ggplot2::aes(x = .data$p, y = .data$L,
                                       colour = .data$year)) +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = 2,
      colour = palette_ghs3("line_strong"), linewidth = 0.7) +
    ggplot2::geom_line(linewidth = 1.2) +
    ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_colour_ghs3(name = NULL) +
    theme_ghs3() +
    ggplot2::theme(legend.position = "top") +
    labs_news(
      title = "\u4eba\u5747 CHE \u00b7 \u4eba\u53e3\u52a0\u6743\u6d1b\u4f26\u5179\u66f2\u7ebf",
      subtitle = "\u8d8a\u504f\u79bb\u5bf9\u89d2\u7ebf \u2192 \u8d8a\u4e0d\u5e73\u7b49",
      x = "\u4eba\u53e3\u7d2f\u8ba1\u5360\u6bd4", y = "\u603b CHE \u7d2f\u8ba1\u5360\u6bd4",
      caption = .cap_equity("\u4ee5 WDI \u4eba\u53e3\u4e3a\u6743"))
}

plot_equity_gini_trend <- function(master) {
  ensure_pkgs(c("ggplot2"))
  yrs <- sort(unique(master$year))
  g <- vapply(yrs, function(y) {
    d <- master[master$year == y &
                is.finite(master$che_pc_usd2023) &
                is.finite(master$pop) & master$pop > 0, ]
    .weighted_gini(d$che_pc_usd2023, d$pop)
  }, numeric(1))
  agg <- data.frame(year = yrs, gini = g)
  ggplot2::ggplot(agg, ggplot2::aes(x = .data$year, y = .data$gini)) +
    ggplot2::geom_line(colour = palette_ghs3("primary"), linewidth = 1.4) +
    ggplot2::geom_point(colour = palette_ghs3("primary"), size = 2.4) +
    theme_ghs3() +
    labs_news(
      title = "\u4eba\u5747 CHE \u00b7 \u4eba\u53e3\u52a0\u6743 Gini \u8f68\u8ff9",
      subtitle = "Gini \u4ece 2000 \u7ea6 0.66 \u964d\u81f3 2022 \u7ea6 0.55\uff08\u53d8\u5316\u7f13\u6162\uff09",
      x = NULL, y = "Gini",
      caption = .cap_equity())
}

plot_equity_theil_between_within <- function(master) {
  ensure_pkgs(c("ggplot2"))
  yrs <- sort(unique(master$year))
  out <- lapply(yrs, function(y) {
    d <- master[master$year == y &
                is.finite(master$che_pc_usd2023) &
                is.finite(master$pop) & master$pop > 0 &
                !is.na(master$income_group), ]
    tb <- .theil_between_within(d$che_pc_usd2023, d$income_group, d$pop)
    data.frame(year = y, between = unname(tb["between"]),
               within = unname(tb["within"]))
  })
  agg <- do.call(rbind, out)
  long <- rbind(
    data.frame(year = agg$year, part = "\u7ec4\u95f4", val = agg$between),
    data.frame(year = agg$year, part = "\u7ec4\u5185", val = agg$within)
  )
  ggplot2::ggplot(long, ggplot2::aes(x = .data$year, y = .data$val,
                                         fill = .data$part)) +
    ggplot2::geom_area(alpha = 0.85, position = "stack") +
    ggplot2::scale_fill_manual(
      values = c("\u7ec4\u95f4" = palette_ghs3("primary"),
                 "\u7ec4\u5185" = palette_ghs3("secondary")),
      name = NULL) +
    theme_ghs3() +
    ggplot2::theme(legend.position = "top") +
    labs_news(
      title = "Theil-T \u5206\u89e3 \u00b7 \u4eba\u5747 CHE",
      subtitle = "\u7ec4\u95f4\uff08\u6536\u5165\u7ec4\uff09\u4e3b\u5bfc\u603b\u4e0d\u5e73\u7b49\uff1b\u7ec4\u5185\u4e0b\u964d\u8d8b\u52bf\u5c0f",
      x = NULL, y = "Theil \u6307\u6570",
      caption = .cap_equity("\u4eba\u53e3\u52a0\u6743"))
}

plot_equity_concentration_lifeexp <- function(master, year = NULL) {
  ensure_pkgs(c("ggplot2"))
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year &
              is.finite(master$che_pc_usd2023) &
              is.finite(master$life_exp) &
              is.finite(master$pop) & master$pop > 0, ]
  if (!nrow(d)) return(ggplot2::ggplot() + ggplot2::theme_void())
  ord <- order(d$che_pc_usd2023)
  d <- d[ord, ]
  d$cw <- cumsum(d$pop) / sum(d$pop)
  d$cy <- cumsum(d$life_exp * d$pop) / sum(d$life_exp * d$pop)
  ggplot2::ggplot(d, ggplot2::aes(x = .data$cw, y = .data$cy)) +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = 2,
      colour = palette_ghs3("line_strong"), linewidth = 0.7) +
    ggplot2::geom_line(colour = palette_ghs3("primary"), linewidth = 1.4) +
    ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    theme_ghs3() +
    labs_news(
      title = sprintf("\u96c6\u4e2d\u5ea6\u66f2\u7ebf\uff1aCHE\uff08\u6392\u5e8f\uff09 \u2194 \u5bff\u547d \u00b7 %d", year),
      subtitle = "\u9760\u8fd1\u5bf9\u89d2\u7ebf = \u5bff\u547d\u5728\u6536\u5165\u7ec4\u95f4\u8f83\u5e73\u5747",
      x = "\u6309 CHE \u6392\u5e8f\u7684\u4eba\u53e3\u5360\u6bd4", y = "\u5bff\u547d\u52a0\u6743\u5360\u6bd4",
      caption = .cap_equity("WHO GHED + WDI life expectancy"))
}

plot_equity_index_dot <- function(master, year = NULL) {
  ensure_pkgs(c("ggplot2"))
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  metrics <- c("che_pc_usd2023", "gghed_usd2023", "hf3_usd2023",
               "ext_usd2023")
  rows <- list()
  for (k in metrics) {
    d <- master[master$year == year &
                is.finite(master[[k]]) &
                is.finite(master$pop) & master$pop > 0 &
                master[[k]] > 0, ]
    if (!nrow(d)) next
    rows[[k]] <- data.frame(
      metric = k,
      gini   = .weighted_gini(d[[k]], d$pop),
      theil  = .weighted_theil_t(d[[k]], d$pop))
  }
  if (!length(rows)) return(ggplot2::ggplot() + ggplot2::theme_void())
  agg <- do.call(rbind, rows)
  agg$metric <- factor(agg$metric, levels = metrics, labels = c(
    "\u4eba\u5747 CHE", "\u653f\u5e9c GGHED", "\u81ea\u4ed8 OOP",
    "\u5916\u90e8\u63f4\u52a9 EXT"))
  long <- rbind(
    data.frame(metric = agg$metric, idx = "Gini",  val = agg$gini),
    data.frame(metric = agg$metric, idx = "Theil", val = agg$theil)
  )
  ggplot2::ggplot(long, ggplot2::aes(x = .data$val, y = .data$metric,
                                         colour = .data$idx)) +
    ggplot2::geom_point(size = 3.8) +
    ggplot2::geom_segment(ggplot2::aes(x = 0, xend = .data$val,
        y = .data$metric, yend = .data$metric), linewidth = 0.6) +
    ggplot2::scale_colour_manual(
      values = c("Gini" = palette_ghs3("primary"),
                 "Theil" = palette_ghs3("highlight")), name = NULL) +
    theme_ghs3() +
    ggplot2::theme(legend.position = "top") +
    labs_news(
      title = sprintf("\u4e0d\u5e73\u7b49\u6307\u6570 \u00b7 \u591a\u8d44\u91d1\u8d44\u6e90 \u00b7 %d", year),
      subtitle = "OOP \u4e0d\u5e73\u7b49\u8f83\u4f4e\uff1bEXT \u8d44\u91d1\u96c6\u4e2d\u5ea6\u6700\u9ad8",
      x = NULL, y = NULL,
      caption = .cap_equity())
}


plot_equity_che_pc_iqr <- function(master) {
  ensure_pkgs(c("ggplot2"))
  d <- master[is.finite(master$che_pc_usd2023) &
              master$che_pc_usd2023 > 0, ]
  q <- stats::aggregate(che_pc_usd2023 ~ year, data = d,
    FUN = function(v) stats::quantile(v, c(0.10, 0.25, 0.50, 0.75, 0.90),
                                       na.rm = TRUE))
  mat <- as.data.frame(q$che_pc_usd2023)
  agg <- data.frame(year = q$year, p10 = mat[, 1], p25 = mat[, 2],
                    p50 = mat[, 3], p75 = mat[, 4], p90 = mat[, 5])
  ggplot2::ggplot(agg, ggplot2::aes(x = .data$year)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$p10, ymax = .data$p90),
      fill = palette_ghs3("primary"), alpha = 0.18) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$p25, ymax = .data$p75),
      fill = palette_ghs3("primary"), alpha = 0.30) +
    ggplot2::geom_line(ggplot2::aes(y = .data$p50),
      colour = palette_ghs3("primary"), linewidth = 1.4) +
    ggplot2::scale_y_log10(labels = scales::dollar_format(accuracy = 1)) +
    theme_ghs3() +
    labs_news(
      title = "\u4eba\u5747 CHE \u00b7 \u8de8\u56fd\u5206\u4f4d (log)",
      subtitle = "\u4e2d\u95f4\u9634\u5f71 = IQR\uff1b\u5916\u9634 = 80% \u6781\u5dee",
      x = NULL, y = "USD\u4eba\u5747 (2023, log)",
      caption = .cap_equity())
}

plot_equity_top_bottom_ratio <- function(master) {
  ensure_pkgs(c("ggplot2"))
  yrs <- sort(unique(master$year))
  r <- vapply(yrs, function(y) {
    d <- master[master$year == y & is.finite(master$che_pc_usd2023) &
                master$che_pc_usd2023 > 0, ]
    if (nrow(d) < 10) return(NA_real_)
    q <- stats::quantile(d$che_pc_usd2023, c(0.20, 0.80), na.rm = TRUE)
    top <- mean(d$che_pc_usd2023[d$che_pc_usd2023 >= q[2]], na.rm = TRUE)
    bot <- mean(d$che_pc_usd2023[d$che_pc_usd2023 <= q[1]], na.rm = TRUE)
    if (bot <= 0) NA_real_ else top / bot
  }, numeric(1))
  agg <- data.frame(year = yrs, ratio = r)
  ggplot2::ggplot(agg, ggplot2::aes(x = .data$year, y = .data$ratio)) +
    ggplot2::geom_line(colour = palette_ghs3("highlight"), linewidth = 1.4) +
    ggplot2::geom_point(colour = palette_ghs3("highlight"), size = 2.4) +
    theme_ghs3() +
    labs_news(
      title = "\u4eba\u5747 CHE \u00b7 Top20% / Bottom20% \u5747\u503c\u6bd4",
      subtitle = "\u5012\u6570 = \u4f4e\u5217\u5728\u9ad8\u5217\u4e2d\u7684\u4ee3\u8868\u9762\u79ef\uff1b\u8d8a\u9ad8 \u2192 \u8d8a\u4e0d\u5e73\u7b49",
      x = NULL, y = "\u500d\u6570",
      caption = .cap_equity("80/20 \u5206\u4f4d"))
}

plot_equity_dispersion_income <- function(master) {
  ensure_pkgs(c("ggplot2"))
  d <- master[is.finite(master$che_pc_usd2023) &
              master$che_pc_usd2023 > 0 &
              !is.na(master$income_group), ]
  d$income_group <- factor(d$income_group,
    levels = c("Low income", "Lower middle income",
               "Upper middle income", "High income"))
  cvf <- function(v) stats::sd(v, na.rm = TRUE) / mean(v, na.rm = TRUE)
  agg <- stats::aggregate(che_pc_usd2023 ~ income_group + year, data = d,
                          FUN = cvf)
  ggplot2::ggplot(agg, ggplot2::aes(x = .data$year, y = .data$che_pc_usd2023,
                                       colour = .data$income_group)) +
    ggplot2::geom_line(linewidth = 1.2) +
    scale_colour_brand_income(name = NULL) +
    theme_ghs3() +
    ggplot2::theme(legend.position = "top") +
    labs_news(
      title = "\u7ec4\u5185\u53d8\u5f02\u7cfb\u6570 (CV) \u00b7 \u4eba\u5747 CHE",
      subtitle = "\u4f4e\u6536\u5165\u7ec4\u53d8\u5f02\u8f83\u5927\uff1b\u9ad8\u6536\u5165\u7ec4\u8f83\u7d27\u7f29",
      x = NULL, y = "CV (SD / mean)",
      caption = .cap_equity())
}

plot_equity_logvar_trend <- function(master) {
  ensure_pkgs(c("ggplot2"))
  d <- master[is.finite(master$che_pc_usd2023) &
              master$che_pc_usd2023 > 0, ]
  d$logche <- log(d$che_pc_usd2023)
  agg <- stats::aggregate(logche ~ year, data = d,
                          FUN = function(v) stats::var(v, na.rm = TRUE))
  ggplot2::ggplot(agg, ggplot2::aes(x = .data$year, y = .data$logche)) +
    ggplot2::geom_line(colour = palette_ghs3("primary"), linewidth = 1.4) +
    ggplot2::geom_point(colour = palette_ghs3("primary"), size = 2.3) +
    theme_ghs3() +
    labs_news(
      title = "log(\u4eba\u5747 CHE) \u00b7 \u8de8\u56fd\u65b9\u5dee",
      subtitle = "\u4e0b\u964d \u2192 \u8de8\u56fd\u6536\u655b\uff1b\u4e0a\u5347 \u2192 \u53d1\u6563",
      x = NULL, y = "Var(log CHE\u4eba\u5747)",
      caption = .cap_equity())
}

plot_equity_che_fan <- function(master) {
  ensure_pkgs(c("ggplot2"))
  d <- master[is.finite(master$che_pc_usd2023) &
              master$che_pc_usd2023 > 0, ]
  qs <- c(0.05, 0.25, 0.50, 0.75, 0.95)
  q <- stats::aggregate(che_pc_usd2023 ~ year, data = d,
    FUN = function(v) stats::quantile(v, qs, na.rm = TRUE))
  mat <- as.data.frame(q$che_pc_usd2023)
  agg <- data.frame(year = q$year, p05 = mat[, 1], p25 = mat[, 2],
                    p50 = mat[, 3], p75 = mat[, 4], p95 = mat[, 5])
  ggplot2::ggplot(agg, ggplot2::aes(x = .data$year)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$p05, ymax = .data$p95),
      fill = palette_ghs3("primary"), alpha = 0.10) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$p25, ymax = .data$p75),
      fill = palette_ghs3("primary"), alpha = 0.25) +
    ggplot2::geom_line(ggplot2::aes(y = .data$p50),
      colour = palette_ghs3("primary"), linewidth = 1.5) +
    ggplot2::scale_y_log10(labels = scales::dollar_format(accuracy = 1)) +
    theme_ghs3() +
    labs_news(
      title = "\u4eba\u5747 CHE \u00b7 \u8de8\u56fd\u5206\u4f4d\u626d\u7ebf\u56fe",
      subtitle = "5/25/50/75/95 \u5206\u4f4d\u626d\u7ebf",
      x = NULL, y = "USD\u4eba\u5747 (2023, log)",
      caption = .cap_equity())
}


plot_equity_continent_oop_box <- function(master, year = NULL) {
  ensure_pkgs(c("ggplot2"))
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year & is.finite(master$hf3_che) &
              !is.na(master$continent), ]
  ggplot2::ggplot(d, ggplot2::aes(
      x = .data$continent, y = .data$hf3_che,
      fill = .data$continent)) +
    ggplot2::geom_boxplot(outlier.shape = NA, alpha = 0.55,
      colour = "#1a1f28") +
    ggplot2::geom_jitter(width = 0.18, alpha = 0.45, size = 1.2) +
    ggplot2::scale_y_continuous(
        labels = scales::percent_format(accuracy = 1, scale = 1)) +
    scale_fill_brand_continent(guide = "none") +
    theme_ghs3() +
    labs_news(
      title = sprintf("OOP / CHE \u00b7 \u5927\u6d32\u7bb1\u7ebf \u00b7 %d", year),
      subtitle = "\u4e9a\u6d32 / \u975e\u6d32 \u4e2d\u4f4d\u8f83\u9ad8\uff1b\u5404\u6d32\u5185\u90e8\u5dee\u5f02\u4e5f\u5927",
      x = NULL, y = "OOP / CHE",
      caption = .cap_equity())
}

plot_equity_region_heat <- function(master) {
  ensure_pkgs(c("ggplot2"))
  d <- master[is.finite(master$che_pc_usd2023) &
              master$che_pc_usd2023 > 0 &
              !is.na(master$region23), ]
  agg <- stats::aggregate(che_pc_usd2023 ~ region23 + year, data = d,
                          FUN = function(v) stats::median(v, na.rm = TRUE))
  agg$region23 <- factor(agg$region23,
    levels = unique(agg$region23[order(-agg$che_pc_usd2023)]))
  ggplot2::ggplot(agg, ggplot2::aes(
      x = .data$year, y = .data$region23, fill = .data$che_pc_usd2023)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.3) +
    scale_fill_ghs3_seq(palette = "earth", trans = "log10",
      name = "\u4eba\u5747 CHE",
      labels = scales::dollar_format(accuracy = 1)) +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(8)) +
    theme_ghs3(grid = FALSE) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(size = 8)) +
    labs_news(
      title = "Region23 \u00d7 \u5e74\u4efd \u00b7 \u4eba\u5747 CHE \u4e2d\u4f4d\u70ed\u56fe",
      subtitle = "\u6309\u4e2d\u4f4d\u9650 2022 \u964d\u5e8f\uff1b\u8272\u6df1 = \u4eba\u5747 CHE \u9ad8",
      x = NULL, y = NULL,
      caption = .cap_equity("WB Region23"))
}

plot_equity_continent_cv <- function(master) {
  ensure_pkgs(c("ggplot2"))
  d <- master[is.finite(master$che_pc_usd2023) &
              master$che_pc_usd2023 > 0 &
              !is.na(master$continent), ]
  cvf <- function(v) stats::sd(v, na.rm = TRUE) / mean(v, na.rm = TRUE)
  agg <- stats::aggregate(che_pc_usd2023 ~ continent + year, data = d,
                          FUN = cvf)
  ggplot2::ggplot(agg, ggplot2::aes(x = .data$year, y = .data$che_pc_usd2023,
                                       colour = .data$continent)) +
    ggplot2::geom_line(linewidth = 1.2) +
    scale_colour_brand_continent(name = NULL) +
    theme_ghs3() +
    ggplot2::theme(legend.position = "top") +
    labs_news(
      title = "\u5927\u6d32\u7ec4\u5185 CV \u00b7 \u4eba\u5747 CHE",
      subtitle = "\u53cd\u6620\u7ec4\u5185\u5dee\u5f02\u53d8\u5316\u8d8b\u52bf",
      x = NULL, y = "CV",
      caption = .cap_equity())
}

plot_equity_oop_vs_che_facet <- function(master, year = NULL) {
  ensure_pkgs(c("ggplot2"))
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year & is.finite(master$hf3_che) &
              is.finite(master$che_pc_usd2023) &
              master$che_pc_usd2023 > 0 &
              !is.na(master$income_group), ]
  d$income_group <- factor(d$income_group,
    levels = c("Low income", "Lower middle income",
               "Upper middle income", "High income"))
  ggplot2::ggplot(d, ggplot2::aes(
      x = .data$che_pc_usd2023, y = .data$hf3_che,
      colour = .data$income_group)) +
    ggplot2::geom_point(alpha = 0.85, size = 2.2) +
    ggplot2::geom_smooth(method = "loess", se = FALSE, formula = y ~ x,
      colour = palette_ghs3("ink"), linewidth = 0.9) +
    ggplot2::facet_wrap(~ .data$income_group, ncol = 2) +
    ggplot2::scale_x_log10(labels = scales::dollar_format(accuracy = 1)) +
    ggplot2::scale_y_continuous(
        labels = scales::percent_format(accuracy = 1, scale = 1)) +
    scale_colour_brand_income(guide = "none") +
    theme_ghs3() +
    labs_news(
      title = sprintf("OOP \u00d7 \u4eba\u5747 CHE \u00b7 %d", year),
      subtitle = "\u5185\u90e8\u8f74\u5e7f\uff1aOOP \u968f\u4eba\u5747 CHE \u5347\u9ad8\u800c\u4e0b\u964d",
      x = "\u4eba\u5747 CHE (log)", y = "OOP / CHE",
      caption = .cap_equity())
}

plot_equity_continent_rank <- function(master,
                                        years = c(2000, 2010, 2015, 2020, 2022)) {
  ensure_pkgs(c("ggplot2"))
  d <- master[master$year %in% years &
              is.finite(master$che_pc_usd2023) &
              master$che_pc_usd2023 > 0 &
              !is.na(master$continent), ]
  agg <- stats::aggregate(che_pc_usd2023 ~ continent + year, data = d,
                          FUN = function(v) stats::median(v, na.rm = TRUE))
  agg$rank <- ave(agg$che_pc_usd2023, agg$year,
                  FUN = function(v) rank(-v, ties.method = "min"))
  ggplot2::ggplot(agg, ggplot2::aes(x = .data$year, y = .data$rank,
                                         group = .data$continent,
                                         colour = .data$continent)) +
    ggplot2::geom_line(linewidth = 1.4) +
    ggplot2::geom_point(size = 3.4) +
    ggplot2::scale_y_reverse(breaks = 1:6) +
    ggplot2::scale_x_continuous(breaks = years) +
    scale_colour_brand_continent(name = NULL) +
    theme_ghs3() +
    ggplot2::theme(legend.position = "top") +
    labs_news(
      title = "\u5927\u6d32\u4eba\u5747 CHE \u4e2d\u4f4d\u6392\u540d \u00b7 bumpchart",
      subtitle = "\u7f8e\u6d32 \u00b7 \u6b27\u6d32 \u00b7 \u4e9a\u6d32 \u4f4d\u6b21\u76f8\u5bf9\u7a33\u5b9a\uff1b\u975e\u6d32 \u00b7 \u6d77\u6d0b\u6d32 \u504f\u540e",
      x = NULL, y = "\u4e2d\u4f4d\u6392\u540d\uff081 \u6700\u9ad8\uff09",
      caption = .cap_equity())
}


plot_equity_high_oop_top <- function(master, year = NULL, n = 20) {
  ensure_pkgs(c("ggplot2"))
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year & is.finite(master$hf3_che), ]
  d <- d[order(-d$hf3_che), ]
  picked <- utils::head(d, n)
  picked$country_name <- factor(picked$country_name,
    levels = picked$country_name[order(picked$hf3_che)])
  ggplot2::ggplot(picked, ggplot2::aes(
      x = .data$hf3_che, y = .data$country_name)) +
    ggplot2::geom_col(fill = palette_ghs3("bad"), alpha = 0.88) +
    ggplot2::scale_x_continuous(
        labels = scales::percent_format(accuracy = 1, scale = 1)) +
    theme_ghs3(grid = "x") +
    labs_news(
      title = sprintf("OOP / CHE \u6700\u9ad8 %d \u56fd \u00b7 %d", n, year),
      subtitle = "\u9ad8 OOP \u4ee3\u8868\u8d22\u52a1\u4fdd\u62a4\u8584\u5f31\uff1b\u91cd\u70b9\u533a\u95f4 > 45%",
      x = "OOP / CHE", y = NULL,
      caption = .cap_equity("WHO GHED HF3"))
}

plot_equity_oop_change <- function(master, y1 = NULL, y2 = NULL, n = 12) {
  ensure_pkgs(c("ggplot2"))
  if (is.null(y1)) y1 <- min(master$year, na.rm = TRUE)
  if (is.null(y2)) y2 <- max(master$year, na.rm = TRUE)
  d1 <- master[master$year == y1, c("iso3_code", "country_name", "hf3_che")]
  d2 <- master[master$year == y2, c("iso3_code", "hf3_che")]
  m <- merge(d1, d2, by = "iso3_code", suffixes = c(".y1", ".y2"))
  m$delta <- m$hf3_che.y2 - m$hf3_che.y1
  m <- m[is.finite(m$delta), ]
  m <- m[order(m$delta), ]
  best <- utils::head(m, n); worst <- utils::tail(m, n)
  best$grp <- "\u6700\u5927\u4e0b\u964d\uff08\u4fdd\u62a4\u63d0\u5347\uff09"
  worst$grp <- "\u6700\u5927\u4e0a\u5347\uff08\u9000\u6b65\uff09"
  m2 <- rbind(best, worst)
  m2$country_name <- factor(m2$country_name,
    levels = m2$country_name[order(m2$delta)])
  ggplot2::ggplot(m2, ggplot2::aes(
      x = .data$delta, y = .data$country_name, fill = .data$grp)) +
    ggplot2::geom_col(alpha = 0.88) +
    ggplot2::scale_x_continuous(
        labels = scales::percent_format(accuracy = 1, scale = 1)) +
    ggplot2::scale_fill_manual(
      values = c("\u6700\u5927\u4e0b\u964d\uff08\u4fdd\u62a4\u63d0\u5347\uff09" =
                   palette_ghs3("good"),
                 "\u6700\u5927\u4e0a\u5347\uff08\u9000\u6b65\uff09" =
                   palette_ghs3("bad")),
      name = NULL) +
    theme_ghs3(grid = "x") +
    labs_news(
      title = sprintf("OOP / CHE \u53d8\u5316 \u00b7 %d\u2192%d Top/Bot %d",
                       y1, y2, n),
      subtitle = "\u5de6\u504f = OOP \u4e0b\u964d\uff08\u597d\uff09\uff1b\u53f3\u504f = OOP \u4e0a\u5347",
      x = "\u53d8\u5316 (\u767e\u5206\u70b9)", y = NULL,
      caption = .cap_equity())
}

plot_equity_oop_vs_gghed <- function(master, year = NULL) {
  ensure_pkgs(c("ggplot2"))
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year & is.finite(master$hf3_che) &
              is.finite(master$gghed_che), ]
  fit <- stats::lm(hf3_che ~ gghed_che, data = d)
  rho <- stats::cor(d$hf3_che, d$gghed_che, use = "complete.obs")
  ggplot2::ggplot(d, ggplot2::aes(x = .data$gghed_che, y = .data$hf3_che,
                                      colour = .data$continent)) +
    ggplot2::geom_point(alpha = 0.85, size = 2.4) +
    ggplot2::geom_smooth(method = "lm", se = TRUE,
      formula = y ~ x, linewidth = 1, colour = palette_ghs3("ink"),
      fill = "grey80", alpha = 0.25) +
    ggplot2::scale_x_continuous(
        labels = scales::percent_format(accuracy = 1, scale = 1)) +
    ggplot2::scale_y_continuous(
        labels = scales::percent_format(accuracy = 1, scale = 1)) +
    scale_colour_brand_continent(name = NULL) +
    theme_ghs3() +
    ggplot2::theme(legend.position = "top") +
    labs_news(
      title = sprintf("OOP vs \u653f\u5e9c GGHED \u00b7 %d", year),
      subtitle = sprintf("\u8d1f\u5411\u66ff\u4ee3\uff1bcor = %.2f", rho),
      x = "GGHED / CHE", y = "OOP / CHE",
      caption = .cap_equity("\u653f\u5e9c\u6295\u5165\u4e0a\u5347 \u2192 OOP \u4e0b\u964d"))
}

plot_equity_threshold_share <- function(master) {
  ensure_pkgs(c("ggplot2"))
  d <- master[is.finite(master$hf3_che), ]
  thresh <- c(25, 40, 55)
  rows <- list()
  for (th in thresh) {
    agg <- stats::aggregate(hf3_che ~ year, data = d,
                              FUN = function(v) mean(v > th, na.rm = TRUE))
    agg$th <- sprintf("> %d%%", as.integer(th))
    names(agg)[2] <- "share"
    rows[[as.character(th)]] <- agg
  }
  long <- do.call(rbind, rows)
  long$th <- factor(long$th, levels = sort(unique(long$th)))
  ggplot2::ggplot(long, ggplot2::aes(x = .data$year, y = .data$share,
                                          colour = .data$th)) +
    ggplot2::geom_line(linewidth = 1.3) +
    ggplot2::geom_point(size = 2) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_colour_ghs3(name = NULL) +
    theme_ghs3() +
    ggplot2::theme(legend.position = "top") +
    labs_news(
      title = "OOP \u9ad8\u8d1f\u62c5\u56fd\u5bb6\u5360\u6bd4 \u00b7 \u4e09\u9608\u503c",
      subtitle = "\u4ee3\u8868\u8d22\u52a1\u4fdd\u62a4\u8584\u5f31\u56fd\u7684\u6536\u655b\u8d8b\u52bf",
      x = NULL, y = "\u8de8\u56fd\u5360\u6bd4",
      caption = .cap_equity())
}

plot_equity_oop_velocity <- function(master) {
  ensure_pkgs(c("ggplot2"))
  d <- master[is.finite(master$hf3_che) & master$hf3_che > 0 &
              !is.na(master$income_group), ]
  d <- d[order(d$iso3_code, d$year), ]
  d$lag <- ave(d$hf3_che, d$iso3_code,
               FUN = function(v) c(NA_real_, v[-length(v)]))
  d$gr <- with(d, (hf3_che - lag) / lag)
  d <- d[is.finite(d$gr) & abs(d$gr) < 0.5, ]
  if (!nrow(d)) return(ggplot2::ggplot() + ggplot2::theme_void())
  d$income_group <- factor(d$income_group,
    levels = c("Low income", "Lower middle income",
               "Upper middle income", "High income"))
  ggplot2::ggplot(d, ggplot2::aes(
      x = .data$income_group, y = .data$gr,
      fill = .data$income_group)) +
    ggplot2::geom_violin(alpha = 0.55, scale = "width") +
    ggplot2::geom_boxplot(width = 0.18, outlier.shape = NA,
      colour = "#1a1f28", fill = "white", alpha = 0.85) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2,
      colour = "#1a1f28", alpha = 0.45) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1),
      limits = c(-0.30, 0.30)) +
    scale_fill_brand_income(guide = "none") +
    theme_ghs3() +
    labs_news(
      title = "OOP \u5e74\u5316\u53d8\u5316\u7387 \u00b7 \u6309\u6536\u5165\u7ec4",
      subtitle = "\u8d1f\u503c = \u4fdd\u62a4\u63d0\u5347\uff1b\u9ad8\u6536\u5165\u56fd\u96c6\u4e2d\u5728 0% \u9644\u8fd1",
      x = NULL, y = "\u5e74\u5316\u53d8\u5316",
      caption = .cap_equity("country-year diff(OOP)/lag"))
}


ghs_export_equity <- function(master = NULL, fig_dir = NULL) {
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
    eq_oop_share_global       = plot_equity_oop_share_global(master),
    eq_oop_share_income       = plot_equity_oop_share_income(master),
    eq_oop_share_continent    = plot_equity_oop_share_continent(master),
    eq_oop_box_income         = plot_equity_oop_box_income(master),
    eq_catastrophic_proxy     = plot_equity_catastrophic_proxy(master),
    eq_lorenz_che             = plot_equity_lorenz_che(master),
    eq_gini_trend             = plot_equity_gini_trend(master),
    eq_theil_decomp           = plot_equity_theil_between_within(master),
    eq_concentration_lifeexp  = plot_equity_concentration_lifeexp(master),
    eq_index_dot              = plot_equity_index_dot(master),
    eq_che_pc_iqr             = plot_equity_che_pc_iqr(master),
    eq_top_bottom_ratio       = plot_equity_top_bottom_ratio(master),
    eq_dispersion_income      = plot_equity_dispersion_income(master),
    eq_logvar_trend           = plot_equity_logvar_trend(master),
    eq_che_fan                = plot_equity_che_fan(master),
    eq_continent_oop_box      = plot_equity_continent_oop_box(master),
    eq_region_heat            = plot_equity_region_heat(master),
    eq_continent_cv           = plot_equity_continent_cv(master),
    eq_oop_vs_che_facet       = plot_equity_oop_vs_che_facet(master),
    eq_continent_rank         = plot_equity_continent_rank(master),
    eq_high_oop_top           = plot_equity_high_oop_top(master),
    eq_oop_change             = plot_equity_oop_change(master),
    eq_oop_vs_gghed           = plot_equity_oop_vs_gghed(master),
    eq_threshold_share        = plot_equity_threshold_share(master),
    eq_oop_velocity           = plot_equity_oop_velocity(master)
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
      message("[equity export] ", nm, " failed: ", conditionMessage(e))
      FALSE
    })
  }
  invisible(out)
}
