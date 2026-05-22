
if (!exists("ensure_pkgs", mode = "function")) {
  source(file.path("\u7a0b\u5e8f", "00_utils.R"))
}

.has_pkg <- function(pkg) requireNamespace(pkg, quietly = TRUE)

.cap_outcomes <- function(extra = NULL) {
  base <- "\u6570\u636e \u00b7 WHO GHED 2024 + WDI (life expectancy / U5MR)"
  if (is.null(extra) || !nzchar(extra)) base
  else paste0(base, " \u00b7 ", extra)
}

.decile_bin <- function(x, n = 10) {
  probs <- seq(0, 1, length.out = n + 1)
  brks <- stats::quantile(x, probs, na.rm = TRUE)
  brks <- unique(brks)
  if (length(brks) < 3) return(rep(NA_character_, length(x)))
  cut(x, brks, include.lowest = TRUE,
      labels = paste0("D", seq_len(length(brks) - 1)))
}

.upper_envelope <- function(x, y, n = 25) {
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]; y <- y[ok]
  if (length(x) < 5) return(data.frame(x = numeric(0), y = numeric(0)))
  brks <- stats::quantile(x, probs = seq(0, 1, length.out = n + 1),
                          na.rm = TRUE)
  brks <- unique(brks)
  if (length(brks) < 4) return(data.frame(x = numeric(0), y = numeric(0)))
  out <- data.frame(x = numeric(0), y = numeric(0))
  for (i in seq_len(length(brks) - 1)) {
    sel <- x >= brks[i] & x <= brks[i + 1]
    if (sum(sel) >= 2) out <- rbind(out, data.frame(
      x = stats::median(x[sel]), y = max(y[sel])))
  }
  out <- out[order(out$x), ]
  out$y <- cummax(out$y)
  out
}

.lower_envelope <- function(x, y, n = 25) {
  env <- .upper_envelope(x, -y, n)
  env$y <- -env$y
  env
}

.median_iqr <- function(v) {
  v <- v[is.finite(v)]
  if (!length(v)) return(data.frame(y = NA_real_, ymin = NA_real_,
                                      ymax = NA_real_))
  qs <- stats::quantile(v, c(0.25, 0.5, 0.75), na.rm = TRUE)
  data.frame(y = unname(qs[2]),
             ymin = unname(qs[1]),
             ymax = unname(qs[3]))
}


plot_outcome_lexis_lifeexp <- function(master) {
  ensure_pkgs(c("ggplot2"))
  d <- master[is.finite(master$life_exp) &
              is.finite(master$che_pc_usd2023) &
              master$che_pc_usd2023 > 0, ]
  if (!nrow(d)) return(ggplot2::ggplot() + ggplot2::theme_void())
  d$decile <- ave(d$che_pc_usd2023, d$year,
                  FUN = function(v) as.character(.decile_bin(v, 10)))
  agg <- stats::aggregate(life_exp ~ year + decile, data = d,
                          FUN = function(v) stats::median(v, na.rm = TRUE))
  agg$decile <- factor(agg$decile, levels = paste0("D", 1:10))
  ggplot2::ggplot(agg, ggplot2::aes(
      x = .data$year, y = .data$decile, fill = .data$life_exp)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.3) +
    scale_fill_ghs3_seq(palette = "earth",
                         name = "\u9884\u671f\u5bff\u547d\uff08\u5e74\uff09") +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(10)) +
    theme_ghs3(grid = FALSE) +
    ggplot2::theme(panel.grid = ggplot2::element_blank()) +
    labs_news(
      title = "\u9884\u671f\u5bff\u547d Lexis \u9762 \u00b7 \u5e74\u4efd \u00d7 CHE_pc \u5341\u5206\u4f4d",
      subtitle = "D1 = \u4eba\u5747 CHE \u6700\u4f4e 10%\uff0cD10 = \u6700\u9ad8 10%",
      x = "\u5e74\u4efd", y = "\u4eba\u5747 CHE \u5206\u4f4d",
      caption = .cap_outcomes())
}

plot_outcome_lexis_u5mr <- function(master) {
  ensure_pkgs(c("ggplot2"))
  d <- master[is.finite(master$u5mr) &
              is.finite(master$che_pc_usd2023) &
              master$che_pc_usd2023 > 0 & master$u5mr > 0, ]
  if (!nrow(d)) return(ggplot2::ggplot() + ggplot2::theme_void())
  d$decile <- ave(d$che_pc_usd2023, d$year,
                  FUN = function(v) as.character(.decile_bin(v, 10)))
  agg <- stats::aggregate(u5mr ~ year + decile, data = d,
                          FUN = function(v) stats::median(v, na.rm = TRUE))
  agg$decile <- factor(agg$decile, levels = paste0("D", 1:10))
  ggplot2::ggplot(agg, ggplot2::aes(
      x = .data$year, y = .data$decile, fill = .data$u5mr)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.3) +
    scale_fill_ghs3_seq(palette = "ember", trans = "log10",
                         name = "U5MR /1000\nlog10") +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(10)) +
    theme_ghs3(grid = FALSE) +
    labs_news(
      title = "U5MR Lexis \u9762 \u00b7 \u5e74\u4efd \u00d7 CHE_pc \u5341\u5206\u4f4d",
      subtitle = "log10\uff1bD1 \u8d70\u5411 D10 = \u539a\u91d1\u91d1\u8d2d\u51fa\u5b50\u7ae5\u751f\u5b58",
      x = "\u5e74\u4efd", y = "\u4eba\u5747 CHE \u5206\u4f4d",
      caption = .cap_outcomes())
}

plot_outcome_lexis_dual <- function(master) {
  ensure_pkgs(c("ggplot2"))
  d <- master[is.finite(master$life_exp) &
              is.finite(master$u5mr) & master$u5mr > 0 &
              is.finite(master$che_pc_usd2023) &
              master$che_pc_usd2023 > 0, ]
  if (!nrow(d)) return(ggplot2::ggplot() + ggplot2::theme_void())
  d$decile <- ave(d$che_pc_usd2023, d$year,
                  FUN = function(v) as.character(.decile_bin(v, 10)))
  a <- stats::aggregate(cbind(life_exp, u5mr) ~ year + decile, data = d,
                        FUN = function(v) stats::median(v, na.rm = TRUE))
  long <- rbind(
    data.frame(year = a$year, decile = a$decile, value = a$life_exp,
               var = "\u9884\u671f\u5bff\u547d\uff08\u5e74\uff09"),
    data.frame(year = a$year, decile = a$decile, value = log10(a$u5mr),
               var = "log10 U5MR"))
  long$decile <- factor(long$decile, levels = paste0("D", 1:10))
  ggplot2::ggplot(long, ggplot2::aes(
      x = .data$year, y = .data$decile, fill = .data$value)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.2) +
    ggplot2::facet_wrap(~ .data$var, ncol = 1, scales = "free") +
    scale_fill_ghs3_seq(palette = "ocean", name = NULL) +
    theme_ghs3(grid = FALSE) +
    ggplot2::theme(
      strip.text = ggplot2::element_text(face = "bold", size = 12),
      panel.spacing = ggplot2::unit(14, "points")) +
    labs_news(
      title = "Lexis \u53cc\u9762\u677f\uff1a\u5bff\u547d \u00b7 U5MR",
      subtitle = "\u540c\u4e00 (\u5e74\uff0c\u5341\u5206\u4f4d) \u4e0a\u4e24\u4e2a\u4ea7\u51fa\u7684\u5e73\u884c\u8f68\u8ff9",
      x = "\u5e74\u4efd", y = "\u4eba\u5747 CHE \u5206\u4f4d",
      caption = .cap_outcomes())
}


plot_outcome_lifeexp_trend_global <- function(master) {
  ensure_pkgs(c("ggplot2"))
  d <- master[is.finite(master$life_exp), ]
  med <- stats::aggregate(life_exp ~ year, data = d,
    FUN = function(v) c(med = stats::median(v, na.rm = TRUE),
                         p25 = stats::quantile(v, 0.25, na.rm = TRUE),
                         p75 = stats::quantile(v, 0.75, na.rm = TRUE)))
  vals <- as.data.frame(med$life_exp)
  m <- data.frame(year = med$year, median = vals$med,
                  p25 = vals$`p25.25%`, p75 = vals$`p75.75%`)
  ggplot2::ggplot(m, ggplot2::aes(x = .data$year)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$p25, ymax = .data$p75),
      fill = palette_ghs3("primary"), alpha = 0.32) +
    ggplot2::geom_line(ggplot2::aes(y = .data$median),
      colour = palette_ghs3("primary"), linewidth = 1.4) +
    ggplot2::geom_point(ggplot2::aes(y = .data$median),
      colour = palette_ghs3("primary"), size = 2) +
    theme_ghs3() +
    labs_news(
      title = "\u5168\u7403\u9884\u671f\u5bff\u547d \u00b7 \u4e2d\u4f4d\u4e0e IQR",
      subtitle = "\u9634\u5f71\u4e3a 25-75 \u5206\u4f4d\u533a\u95f4\uff1b\u5b9e\u7ebf\u4e3a\u4e2d\u4f4d",
      x = NULL, y = "\u9884\u671f\u5bff\u547d\uff08\u5e74\uff09",
      caption = .cap_outcomes())
}

plot_outcome_lifeexp_trend_income <- function(master) {
  ensure_pkgs(c("ggplot2"))
  d <- master[is.finite(master$life_exp) &
              !is.na(master$income_group), ]
  d$income_group <- factor(d$income_group,
    levels = c("Low income", "Lower middle income",
               "Upper middle income", "High income"))
  ggplot2::ggplot(d, ggplot2::aes(
      x = .data$year, y = .data$life_exp,
      group = .data$income_group, colour = .data$income_group)) +
    ggplot2::stat_summary(geom = "ribbon", fun.data = .median_iqr,
      ggplot2::aes(fill = .data$income_group, group = .data$income_group),
      colour = NA, alpha = 0.18) +
    ggplot2::stat_summary(geom = "line", fun = stats::median,
      linewidth = 1.2) +
    scale_colour_brand_income(name = NULL) +
    scale_fill_brand_income(guide = "none") +
    theme_ghs3() +
    ggplot2::theme(legend.position = "top") +
    labs_news(
      title = "\u9884\u671f\u5bff\u547d \u00b7 \u6309\u6536\u5165\u7ec4",
      subtitle = "\u4e2d\u4f4d\u00b7\u9634\u5f71 = IQR\uff1b\u9ad8\u6536\u5165\u4e0e\u4f4e\u6536\u5165 \u5dee\u8ddd \u224820 \u5e74",
      x = NULL, y = "\u9884\u671f\u5bff\u547d\uff08\u5e74\uff09",
      caption = .cap_outcomes())
}

plot_outcome_lifeexp_trend_continent <- function(master) {
  ensure_pkgs(c("ggplot2"))
  d <- master[is.finite(master$life_exp) &
              !is.na(master$continent), ]
  ggplot2::ggplot(d, ggplot2::aes(
      x = .data$year, y = .data$life_exp,
      colour = .data$continent)) +
    ggplot2::stat_summary(geom = "line", fun = stats::median,
      linewidth = 1.2) +
    scale_colour_brand_continent(name = NULL) +
    theme_ghs3() +
    ggplot2::theme(legend.position = "top") +
    labs_news(
      title = "\u9884\u671f\u5bff\u547d \u00b7 \u6309\u5927\u6d32",
      subtitle = "\u4e2d\u4f4d\u8f68\u8ff9\uff1b\u975e\u6d32 \u4ecd\u662f\u4ea7\u51fa\u8f83\u4f4e",
      x = NULL, y = "\u9884\u671f\u5bff\u547d\uff08\u5e74\uff09",
      caption = .cap_outcomes())
}

plot_outcome_u5mr_trend_global <- function(master) {
  ensure_pkgs(c("ggplot2"))
  d <- master[is.finite(master$u5mr) & master$u5mr > 0, ]
  ggplot2::ggplot(d, ggplot2::aes(x = .data$year, y = .data$u5mr)) +
    ggplot2::stat_summary(geom = "ribbon", fun.data = .median_iqr,
      fill = palette_ghs3("bad"), alpha = 0.22) +
    ggplot2::stat_summary(geom = "line", fun = stats::median,
      colour = palette_ghs3("bad"), linewidth = 1.4) +
    ggplot2::scale_y_log10() +
    theme_ghs3() +
    labs_news(
      title = "\u5168\u7403 U5MR \u4e2d\u4f4d \u00b7 log \u8f74",
      subtitle = "\u9634\u5f71 = IQR\uff1b\u4e0b\u964d\u8d8b\u52bf\u660e\u663e\u4f46\u4f4e\u6536\u5165\u56fd\u4ecd\u6709\u8ddf\u8fdb\u7a7a\u95f4",
      x = NULL, y = "U5MR /1000",
      caption = .cap_outcomes())
}

plot_outcome_u5mr_trend_income <- function(master) {
  ensure_pkgs(c("ggplot2"))
  d <- master[is.finite(master$u5mr) & master$u5mr > 0 &
              !is.na(master$income_group), ]
  d$income_group <- factor(d$income_group,
    levels = c("Low income", "Lower middle income",
               "Upper middle income", "High income"))
  ggplot2::ggplot(d, ggplot2::aes(
      x = .data$year, y = .data$u5mr,
      colour = .data$income_group, group = .data$income_group)) +
    ggplot2::stat_summary(geom = "line", fun = stats::median,
      linewidth = 1.2) +
    ggplot2::scale_y_log10() +
    scale_colour_brand_income(name = NULL) +
    theme_ghs3() +
    ggplot2::theme(legend.position = "top") +
    labs_news(
      title = "U5MR \u00b7 \u6309\u6536\u5165\u7ec4",
      subtitle = "log \u8f74\u4e0b\u521a\u80fd\u770b\u51fa\u4f4e\u6536\u5165\u56fd\u7684\u4e0b\u964d\u8def\u5f84",
      x = NULL, y = "U5MR /1000",
      caption = .cap_outcomes())
}

plot_outcome_u5mr_trend_continent <- function(master) {
  ensure_pkgs(c("ggplot2"))
  d <- master[is.finite(master$u5mr) & master$u5mr > 0 &
              !is.na(master$continent), ]
  ggplot2::ggplot(d, ggplot2::aes(
      x = .data$year, y = .data$u5mr,
      colour = .data$continent)) +
    ggplot2::stat_summary(geom = "line", fun = stats::median,
      linewidth = 1.2) +
    ggplot2::scale_y_log10() +
    scale_colour_brand_continent(name = NULL) +
    theme_ghs3() +
    ggplot2::theme(legend.position = "top") +
    labs_news(
      title = "U5MR \u00b7 \u6309\u5927\u6d32",
      subtitle = "\u4e2d\u4f4d\u8f68\u8ff9 (\u5bf9\u6570\u5c3a\u5ea6)",
      x = NULL, y = "U5MR /1000",
      caption = .cap_outcomes())
}

plot_outcome_lifeexp_gap_to_frontier <- function(master, year = NULL) {
  ensure_pkgs(c("ggplot2"))
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year & is.finite(master$life_exp), ]
  if (!nrow(d)) return(ggplot2::ggplot() + ggplot2::theme_void())
  top5 <- mean(utils::head(sort(d$life_exp, decreasing = TRUE), 5))
  d$gap <- top5 - d$life_exp
  d <- d[order(-d$gap), ]
  picked <- d[seq_len(min(20, nrow(d))), ]
  picked$country_name <- factor(picked$country_name,
    levels = picked$country_name[order(-picked$gap)])
  ggplot2::ggplot(picked, ggplot2::aes(
      x = .data$gap, y = .data$country_name)) +
    ggplot2::geom_col(fill = palette_ghs3("bad"), alpha = 0.85) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    theme_ghs3(grid = "x") +
    labs_news(
      title = sprintf("\u9884\u671f\u5bff\u547d\u4e0e\u524d\u6cbf\u7684\u5dee\u8ddd \u00b7 %d", year),
      subtitle = sprintf("\u524d\u6cbf = top-5 \u5747\u503c %.1f\u5e74\uff1b\u663e\u793a\u5dee\u8ddd\u6700\u5927 20 \u56fd",
                          top5),
      x = "\u5dee\u8ddd\uff08\u5e74\uff09", y = NULL,
      caption = .cap_outcomes())
}

plot_outcome_u5mr_gap_to_frontier <- function(master, year = NULL) {
  ensure_pkgs(c("ggplot2"))
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year & is.finite(master$u5mr) &
              master$u5mr > 0, ]
  if (!nrow(d)) return(ggplot2::ggplot() + ggplot2::theme_void())
  bot5 <- mean(utils::head(sort(d$u5mr), 5))
  d$gap <- d$u5mr - bot5
  d <- d[order(-d$gap), ]
  picked <- d[seq_len(min(20, nrow(d))), ]
  picked$country_name <- factor(picked$country_name,
    levels = picked$country_name[order(-picked$gap)])
  ggplot2::ggplot(picked, ggplot2::aes(
      x = .data$gap, y = .data$country_name)) +
    ggplot2::geom_col(fill = palette_ghs3("warn"), alpha = 0.85) +
    theme_ghs3(grid = "x") +
    labs_news(
      title = sprintf("U5MR \u4e0e\u524d\u6cbf\u7684\u5dee\u8ddd \u00b7 %d", year),
      subtitle = sprintf("\u524d\u6cbf = bot-5 \u5747\u503c %.2f /1000\uff1b\u9700\u8981\u52aa\u529b\u8ddf\u8fdb 20 \u56fd",
                          bot5),
      x = "U5MR \u5dee\u8ddd /1000", y = NULL,
      caption = .cap_outcomes())
}

plot_outcome_lifeexp_gain <- function(master,
                                        y1 = NULL, y2 = NULL,
                                        n = 12) {
  ensure_pkgs(c("ggplot2"))
  if (is.null(y1)) y1 <- 2000
  if (is.null(y2)) y2 <- max(master$year, na.rm = TRUE)
  a <- master[master$year == y1, c("country_name", "iso3_code", "life_exp")]
  b <- master[master$year == y2, c("iso3_code", "life_exp")]
  names(a)[3] <- "v1"; names(b)[2] <- "v2"
  m <- merge(a, b, by = "iso3_code")
  m$gain <- m$v2 - m$v1
  m <- m[is.finite(m$gain), ]
  top <- utils::head(m[order(-m$gain), ], n)
  bot <- utils::head(m[order(m$gain), ], n)
  top$grp <- "Top \u589e\u957f"
  bot$grp <- "Bottom \u589e\u957f"
  m2 <- rbind(top, bot)
  m2$country_name <- factor(m2$country_name,
    levels = m2$country_name[order(m2$gain)])
  ggplot2::ggplot(m2, ggplot2::aes(
      x = .data$gain, y = .data$country_name, fill = .data$grp)) +
    ggplot2::geom_col(alpha = 0.88) +
    ggplot2::scale_fill_manual(
      values = c("Top \u589e\u957f" = palette_ghs3("good"),
                 "Bottom \u589e\u957f" = palette_ghs3("bad")),
      name = NULL) +
    theme_ghs3(grid = "x") +
    labs_news(
      title = sprintf("\u9884\u671f\u5bff\u547d \u00b7 %d\u2192%d \u589e\u957f\u6392\u540d (Top/Bot %d)",
                       y1, y2, n),
      subtitle = "\u6b63\u5411 = \u671f\u95f4\u589e\u957f\uff0c\u8d1f\u5411 = \u4e0b\u964d",
      x = "\u5e74\u589e\u957f", y = NULL,
      caption = .cap_outcomes())
}

plot_outcome_u5mr_reduction <- function(master,
                                          y1 = NULL, y2 = NULL,
                                          n = 12) {
  ensure_pkgs(c("ggplot2"))
  if (is.null(y1)) y1 <- 2000
  if (is.null(y2)) y2 <- max(master$year, na.rm = TRUE)
  a <- master[master$year == y1, c("country_name", "iso3_code", "u5mr")]
  b <- master[master$year == y2, c("iso3_code", "u5mr")]
  names(a)[3] <- "v1"; names(b)[2] <- "v2"
  m <- merge(a, b, by = "iso3_code")
  m$delta <- m$v2 - m$v1
  m <- m[is.finite(m$delta), ]
  best <- utils::head(m[order(m$delta), ], n)
  worst <- utils::head(m[order(-m$delta), ], n)
  best$grp <- "\u6700\u5927\u524a\u51cf"
  worst$grp <- "\u6700\u5c0f\u524a\u51cf\u6216\u4e0a\u5347"
  m2 <- rbind(best, worst)
  m2$country_name <- factor(m2$country_name,
    levels = m2$country_name[order(m2$delta)])
  ggplot2::ggplot(m2, ggplot2::aes(
      x = .data$delta, y = .data$country_name, fill = .data$grp)) +
    ggplot2::geom_col(alpha = 0.88) +
    ggplot2::scale_fill_manual(
      values = c("\u6700\u5927\u524a\u51cf" = palette_ghs3("good"),
                 "\u6700\u5c0f\u524a\u51cf\u6216\u4e0a\u5347" = palette_ghs3("bad")),
      name = NULL) +
    theme_ghs3(grid = "x") +
    labs_news(
      title = sprintf("U5MR \u00b7 %d\u2192%d \u53d8\u5316\u6392\u540d (Top/Bot %d)",
                       y1, y2, n),
      subtitle = "\u8d1f\u5411 = U5MR \u4e0b\u964d\uff08\u53cd\u8f6c\u53d8\u597d\uff09",
      x = "\u53d8\u5316 (/1000)", y = NULL,
      caption = .cap_outcomes())
}


plot_outcome_frontier_lifeexp <- function(master, year = NULL) {
  ensure_pkgs(c("ggplot2"))
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year & is.finite(master$life_exp) &
              is.finite(master$che_pc_usd2023) &
              master$che_pc_usd2023 > 0, ]
  d$logche <- log10(d$che_pc_usd2023)
  env <- .upper_envelope(d$logche, d$life_exp, n = 25)
  ggplot2::ggplot(d, ggplot2::aes(
      x = .data$logche, y = .data$life_exp)) +
    ggplot2::geom_point(ggplot2::aes(colour = .data$continent),
      alpha = 0.85, size = 2.4) +
    ggplot2::geom_line(data = env, ggplot2::aes(x = .data$x, y = .data$y),
      colour = palette_ghs3("good"), linewidth = 1.4) +
    ggplot2::geom_smooth(method = "loess", se = FALSE,
      colour = palette_ghs3("primary"), linewidth = 1,
      formula = y ~ x) +
    scale_colour_brand_continent(name = NULL) +
    theme_ghs3() +
    ggplot2::theme(legend.position = "top") +
    labs_news(
      title = sprintf("\u751f\u4ea7\u524d\u6cbf \u00b7 \u4eba\u5747 CHE \u2192 \u9884\u671f\u5bff\u547d \u00b7 %d", year),
      subtitle = "\u4e0a\u5305\u7edc\u7eff = \u4e0e\u4f60\u4eba\u5747 CHE \u540c\u6863\u53ef\u8fbe\u7684\u6700\u9ad8\u5bff\u547d",
      x = "log10 \u4eba\u5747 CHE", y = "\u9884\u671f\u5bff\u547d\uff08\u5e74\uff09",
      caption = .cap_outcomes())
}

plot_outcome_frontier_u5mr <- function(master, year = NULL) {
  ensure_pkgs(c("ggplot2"))
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year & is.finite(master$u5mr) &
              master$u5mr > 0 &
              is.finite(master$che_pc_usd2023) &
              master$che_pc_usd2023 > 0, ]
  d$logche <- log10(d$che_pc_usd2023)
  d$logu5 <- log10(d$u5mr)
  env <- .lower_envelope(d$logche, d$logu5, n = 25)
  ggplot2::ggplot(d, ggplot2::aes(
      x = .data$logche, y = .data$logu5)) +
    ggplot2::geom_point(ggplot2::aes(colour = .data$continent),
      alpha = 0.85, size = 2.4) +
    ggplot2::geom_line(data = env, ggplot2::aes(x = .data$x, y = .data$y),
      colour = palette_ghs3("good"), linewidth = 1.4) +
    ggplot2::geom_smooth(method = "loess", se = FALSE,
      colour = palette_ghs3("primary"), linewidth = 1,
      formula = y ~ x) +
    scale_colour_brand_continent(name = NULL) +
    theme_ghs3() +
    ggplot2::theme(legend.position = "top") +
    labs_news(
      title = sprintf("\u751f\u4ea7\u524d\u6cbf \u00b7 \u4eba\u5747 CHE \u2192 U5MR (log) \u00b7 %d",
                       year),
      subtitle = "\u4e0b\u5305\u7edc\u7eff = \u4e0e\u4f60\u4eba\u5747 CHE \u540c\u6863\u53ef\u8fbe\u7684\u6700\u4f4e U5MR",
      x = "log10 \u4eba\u5747 CHE", y = "log10 U5MR",
      caption = .cap_outcomes())
}

plot_outcome_frontier_composite <- function(master, year = NULL) {
  ensure_pkgs(c("ggplot2"))
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year & is.finite(master$life_exp) &
              is.finite(master$u5mr) & master$u5mr > 0 &
              is.finite(master$che_pc_usd2023) &
              master$che_pc_usd2023 > 0, ]
  z <- function(x) (x - mean(x, na.rm = TRUE)) / stats::sd(x, na.rm = TRUE)
  d$logche <- log10(d$che_pc_usd2023)
  d$out <- z(d$life_exp) - z(log10(d$u5mr))
  env <- .upper_envelope(d$logche, d$out, n = 25)
  ggplot2::ggplot(d, ggplot2::aes(
      x = .data$logche, y = .data$out)) +
    ggplot2::geom_point(ggplot2::aes(colour = .data$continent),
      alpha = 0.85, size = 2.4) +
    ggplot2::geom_line(data = env, ggplot2::aes(x = .data$x, y = .data$y),
      colour = palette_ghs3("good"), linewidth = 1.4) +
    scale_colour_brand_continent(name = NULL) +
    theme_ghs3() +
    ggplot2::theme(legend.position = "top") +
    labs_news(
      title = sprintf("\u590d\u5408\u4ea7\u51fa\u6307\u6570 (z[\u5bff\u547d] - z[log U5MR]) \u00b7 %d", year),
      subtitle = "\u4e0a\u5305\u7edc\u7eff = \u540c\u6863\u53ef\u8fbe\u7684\u6700\u4f73\u4ea7\u51fa\u7efc\u5408\u5f97\u5206",
      x = "log10 \u4eba\u5747 CHE", y = "\u590d\u5408\u4ea7\u51fa Z",
      caption = .cap_outcomes())
}

plot_outcome_efficiency_score <- function(master, year = NULL, n = 14) {
  ensure_pkgs(c("ggplot2"))
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year & is.finite(master$life_exp) &
              is.finite(master$che_pc_usd2023) &
              master$che_pc_usd2023 > 0, ]
  d$logche <- log10(d$che_pc_usd2023)
  fit <- stats::lm(life_exp ~ logche, data = d)
  d$pred <- stats::predict(fit, d)
  d$res <- d$life_exp - d$pred
  d <- d[order(-d$res), ]
  top <- utils::head(d, n)
  bot <- utils::tail(d, n)
  top$grp <- "\u9ad8\u6548 (\u5b9e\u9645 > \u9884\u671f)"
  bot$grp <- "\u4f4e\u6548 (\u5b9e\u9645 < \u9884\u671f)"
  m <- rbind(top, bot)
  m$country_name <- factor(m$country_name,
    levels = m$country_name[order(m$res)])
  ggplot2::ggplot(m, ggplot2::aes(
      x = .data$res, y = .data$country_name, fill = .data$grp)) +
    ggplot2::geom_col(alpha = 0.88) +
    ggplot2::scale_fill_manual(
      values = c("\u9ad8\u6548 (\u5b9e\u9645 > \u9884\u671f)" = palette_ghs3("good"),
                 "\u4f4e\u6548 (\u5b9e\u9645 < \u9884\u671f)" = palette_ghs3("bad")),
      name = NULL) +
    theme_ghs3(grid = "x") +
    labs_news(
      title = sprintf("\u6548\u7387\u5f97\u5206 \u00b7 %d", year),
      subtitle = "\u6b63 = \u540c\u6863\u4eba\u5747 CHE \u4e0b\u5bff\u547d\u9886\u5148\uff1b\u8d1f = \u843d\u540e",
      x = "\u5bff\u547d\u6b8b\u5dee\uff08\u5e74\uff09", y = NULL,
      caption = .cap_outcomes("OLS residual"))
}

plot_outcome_frontier_panel_income <- function(master, year = NULL) {
  ensure_pkgs(c("ggplot2"))
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year & is.finite(master$life_exp) &
              is.finite(master$che_pc_usd2023) &
              master$che_pc_usd2023 > 0 &
              !is.na(master$income_group), ]
  d$income_group <- factor(d$income_group,
    levels = c("Low income", "Lower middle income",
               "Upper middle income", "High income"))
  d$logche <- log10(d$che_pc_usd2023)
  ggplot2::ggplot(d, ggplot2::aes(
      x = .data$logche, y = .data$life_exp)) +
    ggplot2::geom_point(ggplot2::aes(colour = .data$income_group),
      alpha = 0.85, size = 2.2) +
    ggplot2::geom_smooth(method = "lm", se = TRUE, linewidth = 0.9,
      colour = palette_ghs3("ink"), fill = "grey80", alpha = 0.25,
      formula = y ~ x) +
    ggplot2::facet_wrap(~ .data$income_group, ncol = 2) +
    scale_colour_brand_income(guide = "none") +
    theme_ghs3() +
    ggplot2::theme(
      strip.text = ggplot2::element_text(face = "bold"),
      panel.spacing = ggplot2::unit(16, "points")) +
    labs_news(
      title = sprintf("\u751f\u4ea7\u524d\u6cbf \u00b7 \u6309\u6536\u5165\u7ec4\u5206\u9762 \u00b7 %d",
                       year),
      subtitle = "OLS \u62df\u5408\u00b7\u8fb9\u9645\u6536\u76ca\u968f\u6536\u5165\u7ec4\u9012\u51cf",
      x = "log10 \u4eba\u5747 CHE", y = "\u9884\u671f\u5bff\u547d\uff08\u5e74\uff09",
      caption = .cap_outcomes())
}


plot_outcome_elasticity_lifeexp <- function(master, year = NULL) {
  ensure_pkgs(c("ggplot2"))
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year & is.finite(master$life_exp) &
              master$life_exp > 0 &
              is.finite(master$che_pc_usd2023) &
              master$che_pc_usd2023 > 0, ]
  d$loglife <- log(d$life_exp)
  d$logche <- log(d$che_pc_usd2023)
  fit <- stats::lm(loglife ~ logche, data = d)
  cof <- stats::coef(fit)
  ci <- stats::confint(fit)
  beta <- cof["logche"]
  ci_lo <- ci["logche", 1]
  ci_hi <- ci["logche", 2]
  ggplot2::ggplot(d, ggplot2::aes(x = .data$logche, y = .data$loglife)) +
    ggplot2::geom_point(ggplot2::aes(colour = .data$continent),
      alpha = 0.85, size = 2.3) +
    ggplot2::geom_smooth(method = "lm", se = TRUE,
      colour = palette_ghs3("primary"), linewidth = 1.2,
      formula = y ~ x) +
    scale_colour_brand_continent(name = NULL) +
    theme_ghs3() +
    ggplot2::theme(legend.position = "top") +
    labs_news(
      title = sprintf("\u5bff\u547d\u5f39\u6027 (log-log) \u00b7 %d", year),
      subtitle = sprintf("\u659c\u7387 \u03b2 = %.3f\uff0c95%%CI [%.3f, %.3f]\uff1b\u5373 1%% CHE \u589e \u2192 \u5bff\u547d %.3f%%",
                          beta, ci_lo, ci_hi, beta),
      x = "log \u4eba\u5747 CHE", y = "log \u9884\u671f\u5bff\u547d",
      caption = .cap_outcomes("OLS log-log"))
}

plot_outcome_elasticity_u5mr <- function(master, year = NULL) {
  ensure_pkgs(c("ggplot2"))
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year & is.finite(master$u5mr) &
              master$u5mr > 0 &
              is.finite(master$che_pc_usd2023) &
              master$che_pc_usd2023 > 0, ]
  d$logu5 <- log(d$u5mr)
  d$logche <- log(d$che_pc_usd2023)
  fit <- stats::lm(logu5 ~ logche, data = d)
  beta <- stats::coef(fit)["logche"]
  ci <- stats::confint(fit)["logche", ]
  ggplot2::ggplot(d, ggplot2::aes(x = .data$logche, y = .data$logu5)) +
    ggplot2::geom_point(ggplot2::aes(colour = .data$continent),
      alpha = 0.85, size = 2.3) +
    ggplot2::geom_smooth(method = "lm", se = TRUE,
      colour = palette_ghs3("bad"), linewidth = 1.2,
      formula = y ~ x) +
    scale_colour_brand_continent(name = NULL) +
    theme_ghs3() +
    ggplot2::theme(legend.position = "top") +
    labs_news(
      title = sprintf("U5MR \u5f39\u6027 (log-log) \u00b7 %d", year),
      subtitle = sprintf("\u659c\u7387 \u03b2 = %.3f\uff0c95%%CI [%.3f, %.3f]\uff1b1%% CHE \u589e \u2192 U5MR %.2f%%",
                          beta, ci[1], ci[2], beta),
      x = "log \u4eba\u5747 CHE", y = "log U5MR",
      caption = .cap_outcomes("OLS log-log"))
}

plot_outcome_residual_lifeexp <- function(master, year = NULL, n = 12) {
  ensure_pkgs(c("ggplot2"))
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year & is.finite(master$life_exp) &
              is.finite(master$che_pc_usd2023) &
              master$che_pc_usd2023 > 0, ]
  d$loglife <- log(d$life_exp)
  d$logche <- log(d$che_pc_usd2023)
  fit <- stats::lm(loglife ~ logche, data = d)
  d$res <- stats::residuals(fit) * 100
  d <- d[order(-d$res), ]
  top <- utils::head(d, n); bot <- utils::tail(d, n)
  top$grp <- "\u8d85\u9884\u671f"; bot$grp <- "\u4f4e\u9884\u671f"
  m <- rbind(top, bot)
  m$country_name <- factor(m$country_name,
    levels = m$country_name[order(m$res)])
  ggplot2::ggplot(m, ggplot2::aes(
      x = .data$res, y = .data$country_name, fill = .data$grp)) +
    ggplot2::geom_col(alpha = 0.88) +
    ggplot2::scale_fill_manual(
      values = c("\u8d85\u9884\u671f" = palette_ghs3("good"),
                 "\u4f4e\u9884\u671f" = palette_ghs3("bad")),
      name = NULL) +
    theme_ghs3(grid = "x") +
    labs_news(
      title = sprintf("\u5bff\u547d\u5bf9\u6570\u6b8b\u5dee \u00b7 %d", year),
      subtitle = "\u6b63\u503c% = \u540c\u6863 CHE \u4e0b\u5bff\u547d\u9886\u5148\uff1b\u8d1f = \u843d\u540e",
      x = "log-life \u6b8b\u5dee (%)", y = NULL,
      caption = .cap_outcomes("OLS log-log residual"))
}

plot_outcome_residual_u5mr <- function(master, year = NULL, n = 12) {
  ensure_pkgs(c("ggplot2"))
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year & is.finite(master$u5mr) &
              master$u5mr > 0 &
              is.finite(master$che_pc_usd2023) &
              master$che_pc_usd2023 > 0, ]
  d$logu5 <- log(d$u5mr)
  d$logche <- log(d$che_pc_usd2023)
  fit <- stats::lm(logu5 ~ logche, data = d)
  d$res <- stats::residuals(fit) * 100
  d <- d[order(d$res), ]
  top <- utils::head(d, n)
  bot <- utils::tail(d, n)
  top$grp <- "\u6bd4\u9884\u671f\u4f4e (\u597d)"
  bot$grp <- "\u6bd4\u9884\u671f\u9ad8 (\u574f)"
  m <- rbind(top, bot)
  m$country_name <- factor(m$country_name,
    levels = m$country_name[order(m$res)])
  ggplot2::ggplot(m, ggplot2::aes(
      x = .data$res, y = .data$country_name, fill = .data$grp)) +
    ggplot2::geom_col(alpha = 0.88) +
    ggplot2::scale_fill_manual(
      values = c("\u6bd4\u9884\u671f\u4f4e (\u597d)" = palette_ghs3("good"),
                 "\u6bd4\u9884\u671f\u9ad8 (\u574f)" = palette_ghs3("bad")),
      name = NULL) +
    theme_ghs3(grid = "x") +
    labs_news(
      title = sprintf("U5MR \u5bf9\u6570\u6b8b\u5dee \u00b7 %d", year),
      subtitle = "\u540c\u6863 CHE \u4e0b\u00b7\u5b9e\u9645 \u4e0e \u9884\u671f \u504f\u79bb",
      x = "log U5MR \u6b8b\u5dee (%)", y = NULL,
      caption = .cap_outcomes("OLS log-log residual"))
}

plot_outcome_residual_map_lifeexp <- function(master, world_sf,
                                                year = NULL) {
  ensure_pkgs(c("ggplot2", "sf"))
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year & is.finite(master$life_exp) &
              is.finite(master$che_pc_usd2023) &
              master$che_pc_usd2023 > 0, ]
  d$loglife <- log(d$life_exp)
  d$logche <- log(d$che_pc_usd2023)
  fit <- stats::lm(loglife ~ logche, data = d)
  d$res <- stats::residuals(fit) * 100
  sfd <- merge(world_sf, d[, c("iso3_code", "res")],
                by = "iso3_code", all.x = TRUE)
  sfd <- tryCatch(sf::st_transform(sfd, "+proj=robin"),
                  error = function(e) sfd)
  ggplot2::ggplot(sfd) +
    ggplot2::geom_sf(ggplot2::aes(fill = .data$res),
                      colour = "white", linewidth = 0.16) +
    scale_fill_ghs3_div(midpoint = 0,
      na.value = "#3a3f48",
      name = "log-life \u6b8b\u5dee (%)") +
    theme_ghs3(grid = FALSE) +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      legend.position = "right") +
    labs_news(
      title = sprintf("\u5bff\u547d\u6b8b\u5dee\u5730\u7406\u5206\u5e03 \u00b7 %d", year),
      subtitle = "\u84dd\u7eff = \u540c\u6863 CHE \u4e0b\u8d85\u51fa\u9884\u671f\uff1b\u7c89\u7ea2 = \u843d\u540e\u9884\u671f",
      caption = .cap_outcomes("OLS log-log residual"))
}


plot_outcome_decoupling_track <- function(master,
                                            isos = c("USA", "CHN", "IND",
                                                       "BRA", "JPN", "DEU",
                                                       "NGA", "ZAF")) {
  ensure_pkgs(c("ggplot2"))
  d <- master[is.finite(master$life_exp) &
              is.finite(master$che_pc_usd2023) &
              master$che_pc_usd2023 > 0 &
              master$iso3_code %in% isos, ]
  if (!nrow(d)) return(ggplot2::ggplot() + ggplot2::theme_void())
  d <- d[order(d$iso3_code, d$year), ]
  endpts <- do.call(rbind, lapply(split(d, d$iso3_code), function(g) {
    g[c(1, nrow(g)), ]
  }))
  ggplot2::ggplot(d, ggplot2::aes(
      x = .data$che_pc_usd2023, y = .data$life_exp,
      group = .data$iso3_code, colour = .data$iso3_code)) +
    ggplot2::geom_path(linewidth = 1, alpha = 0.7) +
    ggplot2::geom_point(data = endpts, size = 3) +
    ggplot2::scale_x_log10(
      labels = function(v) paste0("$", format(v, big.mark = ","))) +
    ggplot2::scale_colour_manual(values = ghs3_palette_discrete,
                                  name = NULL) +
    theme_ghs3() +
    ggplot2::theme(legend.position = "top") +
    labs_news(
      title = "\u4eba\u5747 CHE \u00d7 \u5bff\u547d \u8f68\u8ff9 (\u6309 ISO3)",
      subtitle = "\u70b9 = \u5e74\u4efd\u8d77\u70b9/\u7ec8\u70b9\uff0c\u8f68\u8ff9 = \u9006\u65f6\u95f4 / \u9032\u65f6\u95f4",
      x = "\u4eba\u5747 CHE (log)", y = "\u9884\u671f\u5bff\u547d (\u5e74)",
      caption = .cap_outcomes())
}

plot_outcome_u5mr_velocity <- function(master) {
  ensure_pkgs(c("ggplot2"))
  d <- master[is.finite(master$u5mr) & master$u5mr > 0 &
              !is.na(master$income_group), ]
  d$income_group <- factor(d$income_group,
    levels = c("Low income", "Lower middle income",
               "Upper middle income", "High income"))
  d <- d[order(d$iso3_code, d$year), ]
  rates <- do.call(rbind, lapply(split(d, d$iso3_code), function(g) {
    if (nrow(g) < 2) return(NULL)
    g$lag <- c(NA, g$u5mr[-nrow(g)])
    g$rate <- (g$u5mr / g$lag) - 1
    g
  }))
  rates <- rates[is.finite(rates$rate), ]
  ggplot2::ggplot(rates, ggplot2::aes(
      x = .data$income_group, y = .data$rate,
      fill = .data$income_group)) +
    ggplot2::geom_violin(alpha = 0.6, scale = "width") +
    ggplot2::geom_boxplot(width = 0.18, outlier.shape = NA,
      colour = "#1a1f28", fill = "white", alpha = 0.85) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2,
      colour = "#1a1f28", alpha = 0.45) +
    ggplot2::scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      limits = c(-0.15, 0.15)) +
    scale_fill_brand_income(guide = "none") +
    theme_ghs3() +
    labs_news(
      title = "U5MR \u5e74\u5316\u53d8\u5316\u7387 \u00b7 \u6309\u6536\u5165\u7ec4",
      subtitle = "\u8d1f\u503c = \u4e0b\u964d\uff08\u597d\uff09\uff1b\u4f4e\u6536\u5165\u56fd\u5747\u503c\u4e2d\u4f4d\u7ea6 -3% / \u5e74",
      x = NULL, y = "U5MR \u5e74\u5316\u53d8\u5316",
      caption = .cap_outcomes("WDI U5MR, country-level annualised"))
}


ghs_export_outcomes <- function(master = NULL, world_sf = NULL,
                                  fig_dir = NULL) {
  if (is.null(master)) {
    cache <- file.path(proj_root(), "\u6d3e\u751f\u6570\u636e",
                        "\u5904\u7406\u7ed3\u679c", "master_enriched.rds")
    if (!file.exists(cache)) stop("Missing master_enriched.rds")
    master <- readRDS(cache)
  }
  if (is.null(world_sf)) {
    cache <- file.path(proj_root(), "\u6d3e\u751f\u6570\u636e",
                        "\u5904\u7406\u7ed3\u679c", "world_sf_medium.rds")
    if (file.exists(cache)) world_sf <- readRDS(cache)
  }
  if (is.null(fig_dir)) {
    fig_dir <- file.path(proj_root(), "\u5206\u6790\u8f93\u51fa",
                          "\u56fe\u8868")
  }
  dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

  plots <- list(
    outcome_lexis_lifeexp           = plot_outcome_lexis_lifeexp(master),
    outcome_lexis_u5mr              = plot_outcome_lexis_u5mr(master),
    outcome_lexis_dual              = plot_outcome_lexis_dual(master),
    outcome_lifeexp_trend_global    = plot_outcome_lifeexp_trend_global(master),
    outcome_lifeexp_trend_income    = plot_outcome_lifeexp_trend_income(master),
    outcome_lifeexp_trend_continent = plot_outcome_lifeexp_trend_continent(master),
    outcome_u5mr_trend_global       = plot_outcome_u5mr_trend_global(master),
    outcome_u5mr_trend_income       = plot_outcome_u5mr_trend_income(master),
    outcome_u5mr_trend_continent    = plot_outcome_u5mr_trend_continent(master),
    outcome_lifeexp_gap_frontier    = plot_outcome_lifeexp_gap_to_frontier(master),
    outcome_u5mr_gap_frontier       = plot_outcome_u5mr_gap_to_frontier(master),
    outcome_lifeexp_gain            = plot_outcome_lifeexp_gain(master),
    outcome_u5mr_reduction          = plot_outcome_u5mr_reduction(master),
    outcome_frontier_lifeexp        = plot_outcome_frontier_lifeexp(master),
    outcome_frontier_u5mr           = plot_outcome_frontier_u5mr(master),
    outcome_frontier_composite      = plot_outcome_frontier_composite(master),
    outcome_efficiency_score        = plot_outcome_efficiency_score(master),
    outcome_frontier_panel_income   = plot_outcome_frontier_panel_income(master),
    outcome_elasticity_lifeexp      = plot_outcome_elasticity_lifeexp(master),
    outcome_elasticity_u5mr         = plot_outcome_elasticity_u5mr(master),
    outcome_residual_lifeexp        = plot_outcome_residual_lifeexp(master),
    outcome_residual_u5mr           = plot_outcome_residual_u5mr(master),
    outcome_decoupling_track        = plot_outcome_decoupling_track(master),
    outcome_u5mr_velocity           = plot_outcome_u5mr_velocity(master)
  )
  if (!is.null(world_sf)) {
    plots$outcome_residual_map_lifeexp <-
      plot_outcome_residual_map_lifeexp(master, world_sf)
  }

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
      message("[outcome export] ", nm, " failed: ", conditionMessage(e))
      FALSE
    })
  }
  invisible(out)
}
