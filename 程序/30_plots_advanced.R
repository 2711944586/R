
if (!exists("ensure_pkgs", mode = "function")) {
  source(file.path("\u7a0b\u5e8f", "00_utils.R"))
}


.has_pkg <- function(pkg) requireNamespace(pkg, quietly = TRUE)

.cap_news <- function(extra = NULL) {
  base <- "\u6570\u636e \u00b7 WHO GHED 2024 + WDI 2024-10"
  if (is.null(extra) || !nzchar(extra)) base
  else paste0(base, " \u00b7 ", extra)
}


plot_adv_ridge_oops_by_income <- function(master, year = NULL) {
  ensure_pkgs(c("ggplot2"))
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year &
              is.finite(master$hf3_che) &
              !is.na(master$income_group), ]
  if (!nrow(d)) return(ggplot2::ggplot() + ggplot2::theme_void())
  d$income_group <- factor(
    d$income_group,
    levels = c("Low income", "Lower middle income",
               "Upper middle income", "High income"))
  if (.has_pkg("ggridges")) {
    p <- ggplot2::ggplot(d, ggplot2::aes(
        x = .data$hf3_che, y = .data$income_group,
        fill = .data$income_group)) +
      ggridges::geom_density_ridges(scale = 1.6, alpha = 0.86,
                                     colour = "white", rel_min_height = 0.005) +
      scale_fill_brand_income(guide = "none")
  } else {
    p <- ggplot2::ggplot(d, ggplot2::aes(
        x = .data$hf3_che, y = .data$income_group,
        fill = .data$income_group)) +
      ggplot2::geom_violin(alpha = 0.86, colour = "white") +
      scale_fill_brand_income(guide = "none")
  }
  p +
    ggplot2::scale_x_continuous(
      labels = function(v) paste0(v, "%"), limits = c(0, 90)) +
    theme_ghs3(grid = "x") +
    labs_news(
      title = sprintf("OOPS \u5360 CHE \u5206\u5e03 \u00b7 %d", year),
      subtitle = "\u9ad8\u6536\u5165\u56fd\u96c6\u4e2d\u5728\u4f4e OOPS \u533a\u95f4\uff0c\u4f4e\u6536\u5165\u56fd\u957f\u5c3e\u660e\u663e",
      x = "OOPS \u5360 CHE\uff08%\uff09", y = NULL,
      caption = .cap_news())
}

plot_adv_ridge_che_pc_evolution <- function(master,
                                            years = c(2000, 2008, 2014, 2020,
                                                       max(master$year))) {
  ensure_pkgs(c("ggplot2"))
  d <- master[master$year %in% years &
              is.finite(master$che_pc_usd2023) &
              master$che_pc_usd2023 > 0, ]
  if (!nrow(d)) return(ggplot2::ggplot() + ggplot2::theme_void())
  d$year_f <- factor(d$year, levels = sort(years))
  if (.has_pkg("ggridges")) {
    p <- ggplot2::ggplot(d, ggplot2::aes(
        x = log10(.data$che_pc_usd2023), y = .data$year_f,
        fill = ggplot2::after_stat(.data$x))) +
      ggridges::geom_density_ridges_gradient(
        scale = 1.7, colour = "white", rel_min_height = 0.005) +
      scale_fill_ghs3_seq(palette = "ember", guide = "none")
  } else {
    p <- ggplot2::ggplot(d, ggplot2::aes(
        x = log10(.data$che_pc_usd2023), y = .data$year_f,
        fill = .data$year_f)) +
      ggplot2::geom_violin(alpha = 0.86, colour = "white") +
      scale_fill_ghs3(guide = "none")
  }
  p + ggplot2::scale_x_continuous(
        breaks = c(1, 2, 3, 4),
        labels = c("$10", "$100", "$1k", "$10k")) +
    theme_ghs3(grid = "x") +
    labs_news(
      title = "\u4eba\u5747 CHE \u5206\u5e03 \u00b7 \u8de8\u5e74\u5e8f\u5217",
      subtitle = "\u5bf9\u6570\u8f74\u5c55\u793a\u5168\u7403\u5206\u5e03\u53f3\u79fb\u4e0e\u5c3e\u90e8\u538b\u7f29",
      x = "\u4eba\u5747 CHE\uff082023 USD\uff0c log10 \u8f74\uff09", y = NULL,
      caption = .cap_news("USD2023 constant"))
}

plot_adv_ridge_gghed_by_continent <- function(master, year = NULL) {
  ensure_pkgs(c("ggplot2"))
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year &
              is.finite(master$gghed_che) &
              !is.na(master$continent), ]
  if (!nrow(d)) return(ggplot2::ggplot() + ggplot2::theme_void())
  med <- stats::aggregate(gghed_che ~ continent, data = d, median)
  d$continent <- factor(d$continent,
    levels = med$continent[order(med$gghed_che)])
  if (.has_pkg("ggridges")) {
    p <- ggplot2::ggplot(d, ggplot2::aes(
        x = .data$gghed_che, y = .data$continent,
        fill = .data$continent)) +
      ggridges::geom_density_ridges(
        scale = 1.5, alpha = 0.86, colour = "white",
        quantile_lines = TRUE, quantiles = 0.5)
  } else {
    p <- ggplot2::ggplot(d, ggplot2::aes(
        x = .data$gghed_che, y = .data$continent,
        fill = .data$continent)) +
      ggplot2::geom_boxplot(alpha = 0.86)
  }
  p + scale_fill_brand_continent(guide = "none") +
    ggplot2::scale_x_continuous(labels = function(v) paste0(v, "%")) +
    theme_ghs3(grid = "x") +
    labs_news(
      title = sprintf("GGHED \u5360 CHE \u00b7 \u5404\u6d32\u5206\u5e03 %d", year),
      subtitle = "\u4e2d\u4f4d\u6570\u7ebf\u5448\u73b0\u5404\u6d32\u4e2d\u5fc3\u4f4d\u7f6e",
      x = "GGHED \u5360 CHE\uff08%\uff09", y = NULL,
      caption = .cap_news())
}


plot_adv_beeswarm_che_pc <- function(master, year = NULL) {
  ensure_pkgs(c("ggplot2"))
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year &
              is.finite(master$che_pc_usd2023) &
              master$che_pc_usd2023 > 0 &
              !is.na(master$continent), ]
  if (!nrow(d)) return(ggplot2::ggplot() + ggplot2::theme_void())
  med <- stats::aggregate(che_pc_usd2023 ~ continent, data = d, median)
  d$continent <- factor(d$continent,
    levels = med$continent[order(med$che_pc_usd2023)])
  if (.has_pkg("ggbeeswarm")) {
    p <- ggplot2::ggplot(d, ggplot2::aes(
        x = .data$continent, y = .data$che_pc_usd2023,
        colour = .data$continent)) +
      ggbeeswarm::geom_quasirandom(width = 0.32, alpha = 0.85, size = 2.4)
  } else {
    p <- ggplot2::ggplot(d, ggplot2::aes(
        x = .data$continent, y = .data$che_pc_usd2023,
        colour = .data$continent)) +
      ggplot2::geom_jitter(width = 0.30, alpha = 0.85, size = 2.4)
  }
  p + scale_colour_brand_continent(guide = "none") +
    ggplot2::scale_y_log10(
      labels = function(v) paste0("$", format(round(v), big.mark = ","))) +
    theme_ghs3() +
    labs_news(
      title = sprintf("\u4eba\u5747 CHE \u00b7 \u56fd\u5bb6\u6563\u70b9 %d", year),
      subtitle = "\u6bcf\u4e00\u70b9\u4e3a\u4e00\u56fd\uff1bY \u8f74\u4e3a log \u5c3a\u5ea6",
      x = NULL, y = "\u4eba\u5747 CHE\uff082023 USD\uff09",
      caption = .cap_news())
}

plot_adv_beeswarm_oops <- function(master, year = NULL) {
  ensure_pkgs(c("ggplot2"))
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year &
              is.finite(master$hf3_che) &
              !is.na(master$continent), ]
  if (!nrow(d)) return(ggplot2::ggplot() + ggplot2::theme_void())
  med <- stats::aggregate(hf3_che ~ continent, data = d, median)
  d$continent <- factor(d$continent,
    levels = med$continent[order(med$hf3_che)])
  if (.has_pkg("ggbeeswarm")) {
    p <- ggplot2::ggplot(d, ggplot2::aes(
        x = .data$continent, y = .data$hf3_che,
        colour = .data$hf3_che)) +
      ggbeeswarm::geom_quasirandom(width = 0.34, alpha = 0.88, size = 2.6)
  } else {
    p <- ggplot2::ggplot(d, ggplot2::aes(
        x = .data$continent, y = .data$hf3_che,
        colour = .data$hf3_che)) +
      ggplot2::geom_jitter(width = 0.30, alpha = 0.85, size = 2.4)
  }
  p +
    scale_colour_ghs3_seq(palette = "ember", guide = "none") +
    ggplot2::scale_y_continuous(labels = function(v) paste0(v, "%"),
                                  limits = c(0, 90)) +
    theme_ghs3() +
    labs_news(
      title = sprintf("OOPS \u5206\u5e03 \u00b7 \u5404\u6d32 %d", year),
      subtitle = "\u989c\u8272\u8d8a\u6df1\u8d8a\u63a5\u8fd1\u5168\u73b0\u91d1\u533b\u7597\u4f53\u7cfb",
      x = NULL, y = "OOPS \u5360 CHE\uff08%\uff09",
      caption = .cap_news())
}

plot_adv_beeswarm_gghed <- function(master, year = NULL) {
  ensure_pkgs(c("ggplot2"))
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year &
              is.finite(master$gghed_che) &
              !is.na(master$continent), ]
  if (!nrow(d)) return(ggplot2::ggplot() + ggplot2::theme_void())
  if (.has_pkg("ggbeeswarm")) {
    p <- ggplot2::ggplot(d, ggplot2::aes(
        x = .data$continent, y = .data$gghed_che,
        colour = .data$continent)) +
      ggbeeswarm::geom_quasirandom(width = 0.32, alpha = 0.86, size = 2.4)
  } else {
    p <- ggplot2::ggplot(d, ggplot2::aes(
        x = .data$continent, y = .data$gghed_che,
        colour = .data$continent)) +
      ggplot2::geom_jitter(width = 0.30, alpha = 0.85, size = 2.4)
  }
  p + scale_colour_brand_continent(guide = "none") +
    ggplot2::scale_y_continuous(labels = function(v) paste0(v, "%")) +
    theme_ghs3() +
    labs_news(
      title = sprintf("GGHED \u5206\u5e03 \u00b7 \u5404\u6d32 %d", year),
      subtitle = "\u9ad8\u6536\u5165\u4f53\u7cfb GGHED \u5747\u8fbe 60%+\uff1b\u4f4e\u6536\u5165\u4f53\u7cfb\u6d6e\u5728 30% \u4e0a\u4e0b",
      x = NULL, y = "GGHED \u5360 CHE\uff08%\uff09",
      caption = .cap_news())
}

plot_adv_beeswarm_lifeexp <- function(master, year = NULL) {
  ensure_pkgs(c("ggplot2"))
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year &
              is.finite(master$life_exp) &
              is.finite(master$che_pc_usd2023) &
              master$che_pc_usd2023 > 0 &
              !is.na(master$continent), ]
  if (!nrow(d)) return(ggplot2::ggplot() + ggplot2::theme_void())
  if (.has_pkg("ggbeeswarm")) {
    p <- ggplot2::ggplot(d, ggplot2::aes(
        x = .data$continent, y = .data$life_exp,
        colour = log10(.data$che_pc_usd2023))) +
      ggbeeswarm::geom_quasirandom(width = 0.33, alpha = 0.88, size = 2.6)
  } else {
    p <- ggplot2::ggplot(d, ggplot2::aes(
        x = .data$continent, y = .data$life_exp,
        colour = log10(.data$che_pc_usd2023))) +
      ggplot2::geom_jitter(width = 0.30, alpha = 0.85, size = 2.4)
  }
  p + scale_colour_ghs3_seq(palette = "ocean",
                             name = "log10 CHE_pc") +
    theme_ghs3() +
    labs_news(
      title = sprintf("\u9884\u671f\u5bff\u547d \u00b7 \u5404\u6d32 %d", year),
      subtitle = "\u989c\u8272\u4ee3\u8868\u4eba\u5747 CHE\uff08\u5bf9\u6570\uff09",
      x = NULL, y = "\u9884\u671f\u5bff\u547d\uff08\u5e74\uff09",
      caption = .cap_news("WDI life expectancy"))
}


plot_adv_stream_global_sources <- function(master) {
  ensure_pkgs(c("ggplot2"))
  d <- master[is.finite(master$year) &
              is.finite(master$gghed_usd2023) &
              is.finite(master$pvtd_usd2023) &
              is.finite(master$ext_usd2023), ]
  if (!nrow(d)) return(ggplot2::ggplot() + ggplot2::theme_void())
  agg <- stats::aggregate(
    cbind(GGHED = d$gghed_usd2023,
          `PVT-D` = d$pvtd_usd2023,
          EXT = d$ext_usd2023) ~ d$year, FUN = sum, na.rm = TRUE)
  names(agg)[1] <- "year"
  long <- stats::reshape(agg, direction = "long",
    varying = list(c("GGHED", "PVT-D", "EXT")),
    v.names = "value", times = c("GGHED", "PVT-D", "EXT"),
    timevar = "source", idvar = "year")
  long$source <- factor(long$source, levels = c("GGHED", "PVT-D", "EXT"))
  ggplot2::ggplot(long, ggplot2::aes(
      x = .data$year, y = .data$value / 1e9, fill = .data$source)) +
    ggplot2::geom_area(position = "stack", alpha = 0.92) +
    ggplot2::scale_y_continuous(
      labels = function(v) paste0("$", format(v, big.mark = ","), "B")) +
    ggplot2::scale_x_continuous(
      breaks = seq(2000, 2025, by = 5)) +
    ggplot2::scale_fill_manual(
      values = c(GGHED = brand_palette$source[["gghed"]],
                 `PVT-D` = brand_palette$source[["pvtd"]],
                 EXT = brand_palette$source[["ext"]]),
      name = NULL) +
    theme_ghs3(grid = "y") +
    labs_news(
      title = "\u5168\u7403\u4e09\u5927\u8d44\u91d1\u6765\u6e90 \u00b7 \u5806\u53e0\u9762\u79ef",
      subtitle = "GGHED \u00b7 PVT-D \u00b7 EXT \u603b\u989d\uff082023 USD\uff09",
      x = NULL, y = "\u603b\u989d\uff08\u5341\u4ebf USD2023\uff09",
      caption = .cap_news())
}

plot_adv_stream_continent <- function(master) {
  ensure_pkgs(c("ggplot2"))
  d <- master[is.finite(master$year) &
              is.finite(master$che_usd2023) &
              !is.na(master$continent), ]
  if (!nrow(d)) return(ggplot2::ggplot() + ggplot2::theme_void())
  agg <- stats::aggregate(che_usd2023 ~ year + continent, data = d,
                          FUN = sum, na.rm = TRUE)
  ggplot2::ggplot(agg, ggplot2::aes(
      x = .data$year, y = .data$che_usd2023 / 1e9,
      fill = .data$continent)) +
    ggplot2::geom_area(position = "stack", alpha = 0.92) +
    ggplot2::scale_y_continuous(
      labels = function(v) paste0("$", format(v, big.mark = ","), "B")) +
    ggplot2::scale_x_continuous(breaks = seq(2000, 2025, by = 5)) +
    scale_fill_brand_continent(name = NULL) +
    theme_ghs3(grid = "y") +
    labs_news(
      title = "\u5168\u7403 CHE \u603b\u989d \u00b7 \u6309\u5927\u6d32\u5806\u53e0",
      subtitle = "\u4ee5\u7f8e\u6d32\u4e3a\u4e3b\u3001\u4e9a\u6d32\u589e\u957f\u6700\u5feb",
      x = NULL, y = "CHE\uff08\u5341\u4ebf USD2023\uff09",
      caption = .cap_news())
}

plot_adv_stream_income <- function(master) {
  ensure_pkgs(c("ggplot2"))
  d <- master[is.finite(master$year) &
              is.finite(master$che_usd2023) &
              !is.na(master$income_group), ]
  if (!nrow(d)) return(ggplot2::ggplot() + ggplot2::theme_void())
  d$income_group <- factor(
    d$income_group,
    levels = c("Low income", "Lower middle income",
               "Upper middle income", "High income"))
  agg <- stats::aggregate(che_usd2023 ~ year + income_group,
                          data = d, FUN = sum, na.rm = TRUE)
  ggplot2::ggplot(agg, ggplot2::aes(
      x = .data$year, y = .data$che_usd2023 / 1e9,
      fill = .data$income_group)) +
    ggplot2::geom_area(position = "stack", alpha = 0.92) +
    ggplot2::scale_y_continuous(
      labels = function(v) paste0("$", format(v, big.mark = ","), "B")) +
    ggplot2::scale_x_continuous(breaks = seq(2000, 2025, by = 5)) +
    scale_fill_brand_income(name = NULL) +
    theme_ghs3(grid = "y") +
    labs_news(
      title = "\u5168\u7403 CHE \u603b\u989d \u00b7 \u6309\u6536\u5165\u7ec4\u5806\u53e0",
      subtitle = "\u9ad8\u6536\u5165\u4f53\u7cfb\u541e\u5410\u4e0d\u53d8\uff0c\u4e2d\u4e0a\u6536\u5165\u589e\u957f\u8d8b\u9a71",
      x = NULL, y = "CHE\uff08\u5341\u4ebf USD2023\uff09",
      caption = .cap_news())
}


plot_adv_bump_top25 <- function(master,
                                 years = c(2000, 2010,
                                           max(master$year)),
                                 n = 25) {
  ensure_pkgs(c("ggplot2"))
  d <- master[master$year %in% years &
              is.finite(master$che_pc_usd2023) &
              master$che_pc_usd2023 > 0, ]
  if (!nrow(d)) return(ggplot2::ggplot() + ggplot2::theme_void())
  d$rank <- stats::ave(-d$che_pc_usd2023, d$year,
                       FUN = function(x) rank(x, ties.method = "first"))
  iso_top <- unique(d$iso3_code[d$year == max(years) & d$rank <= n])
  d <- d[d$iso3_code %in% iso_top, ]
  if (.has_pkg("ggbump")) {
    p <- ggplot2::ggplot(d, ggplot2::aes(
        x = .data$year, y = .data$rank, group = .data$iso3_code,
        colour = .data$iso3_code)) +
      ggbump::geom_bump(linewidth = 1.2, smooth = 8) +
      ggplot2::geom_point(size = 3.4)
  } else {
    p <- ggplot2::ggplot(d, ggplot2::aes(
        x = .data$year, y = .data$rank, group = .data$iso3_code,
        colour = .data$iso3_code)) +
      ggplot2::geom_line(linewidth = 1.1, alpha = 0.85) +
      ggplot2::geom_point(size = 3.2)
  }
  if (.has_pkg("ggrepel")) {
    p <- p + ggrepel::geom_text_repel(
      data = d[d$year == max(years), ],
      ggplot2::aes(label = .data$iso3_code),
      hjust = 0, nudge_x = 0.4, size = 3.6,
      family = get_brand_fonts()$sans, colour = brand_palette$ink,
      direction = "y", max.overlaps = Inf, segment.alpha = 0.4)
  }
  p + ggplot2::scale_y_reverse(breaks = seq(1, n, by = 5)) +
    ggplot2::scale_x_continuous(breaks = years,
      expand = ggplot2::expansion(mult = c(0.04, 0.18))) +
    ggplot2::scale_colour_manual(
      values = palette_ghs3_discrete(min(n, length(iso_top))),
      guide = "none") +
    theme_ghs3(grid = "y") +
    labs_news(
      title = sprintf("\u4eba\u5747 CHE Top %d \u00b7 \u6392\u540d\u53d8\u8fc1", n),
      subtitle = "\u989c\u8272\u8d8a\u8fde\u8d2f\u8868\u793a\u6392\u540d\u8d8a\u7a33",
      x = NULL, y = "\u6392\u540d",
      caption = .cap_news("rank \u00b7 lower is better"))
}

plot_adv_slope_smallmultiples <- function(master,
                                          var = "che_pc_usd2023",
                                          y1 = NULL, y2 = NULL,
                                          top_n = 10) {
  ensure_pkgs(c("ggplot2"))
  if (is.null(y1)) y1 <- min(master$year, na.rm = TRUE)
  if (is.null(y2)) y2 <- max(master$year, na.rm = TRUE)
  d <- master[master$year %in% c(y1, y2) &
              is.finite(master[[var]]) &
              !is.na(master$continent), ]
  if (!nrow(d)) return(ggplot2::ggplot() + ggplot2::theme_void())
  d_split <- split(d, d$continent)
  picked <- do.call(rbind, lapply(d_split, function(sub) {
    isos <- sub$iso3_code[sub$year == y2]
    isos <- isos[order(-sub[[var]][match(isos, sub$iso3_code[sub$year == y2])])]
    isos <- utils::head(isos, top_n)
    sub[sub$iso3_code %in% isos, ]
  }))
  ggplot2::ggplot(picked, ggplot2::aes(
      x = factor(.data$year, levels = c(y1, y2)),
      y = .data[[var]],
      group = .data$iso3_code,
      colour = .data$continent)) +
    ggplot2::geom_line(linewidth = 0.7, alpha = 0.7) +
    ggplot2::geom_point(size = 2.6) +
    ggplot2::facet_wrap(~ .data$continent, scales = "free_y", nrow = 2) +
    scale_colour_brand_continent(guide = "none") +
    theme_ghs3() +
    ggplot2::theme(
      panel.spacing = ggplot2::unit(20, "points"),
      strip.text = ggplot2::element_text(face = "bold")) +
    labs_news(
      title = sprintf("%s \u00b7 %s \u8de8\u5e74\u53d8\u5316",
                       var, paste(y1, "vs", y2)),
      subtitle = "\u6bcf\u6d32\u9009 Top 10 \u56fd\u5bb6\uff0c\u659c\u7387\u6307\u5411\u53d8\u5316\u65b9\u5411",
      x = NULL, y = NULL,
      caption = .cap_news())
}

plot_adv_lollipop_che_change <- function(master, n = 15) {
  ensure_pkgs(c("ggplot2"))
  yrs <- range(master$year, na.rm = TRUE)
  d_first <- master[master$year == yrs[1],
                     c("iso3_code", "country_name", "che_pc_usd2023")]
  d_last <- master[master$year == yrs[2],
                    c("iso3_code", "country_name", "che_pc_usd2023")]
  m <- merge(d_first, d_last, by = c("iso3_code", "country_name"),
             suffixes = c("_first", "_last"))
  m$diff <- m$che_pc_usd2023_last - m$che_pc_usd2023_first
  m <- m[is.finite(m$diff), ]
  if (!nrow(m)) return(ggplot2::ggplot() + ggplot2::theme_void())
  m_top <- utils::head(m[order(-m$diff), ], n)
  m_bot <- utils::head(m[order(m$diff), ], n)
  m_top$grp <- "Top \u4e0a\u5347"
  m_bot$grp <- "Bottom \u4e0b\u964d/\u505c\u6ede"
  m2 <- rbind(m_top, m_bot)
  m2$country_name <- factor(m2$country_name,
    levels = m2$country_name[order(m2$diff)])
  ggplot2::ggplot(m2, ggplot2::aes(
      x = .data$diff, y = .data$country_name,
      colour = .data$grp)) +
    ggplot2::geom_segment(ggplot2::aes(
        x = 0, xend = .data$diff,
        y = .data$country_name, yend = .data$country_name),
      linewidth = 0.7) +
    ggplot2::geom_point(size = 3.4) +
    ggplot2::scale_colour_manual(
      values = c("Top \u4e0a\u5347" = palette_ghs3("good"),
                 "Bottom \u4e0b\u964d/\u505c\u6ede" = palette_ghs3("bad")),
      name = NULL) +
    ggplot2::scale_x_continuous(
      labels = function(v) paste0("$", format(v, big.mark = ","))) +
    theme_ghs3(grid = "x") +
    labs_news(
      title = sprintf("\u4eba\u5747 CHE \u00b7 %d\u2192%d Top/Bottom %d \u53d8\u5316",
                       yrs[1], yrs[2], n),
      subtitle = "\u8d77\u70b9\u5230\u7ec8\u70b9\u7684\u7eddob\u53d8\u5316\u91cf\uff082023 USD\uff09",
      x = "\u4eba\u5747 CHE \u589e\u91cf\uff082023 USD\uff09", y = NULL,
      caption = .cap_news())
}


plot_adv_treemap_continent_che <- function(master, year = NULL) {
  ensure_pkgs(c("ggplot2"))
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year &
              is.finite(master$che_usd2023) &
              !is.na(master$continent), ]
  if (!nrow(d)) return(ggplot2::ggplot() + ggplot2::theme_void())
  if (!.has_pkg("treemapify")) {
    return(ggplot2::ggplot(d) +
             ggplot2::geom_text(ggplot2::aes(x = 0, y = 0,
               label = "treemapify not installed"),
               family = get_brand_fonts()$sans) +
             theme_ghs3())
  }
  ggplot2::ggplot(d, ggplot2::aes(
      area = .data$che_usd2023,
      fill = .data$continent,
      label = .data$iso3_code,
      subgroup = .data$continent)) +
    treemapify::geom_treemap(colour = "white", size = 1.4) +
    treemapify::geom_treemap_subgroup_border(colour = "white", size = 3) +
    treemapify::geom_treemap_text(
      colour = "white", place = "centre", grow = TRUE,
      family = get_brand_fonts()$sans) +
    scale_fill_brand_continent(guide = "none") +
    labs_news(
      title = sprintf("CHE \u603b\u989d \u00b7 \u5927\u6d32\u5d4c\u5957\u6811\u56fe %d", year),
      subtitle = "\u9762\u79ef = CHE \u7eddob\u989d\uff088 \u5927\u56fd\u5e26\u54c1\u724c\u8272\u5757\uff09",
      caption = .cap_news())
}

plot_adv_parallel_finance <- function(master, year = NULL) {
  ensure_pkgs(c("ggplot2"))
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year &
              is.finite(master$gghed_che) &
              is.finite(master$pvtd_che) &
              is.finite(master$ext_che) &
              is.finite(master$hf3_che) &
              !is.na(master$income_group), ]
  if (!nrow(d)) return(ggplot2::ggplot() + ggplot2::theme_void())
  d$income_group <- factor(d$income_group,
    levels = c("Low income", "Lower middle income",
               "Upper middle income", "High income"))
  cols <- c(GGHED = "gghed_che", `OOPS` = "hf3_che",
            `PVT-D` = "pvtd_che", EXT = "ext_che")
  long <- do.call(rbind, lapply(seq_along(cols), function(i) data.frame(
    iso3_code = d$iso3_code,
    income_group = d$income_group,
    var = names(cols)[i],
    share = d[[cols[i]]],
    stringsAsFactors = FALSE)))
  long$var <- factor(long$var, levels = c("GGHED", "OOPS", "PVT-D", "EXT"))
  ggplot2::ggplot(long, ggplot2::aes(
      x = .data$var, y = .data$share,
      group = .data$iso3_code, colour = .data$income_group)) +
    ggplot2::geom_line(alpha = 0.55, linewidth = 0.5) +
    ggplot2::geom_point(alpha = 0.7, size = 1.6) +
    ggplot2::scale_y_continuous(
      labels = function(v) paste0(v, "%"), limits = c(0, 100)) +
    scale_colour_brand_income(name = NULL) +
    theme_ghs3() +
    ggplot2::theme(legend.position = "top") +
    labs_news(
      title = sprintf("\u7b79\u8d44\u7ed3\u6784\u5e73\u884c\u5750\u6807 \u00b7 %d", year),
      subtitle = "\u6bcf\u4e00\u6761\u7ebf\u4e3a\u4e00\u56fd\uff1bGGHED / OOPS / PVT-D / EXT \u56db\u8f74",
      x = NULL, y = "\u5360 CHE\uff08%\uff09",
      caption = .cap_news())
}

plot_adv_marimekko_finance <- function(master, year = NULL) {
  ensure_pkgs(c("ggplot2"))
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year &
              is.finite(master$che_usd2023) &
              is.finite(master$gghed_usd2023) &
              is.finite(master$pvtd_usd2023) &
              is.finite(master$ext_usd2023) &
              !is.na(master$income_group), ]
  if (!nrow(d)) return(ggplot2::ggplot() + ggplot2::theme_void())
  d$income_group <- factor(d$income_group,
    levels = c("Low income", "Lower middle income",
               "Upper middle income", "High income"))
  agg <- stats::aggregate(
    cbind(che = che_usd2023,
          GGHED = gghed_usd2023,
          `PVT-D` = pvtd_usd2023,
          EXT = ext_usd2023) ~ income_group, data = d, sum, na.rm = TRUE)
  long <- stats::reshape(agg,
    direction = "long",
    varying = list(c("GGHED", "PVT-D", "EXT")),
    v.names = "value",
    times = c("GGHED", "PVT-D", "EXT"),
    timevar = "source", idvar = "income_group")
  long$source <- factor(long$source, levels = c("GGHED", "PVT-D", "EXT"))
  long$share <- long$value / long$che
  ggplot2::ggplot(long, ggplot2::aes(
      x = .data$income_group, y = .data$share,
      fill = .data$source)) +
    ggplot2::geom_col(width = 0.85, alpha = 0.92, colour = "white",
                      linewidth = 0.4) +
    ggplot2::scale_y_continuous(
      labels = function(v) paste0(round(v * 100), "%"),
      expand = c(0, 0)) +
    ggplot2::scale_fill_manual(
      values = c(GGHED = brand_palette$source[["gghed"]],
                 `PVT-D` = brand_palette$source[["pvtd"]],
                 EXT = brand_palette$source[["ext"]]),
      name = NULL) +
    theme_ghs3(grid = "y") +
    labs_news(
      title = sprintf("\u8d44\u91d1\u6765\u6e90\u5360\u6bd4 \u00b7 \u6309\u6536\u5165\u7ec4 %d", year),
      subtitle = "\u4f4e\u6536\u5165\u4f53\u7cfb\u5916\u63f4\u5360\u6bd4\u660e\u663e\uff1b\u9ad8\u6536\u5165\u4f53\u7cfb GGHED \u4e3a\u4e3b",
      x = NULL, y = "\u5360 CHE\uff08%\uff09",
      caption = .cap_news())
}


plot_adv_radial_hc_purpose <- function(master,
                                       iso = "USA", year = NULL) {
  ensure_pkgs(c("ggplot2"))
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  row <- master[master$iso3_code == iso & master$year == year, ]
  if (!nrow(row)) return(ggplot2::ggplot() + ggplot2::theme_void())
  hc_cols <- c("hc1_che", "hc2_che", "hc3_che", "hc4_che",
               "hc5_che", "hc6_che", "hc7_che", "hc9_che")
  vals <- vapply(hc_cols, function(c) {
    v <- row[[c]]; if (length(v) && is.finite(v[[1]])) v[[1]] else NA_real_
  }, numeric(1))
  d <- data.frame(
    label = c("HC1\u00b7\u4f4f\u9662", "HC2\u00b7\u95e8\u8bca",
              "HC3\u00b7\u957f\u671f\u62a4\u7406", "HC4\u00b7\u8f85\u52a9\u670d\u52a1",
              "HC5\u00b7\u533b\u836f", "HC6\u00b7\u9884\u9632",
              "HC7\u00b7\u7ba1\u7406", "HC9\u00b7\u5176\u4ed6"),
    value = unname(vals))
  d <- d[is.finite(d$value), ]
  if (!nrow(d)) return(ggplot2::ggplot() + ggplot2::theme_void())
  d$label <- factor(d$label, levels = d$label)
  ggplot2::ggplot(d, ggplot2::aes(
      x = .data$label, y = .data$value, fill = .data$value)) +
    ggplot2::geom_col(width = 0.86, alpha = 0.92, colour = "white") +
    ggplot2::coord_polar(theta = "x", start = -pi / 8) +
    scale_fill_ghs3_seq(palette = "ember", guide = "none") +
    theme_ghs3() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(
      size = 10, lineheight = 1.1)) +
    labs_news(
      title = sprintf("HC \u7528\u9014\u5206\u5e03 \u00b7 %s %d",
                       row$country_name[1], year),
      subtitle = "HC \u5206\u7c7b\uff1a\u4f4f\u9662 / \u95e8\u8bca / \u957f\u671f\u62a4\u7406 / \u8f85\u52a9 / \u533b\u836f / \u9884\u9632 / \u7ba1\u7406 / \u5176\u4ed6",
      x = NULL, y = "\u5360 CHE\uff08%\uff09",
      caption = .cap_news())
}

plot_adv_calendar_growth <- function(master, top_n = 36) {
  ensure_pkgs(c("ggplot2"))
  d <- master[is.finite(master$year) &
              is.finite(master$che_pc_usd2023) &
              master$che_pc_usd2023 > 0, ]
  d <- d[order(d$iso3_code, d$year), ]
  d$prev <- stats::ave(d$che_pc_usd2023, d$iso3_code,
                       FUN = function(x) c(NA, x[-length(x)]))
  d$g <- (d$che_pc_usd2023 / d$prev) - 1
  d <- d[is.finite(d$g), ]
  if (!nrow(d)) return(ggplot2::ggplot() + ggplot2::theme_void())
  iso_top <- utils::head(
    names(sort(table(d$iso3_code), decreasing = TRUE)), top_n)
  d <- d[d$iso3_code %in% iso_top, ]
  d$iso3_code <- factor(d$iso3_code, levels = iso_top)
  ggplot2::ggplot(d, ggplot2::aes(
      x = .data$year, y = .data$iso3_code,
      fill = pmin(pmax(.data$g, -0.2), 0.4))) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.3) +
    scale_fill_ghs3_div(midpoint = 0,
                        labels = scales::percent_format(accuracy = 1),
                        name = "\u540c\u6bd4\u589e\u901f") +
    theme_ghs3(grid = FALSE) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = 8.5),
      panel.grid = ggplot2::element_blank()) +
    ggplot2::scale_x_continuous(
      breaks = seq(2000, 2025, by = 5), expand = c(0.01, 0)) +
    labs_news(
      title = "\u4eba\u5747 CHE \u540c\u6bd4\u589e\u901f \u00b7 \u65e5\u5386\u70ed\u56fe",
      subtitle = sprintf("\u8986\u76d6 %d \u4e2a\u6837\u672c\u6700\u5168\u7684\u56fd\u5bb6", top_n),
      x = NULL, y = NULL,
      caption = .cap_news())
}

plot_adv_density_2d_finance <- function(master, year = NULL) {
  ensure_pkgs(c("ggplot2"))
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year &
              is.finite(master$gghed_che) &
              is.finite(master$hf3_che) &
              !is.na(master$income_group), ]
  if (!nrow(d)) return(ggplot2::ggplot() + ggplot2::theme_void())
  d$income_group <- factor(d$income_group,
    levels = c("Low income", "Lower middle income",
               "Upper middle income", "High income"))
  ggplot2::ggplot(d, ggplot2::aes(
      x = .data$gghed_che, y = .data$hf3_che)) +
    ggplot2::stat_density_2d(
      ggplot2::aes(fill = ggplot2::after_stat(.data$level)),
      geom = "polygon", colour = "white", alpha = 0.75) +
    ggplot2::geom_point(size = 1.6, alpha = 0.6,
                        colour = palette_ghs3("primary")) +
    ggplot2::facet_wrap(~ .data$income_group, ncol = 2) +
    scale_fill_ghs3_seq(palette = "ember", guide = "none") +
    ggplot2::scale_x_continuous(
      labels = function(v) paste0(v, "%"), limits = c(0, 100)) +
    ggplot2::scale_y_continuous(
      labels = function(v) paste0(v, "%"), limits = c(0, 90)) +
    theme_ghs3() +
    labs_news(
      title = sprintf("GGHED \u00d7 OOPS \u00b7 \u6536\u5165\u7ec4\u5bc6\u5ea6 %d", year),
      subtitle = "\u6309\u6536\u5165\u7ec4\u62c6\u5206\u540e\u7684\u8054\u5408\u5206\u5e03\u5728\u5750\u6807\u5e73\u9762\u4e0a\u7684\u4f4d\u7f6e",
      x = "GGHED \u5360 CHE\uff08%\uff09",
      y = "OOPS \u5360 CHE\uff08%\uff09",
      caption = .cap_news())
}


plot_adv_che_pc_by_continent <- function(master) {
  ensure_pkgs(c("ggplot2"))
  d <- master[is.finite(master$year) &
              is.finite(master$che_pc_usd2023) &
              master$che_pc_usd2023 > 0 &
              !is.na(master$continent), ]
  if (!nrow(d)) return(ggplot2::ggplot() + ggplot2::theme_void())
  med <- stats::aggregate(che_pc_usd2023 ~ year + continent, data = d,
                          FUN = function(x) stats::median(x, na.rm = TRUE))
  p25 <- stats::aggregate(che_pc_usd2023 ~ year + continent, data = d,
                          FUN = function(x) stats::quantile(x, 0.25,
                                                              na.rm = TRUE))
  p75 <- stats::aggregate(che_pc_usd2023 ~ year + continent, data = d,
                          FUN = function(x) stats::quantile(x, 0.75,
                                                              na.rm = TRUE))
  m <- merge(merge(med, p25, by = c("year", "continent"),
                    suffixes = c("_med", "_p25")),
              p75, by = c("year", "continent"))
  names(m)[names(m) == "che_pc_usd2023_med"] <- "median"
  names(m)[names(m) == "che_pc_usd2023_p25"] <- "p25"
  names(m)[names(m) == "che_pc_usd2023"] <- "p75"
  ggplot2::ggplot(m, ggplot2::aes(x = .data$year, group = .data$continent)) +
    ggplot2::geom_ribbon(ggplot2::aes(
        ymin = .data$p25, ymax = .data$p75,
        fill = .data$continent), alpha = 0.32) +
    ggplot2::geom_line(ggplot2::aes(
        y = .data$median, colour = .data$continent), linewidth = 1) +
    ggplot2::facet_wrap(~ .data$continent, ncol = 3, scales = "free_y") +
    scale_colour_brand_continent(guide = "none") +
    scale_fill_brand_continent(guide = "none") +
    ggplot2::scale_y_log10(
      labels = function(v) paste0("$", format(round(v), big.mark = ","))) +
    theme_ghs3() +
    ggplot2::theme(
      panel.spacing = ggplot2::unit(20, "points"),
      strip.text = ggplot2::element_text(face = "bold")) +
    labs_news(
      title = "\u4eba\u5747 CHE \u00b7 \u5927\u6d32\u4e2d\u4f4d\u6570\u4e0e\u7eed\u6837\u533a\u95f4",
      subtitle = "\u9634\u5f71\u4e3a IQR\uff0c\u7ebf\u4e3a\u4e2d\u4f4d\u6570\uff1blog y \u8f74",
      x = NULL, y = "\u4eba\u5747 CHE\uff082023 USD\uff09",
      caption = .cap_news())
}

plot_adv_sources_by_income <- function(master) {
  ensure_pkgs(c("ggplot2"))
  d <- master[is.finite(master$year) &
              is.finite(master$gghed_che) &
              is.finite(master$pvtd_che) &
              is.finite(master$ext_che) &
              !is.na(master$income_group), ]
  if (!nrow(d)) return(ggplot2::ggplot() + ggplot2::theme_void())
  d$income_group <- factor(d$income_group,
    levels = c("Low income", "Lower middle income",
               "Upper middle income", "High income"))
  med <- stats::aggregate(
    cbind(GGHED = d$gghed_che, `PVT-D` = d$pvtd_che, EXT = d$ext_che) ~
      d$year + d$income_group, FUN = function(x) median(x, na.rm = TRUE))
  names(med)[1:2] <- c("year", "income_group")
  long <- stats::reshape(med, direction = "long",
    varying = list(c("GGHED", "PVT-D", "EXT")),
    v.names = "share",
    times = c("GGHED", "PVT-D", "EXT"),
    timevar = "source", idvar = c("year", "income_group"))
  long$source <- factor(long$source, levels = c("GGHED", "PVT-D", "EXT"))
  ggplot2::ggplot(long, ggplot2::aes(
      x = .data$year, y = .data$share,
      colour = .data$source)) +
    ggplot2::geom_line(linewidth = 1.05) +
    ggplot2::facet_wrap(~ .data$income_group, ncol = 2) +
    ggplot2::scale_colour_manual(
      values = c(GGHED = brand_palette$source[["gghed"]],
                 `PVT-D` = brand_palette$source[["pvtd"]],
                 EXT = brand_palette$source[["ext"]]),
      name = NULL) +
    ggplot2::scale_y_continuous(
      labels = function(v) paste0(v, "%"), limits = c(0, 100)) +
    theme_ghs3() +
    ggplot2::theme(panel.spacing = ggplot2::unit(20, "points")) +
    labs_news(
      title = "\u4e09\u5927\u8d44\u91d1\u6765\u6e90 \u00b7 \u4e2d\u4f4d\u5360\u6bd4\u8de8\u5e74\u8d8b\u52bf",
      subtitle = "\u6309\u4e16\u94f6\u6536\u5165\u7ec4\u62c6\u5206\uff0c\u7ebf\u6761\u4e3a\u5e74\u5ea6\u4e2d\u4f4d\u6570",
      x = NULL, y = "\u5360 CHE\uff08%\uff09",
      caption = .cap_news())
}


plot_adv_topbot_dotplot <- function(master, var = "che_pc_usd2023",
                                    n = 15, year = NULL) {
  ensure_pkgs(c("ggplot2"))
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year & is.finite(master[[var]]), ]
  if (!nrow(d)) return(ggplot2::ggplot() + ggplot2::theme_void())
  d_top <- utils::head(d[order(-d[[var]]), ], n)
  d_bot <- utils::head(d[order(d[[var]]), ], n)
  d_top$grp <- "Top"
  d_bot$grp <- "Bottom"
  m <- rbind(d_top, d_bot)
  m$country_name <- factor(m$country_name,
    levels = m$country_name[order(m[[var]])])
  unit <- if (grepl("usd", var)) "USD2023"
          else if (grepl("che$", var)) "%"
          else if (var %in% c("life_exp")) "\u5e74"
          else if (var %in% c("u5mr")) "/1000"
          else ""
  ggplot2::ggplot(m, ggplot2::aes(
      x = .data[[var]], y = .data$country_name,
      colour = .data$grp)) +
    ggplot2::geom_segment(ggplot2::aes(x = 0, xend = .data[[var]],
        y = .data$country_name, yend = .data$country_name),
      linewidth = 0.7, colour = "grey75") +
    ggplot2::geom_point(size = 3.5, alpha = 0.92) +
    ggplot2::scale_colour_manual(
      values = c(Top = palette_ghs3("primary"),
                 Bottom = palette_ghs3("bad")),
      name = NULL) +
    theme_ghs3(grid = "x") +
    labs_news(
      title = sprintf("%s \u00b7 Top/Bottom %d \u56fd %d",
                       var, n, year),
      subtitle = "\u7eddob\u503c\u6700\u9ad8 / \u6700\u4f4e\u7684 \u4e24\u7aef",
      x = paste(var, unit), y = NULL,
      caption = .cap_news())
}


ghs_export_advanced <- function(master = NULL, fig_dir = NULL,
                                 base_size = 12) {
  if (is.null(master)) {
    master_path <- file.path(proj_root(), "\u6d3e\u751f\u6570\u636e",
                              "\u5904\u7406\u7ed3\u679c", "master_enriched.rds")
    if (!file.exists(master_path))
      stop("Missing master_enriched.rds")
    master <- readRDS(master_path)
  }
  if (is.null(fig_dir)) {
    fig_dir <- file.path(proj_root(), "\u5206\u6790\u8f93\u51fa",
                          "\u56fe\u8868")
  }
  dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

  plots <- list(
    adv_ridge_oops_by_income       = plot_adv_ridge_oops_by_income(master),
    adv_ridge_che_pc_evolution     = plot_adv_ridge_che_pc_evolution(master),
    adv_ridge_gghed_by_continent   = plot_adv_ridge_gghed_by_continent(master),
    adv_beeswarm_che_pc            = plot_adv_beeswarm_che_pc(master),
    adv_beeswarm_oops              = plot_adv_beeswarm_oops(master),
    adv_beeswarm_gghed             = plot_adv_beeswarm_gghed(master),
    adv_beeswarm_lifeexp           = plot_adv_beeswarm_lifeexp(master),
    adv_stream_global_sources      = plot_adv_stream_global_sources(master),
    adv_stream_continent           = plot_adv_stream_continent(master),
    adv_stream_income              = plot_adv_stream_income(master),
    adv_bump_top25                 = plot_adv_bump_top25(master),
    adv_slope_smallmultiples       = plot_adv_slope_smallmultiples(master),
    adv_lollipop_che_change        = plot_adv_lollipop_che_change(master),
    adv_treemap_continent_che      = plot_adv_treemap_continent_che(master),
    adv_parallel_finance           = plot_adv_parallel_finance(master),
    adv_marimekko_finance          = plot_adv_marimekko_finance(master),
    adv_radial_hc_usa              = plot_adv_radial_hc_purpose(master, "USA"),
    adv_radial_hc_chn              = plot_adv_radial_hc_purpose(master, "CHN"),
    adv_radial_hc_deu              = plot_adv_radial_hc_purpose(master, "DEU"),
    adv_radial_hc_bra              = plot_adv_radial_hc_purpose(master, "BRA"),
    adv_radial_hc_zaf              = plot_adv_radial_hc_purpose(master, "ZAF"),
    adv_radial_hc_jpn              = plot_adv_radial_hc_purpose(master, "JPN"),
    adv_calendar_growth            = plot_adv_calendar_growth(master),
    adv_density_2d_finance         = plot_adv_density_2d_finance(master),
    adv_che_pc_by_continent        = plot_adv_che_pc_by_continent(master),
    adv_sources_by_income          = plot_adv_sources_by_income(master),
    adv_topbot_che_pc              = plot_adv_topbot_dotplot(master,
                                       "che_pc_usd2023", 15),
    adv_topbot_oops                = plot_adv_topbot_dotplot(master,
                                       "hf3_che", 15),
    adv_topbot_gghed               = plot_adv_topbot_dotplot(master,
                                       "gghed_che", 15),
    adv_topbot_life                = plot_adv_topbot_dotplot(master,
                                       "life_exp", 15)
  )

  out <- character(0)
  for (nm in names(plots)) {
    p <- plots[[nm]]
    if (is.null(p)) next
    ok <- tryCatch({
      png_path <- file.path(fig_dir, paste0(nm, ".png"))
      svg_path <- file.path(fig_dir, paste0(nm, ".svg"))
      ggplot2::ggsave(png_path, p, width = 10, height = 6, dpi = 160,
                      bg = brand_palette$paper)
      ggplot2::ggsave(svg_path, p, width = 10, height = 6, bg = brand_palette$paper)
      out <<- c(out, png_path, svg_path)
      TRUE
    }, error = function(e) {
      message("[adv export] ", nm, " failed: ", conditionMessage(e))
      FALSE
    })
  }
  invisible(out)
}
