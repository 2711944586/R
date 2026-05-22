
if (!exists("ensure_pkgs", mode = "function")) {
  source(file.path("\u7a0b\u5e8f", "00_utils.R"))
}

.st <- function(bs = 12) {
  if (exists("theme_ghs3", mode = "function")) theme_ghs3(base_size = bs)
  else if (exists("theme_ghs2", mode = "function")) theme_ghs2(base_size = bs)
  else ggplot2::theme_minimal(base_size = bs)
}

.sc <- function(n, extra = NULL) {
  base <- sprintf(
    "\u6570\u636e: WHO GHED 2024-12 \u00b7 N = %d \u00b7 \u5206\u6790: \u5e84\u9882 (20241334)", n)
  if (!is.null(extra)) paste0(base, " \u00b7 ", extra) else base
}

.yr_max <- function(m) max(m$year, na.rm = TRUE)

plot_div_lorenz <- function(master, years = c(2000, 2010, 2023)) {
  ensure_pkgs(c("dplyr", "ggplot2"))
  plots_data <- lapply(years, function(yr) {
    d <- master[master$year == yr & is.finite(master$che_pc_usd2023), ]
    d <- d[order(d$che_pc_usd2023), ]
    n <- nrow(d)
    if (n < 10) return(NULL)
    data.frame(
      year = as.character(yr),
      cum_pop = seq_len(n) / n,
      cum_che = cumsum(d$che_pc_usd2023) / sum(d$che_pc_usd2023)
    )
  })
  df <- do.call(rbind, plots_data[!vapply(plots_data, is.null, logical(1))])
  ggplot2::ggplot(df, ggplot2::aes(.data$cum_pop, .data$cum_che, color = .data$year)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#5d667a") +
    ggplot2::geom_line(linewidth = 1.2) +
    ggplot2::scale_color_manual(values = c("#1d3f5f", "#c46327", "#2a857a")) +
    .st() +
    ggplot2::labs(
      title = "\u4eba\u5747\u536b\u751f\u652f\u51fa\u7684 Lorenz \u66f2\u7ebf\u6f14\u5316",
      subtitle = "\u66f2\u7ebf\u8ddd\u5bf9\u89d2\u7ebf\u8d8a\u8fdc\uff0c\u4e0d\u5e73\u7b49\u7a0b\u5ea6\u8d8a\u9ad8",
      x = "\u7d2f\u79ef\u56fd\u5bb6\u6bd4\u4f8b", y = "\u7d2f\u79ef CHE \u4efd\u989d",
      color = "\u5e74\u4efd",
      caption = .sc(nrow(master[master$year == max(years), ])))
}

plot_div_polar_purpose <- function(master) {
  ensure_pkgs(c("dplyr", "ggplot2"))
  yr <- .yr_max(master)
  cols <- paste0("hc", c(1:7, 9), "_che")
  cols <- cols[cols %in% names(master)]
  if (length(cols) < 4) return(NULL)
  vals <- colMeans(master[master$year == yr, cols, drop = FALSE], na.rm = TRUE)
  df <- data.frame(
    purpose = names(vals),
    value = as.numeric(vals),
    stringsAsFactors = FALSE
  )
  df$purpose <- sub("_che$", "", df$purpose)
  df <- df[order(-df$value), ]
  df$purpose <- factor(df$purpose, levels = df$purpose)
  ggplot2::ggplot(df, ggplot2::aes(.data$purpose, .data$value, fill = .data$purpose)) +
    ggplot2::geom_col(width = 0.8, show.legend = FALSE) +
    ggplot2::coord_polar(start = 0) +
    ggplot2::scale_fill_manual(values = palette_ghs3_discrete(nrow(df))) +
    .st() +
    ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_blank()) +
    ggplot2::labs(
      title = sprintf("\u536b\u751f\u652f\u51fa\u7528\u9014\u7ed3\u6784 (%d)", yr),
      subtitle = "HC1\u2013HC9 \u5168\u7403\u5747\u503c \u00b7 \u6781\u5750\u6807",
      x = NULL, y = NULL,
      caption = .sc(sum(master$year == yr & is.finite(master$hf3_che))))
}

plot_div_dumbbell_che <- function(master) {
  ensure_pkgs(c("dplyr", "ggplot2"))
  yr_a <- min(master$year, na.rm = TRUE)
  yr_b <- .yr_max(master)
  da <- master[master$year == yr_a & is.finite(master$che_pc_usd2023),
               c("iso3_code", "country_name", "che_pc_usd2023")]
  db <- master[master$year == yr_b & is.finite(master$che_pc_usd2023),
               c("iso3_code", "che_pc_usd2023")]
  names(da)[3] <- "val_a"; names(db)[2] <- "val_b"
  m <- merge(da, db, by = "iso3_code")
  m$change <- m$val_b - m$val_a
  m <- utils::head(m[order(-m$change), ], 20)
  m$country_name <- factor(m$country_name, levels = rev(m$country_name))
  ggplot2::ggplot(m) +
    ggplot2::geom_segment(ggplot2::aes(x = .data$val_a, xend = .data$val_b,
                                        y = .data$country_name, yend = .data$country_name),
                          color = "#5d667a", linewidth = 0.6) +
    ggplot2::geom_point(ggplot2::aes(x = .data$val_a, y = .data$country_name),
                        color = "#2a857a", size = 3) +
    ggplot2::geom_point(ggplot2::aes(x = .data$val_b, y = .data$country_name),
                        color = "#c46327", size = 3) +
    ggplot2::scale_x_continuous(labels = scales::label_dollar()) +
    .st() +
    ggplot2::labs(
      title = sprintf("\u4eba\u5747 CHE \u589e\u957f\u6700\u5927\u7684 20 \u56fd (%d\u2192%d)", yr_a, yr_b),
      subtitle = "\u7eff\u8272 = \u8d77\u59cb\u5e74 \u00b7 \u6a59\u8272 = \u6700\u65b0\u5e74",
      x = "\u4eba\u5747 CHE (USD)", y = NULL,
      caption = .sc(nrow(m)))
}

plot_div_waterfall_che <- function(master) {
  ensure_pkgs(c("dplyr", "ggplot2"))
  d <- master[is.finite(master$che_usd2023) & !is.na(master$continent), ]
  yr_a <- min(d$year); yr_b <- max(d$year)
  by_cont <- stats::aggregate(che_usd2023 ~ continent + year, data = d, FUN = sum)
  start_total <- sum(by_cont$che_usd2023[by_cont$year == yr_a])
  deltas <- tapply(by_cont$che_usd2023[by_cont$year == yr_b] -
                   by_cont$che_usd2023[by_cont$year == yr_a],
                   by_cont$continent[by_cont$year == yr_b], sum)
  deltas <- sort(deltas, decreasing = TRUE)
  df <- data.frame(
    label = c("Start", names(deltas), "End"),
    value = c(start_total, as.numeric(deltas), start_total + sum(deltas)),
    type = c("total", rep("delta", length(deltas)), "total"),
    stringsAsFactors = FALSE
  )
  df$label <- factor(df$label, levels = df$label)
  df$yend <- cumsum(c(start_total, deltas, 0))
  df$ystart <- c(0, utils::head(df$yend, -1))
  df$ystart[1] <- 0; df$yend[1] <- start_total
  df$ystart[nrow(df)] <- 0; df$yend[nrow(df)] <- start_total + sum(deltas)
  ggplot2::ggplot(df, ggplot2::aes(.data$label)) +
    ggplot2::geom_rect(ggplot2::aes(xmin = as.numeric(.data$label) - 0.35,
                                     xmax = as.numeric(.data$label) + 0.35,
                                     ymin = .data$ystart / 1e12,
                                     ymax = .data$yend / 1e12,
                                     fill = .data$type)) +
    ggplot2::scale_fill_manual(values = c(total = "#1d3f5f", delta = "#c46327"), guide = "none") +
    ggplot2::scale_y_continuous(labels = scales::label_dollar(suffix = "T")) +
    .st() +
    ggplot2::labs(
      title = sprintf("\u5168\u7403 CHE \u589e\u91cf\u5206\u89e3 (%d\u2192%d)", yr_a, yr_b),
      subtitle = "\u6309\u5927\u6d32\u5206\u89e3\u7684\u589e\u91cf\u8d21\u732e",
      x = NULL, y = "CHE (T USD)",
      caption = .sc(length(unique(d$iso3_code))))
}

plot_div_dotmatrix_oops <- function(master) {
  ensure_pkgs(c("dplyr", "ggplot2"))
  yr <- .yr_max(master)
  d <- master[master$year == yr & is.finite(master$hf3_che) & !is.na(master$continent), ]
  d <- d[order(d$continent, -d$hf3_che), ]
  d$rank <- seq_len(nrow(d))
  d$high_risk <- d$hf3_che > 40
  ggplot2::ggplot(d, ggplot2::aes(x = .data$rank %% 15, y = .data$rank %/% 15,
                                   color = .data$high_risk)) +
    ggplot2::geom_point(size = 2.5) +
    ggplot2::scale_color_manual(values = c("FALSE" = "#5d667a", "TRUE" = "#a23b3b"),
                                labels = c("OOPS \u226440%", "OOPS >40%")) +
    .st() +
    ggplot2::theme(axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank()) +
    ggplot2::labs(
      title = sprintf("OOPS \u9ad8\u98ce\u9669\u56fd\u5bb6\u70b9\u9635\u56fe (%d)", yr),
      subtitle = "\u6bcf\u4e2a\u70b9\u4ee3\u8868\u4e00\u4e2a\u56fd\u5bb6 \u00b7 \u7ea2\u8272 = OOPS > 40%",
      x = NULL, y = NULL, color = NULL,
      caption = .sc(nrow(d)))
}

plot_div_stacked_sources <- function(master) {
  ensure_pkgs(c("dplyr", "ggplot2", "tidyr"))
  d <- master[is.finite(master$gghed_che) & is.finite(master$pvtd_che) &
              is.finite(master$ext_che), ]
  agg <- stats::aggregate(cbind(gghed_che, pvtd_che, ext_che) ~ year, data = d, FUN = mean)
  long <- tidyr::pivot_longer(agg, cols = c("gghed_che", "pvtd_che", "ext_che"),
                               names_to = "source", values_to = "pct")
  long$source <- factor(long$source, levels = c("gghed_che", "pvtd_che", "ext_che"),
                         labels = c("GGHE-D", "PVT-D", "EXT"))
  ggplot2::ggplot(long, ggplot2::aes(.data$year, .data$pct, fill = .data$source)) +
    ggplot2::geom_area(alpha = 0.85) +
    ggplot2::scale_fill_manual(values = c("GGHE-D" = "#1d3f5f", "PVT-D" = "#c46327", "EXT" = "#2a857a")) +
    .st() +
    ggplot2::labs(
      title = "\u5168\u7403\u536b\u751f\u7b79\u8d44\u4e09\u6e90\u7ed3\u6784\u6f14\u5316",
      subtitle = "\u653f\u5e9c (GGHE-D) + \u79c1\u4eba (PVT-D) + \u5916\u63f4 (EXT) \u5360 CHE %",
      x = NULL, y = "\u5360 CHE (%)", fill = NULL,
      caption = .sc(length(unique(d$iso3_code))))
}

plot_div_ridges_income_years <- function(master) {
  ensure_pkgs(c("dplyr", "ggplot2", "ggridges"))
  yrs <- c(2000, 2005, 2010, 2015, 2020)
  d <- master[master$year %in% yrs & is.finite(master$che_pc_usd2023) &
              !is.na(master$income_group), ]
  d$year_f <- factor(d$year)
  ggplot2::ggplot(d, ggplot2::aes(x = log10(.data$che_pc_usd2023), y = .data$year_f,
                                   fill = .data$income_group)) +
    ggridges::geom_density_ridges(alpha = 0.6, scale = 1.2) +
    scale_fill_brand_income() +
    .st() +
    ggplot2::labs(
      title = "\u4eba\u5747 CHE \u5206\u5e03\u7684\u65f6\u5e8f\u6f14\u5316",
      subtitle = "\u6309\u6536\u5165\u7ec4\u7740\u8272 \u00b7 \u5bf9\u6570\u5750\u6807 \u00b7 5 \u4e2a\u622a\u9762\u5e74",
      x = "log10(\u4eba\u5747 CHE)", y = NULL, fill = "\u6536\u5165\u7ec4",
      caption = .sc(length(unique(d$iso3_code))))
}

plot_div_corr_heatmap <- function(master) {
  ensure_pkgs(c("dplyr", "ggplot2"))
  yr <- .yr_max(master)
  vars <- c("che_pc_usd2023", "gghed_che", "hf3_che", "ext_che", "life_exp", "u5mr")
  vars <- vars[vars %in% names(master)]
  d <- master[master$year == yr, vars, drop = FALSE]
  d <- d[stats::complete.cases(d), ]
  if (nrow(d) < 20) return(NULL)
  cor_mat <- stats::cor(d, use = "pairwise.complete.obs")
  long <- expand.grid(var1 = rownames(cor_mat), var2 = colnames(cor_mat), stringsAsFactors = FALSE)
  long$r <- as.vector(cor_mat)
  ggplot2::ggplot(long, ggplot2::aes(.data$var1, .data$var2, fill = .data$r)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.5) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", .data$r)), size = 3.5) +
    scale_fill_ghs3_div(midpoint = 0) +
    .st() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::labs(
      title = sprintf("\u536b\u751f\u652f\u51fa\u6838\u5fc3\u53d8\u91cf\u76f8\u5173\u77e9\u9635 (%d)", yr),
      subtitle = "Pearson \u76f8\u5173\u7cfb\u6570 \u00b7 \u53d1\u6563\u8272\u9636",
      x = NULL, y = NULL, fill = "r",
      caption = .sc(nrow(d)))
}

plot_div_beeswarm_oops <- function(master) {
  ensure_pkgs(c("dplyr", "ggplot2"))
  yr <- .yr_max(master)
  d <- master[master$year == yr & is.finite(master$hf3_che) & !is.na(master$continent), ]
  ggplot2::ggplot(d, ggplot2::aes(.data$continent, .data$hf3_che, color = .data$continent)) +
    ggplot2::geom_jitter(width = 0.25, alpha = 0.7, size = 2) +
    ggplot2::stat_summary(fun = median, geom = "crossbar", width = 0.5,
                          color = "#0d121b", linewidth = 0.6) +
    scale_colour_brand_continent() +
    .st() +
    ggplot2::labs(
      title = sprintf("OOPS \u5360\u6bd4\u7684\u5927\u6d32\u5206\u5e03 (%d)", yr),
      subtitle = "\u6bcf\u4e2a\u70b9 = \u4e00\u4e2a\u56fd\u5bb6 \u00b7 \u6a2a\u7ebf = \u4e2d\u4f4d\u6570",
      x = NULL, y = "OOPS (%)", color = NULL,
      caption = .sc(nrow(d)))
}

plot_div_facet_scatter_life <- function(master) {
  ensure_pkgs(c("dplyr", "ggplot2"))
  yr <- .yr_max(master)
  d <- master[master$year == yr & is.finite(master$che_pc_usd2023) &
              is.finite(master$life_exp) & !is.na(master$continent), ]
  ggplot2::ggplot(d, ggplot2::aes(.data$che_pc_usd2023, .data$life_exp)) +
    ggplot2::geom_point(ggplot2::aes(color = .data$continent), alpha = 0.7, size = 2, show.legend = FALSE) +
    ggplot2::geom_smooth(method = "loess", se = FALSE, color = "#c46327", linewidth = 0.8) +
    ggplot2::scale_x_log10(labels = scales::label_dollar()) +
    ggplot2::facet_wrap(~continent, ncol = 3) +
    scale_colour_brand_continent() +
    .st() +
    ggplot2::labs(
      title = sprintf("\u4eba\u5747 CHE \u4e0e\u9884\u671f\u5bff\u547d\u7684\u5927\u6d32\u5206\u9762 (%d)", yr),
      subtitle = "\u5bf9\u6570\u5750\u6807 \u00b7 LOESS \u62df\u5408",
      x = "\u4eba\u5747 CHE (USD, log)", y = "\u9884\u671f\u5bff\u547d (\u5c81)",
      caption = .sc(nrow(d)))
}

ghs_export_diverse <- function(master, fig_dir = NULL) {
  ensure_pkgs(c("dplyr", "ggplot2"))
  if (is.null(fig_dir)) fig_dir <- file.path(proj_root(), "\u5206\u6790\u8f93\u51fa", "\u56fe\u8868")
  dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

  fns <- list(
    list(fn = plot_div_lorenz, name = "255_lorenz_che_evolution", w = 10, h = 7),
    list(fn = plot_div_polar_purpose, name = "256_polar_hc_purpose", w = 9, h = 9),
    list(fn = plot_div_dumbbell_che, name = "257_dumbbell_che_growth_top20", w = 11, h = 8),
    list(fn = plot_div_waterfall_che, name = "258_waterfall_che_continent", w = 11, h = 7),
    list(fn = plot_div_dotmatrix_oops, name = "259_dotmatrix_oops_risk", w = 10, h = 8),
    list(fn = plot_div_stacked_sources, name = "260_area_stacked_sources", w = 10, h = 6),
    list(fn = plot_div_ridges_income_years, name = "261_ridges_income_years", w = 10, h = 7),
    list(fn = plot_div_corr_heatmap, name = "262_corr_heatmap_vars", w = 9, h = 8),
    list(fn = plot_div_beeswarm_oops, name = "263_beeswarm_oops_continent", w = 10, h = 7),
    list(fn = plot_div_facet_scatter_life, name = "264_facet_scatter_che_life", w = 12, h = 8)
  )

  saved <- character(0)
  for (item in fns) {
    p <- tryCatch(item$fn(master), error = function(e) { message(item$name, ": ", conditionMessage(e)); NULL })
    if (!is.null(p)) {
      save_fig(p, item$name, width = item$w, height = item$h, dir = fig_dir)
      saved <- c(saved, item$name)
    }
  }

  for (yr in c(2005, 2010, 2015)) {
    nm <- sprintf("265_facet_scatter_che_life_%d", yr)
    p <- tryCatch({
      d <- master[master$year == yr & is.finite(master$che_pc_usd2023) &
                  is.finite(master$life_exp) & !is.na(master$continent), ]
      ggplot2::ggplot(d, ggplot2::aes(.data$che_pc_usd2023, .data$life_exp)) +
        ggplot2::geom_point(ggplot2::aes(color = .data$continent), alpha = 0.7, size = 2.5) +
        ggplot2::geom_smooth(method = "loess", se = TRUE, color = "#c46327",
                             linewidth = 0.8, alpha = 0.12) +
        ggplot2::scale_x_log10(labels = scales::label_dollar()) +
        scale_colour_brand_continent() + .st() +
        ggplot2::labs(
          title = sprintf("\u4eba\u5747 CHE \u4e0e\u9884\u671f\u5bff\u547d (%d)", yr),
          subtitle = sprintf("%d \u56fd\u5bb6 \u00b7 \u5bf9\u6570\u5750\u6807", nrow(d)),
          x = "\u4eba\u5747 CHE (USD, log)", y = "\u9884\u671f\u5bff\u547d",
          color = "\u5927\u6d32", caption = .sc(nrow(d)))
    }, error = function(e) NULL)
    if (!is.null(p)) { save_fig(p, nm, dir = fig_dir); saved <- c(saved, nm) }
  }

  for (info in list(
    list(col = "gghed_che", zh = "GGHE-D", nm = "268_violin_gghed"),
    list(col = "ext_che", zh = "\u5916\u63f4 EXT", nm = "269_violin_ext"),
    list(col = "che_pc_usd2023", zh = "\u4eba\u5747 CHE", nm = "270_violin_che_pc")
  )) {
    p <- tryCatch({
      yr <- .yr_max(master)
      d <- master[master$year == yr & is.finite(master[[info$col]]) & !is.na(master$continent), ]
      ggplot2::ggplot(d, ggplot2::aes(.data$continent, .data[[info$col]], fill = .data$continent)) +
        ggplot2::geom_violin(alpha = 0.5, show.legend = FALSE) +
        ggplot2::geom_jitter(width = 0.15, alpha = 0.4, size = 1.5, show.legend = FALSE) +
        scale_fill_brand_continent() + .st() +
        ggplot2::labs(title = sprintf("%s \u5206\u5e03\u6309\u5927\u6d32 (%d)", info$zh, yr),
                     x = NULL, y = info$col, caption = .sc(nrow(d)))
    }, error = function(e) NULL)
    if (!is.null(p)) { save_fig(p, info$nm, dir = fig_dir); saved <- c(saved, info$nm) }
  }

  for (info in list(
    list(col = "che_pc_usd2023", zh = "\u4eba\u5747 CHE", nm = "271_period_che"),
    list(col = "hf3_che", zh = "OOPS", nm = "272_period_oops"),
    list(col = "gghed_che", zh = "GGHE-D", nm = "273_period_gghed")
  )) {
    p <- tryCatch({
      d <- master[is.finite(master[[info$col]]) & !is.na(master$continent), ]
      d$period <- cut(d$year, breaks = c(1999, 2007, 2015, 2024),
                      labels = c("2000-07", "2008-15", "2016-23"))
      agg <- stats::aggregate(stats::as.formula(paste(info$col, "~ period + continent")),
                               data = d, FUN = mean)
      ggplot2::ggplot(agg, ggplot2::aes(.data$continent, .data[[info$col]], fill = .data$period)) +
        ggplot2::geom_col(position = "dodge", width = 0.7) +
        ggplot2::scale_fill_manual(values = c("#1d3f5f", "#c46327", "#2a857a")) +
        .st() +
        ggplot2::labs(title = sprintf("%s \u4e09\u671f\u5bf9\u6bd4", info$zh),
                     x = NULL, y = info$col, fill = "\u65f6\u6bb5",
                     caption = .sc(length(unique(d$iso3_code))))
    }, error = function(e) NULL)
    if (!is.null(p)) { save_fig(p, info$nm, dir = fig_dir); saved <- c(saved, info$nm) }
  }

  for (info in list(
    list(col = "che_pc_usd2023", zh = "\u4eba\u5747 CHE", nm = "274_trend_income_che"),
    list(col = "hf3_che", zh = "OOPS", nm = "275_trend_income_oops"),
    list(col = "life_exp", zh = "\u9884\u671f\u5bff\u547d", nm = "276_trend_income_life"),
    list(col = "ext_che", zh = "\u5916\u63f4", nm = "277_trend_income_ext")
  )) {
    p <- tryCatch({
      d <- master[is.finite(master[[info$col]]) & !is.na(master$income_group), ]
      agg <- stats::aggregate(stats::as.formula(paste(info$col, "~ year + income_group")),
                               data = d, FUN = mean)
      ggplot2::ggplot(agg, ggplot2::aes(.data$year, .data[[info$col]], color = .data$income_group)) +
        ggplot2::geom_line(linewidth = 1.1) + ggplot2::geom_point(size = 1.5) +
        scale_colour_brand_income() + .st() +
        ggplot2::labs(title = sprintf("%s \u6309\u6536\u5165\u7ec4\u8d8b\u52bf", info$zh),
                     x = NULL, y = info$col, color = "\u6536\u5165\u7ec4",
                     caption = .sc(length(unique(d$iso3_code))))
    }, error = function(e) NULL)
    if (!is.null(p)) { save_fig(p, info$nm, dir = fig_dir); saved <- c(saved, info$nm) }
  }

  for (cont in c("Africa", "Asia", "Europe", "Americas")) {
    nm <- sprintf("278_area_%s_sources", tolower(cont))
    p <- tryCatch({
      d <- master[master$continent == cont & is.finite(master$gghed_che), ]
      agg <- stats::aggregate(cbind(gghed_che, pvtd_che, ext_che) ~ year, data = d, FUN = mean)
      long <- tidyr::pivot_longer(agg, -year, names_to = "source", values_to = "pct")
      long$source <- factor(long$source, levels = c("gghed_che", "pvtd_che", "ext_che"),
                             labels = c("GGHE-D", "PVT-D", "EXT"))
      ggplot2::ggplot(long, ggplot2::aes(.data$year, .data$pct, fill = .data$source)) +
        ggplot2::geom_area(alpha = 0.8) +
        ggplot2::scale_fill_manual(values = c("#1d3f5f", "#c46327", "#2a857a")) +
        .st() +
        ggplot2::labs(title = sprintf("%s \u7b79\u8d44\u7ed3\u6784\u6f14\u5316", cont),
                     x = NULL, y = "% of CHE", fill = NULL,
                     caption = .sc(length(unique(d$iso3_code))))
    }, error = function(e) NULL)
    if (!is.null(p)) { save_fig(p, nm, dir = fig_dir); saved <- c(saved, nm) }
  }

  for (info in list(
    list(col = "che_pc_usd2023", zh = "\u4eba\u5747 CHE", nm = "282_hist_che_pc", log = TRUE),
    list(col = "hf3_che", zh = "OOPS", nm = "283_hist_oops", log = FALSE),
    list(col = "life_exp", zh = "\u9884\u671f\u5bff\u547d", nm = "284_hist_life_exp", log = FALSE)
  )) {
    p <- tryCatch({
      yr <- .yr_max(master)
      d <- master[master$year == yr & is.finite(master[[info$col]]), ]
      x_val <- if (info$log) log10(d[[info$col]]) else d[[info$col]]
      df <- data.frame(x = x_val)
      ggplot2::ggplot(df, ggplot2::aes(.data$x)) +
        ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                                bins = 30, fill = "#1d3f5f", alpha = 0.6) +
        ggplot2::geom_density(color = "#c46327", linewidth = 1) +
        .st() +
        ggplot2::labs(
          title = sprintf("%s \u5206\u5e03 (%d)", info$zh, yr),
          subtitle = if (info$log) "\u5bf9\u6570\u5750\u6807" else "\u539f\u59cb\u5750\u6807",
          x = if (info$log) paste0("log10(", info$col, ")") else info$col,
          y = "\u5bc6\u5ea6", caption = .sc(nrow(d)))
    }, error = function(e) NULL)
    if (!is.null(p)) { save_fig(p, info$nm, dir = fig_dir); saved <- c(saved, info$nm) }
  }

  for (info in list(
    list(col = "hf3_che", zh = "OOPS \u6700\u9ad8", nm = "285_lollipop_oops_top", desc = TRUE),
    list(col = "gghed_che", zh = "GGHE-D \u6700\u9ad8", nm = "286_lollipop_gghed_top", desc = TRUE),
    list(col = "che_pc_usd2023", zh = "\u4eba\u5747 CHE \u6700\u4f4e", nm = "287_lollipop_che_bottom", desc = FALSE)
  )) {
    p <- tryCatch({
      yr <- .yr_max(master)
      d <- master[master$year == yr & is.finite(master[[info$col]]), ]
      d <- if (info$desc) utils::head(d[order(-d[[info$col]]), ], 20)
           else utils::head(d[order(d[[info$col]]), ], 20)
      d$country_name <- factor(d$country_name, levels = rev(d$country_name))
      ggplot2::ggplot(d, ggplot2::aes(.data[[info$col]], .data$country_name)) +
        ggplot2::geom_segment(ggplot2::aes(x = 0, xend = .data[[info$col]],
                                            y = .data$country_name, yend = .data$country_name),
                              color = "#5d667a", linewidth = 0.5) +
        ggplot2::geom_point(color = "#1d3f5f", size = 3.5) +
        .st() +
        ggplot2::labs(title = sprintf("%s 20 \u56fd (%d)", info$zh, yr),
                     x = info$col, y = NULL, caption = .sc(nrow(master[master$year == yr, ])))
    }, error = function(e) NULL)
    if (!is.null(p)) { save_fig(p, info$nm, dir = fig_dir); saved <- c(saved, info$nm) }
  }

  for (info in list(
    list(col = "hf3_che", zh = "OOPS", nm = "288_slope_oops"),
    list(col = "gghed_che", zh = "GGHE-D", nm = "289_slope_gghed")
  )) {
    p <- tryCatch({
      yr_a <- min(master$year, na.rm = TRUE); yr_b <- .yr_max(master)
      da <- master[master$year == yr_a & is.finite(master[[info$col]]),
                   c("iso3_code", "country_name", info$col)]
      db <- master[master$year == yr_b & is.finite(master[[info$col]]),
                   c("iso3_code", info$col)]
      names(da)[3] <- "start"; names(db)[2] <- "end"
      m <- merge(da, db, by = "iso3_code")
      m$change <- m$end - m$start
      top <- rbind(utils::head(m[order(-m$change), ], 8), utils::head(m[order(m$change), ], 8))
      long <- data.frame(
        country = rep(top$country_name, 2),
        year = rep(c(yr_a, yr_b), each = nrow(top)),
        value = c(top$start, top$end),
        stringsAsFactors = FALSE
      )
      ggplot2::ggplot(long, ggplot2::aes(.data$year, .data$value, group = .data$country)) +
        ggplot2::geom_line(alpha = 0.6, color = "#5d667a") +
        ggplot2::geom_point(ggplot2::aes(color = factor(.data$year)), size = 3) +
        ggplot2::scale_color_manual(values = c("#2a857a", "#c46327")) +
        ggrepel::geom_text_repel(
          data = long[long$year == yr_b, ],
          ggplot2::aes(label = .data$country), size = 3, nudge_x = 0.5, max.overlaps = 20) +
        .st() +
        ggplot2::labs(title = sprintf("%s \u53d8\u5316\u6700\u5927\u7684 16 \u56fd (%d\u2192%d)", info$zh, yr_a, yr_b),
                     x = NULL, y = info$col, color = "\u5e74\u4efd",
                     caption = .sc(nrow(m)))
    }, error = function(e) NULL)
    if (!is.null(p)) { save_fig(p, info$nm, width = 11, height = 8, dir = fig_dir); saved <- c(saved, info$nm) }
  }

  total_png <- length(list.files(fig_dir, pattern = "\\.png$"))
  cat(sprintf("[diverse] exported %d new figures. Total PNG: %d\n", length(saved), total_png))
  invisible(saved)
}
