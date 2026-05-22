
if (!exists("ensure_pkgs", mode = "function")) {
  source(file.path("\u7a0b\u5e8f", "00_utils.R"))
}


.has_pkg <- function(pkg) requireNamespace(pkg, quietly = TRUE)

.cap_news_map <- function(extra = NULL) {
  base <- "\u6570\u636e \u00b7 WHO GHED 2024 \u00b7 \u8fb9\u754c \u00b7 Natural Earth"
  if (is.null(extra) || !nzchar(extra)) base
  else paste0(base, " \u00b7 ", extra)
}

join_master_to_sf <- function(master, world_sf, var, year) {
  ensure_pkgs(c("sf"))
  m <- master[master$year == year, c("iso3_code", "country_name", var)]
  names(m)[3] <- "value"
  out <- merge(world_sf, m, by = "iso3_code", all.x = TRUE)
  out
}

.maybe_robinson <- function(sf_obj) {
  ensure_pkgs(c("sf"))
  res <- tryCatch(sf::st_transform(sf_obj, "+proj=robin"),
                  error = function(e) sf_obj)
  res
}

.theme_map <- function(base_size = 12) {
  ensure_pkgs(c("ggplot2"))
  theme_ghs3(base_size = base_size, grid = FALSE) +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      legend.position = "right",
      legend.title = ggplot2::element_text(size = 10, face = "bold"),
      legend.key.width = ggplot2::unit(12, "points"),
      legend.key.height = ggplot2::unit(48, "points"))
}


plot_map_world_var <- function(master, world_sf,
                                 var = "che_pc_usd2023",
                                 year = NULL,
                                 palette = "ember",
                                 trans = "identity",
                                 title = NULL,
                                 subtitle = NULL,
                                 legend_label = NULL,
                                 caption_extra = NULL) {
  ensure_pkgs(c("ggplot2", "sf"))
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  sfd <- join_master_to_sf(master, world_sf, var, year)
  sfd <- .maybe_robinson(sfd)
  ggplot2::ggplot(sfd) +
    ggplot2::geom_sf(ggplot2::aes(fill = .data$value),
                      colour = "white", linewidth = 0.18) +
    scale_fill_ghs3_seq(palette = palette,
                         na.value = "#3a3f48",
                         trans = trans,
                         name = legend_label %||% var) +
    .theme_map() +
    labs_news(
      title = title %||% sprintf("%s \u00b7 %d", var, year),
      subtitle = subtitle %||%
        "\u6df1\u8272 = \u9ad8\u6307\u6807\uff0c\u7070\u8272 = \u65e0\u6570\u636e\uff1bRobinson \u6295\u5f71",
      caption = .cap_news_map(caption_extra))
}

plot_map_che_pc      <- function(master, world_sf, year = NULL)
  plot_map_world_var(master, world_sf, "che_pc_usd2023", year,
    palette = "ocean", trans = "log10",
    legend_label = "\u4eba\u5747 CHE\nlog10 USD2023",
    title = sprintf("\u4eba\u5747 \u533b\u7597\u603b\u652f\u51fa \u00b7 %d",
                    year %||% max(master$year, na.rm = TRUE)),
    subtitle = "log10 \u5c3a\u5ea6\uff1b\u4eba\u5747 CHE \u8de8\u8d8a 3 \u4e2a\u6570\u91cf\u7ea7")

plot_map_gghed       <- function(master, world_sf, year = NULL)
  plot_map_world_var(master, world_sf, "gghed_che", year,
    palette = "ocean",
    legend_label = "GGHED \u5360 CHE (%)",
    title = sprintf("\u653f\u5e9c\u536b\u751f\u652f\u51fa\u5360\u6bd4 \u00b7 %d",
                    year %||% max(master$year, na.rm = TRUE)),
    subtitle = "\u989c\u8272\u8d8a\u6df1 = \u653f\u5e9c\u4e3b\u5bfc")

plot_map_oops        <- function(master, world_sf, year = NULL)
  plot_map_world_var(master, world_sf, "hf3_che", year,
    palette = "ember",
    legend_label = "OOPS \u5360 CHE (%)",
    title = sprintf("\u81ea\u4ed8\u5360\u6bd4 \u00b7 %d",
                    year %||% max(master$year, na.rm = TRUE)),
    subtitle = "\u989c\u8272\u8d8a\u6df1 = OOPS \u8d1f\u62c5\u8d8a\u91cd")

plot_map_lifeexp     <- function(master, world_sf, year = NULL)
  plot_map_world_var(master, world_sf, "life_exp", year,
    palette = "earth",
    legend_label = "\u9884\u671f\u5bff\u547d\uff08\u5e74\uff09",
    title = sprintf("\u9884\u671f\u5bff\u547d \u00b7 %d",
                    year %||% max(master$year, na.rm = TRUE)),
    subtitle = "\u8c03\u9ad8\u53ef\u80fd\u8d8b\u8fd1 80 \u5e74\uff0c\u4f4e\u533a 55-65 \u5e74",
    caption_extra = "WDI life expectancy")

plot_map_u5mr        <- function(master, world_sf, year = NULL)
  plot_map_world_var(master, world_sf, "u5mr", year,
    palette = "ember", trans = "sqrt",
    legend_label = "U5MR \uff08\u4e2d\u540e\u4e94\u5c81\u5c0f\u5b69\u6b7b\u4ea1\u7387 /1000\uff09",
    title = sprintf("U5MR \u00b7 %d",
                    year %||% max(master$year, na.rm = TRUE)),
    subtitle = "sqrt \u8f74\u62c9\u51fa\u4f4e\u503c\u5dee\u5f02\uff1b\u989c\u8272\u8d8a\u6df1 = \u6b7b\u4ea1\u7387\u8d8a\u9ad8",
    caption_extra = "WDI under-5 mortality")


plot_map_continent <- function(master, world_sf,
                                continent = "Africa",
                                var = "che_pc_usd2023",
                                year = NULL,
                                palette = "ember", trans = "identity") {
  ensure_pkgs(c("ggplot2", "sf"))
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  sfd <- join_master_to_sf(master, world_sf, var, year)
  sub <- sfd[sfd$continent_sf == continent, ]
  if (!nrow(sub)) return(ggplot2::ggplot() + ggplot2::theme_void())
  ggplot2::ggplot(sub) +
    ggplot2::geom_sf(ggplot2::aes(fill = .data$value),
                      colour = "white", linewidth = 0.22) +
    scale_fill_ghs3_seq(palette = palette, trans = trans,
                         na.value = "#3a3f48", name = var) +
    .theme_map() +
    labs_news(
      title = sprintf("%s \u00b7 %s \u00b7 %d", continent, var, year),
      subtitle = "\u533a\u57df\u7f29\u653e\u00b7\u8be6\u67e5\u56fd\u4e0e\u56fd\u5dee\u5f02",
      caption = .cap_news_map())
}


.bivariate_class <- function(x, y) {
  qx <- stats::quantile(x, c(1/3, 2/3), na.rm = TRUE)
  qy <- stats::quantile(y, c(1/3, 2/3), na.rm = TRUE)
  xx <- ifelse(x <= qx[1], "A", ifelse(x <= qx[2], "B", "C"))
  yy <- ifelse(y <= qy[1], "1", ifelse(y <= qy[2], "2", "3"))
  out <- paste0(xx, yy)
  out[is.na(x) | is.na(y)] <- NA_character_
  out
}

.bivariate_palette <- c(
  A1 = "#e8e8e8", B1 = "#cae8d4", C1 = "#7ec38b",
  A2 = "#dac9e2", B2 = "#a4c4d4", C2 = "#4d8c8e",
  A3 = "#9c6fa1", B3 = "#6f7a9f", C3 = "#28556e"
)

plot_map_bivariate <- function(master, world_sf,
                                 x_var = "che_pc_usd2023",
                                 y_var = "life_exp",
                                 year = NULL,
                                 x_label = NULL,
                                 y_label = NULL,
                                 title = NULL,
                                 subtitle = NULL) {
  ensure_pkgs(c("ggplot2", "sf"))
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  base <- master[master$year == year,
                 c("iso3_code", x_var, y_var)]
  base$class <- .bivariate_class(base[[x_var]], base[[y_var]])
  sfd <- merge(world_sf, base, by = "iso3_code", all.x = TRUE)
  sfd <- .maybe_robinson(sfd)
  main <- ggplot2::ggplot(sfd) +
    ggplot2::geom_sf(ggplot2::aes(fill = .data$class),
                      colour = "white", linewidth = 0.18) +
    ggplot2::scale_fill_manual(values = .bivariate_palette,
                                na.value = "#3a3f48",
                                guide = "none") +
    .theme_map() +
    labs_news(
      title = title %||% sprintf("Bivariate \u00b7 %s \u00d7 %s \u00b7 %d",
                                  x_var, y_var, year),
      subtitle = subtitle %||% "3\u00d73 \u989c\u8272\u77e9\u9635\u00b7\u4f4e\u4e2d\u9ad8\u5404\u4e09\u5206\u4f4d",
      caption = .cap_news_map("self-implemented 3x3 bivariate"))

  leg_df <- data.frame(
    x = rep(c("A", "B", "C"), 3),
    y = rep(c("1", "2", "3"), each = 3),
    cls = paste0(rep(c("A", "B", "C"), 3),
                 rep(c("1", "2", "3"), each = 3)))
  leg <- ggplot2::ggplot(leg_df, ggplot2::aes(
      x = .data$x, y = .data$y, fill = .data$cls)) +
    ggplot2::geom_tile(colour = "white", linewidth = 1.4) +
    ggplot2::scale_fill_manual(values = .bivariate_palette,
                                guide = "none") +
    ggplot2::scale_x_discrete(
      labels = c("A" = "\u4f4e", "B" = "\u4e2d", "C" = "\u9ad8")) +
    ggplot2::scale_y_discrete(
      labels = c("1" = "\u4f4e", "2" = "\u4e2d", "3" = "\u9ad8")) +
    theme_ghs3(base_size = 9, grid = FALSE) +
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = 9, face = "bold"),
      axis.text = ggplot2::element_text(size = 8),
      plot.background = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank()) +
    ggplot2::labs(x = x_label %||% x_var,
                   y = y_label %||% y_var)

  if (.has_pkg("cowplot")) {
    cowplot::ggdraw(main) +
      cowplot::draw_plot(leg, x = 0.02, y = 0.04,
                          width = 0.18, height = 0.22)
  } else {
    main
  }
}

plot_map_bivariate_che_life <- function(master, world_sf, year = NULL)
  plot_map_bivariate(master, world_sf, "che_pc_usd2023", "life_exp", year,
    x_label = "\u4eba\u5747 CHE", y_label = "\u9884\u671f\u5bff\u547d",
    title = sprintf("\u4eba\u5747 CHE \u00d7 \u9884\u671f\u5bff\u547d \u00b7 %d",
                    year %||% max(master$year, na.rm = TRUE)),
    subtitle = "\u5de6\u4e0b\uff1a\u4f4e\u6536\u5165\u4f4e\u5bff\u547d \u00b7 \u53f3\u4e0a\uff1a\u9ad8\u6536\u5165\u9ad8\u5bff\u547d")

plot_map_bivariate_gghed_oops <- function(master, world_sf, year = NULL)
  plot_map_bivariate(master, world_sf, "gghed_che", "hf3_che", year,
    x_label = "GGHED", y_label = "OOPS",
    title = sprintf("GGHED \u00d7 OOPS \u00b7 %d",
                    year %||% max(master$year, na.rm = TRUE)),
    subtitle = "\u5de6\u4e0a\uff1a\u4f4e GGHED + \u9ad8 OOPS = \u8d22\u52a1\u4fdd\u62a4\u9ad8\u538b\u533a")

plot_map_bivariate_gghed_u5mr <- function(master, world_sf, year = NULL)
  plot_map_bivariate(master, world_sf, "gghed_che", "u5mr", year,
    x_label = "GGHED", y_label = "U5MR",
    title = sprintf("GGHED \u00d7 U5MR \u00b7 %d",
                    year %||% max(master$year, na.rm = TRUE)),
    subtitle = "\u53f3\u4e0a\uff1a\u9ad8 GGHED \u4f46 U5MR \u4e0d\u964d \u00b7 \u6709\u6548\u6027\u95ee\u9898")


plot_map_change <- function(master, world_sf,
                              var = "che_pc_usd2023",
                              y1 = NULL, y2 = NULL,
                              pct_change = FALSE,
                              title = NULL,
                              subtitle = NULL,
                              legend_label = NULL) {
  ensure_pkgs(c("ggplot2", "sf"))
  if (is.null(y1)) y1 <- min(master$year, na.rm = TRUE)
  if (is.null(y2)) y2 <- max(master$year, na.rm = TRUE)
  a <- master[master$year == y1, c("iso3_code", var)]
  b <- master[master$year == y2, c("iso3_code", var)]
  names(a)[2] <- "v1"; names(b)[2] <- "v2"
  m <- merge(a, b, by = "iso3_code")
  m$delta <- if (pct_change) (m$v2 / m$v1) - 1 else m$v2 - m$v1
  m$delta[!is.finite(m$delta)] <- NA_real_
  sfd <- merge(world_sf, m[, c("iso3_code", "delta")],
               by = "iso3_code", all.x = TRUE)
  sfd <- .maybe_robinson(sfd)

  fill_scale <- if (pct_change) {
    scale_fill_ghs3_div(midpoint = 0,
      labels = scales::percent_format(accuracy = 1),
      na.value = "#3a3f48",
      name = legend_label %||% sprintf("\u00d7%d %% \u589e\u51cf", y2-y1))
  } else {
    scale_fill_ghs3_div(midpoint = 0, na.value = "#3a3f48",
      name = legend_label %||% sprintf("\u0394%s", var))
  }

  ggplot2::ggplot(sfd) +
    ggplot2::geom_sf(ggplot2::aes(fill = .data$delta),
                      colour = "white", linewidth = 0.18) +
    fill_scale +
    .theme_map() +
    labs_news(
      title = title %||% sprintf("\u0394%s \u00b7 %d \u2192 %d",
                                  var, y1, y2),
      subtitle = subtitle %||%
        "\u8d77\u70b9-\u7ec8\u70b9\u53d8\u5316\uff1b\u84dd\u7eff\u8868\u6539\u5584\uff0c\u7c89\u7ea2\u8868\u9000\u6b65",
      caption = .cap_news_map())
}

plot_map_che_pc_change <- function(master, world_sf, y1 = NULL, y2 = NULL)
  plot_map_change(master, world_sf, "che_pc_usd2023", y1, y2,
    pct_change = TRUE,
    title = sprintf("\u4eba\u5747 CHE \u589e\u957f\u7387 \u00b7 %d \u2192 %d",
                    y1 %||% min(master$year, na.rm = TRUE),
                    y2 %||% max(master$year, na.rm = TRUE)),
    subtitle = "%\u589e\u957f\uff1b\u7eff\u84dd = \u589e\u957f\uff0c\u7c89\u7ea2 = \u4e0b\u964d")

plot_map_oops_change <- function(master, world_sf, y1 = NULL, y2 = NULL)
  plot_map_change(master, world_sf, "hf3_che", y1, y2,
    pct_change = FALSE,
    title = sprintf("OOPS \u5360\u6bd4\u53d8\u5316 \u00b7 %d \u2192 %d",
                    y1 %||% min(master$year, na.rm = TRUE),
                    y2 %||% max(master$year, na.rm = TRUE)),
    subtitle = "\u767e\u5206\u70b9\u53d8\u5316\uff1b\u84dd = OOPS \u4e0b\u964d (\u4fdd\u62a4\u6539\u5584)")

plot_map_lifeexp_change <- function(master, world_sf, y1 = NULL, y2 = NULL)
  plot_map_change(master, world_sf, "life_exp", y1, y2,
    pct_change = FALSE,
    title = sprintf("\u9884\u671f\u5bff\u547d\u53d8\u5316 \u00b7 %d \u2192 %d",
                    y1 %||% min(master$year, na.rm = TRUE),
                    y2 %||% max(master$year, na.rm = TRUE)),
    subtitle = "\u5e74\u4efd\u53d8\u5316\uff1b\u84dd\u7eff = \u4ef0\u67d3\u9971\u4e0a")


plot_map_quintile <- function(master, world_sf,
                                var = "che_pc_usd2023",
                                year = NULL,
                                palette = "ember",
                                title = NULL, subtitle = NULL) {
  ensure_pkgs(c("ggplot2", "sf"))
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  base <- master[master$year == year, c("iso3_code", var)]
  vals <- base[[var]]
  brks <- stats::quantile(vals, c(0, 0.2, 0.4, 0.6, 0.8, 1),
                          na.rm = TRUE)
  brks <- unique(brks)
  if (length(brks) < 3) {
    return(ggplot2::ggplot() + ggplot2::theme_void() +
             labs_news(title = "\u6570\u636e\u4e0d\u8db3\u4ee5\u5206\u4e94\u5206\u4f4d"))
  }
  base$bin <- cut(vals, brks, include.lowest = TRUE,
                  labels = paste0("Q", seq_len(length(brks) - 1)))
  sfd <- merge(world_sf, base[, c("iso3_code", "bin")],
                by = "iso3_code", all.x = TRUE)
  sfd <- .maybe_robinson(sfd)
  fill_vals <- palette_ghs3_sequential(length(brks) - 1, palette)
  ggplot2::ggplot(sfd) +
    ggplot2::geom_sf(ggplot2::aes(fill = .data$bin),
                      colour = "white", linewidth = 0.18) +
    ggplot2::scale_fill_manual(values = fill_vals,
                                na.value = "#3a3f48", name = var,
                                drop = FALSE) +
    .theme_map() +
    labs_news(
      title = title %||% sprintf("%s \u00b7 \u4e94\u5206\u4f4d %d", var, year),
      subtitle = subtitle %||% "Q1 = \u6700\u4f4e 20%\uff0cQ5 = \u6700\u9ad8 20%",
      caption = .cap_news_map())
}


plot_map_bubble <- function(master, world_sf,
                              var = "che_usd2023",
                              year = NULL,
                              palette = "ocean",
                              max_radius = 14,
                              title = NULL, subtitle = NULL,
                              legend_label = NULL) {
  ensure_pkgs(c("ggplot2", "sf"))
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  base <- master[master$year == year, c("iso3_code", var)]
  sfd <- merge(world_sf, base, by = "iso3_code", all.x = TRUE)
  ctr <- suppressWarnings(sf::st_centroid(sf::st_geometry(sfd)))
  ctr_pts <- sf::st_coordinates(ctr)
  pts <- data.frame(lon = ctr_pts[, 1], lat = ctr_pts[, 2],
                    value = sfd[[var]])
  pts <- pts[is.finite(pts$value) & pts$value > 0, ]
  if (!nrow(pts)) return(ggplot2::ggplot() + ggplot2::theme_void())
  sfd <- .maybe_robinson(sfd)

  ggplot2::ggplot() +
    ggplot2::geom_sf(data = sfd,
      fill = "#1e252f", colour = "#3a4250", linewidth = 0.14) +
    ggplot2::geom_point(data = pts,
      ggplot2::aes(x = .data$lon, y = .data$lat,
                    size = .data$value,
                    colour = .data$value),
      alpha = 0.85, stroke = 0.4) +
    ggplot2::coord_sf(crs = "+proj=robin", default = TRUE) +
    ggplot2::scale_size_area(max_size = max_radius,
      labels = function(v) paste0("$", format(v / 1e9,
                                                big.mark = ","), "B"),
      name = legend_label %||% var) +
    scale_colour_ghs3_seq(palette = palette,
                           guide = "none",
                           trans = "log10") +
    .theme_map() +
    labs_news(
      title = title %||% sprintf("%s \u00b7 \u6bd4\u4f8b\u6c14\u6ce1 %d", var, year),
      subtitle = subtitle %||%
        "\u6c14\u6ce1\u9762\u79ef\u4ee3\u8868\u603b\u989d\uff1b\u989c\u8272\u8d8a\u6df1\u8868\u793a log \u8d8a\u9ad8",
      caption = .cap_news_map())
}


plot_map_smallmultiples <- function(master, world_sf,
                                       var = "che_pc_usd2023",
                                       years = c(2000, 2008, 2014,
                                                  max(master$year)),
                                       palette = "ember",
                                       trans = "log10") {
  ensure_pkgs(c("ggplot2", "sf"))
  data_list <- lapply(years, function(y) {
    s <- join_master_to_sf(master, world_sf, var, y)
    s$.year <- y
    s
  })
  big <- do.call(rbind, data_list)
  big <- .maybe_robinson(big)
  big$.year <- factor(big$.year, levels = years)
  ggplot2::ggplot(big) +
    ggplot2::geom_sf(ggplot2::aes(fill = .data$value),
                      colour = "white", linewidth = 0.12) +
    ggplot2::facet_wrap(~ .data$.year, ncol = 2) +
    scale_fill_ghs3_seq(palette = palette, trans = trans,
                         na.value = "#3a3f48", name = var) +
    .theme_map() +
    ggplot2::theme(
      strip.text = ggplot2::element_text(face = "bold", size = 13),
      panel.spacing = ggplot2::unit(14, "points")) +
    labs_news(
      title = sprintf("%s \u00b7 4 \u4e2a\u65f6\u70b9", var),
      subtitle = paste(years, collapse = "  \u00b7  "),
      caption = .cap_news_map())
}


ghs_export_maps_advanced <- function(master = NULL, world_sf = NULL,
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
    if (!file.exists(cache)) stop("Missing world_sf_medium.rds")
    world_sf <- readRDS(cache)
  }
  if (is.null(fig_dir)) {
    fig_dir <- file.path(proj_root(), "\u5206\u6790\u8f93\u51fa",
                          "\u56fe\u8868")
  }
  dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

  plots <- list(
    map_world_che_pc            = plot_map_che_pc(master, world_sf),
    map_world_gghed             = plot_map_gghed(master, world_sf),
    map_world_oops              = plot_map_oops(master, world_sf),
    map_world_lifeexp           = plot_map_lifeexp(master, world_sf),
    map_world_u5mr              = plot_map_u5mr(master, world_sf),
    map_africa_che_pc           = plot_map_continent(master, world_sf,
                                                       "Africa", "che_pc_usd2023",
                                                       trans = "log10"),
    map_africa_gghed            = plot_map_continent(master, world_sf,
                                                       "Africa", "gghed_che",
                                                       palette = "ocean"),
    map_asia_che_pc             = plot_map_continent(master, world_sf,
                                                       "Asia", "che_pc_usd2023",
                                                       trans = "log10"),
    map_asia_oops               = plot_map_continent(master, world_sf,
                                                       "Asia", "hf3_che"),
    map_europe_che_pc           = plot_map_continent(master, world_sf,
                                                       "Europe", "che_pc_usd2023",
                                                       trans = "log10",
                                                       palette = "ocean"),
    map_americas_che_pc         = plot_map_continent(master, world_sf,
                                                       "Americas",
                                                       "che_pc_usd2023",
                                                       trans = "log10",
                                                       palette = "ember"),
    map_oceania_che_pc          = plot_map_continent(master, world_sf,
                                                       "Oceania",
                                                       "che_pc_usd2023",
                                                       trans = "log10"),
    map_bivariate_che_life      = plot_map_bivariate_che_life(master, world_sf),
    map_bivariate_gghed_oops    = plot_map_bivariate_gghed_oops(master, world_sf),
    map_bivariate_gghed_u5mr    = plot_map_bivariate_gghed_u5mr(master, world_sf),
    map_change_che_pc           = plot_map_che_pc_change(master, world_sf),
    map_change_oops             = plot_map_oops_change(master, world_sf),
    map_change_lifeexp          = plot_map_lifeexp_change(master, world_sf),
    map_quintile_che_pc         = plot_map_quintile(master, world_sf,
                                                     "che_pc_usd2023",
                                                     palette = "ember"),
    map_quintile_gghed          = plot_map_quintile(master, world_sf,
                                                     "gghed_che",
                                                     palette = "ocean"),
    map_quintile_oops           = plot_map_quintile(master, world_sf,
                                                     "hf3_che",
                                                     palette = "ember"),
    map_quintile_lifeexp        = plot_map_quintile(master, world_sf,
                                                     "life_exp",
                                                     palette = "earth"),
    map_bubble_che_total        = plot_map_bubble(master, world_sf,
                                                    "che_usd2023",
                                                    palette = "ocean"),
    map_bubble_oops_total       = plot_map_bubble(master, world_sf,
                                                    "hf3_usd2023",
                                                    palette = "ember",
                                                    legend_label = "OOPS \u603b\u989d"),
    map_sm_che_pc               = plot_map_smallmultiples(master, world_sf,
                                                            "che_pc_usd2023",
                                                            palette = "ocean",
                                                            trans = "log10"),
    map_sm_oops                 = plot_map_smallmultiples(master, world_sf,
                                                            "hf3_che",
                                                            palette = "ember"),
    map_sm_gghed                = plot_map_smallmultiples(master, world_sf,
                                                            "gghed_che",
                                                            palette = "ocean")
  )

  out <- character(0)
  for (nm in names(plots)) {
    p <- plots[[nm]]
    if (is.null(p)) next
    res <- tryCatch({
      png_path <- file.path(fig_dir, paste0(nm, ".png"))
      svg_path <- file.path(fig_dir, paste0(nm, ".svg"))
      ggplot2::ggsave(png_path, p, width = 11, height = 6.5, dpi = 160,
                      bg = brand_palette$paper)
      ggplot2::ggsave(svg_path, p, width = 11, height = 6.5,
                      bg = brand_palette$paper)
      out <<- c(out, png_path, svg_path)
      TRUE
    }, error = function(e) {
      message("[map export] ", nm, " failed: ", conditionMessage(e))
      FALSE
    })
  }
  invisible(out)
}
