
plot_v2_oops_small_multiples <- function(master,
                                          n_year_step = 1,
                                          year_min = 2000,
                                          year_max = 2023,
                                          world_sf = NULL) {
  if (is.null(world_sf)) {
    world_sf <- tryCatch(load_world_sf("medium", 0.08), error = function(e) NULL)
  }
  if (is.null(world_sf)) return(ggplot2::ggplot())
  if (!"hf3_che" %in% names(master)) return(ggplot2::ggplot())

  yrs <- seq(year_min, year_max, by = n_year_step)
  d <- master[master$year %in% yrs &
              is.finite(master$hf3_che),
              c("iso3_code", "year", "hf3_che")]
  if (nrow(d) == 0) return(ggplot2::ggplot())

  joined <- merge(world_sf, d, by = "iso3_code", all.y = TRUE)
  if (!inherits(joined, "sf"))
    joined <- sf::st_as_sf(joined)

  ggplot2::ggplot(joined) +
    ggplot2::geom_sf(ggplot2::aes(fill = hf3_che),
                     colour = NA, linewidth = 0) +
    ggplot2::scale_fill_distiller(
      palette = "RdYlBu", direction = -1, na.value = "grey90",
      limits = c(0, 80), oob = scales::squish,
      name = "OOPS / CHE  (%)") +
    ggplot2::facet_wrap(~year, ncol = 6) +
    ggplot2::coord_sf(crs = sf::st_crs(4326), expand = FALSE) +
    labs_news(
      title    = "OOPS \u4e16\u754c\u5730\u56fe\u00b7\u5c0f\u591a\u9762\u00b7%d\u2013%d",
      subtitle = "\u989c\u8272\u8d8a\u7ea2\u00b7\u5c45\u6c11\u81ea\u4ed8\u8d1f\u62c5\u8d8a\u91cd",
      x = NULL, y = NULL
    ) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.key.width = grid::unit(2, "cm"),
      strip.text = ggplot2::element_text(face = "bold"),
      panel.grid = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    )
}

plot_v2_bivariate_oops_gov <- function(master, year = 2022, world_sf = NULL) {
  if (is.null(world_sf)) {
    world_sf <- tryCatch(load_world_sf("medium", 0.08), error = function(e) NULL)
  }
  if (is.null(world_sf)) return(ggplot2::ggplot())
  if (!all(c("hf3_che", "gghed_che") %in% names(master))) return(ggplot2::ggplot())

  d <- master[master$year == year &
              is.finite(master$hf3_che) &
              is.finite(master$gghed_che),
              c("iso3_code", "hf3_che", "gghed_che")]
  if (nrow(d) == 0) return(ggplot2::ggplot())

  d$oops_t <- cut(d$hf3_che, breaks = stats::quantile(d$hf3_che,
                                                       c(0, .33, .67, 1),
                                                       na.rm = TRUE),
                   labels = c("L", "M", "H"), include.lowest = TRUE)
  d$gov_t  <- cut(d$gghed_che, breaks = stats::quantile(d$gghed_che,
                                                         c(0, .33, .67, 1),
                                                         na.rm = TRUE),
                   labels = c("L", "M", "H"), include.lowest = TRUE)
  d$bicode <- paste0("OOPS-", d$oops_t, "/Gov-", d$gov_t)

  pal <- c(
    "OOPS-L/Gov-L" = "#e8e8e8", "OOPS-M/Gov-L" = "#dfb0d6", "OOPS-H/Gov-L" = "#be64ac",
    "OOPS-L/Gov-M" = "#ace4e4", "OOPS-M/Gov-M" = "#a5add3", "OOPS-H/Gov-M" = "#8c62aa",
    "OOPS-L/Gov-H" = "#5ac8c8", "OOPS-M/Gov-H" = "#5698b9", "OOPS-H/Gov-H" = "#3b4994"
  )

  joined <- merge(world_sf, d, by = "iso3_code", all.y = TRUE)
  if (!inherits(joined, "sf"))
    joined <- sf::st_as_sf(joined)

  ggplot2::ggplot(joined) +
    ggplot2::geom_sf(ggplot2::aes(fill = bicode),
                     colour = "white", linewidth = 0.05) +
    ggplot2::scale_fill_manual(values = pal, na.value = "grey92",
                               name = "OOPS \u00d7 \u653f\u5e9c") +
    ggplot2::coord_sf(crs = sf::st_crs(4326)) +
    labs_news(
      title    = "\u53cc\u53d8\u91cf\u5730\u56fe\u00b7OOPS \u00d7 \u653f\u5e9c\u4efd\u989d",
      subtitle = sprintf("%d\u5e74\u00b7\u5de6\u4e0b=\u90fd\u4f4e\u00b7\u53f3\u4e0a=\u90fd\u9ad8", year),
      x = NULL, y = NULL
    ) +
    ggplot2::theme(
      legend.position = "right",
      panel.grid = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    )
}

plot_v2_covid_dumbbell <- function(master, n_top = 30) {
  if (!"hf3_che" %in% names(master)) return(ggplot2::ggplot())
  d19 <- master[master$year == 2019 & is.finite(master$hf3_che),
                c("iso3_code", "country_name", "continent", "hf3_che")]
  d22 <- master[master$year == 2022 & is.finite(master$hf3_che),
                c("iso3_code", "hf3_che")]
  names(d19)[4] <- "y2019"
  names(d22)[2] <- "y2022"
  d <- merge(d19, d22, by = "iso3_code")
  d$delta <- d$y2022 - d$y2019
  d <- d[order(-abs(d$delta)), ][seq_len(min(n_top, nrow(d))), ]
  d$country_name <- factor(d$country_name,
                           levels = rev(d$country_name[order(-abs(d$delta))]))

  d$dir <- ifelse(d$delta > 0, "up", "down")

  ggplot2::ggplot(d) +
    ggplot2::geom_segment(ggplot2::aes(x = y2019, xend = y2022,
                                        y = country_name, yend = country_name,
                                        colour = dir),
                          linewidth = 1) +
    ggplot2::geom_point(ggplot2::aes(x = y2019, y = country_name),
                        size = 3, colour = brand_palette$muted) +
    ggplot2::geom_point(ggplot2::aes(x = y2022, y = country_name),
                        size = 3, colour = brand_palette$pvtd) +
    ggplot2::scale_colour_manual(
      values = c(up = brand_palette$pvtd, down = brand_palette$ext),
      labels = c(up = "OOPS \u4e0a\u5347", down = "OOPS \u4e0b\u964d"),
      name = NULL) +
    labs_news(
      title    = "\u75ab\u60c5\u524d\u540e\u7684 OOPS \u53d8\u5316\u00b7Top %d",
      subtitle = "2019 \u2022 \u2192 2022 \u2022 \u00b7 \u6a59\u7ea2=\u6076\u5316\u00b7\u6a44\u7eff=\u6539\u5584",
      x = "OOPS / CHE  (%)", y = NULL
    ) +
    ggplot2::theme(legend.position = "top")
}

plot_v2_slope_oops <- function(master, n_show = 25) {
  if (!"hf3_che" %in% names(master)) return(ggplot2::ggplot())
  d00 <- master[master$year == 2000 & is.finite(master$hf3_che),
                c("iso3_code", "country_name", "continent", "hf3_che")]
  d23 <- master[master$year == 2023 & is.finite(master$hf3_che),
                c("iso3_code", "hf3_che")]
  names(d00)[4] <- "y00"
  names(d23)[2] <- "y23"
  d <- merge(d00, d23, by = "iso3_code")
  d$delta <- d$y23 - d$y00
  d <- d[order(-abs(d$delta)), ][seq_len(min(n_show, nrow(d))), ]

  long <- rbind(
    data.frame(country = d$country_name, continent = d$continent,
               year = 2000, value = d$y00, stringsAsFactors = FALSE),
    data.frame(country = d$country_name, continent = d$continent,
               year = 2023, value = d$y23, stringsAsFactors = FALSE)
  )

  ggplot2::ggplot(long, ggplot2::aes(year, value, group = country,
                                      colour = continent)) +
    ggplot2::geom_line(linewidth = 0.8, alpha = 0.7) +
    ggplot2::geom_point(size = 2.2) +
    ggrepel::geom_text_repel(
      data = long[long$year == 2023, ],
      ggplot2::aes(label = country),
      hjust = 0, nudge_x = 0.4, direction = "y", size = 2.6,
      family = "sans", segment.size = 0.2, max.overlaps = 25
    ) +
    scale_colour_brand_continent(name = NULL) +
    ggplot2::scale_x_continuous(breaks = c(2000, 2023),
                                limits = c(1998, 2030)) +
    labs_news(
      title    = "\u53d8\u5316\u6700\u5267\u70c8\u7684 25 \u56fd\u00b7OOPS \u8d70\u52bf",
      subtitle = "\u4e24\u70b9\u4e00\u7ebf\u00b72000 \u2192 2023",
      x = NULL, y = "OOPS / CHE  (%)"
    ) +
    ggplot2::theme(legend.position = "top")
}

plot_v2_continent_stream <- function(master) {
  d <- master[is.finite(master$gghed_usd2023) &
              is.finite(master$pvtd_usd2023) &
              is.finite(master$ext_usd2023) &
              !is.na(master$continent), ]
  if (nrow(d) == 0) return(ggplot2::ggplot())

  agg <- d |>
    dplyr::group_by(continent, year) |>
    dplyr::summarise(
      Government = sum(gghed_usd2023, na.rm = TRUE) / 1e9,
      Private    = sum(pvtd_usd2023,  na.rm = TRUE) / 1e9,
      External   = sum(ext_usd2023,   na.rm = TRUE) / 1e9,
      .groups = "drop"
    ) |>
    tidyr::pivot_longer(c("Government", "Private", "External"),
                         names_to = "source", values_to = "value")

  ggplot2::ggplot(agg, ggplot2::aes(year, value, fill = source)) +
    ggplot2::geom_area(position = "stack", alpha = 0.85) +
    ggplot2::facet_wrap(~continent, scales = "free_y", ncol = 3) +
    scale_fill_brand_source(name = NULL) +
    ggplot2::scale_y_continuous(labels = function(x) paste0(x, " B")) +
    labs_news(
      title    = "\u4e09\u6e90\u00b7\u603b\u989d\u00b7\u5927\u6d32\u5206\u9762",
      subtitle = "USD 2023 \u00b7 stack area \u00b7 \u4e1c\u4e9a/\u5317\u7f8e\u91cf\u7ea7\u5dee\u5f02\u660e\u663e",
      x = NULL, y = "Total spending (USD bn 2023)"
    ) +
    ggplot2::theme(legend.position = "top")
}

plot_v2_country_compare_hc <- function(master,
                                        countries = c("USA", "CHN", "KEN")) {
  if (!all(c("hc1_che", "hc6_che") %in% names(master)))
    return(ggplot2::ggplot())
  d <- master[master$iso3_code %in% countries &
              !is.na(master$hc1_che) &
              !is.na(master$hc6_che) &
              master$year >= 2016, ]
  if (nrow(d) == 0) return(ggplot2::ggplot())

  d$ratio <- d$hc6_che / pmax(d$hc1_che, 1e-6)

  unames <- as.character(unique(d$country_name))
  pal <- c(brand_palette$gghed, brand_palette$pvtd, brand_palette$ext,
           brand_palette$africa, brand_palette$asia, brand_palette$europe)
  pal <- pal[seq_along(unames)]
  scale_vals <- stats::setNames(pal, unames)

  ggplot2::ggplot(d, ggplot2::aes(year, ratio,
                                   colour = country_name,
                                   group = country_name)) +
    ggplot2::geom_line(linewidth = 1.1) +
    ggplot2::geom_point(size = 2.6) +
    ggrepel::geom_text_repel(
      data = d |> dplyr::group_by(country_name) |>
                  dplyr::slice_max(year, n = 1) |>
                  dplyr::ungroup(),
      ggplot2::aes(label = country_name),
      hjust = 0, nudge_x = 0.3, direction = "y",
      size = 3.2, family = "sans"
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(0.1, scale = 1)) +
    ggplot2::scale_colour_manual(values = scale_vals, guide = "none") +
    labs_news(
      title    = "\u9884\u9632\uff1a\u6cbb\u7597 \u00b7 \u4e09\u56fd\u5bf9\u6bd4",
      subtitle = "hc6 / hc1\u00b7\u4f4e\u4e8e 0.1 \u8868\u793a\u4e25\u91cd\u504f\u91cd\u6cbb\u7597",
      x = NULL, y = "hc6 / hc1"
    )
}


ghs_export_v2_dataviz <- function(master,
                                   world_sf = NULL,
                                   out_dir = file.path("分析输出", "图表"),
                                   verbose = TRUE) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  if (is.null(world_sf)) {
    world_sf <- tryCatch(load_world_sf("medium", 0.08), error = function(e) NULL)
  }

  jobs <- list(
    list(id = "v2_oops_small_multiples",
         fn = function() plot_v2_oops_small_multiples(master, n_year_step = 4, world_sf = world_sf),
         w = 14, h = 8),
    list(id = "v2_bivariate_oops_gov",
         fn = function() plot_v2_bivariate_oops_gov(master, world_sf = world_sf),
         w = 12, h = 6),
    list(id = "v2_covid_dumbbell",
         fn = function() plot_v2_covid_dumbbell(master),
         w = 9, h = 9),
    list(id = "v2_slope_oops",
         fn = function() plot_v2_slope_oops(master),
         w = 9, h = 8),
    list(id = "v2_continent_stream",
         fn = function() plot_v2_continent_stream(master),
         w = 12, h = 6),
    list(id = "v2_country_compare_hc",
         fn = function() plot_v2_country_compare_hc(master),
         w = 9, h = 5)
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
      if (verbose) cat("[v2_dataviz]", j$id, "saved\n")
      ok <- ok + 1
    }
  }
  invisible(ok)
}
