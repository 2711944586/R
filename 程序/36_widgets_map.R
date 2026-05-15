# =============================================================================
# 程序/36_widgets_map.R   —— 交互式地图 widget（C1 阶段）
# -----------------------------------------------------------------------------
# 20 个 imap_* 函数（leaflet / plotly choropleth / mapbox 风格）
# 命名前缀：imap_
# =============================================================================

if (!exists("ensure_pkgs", mode = "function")) {
  source(file.path("\u7a0b\u5e8f", "00_utils.R"))
}

.has_pkg <- function(pkg) requireNamespace(pkg, quietly = TRUE)

.imap_palette <- function(palette = "default", n = 9) {
  if (exists("palette_ghs3_sequential", mode = "function")) {
    palette_ghs3_sequential(n, palette)
  } else {
    grDevices::colorRampPalette(c("#fbf6ee", "#1d3f5f"))(n)
  }
}

.imap_join_geom <- function(master, world_sf, year, var) {
  d <- master[master$year == year, c("iso3_code", "country_name",
                                        "continent", var)]
  out <- merge(world_sf, d, by = "iso3_code", all.x = TRUE)
  out
}

.imap_label_html <- function(name, iso, var, val,
                              fmt = function(x) sprintf("%.2f", x)) {
  ensure_pkgs("htmltools")
  paste0("<strong>", name, "</strong> (", iso, ")<br>",
         "<span style='font-size:11px'>", var, " = ", fmt(val), "</span>")
}

# =============================================================================
# 1-5 · leaflet 单指标 choropleth（5 个指标）
# =============================================================================

#' imap1 \u4eba\u5747 CHE leaflet
imap_che_pc <- function(master, world_sf, year = NULL) {
  if (!.has_pkg("leaflet") || !.has_pkg("sf")) return(NULL)
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  g <- .imap_join_geom(master, world_sf, year, "che_pc_usd2023")
  vals <- g$che_pc_usd2023
  vals[!is.finite(vals)] <- NA
  pal <- leaflet::colorNumeric(palette = .imap_palette("earth", 9),
                                 domain = log10(pmax(vals, 1, na.rm = TRUE)),
                                 na.color = "#cccccc")
  labs <- vapply(seq_len(nrow(g)), function(i) .imap_label_html(
    g$country_name[i], g$iso3_code[i], "USD/cap",
    g$che_pc_usd2023[i], scales::dollar_format(accuracy = 1)),
    character(1))
  leaflet::leaflet(g, options = leaflet::leafletOptions(zoomSnap = 0.25)) |>
    leaflet::addProviderTiles("CartoDB.Positron") |>
    leaflet::addPolygons(
      fillColor = ~pal(log10(pmax(che_pc_usd2023, 1))),
      weight = 0.5, color = "#1a1f28", fillOpacity = 0.85,
      label = lapply(labs, htmltools::HTML),
      highlightOptions = leaflet::highlightOptions(weight = 2,
        color = "#c46327", bringToFront = TRUE)) |>
    leaflet::addLegend(pal = pal,
      values = log10(pmax(vals, 1, na.rm = TRUE)),
      title = sprintf("log10 \u4eba\u5747 CHE \u00b7 %d", year),
      position = "bottomright")
}

#' imap2 OOP \u5360 CHE leaflet
imap_oop <- function(master, world_sf, year = NULL) {
  if (!.has_pkg("leaflet") || !.has_pkg("sf")) return(NULL)
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  g <- .imap_join_geom(master, world_sf, year, "hf3_che")
  vals <- g$hf3_che; vals[!is.finite(vals)] <- NA
  pal <- leaflet::colorNumeric(palette = .imap_palette("ember", 9),
                                 domain = vals, na.color = "#cccccc")
  labs <- vapply(seq_len(nrow(g)), function(i) .imap_label_html(
    g$country_name[i], g$iso3_code[i], "OOP/CHE",
    g$hf3_che[i], function(x) sprintf("%.1f%%", x)),
    character(1))
  leaflet::leaflet(g) |>
    leaflet::addProviderTiles("CartoDB.Positron") |>
    leaflet::addPolygons(
      fillColor = ~pal(hf3_che),
      weight = 0.5, color = "#1a1f28", fillOpacity = 0.85,
      label = lapply(labs, htmltools::HTML),
      highlightOptions = leaflet::highlightOptions(weight = 2,
        color = "#c46327", bringToFront = TRUE)) |>
    leaflet::addLegend(pal = pal, values = vals,
      title = sprintf("OOP / CHE \u00b7 %d", year),
      labFormat = leaflet::labelFormat(suffix = "%"),
      position = "bottomright")
}

#' imap3 GGHED leaflet
imap_gghed <- function(master, world_sf, year = NULL) {
  if (!.has_pkg("leaflet") || !.has_pkg("sf")) return(NULL)
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  g <- .imap_join_geom(master, world_sf, year, "gghed_che")
  vals <- g$gghed_che; vals[!is.finite(vals)] <- NA
  pal <- leaflet::colorNumeric(palette = .imap_palette("ocean", 9),
                                 domain = vals, na.color = "#cccccc")
  labs <- vapply(seq_len(nrow(g)), function(i) .imap_label_html(
    g$country_name[i], g$iso3_code[i], "GGHED/CHE",
    g$gghed_che[i], function(x) sprintf("%.1f%%", x)),
    character(1))
  leaflet::leaflet(g) |>
    leaflet::addProviderTiles("CartoDB.Positron") |>
    leaflet::addPolygons(fillColor = ~pal(gghed_che),
      weight = 0.5, color = "#1a1f28", fillOpacity = 0.85,
      label = lapply(labs, htmltools::HTML)) |>
    leaflet::addLegend(pal = pal, values = vals,
      title = sprintf("GGHED / CHE \u00b7 %d", year),
      labFormat = leaflet::labelFormat(suffix = "%"),
      position = "bottomright")
}

#' imap4 \u9884\u671f\u5bff\u547d leaflet
imap_lifeexp <- function(master, world_sf, year = NULL) {
  if (!.has_pkg("leaflet") || !.has_pkg("sf")) return(NULL)
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  g <- .imap_join_geom(master, world_sf, year, "life_exp")
  vals <- g$life_exp; vals[!is.finite(vals)] <- NA
  pal <- leaflet::colorNumeric(palette = .imap_palette("default", 9),
                                 domain = vals, na.color = "#cccccc")
  labs <- vapply(seq_len(nrow(g)), function(i) .imap_label_html(
    g$country_name[i], g$iso3_code[i], "Life expectancy",
    g$life_exp[i], function(x) sprintf("%.1f", x)),
    character(1))
  leaflet::leaflet(g) |>
    leaflet::addProviderTiles("CartoDB.Positron") |>
    leaflet::addPolygons(fillColor = ~pal(life_exp),
      weight = 0.5, color = "#1a1f28", fillOpacity = 0.85,
      label = lapply(labs, htmltools::HTML)) |>
    leaflet::addLegend(pal = pal, values = vals,
      title = sprintf("\u9884\u671f\u5bff\u547d \u00b7 %d", year),
      position = "bottomright")
}

#' imap5 U5MR leaflet
imap_u5mr <- function(master, world_sf, year = NULL) {
  if (!.has_pkg("leaflet") || !.has_pkg("sf")) return(NULL)
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  g <- .imap_join_geom(master, world_sf, year, "u5mr")
  vals <- g$u5mr; vals[!is.finite(vals)] <- NA
  pal <- leaflet::colorNumeric(palette = .imap_palette("ember", 9),
                                 domain = log10(pmax(vals, 1)),
                                 na.color = "#cccccc")
  labs <- vapply(seq_len(nrow(g)), function(i) .imap_label_html(
    g$country_name[i], g$iso3_code[i], "U5MR /1000",
    g$u5mr[i], function(x) sprintf("%.1f", x)),
    character(1))
  leaflet::leaflet(g) |>
    leaflet::addProviderTiles("CartoDB.Positron") |>
    leaflet::addPolygons(fillColor = ~pal(log10(pmax(u5mr, 1))),
      weight = 0.5, color = "#1a1f28", fillOpacity = 0.85,
      label = lapply(labs, htmltools::HTML)) |>
    leaflet::addLegend(pal = pal, values = log10(pmax(vals, 1)),
      title = sprintf("log10 U5MR \u00b7 %d", year),
      position = "bottomright")
}

# =============================================================================
# 6-10 · 五分位 / bivariate / 变化
# =============================================================================

#' imap6 \u4eba\u5747 CHE \u4e94\u5206\u4f4d leaflet
imap_quintile_che_pc <- function(master, world_sf, year = NULL) {
  if (!.has_pkg("leaflet") || !.has_pkg("sf")) return(NULL)
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  g <- .imap_join_geom(master, world_sf, year, "che_pc_usd2023")
  bins <- stats::quantile(g$che_pc_usd2023,
                          probs = seq(0, 1, 0.2), na.rm = TRUE)
  bins <- unique(bins)
  if (length(bins) < 3) return(NULL)
  pal <- leaflet::colorBin(palette = .imap_palette("default", 5),
                            bins = bins, na.color = "#cccccc")
  leaflet::leaflet(g) |>
    leaflet::addProviderTiles("CartoDB.Positron") |>
    leaflet::addPolygons(fillColor = ~pal(che_pc_usd2023),
      weight = 0.5, color = "#1a1f28", fillOpacity = 0.85,
      label = lapply(g$country_name, htmltools::HTML)) |>
    leaflet::addLegend(pal = pal, values = g$che_pc_usd2023,
      title = sprintf("\u4eba\u5747 CHE \u4e94\u5206\u4f4d \u00b7 %d", year),
      position = "bottomright")
}

#' imap7 bivariate leaflet (CHE_pc \u00d7 life_exp)
imap_bivariate <- function(master, world_sf, year = NULL) {
  if (!.has_pkg("leaflet") || !.has_pkg("sf")) return(NULL)
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year & is.finite(master$che_pc_usd2023) &
              is.finite(master$life_exp), ]
  q_x <- stats::quantile(d$che_pc_usd2023, c(1/3, 2/3), na.rm = TRUE)
  q_y <- stats::quantile(d$life_exp, c(1/3, 2/3), na.rm = TRUE)
  bx <- cut(d$che_pc_usd2023, c(-Inf, q_x, Inf), labels = 1:3)
  by <- cut(d$life_exp, c(-Inf, q_y, Inf), labels = 1:3)
  d$cell <- paste0(bx, by)
  cell_pal <- c("11" = "#e8e8e8", "12" = "#aac4d1", "13" = "#6c9fbf",
                "21" = "#e5b099", "22" = "#b29ab2", "23" = "#7090b8",
                "31" = "#d97539", "32" = "#b56c6c", "33" = "#7d4a72")
  d$col <- cell_pal[d$cell]
  g <- merge(world_sf, d[, c("iso3_code", "cell", "col",
                                 "country_name", "che_pc_usd2023",
                                 "life_exp")],
              by = "iso3_code", all.x = TRUE)
  g$col[is.na(g$col)] <- "#cccccc"
  labs <- vapply(seq_len(nrow(g)), function(i) {
    if (is.na(g$cell[i])) "" else
      sprintf("<strong>%s</strong><br>CHE\u4eba\u5747 = %s<br>\u5bff\u547d = %.1f<br>cell = %s",
              g$country_name[i],
              scales::dollar(g$che_pc_usd2023[i]),
              g$life_exp[i], g$cell[i])
  }, character(1))
  leaflet::leaflet(g) |>
    leaflet::addProviderTiles("CartoDB.Positron") |>
    leaflet::addPolygons(fillColor = ~col,
      weight = 0.5, color = "#1a1f28", fillOpacity = 0.85,
      label = lapply(labs, htmltools::HTML))
}

#' imap8 CHE\u4eba\u5747 \u53d8\u5316 (2000 \u2192 latest)
imap_change_che_pc <- function(master, world_sf,
                                 y1 = 2000, y2 = NULL) {
  if (!.has_pkg("leaflet") || !.has_pkg("sf")) return(NULL)
  if (is.null(y2)) y2 <- max(master$year, na.rm = TRUE)
  d1 <- master[master$year == y1, c("iso3_code", "country_name",
                                       "che_pc_usd2023")]
  d2 <- master[master$year == y2, c("iso3_code", "che_pc_usd2023")]
  m <- merge(d1, d2, by = "iso3_code", suffixes = c(".y1", ".y2"))
  m$delta <- log(m$che_pc_usd2023.y2 / m$che_pc_usd2023.y1)
  g <- merge(world_sf, m, by = "iso3_code", all.x = TRUE)
  pal <- leaflet::colorNumeric(palette = c("#a23b3b", "#fbf6ee", "#1d3f5f"),
                                 domain = c(-1, 1), na.color = "#cccccc")
  labs <- vapply(seq_len(nrow(g)), function(i) {
    if (is.na(g$delta[i])) "" else
      sprintf("<strong>%s</strong><br>%d\u2192%d log\u500d = %.2f",
              g$country_name[i], y1, y2, g$delta[i])
  }, character(1))
  leaflet::leaflet(g) |>
    leaflet::addProviderTiles("CartoDB.Positron") |>
    leaflet::addPolygons(fillColor = ~pal(delta),
      weight = 0.5, color = "#1a1f28", fillOpacity = 0.85,
      label = lapply(labs, htmltools::HTML)) |>
    leaflet::addLegend(pal = pal, values = c(-1, -0.5, 0, 0.5, 1),
      title = sprintf("log(\u500d\u6570) %d\u2192%d", y1, y2),
      position = "bottomright")
}

#' imap9 OOP \u53d8\u5316 (2000 \u2192 latest)
imap_change_oop <- function(master, world_sf, y1 = 2000, y2 = NULL) {
  if (!.has_pkg("leaflet") || !.has_pkg("sf")) return(NULL)
  if (is.null(y2)) y2 <- max(master$year, na.rm = TRUE)
  d1 <- master[master$year == y1, c("iso3_code", "country_name", "hf3_che")]
  d2 <- master[master$year == y2, c("iso3_code", "hf3_che")]
  m <- merge(d1, d2, by = "iso3_code", suffixes = c(".y1", ".y2"))
  m$delta <- m$hf3_che.y2 - m$hf3_che.y1
  g <- merge(world_sf, m, by = "iso3_code", all.x = TRUE)
  pal <- leaflet::colorNumeric(palette = c("#2a857a", "#fbf6ee", "#a23b3b"),
                                 domain = c(-30, 30), na.color = "#cccccc")
  labs <- vapply(seq_len(nrow(g)), function(i) {
    if (is.na(g$delta[i])) "" else
      sprintf("<strong>%s</strong><br>\u0394 OOP = %+.1f pp",
              g$country_name[i], g$delta[i])
  }, character(1))
  leaflet::leaflet(g) |>
    leaflet::addProviderTiles("CartoDB.Positron") |>
    leaflet::addPolygons(fillColor = ~pal(delta),
      weight = 0.5, color = "#1a1f28", fillOpacity = 0.85,
      label = lapply(labs, htmltools::HTML)) |>
    leaflet::addLegend(pal = pal, values = c(-30, -10, 0, 10, 30),
      title = sprintf("\u0394 OOP / CHE %d\u2192%d (pp)", y1, y2),
      position = "bottomright")
}

#' imap10 effiency (life_exp residual after CHE/cap)
imap_efficiency <- function(master, world_sf, year = NULL) {
  if (!.has_pkg("leaflet") || !.has_pkg("sf")) return(NULL)
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year & is.finite(master$life_exp) &
              is.finite(master$che_pc_usd2023) &
              master$che_pc_usd2023 > 0, ]
  if (!nrow(d)) return(NULL)
  fit <- stats::lm(life_exp ~ log(che_pc_usd2023), data = d)
  d$res <- stats::residuals(fit)
  g <- merge(world_sf, d[, c("iso3_code", "country_name", "res")],
              by = "iso3_code", all.x = TRUE)
  pal <- leaflet::colorNumeric(palette = c("#a23b3b", "#fbf6ee", "#2a857a"),
                                 domain = c(-10, 10), na.color = "#cccccc")
  labs <- vapply(seq_len(nrow(g)), function(i) {
    if (is.na(g$res[i])) "" else
      sprintf("<strong>%s</strong><br>\u5bff\u547d\u6b8b\u5dee = %+.1f\u5e74",
              g$country_name[i], g$res[i])
  }, character(1))
  leaflet::leaflet(g) |>
    leaflet::addProviderTiles("CartoDB.Positron") |>
    leaflet::addPolygons(fillColor = ~pal(res),
      weight = 0.5, color = "#1a1f28", fillOpacity = 0.85,
      label = lapply(labs, htmltools::HTML)) |>
    leaflet::addLegend(pal = pal, values = c(-10, 0, 10),
      title = sprintf("\u5bff\u547d\u6b8b\u5dee (\u540c\u6863 CHE \u4e0b) \u00b7 %d",
                       year),
      position = "bottomright")
}

# =============================================================================
# 11-15 · 比例气泡 / 点叠加 / leaflet 高级
# =============================================================================

#' imap11 \u603b CHE \u6c14\u6ce1\u53e0\u52a0
imap_bubble_che_total <- function(master, world_sf, year = NULL) {
  if (!.has_pkg("leaflet") || !.has_pkg("sf")) return(NULL)
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year & is.finite(master$che_usd2023), ]
  if (!nrow(d)) return(NULL)
  g <- merge(world_sf, d[, c("iso3_code", "country_name", "che_usd2023")],
              by = "iso3_code", all.x = TRUE)
  ce <- sf::st_centroid(sf::st_make_valid(g))
  ce$radius <- sqrt(pmax(ce$che_usd2023, 0)) / 800
  ce <- ce[is.finite(ce$radius) & ce$radius > 0, ]
  if (!nrow(ce)) return(NULL)
  leaflet::leaflet(ce) |>
    leaflet::addProviderTiles("CartoDB.PositronNoLabels") |>
    leaflet::addCircles(
      radius = ~radius * 1000,
      stroke = TRUE, weight = 0.5, color = "#1a1f28",
      fillColor = "#c46327", fillOpacity = 0.55,
      label = ~paste0(country_name, " \u00b7 $",
        formatC(che_usd2023 / 1e9, digits = 1, format = "f"), "B"))
}

#' imap12 OOP \u6c14\u6ce1\u00b7\u5927\u6d32\u989c\u8272
imap_bubble_oop_continent <- function(master, world_sf, year = NULL) {
  if (!.has_pkg("leaflet") || !.has_pkg("sf")) return(NULL)
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year & is.finite(master$hf3_che) &
              is.finite(master$pop), ]
  g <- merge(world_sf, d[, c("iso3_code", "country_name",
                                 "continent", "hf3_che", "pop")],
              by = "iso3_code", all.x = TRUE)
  ce <- sf::st_centroid(sf::st_make_valid(g))
  ce <- ce[is.finite(ce$hf3_che) & is.finite(ce$pop), ]
  if (!nrow(ce)) return(NULL)
  cont_colors <- c("Asia" = "#c46327", "Europe" = "#1d3f5f",
                    "Africa" = "#2a857a", "Americas" = "#7c5b9a",
                    "Oceania" = "#5b8aa6")
  ce$col <- cont_colors[ce$continent]
  ce$col[is.na(ce$col)] <- "#888888"
  ce$radius <- log(ce$pop) * 1.2
  leaflet::leaflet(ce) |>
    leaflet::addProviderTiles("CartoDB.PositronNoLabels") |>
    leaflet::addCircleMarkers(
      radius = ~radius, stroke = TRUE, weight = 0.7, color = "#1a1f28",
      fillColor = ~col, fillOpacity = 0.7,
      label = ~paste0(country_name, " \u00b7 OOP = ",
        sprintf("%.1f%%", hf3_che)))
}

#' imap13 \u70ed\u70b9\u00b7Top10 \u9ad8 OOP \u56fd\u5bb6\u6807\u8bb0
imap_top10_oop <- function(master, world_sf, year = NULL) {
  if (!.has_pkg("leaflet") || !.has_pkg("sf")) return(NULL)
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year & is.finite(master$hf3_che), ]
  d <- d[order(-d$hf3_che), ]
  picked <- utils::head(d, 10)
  g <- merge(world_sf, picked[, c("iso3_code", "country_name", "hf3_che")],
              by = "iso3_code")
  ce <- sf::st_centroid(sf::st_make_valid(g))
  leaflet::leaflet(world_sf) |>
    leaflet::addProviderTiles("CartoDB.Positron") |>
    leaflet::addPolygons(weight = 0.3, color = "#a8b0c0",
      fillColor = "#f1e8da", fillOpacity = 0.8) |>
    leaflet::addCircleMarkers(data = ce,
      radius = 10, weight = 2, color = "#a23b3b",
      fillColor = "#a23b3b", fillOpacity = 0.7,
      label = ~paste0(country_name, " \u00b7 OOP = ",
        sprintf("%.1f%%", hf3_che)),
      labelOptions = leaflet::labelOptions(style = list(
        "font-weight" = "600")))
}

#' imap14 \u56fd\u5bb6\u70b9\u5e7f\u544a\u00b7\u70b9\u51fb\u67e5\u770b
imap_country_points <- function(master, world_sf, year = NULL) {
  if (!.has_pkg("leaflet") || !.has_pkg("sf")) return(NULL)
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year, ]
  g <- merge(world_sf, d[, c("iso3_code", "country_name", "continent",
                                 "che_pc_usd2023", "hf3_che",
                                 "life_exp", "u5mr")],
              by = "iso3_code", all.x = TRUE)
  ce <- sf::st_centroid(sf::st_make_valid(g))
  ce <- ce[is.finite(ce$che_pc_usd2023), ]
  if (!nrow(ce)) return(NULL)
  popup_html <- vapply(seq_len(nrow(ce)), function(i) {
    paste0("<strong>", ce$country_name[i], "</strong> (",
           ce$iso3_code[i], ")<br>",
           "CHE\u4eba\u5747: ", scales::dollar(ce$che_pc_usd2023[i]),
           "<br>",
           "OOP/CHE: ", sprintf("%.1f%%", ce$hf3_che[i]), "<br>",
           "\u5bff\u547d: ", sprintf("%.1f \u5e74", ce$life_exp[i]),
           "<br>U5MR: ", sprintf("%.1f", ce$u5mr[i]))
  }, character(1))
  leaflet::leaflet(world_sf) |>
    leaflet::addProviderTiles("CartoDB.Positron") |>
    leaflet::addPolygons(weight = 0.2, color = "#a8b0c0",
      fillColor = "#f1e8da", fillOpacity = 0.8) |>
    leaflet::addCircleMarkers(data = ce, radius = 4,
      stroke = TRUE, weight = 1, color = "#1a1f28",
      fillColor = "#1d3f5f", fillOpacity = 0.65,
      popup = popup_html)
}

#' imap15 \u9ad8\u4eae\u5355\u56fd\u00b7\u4ee5\u4f8b USA / CHN
imap_highlight_country <- function(master, world_sf, iso = "CHN",
                                     year = NULL) {
  if (!.has_pkg("leaflet") || !.has_pkg("sf")) return(NULL)
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year, ]
  g <- merge(world_sf, d[, c("iso3_code", "country_name",
                                 "che_pc_usd2023")],
              by = "iso3_code", all.x = TRUE)
  g$highlight <- g$iso3_code == iso
  leaflet::leaflet(g) |>
    leaflet::addProviderTiles("CartoDB.Positron") |>
    leaflet::addPolygons(fillColor = ~ifelse(highlight,
        "#c46327", "#e0e6ee"),
      weight = ~ifelse(highlight, 2, 0.5),
      color = ~ifelse(highlight, "#1a1f28", "#a8b0c0"),
      fillOpacity = ~ifelse(highlight, 0.9, 0.7),
      label = ~country_name)
}

# =============================================================================
# 16-20 · plotly choropleth / 高级布局
# =============================================================================

#' imap16 plotly choropleth \u00b7 year frame
imap_plotly_animation <- function(master, world_sf = NULL,
                                    var = "che_pc_usd2023") {
  if (!.has_pkg("plotly")) return(NULL)
  d <- master[is.finite(master[[var]]), ]
  d$z <- if (var %in% c("che_pc_usd2023", "u5mr", "che_usd2023"))
           log10(pmax(d[[var]], 1)) else d[[var]]
  p <- plotly::plot_ly(d,
    type = "choropleth", locations = ~iso3_code,
    z = ~z, frame = ~year,
    text = ~paste(country_name, "<br>", var, "=",
                  signif(d[[var]], 3)),
    colorscale = "Earth",
    colorbar = list(title = var)) |>
    plotly::layout(geo = list(projection = list(type = "robinson"),
                              showcountries = TRUE,
                              countrycolor = "#1a1f28"),
                   title = paste("\u4ea4\u4e92\u5730\u56fe (frame)",
                                  var),
                   margin = list(t = 60, b = 20, l = 20, r = 20),
                   paper_bgcolor = "#fbf6ee",
                   font = list(family = "PingFang SC, sans-serif"))
  p
}

#' imap17 plotly choropleth \u00b7 single year
imap_plotly_choropleth <- function(master, world_sf = NULL,
                                     year = NULL,
                                     var = "che_pc_usd2023") {
  if (!.has_pkg("plotly")) return(NULL)
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year & is.finite(master[[var]]), ]
  d$z <- if (var %in% c("che_pc_usd2023", "u5mr"))
           log10(pmax(d[[var]], 1)) else d[[var]]
  plotly::plot_ly(d, type = "choropleth", locations = ~iso3_code,
    z = ~z, text = ~country_name, colorscale = "Earth",
    colorbar = list(title = var)) |>
    plotly::layout(geo = list(projection = list(type = "mollweide"),
                              showframe = FALSE),
                   title = sprintf("%s \u00b7 %d", var, year),
                   paper_bgcolor = "#fbf6ee")
}

#' imap18 plotly mapbox density (need token; fallback)
imap_plotly_density <- function(master, world_sf = NULL, year = NULL) {
  if (!.has_pkg("plotly")) return(NULL)
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year & is.finite(master$che_usd2023), ]
  if (!nrow(d)) return(NULL)
  # mapbox 需 token，这里用 scatter_geo 近似
  plotly::plot_ly(d, type = "scattergeo", locations = ~iso3_code,
    mode = "markers", marker = list(
      size = ~sqrt(pmax(che_usd2023, 0)) / 1e4,
      color = ~log10(pmax(che_pc_usd2023, 1)),
      colorscale = "Earth",
      sizemode = "diameter",
      opacity = 0.75,
      line = list(color = "#1a1f28", width = 0.5)),
    text = ~paste(country_name, "<br>$",
                   formatC(che_usd2023 / 1e9, 1, format = "f"), "B")) |>
    plotly::layout(geo = list(projection = list(type = "robinson"),
                              showcountries = TRUE),
                   title = sprintf("\u603b CHE \u00b7 \u56fd\u5bb6\u5e94\u7528\u70b9 \u00b7 %d",
                                    year),
                   paper_bgcolor = "#fbf6ee")
}

#' imap19 leaflet \u53cc\u9762\u677f \u00b7 \u5e74\u4efd\u5bf9\u6bd4
imap_dual_compare <- function(master, world_sf,
                                 y1 = 2000, y2 = NULL) {
  if (!.has_pkg("leaflet") || !.has_pkg("sf") || !.has_pkg("htmltools"))
    return(NULL)
  if (is.null(y2)) y2 <- max(master$year, na.rm = TRUE)
  l1 <- imap_che_pc(master, world_sf, y1)
  l2 <- imap_che_pc(master, world_sf, y2)
  if (is.null(l1) || is.null(l2)) return(NULL)
  htmltools::div(style = "display:grid;grid-template-columns:1fr 1fr;
                            gap:10px;width:100%",
    htmltools::div(style = "border:1px solid #d8c89e",
                    htmltools::tags$h4(sprintf("%d", y1),
                      style = "padding:6px 10px;margin:0;
                                background:#f1e8da;color:#1a1f28"),
                    l1),
    htmltools::div(style = "border:1px solid #d8c89e",
                    htmltools::tags$h4(sprintf("%d", y2),
                      style = "padding:6px 10px;margin:0;
                                background:#f1e8da;color:#1a1f28"),
                    l2))
}

#' imap20 leaflet \u8de8\u5e74 layer \u5207\u6362\uff080 1 0 6 0 2 2\uff09
imap_layer_years <- function(master, world_sf,
                              years = c(2000, 2010, 2018, 2022)) {
  if (!.has_pkg("leaflet") || !.has_pkg("sf")) return(NULL)
  pal <- leaflet::colorNumeric(palette = .imap_palette("earth", 9),
                                 domain = log10(c(5, 14000)),
                                 na.color = "#cccccc")
  m <- leaflet::leaflet() |>
    leaflet::addProviderTiles("CartoDB.Positron")
  for (y in years) {
    g <- .imap_join_geom(master, world_sf, y, "che_pc_usd2023")
    g <- g[is.finite(g$che_pc_usd2023) & g$che_pc_usd2023 > 0, ]
    m <- m |> leaflet::addPolygons(data = g,
      fillColor = ~pal(log10(che_pc_usd2023)),
      weight = 0.4, color = "#1a1f28", fillOpacity = 0.85,
      label = ~paste0(country_name, " \u00b7 ",
        scales::dollar_format(accuracy = 1)(che_pc_usd2023)),
      group = sprintf("\u5e74 %d", y))
  }
  m |>
    leaflet::addLayersControl(baseGroups = sprintf("\u5e74 %d", years),
      options = leaflet::layersControlOptions(collapsed = FALSE,
                                                position = "topright")) |>
    leaflet::addLegend(pal = pal, values = log10(c(5, 14000)),
      title = "log10 \u4eba\u5747 CHE", position = "bottomright")
}

# =============================================================================
# 导出器：批量验证 widget 函数能创建对象
# =============================================================================

#' 批量校验 C1 widget 函数（不写文件）
ghs_validate_widgets_map <- function(master = NULL, world_sf = NULL) {
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
  fns <- list(
    imap_che_pc, imap_oop, imap_gghed, imap_lifeexp, imap_u5mr,
    imap_quintile_che_pc, imap_bivariate, imap_change_che_pc,
    imap_change_oop, imap_efficiency,
    imap_bubble_che_total, imap_bubble_oop_continent,
    imap_top10_oop, imap_country_points, imap_highlight_country,
    imap_plotly_animation, imap_plotly_choropleth,
    imap_plotly_density, imap_dual_compare, imap_layer_years)
  names(fns) <- c(
    "imap_che_pc", "imap_oop", "imap_gghed", "imap_lifeexp", "imap_u5mr",
    "imap_quintile_che_pc", "imap_bivariate", "imap_change_che_pc",
    "imap_change_oop", "imap_efficiency",
    "imap_bubble_che_total", "imap_bubble_oop_continent",
    "imap_top10_oop", "imap_country_points", "imap_highlight_country",
    "imap_plotly_animation", "imap_plotly_choropleth",
    "imap_plotly_density", "imap_dual_compare", "imap_layer_years")
  results <- list()
  for (nm in names(fns)) {
    res <- tryCatch({
      obj <- fns[[nm]](master, world_sf)
      list(ok = !is.null(obj), class = class(obj)[1])
    }, error = function(e) list(ok = FALSE, class = conditionMessage(e)))
    results[[nm]] <- res
  }
  results
}

#' \u6279\u91cf\u5bfc\u51fa C1 \u5730\u56fe widget \u4e3a HTML \u6587\u4ef6
#' @param master \u4e3b\u6570\u636e
#' @param world_sf sf \u5bf9\u8c61
#' @param out_dir \u8f93\u51fa\u76ee\u5f55
#' @param verbose \u662f\u5426\u6253\u5370\u8fdb\u5ea6
ghs_export_widgets_map <- function(master = NULL, world_sf = NULL,
                                     out_dir = file.path("\u5206\u6790\u8f93\u51fa",
                                                          "\u4ea4\u4e92\u7ec4\u4ef6"),
                                     verbose = TRUE) {
  if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
    if (verbose) message("[map] htmlwidgets \u4e0d\u53ef\u7528\uff0c\u8df3\u8fc7")
    return(invisible(character(0)))
  }
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
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  fn_names <- ls(envir = .GlobalEnv, pattern = "^imap_[a-z]")
  produced <- character(0)
  for (nm in fn_names) {
    f <- get(nm, envir = .GlobalEnv)
    obj <- tryCatch(f(master, world_sf), error = function(e) NULL)
    if (is.null(obj)) {
      if (verbose) message("[map] skip ", nm)
      next
    }
    out_name <- paste0("imap_", sub("^imap_", "", nm))
    p <- if (exists("ghs_save_widget", mode = "function")) {
      ghs_save_widget(obj, out_name, dir = out_dir)
    } else {
      pp <- file.path(out_dir, paste0(out_name, ".html"))
      tryCatch(htmlwidgets::saveWidget(obj, file = pp,
                                          selfcontained = TRUE),
                error = function(e) NULL)
      pp
    }
    if (!is.null(p)) {
      produced <- c(produced, p)
      if (verbose) message("[map] saved ", nm)
    }
  }
  if (verbose) message("[map] total saved: ", length(produced))
  invisible(produced)
}
