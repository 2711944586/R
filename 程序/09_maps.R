
if (!exists("proj_root", mode = "function")) {
  source(file.path("程序", "00_utils.R"))
}

leaflet_choropleth <- function(master, world_sf,
                                indicator_col = "hf3_che",
                                year_focus = 2023,
                                palette = "viridis",
                                title = NULL) {
  ensure_pkgs(c("dplyr"))
  if (!requireNamespace("leaflet", quietly = TRUE)) return(NULL)
  if (!requireNamespace("sf", quietly = TRUE)) return(NULL)
  d <- master |>
    dplyr::filter(.data$year == year_focus) |>
    dplyr::select("iso3_code", val = !!rlang::sym(indicator_col))
  map_df <- world_sf |>
    dplyr::left_join(d, by = "iso3_code")
  pal <- leaflet::colorNumeric(palette, domain = map_df$val, na.color = "#cccccc")
  popup_html <- sprintf(
    "<b>%s</b><br/>%s = %s",
    map_df$country_name %||% map_df$NAME %||% map_df$iso3_code,
    indicator_col,
    ifelse(is.finite(map_df$val), formatC(map_df$val, digits = 2, format = "f"), "—")
  )
  leaflet::leaflet(map_df) |>
    leaflet::addProviderTiles("CartoDB.PositronNoLabels") |>
    leaflet::addPolygons(
      fillColor = ~pal(val),
      weight = 0.5, color = "white",
      fillOpacity = 0.85,
      popup = popup_html,
      highlightOptions = leaflet::highlightOptions(
        weight = 2, color = "#333", fillOpacity = 0.95, bringToFront = TRUE
      )
    ) |>
    leaflet::addLegend(
      position = "bottomright",
      pal = pal, values = ~val,
      title = title %||% indicator_col,
      opacity = 1, na.label = "\u7f3a\u5931"
    )
}

add_map_basics <- function(map) {
  if (!requireNamespace("leaflet", quietly = TRUE)) return(map)
  map |>
    leaflet::addScaleBar(position = "bottomleft")
}
