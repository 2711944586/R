# =============================================================================
# 程序/18_widgets_other.R  ·  v2 leaflet / reactable / networkD3 / DT widget（6 个）
# -----------------------------------------------------------------------------

#' W3.1 · leaflet 主世界地图（5 指标可切换）
#' @export
widget_v2_leaflet_choropleth <- function(master, world_sf,
                                          year = 2022,
                                          metrics = c("hf3_che","gghed_che",
                                                       "ext_che","che_pc_usd2023",
                                                       "hc6_che")) {
  if (!requireNamespace("leaflet", quietly = TRUE)) return(NULL)
  if (is.null(world_sf)) return(NULL)
  metrics <- intersect(metrics, names(master))
  if (!length(metrics)) return(NULL)
  d <- master[master$year == year, c("iso3_code", metrics)]
  joined <- merge(world_sf, d, by = "iso3_code", all.x = TRUE)
  if (!inherits(joined, "sf")) joined <- sf::st_as_sf(joined)

  m <- leaflet::leaflet(joined,
    options = leaflet::leafletOptions(zoomControl = TRUE,
                                       worldCopyJump = FALSE,
                                       minZoom = 1.5, maxZoom = 5)) |>
    leaflet::addProviderTiles("CartoDB.PositronNoLabels") |>
    leaflet::setView(lng = 0, lat = 25, zoom = 2)

  for (metric in metrics) {
    vals <- joined[[metric]]
    pal <- leaflet::colorNumeric("RdYlBu",
                                  domain = vals,
                                  na.color = "#E5E7EB",
                                  reverse = TRUE)
    nm <- if ("country_name_sf" %in% names(joined)) joined$country_name_sf
          else if ("country_name" %in% names(joined)) joined$country_name
          else joined$iso3_code
    label <- sprintf("<b>%s</b><br>%s: %s",
                      nm, metric,
                      ifelse(is.na(vals), "n/a", sprintf("%.1f", vals)))
    m <- leaflet::addPolygons(m,
      group = metric,
      fillColor = ~pal(vals),
      fillOpacity = 0.78,
      color = "white", weight = 0.4,
      label = lapply(label, htmltools::HTML),
      highlightOptions = leaflet::highlightOptions(weight = 1.5,
                                                     color = "#1A1A1F",
                                                     bringToFront = TRUE)
    ) |>
      leaflet::addLegend(pal = pal, values = vals,
                          title = metric, position = "bottomright",
                          group = metric, opacity = 0.85)
  }

  m |>
    leaflet::addLayersControl(
      baseGroups = metrics,
      options = leaflet::layersControlOptions(collapsed = FALSE,
                                                position = "topright")
    ) |>
    leaflet::hideGroup(setdiff(metrics, metrics[[1]]))
}

#' W3.2 · reactable 国家排行（含 sparkbar）
#' @export
widget_v2_reactable_rank <- function(master, year = 2023) {
  if (!requireNamespace("reactable", quietly = TRUE)) return(NULL)
  d <- master[master$year == year &
              is.finite(master$che_pc_usd2023), ]
  if (nrow(d) == 0) return(NULL)
  d_full <- master[is.finite(master$che_pc_usd2023), ]
  spark <- d_full |>
    dplyr::group_by(iso3_code) |>
    dplyr::summarise(trend = list(che_pc_usd2023), .groups = "drop")
  d <- merge(d, spark, by = "iso3_code", all.x = TRUE)
  d <- d[order(-d$che_pc_usd2023),
         c("country_name","continent","income_group",
           "che_pc_usd2023","hf3_che","ext_che","trend")]
  names(d)[1:6] <- c("Country","Continent","Income group",
                       "CHE/cap (USD)","OOPS %","Aid %")

  reactable::reactable(d,
    pagination = TRUE, defaultPageSize = 15,
    searchable = TRUE, sortable = TRUE,
    showPageSizeOptions = TRUE,
    striped = FALSE, highlight = TRUE,
    theme = reactable::reactableTheme(
      style = list(fontFamily = "Inter, system-ui, sans-serif"),
      borderColor = "#1A1A1F1F",
      headerStyle = list(backgroundColor = "#FAF7F2",
                          borderBottom = "2px solid #1A1A1F",
                          fontWeight = 600)),
    columns = list(
      `CHE/cap (USD)` = reactable::colDef(
        format = reactable::colFormat(prefix = "$", separators = TRUE,
                                       digits = 0),
        align = "right"),
      `OOPS %` = reactable::colDef(
        format = reactable::colFormat(suffix = "%", digits = 1),
        align = "right"),
      `Aid %` = reactable::colDef(
        format = reactable::colFormat(suffix = "%", digits = 1),
        align = "right"),
      trend = reactable::colDef(
        name = "Trend",
        cell = function(values) {
          if (is.null(values) || all(is.na(values))) return("\u2014")
          paste(rep("\u25a0", min(length(values), 23)), collapse = "")
        },
        html = TRUE, align = "left"
      )
    )
  )
}

#' W3.3 · DT 全量浏览（30 列字段）
#' @export
widget_v2_dt_atlas <- function(master) {
  if (!requireNamespace("DT", quietly = TRUE)) return(NULL)
  show_cols <- intersect(c(
    "country_name","iso3_code","year","continent","income_group",
    "che_usd2023","che_pc_usd2023","gghed_che","pvtd_che","ext_che",
    "hf1_che","hf2_che","hf3_che","hfnec_che",
    "hc1_che","hc6_che",
    "gdp_pc_usd","life_exp","u5mr","pop"
  ), names(master))
  d <- master[, show_cols]

  DT::datatable(
    d,
    extensions = c("Buttons","FixedHeader","Scroller"),
    options = list(
      pageLength = 25,
      dom = "Bfrtip",
      buttons = c("copy","csv","excel"),
      deferRender = TRUE,
      scrollY = 500, scroller = TRUE,
      fixedHeader = TRUE,
      initComplete = DT::JS(
        "function(){",
        "$(this.api().table().header()).css({",
        "  'background-color':'#FAF7F2',",
        "  'border-bottom':'2px solid #1A1A1F',",
        "  'font-family':'Inter,system-ui,sans-serif'});}")
    ),
    rownames = FALSE,
    filter = "top",
    class = "compact stripe"
  ) |>
    DT::formatRound(columns = intersect(c("che_usd2023","che_pc_usd2023",
                                            "gdp_pc_usd","life_exp","u5mr","pop",
                                            "gghed_che","pvtd_che","ext_che",
                                            "hf1_che","hf2_che","hf3_che","hfnec_che",
                                            "hc1_che","hc6_che"), names(d)),
                     digits = 1)
}

#' W3.4 · networkD3 国家相似图（基于 PCA 距离）
#' @export
widget_v2_country_network <- function(master, year = 2022, k = 5) {
  if (!requireNamespace("networkD3", quietly = TRUE)) return(NULL)
  cols <- intersect(c("hf1_che","hf2_che","hf3_che",
                       "gghed_che","pvtd_che","ext_che",
                       "hc1_che","hc6_che","che_pc_usd2023"),
                     names(master))
  if (length(cols) < 4) return(NULL)
  d <- master[master$year == year, c("country_name","iso3_code","continent", cols)]
  d <- d[stats::complete.cases(d[cols]), ]
  if (nrow(d) < 20) return(NULL)
  X <- scale(as.matrix(d[cols]))
  dm <- as.matrix(stats::dist(X))
  diag(dm) <- Inf

  edges <- do.call(rbind, lapply(seq_len(nrow(dm)), function(i) {
    nn <- order(dm[i, ])[seq_len(k)]
    data.frame(source = i - 1, target = nn - 1,
                value = 1 / pmax(dm[i, nn], 1e-3),
                stringsAsFactors = FALSE)
  }))

  nodes <- data.frame(
    name = d$country_name,
    group = as.integer(as.factor(d$continent)),
    size = 10
  )

  networkD3::forceNetwork(Links = edges, Nodes = nodes,
    Source = "source", Target = "target", Value = "value",
    NodeID = "name", Group = "group", Nodesize = "size",
    fontFamily = "Inter, system-ui, sans-serif",
    opacity = 0.9, zoom = TRUE, legend = TRUE,
    bounded = TRUE, charge = -90)
}

#' W3.5 · sankey 三段（来源 → HF → HC）
#' @export
widget_v2_sankey_flows <- function(master, year = 2022) {
  if (!requireNamespace("networkD3", quietly = TRUE)) return(NULL)
  d <- master[master$year == year &
              !is.na(master$gghed_che) &
              !is.na(master$pvtd_che) &
              !is.na(master$ext_che) &
              !is.na(master$hf1_che) &
              !is.na(master$hf2_che) &
              !is.na(master$hf3_che) &
              !is.na(master$hc1_che) &
              !is.na(master$hc6_che), ]
  if (nrow(d) == 0) return(NULL)

  agg <- function(x) sum(x, na.rm = TRUE)
  S  <- c(agg(d$gghed_che), agg(d$pvtd_che), agg(d$ext_che))
  HF <- c(agg(d$hf1_che), agg(d$hf2_che), agg(d$hf3_che))
  HC <- c(agg(d$hc1_che), agg(d$hc6_che))

  nodes <- data.frame(name = c("Government","Private","External",
                                 "HF1 Gov scheme","HF2 Insurance","HF3 OOPS",
                                 "HC1 Curative","HC6 Preventive"))

  links <- data.frame(
    source = c(0,0,1,2,3,3,4,5),
    target = c(3,4,5,3,6,7,6,6),
    value  = c(S[1]*0.7, S[1]*0.3, S[2], S[3]*0.5,
                HF[1]*0.7, HF[1]*0.3, HF[2], HF[3]*0.4)
  )

  networkD3::sankeyNetwork(Links = links, Nodes = nodes,
    Source = "source", Target = "target", Value = "value",
    NodeID = "name", units = "% CHE", fontSize = 12,
    fontFamily = "Inter, system-ui, sans-serif",
    nodeWidth = 22, nodePadding = 18,
    sinksRight = FALSE)
}

#' W3.6 · KPI 卡片 grid（HTML 直接嵌入 Quarto）
#' @export
widget_v2_kpi_grid <- function(master, year = 2022) {
  if (!requireNamespace("htmltools", quietly = TRUE)) return(NULL)
  d <- master[master$year == year, ]
  if (!nrow(d)) return(NULL)
  total_che <- sum(d$che_usd2023, na.rm = TRUE) / 1e12
  med_pc    <- stats::median(d$che_pc_usd2023, na.rm = TRUE)
  oops_med  <- stats::median(d$hf3_che, na.rm = TRUE)
  ext_med   <- stats::median(d$ext_che, na.rm = TRUE)

  htmltools::div(class = "kpi-grid",
    htmltools::HTML(
      paste0(
        kpi_card_html(sprintf("$%.1fT", total_che),
                       label = "Total CHE",
                       sublabel = sprintf("%d \u00b7 \u5168\u7403\u5408\u8ba1", year)),
        kpi_card_html(sprintf("$%.0f", med_pc),
                       label = "Median CHE per capita",
                       sublabel = sprintf("%d \u00b7 USD 2023", year)),
        kpi_card_html(sprintf("%.0f%%", oops_med),
                       label = "Median OOPS share",
                       sublabel = sprintf("%d \u00b7 hf3_che", year)),
        kpi_card_html(sprintf("%.0f%%", ext_med),
                       label = "Median external aid share",
                       sublabel = sprintf("%d \u00b7 ext_che", year))
      )
    )
  )
}

# ---- 批量导出 -------------------------------------------------------------

#' v2 其他 widget（4 个支持 saveWidget）批量保存
#' @export
ghs_export_v2_widgets_other <- function(master, world_sf = NULL,
                                         out_dir = file.path("分析输出", "交互组件"),
                                         verbose = TRUE) {
  if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
    message("htmlwidgets missing - skip"); return(invisible(0))
  }
  if (is.null(world_sf)) {
    world_sf <- tryCatch(load_world_sf("medium", 0.08), error = function(e) NULL)
  }
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  jobs <- list(
    list(id = "v2_w_leaflet_choropleth", fn = function() widget_v2_leaflet_choropleth(master, world_sf)),
    list(id = "v2_w_reactable_rank",     fn = function() widget_v2_reactable_rank(master)),
    list(id = "v2_w_dt_atlas",           fn = function() widget_v2_dt_atlas(master)),
    list(id = "v2_w_country_network",    fn = function() widget_v2_country_network(master)),
    list(id = "v2_w_sankey_flows",       fn = function() widget_v2_sankey_flows(master))
  )

  ok <- 0
  for (j in jobs) {
    p <- tryCatch(j$fn(), error = function(e) {
      if (verbose) message("[", j$id, "] error: ", conditionMessage(e))
      NULL
    })
    if (is.null(p)) next
    f <- file.path(out_dir, paste0(j$id, ".html"))
    tryCatch({
      htmlwidgets::saveWidget(p, file = f, selfcontained = TRUE,
                                title = j$id)
      if (exists("ghs_widget_shell", mode = "function") &&
          exists("ghs_widget_label", mode = "function")) {
        shell_fn <- get("ghs_widget_shell", mode = "function")
        label_fn <- get("ghs_widget_label", mode = "function")
        shell_fn(f, title = label_fn(j$id),
                 source = "WHO GHED · 程序/18_widgets_other.R")
      }
      if (verbose) cat("[v2_widget]", j$id, "saved\n")
      ok <- ok + 1
    }, error = function(e) {
      if (verbose) message("[", j$id, "] save failed: ", conditionMessage(e))
    })
  }
  invisible(ok)
}
