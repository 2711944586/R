# =============================================================================
# 程序/08_plot_interactive.R
# -----------------------------------------------------------------------------
# 交互图工厂：plotly / dygraphs / networkD3 包装。所有函数都在缺依赖时降级。
# =============================================================================

if (!exists("proj_root", mode = "function")) {
  source(file.path("程序", "00_utils.R"))
}

# ---- 1. 时序折线（plotly hover + 范围滑块） --------------------------------
#' 一国一指标的交互时序
#' @param df 长表 (year, country_name, value) 或宽表（自动检测）
#' @param value_col 数值列名（仅在宽表时使用）
ts_plotly <- function(df, value_col = NULL,
                      title = NULL, ylab = NULL,
                      smooth = FALSE) {
  ensure_pkgs(c("dplyr"))
  if (!requireNamespace("plotly", quietly = TRUE)) {
    warning("plotly not installed; returning ggplot fallback")
    p <- ggplot2::ggplot(df, ggplot2::aes(.data$year,
                                           !!rlang::sym(value_col %||% "value"))) +
      ggplot2::geom_line() +
      theme_ghs()
    return(p)
  }
  if (!is.null(value_col)) {
    df <- df |>
      dplyr::transmute(country_name = .data$country_name,
                       year = .data$year,
                       value = !!rlang::sym(value_col))
  }
  fig <- plotly::plot_ly(df,
                          x = ~year, y = ~value,
                          color = ~country_name,
                          type  = "scatter", mode = "lines+markers",
                          hovertemplate = paste0(
                            "<b>%{customdata}</b><br>",
                            "Year: %{x}<br>Value: %{y:.2f}<extra></extra>"
                          ),
                          customdata = ~country_name) |>
    plotly::layout(
      title = list(text = title %||% "", x = 0.05),
      xaxis = list(title = "Year",
                   rangeslider = list(visible = TRUE),
                   rangeselector = list(buttons = list(
                     list(count = 5, label = "5y", step = "year",
                          stepmode = "backward"),
                     list(count = 10, label = "10y", step = "year",
                          stepmode = "backward"),
                     list(step = "all", label = "All")
                   ))),
      yaxis = list(title = ylab %||% "value"),
      hovermode = "x unified",
      legend = list(orientation = "h", y = -0.25),
      margin = list(t = 60, b = 80)
    ) |>
    plotly::config(displaylogo = FALSE,
                   modeBarButtonsToRemove = c("autoScale2d", "select2d", "lasso2d"))
  fig
}

# ---- 2. 散点 (Gapminder 风格) -----------------------------------------------
gapminder_plotly <- function(df, x = "gdp_pc_usd", y = "che_pc_usd2023",
                             size = "pop", color = "continent",
                             frame = "year",
                             log_x = TRUE, log_y = TRUE,
                             title = NULL) {
  ensure_pkgs(c("dplyr"))
  if (!requireNamespace("plotly", quietly = TRUE)) {
    warning("plotly not installed")
    return(NULL)
  }
  d <- df |>
    dplyr::filter(
      is.finite(!!rlang::sym(x)),
      is.finite(!!rlang::sym(y))
    )
  fig <- plotly::plot_ly(
    d,
    x = stats::as.formula(paste0("~", x)),
    y = stats::as.formula(paste0("~", y)),
    size = stats::as.formula(paste0("~", size)),
    color = stats::as.formula(paste0("~", color)),
    frame = stats::as.formula(paste0("~", frame)),
    text = ~country_name,
    hovertemplate = paste0(
      "<b>%{text}</b><br>",
      x, ": %{x:,.0f}<br>",
      y, ": %{y:,.0f}<extra></extra>"
    ),
    type = "scatter", mode = "markers",
    marker = list(opacity = 0.75, sizemode = "area", sizeref = 0.5)
  ) |>
    plotly::layout(
      title = list(text = title %||% "", x = 0.05),
      xaxis = list(title = x, type = if (log_x) "log" else "linear"),
      yaxis = list(title = y, type = if (log_y) "log" else "linear"),
      legend = list(orientation = "h", y = -0.2)
    ) |>
    plotly::animation_opts(frame = 800, transition = 300, redraw = FALSE) |>
    plotly::animation_slider(currentvalue = list(prefix = "Year: ")) |>
    plotly::config(displaylogo = FALSE)
  fig
}

# ---- 3. 桑基图（来源 -> 筹资方案 -> 用途） ---------------------------------
#' 简单的桑基：各源 (政府/私人/外援) -> 各筹资方案
#' @param master 宽表（含份额列）
sankey_sources_to_schemes <- function(master, year_focus = 2022,
                                       region = NULL) {
  ensure_pkgs(c("dplyr"))
  if (!requireNamespace("plotly", quietly = TRUE)) return(NULL)
  d <- master |> dplyr::filter(.data$year == year_focus)
  if (!is.null(region) && "continent" %in% names(d)) {
    d <- d |> dplyr::filter(.data$continent == region)
  }
  if (nrow(d) == 0) return(NULL)
  agg <- d |>
    dplyr::summarise(
      gghed = mean(.data$gghed_che, na.rm = TRUE),
      pvtd  = mean(.data$pvtd_che,  na.rm = TRUE),
      ext   = mean(.data$ext_che,   na.rm = TRUE),
      hf1   = mean(.data$hf1_che,   na.rm = TRUE),
      hf2   = mean(.data$hf2_che,   na.rm = TRUE),
      hf3   = mean(.data$hf3_che,   na.rm = TRUE),
      hf4   = mean(.data$hf4_che,   na.rm = TRUE),
      hfnec = mean(.data$hfnec_che, na.rm = TRUE)
    )
  # 启发式映射：政府主导 -> hf1; 私人 -> hf2+hf3; 外援 -> hf4 等。
  # 这里只做近似流向（数据本身没有给精确映射）。
  src_lbl <- c("\u653f\u5e9c GGHE-D", "\u79c1\u4eba PVT-D", "\u5916\u63f4 EXT")
  sch_lbl <- c("HF1 \u653f\u5e9c\u8ba1\u5212", "HF2 \u793e\u4fdd",
               "HF3 OOPS", "HF4 \u81ea\u613f", "HFnec")
  nodes <- data.frame(name = c(src_lbl, sch_lbl))
  links <- data.frame(
    source = c(0, 0, 1, 1, 1, 2, 2),
    target = c(3, 4, 5, 6, 4, 6, 7) + length(src_lbl),
    value  = c(agg$gghed * 0.7, agg$gghed * 0.3,
               agg$pvtd * 0.6, agg$pvtd * 0.3, agg$pvtd * 0.1,
               agg$ext  * 0.5, agg$ext  * 0.5)
  ) - 0  # 注意 plotly 桑基要求 source/target 是节点 index
  links$source <- links$source
  links$target <- links$target

  plotly::plot_ly(
    type = "sankey",
    orientation = "h",
    node = list(
      label = nodes$name, pad = 15, thickness = 20,
      line = list(color = "white", width = 0.5),
      color = c("#1B5E88", "#E07B00", "#2E8B57",
                "#4F81BD", "#9BBB59", "#C0504D", "#8064A2", "#7F7F7F")
    ),
    link = list(
      source = links$source, target = links$target,
      value  = pmax(links$value, 0)
    )
  ) |>
    plotly::layout(
      title = sprintf("\u7b79\u8d44\u6d41\u5411\u793a\u610f \u00b7 %d", year_focus),
      font = list(size = 13),
      margin = list(t = 60)
    )
}

# ---- 4. 高亮国家时序：plotly + crosstalk-friendly --------------------------
highlight_country_ts <- function(master,
                                  isos = c("CHN", "USA", "IND", "BRA", "ZAF"),
                                  value_col = "che_pc_usd2023",
                                  title = "\u4eba\u5747 CHE \u8d70\u52bf\u5bf9\u6bd4") {
  ensure_pkgs(c("dplyr"))
  if (!requireNamespace("plotly", quietly = TRUE)) return(NULL)
  val <- rlang::sym(value_col)
  d <- master |>
    dplyr::filter(.data$iso3_code %in% isos, is.finite(!!val))
  plotly::plot_ly(d,
                   x = ~year, y = stats::as.formula(paste0("~", value_col)),
                   color = ~country_name,
                   type = "scatter", mode = "lines+markers",
                   hovertemplate = "<b>%{customdata}</b><br>%{x}: %{y:,.0f}<extra></extra>",
                   customdata = ~country_name) |>
    plotly::layout(
      title = list(text = title, x = 0.05),
      xaxis = list(title = "Year"),
      yaxis = list(title = value_col),
      hovermode = "x unified",
      legend = list(orientation = "h", y = -0.2)
    )
}

# ---- 5. dygraphs（适合时序仪表盘小图） -------------------------------------
ts_dygraph <- function(df, value_col, country_name = NULL, title = NULL) {
  if (!requireNamespace("dygraphs", quietly = TRUE)) return(NULL)
  if (!requireNamespace("xts", quietly = TRUE)) return(NULL)
  d <- df
  if (!is.null(country_name)) {
    d <- df[df$country_name == country_name, , drop = FALSE]
  }
  v <- d[[value_col]]
  ts_obj <- xts::xts(v, order.by = as.Date(paste0(d$year, "-01-01")))
  dygraphs::dygraph(ts_obj, main = title) |>
    dygraphs::dyRangeSelector() |>
    dygraphs::dyOptions(strokeWidth = 2, drawPoints = TRUE, pointSize = 3,
                        colors = "#1B5E88")
}

# ---- 6. \u96f7\u8fbe\u56fe (echarts4r) -------------------------------------
#' \u591a\u56fd 5 \u7ef4\u96f7\u8fbe
radar_echarts <- function(master,
                          isos = c("CHN", "USA", "DEU", "BRA", "ZAF"),
                          year_focus = 2022,
                          title = NULL) {
  if (!requireNamespace("echarts4r", quietly = TRUE)) return(NULL)
  ensure_pkgs(c("dplyr", "tidyr"))
  d <- master |>
    dplyr::filter(.data$iso3_code %in% isos, .data$year == year_focus) |>
    dplyr::transmute(
      country_name = .data$country_name,
      oops_pct  = .data$hf3_che,
      gov_pct   = .data$gghed_che,
      pvt_pct   = .data$pvtd_che,
      ext_pct   = .data$ext_che,
      prev_pct  = .data$hc6_che %||% NA_real_
    ) |>
    tidyr::replace_na(list(prev_pct = 0))
  if (nrow(d) == 0) return(NULL)
  echarts4r::e_charts(d, .data$country_name) |>
    echarts4r::e_radar(.data$oops_pct, max = 100, name = "OOPS %") |>
    echarts4r::e_radar(.data$gov_pct,  max = 100,
                        name = "\u653f\u5e9c %") |>
    echarts4r::e_radar(.data$pvt_pct,  max = 100,
                        name = "\u79c1\u4eba %") |>
    echarts4r::e_radar(.data$ext_pct,  max = 100,
                        name = "\u5916\u63f4 %") |>
    echarts4r::e_radar(.data$prev_pct, max = 30,
                        name = "\u9884\u9632 %") |>
    echarts4r::e_tooltip(trigger = "item") |>
    echarts4r::e_title(text = title %||%
                          sprintf("%d \u591a\u56fd\u96f7\u8fbe\u5bf9\u6bd4",
                                   year_focus))
}

# ---- 7. \u8d5b\u8dd1\u67f1\u72b6\u56fe (plotly bar race) -------------------
bar_race_plotly <- function(master, value_col = "che_pc_usd2023",
                              top_n = 15, title = NULL) {
  if (!requireNamespace("plotly", quietly = TRUE)) return(NULL)
  ensure_pkgs(c("dplyr"))
  val <- rlang::sym(value_col)
  d <- master |>
    dplyr::filter(is.finite(!!val)) |>
    dplyr::group_by(.data$year) |>
    dplyr::mutate(rk = rank(-!!val, ties.method = "first")) |>
    dplyr::filter(.data$rk <= top_n) |>
    dplyr::ungroup() |>
    dplyr::arrange(.data$year, .data$rk)
  plotly::plot_ly(d,
                   x = stats::as.formula(paste0("~", value_col)),
                   y = ~country_name,
                   frame = ~year, color = ~continent,
                   type = "bar", orientation = "h",
                   hovertemplate = "%{y}: %{x:,.0f}<extra></extra>") |>
    plotly::layout(
      title = list(text = title %||% sprintf("Top %d \u8d5b\u8dd1", top_n)),
      xaxis = list(title = value_col),
      yaxis = list(title = "", categoryorder = "total ascending"),
      margin = list(l = 100, r = 20, t = 60, b = 60)
    ) |>
    plotly::animation_opts(frame = 800, transition = 300, redraw = TRUE) |>
    plotly::animation_slider(currentvalue = list(prefix = "Year: ")) |>
    plotly::config(displaylogo = FALSE)
}

# ---- 8. reactable rich table ----------------------------------------------
#' \u56fd\u5bb6\u6392\u884c reactable\uff0c\u542b\u8ff7\u4f60\u6761\u5217
reactable_country_rank <- function(master, value_col = "hf3_che",
                                     year_focus = 2022, top_n = 30) {
  if (!requireNamespace("reactable", quietly = TRUE)) return(NULL)
  ensure_pkgs(c("dplyr"))
  val <- rlang::sym(value_col)
  d <- master |>
    dplyr::filter(.data$year == year_focus, is.finite(!!val)) |>
    dplyr::arrange(dplyr::desc(!!val)) |>
    dplyr::slice_head(n = top_n) |>
    dplyr::transmute(
      Rank        = dplyr::row_number(),
      Country     = .data$country_name,
      Continent   = .data$continent,
      IncomeGroup = .data$income_group,
      Value       = round(!!val, 2)
    )
  bar_chart <- function(label, width = "100%", height = "0.875rem",
                          fill = "#1B5E88", background = "#e1e1e1") {
    bar <- htmltools::div(
      style = sprintf("background:%s;width:%s;height:%s",
                       fill, width, height))
    chart <- htmltools::div(
      style = sprintf("flex-grow:1;margin-left:6px;background:%s;",
                       background), bar)
    htmltools::div(
      style = "display:flex;align-items:center;",
      label, chart
    )
  }
  reactable::reactable(
    d,
    searchable = TRUE,
    pagination = TRUE,
    defaultPageSize = 12,
    highlight = TRUE,
    columns = list(
      Value = reactable::colDef(
        name = value_col,
        cell = function(value) {
          mx <- max(d$Value, na.rm = TRUE)
          width <- sprintf("%0.0f%%", value / mx * 100)
          bar_chart(format(value, nsmall = 1), width = width)
        }
      )
    )
  )
}

# ---- 9. ternary plotly ----------------------------------------------------
ternary_plotly <- function(master, year_focus = 2022, title = NULL) {
  if (!requireNamespace("plotly", quietly = TRUE)) return(NULL)
  ensure_pkgs("dplyr")
  d <- master |>
    dplyr::filter(.data$year == year_focus,
                  is.finite(.data$hf1_che),
                  is.finite(.data$hf2_che),
                  is.finite(.data$hf3_che),
                  !is.na(.data$continent))
  total <- d$hf1_che + d$hf2_che + d$hf3_che
  d <- dplyr::mutate(
    d,
    a_norm = .data$hf1_che / total,
    b_norm = .data$hf2_che / total,
    c_norm = .data$hf3_che / total
  )
  plotly::plot_ly(
    d, type = "scatterternary",
    a = ~a_norm, b = ~b_norm, c = ~c_norm,
    color = ~continent,
    text = ~country_name,
    hovertemplate = paste0(
      "<b>%{text}</b><br>",
      "HF1=%{a:.1%}<br>HF2=%{b:.1%}<br>HF3=%{c:.1%}<extra></extra>"
    ),
    mode = "markers",
    marker = list(size = 8, opacity = 0.75)
  ) |>
    plotly::layout(
      title = title %||% sprintf("%d HF1/HF2/HF3 \u4ea4\u4e92\u4e09\u5143\u56fe",
                                   year_focus),
      ternary = list(
        sum = 1,
        aaxis = list(title = "HF1 \u653f\u5e9c"),
        baxis = list(title = "HF2 \u793e\u4fdd"),
        caxis = list(title = "HF3 OOPS")
      )
    ) |>
    plotly::config(displaylogo = FALSE)
}

# ---- 10. forecast subplot --------------------------------------------------
forecast_plotly <- function(master, isos = c("CHN", "USA", "IND", "BRA"),
                              value_col = "che_pc_usd2023", h = 5,
                              title = NULL) {
  if (!requireNamespace("plotly", quietly = TRUE)) return(NULL)
  ensure_pkgs("dplyr")
  plots <- lapply(isos, function(iso) {
    d <- master |>
      dplyr::filter(.data$iso3_code == iso) |>
      dplyr::arrange(.data$year)
    if (nrow(d) < 5) return(NULL)
    fc <- fit_forecast(d[[value_col]], d$year, h = h)
    if (is.null(fc)) return(NULL)
    fig <- plotly::plot_ly()
    fig <- plotly::add_lines(
      fig, x = d$year, y = d[[value_col]],
      name = paste(d$country_name[1], "history"),
      line = list(color = "#1B5E88", width = 2.5)
    )
    fig <- plotly::add_ribbons(
      fig, x = fc$year, ymin = fc$lo_95, ymax = fc$hi_95,
      name = "95% PI",
      fillcolor = "rgba(192,80,77,0.15)",
      line = list(color = "transparent")
    )
    fig <- plotly::add_lines(
      fig, x = fc$year, y = fc$point,
      name = paste(d$country_name[1], "forecast"),
      line = list(color = "#C0504D", width = 2.5, dash = "dash")
    )
    fig
  })
  plots <- Filter(Negate(is.null), plots)
  if (!length(plots)) return(NULL)
  plotly::subplot(plots, nrows = ceiling(length(plots) / 2), shareX = FALSE,
                   shareY = FALSE) |>
    plotly::layout(title = list(text = title %||% sprintf(
      "ARIMA \u9884\u6d4b\u4ea4\u4e92\u56fe (h=%d)", h)),
                    showlegend = FALSE) |>
    plotly::config(displaylogo = FALSE)
}

# ---- 11. inequality plotly ------------------------------------------------
inequality_plotly <- function(master, title = NULL) {
  if (!requireNamespace("plotly", quietly = TRUE)) return(NULL)
  ensure_pkgs(c("dplyr", "tidyr"))
  ineq <- inequality_by_year(master)
  d <- ineq |>
    tidyr::pivot_longer(c("gini_eq", "gini_pop", "atk05", "atk1", "theil_pop"),
                        names_to = "metric", values_to = "value") |>
    dplyr::filter(is.finite(.data$value))
  plotly::plot_ly(d, x = ~year, y = ~value, color = ~metric,
                   type = "scatter", mode = "lines+markers",
                   hovertemplate = "%{x}: %{y:.3f}<extra></extra>") |>
    plotly::layout(
      title = list(text = title %||% "\u4e0d\u5e73\u7b49\u591a\u6307\u6807\u8d70\u52bf"),
      xaxis = list(title = "Year"),
      yaxis = list(title = "Index"),
      hovermode = "x unified",
      legend = list(orientation = "h", y = -0.2)
    ) |>
    plotly::config(displaylogo = FALSE)
}

# ---- 12. heatmap plotly ---------------------------------------------------
heatmap_oops_plotly <- function(master, top_n = 40, title = NULL) {
  if (!requireNamespace("plotly", quietly = TRUE)) return(NULL)
  ensure_pkgs(c("dplyr", "tidyr"))
  pool <- master |>
    dplyr::filter(.data$year == max(.data$year, na.rm = TRUE),
                  is.finite(.data$hf3_che)) |>
    dplyr::slice_max(.data$hf3_che, n = top_n) |>
    dplyr::pull(.data$iso3_code)
  d <- master |>
    dplyr::filter(.data$iso3_code %in% pool, is.finite(.data$hf3_che)) |>
    dplyr::select("year", "country_name", "hf3_che") |>
    tidyr::pivot_wider(names_from = "year", values_from = "hf3_che")
  year_cols <- sort(setdiff(names(d), "country_name"))
  z <- as.matrix(d[, year_cols, drop = FALSE])
  ord <- order(rowMeans(z, na.rm = TRUE))
  z <- z[ord, , drop = FALSE]
  countries <- d$country_name[ord]
  plotly::plot_ly(
    x = as.integer(year_cols), y = countries, z = z,
    type = "heatmap", colorscale = "YlOrRd",
    hovertemplate = "%{y} \u00b7 %{x}: %{z:.1f}%<extra></extra>"
  ) |>
    plotly::layout(
      title = list(text = title %||% sprintf(
        "Top-%d OOPS \u70ed\u529b\u4ea4\u4e92\u56fe", top_n)),
      xaxis = list(title = ""),
      yaxis = list(title = "")
    ) |>
    plotly::config(displaylogo = FALSE)
}

# ---- 13. wbplot world bank style line for country  ------------------------
country_wb_plotly <- function(master, iso = "CHN",
                                value_col = "che_pc_usd2023",
                                title = NULL) {
  if (!requireNamespace("plotly", quietly = TRUE)) return(NULL)
  ensure_pkgs("dplyr")
  d <- master |>
    dplyr::filter(.data$iso3_code == iso) |>
    dplyr::arrange(.data$year)
  if (nrow(d) == 0) return(NULL)
  cn <- d$country_name[1]
  plotly::plot_ly(d, x = ~year,
                   y = stats::as.formula(paste0("~", value_col)),
                   type = "scatter", mode = "lines+markers",
                   line = list(color = "#1B5E88", width = 3),
                   marker = list(color = "#1B5E88", size = 6),
                   hovertemplate = sprintf(
                     "<b>%s</b><br>%%{x}: %%{y:,.2f}<extra></extra>", cn)) |>
    plotly::layout(
      title = list(text = title %||% sprintf("%s \u00b7 %s", cn, value_col)),
      xaxis = list(title = "Year"),
      yaxis = list(title = value_col)
    ) |>
    plotly::config(displaylogo = FALSE)
}
