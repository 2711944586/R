# =============================================================================
# 程序/37_widgets_advanced.R   —— 高级交互组件（C2 阶段）
# -----------------------------------------------------------------------------
# 75 个 iadv_* 函数，覆盖 plotly / highcharter / echarts4r / reactable / DT /
#   networkD3 / crosstalk / htmltools 卡片。所有函数在包缺失时返回 NULL，
#   保证在最小依赖下不中断。
# 命名前缀：iadv_
# =============================================================================

if (!exists("ensure_pkgs", mode = "function")) {
  source(file.path("\u7a0b\u5e8f", "00_utils.R"))
}

.iadv_has  <- function(pkg) requireNamespace(pkg, quietly = TRUE)

.iadv_paper  <- "#fbf6ee"
.iadv_ink    <- "#1a1f28"
.iadv_primary <- "#1d3f5f"
.iadv_secondary <- "#c46327"
.iadv_good   <- "#2a857a"
.iadv_warn   <- "#c89a3b"
.iadv_bad    <- "#a23b3b"
.iadv_palette <- c(.iadv_primary, .iadv_secondary, .iadv_good,
                    .iadv_warn, .iadv_bad,
                    "#5b8aa6", "#7c5b9a", "#e0904c", "#86a062", "#b85578")

.iadv_layout <- function(p, title = NULL, ...) {
  if (!.iadv_has("plotly")) return(p)
  plotly::layout(p,
    title = if (is.null(title)) NULL else list(text = title,
                                                  x = 0, xanchor = "left",
                                                  font = list(size = 16,
                                                                color = .iadv_ink)),
    paper_bgcolor = .iadv_paper,
    plot_bgcolor = .iadv_paper,
    font = list(family = "PingFang SC, sans-serif",
                color = .iadv_ink),
    margin = list(t = 60, b = 60, l = 60, r = 30),
    ...)
}

# =============================================================================
# A. plotly time / series (15)
# =============================================================================

#' iadv1 \u4eba\u5747 CHE \u591a\u56fd\u65f6\u5e8f
iadv_che_pc_lines <- function(master,
                                isos = c("USA", "CHN", "JPN", "DEU",
                                          "BRA", "IND", "ZAF")) {
  if (!.iadv_has("plotly")) return(NULL)
  d <- master[master$iso3_code %in% isos &
              is.finite(master$che_pc_usd2023), ]
  if (!nrow(d)) return(NULL)
  p <- plotly::plot_ly(d, x = ~year, y = ~che_pc_usd2023,
    color = ~country_name, colors = .iadv_palette,
    type = "scatter", mode = "lines+markers",
    hovertemplate = paste0("<b>%{fullData.name}</b><br>",
                            "\u5e74: %{x}<br>$%{y:,.0f}<extra></extra>"))
  .iadv_layout(p, "\u4eba\u5747 CHE \u591a\u56fd\u65f6\u5e8f",
                xaxis = list(title = "\u5e74\u4efd"),
                yaxis = list(title = "USD2023 / \u4eba",
                              type = "log"))
}

#' iadv2 OOP \u5360 CHE \u591a\u56fd
iadv_oop_lines <- function(master,
                            isos = c("USA", "CHN", "IND", "BRA",
                                      "DEU", "JPN", "NGA", "EGY")) {
  if (!.iadv_has("plotly")) return(NULL)
  d <- master[master$iso3_code %in% isos &
              is.finite(master$hf3_che), ]
  if (!nrow(d)) return(NULL)
  p <- plotly::plot_ly(d, x = ~year, y = ~hf3_che,
    color = ~country_name, colors = .iadv_palette,
    type = "scatter", mode = "lines",
    line = list(width = 2.5))
  .iadv_layout(p, "OOP / CHE \u591a\u56fd\u65f6\u5e8f",
                xaxis = list(title = "\u5e74\u4efd"),
                yaxis = list(title = "OOP / CHE (%)",
                              ticksuffix = "%"))
}

#' iadv3 \u603b CHE \u5806\u53e0\u9762\u00b7\u6309\u5927\u6d32
iadv_che_total_stacked <- function(master) {
  if (!.iadv_has("plotly") || !.iadv_has("dplyr")) return(NULL)
  d <- master |>
    dplyr::filter(is.finite(.data$che_usd2023),
                  !is.na(.data$continent)) |>
    dplyr::group_by(.data$year, .data$continent) |>
    dplyr::summarise(che = sum(.data$che_usd2023, na.rm = TRUE) / 1e9,
                      .groups = "drop")
  if (!nrow(d)) return(NULL)
  p <- plotly::plot_ly(d, x = ~year, y = ~che,
    color = ~continent, colors = .iadv_palette,
    type = "scatter", mode = "none", stackgroup = "one",
    hovertemplate = "%{x} \u00b7 %{fullData.name}<br>$%{y:,.0f}B<extra></extra>")
  .iadv_layout(p, "\u5168\u7403\u603b CHE \u00b7 \u5806\u53e0\u9762\u79ef",
                xaxis = list(title = "\u5e74\u4efd"),
                yaxis = list(title = "\u603b CHE (USD2023, B)"))
}

#' iadv4 \u5168\u7403\u52a0\u6743\u5747\u503c
iadv_global_weighted_avg <- function(master) {
  if (!.iadv_has("plotly") || !.iadv_has("dplyr")) return(NULL)
  d <- master |>
    dplyr::filter(is.finite(.data$che_pc_usd2023),
                  is.finite(.data$pop)) |>
    dplyr::group_by(.data$year) |>
    dplyr::summarise(wavg = stats::weighted.mean(.data$che_pc_usd2023,
                                                  .data$pop, na.rm = TRUE),
                      median = stats::median(.data$che_pc_usd2023, na.rm = TRUE),
                      .groups = "drop")
  if (!nrow(d)) return(NULL)
  p <- plotly::plot_ly(d, x = ~year) |>
    plotly::add_trace(y = ~wavg, type = "scatter", mode = "lines+markers",
                       name = "\u4eba\u53e3\u52a0\u6743",
                       line = list(color = .iadv_primary, width = 3)) |>
    plotly::add_trace(y = ~median, type = "scatter", mode = "lines+markers",
                       name = "\u4e2d\u4f4d\u6570",
                       line = list(color = .iadv_secondary, width = 2,
                                     dash = "dot"))
  .iadv_layout(p, "\u5168\u7403\u4eba\u5747 CHE \u00b7 \u52a0\u6743\u4e0e\u4e2d\u4f4d\u5bf9\u6bd4",
                xaxis = list(title = "\u5e74\u4efd"),
                yaxis = list(title = "USD2023 / \u4eba"))
}

#' iadv5 \u9884\u671f\u5bff\u547d \u00b7 \u6536\u5165\u7ec4 facet
iadv_lifeexp_byinc <- function(master) {
  if (!.iadv_has("plotly") || !.iadv_has("dplyr")) return(NULL)
  d <- master |>
    dplyr::filter(is.finite(.data$life_exp),
                  !is.na(.data$income_group)) |>
    dplyr::group_by(.data$year, .data$income_group) |>
    dplyr::summarise(le = stats::median(.data$life_exp, na.rm = TRUE),
                      .groups = "drop")
  if (!nrow(d)) return(NULL)
  d$income_group <- factor(d$income_group,
    levels = c("Low income", "Lower middle income",
               "Upper middle income", "High income"))
  p <- plotly::plot_ly(d, x = ~year, y = ~le,
    color = ~income_group, colors = .iadv_palette,
    type = "scatter", mode = "lines+markers",
    line = list(width = 2.5))
  .iadv_layout(p, "\u9884\u671f\u5bff\u547d \u00b7 \u6536\u5165\u7ec4\u4e2d\u4f4d\u6570",
                xaxis = list(title = "\u5e74\u4efd"),
                yaxis = list(title = "\u9884\u671f\u5bff\u547d\uff08\u5e74\uff09"))
}

#' iadv6 U5MR \u5e74\u4efd\u591a\u56fd
iadv_u5mr_multi <- function(master,
                              isos = c("USA", "CHN", "IND", "NGA",
                                        "BRA", "ZAF", "DEU")) {
  if (!.iadv_has("plotly")) return(NULL)
  d <- master[master$iso3_code %in% isos &
              is.finite(master$u5mr), ]
  if (!nrow(d)) return(NULL)
  p <- plotly::plot_ly(d, x = ~year, y = ~u5mr,
    color = ~country_name, colors = .iadv_palette,
    type = "scatter", mode = "lines",
    line = list(width = 2.5))
  .iadv_layout(p, "U5MR \u00b7 \u591a\u56fd\u65f6\u5e8f",
                xaxis = list(title = "\u5e74\u4efd"),
                yaxis = list(title = "U5MR / 1000 \u6d3b\u4ea7",
                              type = "log"))
}

#' iadv7 \u53cc\u8f74 \u00b7 OOP \u4e0e\u5bff\u547d
iadv_dualaxis_oop_lifeexp <- function(master, iso = "BRA") {
  if (!.iadv_has("plotly")) return(NULL)
  d <- master[master$iso3_code == iso &
              is.finite(master$hf3_che) &
              is.finite(master$life_exp), ]
  if (!nrow(d)) return(NULL)
  p <- plotly::plot_ly() |>
    plotly::add_trace(data = d, x = ~year, y = ~hf3_che,
                       type = "scatter", mode = "lines+markers",
                       name = "OOP / CHE",
                       line = list(color = .iadv_secondary, width = 3),
                       yaxis = "y1") |>
    plotly::add_trace(data = d, x = ~year, y = ~life_exp,
                       type = "scatter", mode = "lines+markers",
                       name = "\u9884\u671f\u5bff\u547d",
                       line = list(color = .iadv_primary, width = 3),
                       yaxis = "y2") |>
    plotly::layout(
      yaxis = list(title = list(text = "OOP / CHE",
                                 font = list(color = .iadv_secondary))),
      yaxis2 = list(title = list(text = "\u5bff\u547d\uff08\u5e74\uff09",
                                  font = list(color = .iadv_primary)),
                     overlaying = "y", side = "right"))
  .iadv_layout(p, sprintf("%s \u00b7 OOP \u00d7 \u9884\u671f\u5bff\u547d \u53cc\u8f74", iso),
                xaxis = list(title = "\u5e74\u4efd"))
}

#' iadv8 plotly bar race-style (year frame, top 15)
iadv_bar_race <- function(master) {
  if (!.iadv_has("plotly") || !.iadv_has("dplyr")) return(NULL)
  d <- master |>
    dplyr::filter(is.finite(.data$che_pc_usd2023),
                  .data$year %in% seq(2000, max(master$year, na.rm = TRUE), 2)) |>
    dplyr::group_by(.data$year) |>
    dplyr::arrange(dplyr::desc(.data$che_pc_usd2023)) |>
    dplyr::slice_head(n = 15) |>
    dplyr::ungroup()
  if (!nrow(d)) return(NULL)
  p <- plotly::plot_ly(d, x = ~che_pc_usd2023,
    y = ~stats::reorder(country_name, che_pc_usd2023),
    frame = ~year, type = "bar", orientation = "h",
    marker = list(color = .iadv_primary,
                  line = list(color = .iadv_ink, width = 0.5)))
  .iadv_layout(p, "Top15 \u4eba\u5747 CHE \u00b7 year-frame",
                xaxis = list(title = "USD2023 / \u4eba", type = "log"),
                yaxis = list(title = NA))
}

#' iadv9 \u586b\u5145\u9762\u79ef \u00b7 HF1/HF2/HF3 \u5168\u7403\u5360\u6bd4
iadv_hf_share_area <- function(master) {
  if (!.iadv_has("plotly") || !.iadv_has("dplyr")) return(NULL)
  if (!all(c("hf1_che", "hf2_che", "hf3_che") %in% names(master))) return(NULL)
  d <- master |>
    dplyr::filter(is.finite(.data$hf1_che),
                  is.finite(.data$hf2_che),
                  is.finite(.data$hf3_che),
                  is.finite(.data$pop)) |>
    dplyr::group_by(.data$year) |>
    dplyr::summarise(
      public  = stats::weighted.mean(.data$hf1_che, .data$pop, na.rm = TRUE),
      private = stats::weighted.mean(.data$hf2_che, .data$pop, na.rm = TRUE),
      oop     = stats::weighted.mean(.data$hf3_che, .data$pop, na.rm = TRUE),
      .groups = "drop") |>
    tidyr::pivot_longer(-"year", names_to = "source",
                         values_to = "share")
  d$source <- factor(d$source,
                      levels = c("public", "private", "oop"),
                      labels = c("\u516c\u5171/\u793e\u4fdd",
                                  "\u79c1\u4eba\u4fdd\u9669",
                                  "\u5c45\u6c11\u81ea\u4ed8"))
  p <- plotly::plot_ly(d, x = ~year, y = ~share,
    color = ~source, colors = c(.iadv_primary, .iadv_secondary, .iadv_bad),
    type = "scatter", mode = "none", stackgroup = "one",
    groupnorm = "percent")
  .iadv_layout(p, "\u5168\u7403\u8d44\u91d1\u6765\u6e90\u5360\u6bd4 \u00b7 \u4eba\u53e3\u52a0\u6743",
                xaxis = list(title = "\u5e74\u4efd"),
                yaxis = list(title = "\u5360\u6bd4 (%)", ticksuffix = "%"))
}

#' iadv10 plotly waterfall \u00b7 \u4eba\u5747 CHE \u589e\u91cf\u9636\u68af
iadv_waterfall_che <- function(master,
                                 isos = c("USA", "CHN", "DEU", "JPN", "BRA")) {
  if (!.iadv_has("plotly")) return(NULL)
  yrs <- range(master$year, na.rm = TRUE)
  d1 <- master[master$year == yrs[1] & master$iso3_code %in% isos, ]
  d2 <- master[master$year == yrs[2] & master$iso3_code %in% isos, ]
  m <- merge(d1[, c("iso3_code", "country_name", "che_pc_usd2023")],
              d2[, c("iso3_code", "che_pc_usd2023")],
              by = "iso3_code", suffixes = c(".y1", ".y2"))
  m$delta <- m$che_pc_usd2023.y2 - m$che_pc_usd2023.y1
  if (!nrow(m)) return(NULL)
  p <- plotly::plot_ly(m, type = "waterfall",
    x = ~country_name, y = ~delta, measure = rep("relative", nrow(m)),
    text = ~sprintf("%+.0f", delta),
    decreasing = list(marker = list(color = .iadv_bad)),
    increasing = list(marker = list(color = .iadv_good)))
  .iadv_layout(p, sprintf("\u4eba\u5747 CHE \u589e\u91cf\u00b7 %d \u2192 %d", yrs[1], yrs[2]),
                yaxis = list(title = "\u0394 USD2023"))
}

#' iadv11 ribbon \u00b7 \u5168\u7403 25/50/75 \u5206\u4f4d\u5305\u7edc
iadv_ribbon_quantiles <- function(master) {
  if (!.iadv_has("plotly") || !.iadv_has("dplyr")) return(NULL)
  d <- master |>
    dplyr::filter(is.finite(.data$che_pc_usd2023)) |>
    dplyr::group_by(.data$year) |>
    dplyr::summarise(
      q25 = stats::quantile(.data$che_pc_usd2023, .25, na.rm = TRUE),
      q50 = stats::quantile(.data$che_pc_usd2023, .50, na.rm = TRUE),
      q75 = stats::quantile(.data$che_pc_usd2023, .75, na.rm = TRUE),
      q10 = stats::quantile(.data$che_pc_usd2023, .10, na.rm = TRUE),
      q90 = stats::quantile(.data$che_pc_usd2023, .90, na.rm = TRUE),
      .groups = "drop")
  if (!nrow(d)) return(NULL)
  p <- plotly::plot_ly() |>
    plotly::add_ribbons(x = d$year, ymin = d$q10, ymax = d$q90,
                         name = "10\u201390",
                         line = list(color = "transparent"),
                         fillcolor = "rgba(29,63,95,0.18)") |>
    plotly::add_ribbons(x = d$year, ymin = d$q25, ymax = d$q75,
                         name = "25\u201375",
                         line = list(color = "transparent"),
                         fillcolor = "rgba(29,63,95,0.32)") |>
    plotly::add_lines(x = d$year, y = d$q50,
                       name = "\u4e2d\u4f4d\u6570",
                       line = list(color = .iadv_primary, width = 3))
  .iadv_layout(p, "\u5168\u7403\u4eba\u5747 CHE \u5206\u4f4d\u4f4d\u5305\u7edc",
                xaxis = list(title = "\u5e74\u4efd"),
                yaxis = list(title = "USD2023 / \u4eba", type = "log"))
}

#' iadv12 plotly funnel \u00b7 \u8d44\u91d1\u4e09\u6bb5
iadv_funnel_sources <- function(master, year = NULL) {
  if (!.iadv_has("plotly")) return(NULL)
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year &
              is.finite(master$che_usd2023), ]
  if (!nrow(d)) return(NULL)
  tot <- sum(d$che_usd2023, na.rm = TRUE) / 1e9
  hf1 <- sum(d$che_usd2023 * d$hf1_che / 100, na.rm = TRUE) / 1e9
  hf2 <- sum(d$che_usd2023 * d$hf2_che / 100, na.rm = TRUE) / 1e9
  hf3 <- sum(d$che_usd2023 * d$hf3_che / 100, na.rm = TRUE) / 1e9
  p <- plotly::plot_ly(type = "funnel",
    y = c("\u603b CHE", "HF1 \u516c\u5171", "HF2 \u79c1\u4eba",
          "HF3 \u81ea\u4ed8"),
    x = c(tot, hf1, hf2, hf3),
    text = sprintf("$%.0f B", c(tot, hf1, hf2, hf3)),
    marker = list(color = c(.iadv_ink, .iadv_primary,
                             .iadv_secondary, .iadv_bad)))
  .iadv_layout(p, sprintf("\u5168\u7403\u8d44\u91d1\u6765\u6e90\u6f0f\u6597 \u00b7 %d", year))
}

#' iadv13 plotly heatmap \u00b7 \u5e74\u00d7\u6536\u5165\u7ec4
iadv_heatmap_year_inc <- function(master, var = "hf3_che") {
  if (!.iadv_has("plotly") || !.iadv_has("dplyr")) return(NULL)
  d <- master |>
    dplyr::filter(is.finite(.data[[var]]),
                  !is.na(.data$income_group)) |>
    dplyr::group_by(.data$year, .data$income_group) |>
    dplyr::summarise(v = stats::median(.data[[var]], na.rm = TRUE),
                      .groups = "drop") |>
    tidyr::pivot_wider(names_from = "income_group",
                        values_from = "v")
  if (!nrow(d)) return(NULL)
  mat <- as.matrix(d[, -1])
  rownames(mat) <- d$year
  p <- plotly::plot_ly(z = mat, x = colnames(mat), y = rownames(mat),
                        type = "heatmap", colorscale = "YlOrRd")
  .iadv_layout(p, sprintf("%s \u00b7 \u5e74\u00d7\u6536\u5165\u7ec4 \u70ed\u529b\u56fe", var),
                xaxis = list(title = NULL),
                yaxis = list(title = "\u5e74\u4efd"))
}

#' iadv14 plotly 3D \u00b7 GDP \u00d7 OOP \u00d7 \u5bff\u547d
iadv_3d_scatter <- function(master, year = NULL) {
  if (!.iadv_has("plotly")) return(NULL)
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year &
              is.finite(master$gdp_pc_usd) &
              is.finite(master$hf3_che) &
              is.finite(master$life_exp), ]
  if (!nrow(d)) return(NULL)
  p <- plotly::plot_ly(d, x = ~log10(gdp_pc_usd), y = ~hf3_che,
    z = ~life_exp, color = ~continent, colors = .iadv_palette,
    type = "scatter3d", mode = "markers",
    marker = list(size = 4, opacity = 0.85),
    text = ~country_name, hovertemplate = paste0(
      "<b>%{text}</b><br>log10 GDP/cap = %{x:.2f}<br>",
      "OOP = %{y:.1f}%%<br>\u5bff\u547d = %{z:.1f}<extra></extra>"))
  plotly::layout(p,
    title = list(text = sprintf("3D\u00b7GDP \u00d7 OOP \u00d7 \u5bff\u547d \u00b7 %d", year),
                  x = 0),
    scene = list(xaxis = list(title = "log10 GDP/cap"),
                  yaxis = list(title = "OOP / CHE"),
                  zaxis = list(title = "Life Exp")),
    paper_bgcolor = .iadv_paper,
    font = list(family = "PingFang SC, sans-serif"))
}

#' iadv15 \u591a\u56fd\u53e0\u52a0 \u00b7 latest year OOP \u6392\u5e8f
iadv_oop_rank_latest <- function(master, top_n = 25) {
  if (!.iadv_has("plotly")) return(NULL)
  yr <- max(master$year, na.rm = TRUE)
  d <- master[master$year == yr & is.finite(master$hf3_che), ]
  d <- d[order(-d$hf3_che), ]
  d <- utils::head(d, top_n)
  if (!nrow(d)) return(NULL)
  p <- plotly::plot_ly(d, x = ~hf3_che,
    y = ~stats::reorder(country_name, hf3_che),
    type = "bar", orientation = "h",
    marker = list(color = ~hf3_che, colorscale = "YlOrRd"),
    text = ~sprintf("%.1f%%", hf3_che),
    textposition = "outside")
  .iadv_layout(p, sprintf("\u9ad8 OOP \u524d %d \u56fd \u00b7 %d", top_n, yr),
                xaxis = list(title = "OOP / CHE (%)"),
                yaxis = list(title = NA))
}

# =============================================================================
# B. plotly distribution / structure (10)
# =============================================================================

#' iadv16 \u4ea7\u51fa\u5206\u5e03 violin \u00b7 \u6536\u5165\u7ec4
iadv_violin_inc <- function(master, year = NULL) {
  if (!.iadv_has("plotly")) return(NULL)
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year &
              is.finite(master$che_pc_usd2023) &
              !is.na(master$income_group), ]
  if (!nrow(d)) return(NULL)
  d$income_group <- factor(d$income_group,
    levels = c("Low income", "Lower middle income",
               "Upper middle income", "High income"))
  p <- plotly::plot_ly(d, x = ~income_group, y = ~che_pc_usd2023,
    color = ~income_group, colors = .iadv_palette,
    type = "violin", box = list(visible = TRUE), points = "all",
    pointpos = -0.5, jitter = 0.3)
  .iadv_layout(p, sprintf("\u4eba\u5747 CHE \u00b7 violin \u00d7 \u6536\u5165\u7ec4 \u00b7 %d", year),
                yaxis = list(title = "USD2023 / \u4eba", type = "log"))
}

#' iadv17 \u7bb1\u7ebf\u00b7\u5927\u6d32
iadv_box_continent <- function(master, year = NULL, var = "che_pc_usd2023") {
  if (!.iadv_has("plotly")) return(NULL)
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year &
              is.finite(master[[var]]) &
              !is.na(master$continent), ]
  if (!nrow(d)) return(NULL)
  p <- plotly::plot_ly(d, x = ~continent, y = ~.data[[var]],
                        color = ~continent, colors = .iadv_palette,
                        type = "box", boxpoints = "all",
                        jitter = 0.4, pointpos = 0)
  .iadv_layout(p, sprintf("%s \u00b7 \u5927\u6d32\u7bb1\u7ebf \u00b7 %d", var, year),
                yaxis = list(title = var,
                              type = if (var == "che_pc_usd2023") "log" else "linear"))
}

#' iadv18 plotly histogram \u00b7 OOP
iadv_hist_oop <- function(master, year = NULL) {
  if (!.iadv_has("plotly")) return(NULL)
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year & is.finite(master$hf3_che), ]
  if (!nrow(d)) return(NULL)
  p <- plotly::plot_ly(d, x = ~hf3_che,
                        type = "histogram", nbinsx = 30,
                        marker = list(color = .iadv_secondary,
                                       line = list(color = .iadv_ink, width = 0.5)))
  .iadv_layout(p, sprintf("OOP \u5206\u5e03 \u00b7 %d \u5e74", year),
                xaxis = list(title = "OOP / CHE (%)"),
                yaxis = list(title = "\u56fd\u5bb6\u6570"))
}

#' iadv19 plotly density \u00b7 \u4eba\u5747 CHE
iadv_density_che <- function(master, years = c(2000, 2010, 2022)) {
  if (!.iadv_has("plotly")) return(NULL)
  d <- master[master$year %in% years & is.finite(master$che_pc_usd2023), ]
  if (!nrow(d)) return(NULL)
  d$lche <- log10(pmax(d$che_pc_usd2023, 1))
  p <- plotly::plot_ly()
  cols <- .iadv_palette[seq_along(years)]
  for (i in seq_along(years)) {
    sub <- d[d$year == years[i], ]
    if (nrow(sub) > 5) {
      dens <- stats::density(sub$lche, na.rm = TRUE)
      p <- plotly::add_trace(p, x = dens$x, y = dens$y,
                              type = "scatter", mode = "lines",
                              name = as.character(years[i]),
                              line = list(color = cols[i], width = 2.5),
                              fill = "tozeroy",
                              fillcolor = paste0(cols[i], "33"))
    }
  }
  .iadv_layout(p, "\u4eba\u5747 CHE \u00b7 \u5bc6\u5ea6\u591a\u5e74\u5bf9\u6bd4",
                xaxis = list(title = "log10 USD2023/\u4eba"),
                yaxis = list(title = "\u5bc6\u5ea6"))
}

#' iadv20 plotly sunburst \u00b7 \u5927\u6d32 \u00b7 \u6536\u5165\u7ec4 \u00b7 \u56fd\u5bb6
iadv_sunburst_che <- function(master, year = NULL) {
  if (!.iadv_has("plotly") || !.iadv_has("dplyr")) return(NULL)
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year &
              is.finite(master$che_usd2023) &
              !is.na(master$continent) &
              !is.na(master$income_group), ]
  if (!nrow(d)) return(NULL)
  labels <- c("\u5168\u7403",
              unique(d$continent),
              unique(paste(d$continent, d$income_group, sep = "/")),
              d$country_name)
  parents <- c("",
               rep("\u5168\u7403", length(unique(d$continent))),
               vapply(unique(paste(d$continent, d$income_group, sep = "/")),
                       function(x) strsplit(x, "/")[[1]][1],
                       character(1)),
               paste(d$continent, d$income_group, sep = "/"))
  values <- c(sum(d$che_usd2023, na.rm = TRUE),
              vapply(unique(d$continent), function(c)
                sum(d$che_usd2023[d$continent == c], na.rm = TRUE),
                numeric(1)),
              vapply(unique(paste(d$continent, d$income_group, sep = "/")),
                      function(k) {
                        parts <- strsplit(k, "/")[[1]]
                        sum(d$che_usd2023[d$continent == parts[1] &
                                           d$income_group == parts[2]],
                            na.rm = TRUE)
                      }, numeric(1)),
              d$che_usd2023)
  p <- plotly::plot_ly(type = "sunburst",
                        labels = labels, parents = parents, values = values,
                        branchvalues = "total",
                        marker = list(colors = .iadv_palette))
  .iadv_layout(p, sprintf("\u8d44\u91d1\u65ed\u65e5 \u00b7 \u5927\u6d32-\u6536\u5165-\u56fd \u00b7 %d", year))
}

#' iadv21 plotly treemap \u00b7 \u603b CHE
iadv_treemap_che <- function(master, year = NULL) {
  if (!.iadv_has("plotly")) return(NULL)
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year &
              is.finite(master$che_usd2023) &
              !is.na(master$continent), ]
  d <- d[order(-d$che_usd2023), ]
  d <- utils::head(d, 60)
  if (!nrow(d)) return(NULL)
  labels <- c("\u5168\u7403", unique(d$continent), d$country_name)
  parents <- c("", rep("\u5168\u7403", length(unique(d$continent))),
                d$continent)
  values <- c(sum(d$che_usd2023, na.rm = TRUE) / 1e9,
              vapply(unique(d$continent), function(c)
                sum(d$che_usd2023[d$continent == c], na.rm = TRUE) / 1e9,
                numeric(1)),
              d$che_usd2023 / 1e9)
  p <- plotly::plot_ly(type = "treemap",
                        labels = labels, parents = parents, values = values,
                        branchvalues = "total",
                        textinfo = "label+value")
  .iadv_layout(p, sprintf("\u603b CHE Treemap \u00b7 Top60 \u00b7 %d", year))
}

#' iadv22 plotly parallel coordinates
iadv_parcoords <- function(master, year = NULL) {
  if (!.iadv_has("plotly")) return(NULL)
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  cols <- intersect(c("che_pc_usd2023", "hf3_che", "gghed_che",
                       "ext_che", "life_exp", "u5mr"),
                     names(master))
  d <- master[master$year == year, c("iso3_code", "country_name",
                                        "continent", cols)]
  d <- d[stats::complete.cases(d), ]
  if (!nrow(d)) return(NULL)
  d$continent_id <- as.integer(factor(d$continent))
  dims <- lapply(cols, function(cc)
    list(label = cc, values = d[[cc]]))
  p <- plotly::plot_ly(d, type = "parcoords",
    line = list(color = ~continent_id, colorscale = "Viridis",
                  showscale = FALSE),
    dimensions = dims)
  .iadv_layout(p, sprintf("\u591a\u53d8\u91cf\u5e73\u884c\u5750\u6807 \u00b7 %d", year))
}

#' iadv23 plotly polar \u00b7 6 \u5927\u6d32\u591a\u8f74
iadv_polar_radar <- function(master, year = NULL) {
  if (!.iadv_has("plotly") || !.iadv_has("dplyr")) return(NULL)
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  cols <- intersect(c("che_pc_usd2023", "hf3_che", "life_exp",
                       "u5mr", "gghed_che", "pop"),
                     names(master))
  d <- master |>
    dplyr::filter(.data$year == .env$year, !is.na(.data$continent)) |>
    dplyr::group_by(.data$continent) |>
    dplyr::summarise(dplyr::across(dplyr::all_of(cols),
                                    function(x) stats::median(x, na.rm = TRUE)),
                      .groups = "drop")
  if (!nrow(d)) return(NULL)
  for (cc in cols) {
    v <- d[[cc]]
    rng <- range(v, na.rm = TRUE)
    if (diff(rng) <= 0) next
    sign <- if (cc == "u5mr") -1 else 1
    d[[cc]] <- (v - rng[1]) / diff(rng)
    if (sign < 0) d[[cc]] <- 1 - d[[cc]]
  }
  p <- plotly::plot_ly(type = "scatterpolar")
  cols_pal <- .iadv_palette[seq_len(nrow(d))]
  for (i in seq_len(nrow(d))) {
    p <- plotly::add_trace(p, type = "scatterpolar",
      r = c(unlist(d[i, cols]), d[[cols[1]]][i]),
      theta = c(cols, cols[1]),
      name = d$continent[i], fill = "toself",
      fillcolor = paste0(cols_pal[i], "33"),
      line = list(color = cols_pal[i], width = 2))
  }
  plotly::layout(p, polar = list(radialaxis = list(visible = TRUE,
                                                     range = c(0, 1))),
    title = sprintf("\u591a\u8f74\u96f7\u8fbe\u00b7\u5927\u6d32\u4e2d\u4f4d\u6570 \u00b7 %d", year),
    paper_bgcolor = .iadv_paper,
    font = list(family = "PingFang SC, sans-serif"))
}

#' iadv24 plotly bubble matrix scatter
iadv_bubble_matrix <- function(master, year = NULL) {
  if (!.iadv_has("plotly")) return(NULL)
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year &
              is.finite(master$che_pc_usd2023) &
              is.finite(master$life_exp) &
              is.finite(master$pop), ]
  if (!nrow(d)) return(NULL)
  p <- plotly::plot_ly(d, x = ~che_pc_usd2023, y = ~life_exp,
    size = ~pop, sizes = c(8, 60),
    color = ~continent, colors = .iadv_palette,
    text = ~country_name,
    type = "scatter", mode = "markers",
    marker = list(opacity = 0.7, line = list(color = .iadv_ink, width = 0.5)),
    hovertemplate = paste0("<b>%{text}</b><br>CHE = $%{x:,.0f}<br>",
                            "\u5bff\u547d = %{y:.1f}<extra></extra>"))
  .iadv_layout(p, sprintf("\u4eba\u5747 CHE \u00d7 \u5bff\u547d \u00b7 \u4eba\u53e3\u5bf9\u6570 \u00b7 %d", year),
                xaxis = list(title = "USD2023 / \u4eba", type = "log"),
                yaxis = list(title = "\u9884\u671f\u5bff\u547d"))
}

#' iadv25 \u9879\u76ee\u72b6\u6001\u7ed3\u6784 \u00b7 plotly icicle
iadv_icicle <- function(master, year = NULL) {
  if (!.iadv_has("plotly")) return(NULL)
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year &
              is.finite(master$che_usd2023), ]
  d <- d[order(-d$che_usd2023), ]
  d <- utils::head(d, 50)
  if (!nrow(d)) return(NULL)
  labels <- c("\u5168\u7403", unique(d$continent), d$country_name)
  parents <- c("", rep("\u5168\u7403", length(unique(d$continent))),
                d$continent)
  values <- c(sum(d$che_usd2023, na.rm = TRUE) / 1e9,
              vapply(unique(d$continent), function(c)
                sum(d$che_usd2023[d$continent == c], na.rm = TRUE) / 1e9,
                numeric(1)),
              d$che_usd2023 / 1e9)
  p <- plotly::plot_ly(type = "icicle",
    labels = labels, parents = parents, values = values,
    branchvalues = "total")
  .iadv_layout(p, sprintf("Icicle \u00b7 \u603b CHE \u00b7 %d", year))
}

# =============================================================================
# C. reactable \u8868 (10)
# =============================================================================

.iadv_sparkline_html <- function(values, color = .iadv_primary, w = 80, h = 24) {
  values <- values[is.finite(values)]
  if (length(values) < 2) return("")
  vmin <- min(values); vmax <- max(values)
  if (vmax == vmin) return("")
  x <- seq(0, w, length.out = length(values))
  y <- h - (values - vmin) / (vmax - vmin) * (h - 4) - 2
  pts <- paste(sprintf("%.1f,%.1f", x, y), collapse = " ")
  sprintf(
    paste0('<svg width="%d" height="%d" style="vertical-align:middle">',
            '<polyline points="%s" fill="none" stroke="%s" ',
            'stroke-width="1.5"/></svg>'),
    w, h, pts, color)
}

#' iadv26 reactable \u00b7 \u4eba\u5747 CHE Top 50 \u6392\u540d\u8868
iadv_rt_top_che_pc <- function(master, year = NULL, top_n = 50) {
  if (!.iadv_has("reactable")) return(NULL)
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year &
              is.finite(master$che_pc_usd2023), ]
  d <- d[order(-d$che_pc_usd2023), ]
  d <- utils::head(d, top_n)
  if (!nrow(d)) return(NULL)
  spark <- vapply(d$iso3_code, function(iso) {
    sub <- master[master$iso3_code == iso &
                  is.finite(master$che_pc_usd2023), ]
    sub <- sub[order(sub$year), ]
    .iadv_sparkline_html(sub$che_pc_usd2023)
  }, character(1))
  df <- data.frame(
    rank      = seq_len(nrow(d)),
    country   = d$country_name,
    iso       = d$iso3_code,
    continent = d$continent,
    che_pc    = d$che_pc_usd2023,
    oop       = d$hf3_che,
    life_exp  = d$life_exp,
    trend     = spark,
    stringsAsFactors = FALSE
  )
  reactable::reactable(df,
    pagination = TRUE, defaultPageSize = 15,
    searchable = TRUE, striped = TRUE, highlight = TRUE,
    columns = list(
      rank      = reactable::colDef(name = "\u6392\u540d", maxWidth = 60),
      country   = reactable::colDef(name = "\u56fd\u5bb6",
                                      minWidth = 140),
      iso       = reactable::colDef(name = "ISO", maxWidth = 70),
      continent = reactable::colDef(name = "\u5927\u6d32",
                                      maxWidth = 100),
      che_pc    = reactable::colDef(name = "\u4eba\u5747 CHE",
                                      format = reactable::colFormat(
                                        prefix = "$", separators = TRUE,
                                        digits = 0)),
      oop       = reactable::colDef(name = "OOP %", format =
                                      reactable::colFormat(digits = 1,
                                                            suffix = "%")),
      life_exp  = reactable::colDef(name = "\u5bff\u547d", format =
                                      reactable::colFormat(digits = 1)),
      trend     = reactable::colDef(name = "\u8d8b\u52bf",
                                      html = TRUE, sortable = FALSE,
                                      minWidth = 100)),
    theme = reactable::reactableTheme(
      borderColor = "#d8c89e", stripedColor = "#f4ecdb",
      highlightColor = "#fbe5c8", style = list(fontFamily = "PingFang SC")))
}

#' iadv27 reactable \u00b7 OOP \u9ad8/\u4f4e\u53cc\u9762\u677f
iadv_rt_oop_extremes <- function(master, year = NULL, n_each = 20) {
  if (!.iadv_has("reactable")) return(NULL)
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year & is.finite(master$hf3_che), ]
  if (!nrow(d)) return(NULL)
  d <- d[order(-d$hf3_che), ]
  top <- utils::head(d, n_each); top$flag <- "\u9ad8"
  bot <- utils::tail(d, n_each); bot$flag <- "\u4f4e"
  comb <- rbind(top, bot)
  df <- data.frame(
    flag      = comb$flag,
    country   = comb$country_name,
    continent = comb$continent,
    oop       = comb$hf3_che,
    che_pc    = comb$che_pc_usd2023,
    life_exp  = comb$life_exp,
    stringsAsFactors = FALSE)
  reactable::reactable(df,
    pagination = TRUE, defaultPageSize = 20,
    searchable = TRUE, striped = TRUE,
    groupBy = "flag",
    columns = list(
      flag      = reactable::colDef(name = "\u7c7b", maxWidth = 60),
      country   = reactable::colDef(name = "\u56fd\u5bb6"),
      continent = reactable::colDef(name = "\u5927\u6d32",
                                      maxWidth = 100),
      oop       = reactable::colDef(name = "OOP %",
        format = reactable::colFormat(digits = 1, suffix = "%"),
        style = function(value) {
          col <- if (value > 50) "#a23b3b" else if (value > 30) "#c46327"
                  else if (value > 15) "#c89a3b" else "#2a857a"
          list(color = col, fontWeight = "600")
        }),
      che_pc    = reactable::colDef(name = "\u4eba\u5747 CHE",
        format = reactable::colFormat(prefix = "$", digits = 0,
                                        separators = TRUE)),
      life_exp  = reactable::colDef(name = "\u5bff\u547d",
        format = reactable::colFormat(digits = 1))),
    theme = reactable::reactableTheme(
      borderColor = "#d8c89e", style = list(fontFamily = "PingFang SC")))
}

#' iadv28 reactable \u00b7 \u5927\u6d32\u804a\u7c4d\u6c47\u603b
iadv_rt_continent_summary <- function(master, year = NULL) {
  if (!.iadv_has("reactable") || !.iadv_has("dplyr")) return(NULL)
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master |>
    dplyr::filter(.data$year == .env$year, !is.na(.data$continent)) |>
    dplyr::group_by(.data$continent) |>
    dplyr::summarise(
      n_country = dplyr::n(),
      che_total = sum(.data$che_usd2023, na.rm = TRUE) / 1e9,
      che_pc_med = stats::median(.data$che_pc_usd2023, na.rm = TRUE),
      oop_med   = stats::median(.data$hf3_che, na.rm = TRUE),
      life_med  = stats::median(.data$life_exp, na.rm = TRUE),
      u5mr_med  = stats::median(.data$u5mr, na.rm = TRUE),
      .groups = "drop")
  reactable::reactable(d,
    pagination = FALSE, striped = TRUE,
    columns = list(
      continent  = reactable::colDef(name = "\u5927\u6d32"),
      n_country  = reactable::colDef(name = "\u56fd\u5bb6\u6570",
                                       maxWidth = 90),
      che_total  = reactable::colDef(name = "\u603b CHE (B$)",
        format = reactable::colFormat(digits = 0, separators = TRUE)),
      che_pc_med = reactable::colDef(name = "CHE\u4eba\u5747(\u4e2d)",
        format = reactable::colFormat(prefix = "$", digits = 0)),
      oop_med    = reactable::colDef(name = "OOP%(\u4e2d)",
        format = reactable::colFormat(digits = 1, suffix = "%")),
      life_med   = reactable::colDef(name = "\u5bff\u547d(\u4e2d)",
        format = reactable::colFormat(digits = 1)),
      u5mr_med   = reactable::colDef(name = "U5MR(\u4e2d)",
        format = reactable::colFormat(digits = 1))),
    theme = reactable::reactableTheme(
      borderColor = "#d8c89e", style = list(fontFamily = "PingFang SC")))
}

#' iadv29 reactable \u00b7 \u6536\u5165\u7ec4\u804a\u7c4d
iadv_rt_income_summary <- function(master, year = NULL) {
  if (!.iadv_has("reactable") || !.iadv_has("dplyr")) return(NULL)
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master |>
    dplyr::filter(.data$year == .env$year, !is.na(.data$income_group)) |>
    dplyr::group_by(.data$income_group) |>
    dplyr::summarise(
      n         = dplyr::n(),
      pop_total = sum(.data$pop, na.rm = TRUE) / 1e6,
      che_pc    = stats::weighted.mean(.data$che_pc_usd2023,
                                        .data$pop, na.rm = TRUE),
      oop       = stats::weighted.mean(.data$hf3_che,
                                        .data$pop, na.rm = TRUE),
      life_exp  = stats::weighted.mean(.data$life_exp,
                                        .data$pop, na.rm = TRUE),
      .groups = "drop")
  reactable::reactable(d, pagination = FALSE, striped = TRUE,
    columns = list(
      income_group = reactable::colDef(name = "\u6536\u5165\u7ec4"),
      n            = reactable::colDef(name = "\u56fd\u5bb6\u6570",
                                         maxWidth = 90),
      pop_total    = reactable::colDef(name = "\u603b\u4eba\u53e3 (M)",
        format = reactable::colFormat(digits = 0, separators = TRUE)),
      che_pc       = reactable::colDef(name = "CHE\u4eba\u5747\u00b7\u52a0\u6743",
        format = reactable::colFormat(prefix = "$", digits = 0)),
      oop          = reactable::colDef(name = "OOP%\u00b7\u52a0\u6743",
        format = reactable::colFormat(digits = 1, suffix = "%")),
      life_exp     = reactable::colDef(name = "\u5bff\u547d\u00b7\u52a0\u6743",
        format = reactable::colFormat(digits = 1))),
    theme = reactable::reactableTheme(borderColor = "#d8c89e",
      style = list(fontFamily = "PingFang SC")))
}

#' iadv30 reactable \u00b7 \u589e\u957f\u51a0\u519b\u8868\u00b7\u542b\u8f6e\u8be2 spark
iadv_rt_growth_champions <- function(master) {
  if (!.iadv_has("reactable") || !.iadv_has("dplyr")) return(NULL)
  yrs <- range(master$year, na.rm = TRUE)
  base <- master[master$year == yrs[1],
                  c("iso3_code", "country_name",
                     "che_pc_usd2023")]
  late <- master[master$year == yrs[2],
                  c("iso3_code", "che_pc_usd2023")]
  m <- merge(base, late, by = "iso3_code",
              suffixes = c(".y1", ".y2"))
  span <- yrs[2] - yrs[1]
  m$cagr <- (m$che_pc_usd2023.y2 / m$che_pc_usd2023.y1) ^ (1 / span) - 1
  m <- m[is.finite(m$cagr) & m$cagr > 0, ]
  m <- m[order(-m$cagr), ]
  m <- utils::head(m, 50)
  spark <- vapply(m$iso3_code, function(iso) {
    s <- master[master$iso3_code == iso &
                is.finite(master$che_pc_usd2023), ]
    s <- s[order(s$year), ]
    .iadv_sparkline_html(s$che_pc_usd2023, .iadv_good)
  }, character(1))
  df <- data.frame(
    rank    = seq_len(nrow(m)),
    country = m$country_name,
    cagr    = m$cagr * 100,
    y1      = m$che_pc_usd2023.y1,
    y2      = m$che_pc_usd2023.y2,
    trend   = spark,
    stringsAsFactors = FALSE)
  reactable::reactable(df, pagination = TRUE,
    defaultPageSize = 15, searchable = TRUE, striped = TRUE,
    columns = list(
      rank    = reactable::colDef(name = "\u6392\u540d",
                                    maxWidth = 60),
      country = reactable::colDef(name = "\u56fd\u5bb6"),
      cagr    = reactable::colDef(name = "CAGR %",
                                    format = reactable::colFormat(digits = 2,
                                                                    suffix = "%")),
      y1      = reactable::colDef(name = sprintf("%d $", yrs[1]),
                                    format = reactable::colFormat(prefix = "$",
                                                                    digits = 0)),
      y2      = reactable::colDef(name = sprintf("%d $", yrs[2]),
                                    format = reactable::colFormat(prefix = "$",
                                                                    digits = 0)),
      trend   = reactable::colDef(name = "\u8f68\u8ff9",
                                    html = TRUE, sortable = FALSE)),
    theme = reactable::reactableTheme(borderColor = "#d8c89e",
      style = list(fontFamily = "PingFang SC")))
}

#' iadv31 reactable \u00b7 \u5e74\u4efd\u00d7\u5927\u6d32\u00b7\u9762\u4eba CHE \u53d8\u5316
iadv_rt_change_by_continent <- function(master) {
  if (!.iadv_has("reactable") || !.iadv_has("dplyr")) return(NULL)
  d <- master |>
    dplyr::filter(is.finite(.data$che_pc_usd2023),
                  !is.na(.data$continent),
                  .data$year %in% c(2000, 2010, 2022)) |>
    dplyr::group_by(.data$year, .data$continent) |>
    dplyr::summarise(che_pc = stats::median(.data$che_pc_usd2023,
                                              na.rm = TRUE),
                      .groups = "drop") |>
    tidyr::pivot_wider(names_from = "year", values_from = "che_pc",
                        names_prefix = "y_")
  reactable::reactable(d, pagination = FALSE, striped = TRUE,
    columns = list(
      continent = reactable::colDef(name = "\u5927\u6d32"),
      y_2000 = reactable::colDef(name = "2000 \u4e2d\u4f4d",
        format = reactable::colFormat(prefix = "$", digits = 0)),
      y_2010 = reactable::colDef(name = "2010 \u4e2d\u4f4d",
        format = reactable::colFormat(prefix = "$", digits = 0)),
      y_2022 = reactable::colDef(name = "2022 \u4e2d\u4f4d",
        format = reactable::colFormat(prefix = "$", digits = 0))),
    theme = reactable::reactableTheme(borderColor = "#d8c89e",
      style = list(fontFamily = "PingFang SC")))
}

#' iadv32 reactable \u00b7 \u51b2\u51fb\u54cd\u5e94\u8868
iadv_rt_shock_response <- function(master) {
  if (!.iadv_has("reactable")) return(NULL)
  yrs <- c(2007, 2009, 2019, 2021)
  if (!all(yrs %in% master$year)) return(NULL)
  d <- master[master$year %in% yrs &
              is.finite(master$che_pc_usd2023), ]
  spl <- split(d, d$iso3_code)
  rows <- lapply(spl, function(g) {
    if (nrow(g) < 4) return(NULL)
    g <- g[match(yrs, g$year), ]
    if (any(is.na(g$che_pc_usd2023))) return(NULL)
    data.frame(
      country = g$country_name[1],
      continent = g$continent[1],
      gfc_impact   = (g$che_pc_usd2023[2] - g$che_pc_usd2023[1]) /
                      g$che_pc_usd2023[1] * 100,
      covid_impact = (g$che_pc_usd2023[4] - g$che_pc_usd2023[3]) /
                      g$che_pc_usd2023[3] * 100,
      stringsAsFactors = FALSE)
  })
  df <- do.call(rbind, rows)
  if (is.null(df) || !nrow(df)) return(NULL)
  df <- df[order(df$covid_impact), ]
  reactable::reactable(df, pagination = TRUE, defaultPageSize = 20,
    searchable = TRUE, striped = TRUE,
    columns = list(
      country      = reactable::colDef(name = "\u56fd\u5bb6"),
      continent    = reactable::colDef(name = "\u5927\u6d32",
                                          maxWidth = 100),
      gfc_impact   = reactable::colDef(name = "GFC 07\u219209 %",
        format = reactable::colFormat(digits = 1, suffix = "%"),
        style = function(v) list(color = if (v < 0) "#a23b3b" else "#2a857a")),
      covid_impact = reactable::colDef(name = "COVID 19\u219221 %",
        format = reactable::colFormat(digits = 1, suffix = "%"),
        style = function(v) list(color = if (v < 0) "#a23b3b" else "#2a857a"))),
    theme = reactable::reactableTheme(borderColor = "#d8c89e",
      style = list(fontFamily = "PingFang SC")))
}

#' iadv33 reactable \u00b7 \u8d22\u52a1\u4fdd\u62a4\u6307\u6570\u8868
iadv_rt_finprot <- function(master, year = NULL) {
  if (!.iadv_has("reactable")) return(NULL)
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year &
              is.finite(master$hf3_che) &
              is.finite(master$gghed_che), ]
  d$score <- 100 - 0.7 * d$hf3_che + 0.3 * d$gghed_che
  d <- d[order(-d$score), ]
  d <- utils::head(d, 50)
  df <- data.frame(
    rank      = seq_len(nrow(d)),
    country   = d$country_name,
    continent = d$continent,
    score     = d$score,
    oop       = d$hf3_che,
    gghed     = d$gghed_che,
    stringsAsFactors = FALSE)
  reactable::reactable(df, pagination = TRUE,
    defaultPageSize = 15, searchable = TRUE, striped = TRUE,
    columns = list(
      rank      = reactable::colDef(name = "\u6392\u540d", maxWidth = 60),
      country   = reactable::colDef(name = "\u56fd\u5bb6"),
      continent = reactable::colDef(name = "\u5927\u6d32", maxWidth = 100),
      score     = reactable::colDef(name = "FinProt \u5f97\u5206",
        format = reactable::colFormat(digits = 1),
        style = function(v) list(fontWeight = "600",
                                  color = if (v > 80) "#2a857a"
                                          else if (v > 60) "#c89a3b"
                                          else "#a23b3b")),
      oop       = reactable::colDef(name = "OOP %",
        format = reactable::colFormat(digits = 1, suffix = "%")),
      gghed     = reactable::colDef(name = "GGHED %",
        format = reactable::colFormat(digits = 1, suffix = "%"))),
    theme = reactable::reactableTheme(borderColor = "#d8c89e",
      style = list(fontFamily = "PingFang SC")))
}

#' iadv34 reactable \u00b7 \u8de8\u5e74\u5bf9\u6bd4
iadv_rt_compare_years <- function(master, isos = c("USA", "CHN",
                                                      "JPN", "DEU",
                                                      "GBR", "IND",
                                                      "BRA"),
                                     years = c(2000, 2010, 2022)) {
  if (!.iadv_has("reactable")) return(NULL)
  d <- master[master$iso3_code %in% isos &
              master$year %in% years &
              is.finite(master$che_pc_usd2023), ]
  if (!nrow(d)) return(NULL)
  d2 <- stats::aggregate(che_pc_usd2023 ~ iso3_code + country_name + year,
                          data = d, FUN = mean)
  w <- tidyr::pivot_wider(d2, names_from = "year",
                            values_from = "che_pc_usd2023",
                            names_prefix = "y_")
  reactable::reactable(w, pagination = FALSE, striped = TRUE,
    columns = stats::setNames(
      lapply(names(w), function(n) {
        if (startsWith(n, "y_"))
          reactable::colDef(name = sub("y_", "", n),
                              format = reactable::colFormat(prefix = "$",
                                                              digits = 0))
        else reactable::colDef(name = n)
      }), names(w)),
    theme = reactable::reactableTheme(borderColor = "#d8c89e",
      style = list(fontFamily = "PingFang SC")))
}

#' iadv35 reactable \u00b7 \u672a\u8fbe\u6807\u62a5\u8b66
iadv_rt_below_threshold <- function(master, year = NULL,
                                       thr_oop = 30, thr_gghed = 50) {
  if (!.iadv_has("reactable")) return(NULL)
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year &
              is.finite(master$hf3_che) &
              is.finite(master$gghed_che), ]
  d$alert <- d$hf3_che > thr_oop | d$gghed_che < thr_gghed
  alert <- d[d$alert, ]
  if (!nrow(alert)) return(NULL)
  alert <- alert[order(-alert$hf3_che), ]
  df <- data.frame(
    country   = alert$country_name,
    continent = alert$continent,
    income    = alert$income_group,
    oop       = alert$hf3_che,
    gghed     = alert$gghed_che,
    stringsAsFactors = FALSE)
  reactable::reactable(df, pagination = TRUE, defaultPageSize = 20,
    searchable = TRUE, striped = TRUE,
    columns = list(
      country   = reactable::colDef(name = "\u56fd\u5bb6"),
      continent = reactable::colDef(name = "\u5927\u6d32",
                                       maxWidth = 100),
      income    = reactable::colDef(name = "\u6536\u5165\u7ec4",
                                       maxWidth = 140),
      oop       = reactable::colDef(name = "OOP %",
        format = reactable::colFormat(digits = 1, suffix = "%"),
        style = function(v) list(color = if (v > thr_oop) "#a23b3b"
                                          else "#0d121b",
                                  fontWeight = "600")),
      gghed     = reactable::colDef(name = "GGHED %",
        format = reactable::colFormat(digits = 1, suffix = "%"),
        style = function(v) list(color = if (v < thr_gghed) "#a23b3b"
                                          else "#0d121b",
                                  fontWeight = "600"))),
    theme = reactable::reactableTheme(borderColor = "#d8c89e",
      style = list(fontFamily = "PingFang SC")))
}

# =============================================================================
# D. DT \u8868 (10)
# =============================================================================

.iadv_dt_opts <- function() {
  list(pageLength = 15, lengthMenu = c(10, 15, 25, 50),
       searchHighlight = TRUE, scrollX = TRUE,
       dom = "Bfrtip", buttons = c("copy", "csv", "excel"))
}

#' iadv36 DT \u00b7 \u5168\u5b57\u6bb5\u6d4f\u89c8
iadv_dt_master_browse <- function(master, year = NULL) {
  if (!.iadv_has("DT")) return(NULL)
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  cols <- intersect(c("country_name", "iso3_code", "continent",
                       "income_group", "year", "che_pc_usd2023",
                       "hf3_che", "gghed_che", "ext_che",
                       "life_exp", "u5mr", "pop", "gdp_pc_usd"),
                     names(master))
  d <- master[master$year == year, cols]
  DT::datatable(d, rownames = FALSE,
    options = .iadv_dt_opts(),
    extensions = c("Buttons")) |>
    DT::formatCurrency(intersect(c("che_pc_usd2023", "gdp_pc_usd"), cols),
                       currency = "$", digits = 0) |>
    DT::formatRound(intersect(c("hf3_che", "gghed_che",
                                  "ext_che", "life_exp"), cols), digits = 1) |>
    DT::formatRound(intersect("u5mr", cols), digits = 1) |>
    DT::formatRound(intersect("pop", cols), digits = 0)
}

#' iadv37 DT \u00b7 \u540c\u671f\u4e09\u5e74\u5bf9\u6bd4
iadv_dt_threeyear <- function(master, years = c(2000, 2010, 2022)) {
  if (!.iadv_has("DT")) return(NULL)
  d <- master[master$year %in% years &
              is.finite(master$che_pc_usd2023), ]
  if (!nrow(d)) return(NULL)
  d2 <- stats::aggregate(che_pc_usd2023 ~ iso3_code + country_name +
                            continent + year, data = d, FUN = mean)
  w <- tidyr::pivot_wider(d2, names_from = "year",
                            values_from = "che_pc_usd2023",
                            names_prefix = "y_")
  DT::datatable(w, rownames = FALSE, options = .iadv_dt_opts(),
                 extensions = "Buttons") |>
    DT::formatCurrency(grep("^y_", names(w), value = TRUE),
                        currency = "$", digits = 0)
}

#' iadv38 DT \u00b7 \u4eba\u5747 CHE \u5e74\u5ea6\u53d8\u5316
iadv_dt_yoy <- function(master, isos = c("USA", "CHN", "DEU", "JPN",
                                           "GBR", "BRA", "IND", "ZAF",
                                           "NGA")) {
  if (!.iadv_has("DT")) return(NULL)
  d <- master[master$iso3_code %in% isos &
              is.finite(master$che_pc_usd2023), ]
  d <- d[order(d$iso3_code, d$year), ]
  d$yoy <- ave(d$che_pc_usd2023, d$iso3_code,
                FUN = function(x) c(NA, diff(x) / utils::head(x, -1) * 100))
  w <- tidyr::pivot_wider(d[, c("country_name", "year", "yoy")],
                            names_from = "year", values_from = "yoy")
  DT::datatable(w, rownames = FALSE, options = .iadv_dt_opts(),
                 extensions = "Buttons") |>
    DT::formatRound(setdiff(names(w), "country_name"), digits = 1)
}

#' iadv39 DT \u00b7 \u9884\u671f\u5bff\u547d \u00b7 U5MR \u4ea4\u53c9
iadv_dt_lifeexp_u5mr <- function(master, year = NULL) {
  if (!.iadv_has("DT")) return(NULL)
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year &
              is.finite(master$life_exp) &
              is.finite(master$u5mr), ]
  d <- d[, c("country_name", "continent", "income_group",
              "life_exp", "u5mr", "che_pc_usd2023")]
  DT::datatable(d, rownames = FALSE, options = .iadv_dt_opts(),
                 extensions = "Buttons") |>
    DT::formatCurrency("che_pc_usd2023", currency = "$", digits = 0) |>
    DT::formatRound(c("life_exp", "u5mr"), digits = 1)
}

#' iadv40 DT \u00b7 GGHED \u00d7 OOP \u4e1c
iadv_dt_gghed_oop <- function(master, year = NULL) {
  if (!.iadv_has("DT")) return(NULL)
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year &
              is.finite(master$gghed_che) &
              is.finite(master$hf3_che), ]
  d$mix <- d$gghed_che - d$hf3_che
  d <- d[order(-d$mix),
          c("country_name", "continent", "gghed_che",
             "hf3_che", "mix")]
  DT::datatable(d, rownames = FALSE, options = .iadv_dt_opts(),
                 extensions = "Buttons") |>
    DT::formatRound(c("gghed_che", "hf3_che", "mix"), digits = 1)
}

#' iadv41 DT \u00b7 \u9ad8\u589e\u957f\u5012\u5e8f
iadv_dt_growth_desc <- function(master) {
  if (!.iadv_has("DT")) return(NULL)
  yrs <- range(master$year, na.rm = TRUE)
  base <- master[master$year == yrs[1],
                  c("iso3_code", "country_name", "che_pc_usd2023")]
  late <- master[master$year == yrs[2],
                  c("iso3_code", "che_pc_usd2023")]
  m <- merge(base, late, by = "iso3_code",
              suffixes = c(".y1", ".y2"))
  span <- yrs[2] - yrs[1]
  m$cagr <- (m$che_pc_usd2023.y2 / m$che_pc_usd2023.y1) ^
              (1 / span) - 1
  m <- m[is.finite(m$cagr), ]
  m <- m[order(-m$cagr), ]
  df <- data.frame(
    country = m$country_name,
    y1 = m$che_pc_usd2023.y1, y2 = m$che_pc_usd2023.y2,
    cagr = m$cagr * 100)
  DT::datatable(df, rownames = FALSE, options = .iadv_dt_opts(),
                 extensions = "Buttons") |>
    DT::formatCurrency(c("y1", "y2"), currency = "$", digits = 0) |>
    DT::formatRound("cagr", digits = 2)
}

#' iadv42 DT \u00b7 \u5916\u63f4\u00b7\u9ad8\u4f9d\u8d56
iadv_dt_extdep <- function(master, year = NULL, thr = 15) {
  if (!.iadv_has("DT")) return(NULL)
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year &
              is.finite(master$ext_che) &
              master$ext_che > thr, ]
  if (!nrow(d)) return(NULL)
  d <- d[order(-d$ext_che),
          c("country_name", "continent", "ext_che",
             "che_pc_usd2023", "life_exp")]
  DT::datatable(d, rownames = FALSE, options = .iadv_dt_opts(),
                 extensions = "Buttons") |>
    DT::formatRound(c("ext_che", "life_exp"), digits = 1) |>
    DT::formatCurrency("che_pc_usd2023", currency = "$", digits = 0)
}

#' iadv43 DT \u00b7 GDP-CHE \u8ddf\u968f
iadv_dt_gdp_che <- function(master, year = NULL) {
  if (!.iadv_has("DT")) return(NULL)
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year &
              is.finite(master$gdp_pc_usd) &
              is.finite(master$che_pc_usd2023), ]
  d$che_gdp_ratio <- d$che_pc_usd2023 / d$gdp_pc_usd * 100
  d <- d[, c("country_name", "continent", "gdp_pc_usd",
              "che_pc_usd2023", "che_gdp_ratio")]
  d <- d[order(-d$che_gdp_ratio), ]
  DT::datatable(d, rownames = FALSE, options = .iadv_dt_opts(),
                 extensions = "Buttons") |>
    DT::formatCurrency(c("gdp_pc_usd", "che_pc_usd2023"),
                        currency = "$", digits = 0) |>
    DT::formatRound("che_gdp_ratio", digits = 2)
}

#' iadv44 DT \u00b7 SDG 3.8 \u5012\u9000
iadv_dt_sdg38_alarm <- function(master) {
  if (!.iadv_has("DT")) return(NULL)
  yrs <- range(master$year, na.rm = TRUE)
  b <- master[master$year == yrs[1],
               c("iso3_code", "country_name", "hf3_che")]
  e <- master[master$year == yrs[2],
               c("iso3_code", "hf3_che")]
  m <- merge(b, e, by = "iso3_code",
              suffixes = c(".y1", ".y2"))
  m$delta_oop <- m$hf3_che.y2 - m$hf3_che.y1
  m <- m[is.finite(m$delta_oop) & m$delta_oop > 0, ]
  m <- m[order(-m$delta_oop), ]
  df <- data.frame(
    country = m$country_name, oop_y1 = m$hf3_che.y1,
    oop_y2 = m$hf3_che.y2, delta = m$delta_oop)
  DT::datatable(df, rownames = FALSE, options = .iadv_dt_opts(),
                 extensions = "Buttons") |>
    DT::formatRound(c("oop_y1", "oop_y2", "delta"), digits = 1)
}

#' iadv45 DT \u00b7 \u539f\u59cb\u9762\u677f\u00b7\u5168\u8a00\u4e2d\u8bcd
iadv_dt_full_panel <- function(master) {
  if (!.iadv_has("DT")) return(NULL)
  cols <- intersect(c("country_name", "iso3_code", "year",
                       "che_pc_usd2023", "hf3_che", "gghed_che",
                       "life_exp", "u5mr"), names(master))
  d <- utils::head(master[, cols], 5000)
  DT::datatable(d, rownames = FALSE, options = .iadv_dt_opts(),
                 extensions = "Buttons", filter = "top") |>
    DT::formatCurrency("che_pc_usd2023", currency = "$", digits = 0) |>
    DT::formatRound(c("hf3_che", "gghed_che",
                       "life_exp", "u5mr"), digits = 1)
}

# =============================================================================
# E. networkD3 / sankey (5)
# =============================================================================

#' iadv46 sankey \u00b7 \u8d44\u91d1\u4e09\u6bb5 (\u5168\u7403)
iadv_sankey_3stage <- function(master, year = NULL) {
  if (!.iadv_has("networkD3")) return(NULL)
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year &
              is.finite(master$che_usd2023), ]
  if (!nrow(d)) return(NULL)
  tot <- sum(d$che_usd2023, na.rm = TRUE)
  pub <- sum(d$che_usd2023 * d$hf1_che / 100, na.rm = TRUE)
  pri <- sum(d$che_usd2023 * d$hf2_che / 100, na.rm = TRUE)
  oop <- sum(d$che_usd2023 * d$hf3_che / 100, na.rm = TRUE)
  ext <- sum(d$che_usd2023 * d$ext_che / 100, na.rm = TRUE)
  nodes <- data.frame(name = c("\u603b CHE",
                                 "\u516c\u5171/\u793e\u4fdd",
                                 "\u79c1\u4eba\u4fdd\u9669",
                                 "\u5c45\u6c11\u81ea\u4ed8",
                                 "\u5916\u63f4"))
  links <- data.frame(source = c(0, 0, 0, 0),
                       target = c(1, 2, 3, 4),
                       value  = c(pub, pri, oop, ext) / 1e9)
  networkD3::sankeyNetwork(Links = links, Nodes = nodes,
    Source = "source", Target = "target", Value = "value",
    NodeID = "name", units = "B$",
    fontSize = 13, nodeWidth = 25,
    colourScale = "d3.scaleOrdinal().range([\"#1d3f5f\",\"#2a857a\",\"#c46327\",\"#a23b3b\",\"#774314\"])")
}

#' iadv47 sankey \u00b7 \u5927\u6d32 \u2192 \u6536\u5165\u7ec4 \u2192 OOP \u533a\u95f4
iadv_sankey_continent_oop <- function(master, year = NULL) {
  if (!.iadv_has("networkD3") || !.iadv_has("dplyr")) return(NULL)
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year &
              is.finite(master$hf3_che) &
              is.finite(master$che_usd2023) &
              !is.na(master$continent) &
              !is.na(master$income_group), ]
  if (!nrow(d)) return(NULL)
  d$oop_band <- cut(d$hf3_che, c(-Inf, 15, 30, 50, Inf),
                     labels = c("<15%", "15-30%",
                                 "30-50%", ">50%"))
  agg <- dplyr::summarise(dplyr::group_by(d, .data$continent,
                                            .data$income_group,
                                            .data$oop_band),
                            v = sum(.data$che_usd2023) / 1e9,
                            .groups = "drop")
  nodes <- data.frame(name = c(unique(as.character(agg$continent)),
                                 unique(as.character(agg$income_group)),
                                 unique(as.character(agg$oop_band))))
  idx <- function(x) match(x, nodes$name) - 1L
  links1 <- dplyr::summarise(dplyr::group_by(agg, .data$continent,
                                                .data$income_group),
                              v = sum(.data$v), .groups = "drop")
  links1 <- data.frame(source = idx(as.character(links1$continent)),
                        target = idx(as.character(links1$income_group)),
                        value  = links1$v)
  links2 <- dplyr::summarise(dplyr::group_by(agg, .data$income_group,
                                                .data$oop_band),
                              v = sum(.data$v), .groups = "drop")
  links2 <- data.frame(source = idx(as.character(links2$income_group)),
                        target = idx(as.character(links2$oop_band)),
                        value  = links2$v)
  links <- rbind(links1, links2)
  networkD3::sankeyNetwork(Links = links, Nodes = nodes,
    Source = "source", Target = "target", Value = "value",
    NodeID = "name", units = "B$", fontSize = 11, nodeWidth = 20)
}

#' iadv48 forceNetwork \u00b7 \u56fd\u5bb6\u76f8\u4f3c
iadv_force_country_sim <- function(master, year = NULL, k = 4) {
  if (!.iadv_has("networkD3")) return(NULL)
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  cols <- intersect(c("hf1_che", "hf2_che", "hf3_che",
                       "gghed_che", "ext_che", "che_pc_usd2023"),
                     names(master))
  d <- master[master$year == year, c("iso3_code", "country_name",
                                        "continent", cols)]
  d <- d[stats::complete.cases(d), ]
  if (nrow(d) < 30) return(NULL)
  X <- scale(as.matrix(d[, cols]))
  dist <- as.matrix(stats::dist(X))
  nodes <- data.frame(name = d$country_name,
                        group = as.integer(factor(d$continent)),
                        size = log(pmax(d$che_pc_usd2023, 1)) * 3)
  links <- do.call(rbind, lapply(seq_len(nrow(dist)), function(i) {
    nei <- order(dist[i, ])[2:(k + 1)]
    data.frame(source = i - 1L, target = nei - 1L, value = 1)
  }))
  networkD3::forceNetwork(Links = links, Nodes = nodes,
    Source = "source", Target = "target", Value = "value",
    NodeID = "name", Nodesize = "size", Group = "group",
    opacity = 0.9, fontSize = 11,
    linkDistance = networkD3::JS("function(d) { return 20; }"),
    colourScale = networkD3::JS(
      "d3.scaleOrdinal(['#1d3f5f','#c46327','#2a857a','#7c5b9a','#a23b3b'])"))
}

#' iadv49 chord \u00b7 \u5e74\u4efd\u95f4\u6392\u540d\u8de8\u8d8a
iadv_chord_rank_flow <- function(master, years = c(2000, 2022)) {
  if (!.iadv_has("networkD3")) return(NULL)
  d <- master[master$year %in% years &
              is.finite(master$che_pc_usd2023), ]
  if (!nrow(d)) return(NULL)
  d <- stats::aggregate(che_pc_usd2023 ~ iso3_code + country_name + year,
                          data = d, FUN = mean)
  d$rank <- stats::ave(-d$che_pc_usd2023, d$year, FUN = rank)
  d$band <- cut(d$rank, c(0, 20, 50, 100, Inf),
                 labels = c("Top20", "21-50", "51-100", "100+"))
  w <- tidyr::pivot_wider(d[, c("iso3_code", "year", "band")],
                            names_from = "year", values_from = "band")
  w <- w[stats::complete.cases(w), ]
  agg <- as.data.frame(table(w[[as.character(years[1])]],
                              w[[as.character(years[2])]]))
  names(agg) <- c("from", "to", "freq")
  agg <- agg[agg$freq > 0, ]
  nodes <- data.frame(name = sort(unique(c(as.character(agg$from),
                                              as.character(agg$to)))))
  links <- data.frame(source = match(as.character(agg$from),
                                        nodes$name) - 1L,
                        target = match(as.character(agg$to),
                                        nodes$name) - 1L,
                        value  = agg$freq)
  networkD3::sankeyNetwork(Links = links, Nodes = nodes,
    Source = "source", Target = "target", Value = "value",
    NodeID = "name", fontSize = 12, units = "\u56fd")
}

#' iadv50 diagonalNetwork \u00b7 \u5206\u7c7b\u6811
iadv_diagonal_tree <- function(master, year = NULL) {
  if (!.iadv_has("networkD3")) return(NULL)
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year &
              !is.na(master$continent) &
              !is.na(master$income_group), ]
  d <- utils::head(d[order(-d$che_pc_usd2023), ], 60)
  if (!nrow(d)) return(NULL)
  build_continent <- function(cont) {
    sub <- d[d$continent == cont, ]
    groups <- split(sub, sub$income_group)
    list(name = cont, children = lapply(names(groups), function(g)
      list(name = g, children = lapply(groups[[g]]$country_name,
                                        function(n) list(name = n)))))
  }
  tree <- list(name = "\u5168\u7403",
                children = lapply(unique(d$continent), build_continent))
  networkD3::diagonalNetwork(List = tree, fontSize = 12,
                              opacity = 0.95)
}

# =============================================================================
# F. crosstalk linked views (5)
# =============================================================================

#' iadv51 crosstalk \u00b7 scatter \u00b7 \u5927\u6d32\u8054\u52a8
iadv_ctk_scatter_table <- function(master, year = NULL) {
  if (!.iadv_has("crosstalk") || !.iadv_has("plotly") ||
      !.iadv_has("DT")) return(NULL)
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year &
              is.finite(master$che_pc_usd2023) &
              is.finite(master$life_exp), ]
  if (!nrow(d)) return(NULL)
  sd <- crosstalk::SharedData$new(d[, c("country_name", "continent",
                                            "che_pc_usd2023", "life_exp")])
  sc <- plotly::plot_ly(sd, x = ~che_pc_usd2023, y = ~life_exp,
    color = ~continent, colors = .iadv_palette,
    text = ~country_name, type = "scatter", mode = "markers",
    marker = list(size = 8, opacity = 0.8)) |>
    .iadv_layout("\u70b9\u51fb\u70b9\u8868\u8054\u52a8",
                  xaxis = list(title = "USD2023/\u4eba",
                                type = "log"),
                  yaxis = list(title = "\u5bff\u547d"))
  tb <- DT::datatable(sd, rownames = FALSE,
                       options = list(pageLength = 8, scrollX = TRUE)) |>
    DT::formatCurrency("che_pc_usd2023", currency = "$", digits = 0) |>
    DT::formatRound("life_exp", digits = 1)
  htmltools::div(crosstalk::filter_select("cont", "\u5927\u6d32",
                                            sd, ~continent),
                  htmltools::tags$br(), sc,
                  htmltools::tags$br(), tb)
}

#' iadv52 crosstalk \u00b7 \u8d8b\u52bf + \u8868
iadv_ctk_trend_table <- function(master,
                                    isos = c("USA", "CHN", "DEU",
                                              "JPN", "BRA", "IND")) {
  if (!.iadv_has("crosstalk") || !.iadv_has("plotly") ||
      !.iadv_has("DT")) return(NULL)
  d <- master[master$iso3_code %in% isos &
              is.finite(master$che_pc_usd2023), ]
  if (!nrow(d)) return(NULL)
  sd <- crosstalk::SharedData$new(d[, c("country_name", "year",
                                            "che_pc_usd2023", "hf3_che",
                                            "life_exp")])
  plt <- plotly::plot_ly(sd, x = ~year, y = ~che_pc_usd2023,
    color = ~country_name, colors = .iadv_palette,
    type = "scatter", mode = "lines+markers") |>
    .iadv_layout("\u70b9\u9009\u56fd\u5bb6\u540c\u6b65",
                  yaxis = list(type = "log"))
  tb <- DT::datatable(sd, rownames = FALSE,
                       options = list(pageLength = 8, scrollX = TRUE)) |>
    DT::formatCurrency("che_pc_usd2023", currency = "$", digits = 0) |>
    DT::formatRound(c("hf3_che", "life_exp"), digits = 1)
  htmltools::div(plt, htmltools::tags$br(), tb)
}

#' iadv53 crosstalk \u00b7 \u53cc\u70b9\u9762\u677f
iadv_ctk_dual_scatter <- function(master, year = NULL) {
  if (!.iadv_has("crosstalk") || !.iadv_has("plotly")) return(NULL)
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year &
              is.finite(master$gghed_che) &
              is.finite(master$hf3_che) &
              is.finite(master$life_exp), ]
  if (!nrow(d)) return(NULL)
  sd <- crosstalk::SharedData$new(d[, c("country_name", "continent",
                                            "gghed_che", "hf3_che",
                                            "life_exp")])
  s1 <- plotly::plot_ly(sd, x = ~gghed_che, y = ~hf3_che,
    type = "scatter", mode = "markers",
    color = ~continent, colors = .iadv_palette,
    text = ~country_name,
    marker = list(size = 8)) |>
    .iadv_layout("GGHED \u00d7 OOP",
                  xaxis = list(title = "GGHED %"),
                  yaxis = list(title = "OOP %"))
  s2 <- plotly::plot_ly(sd, x = ~hf3_che, y = ~life_exp,
    type = "scatter", mode = "markers",
    color = ~continent, colors = .iadv_palette,
    text = ~country_name,
    marker = list(size = 8)) |>
    .iadv_layout("OOP \u00d7 \u5bff\u547d",
                  xaxis = list(title = "OOP %"),
                  yaxis = list(title = "\u5bff\u547d"))
  htmltools::div(style = "display:grid;grid-template-columns:1fr 1fr;gap:10px",
                  s1, s2)
}

#' iadv54 crosstalk \u00b7 \u70b9\u51fb\u9ad8\u4eae
iadv_ctk_brushable <- function(master, year = NULL) {
  if (!.iadv_has("crosstalk") || !.iadv_has("plotly")) return(NULL)
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year, ]
  if (!nrow(d)) return(NULL)
  sd <- crosstalk::SharedData$new(d[, c("country_name", "continent",
                                            "income_group",
                                            "che_pc_usd2023",
                                            "hf3_che",
                                            "life_exp")])
  p <- plotly::plot_ly(sd, x = ~che_pc_usd2023, y = ~life_exp,
    color = ~continent, colors = .iadv_palette,
    type = "scatter", mode = "markers",
    text = ~country_name,
    marker = list(size = 10, opacity = 0.8)) |>
    plotly::layout(dragmode = "select",
                    paper_bgcolor = .iadv_paper,
                    xaxis = list(type = "log"))
  htmltools::div(
    htmltools::div(style = "display:grid;grid-template-columns:1fr 1fr;gap:10px",
      crosstalk::filter_select("cont2", "\u5927\u6d32",
                                sd, ~continent),
      crosstalk::filter_select("inc2", "\u6536\u5165\u7ec4",
                                sd, ~income_group)),
    htmltools::tags$br(), p)
}

#' iadv55 crosstalk \u00b7 \u53cc\u9762\u677f trend + scatter
iadv_ctk_panel_combo <- function(master) {
  if (!.iadv_has("crosstalk") || !.iadv_has("plotly")) return(NULL)
  d <- master[master$year %in% seq(2000, max(master$year, na.rm = TRUE), 2) &
              is.finite(master$che_pc_usd2023) &
              is.finite(master$life_exp), ]
  if (!nrow(d)) return(NULL)
  sd <- crosstalk::SharedData$new(d[, c("country_name", "year",
                                            "continent",
                                            "che_pc_usd2023",
                                            "life_exp")])
  pl1 <- plotly::plot_ly(sd, x = ~year, y = ~che_pc_usd2023,
    color = ~continent, colors = .iadv_palette,
    type = "scatter", mode = "lines",
    line = list(width = 1.2)) |>
    .iadv_layout("\u4eba\u5747 CHE \u8d8b\u52bf",
                  yaxis = list(type = "log"))
  pl2 <- plotly::plot_ly(sd, x = ~che_pc_usd2023, y = ~life_exp,
    color = ~continent, colors = .iadv_palette,
    type = "scatter", mode = "markers",
    marker = list(size = 6, opacity = 0.7)) |>
    .iadv_layout("CHE \u00d7 \u5bff\u547d",
                  xaxis = list(type = "log"))
  htmltools::div(style = "display:grid;grid-template-columns:1fr 1fr;gap:10px",
                  pl1, pl2)
}

# =============================================================================
# G. htmltools / KPI \u5361\u7247 (10)
# =============================================================================

.iadv_kpi_card <- function(title, value, delta = NULL,
                            unit = "", color = .iadv_primary) {
  htmltools::div(
    class = "ghs-kpi-card",
    style = paste0("background:#fff;border:1px solid ", color,
                    ";border-radius:12px;padding:14px 18px;",
                    "box-shadow:0 1px 4px rgba(0,0,0,0.05)"),
    htmltools::tags$div(style = paste0("color:", color,
                                         ";font-size:12px;",
                                         "font-weight:600;",
                                         "text-transform:uppercase;",
                                         "letter-spacing:1.5px"),
                          title),
    htmltools::tags$div(style = "font-size:28px;font-weight:700;color:#1a1f28",
                          paste0(value, " ", unit)),
    if (!is.null(delta))
      htmltools::tags$div(style = paste0("color:",
        if (delta > 0) .iadv_good else .iadv_bad,
        ";font-size:13px;font-weight:600"),
        sprintf("%s %.1f%%", if (delta > 0) "\u25b2" else "\u25bc",
                abs(delta))))
}

#' iadv56 KPI grid \u00b7 \u5168\u7403\u603b\u89c8
iadv_kpi_global <- function(master, year = NULL) {
  if (!.iadv_has("htmltools")) return(NULL)
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year, ]
  if (!nrow(d)) return(NULL)
  prev <- master[master$year == year - 1, ]
  che_tot <- sum(d$che_usd2023, na.rm = TRUE) / 1e12
  prev_tot <- if (nrow(prev)) sum(prev$che_usd2023, na.rm = TRUE) / 1e12 else NA
  d_che <- if (is.finite(prev_tot) && prev_tot > 0)
             (che_tot - prev_tot) / prev_tot * 100 else NA
  oop <- stats::weighted.mean(d$hf3_che, d$pop, na.rm = TRUE)
  life <- stats::weighted.mean(d$life_exp, d$pop, na.rm = TRUE)
  u5mr <- stats::weighted.mean(d$u5mr, d$pop, na.rm = TRUE)
  htmltools::div(
    style = paste0("display:grid;grid-template-columns:repeat(4,1fr);",
                    "gap:14px"),
    .iadv_kpi_card("\u603b CHE",
                     sprintf("$%.2f", che_tot),
                     d_che, "T", .iadv_primary),
    .iadv_kpi_card("OOP \u52a0\u6743",
                     sprintf("%.1f", oop), NULL,
                     "%", .iadv_secondary),
    .iadv_kpi_card("\u9884\u671f\u5bff\u547d",
                     sprintf("%.1f", life), NULL,
                     "yr", .iadv_good),
    .iadv_kpi_card("U5MR",
                     sprintf("%.1f", u5mr), NULL,
                     "/1000", .iadv_bad))
}

#' iadv57 KPI grid \u00b7 \u5355\u56fd
iadv_kpi_country <- function(master, iso = "USA", year = NULL) {
  if (!.iadv_has("htmltools")) return(NULL)
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$iso3_code == iso & master$year == year, ]
  prev <- master[master$iso3_code == iso & master$year == year - 1, ]
  if (!nrow(d)) return(NULL)
  delta_che <- if (nrow(prev) > 0 && is.finite(prev$che_pc_usd2023[1]))
    (d$che_pc_usd2023[1] - prev$che_pc_usd2023[1]) /
      prev$che_pc_usd2023[1] * 100 else NA
  htmltools::div(
    style = paste0("display:grid;grid-template-columns:repeat(4,1fr);",
                    "gap:14px"),
    .iadv_kpi_card(sprintf("%s CHE/\u4eba", iso),
                     sprintf("$%.0f", d$che_pc_usd2023[1]),
                     delta_che, "", .iadv_primary),
    .iadv_kpi_card("OOP",
                     sprintf("%.1f", d$hf3_che[1]), NULL,
                     "%", .iadv_secondary),
    .iadv_kpi_card("\u5bff\u547d",
                     sprintf("%.1f", d$life_exp[1]), NULL,
                     "yr", .iadv_good),
    .iadv_kpi_card("U5MR",
                     sprintf("%.1f", d$u5mr[1]), NULL,
                     "/1000", .iadv_bad))
}

#' iadv58 KPI grid \u00b7 \u8de8\u5e74\u53d8\u5316
iadv_kpi_change <- function(master) {
  if (!.iadv_has("htmltools")) return(NULL)
  yrs <- range(master$year, na.rm = TRUE)
  d1 <- master[master$year == yrs[1], ]
  d2 <- master[master$year == yrs[2], ]
  che_g <- (sum(d2$che_usd2023, na.rm = TRUE) /
              sum(d1$che_usd2023, na.rm = TRUE)) - 1
  oop_d <- stats::weighted.mean(d2$hf3_che, d2$pop, na.rm = TRUE) -
            stats::weighted.mean(d1$hf3_che, d1$pop, na.rm = TRUE)
  le_d <- stats::weighted.mean(d2$life_exp, d2$pop, na.rm = TRUE) -
           stats::weighted.mean(d1$life_exp, d1$pop, na.rm = TRUE)
  u5_d <- stats::weighted.mean(d2$u5mr, d2$pop, na.rm = TRUE) -
           stats::weighted.mean(d1$u5mr, d1$pop, na.rm = TRUE)
  htmltools::div(
    style = paste0("display:grid;grid-template-columns:repeat(4,1fr);",
                    "gap:14px"),
    .iadv_kpi_card(sprintf("CHE \u00b7 %d\u2192%d", yrs[1], yrs[2]),
                     sprintf("%+.0f%%", che_g * 100), NULL,
                     "", .iadv_primary),
    .iadv_kpi_card(sprintf("OOP \u00b7 %d\u2192%d", yrs[1], yrs[2]),
                     sprintf("%+.1f", oop_d), NULL,
                     "pp", .iadv_secondary),
    .iadv_kpi_card(sprintf("\u5bff\u547d \u00b7 %d\u2192%d", yrs[1], yrs[2]),
                     sprintf("%+.1f", le_d), NULL,
                     "yr", .iadv_good),
    .iadv_kpi_card(sprintf("U5MR \u00b7 %d\u2192%d", yrs[1], yrs[2]),
                     sprintf("%+.1f", u5_d), NULL,
                     "/1000", .iadv_bad))
}

#' iadv59 progress bar grid \u00b7 SDG-3 \u8fdb\u5ea6
iadv_progress_sdg3 <- function(master, year = NULL) {
  if (!.iadv_has("htmltools")) return(NULL)
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year, ]
  targets <- list(
    life = list(t = 75, v = stats::weighted.mean(d$life_exp,
                                                   d$pop, na.rm = TRUE),
                col = .iadv_good, label = "\u9884\u671f\u5bff\u547d \u2265 75",
                unit = "yr"),
    u5m  = list(t = 25, v = stats::weighted.mean(d$u5mr,
                                                  d$pop, na.rm = TRUE),
                col = .iadv_warn, label = "U5MR \u2264 25 \u00b7 \u5012\u5411",
                unit = "/1000", reverse = TRUE),
    oop  = list(t = 20, v = stats::weighted.mean(d$hf3_che,
                                                  d$pop, na.rm = TRUE),
                col = .iadv_secondary, label = "OOP \u2264 20 \u00b7 \u5012\u5411",
                unit = "%", reverse = TRUE))
  bar <- function(it) {
    pct <- if (!is.null(it$reverse) && it$reverse)
      max(0, min(100, (1 - it$v / it$t) * 100))
    else
      max(0, min(100, it$v / it$t * 100))
    htmltools::div(
      style = paste0("background:#f4ecdb;border:1px solid #d8c89e;",
                      "border-radius:8px;padding:10px 14px"),
      htmltools::tags$div(style = "font-weight:600;color:#1a1f28",
                            it$label),
      htmltools::tags$div(style = paste0("font-size:12px;color:#534a36;",
                                            "margin-bottom:6px"),
        sprintf("\u5f53\u524d %.1f %s \u00b7 \u76ee\u6807 %.0f %s",
                it$v, it$unit, it$t, it$unit)),
      htmltools::tags$div(
        style = "background:#e0d7c6;height:10px;border-radius:5px;overflow:hidden",
        htmltools::tags$div(
          style = paste0("background:", it$col, ";width:", pct,
                          "%;height:100%"), "")))
  }
  htmltools::div(
    style = paste0("display:grid;grid-template-columns:repeat(3,1fr);",
                    "gap:14px"),
    bar(targets$life), bar(targets$u5m), bar(targets$oop))
}

#' iadv60 ranking strip \u00b7 \u4eba\u5747 CHE Top10 \u6298\u53e0\u68a6
iadv_rank_strip <- function(master, year = NULL, top_n = 10) {
  if (!.iadv_has("htmltools")) return(NULL)
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year &
              is.finite(master$che_pc_usd2023), ]
  d <- d[order(-d$che_pc_usd2023), ]
  d <- utils::head(d, top_n)
  max_v <- max(d$che_pc_usd2023, na.rm = TRUE)
  rows <- lapply(seq_len(nrow(d)), function(i) {
    pct <- d$che_pc_usd2023[i] / max_v * 100
    htmltools::div(
      style = "display:grid;grid-template-columns:30px 140px 1fr 100px;align-items:center;gap:8px;padding:4px 8px",
      htmltools::tags$span(style = "font-weight:600;color:#534a36",
                             paste0("#", i)),
      htmltools::tags$span(style = "color:#1a1f28", d$country_name[i]),
      htmltools::div(
        style = "background:#e0d7c6;height:14px;border-radius:7px;overflow:hidden",
        htmltools::div(
          style = paste0("background:linear-gradient(90deg,",
                          .iadv_primary, ",", .iadv_secondary,
                          ");width:", pct, "%;height:100%"), "")),
      htmltools::tags$span(style = "color:#1a1f28;font-weight:600;text-align:right",
        sprintf("$%s",
                 formatC(d$che_pc_usd2023[i], big.mark = ",",
                          format = "d"))))
  })
  htmltools::div(style = "background:#fff;border:1px solid #d8c89e;border-radius:12px;padding:12px",
    htmltools::tags$h4(style = "margin:0 0 8px;color:#1a1f28",
      sprintf("\u4eba\u5747 CHE Top%d \u00b7 %d", top_n, year)),
    rows)
}

#' iadv61 swatch \u00b7 \u989c\u8272\u4e0e\u67d3\u8272\u793a\u4f8b
iadv_swatch_palette <- function() {
  if (!.iadv_has("htmltools")) return(NULL)
  pal <- .iadv_palette
  names(pal) <- c("primary", "secondary", "good", "warn", "bad",
                   "sky", "violet", "amber", "olive", "rose")
  htmltools::div(style = "display:flex;flex-wrap:wrap;gap:10px",
    lapply(names(pal), function(nm)
      htmltools::div(style = paste0("display:flex;align-items:center;gap:8px;",
                                       "background:#fff;border:1px solid #d8c89e;",
                                       "border-radius:8px;padding:6px 10px"),
        htmltools::div(style = paste0("width:24px;height:24px;",
                                         "border-radius:4px;background:", pal[[nm]]),
                         ""),
        htmltools::tags$span(style = "font-family:monospace;font-size:12px",
                                paste0(nm, " ", pal[[nm]])))))
}

#' iadv62 spark line group \u00b7 5 \u56fd\u8de8\u8868\u8d8b\u52bf
iadv_sparkline_panel <- function(master,
                                    isos = c("USA", "CHN", "DEU",
                                              "JPN", "BRA")) {
  if (!.iadv_has("htmltools")) return(NULL)
  rows <- lapply(isos, function(iso) {
    s <- master[master$iso3_code == iso &
                is.finite(master$che_pc_usd2023), ]
    s <- s[order(s$year), ]
    if (!nrow(s)) return(NULL)
    cagr <- (utils::tail(s$che_pc_usd2023, 1) /
               utils::head(s$che_pc_usd2023, 1)) ^
              (1 / (diff(range(s$year)))) - 1
    htmltools::div(
      style = "display:grid;grid-template-columns:80px 1fr 80px;align-items:center;gap:10px;padding:6px 10px",
      htmltools::tags$span(style = "font-weight:600", iso),
      htmltools::HTML(.iadv_sparkline_html(s$che_pc_usd2023,
                                            .iadv_primary,
                                            w = 200, h = 30)),
      htmltools::tags$span(style = paste0("text-align:right;font-weight:600;",
                                             "color:",
                                             if (cagr > 0) .iadv_good
                                             else .iadv_bad),
                              sprintf("%+.1f%%", cagr * 100)))
  })
  htmltools::div(style = "background:#fff;border:1px solid #d8c89e;border-radius:12px;padding:12px",
    htmltools::tags$h4(style = "margin:0 0 8px;color:#1a1f28",
      "\u4eba\u5747 CHE \u8de8\u56fd\u7eaa\u9886"),
    rows)
}

#' iadv63 alert \u9762\u677f
iadv_alert_panel <- function(master, year = NULL) {
  if (!.iadv_has("htmltools")) return(NULL)
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year, ]
  high_oop <- sum(d$hf3_che > 50, na.rm = TRUE)
  high_ext <- sum(d$ext_che > 30, na.rm = TRUE)
  low_le   <- sum(d$life_exp < 60, na.rm = TRUE)
  high_u5  <- sum(d$u5mr > 80, na.rm = TRUE)
  alert <- function(n, label, col) {
    htmltools::div(
      style = paste0("background:#fff;border-left:4px solid ", col,
                      ";padding:10px 14px;border-radius:6px"),
      htmltools::tags$div(style = paste0("font-size:24px;font-weight:700;",
                                           "color:", col), n),
      htmltools::tags$div(style = "color:#534a36;font-size:13px",
                            label))
  }
  htmltools::div(
    style = paste0("display:grid;grid-template-columns:repeat(4,1fr);",
                    "gap:14px"),
    alert(high_oop, "OOP > 50% \u56fd\u5bb6\u6570", .iadv_bad),
    alert(high_ext, "\u5916\u63f4 > 30% \u56fd\u5bb6\u6570", .iadv_warn),
    alert(low_le, "\u5bff\u547d < 60 \u5e74\u56fd\u5bb6\u6570", .iadv_secondary),
    alert(high_u5, "U5MR > 80 \u56fd\u5bb6\u6570", .iadv_bad))
}

#' iadv64 dashboard summary \u00b7 \u8868\u4e0a\u4f30\u91cf
iadv_dashboard_overview <- function(master, year = NULL) {
  if (!.iadv_has("htmltools")) return(NULL)
  htmltools::tagList(
    iadv_kpi_global(master, year),
    htmltools::tags$br(),
    iadv_alert_panel(master, year),
    htmltools::tags$br(),
    iadv_progress_sdg3(master, year))
}

#' iadv65 callout box \u00b7 finding banner
iadv_callout <- function(title, body,
                           kind = c("info", "warn", "alert", "good")) {
  if (!.iadv_has("htmltools")) return(NULL)
  kind <- match.arg(kind)
  col <- switch(kind, info = .iadv_primary, warn = .iadv_warn,
                 alert = .iadv_bad, good = .iadv_good)
  bg  <- switch(kind, info = "#e6edf4", warn = "#fbeed1",
                 alert = "#fcdcdc", good = "#d8eae5")
  icon <- switch(kind, info = "i", warn = "!", alert = "\u26a0",
                  good = "\u2713")
  htmltools::div(
    style = paste0("display:flex;align-items:flex-start;gap:14px;",
                    "background:", bg,
                    ";border-left:4px solid ", col,
                    ";padding:14px 18px;border-radius:8px;margin:10px 0"),
    htmltools::div(style = paste0("font-size:24px;font-weight:700;color:", col),
                     icon),
    htmltools::div(htmltools::tags$h4(style = paste0("margin:0 0 4px;color:", col), title),
                     htmltools::tags$div(style = "color:#1a1f28;font-size:14px;line-height:1.5",
                                            body)))
}

# =============================================================================
# H. plotly extras / \u53cc\u91cd\u4f9d\u8d56\u964d\u7ea7 (15)
# =============================================================================

#' iadv66 plotly area smooth \u00b7 \u5168\u7403\u603b CHE (\u52a0\u6743)
iadv_area_smooth_che <- function(master) {
  if (!.iadv_has("plotly") || !.iadv_has("dplyr")) return(NULL)
  d <- master |>
    dplyr::filter(is.finite(.data$che_usd2023)) |>
    dplyr::group_by(.data$year) |>
    dplyr::summarise(che = sum(.data$che_usd2023, na.rm = TRUE) / 1e12,
                      .groups = "drop")
  if (!nrow(d)) return(NULL)
  p <- plotly::plot_ly(d, x = ~year, y = ~che, type = "scatter",
                        mode = "lines",
                        fill = "tozeroy",
                        line = list(shape = "spline", smoothing = 1.3,
                                      color = .iadv_primary, width = 3),
                        fillcolor = "rgba(29,63,95,0.18)",
                        name = "\u603b CHE")
  .iadv_layout(p, "\u5168\u7403\u603b CHE \u00b7 spline \u5e73\u6ed1",
                yaxis = list(title = "USD2023 / T",
                              tickformat = ".1f"))
}

#' iadv67 plotly violin \u5c0f\u500d \u00b7 \u5927\u6d32
iadv_violin_small_multi <- function(master, year = NULL,
                                       var = "che_pc_usd2023") {
  if (!.iadv_has("plotly")) return(NULL)
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year &
              is.finite(master[[var]]) &
              !is.na(master$continent), ]
  if (!nrow(d)) return(NULL)
  p <- plotly::plot_ly(d, x = ~continent, y = ~.data[[var]],
                        color = ~continent, colors = .iadv_palette,
                        type = "violin", points = "all", pointpos = 0,
                        jitter = 0.3, box = list(visible = TRUE))
  .iadv_layout(p, sprintf("\u5927\u6d32\u4ee4 violin \u00b7 %s \u00b7 %d",
                            var, year),
                yaxis = list(type = if (grepl("usd", var)) "log"
                                       else "linear"))
}

#' iadv68 plotly small multiples \u00b7 \u591a\u56fd trend grid
iadv_small_multi_trend <- function(master,
                                       isos = c("USA", "CHN", "DEU",
                                                 "JPN", "BRA", "IND",
                                                 "ZAF", "NGA", "EGY")) {
  if (!.iadv_has("plotly")) return(NULL)
  d <- master[master$iso3_code %in% isos &
              is.finite(master$che_pc_usd2023), ]
  if (!nrow(d)) return(NULL)
  p <- plotly::plot_ly(d, x = ~year, y = ~che_pc_usd2023,
                        color = ~country_name, colors = .iadv_palette,
                        type = "scatter", mode = "lines",
                        line = list(width = 1.6)) |>
    plotly::layout(yaxis = list(type = "log"))
  .iadv_layout(p, "\u591a\u56fd CHE/\u4eba \u8d8b\u52bf",
                xaxis = list(title = "\u5e74\u4efd"))
}

#' iadv69 plotly conditional area \u00b7 \u51b2\u51fb\u68a6\u6cb3
iadv_area_shock_bands <- function(master) {
  if (!.iadv_has("plotly") || !.iadv_has("dplyr")) return(NULL)
  d <- master |>
    dplyr::filter(is.finite(.data$che_pc_usd2023),
                  is.finite(.data$pop)) |>
    dplyr::group_by(.data$year) |>
    dplyr::summarise(wavg = stats::weighted.mean(.data$che_pc_usd2023,
                                                  .data$pop, na.rm = TRUE),
                      .groups = "drop")
  if (!nrow(d)) return(NULL)
  shapes <- list(
    list(type = "rect", x0 = 2008, x1 = 2009.5, y0 = 0, y1 = 1,
         yref = "paper", fillcolor = "#a23b3b", opacity = 0.10,
         line = list(width = 0)),
    list(type = "rect", x0 = 2020, x1 = 2021.5, y0 = 0, y1 = 1,
         yref = "paper", fillcolor = "#a23b3b", opacity = 0.10,
         line = list(width = 0)))
  p <- plotly::plot_ly(d, x = ~year, y = ~wavg,
                        type = "scatter", mode = "lines+markers",
                        line = list(color = .iadv_primary, width = 3))
  .iadv_layout(p, "\u4eba\u5747 CHE \u00b7 \u5e26 GFC / COVID \u9634\u5f71",
                xaxis = list(title = "\u5e74\u4efd"),
                yaxis = list(title = "USD2023 / \u4eba", type = "log"),
                shapes = shapes)
}

#' iadv70 plotly slope chart
iadv_slope_chart <- function(master, isos = NULL,
                                years = c(2000, 2022)) {
  if (!.iadv_has("plotly")) return(NULL)
  if (is.null(isos)) {
    base <- master[master$year == years[2] &
                    is.finite(master$che_pc_usd2023), ]
    base <- base[order(-base$che_pc_usd2023), , drop = FALSE]
    isos <- utils::head(base$iso3_code, 15)
  }
  d <- master[master$iso3_code %in% isos &
              master$year %in% years, ]
  if (!nrow(d)) return(NULL)
  p <- plotly::plot_ly(d, x = ~factor(year), y = ~che_pc_usd2023,
                        color = ~country_name, colors = .iadv_palette,
                        type = "scatter", mode = "lines+markers",
                        line = list(width = 2.5))
  .iadv_layout(p, sprintf("\u4eba\u5747 CHE Slope \u00b7 %d \u2192 %d",
                            years[1], years[2]),
                xaxis = list(title = NA),
                yaxis = list(type = "log",
                              title = "USD2023 / \u4eba"))
}

#' iadv71 plotly dot-plot dumbbell
iadv_dumbbell <- function(master, years = c(2000, 2022),
                            isos = NULL) {
  if (!.iadv_has("plotly")) return(NULL)
  if (is.null(isos)) {
    base <- master[master$year == years[2], ]
    base <- base[order(-base$che_pc_usd2023), , drop = FALSE]
    isos <- utils::head(base$iso3_code, 20)
  }
  d <- master[master$iso3_code %in% isos &
              master$year %in% years, ]
  if (!nrow(d)) return(NULL)
  w <- tidyr::pivot_wider(d[, c("iso3_code", "country_name",
                                  "year", "che_pc_usd2023")],
                            names_from = "year",
                            values_from = "che_pc_usd2023",
                            names_prefix = "y_")
  y1 <- paste0("y_", years[1])
  y2 <- paste0("y_", years[2])
  w <- w[stats::complete.cases(w[, c(y1, y2)]), ]
  if (!nrow(w)) return(NULL)
  w$country_name <- factor(w$country_name,
    levels = w$country_name[order(w[[y2]])])
  p <- plotly::plot_ly()
  for (i in seq_len(nrow(w))) {
    p <- plotly::add_trace(p,
      x = c(w[[y1]][i], w[[y2]][i]),
      y = c(w$country_name[i], w$country_name[i]),
      type = "scatter", mode = "lines",
      line = list(color = "#a8b0c0", width = 2),
      showlegend = FALSE)
  }
  p <- plotly::add_trace(p, x = w[[y1]], y = w$country_name,
                          type = "scatter", mode = "markers",
                          marker = list(color = .iadv_primary, size = 10),
                          name = as.character(years[1]))
  p <- plotly::add_trace(p, x = w[[y2]], y = w$country_name,
                          type = "scatter", mode = "markers",
                          marker = list(color = .iadv_secondary, size = 10),
                          name = as.character(years[2]))
  .iadv_layout(p, sprintf("\u4eba\u5747 CHE Dumbbell \u00b7 %d \u2192 %d",
                            years[1], years[2]),
                xaxis = list(title = "USD2023 / \u4eba", type = "log"),
                yaxis = list(title = NA))
}

#' iadv72 plotly lollipop \u00b7 OOP
iadv_lollipop_oop <- function(master, year = NULL, top_n = 30) {
  if (!.iadv_has("plotly")) return(NULL)
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year & is.finite(master$hf3_che), ]
  d <- d[order(-d$hf3_che), ]
  d <- utils::head(d, top_n)
  if (!nrow(d)) return(NULL)
  d$country_name <- factor(d$country_name, levels = rev(d$country_name))
  p <- plotly::plot_ly()
  for (i in seq_len(nrow(d))) {
    p <- plotly::add_trace(p,
      x = c(0, d$hf3_che[i]), y = c(d$country_name[i], d$country_name[i]),
      type = "scatter", mode = "lines",
      line = list(color = "#a8b0c0", width = 2),
      showlegend = FALSE)
  }
  p <- plotly::add_trace(p, x = d$hf3_che, y = d$country_name,
                          type = "scatter", mode = "markers",
                          marker = list(color = .iadv_secondary, size = 12),
                          showlegend = FALSE,
                          text = ~sprintf("%.1f%%", d$hf3_che))
  .iadv_layout(p, sprintf("OOP \u68d2\u68d2\u7cd6 Top%d \u00b7 %d", top_n, year),
                xaxis = list(title = "OOP / CHE (%)"))
}

#' iadv73 plotly ribbon \u00b7 \u5927\u6d32\u8de8\u5e74
iadv_continent_ribbon <- function(master, var = "che_pc_usd2023") {
  if (!.iadv_has("plotly") || !.iadv_has("dplyr")) return(NULL)
  d <- master |>
    dplyr::filter(is.finite(.data[[var]]), !is.na(.data$continent)) |>
    dplyr::group_by(.data$year, .data$continent) |>
    dplyr::summarise(
      q25 = stats::quantile(.data[[var]], .25, na.rm = TRUE),
      q50 = stats::quantile(.data[[var]], .50, na.rm = TRUE),
      q75 = stats::quantile(.data[[var]], .75, na.rm = TRUE),
      .groups = "drop")
  if (!nrow(d)) return(NULL)
  conts <- unique(d$continent)
  cols <- stats::setNames(.iadv_palette[seq_along(conts)], conts)
  p <- plotly::plot_ly()
  for (cc in conts) {
    sub <- d[d$continent == cc, ]
    p <- plotly::add_ribbons(p, x = sub$year, ymin = sub$q25,
                              ymax = sub$q75,
                              line = list(color = "transparent"),
                              fillcolor = paste0(cols[cc], "55"),
                              name = paste0(cc, " 25-75"))
    p <- plotly::add_lines(p, x = sub$year, y = sub$q50,
                            line = list(color = cols[cc], width = 2.5),
                            name = paste0(cc, " med"))
  }
  .iadv_layout(p, sprintf("%s \u5927\u6d32 IQR \u8d8b\u52bf", var),
                yaxis = list(type = if (grepl("usd", var)) "log"
                                       else "linear"))
}

#' iadv74 plotly waterfall \u8d44\u91d1\u5e8f\u5217
iadv_waterfall_continents <- function(master, year = NULL) {
  if (!.iadv_has("plotly") || !.iadv_has("dplyr")) return(NULL)
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master |>
    dplyr::filter(.data$year == .env$year,
                  is.finite(.data$che_usd2023),
                  !is.na(.data$continent)) |>
    dplyr::group_by(.data$continent) |>
    dplyr::summarise(v = sum(.data$che_usd2023, na.rm = TRUE) / 1e9,
                      .groups = "drop")
  if (!nrow(d)) return(NULL)
  p <- plotly::plot_ly(type = "waterfall",
    x = d$continent, y = d$v,
    measure = rep("relative", nrow(d)),
    text = sprintf("$%.0f B", d$v),
    decreasing = list(marker = list(color = .iadv_bad)),
    increasing = list(marker = list(color = .iadv_primary)),
    totals = list(marker = list(color = .iadv_ink)))
  .iadv_layout(p, sprintf("\u5168\u7403\u603b CHE \u4e3b\u8981\u5927\u6d32 \u00b7 %d", year),
                yaxis = list(title = "USD2023 / B"))
}

#' iadv75 plotly indicator gauge group
iadv_gauge_grid <- function(master, year = NULL) {
  if (!.iadv_has("plotly")) return(NULL)
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year, ]
  oop <- stats::weighted.mean(d$hf3_che, d$pop, na.rm = TRUE)
  gghed <- stats::weighted.mean(d$gghed_che, d$pop, na.rm = TRUE)
  life <- stats::weighted.mean(d$life_exp, d$pop, na.rm = TRUE)
  p <- plotly::plot_ly() |>
    plotly::add_trace(type = "indicator", mode = "gauge+number",
      value = oop,
      title = list(text = "OOP \u52a0\u6743 %"),
      gauge = list(axis = list(range = c(0, 80)),
                    bar = list(color = .iadv_secondary),
                    threshold = list(line = list(color = "#a23b3b",
                                                   width = 2),
                                      value = 30)),
      domain = list(row = 0, column = 0)) |>
    plotly::add_trace(type = "indicator", mode = "gauge+number",
      value = gghed,
      title = list(text = "GGHED \u52a0\u6743 %"),
      gauge = list(axis = list(range = c(0, 100)),
                    bar = list(color = .iadv_primary)),
      domain = list(row = 0, column = 1)) |>
    plotly::add_trace(type = "indicator", mode = "gauge+number",
      value = life,
      title = list(text = "\u5bff\u547d \u52a0\u6743 yr"),
      gauge = list(axis = list(range = c(40, 90)),
                    bar = list(color = .iadv_good)),
      domain = list(row = 0, column = 2)) |>
    plotly::layout(grid = list(rows = 1, columns = 3,
                                pattern = "independent"),
                    paper_bgcolor = .iadv_paper,
                    title = sprintf("\u5168\u7403\u4eea\u8868\u76d8 \u00b7 %d", year))
  p
}

#' iadv76 plotly density 2D
iadv_density2d <- function(master, year = NULL) {
  if (!.iadv_has("plotly")) return(NULL)
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year &
              is.finite(master$che_pc_usd2023) &
              is.finite(master$life_exp), ]
  if (!nrow(d)) return(NULL)
  d$lche <- log10(pmax(d$che_pc_usd2023, 1))
  p <- plotly::plot_ly(d, x = ~lche, y = ~life_exp,
    type = "histogram2dcontour", colorscale = "Earth",
    contours = list(showlabels = TRUE)) |>
    plotly::add_trace(type = "scatter", mode = "markers",
                       marker = list(color = "rgba(26,31,40,0.3)",
                                       size = 4),
                       showlegend = FALSE)
  .iadv_layout(p, sprintf("\u53cc\u53d8\u91cf\u5bc6\u5ea6\u00b7log CHE \u00d7 \u5bff\u547d \u00b7 %d",
                            year),
                xaxis = list(title = "log10 CHE/\u4eba"),
                yaxis = list(title = "\u9884\u671f\u5bff\u547d"))
}

#' iadv77 plotly indicator KPI grid (4)
iadv_kpi_indicator <- function(master, year = NULL) {
  if (!.iadv_has("plotly")) return(NULL)
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year, ]
  prev <- master[master$year == year - 1, ]
  che_now <- sum(d$che_usd2023, na.rm = TRUE) / 1e12
  che_prev <- sum(prev$che_usd2023, na.rm = TRUE) / 1e12
  oop <- stats::weighted.mean(d$hf3_che, d$pop, na.rm = TRUE)
  oop_prev <- stats::weighted.mean(prev$hf3_che, prev$pop, na.rm = TRUE)
  life <- stats::weighted.mean(d$life_exp, d$pop, na.rm = TRUE)
  life_prev <- stats::weighted.mean(prev$life_exp, prev$pop, na.rm = TRUE)
  u5 <- stats::weighted.mean(d$u5mr, d$pop, na.rm = TRUE)
  u5_prev <- stats::weighted.mean(prev$u5mr, prev$pop, na.rm = TRUE)
  add_ind <- function(pp, value, ref, label, col, col2) {
    plotly::add_trace(pp,
      type = "indicator", mode = "number+delta",
      value = value,
      delta = list(reference = ref, relative = TRUE,
                    valueformat = ".1%"),
      title = list(text = label,
                     font = list(size = 14, color = col)),
      number = list(valueformat = ".2f",
                      font = list(color = col)),
      domain = list(row = 0, column = col2))
  }
  p <- plotly::plot_ly()
  p <- add_ind(p, che_now, che_prev, "\u603b CHE (T$)",
                .iadv_primary, 0)
  p <- add_ind(p, oop, oop_prev, "OOP %",
                .iadv_secondary, 1)
  p <- add_ind(p, life, life_prev, "\u5bff\u547d",
                .iadv_good, 2)
  p <- add_ind(p, u5, u5_prev, "U5MR",
                .iadv_bad, 3)
  plotly::layout(p,
    grid = list(rows = 1, columns = 4, pattern = "independent"),
    paper_bgcolor = .iadv_paper)
}

#' iadv78 plotly polar bar \u00b7 \u8d44\u91d1\u6e90\u5934\u73af
iadv_polar_bar <- function(master, year = NULL) {
  if (!.iadv_has("plotly")) return(NULL)
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year, ]
  vals <- c(stats::weighted.mean(d$hf1_che, d$pop, na.rm = TRUE),
             stats::weighted.mean(d$hf2_che, d$pop, na.rm = TRUE),
             stats::weighted.mean(d$hf3_che, d$pop, na.rm = TRUE),
             stats::weighted.mean(d$ext_che, d$pop, na.rm = TRUE))
  labels <- c("\u516c\u5171/\u793e\u4fdd", "\u79c1\u4eba\u4fdd\u9669",
               "\u5c45\u6c11\u81ea\u4ed8", "\u5916\u63f4")
  p <- plotly::plot_ly(type = "barpolar",
    r = vals, theta = labels,
    marker = list(color = c(.iadv_primary, .iadv_secondary,
                              .iadv_bad, .iadv_warn)),
    text = sprintf("%.1f%%", vals))
  plotly::layout(p,
    title = sprintf("\u8d44\u91d1\u6765\u6e90\u73af \u00b7 %d", year),
    polar = list(radialaxis = list(visible = TRUE)),
    paper_bgcolor = .iadv_paper,
    font = list(family = "PingFang SC, sans-serif"))
}

#' iadv79 plotly subplot grid \u00b7 4 \u6307\u6807
iadv_subplot_4metric <- function(master,
                                    isos = c("USA", "CHN", "DEU",
                                              "JPN", "BRA")) {
  if (!.iadv_has("plotly")) return(NULL)
  d <- master[master$iso3_code %in% isos, ]
  if (!nrow(d)) return(NULL)
  base <- function(var, ttl) {
    plotly::plot_ly(d, x = ~year, y = ~.data[[var]],
                     color = ~country_name, colors = .iadv_palette,
                     type = "scatter", mode = "lines",
                     showlegend = FALSE) |>
      plotly::layout(title = ttl)
  }
  p1 <- base("che_pc_usd2023", "\u4eba\u5747 CHE")
  p2 <- base("hf3_che",         "OOP %")
  p3 <- base("life_exp",        "\u5bff\u547d")
  p4 <- base("u5mr",            "U5MR")
  plotly::subplot(p1, p2, p3, p4, nrows = 2, shareX = TRUE,
                   titleX = FALSE) |>
    plotly::layout(title = "4 \u6307\u6807\u591a\u56fd subplot",
                    paper_bgcolor = .iadv_paper)
}

#' iadv80 plotly bar mean+CI
iadv_bar_ci <- function(master, year = NULL,
                          var = "hf3_che") {
  if (!.iadv_has("plotly") || !.iadv_has("dplyr")) return(NULL)
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master |>
    dplyr::filter(.data$year == .env$year,
                  is.finite(.data[[var]]),
                  !is.na(.data$continent)) |>
    dplyr::group_by(.data$continent) |>
    dplyr::summarise(
      mu = mean(.data[[var]], na.rm = TRUE),
      sd = stats::sd(.data[[var]], na.rm = TRUE),
      n  = dplyr::n(),
      .groups = "drop")
  d$ci <- 1.96 * d$sd / sqrt(d$n)
  p <- plotly::plot_ly(d, x = ~continent, y = ~mu,
                        type = "bar",
                        error_y = list(array = ~ci, color = .iadv_ink),
                        marker = list(color = .iadv_palette[seq_len(nrow(d))],
                                       line = list(color = .iadv_ink,
                                                     width = 0.5)))
  .iadv_layout(p, sprintf("%s \u00b7 \u5927\u6d32\u5747\u503c\u00b1 95%% CI \u00b7 %d",
                            var, year),
                yaxis = list(title = var))
}

# =============================================================================
# I. highcharter / echarts4r \u53cc\u91cd\u4f9d\u8d56\u964d\u7ea7 (10)
#    \u672a\u5b89\u88c5\u65f6\u8f6c\u4ea4 plotly \u540c\u6548\u53ef\u89c6\u5316
# =============================================================================

#' iadv81 hc \u00b7 \u591a\u56fd\u8d8b\u52bf (\u964d\u7ea7\u4e3a plotly)
iadv_hc_country_lines <- function(master,
                                      isos = c("USA", "CHN", "DEU",
                                                "JPN")) {
  if (.iadv_has("highcharter")) {
    d <- master[master$iso3_code %in% isos &
                is.finite(master$che_pc_usd2023), ]
    if (!nrow(d)) return(NULL)
    return(highcharter::hchart(d,
      "line", highcharter::hcaes(x = year, y = che_pc_usd2023,
                                  group = country_name)) |>
        highcharter::hc_title(text = "\u591a\u56fd\u4eba\u5747 CHE") |>
        highcharter::hc_yAxis(type = "logarithmic") |>
        highcharter::hc_colors(.iadv_palette))
  }
  iadv_che_pc_lines(master, isos)
}

#' iadv82 hc \u00b7 stream/area (\u964d\u7ea7 plotly)
iadv_hc_stream <- function(master) {
  if (.iadv_has("highcharter") && .iadv_has("dplyr")) {
    d <- master |>
      dplyr::filter(is.finite(.data$che_usd2023),
                    !is.na(.data$continent)) |>
      dplyr::group_by(.data$year, .data$continent) |>
      dplyr::summarise(v = sum(.data$che_usd2023) / 1e9,
                        .groups = "drop")
    return(highcharter::hchart(d, "streamgraph",
      highcharter::hcaes(x = year, y = v, group = continent)) |>
        highcharter::hc_title(text = "\u5168\u7403 CHE streamgraph") |>
        highcharter::hc_colors(.iadv_palette))
  }
  iadv_che_total_stacked(master)
}

#' iadv83 hc \u00b7 packed bubble (\u964d\u7ea7 plotly)
iadv_hc_packed <- function(master, year = NULL) {
  if (.iadv_has("highcharter")) {
    if (is.null(year)) year <- max(master$year, na.rm = TRUE)
    d <- master[master$year == year &
                is.finite(master$che_usd2023), ]
    d <- utils::head(d[order(-d$che_usd2023), ], 50)
    return(highcharter::hchart(d, "packedbubble",
      highcharter::hcaes(name = country_name, value = che_usd2023,
                          group = continent)) |>
        highcharter::hc_colors(.iadv_palette))
  }
  iadv_treemap_che(master, year)
}

#' iadv84 hc \u00b7 dependency wheel (\u964d\u7ea7 networkD3)
iadv_hc_wheel <- function(master, year = NULL) {
  if (.iadv_has("highcharter")) {
    if (is.null(year)) year <- max(master$year, na.rm = TRUE)
    d <- master[master$year == year, ]
    rows <- list()
    for (cc in unique(d$continent)) {
      ig_split <- table(d$income_group[d$continent == cc])
      for (ig in names(ig_split))
        rows[[length(rows) + 1]] <- list(from = cc, to = ig,
                                            weight = as.numeric(ig_split[[ig]]))
    }
    return(highcharter::highchart() |>
      highcharter::hc_chart(type = "dependencywheel") |>
      highcharter::hc_add_series(name = "Flow", data = rows) |>
      highcharter::hc_colors(.iadv_palette))
  }
  iadv_sankey_continent_oop(master, year)
}

#' iadv85 hc \u00b7 item chart (\u964d\u7ea7 plotly bar)
iadv_hc_item <- function(master, year = NULL, top_n = 30) {
  if (.iadv_has("highcharter")) {
    if (is.null(year)) year <- max(master$year, na.rm = TRUE)
    d <- master[master$year == year &
                is.finite(master$che_pc_usd2023), ]
    d <- utils::head(d[order(-d$che_pc_usd2023), ], top_n)
    return(highcharter::hchart(d, "item",
      highcharter::hcaes(name = country_name, y = che_pc_usd2023)))
  }
  iadv_oop_rank_latest(master, top_n)
}

#' iadv86 echarts4r calendar (\u964d\u7ea7 plotly heatmap)
iadv_ec_calendar <- function(master) {
  if (.iadv_has("echarts4r") && .iadv_has("dplyr")) {
    d <- master |>
      dplyr::filter(is.finite(.data$che_pc_usd2023)) |>
      dplyr::group_by(.data$year) |>
      dplyr::summarise(v = stats::median(.data$che_pc_usd2023,
                                          na.rm = TRUE),
                        .groups = "drop")
    d$date <- as.Date(sprintf("%d-06-01", d$year))
    return(echarts4r::e_charts(d, date) |>
      echarts4r::e_calendar(range = range(d$year)) |>
      echarts4r::e_heatmap(v, coord_system = "calendar"))
  }
  iadv_heatmap_year_inc(master)
}

#' iadv87 echarts4r river (\u964d\u7ea7 plotly stack area)
iadv_ec_river <- function(master) {
  if (.iadv_has("echarts4r") && .iadv_has("dplyr")) {
    d <- master |>
      dplyr::filter(is.finite(.data$che_usd2023),
                    !is.na(.data$continent)) |>
      dplyr::group_by(.data$year, .data$continent) |>
      dplyr::summarise(v = sum(.data$che_usd2023) / 1e9,
                        .groups = "drop")
    return(echarts4r::e_charts(d, year) |>
      echarts4r::e_river_(value = "v", legend = "continent"))
  }
  iadv_che_total_stacked(master)
}

#' iadv88 echarts4r sunburst (\u964d\u7ea7 plotly sunburst)
iadv_ec_sunburst <- function(master, year = NULL) {
  if (.iadv_has("echarts4r")) {
    return(NULL)
  }
  iadv_sunburst_che(master, year)
}

#' iadv89 echarts4r graph network (\u964d\u7ea7 forceNetwork)
iadv_ec_graph <- function(master, year = NULL) {
  if (.iadv_has("echarts4r")) {
    return(NULL)
  }
  iadv_force_country_sim(master, year)
}

#' iadv90 echarts4r liquid (\u964d\u7ea7 indicator)
iadv_ec_liquid <- function(master, year = NULL) {
  if (.iadv_has("echarts4r")) {
    if (is.null(year)) year <- max(master$year, na.rm = TRUE)
    d <- master[master$year == year, ]
    oop <- stats::weighted.mean(d$hf3_che, d$pop, na.rm = TRUE) / 100
    return(echarts4r::e_charts() |>
      echarts4r::e_liquid(oop))
  }
  iadv_gauge_grid(master, year)
}

# =============================================================================
# J. htmltools dashboards extras (10)
# =============================================================================

#' iadv91 caret card grid \u00b7 \u8de8\u56fd\u5e76\u6392
iadv_country_card_grid <- function(master, isos = c("USA", "CHN", "DEU",
                                                       "JPN", "BRA", "IND"),
                                      year = NULL) {
  if (!.iadv_has("htmltools")) return(NULL)
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  cards <- lapply(isos, function(iso) {
    d <- master[master$iso3_code == iso & master$year == year, ]
    if (!nrow(d)) return(NULL)
    htmltools::div(
      style = paste0("background:#fff;border:1px solid #d8c89e;",
                      "border-radius:12px;padding:14px 18px"),
      htmltools::tags$h4(style = "margin:0 0 6px;color:#1a1f28",
                            paste0(iso, " \u00b7 ", year)),
      htmltools::tags$div(style = "font-size:13px;color:#534a36",
        sprintf("\u4eba\u5747 CHE $%s",
                 formatC(d$che_pc_usd2023[1], big.mark = ",",
                          format = "d"))),
      htmltools::tags$div(style = "font-size:13px;color:#534a36",
        sprintf("OOP %.1f%% | \u5bff\u547d %.1f",
                d$hf3_che[1], d$life_exp[1])))
  })
  htmltools::div(style = "display:grid;grid-template-columns:repeat(3,1fr);gap:14px",
                  cards)
}

#' iadv92 spark trend banner \u00b7 5 \u5927\u6d32
iadv_spark_continents <- function(master) {
  if (!.iadv_has("htmltools") || !.iadv_has("dplyr")) return(NULL)
  d <- master |>
    dplyr::filter(is.finite(.data$che_pc_usd2023),
                  !is.na(.data$continent)) |>
    dplyr::group_by(.data$year, .data$continent) |>
    dplyr::summarise(v = stats::median(.data$che_pc_usd2023,
                                        na.rm = TRUE),
                      .groups = "drop")
  rows <- lapply(unique(d$continent), function(cc) {
    sub <- d[d$continent == cc, ]
    sub <- sub[order(sub$year), ]
    htmltools::div(
      style = "display:grid;grid-template-columns:120px 1fr 100px;gap:10px;align-items:center;padding:4px 8px",
      htmltools::tags$span(style = "font-weight:600", cc),
      htmltools::HTML(.iadv_sparkline_html(sub$v, .iadv_primary,
                                            w = 220, h = 30)),
      htmltools::tags$span(style = "text-align:right;font-weight:600",
        sprintf("$%.0f", utils::tail(sub$v, 1))))
  })
  htmltools::div(style = "background:#fff;border:1px solid #d8c89e;border-radius:12px;padding:12px",
    htmltools::tags$h4(style = "margin:0 0 8px", "\u5927\u6d32\u4eba\u5747 CHE \u4e2d\u4f4d\u8d8b\u52bf"),
    rows)
}

#' iadv93 ranking pill \u00b7 \u4eba\u5747 CHE \u5927\u6d32\u6700\u9ad8\u4f4e
iadv_extreme_pill <- function(master, year = NULL) {
  if (!.iadv_has("htmltools")) return(NULL)
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year &
              is.finite(master$che_pc_usd2023), ]
  if (!nrow(d)) return(NULL)
  top <- d[order(-d$che_pc_usd2023)[1], ]
  bot <- d[order(d$che_pc_usd2023)[1], ]
  pill <- function(t, c, v, col) {
    htmltools::div(
      style = paste0("display:inline-block;background:", col,
                      ";color:#fff;padding:6px 14px;border-radius:20px;",
                      "margin:4px 6px;font-weight:600"),
      sprintf("%s: %s ($%s)", t, c,
               formatC(v, big.mark = ",", format = "d")))
  }
  htmltools::div(
    pill("MAX", top$country_name[1], top$che_pc_usd2023[1], .iadv_good),
    pill("MIN", bot$country_name[1], bot$che_pc_usd2023[1], .iadv_bad))
}

#' iadv94 finding card \u00b7 \u53d1\u73b0\u5361\u7247
iadv_finding_card <- function(title, abstract, evidence_text,
                                 source_text = "WHO GHED 2024") {
  if (!.iadv_has("htmltools")) return(NULL)
  htmltools::div(
    style = paste0("background:#fff;border:1px solid #d8c89e;",
                    "border-radius:12px;padding:18px 22px;",
                    "box-shadow:0 1px 6px rgba(0,0,0,0.04)"),
    htmltools::tags$h3(style = "margin:0 0 8px;color:#1a1f28", title),
    htmltools::tags$p(style = "color:#534a36;line-height:1.6;margin:6px 0",
                        abstract),
    htmltools::tags$details(
      htmltools::tags$summary(style = "color:#1d3f5f;cursor:pointer",
                                 "\u8bc1\u636e"),
      htmltools::tags$p(style = "color:#1a1f28;line-height:1.6",
                          evidence_text)),
    htmltools::tags$p(style = "color:#8a7e5f;font-size:12px;margin:6px 0 0",
                        sprintf("Source: %s", source_text)))
}

#' iadv95 milestone timeline
iadv_milestone_timeline <- function(master = NULL, events = NULL) {
  if (!.iadv_has("htmltools")) return(NULL)
  # master \u4ec5\u7528\u4e8e\u7edf\u4e00\u7ea6\u5b9a\uff0c\u5185\u90e8\u4e0d\u4f9d\u8d56\uff1bevents \u63d0\u4f9b\u65f6\u4f7f\u7528
  if (is.null(events)) {
    events <- list(
      list(year = 2008,
             title = "\u5168\u7403\u91d1\u878d\u5371\u673a",
             text = "OECD \u591a\u56fd\u8d22\u653f\u538b\u529b\u4e0a\u5347"),
      list(year = 2010,
             title = "WHO Global Health Spending v1",
             text = "\u9996\u6b21\u516c\u5e03\u8de8\u56fd\u53ef\u6bd4\u6570\u636e"),
      list(year = 2015,
             title = "SDG \u542f\u52a8",
             text = "Goal 3 \u4e0e\u8d22\u52a1\u4fdd\u62a4\u6307\u6807\u8bbe\u7acb"),
      list(year = 2020,
             title = "COVID-19",
             text = "\u4ea7\u51fa\u4e0e\u8d22\u52a1\u4fdd\u62a4\u9762\u4e34\u540c\u6b65\u51b2\u51fb"),
      list(year = 2023,
             title = "WHO GHED 2024",
             text = "2000\u20132023 \u6700\u65b0\u9762\u677f\u53d1\u5e03"))
  }
  htmltools::div(
    style = paste0("position:relative;padding-left:30px;",
                    "border-left:3px solid #d8c89e"),
    lapply(events, function(e) {
      htmltools::div(
        style = "margin:10px 0;padding:10px 14px;background:#fff;border:1px solid #d8c89e;border-radius:8px;position:relative",
        htmltools::div(
          style = paste0("position:absolute;left:-39px;top:14px;",
                          "width:18px;height:18px;border-radius:50%;",
                          "background:", .iadv_primary,
                          ";border:3px solid #fbf6ee"), ""),
        htmltools::tags$div(style = "font-weight:600;color:#1d3f5f",
                              sprintf("%d \u00b7 %s", e$year, e$title)),
        htmltools::tags$div(style = "color:#534a36;font-size:13px",
                              e$text))
    }))
}

#' iadv96 quote callout
iadv_quote_callout <- function(quote_text, attrib) {
  if (!.iadv_has("htmltools")) return(NULL)
  htmltools::div(
    style = paste0("background:#fff;border-left:6px solid ", .iadv_primary,
                    ";padding:18px 26px;font-family:serif;",
                    "border-radius:6px;margin:14px 0"),
    htmltools::tags$blockquote(
      style = "margin:0;font-size:18px;line-height:1.7;color:#1a1f28",
      paste0("\u201c", quote_text, "\u201d")),
    htmltools::tags$p(
      style = "margin:8px 0 0;color:#534a36;font-size:13px;text-align:right",
      paste0("\u2014 ", attrib)))
}

#' iadv97 stats badge row
iadv_stats_badge <- function(master, year = NULL) {
  if (!.iadv_has("htmltools")) return(NULL)
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year, ]
  n <- nrow(d)
  cov <- sum(!is.na(d$che_pc_usd2023))
  badge <- function(label, val, col) {
    htmltools::div(
      style = paste0("background:", col, ";color:#fff;padding:6px 14px;",
                      "border-radius:14px;font-weight:600;",
                      "display:inline-block;margin:2px"),
      sprintf("%s: %s", label, val))
  }
  htmltools::div(
    badge("\u5e74\u4efd", year, .iadv_primary),
    badge("\u56fd\u5bb6", n, .iadv_secondary),
    badge("\u8986\u76d6", cov, .iadv_good),
    badge("\u8986\u76d6\u7387",
           sprintf("%.0f%%", cov / n * 100), .iadv_warn))
}

#' iadv98 \u9876\u90e8\u6982\u8981 banner
iadv_top_banner <- function(master) {
  if (!.iadv_has("htmltools")) return(NULL)
  htmltools::div(
    style = paste0("background:linear-gradient(135deg,",
                    .iadv_primary, " 0%,",
                    "#0d121b 100%);color:#fff;",
                    "padding:24px 28px;border-radius:12px;",
                    "margin-bottom:16px"),
    htmltools::tags$h2(
      style = "margin:0 0 8px;color:#fff;font-weight:700",
      "Global Health Spending \u00b7 2000\u20132023"),
    htmltools::tags$p(
      style = "margin:0;color:#f4ecdb;line-height:1.6",
      sprintf("%d \u4e2a\u56fd\u5bb6 \u00b7 %d \u4efd\u989d \u00b7 %d \u5e74 WHO GHED",
              length(unique(master$iso3_code)),
              nrow(master),
              max(master$year, na.rm = TRUE))))
}

#' iadv99 \u8d44\u91d1\u8c1b\u9762\u00b7\u5360\u6bd4\u73af
iadv_donut_finance <- function(master, year = NULL) {
  if (!.iadv_has("plotly")) return(NULL)
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  d <- master[master$year == year, ]
  vals <- c(stats::weighted.mean(d$hf1_che, d$pop, na.rm = TRUE),
             stats::weighted.mean(d$hf2_che, d$pop, na.rm = TRUE),
             stats::weighted.mean(d$hf3_che, d$pop, na.rm = TRUE))
  labels <- c("\u516c\u5171/\u793e\u4fdd", "\u79c1\u4eba\u4fdd\u9669",
               "\u5c45\u6c11\u81ea\u4ed8")
  p <- plotly::plot_ly(type = "pie",
    labels = labels, values = vals, hole = 0.55,
    marker = list(colors = c(.iadv_primary, .iadv_secondary,
                               .iadv_bad),
                    line = list(color = "#fff", width = 2)),
    textinfo = "label+percent")
  .iadv_layout(p, sprintf("\u5168\u7403\u8d44\u91d1\u73af \u00b7 %d", year))
}

#' iadv100 \u8868\u4e0e\u56fe\u5e76\u6392
iadv_table_plus_plot <- function(master, year = NULL) {
  if (!.iadv_has("htmltools")) return(NULL)
  if (is.null(year)) year <- max(master$year, na.rm = TRUE)
  htmltools::div(
    style = "display:grid;grid-template-columns:1fr 1fr;gap:14px",
    iadv_rt_continent_summary(master, year),
    iadv_donut_finance(master, year))
}

# =============================================================================
# \u6279\u91cf\u9a8c\u8bc1\u5668
# =============================================================================

#' \u9a8c\u8bc1 C2 widget \u53ef\u6784\u9020
ghs_validate_widgets_advanced <- function(master = NULL) {
  if (is.null(master)) {
    cache <- file.path(proj_root(), "\u6d3e\u751f\u6570\u636e",
                        "\u5904\u7406\u7ed3\u679c",
                        "master_enriched.rds")
    if (!file.exists(cache)) stop("Missing master_enriched.rds")
    master <- readRDS(cache)
  }
  fn_names <- ls(envir = .GlobalEnv, pattern = "^iadv_[a-z]")
  fn_names <- fn_names[vapply(fn_names, function(n)
    is.function(get(n, envir = .GlobalEnv)) &&
      !startsWith(n, "iadv_") || TRUE, logical(1))]
  fn_names <- intersect(fn_names, ls(envir = .GlobalEnv))
  results <- list()
  for (nm in fn_names) {
    f <- get(nm, envir = .GlobalEnv)
    res <- tryCatch({
      obj <- f(master)
      list(ok = !is.null(obj), class = class(obj)[1])
    }, error = function(e) list(ok = FALSE, class = conditionMessage(e)))
    results[[nm]] <- res
  }
  results
}

#' \u6279\u91cf\u5bfc\u51fa C2 \u9ad8\u7ea7 widget \u4e3a HTML
#' @param master \u4e3b\u6570\u636e
#' @param out_dir \u8f93\u51fa\u76ee\u5f55
#' @param verbose \u8fdb\u5ea6
#' @param max_n \u9650\u5236\u5bfc\u51fa\u4e2a\u6570\uff08\u9ed8\u8ba4 200\uff09\uff0c\u907f\u514d\u751f\u6210\u8fc7\u591a
ghs_export_widgets_advanced <- function(master = NULL,
                                          out_dir = file.path("\u5206\u6790\u8f93\u51fa",
                                                                "\u4ea4\u4e92\u7ec4\u4ef6"),
                                          verbose = TRUE,
                                          max_n = 200) {
  if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
    if (verbose) message("[adv] htmlwidgets \u4e0d\u53ef\u7528\uff0c\u8df3\u8fc7")
    return(invisible(character(0)))
  }
  if (is.null(master)) {
    cache <- file.path(proj_root(), "\u6d3e\u751f\u6570\u636e",
                        "\u5904\u7406\u7ed3\u679c", "master_enriched.rds")
    if (!file.exists(cache)) stop("Missing master_enriched.rds")
    master <- readRDS(cache)
  }
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  fn_names <- ls(envir = .GlobalEnv, pattern = "^iadv_[a-z]")
  if (length(fn_names) > max_n) fn_names <- utils::head(fn_names, max_n)
  produced <- character(0)
  for (nm in fn_names) {
    f <- get(nm, envir = .GlobalEnv)
    obj <- tryCatch(f(master), error = function(e) NULL)
    if (is.null(obj)) {
      if (verbose) message("[adv] skip ", nm)
      next
    }
    p <- if (exists("ghs_save_widget", mode = "function")) {
      ghs_save_widget(obj, nm, dir = out_dir)
    } else {
      pp <- file.path(out_dir, paste0(nm, ".html"))
      tryCatch(htmlwidgets::saveWidget(obj, file = pp,
                                          selfcontained = TRUE),
                error = function(e) NULL)
      pp
    }
    if (!is.null(p)) {
      produced <- c(produced, p)
      if (verbose && length(produced) %% 10 == 0)
        message("[adv] saved ", length(produced), " so far\u2026")
    }
  }
  if (verbose) message("[adv] total saved: ", length(produced))
  invisible(produced)
}
