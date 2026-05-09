# =============================================================================
# 程序/25_widgets_more.R
# -----------------------------------------------------------------------------
# 第二批交互 widget 扩展（18 个），统一通过 ghs_export_widgets_more() 入口调用，
# 与 17/18_widgets_*.R 互补；只生成 standalone HTML，不修改既有文件。
# =============================================================================

.gxw_layout <- function(p, title = NULL, subtitle = NULL,
                         source = "WHO GHED 2024",
                         legend_orient = "h") {
  if (!requireNamespace("plotly", quietly = TRUE)) return(p)
  plotly::layout(
    p,
    title = list(
      text = paste0(
        if (!is.null(title)) sprintf("<b>%s</b>", title) else "",
        if (!is.null(subtitle))
          sprintf("<br><sup style='color:#5C5C66;font-weight:normal'>%s</sup>",
                  subtitle) else ""
      ),
      x = 0, xanchor = "left",
      font = list(family = "Inter, system-ui, sans-serif",
                  size = 18, color = "#1A1A1F")
    ),
    font = list(family = "Inter, system-ui, sans-serif", color = "#1A1A1F"),
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor = "rgba(0,0,0,0)",
    hovermode = "closest",
    legend = list(orientation = legend_orient, y = -0.18),
    margin = list(t = 70, r = 16, b = 70, l = 60),
    annotations = list(
      list(
        x = 0, y = -0.28, xref = "paper", yref = "paper",
        text = sprintf("<i>Source · %s · 庄颂 (20241334)</i>", source),
        showarrow = FALSE, xanchor = "left",
        font = list(size = 11, color = "#5C5C66")
      )
    )
  )
}

.gxw_latest_year <- function(master) max(master$year, na.rm = TRUE)

# 1. continent OOPS weighted trend lines
widget_more_continent_oops <- function(master) {
  if (!requireNamespace("plotly", quietly = TRUE)) return(NULL)
  d <- master[is.finite(master$hf3_che) &
                is.finite(master$che_usd2023) &
                !is.na(master$continent), ]
  if (!nrow(d)) return(NULL)
  agg <- stats::aggregate(
    list(oops = d$hf3_che * d$che_usd2023,
         w    = d$che_usd2023),
    list(continent = d$continent, year = d$year), sum, na.rm = TRUE)
  agg$oops <- agg$oops / agg$w
  p <- plotly::plot_ly(agg, x = ~year, y = ~oops, color = ~continent,
                        type = "scatter", mode = "lines+markers",
                        hovertemplate = "%{x}: %{y:.1f}%<extra>%{fullData.name}</extra>")
  .gxw_layout(p,
    title = "\u5927\u6d32 OOPS \u5360\u6bd4\u8d8b\u52bf\uff08CHE \u52a0\u6743\uff09",
    subtitle = "2000\u20132023 \u00b7 \u9f20\u6807\u7559\u540d\u67e5\u770b\u5e74\u5ea6\u8be6\u7ec6")
}

# 2. global HF1/2/3 share area
widget_more_global_hf <- function(master) {
  if (!requireNamespace("plotly", quietly = TRUE)) return(NULL)
  d <- master[is.finite(master$hf1_usd2023) &
                is.finite(master$hf2_usd2023) &
                is.finite(master$hf3_usd2023), ]
  if (!nrow(d)) return(NULL)
  agg <- stats::aggregate(
    cbind(HF1 = d$hf1_usd2023, HF2 = d$hf2_usd2023,
          HF3 = d$hf3_usd2023),
    list(year = d$year), sum, na.rm = TRUE)
  agg$total <- agg$HF1 + agg$HF2 + agg$HF3
  for (k in c("HF1", "HF2", "HF3")) agg[[k]] <- agg[[k]] / agg$total * 100
  p <- plotly::plot_ly(agg, x = ~year)
  p <- plotly::add_trace(p, y = ~HF1, name = "HF1 \u653f\u5e9c",
                          type = "scatter", mode = "lines",
                          stackgroup = "one", fillcolor = "#1B5E88",
                          line = list(width = 0))
  p <- plotly::add_trace(p, y = ~HF2, name = "HF2 \u4fdd\u9669",
                          type = "scatter", mode = "lines",
                          stackgroup = "one", fillcolor = "#2A857A",
                          line = list(width = 0))
  p <- plotly::add_trace(p, y = ~HF3, name = "HF3 OOPS",
                          type = "scatter", mode = "lines",
                          stackgroup = "one", fillcolor = "#C46B27",
                          line = list(width = 0))
  p <- plotly::layout(p, yaxis = list(ticksuffix = "%", range = c(0, 100)))
  .gxw_layout(p,
    title = "\u5168\u7403\u4e09\u6e90\u5360\u6bd4\u52a8\u6001",
    subtitle = "HF1 \u653f\u5e9c \u00b7 HF2 \u4fdd\u9669 \u00b7 HF3 OOPS \u00b7 USD2023 \u52a0\u603b")
}

# 3. income-group CHE/cap weighted lines
widget_more_income_che <- function(master) {
  if (!requireNamespace("plotly", quietly = TRUE)) return(NULL)
  d <- master[is.finite(master$che_pc_usd2023) &
                is.finite(master$pop) &
                !is.na(master$income_group), ]
  if (!nrow(d)) return(NULL)
  agg <- stats::aggregate(
    list(che = d$che_pc_usd2023 * d$pop, w = d$pop),
    list(income_group = d$income_group, year = d$year), sum, na.rm = TRUE)
  agg$che <- agg$che / agg$w
  agg$income_group <- factor(agg$income_group,
    levels = c("Low income", "Lower middle income",
               "Upper middle income", "High income"))
  p <- plotly::plot_ly(agg, x = ~year, y = ~che, color = ~income_group,
                        type = "scatter", mode = "lines+markers",
                        hovertemplate = "%{x}: $%{y:,.0f}<extra>%{fullData.name}</extra>")
  p <- plotly::layout(p, yaxis = list(type = "log",
                                       title = "CHE per capita (USD2023)"))
  .gxw_layout(p,
    title = "\u4eba\u5747 CHE \u00b7 \u6536\u5165\u7ec4\u4eba\u53e3\u52a0\u6743",
    subtitle = "log \u7eb5\u8f74\u00b72000\u20132023")
}

# 4. CHE / GDP world share trend
widget_more_che_gdp_world <- function(master) {
  if (!requireNamespace("plotly", quietly = TRUE)) return(NULL)
  d <- master[is.finite(master$che_usd2023) &
                is.finite(master$gdp_pc_usd) &
                is.finite(master$pop), ]
  if (!nrow(d)) return(NULL)
  d$gdp <- d$gdp_pc_usd * d$pop
  agg <- stats::aggregate(cbind(che = d$che_usd2023, gdp = d$gdp),
                           list(year = d$year), sum, na.rm = TRUE)
  agg$share <- agg$che / agg$gdp * 100
  p <- plotly::plot_ly(agg, x = ~year, y = ~share,
                        type = "scatter", mode = "lines+markers+lines",
                        line = list(color = "#2A857A", width = 2.5),
                        marker = list(color = "#2A857A", size = 7),
                        hovertemplate = "%{x}: %{y:.2f}%<extra></extra>")
  p <- plotly::layout(p, yaxis = list(ticksuffix = "%"))
  .gxw_layout(p,
    title = "\u5168\u7403 CHE \u5360 GDP \u6bd4\u91cd",
    subtitle = "\u7531 CHE \u603b\u989d / GDP \u603b\u989d \u8ba1\u7b97 \u00b7 \u9879\u542b\u5916\u63f4")
}

# 5. OOPS dumbbell decline 2000 vs latest plotly
widget_more_oops_dumbbell <- function(master) {
  if (!requireNamespace("plotly", quietly = TRUE)) return(NULL)
  yrs <- c(2000, .gxw_latest_year(master))
  d <- master[master$year %in% yrs & is.finite(master$hf3_che), ]
  if (!nrow(d)) return(NULL)
  w <- reshape2::dcast(d, iso3_code + country_name + continent ~ year,
                        value.var = "hf3_che")
  cols <- as.character(yrs)
  if (!all(cols %in% names(w))) return(NULL)
  w$delta <- w[[cols[2]]] - w[[cols[1]]]
  w <- w[is.finite(w$delta), ]
  w <- w[order(w$delta), ]
  top <- utils::head(w, 25)
  top$country_name <- factor(top$country_name,
    levels = rev(top$country_name))
  p <- plotly::plot_ly()
  p <- plotly::add_segments(p, x = top[[cols[1]]], xend = top[[cols[2]]],
                             y = top$country_name, yend = top$country_name,
                             line = list(color = "#5C5C66", width = 2),
                             showlegend = FALSE,
                             hoverinfo = "skip")
  p <- plotly::add_markers(p, x = top[[cols[1]]], y = top$country_name,
                            name = sprintf("%s OOPS", cols[1]),
                            marker = list(color = "#5C5C66", size = 9),
                            hovertemplate = paste0("%{y}: ", cols[1],
                                                   " OOPS = %{x:.1f}%<extra></extra>"))
  p <- plotly::add_markers(p, x = top[[cols[2]]], y = top$country_name,
                            name = sprintf("%s OOPS", cols[2]),
                            marker = list(color = "#1B5E88", size = 11),
                            hovertemplate = paste0("%{y}: ", cols[2],
                                                   " OOPS = %{x:.1f}%<extra></extra>"))
  p <- plotly::layout(p, xaxis = list(ticksuffix = "%"),
                      yaxis = list(autorange = "reversed"))
  .gxw_layout(p,
    title = sprintf("OOPS \u4e0b\u964d\u6700\u5feb 25 \u56fd \u00b7 %s\u2192%s", yrs[1], yrs[2]),
    subtitle = "\u5de6\u70b9 = 2000\uff1b\u53f3\u70b9 = \u6700\u8fd1\u5e74")
}

# 6. CHE per capita CAGR top 30 bar
widget_more_che_cagr_bar <- function(master) {
  if (!requireNamespace("plotly", quietly = TRUE)) return(NULL)
  yrs <- c(2000, .gxw_latest_year(master))
  d <- master[master$year %in% yrs & is.finite(master$che_pc_usd2023), ]
  if (!nrow(d)) return(NULL)
  w <- reshape2::dcast(d, iso3_code + country_name + continent ~ year,
                        value.var = "che_pc_usd2023")
  cols <- as.character(yrs)
  if (!all(cols %in% names(w))) return(NULL)
  w <- w[is.finite(w[[cols[1]]]) & w[[cols[1]]] > 30 &
           is.finite(w[[cols[2]]]), ]
  w$cagr <- (w[[cols[2]]] / w[[cols[1]]]) ^ (1 / 23) - 1
  w <- w[is.finite(w$cagr), ]
  w <- w[order(-w$cagr), ]
  top <- utils::head(w, 30)
  top$country_name <- factor(top$country_name,
    levels = rev(top$country_name))
  p <- plotly::plot_ly(top, x = ~cagr * 100, y = ~country_name,
                        color = ~continent, type = "bar",
                        orientation = "h",
                        hovertemplate = "%{y}: %{x:.2f}%<extra>%{fullData.name}</extra>")
  p <- plotly::layout(p, xaxis = list(ticksuffix = "%"))
  .gxw_layout(p,
    title = "\u4eba\u5747 CHE 23 \u5e74 CAGR \u524d 30 \u56fd",
    subtitle = "\u590d\u5408\u589e\u901f\uff08\u8d77\u70b9 > USD30\uff09")
}

# 7. life expectancy delta top 25 bar
widget_more_life_top_bar <- function(master) {
  if (!requireNamespace("plotly", quietly = TRUE)) return(NULL)
  yrs <- c(2000, .gxw_latest_year(master))
  d <- master[master$year %in% yrs & is.finite(master$life_exp), ]
  if (!nrow(d)) return(NULL)
  w <- reshape2::dcast(d, iso3_code + country_name + continent ~ year,
                        value.var = "life_exp")
  cols <- as.character(yrs)
  if (!all(cols %in% names(w))) return(NULL)
  w$delta <- w[[cols[2]]] - w[[cols[1]]]
  w <- w[is.finite(w$delta), ]
  w <- w[order(-w$delta), ]
  top <- utils::head(w, 25)
  top$country_name <- factor(top$country_name,
    levels = rev(top$country_name))
  p <- plotly::plot_ly(top, x = ~delta, y = ~country_name,
                        color = ~continent, type = "bar",
                        orientation = "h",
                        hovertemplate = "%{y}: +%{x:.1f} \u5e74<extra>%{fullData.name}</extra>")
  .gxw_layout(p,
    title = "\u9884\u671f\u5bff\u547d\u589e\u957f\u6700\u5feb\u7684 25 \u56fd",
    subtitle = "2023 \u2212 2000 \u00b7 \u5355\u4f4d\uff1a\u5e74")
}

# 8. U5MR decline top 25 bar
widget_more_u5mr_top_bar <- function(master) {
  if (!requireNamespace("plotly", quietly = TRUE)) return(NULL)
  yrs <- c(2000, .gxw_latest_year(master))
  d <- master[master$year %in% yrs & is.finite(master$u5mr), ]
  if (!nrow(d)) return(NULL)
  w <- reshape2::dcast(d, iso3_code + country_name + continent ~ year,
                        value.var = "u5mr")
  cols <- as.character(yrs)
  if (!all(cols %in% names(w))) return(NULL)
  w$delta <- w[[cols[1]]] - w[[cols[2]]]
  w <- w[is.finite(w$delta), ]
  w <- w[order(-w$delta), ]
  top <- utils::head(w, 25)
  top$country_name <- factor(top$country_name,
    levels = rev(top$country_name))
  p <- plotly::plot_ly(top, x = ~delta, y = ~country_name,
                        color = ~continent, type = "bar",
                        orientation = "h",
                        hovertemplate = "%{y}: -%{x:.0f} \u70b9<extra>%{fullData.name}</extra>")
  .gxw_layout(p,
    title = "U5MR \u4e0b\u964d\u6700\u591a 25 \u56fd",
    subtitle = "2000 \u2212 2023 \u00b7 \u5355\u4f4d\uff1a/1000 \u6d3b\u4ea7")
}

# 9. external aid top 25 bar (latest year)
widget_more_ext_top_bar <- function(master) {
  if (!requireNamespace("plotly", quietly = TRUE)) return(NULL)
  yr <- .gxw_latest_year(master)
  d <- master[master$year == yr & is.finite(master$ext_che), ]
  if (!nrow(d)) return(NULL)
  d <- d[order(-d$ext_che), ]
  top <- utils::head(d, 25)
  top$country_name <- factor(top$country_name,
    levels = rev(top$country_name))
  p <- plotly::plot_ly(top, x = ~ext_che, y = ~country_name,
                        color = ~continent, type = "bar",
                        orientation = "h",
                        hovertemplate = "%{y}: %{x:.1f}%<extra>%{fullData.name}</extra>")
  p <- plotly::layout(p, xaxis = list(ticksuffix = "%"))
  .gxw_layout(p,
    title = sprintf("\u5916\u63f4\u5360 CHE \u6700\u9ad8 25 \u56fd \u00b7 %d", yr),
    subtitle = "EXT > 30% \u4e3a\u9ad8\u4f9d\u8d56\u9608\u503c")
}

# 10. OOPS vs GDP/cap bubble (latest year)
widget_more_oops_gdp <- function(master) {
  if (!requireNamespace("plotly", quietly = TRUE)) return(NULL)
  yr <- .gxw_latest_year(master)
  d <- master[master$year == yr &
                is.finite(master$hf3_che) &
                is.finite(master$gdp_pc_usd) &
                is.finite(master$pop), ]
  if (nrow(d) < 30) return(NULL)
  p <- plotly::plot_ly(d, x = ~gdp_pc_usd, y = ~hf3_che,
                        size = ~pmax(pop, 1), color = ~continent,
                        text = ~country_name, type = "scatter", mode = "markers",
                        marker = list(opacity = .75, sizemode = "area",
                                      sizeref = 1e6),
                        hovertemplate = paste0(
                          "<b>%{text}</b><br>GDP/cap: $%{x:,.0f}<br>",
                          "OOPS: %{y:.1f}%<extra></extra>"))
  p <- plotly::layout(p,
    xaxis = list(type = "log", title = "GDP per capita (USD, log)"),
    yaxis = list(ticksuffix = "%", title = "OOPS / CHE"))
  .gxw_layout(p,
    title = sprintf("OOPS \u00d7 GDP/cap \u00b7 %d \u5e74\u6c14\u6ce1", yr),
    subtitle = "\u70b9\u5927\u5c0f = \u4eba\u53e3")
}

# 11. OOPS vs life expectancy bubble
widget_more_oops_life <- function(master) {
  if (!requireNamespace("plotly", quietly = TRUE)) return(NULL)
  yr <- .gxw_latest_year(master)
  d <- master[master$year == yr &
                is.finite(master$hf3_che) &
                is.finite(master$life_exp) &
                is.finite(master$pop), ]
  if (nrow(d) < 30) return(NULL)
  p <- plotly::plot_ly(d, x = ~hf3_che, y = ~life_exp,
                        size = ~pmax(pop, 1), color = ~continent,
                        text = ~country_name, type = "scatter", mode = "markers",
                        marker = list(opacity = .75, sizemode = "area",
                                      sizeref = 1e6),
                        hovertemplate = paste0(
                          "<b>%{text}</b><br>OOPS: %{x:.1f}%<br>",
                          "Life exp: %{y:.1f} \u5e74<extra></extra>"))
  p <- plotly::layout(p, xaxis = list(ticksuffix = "%"))
  .gxw_layout(p,
    title = sprintf("OOPS \u00d7 \u9884\u671f\u5bff\u547d \u00b7 %d", yr),
    subtitle = "\u9ad8 OOPS \u4e0e\u4f4e\u5bff\u547d\u540c\u73b0")
}

# 12. CHE per capita vs life expectancy bubble (log)
widget_more_che_life <- function(master) {
  if (!requireNamespace("plotly", quietly = TRUE)) return(NULL)
  yr <- .gxw_latest_year(master)
  d <- master[master$year == yr &
                is.finite(master$che_pc_usd2023) &
                is.finite(master$life_exp) &
                is.finite(master$pop), ]
  if (nrow(d) < 30) return(NULL)
  p <- plotly::plot_ly(d, x = ~che_pc_usd2023, y = ~life_exp,
                        size = ~pmax(pop, 1), color = ~income_group,
                        text = ~country_name, type = "scatter", mode = "markers",
                        marker = list(opacity = .75, sizemode = "area",
                                      sizeref = 1e6),
                        hovertemplate = paste0(
                          "<b>%{text}</b><br>CHE/cap: $%{x:,.0f}<br>",
                          "Life exp: %{y:.1f}<extra></extra>"))
  p <- plotly::layout(p,
    xaxis = list(type = "log", title = "CHE per capita (USD2023, log)"),
    yaxis = list(title = "Life expectancy"))
  .gxw_layout(p,
    title = sprintf("\u4eba\u5747 CHE \u00d7 \u5bff\u547d \u00b7 %d", yr),
    subtitle = "\u989c\u8272 = \u6536\u5165\u7ec4\uff1b\u70b9\u5927\u5c0f = \u4eba\u53e3")
}

# 13. CHE per capita vs U5MR log-log bubble
widget_more_che_u5 <- function(master) {
  if (!requireNamespace("plotly", quietly = TRUE)) return(NULL)
  yr <- .gxw_latest_year(master)
  d <- master[master$year == yr &
                is.finite(master$che_pc_usd2023) &
                is.finite(master$u5mr) &
                is.finite(master$pop), ]
  if (nrow(d) < 30) return(NULL)
  p <- plotly::plot_ly(d, x = ~che_pc_usd2023, y = ~u5mr,
                        size = ~pmax(pop, 1), color = ~continent,
                        text = ~country_name, type = "scatter", mode = "markers",
                        marker = list(opacity = .75, sizemode = "area",
                                      sizeref = 1e6),
                        hovertemplate = paste0(
                          "<b>%{text}</b><br>CHE/cap: $%{x:,.0f}<br>",
                          "U5MR: %{y:.1f} /1000<extra></extra>"))
  p <- plotly::layout(p,
    xaxis = list(type = "log", title = "CHE per capita (USD2023, log)"),
    yaxis = list(type = "log", title = "U5MR (per 1000, log)"))
  .gxw_layout(p,
    title = sprintf("\u4eba\u5747 CHE \u00d7 U5MR \u00b7 %d", yr),
    subtitle = "\u53cc\u5bf9\u6570\u8f74\u5448\u73b0\u8d1f\u5e42\u5f8b")
}

# 14. yoy heatmap top 30 by CHE
widget_more_yoy_heatmap <- function(master) {
  if (!requireNamespace("plotly", quietly = TRUE)) return(NULL)
  ranking <- master[master$year == .gxw_latest_year(master) &
                      is.finite(master$che_pc_usd2023), ]
  ranking <- ranking[order(-ranking$che_pc_usd2023), ]
  iso_top <- utils::head(ranking, 30)$iso3_code
  d <- master[master$iso3_code %in% iso_top &
                is.finite(master$che_pc_usd2023), ]
  if (!nrow(d)) return(NULL)
  d <- d[order(d$iso3_code, d$year), ]
  d_split <- split(d, d$iso3_code)
  d <- do.call(rbind, lapply(d_split, function(g) {
    if (nrow(g) < 2) return(NULL)
    g$yoy <- c(NA_real_, diff(log(g$che_pc_usd2023))) * 100
    g
  }))
  d <- d[is.finite(d$yoy), ]
  if (!nrow(d)) return(NULL)
  m <- reshape2::dcast(d, country_name ~ year, value.var = "yoy")
  rownames(m) <- m$country_name; m$country_name <- NULL
  m <- as.matrix(m)
  p <- plotly::plot_ly(z = m, x = colnames(m), y = rownames(m),
                        type = "heatmap",
                        colorscale = "RdBu", reversescale = TRUE,
                        zmid = 0,
                        hovertemplate = "%{y} \u00b7 %{x}<br>YoY: %{z:.1f}%<extra></extra>")
  p <- plotly::layout(p, xaxis = list(dtick = 2),
                      yaxis = list(autorange = "reversed"))
  .gxw_layout(p,
    title = "\u4eba\u5747 CHE \u540c\u6bd4\u589e\u901f \u00b7 \u603b\u989d\u524d 30 \u56fd",
    subtitle = "\u8d1f\u503c\u8d1f\u589e\u957f\uff1b2008 \u00b7 2020 \u4e24\u8f6e\u51b2\u51fb\u53ef\u89c1")
}

# 15. correlation timeseries lines
widget_more_corr_overtime <- function(master) {
  if (!requireNamespace("plotly", quietly = TRUE)) return(NULL)
  d <- master[is.finite(master$che_pc_usd2023) &
                is.finite(master$life_exp) &
                is.finite(master$u5mr) &
                is.finite(master$hf3_che), ]
  if (!nrow(d)) return(NULL)
  yrs <- sort(unique(d$year))
  out <- do.call(rbind, lapply(yrs, function(y) {
    g <- d[d$year == y, ]
    if (nrow(g) < 30) return(NULL)
    data.frame(year = y,
               r_life = stats::cor(log(g$che_pc_usd2023), g$life_exp),
               r_u5   = stats::cor(log(g$che_pc_usd2023), log(g$u5mr)),
               r_oops = stats::cor(log(g$che_pc_usd2023), g$hf3_che))
  }))
  if (is.null(out) || !nrow(out)) return(NULL)
  p <- plotly::plot_ly(out, x = ~year)
  p <- plotly::add_trace(p, y = ~r_life,
                          name = "ln CHE \u00d7 \u5bff\u547d (+)",
                          type = "scatter", mode = "lines+markers",
                          line = list(color = "#1B5E88", width = 2.5),
                          marker = list(color = "#1B5E88"))
  p <- plotly::add_trace(p, y = ~r_u5,
                          name = "ln CHE \u00d7 ln U5MR (\u2212)",
                          type = "scatter", mode = "lines+markers",
                          line = list(color = "#C46B27", width = 2.5),
                          marker = list(color = "#C46B27"))
  p <- plotly::add_trace(p, y = ~r_oops,
                          name = "ln CHE \u00d7 OOPS (\u2212)",
                          type = "scatter", mode = "lines+markers",
                          line = list(color = "#2A857A", width = 2.5),
                          marker = list(color = "#2A857A"))
  p <- plotly::layout(p, yaxis = list(title = "Pearson r"))
  .gxw_layout(p,
    title = "CHE \u4e0e\u5065\u5eb7\u4ea7\u51fa\u7684\u8de8\u56fd\u76f8\u5173 \u00b7 \u968f\u65f6\u95f4\u6f02\u79fb",
    subtitle = "\u5404\u5e74\u72ec\u7acb\u8ba1\u7b97 Pearson r")
}

# 16. country compare CHE/cap multi-line
widget_more_country_compare <- function(master,
                                          picks = c("CHN", "IND", "USA",
                                                    "BRA", "NGA", "DEU")) {
  if (!requireNamespace("plotly", quietly = TRUE)) return(NULL)
  d <- master[master$iso3_code %in% picks &
                is.finite(master$che_pc_usd2023), ]
  if (!nrow(d)) return(NULL)
  p <- plotly::plot_ly(d, x = ~year, y = ~che_pc_usd2023,
                        color = ~country_name, type = "scatter",
                        mode = "lines+markers",
                        hovertemplate = "%{x}: $%{y:,.0f}<extra>%{fullData.name}</extra>")
  p <- plotly::layout(p, yaxis = list(type = "log",
                                       title = "CHE per capita (USD2023, log)"))
  .gxw_layout(p,
    title = "6 \u56fd\u4eba\u5747 CHE \u52a8\u6001 \u00b7 log \u7eb5\u8f74",
    subtitle = "CHN / IND / USA / BRA / NGA / DEU")
}

# 17. correlation matrix heatmap (latest year)
widget_more_corr_matrix <- function(master) {
  if (!requireNamespace("plotly", quietly = TRUE)) return(NULL)
  yr <- .gxw_latest_year(master)
  cols <- intersect(c("che_pc_usd2023", "hf3_che", "gghed_che",
                       "ext_che", "pvtd_che", "life_exp", "u5mr",
                       "gdp_pc_usd"), names(master))
  if (length(cols) < 4) return(NULL)
  d <- master[master$year == yr, cols]
  d <- d[stats::complete.cases(d), ]
  if (!nrow(d)) return(NULL)
  m <- stats::cor(d, use = "pairwise.complete.obs")
  p <- plotly::plot_ly(z = m, x = colnames(m), y = rownames(m),
                        type = "heatmap", colorscale = "RdBu",
                        reversescale = TRUE, zmin = -1, zmax = 1,
                        hovertemplate = "%{y} \u2194 %{x}<br>r = %{z:.2f}<extra></extra>")
  p <- plotly::layout(p, yaxis = list(autorange = "reversed"))
  .gxw_layout(p,
    title = sprintf("\u591a\u53d8\u91cf\u76f8\u5173\u70ed\u529b\u56fe \u00b7 %d", yr),
    subtitle = "Pearson r \u00b7 \u4ec5\u4f9d\u9760\u88c5\u8f7d\u53d8\u91cf")
}

# 18. DT full master atlas (DT data atlas covering more columns)
widget_more_dt_atlas <- function(master) {
  if (!requireNamespace("DT", quietly = TRUE)) return(NULL)
  show_cols <- intersect(c(
    "country_name", "iso3_code", "year", "continent", "income_group",
    "che_usd2023", "che_pc_usd2023", "gghed_che", "pvtd_che", "ext_che",
    "hf1_che", "hf2_che", "hf3_che",
    "hc1_che", "hc6_che",
    "gdp_pc_usd", "life_exp", "u5mr", "pop"), names(master))
  d <- master[, show_cols]
  DT::datatable(
    d,
    extensions = c("Buttons", "Scroller"),
    options = list(
      pageLength = 25,
      dom = "Bfrtip",
      buttons = c("copy", "csv", "excel"),
      deferRender = TRUE,
      scrollY = 500, scroller = TRUE,
      initComplete = DT::JS(
        "function(){",
        "$(this.api().table().header()).css({",
        "  'background-color':'#FAF7F2',",
        "  'border-bottom':'2px solid #1A1A1F',",
        "  'font-family':'Inter,system-ui,sans-serif'});}")
    ),
    rownames = FALSE, filter = "top", class = "compact stripe"
  ) |>
    DT::formatRound(columns = intersect(c("che_usd2023", "che_pc_usd2023",
                                            "gdp_pc_usd", "life_exp", "u5mr",
                                            "pop", "gghed_che", "pvtd_che",
                                            "ext_che", "hf1_che", "hf2_che",
                                            "hf3_che", "hc1_che", "hc6_che"),
                                          names(d)), digits = 1)
}

# ---- 入口 ------------------------------------------------------------------

#' 第二批 18 个交互组件扩展
#' @export
ghs_export_widgets_more <- function(master,
                                    out_dir = file.path("分析输出", "交互组件"),
                                    verbose = TRUE) {
  if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
    message("htmlwidgets missing - skip"); return(invisible(0))
  }
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  jobs <- list(
    list(id = "wm_continent_oops",   fn = function() widget_more_continent_oops(master)),
    list(id = "wm_global_hf",        fn = function() widget_more_global_hf(master)),
    list(id = "wm_income_che",       fn = function() widget_more_income_che(master)),
    list(id = "wm_che_gdp_world",    fn = function() widget_more_che_gdp_world(master)),
    list(id = "wm_oops_dumbbell",    fn = function() widget_more_oops_dumbbell(master)),
    list(id = "wm_che_cagr_bar",     fn = function() widget_more_che_cagr_bar(master)),
    list(id = "wm_life_top_bar",     fn = function() widget_more_life_top_bar(master)),
    list(id = "wm_u5mr_top_bar",     fn = function() widget_more_u5mr_top_bar(master)),
    list(id = "wm_ext_top_bar",      fn = function() widget_more_ext_top_bar(master)),
    list(id = "wm_oops_gdp",         fn = function() widget_more_oops_gdp(master)),
    list(id = "wm_oops_life",        fn = function() widget_more_oops_life(master)),
    list(id = "wm_che_life",         fn = function() widget_more_che_life(master)),
    list(id = "wm_che_u5",           fn = function() widget_more_che_u5(master)),
    list(id = "wm_yoy_heatmap",      fn = function() widget_more_yoy_heatmap(master)),
    list(id = "wm_corr_overtime",    fn = function() widget_more_corr_overtime(master)),
    list(id = "wm_country_compare",  fn = function() widget_more_country_compare(master)),
    list(id = "wm_corr_matrix",      fn = function() widget_more_corr_matrix(master)),
    list(id = "wm_dt_atlas",         fn = function() widget_more_dt_atlas(master))
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
                 source = "WHO GHED · 程序/25_widgets_more.R")
      }
      if (verbose) cat("[wm_widget]", j$id, "saved\n")
      ok <- ok + 1
    }, error = function(e) {
      if (verbose) message("[", j$id, "] save failed: ", conditionMessage(e))
    })
  }
  invisible(ok)
}
