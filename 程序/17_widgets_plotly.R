
.plotly_v2_layout <- function(p, title = NULL, subtitle = NULL,
                               source = "WHO GHED 2024") {
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
      font = list(family = "Inter, system-ui, sans-serif", size = 18,
                  color = "#1A1A1F")
    ),
    font = list(family = "Inter, system-ui, sans-serif", color = "#1A1A1F"),
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor  = "rgba(0,0,0,0)",
    hovermode = "closest",
    margin = list(t = 70, r = 16, b = 60, l = 60),
    annotations = list(
      list(
        x = 0, y = -0.2, xref = "paper", yref = "paper",
        text = sprintf("<i>Source · %s · 庄颂 (20241334)</i>", source),
        showarrow = FALSE, xanchor = "left",
        font = list(size = 11, color = "#5C5C66")
      )
    )
  )
}

widget_v2_gapminder_bubble <- function(master) {
  if (!requireNamespace("plotly", quietly = TRUE)) return(NULL)
  d <- master[is.finite(master$gdp_pc_usd) &
              is.finite(master$life_exp) &
              is.finite(master$che_pc_usd2023) &
              !is.na(master$continent), ]
  if (nrow(d) == 0) return(NULL)

  p <- plotly::plot_ly(
    data = d,
    x = ~gdp_pc_usd, y = ~life_exp,
    size = ~pmax(che_pc_usd2023, 1),
    color = ~continent, frame = ~year,
    text = ~country_name,
    type = "scatter", mode = "markers",
    marker = list(opacity = 0.7, sizemode = "area", sizeref = 0.06),
    hovertemplate = paste0(
      "<b>%{text}</b><br>",
      "GDP/cap: $%{x:,.0f}<br>",
      "Life exp: %{y:.1f}<br>",
      "CHE/cap: $%{marker.size:,.0f}<extra></extra>"
    )
  ) |>
  plotly::layout(
    xaxis = list(type = "log", title = "GDP per capita (USD, log)"),
    yaxis = list(title = "Life expectancy (years)")
  )

  .plotly_v2_layout(p,
    title = "Gapminder 风\u00b7\u4e09\u53d8\u91cf\u52a8\u6001",
    subtitle = "GDP/cap \u00d7 \u5bff\u547d \u00d7 CHE/cap \u5927\u5c0f\u00b7\u62d6\u5e74\u4efd")
}

widget_v2_highlight_lines <- function(master,
                                       countries = c("USA","CHN","IND","BRA","NGA","DEU","JPN","KEN","BRA","NOR")) {
  if (!requireNamespace("plotly", quietly = TRUE)) return(NULL)
  d <- master[master$iso3_code %in% countries &
              is.finite(master$che_pc_usd2023), ]
  if (nrow(d) == 0) return(NULL)

  p <- plotly::plot_ly(d, x = ~year, y = ~che_pc_usd2023,
                        color = ~country_name, type = "scatter",
                        mode = "lines+markers",
                        line = list(width = 2),
                        marker = list(size = 5),
                        hovertemplate = paste0(
                          "<b>%{fullData.name}</b><br>",
                          "%{x}: $%{y:,.0f}<extra></extra>"
                        )) |>
    plotly::layout(
      xaxis = list(title = NULL),
      yaxis = list(title = "CHE per capita (USD)"),
      legend = list(orientation = "h", y = -0.2)
    )
  .plotly_v2_layout(p,
    title = "10 \u56fd CHE/cap \u8d70\u52bf",
    subtitle = "\u70b9\u51fb\u56fe\u4f8b\u9ad8\u4eae\u5355\u56fd")
}

widget_v2_oops_heatmap <- function(master, top_n = 30) {
  if (!requireNamespace("plotly", quietly = TRUE)) return(NULL)
  if (!"hf3_che" %in% names(master)) return(NULL)
  most_var <- master |>
    dplyr::filter(is.finite(hf3_che)) |>
    dplyr::group_by(country_name) |>
    dplyr::summarise(rng = diff(range(hf3_che, na.rm = TRUE)), .groups = "drop") |>
    dplyr::slice_max(rng, n = top_n, with_ties = FALSE)
  d <- master[master$country_name %in% most_var$country_name &
              is.finite(master$hf3_che), ]
  m <- reshape2::dcast(d, country_name ~ year, value.var = "hf3_che")
  rownames(m) <- m$country_name; m$country_name <- NULL
  m <- as.matrix(m)

  p <- plotly::plot_ly(
    z = m, x = colnames(m), y = rownames(m),
    type = "heatmap",
    colorscale = "RdBu", reversescale = TRUE,
    zmid = 50,
    hovertemplate = "%{y} \u00b7 %{x}<br>OOPS: %{z:.1f}%<extra></extra>"
  ) |>
    plotly::layout(
      xaxis = list(title = NULL, dtick = 2),
      yaxis = list(title = NULL, autorange = "reversed")
    )

  .plotly_v2_layout(p,
    title = "OOPS \u70ed\u529b\u56fe\u00b7\u53d8\u52a8\u6700\u5927 30 \u56fd",
    subtitle = "\u989c\u8272\u8d8a\u7ea2\u00b7OOPS \u8d8a\u9ad8")
}

widget_v2_ternary <- function(master, year = 2022) {
  if (!requireNamespace("plotly", quietly = TRUE)) return(NULL)
  d <- master[master$year == year &
              is.finite(master$hf1_che) &
              is.finite(master$hf2_che) &
              is.finite(master$hf3_che) &
              !is.na(master$continent), ]
  if (nrow(d) == 0) return(NULL)

  p <- plotly::plot_ly(d, type = "scatterternary", mode = "markers",
                        a = ~hf1_che, b = ~hf2_che, c = ~hf3_che,
                        text = ~country_name, color = ~continent,
                        marker = list(size = 9, opacity = 0.75,
                                      line = list(width = 0.4, color = "#fff")),
                        hovertemplate = paste0(
                          "<b>%{text}</b><br>",
                          "HF1: %{a:.0f}%<br>",
                          "HF2: %{b:.0f}%<br>",
                          "HF3 (OOPS): %{c:.0f}%<extra></extra>"
                        )) |>
    plotly::layout(
      ternary = list(
        sum = 100,
        aaxis = list(title = "HF1 \u00b7 \u653f\u5e9c"),
        baxis = list(title = "HF2 \u00b7 \u4fdd\u9669"),
        caxis = list(title = "HF3 \u00b7 OOPS")
      )
    )

  .plotly_v2_layout(p,
    title = "\u4e09\u5143\u7ed3\u6784\u00b7HF1/HF2/HF3",
    subtitle = sprintf("%d\u5e74\u00b7%d\u4e2a\u56fd\u5bb6", year, nrow(d)))
}

widget_v2_income_violin <- function(master) {
  if (!requireNamespace("plotly", quietly = TRUE)) return(NULL)
  d <- master[is.finite(master$che_pc_usd2023) &
              !is.na(master$income_group), ]
  if (nrow(d) == 0) return(NULL)
  d$income_group <- factor(d$income_group,
                           levels = c("Low","Lower middle","Upper middle","High"))

  p <- plotly::plot_ly(d, x = ~income_group, y = ~che_pc_usd2023,
                        color = ~income_group, frame = ~year,
                        type = "violin", box = list(visible = TRUE),
                        meanline = list(visible = TRUE),
                        hovertemplate = "%{x}<br>$%{y:,.0f}<extra></extra>") |>
    plotly::layout(
      yaxis = list(type = "log", title = "CHE per capita (USD, log)"),
      xaxis = list(title = NULL),
      showlegend = FALSE
    )
  .plotly_v2_layout(p,
    title = "\u6536\u5165\u7ec4\u00b7\u4eba\u5747 CHE \u5c0f\u63d0\u7434",
    subtitle = "\u62d6\u5e74\u4efd\u00b7\u67e5\u770b\u5206\u5e03\u6f02\u79fb")
}

widget_v2_splom <- function(master, year = 2022) {
  if (!requireNamespace("plotly", quietly = TRUE)) return(NULL)
  d <- master[master$year == year &
              is.finite(master$che_pc_usd2023) &
              is.finite(master$gdp_pc_usd) &
              is.finite(master$life_exp) &
              is.finite(master$hf3_che) &
              !is.na(master$continent), ]
  if (nrow(d) == 0) return(NULL)

  dims <- list(
    list(label = "log CHE/cap",  values = log10(d$che_pc_usd2023)),
    list(label = "log GDP/cap",  values = log10(d$gdp_pc_usd)),
    list(label = "Life exp",     values = d$life_exp),
    list(label = "OOPS %",       values = d$hf3_che)
  )

  p <- plotly::plot_ly(
    type = "splom", dimensions = dims,
    marker = list(color = as.factor(d$continent),
                  opacity = 0.7, size = 5,
                  line = list(width = 0.3, color = "#fff")),
    text = d$country_name,
    hovertemplate = "<b>%{text}</b><extra></extra>"
  )
  .plotly_v2_layout(p,
    title = "\u516d\u7ef4\u6563\u70b9\u77e9\u9635",
    subtitle = sprintf("%d\u5e74\u00b7\u989c\u8272=\u5927\u6d32", year))
}

widget_v2_mc_fan <- function(master, n_sim = 500, country_iso = "BRA") {
  if (!requireNamespace("plotly", quietly = TRUE)) return(NULL)
  if (!exists("mc_scenarios", mode = "function")) {
    out <- data.frame(year = 2024:2030,
                      lo = NA_real_, mid = NA_real_, hi = NA_real_)
  } else {
    out <- tryCatch(
      mc_scenarios(master, country_iso = country_iso, n_sim = n_sim),
      error = function(e) NULL)
    if (is.null(out)) {
      out <- data.frame(year = 2024:2030,
                        lo = NA_real_, mid = NA_real_, hi = NA_real_)
    }
  }
  cols <- c("year","lo","mid","hi")
  if (!all(cols %in% names(out))) {
    nm <- names(out)
    if ("p10" %in% nm) names(out)[match("p10", nm)] <- "lo"
    if ("p50" %in% nm) names(out)[match("p50", nm)] <- "mid"
    if ("p90" %in% nm) names(out)[match("p90", nm)] <- "hi"
  }

  p <- plotly::plot_ly(out, x = ~year) |>
    plotly::add_ribbons(ymin = ~lo, ymax = ~hi,
                        line = list(width = 0),
                        fillcolor = "rgba(27,94,136,0.18)",
                        name = "P10\u2013P90") |>
    plotly::add_lines(y = ~mid,
                      line = list(color = "#1B5E88", width = 3),
                      name = "median") |>
    plotly::layout(yaxis = list(title = "Life-year gain"),
                   xaxis = list(title = NULL))
  .plotly_v2_layout(p,
    title = sprintf("\u8499\u7279\u5361\u7f57\u60c5\u666f\u00b7%s", country_iso),
    subtitle = sprintf("%d \u6b21\u6a21\u62df\u00b7\u5bff\u547d\u589e\u76ca\u533a\u95f4", n_sim))
}

widget_v2_scenarios <- function(master, country_iso = "CHN") {
  if (!requireNamespace("plotly", quietly = TRUE)) return(NULL)
  d <- master[master$iso3_code == country_iso &
              is.finite(master$che_pc_usd2023), ]
  if (nrow(d) < 5) return(NULL)
  last_yr <- max(d$year)
  last_v  <- d$che_pc_usd2023[d$year == last_yr]

  yrs_f <- (last_yr + 1):(last_yr + 7)
  growth <- list(`Baseline` = 0.03, `OECD-pace` = 0.06, `Austerity` = -0.01)
  paths <- lapply(names(growth), function(g) {
    v <- last_v * cumprod(rep(1 + growth[[g]], length(yrs_f)))
    data.frame(year = yrs_f, value = v, scenario = g)
  })
  paths <- do.call(rbind, paths)

  hist <- data.frame(year = d$year, value = d$che_pc_usd2023,
                     scenario = "Historical")

  combined <- rbind(hist, paths)
  pal <- c(`Historical` = "#1A1A1F",
           `Baseline`   = "#5C5C66",
           `OECD-pace`  = "#1B5E88",
           `Austerity`  = "#A03B27")

  p <- plotly::plot_ly()
  for (s in unique(combined$scenario)) {
    dd <- combined[combined$scenario == s, ]
    p <- plotly::add_lines(p, data = dd, x = ~year, y = ~value,
                            line = list(color = pal[[s]], width = 2.5,
                                        dash = if (s == "Historical") "solid" else "dot"),
                            name = s,
                            hovertemplate = paste0("%{x}: $%{y:,.0f}<extra>", s, "</extra>"))
  }
  p <- plotly::layout(p,
    xaxis = list(title = NULL),
    yaxis = list(title = "CHE per capita (USD)")
  )
  .plotly_v2_layout(p,
    title = sprintf("\u4e09\u60c5\u666f\u00b7%s CHE/cap", country_iso),
    subtitle = "\u57fa\u7ebf 3% / OECD 6% / \u7d27\u7f29 -1%\u00b7\u672a\u67657\u5e74")
}


ghs_export_v2_widgets_plotly <- function(master,
                                          out_dir = file.path("分析输出", "交互组件"),
                                          verbose = TRUE) {
  if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
    message("htmlwidgets missing - skip"); return(invisible(0))
  }
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  jobs <- list(
    list(id = "v2_w_gapminder_bubble",  fn = function() widget_v2_gapminder_bubble(master)),
    list(id = "v2_w_highlight_lines",   fn = function() widget_v2_highlight_lines(master)),
    list(id = "v2_w_oops_heatmap",      fn = function() widget_v2_oops_heatmap(master)),
    list(id = "v2_w_ternary",           fn = function() widget_v2_ternary(master)),
    list(id = "v2_w_income_violin",     fn = function() widget_v2_income_violin(master)),
    list(id = "v2_w_splom",             fn = function() widget_v2_splom(master)),
    list(id = "v2_w_mc_fan",            fn = function() widget_v2_mc_fan(master)),
    list(id = "v2_w_scenarios",         fn = function() widget_v2_scenarios(master))
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
                 source = "WHO GHED · 程序/17_widgets_plotly.R")
      }
      if (verbose) cat("[v2_widget]", j$id, "saved\n")
      ok <- ok + 1
    }, error = function(e) {
      if (verbose) message("[", j$id, "] save failed: ", conditionMessage(e))
    })
  }
  invisible(ok)
}
