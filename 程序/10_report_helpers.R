# =============================================================================
# 程序/10_report_helpers.R
# -----------------------------------------------------------------------------
# 报告 / 文章生成辅助函数：批量导出图表、生成摘要、写交互 HTML widget。
# =============================================================================

if (!exists("proj_root", mode = "function")) {
  source(file.path("程序", "00_utils.R"))
}

# =============================================================================
# 1. 文本数字摘要（给 Quarto / Rmd inline 用）
# =============================================================================

#' Master 表的摘要数字
#' @param master 宽表
#' @return list(numeric)
ghs_headline_numbers <- function(master) {
  ensure_pkgs(c("dplyr"))
  cur_year <- max(master$year, na.rm = TRUE)
  totals <- master |>
    dplyr::filter(.data$year == cur_year, is.finite(.data$che_usd2023)) |>
    dplyr::summarise(global_che = sum(.data$che_usd2023, na.rm = TRUE),
                     n_countries = dplyr::n_distinct(.data$iso3_code))
  base_row <- master |>
    dplyr::filter(.data$year == 2000, is.finite(.data$che_usd2023)) |>
    dplyr::summarise(che_2000 = sum(.data$che_usd2023, na.rm = TRUE))
  pct_country <- function(year_x, col) {
    master |>
      dplyr::filter(.data$year == year_x, is.finite(.data[[col]])) |>
      dplyr::pull(col) |>
      mean(na.rm = TRUE)
  }
  list(
    cur_year      = cur_year,
    n_countries   = totals$n_countries,
    global_che    = totals$global_che,
    che_2000      = base_row$che_2000,
    growth_rate   = (totals$global_che / base_row$che_2000)^(1 / (cur_year - 2000)) - 1,
    mean_oops_cur = pct_country(cur_year, "hf3_che"),
    mean_gov_cur  = pct_country(cur_year, "gghed_che"),
    mean_ext_cur  = pct_country(cur_year, "ext_che"),
    median_che_pc = stats::median(
      master$che_pc_usd2023[master$year == cur_year], na.rm = TRUE
    )
  )
}

# =============================================================================
# 2. 批量导出静态图（一行调用产出 25+ 张 PNG/SVG）
# =============================================================================

#' 批量产出全部 chapter-level 静态图
#'
#' @param master 宽表
#' @param world_sf 可空，传入用于 choropleth 的 sf 对象
#' @param outputs_dir 输出目录
#' @param formats 格式（默认 png 与 svg）
#' @param verbose 是否打印每张图进度
#' @return invisible(character) 已生成图的相对路径
ghs_export_all_static <- function(master,
                                  world_sf = NULL,
                                  outputs_dir = file.path(proj_root(),
                                                            "分析输出", "图表"),
                                  formats = c("png", "svg"),
                                  verbose = TRUE) {
  ensure_pkgs(c("ggplot2"))
  dir.create(outputs_dir, recursive = TRUE, showWarnings = FALSE)
  recipes <- list(
    list(name = "01_global_sources_area",
         fn = function() plot_source_area(master),
         w = 11, h = 6),
    list(name = "02_oops_ranking_2023",
         fn = function() plot_oops_ranking(master, year_focus = max(master$year)),
         w = 11, h = 8),
    list(name = "03_oops_ridges_income",
         fn = function() plot_oops_ridges_income(master, year_focus = max(master$year)),
         w = 10, h = 6),
    list(name = "04_oops_box_continent",
         fn = function() plot_oops_box_continent(master, year_focus = max(master$year)),
         w = 10, h = 6),
    list(name = "05_world_oops_2023",
         fn = function() if (!is.null(world_sf))
                            plot_world_choropleth(master, world_sf,
                                                  year_focus = max(master$year)),
         w = 12, h = 6),
    list(name = "06_profile_china",
         fn = function() plot_country_profile(master, "CHN"),
         w = 14, h = 9),
    list(name = "07_profile_usa",
         fn = function() plot_country_profile(master, "USA"),
         w = 14, h = 9),
    list(name = "08_profile_india",
         fn = function() plot_country_profile(master, "IND"),
         w = 14, h = 9),
    list(name = "09_covid_dumbbell",
         fn = function() plot_covid_dumbbell(master, top_n = 25),
         w = 11, h = 9),
    list(name = "10_oops_slope",
         fn = function() plot_slope_chart(master, value_col = "hf3_che",
                                           year_a = 2000,
                                           year_b = max(master$year),
                                           top_n = 20),
         w = 10, h = 8),
    list(name = "11_inequality_timeseries",
         fn = function() plot_inequality_timeseries(master),
         w = 10, h = 6),
    list(name = "12_che_pc_ridges",
         fn = function() plot_pc_ridges_income(master, year_focus = max(master$year)),
         w = 10, h = 6),
    list(name = "13_oops_heatmap",
         fn = function() plot_oops_heatmap(master, top_n = 50),
         w = 12, h = 11),
    list(name = "14_continent_radar",
         fn = function() plot_continent_radar(master, year_focus = max(master$year)),
         w = 10, h = 8),
    list(name = "15_small_multiples",
         fn = function() plot_small_multiples(master),
         w = 13, h = 7),
    list(name = "16_lollipop_income",
         fn = function() plot_income_oops_lollipop(master, year_focus = max(master$year)),
         w = 9, h = 5),
    list(name = "17_ext_density",
         fn = function() plot_ext_density(master, year_focus = max(master$year)),
         w = 9, h = 5),
    list(name = "18_oops_bump",
         fn = function() plot_bump_chart(master, top_n = 12,
                                           year_min = 2010,
                                           year_max = max(master$year)),
         w = 11, h = 8),
    list(name = "19_treemap",
         fn = function() plot_che_treemap(master, year_focus = max(master$year)),
         w = 12, h = 8),
    list(name = "20_stream_continent",
         fn = function() plot_stream_continent(master),
         w = 11, h = 6),
    list(name = "21_ternary",
         fn = function() plot_ternary_schemes(master, year_focus = max(master$year)),
         w = 9, h = 8),
    list(name = "22_inequality_pca",
         fn = function() {
           snap <- master |>
             dplyr::filter(.data$year == max(.data$year, na.rm = TRUE)) |>
             tidyr::drop_na("gghed_che", "pvtd_che", "ext_che", "hf3_che")
           pca <- fit_pca(snap, vars = c("gghed_che", "pvtd_che",
                                          "ext_che", "hf3_che"))
           cl <- fit_cluster(pca, k = 4)
           plot_pca_biplot(pca, cl)
         },
         w = 11, h = 8),
    list(name = "23_beta_convergence",
         fn = function() {
           bc <- fit_beta_convergence(master)
           plot_beta_convergence_scatter(bc)
         },
         w = 11, h = 7),
    list(name = "24_forecast_fan",
         fn = function() plot_forecast_fan(master,
                                            isos = c("CHN", "USA", "IND", "BRA"),
                                            h = 5),
         w = 12, h = 8),
    list(name = "25_bivariate_map",
         fn = function() if (!is.null(world_sf))
                            plot_bivariate_map(master, world_sf,
                                                year_focus = max(master$year)),
         w = 12, h = 7),
    list(name = "26_sankey_static",
         fn = function() plot_sankey_static(master, year_focus = max(master$year)),
         w = 11, h = 7),
    list(name = "27_waffle_purpose",
         fn = function() plot_waffle_purpose(master,
                                               year_focus = max(master$year)),
         w = 11, h = 8)
  )

  saved <- character(0)
  for (recipe in recipes) {
    if (verbose) logi("rendering: ", recipe$name)
    p <- tryCatch(recipe$fn(), error = function(e) {
      logw("  failed: ", recipe$name, " | ", conditionMessage(e))
      NULL
    })
    if (is.null(p)) next
    paths <- tryCatch(
      save_fig(p, recipe$name,
               width = recipe$w, height = recipe$h,
               formats = formats, dir = outputs_dir),
      error = function(e) {
        logw("  save_fig failed: ", recipe$name, " | ", conditionMessage(e))
        NULL
      }
    )
    if (!is.null(paths)) saved <- c(saved, paths)
  }
  if (verbose) logi("done; saved ", length(saved), " files (",
                     length(recipes), " recipes)")
  invisible(saved)
}

# =============================================================================
# 3. 批量导出交互 widget 为独立 HTML
# =============================================================================

#' 把交互 widget 写成 standalone HTML（适合 GitHub Pages 嵌入）
ghs_widget_escape <- function(x) {
  x <- as.character(if (is.null(x)) "" else x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x
}

ghs_widget_label <- function(path_or_name) {
  x <- tools::file_path_sans_ext(basename(path_or_name))
  x <- sub("^v2_w_", "", x)
  x <- sub("^[0-9]+_", "", x)
  trimws(gsub("_+", " ", x))
}

ghs_widget_shell <- function(path, title = NULL, caption = NULL,
                             source = "WHO GHED · standalone htmlwidget",
                             fallback = NULL) {
  if (is.null(path) || !file.exists(path)) return(invisible(FALSE))
  html <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  if (grepl("data-ghs-widget-shell", html, fixed = TRUE)) return(invisible(TRUE))
  label <- if (is.null(title) || !nzchar(title)) ghs_widget_label(path) else title
  cap <- if (is.null(caption) || !nzchar(caption)) {
    "交互组件已封装为 standalone HTML；若 iframe 内加载较慢，可等待脚本完成或改用新窗打开。"
  } else caption
  fb <- if (is.null(fallback) || !nzchar(fallback)) {
    "Fallback：如组件空白，请重新运行 Rscript 构建.R widgets，或在浏览器新窗打开当前 HTML。"
  } else fallback
  css <- paste0(
    "<style id=\"ghs-widget-shell-css\">",
    "body{margin:0;background:#FAF7F2;color:#1A1A1F;font-family:Inter,system-ui,-apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif}",
    ".ghs-widget-shell{box-sizing:border-box;min-height:100vh;padding:16px;background:linear-gradient(180deg,#FAF7F2,#fff)}",
    ".ghs-widget-caption,.ghs-widget-source{box-sizing:border-box;background:#fff;border:1px solid rgba(26,26,31,.10);border-radius:14px;padding:12px 16px;box-shadow:0 10px 28px rgba(26,26,31,.06)}",
    ".ghs-widget-caption{display:grid;gap:4px;margin-bottom:12px}.ghs-widget-caption b{font-family:'Source Serif 4',Georgia,serif;font-size:18px;color:#1B5E88}.ghs-widget-caption span{font-size:12.5px;color:#5A5A65;line-height:1.55}",
    ".ghs-widget-stage{position:relative;background:#fff;border:1px solid rgba(26,26,31,.08);border-radius:14px;padding:10px;min-height:420px;overflow:auto}",
    ".ghs-widget-loading,.ghs-widget-empty,.ghs-widget-error{position:absolute;inset:14px;display:flex;align-items:center;justify-content:center;text-align:center;border-radius:12px;background:rgba(250,247,242,.92);color:#5A5A65;font-size:13px;z-index:20}",
    ".ghs-widget-empty,.ghs-widget-error{display:none}.ghs-widget-shell.is-loaded .ghs-widget-loading{display:none}.ghs-widget-shell.is-empty .ghs-widget-empty,.ghs-widget-shell.has-error .ghs-widget-error{display:flex}",
    ".ghs-widget-source{margin-top:12px;display:flex;flex-wrap:wrap;gap:10px;justify-content:space-between;font-size:12px;color:#5A5A65}.ghs-widget-fallback{color:#C46B27;font-weight:700}",
    ".ghs-widget-stage>.html-widget,.ghs-widget-stage>.leaflet{max-width:100%}",
    "</style>"
  )
  shell_open <- sprintf(
    "<main class=\"ghs-widget-shell\" data-ghs-widget-shell=\"true\" data-widget-name=\"%s\"><header class=\"ghs-widget-caption\"><b>%s</b><span>%s</span></header><section class=\"ghs-widget-stage\"><div class=\"ghs-widget-loading\">Loading widget…</div><div class=\"ghs-widget-empty\">Empty state：没有检测到可渲染的 htmlwidget 容器。</div><div class=\"ghs-widget-error\">Error state：组件脚本运行异常。<span></span></div>",
    ghs_widget_escape(label), ghs_widget_escape(label), ghs_widget_escape(cap)
  )
  shell_close <- sprintf(
    "</section><footer class=\"ghs-widget-source\"><span>Source · %s</span><span class=\"ghs-widget-fallback\">%s</span></footer></main>",
    ghs_widget_escape(source), ghs_widget_escape(fb)
  )
  js <- paste0(
    "<script id=\"ghs-widget-shell-js\">",
    "(function(){function mark(){var s=document.querySelector('.ghs-widget-shell');if(!s)return;s.classList.add('is-loaded');var ok=s.querySelector('.html-widget,.leaflet,.plotly,svg,canvas,table');if(!ok)s.classList.add('is-empty');}",
    "window.addEventListener('error',function(e){var s=document.querySelector('.ghs-widget-shell');if(!s)return;s.classList.add('has-error');var m=s.querySelector('.ghs-widget-error span');if(m)m.textContent=e&&e.message?(' '+e.message):'';},true);",
    "if(document.readyState==='complete'||document.readyState==='interactive'){setTimeout(mark,500)}else{window.addEventListener('load',function(){setTimeout(mark,500)})}})();",
    "</script>"
  )
  if (grepl("</head>", html, ignore.case = TRUE)) {
    html <- sub("(?i)</head>", paste0(css, "\n</head>"), html, perl = TRUE)
  } else {
    html <- paste0(css, "\n", html)
  }
  body_open <- regexpr("(?is)<body[^>]*>", html, perl = TRUE)
  body_close <- regexpr("(?is)</body>", html, perl = TRUE)
  if (body_open[1] > 0 && body_close[1] > 0) {
    open_end <- body_open[1] + attr(body_open, "match.length") - 1
    before <- substr(html, 1, open_end)
    inner <- substr(html, open_end + 1, body_close[1] - 1)
    after <- substr(html, body_close[1], nchar(html))
    html <- paste0(before, "\n", shell_open, "\n", inner, "\n", shell_close, "\n", after)
    html <- sub("(?is)</body>", paste0(js, "\n</body>"), html, perl = TRUE)
  } else {
    html <- paste0(shell_open, "\n", html, "\n", shell_close, "\n", js)
  }
  writeLines(html, path, useBytes = TRUE)
  invisible(TRUE)
}

ghs_save_widget <- function(widget, name,
                              dir = file.path(proj_root(), "分析输出", "交互组件"),
                              selfcontained = TRUE) {
  if (is.null(widget)) return(invisible(NULL))
  if (!requireNamespace("htmlwidgets", quietly = TRUE)) return(invisible(NULL))
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(dir, paste0(name, ".html"))
  ok <- tryCatch({
    htmlwidgets::saveWidget(widget, file = path,
                              selfcontained = selfcontained)
    TRUE
  }, error = function(e) {
      logw("widget save failed: ", name, " | ", conditionMessage(e))
      FALSE
    })
  if (isTRUE(ok)) {
    ghs_widget_shell(path, title = ghs_widget_label(name),
                     source = "WHO GHED · 程序/10_report_helpers.R")
  }
  if (!isTRUE(ok)) return(invisible(NULL))
  invisible(path)
}

#' 批量导出全部交互 widget
ghs_export_all_widgets <- function(master, world_sf = NULL,
                                    dir = file.path(proj_root(),
                                                      "分析输出", "交互组件"),
                                    verbose = TRUE) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  produced <- character(0)
  recipes <- list(
    list(name = "01_gapminder_animated",
         fn = function() gapminder_plotly(master)),
    list(name = "02_highlight_ts",
         fn = function() highlight_country_ts(master)),
    list(name = "03_radar_5_countries",
         fn = function() radar_echarts(master)),
    list(name = "04_bar_race",
         fn = function() bar_race_plotly(master, top_n = 12)),
    list(name = "05_ternary",
         fn = function() ternary_plotly(master)),
    list(name = "06_forecast_subplot",
         fn = function() forecast_plotly(master)),
    list(name = "07_inequality",
         fn = function() inequality_plotly(master)),
    list(name = "08_heatmap_oops",
         fn = function() heatmap_oops_plotly(master, top_n = 30)),
    list(name = "09_sankey_sources",
         fn = function() sankey_sources_to_schemes(master,
                                                     year_focus = max(master$year))),
    list(name = "10_china_wb_line",
         fn = function() country_wb_plotly(master, "CHN")),
    list(name = "11_world_leaflet",
         fn = function() if (!is.null(world_sf))
                            leaflet_choropleth(master, world_sf,
                                               year_focus = max(master$year))),
    list(name = "12_reactable_rank",
         fn = function() reactable_country_rank(master,
                                                  year_focus = max(master$year)))
  )
  for (recipe in recipes) {
    if (verbose) logi("widget: ", recipe$name)
    w <- tryCatch(recipe$fn(), error = function(e) {
      logw("  failed: ", recipe$name, " | ", conditionMessage(e))
      NULL
    })
    p <- ghs_save_widget(w, recipe$name, dir = dir)
    if (!is.null(p)) produced <- c(produced, p)
  }
  if (verbose) logi("widgets saved: ", length(produced))
  invisible(produced)
}

# =============================================================================
# 4. 模型结果导出
# =============================================================================

#' 一键运行所有建模并导出 CSV / RDS
#'
#' @param master 宽表
#' @param outputs_dir 输出根目录（会写到 分析输出/模型表）
ghs_export_all_models <- function(master,
                                   outputs_dir = file.path(proj_root(),
                                                              "分析输出",
                                                              "模型表")) {
  ensure_pkgs(c("dplyr"))
  dir.create(outputs_dir, recursive = TRUE, showWarnings = FALSE)

  # (1) 不平等指数面板
  ineq <- tryCatch(inequality_by_year(master), error = function(e) NULL)
  if (!is.null(ineq)) {
    readr::write_csv(ineq, file.path(outputs_dir, "ineq_panel.csv"))
    saveRDS(ineq,        file.path(outputs_dir, "ineq_panel.rds"))
  }

  # (2) COVID 冲击表
  shock <- tryCatch(covid_shock(master), error = function(e) NULL)
  if (!is.null(shock)) {
    readr::write_csv(shock, file.path(outputs_dir, "covid_shock.csv"))
    saveRDS(shock,        file.path(outputs_dir, "covid_shock.rds"))
  }

  # (3) β-收敛
  bc <- tryCatch(fit_beta_convergence(master),
                  error = function(e) NULL)
  if (!is.null(bc)) {
    readr::write_csv(bc$panel, file.path(outputs_dir, "beta_panel.csv"))
    saveRDS(bc, file.path(outputs_dir, "beta_convergence.rds"))
  }

  # (4) 面板固定效应
  fe <- tryCatch(fit_panel_fe(master), error = function(e) NULL)
  if (!is.null(fe)) {
    saveRDS(fe, file.path(outputs_dir, "panel_fe.rds"))
    out <- tryCatch(broom::tidy(fe), error = function(e) NULL)
    if (!is.null(out)) readr::write_csv(out, file.path(outputs_dir,
                                                          "panel_fe_tidy.csv"))
  }

  # (5) PCA + cluster
  snap <- master |>
    dplyr::filter(.data$year == max(.data$year, na.rm = TRUE)) |>
    tidyr::drop_na("gghed_che", "pvtd_che", "ext_che", "hf3_che")
  pca <- tryCatch(fit_pca(snap, vars = c("gghed_che", "pvtd_che",
                                          "ext_che", "hf3_che")),
                   error = function(e) NULL)
  cl <- if (!is.null(pca)) fit_cluster(pca, k = 4) else NULL
  if (!is.null(cl)) {
    saveRDS(list(pca = pca, cluster = cl),
             file.path(outputs_dir, "pca_cluster.rds"))
    readr::write_csv(cl$scores,
                     file.path(outputs_dir, "pca_cluster_scores.csv"))
  }

  # (6) 预测面板（4 国 × 5 年）
  isos <- c("CHN", "USA", "IND", "BRA")
  preds <- lapply(isos, function(iso) {
    di <- master |>
      dplyr::filter(.data$iso3_code == iso) |>
      dplyr::arrange(.data$year)
    fc <- fit_forecast(di$che_pc_usd2023, di$year, h = 5)
    if (!is.null(fc)) {
      fc$iso3_code <- iso
      fc$country_name <- di$country_name[1]
    }
    fc
  })
  preds <- dplyr::bind_rows(Filter(Negate(is.null), preds))
  if (nrow(preds)) {
    readr::write_csv(preds, file.path(outputs_dir, "forecast_5y.csv"))
  }

  invisible(list(
    ineq = ineq, shock = shock, beta = bc, fe = fe,
    pca = pca, cluster = cl, forecast = preds
  ))
}

# =============================================================================
# 5. 一键全流程：data → models → figures → widgets
# =============================================================================

#' 在新机器上一行命令产出全部静态图、widget、模型表
#'
#' @param master 已 enrich 后的宽表（如果 NULL 会重新构建）
#' @param world_sf 可空
ghs_run_everything <- function(master = NULL, world_sf = NULL,
                                outputs_dir = file.path(proj_root(), "分析输出"),
                                figures = TRUE, widgets = TRUE,
                                models = TRUE, verbose = TRUE) {
  if (is.null(master)) {
    cache <- file.path(proj_root(), "派生数据", "处理结果",
                        "master_enriched.rds")
    if (file.exists(cache)) {
      master <- readRDS(cache)
    } else {
      ghs <- load_ghs()
      master <- enrich_master(build_master_wide(ghs), with_wdi = TRUE)
      dir.create(dirname(cache), recursive = TRUE, showWarnings = FALSE)
      saveRDS(master, cache)
    }
  }
  if (is.null(world_sf)) {
    world_sf <- tryCatch(load_world_sf(scale = "medium",
                                         simplify_keep = 0.1),
                          error = function(e) NULL)
  }
  res <- list()
  if (figures) {
    res$figures <- ghs_export_all_static(master, world_sf,
                                          outputs_dir = file.path(
                                            outputs_dir, "图表"),
                                          verbose = verbose)
  }
  if (widgets) {
    res$widgets <- ghs_export_all_widgets(master, world_sf,
                                           dir = file.path(outputs_dir,
                                                            "交互组件"),
                                           verbose = verbose)
  }
  if (models) {
    res$models <- ghs_export_all_models(master,
                                         outputs_dir = file.path(
                                           outputs_dir, "模型表"))
  }
  invisible(res)
}
