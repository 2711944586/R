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
ghs_save_widget <- function(widget, name,
                              dir = file.path(proj_root(), "分析输出", "交互组件"),
                              selfcontained = TRUE) {
  if (is.null(widget)) return(invisible(NULL))
  if (!requireNamespace("htmlwidgets", quietly = TRUE)) return(invisible(NULL))
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(dir, paste0(name, ".html"))
  tryCatch(
    htmlwidgets::saveWidget(widget, file = path,
                              selfcontained = selfcontained),
    error = function(e) {
      logw("widget save failed: ", name, " | ", conditionMessage(e))
    }
  )
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
                                                              "tables")) {
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
                                            outputs_dir, "figures"),
                                          verbose = verbose)
  }
  if (widgets) {
    res$widgets <- ghs_export_all_widgets(master, world_sf,
                                           dir = file.path(outputs_dir,
                                                            "widgets"),
                                           verbose = verbose)
  }
  if (models) {
    res$models <- ghs_export_all_models(master,
                                         outputs_dir = file.path(
                                           outputs_dir, "tables"))
  }
  invisible(res)
}
