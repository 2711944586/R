# =============================================================================
# 程序/13_design_system.R
# -----------------------------------------------------------------------------
# v2 设计系统（数据新闻风）
#   - 字体注册（衬线编辑标题 + Inter 正文 + JetBrains Mono 等宽 + 中文）
#   - 5 套色板 (.brand_palette)
#   - theme_ghs2() — ggplot 主题（替代 theme_ghs()）
#   - labs_news() — 编辑式 labs（含数据源 caption + 副标行高）
#   - annotate_news() — 数据新闻式注释
#   - kpi_card_html() / dl_stats_html() — 给 Quarto/Shiny 共用
# =============================================================================

if (!exists("proj_root", mode = "function")) {
  source(file.path("程序", "00_utils.R"))
}

# ---- 1. 品牌色板 -----------------------------------------------------------

#' v2 品牌色板：莫兰迪基调 + 5 套语义色
brand_palette <- list(
  # 主色调
  ink   = "#1A1A1F",   # 近黑（标题/正文）
  paper = "#FAF7F2",   # 米白（页面背景）
  rule  = "#1A1A1F1F", # 分隔线 12% alpha
  muted = "#5A5A65",   # 次要文本

  # 数据色板 1: 三大资金源（命名色）
  source = c(
    gghed = "#1B5E88",   # 政府 — 深蓝
    pvtd  = "#C46B27",   # 私人 — 赭红
    ext   = "#6B8E5A"    # 外援 — 橄榄绿
  ),

  # 数据色板 2: 大洲（5 色 + 灰）
  continent = c(
    Africa     = "#C0504D",
    Americas   = "#1B5E88",
    Asia       = "#E8833C",
    Europe     = "#2A9D8F",
    Oceania    = "#7B4B94",
    Antarctica = "#9C9C9C"
  ),

  # 数据色板 3: 收入组（顺序色）
  income = c(
    `High income`         = "#0B3D5C",
    `Upper middle income` = "#4F8FBF",
    `Lower middle income` = "#D89B5B",
    `Low income`          = "#A03B27"
  ),

  # 数据色板 4: 顺序（cividis 派生 7 色，色盲友好）
  sequential = c(
    "#00204D", "#1F3F6E", "#445F8E",
    "#728DAF", "#A4BAC6", "#D6DEDB",
    "#FFEFB7"
  ),

  # 数据色板 5: 发散（红-米-蓝 9 色）
  diverging = c(
    "#67001F", "#B2182B", "#D6604D", "#F4A582",
    "#F7F7F7",
    "#92C5DE", "#4393C3", "#2166AC", "#053061"
  ),

  # 强调色
  accent  = "#C46B27",  # 主要强调（赭红）
  accent2 = "#2A9D8F"   # 次要强调（青绿）
)

# ---- 1b. 顶层别名（让 brand_palette$gghed / $africa 等能直接用） -----------
# 这避免每个调用点写 brand_palette$source[["gghed"]] 这样的长串
brand_palette$gghed     <- brand_palette$source[["gghed"]]
brand_palette$pvtd      <- brand_palette$source[["pvtd"]]
brand_palette$ext       <- brand_palette$source[["ext"]]
brand_palette$africa    <- brand_palette$continent[["Africa"]]
brand_palette$americas  <- brand_palette$continent[["Americas"]]
brand_palette$asia      <- brand_palette$continent[["Asia"]]
brand_palette$europe    <- brand_palette$continent[["Europe"]]
brand_palette$oceania   <- brand_palette$continent[["Oceania"]]
brand_palette$rule_dark <- "#1A1A1F40"   # 25% alpha 用于较深的辅助线

# ---- 2. 字体注册 -----------------------------------------------------------

#' 注册 v2 设计系统的所有字体（编辑标题 + Inter + 等宽 + 中文）
#'
#' 容错策略：
#'   - 如果系统缺字体，逐级降级到 sans-serif
#'   - showtext 必须可用，否则返回 FALSE
#' @return 列表（serif / sans / mono / cjk）— 每项是实际可用的 family
register_brand_fonts <- function() {
  if (!requireNamespace("showtext", quietly = TRUE)) return(list(
    serif = "serif", sans = "sans", mono = "mono", cjk = "sans"
  ))
  showtext::showtext_auto()
  has_sysfonts <- requireNamespace("sysfonts", quietly = TRUE)
  if (!has_sysfonts) return(list(
    serif = "serif", sans = "sans", mono = "mono", cjk = "sans"
  ))

  # 候选清单（优先匹配 v2 设计稿，按平台降级）
  candidates <- list(
    serif = c("Source Serif 4", "Source Serif Pro", "Fraunces",
              "PT Serif", "Georgia", "Times New Roman", "serif"),
    sans  = c("Inter", "Inter Tight", "Helvetica Neue", "Helvetica",
              "Arial", "sans"),
    mono  = c("JetBrains Mono", "Fira Code", "Cascadia Code",
              "Consolas", "Courier New", "mono"),
    cjk   = c("Source Han Sans CN", "Source Han Sans SC",
              "Noto Sans CJK SC", "Noto Sans SC",
              "Microsoft YaHei", "SimHei", "PingFang SC", "sans")
  )

  resolved <- list()
  for (role in names(candidates)) {
    picked <- "sans"
    for (cand in candidates[[role]]) {
      ok <- tryCatch({
        sysfonts::font_add(family = cand, regular = paste0(cand, ".otf"))
        TRUE
      }, error = function(e) FALSE, warning = function(w) FALSE)
      if (!ok) ok <- tryCatch({
        sysfonts::font_add(family = cand, regular = paste0(cand, ".ttf"))
        TRUE
      }, error = function(e) FALSE, warning = function(w) FALSE)
      if (!ok) ok <- tryCatch({
        sysfonts::font_add(family = cand, regular = paste0(cand, ".ttc"))
        TRUE
      }, error = function(e) FALSE, warning = function(w) FALSE)
      if (ok) { picked <- cand; break }
    }
    resolved[[role]] <- picked
  }
  invisible(resolved)
}

# 全局缓存（避免重复注册）
.brand_fonts <- NULL
get_brand_fonts <- function() {
  if (is.null(.brand_fonts)) {
    .brand_fonts <<- tryCatch(register_brand_fonts(),
                              error = function(e) list(
                                serif = "serif", sans = "sans",
                                mono  = "mono",  cjk = "sans"))
  }
  .brand_fonts
}

# ---- 3. theme_ghs2 — v2 ggplot 主题 ----------------------------------------

#' v2 数据新闻风 ggplot 主题
#'
#' 与 theme_ghs() 的差异：
#'   - 字体：标题用 Source Serif（衬线编辑），正文用 Inter
#'   - 网格：仅水平浅灰，去掉所有竖网格（数据新闻惯例）
#'   - 标题大字号 + 左对齐 + plot.title.position = "plot"
#'   - caption 放正文左侧（非传统右下），含双语来源
#'   - 去掉 panel border，仅靠留白结构
#'   - background 用米白 paper #FAF7F2
#'
#' @param base_size 基准字号（默认 13）
#' @param grid 网格类型："y" / "x" / TRUE / FALSE
#' @param panel "card" 添加微卡片背景；"flat" 完全无背景
theme_ghs2 <- function(base_size = 13, grid = "y", panel = "flat") {
  ensure_pkgs(c("ggplot2"))
  fonts <- get_brand_fonts()
  pal <- brand_palette

  # CJK fallback：图表常含中文，所以让 title/subtitle/caption 用 cjk 字体
  # （Microsoft YaHei / SimHei / Noto Sans CJK 都同时有 Latin + 中文字形）
  serif_fam <- if (!is.null(fonts$cjk) && nchar(fonts$cjk) &&
                  !identical(fonts$cjk, "sans"))
                 fonts$cjk else fonts$serif
  sans_fam  <- if (!is.null(fonts$cjk) && nchar(fonts$cjk) &&
                  !identical(fonts$cjk, "sans"))
                 fonts$cjk else fonts$sans
  fonts$serif <- serif_fam
  fonts$sans  <- sans_fam

  bg_panel <- switch(panel,
    card = "#FFFFFF",   # 白卡片
    flat = pal$paper,   # 米白同页面
    pal$paper
  )

  th <- ggplot2::theme_minimal(base_size = base_size, base_family = fonts$sans) +
    ggplot2::theme(
      # 标题区
      plot.title = ggplot2::element_text(
        family = fonts$serif, face = "bold",
        size = base_size * 1.55, colour = pal$ink, lineheight = 1.15,
        margin = ggplot2::margin(b = 6)),
      plot.subtitle = ggplot2::element_text(
        family = fonts$sans, size = base_size * 1.02,
        colour = pal$muted, lineheight = 1.4,
        margin = ggplot2::margin(b = 14)),
      plot.caption = ggplot2::element_text(
        family = fonts$sans, size = base_size * 0.78,
        colour = pal$muted, hjust = 0, lineheight = 1.3,
        margin = ggplot2::margin(t = 14)),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.margin = ggplot2::margin(t = 20, r = 16, b = 12, l = 10),

      # 背景
      plot.background  = ggplot2::element_rect(fill = pal$paper, colour = NA),
      panel.background = ggplot2::element_rect(fill = bg_panel, colour = NA),
      panel.border     = ggplot2::element_blank(),

      # 网格（仅水平）
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor   = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(
        colour = "#1A1A1F12", linewidth = 0.35),

      # 轴
      axis.title  = ggplot2::element_text(
        family = fonts$sans, colour = pal$muted, size = base_size * 0.92),
      axis.text   = ggplot2::element_text(
        family = fonts$sans, colour = pal$ink,   size = base_size * 0.86),
      axis.ticks  = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_line(colour = pal$ink, linewidth = 0.45),
      axis.line.y = ggplot2::element_blank(),

      # Strip / facet
      strip.background = ggplot2::element_rect(fill = "#1A1A1F08", colour = NA),
      strip.text       = ggplot2::element_text(
        family = fonts$sans, face = "bold", colour = pal$ink,
        size = base_size * 0.92, margin = ggplot2::margin(4, 4, 4, 4)),

      # 图例
      legend.position    = "top",
      legend.justification = "left",
      legend.title       = ggplot2::element_text(
        family = fonts$sans, face = "bold", colour = pal$ink,
        size = base_size * 0.86),
      legend.text        = ggplot2::element_text(
        family = fonts$sans, colour = pal$ink, size = base_size * 0.86),
      legend.background  = ggplot2::element_rect(fill = NA, colour = NA),
      legend.key         = ggplot2::element_rect(fill = NA, colour = NA),
      legend.margin      = ggplot2::margin(0, 0, 8, 0)
    )

  # 网格调整
  if (identical(grid, FALSE)) {
    th <- th + ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
  } else if (identical(grid, "x")) {
    th <- th + ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(
        colour = "#1A1A1F12", linewidth = 0.35),
      axis.line.x = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_line(colour = pal$ink, linewidth = 0.45))
  }
  th
}

# ---- 4. 色板辅助 -----------------------------------------------------------

#' v2 三大资金源 fill / colour
scale_fill_brand_source <- function(...) {
  ggplot2::scale_fill_manual(
    values = c(
      brand_palette$source,
      `Domestic General Government Health Expenditure (GGHE-D)` = brand_palette$source[["gghed"]],
      `Domestic Private Health Expenditure (PVT-D)`             = brand_palette$source[["pvtd"]],
      `External Health Expenditure (EXT)`                       = brand_palette$source[["ext"]]
    ),
    na.value = "grey80", ...)
}
scale_colour_brand_source <- function(...) {
  ggplot2::scale_colour_manual(values = brand_palette$source, na.value = "grey80", ...)
}

#' v2 大洲 fill / colour
scale_fill_brand_continent <- function(...) {
  ggplot2::scale_fill_manual(values = brand_palette$continent, na.value = "grey80", ...)
}
scale_colour_brand_continent <- function(...) {
  ggplot2::scale_colour_manual(values = brand_palette$continent, na.value = "grey80", ...)
}

#' v2 收入组 fill / colour（顺序色，强调高/低对比）
scale_fill_brand_income <- function(...) {
  ggplot2::scale_fill_manual(values = brand_palette$income, na.value = "grey80", ...)
}
scale_colour_brand_income <- function(...) {
  ggplot2::scale_colour_manual(values = brand_palette$income, na.value = "grey80", ...)
}

#' v2 顺序色（cividis 派生）
scale_fill_brand_seq <- function(...) {
  ggplot2::scale_fill_gradientn(colours = brand_palette$sequential, na.value = "grey90", ...)
}
scale_colour_brand_seq <- function(...) {
  ggplot2::scale_colour_gradientn(colours = brand_palette$sequential, na.value = "grey90", ...)
}

#' v2 发散色（红-米-蓝）
scale_fill_brand_div <- function(midpoint = 0, ...) {
  ggplot2::scale_fill_gradient2(
    low = brand_palette$diverging[2],
    mid = brand_palette$diverging[5],
    high = brand_palette$diverging[8],
    midpoint = midpoint, na.value = "grey90", ...)
}

# ---- 5. 数据新闻式 labs ----------------------------------------------------

#' 数据新闻式 labs（含编辑副标 + 双语来源 caption）
#'
#' @param title 编辑性大标题（一句话结论）
#' @param subtitle 副标（解释 + 单位）
#' @param x,y 轴标签
#' @param caption 数据来源（自动补 "Source · Analysis"）
#' @param tag 章节锚（可空）
labs_news <- function(title = NULL, subtitle = NULL,
                      x = NULL, y = NULL,
                      caption = NULL, tag = NULL) {
  cap <- if (is.null(caption) || nchar(caption) == 0) {
    "\u6570\u636e\u6e90 \u00b7 WHO Global Health Expenditure Database (GHED) 2024"
  } else caption
  cap <- paste0(cap,
                "  \u00b7  \u5206\u6790 Analysis: \u5e84\u9882 (20241334)")
  ggplot2::labs(title = title, subtitle = subtitle,
                x = x, y = y, caption = cap, tag = tag)
}

# ---- 6. 数据新闻式注释 -----------------------------------------------------

#' 在 ggplot 上叠加一个编辑性注释（衬线字体 + 半透明背景）
#'
#' @param plot ggplot 对象
#' @param x,y 注释位置（数据坐标）
#' @param label 文本（支持 \\n 换行）
#' @param hjust,vjust 对齐
#' @param size 字号（geom_text）
#' @param colour 颜色（默认 ink）
add_editorial_note <- function(plot, x, y, label,
                                 hjust = 0, vjust = 1,
                                 size = 3.6, colour = NULL) {
  fonts <- get_brand_fonts()
  col <- if (is.null(colour)) brand_palette$ink else colour
  plot +
    ggplot2::annotate(
      "label", x = x, y = y, label = label,
      hjust = hjust, vjust = vjust,
      family = fonts$serif, size = size, colour = col,
      fill = "#FFFFFFC0", label.size = 0,
      label.padding = ggplot2::unit(0.4, "lines"),
      lineheight = 1.15
    )
}

# ---- 7. 共享 HTML 组件（Quarto / Shiny 都用） ------------------------------

#' KPI 卡片 HTML（大数字 + 单位 + 副标 + 变化箭头）
#'
#' @param value 主数字（已格式化）
#' @param label 副标（说明）
#' @param delta 变化（数字，可空）
#' @param unit 单位（默认空）
#' @param trend "up" / "down" / "flat"（默认按 delta 自动）
kpi_card_html <- function(value, label, delta = NULL, unit = "",
                          sublabel = NULL,
                          trend = c("auto", "up", "down", "flat")) {
  trend <- match.arg(trend)
  if (trend == "auto" && !is.null(delta) && is.finite(delta)) {
    trend <- if (delta > 0.001) "up" else if (delta < -0.001) "down" else "flat"
  }
  arrow <- switch(trend,
    up = "\u2197", down = "\u2198", flat = "\u2192", "\u2192"
  )
  arrow_col <- switch(trend,
    up = "#2A9D8F", down = "#C0504D", flat = "#5A5A65", "#5A5A65"
  )
  delta_txt <- if (!is.null(delta) && is.finite(delta)) {
    sprintf("<span style='color:%s;margin-left:6px;'>%s %+.1f%s</span>",
            arrow_col, arrow, delta, unit)
  } else ""
  sub_txt <- if (!is.null(sublabel) && nchar(sublabel)) {
    sprintf(
      "<div style='font-size:12px;color:#7A7A82;margin-top:6px;'>%s</div>",
      htmltools::htmlEscape(sublabel)
    )
  } else ""
  sprintf(
    paste0(
      "<div class='kpi-card' style='padding:18px 20px;background:#fff;",
      "border:1px solid rgba(26,26,31,0.10);border-radius:8px;'>",
      "<div style='font-size:13px;color:#5A5A65;letter-spacing:0.04em;",
      "text-transform:uppercase;margin-bottom:6px;'>%s</div>",
      "<div style='font-family:\"Source Serif 4\",serif;font-size:32px;",
      "font-weight:600;color:#1A1A1F;line-height:1.1;'>%s%s%s</div>",
      "%s</div>"
    ),
    htmltools::htmlEscape(label),
    htmltools::htmlEscape(value),
    if (nchar(unit)) sprintf("<span style='font-size:0.55em;color:#5A5A65;margin-left:4px;'>%s</span>", unit) else "",
    delta_txt,
    sub_txt
  )
}

#' 描述列表 HTML（label / value 多行）
#'
#' @param ... 命名参数：name = value
dl_stats_html <- function(...) {
  pairs <- list(...)
  if (!length(pairs)) return("")
  rows <- vapply(seq_along(pairs), function(i) {
    sprintf(
      paste0(
        "<dt style='color:#5A5A65;font-size:13px;text-transform:uppercase;",
        "letter-spacing:0.04em;margin-top:8px;'>%s</dt>",
        "<dd style='font-family:\"Inter Tight\",\"Inter\",sans-serif;",
        "font-size:18px;color:#1A1A1F;margin:2px 0 6px;'>%s</dd>"
      ),
      htmltools::htmlEscape(names(pairs)[i]),
      htmltools::htmlEscape(as.character(pairs[[i]]))
    )
  }, character(1))
  sprintf("<dl style='margin:0;'>%s</dl>", paste(rows, collapse = ""))
}

#' 数据新闻式 callout（衬线大字 + 左竖线）
news_callout_html <- function(text, source = NULL,
                              variant = c("default", "warn", "tip")) {
  variant <- match.arg(variant)
  border_col <- switch(variant,
    warn    = "#C46B27", tip = "#2A9D8F", default = "#1B5E88")
  src <- if (!is.null(source) && nchar(source))
    sprintf("<div style='font-size:12px;color:#5A5A65;margin-top:8px;'>%s</div>",
            htmltools::htmlEscape(source))
  else ""
  sprintf(
    paste0(
      "<blockquote class='news-callout' style='border-left:4px solid %s;",
      "padding:12px 16px;margin:24px 0;background:#FFFFFFA0;",
      "font-family:\"Source Serif 4\",serif;font-size:18px;line-height:1.5;",
      "color:#1A1A1F;'>%s%s</blockquote>"
    ),
    border_col, htmltools::htmlEscape(text), src
  )
}
