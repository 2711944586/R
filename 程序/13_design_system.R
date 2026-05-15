# =============================================================================
# 程序/13_design_system.R
# -----------------------------------------------------------------------------
# 设计系统（数据新闻风）
#   - 字体注册（衬线编辑标题 + Inter 正文 + JetBrains Mono 等宽 + 中文）
#   - 5 套色板 (.brand_palette)
#   - theme_ghs2() — ggplot 主题
#   - labs_news() — 编辑式 labs（含数据源 caption + 副标行高）
#   - annotate_news() — 数据新闻式注释
#   - kpi_card_html() / dl_stats_html() — 给 Quarto/Shiny 共用
# =============================================================================

if (!exists("proj_root", mode = "function")) {
  source(file.path("程序", "00_utils.R"))
}

# ---- 1. 品牌色板 -----------------------------------------------------------

#' 品牌色板：莫兰迪基调 + 5 套语义色
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

#' 注册设计系统的所有字体（编辑标题 + Inter + 等宽 + 中文）
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

  # 候选清单（按平台降级）
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

# ---- 3. theme_ghs2 — ggplot 主题 ----------------------------------------

#' 数据新闻风 ggplot 主题
#'
#' 特点：
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

#' 三大资金源 fill / colour
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

#' 大洲 fill / colour
scale_fill_brand_continent <- function(...) {
  ggplot2::scale_fill_manual(values = brand_palette$continent, na.value = "grey80", ...)
}
scale_colour_brand_continent <- function(...) {
  ggplot2::scale_colour_manual(values = brand_palette$continent, na.value = "grey80", ...)
}

#' 收入组 fill / colour（顺序色，强调高/低对比）
scale_fill_brand_income <- function(...) {
  ggplot2::scale_fill_manual(values = brand_palette$income, na.value = "grey80", ...)
}
scale_colour_brand_income <- function(...) {
  ggplot2::scale_colour_manual(values = brand_palette$income, na.value = "grey80", ...)
}

#' 顺序色（cividis 派生）
scale_fill_brand_seq <- function(...) {
  ggplot2::scale_fill_gradientn(colours = brand_palette$sequential, na.value = "grey90", ...)
}
scale_colour_brand_seq <- function(...) {
  ggplot2::scale_colour_gradientn(colours = brand_palette$sequential, na.value = "grey90", ...)
}

#' 发散色（红-米-蓝）
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

# =============================================================================
# 扩展设计系统（加性扩展）
# -----------------------------------------------------------------------------
# 设计目标：
#   1. 引入语义色槽（primary / secondary / good / warn / bad / neutral / highlight / muted）
#   2. light / dark 双模主题
#   3. 离散 12 色 + 顺序 9 色 + 发散 11 色
#   4. 三类 ggplot 变体：default(默认) / data(数据密集) / editorial(编辑大字)
#   5. 跨组件统一：plotly / leaflet / reactable / DT / gt 共享品牌
#
# 原则：
#   - 不破坏现有 API（brand_palette / theme_ghs2 / scale_*_brand_*）
#   - 新模块（30+ / 36+ / 38+）默认使用扩展色板
#   - 旧函数保持可用，可在阶段 B7 渐进迁移
# =============================================================================

# ---- 语义色槽 --------------------------------------------------------------

#' 语义色板（slot → light / dark 双值）
#'
#' 供所有新模块以"语义"取色，避免硬编码 HEX。
ghs3_slots <- list(
  primary    = list(light = "#1d3f5f", dark = "#7aa9d6"),
  secondary  = list(light = "#c46327", dark = "#e8a070"),
  good       = list(light = "#2a857a", dark = "#5dc4b6"),
  warn       = list(light = "#c89a3b", dark = "#e7c46a"),
  bad        = list(light = "#a23b3b", dark = "#e08585"),
  neutral    = list(light = "#5d667a", dark = "#a8b0c0"),
  highlight  = list(light = "#f7c08a", dark = "#f7c08a"),
  muted      = list(light = "#0d121b99",
                    dark  = "#e7e9ee99"),
  ink        = list(light = "#0d121b", dark = "#e7e9ee"),
  paper      = list(light = "#fbf6ee", dark = "#0f141e"),
  paper2     = list(light = "#f1e8da", dark = "#161c2a"),
  # 线条色：使用 8 位 hex（含 alpha）方便 grid / ggplot 直接识别
  line       = list(light = "#0d121b1a",
                    dark  = "#e7e9ee24"),
  line_strong = list(light = "#0d121b2e",
                     dark  = "#e7e9ee3d"),
  code_bg    = list(light = "#0c1424", dark = "#06090f"),
  code_ink   = list(light = "#e6efff", dark = "#cfd6e2")
)

#' 取语义色：palette_ghs3("primary") / palette_ghs3("good", mode="dark")
palette_ghs3 <- function(slot = "primary", mode = c("light", "dark")) {
  mode <- match.arg(mode)
  if (!slot %in% names(ghs3_slots))
    stop("Unknown slot: ", slot,
         "\nValid: ", paste(names(ghs3_slots), collapse = ", "))
  ghs3_slots[[slot]][[mode]]
}

# ---- 2 数据色板 --------------------------------------------------------

#' 离散 12 色（品牌排序，用于多类别图）
#'
#' 顺序：primary, secondary, good, warn, bad, 蓝绿, 紫, 橙黄, 草绿, 玫红,
#'       灰, 海蓝（饱和度递减）。色盲检验通过 deuteranopia / protanopia。
ghs3_palette_discrete <- c(
  "#1d3f5f",  # 1 primary
  "#c46327",  # 2 secondary
  "#2a857a",  # 3 good
  "#c89a3b",  # 4 warn
  "#a23b3b",  # 5 bad
  "#5b8aa6",  # 6
  "#7c5b9a",  # 7
  "#e0904c",  # 8
  "#86a062",  # 9
  "#b85578",  # 10
  "#7e8aa0",  # 11 neutral
  "#3a6e8f"   # 12
)

palette_ghs3_discrete <- function(n = 12) {
  if (n <= length(ghs3_palette_discrete)) {
    ghs3_palette_discrete[seq_len(n)]
  } else {
    grDevices::colorRampPalette(ghs3_palette_discrete)(n)
  }
}

#' 顺序色（9 级）：默认 cividis 派生 + ember / ocean / sage 三套备选
ghs3_palette_sequential_sets <- list(
  default = c("#fbf6ee", "#f1deae", "#e7c178", "#d99a4d",
              "#c46327", "#9d4416", "#6e2c0d", "#3f1505", "#1a0500"),
  ember   = c("#fff7e6", "#ffd9a8", "#ffb56f", "#f7903f",
              "#e0671c", "#b54311", "#7e2c0a", "#481603", "#1a0500"),
  ocean   = c("#f0f7fb", "#cce0ee", "#9bc1dc", "#6ba2c8",
              "#3f7fae", "#1d5e8c", "#0e406b", "#062648", "#031224"),
  sage    = c("#f4faf6", "#d4ecd9", "#a9d6b4", "#7dbf90",
              "#54a772", "#35895a", "#1f6943", "#114a2e", "#062a18"),
  blueorange = c("#0a3055", "#1d4f7a", "#3771a0", "#6896c0",
                 "#a3bfd9", "#f1d2a9", "#e6a368", "#cc6f24", "#7e3c0d")
)

palette_ghs3_sequential <- function(n = 9, palette = "default") {
  set <- ghs3_palette_sequential_sets[[palette]] %||%
    ghs3_palette_sequential_sets$default
  if (n == length(set)) return(set)
  grDevices::colorRampPalette(set)(n)
}

#' 发散色（11 级，红-米-蓝，色盲友好）
ghs3_palette_diverging_set <- c(
  "#67001f", "#b2182b", "#d6604d", "#f4a582", "#fddbc7",
  "#f7f7f7",
  "#d1e5f0", "#92c5de", "#4393c3", "#2166ac", "#053061"
)

palette_ghs3_diverging <- function(n = 11) {
  if (n == length(ghs3_palette_diverging_set))
    return(ghs3_palette_diverging_set)
  grDevices::colorRampPalette(ghs3_palette_diverging_set)(n)
}

# ---- 字体（命名包装） --------------------------------------------

#' 字体注册（语义包装；调用 register_brand_fonts）
ghs_register_fonts <- function() get_brand_fonts()

# ---- ggplot 主题（扩展） ---------------------------------------------------

#' ggplot 主题（支持 light/dark/print 三模式）
#'
#' @param base_size 基准字号（13 / 14 / 16）
#' @param mode 配色模式："light"（默认）/"dark"/"print"
#' @param variant 主题变体：
#'   - "default"   : 通用，moderate 网格
#'   - "data"      : 数据密集（更小留白、强网格）
#'   - "editorial" : 编辑大字（衬线标题、轴最简）
#' @param grid 网格："y"（默认）/ "x" / "xy" / FALSE
theme_ghs3 <- function(base_size = 13,
                        mode = c("light", "dark", "print"),
                        variant = c("default", "data", "editorial"),
                        grid = "y") {
  ensure_pkgs(c("ggplot2"))
  mode <- match.arg(mode)
  variant <- match.arg(variant)

  fonts <- get_brand_fonts()
  serif_fam <- if (!is.null(fonts$cjk) && nchar(fonts$cjk) &&
                  !identical(fonts$cjk, "sans"))
                 fonts$cjk else fonts$serif
  sans_fam  <- if (!is.null(fonts$cjk) && nchar(fonts$cjk) &&
                  !identical(fonts$cjk, "sans"))
                 fonts$cjk else fonts$sans

  ink   <- palette_ghs3("ink",   if (mode == "dark") "dark" else "light")
  muted <- palette_ghs3("neutral", if (mode == "dark") "dark" else "light")
  paper <- if (mode == "print") "#ffffff"
           else palette_ghs3("paper", if (mode == "dark") "dark" else "light")
  line_col <- if (mode == "dark") "#ffffff20" else "#0d121b1c"

  title_size  <- switch(variant,
    editorial = base_size * 1.85,
    data      = base_size * 1.30,
    base_size * 1.55)
  sub_size    <- switch(variant,
    editorial = base_size * 1.10,
    data      = base_size * 0.95,
    base_size * 1.02)
  axis_size   <- switch(variant,
    data = base_size * 0.82, base_size * 0.86)

  th <- ggplot2::theme_minimal(base_size = base_size, base_family = sans_fam) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        family = serif_fam, face = "bold",
        size = title_size, colour = ink, lineheight = 1.12,
        margin = ggplot2::margin(b = 6)),
      plot.subtitle = ggplot2::element_text(
        family = sans_fam, size = sub_size,
        colour = muted, lineheight = 1.4,
        margin = ggplot2::margin(b = if (variant == "editorial") 18 else 14)),
      plot.caption = ggplot2::element_text(
        family = sans_fam, size = base_size * 0.78,
        colour = muted, hjust = 0, lineheight = 1.3,
        margin = ggplot2::margin(t = 14)),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.margin = ggplot2::margin(
        t = if (variant == "editorial") 24 else 18,
        r = 16,
        b = 12,
        l = if (variant == "editorial") 14 else 10),
      plot.background  = ggplot2::element_rect(fill = paper, colour = NA),
      panel.background = ggplot2::element_rect(fill = paper, colour = NA),
      panel.border     = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor   = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = line_col,
                                                  linewidth = 0.35),
      axis.title  = ggplot2::element_text(family = sans_fam,
                                           colour = muted,
                                           size = base_size * 0.92),
      axis.text   = ggplot2::element_text(family = sans_fam,
                                           colour = ink,
                                           size = axis_size),
      axis.ticks  = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_line(colour = ink, linewidth = 0.45),
      axis.line.y = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(
        fill = if (mode == "dark") "#ffffff10" else "#0d121b08",
        colour = NA),
      strip.text = ggplot2::element_text(
        family = sans_fam, face = "bold", colour = ink,
        size = base_size * 0.92,
        margin = ggplot2::margin(4, 4, 4, 4)),
      legend.position    = if (variant == "editorial") "bottom" else "top",
      legend.justification = "left",
      legend.title       = ggplot2::element_text(
        family = sans_fam, face = "bold", colour = ink,
        size = base_size * 0.86),
      legend.text        = ggplot2::element_text(
        family = sans_fam, colour = ink, size = base_size * 0.86),
      legend.background  = ggplot2::element_rect(fill = NA, colour = NA),
      legend.key         = ggplot2::element_rect(fill = NA, colour = NA),
      legend.margin      = ggplot2::margin(0, 0, 8, 0)
    )

  if (identical(grid, FALSE)) {
    th <- th + ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
  } else if (identical(grid, "x")) {
    th <- th + ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(colour = line_col,
                                                  linewidth = 0.35),
      axis.line.x = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_line(colour = ink, linewidth = 0.45))
  } else if (identical(grid, "xy") || identical(grid, "both")) {
    th <- th + ggplot2::theme(
      panel.grid.major.x = ggplot2::element_line(colour = line_col,
                                                  linewidth = 0.35))
  }
  th
}

# ---- 5 ggplot 通用 scale 助手 ------------------------------------------

#' 离散填充：scale_fill_ghs3(n)
scale_fill_ghs3 <- function(...) {
  ggplot2::scale_fill_manual(values = ghs3_palette_discrete, na.value = "grey80", ...)
}
scale_colour_ghs3 <- function(...) {
  ggplot2::scale_colour_manual(values = ghs3_palette_discrete, na.value = "grey80", ...)
}
#' 顺序色：scale_fill_ghs3_seq(palette = "ember"). 可通过 ... 覆盖 na.value。
scale_fill_ghs3_seq <- function(palette = "default", ...) {
  dots <- list(...)
  if (is.null(dots$na.value)) dots$na.value <- "grey90"
  do.call(ggplot2::scale_fill_gradientn,
    c(list(colours = palette_ghs3_sequential(9, palette)), dots))
}
scale_colour_ghs3_seq <- function(palette = "default", ...) {
  dots <- list(...)
  if (is.null(dots$na.value)) dots$na.value <- "grey90"
  do.call(ggplot2::scale_colour_gradientn,
    c(list(colours = palette_ghs3_sequential(9, palette)), dots))
}
#' 发散色：scale_fill_ghs3_div(midpoint = 0). 可通过 ... 覆盖 na.value。
scale_fill_ghs3_div <- function(midpoint = 0, ...) {
  dots <- list(...)
  if (is.null(dots$na.value)) dots$na.value <- "grey90"
  if (is.null(dots$rescaler)) {
    dots$rescaler <- function(x, to = c(0, 1), from = NULL) {
      scales::rescale_mid(x, to = to, mid = midpoint, from = from)
    }
  }
  do.call(ggplot2::scale_fill_gradientn,
    c(list(colours = palette_ghs3_diverging(11)), dots))
}

# ---- 6 plotly 主题包装 ------------------------------------------------

#' 给 plotly 对象套上品牌 layout
#'
#' @param p plotly 对象
#' @param theme "light" / "dark"
#' @param margin 上下左右留白
ghs_plotly_layout <- function(p, theme = c("light", "dark"),
                              margin = list(l = 60, r = 24, t = 56, b = 60),
                              show_modebar = FALSE) {
  if (!requireNamespace("plotly", quietly = TRUE)) return(p)
  theme <- match.arg(theme)
  paper <- palette_ghs3("paper", theme)
  ink   <- palette_ghs3("ink",   theme)
  muted <- palette_ghs3("neutral", theme)
  line_col <- if (theme == "dark") "rgba(255,255,255,0.12)"
              else "rgba(13,18,27,0.10)"
  fonts <- get_brand_fonts()
  body_fam  <- paste0("'", fonts$sans, "', 'Inter', 'Helvetica', sans-serif")
  title_fam <- paste0("'", fonts$serif, "', 'Source Serif 4', serif")

  p2 <- plotly::layout(
    p,
    paper_bgcolor = paper,
    plot_bgcolor  = paper,
    font = list(family = body_fam, size = 13, color = ink),
    margin = margin,
    title = list(font = list(family = title_fam, size = 18, color = ink),
                 x = 0, xanchor = "left", y = 0.97),
    xaxis = list(linecolor = ink, gridcolor = line_col,
                 zerolinecolor = line_col,
                 tickfont = list(family = body_fam, size = 12, color = ink),
                 titlefont = list(family = body_fam, size = 12, color = muted)),
    yaxis = list(linecolor = ink, gridcolor = line_col,
                 zerolinecolor = line_col,
                 tickfont = list(family = body_fam, size = 12, color = ink),
                 titlefont = list(family = body_fam, size = 12, color = muted)),
    legend = list(orientation = "h", x = 0, y = -0.18,
                  font = list(family = body_fam, size = 12, color = ink),
                  bgcolor = "rgba(0,0,0,0)")
  )
  plotly::config(p2,
                 displaylogo = FALSE,
                 displayModeBar = show_modebar,
                 modeBarButtonsToRemove = c("lasso2d", "select2d",
                                            "autoScale2d", "toggleSpikelines"))
}

# ---- 7 leaflet 主题 -----------------------------------------------------

#' 给 leaflet 套上品牌底图（CartoDB Positron / DarkMatter）
ghs_leaflet_provider <- function(map = NULL, theme = c("light", "dark")) {
  theme <- match.arg(theme)
  if (!requireNamespace("leaflet", quietly = TRUE))
    stop("leaflet not available")
  prov <- if (theme == "dark") "CartoDB.DarkMatter" else "CartoDB.Positron"
  if (is.null(map)) map <- leaflet::leaflet()
  map <- leaflet::addProviderTiles(map, prov,
                                   options = leaflet::providerTileOptions(
                                     noWrap = FALSE, opacity = 1))
  map <- leaflet::setView(map, lng = 0, lat = 20, zoom = 2)
  map
}

# ---- 8 reactable / DT / gt 主题 ----------------------------------------

#' reactable 通用主题（list 形式，传给 reactable(theme = ghs_reactable_theme())）
ghs_reactable_theme <- function(mode = c("light", "dark")) {
  mode <- match.arg(mode)
  if (!requireNamespace("reactable", quietly = TRUE)) return(NULL)
  ink <- palette_ghs3("ink", mode)
  paper <- palette_ghs3("paper", mode)
  paper2 <- palette_ghs3("paper2", mode)
  muted <- palette_ghs3("neutral", mode)
  line <- if (mode == "dark") "rgba(255,255,255,0.10)" else "rgba(13,18,27,0.08)"
  reactable::reactableTheme(
    color = ink, backgroundColor = paper,
    borderColor = line, stripedColor = paper2, highlightColor = paper2,
    cellPadding = "10px 12px",
    style = list(fontFamily = "'Inter','Noto Sans CJK SC',sans-serif",
                 fontSize = 13.5),
    headerStyle = list(
      backgroundColor = paper2,
      color = ink, fontWeight = 700, borderBottom = paste0("2px solid ", ink)),
    rowGroupStyle = list(fontWeight = 700, color = ink),
    inputStyle = list(backgroundColor = paper2, color = ink),
    paginationStyle = list(color = muted),
    pageButtonHoverStyle = list(backgroundColor = paper2),
    pageButtonActiveStyle = list(backgroundColor = palette_ghs3("primary", mode),
                                 color = "#ffffff")
  )
}

#' DT 通用 options
ghs_dt_options <- function() {
  list(
    dom = "frtip",
    pageLength = 12,
    autoWidth = FALSE,
    scrollX = TRUE,
    language = list(
      search = "\u641c\u7d22:",
      paginate = list(`previous` = "\u4e0a\u4e00\u9875",
                       `next` = "\u4e0b\u4e00\u9875"),
      info = "\u7b2c _START_ \u2013 _END_ \u6761 / \u5171 _TOTAL_ \u6761",
      lengthMenu = "\u6bcf\u9875 _MENU_ \u6761"
    )
  )
}

#' gt 通用主题（仅在 gt 已安装时生效）
ghs_gt_theme <- function(g) {
  if (!requireNamespace("gt", quietly = TRUE)) return(g)
  g |>
    gt::tab_options(
      table.font.names = "Inter, 'Noto Sans CJK SC', sans-serif",
      table.font.size = 13,
      heading.title.font.size = 18,
      heading.subtitle.font.size = 13,
      column_labels.font.weight = "bold",
      column_labels.background.color = palette_ghs3("paper2"),
      table.border.top.color = palette_ghs3("ink"),
      table.border.top.width = 2,
      heading.align = "left",
      table_body.hlines.color = palette_ghs3("line")
    )
}

# ---- 9 卡片 / 章节头 HTML 组件 ----------------------------------------

#' KPI 卡片（更紧凑、更分层、可选 trend）
ghs_kpi_card <- function(value, label, hint = NULL, trend = NULL,
                         tone = c("primary", "secondary", "good",
                                   "warn", "bad", "neutral")) {
  tone <- match.arg(tone)
  bar <- palette_ghs3(tone)
  trend_html <- ""
  if (!is.null(trend) && is.finite(trend)) {
    arr <- if (trend > 0) "\u2197" else if (trend < 0) "\u2198" else "\u2192"
    col <- if (trend > 0) palette_ghs3("good")
           else if (trend < 0) palette_ghs3("bad")
           else palette_ghs3("neutral")
    trend_html <- sprintf(
      "<span class='ghs-kpi-trend' style='color:%s'>%s %+0.1f%%</span>",
      col, arr, trend * 100)
  }
  hint_html <- if (!is.null(hint) && nchar(hint))
    sprintf("<div class='ghs-kpi-hint'>%s</div>",
            htmltools::htmlEscape(hint))
  else ""
  sprintf(
    paste0("<div class='ghs-kpi' style='--tone:%s'>",
           "<div class='ghs-kpi-value'>%s</div>",
           "<div class='ghs-kpi-label'>%s%s</div>%s",
           "</div>"),
    bar,
    htmltools::htmlEscape(value),
    htmltools::htmlEscape(label),
    trend_html,
    hint_html)
}

#' 章节头（kicker + h2 + lead）
ghs_section_head <- function(kicker, title, lead = NULL,
                              align = c("left", "center")) {
  align <- match.arg(align)
  lead_html <- if (!is.null(lead) && nchar(lead))
    sprintf("<p class='ghs-section-lead'>%s</p>",
            htmltools::htmlEscape(lead))
  else ""
  sprintf(
    paste0("<header class='ghs-section-head' data-align='%s'>",
           "<span class='ghs-kicker'>%s</span>",
           "<h2 class='ghs-section-title'>%s</h2>%s</header>"),
    align,
    htmltools::htmlEscape(kicker),
    htmltools::htmlEscape(title),
    lead_html)
}

#' stat strip（横向数据条；breaks 用于换行）
ghs_stat_strip <- function(items) {
  if (!length(items)) return("")
  cards <- vapply(seq_along(items), function(i) {
    it <- items[[i]]
    sprintf(
      paste0("<div class='ghs-stat-cell'>",
             "<div class='ghs-stat-value'>%s</div>",
             "<div class='ghs-stat-label'>%s</div></div>"),
      htmltools::htmlEscape(it$value %||% "\u2014"),
      htmltools::htmlEscape(it$label %||% ""))
  }, character(1))
  sprintf("<div class='ghs-stat-strip'>%s</div>", paste(cards, collapse = ""))
}

#' callout（4 个语义色：info / good / warn / bad）
ghs_callout <- function(text, tone = c("info", "good", "warn", "bad"),
                         title = NULL) {
  tone <- match.arg(tone)
  bar <- switch(tone,
    info = palette_ghs3("primary"),
    good = palette_ghs3("good"),
    warn = palette_ghs3("warn"),
    bad  = palette_ghs3("bad"))
  title_html <- if (!is.null(title) && nchar(title))
    sprintf("<strong class='ghs-callout-title'>%s</strong>",
            htmltools::htmlEscape(title))
  else ""
  sprintf(
    paste0("<aside class='ghs-callout ghs-callout-%s' style='--bar:%s'>",
           "%s<div class='ghs-callout-body'>%s</div></aside>"),
    tone, bar, title_html, text)
}

# ---- 10 模块标识 ------------------------------------------------------

#' 当前设计系统 + 元数据
GHS3_VERSION <- "3.0.0"
GHS3_META <- list(
  version = GHS3_VERSION,
  released = "2026-05-12",
  required_pkgs = c("ggplot2", "scales", "showtext", "sysfonts",
                    "htmltools", "plotly", "leaflet"),
  optional_pkgs = c("reactable", "DT", "gt", "echarts4r", "highcharter",
                    "ggrepel", "ggdist", "ggridges", "ggbump", "ggh4x",
                    "gganimate", "crosstalk", "Synth", "forecast", "prophet")
)
