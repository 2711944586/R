
if (!exists("proj_root", mode = "function")) {
  source(file.path("程序", "00_utils.R"))
}


brand_palette <- list(
  ink   = "#1A1A1F",
  paper = "#FAF7F2",
  rule  = "#1A1A1F1F",
  muted = "#5A5A65",

  source = c(
    gghed = "#1B5E88",
    pvtd  = "#C46B27",
    ext   = "#6B8E5A"
  ),

  continent = c(
    Africa     = "#C0504D",
    Americas   = "#1B5E88",
    Asia       = "#E8833C",
    Europe     = "#2A9D8F",
    Oceania    = "#7B4B94",
    Antarctica = "#9C9C9C"
  ),

  income = c(
    `High income`         = "#0B3D5C",
    `Upper middle income` = "#4F8FBF",
    `Lower middle income` = "#D89B5B",
    `Low income`          = "#A03B27"
  ),

  sequential = c(
    "#00204D", "#1F3F6E", "#445F8E",
    "#728DAF", "#A4BAC6", "#D6DEDB",
    "#FFEFB7"
  ),

  diverging = c(
    "#67001F", "#B2182B", "#D6604D", "#F4A582",
    "#F7F7F7",
    "#92C5DE", "#4393C3", "#2166AC", "#053061"
  ),

  accent  = "#C46B27",
  accent2 = "#2A9D8F"
)

brand_palette$gghed     <- brand_palette$source[["gghed"]]
brand_palette$pvtd      <- brand_palette$source[["pvtd"]]
brand_palette$ext       <- brand_palette$source[["ext"]]
brand_palette$africa    <- brand_palette$continent[["Africa"]]
brand_palette$americas  <- brand_palette$continent[["Americas"]]
brand_palette$asia      <- brand_palette$continent[["Asia"]]
brand_palette$europe    <- brand_palette$continent[["Europe"]]
brand_palette$oceania   <- brand_palette$continent[["Oceania"]]
brand_palette$rule_dark <- "#1A1A1F40"


register_brand_fonts <- function() {
  if (!requireNamespace("showtext", quietly = TRUE)) return(list(
    serif = "serif", sans = "sans", mono = "mono", cjk = "sans"
  ))
  showtext::showtext_auto()
  has_sysfonts <- requireNamespace("sysfonts", quietly = TRUE)
  if (!has_sysfonts) return(list(
    serif = "serif", sans = "sans", mono = "mono", cjk = "sans"
  ))

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


theme_ghs2 <- function(base_size = 13, grid = "y", panel = "flat") {
  ensure_pkgs(c("ggplot2"))
  fonts <- get_brand_fonts()
  pal <- brand_palette

  serif_fam <- if (!is.null(fonts$cjk) && nchar(fonts$cjk) &&
                  !identical(fonts$cjk, "sans"))
                 fonts$cjk else fonts$serif
  sans_fam  <- if (!is.null(fonts$cjk) && nchar(fonts$cjk) &&
                  !identical(fonts$cjk, "sans"))
                 fonts$cjk else fonts$sans
  fonts$serif <- serif_fam
  fonts$sans  <- sans_fam

  bg_panel <- switch(panel,
    card = "#FFFFFF",
    flat = pal$paper,
    pal$paper
  )

  th <- ggplot2::theme_minimal(base_size = base_size, base_family = fonts$sans) +
    ggplot2::theme(
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

      plot.background  = ggplot2::element_rect(fill = pal$paper, colour = NA),
      panel.background = ggplot2::element_rect(fill = bg_panel, colour = NA),
      panel.border     = ggplot2::element_blank(),

      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor   = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(
        colour = "#1A1A1F12", linewidth = 0.35),

      axis.title  = ggplot2::element_text(
        family = fonts$sans, colour = pal$muted, size = base_size * 0.92),
      axis.text   = ggplot2::element_text(
        family = fonts$sans, colour = pal$ink,   size = base_size * 0.86),
      axis.ticks  = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_line(colour = pal$ink, linewidth = 0.45),
      axis.line.y = ggplot2::element_blank(),

      strip.background = ggplot2::element_rect(fill = "#1A1A1F08", colour = NA),
      strip.text       = ggplot2::element_text(
        family = fonts$sans, face = "bold", colour = pal$ink,
        size = base_size * 0.92, margin = ggplot2::margin(4, 4, 4, 4)),

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

scale_fill_brand_continent <- function(...) {
  ggplot2::scale_fill_manual(values = brand_palette$continent, na.value = "grey80", ...)
}
scale_colour_brand_continent <- function(...) {
  ggplot2::scale_colour_manual(values = brand_palette$continent, na.value = "grey80", ...)
}

scale_fill_brand_income <- function(...) {
  ggplot2::scale_fill_manual(values = brand_palette$income, na.value = "grey80", ...)
}
scale_colour_brand_income <- function(...) {
  ggplot2::scale_colour_manual(values = brand_palette$income, na.value = "grey80", ...)
}

scale_fill_brand_seq <- function(...) {
  ggplot2::scale_fill_gradientn(colours = brand_palette$sequential, na.value = "grey90", ...)
}
scale_colour_brand_seq <- function(...) {
  ggplot2::scale_colour_gradientn(colours = brand_palette$sequential, na.value = "grey90", ...)
}

scale_fill_brand_div <- function(midpoint = 0, ...) {
  ggplot2::scale_fill_gradient2(
    low = brand_palette$diverging[2],
    mid = brand_palette$diverging[5],
    high = brand_palette$diverging[8],
    midpoint = midpoint, na.value = "grey90", ...)
}


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
  line       = list(light = "#0d121b1a",
                    dark  = "#e7e9ee24"),
  line_strong = list(light = "#0d121b2e",
                     dark  = "#e7e9ee3d"),
  code_bg    = list(light = "#0c1424", dark = "#06090f"),
  code_ink   = list(light = "#e6efff", dark = "#cfd6e2")
)

palette_ghs3 <- function(slot = "primary", mode = c("light", "dark")) {
  mode <- match.arg(mode)
  if (!slot %in% names(ghs3_slots))
    stop("Unknown slot: ", slot,
         "\nValid: ", paste(names(ghs3_slots), collapse = ", "))
  ghs3_slots[[slot]][[mode]]
}


ghs3_palette_discrete <- c(
  "#1d3f5f",
  "#c46327",
  "#2a857a",
  "#c89a3b",
  "#a23b3b",
  "#5b8aa6",
  "#7c5b9a",
  "#e0904c",
  "#86a062",
  "#b85578",
  "#7e8aa0",
  "#3a6e8f"
)

palette_ghs3_discrete <- function(n = 12) {
  if (n <= length(ghs3_palette_discrete)) {
    ghs3_palette_discrete[seq_len(n)]
  } else {
    grDevices::colorRampPalette(ghs3_palette_discrete)(n)
  }
}

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


ghs_register_fonts <- function() get_brand_fonts()


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


scale_fill_ghs3 <- function(...) {
  ggplot2::scale_fill_manual(values = ghs3_palette_discrete, na.value = "grey80", ...)
}
scale_colour_ghs3 <- function(...) {
  ggplot2::scale_colour_manual(values = ghs3_palette_discrete, na.value = "grey80", ...)
}
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
