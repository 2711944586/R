
if (!exists("proj_root", mode = "function")) {
  source(file.path("程序", "00_utils.R"))
}

theme_ghs <- function(base_size = 13, base_family = "", grid = TRUE) {
  ensure_pkgs(c("ggplot2"))
  family <- base_family
  if (identical(family, "")) {
    reg <- tryCatch(register_fonts(), error = function(e) FALSE)
    family <- if (!isFALSE(reg) && is.character(reg)) reg else "sans"
  }
  th <- ggplot2::theme_minimal(base_size = base_size, base_family = family) +
    ggplot2::theme(
      plot.title         = ggplot2::element_text(face = "bold", size = base_size * 1.35,
                                                 margin = ggplot2::margin(b = 4)),
      plot.subtitle      = ggplot2::element_text(size = base_size * 1.02,
                                                 colour = "grey30",
                                                 margin = ggplot2::margin(b = 8)),
      plot.caption       = ggplot2::element_text(size = base_size * 0.78,
                                                 colour = "grey45", hjust = 0),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.background    = ggplot2::element_rect(fill = "white", colour = NA),
      panel.background   = ggplot2::element_rect(fill = "white", colour = NA),
      strip.background   = ggplot2::element_rect(fill = "grey95", colour = NA),
      strip.text         = ggplot2::element_text(face = "bold", size = base_size),
      legend.position    = "top",
      legend.title       = ggplot2::element_text(face = "bold"),
      axis.title         = ggplot2::element_text(colour = "grey25"),
      axis.text          = ggplot2::element_text(colour = "grey35")
    )
  if (isFALSE(grid)) {
    th <- th + ggplot2::theme(panel.grid = ggplot2::element_blank())
  } else if (identical(grid, "x")) {
    th <- th + ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                              panel.grid.minor   = ggplot2::element_blank())
  } else if (identical(grid, "y")) {
    th <- th + ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                              panel.grid.minor   = ggplot2::element_blank())
  } else {
    th <- th + ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
  }
  th
}

scale_fill_ghs_source  <- function(...) ggplot2::scale_fill_manual(
  values = c(
    `Domestic General Government Health Expenditure (GGHE-D)` = ghs_palette$funding[["gghed"]],
    `Domestic Private Health Expenditure (PVT-D)`             = ghs_palette$funding[["pvtd"]],
    `External Health Expenditure (EXT)`                       = ghs_palette$funding[["ext"]],
    gghed = ghs_palette$funding[["gghed"]],
    pvtd  = ghs_palette$funding[["pvtd"]],
    ext   = ghs_palette$funding[["ext"]]
  ),
  na.value = "grey70", ...
)

scale_fill_ghs_scheme <- function(...) ggplot2::scale_fill_manual(
  values = c(
    hf1    = ghs_palette$scheme[["hf1"]],
    hf2    = ghs_palette$scheme[["hf2"]],
    hf3    = ghs_palette$scheme[["hf3"]],
    hf4    = ghs_palette$scheme[["hf4"]],
    hfnec  = ghs_palette$scheme[["hfnec"]]
  ),
  na.value = "grey70", ...
)

scale_fill_ghs_continent <- function(...) ggplot2::scale_fill_manual(
  values = ghs_palette$continent, na.value = "grey70", ...
)
scale_colour_ghs_continent <- function(...) ggplot2::scale_colour_manual(
  values = ghs_palette$continent, na.value = "grey70", ...
)

scale_fill_ghs_income <- function(...) ggplot2::scale_fill_manual(
  values = ghs_palette$income, na.value = "grey70", ...
)
scale_colour_ghs_income <- function(...) ggplot2::scale_colour_manual(
  values = ghs_palette$income, na.value = "grey70", ...
)

labs_ghs <- function(title = NULL, subtitle = NULL,
                     x = NULL, y = NULL,
                     caption = ghs_caption_bi(),
                     ...) {
  ggplot2::labs(title = title, subtitle = subtitle,
                x = x, y = y, caption = caption, ...)
}
