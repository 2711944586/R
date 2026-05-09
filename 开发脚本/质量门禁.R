if (!dir.exists("程序") && basename(getwd()) == "开发脚本") setwd("..")

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L || is.na(a[1])) b else a

out_dir <- file.path("分析输出", "质量报告")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

stamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")

norm_path <- function(x) {
  gsub("\\\\", "/", x, fixed = FALSE)
}

read_text <- function(path) {
  if (!file.exists(path)) return("")
  tryCatch({
    paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  }, error = function(e) {
    raw <- readBin(path, what = "raw", n = file.info(path)[["size"]])
    txt <- tryCatch(rawToChar(raw, multiple = FALSE), error = function(e2) "")
    Encoding(txt) <- "UTF-8"
    txt
  })
}

write_csv <- function(x, name) {
  utils::write.csv(x, file.path(out_dir, name), row.names = FALSE, fileEncoding = "UTF-8")
  invisible(x)
}

status_rank <- function(x) {
  if (any(x == "fail", na.rm = TRUE)) "fail" else if (any(x == "warn", na.rm = TRUE)) "warn" else "pass"
}

mk_module <- function(module, df, status_col = "status") {
  data.frame(
    module = module,
    total = nrow(df),
    pass = sum(df[[status_col]] == "pass", na.rm = TRUE),
    warn = sum(df[[status_col]] == "warn", na.rm = TRUE),
    fail = sum(df[[status_col]] == "fail", na.rm = TRUE),
    status = status_rank(df[[status_col]]),
    stringsAsFactors = FALSE
  )
}

escape_html <- function(x) {
  x <- as.character(x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x
}

html_table <- function(df, max_rows = 200L) {
  if (!nrow(df)) return("<p>No rows.</p>")
  show <- head(df, max_rows)
  header <- paste0("<tr>", paste0("<th>", escape_html(names(show)), "</th>", collapse = ""), "</tr>")
  rows <- apply(show, 1, function(r) {
    paste0("<tr>", paste0("<td>", escape_html(r), "</td>", collapse = ""), "</tr>")
  })
  more <- if (nrow(df) > max_rows) sprintf("<p>Showing %d of %d rows.</p>", max_rows, nrow(df)) else ""
  paste0("<table>", header, paste(rows, collapse = "\n"), "</table>", more)
}

check_parse <- function() {
  files <- c(
    list.files("程序", pattern = "[.]R$", full.names = TRUE),
    list.files("开发脚本", pattern = "[.]R$", full.names = TRUE),
    list.files("仪表盘", pattern = "[.]R$", full.names = TRUE),
    "构建.R"
  )
  files <- files[file.exists(files)]
  rows <- lapply(files, function(f) {
    msg <- "OK"
    st <- tryCatch({
      parse(file = f, encoding = "UTF-8")
      "pass"
    }, error = function(e) {
      msg <<- conditionMessage(e)
      "fail"
    })
    data.frame(file = norm_path(f), status = st, message = msg, stringsAsFactors = FALSE)
  })
  out <- if (length(rows)) do.call(rbind, rows) else data.frame(file = character(), status = character(), message = character())
  write_csv(out, "parse_check.csv")
}

check_png <- function() {
  files <- list.files(file.path("分析输出", "图表"), pattern = "[.]png$", full.names = TRUE)
  has_png <- requireNamespace("png", quietly = TRUE)
  rows <- lapply(files, function(f) {
    info <- file.info(f)
    width <- height <- channels <- mean_lum <- sd_lum <- white_ratio <- NA_real_
    note <- "OK"
    st <- "pass"
    if (isTRUE(has_png)) {
      res <- tryCatch({
        img <- png::readPNG(f)
        d <- dim(img)
        if (length(d) == 2L) {
          height <- d[1]; width <- d[2]; channels <- 1
          v <- as.numeric(img)
          idx <- if (length(v) > 200000L) sample.int(length(v), 200000L) else seq_along(v)
          lum <- v[idx]
          white <- v[idx] > 0.985
        } else {
          height <- d[1]; width <- d[2]; channels <- d[3]
          n <- d[1] * d[2]
          idx <- if (n > 200000L) sample.int(n, 200000L) else seq_len(n)
          r <- as.vector(img[, , 1])[idx]
          g <- as.vector(img[, , min(2, d[3])])[idx]
          b <- as.vector(img[, , min(3, d[3])])[idx]
          lum <- 0.2126 * r + 0.7152 * g + 0.0722 * b
          white <- r > 0.985 & g > 0.985 & b > 0.985
        }
        mean_lum <- mean(lum, na.rm = TRUE)
        sd_lum <- stats::sd(lum, na.rm = TRUE)
        white_ratio <- mean(white, na.rm = TRUE)
        TRUE
      }, error = function(e) {
        note <<- conditionMessage(e)
        FALSE
      })
      if (!isTRUE(res)) st <- "warn"
      if (isTRUE(res) && ((is.finite(sd_lum) && sd_lum < 0.004) || (is.finite(white_ratio) && white_ratio > 0.995))) {
        st <- "fail"
        note <- "low visual variation or almost all white pixels"
      }
    } else {
      st <- "warn"
      note <- "optional package png is not installed; file-size check only"
    }
    if (isTRUE(info[["size"]] < 5000)) {
      st <- "fail"
      note <- "file smaller than 5 KB"
    }
    data.frame(
      file = norm_path(f), bytes = info[["size"]], width = width, height = height,
      channels = channels, mean_luminance = mean_lum, sd_luminance = sd_lum,
      white_ratio = white_ratio, status = st, note = note, stringsAsFactors = FALSE
    )
  })
  out <- if (length(rows)) do.call(rbind, rows) else data.frame(file = character(), bytes = numeric(), status = character(), note = character())
  write_csv(out, "blank_figures.csv")
}

check_svg <- function() {
  files <- list.files(file.path("分析输出", "图表"), pattern = "[.]svg$", full.names = TRUE)
  pat <- "not installed|placeholder|rendering failed|htmlwidget-error|traceback|TODO|待补充|这里插入"
  rows <- lapply(files, function(f) {
    txt <- read_text(f)
    hits <- gregexpr(pat, txt, ignore.case = TRUE, perl = TRUE)[[1]]
    n <- if (identical(hits[1], -1L)) 0L else length(hits)
    data.frame(file = norm_path(f), hits = n, status = if (n > 0L) "fail" else "pass", stringsAsFactors = FALSE)
  })
  out <- if (length(rows)) do.call(rbind, rows) else data.frame(file = character(), hits = integer(), status = character())
  write_csv(out, "svg_placeholder_scan.csv")
}

check_figure_diversity <- function() {
  files <- list.files(file.path("分析输出", "图表"), pattern = "[.]png$", full.names = TRUE)
  base <- tolower(tools::file_path_sans_ext(basename(files)))
  rules <- data.frame(
    type = c("趋势图", "分布图", "关系图", "地图", "组成图", "排名图",
             "模型图", "预测图", "不平等图", "质量图", "方法图"),
    minimum = c(10L, 10L, 10L, 10L, 10L, 10L, 5L, 5L, 5L, 5L, 5L),
    pattern = c(
      "area|line|timeseries|small|slope|bump|stream|covid|forecast|fan|changepoint|period|progress|profile",
      "ridge|ridges|density|box|violin|heatmap|hist|corr|matrix|scatter|waterfall",
      "scatter|bubble|bivariate|corr|pca|cluster|beta|elasticity|dea|quantile|outcomes|lifeexp|hc1|hc6|sdg3|efficiency|forecast|profile",
      "world|map|bivariate|continent|regional|country|profile|atlas|aid_dependency|sdg3|spatial",
      "source|sources|scheme|schemes|stream|radar|ternary|treemap|sankey|waffle|purpose|hc|fiscal|share|profile|country_compare|combined",
      "rank|ranking|lollipop|bump|slope|dumbbell|top|bottom|waterfall|dependency|extreme|profile",
      "pca|cluster|beta|forecast|fan|dea|elasticity|quantile|changepoint|corr|matrix|bivariate|efficiency|outcomes",
      "forecast|fan|scenario|changepoint|period|covid|progress|rank_change|beta",
      "inequality|equity|lorenz|gini|theil|atkinson|oops|aid_dependency|fiscal_ghe_rank",
      "heatmap|corr|matrix|box|coverage|missing|quality|pca|cluster|sdg3|period",
      "pca|cluster|beta|dea|quantile|corr|matrix|forecast|changepoint|bivariate|elasticity|efficiency|sankey|ternary"
    ),
    stringsAsFactors = FALSE
  )
  detail <- if (length(files)) {
    rows <- lapply(seq_along(files), function(i) {
      hit <- rules$type[vapply(rules$pattern, grepl, logical(1), x = base[i])]
      data.frame(file = norm_path(files[i]), type = paste(hit, collapse = ";"),
                 status = if (length(hit)) "pass" else "fail",
                 stringsAsFactors = FALSE)
    })
    do.call(rbind, rows)
  } else {
    data.frame(file = "NO_PNG", type = "", status = "fail", stringsAsFactors = FALSE)
  }
  write_csv(detail, "figure_type_map.csv")
  rows <- lapply(seq_len(nrow(rules)), function(i) {
    hits <- if (length(base)) grepl(rules$pattern[i], base) else logical(0)
    examples <- paste(utils::head(base[hits], 8), collapse = ";")
    data.frame(type = rules$type[i], count = sum(hits), minimum = rules$minimum[i],
               examples = examples,
               status = if (sum(hits) >= rules$minimum[i]) "pass" else "fail",
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  write_csv(out, "figure_diversity.csv")
}

extract_links <- function(txt) {
  m <- gregexpr("(?:src|href)\\s*=\\s*['\"][^'\"]+['\"]", txt, ignore.case = TRUE, perl = TRUE)[[1]]
  if (identical(m[1], -1L)) return(character())
  raw <- regmatches(txt, list(m))[[1]]
  raw <- sub("^[^=]+=\\s*['\"]", "", raw, perl = TRUE)
  sub("['\"]$", "", raw, perl = TRUE)
}

check_links <- function() {
  index <- file.path("网站发布", "index.html")
  if (!file.exists(index)) {
    out <- data.frame(url = index, target = index, status = "fail", note = "index.html missing", stringsAsFactors = FALSE)
    return(write_csv(out, "link_check.csv"))
  }
  urls <- unique(extract_links(read_text(index)))
  skip <- grepl("^(https?:|mailto:|tel:|#|javascript:|data:|//)", urls, ignore.case = TRUE)
  local <- urls[!skip]
  rows <- lapply(local, function(u) {
    clean <- sub("[?#].*$", "", u)
    clean <- utils::URLdecode(clean)
    clean <- sub("^/R/", "", clean)
    target <- if (grepl("^/", clean)) clean else file.path(dirname(index), clean)
    ok <- file.exists(target) || dir.exists(target)
    data.frame(url = u, target = norm_path(target), status = if (ok) "pass" else "fail", note = if (ok) "OK" else "missing local asset", stringsAsFactors = FALSE)
  })
  out <- if (length(rows)) do.call(rbind, rows) else data.frame(url = character(), target = character(), status = character(), note = character())
  write_csv(out, "link_check.csv")
}

check_widgets <- function() {
  files <- unique(c(
    list.files(file.path("分析输出", "交互组件"), pattern = "[.]html$", full.names = TRUE),
    list.files(file.path("网站发布", "交互组件"), pattern = "[.]html$", full.names = TRUE)
  ))
  pat <- "htmlwidget-error|plotly_empty|rendering failed|Error in|traceback|object not found"
  rows <- lapply(files, function(f) {
    txt <- read_text(f)
    hits <- gregexpr(pat, txt, ignore.case = TRUE, perl = TRUE)[[1]]
    n <- if (identical(hits[1], -1L)) 0L else length(hits)
    has_shell <- grepl("data-ghs-widget-shell", txt, fixed = TRUE)
    has_loading <- grepl("ghs-widget-loading", txt, fixed = TRUE)
    has_empty <- grepl("ghs-widget-empty", txt, fixed = TRUE)
    has_error <- grepl("ghs-widget-error", txt, fixed = TRUE)
    has_caption <- grepl("ghs-widget-caption", txt, fixed = TRUE)
    has_source <- grepl("ghs-widget-source", txt, fixed = TRUE)
    has_fallback <- grepl("ghs-widget-fallback", txt, fixed = TRUE)
    shell_ok <- has_shell && has_loading && has_empty && has_error &&
      has_caption && has_source && has_fallback
    data.frame(
      file = norm_path(f),
      hits = n,
      shell = has_shell,
      loading = has_loading,
      empty = has_empty,
      error = has_error,
      caption = has_caption,
      source = has_source,
      fallback = has_fallback,
      status = if (n > 0L || !shell_ok) "fail" else "pass",
      stringsAsFactors = FALSE
    )
  })
  out <- if (length(rows)) {
    do.call(rbind, rows)
  } else {
    data.frame(file = "NO_WIDGET_HTML", hits = 0L, shell = FALSE,
               loading = FALSE, empty = FALSE, error = FALSE, caption = FALSE,
               source = FALSE, fallback = FALSE, status = "fail",
               stringsAsFactors = FALSE)
  }
  write_csv(out, "widget_error_scan.csv")
}

check_interactive_coverage <- function() {
  analysis_widgets <- list.files(file.path("分析输出", "交互组件"),
                                 pattern = "[.]html$", full.names = TRUE)
  published_widgets <- list.files(file.path("网站发布", "交互组件"),
                                  pattern = "[.]html$", full.names = TRUE)
  module_files <- list.files(file.path("仪表盘", "模块"),
                             pattern = "^mod_.*[.]R$", full.names = TRUE)
  widget_rows <- function(files, kind) {
    if (!length(files)) return(NULL)
    data.frame(
      kind = kind,
      name = tools::file_path_sans_ext(basename(files)),
      path = norm_path(files),
      openable = file.exists(files) & file.info(files)[["size"]] > 5000,
      stringsAsFactors = FALSE
    )
  }
  module_rows <- if (length(module_files)) {
    rows <- lapply(module_files, function(f) {
      txt <- read_text(f)
      ui_ok <- grepl("_ui\\s*<-\\s*function", txt, perl = TRUE) &&
        grepl("nav_panel|tabPanel", txt, perl = TRUE)
      data.frame(
        kind = "shiny_module",
        name = sub("^mod_", "", tools::file_path_sans_ext(basename(f))),
        path = norm_path(f),
        openable = ui_ok,
        stringsAsFactors = FALSE
      )
    })
    do.call(rbind, rows)
  } else {
    NULL
  }
  detail <- do.call(rbind, Filter(Negate(is.null), list(
    widget_rows(analysis_widgets, "analysis_widget"),
    widget_rows(published_widgets, "published_widget"),
    module_rows
  )))
  if (is.null(detail) || !nrow(detail)) {
    detail <- data.frame(kind = "none", name = "NO_INTERACTIVE_VIEW",
                         path = "", openable = FALSE,
                         stringsAsFactors = FALSE)
  }
  detail$status <- ifelse(detail$openable, "pass", "fail")
  write_csv(detail, "interactive_view_inventory.csv")
  by_kind <- stats::aggregate(openable ~ kind, detail, sum)
  names(by_kind)[2] <- "count"
  total <- sum(detail$openable)
  rows <- rbind(
    data.frame(item = by_kind$kind, count = by_kind$count,
               minimum = c(analysis_widget = 24L,
                           published_widget = 24L,
                           shiny_module = 12L)[by_kind$kind],
               status = ifelse(by_kind$count >= c(analysis_widget = 24L,
                                                   published_widget = 24L,
                                                   shiny_module = 12L)[by_kind$kind],
                               "pass", "fail"),
               stringsAsFactors = FALSE),
    data.frame(item = "total_openable_views", count = total, minimum = 60L,
               status = ifelse(total >= 60L, "pass", "fail"),
               stringsAsFactors = FALSE)
  )
  rows$minimum[is.na(rows$minimum)] <- 0L
  write_csv(rows, "interactive_coverage.csv")
}

check_gallery_notes <- function() {
  index <- file.path("网站发布", "index.html")
  if (!file.exists(index)) {
    out <- data.frame(item = "index.html", value = 0L, status = "fail", note = "index.html missing", stringsAsFactors = FALSE)
    return(write_csv(out, "gallery_notes.csv"))
  }
  txt <- read_text(index)
  txt <- gsub("data:image/[^;]+;base64,[A-Za-z0-9+/=]+", "data:image;base64,STRIPPED", txt, perl = TRUE)
  card_hits <- gregexpr("class='gallery-card'", txt, fixed = TRUE)[[1]]
  note_hits <- gregexpr("<p class='gallery-note'>.*?</p>", txt, perl = TRUE)[[1]]
  card_count <- if (identical(card_hits[1], -1L)) 0L else length(card_hits)
  notes <- if (identical(note_hits[1], -1L)) character() else regmatches(txt, list(note_hits))[[1]]
  note_text <- gsub("<[^>]+>", "", notes, perl = TRUE)
  note_chars <- nchar(note_text, type = "chars", allowNA = FALSE, keepNA = FALSE)
  note_count <- length(notes)
  short_count <- sum(note_chars < 80L)
  rows <- data.frame(
    item = c("gallery cards", "gallery notes", "notes shorter than 80 chars", "minimum note chars"),
    value = c(card_count, note_count, short_count, if (length(note_chars)) min(note_chars) else 0L),
    status = c(
      if (card_count > 0L) "pass" else "fail",
      if (note_count == card_count && note_count > 0L) "pass" else "fail",
      if (short_count == 0L && note_count > 0L) "pass" else "fail",
      if (length(note_chars) && min(note_chars) >= 80L) "pass" else "fail"
    ),
    note = c("static PNG gallery card count", "one explanation per card", "all gallery explanations should be substantial", "minimum explanatory caption length"),
    stringsAsFactors = FALSE
  )
  write_csv(rows, "gallery_notes.csv")
}

scan_source_files <- function() {
  all <- list.files(".", recursive = TRUE, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  all <- all[!grepl("(^|/)([.]git|[.]Rproj[.]user|renv|packrat|分析输出/交互组件|分析输出/质量报告|网站发布/交互组件)(/|$)", norm_path(all))]
  ext <- tolower(tools::file_ext(all))
  keep_ext <- ext %in% c("r", "rmd", "qmd", "md", "yml", "yaml", "html", "js", "css", "txt", "json", "env")
  keep_name <- basename(all) %in% c(".Renviron", ".env", "DESCRIPTION", "README.md")
  all[keep_ext | keep_name]
}

check_secrets <- function() {
  files <- scan_source_files()
  files <- files[norm_path(files) != "./开发脚本/质量门禁.R"]
  pats <- c(
    github_pat = "github_pat_[A-Za-z0-9_]{30,}",
    github_ghp = "ghp_[A-Za-z0-9_]{30,}",
    aws_key = "AKIA[0-9A-Z]{16}",
    shiny_assign = "(?i)SHINYAPPS_(TOKEN|SECRET)\\s*[:=]\\s*['\"]?[A-Za-z0-9/_+=.-]{20,}",
    generic_secret = "(?i)(api[_-]?key|password|secret|token)\\s*[:=]\\s*['\"][^'\"]{16,}['\"]"
  )
  rows <- list()
  for (f in files) {
    txt <- read_text(f)
    if (!nzchar(txt)) next
    for (nm in names(pats)) {
      m <- gregexpr(pats[[nm]], txt, perl = TRUE)[[1]]
      if (identical(m[1], -1L)) next
      rows[[length(rows) + 1L]] <- data.frame(file = norm_path(sub("^[.]/", "", f)), pattern = nm, hits = length(m), status = "fail", stringsAsFactors = FALSE)
    }
  }
  out <- if (length(rows)) do.call(rbind, rows) else data.frame(file = character(), pattern = character(), hits = integer(), status = character())
  write_csv(out, "secret_scan.csv")
}

check_text_traces <- function() {
  files <- c("README.md", list.files("课程提交", pattern = "[.]Rmd$", full.names = TRUE), file.path("网站发布", "index.html"))
  files <- files[file.exists(files)]
  pats <- c(
    ai_self = "作为\\s*AI|ChatGPT|AI\\s*生成|人工智能生成",
    placeholder = "TODO|待补充|这里插入|lorem ipsum|placeholder text|placeholder content|占位文本",
    fake_citation = "引用待补|citation needed|source needed",
    empty_phrase = "全面深入分析|重要意义|综合探讨",
    abnormal_terms = "十平均|凝滑|仁期|雷中型|医医|概心|快请口|谜言|启动仔表盘|不可调勮|镶位|咤价|靾马|复诨|成本分摄|人肉点|心跪|双老外金|股聊表|亢拉|拼均|坚筹|外援划转|不平等双量"
  )
  rows <- list()
  for (f in files) {
    txt <- read_text(f)
    if (grepl("[.]html?$", f, ignore.case = TRUE)) {
      txt <- gsub("data:image/[^;]+;base64,[A-Za-z0-9+/=]+", "data:image;base64,STRIPPED", txt, perl = TRUE)
    }
    for (nm in names(pats)) {
      m <- gregexpr(pats[[nm]], txt, ignore.case = TRUE, perl = TRUE)[[1]]
      if (identical(m[1], -1L)) next
      rows[[length(rows) + 1L]] <- data.frame(file = norm_path(f), pattern = nm, hits = length(m), status = "fail", stringsAsFactors = FALSE)
    }
  }
  out <- if (length(rows)) do.call(rbind, rows) else data.frame(file = character(), pattern = character(), hits = integer(), status = character())
  write_csv(out, "text_trace_scan.csv")
}

check_sizes <- function() {
  files <- c(
    file.path("网站发布", "index.html"),
    file.path("课程提交", "庄颂_20241334.html"),
    list.files(file.path("分析输出", "交互组件"), pattern = "[.]html$", full.names = TRUE),
    list.files(file.path("分析输出", "图表"), pattern = "[.](png|svg)$", full.names = TRUE)
  )
  files <- unique(files[file.exists(files)])
  rows <- lapply(files, function(f) {
    bytes <- file.info(f)[["size"]]
    limit <- if (grepl("index[.]html$|庄颂_20241334[.]html$", f)) 60 * 1024^2 else if (grepl("[.]html$", f)) 25 * 1024^2 else 10 * 1024^2
    st <- if (bytes > limit) "warn" else "pass"
    data.frame(file = norm_path(f), bytes = bytes, mb = round(bytes / 1024^2, 2), limit_mb = round(limit / 1024^2, 2), status = st, stringsAsFactors = FALSE)
  })
  out <- if (length(rows)) do.call(rbind, rows) else data.frame(file = character(), bytes = numeric(), status = character())
  write_csv(out, "artifact_size.csv")
}

check_shiny_bundle <- function() {
  checks <- data.frame(
    item = c("app_structure", "snapshot", "program_library", "shinylive_index"),
    path = c(paste(file.path("仪表盘", c("global.R", "ui.R", "server.R")), collapse = ";"), file.path("仪表盘", "数据快照", "snapshot.rds"), file.path("仪表盘", "程序库"), file.path("网站发布", "仪表盘", "index.html")),
    stringsAsFactors = FALSE
  )
  app_files <- file.path("仪表盘", c("global.R", "ui.R", "server.R"))
  checks$exists <- c(all(file.exists(app_files)), file.exists(checks$path[2]), dir.exists(checks$path[3]), file.exists(checks$path[4]))
  checks$count <- c(
    sum(file.exists(app_files)),
    as.integer(file.exists(checks$path[2])),
    length(list.files(checks$path[3], pattern = "[.]R$", full.names = TRUE)),
    as.integer(file.exists(checks$path[4]))
  )
  checks$status <- ifelse(checks$item == "program_library", ifelse(checks$count >= 10L, "pass", "fail"), ifelse(checks$exists, "pass", "fail"))
  checks$path <- norm_path(checks$path)
  write_csv(checks, "shiny_bundle.csv")
}

check_static_navigation <- function() {
  index_path <- file.path("网站发布", "index.html")
  index <- read_text(index_path)
  nav_ids <- c("executive", "kpi", "methods", "findings", "countries",
               "regional", "period", "sdg3", "lifeexp", "atlas",
               "cluster-detail", "extreme", "simulator", "figure-index",
               "gallery", "widgets", "repro", "conclusion")
  nav_ok <- vapply(nav_ids, function(id) {
    grepl(sprintf("id=['\"]%s['\"]", id), index, perl = TRUE)
  }, logical(1))
  rows <- data.frame(
    item = c("index exists", "desktop nav", "mobile toc", "figure index",
             "figure search", "scroll spy", "navigation anchors"),
    value = c(
      as.character(file.exists(index_path)),
      as.character(grepl("class='links'", index, fixed = TRUE)),
      as.character(grepl("id='mobile-toc'", index, fixed = TRUE)),
      as.character(grepl("id='figure-index'", index, fixed = TRUE)),
      as.character(grepl("filterFigureIndex", index, fixed = TRUE)),
      as.character(grepl(".mobile-toc-links a", index, fixed = TRUE) &&
                     grepl("classList.toggle('active'", index, fixed = TRUE)),
      paste(names(nav_ok)[!nav_ok], collapse = ";")
    ),
    status = c(
      if (file.exists(index_path)) "pass" else "fail",
      if (grepl("class='links'", index, fixed = TRUE)) "pass" else "fail",
      if (grepl("id='mobile-toc'", index, fixed = TRUE)) "pass" else "fail",
      if (grepl("id='figure-index'", index, fixed = TRUE)) "pass" else "fail",
      if (grepl("filterFigureIndex", index, fixed = TRUE)) "pass" else "fail",
      if (grepl(".mobile-toc-links a", index, fixed = TRUE) &&
          grepl("classList.toggle('active'", index, fixed = TRUE)) "pass" else "fail",
      if (all(nav_ok)) "pass" else "fail"
    ),
    stringsAsFactors = FALSE
  )
  write_csv(rows, "static_navigation.csv")
}

check_science_traceability <- function() {
  index <- read_text(file.path("网站发布", "index.html"))
  handbook_path <- file.path("项目文档", "方法手册.md")
  handbook <- read_text(handbook_path)
  dict_path <- file.path("派生数据", "处理结果", "feature_dictionary.csv")
  dict <- if (file.exists(dict_path)) {
    tryCatch(utils::read.csv(dict_path, stringsAsFactors = FALSE,
                             fileEncoding = "UTF-8"), error = function(e) data.frame())
  } else {
    data.frame()
  }
  count_rx <- function(txt, pat) {
    hits <- gregexpr(pat, txt, ignore.case = TRUE, perl = TRUE)[[1]]
    if (identical(hits[1], -1L)) 0L else length(hits)
  }
  required_fields <- c("研究问题", "数据口径", "方法", "代码入口", "主要产物", "局限")
  field_counts <- vapply(required_fields, count_rx, integer(1), txt = handbook)
  dict_cols <- c("variable", "label_zh", "source", "unit", "formula",
                 "role", "valid_n", "missing_rate")
  rows <- data.frame(
    item = c(
      "method handbook exists",
      "F1-F14 method sections",
      paste0("handbook field: ", required_fields),
      "feature dictionary exists",
      "feature dictionary rows",
      "feature dictionary required columns",
      "site method blocks",
      "site limitation notes",
      "site source mentions",
      "site unit mentions",
      "site sample/time mentions",
      "site causal caution",
      "site uncertainty caution"
    ),
    value = c(
      as.character(file.exists(handbook_path)),
      count_rx(handbook, "(?m)^###\\s+F[0-9]+\\b"),
      field_counts,
      as.character(file.exists(dict_path)),
      nrow(dict),
      paste(setdiff(dict_cols, names(dict)), collapse = ";"),
      count_rx(index, "class='method-block'"),
      count_rx(index, "class='limit-note'|研究局限"),
      count_rx(index, "WHO Global Health Expenditure Database|Source|数据来源"),
      count_rx(index, "USD 2023|百分比|占 CHE|per capita"),
      count_rx(index, "2000|2023|样本|国家年份|时间范围"),
      count_rx(index, "不构成因果|不识别因果|不是因果估计|相关"),
      count_rx(index, "不确定|置信区间|预测区间|敏感性|不是确定预言|预测")
    ),
    minimum = c(
      "TRUE",
      14L,
      rep(14L, length(required_fields)),
      "TRUE",
      40L,
      "",
      14L,
      14L,
      10L,
      3L,
      20L,
      10L,
      5L
    ),
    stringsAsFactors = FALSE
  )
  rows$status <- c(
    if (file.exists(handbook_path)) "pass" else "fail",
    if (count_rx(handbook, "(?m)^###\\s+F[0-9]+\\b") >= 14L) "pass" else "fail",
    ifelse(field_counts >= 14L, "pass", "fail"),
    if (file.exists(dict_path)) "pass" else "fail",
    if (nrow(dict) >= 40L) "pass" else "fail",
    if (all(dict_cols %in% names(dict))) "pass" else "fail",
    if (count_rx(index, "class='method-block'") >= 14L) "pass" else "fail",
    if (count_rx(index, "class='limit-note'|研究局限") >= 14L) "pass" else "fail",
    if (count_rx(index, "WHO Global Health Expenditure Database|Source|数据来源") >= 10L) "pass" else "fail",
    if (count_rx(index, "USD 2023|百分比|占 CHE|per capita") >= 3L) "pass" else "fail",
    if (count_rx(index, "2000|2023|样本|国家年份|时间范围") >= 20L) "pass" else "fail",
    if (count_rx(index, "不构成因果|不识别因果|不是因果估计|相关") >= 10L) "pass" else "fail",
    if (count_rx(index, "不确定|置信区间|预测区间|敏感性|不是确定预言|预测") >= 5L) "pass" else "fail"
  )
  write_csv(rows, "science_traceability.csv")
}

check_deliverables <- function() {
  spec <- data.frame(
    item = c(
      "course html", "course rmd", "publish index", "publish widgets",
      "shinylive index", "static png figures", "static svg figures",
      "model tables", "feature mart csv", "feature dictionary",
      "method handbook", "upgrade plan", "readme", "quality report"
    ),
    path = c(
      file.path("课程提交", "庄颂_20241334.html"),
      file.path("课程提交", "庄颂_20241334.Rmd"),
      file.path("网站发布", "index.html"),
      file.path("网站发布", "交互组件"),
      file.path("网站发布", "仪表盘", "index.html"),
      file.path("分析输出", "图表"),
      file.path("分析输出", "图表"),
      file.path("分析输出", "模型表"),
      file.path("派生数据", "处理结果", "feature_mart_country_year.csv"),
      file.path("派生数据", "处理结果", "feature_dictionary.csv"),
      file.path("项目文档", "方法手册.md"),
      file.path("项目文档", "十倍升级详细Plan.md"),
      "README.md",
      file.path("分析输出", "质量报告", "quality_gate.html")
    ),
    pattern = c(
      "", "", "", "[.]html$", "", "[.]png$", "[.]svg$", "[.](csv|rds)$",
      "", "", "", "", "", ""
    ),
    minimum = c(1L, 1L, 1L, 40L, 1L, 85L, 85L, 10L, 1L, 1L, 1L, 1L, 1L, 1L),
    stringsAsFactors = FALSE
  )
  rows <- lapply(seq_len(nrow(spec)), function(i) {
    p <- spec$path[i]
    pat <- spec$pattern[i]
    count <- if (dir.exists(p) && nzchar(pat)) {
      length(list.files(p, pattern = pat, recursive = TRUE, full.names = TRUE))
    } else if (dir.exists(p)) {
      length(list.files(p, recursive = TRUE, full.names = TRUE))
    } else {
      as.integer(file.exists(p))
    }
    bytes <- if (file.exists(p) && !dir.exists(p)) file.info(p)[["size"]] else NA_real_
    data.frame(
      item = spec$item[i],
      path = norm_path(p),
      count = count,
      minimum = spec$minimum[i],
      bytes = bytes,
      status = if (count >= spec$minimum[i]) "pass" else "fail",
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  write_csv(out, "deliverable_manifest.csv")
}

check_readme <- function() {
  readme <- read_text("README.md")
  index <- read_text(file.path("网站发布", "index.html"))
  png_count <- length(list.files(file.path("分析输出", "图表"), pattern = "[.]png$"))
  svg_count <- length(list.files(file.path("分析输出", "图表"), pattern = "[.]svg$"))
  widget_count <- length(list.files(file.path("分析输出", "交互组件"), pattern = "[.]html$"))
  rows <- data.frame(
    item = c("README exists", "Plan indexed", "method handbook indexed", "method handbook", "quality command", "features command", "feature mart", "feature dictionary", "png artifacts", "svg artifacts", "widget artifacts", "site h2", "site iframes", "site fig-frame"),
    value = c(
      as.character(file.exists("README.md")),
      as.character(grepl("十倍升级详细Plan", readme, fixed = TRUE)),
      as.character(grepl("方法手册.md", readme, fixed = TRUE)),
      as.character(file.exists(file.path("项目文档", "方法手册.md"))),
      as.character(grepl("构建.R quality", readme, fixed = TRUE) || grepl("质量门禁.R", readme, fixed = TRUE)),
      as.character(grepl("构建.R features", readme, fixed = TRUE)),
      as.character(file.exists(file.path("派生数据", "处理结果", "feature_mart_country_year.csv"))),
      as.character(file.exists(file.path("派生数据", "处理结果", "feature_dictionary.csv"))),
      png_count,
      svg_count,
      widget_count,
      length(gregexpr("<h2", index, ignore.case = TRUE, fixed = FALSE)[[1]][gregexpr("<h2", index, ignore.case = TRUE, fixed = FALSE)[[1]] > 0]),
      length(gregexpr("<iframe", index, ignore.case = TRUE, fixed = FALSE)[[1]][gregexpr("<iframe", index, ignore.case = TRUE, fixed = FALSE)[[1]] > 0]),
      length(gregexpr("fig-frame", index, ignore.case = TRUE, fixed = FALSE)[[1]][gregexpr("fig-frame", index, ignore.case = TRUE, fixed = FALSE)[[1]] > 0])
    ),
    stringsAsFactors = FALSE
  )
  rows$status <- "pass"
  rows$status[rows$item %in% c("README exists", "Plan indexed", "method handbook indexed", "method handbook", "quality command", "features command", "feature mart", "feature dictionary") & rows$value != "TRUE"] <- "fail"
  write_csv(rows, "readme_consistency.csv")
}

checks <- list(
  parse = check_parse(),
  png = check_png(),
  svg = check_svg(),
  diversity = check_figure_diversity(),
  links = check_links(),
  widgets = check_widgets(),
  interactive = check_interactive_coverage(),
  gallery = check_gallery_notes(),
  secrets = check_secrets(),
  text = check_text_traces(),
  sizes = check_sizes(),
  shiny = check_shiny_bundle(),
  navigation = check_static_navigation(),
  science = check_science_traceability(),
  deliverables = check_deliverables(),
  readme = check_readme()
)

summary <- do.call(rbind, Map(mk_module, names(checks), checks))
write_csv(summary, "quality_gate_summary.csv")

json_path <- file.path(out_dir, "quality_gate.json")
if (requireNamespace("jsonlite", quietly = TRUE)) {
  jsonlite::write_json(list(generated_at = stamp, summary = summary), json_path, pretty = TRUE, auto_unbox = TRUE)
} else {
  rows <- apply(summary, 1, function(r) {
    sprintf('{"module":"%s","total":%s,"pass":%s,"warn":%s,"fail":%s,"status":"%s"}', r[["module"]], r[["total"]], r[["pass"]], r[["warn"]], r[["fail"]], r[["status"]])
  })
  writeLines(c("{", sprintf('  "generated_at": "%s",', stamp), '  "summary": [', paste(paste0("    ", rows), collapse = ",\n"), "  ]", "}"), json_path, useBytes = TRUE)
}

html <- paste0(
  "<!doctype html><html lang='zh-CN'><head><meta charset='utf-8'>",
  "<title>Quality Gate Report</title>",
  "<style>body{font-family:system-ui,-apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif;margin:32px;background:#f8fafc;color:#0f172a}table{border-collapse:collapse;width:100%;margin:16px 0;background:white}th,td{border:1px solid #e2e8f0;padding:8px;text-align:left;font-size:13px}th{background:#e2e8f0}.pass{color:#166534}.warn{color:#92400e}.fail{color:#991b1b}code{background:#e2e8f0;padding:2px 4px;border-radius:4px}</style>",
  "</head><body><h1>Quality Gate Report</h1>",
  sprintf("<p>Generated at <code>%s</code>.</p>", escape_html(stamp)),
  "<h2>Summary</h2>", html_table(summary),
  paste0(sprintf("<h2>%s</h2>", escape_html(names(checks))), vapply(checks, html_table, character(1)), collapse = "\n"),
  "</body></html>"
)
writeLines(html, file.path(out_dir, "quality_gate.html"), useBytes = TRUE)

cat("\n=== QUALITY GATE SUMMARY ===\n")
print(summary, row.names = FALSE)
cat("\n[quality] reports written to ", norm_path(out_dir), "\n", sep = "")

if (any(summary$status == "fail")) {
  stop("quality gate failed; open 分析输出/质量报告/quality_gate.html for details", call. = FALSE)
}

invisible(summary)
