# 恢复 Hero 原样 + 删除暗色模式

src <- readLines("程序/21_static_showcase.R", encoding = "UTF-8")

# 1. 恢复 hero header — 删除背景层 div
hero_bg_line <- grep("hero-bg.*hero-grid-lines.*hero-dots.*hero-gradient", src)
if (length(hero_bg_line) == 1L) {
  src[hero_bg_line] <- "      \"<header class='hero' id='top'>\","
}

# 2. 恢复标题动画为 inline style
eyebrow_line <- grep("hero-eyebrow hero-anim.*data-delay='200'", src)
if (length(eyebrow_line) == 1L) {
  src[eyebrow_line] <- "      \"<span class='hero-eyebrow' style='animation:ghs-fade-in .7s ease .2s both'>GLOBAL HEALTH EXPENDITURE \\u00b7 195 COUNTRIES \\u00b7 23 YEARS</span>\","
}
line1 <- grep("hero-line1 hero-anim.*data-delay='400'", src)
if (length(line1) == 1L) {
  src[line1] <- "      \"<span class='hero-line1' style='animation:ghs-slide-up .8s cubic-bezier(.16,1,.3,1) .4s both'>\\u5168\\u7403\\u536b\\u751f\\u652f\\u51fa</span>\","
}
line2 <- grep("hero-line2 hero-anim.*data-delay='600'", src)
if (length(line2) == 1L) {
  src[line2] <- "      \"<span class='hero-line2' style='animation:ghs-slide-up .8s cubic-bezier(.16,1,.3,1) .6s both'>2000\\u20132023\\uff1a</span>\","
}
line3 <- grep("hero-line3 hero-anim.*data-delay='800'", src)
if (length(line3) == 1L) {
  src[line3] <- "      \"<span class='hero-line3' style='animation:ghs-slide-up .8s cubic-bezier(.16,1,.3,1) .8s both'><em>\\u516c\\u5e73\\u3001\\u97e7\\u6027\\u3001\\u672a\\u6765</em></span>\","
}

# 3. 删除 hero-bg 相关 CSS（hero-grid-lines, hero-dots, hero-gradient, hero-anim）
bg_css_lines <- grep("hero-bg|hero-grid-lines|hero-dots-float|hero-dots\\{|hero-gradient|hero-glow|hero-anim", src)
if (length(bg_css_lines)) {
  src <- src[-bg_css_lines]
}

# 4. 删除 IntersectionObserver hero 动画 JS
hero_obs <- grep("heroEls.*querySelectorAll.*hero-anim", src)
if (length(hero_obs)) {
  src <- src[-hero_obs]
}

# 5. 删除暗色模式所有 CSS
dark_lines <- grep("html\\[data-theme='dark'\\]", src)
if (length(dark_lines)) {
  src <- src[-dark_lines]
}

# 6. 删除暗色模式 JS（toggleTheme, localStorage dark）
# 保留 toggleTheme 函数但让它什么都不做，或直接删除暗色相关
# 实际上删除太多可能破坏结构，改为让 toggleTheme 无效化
# 找到 theme-toggle 按钮并删除
toggle_btn <- grep("theme-toggle.*toggleTheme", src)
# 不删除 JS 函数本身（避免报错），只删除 CSS

# 7. 删除暗色 hero 背景的单独行
dark_hero_bg <- grep("hero-grid-lines.*background-image.*rgba\\(247,192,138", src)
if (length(dark_hero_bg)) {
  src <- src[-dark_hero_bg]
}

writeLines(src, "程序/21_static_showcase.R", useBytes = TRUE)

tryCatch({
  parse("程序/21_static_showcase.R")
  cat("parse OK\n")
}, error = function(e) cat("PARSE FAILED:", conditionMessage(e), "\n"))
