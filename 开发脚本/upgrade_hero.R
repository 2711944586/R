# 升级 Hero 区：动态数据背景 + 可重播动画 + 视觉质感提升

src <- readLines("程序/21_static_showcase.R", encoding = "UTF-8")

# 1. 在 .ghs_hero 中替换 hero HTML —— 添加背景层
hero_sprintf <- grep("paste0\\(\n.*\"%s\",", src)
# 找到 hero header 行
hero_header <- grep("<header class='hero' id='top'>", src, fixed = TRUE)
if (length(hero_header) == 1L) {
  # 替换为带有背景层的 hero
  src[hero_header] <- "      \"<header class='hero' id='top'><div class='hero-bg'><div class='hero-grid-lines'></div><div class='hero-dots'></div><div class='hero-gradient'></div></div>\","
}

# 2. 修改标题动画：从 inline style 改为 class 驱动（可通过 JS 重播）
# 找到 hero-eyebrow 行
eyebrow_line <- grep("hero-eyebrow.*animation:ghs-fade-in", src)
if (length(eyebrow_line) == 1L) {
  src[eyebrow_line] <- "      \"<span class='hero-eyebrow hero-anim' data-delay='200'>GLOBAL HEALTH EXPENDITURE \\u00b7 195 COUNTRIES \\u00b7 23 YEARS</span>\","
}

# hero-line1/2/3
line1 <- grep("hero-line1.*animation:ghs-slide-up", src)
if (length(line1) == 1L) {
  src[line1] <- "      \"<span class='hero-line1 hero-anim' data-delay='400'>\\u5168\\u7403\\u536b\\u751f\\u652f\\u51fa</span>\","
}
line2 <- grep("hero-line2.*animation:ghs-slide-up", src)
if (length(line2) == 1L) {
  src[line2] <- "      \"<span class='hero-line2 hero-anim' data-delay='600'>2000\\u20132023\\uff1a</span>\","
}
line3 <- grep("hero-line3.*animation:ghs-slide-up", src)
if (length(line3) == 1L) {
  src[line3] <- "      \"<span class='hero-line3 hero-anim' data-delay='800'><em>\\u516c\\u5e73\\u3001\\u97e7\\u6027\\u3001\\u672a\\u6765</em></span>\","
}

# 3. 在 .ghs_css() 中添加 hero 背景动画 CSS
# 找到 ".hero{position:relative" 行
hero_css_line <- grep('"\\.hero\\{position:relative', src)
if (length(hero_css_line) == 1L) {
  # 在它后面插入新的背景 CSS
  new_bg_css <- c(
    '    ".hero-bg{position:absolute;inset:0;overflow:hidden;z-index:0}",',
    '    ".hero-grid-lines{position:absolute;inset:0;background-image:linear-gradient(rgba(29,63,95,.04) 1px,transparent 1px),linear-gradient(90deg,rgba(29,63,95,.04) 1px,transparent 1px);background-size:60px 60px;animation:hero-grid-drift 30s linear infinite}",',
    '    "@keyframes hero-grid-drift{to{background-position:60px 60px}}",',
    '    ".hero-dots{position:absolute;inset:0;background-image:radial-gradient(circle,rgba(196,99,39,.12) 1.5px,transparent 1.5px);background-size:40px 40px;animation:hero-dots-float 20s ease-in-out infinite alternate}",',
    '    "@keyframes hero-dots-float{from{transform:translate(0,0)}to{transform:translate(12px,8px)}}",',
    '    ".hero-gradient{position:absolute;inset:0;background:radial-gradient(ellipse 80% 60% at 20% 40%,rgba(247,192,138,.12),transparent 60%),radial-gradient(ellipse 60% 50% at 80% 70%,rgba(29,63,95,.08),transparent 50%);animation:hero-glow 12s ease-in-out infinite alternate}",',
    '    "@keyframes hero-glow{from{opacity:.6;transform:scale(1)}to{opacity:1;transform:scale(1.05)}}",',
    '    ".hero-anim{opacity:0;transform:translateY(30px);transition:opacity .8s cubic-bezier(.16,1,.3,1),transform .8s cubic-bezier(.16,1,.3,1)}",',
    '    ".hero-anim.visible{opacity:1;transform:translateY(0)}",',
    '    ".hero-center{position:relative;z-index:2}",'
  )
  # 在 hero-center 原始行之前插入（hero CSS 之后）
  hero_center_line <- grep('"\\.hero-center\\{position:relative', src)
  if (length(hero_center_line) == 1L) {
    src <- c(src[1:(hero_center_line - 1)], new_bg_css, src[hero_center_line:length(src)])
  }
}

# 4. 在 JS 中添加 IntersectionObserver 实现可重播动画
# 找到 DOMContentLoaded 行
dom_line <- grep("document.addEventListener\\('DOMContentLoaded'", src)
if (length(dom_line) >= 1L) {
  dom_line <- dom_line[length(dom_line)]
  # 在它前面插入 hero 动画 observer
  hero_js <- '    "(function(){var heroEls=document.querySelectorAll(\'.hero-anim\');if(!heroEls.length)return;var obs=new IntersectionObserver(function(entries){entries.forEach(function(en){if(en.isIntersecting){var el=en.target;var d=parseInt(el.dataset.delay||0);setTimeout(function(){el.classList.add(\'visible\');},d);}else{el.classList.remove(\'visible\');}});},{threshold:0.1});heroEls.forEach(function(el){obs.observe(el);});})();",'
  src <- c(src[1:(dom_line - 1)], hero_js, src[dom_line:length(src)])
}

# 5. 暗色模式背景
dark_hero_line <- grep("html\\[data-theme='dark'\\] \\.hero\\{background", src)
if (length(dark_hero_line) == 1L) {
  # 在它后面追加暗色背景层样式
  dark_bg <- '    "html[data-theme=\'dark\'] .hero-grid-lines{background-image:linear-gradient(rgba(247,192,138,.03) 1px,transparent 1px),linear-gradient(90deg,rgba(247,192,138,.03) 1px,transparent 1px)}html[data-theme=\'dark\'] .hero-dots{background-image:radial-gradient(circle,rgba(247,192,138,.08) 1.5px,transparent 1.5px)}html[data-theme=\'dark\'] .hero-gradient{background:radial-gradient(ellipse 80% 60% at 20% 40%,rgba(247,192,138,.06),transparent 60%),radial-gradient(ellipse 60% 50% at 80% 70%,rgba(42,133,122,.06),transparent 50%)}",'
  src <- c(src[1:dark_hero_line], dark_bg, src[(dark_hero_line + 1):length(src)])
}

writeLines(src, "程序/21_static_showcase.R", useBytes = TRUE)

tryCatch({
  parse("程序/21_static_showcase.R")
  cat("parse OK\n")
}, error = function(e) cat("PARSE FAILED:", conditionMessage(e), "\n"))
