# 1. 删除主题切换按钮
# 2. 让 hero 动画每次滚回顶部时重播（用 IntersectionObserver）

src <- readLines("程序/21_static_showcase.R", encoding = "UTF-8")

# 1. 删除 dock 中的 theme-toggle 按钮行
theme_btn_lines <- grep("theme-toggle.*toggleTheme", src)
if (length(theme_btn_lines)) {
  src <- src[-theme_btn_lines]
}

# 2. 删除 toggleTheme JS 函数（不再需要）
toggle_js <- grep("function toggleTheme", src)
if (length(toggle_js)) {
  src <- src[-toggle_js]
}

# 3. 删除 localStorage ghs-theme 恢复逻辑
ls_theme <- grep("localStorage.getItem\\('ghs-theme'\\)", src)
if (length(ls_theme)) {
  src <- src[-ls_theme]
}

# 4. 将 hero 标题的 inline animation 改为 class 驱动 + IO 重播
# 改 eyebrow
eyebrow <- grep("hero-eyebrow.*animation:ghs-fade-in", src)
if (length(eyebrow) == 1L) {
  src[eyebrow] <- "      \"<span class='hero-eyebrow hero-reveal' data-delay='200'>GLOBAL HEALTH EXPENDITURE \\u00b7 195 COUNTRIES \\u00b7 23 YEARS</span>\","
}
# 改 line1
l1 <- grep("hero-line1.*animation:ghs-slide-up.*\\.4s", src)
if (length(l1) == 1L) {
  src[l1] <- "      \"<span class='hero-line1 hero-reveal' data-delay='400'>\\u5168\\u7403\\u536b\\u751f\\u652f\\u51fa</span>\","
}
# 改 line2
l2 <- grep("hero-line2.*animation:ghs-slide-up.*\\.6s", src)
if (length(l2) == 1L) {
  src[l2] <- "      \"<span class='hero-line2 hero-reveal' data-delay='600'>2000\\u20132023\\uff1a</span>\","
}
# 改 line3
l3 <- grep("hero-line3.*animation:ghs-slide-up.*\\.8s", src)
if (length(l3) == 1L) {
  src[l3] <- "      \"<span class='hero-line3 hero-reveal' data-delay='800'><em>\\u516c\\u5e73\\u3001\\u97e7\\u6027\\u3001\\u672a\\u6765</em></span>\","
}

# 5. 添加 hero-reveal CSS（初始隐藏，visible时动画进入）
# 找到 @keyframes ghs-slide-up 行之后插入
ks_line <- grep("@keyframes ghs-slide-up", src)
if (length(ks_line) >= 1L) {
  ks_line <- ks_line[1]
  reveal_css <- '    ".hero-reveal{opacity:0;transform:translateY(30px) scale(.97);filter:blur(2px);transition:opacity .8s cubic-bezier(.16,1,.3,1),transform .8s cubic-bezier(.16,1,.3,1),filter .8s ease}.hero-reveal.in{opacity:1;transform:translateY(0) scale(1);filter:blur(0)}",'
  src <- c(src[1:ks_line], reveal_css, src[(ks_line+1):length(src)])
}

# 6. 添加 IntersectionObserver JS 让 hero-reveal 元素在可见时触发，不可见时重置
dom_line <- grep("document.addEventListener\\('DOMContentLoaded'", src)
if (length(dom_line) >= 1L) {
  dom_line <- dom_line[length(dom_line)]
  hero_io_js <- '    "(function(){var els=document.querySelectorAll(\'.hero-reveal\');if(!els.length)return;var io=new IntersectionObserver(function(entries){entries.forEach(function(e){if(e.isIntersecting){var el=e.target;var d=parseInt(el.dataset.delay||0);setTimeout(function(){el.classList.add(\'in\');},d);}else{e.target.classList.remove(\'in\');}});},{threshold:0.15});els.forEach(function(el){io.observe(el);});})();",'
  src <- c(src[1:(dom_line-1)], hero_io_js, src[dom_line:length(src)])
}

# 7. 删除 .theme-toggle CSS
theme_css <- grep("\\.theme-toggle\\{", src)
if (length(theme_css)) {
  src <- src[-theme_css]
}

writeLines(src, "程序/21_static_showcase.R", useBytes = TRUE)

tryCatch({
  parse("程序/21_static_showcase.R")
  cat("parse OK\n")
}, error = function(e) cat("PARSE FAILED:", conditionMessage(e), "\n"))
