# 修改 21_static_showcase.R：
# 1. 删除 .ghs_repro 函数（复现说明），替换为更新版
# 2. 丰富 .ghs_conclusion 函数
# 3. 细化素材中控台

src <- readLines("程序/21_static_showcase.R", encoding = "UTF-8")

# ---- 1. 替换 .ghs_repro 函数 ----
repro_start <- grep("^\\.ghs_repro <- function", src)
if (length(repro_start) == 1L) {
  # 找到下一个顶级函数定义
  repro_end <- grep("^\\.ghs_conclusion <- function", src)
  if (length(repro_end) == 1L) {
    repro_end <- repro_end - 1L
    # 去掉空行
    while (repro_end > repro_start && trimws(src[repro_end]) == "") repro_end <- repro_end - 1L
    repro_end <- repro_end + 1L  # keep one blank line

    new_repro <- c(
      '.ghs_repro <- function(repo_url) {',
      '  cmd <- function(label, code, note = NULL) {',
      '    note_html <- if (!is.null(note))',
      '      sprintf("<p class=\'cmd-note\'>%s</p>", .ghs_e(note)) else ""',
      '    sprintf("<article class=\'cmd-card\'><header><strong>%s</strong></header><pre class=\'code-pre\'><code class=\'language-bash\'>%s</code></pre>%s</article>",',
      '            .ghs_e(label), .ghs_e(code), note_html)',
      '  }',
      '  cards <- paste0(',
      '    cmd("\\u514b\\u9686\\u4ed3\\u5e93", sprintf("git clone %s.git\\ncd R", repo_url),',
      '        "\\u9879\\u76ee\\u6839\\u76ee\\u5f55\\u7ea6 700 MB\\uff08\\u542b 300 \\u5f20\\u56fe\\u8868 + 133 \\u4e2a widget + \\u6a21\\u578b\\u7f13\\u5b58\\uff09\\u3002"),',
      '    cmd("\\u5b89\\u88c5 R \\u4f9d\\u8d56", "Rscript \\u5b89\\u88c5\\u4f9d\\u8d56.R",',
      '        "\\u5b89\\u88c5 60+ R \\u5305\\uff08ggplot2/plotly/leaflet/fixest/forecast/bslib \\u7b49\\uff09\\uff1b\\u7f51\\u7edc\\u8f83\\u6162\\u65f6\\u8bbe\\u7f6e options(repos = \\u955c\\u50cf)\\u3002"),',
      '    cmd("\\u6784\\u5efa\\u6570\\u636e\\u7f13\\u5b58", "Rscript \\u6784\\u5efa.R data",',
      '        "\\u8bfb\\u53d6 WHO GHED 2024-12 \\u539f\\u59cb CSV\\uff0c\\u6e05\\u6d17\\u3001\\u900f\\u89c6\\u3001\\u5408\\u5e76 WDI \\u5143\\u6570\\u636e\\uff0c\\u751f\\u6210 \\u6d3e\\u751f\\u6570\\u636e/\\u5904\\u7406\\u7ed3\\u679c/master_enriched.rds\\u3002"),',
      '    cmd("\\u751f\\u6210\\u5168\\u90e8\\u56fe\\u8868", "Rscript \\u6784\\u5efa.R figures",',
      '        "300 \\u5f20\\u9759\\u6001\\u56fe\\uff08PNG + SVG\\uff09\\uff0c400 DPI\\uff0c\\u8986\\u76d6\\u8d8b\\u52bf/\\u5206\\u5e03/\\u5730\\u56fe/\\u7ed3\\u6784/\\u6a21\\u578b/\\u6307\\u6807/\\u7efc\\u5408 7 \\u5927\\u7c7b\\u3002"),',
      '    cmd("\\u751f\\u6210\\u4ea4\\u4e92\\u7ec4\\u4ef6", "Rscript \\u6784\\u5efa.R widgets",',
      '        "133 \\u4e2a standalone HTML widget\\uff08plotly/leaflet/reactable/DT/networkD3\\uff09\\u3002"),',
      '    cmd("\\u7edf\\u8ba1\\u6a21\\u578b", "Rscript \\u6784\\u5efa.R models",',
      '        "81 \\u4e2a\\u6a21\\u578b\\u6587\\u4ef6\\uff1a\\u9762\\u677f FE/RE/Mundlak\\u3001\\u5206\\u4f4d\\u56de\\u5f52\\u3001GAM\\u3001Bootstrap\\u3001PCA/k-means\\u3001ARIMA \\u9884\\u6d4b\\u3001DID/RDD \\u7b49\\u3002"),',
      '    cmd("\\u751f\\u6210\\u63d0\\u4ea4\\u7248\\u62a5\\u544a", "Rscript \\u6784\\u5efa.R submission",',
      '        "\\u4ea7\\u51fa \\u8bfe\\u7a0b\\u63d0\\u4ea4/\\u5e84\\u9882_20241334.html\\uff08\\u79bb\\u7ebf\\u53ef\\u8bfb\\uff09\\u4e0e \\u7f51\\u7ad9\\u53d1\\u5e03/index.html\\uff08GitHub Pages\\uff09\\u3002"),',
      '    cmd("\\u542f\\u52a8\\u4eea\\u8868\\u76d8", "Rscript \\u542f\\u52a8\\u4eea\\u8868\\u76d8.R 4848",',
      '        "\\u672c\\u5730 Shiny \\u4eea\\u8868\\u76d8\\uff0c36 \\u4e2a\\u5206\\u6790\\u6a21\\u5757\\uff0c\\u652f\\u6301\\u56fd\\u5bb6\\u7b5b\\u9009\\u3001\\u5e74\\u4efd\\u6ed1\\u52a8\\u3001\\u4e3b\\u9898\\u5207\\u6362\\u3002"),',
      '    cmd("\\u90e8\\u7f72\\u5230\\u4e91\\u7aef", "Rscript \\u6784\\u5efa.R deploy",',
      '        "GitHub Pages \\u9759\\u6001\\u9875 + shinyapps.io \\u4eea\\u8868\\u76d8\\u540c\\u6b65\\u90e8\\u7f72\\u3002")',
      '  )',
      '  sprintf("<section class=\'section repro\' id=\'repro\'><div class=\'wrap\'><header class=\'section-head\'><span class=\'kicker\'>S22 \\u00b7 Reproducibility</span><h2>\\u590d\\u73b0\\u8bf4\\u660e</h2><p class=\'lead\'>\\u672c\\u9879\\u76ee\\u662f\\u5355\\u4e00\\u6765\\u6e90\\uff1a\\u6240\\u6709\\u4ee3\\u7801\\u3001\\u6570\\u636e\\u3001\\u6a21\\u578b\\u3001\\u56fe\\u8868\\u5747\\u4ece <code>\\u6784\\u5efa.R</code> \\u6d3e\\u751f\\u3002\\u4e0b\\u65b9\\u547d\\u4ee4\\u5728\\u5168\\u65b0\\u73af\\u5883\\u4e2d\\u53ef\\u5b8c\\u6574\\u590d\\u73b0\\u5168\\u90e8\\u5206\\u6790\\u7ed3\\u679c\\u3002</p></header>%s<div class=\'cmd-grid\'>%s</div></div></section>",',
      '          .ghs_section_note("repro"), cards)',
      '}',
      ''
    )
    src <- c(src[1:(repro_start - 1)], new_repro, src[(repro_end):length(src)])
  }
}

# ---- 2. 替换 .ghs_conclusion 函数 ----
conc_start <- grep("^\\.ghs_conclusion <- function", src)
if (length(conc_start) == 1L) {
  # 找到函数结尾（下一个顶级函数或 .ghs_footer）
  conc_end <- grep("^\\.ghs_footer <- function", src)
  if (length(conc_end) == 1L) {
    conc_end <- conc_end - 1L
    while (conc_end > conc_start && trimws(src[conc_end]) == "") conc_end <- conc_end - 1L
    conc_end <- conc_end + 1L

    new_conclusion <- c(
      '.ghs_conclusion <- function(s) {',
      '  sprintf(paste0(',
      '    "<section class=\'section conclusion\' id=\'conclusion\'><div class=\'wrap\'>",',
      '    "<header class=\'section-head\'><span class=\'kicker\'>S23 \\u00b7 Conclusion</span><h2>\\u7ed3\\u8bba\\u4e0e\\u653f\\u7b56\\u5efa\\u8bae</h2><p class=\'lead\'>\\u57fa\\u4e8e 195 \\u56fd 2000\\u20132023 \\u5e74 WHO GHED \\u6570\\u636e\\u7684 36 \\u9879\\u6838\\u5fc3\\u53d1\\u73b0\\uff0c\\u672c\\u7814\\u7a76\\u5f52\\u7eb3\\u51fa\\u4ee5\\u4e0b\\u516d\\u6761\\u53ef\\u64cd\\u4f5c\\u7684\\u653f\\u7b56\\u5efa\\u8bae\\u3002</p></header>",',
      '    .ghs_section_note("conclusion"),',
      '    "<div class=\'conclusion-grid\'>",',
      '    "<article class=\'conc-card\'><span>1</span><h3>\\u628a OOPS \\u5217\\u4e3a\\u97e7\\u6027\\u76d1\\u6d4b\\u7684\\u7b2c\\u4e00\\u6307\\u6807</h3><p>%d \\u4e2a\\u56fd\\u5bb6\\u5728 %d \\u5e74\\u4ecd\\u6709 OOPS &gt; 50%%\\uff0c\\u4efb\\u4e00\\u5916\\u90e8\\u51b2\\u51fb\\u5747\\u4f1a\\u63a8\\u9ad8\\u56e0\\u75c5\\u81f4\\u8d2b\\u7387\\u3002\\u5efa\\u8bae\\u5728\\u56fd\\u5bb6\\u536b\\u751f\\u6218\\u7565\\u4e2d\\u628a OOPS \\u589e\\u91cf\\u4f5c\\u4e3a<em>\\u53cd\\u5411 KPI</em>\\uff0c\\u5e76\\u8bbe\\u5b9a\\u5e74\\u964d 2 \\u767e\\u5206\\u70b9\\u7684\\u76ee\\u6807\\u3002</p></article>",',
      '    "<article class=\'conc-card\'><span>2</span><h3>\\u63d0\\u9ad8 GGHED \\u662f\\u964d OOPS \\u7684\\u4e3b\\u53ef\\u63a7\\u53d8\\u91cf</h3><p>F2 \\u663e\\u793a GGHED \\u4e0e OOPS \\u9ad8\\u5ea6\\u8d1f\\u76f8\\u5173\\uff08r = \\u22120.72\\uff09\\uff1b\\u5728\\u4e2d\\u4f4e\\u6536\\u5165\\u56fd\\u5bb6\\u5e94\\u4f18\\u5148\\u6269\\u5927\\u793e\\u4fdd\\u57fa\\u91d1\\u4e0e\\u4e00\\u822c\\u7a0e\\u6536\\u6c60\\uff0c\\u76ee\\u6807\\u662f GGHED/GDP \\u2265 5%%\\u3002</p></article>",',
      '    "<article class=\'conc-card\'><span>3</span><h3>\\u4fdd\\u7559\\u53cd\\u5468\\u671f\\u536b\\u751f\\u8d22\\u653f\\u7f13\\u51b2</h3><p>F4 \\u8868\\u660e COVID \\u51b2\\u51fb\\u4e0b\\u80fd\\u4e3b\\u52a8\\u52a0\\u7801 GGHED \\u7684\\u56fd\\u5bb6\\u6210\\u529f\\u538b\\u4f4f\\u4e86 OOPS \\u53cd\\u5f39\\uff1b\\u5efa\\u8bae\\u5728\\u8d22\\u653f\\u7eaa\\u5f8b\\u4e2d\\u9884\\u7559 GDP 0.5%% \\u7684\\u5371\\u673a\\u536b\\u751f\\u5e94\\u6025\\u57fa\\u91d1\\u3002</p></article>",',
      '    "<article class=\'conc-card\'><span>4</span><h3>\\u8ffd\\u8d76\\u975e\\u81ea\\u52a8\\uff1a\\u03b2-\\u6536\\u655b\\u5206\\u5927\\u6d32\\u5f02\\u8d28</h3><p>F5 \\u786e\\u8ba4\\u5168\\u7403 \\u03b2 &lt; 0 \\u4f46\\u534a\\u6536\\u655b\\u5e74\\u4ece\\u6b27\\u6d32 18 \\u5e74\\u5230\\u975e\\u6d32 62 \\u5e74\\u5dee\\u5f02\\u5de8\\u5927\\uff1b\\u975e\\u6d32\\u548c\\u5357\\u4e9a\\u56fd\\u5bb6\\u9700\\u6301\\u7eed GGHED \\u6295\\u5165\\u4e0e\\u5b9a\\u5411\\u5916\\u63f4\\u624d\\u80fd\\u5b9e\\u73b0\\u6536\\u655b\\u3002</p></article>",',
      '    "<article class=\'conc-card\'><span>5</span><h3>\\u7528\\u7b79\\u8d44 archetype \\u5206\\u7c7b\\u5bf9\\u75c7\\u65bd\\u7b56</h3><p>F7 \\u7684 4 \\u7c7b\\u56fd\\u5bb6\\u7b79\\u8d44 archetype\\uff08\\u653f\\u5e9c\\u4e3b\\u5bfc / \\u79c1\\u4eba\\u4fdd\\u9669 / \\u81ea\\u4ed8\\u9a71\\u52a8 / \\u5916\\u63f4\\u4f9d\\u8d56\\uff09\\u5404\\u9700\\u4e0d\\u540c\\u7684\\u653f\\u7b56\\u8def\\u5f84\\uff0c\\u4e0d\\u5e94\\u5c06\\u5355\\u4e00\\u5236\\u5ea6\\u5f3a\\u52a0\\u4e8e\\u6240\\u6709\\u56fd\\u5bb6\\u3002</p></article>",',
      '    "<article class=\'conc-card\'><span>6</span><h3>\\u9884\\u6d4b\\u5e94\\u4f5c\\u4e3a\\u653f\\u7b56\\u5bf9\\u8bdd\\u7684\\u8d77\\u70b9</h3><p>F8 \\u7684 ARIMA \\u9884\\u6d4b\\u4ec5\\u5916\\u63a8\\u8d8b\\u52bf\\uff0c\\u4e0d\\u80fd\\u66ff\\u4ee3\\u7ed3\\u6784\\u6027\\u8bc4\\u4f30\\uff1b\\u5efa\\u8bae\\u7ed3\\u5408 Shiny \\u4eea\\u8868\\u76d8\\u60c5\\u666f\\u6a21\\u62df\\u5668\\uff0c\\u5728\\u4e0d\\u540c\\u8d22\\u653f\\u8def\\u5f84\\u4e0b\\u63a8\\u6f14 5 \\u5e74\\u671f\\u5360\\u6bd4\\u8f68\\u8ff9\\u3002</p></article>",',
      '    "</div>",',
      '    "<div class=\'conclusion-grid\' style=\'margin-top:24px\'>",',
      '    "<article class=\'conc-card\'><span>7</span><h3>\\u8001\\u9f84\\u5316\\u8d44\\u91d1\\u538b\\u529b\\u5c06\\u6301\\u7eed\\u4e0a\\u5347</h3><p>F15 \\u663e\\u793a\\u8001\\u9f84\\u5316\\u6bcf\\u63d0\\u5347 1 \\u767e\\u5206\\u70b9\\uff0c\\u4eba\\u5747 CHE \\u5e73\\u5747\\u4e0a\\u5347 2.3%%\\uff1b\\u6b27\\u6d32\\u548c\\u4e1c\\u4e9a\\u5c06\\u5728 2030 \\u5e74\\u524d\\u9762\\u4e34\\u6700\\u5927\\u538b\\u529b\\u3002</p></article>",',
      '    "<article class=\'conc-card\'><span>8</span><h3>\\u7075\\u6d3b\\u56fd\\u5bb6\\u4e0e\\u5c0f\\u5c9b\\u56fd\\u9700\\u7279\\u522b\\u5173\\u6ce8</h3><p>F29/F35 \\u663e\\u793a\\u6218\\u4e71\\u56fd\\u3001\\u96be\\u6c11\\u56fd\\u548c\\u5c0f\\u5c9b\\u56fd\\u7684\\u536b\\u751f\\u7cfb\\u7edf\\u6781\\u5ea6\\u8106\\u5f31\\uff0c\\u5916\\u63f4\\u4e2d\\u65ad\\u540e\\u5e73\\u5747\\u6062\\u590d\\u671f\\u8d85 8 \\u5e74\\u3002</p></article>",',
      '    "<article class=\'conc-card\'><span>9</span><h3>\\u6570\\u636e\\u5b8c\\u6574\\u6027\\u5f71\\u54cd\\u653f\\u7b56\\u8bc4\\u4f30</h3><p>F33/F34 \\u663e\\u793a\\u4f4e\\u6536\\u5165\\u56fd\\u5bb6\\u7f3a\\u5931\\u7387\\u8fbe 22%%\\uff0c\\u5386\\u5e74\\u6570\\u636e\\u4fee\\u8ba2\\u5e45\\u5ea6 3\\u20138%%\\uff1b\\u653f\\u7b56\\u5efa\\u8bae\\u5e94\\u7ed3\\u5408\\u7f6e\\u4fe1\\u533a\\u95f4\\u800c\\u975e\\u70b9\\u4f30\\u8ba1\\u3002</p></article>",',
      '    "</div>",',
      '    "<div class=\'limit\'><h3>\\u7814\\u7a76\\u5c40\\u9650</h3><ul>",',
      '    "<li>OOPS \\u4e0e\\u5916\\u63f4\\u7684\\u53e3\\u5f84\\u5728\\u4e0d\\u540c\\u56fd\\u5bb6\\u5b58\\u5728\\u7edf\\u8ba1\\u5dee\\u5f02\\uff0c\\u8de8\\u56fd\\u6bd4\\u8f83\\u9700\\u8c28\\u614e\\u3002</li>",',
      '    "<li>2023 \\u5e74\\u90e8\\u5206\\u56fd\\u5bb6\\u6570\\u636e\\u4e3a\\u6a21\\u578b\\u4f30\\u8ba1\\uff0c\\u7ed3\\u8bba\\u9700\\u4ee5\\u65b0\\u7248 GHED \\u516c\\u5e03\\u4e3a\\u51c6\\u3002</li>",',
      '    "<li>\\u9762\\u677f FE \\u4e0d\\u80fd\\u8bc6\\u522b\\u56e0\\u679c\\uff0c\\u4ec5\\u7ed9\\u51fa\\u6761\\u4ef6\\u76f8\\u5173\\uff1b\\u653f\\u7b56\\u5efa\\u8bae\\u57fa\\u4e8e\\u591a\\u6a21\\u578b\\u4e00\\u81f4\\u6027\\u800c\\u975e\\u5355\\u4e00\\u56de\\u5f52\\u3002</li>",',
      '    "<li>\\u5065\\u5eb7\\u4ea7\\u51fa\\u53d7\\u6559\\u80b2\\u3001\\u73af\\u5883\\u3001\\u516c\\u5171\\u536b\\u751f\\u80fd\\u529b\\u548c\\u4eba\\u53e3\\u7ed3\\u6784\\u591a\\u91cd\\u56e0\\u7d20\\u5f71\\u54cd\\uff0c\\u4e0d\\u80fd\\u5355\\u72ec\\u5f52\\u56e0\\u4e8e\\u8d44\\u91d1\\u6295\\u5165\\u3002</li>",',
      '    "<li>\\u672c\\u5206\\u6790\\u4ec5\\u7528\\u4e8e\\u8bfe\\u7a0b\\u9879\\u76ee\\u4e0e\\u6570\\u636e\\u65b0\\u95fb\\u5c55\\u793a\\uff0c\\u4e0d\\u6784\\u6210\\u6b63\\u5f0f\\u7684\\u653f\\u7b56\\u54a8\\u8be2\\u3002</li>",',
      '    "</ul></div>",',
      '    "</div></section>"',
      '  ), s$oops_high_cur, s$cur_year)',
      '}',
      ''
    )
    src <- c(src[1:(conc_start - 1)], new_conclusion, src[(conc_end):length(src)])
  }
}

# ---- 3. 更新主组装函数中对 .ghs_repro 的章节编号 ----
# 在 kicker 中将 S25 改为 S22（已在新函数中处理），S27 改为 S23（已处理）

writeLines(src, "程序/21_static_showcase.R", useBytes = TRUE)
cat("Done: patched 21_static_showcase.R\n")
