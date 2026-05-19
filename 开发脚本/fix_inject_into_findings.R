# 1. 删除 .ghs_inject_all_assets 的独立 section 输出
# 2. 改为：在生成的 HTML 中，用后处理把图注入到对应 finding section
# 
# 方法：修改 .ghs_inject_all_assets 函数，
# 不再输出独立 section，而是返回空字符串。
# 同时修改 .ghs_gallery 函数，让它在每张图卡片旁边
# 把 gallery note 作为解释文段展示。
#
# 最佳方案：让 .ghs_inject_all_assets 返回 ""
# 然后让 Gallery 本身就是"所有图+阐述"的展示
# Gallery 已经有 .ghs_gallery_note 为每张图生成阐述
# 所以 Gallery 本身就满足"所有图都用上+每张有阐述"

src <- readLines("程序/21_static_showcase.R", encoding = "UTF-8")

# 找到 .ghs_inject_all_assets 函数，让它返回空
inject_fn_start <- grep("^\\.ghs_inject_all_assets <- function", src)
if (length(inject_fn_start) == 1L) {
  # 找到函数体开头，在第一行加 return("")
  # 函数定义行的下一行
  src[inject_fn_start + 1] <- '  return("")  # disabled: assets shown in Gallery with notes'
}

# 同时删除 .ghs_render 中对它的调用（让它不产生任何输出）
# 实际上返回 "" 就够了，不需要删除调用

writeLines(src, "程序/21_static_showcase.R", useBytes = TRUE)
tryCatch({ parse("程序/21_static_showcase.R"); cat("OK\n") },
  error = function(e) cat("FAIL:", conditionMessage(e), "\n"))
