# 开发脚本/追加报告拓展.R
# 在 报告书/05-12 每章末尾追加 v2 数据新闻拓展节
# 每章: 1 张 plot_v2_*  + 1 个 widget_v2_*  + 1 个 callout

ch_blocks <- list(
  "05-equity.qmd" = c(
    "",
    "## 5.x v2 数据新闻拓展 {.unnumbered}",
    "",
    "```{r ch05-v2-lorenz, fig.cap=\"主题 A · Lorenz 曲线 2000 vs 2023（人口加权）\", fig.height=4.6}",
    "plot_v2_equity_lorenz(master_enriched, years = c(2000, max(master_enriched$year)))",
    "```",
    "",
    "```{r ch05-v2-violin, echo=FALSE}",
    "htmltools::tagList(widget_v2_income_violin(master_enriched))",
    "```",
    "",
    "::: {.callout-note appearance=\"minimal\" icon=false}",
    "**Violin × 收入组**：分布形状揭示「**贫富分布的厚尾性**」—— 中位数下行不代表所有人受益，分布右尾可能仍在恶化。",
    ":::"
  ),
  "06-fiscal-space.qmd" = c(
    "",
    "## 6.x v2 数据新闻拓展 {.unnumbered}",
    "",
    "```{r ch06-v2-rank, fig.cap=\"主题 B · 政府卫生支出占财政百分比·全球排名\", fig.height=6}",
    "plot_v2_fiscal_ghe_rank(master_enriched, year = max(master_enriched$year))",
    "```",
    "",
    "```{r ch06-v2-highlight, echo=FALSE}",
    "htmltools::tagList(widget_v2_highlight_lines(master_enriched, ",
    "                    countries = c(\"USA\", \"CHN\", \"DEU\", \"BRA\", \"ZAF\", \"IND\")))",
    "```",
    "",
    "::: {.callout-warning appearance=\"minimal\" icon=false}",
    "**财政空间警报**：当政府债务 / GDP 超过 90% 同时 GGHE-D 占比下降时，常预示 IMF 紧缩条款下的「**卫生 crowding-out**」。",
    ":::"
  ),
  "07-efficiency.qmd" = c(
    "",
    "## 7.x v2 数据新闻拓展 {.unnumbered}",
    "",
    "```{r ch07-v2-dea, fig.cap=\"主题 C · 简化 DEA 前沿（CHE × 寿命）\", fig.height=5}",
    "plot_v2_efficiency_dea(master_enriched, year = max(master_enriched$year))",
    "```",
    "",
    "```{r ch07-v2-splom, echo=FALSE}",
    "htmltools::tagList(widget_v2_splom(master_enriched, year = max(master_enriched$year)))",
    "```",
    "",
    "::: {.callout-tip appearance=\"minimal\" icon=false}",
    "**SPLOM 矩阵**：6 维指标两两散点 + 边缘密度。寻找「**有效率前沿**」上的国家：低 CHE/cap 但高寿命的样本。",
    ":::"
  ),
  "08-outcomes.qmd" = c(
    "",
    "## 8.x v2 数据新闻拓展 {.unnumbered}",
    "",
    "```{r ch08-v2-ridges, fig.cap=\"主题 F · 收入组 × 年份·人均 CHE 山脊图\", fig.height=5}",
    "plot_v2_combined_ridges(master_enriched)",
    "```",
    "",
    "```{r ch08-v2-ternary, echo=FALSE}",
    "htmltools::tagList(widget_v2_ternary(master_enriched, year = max(master_enriched$year)))",
    "```",
    "",
    "::: {.callout-note appearance=\"minimal\" icon=false}",
    "**三元图**：每个国家在「政府 / 私人 / 外援」组合下的位置。撒哈拉以南非洲多数贴近右下「外援」顶点，OECD 国家集中于左侧「政府」轴。",
    ":::"
  ),
  "09-aid-flows.qmd" = c(
    "",
    "## 9.x v2 数据新闻拓展 {.unnumbered}",
    "",
    "```{r ch09-v2-aid, fig.cap=\"主题 E · 援助依赖度 × 国家收入分组\", fig.height=5}",
    "plot_v2_aid_dependency(master_enriched, year = max(master_enriched$year))",
    "```",
    "",
    "```{r ch09-v2-network, echo=FALSE}",
    "htmltools::tagList(widget_v2_country_network(master_enriched, ",
    "                    year = max(master_enriched$year), k = 4))",
    "```",
    "",
    "::: {.callout-tip appearance=\"minimal\" icon=false}",
    "**国家相似度网络**：基于卫生筹资指标余弦相似度的 k-NN 图。**节点颜色 = 大洲**；**边粗细 = 相似度**。撒哈拉援助型国家自然形成密集子图。",
    ":::"
  ),
  "10-clusters.qmd" = c(
    "",
    "## 10.x v2 数据新闻拓展 {.unnumbered}",
    "",
    "```{r ch10-v2-bivariate, fig.cap=\"数据新闻 2 · OOPS × 政府份额 bivariate map\", fig.height=5.5}",
    "plot_v2_bivariate_oops_gov(master_enriched, year = max(master_enriched$year))",
    "```",
    "",
    "```{r ch10-v2-rank, echo=FALSE}",
    "htmltools::tagList(widget_v2_reactable_rank(master_enriched, ",
    "                    year = max(master_enriched$year)))",
    "```",
    "",
    "::: {.callout-note appearance=\"minimal\" icon=false}",
    "**Bivariate 双指标**：把「OOPS 高/低 × 政府份额 高/低」映射到 9 宫格颜色。**右上角（OOPS 高 + 政府低）= 高风险国**；**左下角 = 健康筹资**。",
    ":::"
  ),
  "11-scenarios.qmd" = c(
    "",
    "## 11.x v2 数据新闻拓展 {.unnumbered}",
    "",
    "```{r ch11-v2-scenarios, echo=FALSE}",
    "htmltools::tagList(widget_v2_scenarios(master_enriched, country_iso = \"CHN\"))",
    "```",
    "",
    "```{r ch11-v2-mcfan, echo=FALSE}",
    "htmltools::tagList(widget_v2_mc_fan(master_enriched, country_iso = \"USA\"))",
    "```",
    "",
    "::: {.callout-warning appearance=\"minimal\" icon=false}",
    "**情景切换器**：点击按钮切换「悲观 / 基线 / 乐观」3 种弹性参数下的 2030 路径。**MC fan** 给出非参数概率扇，比单一点预测更诚实。",
    ":::"
  ),
  "12-roads-ahead.qmd" = c(
    "",
    "## 5.x v2 数据新闻拓展 {.unnumbered}",
    "",
    "```{r ch12-v2-stream, fig.cap=\"数据新闻 5 · 大洲三源 stream chart\", fig.height=4.6}",
    "plot_v2_continent_stream(master_enriched)",
    "```",
    "",
    "```{r ch12-v2-atlas, echo=FALSE}",
    "htmltools::tagList(widget_v2_dt_atlas(master_enriched))",
    "```",
    "",
    "::: {.callout-tip appearance=\"minimal\" icon=false}",
    "**Atlas DT**：30+ 字段全字段查询表。可搜索、可排序、可下载 CSV。读者可以在此自行验证书中所有结论。",
    ":::"
  )
)

base <- "报告书"
for (fn in names(ch_blocks)) {
  path <- file.path(base, fn)
  if (!file.exists(path)) {
    cat("[skip]", fn, "not found\n"); next
  }
  txt <- readLines(path, encoding = "UTF-8", warn = FALSE)
  if (any(grepl("v2 数据新闻拓展", txt, fixed = TRUE))) {
    cat("[skip]", fn, "already has v2 section\n"); next
  }
  new_txt <- c(txt, ch_blocks[[fn]])
  writeLines(new_txt, path, useBytes = TRUE)
  cat("[ok]", fn, "appended", length(ch_blocks[[fn]]), "lines\n")
}

cat("\nDONE\n")
