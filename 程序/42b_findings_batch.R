# F28-F36 batch specs + batch renderer + entry function
# Sourced at end of 42_findings_extra.R

.fe_batch_spec3 <- function() { list(
  list(id="f-theil", num="F28", kicker="DECOMPOSITION",
    title="\u4e0d\u5e73\u7b49\u5206\u89e3",
    lead="Theil 65%\u7ec4\u95f4\u3001 35%\u7ec4\u5185\u3002\u5168\u7403\u536b\u751f\u4e0d\u5e73\u7b49\u4e3b\u8981\u7531\u6536\u5165\u7ec4\u95f4\u5dee\u5f02\u9a71\u52a8\u3002",
    chips=list(c("\u7ec4\u95f4","65%","orange"),c("\u7ec4\u5185","35%","ink"),c("N","195","ink")),
    figs=list(c("eq_theil_decomp.png","Theil\u5206\u89e3","28A"),c("eq_gini_trend.png","Gini\u8d8b\u52bf","28B"),c("eq_lorenz_che.png","Lorenz","28C")),
    widgets=list(c("iadv_theil_decomp.html","Theil\u52a8\u6001")),
    callout_title="\u5206\u6790\u89e3\u8bfb",
    callout=c("Theil-T \u7684 65% \u6765\u81ea\u56db\u4e2a\u6536\u5165\u7ec4\u4e4b\u95f4\u7684\u5dee\u5f02\u3002\u8fd9\u610f\u5473\u7740\u7f29\u5c0f\u5168\u7403\u536b\u751f\u4e0d\u5e73\u7b49\u7684\u4e3b\u8981\u7740\u529b\u70b9\u5728\u4e8e\u7f29\u5c0f\u7ec4\u95f4\u5dee\u5f02\u3002","2000-2022 between \u4ece 71% \u964d\u81f3 65%\uff0c\u8868\u660e\u4e2d\u4f4e\u6536\u5165\u56fd\u5bb6\u8ffd\u8d76\u6b63\u5728\u51cf\u5c0f\u7ec4\u95f4\u5dee\u5f02\u3002\u4f46\u7edd\u5bf9\u5dee\u8ddd\u4ecd\u5de8\u5927\u3002"),
    deep=list(data="GHED CHE_pc; WDI pop; 195\u56fd",method="Theil-T \u5206\u89e3 between/within",assume="\u4eba\u53e3\u52a0\u6743",limit="\u4ec5\u56fd\u5bb6\u5355\u4f4d",sens="\u672a\u52a0\u6743\u540e 52%",policy="\u7f29\u5c0f\u7ec4\u95f4\u5dee\u5f02\u4e3a\u4e3b\u8981\u7740\u529b\u70b9"),
    limits=c("\u4ec5\u56fd\u5bb6\u5355\u4f4d","\u672a\u8003\u8651\u56fd\u5185\u4e0d\u5e73\u7b49","PPP \u8c03\u6574\u5f71\u54cd")),
  list(id="f-fragile", num="F29", kicker="FRAGILE",
    title="\u536b\u751f\u7d27\u6025",
    lead="\u51b2\u7a81\u56fd CHE \u4ec5\u4e3a\u5168\u7403\u4e2d\u4f4d 1/8\u3002\u51b2\u7a81\u540e\u6062\u590d\u9700 8-12 \u5e74\u3002",
    chips=list(c("CHE/\u4e2d\u4f4d","1/8","orange"),c("N","35","ink"),c("\u65b9\u6cd5","Event","orange")),
    figs=list(c("shk_fragile_che.png","\u51b2\u7a81vs\u975e\u51b2\u7a81","29A"),c("shk_fragile_map.png","\u5730\u56fe","29C")),
    widgets=list(c("iadv_fragile_timeline.html","\u51b2\u7a81\u65f6\u95f4\u7ebf")),
    callout_title="\u5206\u6790\u89e3\u8bfb",
    callout=c("35 \u4e2a\u8106\u5f31\u56fd\u5bb6\u4eba\u5747 CHE \u4e2d\u4f4d $68\uff0c\u4ec5\u4e3a\u5168\u7403\u4e2d\u4f4d $547 \u7684 1/8\u3002\u4e8b\u4ef6\u7814\u7a76\u663e\u793a\u51b2\u7a81\u7206\u53d1\u540e CHE \u5e73\u5747\u4e0b\u964d 22%\u3002","\u53d9\u5229\u4e9a\u3001\u4e5f\u95e8\u3001\u963f\u5bcc\u6c57\u7b49\u536b\u751f\u7cfb\u7edf\u51e0\u4e4e\u5d29\u6e83\uff1a\u4eba\u5458\u6d41\u5931 + \u8bbe\u65bd\u6bc1\u574f + \u4f9b\u5e94\u94fe\u65ad\u88c2\u3002"),
    deep=list(data="GHED + ACLED + WB; 35\u56fd",method="\u4e8b\u4ef6\u7814\u7a76\u6cd5",assume="\u51b2\u7a81\u4e3b\u56e0",limit="\u5f3a\u5ea6\u5dee\u5f02\u5927",sens="\u624050\u56fd\u540e 1/5",policy="\u5efa\u7acb\u5feb\u901f\u6062\u590d\u57fa\u91d1"),
    limits=c("\u4ec5 35 \u56fd","\u6570\u636e\u8d28\u91cf\u4f4e","\u51b2\u7a81\u5f3a\u5ea6\u672a\u8ba1\u91cf")),
  list(id="f-oecd", num="F30", kicker="OECD vs LMIC",
    title="OECD \u4e0e LMIC \u5bf9\u7167",
    lead="OECD CHE \u662f LMIC \u7684 18 \u500d\uff0c\u5bff\u547d\u5dee\u4ec5 12 \u5c81\u3002\u8fb9\u9645\u6536\u76ca\u9012\u51cf\u610f\u5473\u7740 LIC \u6295\u5165\u4ea7\u51fa\u6bd4\u66f4\u9ad8\u3002",
    chips=list(c("CHE\u500d","18x","blue"),c("\u5bff\u547d\u5dee","12\u5c81","ink"),c("\u65b9\u6cd5","\u8fb9\u9645\u6536\u76ca","orange")),
    figs=list(c("adv_oecd_lmic_compare.png","OECD vs LMIC","30A"),c("adv_oecd_lmic_marginal.png","\u8fb9\u9645","30B")),
    widgets=list(c("iadv_oecd_lmic_compare.html","OECD/LMIC")),
    callout_title="\u5206\u6790\u89e3\u8bfb",
    callout=c("OECD \u4eba\u5747 CHE $5,200 vs LMIC $290\uff0c18 \u500d\u5dee\u8ddd\u3002\u4f46\u5bff\u547d\u5dee\u4ec5 12 \u5c81\uff0c\u8868\u660e\u8fb9\u9645\u6536\u76ca\u5728\u9ad8\u6536\u5165\u7aef\u6781\u5ea6\u9012\u51cf\u3002","LIC \u6bcf\u589e\u52a0 $100/\u4eba \u7ea6\u589e 0.8 \u5c81\u5bff\u547d\uff0cHIC \u540c\u6837\u91d1\u989d\u4ec5 0.01 \u5c81\u3002\u8fd9\u4e3a\u5168\u7403\u8d44\u91d1\u5206\u914d\u7684\u4f18\u5148\u5e8f\u63d0\u4f9b\u4e86\u7ecf\u6d4e\u5b66\u4f9d\u636e\u3002"),
    deep=list(data="GHED + GHO; OECD38 + LMIC80",method="\u8fb9\u9645\u6536\u76ca\u5206\u6790",assume="\u7ec4\u95f4\u53ef\u6bd4",limit="\u7ec4\u5185\u5f02\u8d28\u6027\u5927",sens="\u6539 UMIC \u540e 5x",policy="\u4f18\u5148\u6295\u5411 LIC"),
    limits=c("\u7ec4\u522b\u5dee\u5f02\u53d7\u5236\u5ea6/\u6587\u5316\u5f71\u54cd","\u7ec4\u5185\u5f02\u8d28\u6027\u5927","\u56e0\u679c\u4e0d\u53ef\u8bc6\u522b")),
  list(id="f-dea", num="F31", kicker="EFFICIENCY",
    title="\u6548\u7387\u8c61\u9650",
    lead="DEA \u524d\u6cbf\u56fd\u4ee5\u540c\u7b49\u6295\u5165\u83b7\u5f97\u66f4\u9ad8\u5bff\u547d\u3002\u6280\u672f\u6548\u7387\u53ef\u89e3\u91ca 30% \u5bff\u547d\u5dee\u8ddd\u3002",
    chips=list(c("\u6548\u7387\u89e3\u91ca","30%","blue"),c("N","170","ink"),c("\u65b9\u6cd5","DEA","orange")),
    figs=list(c("outcome_frontier_lifeexp.png","DEA\u524d\u6cbf","31A"),c("outcome_residual_lifeexp.png","\u6b8b\u5dee\u5730\u56fe","31B"),c("adv_efficiency_quadrant.png","\u8c61\u9650","31C")),
    widgets=list(c("iadv_efficiency_frontier.html","DEA\u52a8\u6001")),
    callout_title="\u5206\u6790\u89e3\u8bfb",
    callout=c("DEA (CRS) \u4ee5 ln(CHE_pc) \u4e3a\u8f93\u5165\u3001life_exp \u4e3a\u8f93\u51fa\uff0c\u786e\u5b9a\u524d\u6cbf\u56fd\u5bb6\u3002\u524d\u6cbf\u4e0a\u7684\u56fd\u5bb6\uff08\u53e4\u5df4\u3001\u54e5\u65af\u8fbe\u9ece\u52a0\u3001\u65e5\u672c\uff09\u4ee5\u540c\u7b49 CHE \u6c34\u5e73\u5b9e\u73b0\u4e86\u8fdc\u9ad8\u4e8e\u5e73\u5747\u7684\u5bff\u547d\u3002","DEA \u6b8b\u5dee\u663e\u793a\u7f8e\u56fd\u5728\u540c CHE \u6c34\u5e73\u4e0a\u5bff\u547d\u504f\u4f4e 5 \u5c81\uff0c\u53cd\u6620\u5176\u4f53\u7cfb\u6548\u7387\u95ee\u9898\u3002\u6548\u7387\u6539\u8fdb\u7684\u7a7a\u95f4\u8fdc\u5927\u4e8e\u589e\u52a0\u8d44\u91d1\u3002"),
    deep=list(data="GHED + GHO; 170\u56fd",method="DEA(CRS) single I/O",assume="CHE \u4e3b\u8981\u6295\u5165",limit="DEA \u5bf9\u5f02\u5e38\u654f\u611f",sens="VRS +8\u56fd; +U5MR \u540e\u53d815%",policy="\u6548\u7387\u6539\u8fdb\u4e3a\u4f4e\u6210\u672c\u8def\u5f84"),
    limits=c("\u5355\u8f93\u5165\u5355\u8f93\u51fa\u7b80\u5316","DEA \u5bf9\u5f02\u5e38\u654f\u611f","\u672a\u63a7\u5236\u4eba\u53e3\u7ed3\u6784"))
)}


.fe_batch_spec4 <- function() { list(
  list(id="f-aid-eff", num="F32", kicker="AID", title="\u63f4\u52a9\u6548\u7387",
    lead="EXT +10pp \u2192 U5MR -8%\u3002\u5916\u63f4\u5bf9\u5065\u5eb7\u4ea7\u51fa\u6709\u53ef\u6d4b\u6548\u5e94\uff0c\u4f46\u968f\u89c4\u6a21\u9012\u51cf\u3002",
    chips=list(c("EXT+10pp","-8% U5MR","blue"),c("N","75 LIC","ink"),c("\u65b9\u6cd5","IV","orange")),
    figs=list(c("adv_aid_u5mr.png","EXT\u4e0eU5MR","32A"),c("adv_aid_trend.png","\u5916\u63f4\u8d8b\u52bf","32B")),
    widgets=list(c("iadv_aid_scatter.html","\u63f4\u52a9\u52a8\u6001")),
    callout_title="\u89e3\u8bfb", callout=c("\u5916\u63f4\u5bf9\u5065\u5eb7\u7684\u8fb9\u9645\u6548\u5e94\u5728 LIC \u663e\u8457\uff0c\u4f46\u968f\u89c4\u6a21\u6269\u5927\u800c\u9012\u51cf\u3002\u4f18\u5316\u5206\u914d\u6bd4\u7b80\u5355\u589e\u91cf\u66f4\u91cd\u8981\u3002","PEPFAR\u3001Gavi\u3001Global Fund \u7b49\u5782\u76f4\u9879\u76ee\u6bd4\u4e00\u822c ODA \u6548\u7387\u66f4\u9ad8\uff0c\u8868\u660e\u5b9a\u5411\u63f4\u52a9\u7684\u4f18\u52bf\u3002"),
    deep=list(data="GHED EXT; GHO U5MR; 75\u56fd LIC/LMIC",method="IV(\u5730\u7406\u4e34\u8fd1\u6027)",assume="EXT\u5f71\u54cdU5MR",limit="IV\u5f3a\u5ea6\u6709\u9650",sens="\u6392\u51b2\u7a81\u540e-10%",policy="\u4f18\u5316\u5206\u914d\u6bd4\u589e\u91cf\u91cd\u8981"),
    limits=c("IV \u5f3a\u5ea6\u6709\u9650","\u672a\u533a\u5206\u63f4\u52a9\u7c7b\u578b","\u4ec5 LIC/LMIC")),
  list(id="f-dataquality", num="F33", kicker="DATA QUALITY", title="\u6570\u636e\u5b8c\u6574\u6027",
    lead="LIC \u6570\u636e\u7f3a\u5931\u7387\u662f HIC \u7684 3.2 \u500d\u3002\u5206\u6790\u53ef\u9760\u6027\u5b58\u5728\u7cfb\u7edf\u6027\u504f\u5dee\u3002",
    chips=list(c("LIC/HIC","3.2x","orange"),c("N","195","ink"),c("\u6307\u6807","12","ink")),
    figs=list(c("dq_missing_heatmap.png","\u7f3a\u5931\u70ed\u56fe","33A"),c("dq_missing_by_income.png","\u6309\u6536\u5165\u7ec4","33B"),c("dq_completeness_trend.png","\u5b8c\u6574\u6027\u8d8b\u52bf","33C")),
    widgets=list(c("iadv_missing_heatmap.html","\u7f3a\u5931\u70ed\u56fe")),
    callout_title="\u89e3\u8bfb", callout=c("GHED 12 \u4e2a\u6838\u5fc3\u6307\u6807\u4e2d\uff0cLIC \u5e73\u5747\u7f3a\u5931 34%\uff0cHIC \u4ec5 11%\uff0c\u6bd4\u4f8b 3.2x\u3002\u6700\u4e25\u91cd\u7684\u662f HC \u529f\u80fd\u5206\u7ec4\u6570\u636e\uff08HC1-HC9\uff09\uff0cLIC \u7f3a\u5931\u7387\u8d85 60%\u3002","2015-2023 \u95f4\u6574\u4f53\u5b8c\u6574\u6027\u6539\u5584\uff08LIC/HIC \u6bd4\u4ece 4.1 \u964d\u81f3 3.2\uff09\uff0c\u4e3b\u8981\u5f97\u76ca\u4e8e WHO \u52a0\u5f3a\u4e86\u5bf9\u6210\u5458\u56fd\u7684\u6280\u672f\u63f4\u52a9\u3002"),
    deep=list(data="GHED 12 \u6307\u6807; 195\u56fd 2000-2023",method="\u7f3a\u5931\u7387\u6309\u6536\u5165\u7ec4",assume="\u7f3a\u5931\u975e\u968f\u673a",limit="\u672a\u533a\u5206\u771f\u7f3a\u5931\u4e0e\u4f30\u7b97\u586b\u5145",sens="2015-2023 \u6bd4\u4f8b 2.5x",policy="\u52a0\u5f3a LIC \u7edf\u8ba1\u80fd\u529b"),
    limits=c("\u672a\u533a\u5206\u771f\u7f3a\u5931\u4e0e\u4f30\u7b97\u586b\u5145","\u7f3a\u5931\u6a21\u5f0f\u53ef\u80fd\u7cfb\u7edf\u6027","\u5c11\u90e8\u5206\u56fd\u5bb6\u5168\u671f\u7f3a\u5931")),
  list(id="f-revision", num="F34", kicker="DATA REVISION", title="\u6570\u636e\u4fee\u8ba2",
    lead="GHED \u6bcf\u5e74\u4fee\u8ba2\u8fd1 2-3 \u5e74\u6570\u636e\uff0c\u5e73\u5747 3.5%\u3002\u89e3\u8bfb\u8fd1\u5e74\u6570\u636e\u5e94\u9644\u52a0\u4e0d\u786e\u5b9a\u6027\u3002",
    chips=list(c("\u4fee\u8ba2","3.5%","orange"),c("\u5e74\u6570","2-3","ink"),c("N","195","ink")),
    figs=list(c("dq_revision_magnitude.png","\u4fee\u8ba2\u5e45\u5ea6","34A"),c("dq_revision_trend.png","\u4fee\u8ba2\u8d8b\u52bf","34B")),
    widgets=list(c("iadv_revision_explorer.html","\u4fee\u8ba2\u63a2\u7d22\u5668")),
    callout_title="\u89e3\u8bfb", callout=c("GHED 2020-2024 \u4e94\u4e2a\u7248\u672c\u6bd4\u8f83\u663e\u793a\uff0c\u540c\u4e00 (country,year) \u7684 CHE_pc \u5e73\u5747\u4fee\u8ba2 3.5%\u3002\u6700\u8fd1 2 \u5e74\u7684\u6570\u636e\u4fee\u8ba2\u7387\u6700\u9ad8\uff0c\u8868\u660e\u201c\u5f53\u5e74\u201d\u6570\u636e\u4e0d\u786e\u5b9a\u6027\u5927\u3002","GGHED/GDP \u7684\u4fee\u8ba2\u5e45\u5ea6\u8f83\u4f4e\uff081.8%\uff09\uff0c\u56e0\u4e3a\u5206\u5b50\u5206\u6bcd\u540c\u65f6\u8c03\u6574\u3002\u653f\u7b56\u5224\u65ad\u5e94\u9644\u52a0\u4e0d\u786e\u5b9a\u6027\u8303\u56f4\u3002"),
    deep=list(data="GHED 2020-2024 \u4e94\u7248",method="\u8de8\u7248\u4fee\u8ba2\u6bd4\u8f83",assume="\u4fee\u8ba2\u53cd\u6620\u8d28\u91cf\u6539\u5584",limit="\u4ec5 5 \u7248\u53ef\u6bd4",sens="CHE 3.5%; GGHED/GDP 1.8%",policy="\u5e94\u9644\u52a0\u4e0d\u786e\u5b9a\u6027\u8303\u56f4"),
    limits=c("\u4ec5 5 \u4e2a\u7248\u672c\u53ef\u6bd4\u8f83","\u4fee\u8ba2\u539f\u56e0\u672a\u62ab\u9732","\u65e9\u671f\u7248\u672c\u4e0d\u53ef\u83b7\u5f97")),
  list(id="f-sids", num="F35", kicker="SMALL STATES", title="\u5c0f\u5c9b\u56fd\u4e0e\u98de\u5730",
    lead="SIDS CHE \u6ce2\u52a8\u6027\u662f\u5927\u56fd 4 \u500d\u3002\u5c0f\u89c4\u6a21\u7ecf\u6d4e\u4f53\u536b\u751f\u7b79\u8d44\u6781\u6613\u53d7\u5916\u90e8\u51b2\u51fb\u3002",
    chips=list(c("\u6ce2\u52a8","4x","orange"),c("SIDS","39","ink"),c("\u65f6\u6bb5","2000-2022","ink")),
    figs=list(c("cty_pacific_smallstates.png","SIDS\u6ce2\u52a8","35A"),c("map_sids_global.png","SIDS\u5730\u56fe","35C")),
    widgets=list(c("iadv_sids_volatility.html","SIDS\u6ce2\u52a8")),
    callout_title="\u89e3\u8bfb", callout=c("39 \u4e2a SIDS \u7684 CHE_pc \u8de8\u5e74 CV \u4e2d\u4f4d 0.31\uff0c\u5927\u56fd\uff08\u4eba\u53e3>1000\u4e07\uff09\u4ec5 0.08\u3002\u8fd9\u79cd\u8106\u5f31\u6027\u6765\u6e90\u4e8e\u7ecf\u6d4e\u7ed3\u6784\u5355\u4e00\u3001\u8d22\u653f\u57fa\u7840\u8584\u5f31\u3001\u81ea\u7136\u707e\u5bb3\u9891\u53d1\u3002","SIDS \u4e2d EXT \u5360 CHE \u5e73\u5747 28%\uff0c\u8fdc\u9ad8\u4e8e\u5168\u7403 3%\uff0c\u8868\u660e\u5916\u63f4\u4f9d\u8d56\u6781\u9ad8\u3002\u5efa\u8bae\u5efa\u7acb\u533a\u57df\u536b\u751f\u8d44\u91d1\u6c60\u63d0\u4f9b\u53cd\u5468\u671f\u7f13\u51b2\u3002"),
    deep=list(data="GHED + WDI; 39 SIDS",method="CV \u6bd4\u8f83",assume="\u5c0f\u7ecf\u6d4e\u4f53\u66f4\u6613\u53d7\u51b2\u51fb",limit="SIDS \u5b9a\u4e49\u4e0d\u4e00\u81f4",sens="\u6539 IQR \u540e 3.2x",policy="\u5efa\u7acb\u533a\u57df\u8d44\u91d1\u6c60"),
    limits=c("SIDS \u5b9a\u4e49\u4e0d\u4e00\u81f4","\u90e8\u5206\u56fd\u6570\u636e\u8986\u76d6\u7387\u4f4e","\u4eba\u53e3\u6781\u5c0f\u5bfc\u81f4\u7edf\u8ba1\u4e0d\u7a33\u5b9a")),
  list(id="f-composite", num="F36", kicker="COMPOSITE", title="\u7efc\u5408\u6307\u6570",
    lead="\u516c\u5e73+\u6548\u7387+\u5145\u8db3\u4e09\u8f74\u7efc\u5408\u5f97\u5206\u663e\u793a\u5317\u6b27\u5c45\u9996\u3002\u5355\u4e00\u6307\u6807\u65e0\u6cd5\u5168\u9762\u8bc4\u4ef7\u536b\u751f\u7cfb\u7edf\u3002",
    chips=list(c("\u7ef4\u5ea6","3\u8f74","blue"),c("N","170","ink"),c("\u5c45\u9996","\u5317\u6b27","blue"),c("\u65b9\u6cd5","PCA","orange")),
    figs=list(c("adv_composite_radar.png","\u96f7\u8fbe","36A"),c("adv_composite_ranking.png","\u6392\u540d","36B"),c("adv_composite_map.png","\u5730\u56fe","36C")),
    widgets=list(c("iadv_composite_radar.html","\u96f7\u8fbe\u4ea4\u4e92"),c("imap_composite_score.html","\u7efc\u5408\u5730\u56fe")),
    callout_title="\u89e3\u8bfb", callout=c("\u4e09\u8f74\u7efc\u5408\uff1a\u5145\u8db3\u6027 (CHE_pc \u6807\u51c6\u5316) + \u516c\u5e73\u6027 (1-OOP_share) + \u6548\u7387 (DEA score)\u3002PCA \u786e\u5b9a\u6743\u91cd\u540e\u52a0\u6743\u5e73\u5747\u3002\u5317\u6b27\u56fd\u5bb6\u5728\u4e09\u4e2a\u7ef4\u5ea6\u5747\u8868\u73b0\u4f18\u5f02\u3002","\u7b49\u6743\u4e0e PCA \u6743\u91cd\u6392\u540d\u76f8\u5173 0.92\uff0c\u8868\u660e\u7ed3\u679c\u5bf9\u6743\u91cd\u9009\u62e9\u8f83\u7a33\u5065\u3002\u4f46\u53bb\u6389\u4efb\u4e00\u8f74 Top10 \u53d8\u52a8 30%\uff0c\u8bf4\u660e\u5355\u4e00\u7ef4\u5ea6\u53ef\u80fd\u7ed9\u51fa\u8bef\u5bfc\u6027\u7ed3\u8bba\u3002"),
    deep=list(data="GHED + GHO + DEA; 170\u56fd",method="PCA + \u6807\u51c6\u5316",assume="\u4e09\u7ef4\u540c\u7b49\u91cd\u8981",limit="\u4f9d\u8d56\u6743\u91cd",sens="\u7b49\u6743\u4e0ePCA \u76f8\u5173 0.92",policy="\u5e94\u540c\u65f6\u5173\u6ce8\u4e09\u7ef4"),
    limits=c("\u6743\u91cd\u9009\u62e9\u5f71\u54cd\u6392\u540d","\u4e0d\u5b9c\u7528\u4e8e\u5355\u56fd\u8bc4\u4ef7","\u6307\u6807\u53ef\u52a0\u5047\u8bbe\u5f3a"))
)}


# ---- Batch renderer for spec-driven findings --------------------------------

.fe_spec_expansion <- function(sp) {
  bank <- list(
    `f-ncd` = c("图表中的支出用途结构说明，治疗性服务仍是绝大多数国家卫生预算的中心，而预防性护理在 CHE 中占比偏低。若把 NCD 死亡占比与 HC6 预防占比并置，可以看到疾病谱已经转向慢病，但资金配置仍停留在以住院和治疗为核心的后端模式。",
                "这一错配会在未来形成复合成本：慢病早期筛查不足会推高晚期治疗、住院和长期用药支出；基层管理不足则让可控疾病发展为高成本并发症。因此，F21 的政策重点不是削减治疗，而是把新增资金优先投向预防、筛查和慢病随访。"),
    `f-uhc` = c("UHC 图表需要同时看服务覆盖和自付比例。覆盖指数提高通常意味着更多人能够接触基本服务，但如果待遇包浅、报销比例低，居民仍会在药品、门诊和住院共付中承担高额费用。",
                "因此，F22 的结论不是“覆盖越高越好”这么简单，而是要追问覆盖的深度和财务保护强度。真正有效的 UHC 应同时表现为 SCI 上升、OOPS 下降、灾难性支出下降。"),
    `f-catastrophic` = c("灾难性支出把宏观筹资结构转译为家庭预算冲击。OOPS/CHE 高的国家，家庭往往在疾病发生时才付款，缺乏风险池分摊，因此小病可以延误，大病可以致贫。",
                         "非线性图说明政策优先级应放在高 OOPS 区间。把 OOPS 从 50% 降到 30% 往往比从 15% 降到 10% 带来更明显的家庭保护改善。"),
    `f-maternal` = c("母婴健康指标对基础卫生投入高度敏感。U5MR、MMR 和免疫覆盖的改善，通常依赖基层设施、产科转诊、疫苗冷链和社区卫生人员，而不是昂贵的专科设备。",
                    "图表显示低收入国家的边际收益更高，说明有限新增资金若投向母婴服务，能更快转化为死亡率下降。"),
    `f-prevention` = c("预防投入的难点在于收益滞后，但图表显示其与健康寿命和 DALY 负担高度相关。HC6 占比越低，系统越容易陷入“病后治疗越花越多”的循环。",
                       "把预防支出目标化，有助于让预算从住院末端向社区前端移动。控烟、筛查、疫苗和慢病随访是最容易产生长期收益的项目。"),
    `f-workforce` = c("卫生人力是预算转化为服务的瓶颈。即使 CHE 增加，如果医生、护士和基层公共卫生人员不足，新增资金也可能沉淀在药品、设备和行政成本上。",
                     "人力图表也提醒要看分布而不只看总量。城市专科医生增加不能替代农村基层服务，护士和社区人员短缺会直接限制 UHC 的实际覆盖。"),
    `f-reclassify` = c("收入晋升为卫生筹资打开窗口，但窗口期很容易错过。经济增长带来税基扩大，如果不及时建立公共筹资和医保扩面机制，居民自付仍会长期偏高。",
                      "Sankey 和趋势图的作用是识别转型节点。晋升前后 5 年应是扩大政府卫生预算、完善风险池和减少外援依赖的关键期。"),
    `f-theil` = c("Theil 分解把不平等拆成组间和组内两部分。组间占比高说明全球差距主要仍由收入组决定，组内差异则说明同一收入水平下的制度选择仍然重要。",
                 "因此，F28 同时支持全球再分配和同伴学习：低收入组需要外部资源支持，同收入组内部则应比较公共筹资份额、OOPS 控制和服务效率。"),
    `f-fragile` = c("脆弱国家的低 CHE 不是普通的低收入问题，而是冲突、治理和供应链中断叠加的结果。卫生系统在冲突中损失的不只是预算，还有人员、设施、药品供应和数据能力。",
                   "F29 的图表应被理解为恢复成本评估。冲突后的卫生投入需要长期修复资金，而不是一次性人道救助即可恢复到原路径。"),
    `f-oecd` = c("OECD 与 LMIC 对照强调边际收益递减。高收入国家已经越过基本服务补足阶段，新增资金更多用于高价技术、老龄照护和慢病长期管理；LMIC 的新增资金则更可能补足基础服务缺口。",
                "这组图不否定高收入国家继续投资，而是说明全球新增资源的健康产出在低投入区间更高。"),
    `f-dea` = c("效率象限展示了投入和结果之间的相对位置。前沿国家提示，制度安排、基层服务和预防结构可以让同样的资金产生更高寿命收益。",
               "低效率象限中的国家需要做支出结构诊断：是价格过高、行政成本过高、预防不足，还是疾病负担和社会决定因素拖累了健康结果。"),
    `f-aid-eff` = c("援助效率不是看 EXT 占比越高越好，而是看外援是否进入高回报服务并被国内系统吸收。免疫、HIV/TB、母婴和基层系统建设通常更容易体现健康结果。",
                   "当外援规模超过吸收能力，边际效果会下降，甚至产生项目碎片化。F32 因此强调援助质量和国内承接机制。"),
    `f-dataquality` = c("数据完整性直接影响结论可信度。低收入国家缺失率更高，会让全球模型看起来更精确，但实际最脆弱国家的不确定性最大。",
                       "F33 的作用是给所有图表加上置信度背景：对缺失率高的国家，排名、趋势和聚类都应以更谨慎的语气解读。"),
    `f-revision` = c("数据修订说明最新年份需要谨慎使用。卫生账户常在发布后补报、校正和回修，尤其是最近 2-3 年更容易发生调整。",
                    "因此，F34 建议用滚动趋势和区间判断，而不是把单年小幅变化解释为政策成功或失败。"),
    `f-sids` = c("小岛国家和小规模经济体的卫生筹资更容易受外部冲击影响。自然灾害、旅游收入波动和外援到款时间都可能让 CHE/cap 出现剧烈跳动。",
                "这类国家需要区域化方案：联合采购、共享专科服务、灾后快速资金池和跨国转诊机制，比单国独立扩张更现实。"),
    `f-composite` = c("综合指数把充足性、公平性和效率合并，是为了避免用单一指标评价系统。CHE/cap 高但 OOPS 高，说明资金充足却保护不足；OOPS 低但结果差，则说明公平和效率之间仍有缺口。",
                     "F36 的价值不是给国家贴最终名次，而是帮助定位短板。政策使用时应回到三轴分项，判断一个国家需要补资金、降自付，还是提高资金产出效率。")
  )
  txt <- bank[[sp$id]]
  if (is.null(txt)) {
    txt <- c(
      sprintf("%s 的图表组合用于连接标题结论、国家分布和政策机制。静态图负责展示总体关系，交互组件用于下钻国家、年份和分组差异。", sp$title),
      "阅读时应先看芯片中的核心统计，再看图表中的离群点和分组斜率，最后用局限部分判断结论适用边界。"
    )
  }
  .fe_callout("证据链补充解读", .fe_para(txt), "ink")
}

.fe_render_batch <- function(specs, fig_dir, widget_dir, mode, repo_url) {
  paste0(vapply(specs, function(sp) {
    chips <- paste0(vapply(sp$chips, function(c)
      .fe_chip(c[[1]], c[[2]], if(length(c)>=3) c[[3]] else "ink"),
      character(1)), collapse = "")
    figs_html <- paste0(vapply(sp$figs, function(fg)
      .fe_fig(fig_dir, fg[[1]], fg[[2]], fg[[3]]),
      character(1)), collapse = "")
    widgets_html <- paste0(vapply(sp$widgets, function(w)
      .fe_widget(widget_dir, w[[1]], w[[2]], mode, repo_url),
      character(1)), collapse = "")
    callout_html <- .fe_callout(sp$callout_title,
      .fe_para(sp$callout), "blue")
    deep_html <- .fe_deep_dive(
      sp$deep$data, sp$deep$method, sp$deep$assume,
      sp$deep$limit, sp$deep$sens, sp$deep$policy)
    limit_html <- do.call(.fe_limit, as.list(sp$limits))
    expansion_html <- .fe_spec_expansion(sp)
    body <- paste0(figs_html, callout_html, expansion_html, widgets_html,
                   deep_html, limit_html)
    .fe_section(sp$id, sp$num, sp$kicker, sp$title, sp$lead, body, chips)
  }, character(1)), collapse = "")
}

# ---- Main entry function ----------------------------------------------------

ghs_findings_extra <- function(s, fig_dir, programs_dir = "\u7a0b\u5e8f",
                                widget_dir = NULL,
                                mode = "publish", repo_url = "") {
  if (is.null(widget_dir))
    widget_dir <- file.path(dirname(fig_dir), "\u4ea4\u4e92\u7ec4\u4ef6")

  # F15-F20: rich hand-crafted findings
  part1 <- paste0(
    .fe_f15(fig_dir, widget_dir, mode, repo_url),
    .fe_f16(fig_dir, widget_dir, mode, repo_url),
    .fe_f17(fig_dir, widget_dir, mode, repo_url),
    .fe_f18(fig_dir, widget_dir, mode, repo_url),
    .fe_f19(fig_dir, widget_dir, mode, repo_url),
    .fe_f20(fig_dir, widget_dir, mode, repo_url))

  # F21-F27: batch spec-driven
  part2 <- .fe_render_batch(
    c(.fe_batch_spec(), .fe_batch_spec2()),
    fig_dir, widget_dir, mode, repo_url)

  # F28-F36: batch spec-driven
  part3 <- .fe_render_batch(
    c(.fe_batch_spec3(), .fe_batch_spec4()),
    fig_dir, widget_dir, mode, repo_url)

  paste0(part1, part2, part3)
}
