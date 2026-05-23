(function () {
  "use strict";

  document.documentElement.classList.add("ghs-js-ready");

  var revealObserver = null;
  var widgetFrameObserver = null;
  var enhancementTimer = null;
  var revealSelector = [
    ".v3-card",
    ".v3-kpi",
    ".v3-insight",
    ".v3-chart-guide",
    ".widget-workbench-bar",
    ".overview-signal-card",
    ".widget-type-card",
    ".widget-feature-btn",
    ".widget-gallery-card"
  ].join(", ");

  function markWidgetContainers() {
    document.querySelectorAll(".html-widget, .leaflet-container, .rt-table, .dataTables_wrapper")
      .forEach(function (node) {
        node.setAttribute("data-ghs-mounted", "true");
        var panel = node.closest(".v3-card, .card, .bslib-card");
        if (panel) {
          panel.classList.add("has-mounted-widget");
        }
      });
  }

  function loadWidgetFrame(frame) {
    if (!frame || frame.dataset.ghsWidgetLoaded === "true") return;
    var src = frame.getAttribute("data-widget-src");
    if (!src) return;
    frame.dataset.ghsWidgetLoaded = "true";
    var card = frame.closest(".widget-gallery-card, .section-widget-card");
    if (card) {
      card.classList.remove("is-deferred", "is-loaded");
      card.classList.add("is-loading");
    }
    frame.addEventListener("load", function () {
      if (card) {
        card.classList.remove("is-loading");
        card.classList.add("is-loaded");
      }
    }, { once: true });
    frame.setAttribute("src", src);
  }

  function resizeWidgetFrame(frame) {
    if (!frame) return;
    [90, 320, 900].forEach(function (delay) {
      window.setTimeout(function () {
        try {
          if (frame.contentWindow) {
            frame.contentWindow.dispatchEvent(new Event("resize"));
          }
        } catch (error) {
          // Cross-origin widget frames resize with the iframe viewport itself.
        }
        window.dispatchEvent(new Event("resize"));
      }, delay);
    });
  }

  function loadStandaloneFrames(scope) {
    var root = scope || document;
    var frames = root.querySelectorAll("iframe[data-widget-src]:not([src])");

    if (!("IntersectionObserver" in window)) {
      Array.prototype.slice.call(frames, 0, 6).forEach(loadWidgetFrame);
      return;
    }

    if (!widgetFrameObserver) {
      widgetFrameObserver = new IntersectionObserver(function (entries) {
        entries.forEach(function (entry) {
          if (entry.isIntersecting) {
            loadWidgetFrame(entry.target);
            widgetFrameObserver.unobserve(entry.target);
          }
        });
      }, {
        threshold: 0.02,
        rootMargin: "420px 0px"
      });
    }

    frames.forEach(function (frame) {
      if (frame.dataset.ghsWidgetObserved !== "true") {
        frame.dataset.ghsWidgetObserved = "true";
        widgetFrameObserver.observe(frame);
      }
    });
  }

  window.ghsLoadWidgetFrame = loadWidgetFrame;
  window.ghsResizeWidgetFrame = resizeWidgetFrame;
  window.ghsLoadStandaloneFrames = loadStandaloneFrames;

  function refreshActiveStandaloneFrames() {
    var activePane = document.querySelector(".tab-pane.active");
    if (!activePane) return;
    loadStandaloneFrames(activePane);
  }

  function revealPanels() {
    var targets = document.querySelectorAll(revealSelector);

    if (!("IntersectionObserver" in window)) {
      targets.forEach(function (node) {
        node.classList.add("is-visible");
      });
      return;
    }

    if (!revealObserver) {
      revealObserver = new IntersectionObserver(function (entries) {
        entries.forEach(function (entry) {
          if (entry.isIntersecting) {
            entry.target.classList.add("is-visible");
            revealObserver.unobserve(entry.target);
          }
        });
      }, {
        threshold: 0.08,
        rootMargin: "0px 0px -8% 0px"
      });
    }

    targets.forEach(function (node) {
      if (node.dataset.ghsRevealBound !== "true") {
        node.dataset.ghsRevealBound = "true";
        revealObserver.observe(node);
      }
    });
  }

  function applyRevealStagger() {
    document.querySelectorAll(".tab-pane.active .v3-card, .tab-pane.active .v3-kpi, .tab-pane.active .v3-insight, .tab-pane.active .overview-signal-card, .tab-pane.active .widget-type-card, .tab-pane.active .widget-feature-btn, .tab-pane.active .widget-gallery-card")
      .forEach(function (node, index) {
        node.style.setProperty("--ghs-stagger", String(Math.min(index, 10) * 34) + "ms");
      });
  }

  function markScrolledNav() {
    document.documentElement.classList.toggle("ghs-nav-scrolled", window.scrollY > 8);
  }

  function labelIconOnlyButtons() {
    document.querySelectorAll("button:not([aria-label])").forEach(function (button) {
      var text = (button.textContent || "").trim();
      if (text.length > 0 && text.length <= 40) {
        button.setAttribute("aria-label", text);
      }
    });
  }

  function syncWidgetExpandButtons(scope) {
    var root = scope || document;
    root.querySelectorAll(".section-widget-expand-btn").forEach(function (button) {
      var card = button.closest(".section-widget-card");
      var expanded = !!(card && card.classList.contains("is-expanded"));
      button.setAttribute("aria-expanded", expanded ? "true" : "false");
      button.textContent = expanded ? "收起" : "展开阅读";
    });
  }

  function toggleSectionWidget(button) {
    var card = button && button.closest(".section-widget-card");
    if (!card) return;
    var expanded = !card.classList.contains("is-expanded");
    card.classList.toggle("is-expanded", expanded);
    button.setAttribute("aria-expanded", expanded ? "true" : "false");
    button.textContent = expanded ? "收起" : "展开阅读";

    var frame = card.querySelector("iframe[data-widget-src]");
    if (frame) {
      loadWidgetFrame(frame);
      resizeWidgetFrame(frame);
      frame.addEventListener("load", function () {
        resizeWidgetFrame(frame);
      }, { once: true });
    }

    if (expanded) {
      window.setTimeout(function () {
        card.scrollIntoView({ behavior: "smooth", block: "start" });
      }, 80);
    }
  }

  function runEnhancements() {
    markWidgetContainers();
    loadStandaloneFrames();
    revealPanels();
    applyRevealStagger();
    labelIconOnlyButtons();
    syncWidgetExpandButtons();
    markScrolledNav();
  }

  function scheduleEnhancements() {
    window.clearTimeout(enhancementTimer);
    enhancementTimer = window.setTimeout(runEnhancements, 40);
  }

  function scrollTabToTop() {
    window.setTimeout(function () {
      window.scrollTo({ top: 0, behavior: "smooth" });
    }, 90);
  }

  function closeOpenNavigation() {
    document.querySelectorAll(".navbar .dropdown-toggle.show").forEach(function (toggle) {
      if (window.bootstrap && window.bootstrap.Dropdown) {
        var inst = window.bootstrap.Dropdown.getOrCreateInstance(toggle);
        inst.hide();
      } else {
        toggle.classList.remove("show");
        toggle.setAttribute("aria-expanded", "false");
      }
    });

    document.querySelectorAll(".navbar .dropdown-menu.show").forEach(function (menu) {
      menu.classList.remove("show");
    });

    document.querySelectorAll(".navbar .navbar-collapse.show").forEach(function (collapse) {
      if (window.bootstrap && window.bootstrap.Collapse) {
        var inst = window.bootstrap.Collapse.getOrCreateInstance(collapse, { toggle: false });
        inst.hide();
      } else {
        collapse.classList.remove("show");
      }
    });
  }

  document.addEventListener("shiny:busy", function () {
    document.documentElement.classList.add("ghs-shiny-busy");
  });

  document.addEventListener("shiny:idle", function () {
    document.documentElement.classList.remove("ghs-shiny-busy");
  });

  document.addEventListener("DOMContentLoaded", function () {
    runEnhancements();
    if ("MutationObserver" in window && document.body) {
      var observer = new MutationObserver(function (mutations) {
        var shouldRun = mutations.some(function (mutation) {
          return Array.prototype.some.call(mutation.addedNodes, function (node) {
            return node.nodeType === 1 && (
              node.matches && node.matches("iframe[data-widget-src], .widget-gallery-card, .html-widget, .leaflet-container, .rt-table, .dataTables_wrapper") ||
              node.querySelector && node.querySelector("iframe[data-widget-src], .widget-gallery-card, .html-widget, .leaflet-container, .rt-table, .dataTables_wrapper")
            );
          });
        });
        if (shouldRun) scheduleEnhancements();
      });
      observer.observe(document.body, { childList: true, subtree: true });
    }
  });
  window.addEventListener("scroll", markScrolledNav, { passive: true });
  document.addEventListener("shown.bs.tab", function () {
    scrollTabToTop();
    scheduleEnhancements();
    window.setTimeout(refreshActiveStandaloneFrames, 160);
    window.setTimeout(refreshActiveStandaloneFrames, 800);
  });
  document.addEventListener("click", function (event) {
    var insideNavbar = event.target.closest(".navbar, nav.navbar");
    if (!insideNavbar) {
      closeOpenNavigation();
    }

    if (event.target.closest(".navbar .dropdown-item, .navbar .nav-link:not(.dropdown-toggle)")) {
      window.setTimeout(closeOpenNavigation, 80);
    }

    var expandButton = event.target.closest(".section-widget-expand-btn");
    if (expandButton) {
      event.preventDefault();
      toggleSectionWidget(expandButton);
      return;
    }

    if (event.target.closest(".v3-module-card, .module-card")) {
      scrollTabToTop();
    }
  });
  document.addEventListener("keydown", function (event) {
    if (event.key === "Escape") {
      closeOpenNavigation();
    }
  });
  document.addEventListener("shiny:value", function () {
    scheduleEnhancements();
  });
})();
(function () {
  var widgetBase = "https://2711944586.github.io/R/交互组件/";
  var widgetTopics = [
    { keys: ["总览", "overview", "global"], title: "总览地图与全球趋势", files: [["11_world_leaflet.html", "世界地图"], ["01_gapminder_animated.html", "动态气泡"], ["iadv_global_weighted_avg.html", "加权均值"]] },
    { keys: ["国家", "country", "画像", "compare"], title: "国家画像交互组件", files: [["33_country_compare.html", "国家比较"], ["13_highlight_lines.html", "国家轨迹"], ["iadv_dt_master_browse.html", "主表浏览"]] },
    { keys: ["区域", "regional", "continent", "地图", "map"], title: "区域地图与大洲对比", files: [["iadv_heatmap_year_inc.html", "收入组热力"], ["iadv_rt_continent_summary.html", "大洲摘要"], ["iadv_continent_ribbon.html", "大洲带状图"]] },
    { keys: ["筹资", "financing", "资金", "hf"], title: "筹资结构交互组件", files: [["iadv_donut_finance.html", "筹资环图"], ["09_sankey_sources.html", "资金流向"], ["iadv_hf_share_area.html", "份额趋势"]] },
    { keys: ["用途", "purpose", "功能", "hc"], title: "支出功能结构组件", files: [["iadv_sunburst_che.html", "旭日结构"], ["iadv_waterfall_che.html", "瀑布分解"], ["05_ternary.html", "三元结构"]] },
    { keys: ["公平", "equity", "自付", "oop", "inequality"], title: "公平与自付风险组件", files: [["39_oops_dumbbell.html", "OOPS 哑铃"], ["07_inequality.html", "不平等"], ["iadv_rt_oop_extremes.html", "极端国家"]] },
    { keys: ["产出", "outcome", "health", "life", "sdg"], title: "健康产出交互组件", files: [["28_che_life.html", "资金与寿命"], ["29_che_u5.html", "儿童死亡"], ["iadv_lifeexp_byinc.html", "寿命收入组"]] },
    { keys: ["冲击", "shock", "pandemic", "变化", "transition"], title: "冲击与变化组件", files: [["iadv_area_shock_bands.html", "冲击带"], ["43_yoy_heatmap.html", "同比热力"], ["04_bar_race.html", "排行动画"]] },
    { keys: ["预测", "forecast", "scenario", "情景"], title: "预测与情景组件", files: [["06_forecast_subplot.html", "预测面板"], ["13_scenarios.html", "情景路径"], ["iadv_bar_ci.html", "置信区间"]] },
    { keys: ["质量", "方法", "method", "robust", "data"], title: "数据与方法组件", files: [["13_dt_atlas.html", "数据表"], ["31_corr_matrix.html", "相关矩阵"], ["iadv_parcoords.html", "平行坐标"]] }
  ];
  var fallbackWidgets = [
    ["11_world_leaflet.html", "世界地图"],
    ["01_gapminder_animated.html", "动态气泡"],
    ["iadv_dt_master_browse.html", "主表浏览"]
  ];

  function widgetUrl(file) {
    return widgetBase + encodeURIComponent(file);
  }

  function topicFor(text) {
    var hay = (text || "").toLowerCase();
    for (var i = 0; i < widgetTopics.length; i += 1) {
      if (widgetTopics[i].keys.some(function (key) { return hay.indexOf(key.toLowerCase()) >= 0; })) {
        return widgetTopics[i];
      }
    }
    return { title: "本板块精选交互组件", files: fallbackWidgets };
  }

  function panelTitle(panel) {
    var head = panel.querySelector("h1, h2, .card-title, .navbar-title, .page-title");
    return head ? head.textContent.trim() : "";
  }

  function buildModuleStrip(panel) {
    if (!panel || panel.querySelector(".section-widget-strip, .js-module-widget-strip")) return;
    if (panel.matches(".home-page, .home-title-page") || panel.querySelector(".home-title-cover")) return;
    var text = (panelTitle(panel) + " " + (panel.textContent || "")).trim();
    if (text.length < 20 || /首页|home-title|封面/.test(text)) return;
    var topic = topicFor(text);
    var strip = document.createElement("section");
    strip.className = "section-widget-strip js-module-widget-strip";
    strip.style.marginTop = "24px";
    strip.innerHTML = [
      "<div class='section-widget-strip-head'>",
      "<span>Interactive widgets</span>",
      "<strong>" + topic.title + "</strong>",
      "</div>",
      "<div class='section-widget-grid'>",
      topic.files.map(function (item) {
        var url = widgetUrl(item[0]);
        return [
          "<article class='section-widget-card widget-gallery-card is-deferred'>",
          "<header><strong>" + item[1] + "</strong><span class='section-widget-card-actions'><button type='button' class='section-widget-expand-btn' aria-expanded='false'>展开阅读</button><a href='" + url + "' target='_blank' rel='noreferrer'>新窗</a></span></header>",
          "<div class='section-widget-frame widget-gallery-frame'><div class='widget-frame-placeholder'>进入视口后加载线上真实组件。</div><iframe data-widget-src='" + url + "' loading='lazy' referrerpolicy='no-referrer' title='" + item[1] + "'></iframe></div>",
          "</article>"
        ].join("");
      }).join(""),
      "</div>"
    ].join("");
    var anchor = panel.querySelector(".section-head, .page-head, .hero, .bslib-card, .card");
    if (anchor && anchor.parentNode === panel) {
      anchor.insertAdjacentElement("afterend", strip);
    } else {
      panel.insertBefore(strip, panel.firstChild);
    }
  }

  function ensureModuleWidgetStrips() {
    var panels = Array.prototype.slice.call(document.querySelectorAll(".tab-pane, [role='tabpanel']"));
    if (!panels.length) panels = Array.prototype.slice.call(document.querySelectorAll("main > section, .bslib-page"));
    panels.forEach(buildModuleStrip);
  }

  function loadWidgetFrames(scope) {
    if (window.ghsLoadStandaloneFrames) {
      window.ghsLoadStandaloneFrames(scope || document);
    }
  }

  window.ghsLoadWidgetFrames = loadWidgetFrames;

  document.addEventListener("DOMContentLoaded", function () {
    ensureModuleWidgetStrips();
    loadWidgetFrames(document);
  });

  document.addEventListener("shown.bs.tab", function () {
    ensureModuleWidgetStrips();
    loadWidgetFrames(document);
  });

  new MutationObserver(function (mutations) {
    mutations.forEach(function (mutation) {
      mutation.addedNodes.forEach(function (node) {
        if (node.nodeType === 1) {
          if (node.matches && node.matches("iframe[data-widget-src]")) {
            loadWidgetFrames(node.parentNode || document);
          } else if (node.querySelector && node.querySelector("iframe[data-widget-src]")) {
            loadWidgetFrames(node);
          }
        }
      });
    });
    ensureModuleWidgetStrips();
  }).observe(document.documentElement, { childList: true, subtree: true });
})();
