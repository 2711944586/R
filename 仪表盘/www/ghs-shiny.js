(function () {
  "use strict";

  document.documentElement.classList.add("ghs-js-ready");

  var revealObserver = null;
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

  function loadStandaloneFrames() {
    var frames = document.querySelectorAll("iframe[data-widget-src]");

    function loadFrame(frame) {
      if (!frame || frame.dataset.ghsWidgetLoaded === "true") return;
      var src = frame.getAttribute("data-widget-src");
      if (!src) return;
      frame.dataset.ghsWidgetLoaded = "true";
      var card = frame.closest(".widget-gallery-card");
      if (!frame.getAttribute("src")) {
        frame.setAttribute("src", src);
        if (card) card.classList.add("is-loading");
      }
      frame.addEventListener("load", function () {
        if (card) {
          card.classList.remove("is-loading");
          card.classList.add("is-loaded");
        }
      }, { once: true });
      if (frame.getAttribute("src")) {
        if (card) {
          card.classList.remove("is-loading");
          card.classList.add("is-loaded");
        }
      }
    }

    frames.forEach(loadFrame);
  }

  function refreshActiveStandaloneFrames() {
    var activePane = document.querySelector(".tab-pane.active");
    if (!activePane) return;
    activePane.querySelectorAll(".section-widget-strip iframe[data-widget-src]").forEach(function (frame) {
      var src = frame.getAttribute("data-widget-src");
      if (!src || frame.dataset.ghsActiveRefresh === "true") return;
      frame.dataset.ghsActiveRefresh = "true";
      var card = frame.closest(".widget-gallery-card");
      if (card) {
        card.classList.remove("is-loaded");
        card.classList.add("is-loading");
      }
      frame.addEventListener("load", function () {
        if (card) {
          card.classList.remove("is-loading");
          card.classList.add("is-loaded");
        }
      }, { once: true });
      var sep = src.indexOf("?") >= 0 ? "&" : "?";
      frame.setAttribute("src", src + sep + "ghs_view=" + Date.now());
    });
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

  function runEnhancements() {
    markWidgetContainers();
    loadStandaloneFrames();
    revealPanels();
    applyRevealStagger();
    labelIconOnlyButtons();
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
