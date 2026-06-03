/* =============================================================================
 * ViewR 1.0.0 -- viewr.js
 * Dependency-free vanilla-JS data explorer htmlwidget.
 * Concept: column statistics are pre-computed in R and shipped as `profile`;
 * this engine focuses purely on rendering, virtualization and interaction.
 * ===========================================================================*/
HTMLWidgets.widget({
  name: "viewdt",
  type: "output",

  factory: function (el, width, height) {

    /* ---- small DOM helpers ---------------------------------------------- */
    function h(tag, cls, html) {
      var e = document.createElement(tag);
      if (cls) e.className = cls;
      if (html != null) e.innerHTML = html;
      return e;
    }
    function esc(s) {
      if (s == null) return "";
      return String(s).replace(/[&<>"']/g, function (c) {
        return { "&": "&amp;", "<": "&lt;", ">": "&gt;",
                 '"': "&quot;", "'": "&#39;" }[c];
      });
    }
    function fmtNum(v) {
      if (v == null || isNaN(v)) return "";
      if (Math.abs(v) >= 1e6 || (Math.abs(v) < 1e-3 && v !== 0))
        return v.toExponential(2);
      return (Math.round(v * 1000) / 1000).toLocaleString();
    }
    function pct(p) { return (Math.round(p * 10) / 10) + "%"; }

    /* ---- per-instance state --------------------------------------------- */
    var S = {
      cols: null,        // {name: [values]}
      names: [],         // all column names
      profile: {},       // name -> profile object
      nrow: 0,
      datasetName: "data",
      opt: {},
      visible: [],       // visible column names (ordered)
      pinnedCols: [],    // (reserved)
      order: [],         // current row index order (post filter+sort)
      sort: [],          // [{col, dir}]
      filters: [],       // [{col, op, value, values}]
      logic: "AND",
      search: "",
      pinnedRows: {},    // rowIndex -> true
      rowH: 30,
      buffer: 6,
      theme: "light"
    };

    /* ---- DOM scaffold ---------------------------------------------------- */
    el.classList.add("viewr-root");
    var toolbar  = h("div", "viewr-toolbar");
    var gridWrap = h("div", "viewr-gridwrap");
    var headerEl = h("div", "viewr-header");
    var bodyEl   = h("div", "viewr-body");
    var spacer   = h("div", "viewr-spacer");
    var rowsEl   = h("div", "viewr-rows");
    bodyEl.appendChild(spacer);
    bodyEl.appendChild(rowsEl);
    gridWrap.appendChild(headerEl);
    gridWrap.appendChild(bodyEl);
    var drawer   = h("div", "viewr-drawer");
    var overlay  = h("div", "viewr-overlay");
    var tooltip  = h("div", "viewr-tooltip");
    el.appendChild(toolbar);
    el.appendChild(gridWrap);
    el.appendChild(drawer);
    el.appendChild(overlay);
    el.appendChild(tooltip);

    overlay.addEventListener("click", closeModals);

    /* ---- theme ---------------------------------------------------------- */
    function resolveTheme(t) {
      if (t === "auto") {
        var dark = window.matchMedia &&
          window.matchMedia("(prefers-color-scheme: dark)").matches;
        // also respect RStudio dark viewer
        if (document.body && /rstudio.*dark|dark/.test(
            (document.body.className || ""))) dark = true;
        return dark ? "dark" : "light";
      }
      return t;
    }

    /* ---- column kind helpers -------------------------------------------- */
    function isNumeric(name) {
      var k = S.profile[name].kind;
      return k === "numeric" || k === "datetime";
    }

    /* =====================================================================
     * TOOLBAR
     * ===================================================================*/
    function buildToolbar() {
      toolbar.innerHTML = "";

      // LEFT: action buttons
      var left = h("div", "viewr-tb-left");
      if (S.opt.query_builder) {
        var fbtn = btn("Filter", function () { openFilterModal(); });
        fbtn.classList.add("ico-filter");
        S.filterBtn = fbtn;
        left.appendChild(fbtn);
      }
      if (S.opt.column_picker) {
        var cbtn = btn("Columns", function () { openColumnModal(); });
        cbtn.classList.add("ico-cols");
        left.appendChild(cbtn);
      }
      if (S.opt.code_export) {
        var kbtn = btn("Code", function () { openCodeModal(); });
        kbtn.classList.add("ico-code");
        left.appendChild(kbtn);
      }
      toolbar.appendChild(left);

      // CENTER: global search
      var center = h("div", "viewr-tb-center");
      if (S.opt.global_search) {
        var wrap = h("div", "viewr-searchwrap");
        wrap.appendChild(h("span", "viewr-search-ico", "⌕"));
        var search = h("input", "viewr-search");
        search.type = "text";
        search.placeholder = "Search all columns…";
        search.addEventListener("input", function () {
          S.search = this.value.toLowerCase();
          recompute(); render();
        });
        wrap.appendChild(search);
        center.appendChild(wrap);
      }
      toolbar.appendChild(center);

      // RIGHT: row & column counts
      var right = h("div", "viewr-tb-right");
      var count = h("span", "viewr-count");
      S.countEl = count;
      right.appendChild(count);
      toolbar.appendChild(right);

      updateCount();
      updateFilterBadge();
    }
    function btn(label, fn) {
      var b = h("button", "viewr-btn", label);
      b.addEventListener("click", fn);
      return b;
    }
    function updateCount() {
      var c = S.countEl;
      if (!c) return;
      var rowsTxt = S.order.length === S.nrow
        ? "<b>" + S.nrow.toLocaleString() + "</b> rows"
        : "<b>" + S.order.length.toLocaleString() + "</b> / " +
          S.nrow.toLocaleString() + " rows";
      var colsTxt = "<b>" + S.visible.length + "</b> / " +
        S.names.length + " cols";
      c.innerHTML = rowsTxt + "<span class='viewr-count-sep'>·</span>" + colsTxt;
    }
    function updateFilterBadge() {
      var b = S.filterBtn;
      if (!b) return;
      var n = S.filters.length;
      b.innerHTML = "Filter" + (n ? " <span class='viewr-badge'>" + n + "</span>" : "");
    }

    /* =====================================================================
     * HEADER (Kaggle-style micro-dashboards)
     * ===================================================================*/
    function buildHeader() {
      headerEl.innerHTML = "";
      var idx = h("div", "viewr-hcell viewr-rowidx-h", "#");
      headerEl.appendChild(idx);

      S.visible.forEach(function (name) {
        var p = S.profile[name];
        var cell = h("div", "viewr-hcell" + (isNumeric(name) ? " num" : ""));
        cell.setAttribute("data-col", name);

        // title row: badge + name + sort + info
        var title = h("div", "viewr-htitle");
        if (S.opt.type_badges)
          title.appendChild(h("span", "viewr-badge-type k-" + p.kind, esc(p.badge)));
        var nameWrap = h("div", "viewr-hname");
        nameWrap.appendChild(h("span", "viewr-colname", esc(name)));
        if (S.opt.show_labels && p.label)
          nameWrap.appendChild(h("span", "viewr-collabel", esc(p.label)));
        title.appendChild(nameWrap);

        var sortBtn = h("span", "viewr-sort", sortGlyph(name));
        sortBtn.addEventListener("click", function (e) {
          e.stopPropagation(); toggleSort(name);
        });
        title.appendChild(sortBtn);

        var info = h("span", "viewr-info", "ⓘ");
        info.addEventListener("mouseenter", function (e) { showTooltip(e, name); });
        info.addEventListener("mouseleave", hideTooltip);
        title.appendChild(info);
        cell.appendChild(title);

        // mini distribution
        if (S.opt.histograms) {
          var mini = miniViz(name);
          if (S.opt.insights)
            mini.addEventListener("click", function () { openInsights(name); });
          cell.appendChild(mini);
        }

        // missingness bar
        if (S.opt.missing_bars) cell.appendChild(missingBar(p));

        headerEl.appendChild(cell);
      });
    }

    function sortGlyph(name) {
      var s = S.sort.filter(function (x) { return x.col === name; })[0];
      if (!s) return "⇅";
      return s.dir === "asc" ? "▲" : "▼";
    }

    function miniViz(name) {
      var p = S.profile[name];
      var box = h("div", "viewr-mini");
      if (isNumeric(name) && p.hist) {
        var max = Math.max.apply(null, p.hist) || 1;
        var bars = h("div", "viewr-spark");
        p.hist.forEach(function (c) {
          var b = h("div", "viewr-spark-bar");
          b.style.height = (4 + 22 * c / max) + "px";
          bars.appendChild(b);
        });
        box.appendChild(bars);
        var lab = h("div", "viewr-mini-lab");
        lab.innerHTML = "<span>" + fmtNum(p.min) + "</span><span>" +
                        fmtNum(p.max) + "</span>";
        box.appendChild(lab);
      } else if (p.top && p.top.length) {
        var total = p.top.reduce(function (a, t) { return a + t.count; }, 0) || 1;
        var bar = h("div", "viewr-catbar");
        var palette = ["#4f8cff", "#36c5a8", "#f5a623", "#d96ad9", "#9aa7b8"];
        p.top.slice(0, 3).forEach(function (t, i) {
          var seg = h("div", "viewr-catseg");
          seg.style.width = (100 * t.count / total) + "%";
          seg.style.background = palette[i];
          seg.title = t.value + " — " + t.count + " (" + t.pct + "%)";
          bar.appendChild(seg);
        });
        box.appendChild(bar);
        var leg = h("div", "viewr-mini-lab");
        leg.innerHTML = p.top.slice(0, 2).map(function (t, i) {
          return "<span><i style='background:" + palette[i] +
            "'></i>" + esc(t.value) + " " + t.pct + "%</span>";
        }).join("");
        box.appendChild(leg);
      }
      return box;
    }

    function missingBar(p) {
      var wrap = h("div", "viewr-missbar");
      var complete = 100 - p.miss_pct;
      var cls = complete > 95 ? "good" : complete >= 50 ? "warn" : "bad";
      var fill = h("div", "viewr-missfill " + cls);
      fill.style.width = complete + "%";
      wrap.appendChild(fill);
      wrap.title = p.n.toLocaleString() + " values • " +
        p.n_miss.toLocaleString() + " missing (" + p.miss_pct + "%)";
      return wrap;
    }

    /* =====================================================================
     * TOOLTIP metadata card
     * ===================================================================*/
    function showTooltip(e, name) {
      var p = S.profile[name];
      var html = "<div class='viewr-tt-title'>" + esc(name) +
        (p.label ? " <small>" + esc(p.label) + "</small>" : "") + "</div>";
      html += "<div class='viewr-tt-grid'>" +
        ttStat("Rows", p.n.toLocaleString()) +
        ttStat("Unique", p.n_unique.toLocaleString()) +
        ttStat("Missing", p.n_miss.toLocaleString() + " (" + p.miss_pct + "%)") +
        ttStat("Type", p.kind) + "</div>";
      if (isNumeric(name) && p.min != null) {
        html += "<div class='viewr-tt-grid'>" +
          ttStat("Min", fmtNum(p.min)) + ttStat("Max", fmtNum(p.max)) +
          ttStat("Mean", fmtNum(p.mean)) + ttStat("Median", fmtNum(p.median)) +
          "</div>";
      } else if (p.top) {
        html += "<table class='viewr-tt-table'><tr><th>Value</th><th>n</th><th>%</th></tr>";
        p.top.slice(0, 5).forEach(function (t) {
          html += "<tr><td>" + esc(t.value) + "</td><td>" + t.count +
            "</td><td>" + t.pct + "%</td></tr>";
        });
        html += "</table>";
      }
      tooltip.innerHTML = html;
      tooltip.style.display = "block";
      var r = e.target.getBoundingClientRect();
      var er = el.getBoundingClientRect();
      tooltip.style.left = Math.min(r.left - er.left, el.clientWidth - 280) + "px";
      tooltip.style.top = (r.bottom - er.top + 6) + "px";
    }
    function ttStat(k, v) {
      return "<div class='viewr-tt-stat'><span>" + k + "</span><b>" + v + "</b></div>";
    }
    function hideTooltip() { tooltip.style.display = "none"; }

    /* =====================================================================
     * DATA INSIGHTS DRAWER
     * ===================================================================*/
    function openInsights(name) {
      var p = S.profile[name];
      drawer.innerHTML = "";
      var head = h("div", "viewr-drawer-head");
      head.innerHTML = "<div><b>" + esc(name) + "</b>" +
        (p.label ? " <span class='viewr-collabel'>" + esc(p.label) + "</span>" : "") +
        "</div>";
      var close = h("button", "viewr-x", "✕");
      close.addEventListener("click", closeDrawer);
      head.appendChild(close);
      drawer.appendChild(head);

      var ov = h("div", "viewr-drawer-sec");
      ov.innerHTML = "<span class='viewr-badge-type k-" + p.kind + "'>" +
        esc(p.badge) + "</span> " + p.kind + " • " +
        p.n_unique.toLocaleString() + " unique values";
      drawer.appendChild(ov);

      // completeness
      var comp = h("div", "viewr-drawer-sec");
      var c = 100 - p.miss_pct;
      var cls = c > 95 ? "good" : c >= 50 ? "warn" : "bad";
      comp.innerHTML = "<div class='viewr-sec-title'>Completeness & Quality</div>" +
        "<div class='viewr-missbar big'><div class='viewr-missfill " + cls +
        "' style='width:" + c + "%'></div></div>" +
        "<div class='viewr-comp-lab'><span>" + (p.n - p.n_miss).toLocaleString() +
        " valid (" + pct(c) + ")</span><span>" + p.n_miss.toLocaleString() +
        " missing (" + p.miss_pct + "%)</span></div>";
      drawer.appendChild(comp);

      if (isNumeric(name) && p.hist) {
        var stat = h("div", "viewr-drawer-sec");
        stat.innerHTML = "<div class='viewr-sec-title'>Descriptive Statistics</div>" +
          "<div class='viewr-statgrid'>" +
          sg("Min", fmtNum(p.min)) + sg("Max", fmtNum(p.max)) +
          sg("Mean", fmtNum(p.mean)) + sg("Median", fmtNum(p.median)) + "</div>";
        drawer.appendChild(stat);
        var dist = h("div", "viewr-drawer-sec");
        dist.innerHTML = "<div class='viewr-sec-title'>Distribution</div>";
        dist.appendChild(svgHistogram(p));
        drawer.appendChild(dist);
      } else if (p.top) {
        var dist2 = h("div", "viewr-drawer-sec");
        dist2.innerHTML = "<div class='viewr-sec-title'>Top categories</div>";
        dist2.appendChild(paretoChart(p));
        drawer.appendChild(dist2);
      }

      drawer.classList.add("open");
      overlay.classList.add("show");
    }
    function sg(k, v) {
      return "<div class='viewr-sg'><span>" + k + "</span><b>" + v + "</b></div>";
    }
    function closeDrawer() {
      drawer.classList.remove("open");
      overlay.classList.remove("show");
    }

    function svgHistogram(p) {
      var W = 320, H = 180, pad = 28;
      var max = Math.max.apply(null, p.hist) || 1;
      var n = p.hist.length;
      var bw = (W - pad - 8) / n;
      var svg = svgEl("svg", { viewBox: "0 0 " + W + " " + H, "class": "viewr-svg" });
      // gridlines
      for (var g = 0; g <= 4; g++) {
        var y = pad / 2 + (H - pad - pad / 2) * g / 4;
        svg.appendChild(svgEl("line", { x1: pad, y1: y, x2: W - 4, y2: y,
          "class": "viewr-grid" }));
      }
      p.hist.forEach(function (c, i) {
        var bh = (H - pad - pad / 2) * c / max;
        var rect = svgEl("rect", {
          x: pad + i * bw + 1, y: H - pad - bh,
          width: Math.max(bw - 2, 1), height: bh, "class": "viewr-hbar"
        });
        var lo = p.breaks ? fmtNum(p.breaks[i]) : "";
        var hi = p.breaks ? fmtNum(p.breaks[i + 1]) : "";
        rect.appendChild(svgEl("title", {}, "[" + lo + ", " + hi + "]: " +
          c + " (" + Math.round(1000 * c / p.n) / 10 + "%)"));
        svg.appendChild(rect);
      });
      svg.appendChild(svgText(pad, H - 6, fmtNum(p.min), "start"));
      svg.appendChild(svgText(W - 4, H - 6, fmtNum(p.max), "end"));
      svg.appendChild(svgText(pad - 4, pad / 2 + 4, String(max), "end", "viewr-axis"));
      return svg;
    }
    function paretoChart(p) {
      var rows = p.top.slice(0, 10);
      var max = rows[0] ? rows[0].count : 1;
      var box = h("div", "viewr-pareto");
      rows.forEach(function (t) {
        var r = h("div", "viewr-pareto-row");
        r.innerHTML =
          "<div class='viewr-pareto-lab' title='" + esc(t.value) + "'>" +
          esc(t.value) + "</div>" +
          "<div class='viewr-pareto-track'><div class='viewr-pareto-fill' style='width:" +
          (100 * t.count / max) + "%'></div></div>" +
          "<div class='viewr-pareto-val'>" + t.count.toLocaleString() +
          " <small>" + t.pct + "%</small></div>";
        box.appendChild(r);
      });
      return box;
    }
    function svgEl(tag, attrs, text) {
      var e = document.createElementNS("http://www.w3.org/2000/svg", tag);
      for (var k in attrs) e.setAttribute(k, attrs[k]);
      if (text != null) e.textContent = text;
      return e;
    }
    function svgText(x, y, t, anchor, cls) {
      return svgEl("text", { x: x, y: y, "text-anchor": anchor,
        "class": cls || "viewr-axis" }, t);
    }

    /* =====================================================================
     * FILTER / QUERY BUILDER
     * ===================================================================*/
    var OPS = {
      numeric:  ["=", "!=", "<", "<=", ">", ">=", "is NA", "not NA"],
      datetime: ["=", "!=", "<", "<=", ">", ">=", "is NA", "not NA"],
      logical:  ["=", "!=", "is NA", "not NA"],
      character:["=", "!=", "contains", "is in", "is not in", "is NA", "not NA"]
    };
    function openFilterModal() {
      var m = modal("Query builder");
      var body = m.body;
      var logicRow = h("div", "viewr-logic");
      logicRow.innerHTML = "Match " +
        "<label><input type='radio' name='vlogic' value='AND'" +
        (S.logic === "AND" ? " checked" : "") + "> ALL (AND)</label> " +
        "<label><input type='radio' name='vlogic' value='OR'" +
        (S.logic === "OR" ? " checked" : "") + "> ANY (OR)</label>";
      body.appendChild(logicRow);

      var list = h("div", "viewr-rules");
      body.appendChild(list);
      var draft = S.filters.map(function (f) { return Object.assign({}, f); });
      if (!draft.length) draft.push(newRule());

      function newRule() {
        return { col: S.visible[0], op: "=", value: "", values: [] };
      }
      function renderRules() {
        list.innerHTML = "";
        draft.forEach(function (r, i) { list.appendChild(ruleRow(r, i)); });
      }
      function ruleRow(r, i) {
        var row = h("div", "viewr-rule");
        var colSel = h("select", "viewr-rule-col");
        S.visible.forEach(function (n) {
          var o = h("option", null, esc(n)); o.value = n;
          if (n === r.col) o.selected = true; colSel.appendChild(o);
        });
        colSel.addEventListener("change", function () {
          r.col = this.value; r.op = OPS[S.profile[r.col].kind][0];
          r.value = ""; r.values = []; renderRules();
        });
        var opSel = h("select", "viewr-rule-op");
        OPS[S.profile[r.col].kind].forEach(function (op) {
          var o = h("option", null, op); o.value = op;
          if (op === r.op) o.selected = true; opSel.appendChild(o);
        });
        opSel.addEventListener("change", function () {
          r.op = this.value; renderRules();
        });
        row.appendChild(colSel); row.appendChild(opSel);

        if (r.op === "is NA" || r.op === "not NA") {
          // no value input
        } else if ((r.op === "is in" || r.op === "is not in")) {
          row.appendChild(multiSelect(r));
        } else {
          var inp = h("input", "viewr-rule-val");
          inp.type = isNumeric(r.col) ? "text" : "text";
          inp.value = r.value;
          inp.placeholder = "value";
          inp.addEventListener("input", function () { r.value = this.value; });
          row.appendChild(inp);
        }
        var del = h("button", "viewr-x small", "✕");
        del.addEventListener("click", function () {
          draft.splice(i, 1); if (!draft.length) draft.push(newRule());
          renderRules();
        });
        row.appendChild(del);
        return row;
      }
      function multiSelect(r) {
        var wrap = h("div", "viewr-multi");
        var p = S.profile[r.col];
        var cats = (p.top || []).map(function (t) { return t.value; });
        var search = h("input", "viewr-multi-search");
        search.placeholder = "search…";
        var listb = h("div", "viewr-multi-list");
        function paint(q) {
          listb.innerHTML = "";
          cats.filter(function (c) {
            return !q || c.toLowerCase().indexOf(q) > -1;
          }).forEach(function (c) {
            var id = "m" + Math.random().toString(36).slice(2);
            var lab = h("label", "viewr-multi-item");
            var cb = h("input"); cb.type = "checkbox"; cb.value = c;
            cb.checked = r.values.indexOf(c) > -1;
            cb.addEventListener("change", function () {
              if (this.checked) r.values.push(c);
              else r.values = r.values.filter(function (v) { return v !== c; });
            });
            lab.appendChild(cb); lab.appendChild(document.createTextNode(" " + c));
            listb.appendChild(lab);
          });
        }
        search.addEventListener("input", function () { paint(this.value.toLowerCase()); });
        paint("");
        wrap.appendChild(search); wrap.appendChild(listb);
        return wrap;
      }
      renderRules();

      var addBtn = btn("+ Add condition", function () {
        draft.push(newRule()); renderRules();
      });
      addBtn.className = "viewr-btn ghost";
      body.appendChild(addBtn);

      m.addAction("Clear all", function () {
        S.filters = []; S.logic = "AND";
        recompute(); render(); closeModals();
      }, "ghost");
      m.addAction("Apply", function () {
        S.logic = (body.querySelector("input[name=vlogic]:checked") || {}).value || "AND";
        S.filters = draft.filter(function (r) {
          return r.op === "is NA" || r.op === "not NA" ||
            (r.values && r.values.length) || r.value !== "";
        });
        recompute(); render(); closeModals();
      }, "primary");
    }

    /* =====================================================================
     * COLUMN PICKER
     * ===================================================================*/
    function openColumnModal() {
      var m = modal("Columns");
      var b = m.body;
      var tools = h("div", "viewr-col-tools");
      tools.appendChild(btn("Select all", function () {
        S.visible = S.names.slice(); paint(); applyCols();
      }));
      tools.appendChild(btn("Clear", function () {
        S.visible = []; paint(); applyCols();
      }));
      b.appendChild(tools);
      var list = h("div", "viewr-col-list");
      b.appendChild(list);
      function paint() {
        list.innerHTML = "";
        S.names.forEach(function (n) {
          var lab = h("label", "viewr-col-item");
          var cb = h("input"); cb.type = "checkbox";
          cb.checked = S.visible.indexOf(n) > -1;
          cb.addEventListener("change", function () {
            if (this.checked) {
              if (S.visible.indexOf(n) < 0) {
                S.visible = S.names.filter(function (x) {
                  return S.visible.indexOf(x) > -1 || x === n;
                });
              }
            } else {
              S.visible = S.visible.filter(function (x) { return x !== n; });
            }
            applyCols();
          });
          lab.appendChild(cb);
          lab.appendChild(h("span", "viewr-badge-type k-" + S.profile[n].kind,
            esc(S.profile[n].badge)));
          lab.appendChild(document.createTextNode(" " + n));
          list.appendChild(lab);
        });
      }
      function applyCols() { buildHeader(); render(); updateCount(); updateFilterBadge(); }
      paint();
    }

    /* =====================================================================
     * CODE GENERATOR
     * ===================================================================*/
    function openCodeModal() {
      var m = modal("Reproducible code");
      var b = m.body;
      var tabs = h("div", "viewr-tabs");
      var pre = h("pre", "viewr-code");
      var langs = ["dplyr", "base R", "SQL"];
      langs.forEach(function (lang, i) {
        var t = h("button", "viewr-tab" + (i === 0 ? " active" : ""), lang);
        t.addEventListener("click", function () {
          tabs.querySelectorAll(".viewr-tab").forEach(function (x) {
            x.classList.remove("active");
          });
          t.classList.add("active");
          pre.textContent = genCode(lang);
        });
        tabs.appendChild(t);
      });
      b.appendChild(tabs);
      pre.textContent = genCode("dplyr");
      b.appendChild(pre);
      m.addAction("Copy", function () {
        navigator.clipboard && navigator.clipboard.writeText(pre.textContent);
      }, "primary");
    }
    function quoteVal(col, v) {
      return isNumeric(col) ? v : '"' + String(v).replace(/"/g, '\\"') + '"';
    }
    function genCode(lang) {
      var d = S.datasetName;
      var allVisible = S.visible.length === S.names.length;
      if (lang === "dplyr") {
        var s = d;
        if (S.filters.length) {
          var conj = S.logic === "AND" ? ", " : " | ";
          var parts = S.filters.map(function (f) { return dplyrCond(f); });
          s += "\n  %>% filter(" +
            (S.logic === "OR" ? parts.join(" | ") : parts.join(", ")) + ")";
        }
        if (S.sort.length)
          s += "\n  %>% arrange(" + S.sort.map(function (o) {
            return o.dir === "desc" ? "desc(" + o.col + ")" : o.col;
          }).join(", ") + ")";
        if (!allVisible)
          s += "\n  %>% select(" + S.visible.join(", ") + ")";
        return s;
      }
      if (lang === "base R") {
        var cond = S.filters.map(function (f) { return baseCond(f, d); });
        var rowExpr = cond.length
          ? cond.join(S.logic === "AND" ? " & " : " | ") : "";
        var colExpr = allVisible ? "" :
          ", c(" + S.visible.map(function (c) { return '"' + c + '"'; }).join(", ") + ")";
        return d + "[" + rowExpr + (colExpr || (rowExpr ? ", " : ", ")) +
          (allVisible ? "" : "") + "]";
      }
      // SQL
      var cols = allVisible ? "*" : S.visible.join(", ");
      var sql = "SELECT " + cols + "\nFROM " + d;
      if (S.filters.length) {
        var w = S.filters.map(function (f) { return sqlCond(f); });
        sql += "\nWHERE " + w.join(S.logic === "AND" ? "\n  AND " : "\n  OR ");
      }
      if (S.sort.length)
        sql += "\nORDER BY " + S.sort.map(function (o) {
          return o.col + " " + o.dir.toUpperCase();
        }).join(", ");
      return sql + ";";
    }
    function dplyrCond(f) {
      var c = f.col;
      switch (f.op) {
        case "is NA":  return "is.na(" + c + ")";
        case "not NA": return "!is.na(" + c + ")";
        case "contains": return "grepl(" + quoteVal(c, f.value) + ", " + c + ")";
        case "is in": return c + " %in% c(" + f.values.map(function (v) {
          return quoteVal(c, v); }).join(", ") + ")";
        case "is not in": return "!" + c + " %in% c(" + f.values.map(function (v) {
          return quoteVal(c, v); }).join(", ") + ")";
        case "=":  return c + " == " + quoteVal(c, f.value);
        case "!=": return c + " != " + quoteVal(c, f.value);
        default:   return c + " " + f.op + " " + quoteVal(c, f.value);
      }
    }
    function baseCond(f, d) {
      var c = d + "$" + f.col;
      switch (f.op) {
        case "is NA":  return "is.na(" + c + ")";
        case "not NA": return "!is.na(" + c + ")";
        case "contains": return "grepl(" + quoteVal(f.col, f.value) + ", " + c + ")";
        case "is in": return c + " %in% c(" + f.values.map(function (v) {
          return quoteVal(f.col, v); }).join(", ") + ")";
        case "is not in": return "!" + c + " %in% c(" + f.values.map(function (v) {
          return quoteVal(f.col, v); }).join(", ") + ")";
        case "=":  return c + " == " + quoteVal(f.col, f.value);
        case "!=": return c + " != " + quoteVal(f.col, f.value);
        default:   return c + " " + f.op + " " + quoteVal(f.col, f.value);
      }
    }
    function sqlCond(f) {
      var c = f.col;
      function q(v) { return isNumeric(c) ? v : "'" + String(v).replace(/'/g, "''") + "'"; }
      switch (f.op) {
        case "is NA":  return c + " IS NULL";
        case "not NA": return c + " IS NOT NULL";
        case "contains": return c + " LIKE '%" + String(f.value).replace(/'/g, "''") + "%'";
        case "is in": return c + " IN (" + f.values.map(q).join(", ") + ")";
        case "is not in": return c + " NOT IN (" + f.values.map(q).join(", ") + ")";
        case "=":  return c + " = " + q(f.value);
        case "!=": return c + " <> " + q(f.value);
        default:   return c + " " + f.op + " " + q(f.value);
      }
    }

    /* =====================================================================
     * MODAL infra
     * ===================================================================*/
    function modal(title) {
      closeModals();
      var box = h("div", "viewr-modal");
      var head = h("div", "viewr-modal-head");
      head.appendChild(h("b", null, title));
      var x = h("button", "viewr-x", "✕");
      x.addEventListener("click", closeModals);
      head.appendChild(x);
      var body = h("div", "viewr-modal-body");
      var foot = h("div", "viewr-modal-foot");
      box.appendChild(head); box.appendChild(body); box.appendChild(foot);
      el.appendChild(box);
      overlay.classList.add("show");
      box._active = true;
      return {
        body: body,
        addAction: function (label, fn, cls) {
          var b = h("button", "viewr-btn " + (cls || ""), label);
          b.addEventListener("click", fn); foot.appendChild(b);
        }
      };
    }
    function closeModals() {
      el.querySelectorAll(".viewr-modal").forEach(function (m) { m.remove(); });
      if (!drawer.classList.contains("open")) overlay.classList.remove("show");
      hideTooltip();
    }

    /* =====================================================================
     * SORT + FILTER engine (operates on row-index order)
     * ===================================================================*/
    function toggleSort(name) {
      var existing = S.sort.filter(function (s) { return s.col === name; })[0];
      if (!existing) S.sort = [{ col: name, dir: "asc" }];
      else if (existing.dir === "asc") existing.dir = "desc";
      else S.sort = [];
      recompute(); buildHeader(); render();
    }
    function passFilter(i) {
      if (!S.filters.length && !S.search) return true;
      // global search
      if (S.search) {
        var hit = false;
        for (var v = 0; v < S.visible.length; v++) {
          var val = S.cols[S.visible[v]][i];
          if (val != null && String(val).toLowerCase().indexOf(S.search) > -1) {
            hit = true; break;
          }
        }
        if (!hit) return false;
      }
      if (!S.filters.length) return true;
      var results = S.filters.map(function (f) { return testCond(f, i); });
      return S.logic === "AND"
        ? results.every(Boolean) : results.some(Boolean);
    }
    function testCond(f, i) {
      var raw = S.cols[f.col][i];
      var num = isNumeric(f.col);
      switch (f.op) {
        case "is NA":  return raw == null;
        case "not NA": return raw != null;
      }
      if (raw == null) return false;
      switch (f.op) {
        case "contains": return String(raw).toLowerCase()
          .indexOf(String(f.value).toLowerCase()) > -1;
        case "is in":     return f.values.indexOf(String(raw)) > -1;
        case "is not in": return f.values.indexOf(String(raw)) < 0;
      }
      var a = num ? +raw : String(raw);
      var b = num ? +f.value : String(f.value);
      switch (f.op) {
        case "=":  return a == b;
        case "!=": return a != b;
        case "<":  return a < b;
        case "<=": return a <= b;
        case ">":  return a > b;
        case ">=": return a >= b;
      }
      return true;
    }
    function recompute() {
      var ord = [];
      for (var i = 0; i < S.nrow; i++) if (passFilter(i)) ord.push(i);
      if (S.sort.length) {
        var s = S.sort[0];
        var col = S.cols[s.col];
        var num = isNumeric(s.col);
        var sign = s.dir === "asc" ? 1 : -1;
        ord.sort(function (x, y) {
          var a = col[x], b = col[y];
          if (a == null) return 1;
          if (b == null) return -1;
          if (num) { a = +a; b = +b; }
          return a < b ? -sign : a > b ? sign : 0;
        });
      }
      S.order = ord;
      updateCount();
    }

    /* =====================================================================
     * VIRTUALIZED BODY
     * ===================================================================*/
    function render() {
      spacer.style.height = (S.order.length * S.rowH) + "px";
      var scrollTop = bodyEl.scrollTop;
      var viewH = bodyEl.clientHeight || 400;
      var first = Math.max(0, Math.floor(scrollTop / S.rowH) - S.buffer);
      var last = Math.min(S.order.length,
        Math.ceil((scrollTop + viewH) / S.rowH) + S.buffer);
      rowsEl.style.transform = "translateY(" + (first * S.rowH) + "px)";
      rowsEl.innerHTML = "";
      var na = esc(S.opt.na_string);
      for (var r = first; r < last; r++) {
        var di = S.order[r];
        var row = h("div", "viewr-row" + (S.pinnedRows[di] ? " pinned" : ""));
        row.style.height = S.rowH + "px";
        var idx = h("div", "viewr-cell viewr-rowidx",
          (S.pinnedRows[di] ? "📌" : (di + 1)));
        (function (di) {
          idx.addEventListener("click", function () {
            if (S.pinnedRows[di]) delete S.pinnedRows[di];
            else S.pinnedRows[di] = true;
            render();
          });
        })(di);
        row.appendChild(idx);
        for (var c = 0; c < S.visible.length; c++) {
          var name = S.visible[c];
          var val = S.cols[name][di];
          var cell = h("div", "viewr-cell" + (isNumeric(name) ? " num" : ""));
          if (val == null) cell.innerHTML = "<span class='viewr-na'>" + na + "</span>";
          else cell.textContent = val;
          row.appendChild(cell);
        }
        rowsEl.appendChild(row);
      }
      syncWidths();
    }

    // keep header + rows column widths aligned
    function syncWidths() {
      // CSS grid handles widths via template; nothing extra needed,
      // but we mirror header template onto rows for perfect alignment.
      var tmpl = "var(--viewr-idxw) " +
        S.visible.map(function () { return "minmax(110px, 1fr)"; }).join(" ");
      headerEl.style.gridTemplateColumns = tmpl;
      rowsEl.querySelectorAll(".viewr-row").forEach(function (rw) {
        rw.style.gridTemplateColumns = tmpl;
      });
    }

    bodyEl.addEventListener("scroll", function () {
      headerEl.scrollLeft = bodyEl.scrollLeft;
      render();
    });

    /* =====================================================================
     * htmlwidgets entry points
     * ===================================================================*/
    return {
      renderValue: function (x) {
        S.cols    = (typeof x.data === "string") ? JSON.parse(x.data) : x.data;
        S.names   = Array.isArray(x.columns) ? x.columns.slice()
                                             : Object.keys(S.cols);
        S.nrow    = x.nrow;
        S.datasetName = x.datasetName || "data";
        S.opt     = x.options || {};
        S.profile = {};
        (x.profile || []).forEach(function (p) { S.profile[p.name] = p; });

        var hidden = S.opt.hidden_columns || [];
        S.visible = S.names.filter(function (n) { return hidden.indexOf(n) < 0; });
        S.theme   = resolveTheme(S.opt.theme || "auto");
        el.setAttribute("data-theme", S.theme);
        el.style.setProperty("--viewr-idxw", "56px");

        buildToolbar();
        buildHeader();
        recompute();
        render();
      },
      resize: function (w, hgt) { render(); }
    };
  }
});
