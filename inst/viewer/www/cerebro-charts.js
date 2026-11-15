/*
 * Summary charts: stacked bars, a vertical Sankey, violins (one or more
 * panels), lines (filled or open, on a linear or log x axis) and canvas
 * scatter points, drawn from the compact payloads built in
 * plotting_functions.R (and immune_repertoire/visualizations.R). Hover text,
 * tick formatting, legend toggles and downloads all happen here, without a
 * server round trip.
 */
(function (root) {
  'use strict';
  if (!root || root.CerebroCharts) return;

  var FONT = '"Segoe UI Variable", "Segoe UI", -apple-system, BlinkMacSystemFont, ' +
    '"Helvetica Neue", Arial, sans-serif';
  // Same tokens as cerebro_plotly_theme() and the custom.css --c-* palette.
  var FALLBACK = { grid: '#ececec', axis: '#e0e0e0', tick: '#6b6b70', title: '#1c1c1e', surface: '#ffffff' };
  var CSS_VARS = { grid: '--c-border', axis: '--c-border-2', tick: '--c-text-2', title: '--c-text', surface: '--c-surface' };
  var BOX = '#555555';
  var GREY = '#898989';
  var METRICS = {
    groups_by_other_group_plot: 'composition',
    groups_nUMI_plot: 'nUMI',
    groups_nGene_plot: 'nGene',
    groups_percent_mt_plot: 'percent_mt',
    groups_percent_ribo_plot: 'percent_ribo'
  };

  // ---- helpers -------------------------------------------------------------
  function arr(value) {
    if (value == null) return [];
    return Array.isArray(value) ? value : [value];
  }
  function nums(value) {
    return arr(value).map(function (v) { return v == null ? NaN : Number(v); });
  }
  function esc(value) {
    return String(value).replace(/[&<>"']/g, function (c) {
      return { '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' }[c];
    });
  }
  function color(value) {
    value = String(value == null ? '' : value).trim();
    return /^#[0-9a-f]{3,8}$/i.test(value) || /^rgba?\([\d.,\s%]+\)$/i.test(value) ||
      /^[a-z]{3,20}$/i.test(value) ? value : GREY;
  }
  function clip(label, max) {
    label = String(label);
    return label.length > max ? label.slice(0, max - 1) + '…' : label;
  }
  function px(value) {
    return Math.round(value * 100) / 100;
  }
  function maxOf(values, fallback) {
    var out = -Infinity;
    for (var i = 0; i < values.length; i++) if (values[i] > out) out = values[i];
    return out === -Infinity ? fallback : out;
  }
  function minOf(values, fallback) {
    var out = Infinity;
    for (var i = 0; i < values.length; i++) if (values[i] < out) out = values[i];
    return out === Infinity ? fallback : out;
  }
  function add(a, b) { return a + b; }

  // base64 little-endian Float32 / Int32 columns (cerebroChartBinary()).
  function decode(text, Type) {
    if (typeof text !== 'string' || !text.length) return new Type(0);
    var binary = root.atob(text), bytes = new Uint8Array(binary.length);
    for (var i = 0; i < binary.length; i++) bytes[i] = binary.charCodeAt(i);
    return new Type(bytes.buffer, 0, Math.floor(bytes.length / 4));
  }

  function theme() {
    var out = { font: FONT }, style = null;
    try {
      if (root.getComputedStyle && root.document && root.document.documentElement) {
        style = root.getComputedStyle(root.document.documentElement);
      }
    } catch (ignore) {}
    Object.keys(FALLBACK).forEach(function (key) {
      var value = style ? String(style.getPropertyValue(CSS_VARS[key]) || '').trim() : '';
      out[key] = value ? color(value) : FALLBACK[key];
    });
    return out;
  }

  var measureContext = null;
  function textWidth(text, size) {
    text = String(text);
    size = size || 12;
    if (measureContext === null) {
      measureContext = false;
      try {
        var canvas = root.document && root.document.createElement('canvas');
        measureContext = canvas && canvas.getContext && canvas.getContext('2d') || false;
      } catch (ignore) {}
    }
    if (measureContext) {
      measureContext.font = size + 'px ' + FONT;
      return measureContext.measureText(text).width;
    }
    return text.length * size * 0.56;
  }

  // ---- number formatting (d3-style ",.0f", ",.0%", ",.1%") -----------------
  function fixed(value, digits) {
    if (!isFinite(value)) return String(value);
    var text = Math.abs(value).toFixed(digits), parts = text.split('.');
    parts[0] = parts[0].replace(/\B(?=(\d{3})+(?!\d))/g, ',');
    return (value < 0 && Number(text) !== 0 ? '-' : '') + parts.join('.');
  }
  // Three significant digits, thousands separated from 1,000 on.
  function significant(value) {
    if (!isFinite(value)) return String(value);
    if (Math.abs(value) >= 1000) return fixed(value, 0);
    return String(Number(Number(value).toPrecision(3)));
  }
  function formatValue(value, percent, digits) {
    return percent ? fixed(value * 100, digits) + '%' : fixed(value, digits);
  }
  // Hover values: "percent" (0-1) with one decimal, "integer", or "number".
  function formatMode(value, mode) {
    if (mode === 'percent') return fixed(value * 100, 1) + '%';
    if (mode === 'integer') return fixed(value, 0);
    return significant(value);
  }
  function tickDigits(step, percent) {
    var scaled = percent ? step * 100 : step;
    return scaled >= 1 ? 0 : Math.min(6, Math.ceil(-Math.log(scaled) / Math.LN10 - 1e-9));
  }

  function niceStep(span, count) {
    var raw = span / Math.max(1, count);
    var power = Math.pow(10, Math.floor(Math.log(raw) / Math.LN10));
    var unit = raw / power;
    return (unit >= 7.5 ? 10 : unit >= 3.5 ? 5 : unit >= 1.5 ? 2 : 1) * power;
  }
  function ticks(lo, hi, count, minStep) {
    if (!(hi > lo)) hi = lo + 1;
    var step = Math.max(niceStep(hi - lo, count), minStep || 0);
    var out = [], start = Math.ceil(lo / step - 1e-9) * step;
    for (var v = start; v <= hi + step * 1e-9 && out.length < 50; v += step) {
      out.push(Number(v.toPrecision(12)));
    }
    return { values: out, step: step };
  }
  // Ticks of a log10 axis, positions in log10 units from `lo` to `hi`: 1-9,
  // 1-2-5 or 1 per decade, whichever has at most `count`, else every k-th
  // power of ten.
  function logTicks(lo, hi, count) {
    var first = Math.floor(lo + 1e-9), last = Math.ceil(hi - 1e-9), out;
    var sets = [[1, 2, 3, 4, 5, 6, 7, 8, 9], [1, 2, 5], [1]];
    for (var s = 0; s < sets.length; s++) {
      out = [];
      for (var d = first; d <= last; d++) {
        for (var m = 0; m < sets[s].length; m++) {
          var v = d + Math.log(sets[s][m]) / Math.LN10;
          if (v >= lo - 1e-9 && v <= hi + 1e-9) out.push(v);
        }
      }
      if (out.length <= count) return { values: out, step: 1 };
    }
    var every = Math.ceil((last - first) / Math.max(1, count));
    out = [];
    // `+ 0` turns the -0 that Math.ceil() gives just below zero into 0.
    for (d = Math.ceil(lo / every - 1e-9) * every + 0; d <= hi + 1e-9; d += every) out.push(d);
    return { values: out, step: every };
  }
  function logLabel(v) {
    return significant(Number(Math.pow(10, v).toPrecision(12)));
  }
  // Hover values: "integer", "number" (three significant digits), "percent",
  // or a number of decimals.
  function formatAs(value, format) {
    return typeof format === 'number' ? fixed(value, format) : formatMode(value, format);
  }

  // ---- shared SVG pieces ---------------------------------------------------
  function svgOpen(width, height, th, background) {
    return '<svg xmlns="http://www.w3.org/2000/svg" class="cc-svg" width="' + px(width) +
      '" height="' + px(height) + '" viewBox="0 0 ' + px(width) + ' ' + px(height) +
      '" font-family="' + esc(th.font) + '" font-size="12">' +
      (background ? '<rect width="100%" height="100%" fill="' + th.surface + '"/>' : '');
  }
  function line(x1, y1, x2, y2, stroke, extra) {
    return '<line x1="' + px(x1) + '" y1="' + px(y1) + '" x2="' + px(x2) + '" y2="' + px(y2) +
      '" stroke="' + stroke + '"' + (extra || '') + '/>';
  }
  function text(x, y, content, attributes) {
    return '<text x="' + px(x) + '" y="' + px(y) + '"' + (attributes || '') + '>' + esc(content) + '</text>';
  }
  function swatch(fill) {
    return '<span class="cc-tip-swatch" style="background:' + color(fill) + '"></span>';
  }
  function hitRect(index, x, y, width, height) {
    return '<rect class="cc-hit" data-tip="' + index + '" x="' + px(x) + '" y="' + px(y) + '" width="' +
      px(width) + '" height="' + px(height) + '" fill="transparent"/>';
  }

  // Y axis with grid lines; returns the scale and the drawn markup.
  function valueAxis(spec, th) {
    var span = spec.hi - spec.lo;
    var y = function (v) { return spec.top + (spec.hi - v) / span * spec.height; };
    var out = [];
    spec.ticks.values.forEach(function (v) {
      var yy = y(v);
      out.push(line(spec.left, yy, spec.left + spec.width, yy, th.grid, ' stroke-width="1"'));
      if (spec.labels) {
        out.push(text(spec.left - 6, yy + 4, spec.labels(v), ' text-anchor="end" fill="' + th.tick + '"'));
      }
    });
    out.push(line(spec.left, spec.top, spec.left, spec.top + spec.height, th.axis));
    out.push(line(spec.left, spec.top + spec.height, spec.left + spec.width, spec.top + spec.height, th.axis));
    if (spec.title) {
      var cx = spec.titleX == null ? 14 : spec.titleX, cy = spec.top + spec.height / 2;
      out.push(text(cx, cy, spec.title, ' text-anchor="middle" font-size="13" fill="' + th.title +
        '" transform="rotate(-90 ' + px(cx) + ' ' + px(cy) + ')"'));
    }
    return { y: y, markup: out.join('') };
  }

  // Numeric x axis under a plot area; returns the scale and the drawn markup.
  function numberAxis(spec, th) {
    var span = spec.hi - spec.lo;
    var x = function (v) { return spec.left + (v - spec.lo) / span * spec.width; };
    var digits = tickDigits(spec.ticks.step, false);
    var label = spec.labels || function (v) { return fixed(v, digits); };
    var out = [], base = spec.top + spec.height;
    spec.ticks.values.forEach(function (v) {
      var xx = x(v);
      out.push(line(xx, spec.top, xx, base, th.grid, ' stroke-width="1"'));
      out.push(text(xx, base + 16, label(v), ' text-anchor="middle" fill="' + th.tick + '"'));
    });
    if (spec.title) {
      out.push(text(spec.left + spec.width / 2, base + 34, spec.title, ' text-anchor="middle" font-size="13" fill="' +
        th.title + '"'));
    }
    return { x: x, markup: out.join('') };
  }

  // Category axis labels, rotated when they do not fit their band.
  function categoryLayout(labels, band) {
    var shown = labels.map(function (label) { return clip(label, 24); });
    var widest = maxOf(shown.map(function (label) { return textWidth(label); }), 0);
    var rotate = widest + 6 > band;
    return { shown: shown, rotate: rotate, height: rotate ? widest * 0.71 + 18 : 24 };
  }
  function categoryLabels(layout, x, baseline, th) {
    return layout.shown.map(function (label, j) {
      if (!layout.rotate) {
        return text(x(j), baseline + 16, label, ' text-anchor="middle" fill="' + th.tick + '"');
      }
      var cx = x(j) + 4, cy = baseline + 10;
      return text(cx, cy, label, ' text-anchor="end" fill="' + th.tick +
        '" transform="rotate(-45 ' + px(cx) + ' ' + px(cy) + ')"');
    }).join('');
  }

  // Plot frame shared by bars and violins inside `box` ({x, y, width, height}):
  // margins from tick and label sizes, plus room for an x title below.
  function frame(box, domain, labels, percent, integer, xTitle, axisLabels) {
    var estimate = ticks(domain[0], domain[1], Math.max(2, Math.round((box.height - 60) / 55)), integer ? 1 : 0);
    var digits = tickDigits(estimate.step, percent);
    var format = function (v) { return formatValue(v, percent, digits); };
    var left = box.x + 34 + (axisLabels === false ? -8 :
      maxOf(estimate.values.map(function (v) { return textWidth(format(v)); }), 10));
    var plotWidth = Math.max(10, box.x + box.width - left - 12);
    var band = plotWidth / Math.max(1, labels.length);
    var axis = categoryLayout(labels, band);
    var bottom = Math.min(box.height * 0.45, axis.height) + (xTitle ? 20 : 0);
    var top = box.y + 12;
    return {
      left: left, top: top, width: plotWidth, height: Math.max(10, box.y + box.height - top - bottom),
      band: band, axis: axis, ticks: estimate, format: format, titleX: box.x + 14,
      x: function (j) { return left + band * (j + 0.5); }
    };
  }

  // ---- bar chart -----------------------------------------------------------
  function barSvg(model, width, height, hidden, th, tips, exporting) {
    var categories = model.categories, series = model.series, percent = model.percent;
    var visible = series.map(function (_, i) { return !hidden[i]; });
    var totals = categories.map(function (_, j) {
      var sum = 0;
      for (var i = 0; i < series.length; i++) if (visible[i]) sum += model.values[i][j] || 0;
      return sum;
    });
    var hi = percent ? 1 : maxOf(totals, 0) * 1.05;
    if (!(hi > 0)) hi = 1;
    var integer = !percent && model.format === 'integer';
    var f = frame({ x: 0, y: 0, width: width, height: height }, [0, hi], categories, percent, integer,
      model.xTitle);
    var axis = valueAxis({
      lo: 0, hi: hi, top: f.top, height: f.height, left: f.left, width: f.width,
      ticks: f.ticks, labels: f.format, title: model.yTitle
    }, th);
    var out = [svgOpen(width, height, th, exporting), axis.markup];
    var barWidth = Math.max(1, f.band * 0.8);
    var outline = model.outline ? ' stroke="' + model.outline + '" stroke-width="' + model.outlineWidth + '"' : '';
    categories.forEach(function (category, j) {
      var cumulative = 0, x0 = f.x(j) - barWidth / 2;
      var rows = [];
      for (var i = 0; i < series.length; i++) {
        var value = model.values[i][j] || 0;
        if (!visible[i] || !(value > 0)) continue;
        var fill = model.categoryColors ? model.categoryColors[j] : model.colors[i];
        var y1 = axis.y(cumulative), y0 = axis.y(cumulative + value);
        out.push('<rect x="' + px(x0) + '" y="' + px(y0) + '" width="' + px(barWidth) + '" height="' +
          px(Math.max(0, y1 - y0)) + '" fill="' + fill + '"' + outline + '/>');
        cumulative += value;
        rows.push(swatch(fill) + '<b>' + esc(model.categoryColors ? category : series[i]) + ':</b> ' +
          formatMode(value, percent ? 'percent' : model.format) +
          (model.counts ? ' (' + fixed(model.counts[i][j] || 0, 0) + ')' : ''));
      }
      if (!exporting) {
        tips.push(model.categoryColors ? (rows[0] || '<b>' + esc(category) + ':</b> 0') :
          '<div class="cc-tip-title">' + esc(category) + '</div>' + rows.reverse().join('<br>'));
        out.push(hitRect(tips.length - 1, f.x(j) - f.band / 2, f.top, f.band, f.height));
      }
    });
    out.push(categoryLabels(f.axis, f.x, f.top + f.height, th));
    if (model.xTitle) {
      out.push(text(f.left + f.width / 2, height - 6, model.xTitle,
        ' text-anchor="middle" font-size="13" fill="' + th.title + '"'));
    }
    out.push('</svg>');
    return out.join('');
  }

  function legendHtml(model, hidden) {
    var items = model.series.map(function (label, i) {
      var off = !!hidden[i];
      return '<button type="button" class="cc-legend-item' + (off ? ' is-hidden' : '') +
        '" data-series="' + i + '" aria-pressed="' + (off ? 'false' : 'true') + '" title="' + esc(label) +
        '"><span class="cc-legend-swatch" style="background:' + model.colors[i] + '"></span>' +
        '<span class="cc-legend-label">' + esc(label) + '</span></button>';
    });
    return '<div class="cc-legend" role="group" aria-label="' + esc(model.legendTitle || 'Legend') + '">' +
      (model.legendTitle ? '<div class="cc-legend-title">' + esc(model.legendTitle) + '</div>' : '') +
      items.join('') + '</div>';
  }

  // Legend drawn into exported images, one row per series.
  function legendSvg(model, hidden, x, th) {
    var out = [], y = 18;
    if (model.legendTitle) {
      out.push(text(x, y, model.legendTitle, ' font-weight="600" fill="' + th.title + '"'));
      y += 20;
    }
    model.series.forEach(function (label, i) {
      out.push('<rect x="' + px(x) + '" y="' + px(y - 10) + '" width="12" height="12" rx="2" fill="' +
        model.colors[i] + '"' + (hidden[i] ? ' fill-opacity="0.25"' : '') + '/>');
      out.push(text(x + 18, y, clip(label, 32), ' fill="' + (hidden[i] ? th.tick : th.title) + '"'));
      y += 18;
    });
    var width = 26 + maxOf(model.series.map(function (label) { return textWidth(clip(label, 32)); }), 0);
    return { markup: out.join(''), width: Math.max(width, model.legendTitle ? textWidth(model.legendTitle) : 0) + 12, height: y };
  }
  function hasLegend(model) {
    return (model.type === 'bar' && !model.categoryColors) || model.type === 'lines' ||
      (model.type === 'scatter' && model.series.length > 0);
  }

  // ---- violins -------------------------------------------------------------
  function violinTip(panel, mode, i) {
    var f = function (v) { return formatMode(v, mode); };
    var rows = [
      ['n', fixed(panel.n[i], 0)], ['max', f(panel.maximum[i])], ['upper fence', f(panel.upperfence[i])],
      ['q3', f(panel.q3[i])], ['mean', f(panel.mean[i])], ['median', f(panel.median[i])],
      ['q1', f(panel.q1[i])], ['lower fence', f(panel.lowerfence[i])], ['min', f(panel.minimum[i])]
    ].map(function (row) { return '<b>' + row[0] + ':</b> ' + row[1]; });
    if (panel.approximate[i]) rows.push('<i>density: quantile approximation</i>');
    return '<div class="cc-tip-title">' + swatch(panel.colors[i]) + esc(panel.groups[i]) +
      (panel.title ? ' · ' + esc(panel.title) : '') + '</div>' + rows.join('<br>');
  }

  function violinPanel(panel, box, lo, hi, model, th, tips, exporting, first) {
    var percent = model.percent, groups = panel.groups;
    var integer = model.mode === 'integer' && hi - lo > 4;
    var f = frame(box, [lo, hi], groups, percent, integer, panel.title, first);
    var axis = valueAxis({
      lo: lo, hi: hi, top: f.top, height: f.height, left: f.left, width: f.width, titleX: f.titleX,
      ticks: f.ticks, labels: first ? f.format : null, title: first ? model.yTitle : ''
    }, th);
    var out = [axis.markup];
    var half = Math.min(f.band * 0.42, 90), boxHalf = Math.max(2, Math.min(f.band * 0.06, 12));
    groups.forEach(function (group, i) {
      var cx = f.x(i), fill = panel.colors[i], violin = panel.violins[i] || {};
      var density = nums(violin.density), vlo = Number(violin.lo), vhi = Number(violin.hi);
      if (density.length > 1) {
        var right = [], left = [], count = density.length;
        for (var k = 0; k < count; k++) {
          var yy = px(axis.y(vlo + (vhi - vlo) * k / (count - 1))), w = (density[k] || 0) * half;
          right.push(px(cx + w) + ',' + yy);
          left.unshift(px(cx - w) + ',' + yy);
        }
        out.push('<path d="M' + right.join('L') + 'L' + left.join('L') + 'Z" fill="' + fill +
          '" fill-opacity="0.5" stroke="' + fill + '" stroke-width="1.5" stroke-linejoin="round"/>');
      } else if (isFinite(vlo)) {
        out.push(line(cx - half, axis.y(vlo), cx + half, axis.y(vlo), fill, ' stroke-width="2"'));
      }
      var q1 = axis.y(panel.q1[i]), q3 = axis.y(panel.q3[i]);
      var upper = axis.y(panel.upperfence[i]), lower = axis.y(panel.lowerfence[i]);
      if ([q1, q3, upper, lower].every(isFinite)) {
        out.push(line(cx, q3, cx, upper, BOX), line(cx, q1, cx, lower, BOX));
        out.push(line(cx - boxHalf / 2, upper, cx + boxHalf / 2, upper, BOX));
        out.push(line(cx - boxHalf / 2, lower, cx + boxHalf / 2, lower, BOX));
        out.push('<rect x="' + px(cx - boxHalf) + '" y="' + px(q3) + '" width="' + px(boxHalf * 2) +
          '" height="' + px(Math.max(0.5, q1 - q3)) + '" fill="rgba(255,255,255,0.5)" stroke="' + BOX + '"/>');
        out.push(line(cx - boxHalf, axis.y(panel.median[i]), cx + boxHalf, axis.y(panel.median[i]), BOX,
          ' stroke-width="2"'));
        if (isFinite(panel.mean[i])) {
          out.push(line(cx - boxHalf, axis.y(panel.mean[i]), cx + boxHalf, axis.y(panel.mean[i]), BOX,
            ' stroke-dasharray="3,2"'));
        }
      }
      if (!exporting) {
        tips.push(violinTip(panel, model.mode, i));
        out.push(hitRect(tips.length - 1, cx - f.band / 2, f.top, f.band, f.height));
      }
    });
    out.push(categoryLabels(f.axis, f.x, f.top + f.height, th));
    if (panel.title) {
      out.push(text(f.left + f.width / 2, box.y + box.height - 6, panel.title,
        ' text-anchor="middle" font-size="13" fill="' + th.title + '"'));
    }
    return out.join('');
  }

  function violinSvg(model, width, height, th, tips, exporting) {
    var panels = model.panels, lo, hi;
    if (model.percent) {
      lo = 0; hi = 1;
    } else {
      var mins = [], maxs = [];
      panels.forEach(function (panel) {
        mins = mins.concat(panel.minimum.filter(isFinite));
        maxs = maxs.concat(panel.maximum.filter(isFinite));
      });
      lo = minOf(mins, NaN); hi = maxOf(maxs, NaN);
      if (!isFinite(lo) || !isFinite(hi)) { lo = 0; hi = 1; }
      var pad = hi > lo ? (hi - lo) * 0.05 : 1;
      lo -= pad; hi += pad;
    }
    var columns = Math.min(3, panels.length), rows = Math.ceil(panels.length / columns);
    var out = [svgOpen(width, height, th, exporting)];
    var cellWidth = width / columns, cellHeight = height / rows;
    panels.forEach(function (panel, p) {
      var box = { x: (p % columns) * cellWidth, y: Math.floor(p / columns) * cellHeight,
        width: cellWidth, height: cellHeight };
      out.push(violinPanel(panel, box, lo, hi, model, th, tips, exporting, p % columns === 0));
    });
    out.push('</svg>');
    return out.join('');
  }

  // ---- filled lines --------------------------------------------------------
  // `xLog` puts x on a log10 axis (points at x <= 0 are left out); `fill`
  // false draws open lines. `hoverMode` "closest" shows the point nearest the
  // pointer, else every series at the pointer's x.
  function linesSvg(model, width, height, hidden, th, tips, exporting, extras) {
    var tx = model.xLog ? function (v) { return v > 0 ? Math.log(v) / Math.LN10 : NaN; } :
      function (v) { return v; };
    var xs = [], ys = [];
    model.x.forEach(function (x, i) {
      if (hidden[i]) return;
      var t = x.map(tx).filter(isFinite);
      xs.push(minOf(t, NaN), maxOf(t, NaN));
      ys.push(maxOf(model.y[i], 0));
    });
    var xlo = model.xRange ? tx(model.xRange[0]) : minOf(xs.filter(isFinite), 0);
    var xhi = model.xRange ? tx(model.xRange[1]) : maxOf(xs.filter(isFinite), 1);
    if (!isFinite(xlo)) xlo = 0;
    if (!(xhi > xlo)) xhi = xlo + 1;
    var yhi = maxOf(ys, 0) * 1.05;
    if (!(yhi > 0)) yhi = 1;
    var yt = ticks(0, yhi, Math.max(2, Math.round((height - 70) / 55)));
    var ydigits = tickDigits(yt.step, false);
    var yformat = function (v) { return fixed(v, ydigits); };
    var left = 34 + maxOf(yt.values.map(function (v) { return textWidth(yformat(v)); }), 10);
    var top = 12, plotWidth = Math.max(10, width - left - 16), plotHeight = Math.max(10, height - top - 46);
    var yaxis = valueAxis({ lo: 0, hi: yhi, top: top, height: plotHeight, left: left, width: plotWidth,
      ticks: yt, labels: yformat, title: model.yTitle }, th);
    var xcount = Math.max(2, Math.round(plotWidth / 90));
    var xaxis = numberAxis({ lo: xlo, hi: xhi, top: top, height: plotHeight, left: left, width: plotWidth,
      ticks: model.xLog ? logTicks(xlo, xhi, xcount) : ticks(xlo, xhi, xcount),
      labels: model.xLog ? logLabel : null, title: model.xTitle }, th);
    var out = [svgOpen(width, height, th, exporting), yaxis.markup, xaxis.markup];
    var base = px(yaxis.y(0));
    // Screen positions of the drawn points, kept for the closest-point hover.
    var screen = [];
    model.x.forEach(function (x, i) {
      if (hidden[i]) return;
      var y = model.y[i], sx = [], sy = [], index = [];
      for (var k = 0; k < x.length; k++) {
        var xx = xaxis.x(tx(x[k])), yy = yaxis.y(y[k]);
        if (!isFinite(xx) || !isFinite(yy)) continue;
        sx.push(xx); sy.push(yy); index.push(k);
      }
      screen.push({ series: i, x: sx, y: sy, index: index });
      if (sx.length < 2) return;
      var points = sx.map(function (xx, k) { return px(xx) + ',' + px(sy[k]); });
      var stroke = model.lineColor || model.colors[i];
      out.push(model.fill ?
        '<path d="M' + px(sx[0]) + ',' + base + 'L' + points.join('L') + 'L' + px(sx[sx.length - 1]) + ',' +
          base + 'Z" fill="' + model.colors[i] + '" fill-opacity="' + model.fillOpacity + '" stroke="' + stroke +
          '" stroke-width="' + model.lineWidth + '"/>' :
        '<path d="M' + points.join('L') + '" fill="none" stroke="' + stroke + '" stroke-width="' +
          model.lineWidth + '" stroke-linejoin="round"/>');
    });
    out.push('</svg>');
    if (!exporting && extras && model.hoverMode === 'closest') {
      extras.hover = function (mx, my) {
        if (mx < left || mx > left + plotWidth || my < top || my > top + plotHeight) return null;
        var best = null, bestDistance = 400;
        screen.forEach(function (s) {
          for (var k = 0; k < s.x.length; k++) {
            var dx = s.x[k] - mx, dy = s.y[k] - my, d = dx * dx + dy * dy;
            if (d < bestDistance) { bestDistance = d; best = { series: s.series, k: s.index[k] }; }
          }
        });
        if (!best) return null;
        var i = best.series;
        return '<div class="cc-tip-title">' + swatch(model.colors[i]) + esc(model.namePrefix + model.series[i]) +
          '</div><b>' + esc(model.hoverX) + ':</b> ' + formatAs(model.x[i][best.k], model.xFormat) + '<br><b>' +
          esc(model.hoverY) + ':</b> ' + formatAs(model.y[i][best.k], model.yFormat);
      };
    } else if (!exporting && extras) {
      extras.hover = function (mx, my) {
        if (mx < left || mx > left + plotWidth || my < top || my > top + plotHeight) return null;
        var at = xlo + (mx - left) / plotWidth * (xhi - xlo), rows = [];
        if (model.xLog) at = Math.pow(10, at);
        model.x.forEach(function (x, i) {
          if (hidden[i] || !x.length) return;
          var lo = 0, hi = x.length - 1;
          while (hi - lo > 1) { var mid = (lo + hi) >> 1; if (x[mid] < at) lo = mid; else hi = mid; }
          var k = Math.abs(x[lo] - at) <= Math.abs(x[hi] - at) ? lo : hi;
          rows.push(swatch(model.colors[i]) + '<b>' + esc(model.namePrefix + model.series[i]) + ':</b> ' +
            formatAs(model.y[i][k], model.yFormat));
        });
        return rows.length ? '<div class="cc-tip-title">' + esc(model.xTitle) + ': ' +
          formatAs(at, model.xFormat) + '</div>' + '<b>' + esc(model.yTitle) + '</b><br>' + rows.join('<br>') : null;
      };
    }
    return out.join('');
  }

  // ---- scatter points (canvas) ---------------------------------------------
  function scatterSvg(model, width, height, hidden, th, tips, exporting, extras) {
    var n = model.n, x = model.x, y = model.y;
    var xlo = Infinity, xhi = -Infinity, ylo = Infinity, yhi = -Infinity;
    for (var i = 0; i < n; i++) {
      if (x[i] < xlo) xlo = x[i]; if (x[i] > xhi) xhi = x[i];
      if (y[i] < ylo) ylo = y[i]; if (y[i] > yhi) yhi = y[i];
    }
    if (!(xhi >= xlo)) { xlo = 0; xhi = 1; }
    if (!(yhi >= ylo)) { ylo = 0; yhi = 1; }
    var xpad = (xhi - xlo || 1) * 0.04, ypad = (yhi - ylo || 1) * 0.06;
    xlo -= xpad; xhi += xpad; ylo -= ypad; yhi += ypad;
    var yt = ticks(ylo, yhi, Math.max(2, Math.round((height - 70) / 55)));
    var ydigits = tickDigits(yt.step, false);
    var yformat = function (v) { return fixed(v, ydigits); };
    var left = 34 + maxOf(yt.values.map(function (v) { return textWidth(yformat(v)); }), 10);
    var top = 12, plotWidth = Math.max(10, width - left - 16), plotHeight = Math.max(10, height - top - 46);
    var yaxis = valueAxis({ lo: ylo, hi: yhi, top: top, height: plotHeight, left: left, width: plotWidth,
      ticks: yt, labels: yformat, title: model.yTitle }, th);
    var xaxis = numberAxis({ lo: xlo, hi: xhi, top: top, height: plotHeight, left: left, width: plotWidth,
      ticks: ticks(xlo, xhi, Math.max(2, Math.round(plotWidth / 90))), title: model.xTitle }, th);
    var out = [svgOpen(width, height, th, exporting), yaxis.markup, xaxis.markup];
    var area = { x: left, y: top, width: plotWidth, height: plotHeight };
    var paint = function (context, ratio) {
      var radius = Math.max(0.5, model.size / 2) * ratio, border = n <= 20000;
      context.save();
      context.beginPath();
      context.rect(0, 0, plotWidth * ratio, plotHeight * ratio);
      context.clip();
      context.globalAlpha = model.opacity;
      context.lineWidth = ratio;
      context.strokeStyle = th.axis;
      model.order.forEach(function (range, g) {
        if (hidden[g] || range[1] <= range[0]) return;
        context.fillStyle = model.colors[g] || GREY;
        context.beginPath();
        for (var k = range[0]; k < range[1]; k++) {
          var p = model.sorted[k];
          var cx = (xaxis.x(x[p]) - left) * ratio, cy = (yaxis.y(y[p]) - top) * ratio;
          if (radius <= 1.5) {
            context.rect(cx - radius, cy - radius, radius * 2, radius * 2);
          } else {
            context.moveTo(cx + radius, cy);
            context.arc(cx, cy, radius, 0, 2 * Math.PI);
          }
        }
        context.fill();
        if (border && radius > 1.5) context.stroke();
      });
      context.restore();
    };
    // Exported images put the points here (exportSvg()); on screen a canvas
    // under the SVG draws them.
    out.push('<!--cc-points-->');
    if (model.trend && model.trend.x.length > 1) {
      var tx = model.trend.x, ty = model.trend.y, points = [];
      for (var k = 0; k < tx.length; k++) {
        if (isFinite(ty[k])) points.push(px(xaxis.x(tx[k])) + ',' + px(yaxis.y(ty[k])));
      }
      if (points.length > 1) {
        out.push('<path d="M' + points.join('L') + '" fill="none" stroke="' + model.trend.color +
          '" stroke-width="' + model.trend.width + '"/>');
      }
    }
    out.push('</svg>');
    if (extras) {
      extras.canvas = { area: area, paint: paint };
      if (!exporting) {
        var grid = null, cell = 8;
        var buildGrid = function () {
          var columns = Math.ceil(plotWidth / cell) + 1, rows = Math.ceil(plotHeight / cell) + 1;
          var head = new Int32Array(columns * rows).fill(-1), next = new Int32Array(n);
          for (var p = 0; p < n; p++) {
            var g = model.group[p];
            if (g >= 0 && hidden[g]) { next[p] = -1; continue; }
            var c = Math.floor((xaxis.x(x[p]) - left) / cell), r = Math.floor((yaxis.y(y[p]) - top) / cell);
            if (c < 0 || r < 0 || c >= columns || r >= rows) { next[p] = -1; continue; }
            next[p] = head[r * columns + c];
            head[r * columns + c] = p;
          }
          return { head: head, next: next, columns: columns, rows: rows };
        };
        extras.hover = function (mx, my) {
          if (mx < left || mx > left + plotWidth || my < top || my > top + plotHeight) return null;
          if (!grid) grid = buildGrid();
          var c0 = Math.floor((mx - left) / cell), r0 = Math.floor((my - top) / cell);
          var best = -1, bestDistance = 64;
          for (var r = r0 - 1; r <= r0 + 1; r++) {
            for (var c = c0 - 1; c <= c0 + 1; c++) {
              if (r < 0 || c < 0 || r >= grid.rows || c >= grid.columns) continue;
              for (var p = grid.head[r * grid.columns + c]; p >= 0; p = grid.next[p]) {
                var dx = xaxis.x(x[p]) - mx, dy = yaxis.y(y[p]) - my, d = dx * dx + dy * dy;
                if (d < bestDistance) { bestDistance = d; best = p; }
              }
            }
          }
          return best < 0 ? null : scatterTip(model, best);
        };
      }
    }
    return out.join('');
  }
  function scatterTip(model, p) {
    var g = model.group[p], rows = [];
    model.labelNames.forEach(function (name, k) {
      rows.push('<b>' + esc(name) + ':</b> ' + esc(model.labelValues[k][p]));
    });
    model.hoverNames.forEach(function (name, k) {
      var v = model.hoverValues[k][p];
      rows.push('<b>' + esc(name) + ':</b> ' + (Math.round(v) === v ? fixed(v, 0) : significant(v)));
    });
    rows.push('<b>' + esc(model.xTitle) + ':</b> ' + fixed(model.x[p], 2));
    rows.push('<b>' + esc(model.yTitle) + ':</b> ' + significant(model.y[p]));
    return '<div class="cc-tip-title">' + (g >= 0 ? swatch(model.colors[g]) + esc(model.groupTitle ?
      model.groupTitle + ' ' + model.series[g] : model.series[g]) : '') + '</div>' + rows.join('<br>');
  }

  // ---- Sankey --------------------------------------------------------------
  function sankeyRow(labels, totals, width, pad, scale) {
    var used = 0;
    totals.forEach(function (t) { used += t * scale; });
    var x = (width - used - pad * Math.max(0, labels.length - 1)) / 2;
    return totals.map(function (t) {
      var node = { x: x, width: t * scale, offset: 0 };
      x += node.width + pad;
      return node;
    });
  }
  function sankeyLabels(labels, nodes, y, above, th) {
    var shown = labels.map(function (label) { return clip(label, 20); });
    var rotate = shown.some(function (label, i) { return textWidth(label) + 4 > nodes[i].width + 15; });
    var extent = rotate ? maxOf(shown.map(function (label) { return textWidth(label); }), 0) * 0.71 + 12 : 20;
    var markup = shown.map(function (label, i) {
      var cx = nodes[i].x + nodes[i].width / 2;
      if (!rotate) {
        return text(cx, above ? y - 6 : y + 15, label, ' text-anchor="middle" fill="' + th.title + '"');
      }
      var ly = above ? y - 6 : y + 6;
      return text(cx, ly, label, ' fill="' + th.title + '" dominant-baseline="' + (above ? 'auto' : 'hanging') +
        '" transform="rotate(' + (above ? -45 : 45) + ' ' + px(cx) + ' ' + px(ly) + ')"');
    }).join('');
    return { markup: markup, extent: extent };
  }

  function sankeySvg(model, width, height, th, tips, exporting) {
    var sources = model.sources, targets = model.targets, links = model.links;
    var sourceTotals = sources.map(function () { return 0; });
    var targetTotals = targets.map(function () { return 0; });
    links.forEach(function (link) {
      sourceTotals[link.source] += link.value;
      targetTotals[link.target] += link.value;
    });
    var pad = 15, thickness = 20, margin = 16, inner = Math.max(10, width - 2 * margin);
    var available = function (count) { return Math.max(1, inner - pad * Math.max(0, count - 1)); };
    var total = Math.max(maxOf([sourceTotals.reduce(add, 0), targetTotals.reduce(add, 0)], 0), 1);
    var scale = Math.min(available(sources.length), available(targets.length)) / total;
    var top = sankeyRow(sources, sourceTotals, inner, pad, scale);
    var bottom = sankeyRow(targets, targetTotals, inner, pad, scale);
    [top, bottom].forEach(function (row) { row.forEach(function (node) { node.x += margin; }); });
    var topLabels = sankeyLabels(sources, top, 0, true, th);
    var bottomLabels = sankeyLabels(targets, bottom, 0, false, th);
    var y0 = Math.min(height * 0.35, topLabels.extent + 4);
    var y1 = height - Math.min(height * 0.35, bottomLabels.extent + 4);
    if (y1 - y0 < 3 * thickness) { y0 = thickness; y1 = height - thickness; }
    var out = [svgOpen(width, height, th, exporting)];
    var ordered = links.map(function (link, k) { return k; });
    var bySource = ordered.slice().sort(function (a, b) {
      return links[a].source - links[b].source || links[a].target - links[b].target;
    });
    var byTarget = ordered.slice().sort(function (a, b) {
      return links[a].target - links[b].target || links[a].source - links[b].source;
    });
    var start = {}, end = {};
    bySource.forEach(function (k) {
      var node = top[links[k].source];
      start[k] = node.x + node.offset;
      node.offset += links[k].value * scale;
    });
    byTarget.forEach(function (k) {
      var node = bottom[links[k].target];
      end[k] = node.x + node.offset;
      node.offset += links[k].value * scale;
    });
    var ya = y0 + thickness, yb = y1 - thickness, mid = (ya + yb) / 2;
    bySource.forEach(function (k) {
      var link = links[k], w = link.value * scale;
      var a0 = start[k], a1 = a0 + w, b0 = end[k], b1 = b0 + w;
      var d = 'M' + px(a0) + ',' + px(ya) + 'C' + px(a0) + ',' + px(mid) + ' ' + px(b0) + ',' + px(mid) + ' ' +
        px(b0) + ',' + px(yb) + 'L' + px(b1) + ',' + px(yb) + 'C' + px(b1) + ',' + px(mid) + ' ' + px(a1) + ',' +
        px(mid) + ' ' + px(a1) + ',' + px(ya) + 'Z';
      var tip = '';
      if (!exporting) {
        tips.push('<b>' + esc(model.firstTitle) + ':</b> ' + esc(sources[link.source]) + '<br><b>' +
          esc(model.secondTitle) + ':</b> ' + esc(targets[link.target]) + '<br><b>Number of cells:</b> ' +
          fixed(link.value, 0));
        tip = ' class="cc-link" data-tip="' + (tips.length - 1) + '"';
      }
      out.push('<path' + tip + ' d="' + d + '" fill="' + model.sourceColors[link.source] + '" fill-opacity="0.35"/>');
    });
    function nodes(row, labels, totals, colors, y) {
      row.forEach(function (node, i) {
        var tip = '';
        if (!exporting) {
          tips.push('<b>' + esc(labels[i]) + '</b><br>' + fixed(totals[i], 0) + ' cells');
          tip = ' data-tip="' + (tips.length - 1) + '"';
        }
        out.push('<rect' + tip + ' x="' + px(node.x) + '" y="' + px(y) + '" width="' + px(Math.max(1, node.width)) +
          '" height="' + thickness + '" fill="' + colors[i] + '" stroke="' + th.axis + '" stroke-width="0.5"/>');
      });
    }
    nodes(top, sources, sourceTotals, model.sourceColors, y0);
    nodes(bottom, targets, targetTotals, model.targetColors, yb);
    out.push(sankeyLabels(sources, top, y0, true, th).markup);
    out.push(sankeyLabels(targets, bottom, y1, false, th).markup);
    out.push('</svg>');
    return out.join('');
  }

  // ---- model ---------------------------------------------------------------
  function violinModel(value) {
    var groups = arr(value.groups).map(String);
    return {
      title: value.title || '', groups: groups, colors: arr(value.colors).map(color),
      n: nums(value.n), mean: nums(value.mean), q1: nums(value.q1),
      median: nums(value.median), q3: nums(value.q3), lowerfence: nums(value.lowerfence),
      upperfence: nums(value.upperfence), minimum: nums(value.minimum), maximum: nums(value.maximum),
      approximate: arr(value.approximate).map(Boolean), violins: arr(value.violins)
    };
  }

  function normalize(value) {
    var type = value && value.type;
    if (type === 'bar') {
      var series = arr(value.series).map(String);
      var categories = arr(value.categories).map(String);
      var values = arr(value.values).map(nums);
      var categoryColors = value.category_colors == null ? null : arr(value.category_colors).map(color);
      return {
        type: type, categories: categories, series: series,
        colors: arr(value.colors).map(color), categoryColors: categoryColors, values: values,
        percent: !!value.percent, format: value.format || 'integer', outline: value.outline ? color(value.outline) : '',
        outlineWidth: Number(value.outline_width) > 0 ? Number(value.outline_width) : 1,
        counts: value.counts ? arr(value.counts).map(nums) : null,
        xTitle: value.x_title || '', yTitle: value.y_title || '', legendTitle: value.legend_title || '',
        count: categoryColors ? categories.length : series.length
      };
    }
    if (type === 'violin') {
      var panels = (value.panels ? arr(value.panels) : [value]).map(violinModel).filter(function (panel) {
        return panel.groups.length > 0;
      });
      var groupCount = panels.reduce(function (sum, panel) { return sum + panel.groups.length; }, 0);
      return {
        type: type, mode: value.mode || 'number', percent: value.mode === 'percent',
        yTitle: value.y_title || '', panels: panels, groups: panels.length ? panels[0].groups : [],
        count: groupCount
      };
    }
    if (type === 'sankey') {
      var sourceIndex = nums(value.link_source), targetIndex = nums(value.link_target);
      var linkValues = nums(value.link_value);
      var links = [];
      for (var k = 0; k < linkValues.length; k++) {
        if (linkValues[k] > 0) links.push({ source: sourceIndex[k], target: targetIndex[k], value: linkValues[k] });
      }
      return {
        type: type, sources: arr(value.sources).map(String), targets: arr(value.targets).map(String),
        sourceColors: arr(value.source_colors).map(color), targetColors: arr(value.target_colors).map(color),
        firstTitle: value.first_title || '', secondTitle: value.second_title || '', links: links,
        count: links.length ? 1 : 0
      };
    }
    if (type === 'lines') {
      var names = arr(value.series).map(String);
      var range = nums(value.x_range);
      return {
        type: type, series: names, colors: arr(value.colors).map(color),
        x: arr(value.x).map(nums), y: arr(value.y).map(nums),
        xRange: range.length === 2 && range[1] > range[0] ? range : null,
        fillOpacity: isFinite(Number(value.fill_opacity)) ? Number(value.fill_opacity) : 0.5,
        fill: value.fill !== false, xLog: !!value.x_log,
        lineColor: value.line_color ? color(value.line_color) : '',
        lineWidth: Number(value.line_width) > 0 ? Number(value.line_width) : 1,
        hoverMode: value.hover || 'x', hoverX: value.hover_x || value.x_title || '',
        hoverY: value.hover_y || value.y_title || '',
        xFormat: value.x_format == null ? 2 : value.x_format, yFormat: value.y_format == null ? 2 : value.y_format,
        xTitle: value.x_title || '', yTitle: value.y_title || '',
        legendTitle: value.legend_title || '', namePrefix: value.name_prefix || '', count: names.length
      };
    }
    if (type === 'scatter') {
      var n = Number(value.n) || 0, group = decode(value.group, Int32Array);
      var levels = arr(value.levels).map(String);
      // Points sorted by group, so a canvas path is filled once per colour.
      var counts = new Int32Array(levels.length + 1);
      for (var p = 0; p < n; p++) counts[group[p] >= 0 ? group[p] + 1 : 0]++;
      var offsets = new Int32Array(levels.length + 2), order = [];
      for (var g = 0; g <= levels.length; g++) offsets[g + 1] = offsets[g] + counts[g];
      var sorted = new Int32Array(n), cursor = offsets.slice(0, levels.length + 1);
      for (p = 0; p < n; p++) sorted[cursor[group[p] >= 0 ? group[p] + 1 : 0]++] = p;
      for (g = 0; g < levels.length; g++) order.push([offsets[g + 1], offsets[g + 2]]);
      var trend = value.trend && value.trend.x ? {
        x: nums(value.trend.x), y: nums(value.trend.y), color: color(value.trend.color),
        width: Number(value.trend.width) || 2
      } : null;
      return {
        type: type, n: n, x: decode(value.x, Float32Array), y: decode(value.y, Float32Array), group: group,
        series: levels, colors: arr(value.colors).map(color), sorted: sorted, order: order,
        xTitle: value.x_title || '', yTitle: value.y_title || '', groupTitle: value.group_title || '',
        legendTitle: value.group_title || '', size: Number(value.size) || 5,
        opacity: isFinite(Number(value.opacity)) ? Number(value.opacity) : 1,
        hoverNames: arr(value.hover_names).map(String),
        hoverValues: arr(value.hover_values).map(function (v) { return decode(v, Float32Array); }),
        labelNames: arr(value.label_names).map(String),
        labelValues: arr(value.label_values).map(function (v) { return arr(v).map(String); }),
        trend: trend, count: n
      };
    }
    // An empty chart may say why it is empty.
    return { type: 'empty', count: 0, message: value && value.message ? String(value.message) : '' };
  }

  // Whole chart markup for a plot area of width x height. `extras` receives a
  // canvas painter (scatter) and a hover function (lines, scatter).
  function chartSvg(model, width, height, hidden, tips, exporting, extras) {
    var th = theme();
    hidden = hidden || {};
    tips = tips || [];
    if (model.type === 'bar') return barSvg(model, width, height, hidden, th, tips, exporting);
    if (model.type === 'violin') return violinSvg(model, width, height, th, tips, exporting);
    if (model.type === 'sankey') return sankeySvg(model, width, height, th, tips, exporting);
    if (model.type === 'lines') return linesSvg(model, width, height, hidden, th, tips, exporting, extras);
    if (model.type === 'scatter') return scatterSvg(model, width, height, hidden, th, tips, exporting, extras);
    return '';
  }

  function exportSvg(state) {
    var model = state.model, width = state.plotWidth || 600, height = state.plotHeight || 400;
    var extras = {};
    var chart = chartSvg(model, width, height, state.hidden, null, true, extras);
    var body = chart.replace(/^<svg[^>]*>/, '').replace(/<\/svg>$/, '');
    if (extras.canvas && root.document) {
      var area = extras.canvas.area, canvas = root.document.createElement('canvas'), ratio = 2;
      canvas.width = Math.ceil(area.width * ratio);
      canvas.height = Math.ceil(area.height * ratio);
      extras.canvas.paint(canvas.getContext('2d'), ratio);
      var points = '<image x="' + px(area.x) + '" y="' + px(area.y) + '" width="' + px(area.width) +
        '" height="' + px(area.height) + '" href="' + canvas.toDataURL('image/png') + '"/>';
      body = body.replace('<!--cc-points-->', points);
    }
    if (!hasLegend(model)) {
      return { markup: svgOpen(width, height, theme(), true) + body + '</svg>', width: width, height: height };
    }
    var legend = legendSvg(model, state.hidden, width + 8, theme());
    var full = width + legend.width + 8, tall = Math.max(height, legend.height);
    return {
      markup: svgOpen(full, tall, theme(), true) + body + legend.markup + '</svg>',
      width: full, height: tall
    };
  }

  // ---- DOM glue ------------------------------------------------------------
  function toolbarHtml() {
    return '<div class="cc-toolbar" role="toolbar" aria-label="Chart tools">' +
      '<button type="button" class="cc-btn" data-action="svg" title="Download plot as SVG" ' +
      'aria-label="Download plot as SVG">SVG</button>' +
      '<button type="button" class="cc-btn" data-action="png" title="Download plot as a png" ' +
      'aria-label="Download plot as a png">PNG</button></div>';
  }

  function draw(el) {
    var state = el.__cerebroChart;
    if (!state) return false;
    var width = el.clientWidth, height = el.clientHeight || 400;
    if (!(width > 0)) {
      state.pending = true;
      return false;
    }
    state.pending = false;
    state.width = width;
    state.height = height;
    state.tips = [];
    var model = state.model;
    if (!state.count) {
      el.innerHTML = '<div class="cc-empty">' +
        esc(model.message || 'No cells to display.').replace(/\n/g, '<br>') + '</div>';
      return true;
    }
    var legend = el.querySelector('.cc-legend');
    var scroll = legend ? legend.scrollTop : 0;
    el.innerHTML = toolbarHtml() + '<div class="cc-plot"></div>' +
      (hasLegend(model) ? legendHtml(model, state.hidden) : '') +
      '<div class="cc-tip" role="tooltip" hidden></div>';
    legend = el.querySelector('.cc-legend');
    if (legend && scroll) legend.scrollTop = scroll;
    var plot = el.querySelector('.cc-plot');
    state.plotWidth = plot.clientWidth || width;
    state.plotHeight = plot.clientHeight || height;
    var extras = {};
    plot.innerHTML = chartSvg(model, state.plotWidth, state.plotHeight, state.hidden, state.tips, false, extras);
    state.hover = extras.hover || null;
    if (extras.canvas) {
      var area = extras.canvas.area, ratio = root.devicePixelRatio || 1;
      var canvas = root.document.createElement('canvas');
      canvas.className = 'cc-canvas';
      canvas.width = Math.ceil(area.width * ratio);
      canvas.height = Math.ceil(area.height * ratio);
      canvas.style.left = px(area.x) + 'px';
      canvas.style.top = px(area.y) + 'px';
      canvas.style.width = px(area.width) + 'px';
      canvas.style.height = px(area.height) + 'px';
      plot.insertBefore(canvas, plot.firstChild);
      extras.canvas.paint(canvas.getContext('2d'), ratio);
    }
    return true;
  }

  function inputValue(id) {
    var element = root.document.getElementById(id);
    if (!element) return null;
    if (root.jQuery) {
      var binding = root.jQuery(element).data('shiny-input-binding');
      if (binding && typeof binding.getValue === 'function') {
        try { return binding.getValue(element); } catch (ignore) {}
      }
    }
    return element.value == null ? null : element.value;
  }

  // Every chart reports `cerebro:chart-ready`; Groups charts also send the
  // event the Groups page timing listens for.
  function report(el, eventKind) {
    var state = el.__cerebroChart;
    if (!state || !state.count || el.offsetParent === null) return;
    var identity = root.cerebroSavedViewDataset || {};
    var fingerprint = String(identity.cell_fingerprint || '');
    el.dataset.cerebroChartReadyFingerprint = fingerprint;
    root.dispatchEvent(new root.CustomEvent('cerebro:chart-ready', {
      detail: { plotId: el.id, type: state.model.type, count: state.count, eventKind: eventKind }
    }));
    if (!METRICS[el.id]) return;
    root.dispatchEvent(new root.CustomEvent('cerebro:groups-primary-ready', {
      detail: {
        page: 'groups',
        plotId: el.id,
        metric: METRICS[el.id] || '',
        traceCount: state.count,
        datasetFingerprint: fingerprint,
        selectedGroup: inputValue('groups_selected_group'),
        secondGroup: inputValue('groups_by_other_group_second_group'),
        plotType: inputValue('groups_by_other_group_plot_type'),
        eventKind: eventKind
      }
    }));
  }

  function hideTip(el) {
    var tip = el.querySelector('.cc-tip');
    if (tip) tip.hidden = true;
  }
  function moveTip(el, event) {
    var state = el.__cerebroChart, tip = el.querySelector('.cc-tip');
    var box = el.getBoundingClientRect();
    var x = event.clientX - box.left, y = event.clientY - box.top;
    var html = null;
    if (state && state.hover) {
      var plot = el.querySelector('.cc-plot'), area = plot ? plot.getBoundingClientRect() : box;
      html = state.hover(event.clientX - area.left, event.clientY - area.top);
    } else {
      var target = event.target && event.target.closest ? event.target.closest('[data-tip]') : null;
      html = state && target ? state.tips[Number(target.getAttribute('data-tip'))] : null;
    }
    if (!tip || html == null) {
      hideTip(el);
      return;
    }
    if (tip.__cerebroTip !== html) {
      tip.innerHTML = html;
      tip.__cerebroTip = html;
    }
    tip.hidden = false;
    var left = x + 14, top = y + 14;
    if (left + tip.offsetWidth > box.width) left = Math.max(0, x - tip.offsetWidth - 14);
    if (top + tip.offsetHeight > box.height) top = Math.max(0, y - tip.offsetHeight - 14);
    tip.style.left = px(left) + 'px';
    tip.style.top = px(top) + 'px';
  }

  function save(blob, filename) {
    var url = root.URL.createObjectURL(blob), link = root.document.createElement('a');
    link.href = url;
    link.download = filename;
    root.document.body.appendChild(link);
    link.click();
    link.parentNode.removeChild(link);
    root.setTimeout(function () { root.URL.revokeObjectURL(url); }, 1000);
  }
  function download(el, kind) {
    var state = el.__cerebroChart;
    if (!state || !state.count) return;
    var image = exportSvg(state), name = el.id || 'chart';
    var blob = new root.Blob([image.markup], { type: 'image/svg+xml;charset=utf-8' });
    if (kind === 'svg') {
      save(blob, name + '.svg');
      return;
    }
    var url = root.URL.createObjectURL(blob), img = new root.Image();
    img.onload = function () {
      var canvas = root.document.createElement('canvas'), ratio = 2;
      canvas.width = Math.ceil(image.width * ratio);
      canvas.height = Math.ceil(image.height * ratio);
      var context = canvas.getContext('2d');
      context.scale(ratio, ratio);
      context.drawImage(img, 0, 0, image.width, image.height);
      root.URL.revokeObjectURL(url);
      canvas.toBlob(function (png) { if (png) save(png, name + '.png'); }, 'image/png');
    };
    img.onerror = function () { root.URL.revokeObjectURL(url); };
    img.src = url;
  }

  function onClick(el, event) {
    var target = event.target && event.target.closest ? event.target : null;
    if (!target) return;
    var action = target.closest('[data-action]');
    if (action && el.contains(action)) {
      download(el, action.getAttribute('data-action'));
      return;
    }
    var item = target.closest('[data-series]'), state = el.__cerebroChart;
    if (!item || !state || !el.contains(item)) return;
    var i = Number(item.getAttribute('data-series'));
    if (state.hidden[i]) delete state.hidden[i]; else state.hidden[i] = true;
    draw(el);
    var again = el.querySelector('[data-series="' + i + '"]');
    if (again && again.focus) again.focus();
  }

  var resizer = null;
  function resized(el) {
    var state = el.__cerebroChart;
    if (!state) return;
    var width = el.clientWidth, height = el.clientHeight;
    if (!(width > 0) || (!state.pending && width === state.width && height === state.height)) return;
    var first = state.pending;
    if (draw(el) && first) report(el, 'primary');
  }
  function observe(el) {
    if (el.__cerebroChartBound) return;
    el.__cerebroChartBound = true;
    el.addEventListener('mousemove', function (event) { moveTip(el, event); });
    el.addEventListener('mouseleave', function () { hideTip(el); });
    el.addEventListener('click', function (event) { onClick(el, event); });
    if (root.ResizeObserver) {
      if (!resizer) {
        resizer = new root.ResizeObserver(function (entries) {
          entries.forEach(function (entry) { resized(entry.target); });
        });
      }
      resizer.observe(el);
    } else if (root.addEventListener) {
      root.addEventListener('resize', function () { resized(el); });
    }
  }

  function renderChart(el, value) {
    observe(el);
    var model = normalize(value);
    el.__cerebroChart = { model: model, count: model.count, hidden: {}, tips: [], pending: false,
      restyle: value.restyle || null };
    if (draw(el)) report(el, 'primary');
  }

  // A "switch" value carries one chart per combination of the controls in
  // `inputs` (views keyed by their values joined with "|", booleans as
  // TRUE/FALSE). The browser shows the view the controls ask for, so changing
  // them redraws here without a server round trip.
  function switchKey(sw) {
    return sw.inputs.map(function (id) {
      var value = inputValue(id);
      if (value === true || value === false) return value ? 'TRUE' : 'FALSE';
      return value == null ? '' : String(value);
    }).join('|');
  }
  function renderSwitch(el) {
    var sw = el.__cerebroSwitch;
    var key = switchKey(sw);
    var view = sw.views[key] || sw.views[sw.fallback];
    if (!view || view === sw.shown) return;
    sw.shown = view;
    renderChart(el, view);
  }
  function render(el, value) {
    if (value && value.type === 'switch') {
      el.__cerebroSwitch = {
        inputs: arr(value.inputs).map(String), views: value.views || {},
        fallback: String(value.fallback || ''), shown: null
      };
      renderSwitch(el);
      return;
    }
    el.__cerebroSwitch = null;
    renderChart(el, value);
  }
  // `restyle` in a payload maps input ids to look settings the browser applies
  // itself (R isolates those inputs), so a slider redraws as it moves, before
  // Shiny's debounce and without R.
  var RESTYLE = {
    fill_opacity: function (model, v) { model.fillOpacity = v; },
    size: function (model, v) { model.size = v; },
    opacity: function (model, v) { model.opacity = v; },
    trend_width: function (model, v) { if (model.trend) model.trend.width = v; }
  };
  function onControlChange(event) {
    var id = event.target && event.target.id, value = Number(event.target && event.target.value);
    if (!id || !isFinite(value)) return;
    charts(root.document).forEach(function (el) {
      var state = el.__cerebroChart, field = state && state.restyle && state.restyle[id];
      if (!RESTYLE[field]) return;
      RESTYLE[field](state.model, value);
      // One redraw per frame: a million scatter points take ~350 ms to paint.
      if (!state.frame) {
        state.frame = root.requestAnimationFrame(function () { state.frame = 0; draw(el); });
      }
    });
  }
  function onInputChanged(event) {
    // The event fires before the binding's value is read back; wait a tick.
    root.setTimeout(function () {
      charts(root.document).forEach(function (el) {
        var sw = el.__cerebroSwitch;
        if (sw && sw.inputs.indexOf(event.name) >= 0) renderSwitch(el);
      });
    }, 0);
  }
  function clear(el) {
    el.__cerebroChart = null;
    el.__cerebroSwitch = null;
    delete el.dataset.cerebroChartReadyFingerprint;
  }

  function charts(pane) {
    return pane && pane.querySelectorAll ? Array.prototype.slice.call(pane.querySelectorAll('.cerebro-chart')) : [];
  }

  // Returning to a tab re-reports charts already drawn for this dataset.
  if (root.document && root.document.addEventListener) {
    root.document.addEventListener('click', function (event) {
      var link = event.target && event.target.closest
        ? event.target.closest('a[href^="#shiny-tab-"]') : null;
      if (!link) return;
      var pane = root.document.getElementById(link.getAttribute('href').slice(1));
      root.requestAnimationFrame(function () {
        var identity = root.cerebroSavedViewDataset || {};
        var fingerprint = String(identity.cell_fingerprint || '');
        charts(pane).forEach(function (el) {
          if (el.__cerebroChart && el.dataset.cerebroChartReadyFingerprint === fingerprint) {
            report(el, 'cached');
          }
        });
      });
    }, true);
  }

  var registered = false;
  function register() {
    var Shiny = root.Shiny, $ = root.jQuery;
    if (registered || !Shiny || !Shiny.OutputBinding || !Shiny.outputBindings || !$) return;
    registered = true;
    var binding = new Shiny.OutputBinding();
    $.extend(binding, {
      find: function (scope) { return $(scope).find('.cerebro-chart'); },
      renderValue: function (el, value) {
        if (value == null) {
          clear(el);
          el.innerHTML = '';
          return;
        }
        render(el, value);
      },
      renderError: function (el, err) {
        clear(el);
        Shiny.OutputBinding.prototype.renderError.call(this, el, err);
      }
    });
    Shiny.outputBindings.register(binding, 'cerebro.chart');
    var $document = $(root.document);
    if ($document && typeof $document.on === 'function') {
      $document.on('shiny:inputchanged.cerebroCharts', onInputChanged);
      $document.on('change.cerebroCharts', 'input', onControlChange);
    }
  }
  register();
  if (!registered && root.document && root.document.addEventListener) {
    root.document.addEventListener('DOMContentLoaded', function () {
      register();
      // Bind outputs that an already initialised Shiny found without us.
      if (registered && root.Shiny.shinyapp && root.Shiny.bindAll) root.Shiny.bindAll(root.document.body);
    });
  }

  root.CerebroCharts = {
    normalize: normalize,
    chartSvg: chartSvg,
    exportSvg: exportSvg,
    ticks: ticks,
    logTicks: logTicks,
    fixed: fixed,
    render: render
  };
})(typeof window !== 'undefined' ? window : this);
