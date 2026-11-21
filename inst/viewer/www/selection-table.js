/*
 * Table of selected cells, built in the browser from columns it already holds
 * (cell names, canonical metadata codes and values). It looks and behaves like
 * the server's DT table (prettifyTable(): column visibility, CSV/Excel
 * download, sortable headers, a filter row, number formatting and colour
 * highlighting, length menu, info and pagination), but only the rows of the
 * shown page become DOM: sorting, filtering, paging and downloads work on
 * index arrays, so a selection of a million cells needs no server round trip.
 * The table is built when it is opened, not on every selection.
 *
 * source = {
 *   n: number of rows,
 *   columns: [{ name, kind: 'text' | 'categorical' | 'numeric',
 *               value(i) -> string | level index (-1 missing) | number,
 *               levels, colors, palette }],
 *   fileName, numberFormatting, colorHighlighting,
 *   colors: { accent, signal, up, down },
 *   open, onToggle(open): whether the table is shown, kept by the caller
 * }
 */
(function (root) {
  'use strict';
  if (!root || root.CerebroSelectionTable) return;

  var PAGE_SIZES = [15, 30, 50, 100, 1000];
  var DEFAULT_COLORS = { accent: '#f97316', signal: '#2f6fd6', up: '#4c9a6b', down: '#c05b5b' };
  var P_VALUE = /pval|p_val|p-val|p\.val|padj|p_adj|p-adj|p\.adj|adjp|adj_p|adj-p|adj\.p|FDR|qval|q_val|q-val|q\.val/i;
  var PERCENT = /pct|percent|%/i;
  var LOG_FC = /logFC|log-FC|log_FC|log\.FC/i;

  function esc(value) {
    return String(value).replace(/[&<>"']/g, function (c) {
      return { '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' }[c];
    });
  }
  function fmtCount(n) { return Number(n).toLocaleString('en-US'); }
  function hexRgb(hex) {
    var m = /^#?([0-9a-f]{6})$/i.exec(String(hex || ''));
    if (!m) return [249, 115, 22];
    var n = parseInt(m[1], 16);
    return [(n >> 16) & 255, (n >> 8) & 255, n & 255];
  }

  // How prettifyTable() formats and colours a numeric column, from the values
  // of the listed rows: integer, percentage, p-value, logFC or other numbers,
  // and the range its colour ramp spans.
  function numericProfile(column, n) {
    var min = Infinity, max = -Infinity, integer = true, complete = true;
    for (var i = 0; i < n; i++) {
      var v = column.value(i);
      if (v == null || !isFinite(v)) { complete = false; continue; }
      if (v < min) min = v;
      if (v > max) max = v;
      if (integer && Math.round(v) !== v) integer = false;
    }
    var role = 'number';
    if (complete && n && P_VALUE.test(column.name) && min >= 0 && max <= 1) role = 'pvalue';
    else if (complete && n && PERCENT.test(column.name) && min >= 0 && max <= 100) role = 'percent';
    else if (complete && n && integer) role = 'integer';
    else if (LOG_FC.test(column.name)) role = 'logfc';
    return {
      role: role, min: min, max: max,
      // Percentages on a 0-100 scale are shown as fractions of one.
      scale: role === 'percent' && max > 1 ? 100 : 1
    };
  }
  function numberText(v, profile, formatting) {
    if (v == null || !isFinite(v)) return '';
    if (!formatting) return String(v);
    switch (profile.role) {
      case 'integer': return Math.round(v).toLocaleString('en-US');
      case 'pvalue': return v.toPrecision(3);
      case 'logfc': return v.toFixed(3);
      case 'percent': return (100 * v / profile.scale).toFixed(2) + '%';
      default: return v.toPrecision(3);
    }
  }
  // The cell style of prettifyTable()'s colour highlighting.
  function numberStyle(v, profile, colors) {
    if (v == null || !isFinite(v)) return '';
    if (profile.role === 'pvalue' || profile.role === 'percent') {
      // DT::styleColorBar(): a bar from the right, as wide as the value.
      var share = Math.max(0, Math.min(1, v / profile.scale));
      var edge = (100 * (1 - share)).toFixed(1);
      return 'background:linear-gradient(90deg,transparent ' + edge + '%,' + colors.signal +
        ' ' + edge + '%);background-size:98% 88%;background-repeat:no-repeat;' +
        'background-position:center;';
    }
    if (!(profile.max > profile.min)) return '';
    // DT::styleInterval() over 101 cuts of a white-to-accent ramp.
    var t = (v - profile.min) / (profile.max - profile.min);
    var step = t <= 0 ? 0 : Math.min(101, Math.ceil(t * 100 - 1e-9));
    var rgb = hexRgb(colors.accent), f = step / 101;
    return 'background-color:rgb(' + rgb.map(function (c) {
      return Math.round(255 + (c - 255) * f);
    }).join(',') + ');';
  }

  function SelectionTable(el, source) {
    this.el = el;
    this.open = !!source.open;
    this.pageSize = PAGE_SIZES[0];
    this.page = 0;
    this.sort = null;           // { column, name, descending }
    this.filters = {};          // column name -> filter
    this.hiddenColumns = {};    // column name -> true
    this.menu = null;
    this.setSource(source);
  }
  SelectionTable.prototype.setSource = function (source) {
    this.source = source;
    this.order = null;
    this.page = 0;
    this.profiles = source.columns.map(function (column) {
      return column.kind === 'numeric' ? numericProfile(column, source.n) : null;
    });
    var names = source.columns.map(function (c) { return c.name; });
    Object.keys(this.filters).forEach(function (name) {
      if (names.indexOf(name) < 0) delete this.filters[name];
    }, this);
    if (this.sort) {
      this.sort.column = names.indexOf(this.sort.name);
      if (this.sort.column < 0) this.sort = null;
    }
    this.render();
  };
  SelectionTable.prototype.display = function (c, i, formatting) {
    var column = this.source.columns[c], v = column.value(i);
    if (column.kind === 'categorical') {
      return v >= 0 && column.levels[v] != null ? String(column.levels[v]) : '';
    }
    if (column.kind === 'numeric') return numberText(v, this.profiles[c], formatting);
    return v == null ? '' : String(v);
  };
  // Rows that pass every filter, in the chosen order.
  SelectionTable.prototype.rows = function () {
    if (this.order) return this.order;
    var source = this.source, n = source.n, keep = new Int32Array(n), k = 0, i;
    var active = source.columns.map(function (column) {
      return { column: column, filter: this.filters[column.name] };
    }, this).filter(function (entry) { return entry.filter; });
    for (i = 0; i < n; i++) {
      var pass = true;
      for (var f = 0; f < active.length && pass; f++) {
        var column = active[f].column, filter = active[f].filter, v = column.value(i);
        if (column.kind === 'categorical') pass = !!filter.levels[v];
        else if (column.kind === 'numeric') {
          pass = v != null && isFinite(v) &&
            !(filter.min != null && v < filter.min) && !(filter.max != null && v > filter.max);
        } else pass = v != null && String(v).toLowerCase().indexOf(filter.text) >= 0;
      }
      if (pass) keep[k++] = i;
    }
    var order = keep.slice(0, k);
    if (this.sort) {
      var col = source.columns[this.sort.column], sign = this.sort.descending ? -1 : 1;
      var perm = new Array(k);
      for (i = 0; i < k; i++) perm[i] = i;
      if (col.kind === 'text') {
        var keys = new Array(k);
        for (i = 0; i < k; i++) keys[i] = String(col.value(order[i]) || '');
        perm.sort(function (a, b) {
          return keys[a] < keys[b] ? -sign : keys[a] > keys[b] ? sign : a - b;
        });
      } else {
        // Level order or value; missing values last.
        var values = new Float64Array(k);
        for (i = 0; i < k; i++) {
          var v = col.value(order[i]);
          values[i] = v == null || !isFinite(v) || (col.kind === 'categorical' && v < 0) ? NaN : v;
        }
        perm.sort(function (a, b) {
          var x = values[a], y = values[b];
          if (x !== x) return y !== y ? a - b : 1;
          if (y !== y) return -1;
          return x === y ? a - b : (x < y ? -sign : sign);
        });
      }
      var sorted = new Int32Array(k);
      for (i = 0; i < k; i++) sorted[i] = order[perm[i]];
      order = sorted;
    }
    this.order = order;
    return order;
  };
  SelectionTable.prototype.visibleColumns = function () {
    var hidden = this.hiddenColumns, columns = this.source.columns;
    return columns.map(function (column, c) { return c; })
      .filter(function (c) { return !hidden[columns[c].name]; });
  };
  SelectionTable.prototype.filterText = function (column) {
    var filter = this.filters[column.name];
    if (!filter) return '';
    if (column.kind === 'categorical') {
      return column.levels.filter(function (level, l) { return filter.levels[l]; }).join(', ');
    }
    if (column.kind === 'numeric') {
      return (filter.min != null ? filter.min : '') + ' ... ' + (filter.max != null ? filter.max : '');
    }
    return filter.text;
  };
  SelectionTable.prototype.pageButtons = function (pages) {
    var page = this.page, items = [];
    var add = function (label, target, disabled, active, extra) {
      items.push('<li class="paginate_button' + (extra || '') + (disabled ? ' disabled' : '') +
        (active ? ' active' : '') + '"><a href="#" data-page="' + target + '">' + label + '</a></li>');
    };
    add('Previous', page - 1, page === 0, false, ' previous');
    // First, last and the pages around the current one (DataTables' "simple_numbers").
    var shown = [];
    if (pages <= 7) {
      for (var p = 0; p < pages; p++) shown.push(p);
    } else if (page < 4) {
      shown = [0, 1, 2, 3, 4, -1, pages - 1];
    } else if (page > pages - 5) {
      shown = [0, -1, pages - 5, pages - 4, pages - 3, pages - 2, pages - 1];
    } else {
      shown = [0, -1, page - 1, page, page + 1, -1, pages - 1];
    }
    shown.forEach(function (p) {
      if (p < 0) items.push('<li class="paginate_button disabled"><a href="#">…</a></li>');
      else add(String(p + 1), p, false, p === page);
    });
    add('Next', page + 1, page >= pages - 1, false, ' next');
    return '<ul class="pagination">' + items.join('') + '</ul>';
  };
  SelectionTable.prototype.render = function () {
    var self = this, source = this.source, el = this.el;
    this.closeMenu();
    if (!this.open) {
      el.innerHTML = '<div class="cst-collapsed"><span>' + fmtCount(source.n) +
        ' selected cells</span><button type="button" class="btn btn-default dt-button cst-open">' +
        '<span>Show table</span></button></div>';
      el.querySelector('.cst-open').addEventListener('click', function () {
        self.open = true;
        if (self.source.onToggle) self.source.onToggle(true);
        self.render();
      });
      return;
    }
    var rows = this.rows(), pages = Math.max(1, Math.ceil(rows.length / this.pageSize));
    if (this.page >= pages) this.page = pages - 1;
    var start = this.page * this.pageSize, end = Math.min(rows.length, start + this.pageSize);
    var columns = source.columns, shownColumns = this.visibleColumns();
    var formatting = source.numberFormatting !== false;
    var highlighting = source.colorHighlighting !== false;
    var colors = Object.assign({}, DEFAULT_COLORS, source.colors || {});
    var head = '<tr>' + shownColumns.map(function (c) {
      var state = self.sort && self.sort.column === c
        ? (self.sort.descending ? 'sorting_desc' : 'sorting_asc') : 'sorting';
      var ariaSort = state === 'sorting_asc' ? 'ascending'
        : state === 'sorting_desc' ? 'descending' : 'none';
      return '<th class="' + state + '" data-col="' + c + '" tabindex="0" aria-sort="' +
        ariaSort + '">' + esc(columns[c].name) + '</th>';
    }).join('') + '</tr><tr class="cst-filters">' + shownColumns.map(function (c) {
      return '<td><input type="search" class="form-control" data-col="' + c +
        '" placeholder="All" value="' + esc(self.filterText(columns[c])) + '"' +
        (columns[c].kind === 'text' ? '' : ' readonly aria-haspopup="true" aria-expanded="false"') +
        ' aria-label="Filter ' + esc(columns[c].name) + '"></td>';
    }).join('') + '</tr>';
    var body = '';
    for (var r = start; r < end; r++) {
      var i = rows[r];
      body += '<tr class="' + ((r - start) % 2 ? 'even' : 'odd') + '">' + shownColumns.map(function (c) {
        var column = columns[c], style = '';
        if (highlighting && column.kind === 'numeric') {
          style = numberStyle(column.value(i), self.profiles[c], colors);
        } else if (highlighting && column.kind === 'categorical' && column.palette) {
          var code = column.value(i), color = code >= 0 ? column.colors[code] : null;
          if (color && /^(#[0-9a-fA-F]{3,8}|(rgb|hsl)a?[(][0-9., %]+[)]|[a-zA-Z]+)$/.test(color)) {
            style = 'background-color:' + color + ';font-weight:bold;';
          }
        }
        return '<td class="cst-' + column.kind + '"' + (style ? ' style="' + style + '"' : '') + '>' +
          esc(self.display(c, i, formatting)) + '</td>';
      }).join('') + '</tr>';
    }
    if (!body) {
      body = '<tr class="odd"><td colspan="' + shownColumns.length +
        '" class="dataTables_empty">No matching records found</td></tr>';
    }
    el.innerHTML =
      '<div class="dataTables_wrapper form-inline dt-bootstrap no-footer">' +
      '<div class="dt-buttons btn-group">' +
      '<button type="button" class="btn btn-default dt-button buttons-collection cst-colvis" ' +
      'aria-haspopup="true" aria-expanded="false">' +
      '<span>Column visibility</span></button>' +
      '<button type="button" class="btn btn-default dt-button buttons-collection cst-download" ' +
      'aria-haspopup="true" aria-expanded="false">' +
      '<span>Download</span></button></div>' +
      '<button type="button" class="btn btn-default dt-button cst-close"><span>Hide table</span></button>' +
      '<div class="dataTables_scroll"><table class="table table-bordered table-condensed ' +
      'dataTable stripe no-footer cst-table"><thead>' + head + '</thead><tbody>' + body +
      '</tbody></table></div>' +
      '<div class="dataTables_length"><label>Show <select class="form-control input-sm">' +
      PAGE_SIZES.map(function (size) {
        return '<option value="' + size + '"' + (size === self.pageSize ? ' selected' : '') + '>' +
          size + '</option>';
      }).join('') + '</select> entries</label></div>' +
      '<div class="cst-footer"><div class="dataTables_info" role="status" aria-live="polite">' +
      (rows.length ? 'Showing ' + fmtCount(start + 1) + ' to ' + fmtCount(end) + ' of ' +
        fmtCount(rows.length) + ' entries' : 'Showing 0 to 0 of 0 entries') +
      (rows.length !== source.n ? ' (filtered from ' + fmtCount(source.n) + ' total entries)' : '') +
      '</div><div class="dataTables_paginate paging_simple_numbers">' +
      this.pageButtons(pages) + '</div></div></div>';
    this.bind(pages);
  };
  SelectionTable.prototype.bind = function (pages) {
    var self = this, el = this.el;
    el.querySelector('.cst-close').addEventListener('click', function () {
      self.open = false;
      if (self.source.onToggle) self.source.onToggle(false);
      self.render();
    });
    el.querySelector('.cst-colvis').addEventListener('click', function (event) {
      self.toggleMenu(event.currentTarget, 'colvis');
    });
    el.querySelector('.cst-download').addEventListener('click', function (event) {
      self.toggleMenu(event.currentTarget, 'download');
    });
    el.querySelector('.dataTables_length select').addEventListener('change', function (event) {
      self.pageSize = Number(event.target.value) || PAGE_SIZES[0];
      self.page = 0; self.render();
    });
    Array.prototype.forEach.call(el.querySelectorAll('.pagination a[data-page]'), function (link) {
      link.addEventListener('click', function (event) {
        event.preventDefault();
        if (link.parentNode.classList.contains('disabled')) return;
        self.page = Math.max(0, Math.min(pages - 1, Number(link.getAttribute('data-page'))));
        self.render();
      });
    });
    Array.prototype.forEach.call(el.querySelectorAll('thead th[data-col]'), function (th) {
      var sortThis = function () {
        var c = Number(th.getAttribute('data-col')), name = self.source.columns[c].name;
        // Ascending, then descending, then ascending again (as DataTables).
        self.sort = { column: c, name: name,
          descending: !!(self.sort && self.sort.column === c && !self.sort.descending) };
        self.order = null; self.page = 0; self.render();
        return c;
      };
      th.addEventListener('click', sortThis);
      // The header is a control too: Enter or Space sorts, and the table is
      // drawn anew, so the same header takes the focus back.
      th.addEventListener('keydown', function (event) {
        if (event.key !== 'Enter' && event.key !== ' ') return;
        event.preventDefault();
        var again = self.el.querySelector('thead th[data-col="' + sortThis() + '"]');
        if (again) again.focus();
      });
    });
    var timer = null;
    Array.prototype.forEach.call(el.querySelectorAll('.cst-filters input'), function (input) {
      var c = Number(input.getAttribute('data-col')), column = self.source.columns[c];
      if (column.kind !== 'text') {
        input.addEventListener('click', function () { self.toggleMenu(input, 'filter', c); });
        // A read-only box opens its menu from the keyboard as well.
        input.addEventListener('keydown', function (event) {
          if (event.key === 'Enter' || event.key === 'ArrowDown') {
            event.preventDefault();
            self.toggleMenu(input, 'filter', c);
          }
        });
        return;
      }
      input.addEventListener('input', function () {
        clearTimeout(timer);
        timer = setTimeout(function () {
          var text = input.value.toLowerCase();
          if (text) self.filters[column.name] = { text: text };
          else delete self.filters[column.name];
          self.order = null; self.page = 0; self.render();
          var again = self.el.querySelector('.cst-filters input[data-col="' + c + '"]');
          if (again) { again.focus(); again.setSelectionRange(again.value.length, again.value.length); }
        }, 250);
      });
    });
  };
  SelectionTable.prototype.closeMenu = function () {
    if (this.menu) {
      this.menu.remove();
      this.menu = null;
      document.removeEventListener('mousedown', this.menuAway, true);
      document.removeEventListener('keydown', this.menuKey, true);
      if (this.menuAnchor) this.menuAnchor.setAttribute('aria-expanded', 'false');
      this.menuAnchor = null;
    }
  };
  // A drop-down under `anchor`: column visibility, downloads, or the filter of
  // a categorical (level check list) or numeric (range) column.
  SelectionTable.prototype.toggleMenu = function (anchor, kind, c) {
    var self = this, key = kind + ':' + (c == null ? '' : c);
    var previous = this.menu && this.menu.getAttribute('data-key');
    this.closeMenu();
    if (previous === key) return;
    var menu = document.createElement('div');
    menu.className = 'dt-button-collection cst-menu';
    menu.setAttribute('data-key', key);
    var columns = this.source.columns;
    if (kind === 'colvis') {
      menu.innerHTML = columns.map(function (column) {
        return '<button type="button" class="dt-button buttons-columnVisibility' +
          (self.hiddenColumns[column.name] ? '' : ' active') + '" data-name="' +
          esc(column.name) + '"><span>' + esc(column.name) + '</span></button>';
      }).join('');
      menu.addEventListener('click', function (event) {
        var button = event.target.closest('button[data-name]');
        if (!button) return;
        var name = button.getAttribute('data-name');
        if (self.hiddenColumns[name]) delete self.hiddenColumns[name];
        else self.hiddenColumns[name] = true;
        self.render();
        var again = self.el.querySelector('.cst-colvis');
        if (again) self.toggleMenu(again, 'colvis');
      });
    } else if (kind === 'download') {
      menu.innerHTML = '<button type="button" class="dt-button" data-format="csv"><span>CSV</span></button>' +
        '<button type="button" class="dt-button" data-format="excel"><span>Excel</span></button>';
      menu.addEventListener('click', function (event) {
        var button = event.target.closest('button[data-format]');
        if (!button) return;
        self.closeMenu();
        if (button.getAttribute('data-format') === 'csv') self.downloadCsv();
        else self.downloadExcel();
      });
    } else {
      var column = columns[c], filter = this.filters[column.name];
      if (column.kind === 'categorical') {
        menu.innerHTML = '<div class="cst-levels">' + column.levels.map(function (level, l) {
          return '<label><input type="checkbox" value="' + l + '"' +
            (filter && filter.levels[l] ? ' checked' : '') + '> ' + esc(level) + '</label>';
        }).join('') + '</div>';
        menu.addEventListener('change', function () {
          var levels = {}, any = false;
          Array.prototype.forEach.call(menu.querySelectorAll('input:checked'), function (box) {
            levels[Number(box.value)] = true; any = true;
          });
          if (any) self.filters[column.name] = { levels: levels };
          else delete self.filters[column.name];
          self.order = null; self.page = 0;
          self.render();
          var again = self.el.querySelector('.cst-filters input[data-col="' + c + '"]');
          if (again) self.toggleMenu(again, 'filter', c);
        });
      } else {
        var profile = this.profiles[c];
        menu.innerHTML = '<div class="cst-range"><input type="number" step="any" class="form-control" ' +
          'data-bound="min" placeholder="' + (isFinite(profile.min) ? profile.min : 'min') + '" value="' +
          (filter && filter.min != null ? filter.min : '') + '"><span>...</span>' +
          '<input type="number" step="any" class="form-control" data-bound="max" placeholder="' +
          (isFinite(profile.max) ? profile.max : 'max') + '" value="' +
          (filter && filter.max != null ? filter.max : '') + '"></div>';
        menu.addEventListener('change', function () {
          var bound = function (name) {
            var value = menu.querySelector('[data-bound="' + name + '"]').value;
            return value === '' ? null : Number(value);
          };
          var min = bound('min'), max = bound('max');
          if (min == null && max == null) delete self.filters[column.name];
          else self.filters[column.name] = { min: min, max: max };
          self.order = null; self.page = 0;
          self.render();
        });
      }
    }
    var rect = anchor.getBoundingClientRect(), host = this.el.getBoundingClientRect();
    menu.style.left = (rect.left - host.left) + 'px';
    menu.style.top = (rect.bottom - host.top + 2) + 'px';
    this.el.style.position = 'relative';
    this.el.appendChild(menu);
    this.menu = menu;
    this.menuAway = function (event) {
      if (!menu.contains(event.target) && event.target !== anchor) self.closeMenu();
    };
    document.addEventListener('mousedown', this.menuAway, true);
    // Escape closes the menu and gives the focus back to what opened it.
    this.menuKey = function (event) {
      if (event.key !== 'Escape') return;
      self.closeMenu();
      anchor.focus();
    };
    document.addEventListener('keydown', this.menuKey, true);
    this.menuAnchor = anchor;
    anchor.setAttribute('aria-expanded', 'true');
  };
  // Every listed row (filters and order applied) in the visible columns.
  SelectionTable.prototype.eachRow = function (visit) {
    var rows = this.rows(), shown = this.visibleColumns(), columns = this.source.columns;
    for (var r = 0; r < rows.length; r++) {
      var i = rows[r];
      visit(shown.map(function (c) {
        return { text: this.display(c, i, false),
          number: columns[c].kind === 'numeric' ? columns[c].value(i) : null };
      }, this));
    }
  };
  SelectionTable.prototype.save = function (parts, type, extension) {
    var url = URL.createObjectURL(new Blob(parts, { type: type }));
    var link = document.createElement('a');
    link.href = url;
    link.download = (this.source.fileName || 'selected_cells') + extension;
    document.body.appendChild(link);
    link.click();
    link.remove();
    setTimeout(function () { URL.revokeObjectURL(url); }, 1000);
  };
  SelectionTable.prototype.downloadCsv = function () {
    var quote = function (text) {
      return /[",\n\r]/.test(text) ? '"' + text.replace(/"/g, '""') + '"' : text;
    };
    var shown = this.visibleColumns(), columns = this.source.columns;
    var parts = [shown.map(function (c) { return quote(columns[c].name); }).join(',') + '\n'];
    var chunk = '';
    this.eachRow(function (cells) {
      chunk += cells.map(function (cell) { return quote(cell.text); }).join(',') + '\n';
      if (chunk.length > 1 << 20) { parts.push(chunk); chunk = ''; }
    });
    parts.push(chunk);
    this.save(parts, 'text/csv', '.csv');
  };
  // An .xlsx workbook with one sheet of inline strings and numbers, in a zip
  // written here without compression.
  SelectionTable.prototype.downloadExcel = function () {
    var encoder = new TextEncoder(), shown = this.visibleColumns(), columns = this.source.columns;
    var xmlText = function (text) {
      return String(text).replace(/[&<>"]/g, function (c) {
        return { '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;' }[c];
      }).replace(/[\u0000-\u0008\u000b\u000c\u000e-\u001f]/g, '');
    };
    var cellXml = function (cell) {
      return cell.number != null && isFinite(cell.number)
        ? '<c><v>' + cell.number + '</v></c>'
        : '<c t="inlineStr"><is><t>' + xmlText(cell.text) + '</t></is></c>';
    };
    var sheet = [encoder.encode('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>' +
      '<worksheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main"><sheetData>' +
      '<row>' + shown.map(function (c) { return cellXml({ text: columns[c].name }); }).join('') + '</row>')];
    var chunk = '';
    this.eachRow(function (cells) {
      chunk += '<row>' + cells.map(cellXml).join('') + '</row>';
      if (chunk.length > 1 << 20) { sheet.push(encoder.encode(chunk)); chunk = ''; }
    });
    sheet.push(encoder.encode(chunk + '</sheetData></worksheet>'));
    var xml = '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>';
    var rels = 'http://schemas.openxmlformats.org/package/2006/relationships';
    var officeRels = 'http://schemas.openxmlformats.org/officeDocument/2006/relationships';
    var files = [
      ['[Content_Types].xml', xml +
        '<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">' +
        '<Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>' +
        '<Default Extension="xml" ContentType="application/xml"/>' +
        '<Override PartName="/xl/workbook.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml"/>' +
        '<Override PartName="/xl/worksheets/sheet1.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"/>' +
        '</Types>'],
      ['_rels/.rels', xml + '<Relationships xmlns="' + rels + '">' +
        '<Relationship Id="rId1" Type="' + officeRels + '/officeDocument" Target="xl/workbook.xml"/>' +
        '</Relationships>'],
      ['xl/workbook.xml', xml +
        '<workbook xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" xmlns:r="' +
        officeRels + '"><sheets><sheet name="Selected cells" sheetId="1" r:id="rId1"/></sheets></workbook>'],
      ['xl/_rels/workbook.xml.rels', xml + '<Relationships xmlns="' + rels + '">' +
        '<Relationship Id="rId1" Type="' + officeRels + '/worksheet" Target="worksheets/sheet1.xml"/>' +
        '</Relationships>'],
      ['xl/worksheets/sheet1.xml', sheet]
    ];
    this.save(zipStored(files.map(function (file) {
      return { name: file[0], parts: Array.isArray(file[1]) ? file[1] : [encoder.encode(file[1])] };
    })), 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet', '.xlsx');
  };

  var CRC_TABLE = (function () {
    var table = new Uint32Array(256);
    for (var n = 0; n < 256; n++) {
      var c = n;
      for (var k = 0; k < 8; k++) c = c & 1 ? 0xedb88320 ^ (c >>> 1) : c >>> 1;
      table[n] = c >>> 0;
    }
    return table;
  })();
  // A zip archive without compression from [{ name, parts: [Uint8Array] }],
  // as Blob parts.
  function zipStored(files) {
    var encoder = new TextEncoder(), out = [], central = [], offset = 0;
    files.forEach(function (file) {
      var name = encoder.encode(file.name), crc = 0xffffffff, size = 0;
      file.parts.forEach(function (part) {
        for (var i = 0; i < part.length; i++) crc = CRC_TABLE[(crc ^ part[i]) & 255] ^ (crc >>> 8);
        size += part.length;
      });
      crc = (crc ^ 0xffffffff) >>> 0;
      var local = new DataView(new ArrayBuffer(30));
      local.setUint32(0, 0x04034b50, true); local.setUint16(4, 20, true);
      local.setUint16(6, 0x0800, true); local.setUint32(14, crc, true);
      local.setUint32(18, size, true); local.setUint32(22, size, true);
      local.setUint16(26, name.length, true);
      out.push(new Uint8Array(local.buffer), name);
      file.parts.forEach(function (part) { out.push(part); });
      var entry = new DataView(new ArrayBuffer(46));
      entry.setUint32(0, 0x02014b50, true); entry.setUint16(4, 20, true);
      entry.setUint16(6, 20, true); entry.setUint16(8, 0x0800, true);
      entry.setUint32(16, crc, true); entry.setUint32(20, size, true);
      entry.setUint32(24, size, true); entry.setUint16(28, name.length, true);
      entry.setUint32(42, offset, true);
      central.push(new Uint8Array(entry.buffer), name);
      offset += 30 + name.length + size;
    });
    var centralSize = central.reduce(function (sum, part) { return sum + part.length; }, 0);
    var end = new DataView(new ArrayBuffer(22));
    end.setUint32(0, 0x06054b50, true); end.setUint16(8, files.length, true);
    end.setUint16(10, files.length, true); end.setUint32(12, centralSize, true);
    end.setUint32(16, offset, true);
    return out.concat(central, [new Uint8Array(end.buffer)]);
  }

  root.CerebroSelectionTable = {
    // Mounts a table on `el`, or gives an existing one new rows (it stays
    // open or closed as it was).
    mount: function (el, source) {
      if (el.__cerebroSelectionTable) {
        el.__cerebroSelectionTable.setSource(source);
      } else {
        el.__cerebroSelectionTable = new SelectionTable(el, source);
      }
      return el.__cerebroSelectionTable;
    },
    numericProfile: numericProfile,
    numberText: numberText,
    numberStyle: numberStyle,
    zipStored: zipStored
  };
})(typeof window !== 'undefined' ? window : this);
