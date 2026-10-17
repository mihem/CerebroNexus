(page) => {
  const root = document.getElementById('shiny-tab-' + page);
  const bar = root.querySelector('.cerebro-viz-toolbar, .cv-topbar, .cerebro-groups-controls');
  const visible = e => !!e && e.getBoundingClientRect().width > 0 && e.getClientRects().length > 0;
  const fields = Array.from(bar.querySelectorAll('.cerebro-viz-primary .form-group, .cv-filters > .cv-ctl, .cerebro-groups-controls .form-group')).filter(visible);
  const boxes = fields.map(e => e.getBoundingClientRect());
  const bounds = bar.getBoundingClientRect();
  const gene = document.getElementById('expression_genes_input')?.closest('.form-group');
  const projection = document.getElementById('expression_projection_to_display')?.closest('.form-group');
  return {
    count: fields.length,
    overflow: boxes.some(r => r.left < bounds.left - 1 || r.right > bounds.right + 1) ||
      document.documentElement.scrollWidth > innerWidth + 1,
    overlap: boxes.some((a, i) => boxes.slice(i + 1).some(b =>
      Math.min(a.right, b.right) - Math.max(a.left, b.left) > 1 &&
      Math.min(a.bottom, b.bottom) - Math.max(a.top, b.top) > 1)),
    inputsFillFields: fields.every(field => {
      const control = field.querySelector('.selectize-control, .bootstrap-select, select.form-control');
      return !visible(control) || Math.abs(control.getBoundingClientRect().width - field.getBoundingClientRect().width) < 2;
    }),
    fieldRows: new Set(boxes.map(r => Math.round(r.bottom))).size,
    geneVisible: !!visible(gene),
    rgbVisible: !!visible(document.getElementById('expression_rgb_layout')?.closest('.form-group')),
    geneWidth: gene?.getBoundingClientRect().width || 0,
    projectionWidth: projection?.getBoundingClientRect().width || 0
  };
}
