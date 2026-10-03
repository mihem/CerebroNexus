# Publication figures

This directory contains reproducible paper-figure code. It reads a validated,
immutable benchmark run without modifying its evidence package.

Generate the Oxford Bioinformatics-ready expression-backend figure with:

```bash
RUN_ROOT=tests/bench/results/benchmark/full
RUN_DIR="$RUN_ROOT/runs/$(cat "$RUN_ROOT/CURRENT")"
OUT_DIR="paper/figures/output/$(basename "$RUN_DIR")"

Rscript paper/figures/draw_expression_backend_benchmark.R \
  "$RUN_DIR" "$OUT_DIR"
```

The script requires Arial, `pdffonts`, ggplot2, patchwork, and systemfonts. It
creates an embedded-font vector PDF, a 350 dpi LZW-compressed TIFF, a separate
figure legend, the `pdffonts` audit, and a checksum manifest tying every output
to the immutable benchmark run.

The figure reports medians and observed minimum-to-maximum ranges. It does not
show confidence intervals or significance tests because this benchmark is a
descriptive engineering study with three build and six access processes per
source/backend pair.
