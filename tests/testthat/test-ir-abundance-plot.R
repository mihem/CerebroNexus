## The functions of a Viewer file, without its reactives and outputs.
ir_abundance_functions <- function(...) {
  scope <- new.env(parent = globalenv())
  scope$`%>%` <- dplyr::`%>%`
  for (file in list(...)) {
    for (expr in parse(file)) {
      if (
        is.call(expr) &&
          as.character(expr[[1]])[1] %in% c("<-", "=") &&
          is.call(expr[[3]]) &&
          identical(expr[[3]][[1]], as.name("function"))
      ) {
        eval(expr, scope)
      }
    }
  }
  scope
}

test_that("Abundance, Homeostasis and Isotype draw in the browser renderer", {
  skip_if_no_node()
  scope <- ir_abundance_functions(
    viewer_test_path("immune_repertoire", "visualizations.R"),
    viewer_test_path("immune_repertoire", "server.R")
  )
  scope$IR_ISOTYPES <- c("IGHM", "IGHG1")
  scope$cerebro_group_colors <- function(n) rep("#123456", n)
  scope$ir_homeostasis_matrix <- function(counts, clone_size) {
    matrix(
      c(.2, .5, .8, .5),
      2L,
      dimnames = list(c("s2", "s1"), c("Small (0 < X <= 1)", "Large"))
    )
  }
  counts <- do.call(
    rbind,
    lapply(seq_len(40L), function(index) {
      data.frame(
        sample = sprintf("s%02d", index),
        abundance = c(30L, 1L, 2L, 5L, 9L) * index,
        n_clones = c(1L, 40L, 12L, 5L, 2L) * 7L
      )
    })
  )
  payloads <- list(
    lines = scope$ir_clonal_abundance_chart(counts, legend = "patient"),
    density = scope$ir_clonal_abundance_chart(counts, scale = TRUE),
    homeostasis = scope$ir_homeostasis_chart(NULL, NULL, "Harmonic"),
    isotype = scope$ir_isotype_chart(
      data.frame(sample = c("a", "a"), isotype = c("IGHM", "IGHG1"), n = 1:2),
      "sample"
    ),
    empty = scope$ir_clonal_abundance_chart(counts[0, ])
  )
  ## The JSON Shiny sends: length-one vectors unboxed unless marked I().
  json <- withr::local_tempfile(fileext = ".json")
  writeLines(
    paste0(
      "{",
      paste0(
        '"',
        names(payloads),
        '":',
        vapply(payloads, function(x) shiny:::toJSON(x), character(1)),
        collapse = ","
      ),
      "}"
    ),
    json
  )
  expect_node_script(
    c(
      "const fs = require('fs'), assert = require('assert/strict');",
      "const window = {};",
      sprintf(
        "new Function('window', fs.readFileSync(%s, 'utf8'))(window);",
        encodeString(viewer_test_path("www", "cerebro-charts.js"), quote = "'")
      ),
      sprintf(
        "const p = JSON.parse(fs.readFileSync(%s, 'utf8'));",
        encodeString(json, quote = "'")
      ),
      "const charts = window.CerebroCharts;",
      "const count = (svg, re) => (svg.match(re) || []).length;",
      ## One open line per sample, on a log axis labelled in abundances.
      "let model = charts.normalize(p.lines);",
      "assert.equal(model.series.length, 40);",
      "assert.deepEqual(model.x[0], [1, 2, 5, 9, 30]);",
      "assert.equal(model.legendTitle, 'patient');",
      "let extras = {}, svg = charts.chartSvg(model, 800, 500, {}, [], false, extras);",
      "assert.equal(count(svg, /fill=\"none\"/g), 40);",
      "assert.equal(count(svg, /fill-opacity/g), 0);",
      "assert.match(svg, />1,000</);",
      "assert.match(svg, />10</);",
      "assert.doesNotMatch(svg, />NaN</);",
      ## Hover names the point nearest the pointer: sample, abundance, clones.
      "const point = /<path d=\"M([\\d.]+),([\\d.]+)L/.exec(svg);",
      "const at = [Number(point[1]) + 1, Number(point[2]) + 1];",
      "let tip = extras.hover(at[0], at[1]);",
      "assert.match(tip, /s01/);",
      "assert.match(tip, /<b>Abundance:<\\/b> 1<br>/);",
      "assert.match(tip, /<b>Clones:<\\/b> 280$/);",
      "assert.equal(extras.hover(0, 0), null);",
      ## Hidden samples are not drawn and not hovered.
      "svg = charts.chartSvg(model, 800, 500, {0: true}, [], false, extras);",
      "assert.equal(count(svg, /fill=\"none\"/g), 39);",
      "assert.doesNotMatch(extras.hover(at[0], at[1]) || '', /s01/);",
      ## Scaled: filled densities outlined in black.
      "model = charts.normalize(p.density);",
      "svg = charts.chartSvg(model, 800, 500, {}, [], false, {});",
      "assert.equal(count(svg, /fill-opacity=\"0.5\"/g), 40);",
      "assert.equal(count(svg, /stroke=\"#000000\" stroke-width=\"0.5\"/g), 40);",
      ## Stacked shares with an x title; the isotype hover adds the counts.
      "model = charts.normalize(p.homeostasis);",
      "assert.deepEqual(model.categories, ['s1', 's2']);",
      "let tips = []; svg = charts.chartSvg(model, 600, 400, {}, tips);",
      "assert.equal(count(svg, /<rect x=/g), 4);",
      "assert.match(svg, />Samples</);",
      "assert.match(tips[0], /50\\.0%/);",
      "model = charts.normalize(p.isotype);",
      "tips = []; charts.chartSvg(model, 600, 400, {}, tips);",
      "assert.match(tips[0], /IGHG1:<\\/b> 66\\.7% \\(2\\)/);",
      "model = charts.normalize(p.empty);",
      "assert.equal(model.count, 0);",
      "assert.match(model.message, /No clonotypes/);",
      ## Log ticks: 1-2-5 per decade when they fit, else powers of ten.
      paste0(
        "assert.deepEqual(charts.logTicks(0, 2, 7).values",
        ".map((v) => Math.round(10 ** v)), [1, 2, 5, 10, 20, 50, 100]);"
      ),
      "assert.deepEqual(charts.logTicks(0, 6, 4).values, [0, 2, 4, 6]);",
      "console.log('ir charts render ok');"
    ),
    pass = "ir charts render ok"
  )
})
