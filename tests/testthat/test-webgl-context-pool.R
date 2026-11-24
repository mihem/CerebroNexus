test_that("without WebGPU a grid of many panels keeps within the page's WebGL contexts", {
  expect_node_fixture(
    "webgl-context-pool.js",
    viewer_test_path("www", "cell_points_gpu.js"),
    viewer_test_path("www", "cell_views.js"),
    pass = "PASS the renderer releases its context without failing"
  )
})

test_that("in Chrome the WebGL2 contexts of 60 panels coming and going stay within the page's limit", {
  skip_if_no_chrome()
  browser <- chromote::ChromoteSession$new()
  on.exit(browser$close(), add = TRUE)
  evaluate <- function(code) {
    result <- browser$Runtime$evaluate(
      expression = code,
      awaitPromise = TRUE,
      returnByValue = TRUE
    )
    if (!is.null(result$exceptionDetails)) {
      stop(
        result$exceptionDetails$exception$description %||%
          result$exceptionDetails$text,
        call. = FALSE
      )
    }
    result$result$value
  }
  evaluate(paste(
    readLines(viewer_test_path("www", "cell_points_gpu.js"), warn = FALSE),
    collapse = "\n"
  ))
  ## A page keeps about 16 contexts and drops the oldest of any more: holding
  ## 15 and releasing the farthest before making another keeps every one.
  held <- evaluate(
    "(async () => {
      const probe = document.createElement('canvas');
      if (!probe.getContext('webgl2')) return null;
      const held = [];
      for (let i = 0; i < 60; i++) {
        const canvas = document.createElement('canvas');
        canvas.width = canvas.height = 50;
        document.body.append(canvas);
        held.push({
          renderer: window.CerebroPointRenderer.createWebGl(canvas),
          canvas: canvas
        });
        if (held.length > 15) {
          const old = held.shift();
          old.renderer.release();
          old.canvas.remove();
        }
      }
      await new Promise(resolve => setTimeout(resolve, 400));
      return {
        held: held.length,
        ready: held.filter(h => h.renderer.isReady()).length,
        lost: held.filter(h => h.renderer.stats().contextLost).length
      };
    })()"
  )
  skip_if(is.null(held), "This Chrome has no WebGL2.")
  expect_identical(held$held, 15L)
  expect_identical(held$ready, 15L)
  expect_identical(held$lost, 0L)
})

test_that("in Chrome 60 panels draw through one shared WebGL2 context and keep their pictures", {
  skip_if_no_chrome()
  browser <- chromote::ChromoteSession$new()
  on.exit(browser$close(), add = TRUE)
  evaluate <- function(code) {
    result <- browser$Runtime$evaluate(
      expression = code,
      awaitPromise = TRUE,
      returnByValue = TRUE
    )
    if (!is.null(result$exceptionDetails)) {
      stop(
        result$exceptionDetails$exception$description %||%
          result$exceptionDetails$text,
        call. = FALSE
      )
    }
    result$result$value
  }
  evaluate(paste(
    readLines(viewer_test_path("www", "cell_points_gpu.js"), warn = FALSE),
    collapse = "\n"
  ))
  ## Each panel's canvas is 2-D and gets the shared context's picture copied
  ## in; no panel makes a context of its own, so none is lost past ~16.
  shared <- evaluate(
    "(async () => {
      const probe = document.createElement('canvas');
      if (!probe.getContext('webgl2')) return null;
      // Two points in the lower half of the data only: a picture copied
      // upside down would show them in the upper half of the panel.
      const n = 2, positions = new Float32Array([0.25, 0.25, 0.75, 0.25]);
      const colors = new Uint8Array(4 * n).fill(255);
      for (let i = 0; i < n; i++) colors[4 * i + 1] = colors[4 * i + 2] = 0;
      const panels = [];
      for (let i = 0; i < 60; i++) {
        const canvas = document.createElement('canvas');
        canvas.style.width = canvas.style.height = '40px';
        document.body.append(canvas);
        const renderer = window.CerebroPointRenderer.createSharedWebGl(canvas);
        renderer.resize(40 + i % 3, 40, 1);
        renderer.setData({positions: positions, colors: colors,
          layers: new Uint32Array(n), count: n, foreground: false});
        panels.push({canvas: canvas, renderer: renderer,
          drawn: renderer.draw({pointSize: 8})});
      }
      await new Promise(resolve => setTimeout(resolve, 400));
      // A point at (0.25, 0.25) of the data lies a quarter in from the left
      // and a quarter up from the bottom of the panel.
      const red = panels.filter(p => {
        const x = Math.round(0.25 * p.canvas.width), y = Math.round(0.75 * p.canvas.height);
        const px = p.canvas.getContext('2d').getImageData(x, y, 1, 1).data;
        return px[0] > 200 && px[1] < 60 && px[3] > 200;
      }).length;
      const top = panels.filter(p => {
        const x = Math.round(0.25 * p.canvas.width), y = Math.round(0.25 * p.canvas.height);
        return p.canvas.getContext('2d').getImageData(x, y, 1, 1).data[3] > 0;
      }).length;
      const blank = panels[0].canvas.getContext('2d').getImageData(20, 20, 1, 1).data[3];
      const stats = panels.map(p => p.renderer.stats());
      panels[0].renderer.release();
      return {
        drawn: panels.filter(p => p.drawn).length,
        red: red,
        top: top,
        blankAlpha: blank,
        shared: stats.filter(s => s.shared && s.backend === 'webgl2').length,
        lost: stats.filter(s => s.contextLost).length,
        ready: panels.filter(p => p.renderer.isReady()).length
      };
    })()"
  )
  skip_if(is.null(shared), "This Chrome has no WebGL2.")
  expect_identical(shared$drawn, 60L)
  expect_identical(shared$shared, 60L)
  expect_identical(shared$lost, 0L)
  expect_identical(shared$red, 60L)
  expect_identical(shared$top, 0L)
  expect_identical(shared$blankAlpha, 0L)
  ## release() gives back one panel's buffers, not the shared context.
  expect_identical(shared$ready, 59L)
})

test_that("a WebGL2 panel out of the window waits for its context instead of drawing on Canvas", {
  expect_node_fixture(
    "webgl-waiting-panels.js",
    viewer_test_path("www", "cell_views.js"),
    pass = "PASS a panel out of the window waits"
  )
})
