test_that("without WebGPU a renderer that failed is replaced by a shared WebGL2 one, a few times, then Canvas", {
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

test_that("the stage observer tells the panels near the window from the far ones", {
  expect_node_fixture(
    "stage-observer.js",
    viewer_test_path("www", "cell_views.js"),
    pass = "PASS the stage observer"
  )
})

test_that("multi-panel grid draws time-slice across animation frames viewport-first", {
  expect_node_fixture(
    "grid-time-slice.js",
    viewer_test_path("www", "cell_views.js"),
    pass = "PASS grid time-sliced draw queue"
  )
})

test_that("multi-panel grids keep cards lightweight and share one GPU context", {
  expect_node_fixture(
    "grid-lightweight-cards.js",
    viewer_test_path("www", "cell_views.js"),
    pass = "PASS lightweight grid cards and shared GPU"
  )
})

test_that("in Chrome shared WebGL2 draws indexed paint order, uniform alpha, and compact Uint8 layers", {
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
  res <- evaluate(
    "(async () => {
      const probe = document.createElement('canvas');
      if (!probe.getContext('webgl2')) return null;
      const canvas = document.createElement('canvas');
      canvas.style.width = canvas.style.height = '40px';
      document.body.append(canvas);
      const renderer = window.CerebroPointRenderer.createSharedWebGl(canvas);
      renderer.resize(40, 40, 1);
      // Two overlapping points at the centre: index 0 is red, index 1 is blue.
      const positions = new Float32Array([0.5, 0.5, 0.5, 0.5]);
      const colors = new Uint8Array([255, 0, 0, 255, 0, 0, 255, 255]);
      const readCenter = () => Array.from(canvas.getContext('2d').getImageData(20, 20, 1, 1).data);
      // 1) drawOrder [0, 1] puts blue on top; null layers skips layer upload.
      renderer.setData({positions, positionsId: 1, colors, colorsId: 2,
        layers: null, layersId: 0, drawOrder: new Uint32Array([0, 1]), drawOrderId: 3,
        count: 2, foreground: false});
      renderer.draw({pointSize: 16});
      const blueTop = readCenter();
      // 2) Reorder to [1, 0] puts red on top without re-uploading positions!
      renderer.setData({positions: null, positionsId: 1, colors: null, colorsId: 2,
        layers: null, layersId: 0, drawOrder: new Uint32Array([1, 0]), drawOrderId: 4,
        count: 2, foreground: false});
      renderer.draw({pointSize: 16});
      const redTop = readCenter();
      // 3) Uniform alpha overrides opacity without re-uploading colors!
      renderer.setData({positions: new Float32Array([0.5, 0.5]), positionsId: 5,
        colors: new Uint8Array([0, 255, 0, 255]), colorsId: 6,
        layers: null, layersId: 0, count: 1, foreground: false});
      renderer.draw({pointSize: 16, alpha: {background: 0.4, foreground: 0.95}});
      const halfGreen = readCenter();
      // 4) Compact Uint8Array layers put foreground index 0 (red) above index 1 (blue).
      renderer.setData({positions, positionsId: 7, colors, colorsId: 8,
        layers: new Uint8Array([1, 0]), layersId: 9, count: 2, foreground: true});
      renderer.draw({pointSize: 16, alpha: {background: 0.05, foreground: 0.95}});
      const fgRed = readCenter();
      const stats = renderer.stats();
      renderer.release();
      return {
        drawsInOrder: stats.orderedDraw === false,
        positionUploads: stats.positionUploads,
        blueTop, redTop, halfGreen, fgRed
      };
    })()"
  )
  skip_if(is.null(res), "This Chrome has no WebGL2.")
  expect_identical(res$positionUploads, 3L)
  expect_gt(res$blueTop[[3]], 200L)
  expect_lt(res$blueTop[[1]], 50L)
  expect_gt(res$redTop[[1]], 200L)
  expect_lt(res$redTop[[3]], 50L)
  expect_gt(res$halfGreen[[2]], 200L)
  expect_gt(res$halfGreen[[4]], 70L)
  expect_lt(res$halfGreen[[4]], 140L)
  expect_gt(res$fgRed[[1]], 200L)
  expect_gt(res$fgRed[[4]], 220L)
})
