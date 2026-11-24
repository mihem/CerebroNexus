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

test_that("a WebGL2 panel out of the window waits for its context instead of drawing on Canvas", {
  expect_node_fixture(
    "webgl-waiting-panels.js",
    viewer_test_path("www", "cell_views.js"),
    pass = "PASS a panel out of the window waits"
  )
})
