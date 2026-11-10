test_that("late frames cannot cross dataset switches or the reconnect handshake", {
  skip_if_demo_data_missing()
  app_dir <- viewer_app_test_path()
  local_app_support(app_dir)
  app <- shinytest2::AppDriver$new(
    app_dir,
    name = "dataset_lifecycle",
    width = 1440,
    height = 1000,
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)
  app$click(selector = 'a[href="#shiny-tab-overview"]')
  ready <- "CerebroDatasetContext.phase()==='ready' && window.cerebroSpecialistViews.get('overview_projection')?.ready()"
  app$wait_for_js(ready, timeout = 60000)
  context <- function() app$get_js("CerebroDatasetContext.current()")
  first <- context()
  host <- "document.getElementById('overview_projection_cell_view_host')"
  original_count <- app$get_js(paste0("Number(", host, ".dataset.pointCount)"))
  for (dataset in c("HLA & TCR", "PBMC - Full (T+B)")) {
    previous <- context()
    app$set_inputs(crb_file_selector = dataset, wait_ = FALSE)
    app$wait_for_js(
      sprintf(
        "(%s) && CerebroDatasetContext.current().generation>%d",
        ready,
        previous$generation
      ),
      timeout = 60000
    )
    app$wait_for_idle(timeout = 30000)
    if (dataset == "HLA & TCR") middle <- context()
  }
  current <- context()
  expect_identical(current$dataset_key, first$dataset_key)
  expect_gt(current$generation, first$generation)
  expect_equal(
    app$get_js(paste0("Number(", host, ".dataset.pointCount)")),
    original_count
  )
  deliver <- function(ctx, token, count) {
    frame <- list(
      id = "overview_projection",
      dataset_context = ctx,
      render_token = token,
      meta = list(
        color_type = "categorical",
        color_variable = "probe",
        traces = list("probe"),
        group_colors = list("#ff0000")
      ),
      data = list(
        selection_key = as.list(paste0("probe-", seq_len(count))),
        x = as.list(seq_len(count)),
        y = as.list(seq_len(count)),
        group = as.list(rep("probe", count)),
        point_size = 5,
        point_opacity = 1,
        reset_axes = TRUE
      )
    )
    ## A Shiny binary message: type length, type, then the wire frame (header
    ## length, JSON header padded to 4 bytes).
    app$run_js(sprintf(
      paste0(
        "window.__frameDelivered=false;(async()=>{",
        "const e=new TextEncoder(),type=e.encode('cell_view_binary'),",
        "header=e.encode(JSON.stringify(%s)),",
        "bytes=new Uint8Array(1+type.length+4+Math.ceil(header.length/4)*4);",
        "bytes[0]=type.length;bytes.set(type,1);",
        "new DataView(bytes.buffer).setUint32(1+type.length,header.length,true);",
        "bytes.set(header,5+type.length);",
        "await Shiny.shinyapp.dispatchMessage(bytes.buffer);",
        "await new Promise(resolve=>setTimeout(resolve,250));",
        "window.__frameDelivered=true;})()"
      ),
      jsonlite::toJSON(frame, auto_unbox = TRUE, null = "null")
    ))
    app$wait_for_js("window.__frameDelivered===true")
  }
  # Both an old different dataset and an old generation of the same dataset
  # must be rejected even when their render token is larger than today's.
  for (old in list(first, middle)) {
    deliver(old, 1000000, 1L)
    expect_equal(
      app$get_js(paste0("Number(", host, ".dataset.pointCount)")),
      original_count
    )
    expect_identical(context(), current)
    expect_true(app$get_js(ready))
  }
  # Hold the real reconnect synchronization request until the pending state
  # has been observed. Then release it to the real Shiny server.
  app$run_js(
    "window.__setInputBeforeReconnect=Shiny.setInputValue;window.__heldSync=null;Shiny.setInputValue=function(name,value,opts){if(name==='viewer_dataset_context_sync'){window.__heldSync={name,value,opts};return;}return window.__setInputBeforeReconnect.apply(this,arguments);};jQuery(document).trigger('shiny:connected');"
  )
  app$wait_for_js(
    "CerebroDatasetContext.phase()==='pending' && window.__heldSync!==null"
  )
  expect_false(app$get_js(
    "window.cerebroSpecialistViews.get('overview_projection').ready()"
  ))
  expect_true(app$get_js(
    "document.querySelector('#shiny-tab-overview .cerebro-toolbar-share').disabled"
  ))
  deliver(current, 1000000, 1L)
  expect_identical(app$get_js("CerebroDatasetContext.phase()"), "pending")
  expect_false(app$get_js(
    "window.cerebroSpecialistViews.get('overview_projection').ready()"
  ))
  app$run_js(
    "Shiny.setInputValue=window.__setInputBeforeReconnect;const r=window.__heldSync;Shiny.setInputValue(r.name,r.value,r.opts);"
  )
  app$wait_for_js(ready, timeout = 30000)
  expect_identical(context(), current)
  app$wait_for_js(
    "!document.querySelector('#shiny-tab-overview .cerebro-toolbar-share').disabled"
  )
  expect_equal(
    app$get_js(paste0("Number(", host, ".dataset.pointCount)")),
    original_count
  )
  # Positive control: a current frame really reaches the renderer. A late
  # frame from the SAME generation must still not replace a newer frame.
  deliver(current, 1000001, 2L)
  app$wait_for_js(paste0(
    "Number(",
    host,
    ".dataset.pointCount)===2 && (",
    ready,
    ")"
  ))
  deliver(current, 1000000, 1L)
  expect_equal(app$get_js(paste0("Number(", host, ".dataset.pointCount)")), 2)
})
