test_that("a summary gate taller than four viewports still counts as visible", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  inst_dir <- viewer_app_test_path()
  suppressWarnings(shinytest2::local_app_support(inst_dir))
  app <- shinytest2::AppDriver$new(
    inst_dir,
    name = "gene_expression_summary_gate",
    height = 700,
    width = 1440,
    load_timeout = 60000
  )
  withr::defer(app$stop())
  app$click(selector = 'a[href="#shiny-tab-geneExpression"]')
  app$wait_for_js(
    "document.getElementById('expression_summary_gate') !== null",
    timeout = 60000
  )
  ## Many groups or genes make the rendered summaries this tall. Filled with
  ## the gate, the viewport shows a fifth of it, below the 0.25 ratio that a
  ## short gate needs. The spacer keeps the gate out of view at the top.
  app$run_js(paste0(
    "(() => {const gate=document.getElementById('expression_summary_gate');",
    "gate.style.minHeight=(5*window.innerHeight)+'px';",
    "const spacer=document.createElement('div');",
    "spacer.style.height=(2*window.innerHeight)+'px';",
    "gate.before(spacer);",
    "window.scrollTo(0,0);",
    "document.querySelector('.content-wrapper').scrollTop=0;})()"
  ))
  app$wait_for_js(
    "Shiny.shinyapp.$inputValues.expression_summary_viewport_request === false",
    timeout = 30000
  )
  expect_lt(
    app$get_js(
      "window.innerHeight/document.getElementById('expression_summary_gate').offsetHeight"
    ),
    0.25
  )
  viewer_show_expression_summary(app, timeout = 15000)
  app$run_js(
    "window.scrollTo(0,0);document.querySelector('.content-wrapper').scrollTop=0"
  )
  app$wait_for_js(
    "Shiny.shinyapp.$inputValues.expression_summary_viewport_request === false",
    timeout = 30000
  )
})
