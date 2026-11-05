test_that("RGB seeds valid genes once and preserves edited or empty channels", {
  shiny::testServer(
    function(input, output, session) {
      viewer_runtime_env(environment())
      list_of_genes <- reactiveVal(c("A", "B", "C", "D"))
      observed <- new.env()
      observed$updates <- list()
      viewer_loaded_dataset_context <- function() {
        list(epoch = "test", dataset_key = "A", generation = 1L)
      }
      session$sendCustomMessage <- function(type, message) {
        observed$updates[[length(observed$updates) + 1L]] <- message
      }
      sys.source(
        viewer_test_path("gene_expression", "obj_selected_genes.R"),
        environment()
      )
    },
    {
      session$setInputs(
        expression_analysis_mode = "Gene(s)",
        expression_genes_input = c("missing", "b", "A", "C", "D"),
        expression_projection_genes_in_separate_panels = "separate"
      )
      expect_length(observed$updates, 0L)
      session$setInputs(expression_projection_genes_in_separate_panels = "rgb")
      expect_identical(
        unname(unlist(tail(observed$updates, 1)[[1]]$genes)),
        c("B", "A", "C")
      )
      session$setInputs(
        expression_rgb_gene_r = "D",
        expression_rgb_gene_g = "D",
        expression_rgb_gene_b = ""
      )
      expect_identical(
        expression_selected_genes_input()$rgb_genes,
        list(r = "D", g = "D", b = NULL)
      )
      observed$updates <- list()
      session$setInputs(
        expression_projection_genes_in_separate_panels = "combined"
      )
      expect_setequal(
        as.character(
          expression_selected_genes_input()$genes_to_display_present
        ),
        c("B", "A", "C", "D")
      )
      session$setInputs(expression_projection_genes_in_separate_panels = "rgb")
      expect_length(observed$updates, 0L)
      session$setInputs(expression_rgb_gene_r = "", expression_rgb_gene_g = "")
      expect_length(
        expression_selected_genes_input()$genes_to_display_present,
        0L
      )
      session$setInputs(expression_rgb_layout = "separate")
      expect_length(observed$updates, 0L)
      session$setInputs(
        expression_projection_genes_in_separate_panels = "combined",
        expression_genes_input = c("C", "D")
      )
      session$setInputs(expression_projection_genes_in_separate_panels = "rgb")
      expect_identical(
        unname(unlist(tail(observed$updates, 1)[[1]]$genes)),
        c("C", "D")
      )
    }
  )
})

test_that("RGB layout changes reuse expression rows while producing a new paint key", {
  shiny::testServer(
    function(input, output, session) {
      viewer_runtime_env(environment())
      ## Server code reads the display mode through viewerControlValue().
      expression_projection_display_mode <- function() {
        input[["expression_projection_genes_in_separate_panels"]]
      }
      sys.source(
        viewer_test_path("gene_expression", "func_expression_summary.R"),
        environment()
      )
      expression_projection_cells_to_show <- function() 1:3
      expression_projection_coordinates <- function() {
        data.frame(x = 1:3, y = 3:1)
      }
      expression_selected_genes <- function() {
        list(
          genes_to_display_present = c("A", "B"),
          rgb_genes = list(r = "A", g = "B", b = NULL)
        )
      }
      getGeneNames <- function() c("A", "B")
      data_set <- function() NULL
      sys.source(
        viewer_test_path(
          "gene_expression",
          "obj_projection_expression_levels.R"
        ),
        environment()
      )
      reads <- new.env()
      reads$count <- 0L
      expressionProjectionCachedRows <- function(...) {
        reads$count <- reads$count + 1L
        list(A = c(0, 1, 2), B = c(0, 10, 20))
      }
    },
    {
      session$setInputs(
        expression_projection_genes_in_separate_panels = "rgb",
        expression_rgb_layout = "composite"
      )
      before <- expression_projection_expression_levels()
      key <- expression_projection_render_key()
      session$setInputs(expression_rgb_layout = "separate")
      expect_identical(expression_projection_expression_levels(), before)
      expect_false(identical(expression_projection_render_key(), key))
      expect_identical(reads$count, 1L)
    }
  )
})

test_that("unsupported separate layouts explicitly identify the mean-expression fallback", {
  shiny::testServer(
    function(input, output, session) {
      expression_selected_genes <- reactive(list(
        genes_to_display_present = seq_len(input$count),
        genes_to_display_missing = character()
      ))
      expression_projection_coordinates <- reactive(matrix(
        0,
        nrow = 3,
        ncol = input$dimensions
      ))
      sys.source(
        viewer_test_path("gene_expression", "out_genes_displayed.R"),
        environment()
      )
    },
    {
      session$setInputs(
        expression_projection_genes_in_separate_panels = "separate",
        count = 10,
        dimensions = 2
      )
      expect_match(
        output$expression_genes_displayed$html,
        "Showing mean expression",
        fixed = TRUE
      )
      session$setInputs(count = 3, dimensions = 3)
      expect_match(
        output$expression_genes_displayed$html,
        "Showing mean expression",
        fixed = TRUE
      )
      session$setInputs(dimensions = 2)
      expect_false(grepl(
        "Showing mean expression",
        output$expression_genes_displayed$html,
        fixed = TRUE
      ))
    }
  )
})

test_that("RGB layouts preserve channel intensity, identity and first-paint completion", {
  viewer_browser_test("rgbLayouts")
})

## The two browser tests below each start on a new session's Gene expression
## page. They share one app process and Chrome tab; the second starts a new
## Shiny session in it, so all session state is fresh without another boot.
gene_display_app <- viewer_shared_app("gene_display_transitions", width = 1440)

test_that("four selected genes survive RGB layout and display-mode round trips", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  app <- gene_display_app()
  app$run_js(paste0(
    "window.modeErrors=[];",
    "window.addEventListener('error',e=>modeErrors.push(e.message));",
    "window.addEventListener('unhandledrejection',e=>modeErrors.push(String(e.reason)));"
  ))
  app$click(selector = 'a[href="#shiny-tab-geneExpression"]')
  viewer_set_selectize(
    app,
    "expression_genes_input",
    c("MS4A1", "CD3D", "CD79A", "CD3E")
  )
  mode <- function(value) {
    app$set_inputs(
      expression_projection_genes_in_separate_panels = value,
      wait_ = FALSE
    )
  }
  channels_js <- "['r','g','b'].map(c=>document.getElementById('expression_rgb_gene_'+c).selectize.getValue())"
  channels <- function() {
    app$get_js(channels_js)
  }
  ## The channels can settle a moment after the RGB frame on a slow runner.
  wait_channels <- function(expected) {
    app$wait_for_js(
      sprintf(
        "JSON.stringify(%s) === %s",
        channels_js,
        jsonlite::toJSON(jsonlite::toJSON(expected), auto_unbox = TRUE)
      ),
      timeout = 30000
    )
  }
  wait_panels <- function(n, mode, layout = NULL) {
    app$wait_for_js(
      sprintf(
        paste0(
          "(() => {const key=Shiny.shinyapp.$inputValues.expression_projection_rendered_state?.render_key||'';",
          "const panes=Array.from(document.querySelectorAll(",
          "'#expression_projection_cell_view_host .cv-pane:not(.cv-hidden)'));",
          "return key.startsWith(%s) && %s && panes.length===%d && panes.every(p=>",
          "Number(p.querySelector('.cv-canvas-wrap > canvas:not(.cv-mini)')?.dataset.pointCount)>0);})()"
        ),
        jsonlite::toJSON(mode, auto_unbox = TRUE),
        if (is.null(layout)) {
          "true"
        } else {
          sprintf(
            "key.endsWith(%s)",
            jsonlite::toJSON(layout, auto_unbox = TRUE)
          )
        },
        n
      ),
      timeout = 30000
    )
  }
  ## Switch immediately after selecting genes, before the first mean frame.
  mode("rgb")
  wait_panels(1L, "rgb", "composite")
  app$wait_for_js(
    "document.getElementById('expression_rgb_gene_b').selectize.getValue() === 'CD79A'",
    timeout = 30000
  )
  mode("separate")
  wait_panels(4L, "separate")
  mode("rgb")
  wait_panels(1L, "rgb", "composite")
  wait_channels(c("MS4A1", "CD3D", "CD79A"))
  expect_identical(
    unlist(channels(), use.names = FALSE),
    c("MS4A1", "CD3D", "CD79A")
  )
  app$set_inputs(expression_rgb_layout = "separate", wait_ = FALSE)
  wait_panels(3L, "rgb", "separate")
  viewer_set_selectize(app, "expression_rgb_gene_g", "MS4A1")
  viewer_set_selectize(app, "expression_rgb_gene_b", "")
  wait_panels(2L, "rgb", "separate")
  mode("combined")
  wait_panels(1L, "combined")
  expect_identical(
    unlist(app$get_js(
      "document.getElementById('expression_genes_input').selectize.getValue()"
    )),
    c("MS4A1", "CD3D", "CD79A", "CD3E")
  )
  mode("rgb")
  wait_panels(2L, "rgb", "separate")
  expect_identical(
    unlist(channels(), use.names = FALSE),
    c("MS4A1", "MS4A1", "")
  )
  app$set_inputs(expression_rgb_layout = "composite", wait_ = FALSE)
  wait_panels(1L, "rgb", "composite")
  viewer_set_selectize(app, "expression_rgb_gene_r", "")
  viewer_set_selectize(app, "expression_rgb_gene_g", "")
  app$wait_for_js(
    "!document.getElementById('expression_feedback')?.classList.contains('is-pending')",
    timeout = 30000
  )
  expect_identical(unlist(channels(), use.names = FALSE), rep("", 3L))
  mode("separate")
  wait_panels(4L, "separate")
  viewer_set_selectize(app, "expression_genes_input", c("CD3E", "CD79A"))
  wait_panels(2L, "separate")
  mode("rgb")
  wait_panels(1L, "rgb", "composite")
  wait_channels(c("CD3E", "CD79A", ""))
  expect_identical(
    unlist(channels(), use.names = FALSE),
    c("CD3E", "CD79A", "")
  )
  expect_length(app$get_js("modeErrors"), 0L)
})

test_that("genes chosen in RGB channels carry over to the gene list", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  app <- viewer_new_session(gene_display_app())
  app$click(selector = 'a[href="#shiny-tab-geneExpression"]')
  mode <- function(value) {
    app$set_inputs(
      expression_projection_genes_in_separate_panels = value,
      wait_ = FALSE
    )
  }
  genes <- function() {
    unlist(app$get_js(
      "document.getElementById('expression_genes_input').selectize.getValue()"
    ))
  }
  channels <- function() {
    unlist(app$get_js(
      "['r','g','b'].map(c=>document.getElementById('expression_rgb_gene_'+c).selectize.getValue())"
    ))
  }
  ## A user edits a channel well before switching mode; wait until the edit has
  ## been sent rather than racing Shiny's input batching.
  sent <- function(id, value) {
    app$wait_for_js(
      sprintf(
        paste0(
          "(() => {const v=Shiny.shinyapp.$inputValues;",
          "const x=v['%1$s:shiny.selectize']!==undefined?v['%1$s:shiny.selectize']:v['%1$s'];",
          "return JSON.stringify(x)===JSON.stringify(%2$s);})()"
        ),
        id,
        jsonlite::toJSON(value, auto_unbox = TRUE)
      ),
      timeout = 20000
    )
  }
  ## Let the mode selector and gene controls bind before using them.
  app$wait_for_js(
    paste0(
      "!!document.getElementById('expression_projection_genes_in_separate_panels') && ",
      "window.CerebroGeneSelector?.ready(document.getElementById('expression_genes_input')?.selectize)"
    ),
    timeout = 30000
  )
  ## Genes chosen only inside RGB become the gene list.
  mode("rgb")
  sent("expression_projection_genes_in_separate_panels", "rgb")
  viewer_set_selectize(app, "expression_rgb_gene_r", "MS4A1")
  viewer_set_selectize(app, "expression_rgb_gene_g", "CD3D")
  sent("expression_rgb_gene_g", "CD3D")
  mode("combined")
  app$wait_for_js(
    "document.getElementById('expression_genes_input').selectize.getValue().length === 2",
    timeout = 20000
  )
  expect_identical(genes(), c("MS4A1", "CD3D"))
  ## An edited channel is appended to an existing list and kept on return.
  viewer_set_selectize(
    app,
    "expression_genes_input",
    c("MS4A1", "CD3D", "CD79A")
  )
  sent("expression_genes_input", c("MS4A1", "CD3D", "CD79A"))
  mode("rgb")
  app$wait_for_js(
    "document.getElementById('expression_rgb_gene_b').selectize.getValue() === 'CD79A'",
    timeout = 20000
  )
  viewer_set_selectize(app, "expression_rgb_gene_r", "CD3E")
  sent("expression_rgb_gene_r", "CD3E")
  mode("separate")
  app$wait_for_js(
    "document.getElementById('expression_genes_input').selectize.getValue().length === 4",
    timeout = 20000
  )
  expect_identical(genes(), c("MS4A1", "CD3D", "CD79A", "CD3E"))
  mode("rgb")
  Sys.sleep(2)
  expect_identical(channels(), c("CD3E", "CD3D", "CD79A"))
})

test_that("the gene status does not resize the projection while a gene loads", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  app <- gene_display_app()
  app$click(selector = 'a[href="#shiny-tab-geneExpression"]')
  app$set_inputs(
    expression_projection_genes_in_separate_panels = "combined",
    wait_ = FALSE
  )
  rendered <- function(gene) {
    app$wait_for_js(
      sprintf(
        paste0(
          "(() => {const key=Shiny.shinyapp.$inputValues.expression_projection_rendered_state?.render_key||'';",
          "const status=document.getElementById('expression_feedback');",
          "return key.split('\u001d')[1]===%s && status && !status.classList.contains('is-pending');})()"
        ),
        jsonlite::toJSON(gene, auto_unbox = TRUE)
      ),
      timeout = 30000
    )
  }
  viewer_set_selectize(app, "expression_genes_input", "")
  rendered("")
  app$run_js(paste0(
    "window.hostHeights=[];window.statusShown=false;",
    "const host=document.getElementById('expression_projection_cell_view_host');",
    "new ResizeObserver(es=>es.forEach(e=>hostHeights.push(Math.round(e.contentRect.height)))).observe(host);",
    "const status=document.getElementById('expression_feedback');",
    "new MutationObserver(()=>{if(!status.hidden)statusShown=true;})",
    ".observe(status,{attributes:true,childList:true,characterData:true,subtree:true});"
  ))
  viewer_set_selectize(app, "expression_genes_input", "CD3D")
  rendered("CD3D")
  expect_true(app$get_js("statusShown"))
  heights <- unlist(app$get_js("hostHeights"))
  expect_gt(length(heights), 0L)
  ## No dip below the final height: the status used to shrink the plot by its own
  ## height until the frame painted, then the plot grew back and drew again.
  expect_gte(min(heights), heights[[length(heights)]] - 2L)
})
