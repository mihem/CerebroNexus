test_that("Linked primary paint gates supplements and replays hidden palette changes", {
  shiny::testServer(
    function(input, output, session) {
      scope <- environment()
      sys.source(viewer_test_path("utility_functions.R"), scope)
      sys.source(viewer_test_path("coordinated_views", "bundle.R"), scope)
      context <- list(epoch = "s", dataset_key = "A", generation = 1)
      viewer_loaded_dataset_context <- function() context
      coordviews_build_log <- list2env(list(
        n = 0L,
        primary_n = 0L,
        sent_n = 0L,
        sent_primary_n = 0L,
        supplemented_primary_n = 0L,
        color_observer_started = FALSE
      ))
      builds <- full_builds <- prepared <- clone_starts <- 0L
      messages <- list()
      capture <- function(type, payload) {
        messages[[length(messages) + 1L]] <<- list(
          type = type,
          payload = payload
        )
      }
      session$sendCustomMessage <- session$sendBinaryMessage <- capture
      coordviews_primary_bundle <- reactive({
        input$dataset
        builds <<- builds + 1L
        coordviews_build_log$primary_n <- builds
        list(
          n = 2L,
          cells = c("c1", "c2"),
          dataset_id = "A",
          dataset_fingerprint = "fp",
          dataset_context = context,
          default_projection = "umap",
          canonical_order_id = "order",
          projections = list(umap = list(x = c(1, 2), y = c(3, 4))),
          groups = list(),
          spaces = list()
        )
      })
      coordviews_bundle <- reactive({
        full_builds <<- full_builds + 1L
        coordviews_build_log$n <- full_builds
        coordviews_primary_bundle()
      })
      viewerProjectionAsset <- function(...) NULL
      reactive_colors <- function() NULL
      coordviews_color_patch <- reactive(list(
        groups = list(),
        marker = input$palette
      ))
      cv_prepare_progressive_supplement <- function(
        primary,
        token,
        modalities
      ) {
        prepared <<- prepared + 1L
        list(
          primary_n = token,
          clone_spec = "clone",
          value = list(
            dataset_context = context,
            spaces = list(list(id = "slice", x = c(10, 20), y = c(30, 40))),
            groups = list(),
            fields = list(list(id = "score", values = c(2, 5)))
          )
        )
      }
      cv_start_clone_task <- function(...) clone_starts <<- clone_starts + 1L
      viewer_server_load(
        viewer_server_statements("coordinated_views", "server.R"),
        scope,
        names = c(
          "coordviews_background_ready",
          "coordviews_image_spaces",
          "coordviews_color_source",
          "coordviews_assets",
          "coordviews_clone_details",
          "coordviews_sent_primary",
          "coordviews_requested_modalities",
          "coordviews_visible",
          "cv_ok",
          "cv_send_progressive_supplement"
        ),
        events = c(
          "coordviews_visible",
          "coordviews_primary_ready",
          "coordviews_wire_fallback"
        )
      )
    },
    {
      session$setInputs(
        coordviews_wire_supported = TRUE,
        dataset = "A",
        palette = "red"
      )
      expect_equal(scope$builds, 0)
      expect_length(scope$messages, 0)
      session$setInputs(coordviews_visible = TRUE)
      expect_equal(scope$builds, 1)
      expect_equal(scope$full_builds, 0)
      expect_identical(scope$messages[[1]]$type, "coordviews_binary")
      raw <- scope$messages[[1]]$payload
      n <- sum(as.integer(raw[1:4]) * 256^(0:3))
      frame <- jsonlite::fromJSON(rawToChar(raw[4 + seq_len(n)]))
      expect_true(frame$progressive)
      expect_null(frame$cells)
      expect_equal(scope$prepared, 0)
      ready <- list(
        dataset_id = "A",
        dataset_fingerprint = "fp",
        dataset_context = context,
        progressive_token = 1L,
        modalities = "spatial"
      )
      for (bad in list(
        modifyList(ready, list(progressive_token = 99)),
        modifyList(ready, list(dataset_context = list(generation = 0))),
        modifyList(ready, list(modalities = "invalid"))
      )) {
        session$setInputs(coordviews_primary_ready = bad)
      }
      expect_equal(scope$prepared, 0)
      expect_equal(scope$clone_starts, 0)
      session$setInputs(coordviews_primary_ready = ready)
      expect_equal(scope$prepared, 1)
      expect_equal(scope$clone_starts, 1)
      expect_identical(coordviews_image_spaces()[[1]]$id, "slice")
      expect_identical(coordviews_color_source()$fields[[1]]$values, c(2, 5))
      session$setInputs(coordviews_primary_ready = c(ready, list(nonce = 2)))
      expect_equal(scope$prepared, 1)
      before <- length(scope$messages)
      session$setInputs(coordviews_visible = FALSE, palette = "blue")
      expect_length(scope$messages, before)
      session$setInputs(coordviews_visible = TRUE)
      expect_identical(tail(scope$messages, 1)[[1]]$payload$marker, "blue")
      expect_equal(scope$builds, 1)
      session$setInputs(coordviews_wire_fallback = ready)
      expect_identical(tail(scope$messages, 1)[[1]]$type, "coordviews_data")
      expect_equal(scope$full_builds, 1)
    }
  )
})

test_that("Linked controls defer genes and use emitted image presets", {
  shiny::testServer(
    function(input, output, session) {
      scope <- environment()
      reads <- 0L
      coordviews_visible <- reactive(isTRUE(input$visible))
      getGeneNames <- function() {
        reads <<- reads + 1L
        c("G2", "G1")
      }
      coordviews_bundle <- function() {
        stop("controls must not build the full bundle")
      }
      coordviews_image_spaces <- reactive(input$spaces)
      viewer_server_load(
        viewer_server_statements("coordinated_views", "server.R"),
        scope,
        names = c("coordviews_gene_names", 'output[["coordviews_image_ui"]]')
      )
    },
    {
      expect_error(coordviews_gene_names(), class = "shiny.silent.error")
      session$setInputs(visible = TRUE)
      expect_error(coordviews_gene_names(), class = "shiny.silent.error")
      expect_equal(scope$reads, 0)
      session$setInputs(coordviews_gene_controls_active = "gene")
      expect_identical(coordviews_gene_names(), c("G1", "G2"))
      expect_equal(scope$reads, 1)
      session$setInputs(spaces = list(list(id = "empty")))
      expect_null(output$coordviews_image_ui)
      preset <- list(
        offsetX = 2500,
        offsetY = -3000,
        scaleX = .05,
        scaleY = 8,
        opacity = .7,
        rotate = 25
      )
      session$setInputs(
        spaces = list(
          list(id = "empty"),
          list(
            id = "image",
            image = list(coord_span = c(10, 20), preset = preset)
          )
        )
      )
      html <- xml2::read_html(output$coordviews_image_ui$html)
      controls <- c(
        "cv-img-offx" = 2500,
        "cv-img-offy" = -3000,
        "cv-img-scalex" = .05,
        "cv-img-scaley" = 8
      )
      for (id in names(controls)) {
        node <- xml2::xml_find_first(html, paste0("//*[@id='", id, "']"))
        expect_equal(as.numeric(xml2::xml_attr(node, "value")), controls[[id]])
        expect_lte(as.numeric(xml2::xml_attr(node, "min")), controls[[id]])
        expect_gte(as.numeric(xml2::xml_attr(node, "max")), controls[[id]])
      }
    }
  )
})

test_that("Linked RGB co-expression sends the loaded dataset context", {
  shiny::testServer(
    function(input, output, session) {
      scope <- environment()
      context <- list(epoch = "s", dataset_key = "A", generation = 3)
      viewer_loaded_dataset_context <- function() context
      coordviews_visible <- reactive(TRUE)
      cv_expression_cells <- function() NULL
      getNumberOfCells <- function() 2L
      cv_gene_values <- function(gene, cells, n) c(1, 4)
      feedback <- list()
      cv_expression_feedback <- function(ok, rgb = FALSE) {
        feedback[[length(feedback) + 1L]] <<- c(ok = ok, rgb = rgb)
      }
      messages <- list()
      session$sendCustomMessage <- function(type, payload) {
        messages[[length(messages) + 1L]] <<- list(
          type = type,
          payload = payload
        )
      }
      sys.source(viewer_test_path("coordinated_views", "bundle.R"), scope)
      viewer_server_load(
        viewer_server_statements("coordinated_views", "server.R"),
        scope,
        names = c("cv_context_payload", "cv_scale_gene_values"),
        events = "coordviews_gene_r"
      )
    },
    {
      session$flushReact()
      expect_length(scope$messages, 0)
      session$setInputs(coordviews_gene_r = "G1")
      expect_length(scope$messages, 1)
      payload <- scope$messages[[1]]$payload
      expect_identical(scope$messages[[1]]$type, "coordviews_rgbval")
      expect_identical(payload$dataset_context, scope$context)
      expect_identical(as.integer(payload$r), c(64L, 255L))
      expect_identical(as.character(payload$genes), c("G1", "", ""))
      session$setInputs(coordviews_gene_r = "")
      payload <- scope$messages[[2]]$payload
      expect_false(payload$ok)
      expect_identical(payload$dataset_context, scope$context)
      expect_identical(
        scope$feedback,
        list(c(ok = TRUE, rgb = TRUE), c(ok = FALSE, rgb = TRUE))
      )
    }
  )
})
