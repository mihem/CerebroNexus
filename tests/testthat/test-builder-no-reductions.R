## A Seurat object without dimensional reductions (an unclustered Xenium
## section, an expression-only object) is added, checked, reviewed and built
## to a CRB and to an App that opens.
builder_e2e_source_runtime(environment())
builder_stage_contract_source_runtime(environment())

builder_no_reduction_object <- function(spatial = FALSE) {
  object <- .builder_test_with_seed(7L, .builder_fixture_object(24L))
  object[["umap"]] <- NULL
  if (spatial) {
    object <- .builder_fixture_add_section(
      object,
      "section_a",
      colnames(object),
      c(10, 20),
      c(96, 72)
    )
  }
  object
}

test_that("Configure shows no projection for an object without reductions", {
  model <- builder_projection_catalog_model(list(
    projection_catalog = list(),
    included_projections = character(),
    default_projection = NULL,
    initial_projections = NULL
  ))
  expect_identical(model$items, list())
  expect_identical(model$included, character())
  expect_identical(model$included_count, 0L)
  expect_null(model$default)
  expect_identical(model$initial, character())
  html <- htmltools::renderTags(builder_projection_catalog_ui(
    "core",
    model
  ))$html
  expect_type(html, "character")
})

test_that("a dataset without reductions is not missing a projection setting", {
  state <- function(reductions) {
    .builder_state_apply_output_setup(list(
      load_state = "loaded",
      readiness = "ready",
      blocking_ids = character(),
      entry = list(
        output_id = "dataset",
        profile = list(reductions = reductions),
        settings = list(
          name = "Dataset",
          groups = "sample",
          reductions = character(),
          organism = "hg",
          layer = "data",
          nUMI = "nCount_RNA",
          nGene = "nFeature_RNA"
        )
      )
    ))
  }
  expect_identical(state(character())$readiness, "ready")
  blocked <- state("umap")
  expect_identical(blocked$readiness, "blocked")
  expect_true("settings_reductions" %in% blocked$blocking_ids)
})

test_that("spatial alignment draws an FOV without any reduction", {
  skip_if_not_installed("SeuratObject")
  object <- builder_no_reduction_object(spatial = TRUE)
  object$cluster <- rep(c("a", "b"), length.out = ncol(object))
  model <- builder_alignment_preview_model(
    object,
    group = "cluster",
    layer = "counts"
  )
  expect_true(model$available, info = model$message)
  expect_null(model$projection_name)
  expect_null(model$transcriptome)
  expect_equal(nrow(model$spatial), ncol(object))
  expect_setequal(model$spatial$cell_barcode, colnames(object))
  expect_identical(
    model$spatial$group,
    object$cluster[match(model$spatial$cell_barcode, colnames(object))],
    ignore_attr = TRUE
  )
  expect_true(all(is.finite(c(model$spatial$x, model$spatial$y))))
  expect_false(is.null(model$bounds))
})

test_that("spatial alignment draws every FOV of a Xenium-like object without reductions", {
  skip_if_not_installed("SeuratObject")
  ## A full section "fov" and a "zoom" FOV that holds only part of the cells.
  object <- builder_no_reduction_object(spatial = FALSE)
  object$cluster <- rep(c("a", "b"), length.out = ncol(object))
  object <- .builder_fixture_add_section(
    object,
    "fov",
    colnames(object),
    c(0, 0),
    c(100, 100)
  )
  zoom <- colnames(object)[seq_len(10L)]
  object <- .builder_fixture_add_section(
    object,
    "zoom",
    zoom,
    c(0, 0),
    c(40, 40)
  )
  for (section in c("fov", "zoom")) {
    model <- builder_alignment_preview_model(
      object,
      group = "cluster",
      layer = "counts",
      section_id = section
    )
    expect_true(model$available, info = paste(section, model$message))
    expect_identical(model$section$id, section)
    expect_setequal(
      model$spatial$cell_barcode,
      if (section == "zoom") zoom else colnames(object)
    )
  }
})

for (spatial in c(FALSE, TRUE)) {
  local({
    spatial <- spatial
    label <- if (spatial) "with a spatial FOV" else "without spatial data"
    test_that(paste("an object without reductions builds,", label), {
      skip_if_not_installed("SeuratObject")
      object <- builder_no_reduction_object(spatial)
      expect_length(SeuratObject::Reductions(object), 0L)
      record <- list(
        id = if (spatial) "no_reduction_spatial" else "no_reduction",
        label = "No reductions",
        make = function() list(object = object, format = "Built-in example")
      )
      entry <- builder_e2e_entry(record)
      expect_identical(entry$profile$reductions %||% character(), character())
      expect_null(entry$settings$default_projection)

      root <- withr::local_tempdir()
      entry$snapshot <- builder_snapshot_seurat(
        object,
        file.path(root, "snapshot"),
        available_bytes = 2^40
      )
      release <- file.path(root, "release")
      plan <- builder_freeze_plan(
        list(entry),
        release,
        make_app = TRUE,
        app_options = list(show_upload_ui = FALSE)
      )
      expect_null(plan$error)
      handle <- builder_coordinator_prepare(plan, entry$id)
      result <- builder_execute_plan(
        plan,
        handle$stage,
        stats::setNames(list(entry$snapshot), entry$id)
      )
      expect_identical(result$state, "success", info = format(result$error))
      result$build_id <- handle$build_id
      published <- builder_coordinator_publish(handle, result)
      expect_true(isTRUE(published$published))

      crb <- readCerebro(published$built[[1L]])
      expect_length(crb$availableProjections(), 0L)
      expect_identical(length(crb$availableSpatial()) > 0L, spatial)

      config <- readRDS(file.path(published$app_dir, "cerebro_config.rds"))
      dataset <- config[[".dataset_catalog"]][[1L]]
      expect_identical(dataset$projection_names, character())
      expect_false(identical(config$initial_page, "projection"))

      ## The App opens on its first page and shows the data set.
      skip_if_not(identical(Sys.getenv("CEREBRO_RUN_BROWSER_TESTS"), "true"))
      skip_if_not_installed("shinytest2")
      port <- httpuv::randomPort(host = "127.0.0.1")
      app <- privacy_start_app(published$app_dir, port, root)
      withr::defer(privacy_stop_app(app))
      privacy_wait_for_app(app)
      driver <- shinytest2::AppDriver$new(
        app$base_url,
        name = record$id,
        load_timeout = 120000
      )
      withr::defer(try(driver$stop(), silent = TRUE))
      driver$wait_for_idle(timeout = 120000)
      driver$wait_for_js(
        paste0(
          "(function(){var n=document.querySelector('#shiny-tab-loadData');",
          "return !!(n && /24/.test(n.innerText));})()"
        ),
        timeout = 120000
      )
      ## Every page the App shows opens without an error.
      visited <- jsonlite::fromJSON(
        driver$get_js(
          "(async () => {
            const sleep = ms => new Promise(r => setTimeout(r, ms));
            const idle = async () => {
              for (let quiet = 0, i = 0; i < 600; i++) {
                const busy = document.documentElement.classList
                  .contains('shiny-busy');
                quiet = busy ? 0 : quiet + 1;
                if (quiet >= 6) return true;
                await sleep(150);
              }
              return false;
            };
            const out = [];
            const tabs = [...document.querySelectorAll(
              '.sidebar-menu a[data-value]'
            )].filter(a => a.offsetParent !== null)
              .map(a => a.getAttribute('data-value'));
            for (const tab of tabs) {
              document.querySelector(
                '.sidebar-menu a[data-value=\"' + tab + '\"]'
              ).click();
              await sleep(300);
              const ok = await idle();
              const errors = [...document.querySelectorAll(
                '.shiny-output-error:not(.shiny-output-error-validation)'
              )].filter(e => e.offsetParent !== null)
                .map(e => e.id + ': ' + e.innerText.slice(0, 150));
              const pane = document.getElementById('shiny-tab-' + tab);
              const isSettled = () => !!pane &&
                window.CerebroPageLifecycle.isSettled(pane);
              for (let i = 0; i < 100 && !isSettled(); i++) await sleep(100);
              const feedback = pane ? [...pane.querySelectorAll(
                '.cerebro-view-feedback'
              )].map(e => e.textContent.trim()).join(' | ') : '';
              out.push({tab: tab, idle: ok, settled: isSettled(),
                errors: errors.join(' | '),
                state: JSON.stringify(pane ? pane.dataset : null) +
                  ' feedback: ' + feedback});
            }
            return JSON.stringify(out);
          })()",
          timeout = 600000
        )
      )
      expect_gt(nrow(visited), 3L)
      expect_identical("spatial" %in% visited$tab, spatial)
      ## Projection and Linked views need a projection.
      expect_false(any(c("overview", "coordinated_views") %in% visited$tab))
      ## Gene expression says why it draws nothing instead of loading forever.
      expect_true(
        visited$settled[visited$tab == "geneExpression"],
        info = visited$state[visited$tab == "geneExpression"]
      )
      for (index in seq_len(nrow(visited))) {
        expect_true(visited$idle[[index]], info = visited$tab[[index]])
        expect_identical(
          visited$errors[[index]],
          "",
          info = visited$tab[[index]]
        )
      }
    })
  })
}
