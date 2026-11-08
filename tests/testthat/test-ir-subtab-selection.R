## The Immune repertoire sub-tabs an author keeps in the Builder are the ones
## the generated App shows; without a choice (older Apps and Projects) the App
## shows every tab its data supports.
builder_stage_contract_source_runtime(environment())

all_ir_tabs <- viewerImmuneRepertoireTabs()$id

test_that("one table names the sub-tabs for the Builder and the Viewer", {
  tabs <- viewerImmuneRepertoireTabs()
  expect_identical(tabs$id[[1L]], "Clonal UMAP")
  expect_identical(tabs$label[[1L]], "Clonal projection")
  expect_identical(tabs$id[tabs$min_samples > 1L], c("Compare", "SizeDist"))
  ## The Viewer builds one tab for each of them.
  viz <- paste(
    readLines(viewer_test_path("immune_repertoire", "visualizations.R")),
    collapse = "\n"
  )
  for (id in tabs$id) {
    expect_match(viz, paste0('"', id, '" = tabPanel('), fixed = TRUE)
  }

  expect_identical(viewerImmuneRepertoireTabSelection(NULL), all_ir_tabs)
  expect_identical(
    viewerImmuneRepertoireTabSelection(c("Compare", "unknown", "Abundance")),
    c("Abundance", "Compare")
  )
  options <- function(value) {
    list(viewer_content = list(A = list(immune_repertoire_tabs = value)))
  }
  visible <- viewerImmuneRepertoireVisibleTabs
  expect_identical(visible(list(), "A", 1L), all_ir_tabs[1:7])
  expect_identical(visible(list(), NULL, 3L), all_ir_tabs)
  expect_identical(
    visible(options(c("Abundance", "Compare")), "A", 3L),
    c("Abundance", "Compare")
  )
  expect_identical(
    visible(options(c("Abundance", "Compare")), "A", 1L),
    "Abundance"
  )
  ## Nothing chosen is supported: every supported tab, never an empty strip.
  expect_identical(visible(options("Compare"), "A", 1L), all_ir_tabs[1:7])
  expect_identical(visible(options("Compare"), "B", 1L), all_ir_tabs[1:7])
  ## An unknown sample count offers the single-sample tabs.
  for (count in list(NULL, NA_integer_, 0L)) {
    expect_identical(visible(list(), "A", count), all_ir_tabs[1:7])
    expect_identical(
      visible(options(c("Abundance", "Compare")), "A", count),
      "Abundance"
    )
  }
})

ir_tabs_manifest <- function(n_samples) {
  list(
    immune_repertoire = list(
      status = "valid",
      disposition = "preserved",
      evidence = list(
        detected = TRUE,
        normalized = list(
          n_rows = 40L,
          n_samples = n_samples,
          chains = c("TRA", "TRB")
        )
      )
    )
  )
}

test_that("Configure offers each sub-tab and greys out unsupported ones", {
  model <- builder_specialized_content_model(list(
    id = "ds1",
    content_manifest = ir_tabs_manifest(1L),
    immune_repertoire_tabs = c("Clonal UMAP", "Diversity", "Compare")
  ))
  tabs <- model$items[[1L]]$tabs
  expect_identical(vapply(tabs, `[[`, character(1), "id"), all_ir_tabs)
  expect_identical(
    vapply(tabs, `[[`, logical(1), "selected"),
    all_ir_tabs %in% c("Clonal UMAP", "Diversity", "Compare")
  )
  expect_identical(
    vapply(tabs, `[[`, logical(1), "available"),
    !all_ir_tabs %in% c("Compare", "SizeDist")
  )

  html <- builder_stage_html(builder_specialized_content_ui(model, "core"))
  expect_match(html, 'id="core-immune_repertoire_tabs"', fixed = TRUE)
  expect_match(html, "shiny-input-checkboxgroup", fixed = TRUE)
  expect_match(
    html,
    'value="Diversity" checked="checked"/>',
    fixed = TRUE
  )
  expect_match(
    html,
    'value="Abundance"/>',
    fixed = TRUE
  )
  expect_match(
    html,
    'value="Compare" checked="checked" disabled="disabled"/>',
    fixed = TRUE
  )
  expect_match(html, "Needs at least 2 samples", fixed = TRUE)

  ## Two samples support every tab; no choice keeps every tab.
  tabs <- builder_specialized_content_model(list(
    content_manifest = ir_tabs_manifest(2L)
  ))$items[[1L]]$tabs
  expect_true(all(vapply(tabs, `[[`, logical(1), "selected")))
  expect_true(all(vapply(tabs, `[[`, logical(1), "available")))

  ## Content left out of the CRB offers no tabs.
  excluded <- ir_tabs_manifest(2L)
  excluded$immune_repertoire$disposition <- "filtered"
  model <- builder_specialized_content_model(list(
    content_manifest = excluded
  ))
  expect_null(model$items[[1L]]$tabs)
})

test_that("the sub-tab choice keeps a supported tab and stores no choice as all", {
  entry <- list(id = "ds1", settings = list(name = "A"))
  kept <- builder_apply_immune_repertoire_tabs(
    entry,
    c("Compare", "Abundance"),
    3L
  )
  expect_null(kept$error)
  expect_identical(
    kept$entry$settings$immune_repertoire_tabs,
    c("Abundance", "Compare")
  )

  every <- builder_apply_immune_repertoire_tabs(kept$entry, all_ir_tabs, 3L)
  expect_null(every$error)
  expect_false("immune_repertoire_tabs" %in% names(every$entry$settings))

  for (value in list(NULL, character(), "Compare")) {
    refused <- builder_apply_immune_repertoire_tabs(kept$entry, value, 1L)
    expect_identical(refused$error, "Keep at least one Immune repertoire tab.")
    expect_identical(refused$entry, kept$entry)
  }
})

test_that("the sub-tab choice survives a Project save and reopen", {
  runtime <- new.env(parent = globalenv())
  sys.source(
    builder_test_inst_path("builder", "core", "bundle_path_contract.R"),
    envir = runtime
  )
  for (file in c("io.R", "project.R", "build.R")) {
    sys.source(builder_test_inst_path("builder", file), envir = runtime)
  }
  root <- withr::local_tempdir()
  manifest <- runtime$builder_project_new_manifest(root, "IR tabs")
  entry <- list(
    id = "ds1",
    revision = 1L,
    settings = list(
      name = "Repertoire",
      immune_repertoire_tabs = c("Clonal UMAP", "Compare")
    ),
    acknowledgements = character(),
    spatial_drafts = list()
  )
  manifest$datasets <- list(runtime$builder_project_dataset_record(
    entry,
    source = list(
      kind = "missing",
      path = NULL,
      fingerprint = list(md5 = strrep("a", 32L))
    ),
    root = root
  ))
  saved <- runtime$builder_project_write(manifest, root)
  restored <- runtime$builder_project_read_dataset_config(
    runtime$builder_project_read(saved$path)$datasets[[1L]],
    root
  )
  expect_identical(
    restored$settings$immune_repertoire_tabs,
    c("Clonal UMAP", "Compare")
  )
  ## A Viewer-only choice: it does not ask for a new CRB.
  changed <- entry
  changed$settings$immune_repertoire_tabs <- "Abundance"
  expect_identical(
    runtime$builder_project_artifact_configuration_digest(changed),
    runtime$builder_project_artifact_configuration_digest(entry)
  )
})

test_that("the App's tab strip shows only the kept sub-tabs", {
  viz <- viewer_server_statements("immune_repertoire", "visualizations.R")
  tabs_file <- viewer_server_statements("immune_repertoire", "tabs.R")
  strip <- function(choice, samples) {
    values <- NULL
    shiny::testServer(
      function(input, output, session) {
        scope <- environment()
        Cerebro.options <- list(
          viewer_content = list(A = list(immune_repertoire_tabs = choice))
        )
        has_scRepertoire <- function() TRUE
        ir_repertoire_available <- function() TRUE
        n_samples <- shiny::reactive(samples)
        ir_last_tab <- shiny::reactiveVal(NULL)
        viewer_requested_dataset_context <- shiny::reactive(
          list(dataset_key = "A")
        )
        viewer_server_load(
          tabs_file,
          scope,
          names = c("ir_visible_tabs", "ir_default_tab")
        )
        viewer_server_load(
          viz,
          scope,
          names = c("ir_fill_plot", "output$ir_visualizations_UI")
        )
      },
      {
        html <- output$ir_visualizations_UI$html
        values <<- list(
          ## Each tab carries its value on its link and on its pane.
          tabs = unique(regmatches(
            html,
            gregexpr('(?<=data-value=")[^"]+', html, perl = TRUE)
          )[[1L]]),
          default = shiny::isolate(ir_default_tab())
        )
      }
    )
    values
  }

  every <- strip(NULL, 3L)
  expect_identical(every$tabs, all_ir_tabs)
  expect_identical(every$default, "Clonal UMAP")
  expect_identical(strip(NULL, 1L)$tabs, all_ir_tabs[1:7])

  kept <- strip(c("Abundance", "Clone Sharing", "Compare"), 3L)
  expect_identical(kept$tabs, c("Abundance", "Clone Sharing", "Compare"))
  expect_identical(kept$default, "Abundance")
  expect_identical(
    strip(c("Abundance", "Compare"), 1L)$tabs,
    "Abundance"
  )
  expect_identical(strip("Compare", 3L)$default, "Compare")
})

test_that("an App without the Clonal projection does not point to it", {
  viz <- viewer_server_statements("immune_repertoire", "visualizations.R")
  settings <- viewer_server_statements("immune_repertoire", "settings.R")
  tabs_file <- viewer_server_statements("immune_repertoire", "tabs.R")
  ## `bound`: the strip has reached the browser; before that the status
  ## follows the tab the strip will open on.
  page <- function(choice, tab, bound = TRUE) {
    values <- NULL
    shiny::testServer(
      function(input, output, session) {
        scope <- environment()
        Cerebro.options <- list(
          viewer_content = list(A = list(immune_repertoire_tabs = choice))
        )
        has_scRepertoire <- function() TRUE
        ir_repertoire_available <- function() TRUE
        ir_settings_requested <- function() TRUE
        n_samples <- shiny::reactive(3L)
        ir_tab <- shiny::reactive(tab)
        viewer_requested_dataset_context <- shiny::reactive(
          list(dataset_key = "A")
        )
        viewer_server_load(
          tabs_file,
          scope,
          names = "ir_visible_tabs"
        )
        viewer_server_load(
          viz,
          scope,
          names = "output$ir_selection_status_UI"
        )
        viewer_server_load(
          settings,
          scope,
          names = "output$ir_group_filters_UI"
        )
      },
      {
        if (bound) {
          session$setInputs(ir_tabs = tab)
        }
        values <<- list(
          status = output$ir_selection_status_UI$html,
          filters = if (bound) output$ir_group_filters_UI$html
        )
      }
    )
    values
  }

  kept <- page(c("Clonal UMAP", "Abundance"), "Abundance")
  expect_match(kept$status, "Selection is available in Clonal projection.")
  expect_match(kept$filters, "apply to the Clonal projection tab")

  left_out <- page(c("Abundance", "Clone Sharing"), "Abundance")
  expect_false(grepl("Clonal", paste(left_out$status, collapse = "")))
  expect_false(grepl("Clonal", left_out$filters))
  ## Nor while its strip is on the way.
  early <- page(c("Abundance", "Clone Sharing"), "Abundance", bound = FALSE)
  expect_null(early$status)
})
