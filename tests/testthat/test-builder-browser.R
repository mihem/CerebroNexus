library(shinytest2)

builder_browser_dir <- builder_profile_inst_path("builder")

test_that("Spatial image action label ignores alignment parameter drafts", {
  source <- paste(
    readLines(
      file.path(builder_browser_dir, "spatial_alignment_server.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  label_output <- regmatches(
    source,
    regexpr(
      paste0(
        'output\\[\\["enhance-add_image_label"\\]\\] <- ',
        'shiny::renderUI\\(\\{[\\s\\S]*?\\n  \\}\\)'
      ),
      source,
      perl = TRUE
    )
  )

  expect_match(label_output, "active_image()", fixed = TRUE)
  expect_false(grepl("draft()", label_output, fixed = TRUE))
})

test_that("metadata retention and Group actions remain independent", {
  js <- paste(
    readLines(
      file.path(builder_browser_dir, "www", "builder.js"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(js, 'action: "set-retention"', fixed = TRUE)
  expect_match(js, "retained: retained", fixed = TRUE)
  expect_match(js, 'action: "set-groups"', fixed = TRUE)
  expect_match(js, "included: included", fixed = TRUE)
  expect_match(js, "default: defaultGroup", fixed = TRUE)
})

test_that("Build conflict dialog accepts one or many existing outputs", {
  js <- paste(
    readLines(
      file.path(builder_browser_dir, "www", "builder.js"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  dialog <- regmatches(
    js,
    regexpr(
      "function showBuildDialog[\\s\\S]*?\\n  }\\n\\n  function",
      js,
      perl = TRUE
    )
  )
  expect_match(dialog, "Array.isArray(message.files)", fixed = TRUE)
  expect_match(dialog, "? [message.files]", fixed = TRUE)
  expect_match(dialog, "shown.forEach", fixed = TRUE)
})

test_that("metadata search reveals matching unavailable rows", {
  js <- paste(
    readLines(
      file.path(builder_browser_dir, "www", "builder.js"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(
    js,
    'root.querySelector(".viewer-group-unavailable")',
    fixed = TRUE
  )
  expect_match(js, "unavailable.open = true", fixed = TRUE)
  expect_match(js, "unavailable.hidden =", fixed = TRUE)
})

test_that("the live status region clears after background work settles", {
  js <- paste(
    readLines(
      file.path(builder_browser_dir, "www", "builder.js"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  announcement <- regmatches(
    js,
    regexpr(
      paste0(
        "function scheduleStatusAnnouncement[\\s\\S]*?\\n  }\\n\\n  ",
        "function updateStatusSemantics"
      ),
      js,
      perl = TRUE
    )
  )
  expect_match(announcement, "if (!next) {", fixed = TRUE)
  expect_match(announcement, "window.clearTimeout(statusTimer);", fixed = TRUE)
  expect_match(
    announcement,
    'ensureLiveRegion().textContent = "";',
    fixed = TRUE
  )
  expect_match(announcement, 'lastAnnouncement = "";', fixed = TRUE)
})

test_that("project restore summary follows each dataset selection", {
  js <- paste(
    readLines(
      file.path(builder_browser_dir, "www", "builder.js"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(js, "function syncProjectRestoreDialog(modal)", fixed = TRUE)
  expect_match(
    js,
    'event.target.matches(".builder-project-restore-row select")',
    fixed = TRUE
  )
  expect_match(js, "row.dataset.currentAction = action;", fixed = TRUE)
  expect_match(js, "counts.skipped", fixed = TRUE)
  expect_match(js, "counts.reloaded", fixed = TRUE)
  expect_match(
    js,
    "will stay unloaded in this session.",
    fixed = TRUE
  )
})

test_that("Spatial editor exposes named images and dynamic action boundaries", {
  environment <- new.env(parent = globalenv())
  shiny_exports <- getNamespaceExports("shiny")
  list2env(
    stats::setNames(
      lapply(
        shiny_exports,
        function(name) getExportedValue("shiny", name)
      ),
      shiny_exports
    ),
    envir = environment
  )
  sys.source(
    file.path(builder_browser_dir, "io.R"),
    envir = environment
  )
  sys.source(
    file.path(builder_browser_dir, "extras.R"),
    envir = environment
  )
  sys.source(
    file.path(builder_browser_dir, "ui", "enhance_stage.R"),
    envir = environment
  )
  html <- htmltools::renderTags(environment$builder_spatial_alignment_ui(
    "enhance",
    list(
      label = "Spatial alignment",
      sections = c("section_a", "section_b"),
      section_labels = c(
        section_a = "section_a · sample: S1 · 2 ROIs",
        section_b = "section_b · sample: S2 · ROI: S2_normal"
      ),
      scenes = list(
        list(
          id = "section_a",
          label = "section_a · sample: S1 · 2 ROIs",
          unit = "Spatial coordinate units",
          observations = list(kind = "cell_or_spot", count = 1200L),
          annotations = list(
            sample = list(
              field = "sample",
              count = 1L,
              values = "S1",
              truncated = FALSE
            ),
            roi = list(
              field = "sample_roi",
              count = 2L,
              values = c("lesion", "border"),
              truncated = FALSE
            )
          ),
          layers = c("points", "raster", "boundaries", "molecules")
        )
      ),
      image_choices = c(`H&E` = "H&E", DAPI = "DAPI"),
      active_image = "H&E"
    )
  ))$html
  minimal_html <- htmltools::renderTags(
    environment$builder_spatial_scene_inventory_ui(list(list(id = "section_b")))
  )$html
  image_html <- htmltools::renderTags(
    environment$builder_tissue_image_file_ui(
      "enhance",
      list(source = list(name = "section.png", type = "image/png", size = 8))
    )
  )$html

  for (text in c(
    "section_a · sample: S1 · 2 ROIs",
    "section_b · sample: S2 · ROI: S2_normal",
    "ROI view",
    "All ROIs",
    "Separate ROIs",
    "Scene inventory",
    "1,200 observations",
    "Samples (sample): S1",
    "ROIs (sample_roi): lesion, border",
    "points",
    "raster",
    "boundaries",
    "molecules",
    "Molecule export is capped at 200,000 records",
    "H&amp;E",
    "DAPI"
  )) {
    expect_match(html, text, fixed = TRUE)
  }
  expect_match(html, 'id="enhance-add_image_label"', fixed = TRUE)
  expect_match(html, 'id="enhance-alignment_status"', fixed = TRUE)
  expect_match(
    image_html,
    'class="enhance-tissue-file-extension">.png<',
    fixed = TRUE
  )
  expect_false(grepl("PNG ·", image_html, fixed = TRUE))
  expect_false(grepl(">Added<", image_html, fixed = TRUE))
  expect_false(grepl(">Spatial space<", html, fixed = TRUE))
  expect_false(grepl("alignment_spatial_label", html, fixed = TRUE))
  expect_match(
    image_html,
    paste0(
      'enhance-tissue-file-actions[\\s\\S]*?>Rename image<',
      '[\\s\\S]*?>Remove image<[\\s\\S]*?</div>'
    ),
    perl = TRUE
  )
  expect_match(
    html,
    'Image settings[\\s\\S]*?id="enhance-reset_align"[\\s\\S]*?</details>',
    perl = TRUE
  )
  expect_match(html, ">Reset image<", fixed = TRUE)
  expect_false(grepl('id="enhance-rename_image"', html, fixed = TRUE))
  expect_false(grepl('id="enhance-drop_image"', html, fixed = TRUE))
  expect_false(grepl(
    "builder-spatial-scene-annotation",
    minimal_html,
    fixed = TRUE
  ))
  expect_false(grepl("Molecule export is capped", minimal_html, fixed = TRUE))

  css <- paste(
    readLines(
      file.path(builder_browser_dir, "www", "builder.features.css"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(
    css,
    paste0(
      ".enhance-tissue-file-actions {\n",
      "  display: flex;\n",
      "  width: 100%;"
    ),
    fixed = TRUE
  )
  expect_match(
    css,
    paste0(
      ".enhance-tissue-file-item {\n",
      "  width: 100%;\n",
      "  grid-template-columns: minmax(0, 1fr);\n",
      "  grid-template-rows: auto auto;"
    ),
    fixed = TRUE
  )
  expect_match(
    css,
    paste0(
      ".spatial-image-file-summary .enhance-tissue-file-meta {\n",
      "  display: flex;\n",
      "  align-items: baseline;"
    ),
    fixed = TRUE
  )
  expect_match(
    css,
    paste0(
      ".spatial-image-file-summary .enhance-tissue-file-name {\n",
      "  flex: 1 1 auto;\n",
      "  font-size: var(--builder-type-small);"
    ),
    fixed = TRUE
  )
  expect_match(
    css,
    paste0(
      ".spatial-image-file-summary .builder-file-list--single {\n",
      "  max-height: none;\n",
      "  overflow: visible;\n",
      "  overscroll-behavior: auto;\n",
      "}"
    ),
    fixed = TRUE
  )
  expect_match(
    css,
    paste0(
      ".spatial-image-flips .checkbox label {\n",
      "  display: flex;\n",
      "  justify-content: center;\n",
      "  align-items: center;"
    ),
    fixed = TRUE
  )
  expect_match(
    css,
    ".builder-spatial-scene-inventory {\n  max-height: 18rem;",
    fixed = TRUE
  )
  expect_match(
    css,
    ".spatial-alignment-figure {\n    display: grid;\n    grid-template-rows: minmax(0, 1fr) auto;",
    fixed = TRUE
  )

  server <- paste(
    readLines(
      file.path(builder_browser_dir, "spatial_alignment_server.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_false(grepl("alignment_spatial_label", server, fixed = TRUE))
})

builder_browser_mock_folder_picker <- function(
  output_dir,
  .local_envir = parent.frame()
) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  fake_bin <- withr::local_tempdir(.local_envir = .local_envir)
  picker_script <- c(
    "#!/bin/sh",
    sprintf(
      "printf '%%s\\n' %s",
      shQuote(normalizePath(output_dir, winslash = "/", mustWork = TRUE))
    )
  )
  pickers <- file.path(fake_bin, c("osascript", "zenity", "kdialog"))
  for (picker in pickers) {
    writeLines(picker_script, picker)
  }
  Sys.chmod(pickers, mode = "0755")
  withr::local_envvar(
    PATH = paste(fake_bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    WSL_DISTRO_NAME = NA_character_,
    WSL_INTEROP = NA_character_,
    .local_envir = .local_envir
  )
  invisible(output_dir)
}

test_that("browser folder picker mock covers macOS and Linux commands", {
  skip_on_os("windows")
  withr::local_envvar(WSL_DISTRO_NAME = "test", WSL_INTEROP = "/test/interop")
  output_dir <- file.path(withr::local_tempdir(), "build-output")
  builder_browser_mock_folder_picker(output_dir)

  pickers <- c("osascript", "zenity", "kdialog")
  expect_true(all(nzchar(Sys.which(pickers))))
  expect_identical(
    trimws(system2("zenity", stdout = TRUE)),
    normalizePath(output_dir, winslash = "/", mustWork = TRUE)
  )
  runtime <- new.env(parent = globalenv())
  sys.source(file.path(builder_browser_dir, "io.R"), envir = runtime)
  picker <- runtime$builder_native_picker_spec(
    "output_directory",
    .system = "Linux"
  )
  expect_identical(basename(picker$command), "zenity")
})

builder_browser_escape <- function(app) {
  app$run_js(paste0(
    "document.activeElement.dispatchEvent(new KeyboardEvent('keydown',",
    "{key:'Escape',bubbles:true}));"
  ))
}

builder_browser_geometry <- function(app) {
  app$get_js(paste0(
    "(() => {",
    "const action = document.getElementById('build');",
    "const box = action.getBoundingClientRect();",
    "const rail = document.querySelector('.rail');",
    "const summary = document.querySelector('.rail-summary');",
    "const main = document.querySelector('.builder-content');",
    "return {",
    "width: window.innerWidth,",
    "documentWidth: document.documentElement.scrollWidth,",
    "pageHeight: document.documentElement.scrollHeight,",
    "railVisible: getComputedStyle(rail).display !== 'none',",
    "summaryVisible: getComputedStyle(summary).display !== 'none',",
    "mainWidth: main.getBoundingClientRect().width,",
    "workflowCopyVisible: Array.from(document.querySelectorAll(",
    "'.builder-workflow-step-copy')).some(node => ",
    "getComputedStyle(node).display !== 'none'),",
    "actionTop: box.top, actionBottom: box.bottom,",
    "viewportHeight: window.innerHeight,",
    "position: getComputedStyle(action).position,",
    "primaryVisible: window.__builderPrimaryActionVisible === true",
    "};",
    "})()"
  ))
}

test_that("builder interaction reflows and preserves accessible state", {
  app_dir <- builder_browser_current_contract_app(
    builder_browser_dir,
    .local_envir = environment()
  )
  local_app_support(app_dir)
  output_dir <- file.path(
    tempdir(),
    paste0("builder-browser-result-", Sys.getpid())
  )
  builder_browser_mock_folder_picker(output_dir)
  withr::defer(unlink(output_dir, recursive = TRUE, force = TRUE))
  app <- AppDriver$new(
    app_dir,
    name = "builder_accessible_interaction",
    width = 1280,
    height = 900,
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)
  app$wait_for_idle(timeout = 30000)

  expect_identical(
    app$get_js("document.documentElement.lang"),
    "en"
  )

  builder_with_browser_diagnostics(app, "browser-accessible-load-example", {
    builder_browser_wait_for_example_ready(app)
    app$click(selector = ".example-btn[data-ex=complete_viewer_data]")
    app$wait_for_js(
      paste0(
        "document.querySelectorAll('.ds.ds--ready[data-ds]').length === 3 && ",
        "document.querySelector('.ds-pick[aria-current=true]') !== null && ",
        "document.querySelector('[aria-current=step]') !== null && ",
        "document.getElementById('complete_dataset_check') !== null"
      ),
      timeout = 60000
    )
    app$wait_for_idle(timeout = 30000)
  })
  builder_browser_dismiss_project_offer(app)
  expect_false(app$get_js(
    "getComputedStyle(document.body).overflowY === 'hidden'"
  ))

  expect_false(app$get_js(
    "document.querySelector('.builder-select-initial') !== null"
  ))
  expect_true(app$get_js(paste0(
    "document.querySelector('.enhance-tissue-file-button i')",
    ".closest('[aria-hidden=true]') !== null"
  )))
  expect_true(app$get_js(paste0(
    "document.querySelector('.enhance-table-add-button i')",
    ".closest('[aria-hidden=true]') !== null"
  )))
  expect_false(app$get_js(
    "document.querySelector('.builder-duplicate') !== null"
  ))
  expect_true(app$get_js(paste0(
    "Array.from(document.querySelectorAll('.enhance-module details'))",
    ".every(node => node.open === false)"
  )))
  expect_true(app$get_js(paste0(
    "Array.from(document.querySelectorAll('.builder-stage-review details'))",
    ".every(node => node.open === false)"
  )))

  ## Exercise the real browser binding across every label transition. The
  ## Configure footer is rendered dynamically; a Shiny actionButton would
  ## reset its client counter when this DOM is replaced and could silently
  ## drop the next click after restoring or revisiting a checked dataset.
  first_dataset <- app$get_js(
    "document.querySelector('.ds-pick[aria-current=true]').closest('.ds').dataset.ds"
  )
  app$wait_for_js(
    paste0(
      "document.getElementById('complete_dataset_check') !== null && ",
      "document.getElementById('complete_dataset_check').disabled === false && ",
      "document.getElementById('complete_dataset_check').textContent.trim() === ",
      "'Finish Checking'"
    ),
    timeout = 10000
  )
  if (
    isTRUE(app$get_js(
      "document.getElementById('core-organism').selectize.getValue() === ''"
    ))
  ) {
    app$run_js(
      "document.getElementById('core-organism').selectize.setValue('hg');"
    )
    app$wait_for_idle(timeout = 10000)
  }
  app$click(selector = "#complete_dataset_check")
  app$wait_for_js(
    sprintf(
      paste0(
        "document.querySelector('.ds[data-ds=%s] ",
        ".ds-ready-summary').classList.contains('is-checked') && ",
        "document.querySelector(",
        "'.ds-pick[aria-current=true]').closest('.ds').dataset.ds !== %s"
      ),
      encodeString(first_dataset, quote = '"'),
      encodeString(first_dataset, quote = '"')
    ),
    timeout = 10000
  )
  app$click(
    selector = sprintf(
      ".ds[data-ds=%s] .ds-pick",
      encodeString(first_dataset, quote = '"')
    )
  )
  ## Let the restored Spatial controls complete their server round-trip before
  ## asserting the check mark. Integer slider values (for example `5L`) and
  ## JSON numeric values (`5`) are semantically identical and must not rewrite
  ## `spatial_point_appearance` or invalidate the checked identity.
  app$wait_for_idle(timeout = 10000)
  app$wait_for_js(
    paste0(
      "document.querySelector('.ds[data-ds=",
      first_dataset,
      "] ",
      ".ds-ready-summary').classList.contains('is-checked') && ",
      "document.getElementById('complete_dataset_check') !== null && ",
      "document.getElementById('complete_dataset_check').textContent.trim() === 'Next'"
    ),
    timeout = 10000
  )
  app$click(selector = "#complete_dataset_check")
  app$wait_for_js(
    sprintf(
      paste0(
        "document.querySelector('.ds-pick[aria-current=true]')",
        ".closest('.ds').dataset.ds !== %s"
      ),
      encodeString(first_dataset, quote = '"')
    ),
    timeout = 10000
  )

  app$get_chromote_session()$set_viewport_size(width = 1440L, height = 900L)
  app$click(selector = ".ds[data-ds=ds2] .ds-pick")
  app$wait_for_js(
    paste0(
      "document.querySelector('.ds[data-ds=ds2] .ds-pick')",
      ".getAttribute('aria-current') === 'true'"
    ),
    timeout = 10000
  )
  app$run_js(paste0(
    "const details = document.querySelector(",
    "'.builder-viewer-spatial-alignment');",
    "details.open = true; details.scrollIntoView({block:'start'});"
  ))
  app$wait_for_js(
    paste0(
      "document.querySelector('.builder-spatial-alignment-body')",
      ".getBoundingClientRect().height > 0 && ",
      "document.querySelector('.builder-spatial-canvas-summary')",
      ".textContent.includes('sampled points')"
    ),
    timeout = 10000
  )
  expect_match(
    app$get_js(
      "document.querySelector('.ds-pick[aria-current=true]').textContent"
    ),
    "Trekker spatial demo",
    fixed = TRUE
  )
  expect_false(app$get_js(
    "document.querySelector('.enhance-tissue-file-item') !== null"
  ))
  spatial_height <- app$get_js(paste0(
    "document.querySelector('.builder-spatial-alignment-body')",
    ".getBoundingClientRect().height"
  ))
  expect_lte(spatial_height, 900)
  app$run_js(
    "document.querySelector('.builder-viewer-spatial-alignment').open = false;"
  )
  app$get_chromote_session()$set_viewport_size(width = 1280L, height = 900L)

  builder_browser_check_all_datasets(app)
  app$wait_for_js(
    "document.getElementById('continue_to_review') !== null",
    timeout = 10000
  )
  readiness_text <- app$get_js(
    "document.querySelector('.builder-configure-readiness').textContent"
  )
  selectize_live_regions <- app$get_js(paste0(
    "document.querySelectorAll(",
    "'body > [role=log][aria-live=assertive]').length"
  ))
  expect_gt(selectize_live_regions, 0L)
  expect_false(
    app$get_js(
      "document.getElementById('continue_to_review').matches(':disabled')"
    ),
    info = readiness_text
  )
  app$click(selector = "#continue_to_review")
  app$wait_for_js(
    paste0(
      "document.getElementById('review-stage') !== null && ",
      "document.querySelectorAll(",
      "'body > [role=log][aria-live=assertive]').length === 0"
    ),
    timeout = 10000
  )
  review_text <- app$get_js(
    "document.getElementById('review-stage').textContent"
  )
  expect_match(review_text, "Creates CRB files", fixed = TRUE)
  expect_match(review_text, "3 datasets", fixed = TRUE)
  expect_false(grepl("Creates Shiny App", review_text, fixed = TRUE))
  expect_false(grepl(
    "Creates CRB files + private App",
    review_text,
    fixed = TRUE
  ))
  expect_true(app$get_js(
    "document.querySelector('#review-stage .review-summary-strip') !== null"
  ))
  expect_false(app$get_js(paste0(
    "document.querySelector('#review-stage input:not([type=hidden]), ",
    "#review-stage select, #review-stage textarea') !== null"
  )))
  app$click("confirm_review")
  app$wait_for_js(
    paste0(
      "document.querySelector('[data-workflow-stage=build]') !== null && ",
      "document.getElementById('build') !== null && ",
      "!document.getElementById('build').disabled"
    ),
    timeout = 30000
  )
  expect_false(app$get_js("document.querySelector('.result-card') !== null"))

  geometry_by_width <- list()
  for (viewport in list(
    c(1920L, 1080L),
    c(1280L, 900L),
    c(768L, 800L),
    c(390L, 844L),
    c(320L, 720L)
  )) {
    app$get_chromote_session()$set_viewport_size(
      width = viewport[[1]],
      height = viewport[[2]]
    )
    app$wait_for_js(
      sprintf(
        "window.innerWidth === %d && window.innerHeight === %d",
        viewport[[1]],
        viewport[[2]]
      ),
      timeout = 10000
    )
    app$run_js(
      "document.getElementById('build').scrollIntoView({block:'center'});"
    )
    app$wait_for_js(
      "window.__builderPrimaryActionVisible === true",
      timeout = 10000
    )
    geometry <- builder_browser_geometry(app)
    geometry_by_width[[as.character(viewport[[1]])]] <- geometry
    expect_lte(geometry$documentWidth, geometry$width + 1)
    expect_gte(geometry$pageHeight, geometry$viewportHeight)
    expect_identical(geometry$position, "static")
    expect_gte(geometry$actionTop, 0)
    expect_lte(geometry$actionBottom, geometry$viewportHeight)

    expect_identical(geometry$summaryVisible, viewport[[1]] <= 928L)
    expect_identical(geometry$railVisible, viewport[[1]] > 928L)
    expect_identical(
      geometry$workflowCopyVisible,
      viewport[[1]] > 640L
    )
  }
  expect_gte(geometry_by_width[["768"]]$mainWidth, 768L * 0.9)
  expect_lte(geometry_by_width[["768"]]$mainWidth, 768L)
  expect_true(geometry_by_width[["1280"]]$railVisible)
  expect_true(geometry_by_width[["1920"]]$railVisible)

  app$click(selector = ".rail-summary")
  app$wait_for_js(
    "document.querySelector('.rail.is-manager-open[role=dialog]') !== null",
    timeout = 10000
  )
  expect_identical(
    app$get_js("document.querySelector('.rail').getAttribute('aria-modal')"),
    "true"
  )
  app$click(selector = ".ds.is-active .builder-drop")
  app$wait_for_js(
    "document.querySelector('.builder-confirm-dialog[role=dialog]') !== null",
    timeout = 10000
  )
  expect_true(app$get_js(paste0(
    "document.querySelector('.builder-confirm-dialog').contains(",
    "document.activeElement)"
  )))
  builder_browser_escape(app)
  app$wait_for_js(
    "document.querySelector('.builder-confirm-dialog') === null",
    timeout = 10000
  )
  expect_true(app$get_js(
    paste0(
      "document.activeElement.classList.contains('builder-drop') || ",
      "document.activeElement.classList.contains('rail-manager-close')"
    )
  ))
  builder_browser_escape(app)
  app$wait_for_js(
    "!document.querySelector('.rail').classList.contains('is-manager-open')",
    timeout = 10000
  )
  expect_true(app$get_js(
    "document.activeElement.classList.contains('rail-summary')"
  ))
  expect_identical(
    app$get_js("document.querySelector('.rail').getAttribute('aria-hidden')"),
    "true"
  )

  app$get_chromote_session()$Emulation$setEmulatedMedia(
    features = list(list(name = "prefers-reduced-motion", value = "reduce"))
  )
  app$wait_for_js("window.__builderMotionDuration === 0", timeout = 10000)
  expect_identical(
    app$get_js(
      "getComputedStyle(document.querySelector('.ds')).transitionDuration"
    ),
    "0s"
  )

  app$get_chromote_session()$Emulation$setDeviceMetricsOverride(
    width = 320,
    height = 720,
    deviceScaleFactor = 4,
    mobile = FALSE
  )
  app$wait_for_js(
    "window.innerWidth === 320 && window.devicePixelRatio === 4",
    timeout = 10000
  )
  app$run_js(
    "document.getElementById('build').scrollIntoView({block:'center'});"
  )
  app$wait_for_js(
    "window.__builderPrimaryActionVisible === true",
    timeout = 10000
  )

  expect_true(app$get_js(
    "document.getElementById('builder-live-status') !== null"
  ))
  expect_identical(
    app$get_js(
      "document.getElementById('builder-live-status').getAttribute('aria-live')"
    ),
    "polite"
  )
  app$wait_for_js(
    "document.getElementById('builder-live-status').textContent.trim() === ''",
    timeout = 10000
  )
})

test_that("duplicate spatial image names open a usable naming modal", {
  skip_if_not(identical(Sys.getenv("CEREBRO_RUN_BROWSER_TESTS"), "true"))
  app_dir <- builder_browser_current_contract_app(
    builder_browser_dir,
    .local_envir = environment()
  )
  local_app_support(app_dir)

  image_path <- tempfile("builder-modal-image-", fileext = ".png")
  png::writePNG(
    array(
      rep(c(0.12, 0.42, 0.82), each = 16L * 16L),
      dim = c(16L, 16L, 3L)
    ),
    image_path
  )
  withr::defer(unlink(image_path, force = TRUE))

  app <- AppDriver$new(
    app_dir,
    name = "builder_duplicate_spatial_image_modal",
    width = 1280,
    height = 900,
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)
  app$wait_for_idle(timeout = 30000)
  builder_with_browser_diagnostics(app, "browser-spatial-modal-load-example", {
    builder_browser_wait_for_example_ready(app)
    app$click(selector = ".example-btn[data-ex=complete_viewer_data]")
    app$wait_for_js(
      "document.getElementById('enhance-tissue_image_file') !== null",
      timeout = 60000
    )
  })
  builder_browser_dismiss_project_offer(app)
  initial_images <- unlist(
    app$get_js(paste0(
      "Object.keys(document.getElementById(",
      "'enhance-active_image').selectize.options).sort()"
    )),
    use.names = FALSE
  )
  initial_image_count <- length(initial_images)

  builder_with_browser_diagnostics(app, "browser-spatial-modal-first-upload", {
    app$upload_file(`enhance-tissue_image_file` = image_path)
    app$wait_for_js(
      sprintf(
        paste0(
          "Object.keys(document.getElementById(",
          "'enhance-active_image').selectize.options).length === %d && ",
          "document.querySelector('.enhance-tissue-file-item') !== null"
        ),
        initial_image_count + 1L
      ),
      timeout = 30000
    )
  })
  section <- app$get_js(
    "document.getElementById('enhance-active_section').value"
  )
  app$upload_file(`enhance-tissue_image_file` = image_path)
  tryCatch(
    app$wait_for_js(
      paste0(
        "(() => { const modal = document.getElementById('shiny-modal'); ",
        "const label = document.getElementById('enhance-new_image_label'); ",
        "const confirm = document.getElementById('enhance-add_image_confirm'); ",
        "return modal && (modal.classList.contains('show') || ",
        "modal.classList.contains('in')) && label && confirm && ",
        "label.classList.contains('shiny-bound-input') && ",
        "confirm.classList.contains('shiny-bound-input') && ",
        "confirm.offsetParent !== null && ",
        "modal.textContent.includes('Name this image'); })()"
      ),
      timeout = 10000
    ),
    error = function(error) {
      logs <- app$get_logs()
      browser_failures <- logs[
        as.character(logs$location) == "chromote" &
          tolower(as.character(logs$level)) %in%
            c("error", "warning", "assert", "throw"),
        ,
        drop = FALSE
      ]
      stop(
        paste(
          conditionMessage(error),
          paste(browser_failures$message, collapse = "\n"),
          sep = "\n"
        ),
        call. = FALSE
      )
    }
  )
  app$set_inputs(`enhance-new_image_label` = "Repeated tissue image")
  app$wait_for_idle(timeout = 10000)
  expect_identical(
    app$get_js("document.getElementById('enhance-new_image_label').value"),
    "Repeated tissue image"
  )
  app$click("enhance-add_image_confirm")
  app$wait_for_js(
    "document.getElementById('shiny-modal') === null",
    timeout = 10000
  )
  app$wait_for_js(
    sprintf(
      paste0(
        "Object.keys(document.getElementById(",
        "'enhance-active_image').selectize.options).length === %d"
      ),
      initial_image_count + 2L
    ),
    timeout = 30000
  )
  expect_identical(
    app$get_js("document.getElementById('enhance-active_section').value"),
    section
  )
  expect_setequal(
    unlist(
      app$get_js(
        paste0(
          "Object.keys(document.getElementById(",
          "'enhance-active_image').selectize.options).sort()"
        )
      ),
      use.names = FALSE
    ),
    c(initial_images, basename(image_path), "Repeated tissue image")
  )
  builder_expect_clean_browser_logs(app)
})

test_that("builder explains a mocked old privacy contract exactly", {
  old_contract_app <- builder_browser_old_contract_app(
    builder_browser_dir,
    .local_envir = environment()
  )
  local_app_support(old_contract_app)
  app <- AppDriver$new(
    old_contract_app,
    name = "builder_old_privacy_contract",
    width = 768,
    height = 800,
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)
  app$wait_for_idle(timeout = 30000)
  builder_browser_wait_for_example_ready(app)
  builder_browser_click_dynamic(
    app,
    ".example-btn[data-ex=complete_viewer_data]"
  )
  builder_browser_dismiss_project_offer(app)
  builder_browser_check_all_datasets(app)
  app$wait_for_js(
    "document.getElementById('continue_to_review') !== null",
    timeout = 10000
  )
  builder_browser_click_dynamic(app, "#continue_to_review")
  app$wait_for_js(
    "document.getElementById('confirm_review') !== null",
    timeout = 10000
  )
  builder_browser_click_dynamic(app, "#confirm_review")
  app$wait_for_js(
    "document.querySelector('.builder-app-capability-reason') !== null",
    timeout = 10000
  )
  expect_true(app$get_js(
    "document.querySelector('input[name=build_output_mode][value=app]').matches(':disabled')"
  ))
  expect_identical(
    app$get_js(
      "document.querySelector('.builder-app-capability-title').textContent.trim()"
    ),
    "Viewer App unavailable"
  )
  expect_match(
    app$get_js(
      "document.querySelector('.builder-app-capability-reason').textContent.trim()"
    ),
    "secure Viewer App export runtime"
  )
  expect_identical(
    app$get_js(
      "document.querySelector('.builder-app-capability-command') === null"
    ),
    TRUE
  )
})
