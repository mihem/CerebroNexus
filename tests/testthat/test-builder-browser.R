library(shinytest2)

builder_browser_dir <- builder_profile_inst_path("builder")

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

## With scroll = TRUE the action is scrolled into view in the same task that
## measures it, so nothing can move the page in between.
builder_browser_geometry <- function(app, scroll = FALSE) {
  app$get_js(paste0(
    "(() => {",
    "const action = document.getElementById('build');",
    if (scroll) {
      "action.scrollIntoView({block: 'center', behavior: 'instant'});"
    },
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
  skip_without_installed_package()
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

  ## The Expression backend menu sits at the foot of the Advanced settings
  ## card; an open card must not clip the dropdown's second option.
  app$run_js(paste0(
    "var card = document.querySelector('.builder-viewer-advanced-settings');",
    "card.querySelector('summary').click();",
    "card.scrollIntoView({block: 'center'});",
    "document.getElementById('core-backend').selectize.open();"
  ))
  app$wait_for_js(
    paste0(
      "(function () {",
      "  var options = document.getElementById('core-backend').selectize",
      "    .$dropdown[0].querySelectorAll('.option');",
      "  var last = options[options.length - 1];",
      "  if (options.length < 2) return false;",
      "  var box = last.getBoundingClientRect();",
      "  var hit = document.elementFromPoint(box.left + 4,",
      "    box.top + box.height / 2);",
      "  return hit !== null && last.contains(hit);",
      "})()"
    ),
    timeout = 5000
  )
  app$run_js(paste0(
    "document.getElementById('core-backend').selectize.close();",
    "document.querySelector('.builder-viewer-advanced-settings ",
    "summary').click();"
  ))

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
    timeout = 30000
  )
  if (
    isTRUE(app$get_js(
      "document.getElementById('core-organism').selectize.getValue() === ''"
    ))
  ) {
    app$run_js(
      "document.getElementById('core-organism').selectize.setValue('hg');"
    )
    app$wait_for_idle(timeout = 30000)
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
    timeout = 30000
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
  app$wait_for_idle(timeout = 30000)
  app$wait_for_js(
    paste0(
      "document.querySelector('.ds[data-ds=",
      first_dataset,
      "] ",
      ".ds-ready-summary').classList.contains('is-checked') && ",
      "document.getElementById('complete_dataset_check') !== null && ",
      "document.getElementById('complete_dataset_check').textContent.trim() === 'Next' && ",
      ## The button stays disabled while the restored Spatial controls are
      ## still being saved ("Saving the latest spatial adjustment…"); a click
      ## then is ignored on purpose.
      "document.getElementById('complete_dataset_check').disabled === false"
    ),
    timeout = 30000
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
    timeout = 30000
  )

  app$get_chromote_session()$set_viewport_size(width = 1440L, height = 900L)
  app$click(selector = ".ds[data-ds=ds2] .ds-pick")
  app$wait_for_js(
    paste0(
      "document.querySelector('.ds[data-ds=ds2] .ds-pick')",
      ".getAttribute('aria-current') === 'true'"
    ),
    timeout = 30000
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
    timeout = 30000
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
    timeout = 30000
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
    timeout = 30000
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

  # Opening the Build step scrolls to the top smoothly; with reduced motion the
  # scroll finishes at once and cannot move the action while it is measured.
  app$get_chromote_session()$Emulation$setEmulatedMedia(
    features = list(list(name = "prefers-reduced-motion", value = "reduce"))
  )
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
      timeout = 30000
    )
    app$run_js(
      "document.getElementById('build').scrollIntoView({block:'center'});"
    )
    app$wait_for_js(
      "window.__builderPrimaryActionVisible === true",
      timeout = 30000
    )
    geometry <- builder_browser_geometry(app, scroll = TRUE)
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
    timeout = 30000
  )
  expect_identical(
    app$get_js("document.querySelector('.rail').getAttribute('aria-modal')"),
    "true"
  )
  app$click(selector = ".ds.is-active .builder-drop")
  app$wait_for_js(
    "document.querySelector('.builder-confirm-dialog[role=dialog]') !== null",
    timeout = 30000
  )
  expect_true(app$get_js(paste0(
    "document.querySelector('.builder-confirm-dialog').contains(",
    "document.activeElement)"
  )))
  builder_browser_escape(app)
  app$wait_for_js(
    "document.querySelector('.builder-confirm-dialog') === null",
    timeout = 30000
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
    timeout = 30000
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
  app$wait_for_js("window.__builderMotionDuration === 0", timeout = 30000)
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
    timeout = 30000
  )
  app$run_js(
    "document.getElementById('build').scrollIntoView({block:'center'});"
  )
  app$wait_for_js(
    "window.__builderPrimaryActionVisible === true",
    timeout = 30000
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
  ## Moving to the Build step announces "Opened build step." and nothing
  ## clears that one; whether it is still there depends on whether a busy or
  ## result message came after it. What must not stay is a progress message.
  app$wait_for_js(
    paste0(
      "['', 'Opened build step.'].indexOf(",
      "document.getElementById('builder-live-status').textContent.trim()) >= 0"
    ),
    timeout = 30000
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
  skip_without_installed_package()
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

## A file whose name has no extension (a `targets` object) is recognised by its
## content; one that is not a dataset ends visibly with the reason, by browser
## drop and by the desktop file dialog alike.
test_that("extensionless datasets load by content and unknown files say why", {
  skip_if_not(identical(Sys.getenv("CEREBRO_RUN_BROWSER_TESTS"), "true"))
  skip_on_os("windows")
  app_dir <- builder_browser_current_contract_app(
    builder_browser_dir,
    .local_envir = environment()
  )
  local_app_support(app_dir)
  files <- withr::local_tempdir()
  fixture <- file.path(
    builder_browser_dir,
    "fixtures",
    "complete_viewer",
    "complete_viewer_data.rds"
  )
  dropped_rds <- file.path(files, "sc_tcr")
  picked_rds <- file.path(files, "sc_tcr_picked")
  stopifnot(file.copy(fixture, dropped_rds), file.copy(fixture, picked_rds))
  dropped_garbage <- file.path(files, "notes")
  picked_garbage <- file.path(files, "notes_picked")
  writeBin(as.raw(rep(0:255, 8L)), dropped_garbage)
  file.copy(dropped_garbage, picked_garbage)

  ## The desktop dialog answers with the second pair of files.
  fake_bin <- withr::local_tempdir()
  picker_script <- c(
    "#!/bin/sh",
    sprintf(
      "printf '%%s\n' %s %s",
      shQuote(normalizePath(picked_rds)),
      shQuote(normalizePath(picked_garbage))
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
    WSL_INTEROP = NA_character_
  )

  app <- AppDriver$new(
    app_dir,
    name = "builder_extensionless_import",
    width = 1280,
    height = 900,
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)
  app$wait_for_idle(timeout = 30000)
  builder_browser_wait_for_example_ready(app)

  ## Browser drop: the files travel through the client import queue.
  app$run_js(paste0(
    "(() => { const input = document.createElement('input');",
    "input.type = 'file'; input.multiple = true; input.id = 'test_drop_files';",
    "input.hidden = true; document.body.appendChild(input); })()"
  ))
  chrome <- app$get_chromote_session()
  root <- chrome$DOM$getDocument()$root$nodeId
  chrome$DOM$setFileInputFiles(
    files = list(normalizePath(dropped_rds), normalizePath(dropped_garbage)),
    nodeId = chrome$DOM$querySelector(root, "#test_drop_files")$nodeId
  )
  app$run_js(paste0(
    "(() => { const transfer = new DataTransfer();",
    "for (const file of document.getElementById('test_drop_files').files) ",
    "transfer.items.add(file);",
    "document.body.dispatchEvent(new DragEvent('drop', ",
    "{dataTransfer: transfer, bubbles: true, cancelable: true})); })()"
  ))
  rejected_row <- paste0(
    "Array.from(document.querySelectorAll('.ds--client-upload'))",
    ".find(row => row.querySelector('.nm').textContent === 'notes')"
  )
  builder_with_browser_diagnostics(app, "browser-extensionless-drop", {
    app$wait_for_js(
      paste0(
        "(() => { const row = ",
        rejected_row,
        ";",
        "return Boolean(row) && row.dataset.loadState === 'rejected' && ",
        "Array.from(document.querySelectorAll('.ds--ready[data-ds] .nm'))",
        ".some(name => name.textContent.trim() === 'sc_tcr'); })()"
      ),
      timeout = 90000
    )
  })
  expect_identical(
    app$get_js(paste0(
      rejected_row,
      ".querySelector('.builder-import-status').textContent"
    )),
    paste0(
      "notes has no file extension and its content is not a supported ",
      "format (RDS, qs2, h5ad, h5mu, SpatialData zip). Save it with one of ",
      "these extensions: .rds, .qs2, .h5ad, .h5mu, .zip."
    )
  )
  expect_identical(
    app$get_js(paste0(
      "(() => { const row = ",
      rejected_row,
      ";",
      "return [row.querySelector('.builder-cancel-client-import') === null,",
      "row.querySelector('.builder-remove-client-failure') !== null]; })()"
    )),
    list(TRUE, TRUE)
  )
  expect_false(app$get_js(
    "document.body.textContent.includes('Waiting for the server')"
  ))

  app$run_js(paste0(
    rejected_row,
    ".querySelector('.builder-remove-client-failure').click()"
  ))

  ## Desktop dialog: the server reads the chosen paths itself.
  builder_browser_dismiss_project_offer(app)
  builder_browser_click_dynamic(app, ".btn.builder-add-datasets")
  builder_with_browser_diagnostics(app, "browser-extensionless-picker", {
    app$wait_for_js(
      paste0(
        "Array.from(document.querySelectorAll('.ds--ready[data-ds] .nm'))",
        ".some(name => name.textContent.trim() === 'sc_tcr_picked') && ",
        "Array.from(document.querySelectorAll('.builder-notice-text'))",
        ".some(text => text.textContent.startsWith(",
        "'notes_picked has no file extension'))"
      ),
      timeout = 90000
    )
  })
  builder_expect_clean_browser_logs(app)
})

## When spatial images or ROI settings require the Viewer App, "CRB files only"
## is visibly unavailable: faded, never clickable, with its reason attached.
test_that("a ruled-out output choice is faded and cannot be picked", {
  skip_if_not(identical(Sys.getenv("CEREBRO_RUN_BROWSER_TESTS"), "true"))
  app_dir <- builder_browser_current_contract_app(
    builder_browser_dir,
    .local_envir = environment()
  )
  local_app_support(app_dir)
  app <- AppDriver$new(
    app_dir,
    name = "builder_disabled_output_choice",
    width = 1280,
    height = 900,
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)
  app$wait_for_idle(timeout = 30000)
  builder_with_browser_diagnostics(app, "browser-disabled-output-build", {
    builder_browser_wait_for_example_ready(app)
    builder_browser_click_dynamic(
      app,
      ".example-btn[data-ex=complete_viewer_data]"
    )
    builder_browser_dismiss_project_offer(app)
    builder_browser_check_all_datasets(app)
    builder_browser_click_dynamic(app, "#continue_to_review")
    app$wait_for_js(
      "document.getElementById('confirm_review') !== null",
      timeout = 10000
    )
    builder_browser_click_dynamic(app, "#confirm_review")
    app$wait_for_js(
      "document.querySelector('.builder-app-required-reason') !== null",
      timeout = 30000
    )
    app$wait_for_idle(timeout = 10000)
  })
  card_state <- function() {
    app$get_js(paste0(
      "(() => { const state = value => { ",
      "const input = document.querySelector(",
      "'input[name=build_output_mode][value=' + value + ']');",
      "const card = input.closest('.radio'); const label = input.closest('label');",
      "return {disabled: input.disabled, ",
      "aria: input.getAttribute('aria-disabled'), ",
      "describedby: input.getAttribute('aria-describedby'), ",
      "opacity: getComputedStyle(label).opacity, ",
      "cursor: getComputedStyle(card).cursor, ",
      "pointer: getComputedStyle(label).pointerEvents, ",
      "checked: input.checked}; };",
      "return {crb: state('crb'), app: state('app')}; })()"
    ))
  }
  before <- card_state()
  expect_identical(
    before$crb,
    list(
      disabled = TRUE,
      aria = "true",
      describedby = "build_output_crb_reason",
      opacity = "0.45",
      cursor = "not-allowed",
      pointer = "none",
      checked = FALSE
    )
  )
  ## The available choice keeps its normal look.
  expect_identical(before$app$opacity, "1")
  expect_identical(before$app$pointer, "auto")
  expect_true(before$app$checked)
  expect_identical(
    app$get_js(paste0(
      "document.getElementById('build_output_crb_reason')",
      ".textContent.trim()"
    )),
    "Spatial images or ROI settings require CRB files + Viewer App output."
  )

  ## A real click anywhere on the card, the radio circle included, changes
  ## nothing.
  chrome <- app$get_chromote_session()
  points <- app$get_js(paste0(
    "(() => { const card = document.querySelector(",
    "'input[name=build_output_mode][value=crb]').closest('.radio');",
    "card.scrollIntoView({block: 'center'});",
    "const box = card.getBoundingClientRect();",
    "const circle = card.querySelector('input').getBoundingClientRect();",
    "return [[box.x + box.width / 2, box.y + box.height / 2],",
    "[circle.x + circle.width / 2, circle.y + circle.height / 2]]; })()"
  ))
  for (point in points) {
    for (type in c("mousePressed", "mouseReleased")) {
      chrome$Input$dispatchMouseEvent(
        type = type,
        x = point[[1L]],
        y = point[[2L]],
        button = "left",
        clickCount = 1L
      )
    }
  }
  app$wait_for_idle(timeout = 10000)
  expect_identical(app$get_value(input = "build_output_mode"), "app")
  expect_identical(card_state(), before)
  builder_expect_clean_browser_logs(app)
})
