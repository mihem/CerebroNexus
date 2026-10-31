builder_browser_contract_app <- function(
  app_dir,
  contract_version,
  .local_envir = parent.frame()
) {
  stopifnot(contract_version %in% c(0L, 1L))
  app_dir <- normalizePath(app_dir, winslash = "/", mustWork = TRUE)
  source_root <- normalizePath(
    file.path(app_dir, "..", ".."),
    winslash = "/",
    mustWork = TRUE
  )
  source_available <- file.exists(file.path(source_root, "DESCRIPTION")) &&
    dir.exists(file.path(source_root, "R"))
  withr::local_envvar(
    CEREBRO_PACKAGE_SOURCE = if (source_available) {
      source_root
    } else {
      NA_character_
    },
    .local_envir = .local_envir
  )
  fixture_root <- tempfile("builder-contract-")
  fixture_dir <- file.path(fixture_root, "builder")
  viewer_dir <- file.path(dirname(app_dir), "viewer")
  if (!dir.exists(viewer_dir)) {
    stop("Builder Viewer support is unavailable.", call. = FALSE)
  }
  dir.create(fixture_root, recursive = TRUE)
  fs::dir_copy(app_dir, fixture_dir)
  fs::dir_copy(viewer_dir, file.path(fixture_root, "viewer"))
  withr::defer(
    unlink(fixture_root, recursive = TRUE, force = TRUE),
    envir = .local_envir
  )

  prerequisite <- file.path(fixture_dir, "prerequisite.R")
  writeLines(
    c(
      readLines(prerequisite, warn = FALSE),
      "",
      "## Test fixture: fix the active Viewer contract in this process.",
      sprintf(
        "builder_installed_app_contract_version <- function(...) %dL",
        as.integer(contract_version)
      ),
      sprintf(
        "builder_source_app_contract_version <- function(...) %dL",
        as.integer(contract_version)
      )
    ),
    prerequisite
  )
  fixture_dir
}

builder_browser_current_contract_app <- function(
  app_dir,
  .local_envir = parent.frame()
) {
  builder_browser_contract_app(app_dir, 1L, .local_envir)
}

builder_browser_old_contract_app <- function(
  app_dir,
  .local_envir = parent.frame()
) {
  builder_browser_contract_app(app_dir, 0L, .local_envir)
}

builder_browser_project_folder_picker <- function(
  project_dir,
  .local_envir = parent.frame()
) {
  skip_on_os("windows")
  dir.create(project_dir, recursive = TRUE, showWarnings = FALSE)
  fake_bin <- withr::local_tempdir(.local_envir = .local_envir)
  picker_script <- c(
    "#!/bin/sh",
    sprintf(
      "printf '%%s\\n' %s",
      shQuote(normalizePath(project_dir, winslash = "/", mustWork = TRUE))
    )
  )
  pickers <- file.path(fake_bin, c("osascript", "zenity", "kdialog"))
  for (picker in pickers) {
    writeLines(picker_script, picker)
  }
  Sys.chmod(pickers, mode = "0755")
  withr::local_envvar(
    PATH = paste(fake_bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    # These tests return fixture paths through the POSIX picker. Native WSL
    # dialogs are exercised separately and must never interrupt unattended CI.
    WSL_DISTRO_NAME = NA_character_,
    WSL_INTEROP = NA_character_,
    .local_envir = .local_envir
  )
  invisible(project_dir)
}

builder_browser_wait_for_example_ready <- function(
  app,
  example = "complete_viewer_data",
  timeout = 60000
) {
  stopifnot(
    is.character(example),
    length(example) == 1L,
    !is.na(example),
    grepl("^[A-Za-z0-9_-]+$", example)
  )
  app$wait_for_js(
    sprintf(
      paste0(
        "(() => { const button = document.querySelector(",
        "'.example-btn[data-ex=%s]'); return button !== null && ",
        "button.disabled === false && ",
        "button.getAttribute('aria-disabled') === 'false'; })()"
      ),
      example
    ),
    timeout = timeout
  )
  invisible(TRUE)
}

builder_browser_wait_for_worker_ready <- function(app, timeout = 60000) {
  app$wait_for_js(
    paste0(
      "document.getElementById('builder-worker-status') !== null && ",
      "document.getElementById('builder-worker-status')",
      ".dataset.workerState === 'ready'"
    ),
    timeout = timeout
  )
  invisible(TRUE)
}

builder_browser_click_dynamic <- function(app, selector) {
  stopifnot(
    is.character(selector),
    length(selector) == 1L,
    !is.na(selector),
    nzchar(selector)
  )
  selector_json <- jsonlite::toJSON(selector, auto_unbox = TRUE)
  app$run_js(sprintf(
    paste0(
      "(() => { const node = document.querySelector(%s); ",
      "if (node === null) throw new Error('Dynamic action is unavailable: ' + %s); ",
      "node.click(); })()"
    ),
    selector_json,
    selector_json
  ))
  invisible(TRUE)
}

builder_browser_check_all_datasets <- function(app, timeout = 10000) {
  for (attempt in seq_len(20L)) {
    can_continue <- app$get_js(paste0(
      "document.getElementById('continue_to_review') !== null && ",
      "document.getElementById('continue_to_review').disabled === false"
    ))
    if (isTRUE(can_continue)) {
      return(invisible(TRUE))
    }
    app$wait_for_js(
      paste0(
        "document.getElementById('complete_dataset_check') !== null && ",
        "document.getElementById('complete_dataset_check').disabled === false"
      ),
      timeout = timeout
    )
    organism_missing <- app$get_js(paste0(
      "document.getElementById('core-organism').selectize.getValue() === ''"
    ))
    if (isTRUE(organism_missing)) {
      builder_browser_click_dynamic(app, "#complete_dataset_check")
      app$wait_for_js(
        paste0(
          "(() => { const box = document.getElementById('core-organism')",
          ".closest('.shiny-input-container').getBoundingClientRect(); ",
          "return box.top >= 0 && box.bottom <= window.innerHeight; })()"
        ),
        timeout = timeout
      )
      app$run_js(
        "document.getElementById('core-organism').selectize.setValue('hg');"
      )
      app$wait_for_idle(timeout = timeout)
    }
    builder_browser_click_dynamic(app, "#complete_dataset_check")
    app$wait_for_idle(timeout = timeout)
  }
  stop("Datasets did not become checked.", call. = FALSE)
}

builder_browser_dismiss_project_offer <- function(app, timeout = 60000) {
  app$wait_for_js(
    paste0(
      "document.querySelector('.ds--client-upload') === null && ",
      "document.querySelector('.ds--import') === null"
    ),
    timeout = timeout
  )
  offered <- app$get_js(paste0(
    "document.querySelector('#shiny-modal .modal-title') !== null && ",
    "document.querySelector('#shiny-modal .modal-title')",
    ".textContent.trim() === 'Save this project'"
  ))
  if (isTRUE(offered)) {
    builder_browser_click_dynamic(app, "#shiny-modal [data-dismiss=modal]")
    app$wait_for_js(
      "document.getElementById('shiny-modal') === null",
      timeout = timeout
    )
  }
  invisible(TRUE)
}

builder_expect_clean_browser_logs <- function(app) {
  logs <- app$get_logs()
  failures <- logs[
    as.character(logs$location) == "chromote" &
      tolower(as.character(logs$level)) %in%
        c("error", "warning", "assert", "throw"),
    ,
    drop = FALSE
  ]
  expect_identical(
    nrow(failures),
    0L,
    info = paste(failures$message, collapse = "\n")
  )
}
