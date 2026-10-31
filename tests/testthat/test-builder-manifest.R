builder_inst_asset_path <- function(...) {
  relative <- file.path(...)
  path <- builder_test_inst_path(relative)
  if (!file.exists(path)) {
    path <- system.file(relative, package = "CerebroNexus")
  }
  path
}

builder_manifest_source_if_present <- function(file, local = parent.frame()) {
  path <- builder_inst_asset_path("builder", file)
  if (nzchar(path) && file.exists(path)) {
    source(path, local = local)
  }
}

builder_contract_source_if_present <- function(local = parent.frame()) {
  path <- builder_inst_asset_path(
    "viewer",
    "core",
    "viewer_content_contract.R"
  )
  if (nzchar(path) && file.exists(path)) {
    source(path, local = local)
  }
}

builder_contract_source_if_present()
builder_manifest_source_if_present("manifest.R")

capture_builder_manifest_error <- function(expr) {
  tryCatch(
    {
      force(expr)
      NULL
    },
    builder_manifest_error = function(error) error
  )
}

expect_builder_manifest_code <- function(expr, code) {
  error <- capture_builder_manifest_error(expr)
  expect_s3_class(error, "builder_manifest_error")
  expect_identical(error$code, code)
  invisible(error)
}

manifest_entry <- function(
  id,
  status = "valid",
  disposition = "preserved",
  artifact_scope = "crb",
  pages = character(),
  required_action = NULL
) {
  builder_manifest_entry(
    id = id,
    source = list(type = "fixture", location = id),
    status = status,
    disposition = disposition,
    artifact_scope = artifact_scope,
    summary = paste("Summary for", id),
    diagnostics = list(source_rows = 10L),
    compatibility = list(viewer = TRUE),
    pages = pages,
    required_action = required_action,
    verifier = paste0("verify_", id)
  )
}

test_that("manifest entries keep status and disposition orthogonal", {
  statuses <- c(
    "checking",
    "valid",
    "attention",
    "blocking",
    "not_applicable"
  )
  dispositions <- c(
    "preserved",
    "generated",
    "converted",
    "attached",
    "filtered",
    "stored_only",
    "rejected"
  )

  for (status in statuses) {
    action <- if (identical(status, "attention")) {
      list(type = "acknowledge", token = status)
    } else {
      NULL
    }
    entry <- manifest_entry(
      paste0("status_", status),
      status = status,
      required_action = action
    )
    expect_identical(entry$status, status)
  }
  for (disposition in dispositions) {
    entry <- manifest_entry(
      paste0("disposition_", disposition),
      disposition = disposition
    )
    expect_identical(entry$disposition, disposition)
  }

  expect_true(is.na(
    manifest_entry(
      "checking_na",
      status = "checking",
      disposition = NA_character_
    )$disposition
  ))
  expect_true(is.na(
    manifest_entry(
      "not_applicable_na",
      status = "not_applicable",
      disposition = NA_character_
    )$disposition
  ))

  expect_builder_manifest_code(
    manifest_entry("bad_status", status = "planned"),
    "invalid_status"
  )
  expect_builder_manifest_code(
    manifest_entry("missing_disposition", disposition = NA_character_),
    "missing_disposition"
  )
  expect_builder_manifest_code(
    manifest_entry("bad_scope", artifact_scope = "viewer"),
    "invalid_artifact_scope"
  )
})

test_that("manifest entries expose the complete typed record", {
  entry <- manifest_entry(
    "immune_metadata",
    disposition = "converted",
    artifact_scope = "both",
    pages = "immune_repertoire"
  )

  expect_s3_class(entry, "builder_manifest_entry")
  expect_named(
    entry,
    c(
      "id",
      "source",
      "status",
      "disposition",
      "artifact_scope",
      "summary",
      "diagnostics",
      "compatibility",
      "pages",
      "required_action",
      "verifier"
    ),
    ignore.order = FALSE
  )
  expect_identical(entry$source$type, "fixture")
  expect_identical(entry$source$location, "immune_metadata")
  expect_identical(entry$pages, "immune_repertoire")
})

test_that("attention entries require one of the three action types", {
  expect_builder_manifest_code(
    manifest_entry("attention_without_action", status = "attention"),
    "missing_required_action"
  )
  expect_builder_manifest_code(
    manifest_entry(
      "attention_bad_action",
      status = "attention",
      required_action = list(type = "retry")
    ),
    "invalid_action_type"
  )
  expect_builder_manifest_code(
    manifest_entry(
      "attention_ack_without_token",
      status = "attention",
      required_action = list(type = "acknowledge")
    ),
    "invalid_action_token"
  )

  for (type in c("acknowledge", "choose", "provide")) {
    action <- list(type = type)
    if (identical(type, "acknowledge")) {
      action$token <- "understood"
    }
    entry <- manifest_entry(
      paste0("attention_", type),
      status = "attention",
      required_action = action
    )
    expect_identical(entry$required_action$type, type)
  }
})

test_that("content manifests accept only valid entries with unique ids", {
  first <- manifest_entry("expression")
  second <- manifest_entry("metadata")
  manifest <- builder_content_manifest(list(first, second))

  expect_s3_class(manifest, "builder_content_manifest")
  expect_identical(names(manifest), c("expression", "metadata"))
  expect_identical(manifest[["metadata"]], second)
  expect_null(manifest[["missing"]])

  expect_builder_manifest_code(
    builder_content_manifest(list(list(id = "forged"))),
    "invalid_entry"
  )
  expect_builder_manifest_code(
    builder_content_manifest(list(first, first)),
    "duplicate_id"
  )

  tampered <- first
  tampered$status <- "planned"
  expect_builder_manifest_code(
    builder_content_manifest(list(tampered)),
    "invalid_status"
  )
})

test_that("readiness uses blocking checking attention ready precedence", {
  expect_identical(
    builder_manifest_readiness(builder_content_manifest(list()))$state,
    "ready"
  )

  manifest <- builder_content_manifest(list(
    manifest_entry(
      "acknowledged",
      status = "attention",
      required_action = list(type = "acknowledge", token = "accepted")
    ),
    manifest_entry(
      "choice",
      status = "attention",
      required_action = list(type = "choose")
    ),
    manifest_entry(
      "still_checking",
      status = "checking",
      disposition = NA_character_
    ),
    manifest_entry(
      "expression",
      status = "blocking",
      disposition = "rejected"
    )
  ))

  readiness <- builder_manifest_readiness(
    manifest,
    acknowledgements = "accepted"
  )
  expect_identical(readiness$state, "blocked")
  expect_identical(readiness$blocking_ids, "expression")
  expect_identical(readiness$checking_ids, "still_checking")
  expect_identical(readiness$attention_ids, "choice")

  without_block <- builder_content_manifest(unname(manifest[1:3]))
  expect_identical(
    builder_manifest_readiness(without_block, "accepted")$state,
    "checking"
  )

  attention_only <- builder_content_manifest(unname(manifest[1:2]))
  attention_readiness <- builder_manifest_readiness(
    attention_only,
    "accepted"
  )
  expect_identical(attention_readiness$state, "needs_attention")
  expect_identical(attention_readiness$attention_ids, "choice")

  acknowledged_only <- builder_content_manifest(list(manifest[[1L]]))
  expect_identical(
    builder_manifest_readiness(acknowledged_only, "accepted")$state,
    "ready"
  )
})

test_that("choose and provide resolve only after their entry becomes valid", {
  for (type in c("choose", "provide")) {
    pending <- builder_content_manifest(list(manifest_entry(
      paste0(type, "_pending"),
      status = "attention",
      required_action = list(type = type)
    )))
    expect_identical(
      builder_manifest_readiness(
        pending,
        acknowledgements = paste0(type, "_pending")
      )$state,
      "needs_attention"
    )

    resolved <- builder_content_manifest(list(manifest_entry(
      paste0(type, "_resolved"),
      status = "valid",
      required_action = list(type = type)
    )))
    expect_identical(
      builder_manifest_readiness(resolved)$state,
      "ready"
    )
  }
})

test_that("manifest sources without package or app helper functions", {
  isolated <- new.env(parent = baseenv())
  core_path <- builder_inst_asset_path(
    "viewer",
    "core",
    "viewer_content_contract.R"
  )
  manifest_path <- builder_inst_asset_path("builder", "manifest.R")
  expect_true(nzchar(core_path) && file.exists(core_path))
  expect_true(nzchar(manifest_path) && file.exists(manifest_path))
  sys.source(core_path, envir = isolated)
  sys.source(manifest_path, envir = isolated)
  assign(
    "%||%",
    function(...) stop("unexpected external helper dependency"),
    envir = isolated
  )

  entry <- isolated$builder_manifest_entry(
    id = "isolated",
    source = list(type = "fixture", location = "memory"),
    status = "valid",
    disposition = "preserved",
    artifact_scope = "both",
    pages = "trajectory"
  )
  manifest <- isolated$builder_content_manifest(list(entry))

  expect_identical(
    isolated$builder_manifest_readiness(manifest)$state,
    "ready"
  )
  expect_identical(
    isolated$builder_viewer_page_contract(manifest)$visible_conditional,
    "trajectory"
  )
})

test_that("builder and worker source shared pages before the manifest", {
  app_path <- builder_inst_asset_path("builder", "app.R")
  session_path <- builder_inst_asset_path("builder", "worker.R")
  expect_true(nzchar(app_path) && file.exists(app_path))
  expect_true(nzchar(session_path) && file.exists(session_path))
  app_lines <- readLines(app_path, warn = FALSE)
  session_lines <- readLines(session_path, warn = FALSE)

  app_core <- grep("viewer_content_contract.R", app_lines, fixed = TRUE)
  app_manifest <- grep(
    'source("manifest.R", local = TRUE)',
    app_lines,
    fixed = TRUE
  )
  expect_length(app_core, 1L)
  expect_length(app_manifest, 1L)
  expect_lt(app_core, app_manifest)

  worker_core <- grep("viewer_content_contract.R", session_lines, fixed = TRUE)
  worker_manifest <- grep(
    'source_utf8(file.path(dir, "manifest.R"), globalenv())',
    session_lines,
    fixed = TRUE
  )
  expect_length(worker_core, 1L)
  expect_length(worker_manifest, 1L)
  expect_lt(worker_core, worker_manifest)
})
