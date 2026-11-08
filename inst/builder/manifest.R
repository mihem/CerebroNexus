##----------------------------------------------------------------------------##
## Typed content-manifest records and build-readiness rules.
##
## Viewer page identities and gates come from the shared bundle-safe contract,
## which app.R and the worker source before this file.
##----------------------------------------------------------------------------##

builder_manifest_entry <- function(
  id,
  source,
  status,
  disposition,
  artifact_scope,
  summary = "",
  diagnostics = list(),
  compatibility = list(),
  pages = character(),
  required_action = NULL,
  verifier = NULL
) {
  values <- .builder_manifest_validate_values(
    id = id,
    source = source,
    status = status,
    disposition = disposition,
    artifact_scope = artifact_scope,
    summary = summary,
    diagnostics = diagnostics,
    compatibility = compatibility,
    pages = pages,
    required_action = required_action,
    verifier = verifier
  )
  structure(values, class = c("builder_manifest_entry", "list"))
}

builder_content_manifest <- function(entries) {
  if (!is.list(entries)) {
    .builder_manifest_abort(
      "invalid_manifest",
      "A content manifest must be built from a list of entries."
    )
  }
  entries <- unname(entries)
  for (entry in entries) {
    .builder_manifest_validate_entry(entry)
  }
  ids <- vapply(entries, function(entry) entry$id, character(1))
  if (anyDuplicated(ids)) {
    .builder_manifest_abort(
      "duplicate_id",
      "Manifest entry ids must be unique."
    )
  }
  names(entries) <- ids
  structure(entries, class = c("builder_content_manifest", "list"))
}

builder_manifest_readiness <- function(
  manifest,
  acknowledgements = character()
) {
  .builder_manifest_validate(manifest)
  if (!is.character(acknowledgements) || anyNA(acknowledgements)) {
    .builder_manifest_abort(
      "invalid_acknowledgements",
      "Acknowledgements must be character tokens."
    )
  }

  entries <- unname(manifest)
  blocking_ids <- vapply(
    Filter(function(entry) identical(entry$status, "blocking"), entries),
    function(entry) entry$id,
    character(1)
  )
  checking_ids <- vapply(
    Filter(function(entry) identical(entry$status, "checking"), entries),
    function(entry) entry$id,
    character(1)
  )
  unresolved_attention <- Filter(
    function(entry) {
      if (!identical(entry$status, "attention")) {
        return(FALSE)
      }
      action <- entry$required_action
      !identical(action[["type"]], "acknowledge") ||
        !action[["token"]] %in% acknowledgements
    },
    entries
  )
  attention_ids <- vapply(
    unresolved_attention,
    function(entry) entry$id,
    character(1)
  )

  state <- if (length(blocking_ids)) {
    "blocked"
  } else if (length(checking_ids)) {
    "checking"
  } else if (length(attention_ids)) {
    "needs_attention"
  } else {
    "ready"
  }

  list(
    state = state,
    blocking_ids = unname(blocking_ids),
    attention_ids = unname(attention_ids),
    checking_ids = unname(checking_ids)
  )
}
