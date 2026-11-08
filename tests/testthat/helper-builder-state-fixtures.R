use_state_only_fixture <- function(datasets = list()) {
  runtime <- parent.frame()
  datasets <- lapply(datasets, function(entry) {
    profile <- entry$profile %||% list()
    recognized <- any(
      c(
        "default_assay",
        "assay_profiles",
        "nUMI",
        "nGene",
        "extras"
      ) %in%
        names(profile)
    )
    if (!recognized && is.null(entry$dataset_profile)) {
      profile$extras <- list()
      entry$profile <- profile
    }
    entry
  })
  ids <- vapply(datasets, `[[`, character(1), "id")
  stopifnot(!anyDuplicated(ids))
  state <- structure(
    list(
      datasets = datasets,
      current_dataset = if (length(ids)) ids[[1L]] else NULL,
      last_removed = NULL,
      can_undo_remove = FALSE,
      revision = 1L,
      .state_only_fixture = TRUE
    ),
    class = c("builder_state", "list")
  )
  for (cache in c(
    "builder_dataset_state_cache",
    "builder_configuration_identity_cache"
  )) {
    value <- get0(cache, runtime, inherits = TRUE)
    if (is.environment(value)) {
      keys <- ls(value, all.names = TRUE)
      if (length(keys)) rm(list = keys, envir = value)
    }
  }
  get("store", runtime, inherits = TRUE)(state)
  invisible(state)
}
