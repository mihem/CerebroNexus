## Guided Core stage.

builder_core_available_assays <- function(profile) {
  profiles <- profile$assay_profiles %||% list()
  if (!is.list(profiles) || is.null(names(profiles))) {
    return(character())
  }
  available <- names(profiles)[vapply(profiles, is.list, logical(1))]
  preferred <- unname(as.character(profile$assays %||% character()))
  unique(c(preferred[preferred %in% available], available))
}

builder_core_resolve_assay <- function(profile, assay = NULL, fallback = NULL) {
  available <- builder_core_available_assays(profile)
  candidates <- unname(as.character(c(
    assay %||% character(),
    fallback %||% character()
  )))
  candidates <- candidates[
    !is.na(candidates) & nzchar(candidates) & candidates %in% available
  ]
  if (length(candidates)) candidates[[1L]] else NULL
}

builder_core_assay_controls <- function(profile, settings, assay) {
  if (
    !is.character(assay) ||
      length(assay) != 1L ||
      is.na(assay) ||
      !nzchar(assay)
  ) {
    return(NULL)
  }
  assay_profile <- profile$assay_profiles[[assay]]
  if (!is.list(assay_profile)) {
    return(NULL)
  }
  select_value <- function(current, choices, default = NULL) {
    choices <- unname(as.character(choices %||% character()))
    selected <- if (
      builder_stage_has_text(current %||% "") &&
        current %in% choices
    ) {
      current
    } else if (
      builder_stage_has_text(default %||% "") &&
        default %in% choices
    ) {
      default
    } else if (length(choices)) {
      choices[[1L]]
    } else {
      character()
    }
    list(choices = choices, selected = unname(selected))
  }
  list(
    layer = select_value(
      settings$layer,
      assay_profile$layers,
      assay_profile$default_layer
    ),
    nUMI = select_value(
      settings$nUMI,
      assay_profile$nUMI_choices,
      assay_profile$nUMI
    ),
    nGene = select_value(
      settings$nGene,
      assay_profile$nGene_choices,
      assay_profile$nGene
    )
  )
}
