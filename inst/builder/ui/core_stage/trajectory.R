.builder_trajectory_selected <- function(included, method, name) {
  is.list(included) &&
    method %in% names(included) &&
    name %in% as.character(included[[method]])
}

builder_trajectory_catalog_model <- function(model) {
  catalog <- model$trajectory_catalog %||% list()
  included <- model$included_trajectories %||% list()
  default <- model$default_trajectory %||% NULL
  previews <- model$trajectory_previews %||% list()
  items <- lapply(seq_along(catalog), function(index) {
    trajectory <- catalog[[index]]
    method <- trajectory$method %||% ""
    name <- trajectory$name %||% ""
    key <- .builder_trajectory_preview_key(method, name)
    selectable <- isTRUE(trajectory$selectable)
    selected <- selectable &&
      .builder_trajectory_selected(
        included,
        method,
        name
      )
    list(
      index = index,
      key = key,
      method = method,
      name = name,
      selectable = selectable,
      included = selected,
      default = selected &&
        is.list(default) &&
        identical(default$method, method) &&
        identical(default$name, name),
      cell_count = as.integer(.builder_viewer_scalar_number(
        trajectory$cell_count,
        0
      )),
      coverage = .builder_viewer_scalar_number(trajectory$coverage, 0),
      state_count = as.integer(.builder_viewer_scalar_number(
        trajectory$state_count,
        0
      )),
      edge_count = as.integer(.builder_viewer_scalar_number(
        trajectory$edge_count,
        0
      )),
      reason = trajectory$reason %||% NULL,
      preview = previews[[key]] %||% NULL
    )
  })
  selected <- Filter(function(item) isTRUE(item$included), items)
  default_item <- Filter(function(item) isTRUE(item$default), selected)
  if (!length(default_item) && length(selected)) {
    default <- list(
      method = selected[[1L]]$method,
      name = selected[[1L]]$name
    )
    items[[selected[[1L]]$index]]$default <- TRUE
  }
  list(
    items = items,
    included_count = length(selected),
    selectable_count = sum(vapply(items, `[[`, logical(1), "selectable")),
    default = default
  )
}

builder_trajectory_catalog_ui <- function(id, model) {
  ns <- NS(id)
  div(
    class = "viewer-trajectory-workspace",
    `data-input-id` = ns("trajectory_action"),
    div(
      class = "viewer-trajectory-gallery",
      lapply(model$items, function(item) {
        checkbox_id <- ns(paste0("trajectory_include_", item$index))
        radio_id <- ns(paste0("trajectory_default_", item$index))
        points <- if (is.list(item$preview)) item$preview$points else NULL
        edges <- if (is.list(item$preview)) item$preview$edges else NULL
        tags$article(
          class = paste(
            "viewer-trajectory-card",
            if (item$included) "is-included" else "",
            if (!item$selectable) "is-unavailable" else ""
          ),
          `data-trajectory-key` = item$key,
          `data-method` = item$method,
          `data-trajectory` = item$name,
          div(
            class = "viewer-card-choice-row",
            tags$label(
              class = "viewer-card-include",
              `for` = checkbox_id,
              tags$input(
                id = checkbox_id,
                type = "checkbox",
                class = "viewer-trajectory-include",
                `data-method` = item$method,
                `data-trajectory` = item$name,
                checked = if (item$included) "checked" else NULL,
                disabled = if (!item$selectable) "disabled" else NULL
              ),
              span("Include")
            ),
            tags$label(
              class = "viewer-card-default",
              `for` = radio_id,
              tags$input(
                id = radio_id,
                type = "radio",
                name = ns("trajectory_default_choice"),
                class = "viewer-trajectory-default",
                `data-method` = item$method,
                `data-trajectory` = item$name,
                value = item$key,
                checked = if (item$default) "checked" else NULL,
                disabled = if (!item$included) "disabled" else NULL
              ),
              span(
                class = "viewer-default-copy",
                if (item$default) "Default" else "Set default"
              )
            )
          ),
          h4(item$name),
          p(class = "viewer-card-method", item$method),
          p(
            class = "viewer-card-meta",
            paste0(format(item$cell_count, big.mark = ","), " cells"),
            span(`aria-hidden` = "true", " · "),
            paste0(item$state_count, " states"),
            span(`aria-hidden` = "true", " · "),
            paste0(round(100 * item$coverage), "% coverage")
          ),
          if (item$selectable) {
            builder_scatter_thumbnail_ui(
              points,
              point_size = 4,
              label = paste(item$name, "trajectory preview"),
              class = "viewer-trajectory-preview",
              edges = edges
            )
          },
          if (!item$selectable && nzchar(item$reason %||% "")) {
            p(class = "viewer-card-reason", item$reason)
          }
        )
      })
    ),
    div(
      class = "visually-hidden viewer-trajectory-status",
      role = "status",
      `aria-live` = "polite"
    )
  )
}
