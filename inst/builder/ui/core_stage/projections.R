.builder_viewer_scalar_number <- function(value, fallback = 0) {
  if (
    is.numeric(value) &&
      length(value) == 1L &&
      !is.na(value) &&
      is.finite(value)
  ) {
    as.numeric(value)
  } else {
    fallback
  }
}

.builder_projection_label <- function(item) {
  switch(
    item$kind %||% "other",
    umap = "UMAP",
    tsne = "t-SNE",
    pca = "PCA",
    item$name %||% item$id
  )
}

.builder_scatter_coordinates <- function(points, edges = NULL) {
  if (!is.data.frame(points) || !nrow(points)) {
    return(NULL)
  }
  valid <- is.finite(points$x) & is.finite(points$y)
  points <- points[valid, , drop = FALSE]
  if (!nrow(points)) {
    return(NULL)
  }
  x_values <- points$x
  y_values <- points$y
  if (is.data.frame(edges) && nrow(edges)) {
    x_values <- c(x_values, edges$x, edges$xend)
    y_values <- c(y_values, edges$y, edges$yend)
  }
  x_range <- range(x_values, finite = TRUE)
  y_range <- range(y_values, finite = TRUE)
  x_span <- diff(x_range)
  y_span <- diff(y_range)
  if (!is.finite(x_span) || x_span == 0) {
    x_span <- 1
  }
  if (!is.finite(y_span) || y_span == 0) {
    y_span <- 1
  }
  project_x <- function(value) 12 + 216 * (value - x_range[[1L]]) / x_span
  project_y <- function(value) 138 - 126 * (value - y_range[[1L]]) / y_span
  list(
    points = data.frame(
      x = project_x(points$x),
      y = project_y(points$y),
      group = as.character(points$group %||% rep("cells", nrow(points))),
      stringsAsFactors = FALSE
    ),
    edges = if (is.data.frame(edges) && nrow(edges)) {
      data.frame(
        x = project_x(edges$x),
        y = project_y(edges$y),
        xend = project_x(edges$xend),
        yend = project_y(edges$yend)
      )
    } else {
      data.frame(
        x = numeric(),
        y = numeric(),
        xend = numeric(),
        yend = numeric()
      )
    }
  )
}

builder_scatter_thumbnail_ui <- function(
  points,
  colors = NULL,
  point_size = 5,
  label = "Coordinate preview",
  class = "viewer-scatter-preview",
  edges = NULL
) {
  coordinates <- .builder_scatter_coordinates(points, edges)
  if (is.null(coordinates)) {
    return(div(
      class = paste(class, "is-loading"),
      span("Loading preview…")
    ))
  }
  groups <- coordinates$points$group
  groups[is.na(groups) | !nzchar(groups)] <- "N/A"
  levels_present <- unique(groups)
  palette <- builder_preview_palette(length(levels_present))
  names(palette) <- levels_present
  if (length(colors)) {
    shared <- intersect(names(colors), levels_present)
    palette[shared] <- as.character(colors[shared])
  }
  if ("N/A" %in% levels_present) {
    palette[["N/A"]] <- "#898989"
  }
  radius <- max(
    0,
    min(
      4.5,
      .builder_viewer_scalar_number(
        point_size,
        5
      ) *
        0.34
    )
  )
  tags$svg(
    class = class,
    viewBox = "0 0 240 150",
    preserveAspectRatio = "xMidYMid meet",
    role = "img",
    `aria-label` = label,
    `data-point-size` = format(point_size, trim = TRUE),
    if (nrow(coordinates$edges)) {
      lapply(seq_len(nrow(coordinates$edges)), function(index) {
        edge <- coordinates$edges[index, ]
        tags$line(
          x1 = round(edge$x, 2L),
          y1 = round(edge$y, 2L),
          x2 = round(edge$xend, 2L),
          y2 = round(edge$yend, 2L),
          class = "viewer-trajectory-edge"
        )
      })
    },
    lapply(seq_len(nrow(coordinates$points)), function(index) {
      point <- coordinates$points[index, ]
      tags$circle(
        cx = round(point$x, 2L),
        cy = round(point$y, 2L),
        r = round(radius, 2L),
        fill = unname(palette[[groups[[index]]]]),
        class = "viewer-scatter-point",
        `data-group` = groups[[index]]
      )
    })
  )
}

builder_projection_catalog_model <- function(model) {
  catalog <- model$projection_catalog %||% list()
  if (!is.list(catalog) || !length(catalog)) {
    ids <- unname(as.character(model$projection_choices %||% character()))
    catalog <- lapply(ids, function(id) {
      list(
        id = id,
        name = id,
        kind = if (grepl("pca", id, ignore.case = TRUE)) "pca" else "other",
        dimensions = 2L,
        cell_count = NA_integer_,
        available = TRUE
      )
    })
    names(catalog) <- ids
  }
  ids <- names(catalog)
  included <- unique(as.character(
    model$included_projections %||% model$default_projection %||% character()
  ))
  included <- included[!is.na(included) & nzchar(included)]
  default <- model$default_projection %||% NULL
  initial <- unique(as.character(
    model$initial_projections %||% default %||% character()
  ))
  initial <- initial[!is.na(initial) & nzchar(initial)]
  previews <- model$projection_previews %||% list()
  items <- lapply(seq_along(catalog), function(index) {
    projection <- catalog[[index]]
    id <- projection$id %||% ids[[index]]
    available <- isTRUE(projection$available)
    selected <- available && id %in% included
    item <- list(
      index = index,
      id = id,
      name = projection$name %||% id,
      kind = projection$kind %||% "other",
      dimensions = as.integer(.builder_viewer_scalar_number(
        projection$dimensions,
        0
      )),
      cell_count = as.integer(.builder_viewer_scalar_number(
        projection$cell_count,
        0
      )),
      available = available,
      reason = projection$reason %||% NULL,
      included = selected,
      default = FALSE,
      initial = FALSE,
      preview = previews[[id]] %||% NULL
    )
    item$label <- .builder_projection_label(item)
    item
  })
  selected_ids <- vapply(
    Filter(function(item) isTRUE(item$included), items),
    `[[`,
    character(1),
    "id"
  )
  if (!default %in% selected_ids) {
    default <- if (length(selected_ids)) selected_ids[[1L]] else NULL
  }
  initial <- selected_ids[selected_ids %in% initial]
  if (!length(initial) && !is.null(default)) {
    initial <- default
  }
  if (!is.null(default) && !default %in% initial) {
    initial <- c(default, initial)
  }
  items <- lapply(items, function(item) {
    item$default <- item$included && identical(item$id, default)
    item$initial <- item$included && item$id %in% initial
    item
  })
  list(
    items = items,
    included = selected_ids,
    included_count = length(selected_ids),
    default = default,
    initial = initial,
    point_size = .builder_viewer_scalar_number(
      model$overview_point_size,
      5
    ),
    point_opacity = .builder_viewer_scalar_number(
      model$overview_point_opacity,
      1
    ),
    percentage_cells_to_show = .builder_viewer_scalar_number(
      model$overview_percentage_cells_to_show,
      100
    ),
    colors = model$preview_colors %||% character(),
    preview_group = model$preview_group %||% ""
  )
}

builder_projection_catalog_ui <- function(id, model) {
  ns <- NS(id)
  div(
    class = "viewer-projection-workspace",
    `data-input-id` = ns("projection_action"),
    `data-preview-group` = model$preview_group,
    div(
      class = "viewer-projection-toolbar",
      div(
        class = "viewer-projection-control",
        tags$label(
          `for` = ns("overview_point_size"),
          span("Initial point size"),
          span(
            class = "viewer-range-value viewer-point-size-value",
            format(model$point_size, trim = TRUE)
          )
        ),
        tags$input(
          id = ns("overview_point_size"),
          class = "viewer-range-input viewer-point-size-input",
          type = "range",
          min = "1",
          max = "20",
          step = "1",
          value = format(model$point_size, trim = TRUE),
          `data-input-id` = ns("point_size")
        )
      ),
      div(
        class = "viewer-projection-control",
        tags$label(
          `for` = ns("overview_point_opacity"),
          span("Initial point opacity"),
          span(
            class = "viewer-range-value viewer-point-opacity-value",
            format(model$point_opacity, trim = TRUE)
          )
        ),
        tags$input(
          id = ns("overview_point_opacity"),
          class = "viewer-range-input viewer-point-opacity-input",
          type = "range",
          min = "0.1",
          max = "1",
          step = "0.1",
          value = format(model$point_opacity, trim = TRUE),
          `data-input-id` = ns("point_opacity")
        )
      ),
      div(
        class = "viewer-projection-control",
        tags$label(
          `for` = ns("overview_percentage_cells_to_show"),
          span("Initial cells shown"),
          span(
            class = paste(
              "viewer-range-value viewer-point-size-value",
              "viewer-cell-percentage-value"
            ),
            paste0(
              format(model$percentage_cells_to_show, trim = TRUE),
              "%"
            )
          )
        ),
        tags$input(
          id = ns("overview_percentage_cells_to_show"),
          class = "viewer-range-input viewer-cell-percentage-input",
          type = "range",
          min = "10",
          max = "100",
          step = "1",
          value = format(model$percentage_cells_to_show, trim = TRUE),
          `data-input-id` = ns("percentage_cells_to_show")
        )
      )
    ),
    tags$p(
      class = "help-block",
      "Point size, opacity and cells shown apply across projection views, including Gene expression and Linked views. Spatial uses Coordinate settings."
    ),
    div(
      class = "viewer-projection-gallery",
      lapply(model$items, function(item) {
        checkbox_id <- ns(paste0("projection_include_", item$index))
        initial_id <- ns(paste0("projection_initial_", item$index))
        radio_id <- ns(paste0("projection_default_", item$index))
        tags$article(
          class = paste(
            "viewer-projection-card",
            if (item$included) "is-included" else "",
            if (!item$available) "is-unavailable" else ""
          ),
          `data-projection` = item$id,
          div(
            class = "viewer-card-choice-row",
            tags$label(
              class = "viewer-card-include",
              `for` = checkbox_id,
              tags$input(
                id = checkbox_id,
                type = "checkbox",
                class = "viewer-projection-include",
                `data-projection` = item$id,
                checked = if (item$included) "checked" else NULL,
                disabled = if (!item$available) "disabled" else NULL
              ),
              span("Include")
            ),
            tags$label(
              class = "viewer-card-initial",
              `for` = initial_id,
              tags$input(
                id = initial_id,
                type = "checkbox",
                class = "viewer-projection-initial",
                `data-projection` = item$id,
                checked = if (item$initial) "checked" else NULL,
                disabled = if (!item$available || item$default) {
                  "disabled"
                } else {
                  NULL
                }
              ),
              span("Show initially")
            ),
            tags$label(
              class = "viewer-card-default",
              `for` = radio_id,
              tags$input(
                id = radio_id,
                type = "radio",
                name = ns("projection_default_choice"),
                class = "viewer-projection-default",
                `data-projection` = item$id,
                value = item$id,
                checked = if (item$default) "checked" else NULL,
                disabled = if (!item$available) "disabled" else NULL
              ),
              span(
                class = "viewer-default-copy",
                if (item$default) "Default" else "Set default"
              )
            )
          ),
          h4(item$label),
          p(
            class = "viewer-card-meta",
            paste0(format(item$cell_count, big.mark = ","), " points"),
            span(`aria-hidden` = "true", " · "),
            paste0(item$dimensions, " dimensions")
          ),
          builder_scatter_thumbnail_ui(
            item$preview,
            colors = model$colors,
            point_size = model$point_size,
            label = paste(item$label, "projection preview"),
            class = "viewer-projection-preview"
          ),
          if (!item$available && nzchar(item$reason %||% "")) {
            p(class = "viewer-card-reason", item$reason)
          }
        )
      })
    ),
    div(
      class = "visually-hidden viewer-projection-status",
      role = "status",
      `aria-live` = "polite"
    )
  )
}
