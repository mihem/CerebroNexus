builder_group_colors_model <- function(
  group,
  levels,
  palette = "cerebro",
  overrides = list(),
  distribution = list()
) {
  valid_group <- builder_stage_has_text(group %||% "")
  levels <- unname(as.character(levels %||% character()))
  levels <- levels[!is.na(levels) & nzchar(levels)]
  if (!valid_group || !length(levels)) {
    return(list(group = "", items = list(), total = 0L, custom_count = 0L))
  }
  group_overrides <- overrides[[group]] %||% character()
  colors <- builder_level_colors(levels, palette, group_overrides)
  custom_levels <- intersect(levels, names(group_overrides))
  distribution_labels <- vapply(
    distribution,
    function(value) builder_group_level_label(value$value %||% ""),
    character(1)
  )
  distribution_counts <- vapply(
    distribution,
    function(value) {
      count <- suppressWarnings(as.numeric(value$count %||% 0))
      if (length(count) != 1L || !is.finite(count) || count < 0) 0 else count
    },
    numeric(1)
  )
  items <- lapply(seq_along(levels), function(index) {
    level <- levels[[index]]
    label <- builder_group_level_label(level)
    count_index <- match(label, distribution_labels)
    list(
      index = index,
      key = level,
      label = label,
      color = unname(colors[[level]]),
      custom = level %in% custom_levels,
      count = if (is.na(count_index)) 0 else distribution_counts[[count_index]]
    )
  })
  list(
    group = group,
    items = items,
    total = length(items),
    custom_count = length(custom_levels)
  )
}

builder_group_colors_ui <- function(id, model) {
  ns <- NS(id)
  if (!length(model$items %||% list())) {
    return(div(
      class = "builder-group-colors is-empty",
      h3("Group colors"),
      p(
        class = "group-color-empty",
        "Select an included Group to set its retained colors."
      )
    ))
  }
  searchable <- model$total > 30L
  largest <- max(c(
    vapply(
      model$items,
      function(item) {
        count <- suppressWarnings(as.numeric(item$count %||% 0))
        if (length(count) != 1L || !is.finite(count) || count < 0) 0 else count
      },
      numeric(1)
    ),
    1
  ))
  div(
    class = "builder-group-colors",
    `data-group` = model$group,
    div(
      class = "viewer-group-distribution-head",
      h5("Distribution"),
      actionButton(
        ns("reset_colors"),
        "Reset colors",
        class = "btn group-color-reset"
      )
    ),
    if (searchable) {
      tagList(
        tags$label(
          class = "group-color-search-label",
          `for` = ns("group_color_search"),
          "Find a group value"
        ),
        tags$input(
          id = ns("group_color_search"),
          type = "search",
          class = "group-color-search",
          autocomplete = "off"
        )
      )
    },
    div(
      class = "group-color-grid viewer-group-distribution-list",
      lapply(model$items, function(item) {
        input_id <- ns(paste0("group_color_", item$index))
        width <- max(3, 100 * item$count / largest)
        div(
          class = "group-color-item viewer-group-distribution-row",
          `data-search` = tolower(item$label),
          tags$input(
            id = input_id,
            type = "color",
            class = "group-color-input",
            value = item$color,
            `data-input-id` = ns("group_color"),
            `data-group` = model$group,
            `data-level` = item$key,
            `aria-label` = paste("Color for", model$group, item$label)
          ),
          tags$label(
            class = "group-color-name",
            `for` = input_id,
            title = item$label,
            item$label
          ),
          span(
            class = "viewer-group-bar-track",
            span(
              class = "viewer-group-bar",
              style = paste0("width:", round(width, 2L), "%")
            )
          ),
          span(
            class = "viewer-group-count",
            format(item$count, big.mark = ",")
          )
        )
      })
    ),
    div(
      class = "visually-hidden group-color-status",
      `aria-live` = "polite",
      `aria-atomic` = "true"
    )
  )
}

builder_metadata_policy_status <- function(policy, id) {
  record <- if (
    is.list(policy) &&
      is.list(policy$columns) &&
      is.list(policy$columns[[id]])
  ) {
    policy$columns[[id]]
  } else {
    NULL
  }
  disposition <- if (
    is.list(record) &&
      builder_stage_has_text(record$disposition %||% "")
  ) {
    record$disposition
  } else {
    "unknown"
  }
  retained <- if (
    is.list(record) &&
      is.logical(record$retain_in_crb) &&
      length(record$retain_in_crb) == 1L &&
      !is.na(record$retain_in_crb)
  ) {
    isTRUE(record$retain_in_crb)
  } else if (
    is.list(record) &&
      is.logical(record$effective_included) &&
      length(record$effective_included) == 1L &&
      !is.na(record$effective_included)
  ) {
    isTRUE(record$effective_included)
  } else {
    NA
  }
  list(
    retained = retained,
    disposition = disposition,
    forced = isTRUE(record$forced %||% record$required),
    sensitive = isTRUE(record$sensitive),
    recommended = isTRUE(record$retain_in_crb),
    supported = if (
      is.list(record) &&
        is.logical(record$supported) &&
        length(record$supported) == 1L &&
        !is.na(record$supported)
    ) {
      isTRUE(record$supported)
    } else {
      !identical(record$retain_in_crb, FALSE) ||
        !identical(disposition, "excluded")
    }
  )
}

builder_group_catalog_model <- function(model) {
  metadata <- model$metadata_catalog %||% list()
  if (!is.list(metadata) || !length(metadata)) {
    choices <- unname(as.character(model$group_choices %||% character()))
    metadata <- lapply(choices, function(name) {
      list(
        name = name,
        classification = "categorical",
        group_eligible = TRUE,
        group_reason = NULL,
        count = NA_integer_,
        distinct_count = length(model$levels[[name]] %||% character()),
        missing_count = 0L,
        missing_percentage = 0,
        sample_values = character(),
        level_counts = list(
          items = list(),
          total = length(model$levels[[name]] %||% character()),
          truncated = FALSE,
          remaining_count = 0L
        )
      )
    })
    names(metadata) <- choices
  }
  ids <- names(metadata)
  if (is.null(ids)) {
    ids <- character()
  }
  if (length(ids)) {
    eligible <- vapply(
      metadata,
      function(column) isTRUE(column$group_eligible),
      logical(1)
    )
    indices <- c(which(eligible), which(!eligible))
    ids <- ids[indices]
    metadata <- metadata[indices]
  }
  included <- unname(as.character(
    model$included_groups %||%
      model$groups %||%
      model$default_group %||%
      character()
  ))
  included <- unique(included[!is.na(included) & nzchar(included)])
  suggested <- unname(as.character(
    model$suggested_groups %||% included
  ))
  default <- model$default_group %||% NULL

  scalar_number <- function(value, fallback = 0) {
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
  type_label <- function(value) {
    switch(
      value %||% "metadata",
      categorical = "Categorical",
      continuous = "Continuous",
      logical = "Logical",
      integer = "Integer",
      character = "Text",
      factor = "Categorical",
      "Metadata"
    )
  }
  items <- lapply(seq_along(ids), function(index) {
    id <- ids[[index]]
    column <- metadata[[index]]
    metadata_status <- builder_metadata_policy_status(
      model$metadata_policy %||% list(),
      id
    )
    eligible <- isTRUE(column$group_eligible)
    category_count <- scalar_number(
      column$level_counts$total,
      scalar_number(column$distinct_count, 0)
    )
    missing <- scalar_number(column$missing_percentage, 0)
    count <- as.integer(scalar_number(column$count, 0))
    missing_count <- as.integer(scalar_number(column$missing_count, 0))
    reason <- column$group_reason %||% ""
    classification <- column$classification %||% "metadata"
    selected <- eligible && id %in% included
    list(
      index = index,
      id = id,
      label = column$name %||% id,
      classification = classification,
      type_label = type_label(classification),
      eligible = eligible,
      reason = reason,
      metadata_retained = metadata_status$retained,
      metadata_disposition = metadata_status$disposition,
      retention_locked = isTRUE(metadata_status$forced) ||
        !isTRUE(column$supported %||% metadata_status$supported),
      retention_sensitive = isTRUE(metadata_status$sensitive),
      recommended_retained = isTRUE(metadata_status$recommended),
      included = selected,
      default = selected && identical(id, default),
      suggested = eligible && id %in% suggested,
      category_count = as.integer(category_count),
      count = count,
      non_missing_count = as.integer(max(0L, count - missing_count)),
      missing_percentage = missing,
      sample_values = utils::head(
        as.character(column$sample_values %||% character()),
        5L
      ),
      distribution = column$level_counts$items %||% list(),
      distribution_truncated = isTRUE(column$level_counts$truncated),
      remaining_count = as.integer(scalar_number(
        column$level_counts$remaining_count,
        0
      )),
      search = tolower(paste(
        id,
        column$name %||% id,
        classification,
        type_label(classification),
        reason
      ))
    )
  })
  included <- vapply(
    Filter(function(item) isTRUE(item$included), items),
    `[[`,
    character(1),
    "id"
  )
  retained <- vapply(
    Filter(function(item) isTRUE(item$metadata_retained), items),
    `[[`,
    character(1),
    "id"
  )
  supported <- vapply(
    Filter(
      function(item) {
        !isTRUE(item$retention_locked) ||
          isTRUE(item$metadata_retained)
      },
      items
    ),
    `[[`,
    character(1),
    "id"
  )
  recommended_retained <- vapply(
    Filter(function(item) isTRUE(item$recommended_retained), items),
    `[[`,
    character(1),
    "id"
  )
  default_valid <- builder_stage_has_text(default %||% "") &&
    default %in% included
  focus_valid <- builder_stage_has_text(default %||% "") && default %in% ids
  preview_items <- Filter(function(item) isTRUE(item$included), items)
  if (default_valid) {
    preview_ids <- vapply(preview_items, `[[`, character(1), "id")
    preview_items <- preview_items[c(
      match(default, preview_ids),
      which(preview_ids != default)
    )]
  }
  preview_items <- utils::head(preview_items, 6L)
  preview_row_count <- min(
    5L,
    max(c(
      0L,
      vapply(
        preview_items,
        function(item) length(item$sample_values %||% character()),
        integer(1)
      )
    ))
  )
  preview_rows <- lapply(seq_len(preview_row_count), function(row_index) {
    vapply(
      preview_items,
      function(item) {
        values <- item$sample_values %||% character()
        value <- if (row_index <= length(values)) values[[row_index]] else ""
        if (is.na(value)) "N/A" else as.character(value)
      },
      character(1)
    )
  })
  list(
    items = items,
    retained = retained,
    supported = supported,
    recommended_retained = recommended_retained,
    included = included,
    default = if (default_valid) {
      default
    } else if (length(included)) {
      included[[1L]]
    } else {
      NULL
    },
    included_count = length(included),
    eligible_count = sum(vapply(items, `[[`, logical(1), "eligible")),
    metadata_preview = list(
      columns = vapply(preview_items, `[[`, character(1), "label"),
      rows = preview_rows,
      shown_columns = length(preview_items),
      total_columns = length(included),
      default_column = if (default_valid) default else NULL,
      column_ids = vapply(preview_items, `[[`, character(1), "id")
    ),
    focus = if (focus_valid) {
      default
    } else if (length(ids)) {
      ids[[1L]]
    } else {
      NULL
    }
  )
}

builder_cell_cycle_catalog_model <- function(model) {
  metadata <- model$metadata_catalog %||% list()
  ids <- builder_cell_cycle_candidate_ids(metadata)
  policy <- model$metadata_policy %||% list()
  ids <- ids[vapply(
    ids,
    function(id) {
      status <- builder_metadata_policy_status(policy, id)
      !identical(status$retained, FALSE)
    },
    logical(1)
  )]
  selected <- unname(as.character(
    model$cell_cycle_columns %||% character()
  ))
  selected <- ids[ids %in% selected]
  items <- lapply(ids, function(id) {
    column <- metadata[[id]] %||% list()
    count <- column$level_counts$total %||% column$distinct_count %||% 0L
    count <- if (is.numeric(count) && length(count) == 1L && !is.na(count)) {
      as.integer(count)
    } else {
      0L
    }
    list(
      id = id,
      label = column$name %||% id,
      phase_count = count,
      included = id %in% selected
    )
  })
  list(
    items = items,
    included = selected,
    included_count = length(selected)
  )
}

builder_cell_cycle_catalog_ui <- function(id, catalog) {
  ns <- NS(id)
  choices <- vapply(
    catalog$items,
    function(item) {
      paste0(
        item$label,
        if (item$phase_count > 0L) {
          paste0(
            " · ",
            item$phase_count,
            if (item$phase_count == 1L) " phase" else " phases"
          )
        } else {
          ""
        }
      )
    },
    character(1)
  )
  values <- vapply(catalog$items, `[[`, character(1), "id")
  names(values) <- choices
  div(
    class = "viewer-cell-cycle-workspace",
    p(
      class = "viewer-cell-cycle-intro",
      paste(
        "Use categorical phase annotations in Color management and",
        "composition views."
      )
    ),
    checkboxGroupInput(
      ns("cell_cycle"),
      label = tags$span(
        class = "visually-hidden",
        "Cell cycle metadata columns"
      ),
      choices = values,
      selected = catalog$included
    )
  )
}

builder_group_detail_model <- function(catalog, group = NULL) {
  items <- catalog$items %||% list()
  ids <- vapply(items, `[[`, character(1), "id")
  if (!builder_stage_has_text(group %||% "") || !group %in% ids) {
    group <- catalog$focus %||% NULL
  }
  if (!builder_stage_has_text(group %||% "") || !group %in% ids) {
    return(list(item = NULL, metadata_preview = catalog$metadata_preview))
  }
  list(
    item = items[[match(group, ids)]],
    metadata_preview = catalog$metadata_preview
  )
}

builder_metadata_preview_ui <- function(preview, disclosure_key = NULL) {
  columns <- preview$columns %||% character()
  column_ids <- preview$column_ids %||% columns
  default_column <- preview$default_column %||% ""
  rows <- preview$rows %||% list()
  if (!length(columns) || !length(rows)) {
    return(tags$section(
      class = "viewer-metadata-preview",
      h4("Preview metadata"),
      p(
        class = "viewer-metadata-preview-note",
        "Select a Group column to preview its metadata."
      )
    ))
  }
  tags$section(
    class = "viewer-metadata-preview",
    h4("Preview metadata"),
    p(
      class = "viewer-metadata-preview-note",
      paste0(
        "Showing the first ",
        length(rows),
        " rows and ",
        length(columns),
        if (length(columns) == 1L) " column" else " columns",
        if ((preview$total_columns %||% length(columns)) > length(columns)) {
          paste0(" of ", preview$total_columns)
        } else {
          ""
        },
        "."
      )
    ),
    div(
      class = "viewer-metadata-preview-scroll",
      tags$table(
        class = "viewer-metadata-preview-table",
        tags$thead(tags$tr(
          tags$th(scope = "col", "Row"),
          lapply(seq_along(columns), function(index) {
            tags$th(
              scope = "col",
              class = if (identical(column_ids[[index]], default_column)) {
                "is-default"
              },
              columns[[index]]
            )
          })
        )),
        tags$tbody(lapply(seq_along(rows), function(row_index) {
          tags$tr(
            class = "viewer-metadata-preview-row",
            tags$th(scope = "row", row_index),
            lapply(seq_along(rows[[row_index]]), function(column_index) {
              value <- rows[[row_index]][[column_index]]
              tags$td(
                class = if (
                  identical(
                    column_ids[[column_index]],
                    default_column
                  )
                ) {
                  "is-default"
                },
                title = value,
                value
              )
            })
          )
        }))
      )
    )
  )
}

builder_group_detail_ui <- function(id, model) {
  ns <- NS(id)
  item <- model$item
  if (!is.list(item)) {
    return(div(
      class = "viewer-group-detail is-empty",
      p("Choose a metadata column to see its details.")
    ))
  }
  percent <- format(
    round(item$missing_percentage, 2L),
    trim = TRUE,
    scientific = FALSE
  )
  distribution <- item$distribution %||% list()
  largest <- if (length(distribution)) {
    max(c(
      vapply(
        distribution,
        function(value) {
          count <- suppressWarnings(as.numeric(value$count %||% 0))
          if (length(count) != 1L || !is.finite(count) || count < 0) {
            0
          } else {
            count
          }
        },
        numeric(1)
      ),
      1
    ))
  } else {
    1
  }
  div(
    class = "viewer-group-detail",
    `data-group` = item$id,
    div(
      class = "viewer-group-detail-head",
      div(
        h4(item$label),
        p(
          item$type_label,
          span(`aria-hidden` = "true", " · "),
          paste0(
            format(item$non_missing_count, big.mark = ","),
            " non-missing"
          ),
          span(`aria-hidden` = "true", " · "),
          paste0(item$category_count, " categories"),
          span(`aria-hidden` = "true", " · "),
          paste0(percent, "% missing")
        )
      )
    ),
    if (
      !item$eligible &&
        (nzchar(item$reason) ||
          !is.na(item$metadata_retained))
    ) {
      div(
        class = "viewer-group-reason",
        if (nzchar(item$reason)) item$reason,
        if (isTRUE(item$metadata_retained)) {
          span(
            class = "viewer-metadata-policy is-retained",
            "Kept as ordinary metadata. Not eligible as a Group."
          )
        } else if (identical(item$metadata_retained, FALSE)) {
          span(
            class = "viewer-metadata-policy is-excluded",
            "Not retained in the CRB."
          )
        }
      )
    },
    div(
      class = "viewer-group-detail-grid",
      tags$section(
        class = "viewer-group-distribution",
        if (item$eligible && item$included) {
          uiOutput(ns("group_colors"))
        } else if (length(distribution)) {
          tagList(
            h5("Distribution"),
            tagList(lapply(distribution, function(value) {
              count <- suppressWarnings(as.numeric(value$count %||% 0))
              if (length(count) != 1L || !is.finite(count) || count < 0) {
                count <- 0
              }
              width <- max(3, 100 * count / largest)
              div(
                class = "viewer-group-distribution-row",
                span(class = "viewer-group-value", value$value),
                span(
                  class = "viewer-group-bar-track",
                  span(
                    class = "viewer-group-bar",
                    style = paste0("width:", round(width, 2L), "%")
                  )
                ),
                span(
                  class = "viewer-group-count",
                  format(count, big.mark = ",")
                )
              )
            }))
          )
        } else {
          p(
            class = "viewer-group-detail-empty",
            "A categorical distribution is not shown for this column."
          )
        }
      ),
      NULL
    )
  )
}

builder_group_catalog_ui <- function(id, catalog) {
  ns <- NS(id)
  render_row <- function(item) {
    checkbox_id <- ns(paste0("group_include_", item$index))
    radio_id <- ns(paste0("group_default_", item$index))
    div(
      class = paste(
        "viewer-group-row",
        if (item$default) "is-focused" else "",
        if (!item$eligible) "is-ineligible" else ""
      ),
      `data-group` = item$id,
      `data-search` = item$search,
      `data-eligible` = tolower(as.character(item$eligible)),
      `data-suggested` = tolower(as.character(item$suggested)),
      `data-recommended-retained` = tolower(as.character(
        item$recommended_retained
      )),
      tags$label(
        class = "viewer-group-check",
        `for` = checkbox_id,
        tags$input(
          id = checkbox_id,
          type = "checkbox",
          class = "viewer-group-include",
          `data-group` = item$id,
          `aria-label` = paste("Include", item$label, "as a Group"),
          checked = if (item$included) "checked" else NULL,
          disabled = if (!item$eligible) "disabled" else NULL
        )
      ),
      tags$button(
        type = "button",
        class = "viewer-group-focus",
        `data-group` = item$id,
        `aria-pressed` = if (item$default) "true" else "false",
        disabled = if (!item$eligible) "disabled" else NULL,
        span(class = "viewer-group-name", item$label),
        if (!item$eligible) {
          span(class = "viewer-group-meta", item$reason)
        }
      ),
      if (item$eligible) {
        tags$label(
          class = "viewer-group-default-label",
          `for` = radio_id,
          tags$input(
            id = radio_id,
            type = "radio",
            name = ns("group_default_choice"),
            class = "viewer-group-default",
            value = item$id,
            `data-group` = item$id,
            checked = if (item$default) "checked" else NULL
          ),
          span(
            class = "viewer-default-copy",
            if (item$default) "Default" else "Set as default"
          )
        )
      }
    )
  }
  available <- Filter(function(item) isTRUE(item$eligible), catalog$items)
  unavailable <- Filter(function(item) !isTRUE(item$eligible), catalog$items)
  unavailable_ui <- if (length(unavailable) > 1L) {
    tags$details(
      class = "viewer-group-unavailable",
      tags$summary(
        span(
          class = "viewer-group-unavailable-chevron",
          `aria-hidden` = "true"
        ),
        span("Unavailable metadata"),
        span(
          class = "viewer-group-unavailable-count",
          length(unavailable)
        )
      ),
      div(
        class = "viewer-group-unavailable-list",
        lapply(unavailable, render_row)
      )
    )
  } else {
    lapply(unavailable, render_row)
  }
  div(
    class = "viewer-group-workspace",
    `data-input-id` = ns("group_action"),
    `data-metadata-input-id` = ns("metadata_action"),
    `data-focus-input-id` = ns("group_focus"),
    div(
      class = "viewer-group-directory",
      div(
        class = "viewer-group-tools",
        tags$label(
          class = "visually-hidden",
          `for` = ns("group_search"),
          "Find metadata"
        ),
        tags$input(
          id = ns("group_search"),
          type = "search",
          class = "viewer-group-search",
          placeholder = "Find metadata",
          autocomplete = "off"
        ),
        div(
          class = "viewer-group-actions",
          tags$button(
            type = "button",
            class = "btn viewer-group-select",
            `data-action` = "suggested",
            "Select suggested"
          ),
          tags$button(
            type = "button",
            class = "btn viewer-group-select",
            `data-action` = "all",
            "Select all eligible"
          )
        )
      ),
      div(
        class = "viewer-group-list",
        lapply(available, render_row),
        unavailable_ui
      ),
      div(
        class = "viewer-group-empty",
        hidden = "hidden",
        "No metadata matches this search."
      ),
      div(
        class = "visually-hidden viewer-group-status",
        role = "status",
        `aria-live` = "polite"
      )
    ),
    div(
      class = "viewer-group-side",
      uiOutput(ns("metadata_preview")),
      uiOutput(ns("group_detail"))
    )
  )
}
