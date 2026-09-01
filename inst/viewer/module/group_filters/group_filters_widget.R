##----------------------------------------------------------------------------##
## Shared Linked views-style group filters for projection-style tabs.
##----------------------------------------------------------------------------##

groupFilterControl <- function(input_id, label, levels, colors) {
  shiny::div(
    id = input_id,
    class = "cv-filt shiny-input-checkboxgroup",
    role = "group",
    `aria-label` = label,
    shiny::tags$button(
      type = "button",
      class = "cv-filt-btn",
      `aria-expanded` = "false",
      shiny::tags$span(label),
      " ",
      shiny::tags$span(
        class = "cv-filt-ct",
        paste0(length(levels), "/", length(levels))
      )
    ),
    shiny::div(
      class = "cv-filt-menu",
      style = "display:none",
      shiny::div(
        class = "cv-filt-acts",
        shiny::tags$button(type = "button", `data-act` = "all", "All"),
        shiny::tags$button(type = "button", `data-act` = "none", "None")
      ),
      lapply(seq_along(levels), function(index) {
        level <- levels[[index]]
        shiny::tags$label(
          class = "cv-filt-item",
          shiny::tags$input(
            type = "checkbox",
            name = input_id,
            value = level,
            checked = "checked"
          ),
          shiny::tags$span(
            class = "cv-dot",
            style = paste0("background:", colors[[index]])
          ),
          shiny::tags$span(level)
        )
      })
    )
  )
}

#' Register the group-filters renderUI for a projection-style tab.
#'
#' Creates `output[[paste0(prefix, "_group_filters_UI")]]` with one shared
#' chip/popover control per grouping variable. Each control keeps the input id
#' `<prefix>_group_filter_<groupName>` used by downstream observers.
#'
#' @param output The Shiny output object from the server function.
#' @param prefix Tab-specific prefix, e.g. "overview_projection" or
#'        "trajectory_projection".
#' @param getGroups Closure returning a character vector of grouping variable
#'        names. Pass the caller's own getGroups() defined in
#'        utility_functions.R.
#' @param getGroupLevels Closure mapping a group name to its levels.
registerGroupFiltersUI <- function(output, prefix, getGroups, getGroupLevels) {
  output_id <- paste0(prefix, "_group_filters_UI")

  output[[output_id]] <- shiny::renderUI({
    filters <- lapply(getGroups(), function(group) {
      levels <- getGroupLevels(group)
      colors <- tryCatch(
        unname(reactive_colors()[[group]][levels]),
        error = function(e) NULL
      )
      if (is.null(colors) || length(colors) != length(levels)) {
        colors <- cerebro_group_colors(length(levels))
      }
      groupFilterControl(
        paste0(prefix, "_group_filter_", group),
        group,
        levels,
        colors
      )
    })
    shiny::div(
      class = "cerebro-group-filters",
      shiny::div(class = "cv-filters-row", filters)
    )
  })

  ## ensure rendered even when the surrounding cerebroBox is collapsed
  shiny::outputOptions(output, output_id, suspendWhenHidden = FALSE)
}

#' Register the info-button modal for the group-filters panel.
#'
#' Wires `input[[paste0(prefix, "_group_filters_info")]]` to a modalDialog.
#' The text differs per tab so it is passed in.
#'
#' @param input The Shiny input object from the server function.
#' @param prefix Same prefix as registerGroupFiltersUI.
#' @param title Modal title.
#' @param text Modal body (typically `HTML("...")`).
registerGroupFiltersInfo <- function(input, prefix, title, text) {
  info_id <- paste0(prefix, "_group_filters_info")
  shiny::observeEvent(input[[info_id]], {
    shiny::showModal(shiny::modalDialog(
      text,
      title = title,
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    ))
  })
}
