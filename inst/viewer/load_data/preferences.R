##----------------------------------------------------------------------------##
## Tab: Preferences
##----------------------------------------------------------------------------##

##
output[["preferences_options"]] <- renderUI({
  ## A switch with a small "i" after its label: hovering it, focusing it or
  ## clicking it (viewer-shell.js) shows the help in a bubble.
  preference <- function(id, label, value, help) {
    help_id <- paste0(id, "_help")
    control <- checkboxInput(
      id,
      tags$span(
        class = "cerebro-pref-label",
        tags$span(label),
        tags$span(
          class = "cerebro-pref-info",
          role = "button",
          tabindex = "0",
          `aria-label` = paste("About", label),
          `aria-controls` = help_id,
          `aria-expanded` = "false",
          "i"
        )
      ),
      value
    )
    control <- htmltools::tagQuery(control)$find("input")$addAttrs(
      role = "switch",
      `aria-describedby` = help_id
    )$allTags()
    div(
      class = "cerebro-preference",
      `data-help` = help,
      control,
      tags$span(
        id = help_id,
        class = "cerebro-pref-tip",
        role = "tooltip",
        help
      )
    )
  }
  tagList(
    div(class = "cerebro-preferences-heading", "Preferences"),
    preference(
      "webgl_checkbox",
      "GPU acceleration",
      TRUE,
      "Draws plots with WebGPU or WebGL. Off uses the CPU."
    ),
    preference(
      "hover_info_in_projections_checkbox",
      "Cell tooltips",
      isTRUE(Cerebro.options[['projections_show_hover_info']] %||% TRUE),
      "Shows a cell's details on hover. A click still pins them."
    ),
    preference(
      "expression_on_top_checkbox",
      "Expression on top",
      ## On unless the author's Gene expression default keeps the input order.
      !identical(
        tryCatch(
          current_page_display_defaults("gene")$plotting_order,
          error = function(error) NULL
        ),
        "Input order"
      ),
      "Gene expression draws the highest values on top. Off keeps the input order."
    )
  )
})

##----------------------------------------------------------------------------##
## "Expression on top" and Gene expression's "Highest expression on top" are one
## setting: each follows the other. The page's control may not exist yet; it
## then opens with this value (expression_projection_default_plotting_order()).
##----------------------------------------------------------------------------##
preferences_expression_on_top_seen <- NULL
observeEvent(input[["expression_on_top_checkbox"]], {
  value <- isTRUE(input[["expression_on_top_checkbox"]])
  ## Its first value is only the switch binding, not a choice.
  first <- is.null(preferences_expression_on_top_seen)
  preferences_expression_on_top_seen <<- value
  if (first) {
    return()
  }
  page <- input[["expression_projection_plotting_order"]]
  if (!is.null(page) && !identical(isTRUE(page), value)) {
    updateCheckboxInput(
      session,
      "expression_projection_plotting_order",
      value = value
    )
  }
})
observeEvent(input[["expression_projection_plotting_order"]], {
  value <- isTRUE(input[["expression_projection_plotting_order"]])
  if (!identical(isTRUE(input[["expression_on_top_checkbox"]]), value)) {
    updateCheckboxInput(session, "expression_on_top_checkbox", value = value)
  }
})

##----------------------------------------------------------------------------##
## Observe WebGL on?
##----------------------------------------------------------------------------##
observeEvent(input[["webgl_checkbox"]], {
  preferences[["use_webgl"]] <- input[["webgl_checkbox"]]
  session$sendCustomMessage(
    "cerebro_gpu_acceleration",
    list(
      enabled = isTRUE(input[["webgl_checkbox"]])
    )
  )
})

##----------------------------------------------------------------------------##
## Observe hover on?
##----------------------------------------------------------------------------##

observeEvent(input[["hover_info_in_projections_checkbox"]], {
  session$sendCustomMessage(
    "cerebro_cell_tooltips",
    list(
      enabled = isTRUE(input[["hover_info_in_projections_checkbox"]])
    )
  )
})
