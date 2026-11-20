##----------------------------------------------------------------------------##
## Tab: Preferences
##----------------------------------------------------------------------------##

##
output[["preferences_options"]] <- renderUI({
  preference <- function(id, label, value, help) {
    control <- checkboxInput(id, label, value)
    control <- htmltools::tagQuery(control)$find("input")$addAttrs(
      role = "switch",
      `aria-describedby` = paste0(id, "_help")
    )$allTags()
    div(
      class = "cerebro-preference",
      title = help,
      `data-help` = help,
      control,
      tags$span(id = paste0(id, "_help"), class = "sr-only", help)
    )
  }
  tagList(
    div(class = "cerebro-preferences-heading", "Preferences"),
    preference(
      "webgl_checkbox",
      "GPU acceleration",
      TRUE,
      "Automatically uses WebGPU or WebGL for supported plots. Turn off to use CPU rendering. Small or unsupported cell views may use Canvas."
    ),
    preference(
      "hover_info_in_projections_checkbox",
      "Cell tooltips",
      isTRUE(Cerebro.options[['projections_show_hover_info']] %||% TRUE),
      "Show brief information on hover in all cell views and the HLA motif network. Click a cell to pin its summary and open Details, even when tooltips are off. Cross-view highlighting is controlled separately."
    ),
    preference(
      "expression_on_top_checkbox",
      "Highest on top",
      ## The author's Gene expression default.
      identical(
        tryCatch(
          current_page_display_defaults("gene")$plotting_order,
          error = function(error) NULL
        ),
        "Highest expression on top"
      ),
      "Gene expression draws the cells with the highest expression on top. Off keeps the input cell order, like Seurat FeaturePlot(order = FALSE). The same as Highest expression on top in the page's settings."
    )
  )
})

##----------------------------------------------------------------------------##
## "Highest on top" and Gene expression's "Highest expression on top" are one
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
