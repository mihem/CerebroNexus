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
    )
  )
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
