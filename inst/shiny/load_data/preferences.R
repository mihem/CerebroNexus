##----------------------------------------------------------------------------##
## Tab: Preferences
##----------------------------------------------------------------------------##

##
output[["preferences_options"]] <- renderUI({
  tagList(
      h3("Preferences"),
      shinyWidgets::prettySwitch(
        "webgl_checkbox",
        label = "Switch on WebGL for better performance. Note that this might not be compatible with every browser.",
        value = TRUE
      ),
      shinyWidgets::prettySwitch(
        "hover_info_in_projections_checkbox",
        label = "Switch on hover info to see additional metadata of each cell when hovering. Note that this increases plotting time.",
        value = Cerebro.options[['projections_show_hover_info']]
      )
    )
})

##----------------------------------------------------------------------------##
## Observe WebGL on?
##----------------------------------------------------------------------------##
observeEvent(input[["webgl_checkbox"]], {
  preferences[["use_webgl"]] <- input[["webgl_checkbox"]]
  print(glue::glue("[{Sys.time()}] WebGL status: {preferences[['use_webgl']]}"))
})

##----------------------------------------------------------------------------##
## Observe hover on?
##----------------------------------------------------------------------------##

observeEvent(input[["hover_info_in_projections_checkbox"]], {
  preferences[["show_hover_info_in_projections"]] <- input[["hover_info_in_projections_checkbox"]]
  print(glue::glue("[{Sys.time()}] Show hover info status: {preferences[['show_hover_info_in_projections']]}"))
})
