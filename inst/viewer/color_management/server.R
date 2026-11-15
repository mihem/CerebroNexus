##----------------------------------------------------------------------------##
## Tab: Color management
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element with color selection boxes for each level in each grouping
## variable.
##----------------------------------------------------------------------------##

## colourpicker::colourInput() as this page uses it, for hundreds of levels:
## the same tags, with its dependencies attached once to the page instead of to
## every picker. colourInput() looks up the package's version for each one
## (0.4 ms), which made two thirds of the 0.4 s that 600 pickers took.
##
## A picker waits to be made one (class `cerebro-colour-pending`: an input of
## type "button" showing its colour, which neither the text nor the colour
## binding takes) until it comes near the screen (www/viewer-shell.js):
## the browser took 1.2 s to make the 600 pickers of a data set at once. A level
## without a picker has its default colour (color_picker_ids()).
color_picker_dependencies <- function() {
  htmltools::findDependencies(colourpicker::colourInput("x", "x", "#000000"))
}
color_picker <- function(input_id, label, value) {
  value <- shiny::restoreInput(id = input_id, default = value)
  shiny::div(
    class = "form-group shiny-input-container",
    `data-shiny-input-type` = "colour",
    shiny::tags$label(label, class = "control-label", `for` = input_id),
    shiny::tags$input(
      id = input_id,
      type = "button",
      class = "form-control cerebro-colour-pending",
      value = value,
      `data-init-value` = value,
      `data-show-colour` = "both",
      `data-palette` = "square"
    )
  )
}

output[["color_assignments_UI"]] <- renderUI({
  req(data_set())
  ## colourpicker is a suggested package; say so instead of failing.
  if (!requireNamespace("colourpicker", quietly = TRUE)) {
    return(div(
      class = "alert alert-warning",
      "Changing colours needs the colourpicker package. Install it with ",
      tags$code("install.packages(\"colourpicker\")"),
      " and restart the app."
    ))
  }
  ## Inputs already display the user's selected values. Read initial colours
  ## without rebuilding every picker when one input (or its binding) changes.
  ## Group, cell-cycle and dataset dependencies below still rebuild the UI.
  initial_colors <- isolate(reactive_colors())
  htmltools::attachDependencies(
    tagList(
      fluidRow(
        tagList({
          group_list <- list()
          for (group_name in getGroups()) {
            group_list[[group_name]] <- box(
              title = boxTitle(group_name),
              status = "primary",
              solidHeader = TRUE,
              width = 4,
              collapsible = TRUE,
              tagList({
                color_list <- list()
                for (group_level in getGroupLevels(group_name)) {
                  color_list[[group_level]] <- color_picker(
                    input_id = color_input_id(group_name, group_level),
                    label = group_level,
                    value = initial_colors[[group_name]][group_level]
                  )
                }
                color_list
              })
            )
          }

          ## if there are columns with cell cycle info, add color selection elements
          ## also for those
          if (length(getCellCycle()) > 0) {
            for (column in getCellCycle()) {
              group_list[[column]] <- box(
                title = boxTitle(column),
                status = "primary",
                solidHeader = TRUE,
                width = 4,
                collapsible = TRUE,
                tagList({
                  color_list <- list()
                  for (state in unique(as.character(getMetaData()[[column]]))) {
                    color_list[[state]] <- color_picker(
                      input_id = color_input_id(column, state),
                      label = state,
                      value = initial_colors[[column]][state]
                    )
                  }
                  color_list
                })
              )
            }
          }

          group_list
        })
      )
    ),
    color_picker_dependencies()
  )
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##

observeEvent(input[["color_assignments_info"]], {
  showModal(
    modalDialog(
      color_assignments_info[["text"]],
      title = color_assignments_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##

color_assignments_info <- list(
  title = "Colours for groups",
  text = p(
    "Set group colours for all Viewer pages."
  )
)
