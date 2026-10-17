##----------------------------------------------------------------------------##
## Tab: Color management
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element with color selection boxes for each level in each grouping
## variable.
##----------------------------------------------------------------------------##

output[["color_assignments_UI"]] <- renderUI({
  req(data_set())
  ## Inputs already display the user's selected values. Read initial colours
  ## without rebuilding every picker when one input (or its binding) changes.
  ## Group, cell-cycle and dataset dependencies below still rebuild the UI.
  initial_colors <- isolate(reactive_colors())
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
                color_list[[group_level]] <- colourpicker::colourInput(
                  inputId = color_input_id(group_name, group_level),
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
                  color_list[[state]] <- colourpicker::colourInput(
                    inputId = color_input_id(column, state),
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
