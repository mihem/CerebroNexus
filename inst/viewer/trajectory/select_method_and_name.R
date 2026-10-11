##----------------------------------------------------------------------------##
## Tab: Trajectory
##
## Select method and name.
##----------------------------------------------------------------------------##

## Publish all first-frame selectors in one UI response. The trajectory name is
## updated in place when the method changes, avoiding a second nested renderUI
## round trip during cold start.
output[["trajectory_primary_controls_UI"]] <- renderUI({
  available_methods <- viewerSupportedTrajectoryMethods(
    getMethodsForTrajectories()
  )
  if (length(available_methods) == 0L) {
    return(textOutput("trajectory_missing"))
  }

  method_labels <- c(
    monocle2 = "Monocle 2",
    marker_guided = "Marker-guided",
    illustrative = "Illustrative"
  )
  selected_method <- isolate(input[["trajectory_selected_method"]])
  if (
    length(selected_method) != 1L ||
      is.na(selected_method) ||
      !selected_method %in% available_methods
  ) {
    selected_method <- available_methods[[1L]]
  }
  available_names <- getNamesOfTrajectories(selected_method)
  selected_name <- isolate(input[["trajectory_selected_name"]])
  if (
    length(selected_name) != 1L ||
      is.na(selected_name) ||
      !selected_name %in% available_names
  ) {
    selected_name <- available_names[[1L]]
  }

  colour_groups <- viewerColourGroupChoices()

  tagList(
    selectInput(
      "trajectory_selected_method",
      label = "Choose a method",
      choices = stats::setNames(
        available_methods,
        unname(method_labels[available_methods])
      ),
      selected = selected_method,
      width = "100%"
    ),
    selectInput(
      "trajectory_selected_name",
      label = "Choose a trajectory",
      choices = available_names,
      selected = selected_name,
      width = "100%"
    ),
    selectInput(
      "trajectory_point_color",
      label = "Colour by",
      choices = list(
        "Trajectory" = c("State" = "state", "Pseudotime" = "pseudotime"),
        "Grouping variables" = colour_groups$choices
      ),
      selected = colour_groups$selected %||% "state",
      width = "100%"
    )
  )
})

observeEvent(
  input[["trajectory_selected_method"]],
  {
    method <- input[["trajectory_selected_method"]]
    available_methods <- viewerSupportedTrajectoryMethods(
      getMethodsForTrajectories()
    )
    req(method %in% available_methods)
    available_names <- getNamesOfTrajectories(method)
    selected_name <- isolate(input[["trajectory_selected_name"]])
    if (
      length(selected_name) != 1L ||
        is.na(selected_name) ||
        !selected_name %in% available_names
    ) {
      selected_name <- available_names[[1L]]
    }
    updateSelectInput(
      session,
      "trajectory_selected_name",
      choices = available_names,
      selected = selected_name
    )
  },
  ignoreInit = TRUE
)

output[["trajectory_missing"]] <- renderText({
  "No trajectories available to display."
})
