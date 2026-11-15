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
    paga = "PAGA (scanpy)",
    scvelo = "RNA velocity (scVelo)",
    pseudotime = "Pseudotime",
    marker_guided = "Marker-guided",
    illustrative = "Illustrative",
    published_pseudotime = "Published pseudotime"
  )
  ## The bound selections while still available, else the first ones; with
  ## nothing bound, what server.R publishes until the selectors bind.
  selected_method <- trajectory_default_method(
    isolate(input[["trajectory_selected_method"]]),
    available_methods
  )
  available_names <- getNamesOfTrajectories(selected_method)
  selected_name <- trajectory_default_name(
    selected_method,
    isolate(input[["trajectory_selected_name"]]),
    available_names
  )

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
      selected = trajectory_default_point_color(colour_groups),
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
    selected_name <- trajectory_default_name(
      method,
      isolate(input[["trajectory_selected_name"]]),
      available_names
    )
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
