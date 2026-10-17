##----------------------------------------------------------------------------##
## Select group.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element to select which group should be shown.
##----------------------------------------------------------------------------##
output[["groups_controls_UI"]] <- renderUI({
  groups <- getGroups()
  if (!length(groups)) {
    return(p(`data-composition-unavailable` = "true", "No grouping variables are available."))
  }
  selected_group <- groups[[1L]]
  primary <- selectInput(
    "groups_selected_group",
    label = "Grouping variable",
    choices = groups,
    selected = selected_group,
    width = "100%"
  )
  if (length(groups) == 1L) {
    return(tagList(
      primary,
      p(`data-composition-unavailable` = "true",
        "At least two grouping variables are needed to compare composition.")
    ))
  }
  tagList(
    primary,
    selectInput(
      "groups_by_other_group_second_group",
      label = "Group to compare to:",
      choices = groups[groups != selected_group]
    ),
    fluidRow(
      column(
        width = 3,
        shinyWidgets::radioGroupButtons(
          inputId = "groups_by_other_group_plot_type",
          label = "Plot type",
          choices = c("Bar chart", "Sankey plot"),
          status = "primary",
          justified = TRUE,
          width = "100%",
          size = "sm"
        )
      ),
      column(
        width = 9,
        style = "padding: 5px;",
        shinyWidgets::materialSwitch(
          inputId = "groups_by_other_group_show_as_percent",
          label = "Show composition as percent [%] (not in Sankey plot):",
          status = "primary",
          inline = TRUE
        ),
        shinyWidgets::materialSwitch(
          inputId = "groups_by_other_group_show_table",
          label = "Show table:",
          status = "primary",
          inline = TRUE
        )
      )
    )
  )
})

observeEvent(
  input[["groups_selected_group"]],
  {
    groups <- getGroups()
    req(length(groups) >= 2L, input[["groups_selected_group"]] %in% groups)
    choices <- groups[groups != input[["groups_selected_group"]]]
    current <- input[["groups_by_other_group_second_group"]]
    selected <- if (!is.null(current) && current %in% choices) {
      current
    } else {
      choices[[1L]]
    }
    updateSelectInput(
      session,
      "groups_by_other_group_second_group",
      choices = choices,
      selected = selected
    )
  },
  ignoreInit = TRUE
)
