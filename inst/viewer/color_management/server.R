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
## test-viewer-color-config.R compares this markup, made a picker, with
## colourInput()'s, so a colourpicker release that changes it is caught.
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

## The pickers of a group's levels as one piece of HTML: color_picker()'s
## markup for each (test-viewer-color-config.R compares them), written at once
## instead of as a tag per level. Building and rendering the tags of the 1,640
## levels of the Allen atlas took 1.4 s.
color_pickers_html <- function(input_ids, labels, values) {
  if (!length(input_ids)) {
    return(shiny::HTML(""))
  }
  values <- vapply(
    seq_along(input_ids),
    function(i) {
      value <- shiny::restoreInput(id = input_ids[[i]], default = values[[i]])
      if (length(value) != 1L || is.na(value)) {
        NA_character_
      } else {
        as.character(value)
      }
    },
    ""
  )
  id <- htmltools::htmlEscape(input_ids, attribute = TRUE)
  value <- ifelse(
    is.na(values),
    "value data-init-value",
    sprintf(
      'value="%1$s" data-init-value="%1$s"',
      htmltools::htmlEscape(values, attribute = TRUE)
    )
  )
  shiny::HTML(paste0(
    '<div class="form-group shiny-input-container" data-shiny-input-type="colour">\n',
    '  <label class="control-label" for="',
    id,
    '">',
    htmltools::htmlEscape(labels),
    "</label>\n",
    '  <input id="',
    id,
    '" type="button" class="form-control cerebro-colour-pending" ',
    value,
    ' data-show-colour="both" data-palette="square"/>\n',
    "</div>",
    collapse = "\n"
  ))
}

## The pickers of `levels` of `variable`, with their colours in `colors`
## (named by level).
color_group_pickers <- function(variable, levels, colors) {
  levels <- as.character(levels)
  values <- if (is.null(colors)) {
    rep(NA_character_, length(levels))
  } else {
    unname(as.character(colors[levels]))
  }
  color_pickers_html(color_input_id(variable, levels), levels, values)
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
              color_group_pickers(
                group_name,
                getGroupLevels(group_name),
                initial_colors[[group_name]]
              )
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
                color_group_pickers(
                  column,
                  unique(as.character(getMetaData()[[column]])),
                  initial_colors[[column]]
                )
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
