##----------------------------------------------------------------------------##
## Sample info.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI elements that show some basic information about the loaded data set.
##----------------------------------------------------------------------------##
#
output[["load_data_sample_info_UI"]] <- renderUI({
  tagList(
    h3("Sample information"),
    fluidRow(
      column(width = 10, valueBoxOutput("load_data_number_of_cells")),
      column(width = 10, valueBoxOutput("load_data_organism")),
      column(width = 10, valueBoxOutput("load_data_date_of_export"))
    )
  )
})

##----------------------------------------------------------------------------##
## Value boxes that show:
## - number of cells in data set
## - organism
## - date of export
##----------------------------------------------------------------------------##

##number of cells
output[["load_data_number_of_cells"]] <- renderValueBox({
  valueBox(
    value = formatC(
      nrow(data_set()$meta_data),
      format = "f",
      big.mark = ",",
      digits = 0
    ),
    subtitle = "Cells",
    color = "light-blue",
    icon = icon("list"),
  )
})

## output[["load_data_number_of_cells"]] <- renderValueBox({
##   box(
##       title = "Cells",
##       width = 5,
##       background = "light-blue",
##       formatC(nrow(data_set()$meta_data), format = "f", big.mark = ",", digits = 0),
##   )
## })

## organism
output[["load_data_organism"]] <- renderValueBox({
  if (getExperiment()$organism == "hg") {
    valueBox(
      value = ifelse(
        !is.null(getExperiment()$organism),
        getExperiment()$organism,
        "not available"
      ),
      subtitle = "Organism",
      color = "yellow",
      icon = icon("user")
    )
  } else {
    valueBox(
      value = ifelse(
        !is.null(getExperiment()$organism),
        getExperiment()$organism,
        "not available"
      ),
      subtitle = "Organism",
      color = "yellow",
      icon = icon("paw")
    )
  }
})

## date of export
## as.character() because the date is otherwise converted to interger
output[["load_data_date_of_export"]] <- renderValueBox({
  valueBox(
    value = ifelse(
      !is.null(getExperiment()$date_of_export),
      as.character(getExperiment()$date_of_export),
      "not available"
    ),
    subtitle = "Date",
    color = "green",
    icon = icon("calendar-day")
  )
})
