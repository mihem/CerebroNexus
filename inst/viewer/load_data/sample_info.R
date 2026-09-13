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
    ## Three stat cards laid out in one row (4/4/4 of the 12-col grid); each
    ## valueBoxOutput's own width is cleared so the enclosing column controls it.
    ## Columns stack automatically on narrow screens.
    fluidRow(
      column(
        width = 4,
        valueBoxOutput("load_data_number_of_cells", width = NULL)
      ),
      column(width = 4, valueBoxOutput("load_data_organism", width = NULL)),
      column(
        width = 4,
        valueBoxOutput("load_data_date_of_export", width = NULL)
      )
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
      getNumberOfCells(),
      format = "f",
      big.mark = ",",
      digits = 0
    ),
    subtitle = "Cells",
    color = "light-blue",
    icon = icon("list"),
  )
})

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
