##----------------------------------------------------------------------------##
## Select category and content.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element to set layout for selection of category and specific content,
## which are split because the content depends on which category is selected.
##----------------------------------------------------------------------------##
output[["extra_material_select_category_and_content_UI"]] <- renderUI({
  categories <- getExtraMaterialCategories()
  if (!length(categories)) {
    return(p("No extra material available."))
  }
  selector <- selectInput(
    "extra_material_selected_category",
    label = "Material type:",
    choices = categories,
    selected = categories[[1L]],
    width = "100%"
  )
  div(
    class = "cerebro-viz-toolbar",
    if (length(categories) == 1L) {
      div(style = "display:none", selector)
    } else {
      selector
    },
    uiOutput("extra_material_selected_content_UI")
  )
})

##----------------------------------------------------------------------------##
## UI element to select which content should be shown.
##----------------------------------------------------------------------------##
output[["extra_material_selected_content_UI"]] <- renderUI({
  categories <- getExtraMaterialCategories()
  req(length(categories))
  category <- input[["extra_material_selected_category"]]
  if (is.null(category)) {
    category <- categories[[1L]]
  }
  req(category %in% categories)
  ## if selected category is `tables`
  if (
    category == "tables" &&
      checkForExtraTables()
  ) {
    groups <- extra_material_table_groups()
    file_choices <- extra_material_table_choices(groups)
    req(length(file_choices))
    selected_file <- input[["extra_material_selected_file"]]
    if (is.null(selected_file) || !selected_file %in% unname(file_choices)) {
      selected_file <- unname(file_choices)[[1L]]
    }
    selection <- extra_material_table_selection(
      groups,
      file_key = selected_file,
      load = FALSE
    )
    req(!is.null(selection))
    sheet_choices <- stats::setNames(
      vapply(selection$group$sheets, `[[`, character(1), "key"),
      vapply(selection$group$sheets, `[[`, character(1), "label")
    )
    selected_sheet <- isolate(input[["extra_material_selected_content"]])
    if (is.null(selected_sheet) || !selected_sheet %in% unname(sheet_choices)) {
      selected_sheet <- unname(sheet_choices)[[1L]]
    }
    tagList(
      if (length(file_choices)) {
        selectInput(
          "extra_material_selected_file",
          label = "File:",
          choices = file_choices,
          selected = selected_file,
          width = "100%"
        )
      },
      if (length(sheet_choices)) {
        selectInput(
          "extra_material_selected_content",
          label = "Table:",
          choices = sheet_choices,
          selected = selected_sheet,
          width = "100%"
        )
      }
    )
    ## if selected category is `plots`
  } else if (
    category == 'plots' &&
      checkForExtraPlots() == TRUE
  ) {
    ##
    selectInput(
      "extra_material_selected_content",
      label = "Plot:",
      choices = getNamesOfExtraPlots(),
      width = "100%"
    )
  }
})
