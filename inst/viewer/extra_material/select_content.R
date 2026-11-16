##----------------------------------------------------------------------------##
## Select category and content.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element to set layout for selection of category and specific content,
## which are split because the content depends on which category is selected.
##----------------------------------------------------------------------------##
## The category shown: the selected one, or the first before the selector has
## reported. A reactiveVal, so that the selector reporting the category the
## page already shows changes nothing.
extra_material_category <- reactiveVal(NULL)
observe(
  {
    categories <- getExtraMaterialCategories()
    chosen <- input[["extra_material_selected_category"]]
    extra_material_category(
      if (!is.null(chosen) && chosen %in% categories) {
        chosen
      } else if (length(categories)) {
        categories[[1L]]
      }
    )
  },
  priority = 100
)

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
    ## Filled at once with the first content's selectors, so that the table
    ## does not wait for this output's first round trip.
    htmltools::tagAppendChildren(
      uiOutput("extra_material_selected_content_UI"),
      tryCatch(
        extra_material_content_selectors(categories, categories[[1L]]),
        shiny.silent.error = function(error) NULL
      )
    )
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
  extra_material_content_selectors(
    categories,
    category,
    input[["extra_material_selected_file"]],
    isolate(input[["extra_material_selected_content"]])
  )
})

## What the content selectors of `category` offer and select: the files and
## tables of "tables", `selected_file` and `selected_sheet` while offered, else
## the first ones; the plots of "plots", the first one. The selectors are drawn
## with these values, and content.R publishes them until the selectors bind.
## NULL when the category has nothing to show.
extra_material_content_values <- function(
  category,
  selected_file = NULL,
  selected_sheet = NULL
) {
  ## if selected category is `tables`
  if (identical(category, "tables") && checkForExtraTables()) {
    groups <- extra_material_table_groups()
    file_choices <- extra_material_table_choices(groups)
    req(length(file_choices))
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
    if (is.null(selected_sheet) || !selected_sheet %in% unname(sheet_choices)) {
      selected_sheet <- unname(sheet_choices)[[1L]]
    }
    list(
      file_choices = file_choices,
      file = selected_file,
      content_choices = sheet_choices,
      content = selected_sheet
    )
    ## if selected category is `plots`
  } else if (identical(category, "plots") && isTRUE(checkForExtraPlots())) {
    choices <- getNamesOfExtraPlots()
    list(content_choices = choices, content = viewerFirstChoice(choices))
  }
}

## The selectors of a category's content: the file and table of "tables", the
## plot of "plots".
extra_material_content_selectors <- function(
  categories,
  category,
  selected_file = NULL,
  selected_sheet = NULL
) {
  req(category %in% categories)
  values <- extra_material_content_values(
    category,
    selected_file,
    selected_sheet
  )
  if (is.null(values)) {
    return(NULL)
  }
  if (category == "tables") {
    tagList(
      if (length(values$file_choices)) {
        selectInput(
          "extra_material_selected_file",
          label = "File:",
          choices = values$file_choices,
          selected = values$file,
          width = "100%"
        )
      },
      if (length(values$content_choices)) {
        selectInput(
          "extra_material_selected_content",
          label = "Table:",
          choices = values$content_choices,
          selected = values$content,
          width = "100%"
        )
      }
    )
  } else {
    selectInput(
      "extra_material_selected_content",
      label = "Plot:",
      choices = values$content_choices,
      selected = values$content,
      width = "100%"
    )
  }
}
