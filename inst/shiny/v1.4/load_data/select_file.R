##----------------------------------------------------------------------------##
## Tab: Load data
##
## Select file.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element to select data to load into Cerebro.
##----------------------------------------------------------------------------##

output[["load_data_select_file_UI"]] <- renderUI({
  if (
    exists('Cerebro.options') &&
      !is.null(Cerebro.options[['mode']]) &&
      Cerebro.options[["mode"]] != "closed"
  ) {
    tagList(
      fluidRow(
        htmlOutput("load_data_mode_open")
      ),
      fluidRow(
        column(
          12,
          titlePanel("Load data"),
          fileInput(
            inputId = "input_file",
            label = "Select input data (.crb or .rds file)",
            multiple = FALSE,
            accept = c(".rds", ".crb", ".cerebro"),
            width = '350px',
            buttonLabel = "Browse...",
            placeholder = "No file selected"
          )
        )
      )
    )
  } else {
    fluidRow(
      htmlOutput("load_data_mode_closed")
    )
  }
})

##----------------------------------------------------------------------------##
## Text message if Cerebro was launched in "open" mode.
##----------------------------------------------------------------------------##

output[["load_data_mode_open"]] <- renderText({
  if (
    exists('Cerebro.options') &&
      !is.null(Cerebro.options[["welcome_message"]])
  ) {
    HTML(Cerebro.options[["welcome_message"]])
  } else {
    HTML(
      "<h3 style='text-align: center; margin-top: 0px'><strong>Welcome to Cerebro!</strong></h3>
      <p style='text-align: center'>Please load your data set or take a look at the pre-loaded data.</p>"
    )
  }
})

##----------------------------------------------------------------------------##
## Text message if Cerebro was launched in "closed" mode.
##----------------------------------------------------------------------------##

output[["load_data_mode_closed"]] <- renderText({
  if (
    exists('Cerebro.options') &&
      !is.null(Cerebro.options[["welcome_message"]])
  ) {
    HTML(Cerebro.options[["welcome_message"]])
  } else {
    HTML(
      "<h3 style='text-align: center; margin-top: 0px'><strong>Welcome to Cerebro!</strong></h3>
      <p style='text-align: center'>Cerebro was launched in 'closed' mode, which means you cannot load your own data set. Instead, take a look at the pre-loaded data.</p>
      <br>"
    )
  }
})

##    checkboxInput(
##                 "hover_checkbox",
##                 label = "Switch on hover info to see additional metadata of each cell. Note that this increases plotting time.",
##                 value = Cerebro.options[['projections_show_hover_info']],
##                 ),
##             checkboxInput(
##                 "webgl_checkbox",
##                 label = "Using WebGL is best for performance but might not be compatible with every browser",
##                 value = TRUE,
##                 )

## ##----------------------------------------------------------------------------##
## ## Oberserve event: use webgl?
## ##----------------------------------------------------------------------------##
## observeEvent(input[["webgl_checkbox"]], {
##   preferences[["use_webgl"]] <- input[["webgl_checkbox"]]
##   print(glue::glue("[{Sys.time()}] WebGL status: {preferences[['use_webgl']]}"))
## })

## ##----------------------------------------------------------------------------##
## ## Oberserve event: show hover?
## ##----------------------------------------------------------------------------##
## observeEvent(input[["hover_checkbox"]], {
##   preferences[["show_hover_info_in_projections"]] <- input[["hover_checkbox"]]
##   print(glue::glue("[{Sys.time()}] Show hover info status: {preferences[['show_hover_info_in_projections']]}"))
## })
