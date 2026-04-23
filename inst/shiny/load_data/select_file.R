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
    show_upload <- TRUE
    if (!is.null(Cerebro.options[["show_upload_ui"]]) && Cerebro.options[["show_upload_ui"]] == FALSE) {
      show_upload <- FALSE
    }
    tagList(
      fluidRow(
        htmlOutput("load_data_mode_open")
      ),
      fluidRow(
        column(12,
          if (show_upload) titlePanel("Load data"),
          if (show_upload) {
            fileInput(
              inputId = "input_file",
              label = "Select input data (.crb or .rds file)",
              multiple = FALSE,
              accept = c(".rds",".crb",".cerebro"),
              width = '350px',
              buttonLabel = "Browse...",
              placeholder = "No file selected"
            )
          },
          ## Show file selector dropdown if multiple files are available
          uiOutput("crb_file_selector_UI")
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
## UI element for selecting from multiple pre-configured files.
##----------------------------------------------------------------------------##

output[["crb_file_selector_UI"]] <- renderUI({
  ## only show if multiple files are available
  if (!is.null(available_crb_files$files) && length(available_crb_files$files) > 1) {
    ## if names are available (from named list), use them directly
    if (!is.null(available_crb_files$names)) {
      file_choices <- setNames(
        available_crb_files$files,
        available_crb_files$names
      )
    } else {
      ## otherwise, create named list by extracting filename or experiment_name
      file_choices <- setNames(
        available_crb_files$files,
        sapply(available_crb_files$files, function(f) {
          ## try to load the file and get experiment_name
          tryCatch({
            if (file.exists(f)) {
              data <- read_cerebro_file(f)
            } else if (exists(f)) {
              data <- get(f)
            } else {
              return(f)
            }
            ## get experiment_name from the loaded data
            if (!is.null(data$getExperiment())) {
              return(data$getExperiment())
            } else {
              return(f)
            }
          }, error = function(e) {
            ## if anything goes wrong, fall back to filename or variable name
            if (file.exists(f)) {
              basename(f)
            } else {
              f  ## if it's a variable name, use as-is
            }
          })
        })
      )
    }
    tagList(
      titlePanel("Select sample dataset"),
      selectInput(
        inputId = "crb_file_selector",
        label = "Select from available datasets:",
        choices = file_choices,
        selected = available_crb_files$selected,
        width = '350px'
      )
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
