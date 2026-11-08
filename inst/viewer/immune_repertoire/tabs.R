## ---- Remember the last selected tab ----------------------------------- ##
## The visualizations tabsetPanel is rebuilt by renderUI when the data set
## changes. Without this, the rebuild resets the selection to the first tab
## (Abundance). We remember the user's current tab so the rebuild can restore
## it (see visualizations.R).
ir_last_tab <- reactiveVal(NULL)
observeEvent(input$ir_tabs, {
  if (!is.null(input$ir_tabs) && nzchar(input$ir_tabs)) {
    ir_last_tab(input$ir_tabs)
  }
})

## The sub-tabs this dataset shows: those the Builder kept (every one when the
## App has no choice) that its sample count supports.
ir_visible_tabs <- reactive({
  viewerImmuneRepertoireVisibleTabs(
    Cerebro.options,
    viewer_requested_dataset_context()$dataset_key,
    n_samples()
  )
})

## The first visible sub-tab: Clonal projection unless the Builder left it out.
## The sample count is read only when the first chosen tab depends on it, so
## the packed Clonal projection landing never waits for the repertoire.
ir_default_tab <- function() {
  first <- viewerImmuneRepertoireVisibleTabs(
    Cerebro.options,
    viewer_requested_dataset_context()$dataset_key,
    Inf
  )[[1L]]
  tabs <- viewerImmuneRepertoireTabs()
  if (tabs$min_samples[match(first, tabs$id)] <= 1L) {
    first
  } else {
    ir_visible_tabs()[[1L]]
  }
}

## The tab the strip opens on (see visualizations.R): the remembered tab, else
## the first visible one. The clonal projection reads it through
## viewerControlValue() and is drawn before the strip reaches the browser.
ir_tab <- viewerControlValue(input, "ir_tabs", function() {
  if (!has_scRepertoire() || !ir_repertoire_available()) {
    return(NULL)
  }
  isolate(ir_last_tab()) %||% ir_default_tab()
})

## ---- Attach tooltips to tab links via JS ------------------------------ ##
observe({
  tab <- input$ir_tabs
  if (is.null(tab)) {
    return()
  }
  # Build JS to add title attributes to all tab links in ir_tabs
  js_lines <- vapply(
    names(ir_tab_help),
    function(name) {
      tip <- ir_tab_help[[name]]$short
      # Escape quotes for JS
      tip <- gsub("'", "\\\\'", tip)
      sprintf(
        "$('#ir_tabs a[data-value=\"%s\"]').attr('title', '%s');",
        name,
        tip
      )
    },
    character(1)
  )
  shinyjs::runjs(paste(js_lines, collapse = "\n"))
})
