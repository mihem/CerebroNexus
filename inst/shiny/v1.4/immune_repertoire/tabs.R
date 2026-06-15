  ## ---- Tab change: update cloneCall choices ----------------------------- ##
  observeEvent(input$ir_tabs, {
    req(has_scRepertoire())
    tab <- input$ir_tabs
    if (tab %in% c("Length", "K-mer")) {
      updateSelectInput(session, "ir_cloneCall",
        choices = c("nt", "aa"),
        selected = if (input$ir_cloneCall %in% c("nt", "aa")) input$ir_cloneCall else "aa"
      )
    } else if (tab %in% c("Gene usage", "vizGenes", "percentGenes",
                           "percentVJ", "AA %", "Entropy",
                           "Isotype", "SHM Proxy", "Paired Scatter")) {
      updateSelectInput(session, "ir_cloneCall", choices = NULL, selected = NULL)
    } else {
      updateSelectInput(session, "ir_cloneCall",
        choices = c("gene", "nt", "aa", "strict"),
        selected = input$ir_cloneCall
      )
    }
    shinyjs::toggleElement(id = "ir_scatter_x", anim = TRUE,
      condition = tab == "Scatter" && n_samples() >= 2)
    shinyjs::toggleElement(id = "ir_scatter_y", anim = TRUE,
      condition = tab == "Scatter" && n_samples() >= 2)
    shinyjs::toggleElement(id = "ir_compare_samples", anim = TRUE,
      condition = tab == "Compare" && n_samples() >= 2)
  })

  ## ---- Attach tooltips to tab links via JS ------------------------------ ##
  observe({
    tab <- input$ir_tabs
    if (is.null(tab)) return()
    # Build JS to add title attributes to all tab links in ir_tabs
    js_lines <- vapply(names(ir_tab_help), function(name) {
      tip <- ir_tab_help[[name]]$short
      # Escape quotes for JS
      tip <- gsub("'", "\\\\'", tip)
      sprintf(
        "$('#ir_tabs a[data-value=\"%s\"]').attr('title', '%s');",
        name, tip
      )
    }, character(1))
    shinyjs::runjs(paste(js_lines, collapse = "\n"))
  })
