## ---- Settings UI ------------------------------------------------------ ##
output$ir_settings_UI <- renderUI({
  req(has_scRepertoire())
  raw <- ir_data_raw()
  if (is.null(raw)) {
    return(div(
      class = "alert alert-warning",
      "No immune repertoire data available. Import data with TCR/BCR annotations first."
    ))
  }

  chains_present <- detect_chains(raw)
  tcr_present <- intersect(chains_present, c("TRA", "TRB", "TRG", "TRD"))
  bcr_present <- intersect(chains_present, c("IGH", "IGK", "IGL"))
  chain_choices <- list("All" = "both")
  if (length(tcr_present) > 0) {
    chain_choices[["TCR"]] <- as.list(setNames(tcr_present, tcr_present))
  }
  if (length(bcr_present) > 0) {
    chain_choices[["BCR"]] <- as.list(setNames(bcr_present, bcr_present))
  }

  # Grouping options come from the data set's declared grouping variables
  # joined onto the IR data by barcode (see ir_data_annotated), so users can
  # group by ANY metadata column (sample, condition, treatment, cell type, ...)
  # rather than only columns embedded in the IR table itself.
  annotated <- ir_data_annotated()
  data_cols <- if (!is.null(annotated)) names(annotated[[1]]) else names(raw[[1]])
  groups <- tryCatch(getGroups(), error = function(e) character(0))
  available_groups <- intersect(groups, data_cols)

  sample_col_opts <- c("(original)" = "(original)", ir_sample_col_choices())

  tagList(
    tags$style(
      "#ir_chain + .selectize-control .selectize-dropdown-content { max-height: none; }"
    ),
    fluidRow(
      column(
        4,
        selectInput(
          "ir_cloneCall",
          "Clone call:",
          choices = c("gene", "nt", "aa", "strict"),
          selected = "gene"
        )
      ),
      column(
        4,
        selectInput(
          "ir_groupBy",
          "Group by:",
          choices = c("None" = "", available_groups),
          selected = "",
          selectize = FALSE
        )
      ),
      column(
        4,
        selectInput(
          "ir_sampleCol",
          "Sample column:",
          choices = sample_col_opts,
          selected = "(original)"
        )
      )
    ),
    fluidRow(
      column(
        6,
        selectInput(
          "ir_chain",
          "Chain:",
          choices = chain_choices,
          selected = "both"
        )
      )
    ),
    uiOutput("ir_scatter_settings")
  )
})

## ---- Dynamic scatter/compare settings (depend on ir_data) ------------- ##
output$ir_scatter_settings <- renderUI({
  data <- ir_data()
  if (is.null(data) || length(data) < 2) {
    return(NULL)
  }
  available_samples <- names(data)
  tagList(
    fluidRow(
      column(
        6,
        selectInput(
          "ir_scatter_x",
          "Sample 1 (Scatter):",
          choices = available_samples,
          selected = available_samples[1]
        )
      ),
      column(
        6,
        selectInput(
          "ir_scatter_y",
          "Sample 2 (Scatter):",
          choices = available_samples,
          selected = available_samples[2]
        )
      )
    ),
    fluidRow(
      column(
        12,
        selectInput(
          "ir_compare_samples",
          "Samples for Compare (select >= 2):",
          choices = available_samples,
          multiple = TRUE,
          selected = available_samples[1:min(2, length(available_samples))]
        )
      )
    )
  )
})

## ---- Reactive: number of samples -------------------------------------- ##
n_samples <- reactive({
  data <- ir_data()
  if (is.null(data)) 0L else length(data)
})
