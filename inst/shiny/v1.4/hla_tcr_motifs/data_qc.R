##----------------------------------------------------------------------------##
## HLA & TCR Motifs — Data & QC tab
##
## The trust centre: shows the active HLA source and its provenance, per-sample
## coverage, and lets the user upload a CSV/TSV HLA table as a SESSION-ONLY
## override (never written back to the .crb). Sample matching is exact only;
## unmatched samples are reported, never fuzzy-guessed.
##----------------------------------------------------------------------------##

## ---- Upload handler: normalize into a session override ---------------- ##
observeEvent(input$hla_upload, {
  f <- input$hla_upload
  if (is.null(f)) {
    return()
  }
  # Delimiter comes from the ORIGINAL name (f$name), not the temp path Shiny
  # hands over. Reading itself lives in hla_read_typing_file so it is unit
  # testable -- it is where the wide format's column names get preserved.
  raw <- tryCatch(
    hla_read_typing_file(f$datapath, name = f$name),
    error = function(e) NULL
  )
  if (is.null(raw) || nrow(raw) == 0) {
    hla_session_typing(NULL)
    showNotification(
      "Could not read the uploaded file as CSV/TSV.",
      type = "error"
    )
    return()
  }
  # Uploaded HLA is session-only; its provenance is whatever the user declares,
  # defaulting to unknown (never guessed as genotyped from format).
  st <- input$hla_upload_source_type
  if (is.null(st) || !nzchar(st)) {
    st <- "unknown"
  }
  norm <- tryCatch(
    hla_normalize_typing(
      raw,
      source_type = st,
      typing_method = "session upload"
    ),
    error = function(e) NULL
  )
  if (is.null(norm) || nrow(norm) == 0) {
    hla_session_typing(NULL)
    showNotification(
      "No valid HLA alleles found after normalization. Check the format.",
      type = "warning"
    )
    return()
  }
  hla_session_typing(norm)
  showNotification(
    sprintf(
      "Loaded %d alleles for %d samples (session only).",
      length(unique(norm$allele)),
      length(unique(norm$sample))
    ),
    type = "message"
  )
})

## ---- Clear the session override --------------------------------------- ##
observeEvent(input$hla_clear_session, {
  hla_session_typing(NULL)
  showNotification("Session HLA override cleared.", type = "message")
})

## ---- QC warnings of the active typing --------------------------------- ##
hla_qc_table <- reactive({
  t <- hla_active_typing()
  qc <- attr(t, "qc")
  if (is.null(qc) || nrow(qc) == 0) {
    return(NULL)
  }
  qc
})

## ---- The Data & QC tab body ------------------------------------------- ##
output$hla_data_qc_ui <- renderUI({
  ir_samples <- names(getImmuneRepertoire())
  tagList(
    fluidRow(
      column(
        width = 6,
        tags$h4("Provide HLA typing"),
        tags$p(
          class = "text-muted",
          style = "font-size: 12px;",
          paste(
            "Upload a CSV/TSV. Accepted shapes: a long table (sample, locus,",
            "allele) or a wide table (a 'sample' column plus HLA-*_1 / HLA-*_2",
            "columns). Uploads are session-only and never modify the .crb."
          )
        ),
        selectInput(
          "hla_upload_source_type",
          "Data provenance:",
          choices = c(
            "Directly genotyped" = "genotyped",
            "Imputed" = "imputed",
            "Synthetic (demo)" = "synthetic",
            "Unknown" = "unknown"
          ),
          selected = "unknown"
        ),
        fileInput(
          "hla_upload",
          NULL,
          accept = c(".csv", ".tsv", "text/csv", "text/tab-separated-values")
        ),
        actionButton("hla_clear_session", "Clear session override"),
        tags$hr(),
        downloadButton("hla_download_template", "Download CSV template")
      ),
      column(
        width = 6,
        tags$h4("Coverage"),
        uiOutput("hla_active_source_ui"),
        tags$p(
          class = "text-muted",
          style = "font-size: 12px;",
          sprintf("Immune-repertoire samples: %d", length(ir_samples))
        ),
        DT::dataTableOutput("hla_coverage_table"),
        uiOutput("hla_mapping_note")
      )
    ),
    tags$hr(),
    fluidRow(
      column(
        width = 7,
        tags$h4("Normalized active typing preview"),
        DT::dataTableOutput("hla_normalized_preview"),
        downloadButton("hla_download_normalized", "Download normalized CSV")
      ),
      column(
        width = 5,
        tags$h4("Sample → analysis-unit mapping"),
        DT::dataTableOutput("hla_donor_mapping_preview")
      )
    ),
    tags$hr(),
    tags$h4("Quality control"),
    uiOutput("hla_qc_ui")
  )
})

output$hla_active_source_ui <- renderUI({
  session_on <- !is.null(hla_session_typing()) &&
    is.data.frame(hla_session_typing()) &&
    nrow(hla_session_typing()) > 0
  t <- hla_active_typing()
  if (!is.data.frame(t) || nrow(t) == 0) {
    return(tags$p(class = "text-muted", "Active HLA source: none"))
  }
  tags$p(
    tags$b("Active HLA source: "),
    if (session_on) "session override" else "stored .crb",
    sprintf(" (%s)", paste(unique(t$source_type), collapse = ", "))
  )
})

## ---- Coverage table (per sample) -------------------------------------- ##
output$hla_coverage_table <- DT::renderDataTable({
  t <- hla_active_typing()
  if (!hla_has_typing()) {
    return(NULL)
  }
  cov <- hla_coverage_by_sample(t)
  # scrollX + nowrap, like every table on this page: the widths here are the
  # user's data (sample names, and a Loci cell listing every typed locus), not
  # anything this layout can size for. Left to wrap, "HLA-A" breaks across two
  # lines mid-token and an identifier stops reading as one value.
  DT::datatable(
    cov,
    rownames = FALSE,
    colnames = c("Sample", "# alleles", "Loci"),
    class = "display nowrap",
    options = list(pageLength = 10, dom = "t", scrollX = TRUE)
  )
})

output$hla_normalized_preview <- DT::renderDataTable({
  t <- hla_active_typing()
  if (!hla_has_typing()) {
    return(NULL)
  }
  DT::datatable(
    t,
    rownames = FALSE,
    class = "display nowrap",
    options = list(pageLength = 8, scrollX = TRUE, dom = "tip")
  )
})

output$hla_donor_mapping_preview <- DT::renderDataTable({
  if (!hla_has_typing()) {
    return(NULL)
  }
  samples <- names(getImmuneRepertoire())
  map <- hla_analysis_unit_map(hla_active_typing(), samples)
  DT::datatable(
    map,
    rownames = FALSE,
    colnames = c("IR sample", "Analysis unit", "Unit type"),
    class = "display nowrap",
    options = list(pageLength = 10, dom = "t", scrollX = TRUE)
  )
})

## ---- Sample-mapping note (exact match only) --------------------------- ##
output$hla_mapping_note <- renderUI({
  if (!hla_has_typing()) {
    return(NULL)
  }
  t <- hla_active_typing()
  ir_samples <- names(getImmuneRepertoire())
  typed <- unique(t$sample)
  unmatched_typing <- setdiff(typed, ir_samples)
  untyped_ir <- setdiff(ir_samples, typed)
  tagList(
    if (length(untyped_ir) > 0) {
      tags$p(
        class = "text-warning",
        style = "font-size: 12px;",
        sprintf(
          "IR samples without HLA typing: %s",
          paste(untyped_ir, collapse = ", ")
        )
      )
    },
    if (length(unmatched_typing) > 0) {
      tags$p(
        class = "text-warning",
        style = "font-size: 12px;",
        sprintf(
          paste0(
            "HLA samples not matching any IR sample (exact match only, no ",
            "fuzzy matching): %s"
          ),
          paste(unmatched_typing, collapse = ", ")
        )
      )
    }
  )
})

## ---- QC panel --------------------------------------------------------- ##
output$hla_qc_ui <- renderUI({
  qc <- hla_qc_table()
  if (is.null(qc)) {
    return(tags$p(class = "text-muted", "No QC warnings."))
  }
  DT::dataTableOutput("hla_qc_table_dt")
})

output$hla_qc_table_dt <- DT::renderDataTable({
  qc <- hla_qc_table()
  if (is.null(qc)) {
    return(NULL)
  }
  DT::datatable(
    qc,
    rownames = FALSE,
    options = list(pageLength = 10, dom = "t")
  )
})

## ---- CSV template download -------------------------------------------- ##
output$hla_download_template <- downloadHandler(
  filename = function() "hla_typing_template.csv",
  content = function(file) {
    ir_samples <- names(getImmuneRepertoire())
    if (length(ir_samples) == 0) {
      ir_samples <- c("sample_1", "sample_2")
    }
    # A minimal long template the normalizer accepts directly.
    tmpl <- data.frame(
      sample = rep(ir_samples, each = 2),
      locus = "HLA-A",
      allele = c("HLA-A*02:01", "HLA-A*01:01"),
      stringsAsFactors = FALSE
    )
    utils::write.csv(tmpl, file, row.names = FALSE)
  }
)

output$hla_download_normalized <- downloadHandler(
  filename = function() "hla_typing_normalized.csv",
  content = function(file) {
    utils::write.csv(hla_active_typing(), file, row.names = FALSE, na = "")
  }
)
