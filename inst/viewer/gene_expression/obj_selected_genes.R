##----------------------------------------------------------------------------##
## Reactive data that holds genes provided by user or in selected gene set.
##----------------------------------------------------------------------------##
expression_selected_genes_input <- reactive({
  req(
    input[["expression_analysis_mode"]],
    list_of_genes()
  )
  ## prepare empty list for data
  gene_sets <- list(
    "genes_to_display" = character(),
    "genes_to_display_present" = character(),
    "genes_to_display_missing" = character(),
    "rgb_genes" = list(r = NULL, g = NULL, b = NULL)
  )
  ## ...
  if (input[["expression_analysis_mode"]] == "Gene(s)") {
    if (
      identical(
        input[["expression_projection_genes_in_separate_panels"]],
        "rgb"
      )
    ) {
      rgb <- lapply(c("r", "g", "b"), function(channel) {
        input[[paste0("expression_rgb_gene_", channel)]]
      })
      names(rgb) <- c("r", "g", "b")
      if (
        !any(vapply(
          rgb,
          function(gene) {
            !is.null(gene) && length(gene) && nzchar(gene[[1L]])
          },
          logical(1)
        ))
      ) {
        previous <- head(input[["expression_genes_input"]] %||% character(), 3)
        rgb[seq_along(previous)] <- as.list(previous)
      }
      gene_sets[["rgb_genes"]] <- rgb
      gene_sets[["genes_to_display"]] <- unique(unlist(Filter(
        function(gene) !is.null(gene) && nzchar(gene),
        rgb
      )))
    } else {
      ## check if user provided input in gene box
      ## ... if user provided input
      if (!is.null(input[["expression_genes_input"]])) {
        ## - grab user input
        ## - split by comma, space, semicolon and line
        ## - convert to vector
        ## - remove spaces
        ## - remove duplicated strings
        ## - remove empty strings
        gene_sets[["genes_to_display"]] <- input[["expression_genes_input"]] %>%
          strsplit(",| |;|\n") %>%
          unlist() %>%
          gsub(pattern = " ", replacement = "", fixed = TRUE) %>%
          unique() %>%
          .[. != ""]
      }
    }
    ## ...
  } else if (input[["expression_analysis_mode"]] == "Gene set") {
    req(input[["expression_select_gene_set"]])
    gene_sets[["genes_to_display"]] <- getGenesForGeneSet(input[[
      "expression_select_gene_set"
    ]])
  }
  ## check which are available in the data set
  genes_to_display_here <- list_of_genes()[match(
    tolower(gene_sets[["genes_to_display"]]),
    tolower(list_of_genes())
  )]
  ## get which genes are available in the data set
  gene_sets[["genes_to_display_present"]] <- na.omit(genes_to_display_here)
  ## get names of provided genes that are not in the data set
  gene_sets[["genes_to_display_missing"]] <- gene_sets[[
    "genes_to_display"
  ]][which(is.na(genes_to_display_here))]
  ## Channel names must use the same dataset spelling as expression rows.
  ## Preserve channel positions (including repeated and absent genes).
  gene_sets[["rgb_genes"]] <- lapply(gene_sets[["rgb_genes"]], function(gene) {
    if (length(gene) != 1L || is.na(gene) || !nzchar(gene)) {
      return(NULL)
    }
    index <- match(tolower(gene), tolower(list_of_genes()))
    if (is.na(index)) NULL else unname(list_of_genes()[index])
  })
  return(gene_sets)
})

## Let a multi-select gesture settle before any million-cell expression read.
## The first value still initializes immediately.
expression_selected_genes <- debounceAfterFirst(
  expression_selected_genes_input,
  200
)
