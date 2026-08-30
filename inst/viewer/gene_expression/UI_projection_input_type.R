##----------------------------------------------------------------------------##
## UI elements to choose whether gene(s) or gene sets should be analyzed
##----------------------------------------------------------------------------##
output[["expression_projection_input_type_UI"]] <- renderUI({
  req(input[["expression_analysis_mode"]])
  if (input[["expression_analysis_mode"]] == "Gene(s)") {
    if (
      identical(
        input[["expression_projection_genes_in_separate_panels"]],
        "rgb"
      )
    ) {
      previous <- head(
        isolate(input[["expression_genes_input"]]) %||% character(),
        3
      )
      channel_input <- function(channel, label, selected = NULL) {
        selected <- if (
          length(selected) == 1 && !is.na(selected) && nzchar(selected)
        ) {
          selected
        } else {
          ""
        }
        genes <- list_of_genes()
        selectizeInput(
          paste0("expression_rgb_gene_", channel),
          label = label,
          choices = stats::setNames(c("", genes), c("", genes)),
          selected = selected,
          multiple = FALSE,
          options = list(
            create = FALSE,
            allowEmptyOption = TRUE,
            placeholder = paste(tolower(label), "gene...")
          )
        )
      }
      return(div(
        class = "cerebro-gene-rgb-row",
        channel_input(
          "r",
          "Red channel",
          if (length(previous) >= 1) previous[[1]]
        ),
        channel_input(
          "g",
          "Green channel",
          if (length(previous) >= 2) previous[[2]]
        ),
        channel_input(
          "b",
          "Blue channel",
          if (length(previous) >= 3) previous[[3]]
        )
      ))
    }
    selectizeInput(
      'expression_genes_input',
      label = 'Gene(s)',
      choices = data.table::as.data.table(data.frame(
        "Genes" = list_of_genes()
      )),
      selected = isolate(input[["expression_genes_input"]]),
      multiple = TRUE,
      options = list(
        create = TRUE,
        plugins = list("remove_button")
      )
    )
  } else if (input[["expression_analysis_mode"]] == "Gene set") {
    selectizeInput(
      'expression_select_gene_set',
      label = 'Gene set',
      choices = data.table::as.data.table(
        data.frame("Gene sets" = c("-", msigdbr:::msigdbr_genesets$gs_name))
      ),
      multiple = FALSE
    )
  }
})
