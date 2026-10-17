test_that("gene validation reports missing names and clears when they resolve", {
  shiny::testServer(
    function(input, output, session) {
      expression_selected_genes <- shiny::reactive(list(
        genes_to_display_missing = input$missing
      ))
      sys.source(
        viewer_test_path("gene_expression", "out_genes_displayed.R"),
        environment()
      )
    },
    {
      session$setInputs(missing = character())
      ## The notice container is always rendered (the separate-panels fallback
      ## shares it), so an empty selection has to render no notice at all.
      feedback <- output$expression_genes_displayed$html
      expect_false(grepl("Not found in this data set:", feedback, fixed = TRUE))
      session$setInputs(missing = c("UnknownGeneA", "UnknownGeneB"))
      feedback <- output$expression_genes_displayed$html
      expect_match(feedback, "Not found in this data set:", fixed = TRUE)
      expect_match(feedback, "UnknownGeneA, UnknownGeneB", fixed = TRUE)
      session$setInputs(missing = character())
      expect_false(grepl(
        "Not found in this data set:",
        output$expression_genes_displayed$html,
        fixed = TRUE
      ))
    }
  )
})
