test_that("preferences expose labelled checkbox controls", {
  shiny::testServer(
    function(input, output, session) {
      Cerebro.options <- list(projections_show_hover_info = TRUE)
      viewer_server_load(
        viewer_server_statements("load_data", "preferences.R"),
        environment(),
        names = "output$preferences_options"
      )
    },
    {
      doc <- xml2::read_html(output$preferences_options$html)
      for (id in c("webgl_checkbox", "hover_info_in_projections_checkbox")) {
        control <- xml2::xml_find_first(doc, paste0(".//*[@id='", id, "']"))
        expect_identical(xml2::xml_attr(control, "type"), "checkbox")
        expect_true(nzchar(trimws(xml2::xml_text(xml2::xml_find_first(
          control,
          "ancestor::label"
        )))))
        row <- xml2::xml_find_first(control, "ancestor::*[@data-help]")
        expect_true(nzchar(xml2::xml_attr(row, "data-help")))
      }
      ## Renderer details live in the GPU switch tooltip, not a heading badge.
      expect_length(
        xml2::xml_find_all(doc, ".//*[@id='cerebro-renderer-status']"),
        0L
      )
    }
  )
})

test_that("colour management has one labelled information action", {
  scope <- viewer_ui_test_env()
  sys.source(viewer_test_path("color_management", "UI.R"), scope)
  doc <- xml2::read_html(as.character(scope$tab_color_management))
  expect_length(
    xml2::xml_find_all(doc, ".//*[@id='color_assignments_info']"),
    1L
  )
  expect_equal(
    xml2::xml_text(xml2::xml_find_first(doc, ".//h1")),
    "Colour management"
  )
})

test_that("result selectors have visible labels and explain empty data", {
  shiny::testServer(
    function(input, output, session) {
      choices <- function(...) if (isTRUE(input$empty)) character() else "group"
      getGroups <- getMethodsForMarkerGenes <- getMethodsForEnrichedPathways <- choices
      getGroupsWithMarkerGenes <- getGroupsWithEnrichedPathways <- choices
      getGroupsWithMostExpressedGenes <- getGroupsWithMeanExpression <- choices
      viewer_result_page_load("marker_genes", environment())
      viewer_result_page_load("enriched_pathways", environment())
      for (path in list(
        c("most_expressed_genes", "select_group.R"),
        c("groups", "select_group.R")
      )) {
        sys.source(do.call(viewer_test_path, as.list(path)), environment())
      }
    },
    {
      outputs <- c(
        "marker_genes_select_method_and_table_UI",
        "enriched_pathways_select_method_and_table_UI",
        "most_expressed_genes_select_group_UI",
        "groups_controls_UI"
      )
      for (id in outputs) {
        doc <- xml2::read_html(output[[id]]$html)
        controls <- xml2::xml_find_all(doc, ".//select")
        expect_gt(length(controls), 0L)
        for (control in controls) {
          label <- xml2::xml_find_first(
            doc,
            paste0(".//label[@for='", xml2::xml_attr(control, "id"), "']")
          )
          expect_true(nzchar(xml2::xml_text(label)), info = id)
        }
      }
      session$setInputs(empty = TRUE)
      for (id in outputs) {
        text <- xml2::xml_text(xml2::read_html(output[[id]]$html))
        expect_true(nzchar(trimws(text)))
        expect_no_match(text, "No data available\\.|Data not available")
      }
    }
  )
})

test_that("inactivity warning can be continued or cancelled before expiry", {
  skip_if_no_node()
  scope <- new.env(parent = globalenv())
  viewer_server_load(
    viewer_server_statements("shiny_UI.R"),
    scope,
    names = c("timeoutSeconds", "timeoutWarningSeconds", "inactivity")
  )
  script <- withr::local_tempfile(fileext = ".js")
  writeLines(scope$inactivity, script)
  expect_node_fixture("inactivity.js", script)
})
